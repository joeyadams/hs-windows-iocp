#include <windows.h>
#include "HsFFI.h"

typedef struct _OverlappedRec {
    OVERLAPPED  ol;
    HsStablePtr sptr;
} OverlappedRec;

// Tells an I/O manager thread what to do when the Overlapped appears on the
// completion port.
typedef enum _OverlappedState {
    O_START,
    O_SIGNAL,
    O_CANCEL,
} OverlappedState;

/*
 * Before an I/O request is started, an Overlapped is allocated with
 * information on how to start it (namely, a callback along with
 * operation-specific context, supplied by "subclassing" Overlapped).
 * The reason we don't just call e.g. ReadFile on the spot is because
 * CancelIo is sensitive to what threads operations are started from.  When
 * CancelIoEx is not available, we sometimes need to send the task of starting
 * I/O to another thread so we can cancel it without canceling other operations
 * on the same handle.  The reason the start callback has to be written in C
 * is so we can do this under the non-threaded RTS, where it is dangerous to
 * call into Haskell from forked OS threads.
 */
typedef BOOL (*StartCallback)(void *overlapped);

typedef struct _Overlapped {
    OVERLAPPED base;
    OverlappedState state;
    union {
        // O_START
        struct {
            // Callback that starts the overlapped I/O, returning TRUE if a
            // completion is expected to appear when the I/O is done.
            StartCallback callback;

            // Haskell function called when the I/O completes.
            // This is initialized by the I/O manager.
            HsStablePtr signal_callback;
        } start;

        // O_SIGNAL
        struct {
            // Called when this overlapped is dequeued from the completion port.
            HsStablePtr callback;
        } signal;

        // O_CANCEL
        //    Call CancelIo on the given handle, then free this Overlapped.
        //    CancelIo cancels pending I/O issued by the current thread on
        //    the given handle, allowing us to cancel individual operations
        //    in the absence of CancelIoEx.
        struct {
            HANDLE handle;
        } cancel;
    };
} Overlapped;

// Allocate an Overlapped as a command to start I\/O.
// See iocp.c for example usage, such as iocp_alloc_ReadFile.
void *iocp_alloc_start(size_t size, StartCallback callback);

// Allocate an Overlapped as a command to issue CancelIo on the given handle.
void *iocp_alloc_cancel(HANDLE handle);

// Free an Overlapped.  Overlapped objects aren't (necessarily) allocated with
// malloc and free, so use this instead of free to free an Overlapped allocated
// with iocp_alloc_start or similar.
void iocp_free(void *ol);

// Start an allocated I/O request.  Return TRUE if a completion is expected to
// be delivered when the I/O is done, or FALSE on failure where no completion
// is expected.
BOOL iocp_start(Overlapped *ol);

// I/O command constructors.  These don't start I/O themselves, but tell the
// I/O manager what to do to start the I/O.
Overlapped *iocp_alloc_ReadFile(HANDLE handle, LPVOID buffer, DWORD bytesToRead);
Overlapped *iocp_alloc_WriteFile(HANDLE handle, LPCVOID buffer, DWORD bytesToWrite);
