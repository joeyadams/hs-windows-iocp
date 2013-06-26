#include <windows.h>
#include "HsFFI.h"

typedef struct _OverlappedRec {
    OVERLAPPED  ol;
    HsStablePtr sptr;
} OverlappedRec;

// Tells an I/O manager thread what to do when the Overlapped appears on the
// completion port.
typedef enum _OverlappedTag {
    O_START,
    O_SIGNAL,
    O_CANCEL,
} OverlappedTag;

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
    OverlappedTag tag;
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
