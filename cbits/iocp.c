#include "iocp.h"
#include <assert.h>

// Original code from Win32 package (errors.c)
LPWSTR iocp_getErrorMessage(DWORD err)
{
    LPWSTR what = NULL;
    DWORD res;

    res = FormatMessageW(
              (FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER),
              NULL,
              err,
              MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), /* Default language */
              (LPWSTR) &what,
              0,
              NULL
          );
    if (res == 0)
        return NULL;
    return what;
}

static void *iocp_alloc(size_t size)
{
    assert(size >= sizeof(Overlapped));
    void *ol = HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, size);
    if (ol == NULL)
        SetLastError(ERROR_NOT_ENOUGH_MEMORY);
    return ol;
}

void iocp_free(void *ol)
{
    if (ol != NULL)
        HeapFree(GetProcessHeap(), 0, ol);
}

void *iocp_alloc_start(size_t size, StartCallback callback)
{
    Overlapped *ol = iocp_alloc(size);
    if (ol != NULL) {
        ol->tag = O_START;
        ol->start.callback = callback;
    }
    return ol;
}

void *iocp_alloc_cancel(HANDLE handle)
{
    Overlapped *ol = iocp_alloc(sizeof(Overlapped));
    if (ol != NULL) {
        ol->tag = O_CANCEL;
        ol->cancel.handle = handle;
    }
    return ol;
}

// Start an allocated I/O request.  Return TRUE if a completion is expected to
// be delivered when the I/O is done, or FALSE on failure where no completion
// is expected.
BOOL iocp_start(Overlapped *ol)
{
    assert(ol->tag == O_START);
    StartCallback start_callback = ol->start.callback;
    HsStablePtr signal_callback = ol->start.signal_callback;

    // Morph the Overlapped into an O_SIGNAL so when it appears in the
    // completion port, the manager will know to treat it as completed I/O
    // instead of another start request.
    ol->tag = O_SIGNAL;
    ol->signal.callback = signal_callback;

    return start_callback(ol);
}

typedef struct {
    Overlapped base;
    HANDLE handle;
    LPVOID buffer;
    DWORD bytesToRead;
} ol_ReadFile;

static BOOL cb_ReadFile(void *ol)
{
    ol_ReadFile *ctx = ol;
    BOOL ok = ReadFile(ctx->handle, ctx->buffer, ctx->bytesToRead, NULL, ol);
    return (ok || GetLastError() == ERROR_IO_PENDING);
}

Overlapped *iocp_alloc_ReadFile(HANDLE handle, LPVOID buffer, DWORD bytesToRead)
{
    ol_ReadFile *ctx = iocp_alloc_start(sizeof(*ctx), cb_ReadFile);
    if (ctx != NULL) {
        ctx->handle = handle;
        ctx->buffer = buffer;
        ctx->bytesToRead = bytesToRead;
    }
    return (Overlapped*) ctx;
}

typedef struct {
    Overlapped base;
    HANDLE handle;
    LPCVOID buffer;
    DWORD bytesToWrite;
} ol_WriteFile;

static BOOL cb_WriteFile(void *ol)
{
    ol_WriteFile *ctx = ol;
    BOOL ok = WriteFile(ctx->handle, ctx->buffer, ctx->bytesToWrite, NULL, ol);
    return (ok || GetLastError() == ERROR_IO_PENDING);
}

Overlapped *iocp_alloc_WriteFile(HANDLE handle, LPCVOID buffer, DWORD bytesToWrite)
{
    ol_WriteFile *ctx = iocp_alloc_start(sizeof(*ctx), cb_WriteFile);
    if (ctx != NULL) {
        ctx->handle = handle;
        ctx->buffer = buffer;
        ctx->bytesToWrite = bytesToWrite;
    }
    return (Overlapped*) ctx;
}
