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

void *iocp_alloc_start(size_t size, StartCallback callback)
{
    Overlapped *ol = iocp_alloc(size);
    if (ol != NULL) {
        ol->state = O_START;
        ol->startCallback = callback;
    }
    return ol;
}

Overlapped *iocp_alloc_cancel(HANDLE handle)
{
    Overlapped *ol = iocp_alloc(sizeof(Overlapped));
    if (ol != NULL) {
        ol->state = O_CANCEL;
        ol->cancelHandle = handle;
    }
    return ol;
}

void iocp_free(void *ol)
{
    if (ol != NULL)
        HeapFree(GetProcessHeap(), 0, ol);
}

BOOL iocp_start(Overlapped *ol)
{
    assert(ol->state == O_START);

    // Morph the Overlapped into an O_SIGNAL so when it appears in the
    // completion port, the manager will know to treat it as completed I/O
    // instead of another start request.
    ol->state = O_SIGNAL;

    return ol->startCallback(ol);
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
