#include <errno.h>
#include <windows.h>

typedef struct _Overlapped {
    OVERLAPPED  raw; // assumed to be at offset 0
    void       *ctx;
} Overlapped;

Overlapped *iocp_new_overlapped(UINT64 offset, void *ctx)
{
    Overlapped *ol = HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(*ol));
    if (ol == NULL) {
        SetLastError(ERROR_NOT_ENOUGH_MEMORY);
        return NULL;
    } else {
        ol->raw.Offset     = (DWORD) offset;
        ol->raw.OffsetHigh = (DWORD) (offset >> 32);
        ol->ctx            = ctx;
        return ol;
    }
}

void *iocp_finish_overlapped(Overlapped *ol)
{
    void *ctx = ol->ctx;
    HeapFree(GetProcessHeap(), 0, ol);
    return ctx;
}

/*
 * Return values:
 *  TRUE:  Completion (successful or otherwise) was received.
 *  FALSE: GetQueuedCompletionStatus failed or timed out.
 *         numBytes_out and ctx_out are undefined.
 */
BOOL iocp_get_next_completion(HANDLE iocp, DWORD timeout,
    DWORD *numBytes_out, DWORD *err_out, void **ctx_out)
{
    OVERLAPPED *raw = NULL;
    ULONG_PTR completionKey = 0;
    BOOL ok;

    *numBytes_out = 0;
    ok = GetQueuedCompletionStatus(iocp, numBytes_out, &completionKey,
            &raw, timeout);
    *err_out = ok ? ERROR_SUCCESS : GetLastError();
    if (raw == NULL)
        return FALSE;
    *ctx_out = iocp_finish_overlapped((Overlapped*) raw);
    return TRUE;
}

typedef ULONGLONG (WINAPI *GetTickCount64_t)(void);

GetTickCount64_t iocp_load_GetTickCount64(void)
{
    return (GetTickCount64_t)
        GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
                       "GetTickCount64");
}

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

