#include <errno.h>
#include <windows.h>

typedef ULONGLONG (WINAPI *GetTickCount64_t)(void);

GetTickCount64_t iocp_load_GetTickCount64(void)
{
    return (GetTickCount64_t)
        GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
                       "GetTickCount64");
}

typedef BOOL (WINAPI *CancelIoEx_t)(HANDLE hFile, LPOVERLAPPED lpOverlapped);

CancelIoEx_t iocp_load_CancelIoEx(void)
{
    return (CancelIoEx_t)
        GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
                       "CancelIoEx");
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
