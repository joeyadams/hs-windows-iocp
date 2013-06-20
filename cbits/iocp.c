#include <errno.h>
#include <windows.h>

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
