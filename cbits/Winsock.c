#include "HsFFI.h"
#include <string.h>
#include <windows.h>
#include <winsock2.h>

static void iocp_winsock_cleanup(void)
{
    WSACleanup();
}

HsBool iocp_winsock_init(void)
{
    WORD version = MAKEWORD(2, 2);
    WSADATA wsaData;
    int rc;

    memset(&wsaData, 0, sizeof(wsaData));
    rc = WSAStartup(version, &wsaData);
    if (rc != 0)
        return HS_BOOL_FALSE;
    if (!(LOBYTE(wsaData.wVersion) == 2 && HIBYTE(wsaData.wVersion) == 2))
    {
        WSACleanup();
        SetLastError(WSAVERNOTSUPPORTED);
        return HS_BOOL_FALSE;
    }

    rc = atexit(iocp_winsock_cleanup);
    if (rc != 0) {
        WSACleanup();
        SetLastError(ERROR_NOT_ENOUGH_MEMORY);
        return HS_BOOL_FALSE;
    }

    return HS_BOOL_TRUE;
}
