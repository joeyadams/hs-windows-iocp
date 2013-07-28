#include <windows.h>
#include "HsFFI.h"

typedef struct _OverlappedRec {
    OVERLAPPED  ol;
    HsStablePtr sptr;
    BOOL        alive;
} OverlappedRec;
