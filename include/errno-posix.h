#include <errno.h>

// Error code values the MinGW headers don't include.
#define EADDRINUSE    100
#define EADDRNOTAVAIL 101
#define EAFNOSUPPORT  102
#define ECONNREFUSED  107
#define ECONNRESET    108
#define EINPROGRESS   112
#define EISCONN       113
#define ENOTCONN      126
#define ENOTSOCK      128
#define EOTHER        131
#define ETIMEDOUT     138
