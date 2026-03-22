/*
 * ffi/network.c — Cross-platform socket FFI for Lean 4
 *
 * Inspired by Haskell's `network` package. Supports:
 * - IPv4 and IPv6 (AF_INET, AF_INET6, AF_UNIX)
 * - TCP (SOCK_STREAM), UDP (SOCK_DGRAM), Raw (SOCK_RAW)
 * - Event multiplexing via kqueue (macOS) / epoll (Linux)
 * - Proper Lean pair encoding (nested ctor(0,2,0))
 * - All errors surfaced as IO.Error (no crashes)
 *
 * Platform: macOS (Darwin) and Linux. No Windows support yet.
 */

#include <lean/lean.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

/* Platform-specific event multiplexing headers */
#ifdef __APPLE__
#include <sys/event.h>
#elif defined(__linux__)
#include <sys/epoll.h>
#endif

/* ────────────────────────────────────────────────────────────
 * Helper: make a Lean IO error from errno
 * ──────────────────────────────────────────────────────────── */
static inline lean_obj_res mk_io_error(const char *msg) {
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(msg)));
}

static inline lean_obj_res mk_io_errno_error(void) {
    return mk_io_error(strerror(errno));
}

/* ────────────────────────────────────────────────────────────
 * Helper: make a Lean pair (Prod)
 *   Lean encodes (a, b) as ctor(0, 2, 0) with fields [a, b]
 * ──────────────────────────────────────────────────────────── */
static inline lean_obj_res mk_pair(lean_obj_arg fst, lean_obj_arg snd) {
    lean_object *p = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(p, 0, fst);
    lean_ctor_set(p, 1, snd);
    return p;
}

/* ────────────────────────────────────────────────────────────
 * Helper: make a Lean List cons / nil
 * ──────────────────────────────────────────────────────────── */
static inline lean_obj_res mk_list_nil(void) {
    return lean_box(0);
}

static inline lean_obj_res mk_list_cons(lean_obj_arg head, lean_obj_arg tail) {
    lean_object *c = lean_alloc_ctor(1, 2, 0);
    lean_ctor_set(c, 0, head);
    lean_ctor_set(c, 1, tail);
    return c;
}

/* ────────────────────────────────────────────────────────────
 * Address family encoding: Family -> UInt8
 *   0 = AF_INET, 1 = AF_INET6, 2 = AF_UNIX
 * ──────────────────────────────────────────────────────────── */
static int family_to_af(uint8_t fam) {
    switch (fam) {
        case 0: return AF_INET;
        case 1: return AF_INET6;
        case 2: return AF_UNIX;
        default: return AF_INET;
    }
}

static uint8_t af_to_family(int af) {
    switch (af) {
        case AF_INET: return 0;
        case AF_INET6: return 1;
        case AF_UNIX: return 2;
        default: return 0;
    }
}

/* ────────────────────────────────────────────────────────────
 * Socket type encoding: SocketType -> UInt8
 *   0 = SOCK_STREAM, 1 = SOCK_DGRAM, 2 = SOCK_RAW
 * ──────────────────────────────────────────────────────────── */
static int socktype_to_st(uint8_t st) {
    switch (st) {
        case 0: return SOCK_STREAM;
        case 1: return SOCK_DGRAM;
        case 2: return SOCK_RAW;
        default: return SOCK_STREAM;
    }
}

/* ────────────────────────────────────────────────────────────
 * Helper: format a sockaddr into (host_string, port)
 * Returns a Lean pair (String x USize)
 * ──────────────────────────────────────────────────────────── */
static lean_obj_res sockaddr_to_lean_pair(struct sockaddr_storage *addr, socklen_t addrlen) {
    char ip[INET6_ADDRSTRLEN + 1];
    uint16_t port = 0;

    if (addr->ss_family == AF_INET) {
        struct sockaddr_in *sin = (struct sockaddr_in *)addr;
        inet_ntop(AF_INET, &sin->sin_addr, ip, sizeof(ip));
        port = ntohs(sin->sin_port);
    } else if (addr->ss_family == AF_INET6) {
        struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *)addr;
        inet_ntop(AF_INET6, &sin6->sin6_addr, ip, sizeof(ip));
        port = ntohs(sin6->sin6_port);
    } else if (addr->ss_family == AF_UNIX) {
        struct sockaddr_un *sun = (struct sockaddr_un *)addr;
        strncpy(ip, sun->sun_path, sizeof(ip) - 1);
        ip[sizeof(ip) - 1] = '\0';
        port = 0;
    } else {
        strcpy(ip, "unknown");
        port = 0;
    }

    return mk_pair(lean_mk_string(ip), lean_box((size_t)port));
}

/* ────────────────────────────────────────────────────────────
 * Helper: resolve host+port to sockaddr_storage
 * Tries getaddrinfo for both IPv4 and IPv6.
 * ──────────────────────────────────────────────────────────── */
static int resolve_addr(const char *host, uint16_t port, int family_hint,
                        struct sockaddr_storage *out, socklen_t *outlen) {
    /* Fast path: try inet_pton for numeric addresses */
    if (family_hint == AF_INET || family_hint == AF_UNSPEC) {
        struct sockaddr_in sin;
        memset(&sin, 0, sizeof(sin));
        sin.sin_family = AF_INET;
        sin.sin_port = htons(port);
        if (inet_pton(AF_INET, host, &sin.sin_addr) == 1) {
            memcpy(out, &sin, sizeof(sin));
            *outlen = sizeof(sin);
            return 0;
        }
    }
    if (family_hint == AF_INET6 || family_hint == AF_UNSPEC) {
        struct sockaddr_in6 sin6;
        memset(&sin6, 0, sizeof(sin6));
        sin6.sin6_family = AF_INET6;
        sin6.sin6_port = htons(port);
        if (inet_pton(AF_INET6, host, &sin6.sin6_addr) == 1) {
            memcpy(out, &sin6, sizeof(sin6));
            *outlen = sizeof(sin6);
            return 0;
        }
    }

    /* Fall back to getaddrinfo for hostnames */
    struct addrinfo hints, *res;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = family_hint;
    hints.ai_socktype = SOCK_STREAM;

    char portstr[16];
    snprintf(portstr, sizeof(portstr), "%u", port);

    int ret = getaddrinfo(host, portstr, &hints, &res);
    if (ret != 0) return -1;

    memcpy(out, res->ai_addr, res->ai_addrlen);
    *outlen = res->ai_addrlen;
    freeaddrinfo(res);
    return 0;
}

/* ================================================================
 * SOCKET CREATION AND MANAGEMENT
 * ================================================================ */

/**
 * socket(domain, type, protocol) -> fd
 * domain: 0=AF_INET, 1=AF_INET6, 2=AF_UNIX
 * type:   0=SOCK_STREAM, 1=SOCK_DGRAM, 2=SOCK_RAW
 */
LEAN_EXPORT lean_obj_res hale_socket_create(uint8_t domain, uint8_t socktype, lean_obj_arg world) {
    int af = family_to_af(domain);
    int st = socktype_to_st(socktype);
    int fd = socket(af, st, 0);
    if (fd < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box((size_t)fd));
}

/**
 * close(fd)
 */
LEAN_EXPORT lean_obj_res hale_socket_close(size_t fd, lean_obj_arg world) {
    if (close((int)fd) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * bind(fd, host, port) -- supports IPv4, IPv6, and numeric addresses
 */
LEAN_EXPORT lean_obj_res hale_socket_bind(size_t fd, lean_obj_arg host, uint16_t port, lean_obj_arg world) {
    const char *h = lean_string_cstr(host);

    /* Determine socket family from the fd using getsockname */
    struct sockaddr_storage ss;
    socklen_t sslen;
    int sock_domain = AF_UNSPEC;
    {
        struct sockaddr_storage tmp;
        socklen_t tmplen = sizeof(tmp);
        if (getsockname((int)fd, (struct sockaddr *)&tmp, &tmplen) == 0) {
            sock_domain = tmp.ss_family;
        }
    }

    if (resolve_addr(h, port, sock_domain, &ss, &sslen) < 0) {
        return mk_io_error("bind: invalid address or hostname resolution failed");
    }

    if (bind((int)fd, (struct sockaddr *)&ss, sslen) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * listen(fd, backlog)
 */
LEAN_EXPORT lean_obj_res hale_socket_listen(size_t fd, size_t backlog, lean_obj_arg world) {
    if (listen((int)fd, (int)backlog) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * accept(fd) -> (client_fd, (remote_host, remote_port))
 *
 * CRITICAL FIX: Returns nested pair (USize x (String x USize))
 * encoded as ctor(0,2,0)[fd, ctor(0,2,0)[host, port]]
 * instead of flat 3-tuple ctor(0,3,0) which caused segfault.
 *
 * Supports both IPv4 and IPv6 peers.
 */
LEAN_EXPORT lean_obj_res hale_socket_accept(size_t fd, lean_obj_arg world) {
    struct sockaddr_storage addr;
    socklen_t addrlen = sizeof(addr);
    int client = accept((int)fd, (struct sockaddr *)&addr, &addrlen);
    if (client < 0) {
        return mk_io_errno_error();
    }

    /* Format peer address */
    lean_obj_res addr_pair = sockaddr_to_lean_pair(&addr, addrlen);

    /* Build nested pair: (client_fd, (host, port)) */
    lean_obj_res result = mk_pair(lean_box((size_t)client), addr_pair);
    return lean_io_result_mk_ok(result);
}

/**
 * connect(fd, host, port) -- supports IPv4 and IPv6
 */
LEAN_EXPORT lean_obj_res hale_socket_connect(size_t fd, lean_obj_arg host, uint16_t port, lean_obj_arg world) {
    const char *h = lean_string_cstr(host);

    struct sockaddr_storage ss;
    socklen_t sslen;
    int sock_domain = AF_UNSPEC;
    {
        struct sockaddr_storage tmp;
        socklen_t tmplen = sizeof(tmp);
        if (getsockname((int)fd, (struct sockaddr *)&tmp, &tmplen) == 0 && tmp.ss_family != 0) {
            sock_domain = tmp.ss_family;
        }
    }

    if (resolve_addr(h, port, sock_domain, &ss, &sslen) < 0) {
        return mk_io_error("connect: invalid address or hostname resolution failed");
    }

    if (connect((int)fd, (struct sockaddr *)&ss, sslen) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/* ================================================================
 * SEND / RECV (TCP)
 * ================================================================ */

/**
 * send(fd, data) -> bytes_sent
 */
LEAN_EXPORT lean_obj_res hale_socket_send(size_t fd, b_lean_obj_arg data, lean_obj_arg world) {
    size_t len = lean_sarray_size(data);
    const uint8_t *buf = lean_sarray_cptr(data);
    ssize_t sent = send((int)fd, buf, len, 0);
    if (sent < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box((size_t)sent));
}

/**
 * recv(fd, maxlen) -> ByteArray
 */
LEAN_EXPORT lean_obj_res hale_socket_recv(size_t fd, size_t maxlen, lean_obj_arg world) {
    uint8_t *buf = malloc(maxlen);
    if (!buf) {
        return mk_io_error("recv: malloc failed");
    }
    ssize_t n = recv((int)fd, buf, maxlen, 0);
    if (n < 0) {
        free(buf);
        return mk_io_errno_error();
    }
    lean_object *arr = lean_alloc_sarray(1, (size_t)n, (size_t)n);
    memcpy(lean_sarray_cptr(arr), buf, (size_t)n);
    free(buf);
    return lean_io_result_mk_ok(arr);
}

/* ================================================================
 * UDP: sendto / recvfrom
 * ================================================================ */

/**
 * sendto(fd, data, host, port) -> bytes_sent
 */
LEAN_EXPORT lean_obj_res hale_socket_sendto(size_t fd, b_lean_obj_arg data,
                                             lean_obj_arg host, uint16_t port,
                                             lean_obj_arg world) {
    size_t len = lean_sarray_size(data);
    const uint8_t *buf = lean_sarray_cptr(data);
    const char *h = lean_string_cstr(host);

    struct sockaddr_storage ss;
    socklen_t sslen;
    if (resolve_addr(h, port, AF_UNSPEC, &ss, &sslen) < 0) {
        return mk_io_error("sendto: invalid address");
    }

    ssize_t sent = sendto((int)fd, buf, len, 0, (struct sockaddr *)&ss, sslen);
    if (sent < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box((size_t)sent));
}

/**
 * recvfrom(fd, maxlen) -> (ByteArray, (host_string, port))
 * Returns nested pair: (ByteArray x (String x USize))
 */
LEAN_EXPORT lean_obj_res hale_socket_recvfrom(size_t fd, size_t maxlen, lean_obj_arg world) {
    uint8_t *buf = malloc(maxlen);
    if (!buf) {
        return mk_io_error("recvfrom: malloc failed");
    }
    struct sockaddr_storage addr;
    socklen_t addrlen = sizeof(addr);
    ssize_t n = recvfrom((int)fd, buf, maxlen, 0, (struct sockaddr *)&addr, &addrlen);
    if (n < 0) {
        free(buf);
        return mk_io_errno_error();
    }

    lean_object *arr = lean_alloc_sarray(1, (size_t)n, (size_t)n);
    memcpy(lean_sarray_cptr(arr), buf, (size_t)n);
    free(buf);

    lean_obj_res addr_pair = sockaddr_to_lean_pair(&addr, addrlen);
    lean_obj_res result = mk_pair(arr, addr_pair);
    return lean_io_result_mk_ok(result);
}

/* ================================================================
 * SOCKET OPTIONS
 * ================================================================ */

/**
 * setsockopt SO_REUSEADDR
 */
LEAN_EXPORT lean_obj_res hale_socket_set_reuseaddr(size_t fd, uint8_t enable, lean_obj_arg world) {
    int val = enable ? 1 : 0;
    if (setsockopt((int)fd, SOL_SOCKET, SO_REUSEADDR, &val, sizeof(val)) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * setsockopt TCP_NODELAY
 */
LEAN_EXPORT lean_obj_res hale_socket_set_nodelay(size_t fd, uint8_t enable, lean_obj_arg world) {
    int val = enable ? 1 : 0;
    if (setsockopt((int)fd, IPPROTO_TCP, TCP_NODELAY, &val, sizeof(val)) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * Set non-blocking mode
 */
LEAN_EXPORT lean_obj_res hale_socket_set_nonblocking(size_t fd, uint8_t enable, lean_obj_arg world) {
    int flags = fcntl((int)fd, F_GETFL, 0);
    if (flags < 0) {
        return mk_io_errno_error();
    }
    if (enable) {
        flags |= O_NONBLOCK;
    } else {
        flags &= ~O_NONBLOCK;
    }
    if (fcntl((int)fd, F_SETFL, flags) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * setsockopt SO_KEEPALIVE
 */
LEAN_EXPORT lean_obj_res hale_socket_set_keepalive(size_t fd, uint8_t enable, lean_obj_arg world) {
    int val = enable ? 1 : 0;
    if (setsockopt((int)fd, SOL_SOCKET, SO_KEEPALIVE, &val, sizeof(val)) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * setsockopt SO_LINGER
 */
LEAN_EXPORT lean_obj_res hale_socket_set_linger(size_t fd, uint8_t enable, size_t seconds, lean_obj_arg world) {
    struct linger lg;
    lg.l_onoff = enable ? 1 : 0;
    lg.l_linger = (int)seconds;
    if (setsockopt((int)fd, SOL_SOCKET, SO_LINGER, &lg, sizeof(lg)) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * setsockopt SO_RCVBUF
 */
LEAN_EXPORT lean_obj_res hale_socket_set_recvbuf(size_t fd, size_t sz, lean_obj_arg world) {
    int val = (int)sz;
    if (setsockopt((int)fd, SOL_SOCKET, SO_RCVBUF, &val, sizeof(val)) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * setsockopt SO_SNDBUF
 */
LEAN_EXPORT lean_obj_res hale_socket_set_sendbuf(size_t fd, size_t sz, lean_obj_arg world) {
    int val = (int)sz;
    if (setsockopt((int)fd, SOL_SOCKET, SO_SNDBUF, &val, sizeof(val)) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * shutdown(fd, how)
 * how: 0=SHUT_RD, 1=SHUT_WR, 2=SHUT_RDWR
 */
LEAN_EXPORT lean_obj_res hale_socket_shutdown(size_t fd, uint8_t how, lean_obj_arg world) {
    int shuthow;
    switch (how) {
        case 0: shuthow = SHUT_RD; break;
        case 1: shuthow = SHUT_WR; break;
        case 2: shuthow = SHUT_RDWR; break;
        default: return mk_io_error("shutdown: invalid how value");
    }
    if (shutdown((int)fd, shuthow) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * getpeername(fd) -> (host_string, port)
 */
LEAN_EXPORT lean_obj_res hale_socket_getpeername(size_t fd, lean_obj_arg world) {
    struct sockaddr_storage addr;
    socklen_t addrlen = sizeof(addr);
    if (getpeername((int)fd, (struct sockaddr *)&addr, &addrlen) < 0) {
        return mk_io_errno_error();
    }
    lean_obj_res pair = sockaddr_to_lean_pair(&addr, addrlen);
    return lean_io_result_mk_ok(pair);
}

/**
 * getsockname(fd) -> (host_string, port)
 */
LEAN_EXPORT lean_obj_res hale_socket_getsockname(size_t fd, lean_obj_arg world) {
    struct sockaddr_storage addr;
    socklen_t addrlen = sizeof(addr);
    if (getsockname((int)fd, (struct sockaddr *)&addr, &addrlen) < 0) {
        return mk_io_errno_error();
    }
    lean_obj_res pair = sockaddr_to_lean_pair(&addr, addrlen);
    return lean_io_result_mk_ok(pair);
}

/* ================================================================
 * GETADDRINFO
 * ================================================================ */

/**
 * getaddrinfo(node, service) -> List (family x (host x port))
 *
 * Returns nested pairs, not flat 3-tuples.
 * Supports both IPv4 and IPv6 results.
 */
LEAN_EXPORT lean_obj_res hale_getaddrinfo(lean_obj_arg node, lean_obj_arg service, lean_obj_arg world) {
    struct addrinfo hints, *res, *p;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    const char *n = lean_string_cstr(node);
    const char *s = lean_string_cstr(service);

    int ret = getaddrinfo(n, s, &hints, &res);
    if (ret != 0) {
        return mk_io_error(gai_strerror(ret));
    }

    /* Build a Lean list in reverse order (prepend) */
    lean_object *list = mk_list_nil();
    for (p = res; p != NULL; p = p->ai_next) {
        char ip[INET6_ADDRSTRLEN];
        uint16_t port = 0;
        uint8_t family;

        if (p->ai_family == AF_INET) {
            struct sockaddr_in *sin = (struct sockaddr_in *)p->ai_addr;
            inet_ntop(AF_INET, &sin->sin_addr, ip, sizeof(ip));
            port = ntohs(sin->sin_port);
            family = 0;
        } else if (p->ai_family == AF_INET6) {
            struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *)p->ai_addr;
            inet_ntop(AF_INET6, &sin6->sin6_addr, ip, sizeof(ip));
            port = ntohs(sin6->sin6_port);
            family = 1;
        } else {
            continue;
        }

        /* Nested pair: (family, (host, port)) */
        lean_obj_res inner = mk_pair(lean_mk_string(ip), lean_box((size_t)port));
        lean_obj_res entry = mk_pair(lean_box((size_t)family), inner);
        list = mk_list_cons(entry, list);
    }

    freeaddrinfo(res);
    return lean_io_result_mk_ok(list);
}

/* ================================================================
 * EVENT MULTIPLEXING: kqueue (macOS) / epoll (Linux)
 * ================================================================ */

/*
 * Event type flags (must match Lean EventType):
 *   bit 0 = readable  (1)
 *   bit 1 = writable  (2)
 *   bit 2 = error      (4)
 */
#define HALE_EV_READABLE 1
#define HALE_EV_WRITABLE 2
#define HALE_EV_ERROR    4

/**
 * Create an event loop fd (kqueue on macOS, epoll on Linux)
 */
LEAN_EXPORT lean_obj_res hale_event_loop_create(lean_obj_arg world) {
#ifdef __APPLE__
    int fd = kqueue();
    if (fd < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box((size_t)fd));
#elif defined(__linux__)
    int fd = epoll_create1(0);
    if (fd < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box((size_t)fd));
#else
    return mk_io_error("event_loop_create: unsupported platform");
#endif
}

/**
 * Register interest in events for a socket fd
 * events: bitmask of HALE_EV_READABLE | HALE_EV_WRITABLE
 */
LEAN_EXPORT lean_obj_res hale_event_loop_add(size_t loop_fd, size_t socket_fd, size_t events, lean_obj_arg world) {
#ifdef __APPLE__
    struct kevent changes[2];
    int nchanges = 0;
    if (events & HALE_EV_READABLE) {
        EV_SET(&changes[nchanges], (uintptr_t)socket_fd, EVFILT_READ, EV_ADD | EV_ENABLE, 0, 0, NULL);
        nchanges++;
    }
    if (events & HALE_EV_WRITABLE) {
        EV_SET(&changes[nchanges], (uintptr_t)socket_fd, EVFILT_WRITE, EV_ADD | EV_ENABLE, 0, 0, NULL);
        nchanges++;
    }
    if (nchanges == 0) {
        return mk_io_error("event_loop_add: no events specified");
    }
    if (kevent((int)loop_fd, changes, nchanges, NULL, 0, NULL) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
#elif defined(__linux__)
    struct epoll_event ev;
    memset(&ev, 0, sizeof(ev));
    ev.data.fd = (int)socket_fd;
    if (events & HALE_EV_READABLE) ev.events |= EPOLLIN;
    if (events & HALE_EV_WRITABLE) ev.events |= EPOLLOUT;
    if (epoll_ctl((int)loop_fd, EPOLL_CTL_ADD, (int)socket_fd, &ev) < 0) {
        /* If already registered, try MOD */
        if (errno == EEXIST) {
            if (epoll_ctl((int)loop_fd, EPOLL_CTL_MOD, (int)socket_fd, &ev) < 0) {
                return mk_io_errno_error();
            }
        } else {
            return mk_io_errno_error();
        }
    }
    return lean_io_result_mk_ok(lean_box(0));
#else
    return mk_io_error("event_loop_add: unsupported platform");
#endif
}

/**
 * Unregister a socket fd from the event loop
 */
LEAN_EXPORT lean_obj_res hale_event_loop_del(size_t loop_fd, size_t socket_fd, lean_obj_arg world) {
#ifdef __APPLE__
    /* Remove both read and write filters; ignore errors if not registered */
    struct kevent changes[2];
    EV_SET(&changes[0], (uintptr_t)socket_fd, EVFILT_READ, EV_DELETE, 0, 0, NULL);
    EV_SET(&changes[1], (uintptr_t)socket_fd, EVFILT_WRITE, EV_DELETE, 0, 0, NULL);
    /* Best effort: kevent may fail for filters not registered */
    kevent((int)loop_fd, &changes[0], 1, NULL, 0, NULL);
    kevent((int)loop_fd, &changes[1], 1, NULL, 0, NULL);
    return lean_io_result_mk_ok(lean_box(0));
#elif defined(__linux__)
    if (epoll_ctl((int)loop_fd, EPOLL_CTL_DEL, (int)socket_fd, NULL) < 0) {
        if (errno != ENOENT) {
            return mk_io_errno_error();
        }
    }
    return lean_io_result_mk_ok(lean_box(0));
#else
    return mk_io_error("event_loop_del: unsupported platform");
#endif
}

/**
 * Wait for events. Returns List (fd x events) where events is a bitmask.
 * timeout_ms: timeout in milliseconds (-1 = block indefinitely)
 */
LEAN_EXPORT lean_obj_res hale_event_loop_wait(size_t loop_fd, size_t timeout_ms, lean_obj_arg world) {
#ifdef __APPLE__
    #define MAX_EVENTS 64
    struct kevent kevents[MAX_EVENTS];
    struct timespec ts;
    struct timespec *tsp = NULL;

    if ((int64_t)timeout_ms >= 0) {
        ts.tv_sec = (time_t)(timeout_ms / 1000);
        ts.tv_nsec = (long)((timeout_ms % 1000) * 1000000);
        tsp = &ts;
    }

    int n = kevent((int)loop_fd, NULL, 0, kevents, MAX_EVENTS, tsp);
    if (n < 0) {
        if (errno == EINTR) {
            return lean_io_result_mk_ok(mk_list_nil());
        }
        return mk_io_errno_error();
    }

    lean_object *list = mk_list_nil();
    for (int i = n - 1; i >= 0; i--) {
        size_t ev_fd = (size_t)kevents[i].ident;
        size_t ev_flags = 0;
        if (kevents[i].filter == EVFILT_READ) ev_flags |= HALE_EV_READABLE;
        if (kevents[i].filter == EVFILT_WRITE) ev_flags |= HALE_EV_WRITABLE;
        if (kevents[i].flags & EV_ERROR) ev_flags |= HALE_EV_ERROR;
        if (kevents[i].flags & EV_EOF) ev_flags |= HALE_EV_ERROR;

        lean_obj_res pair = mk_pair(lean_box(ev_fd), lean_box(ev_flags));
        list = mk_list_cons(pair, list);
    }
    return lean_io_result_mk_ok(list);
    #undef MAX_EVENTS

#elif defined(__linux__)
    #define MAX_EVENTS 64
    struct epoll_event epevents[MAX_EVENTS];

    int n = epoll_wait((int)loop_fd, epevents, MAX_EVENTS, (int)timeout_ms);
    if (n < 0) {
        if (errno == EINTR) {
            return lean_io_result_mk_ok(mk_list_nil());
        }
        return mk_io_errno_error();
    }

    lean_object *list = mk_list_nil();
    for (int i = n - 1; i >= 0; i--) {
        size_t ev_fd = (size_t)epevents[i].data.fd;
        size_t ev_flags = 0;
        if (epevents[i].events & EPOLLIN) ev_flags |= HALE_EV_READABLE;
        if (epevents[i].events & EPOLLOUT) ev_flags |= HALE_EV_WRITABLE;
        if (epevents[i].events & (EPOLLERR | EPOLLHUP)) ev_flags |= HALE_EV_ERROR;

        lean_obj_res pair = mk_pair(lean_box(ev_fd), lean_box(ev_flags));
        list = mk_list_cons(pair, list);
    }
    return lean_io_result_mk_ok(list);
    #undef MAX_EVENTS

#else
    return mk_io_error("event_loop_wait: unsupported platform");
#endif
}

/**
 * Close the event loop fd
 */
LEAN_EXPORT lean_obj_res hale_event_loop_close(size_t loop_fd, lean_obj_arg world) {
    if (close((int)loop_fd) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}
