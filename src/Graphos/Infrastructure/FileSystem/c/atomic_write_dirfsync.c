/*
 * Atomic write primitives for Graphos outputs.
 *
 * These tiny C helpers back Graphos.Infrastructure.FileSystem.AtomicWrite so the
 * Haskell side can make directory entries and file contents durable on disk
 * without depending on unstable internals of the @unix@ package.
 *
 * Everything here is platform-guarded: Linux uses plain @fsync@, while macOS /
 * BSD use @fcntl(F_FULLFSYNC)@, which Apple documents as the only way to force
 * a full physical sync of a file (including directories) against power loss.
 */

/* Enable POSIX directory flags even when compiled under a strict C dialect. */
#ifndef _DEFAULT_SOURCE
# define _DEFAULT_SOURCE 1
#endif

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#ifndef O_DIRECTORY
/* Fallback for platforms whose <fcntl.h> does not provide it. */
# define O_DIRECTORY 0x2000
#endif

/*
 * Open @path@ read-only and fsync it, returning 0 on success or -1 with @errno@
 * set on failure. When @is_dir@ is non-zero the path is opened with
 * @O_DIRECTORY@ and, on macOS / BSD, synced with @fcntl(F_FULLFSYNC)@.
 */
int hs_graphos_fsync_path(const char *path, int is_dir) {
    int fd = open(path, O_RDONLY | (is_dir ? O_DIRECTORY : 0));
    if (fd < 0) {
        return -1;
    }
#if defined(__APPLE__) || defined(__FreeBSD__) \
    || defined(__OpenBSD__) || defined(__NetBSD__) \
    || defined(__DragonFly__)
    if (is_dir) {
        int rc = fcntl(fd, F_FULLFSYNC);
        close(fd);
        return rc == 0 ? 0 : -1;
    }
#endif
    int rc = fsync(fd);
    close(fd);
    return rc == 0 ? 0 : -1;
}

/*
 * Return the device id (@st_dev@) that @path@ lives on, or -1 if @path@ cannot
 * be stat'd. Used to detect when a temp file and its target sit on different
 * filesystems, which would make the rename between them non-atomic.
 */
long hs_graphos_device_of(const char *path) {
    struct stat st;
    if (stat(path, &st) == -1) {
        return -1;
    }
    return (long) st.st_dev;
}
