#if defined (HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif
#include <stdarg.h>

extern int
portable_snprintf (char *str, size_t str_m, const char *fmt, ...);

extern int
portable_vsnprintf (char *str, size_t str_m, const char *fmt, va_list ap);
