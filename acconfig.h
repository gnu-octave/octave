/* acconfig.h

   Descriptive text for the C preprocessor macros that are needed by
   Octave.

   Leave the following blank line there!!  Autoheader needs it.  */




/* Leave that blank line there!!  Autoheader needs it.
   If you're adding to this file, keep in mind:
   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  */

@BOTTOM@

#if defined (__GNUC__)
#define GCC_ATTR_NORETURN __attribute__ ((__noreturn__))
#define GCC_ATTR_UNUSED __attribute__ ((__unused__))
#else
#define GCC_ATTR_NORETURN
#define GCC_ATTR_UNUSED
#endif

#define CONST_CAST(T, E) (T) (E)

#define DYNAMIC_CAST(T, E) (T) (E)

#define REINTERPRET_CAST(T, E) (T) (E)

#define STATIC_CAST(T, E) (T) (E)

#define X_CAST(T, E) (T) (E)

#define HEAVYWEIGHT_INDEXING 1

#define WITH_KPATHSEARCH 1

#if defined(HAVE_F2C) && !defined(F77_FUNC)
#  define F77_FUNC(x,X) x ## _
#  define F77_FUNC_(x,X) x ## __
#endif

#if !defined(HAVE_DEV_T)
typedef dev_t short
#endif

#if !defined(HAVE_INO_T)
typedef ino_t unsigned long
#endif

#if !defined(HAVE_NLINK_T)
typedef nlink_t short
#endif

#if !defined(HAVE_SIGSET_T)
typedef sigset_t int
#endif
