/* acconfig.h

   Descriptive text for the C preprocessor macros that are needed by
   Octave.

   Leave the following blank line there!!  Autoheader needs it.  */


/* Define if you want bounds checking on element references for
   internal array and matrix classes. */
#undef BOUNDS_CHECKING

/* Define if your compiler supports `<>' stuff for template friends. */
#undef CXX_NEW_FRIEND_TEMPLATE_DECL

/* Define if your math.h declares struct exception for matherr() */
#undef EXCEPTION_IN_MATH

/* Define if your Fortran compiler appends an underscore to external
   names. */ 
#undef F77_APPEND_UNDERSCORE

/* Define if your Fortran compiler converts external names to
   upper case. */
#undef F77_UPPERCASE_NAMES

/* Define if your system has a single-arg prototype for gettimeofday. */
#undef GETTIMEOFDAY_NO_TZ 

/* Define if your gnuplot supports multiplt plot windows with X11. */
#undef GNUPLOT_HAS_FRAMES

/* Define if your gnuplot has mutliplot. */
#undef GNUPLOT_HAS_MULTIPLOT

/* Define if your system's struct group has a gr_passwd field. */
#undef HAVE_GR_PASSWD

/* Define if you have isinf(). */
#undef HAVE_ISINF

/* Define if you have isnan(). */
#undef HAVE_ISNAN

/* Define if you have BSD style signals. */
#undef HAVE_BSD_SIGNALS

/* Define if you have POSIX style signals. */
#undef HAVE_POSIX_SIGNALS

/* Define if your system has program_invocation_name. */
#undef HAVE_PROGRAM_INVOCATION_NAME

/* Define if your system has a sys_siglist variable. */
#undef HAVE_SYS_SIGLIST

/* Define if your system defines struct timeval. */
#undef HAVE_TIMEVAL

/* Define if you have System V Release 3 style signals. */
#undef HAVE_USG_SIGHOLD

/* Define if signal handlers must be reinstalled after they are called. */
#undef MUST_REINSTALL_SIGHANDLERS

/* Define (to string::npos) if <string> doesn't. */
#undef NPOS

/* Define to compile smaller kernel. */
#undef OCTAVE_LITE

/* Define if this is Octave. */
#undef OCTAVE_SOURCE

/* Define if your struct rusage only has time information. */
#undef RUSAGE_TIMES_ONLY

/* Define if you are using an SCO system. */
#undef SCO

/* Define this to be the path separator for your system, as a
   character constant */
#undef SEPCHAR

/* Define this to be the path separator for your system, as a
   character string */
#undef SEPCHAR_STR

/* Define if math.h declares signgam. */
#undef SIGNGAM_DECLARED

/* To quiet autoheader. */
#undef SMART_PUTENV

/* Use GNU info for extended help system. */
#undef USE_GNU_INFO

/* Use plplot for plotting. */
#undef USE_PLPLOT

/* Use GNU readline for command line editing and history. */
#undef USE_READLINE

/* Define if using dlopen/dlsym for dynamic linking of object files. */
#undef WITH_DL

/* Define if using dld for dynamic linking of object files. */
#undef WITH_SHL

/* Define if using some method of dynamic linking. */
#undef WITH_DYNAMIC_LINKING

/* Define to `short' if <sys/types.h> doesn't define. */
#undef dev_t

/* Define to `unsigned long' if <sys/types.h> doesn't define. */
#undef ino_t

/* Define to `short' if <sys/types.h> doesn't define. */
#undef nlink_t

/* Define to `int' if <signal.h> doesn't define. */
#undef sigset_t


/* Leave that blank line there!!  Autoheader needs it.
   If you're adding to this file, keep in mind:
   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  */
