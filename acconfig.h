/* acconfig.h

   Descriptive text for the C preprocessor macros that are needed by
   Octave.

   Leave the following blank line there!!  Autoheader needs it.  */


/* Define if your math.h declares struct exception for matherr() */
#undef EXCEPTION_IN_MATH

/* Define if your Fortran compiler appends an underscore to external
   names. */ 
#undef F77_APPEND_UNDERSCORE

/* Define if you don't have FSQP. */
#undef FSQP_MISSING

/* Define if you have finite.  */
#undef HAVE_FINITE

/* Define if you have isinf.  */
#undef HAVE_ISINF

/* Define if you have isnan.  */
#undef HAVE_ISNAN

/* Define if you have sgtty.h. */
#undef HAVE_SGTTY_H

/* Define if your system has a sys_siglist variable. */
#undef HAVE_SYS_SIGLIST

/* Define if you have termios.h. */
#undef HAVE_TERMIOS_H

/* Define if you have termio.h. */
#undef HAVE_TERMIO_H

/* Define if you don't have NPSOL. */
#undef NPSOL_MISSING

/* Define to compile smaller kernel.  Only works if some form of
   dynamic linking is also supported and used. */
#undef OCTAVE_LITE

/* Define if this is Octave. */
#undef OCTAVE_SOURCE

/* Define this to be the path separator for your system, as a
   character constant */
#undef SEPCHAR

/* Define this to be the path separator for your system, as a
   character string */
#undef SEPCHAR_STR

/* Define if you don't have QPSOL. */
#undef QPSOL_MISSING

/* Use GNU info for extended help system. */
#undef USE_GNU_INFO

/* Use GNU readline for command line editing and history. */
#undef USE_READLINE

/* Define if math.h declares signgam. */
#undef SIGNGAM_DECLARED

/* To quiet autoheader. */
#undef SMART_PUTENV

/* Define if using dld for dynamic linking of object files. */
#undef WITH_DLD


/* Leave that blank line there!!  Autoheader needs it.
   If you're adding to this file, keep in mind:
   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  */
