/* acconfig.h

   Descriptive text for the C preprocessor macros that are needed by
   Octave.

   Leave the following blank line there!!  Autoheader needs it.  */


/* Define if your math.h declares struct exception for matherr() */
#undef EXCEPTION_IN_MATH

/* Define if your Fortran compiler appends an underscore to external
   names. */ 
#undef F77_APPEND_UNDERSCORE

/* Define if your Fortran compiler converts external names to
   upper case. */
#undef F77_UPPERCASE_NAMES

/* Define if you don't have FSQP. */
#undef FSQP_MISSING

/* Define if your system has a single-arg prototype for gettimeofday. */
#undef GETTIMEOFDAY_NO_TZ 

/* Define if your gnuplot has mutliplot. */
#undef GNUPLOT_HAS_MULTIPLOT

/* Define if your system has a sys_siglist variable. */
#undef HAVE_SYS_SIGLIST

/* Define if you don't have NPSOL. */
#undef NPSOL_MISSING

/* Define to compile smaller kernel. */
#undef OCTAVE_LITE

/* Define if this is Octave. */
#undef OCTAVE_SOURCE

/* Define if you don't have QPSOL. */
#undef QPSOL_MISSING

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


/* Leave that blank line there!!  Autoheader needs it.
   If you're adding to this file, keep in mind:
   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  */
