/* Compiling snprintf.c with these two symbols defined will create an
   object with portable_snprintf and portable_vsnprintf defined.  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if ! defined HAVE_SNPRINTF
#define HAVE_SNPRINTF 1
#endif

#if ! defined PREFER_PORTABLE_SNPRINTF
#define PREFER_PORTABLE_SNPRINTF 1
#endif

#include "oct-snprintf.h"

#include "snprintf.c"
