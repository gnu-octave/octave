/* pathsearch.c: look up a filename in a path.

Copyright (C) 1993, 94, 95, 96, 97, 98 Karl Berry.
Copyright (C) 1993, 94, 95, 96, 97 Karl Berry & O. Weber.
Copyright (C) 1992, 93, 94, 95, 96 Free Software Foundation, Inc.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#if defined (HAVE_CONFIG_H)
#include <config.h>
#endif

#include "kpse-config.h"
#include "kpse-xfns.h"
#include "kpse.h"

/* c-std.h: the first header files.  */

/* Header files that essentially all of our sources need, and
   that all implementations have.  We include these first, to help with
   NULL being defined multiple times.  */
#include <cstdio>
#include <cstdarg>
#include <cstdlib>
#include <cstring>
#include <climits>
#include <cerrno>
#include <cassert>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#ifdef WIN32
#include <malloc.h>
#endif /* not WIN32 */

#include "sysdir.h"
#include "statdefs.h"

/* define NAME_MAX, the maximum length of a single
   component in a filename.  No such limit may exist, or may vary
   depending on the filesystem.  */

/* Most likely the system will truncate filenames if it is not POSIX,
   and so we can use the BSD value here.  */
#ifndef _POSIX_NAME_MAX
#define _POSIX_NAME_MAX 255
#endif

#ifndef NAME_MAX
#define NAME_MAX _POSIX_NAME_MAX
#endif

/* c-ctype.h: ASCII-safe versions of the <ctype.h> macros.  */

#include <cctype>

/* What separates elements in environment variable path lists?  */
#ifndef ENV_SEP
#ifdef DOSISH
#define ENV_SEP ';'
#define ENV_SEP_STRING ";"
#else
#define ENV_SEP ':'
#define ENV_SEP_STRING ":"
#endif /* not DOS */
#endif /* not ENV_SEP */

#ifndef IS_ENV_SEP
#define IS_ENV_SEP(ch) ((ch) == ENV_SEP)
#endif

/* c-pathmx.h: define PATH_MAX, the maximum length of a filename.
   Since no such limit may exist, it's preferable to dynamically grow
   filenames as needed.  */

/* Cheat and define this as a manifest constant no matter what, instead
   of using pathconf.  I forget why we want to do this.  */

#ifndef _POSIX_PATH_MAX
#define _POSIX_PATH_MAX 255
#endif

#ifndef PATH_MAX
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else
#define PATH_MAX _POSIX_PATH_MAX
#endif
#endif /* not PATH_MAX */

/* debug.h: Runtime tracing.  */

/* If NO_DEBUG is defined (not recommended), skip all this.  */
#ifndef NO_DEBUG

/* OK, we'll have tracing support.  */
#define KPSE_DEBUG

/* Set a bit.  */
#define KPSE_DEBUG_SET(bit) kpathsea_debug |= 1 << (bit)

/* Test if a bit is on.  */
#define KPSE_DEBUG_P(bit) (kpathsea_debug & (1 << (bit)))

#define KPSE_DEBUG_STAT 0		/* stat calls */
#define KPSE_DEBUG_HASH 1		/* hash lookups */
#define KPSE_DEBUG_FOPEN 2		/* fopen/fclose calls */
#define KPSE_DEBUG_PATHS 3		/* search path initializations */
#define KPSE_DEBUG_EXPAND 4		/* path element expansion */
#define KPSE_DEBUG_SEARCH 5		/* searches */
#define KPSE_DEBUG_VARS 6		/* variable values */
#define KPSE_LAST_DEBUG KPSE_DEBUG_VARS

/* A printf for the debugging.  */
#define DEBUGF_START() do { fputs ("kdebug:", stderr)
#define DEBUGF_END()        fflush (stderr); } while (0)

#define DEBUGF(str)							\
  DEBUGF_START (); fputs (str, stderr); DEBUGF_END ()
#define DEBUGF1(str, e1)						\
  DEBUGF_START (); fprintf (stderr, str, e1); DEBUGF_END ()
#define DEBUGF2(str, e1, e2)						\
  DEBUGF_START (); fprintf (stderr, str, e1, e2); DEBUGF_END ()
#define DEBUGF3(str, e1, e2, e3)					\
  DEBUGF_START (); fprintf (stderr, str, e1, e2, e3); DEBUGF_END ()
#define DEBUGF4(str, e1, e2, e3, e4)					\
  DEBUGF_START (); fprintf (stderr, str, e1, e2, e3, e4); DEBUGF_END ()

#undef fopen
#define fopen kpse_fopen_trace
extern FILE *fopen (const char *filename, const char *mode);
#undef fclose
#define fclose kpse_fclose_trace
extern int fclose (FILE *);

#endif /* not NO_DEBUG */

#if defined (WIN32) && !defined (__MINGW32__)

/* System description file for Windows NT.  */

/*
 *      Define symbols to identify the version of Unix this is.
 *      Define all the symbols that apply correctly.
 */

#ifndef DOSISH
#define DOSISH
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN      _MAX_PATH
#endif

#define HAVE_DUP2       	1
#define HAVE_RENAME     	1
#define HAVE_RMDIR      	1
#define HAVE_MKDIR      	1
#define HAVE_GETHOSTNAME	1
#define HAVE_RANDOM		1
#define USE_UTIME		1
#define HAVE_MOUSE		1
#define HAVE_TZNAME		1

/* These have to be defined because our compilers treat __STDC__ as being
   defined (most of them anyway). */

#define access  _access
#define stat    _stat
#define strcasecmp _stricmp
#define strdup  _strdup
#define strncasecmp _strnicmp

#define S_IFMT   _S_IFMT
#define S_IFDIR  _S_IFDIR

/* Define this so that winsock.h definitions don't get included when
   windows.h is...  For this to have proper effect, config.h must
   always be included before windows.h.  */ 
#define _WINSOCKAPI_    1

#include <windows.h>

/* Defines size_t and alloca ().  */
#include <malloc.h>

/* For proper declaration of environ.  */
#include <io.h>
#include <fcntl.h>
#include <process.h>

/* ============================================================ */

#endif /* WIN32 */

/* hash.h: declarations for a hash table.  */

/* A single (key,value) pair.  */
typedef struct hash_element_struct
{
  const char *key;
  const char *value;
  struct hash_element_struct *next;
} hash_element_type;

/* The usual arrangement of buckets initialized to null.  */
typedef struct
{
  hash_element_type **buckets;
  unsigned size;
} hash_table_type;

static hash_table_type hash_create (unsigned size);


#ifdef KPSE_DEBUG
/* How to print the hash results when debugging.  */
extern int kpse_debug_hash_lookup_int;
#endif

/* fn.h: arbitrarily long filenames (or just strings).  */

/* Arbitrarily long filenames; it's inconvenient to use obstacks here,
   because we want to maintain a null terminator.  Also used for
   dynamically growing strings even when the null byte isn't necessary,
   e.g., in `variable.c', since I don't want to pass obstacks around
   everywhere, and one can't free parts of an obstack arbitrarily.  */

typedef struct
{
  char *str;
  unsigned allocated;
  unsigned length; /* includes the terminating null byte, if any */
} fn_type;

#define FN_STRING(fn) ((fn).str)
#define FN_ALLOCATED(fn) ((fn).allocated)
#define FN_LENGTH(fn) ((fn).length)

/* lib.h: other stuff.  */

/* Define common sorts of messages.  */

/* This should be called only after a system call fails.  Don't exit
   with status `errno', because that might be 256, which would mean
   success (exit statuses are truncated to eight bits).  */
#define FATAL_PERROR(str) do { \
  fputs ("pathsearch: ", stderr); \
  perror (str); exit (EXIT_FAILURE); } while (0)

#define START_FATAL() do { \
  fputs ("pathsearch: fatal: ", stderr);
#define END_FATAL() fputs (".\n", stderr); exit (1); } while (0)

#define FATAL(str)							\
  START_FATAL (); fputs (str, stderr); END_FATAL ()
#define FATAL1(str, e1)							\
  START_FATAL (); fprintf (stderr, str, e1); END_FATAL ()
#define FATAL2(str, e1, e2)						\
  START_FATAL (); fprintf (stderr, str, e1, e2); END_FATAL ()
#define FATAL3(str, e1, e2, e3)						\
  START_FATAL (); fprintf (stderr, str, e1, e2, e3); END_FATAL ()
#define FATAL4(str, e1, e2, e3, e4)					\
  START_FATAL (); fprintf (stderr, str, e1, e2, e3, e4); END_FATAL ()
#define FATAL5(str, e1, e2, e3, e4, e5)					\
  START_FATAL (); fprintf (stderr, str, e1, e2, e3, e4, e5); END_FATAL ()
#define FATAL6(str, e1, e2, e3, e4, e5, e6)				\
  START_FATAL (); fprintf (stderr, str, e1, e2, e3, e4, e5, e6); END_FATAL ()


#define START_WARNING() do { fputs ("warning: ", stderr)
#define END_WARNING() fputs (".\n", stderr); fflush (stderr); } while (0)

#define WARNING(str)							\
  START_WARNING (); fputs (str, stderr); END_WARNING ()
#define WARNING1(str, e1)						\
  START_WARNING (); fprintf (stderr, str, e1); END_WARNING ()
#define WARNING2(str, e1, e2)						\
  START_WARNING (); fprintf (stderr, str, e1, e2); END_WARNING ()
#define WARNING3(str, e1, e2, e3)					\
  START_WARNING (); fprintf (stderr, str, e1, e2, e3); END_WARNING ()
#define WARNING4(str, e1, e2, e3, e4)					\
  START_WARNING (); fprintf (stderr, str, e1, e2, e3, e4); END_WARNING ()


/* I find this easier to read.  */
#define STREQ(s1, s2) (strcmp (s1, s2) == 0)
#define STRNEQ(s1, s2, n) (strncmp (s1, s2, n) == 0)
      
/* Support for FAT/ISO-9660 filesystems.  Theoretically this should be
   done at runtime, per filesystem, but that's painful to program.  */
#ifdef MONOCASE_FILENAMES
#define FILESTRCASEEQ(s1, s2) (strcasecmp (s1, s2) == 0)
#define FILESTRNCASEEQ(s1, s2, l) (strncasecmp (s1, s2, l) == 0)
#define FILECHARCASEEQ(c1, c2) (toupper (c1) == toupper (c2))
#else
#define FILESTRCASEEQ STREQ
#define FILESTRNCASEEQ STRNEQ
#define FILECHARCASEEQ(c1, c2) ((c1) == (c2))
#endif

/* This is the maximum number of numerals that result when a 64-bit
   integer is converted to a string, plus one for a trailing null byte,
   plus one for a sign.  */
#define MAX_INT_LENGTH 21

/* If the environment variable TEST is set, return it; otherwise,
   DEFAULT.  This is useful for paths that use more than one envvar.  */
#define ENVVAR(test, default) (getenv (test) ? (test) : (default))


/* (Re)Allocate N items of type T using xmalloc/xrealloc.  */
#define XTALLOC(n, t) ((t *) xmalloc ((n) * sizeof (t)))
#define XTALLOC1(t) XTALLOC (1, t)
#define XRETALLOC(addr, n, t) ((addr) = (t *) xrealloc (addr, (n) * sizeof(t)))

extern "C" char *xbasename (const char *name);

static FILE *xfopen (const char *filename, const char *mode);

static void xfclose (FILE *f, const char *filename);

unsigned long xftell (FILE *f, char *filename);

#ifndef WIN32
static void xclosedir (DIR *d);
#endif

static void *xmalloc (unsigned size);

static void *xrealloc (void *old_ptr, unsigned size);

static char *xstrdup (const char *s);

extern char *xbasename (const char *name);

static int dir_p (const char *fn);

#ifndef WIN32
int dir_links (const char *fn);
#endif

static unsigned hash (hash_table_type table, const char *key);

static void hash_insert (hash_table_type *table, const char *key,
			 const char *value);

static char **hash_lookup (hash_table_type table, const char *key);

static void hash_print (hash_table_type table, int summary_only);

static char *concat (const char *s1, const char *s2);

static char *concat3 (const char *s1, const char *s2, const char *s3);

static char *concatn (const char *str1, ...);

static char *find_suffix (const char *name);

static char *kpse_truncate_filename (const char *name);

static char *kpse_readable_file (const char *name);

static int kpse_absolute_p (const char *filename, int relative_ok);

static str_list_type str_list_init (void);

static void str_list_add (str_list_type *l, char *s);

static void str_list_concat (str_list_type *target, str_list_type more);

static void str_list_free (str_list_type *l);

static void str_llist_add (str_llist_type *l, char *str);

static void str_llist_float (str_llist_type *l, str_llist_elt_type *mover);

static fn_type fn_init (void);

static fn_type fn_copy0 (const char *s, unsigned len);

static void fn_free (fn_type *f);

static void grow (fn_type *f, unsigned len);

static void fn_1grow (fn_type *f, char c);

static void fn_grow (fn_type *f, void *source, unsigned len);

static void fn_str_grow (fn_type *f, const char *s);

static void fn_shrink_to (fn_type *f, unsigned loc);

static char *kpse_var_value (const char *var);

static void expanding (const char *var, int xp);

static int expanding_p (const char *var);

static void expand (fn_type *expansion, const char *start, const char *end);

static char *kpse_var_expand (const char *src);

#include <ctime> /* for `time' */

/* The very first search is for texmf.cnf, called when someone tries to
   initialize the TFM path or whatever.  init_path calls kpse_cnf_get
   which calls kpse_all_path_search to find all the texmf.cnf's.  We
   need to do various special things in this case, since we obviously
   don't yet have the configuration files when we're searching for the
   configuration files.  */
static bool first_search = true;



/* This function is called after every search (except the first, since
   we definitely want to allow enabling the logging in texmf.cnf) to
   record the filename(s) found in $TEXMFLOG.  */

static void
log_search (str_list_type filenames)
{
  static FILE *log_file = NULL;
  static bool first_time = true; /* Need to open the log file?  */
  
  if (first_time) {
    /* Get name from either envvar or config file.  */
    char *log_name = kpse_var_value ("TEXMFLOG");
    first_time = false;
    if (log_name) {
      log_file = xfopen (log_name, "a");
      if (!log_file)
        perror (log_name);
      free (log_name);
    }
  }

  if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH) || log_file) {
    unsigned e;

    /* FILENAMES should never be null, but safety doesn't hurt.  */
    for (e = 0; e < STR_LIST_LENGTH (filenames) && STR_LIST_ELT (filenames, e);
         e++) {
      char *filename = STR_LIST_ELT (filenames, e);

      /* Only record absolute filenames, for privacy.  */
      if (log_file && kpse_absolute_p (filename, false))
        fprintf (log_file, "%lu %s\n", (long unsigned) time (NULL),
                 filename);

      /* And show them online, if debugging.  We've already started
         the debugging line in `search', where this is called, so
         just print the filename here, don't use DEBUGF.  */
      if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
        fputs (filename, stderr);
    }
  }
}

/* Concatenate each element in DIRS with NAME (assume each ends with a
   /, to save time).  If SEARCH_ALL is false, return the first readable
   regular file.  Else continue to search for more.  In any case, if
   none, return a list containing just NULL.

   We keep a single buffer for the potential filenames and reallocate
   only when necessary.  I'm not sure it's noticeably faster, but it
   does seem cleaner.  (We do waste a bit of space in the return
   value, though, since we don't shrink it to the final size returned.)  */

#define INIT_ALLOC 75  /* Doesn't much matter what this number is.  */

static str_list_type
dir_list_search (str_llist_type *dirs, const char *name, bool search_all)
{
  str_llist_elt_type *elt;
  str_list_type ret;
  unsigned name_len = strlen (name);
  unsigned allocated = INIT_ALLOC;
  char *potential = (char *) xmalloc (allocated);

  ret = str_list_init ();
  
  for (elt = *dirs; elt; elt = STR_LLIST_NEXT (*elt))
    {
      const char *dir = STR_LLIST (*elt);
      unsigned dir_len = strlen (dir);
      
      while (dir_len + name_len + 1 > allocated)
        {
          allocated += allocated;
          XRETALLOC (potential, allocated, char);
        }
      
      strcpy (potential, dir);
      strcat (potential, name);
      
      if (kpse_readable_file (potential))
        { 
          str_list_add (&ret, potential);
          
          /* Move this element towards the top of the list.  */
          str_llist_float (dirs, elt);
          
          /* If caller only wanted one file returned, no need to
             terminate the list with NULL; the caller knows to only look
             at the first element.  */
          if (!search_all)
            return ret;

          /* Start new filename.  */
          allocated = INIT_ALLOC;
          potential = (char *) xmalloc (allocated);
        }
    }
  
  /* If we get here, either we didn't find any files, or we were finding
     all the files.  But we're done with the last filename, anyway.  */
  free (potential);
  
  return ret;
}

/* This is called when NAME is absolute or explicitly relative; if it's
   readable, return (a list containing) it; otherwise, return NULL.  */

static str_list_type
absolute_search (char *name)
{
  str_list_type ret_list;
  char *found = kpse_readable_file (name);
  
  /* Some old compilers can't initialize structs.  */
  ret_list = str_list_init ();

  /* If NAME wasn't found, free the expansion.  */
  if (name != found)
    free (name);

  /* Add `found' to the return list even if it's null; that tells
     the caller we didn't find anything.  */
  str_list_add (&ret_list, found);
  
  return ret_list;
}

/* This is the hard case -- look for NAME in PATH.  If ALL is false,
   return the first file found.  Otherwise, search all elements of PATH.  */

static str_list_type
path_search (const char *path, char *name, bool must_exist, bool all)
{
  char *elt;
  str_list_type ret_list;
  bool done = false;
  ret_list = str_list_init (); /* some compilers lack struct initialization */

  for (elt = kpse_path_element (path); !done && elt;
       elt = kpse_path_element (NULL)) {
    str_list_type *found;
    bool allow_disk_search = true;

    if (*elt == '!' && *(elt + 1) == '!') {
      /* Those magic leading chars in a path element means don't search the
         disk for this elt.  And move past the magic to get to the name.  */
      allow_disk_search = false;
      elt += 2;
    }

    /* Do not touch the device if present */
    if (NAME_BEGINS_WITH_DEVICE (elt)) {
      while (IS_DIR_SEP (*(elt + 2)) && IS_DIR_SEP (*(elt + 3))) {
	*(elt + 2) = *(elt + 1);
	*(elt + 1) = *elt;
	elt++;
      }
    } else {
      /* We never want to search the whole disk.  */
      while (IS_DIR_SEP (*elt) && IS_DIR_SEP (*(elt + 1)))
        elt++;
    }
    
    /* Try ls-R, unless we're searching for texmf.cnf.  Our caller
       (search), also tests first_search, and does the resetting.  */
    found = first_search ? NULL : kpse_db_search (name, elt, all);

    /* Search the filesystem if (1) the path spec allows it, and either
         (2a) we are searching for texmf.cnf ; or
         (2b) no db exists; or 
         (2c) no db's are relevant to this elt; or
         (3) MUST_EXIST && NAME was not in the db.
       In (2*), `found' will be NULL.
       In (3),  `found' will be an empty list. */
    if (allow_disk_search && (!found || (must_exist && !STR_LIST (*found)))) {
      str_llist_type *dirs = kpse_element_dirs (elt);
      if (dirs && *dirs) {
        if (!found)
          found = XTALLOC1 (str_list_type);
        *found = dir_list_search (dirs, name, all);
      }
    }

    /* Did we find anything anywhere?  */
    if (found && STR_LIST (*found))
      if (all)
        str_list_concat (&ret_list, *found);
      else {
        str_list_add (&ret_list, STR_LIST_ELT (*found, 0));
        done = true;
      }

    /* Free the list space, if any (but not the elements).  */
    if (found) {
      str_list_free (found);
      free (found);
    }
  }

  /* Free the expanded name we were passed.  It can't be in the return
     list, since the path directories got unconditionally prepended.  */
  free (name);
  
  return ret_list;
}      

/* Search PATH for ORIGINAL_NAME.  If ALL is false, or ORIGINAL_NAME is
   absolute_p, check ORIGINAL_NAME itself.  Otherwise, look at each
   element of PATH for the first readable ORIGINAL_NAME.
   
   Always return a list; if no files are found, the list will
   contain just NULL.  If ALL is true, the list will be
   terminated with NULL.  */

static char **
search (const char *path, const char *original_name,
	bool must_exist, bool all)
{
  str_list_type ret_list;
  char *name;
  bool absolute_p;

  /* Make a leading ~ count as an absolute filename, and expand $FOO's.  */
  name = kpse_expand (original_name);
  
  /* If the first name is absolute or explicitly relative, no need to
     consider PATH at all.  */
  absolute_p = kpse_absolute_p (name, true);
  
  if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
    DEBUGF4 ("start search(file=%s, must_exist=%d, find_all=%d, path=%s).\n",
             name, must_exist, all, path);

  /* Find the file(s). */
  ret_list = absolute_p ? absolute_search (name)
                        : path_search (path, name, must_exist, all);
  
  /* Append NULL terminator if we didn't find anything at all, or we're
     supposed to find ALL and the list doesn't end in NULL now.  */
  if (STR_LIST_LENGTH (ret_list) == 0
      || (all && STR_LIST_LAST_ELT (ret_list) != NULL))
    str_list_add (&ret_list, NULL);

  /* The very first search is for texmf.cnf.  We can't log that, since
     we want to allow setting TEXMFLOG in texmf.cnf.  */
  if (first_search) {
    first_search = false;
  } else {
    /* Record the filenames we found, if desired.  And wrap them in a
       debugging line if we're doing that.  */
    if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
      DEBUGF1 ("search(%s) =>", original_name);
    log_search (ret_list);
    if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
      putc ('\n', stderr);
  }  

  return STR_LIST (ret_list);
}

/* Search PATH for the first NAME.  */

char *
kpse_path_search (const char *path, const char *name, bool must_exist)
{
  static char **ret_list = 0;

  if (ret_list)
    {
      free (ret_list);
      ret_list = 0;  /* Don't let an interrupt in search() cause trouble */
    }

  ret_list = search (path, name, must_exist, false);

  return *ret_list;  /* Freeing this is caller's responsibility */
}


/* Search all elements of PATH for files named NAME.  Not sure if it's
   right to assert `must_exist' here, but it suffices now.  */

char **
kpse_all_path_search (const char *path, const char *name)
{
  char **ret = search (path, name, true, true);
  return ret;
}

/* This is the hard case -- look in each element of PATH for each
   element of NAMES.  If ALL is false, return the first file found.
   Otherwise, search all elements of PATH.  */

static str_list_type
path_find_first_of (const char *path, const char **names,
		    bool must_exist, bool all)
{
  const char **p;
  char *elt;
  const char *name;
  str_list_type ret_list;
  bool done = false;
  ret_list = str_list_init (); /* some compilers lack struct initialization */

  for (elt = kpse_path_element (path); !done && elt;
       elt = kpse_path_element (NULL))
    {
      str_llist_type *dirs;
      str_llist_elt_type *dirs_elt;
      str_list_type *found;
      bool allow_disk_search = true;

      if (*elt == '!' && *(elt + 1) == '!')
	{
	  /* Those magic leading chars in a path element means don't
	     search the disk for this elt.  And move past the magic to
	     get to the name.  */

	  allow_disk_search = false;
	  elt += 2;
	}

      /* Do not touch the device if present */

      if (NAME_BEGINS_WITH_DEVICE (elt))
	{
	  while (IS_DIR_SEP (*(elt + 2)) && IS_DIR_SEP (*(elt + 3)))
	    {
	      *(elt + 2) = *(elt + 1);
	      *(elt + 1) = *elt;
	      elt++;
	    }
	}
      else
	{
	  /* We never want to search the whole disk.  */
	  while (IS_DIR_SEP (*elt) && IS_DIR_SEP (*(elt + 1)))
	    elt++;
	}

      /* We have to search one directory at a time.  */
      dirs = kpse_element_dirs (elt);
      for (dirs_elt = *dirs; dirs_elt; dirs_elt = STR_LLIST_NEXT (*dirs_elt))
	{
	  char *dir = STR_LLIST (*dirs_elt);

	  for (p = names; !done && *p; p++)
	    {
	      name = *p;

	      /* Try ls-R, unless we're searching for texmf.cnf.  Our caller
		 (find_first_of), also tests first_search, and does the
		 resetting.  */
	      found = first_search ? NULL : kpse_db_search (name, dir, all);

	      /* Search the filesystem if (1) the path spec allows it,
		 and either

		   (2a) we are searching for texmf.cnf ; or
		   (2b) no db exists; or 
		   (2c) no db's are relevant to this elt; or
		   (3) MUST_EXIST && NAME was not in the db.

		 In (2*), `found' will be NULL.
		 In (3),  `found' will be an empty list. */

	      if (allow_disk_search
		  && (!found || (must_exist && !STR_LIST (*found))))
		{
		  static str_llist_type *tmp = 0;

		  if (! tmp)
		    {
		      tmp = XTALLOC1 (str_llist_type);
		      *tmp = NULL;
		      str_llist_add (tmp, "");
		    }

		  STR_LLIST (*(*tmp)) = dir;

		  if (!found)
		    found = XTALLOC1 (str_list_type);

		  *found = dir_list_search (tmp, name, all);
		}

	      /* Did we find anything anywhere?  */
	      if (found && STR_LIST (*found))
		{
		  if (all)
		    str_list_concat (&ret_list, *found);
		  else
		    {
		      str_list_add (&ret_list, STR_LIST_ELT (*found, 0));
		      done = true;
		    }
		}

	      /* Free the list space, if any (but not the elements).  */
	      if (found)
		{
		  str_list_free (found);
		  free (found);
		}
	    }
	}
    }

  return ret_list;
}      

static char **
find_first_of (const char *path, const char **names,
	       bool must_exist, bool all)
{
  str_list_type ret_list;

  if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
    {
      const char **p;
      fputs ("start find_first_of((", stderr);
      for (p = names; *p; p++)
	{
	  if (p == names)
	    fputs (*p, stderr);
	  else
	    fprintf (stderr, ", %s", *p);
	}
      fprintf (stderr, "), path=%s, must_exist=%d).\n", path, must_exist);
    }

  /* Find the file. */
  ret_list = path_find_first_of (path, names, must_exist, all);

  /* Append NULL terminator if we didn't find anything at all, or we're
     supposed to find ALL and the list doesn't end in NULL now.  */
  if (STR_LIST_LENGTH (ret_list) == 0
      || (all && STR_LIST_LAST_ELT (ret_list) != NULL))
    str_list_add (&ret_list, NULL);

  /* The very first search is for texmf.cnf.  We can't log that, since
     we want to allow setting TEXMFLOG in texmf.cnf.  */
  if (first_search) {
    first_search = false;
  } else {
    /* Record the filenames we found, if desired.  And wrap them in a
       debugging line if we're doing that.  */
    if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
      {
	const char **p;
	fputs ("find_first_of(", stderr);
	for (p = names; *p; p++)
	  {
	    if (p == names)
	      fputs (*p, stderr);
	    else
	      fprintf (stderr, ", %s", *p);
	  }
	fputs (") =>", stderr);
      }
    log_search (ret_list);
    if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
      putc ('\n', stderr);
  }  

  return STR_LIST (ret_list);
}

/* Search each element of PATH for each element of NAMES.  Return the
   first one found.  */

char *
kpse_path_find_first_of (const char *path, const char **names,
			 bool must_exist)
{
  static char **ret_list = 0;

  if (ret_list)
    {
      free (ret_list);
      ret_list = 0;  /* Don't let an interrupt in search() cause trouble */
    }

  ret_list = find_first_of (path, names, must_exist, false);

  return *ret_list;  /* Freeing this is caller's responsibility */
}

/* Search each element of PATH for each element of NAMES and return a
   list containing everything found, in the order found.  */

char **
kpse_all_path_find_first_of (const char *path, const char **names)
{
  char **ret = find_first_of (path, names, true, true);
  return ret;
}

/* expand.c: general expansion.  Some of this file (the brace-expansion
   code from bash) is covered by the GPL; this is the only GPL-covered
   code in kpathsea.  The part of the file that I wrote (the first
   couple of functions) is covered by the LGPL.  */

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

/* If NAME has a leading ~ or ~user, Unix-style, expand it to the user's
   home directory, and return a new malloced string.  If no ~, or no
   <pwd.h>, just return NAME.  */

static char *
kpse_tilde_expand (const char *name)
{
  const char *expansion;
  const char *home;
  
  assert (name);
  
  /* If no leading tilde, do nothing.  */
  if (*name != '~') {
    expansion = name;
  
  /* If a bare tilde, return the home directory or `.'.  (Very unlikely
     that the directory name will do anyone any good, but ...  */
  } else if (name[1] == 0) {
    expansion = xstrdup (getenv ("HOME"));
    if (!expansion) {
      expansion = xstrdup (".");
    }
  
  /* If `~/', remove any trailing / or replace leading // in $HOME.
     Should really check for doubled intermediate slashes, too.  */
  } else if (IS_DIR_SEP (name[1])) {
    unsigned c = 1;
    home = getenv ("HOME");
    if (!home) {
      home = ".";
    }
    if (IS_DIR_SEP (*home) && IS_DIR_SEP (home[1])) {  /* handle leading // */
      home++;
    }
    if (IS_DIR_SEP (home[strlen (home) - 1])) {        /* omit / after ~ */
      c++;
    }
    expansion = concat (home, name + c);
  
  /* If `~user' or `~user/', look up user in the passwd database (but
     OS/2 doesn't have this concept.  */
  } else
#ifdef HAVE_PWD_H
    {
      struct passwd *p;
      char *user;
      unsigned c = 2;
      while (!IS_DIR_SEP (name[c]) && name[c] != 0) /* find user name */
        c++;
      
      user = (char *) xmalloc (c);
      strncpy (user, name + 1, c - 1);
      user[c - 1] = 0;
      
      /* We only need the cast here for (deficient) systems
         which do not declare `getpwnam' in <pwd.h>.  */
      p = (struct passwd *) getpwnam (user);
      free (user);

      /* If no such user, just use `.'.  */
      home = p ? p->pw_dir : ".";
      if (IS_DIR_SEP (*home) && IS_DIR_SEP (home[1])) { /* handle leading // */
        home++;
      }
      if (IS_DIR_SEP (home[strlen (home) - 1]) && name[c] != 0)
        c++; /* If HOME ends in /, omit the / after ~user. */

      expansion = name[c] == 0 ? xstrdup (home) : concat (home, name + c);
    }
#else /* not HAVE_PWD_H */
    expansion = name;
#endif /* not HAVE_PWD_H */

  /* We may return the same thing as the original, and then we might not
     be returning a malloc-ed string.  Callers beware.  Sorry.  */
  return (char *) expansion;
}

/* Do variable expansion first so ~${USER} works.  (Besides, it's what the
   shells do.)  */

char *
kpse_expand (const char *s)
{
  char *var_expansion = kpse_var_expand (s);
  char *tilde_expansion = kpse_tilde_expand (var_expansion);
  
  /* `kpse_var_expand' always gives us new memory; `kpse_tilde_expand'
     doesn't, necessarily.  So be careful that we don't free what we are
     about to return.  */
  if (tilde_expansion != var_expansion)
    free (var_expansion);
  
  return tilde_expansion;
}


/* Forward declarations of functions from the original expand.c  */
static char **brace_expand (const char *);
static void free_array (char **);

/* If $KPSE_DOT is defined in the environment, prepend it to any relative
   path components. */

static char *
kpse_expand_kpse_dot (char *path)
{
  char *ret, *elt;
  char *kpse_dot = getenv("KPSE_DOT");
#ifdef MSDOS
  bool malloced_kpse_dot = false;
#endif
  
  if (kpse_dot == NULL)
    return path;
  ret = (char *) xmalloc(1);
  *ret = 0;

#ifdef MSDOS
  /* Some setups of ported Bash force $KPSE_DOT to have the //d/foo/bar
     form (when `pwd' is used), which is not understood by libc and the OS.
     Convert them back to the usual d:/foo/bar form.  */
  if (kpse_dot[0] == '/' && kpse_dot[1] == '/'
      && kpse_dot[2] >= 'A' && kpse_dot[2] <= 'z' && kpse_dot[3] == '/') {
    kpse_dot++;
    kpse_dot = xstrdup (kpse_dot);
    kpse_dot[0] = kpse_dot[1];  /* drive letter */
    kpse_dot[1] = ':';
    malloced_kpse_dot = true;
  }
#endif

  for (elt = kpse_path_element (path); elt; elt = kpse_path_element (NULL)) {
    char *save_ret = ret;
    /* We assume that the !! magic is only used on absolute components.
       Single "." get special treatment, as does "./" or its equivalent. */
    if (kpse_absolute_p (elt, false) || (elt[0] == '!' && elt[1] == '!')) {
      ret = concat3(ret, elt, ENV_SEP_STRING);
    } else if (elt[0] == '.' && elt[1] == 0) {
      ret = concat3 (ret, kpse_dot, ENV_SEP_STRING);
    } else if (elt[0] == '.' && IS_DIR_SEP(elt[1])) {
      ret = concatn (ret, kpse_dot, elt + 1, ENV_SEP_STRING, NULL);
    } else {
      ret = concatn (ret, kpse_dot, DIR_SEP_STRING, elt, ENV_SEP_STRING, NULL);
    }
    free (save_ret);
  }

#ifdef MSDOS
  if (malloced_kpse_dot) free (kpse_dot);
#endif

  ret[strlen (ret) - 1] = 0;
  return ret;
}

/* Do brace expansion on ELT; then do variable and ~ expansion on each
   element of the result; then do brace expansion again, in case a
   variable definition contained braces (e.g., $TEXMF).  Return a
   string comprising all of the results separated by ENV_SEP_STRING.  */

static char *
kpse_brace_expand_element (const char *elt)
{
  unsigned i;
  char **expansions = brace_expand (elt);
  char *ret = (char *) xmalloc (1);
  *ret = 0;

  for (i = 0; expansions[i]; i++) {
    /* Do $ and ~ expansion on each element.  */
    char *x = kpse_expand (expansions[i]);
    char *save_ret = ret;
    if (!STREQ (x, expansions[i])) {
      /* If we did any expansions, do brace expansion again.  Since
         recursive variable definitions are not allowed, this recursion
         must terminate.  (In practice, it's unlikely there will ever be
         more than one level of recursion.)  */
      char *save_x = x;
      x = kpse_brace_expand_element (x);
      free (save_x);
    }
    ret = concat3 (ret, x, ENV_SEP_STRING);
    free (save_ret);
    free (x);
  }

  free_array (expansions);
  ret[strlen (ret) - 1] = 0; /* waste the trailing null */
  return ret;
}

/* Be careful to not waste all the memory we allocate for each element.  */

char *
kpse_brace_expand (const char *path)
{
  char *kpse_dot_expansion;
  char *elt;
  unsigned len;
  /* Must do variable expansion first because if we have
       foo = .:~
       TEXINPUTS = $foo
     we want to end up with TEXINPUTS = .:/home/karl.
     Since kpse_path_element is not reentrant, we must get all
     the path elements before we start the loop.  */
  char *xpath = kpse_var_expand (path);
  char *ret = (char *) xmalloc (1);
  *ret = 0;

  for (elt = kpse_path_element (xpath); elt; elt = kpse_path_element (NULL)) {
    char *save_ret = ret;
    /* Do brace expansion first, so tilde expansion happens in {~ka,~kb}.  */
    char *expansion = kpse_brace_expand_element (elt);
    ret = concat3 (ret, expansion, ENV_SEP_STRING);
    free (expansion);
    free (save_ret);
  }

  /* Waste the last byte by overwriting the trailing env_sep with a null.  */
  len = strlen (ret);
  if (len != 0)
    ret[len - 1] = 0;
  free (xpath);

  kpse_dot_expansion = kpse_expand_kpse_dot (ret);
  if (kpse_dot_expansion != ret)
    free (ret);

  return kpse_dot_expansion;
}

/* Expand all special constructs in a path, and include only the actually
   existing directories in the result. */
char *
kpse_path_expand (const char *path)
{
  char *ret;
  char *xpath;
  char *elt;
  unsigned len;

  /* Initialise ret to the empty string. */
  ret = (char *) xmalloc (1);
  *ret = 0;
  len = 0;
  
  /* Expand variables and braces first.  */
  xpath = kpse_brace_expand (path);

  /* Now expand each of the path elements, printing the results */
  for (elt = kpse_path_element (xpath); elt; elt = kpse_path_element (NULL)) {
    str_llist_type *dirs;

    /* Skip and ignore magic leading chars.  */
    if (*elt == '!' && *(elt + 1) == '!')
      elt += 2;

    /* Do not touch the device if present */
    if (NAME_BEGINS_WITH_DEVICE (elt)) {
      while (IS_DIR_SEP (*(elt + 2)) && IS_DIR_SEP (*(elt + 3))) {
        *(elt + 2) = *(elt + 1);
        *(elt + 1) = *elt;
        elt++;
      }
    } else {
      /* We never want to search the whole disk.  */
      while (IS_DIR_SEP (*elt) && IS_DIR_SEP (*(elt + 1)))
        elt++;
    }

    /* Search the disk for all dirs in the component specified.
       Be faster to check the database, but this is more reliable.  */
    dirs = kpse_element_dirs (elt); 
    if (dirs && *dirs) {
      str_llist_elt_type *dir;

      for (dir = *dirs; dir; dir = STR_LLIST_NEXT (*dir)) {
        char *thedir = STR_LLIST (*dir);
        unsigned dirlen = strlen (thedir);
        char *save_ret = ret;
        /* Retain trailing slash if that's the root directory.  */
        if (dirlen == 1 || (dirlen == 3 && NAME_BEGINS_WITH_DEVICE (thedir)
                            && IS_DIR_SEP (thedir[2]))) {
          ret = concat3 (ret, thedir, ENV_SEP_STRING);
          len += dirlen + 1;
          ret[len - 1] = ENV_SEP;
        } else {
          ret = concat (ret, thedir);
          len += dirlen;
          ret [len - 1] = ENV_SEP;
        }
        free (save_ret);
      }
    }
  }
  /* Get rid of trailing ':', if any. */
  if (len != 0)
    ret[len - 1] = 0;
  return ret;
}

/* braces.c -- code for doing word expansion in curly braces. Taken from
   bash 1.14.5.  [Ans subsequently modified for kpatshea.]

   Copyright (C) 1987,1991 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; see the file COPYING.  If not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.  */


#define brace_whitespace(c) (!(c) || (c) == ' ' || (c) == '\t' || (c) == '\n')
#define savestring xstrdup

/* Basic idea:

   Segregate the text into 3 sections: preamble (stuff before an open brace),
   postamble (stuff after the matching close brace) and amble (stuff after
   preamble, and before postamble).  Expand amble, and then tack on the
   expansions to preamble.  Expand postamble, and tack on the expansions to
   the result so far.
 */

/* The character which is used to separate arguments. */
static int brace_arg_separator = ',';

static int brace_gobbler (const char *, int *, int);
static char **expand_amble (const char *),
            **array_concat (char **, char **);

/* Return the length of ARRAY, a NULL terminated array of char *. */
static int
array_len (char **array)
{
  register int i;
  for (i = 0; array[i]; i++);
  return (i);
}

/* Free the contents of ARRAY, a NULL terminated array of char *. */
static void
free_array (char **array)
{
  register int i = 0;

  if (!array) return;

  while (array[i])
    free (array[i++]);
  free (array);
}

/* Allocate and return a new copy of ARRAY and its contents. */
static char **
copy_array (char **array)
{
  register int i;
  int len;
  char **new_array;

  len = array_len (array);

  new_array = (char **)xmalloc ((len + 1) * sizeof (char *));
  for (i = 0; array[i]; i++)
    new_array[i] = savestring (array[i]);
  new_array[i] = (char *)NULL;

  return (new_array);
}


/* Return an array of strings; the brace expansion of TEXT. */
static char **
brace_expand (const char *text)
{
  register int start;
  char *preamble, *amble;
  const char *postamble;
  char **tack, **result;
  int i, c;

  /* Find the text of the preamble. */
  i = 0;
  c = brace_gobbler (text, &i, '{');

  preamble = (char *) xmalloc (i + 1);
  strncpy (preamble, text, i);
  preamble[i] = 0;

  result = (char **) xmalloc (2 * sizeof (char *));
  result[0] = preamble;
  result[1] = NULL;
  
  /* Special case.  If we never found an exciting character, then
     the preamble is all of the text, so just return that. */
  if (c != '{')
    return (result);

  /* Find the amble.  This is the stuff inside this set of braces. */
  start = ++i;
  c = brace_gobbler (text, &i, '}');

  /* What if there isn't a matching close brace? */
  if (!c)
    {
      WARNING1 ("%s: Unmatched {", text);
      free (preamble);		/* Same as result[0]; see initialization. */
      result[0] = savestring (text);
      return (result);
    }

  amble = (char *) xmalloc (1 + (i - start));
  strncpy (amble, &text[start], (i - start));
  amble[i - start] = 0;

  postamble = &text[i + 1];

  tack = expand_amble (amble);
  result = array_concat (result, tack);
  free (amble);
  free_array (tack);

  tack = brace_expand (postamble);
  result = array_concat (result, tack);
  free_array (tack);

  return (result);
}


/* Expand the text found inside of braces.  We simply try to split the
   text at BRACE_ARG_SEPARATORs into separate strings.  We then brace
   expand each slot which needs it, until there are no more slots which
   need it. */
static char **
expand_amble (const char *text)
{
  char **result, **partial;
  char *tem;
  int start, i, c;

  result = NULL;

  for (start = 0, i = 0, c = 1; c; start = ++i)
    {
      int c0, c1;
      int i0, i1;
      i0 = i;
      c0 = brace_gobbler (text, &i0, brace_arg_separator);
      i1 = i;
      c1 = brace_gobbler (text, &i1, ENV_SEP);
      c = c0 | c1;
      i = (i0 < i1 ? i0 : i1);

      tem = (char *) xmalloc (1 + (i - start));
      strncpy (tem, &text[start], (i - start));
      tem[i- start] = 0;

      partial = brace_expand (tem);

      if (!result)
	result = partial;
      else
	{
	  register int lr = array_len (result);
	  register int lp = array_len (partial);
	  register int j;

	  result = (char **) xrealloc (result, (1 + lp + lr) * sizeof (char *));

	  for (j = 0; j < lp; j++)
	    result[lr + j] = partial[j];

	  result[lr + j] = NULL;
	  free (partial);
	}
      free (tem);
    }
  return (result);
}

/* Return a new array of strings which is the result of appending each
   string in ARR2 to each string in ARR1.  The resultant array is
   len (arr1) * len (arr2) long.  For convenience, ARR1 (and its contents)
   are free ()'ed.  ARR1 can be NULL, in that case, a new version of ARR2
   is returned. */
static char **
array_concat (char **arr1, char **arr2)
{
  register int i, j, len, len1, len2;
  register char **result;

  if (!arr1)
    return (copy_array (arr2));

  if (!arr2)
    return (copy_array (arr1));

  len1 = array_len (arr1);
  len2 = array_len (arr2);

  result = (char **) xmalloc ((1 + (len1 * len2)) * sizeof (char *));

  len = 0;
  for (i = 0; i < len2; i++)
    {
      int strlen_2 = strlen (arr2[i]);

      for (j = 0; j < len1; j++)
	{
	  int strlen_1 = strlen (arr1[j]);

	  result[len] = (char *) xmalloc (1 + strlen_1 + strlen_2);
	  strcpy (result[len], arr1[j]);
	  strcpy (result[len] + strlen_1, arr2[i]);
	  len++;
	}
    }
  free_array (arr1);

  result[len] = NULL;
  return (result);
}

/* Start at INDEX, and skip characters in TEXT. Set INDEX to the
   index of the character matching SATISFY.  This understands about
   quoting.  Return the character that caused us to stop searching;
   this is either the same as SATISFY, or 0. */
static int
brace_gobbler (const char *text, int *indx, int satisfy)
{
  register int i, c, quoted, level, pass_next;

  level = quoted = pass_next = 0;

  for (i = *indx; (c = text[i]); i++)
    {
      if (pass_next)
	{
	  pass_next = 0;
	  continue;
	}

      /* A backslash escapes the next character.  This allows backslash to
	 escape the quote character in a double-quoted string. */
      if (c == '\\' && (quoted == 0 || quoted == '"' || quoted == '`'))
        {
          pass_next = 1;
          continue;
        }

      if (quoted)
	{
	  if (c == quoted)
	    quoted = 0;
	  continue;
	}

      if (c == '"' || c == '\'' || c == '`')
	{
	  quoted = c;
	  continue;
	}
      
      if (c == satisfy && !level && !quoted)
	{
	  /* We ignore an open brace surrounded by whitespace, and also
	     an open brace followed immediately by a close brace, that
	     was preceded with whitespace.  */
	  if (c == '{' &&
	      ((!i || brace_whitespace (text[i - 1])) &&
	       (brace_whitespace (text[i + 1]) || text[i + 1] == '}')))
	    continue;
	  /* If this is being compiled as part of bash, ignore the `{'
	     in a `${}' construct */
	  if ((c != '{') || !i || (text[i - 1] != '$'))
	    break;
	}

      if (c == '{')
	level++;
      else if (c == '}' && level)
	level--;
    }

  *indx = i;
  return (c);
}

/* db.c: an external database to avoid filesystem lookups.  */

#ifndef DEFAULT_TEXMFDBS
#define DEFAULT_TEXMFDBS "/usr/local/share/texmf:/var/tmp/texfonts"
#endif

/* Perhaps we could use this for path values themselves; for now, we use
   it only for the program_enabled_p value.  */
typedef enum
{
  kpse_src_implicit,   /* C initialization to zero */
  kpse_src_compile,    /* configure/compile-time default */
  kpse_src_texmf_cnf,  /* texmf.cnf, the kpathsea config file */
  kpse_src_client_cnf, /* application config file, e.g., config.ps */
  kpse_src_env,        /* environment variable */
  kpse_src_x,          /* X Window System resource */
  kpse_src_cmdline     /* command-line option */
} kpse_src_type;


/* For each file format, we record the following information.  The main
   thing that is not part of this structure is the environment variable
   lists. They are used directly in tex-file.c. We could incorporate
   them here, but it would complicate the code a bit. We could also do
   it via variable expansion, but not now, maybe not ever:
   ${PKFONTS-${TEXFONTS-/usr/local/lib/texmf/fonts//}}.  */

typedef struct
{
  const char *type;		/* Human-readable description.  */
  const char *path;		/* The search path to use.  */
  const char *raw_path;	/* Pre-$~ (but post-default) expansion.  */
  const char *path_source;	/* Where the path started from.  */
  const char *override_path;	/* From client environment variable.  */
  const char *client_path;	/* E.g., from dvips's config.ps.  */
  const char *cnf_path;	/* From texmf.cnf.  */
  const char *default_path;	/* If all else fails.  */
  const char **suffix;		/* For kpse_find_file to check for/append.  */
  const char **alt_suffix;	/* More suffixes to check for.  */
  bool suffix_search_only;	/* Only search with a suffix?  */
  const char *program;		/* ``mktexpk'', etc.  */
  const char *program_args;	/* Args to `program'.  */
  bool program_enabled_p;	/* Invoke `program'?  */
  kpse_src_type program_enable_level; /* Who said to invoke `program'.  */
  bool binmode;              /* The files must be opened in binary mode. */
} kpse_format_info_type;

/* The sole variable of that type, indexed by `kpse_file_format_type'.
   Initialized by calls to `kpse_find_file' for `kpse_init_format'.  */
static kpse_format_info_type kpse_format_info;

#define DB_ENVS "TEXMFDBS"

/* And EXPAND_DEFAULT calls kpse_expand_default on try_path and the
   present info->path.  */
#define EXPAND_DEFAULT(try_path, source_string)			\
  if (try_path) {						\
      info->raw_path = try_path;				\
      info->path = kpse_expand_default (try_path, info->path);	\
      info->path_source = source_string;			\
  }

/* Find the final search path to use for the format entry INFO, given
   the compile-time default (DEFAULT_PATH), and the environment
   variables to check (the remaining arguments, terminated with NULL).
   We set the `path' and `path_source' members of INFO.  The
   `client_path' member must already be set upon entry.  */

static void
init_path (kpse_format_info_type *info, const char *default_path, ...)
{
  char *env_name;
  char *var = NULL;
  va_list ap;

  va_start (ap, default_path);

  info->default_path = default_path;

  /* First envvar that's set to a nonempty value will exit the loop.  If
     none are set, we want the first cnf entry that matches.  Find the
     cnf entries simultaneously, to avoid having to go through envvar
     list twice -- because of the PVAR?C macro, that would mean having
     to create a str_list and then use it twice.  Yuck.  */
  while ((env_name = va_arg (ap, char *)) != NULL) {
    /* Since sh doesn't like envvar names with `.', check PATH_prog
       rather than PATH.prog.  */
    if (!var) {
      /* Try simply PATH.  */
      char *env_value = getenv (env_name);
      if (env_value && *env_value) {
        var = env_name;        
      }
    }
    
    if (var && info->cnf_path)
      break;
  }
  va_end (ap);
  
  /* Expand any extra :'s.  For each level, we replace an extra : with
     the path at the next lower level.  For example, an extra : in a
     user-set envvar should be replaced with the path from the cnf file.
     things are complicated because none of the levels above the very
     bottom are guaranteed to exist.  */

  /* Assume we can reliably start with the compile-time default.  */
  info->path = info->raw_path = info->default_path;
  info->path_source = "compile-time paths.h";

  EXPAND_DEFAULT (info->cnf_path, "texmf.cnf");
  EXPAND_DEFAULT (info->client_path, "program config file");
  if (var)
    EXPAND_DEFAULT (getenv (var), concat (var, " environment variable"));
  EXPAND_DEFAULT (info->override_path, "application override variable");
  info->path = kpse_brace_expand (info->path);
}


/* Some file types have more than one suffix.  */

static void
add_suffixes (const char ***list, ...)
{
  const char *s;
  unsigned count = 0;
  va_list ap;

  va_start (ap, list);
  
  while ((s = va_arg (ap, char *)) != NULL) {
    count++;
    XRETALLOC (*list, count + 1, const char *);
    (*list)[count - 1] = s;
  }
  va_end (ap);
  (*list)[count] = NULL;
}


static char *
remove_dbonly (const char *path)
{
  char *ret = XTALLOC(strlen (path) + 1, char), *q=ret;
  const char *p=path;
  bool new_elt=true;

  while (*p) {
    if (new_elt && *p && *p == '!' && *(p+1) == '!')
      p += 2;
    else {
      new_elt = (*p == ENV_SEP);
      *q++ = *p++;
    }
  }
  *q = '\0';
  return(ret);
}

/* Initialize everything for FORMAT.  */

static const char *
kpse_init_format (void)
{
  /* If we get called twice, don't redo all the work.  */
  if (kpse_format_info.path)
    return kpse_format_info.path;
    
  kpse_format_info.type = "ls-R";
  init_path (&kpse_format_info, DEFAULT_TEXMFDBS, DB_ENVS, NULL);
  add_suffixes(&kpse_format_info.suffix, "ls-R", NULL);
  kpse_format_info.path = remove_dbonly (kpse_format_info.path);

#ifdef KPSE_DEBUG
#define MAYBE(member) (kpse_format_info.member ? kpse_format_info.member : "(none)")

  /* Describe the monster we've created.  */
  if (KPSE_DEBUG_P (KPSE_DEBUG_PATHS))
    {
      DEBUGF2 ("Search path for %s files (from %s)\n",
              kpse_format_info.type, kpse_format_info.path_source);
      DEBUGF1 ("  = %s\n", kpse_format_info.path);
      DEBUGF1 ("  before expansion = %s\n", kpse_format_info.raw_path);
      DEBUGF1 ("  application override path = %s\n", MAYBE (override_path));
      DEBUGF1 ("  application config file path = %s\n", MAYBE (client_path));
      DEBUGF1 ("  texmf.cnf path = %s\n", MAYBE (cnf_path));
      DEBUGF1 ("  compile-time path = %s\n", MAYBE (default_path));
      DEBUGF  ("  default suffixes =");
      if (kpse_format_info.suffix) {
        const char **ext;
        for (ext = kpse_format_info.suffix; ext && *ext; ext++) {
          fprintf (stderr, " %s", *ext);
        }
        putc ('\n', stderr);
      } else {
        fputs (" (none)\n", stderr);
      }
      DEBUGF  ("  other suffixes =");
      if (kpse_format_info.alt_suffix) {
        const char **alt;
        for (alt = kpse_format_info.alt_suffix; alt && *alt; alt++) {
          fprintf (stderr, " %s", *alt);
        }
        putc ('\n', stderr);
      } else {
        fputs (" (none)\n", stderr);
      }
      DEBUGF1 ("  search only with suffix = %d\n",kpse_format_info.suffix_search_only);
      DEBUGF1 ("  runtime generation program = %s\n", MAYBE (program));
      DEBUGF1 ("  extra program args = %s\n", MAYBE (program_args));
      DEBUGF1 ("  program enabled = %d\n", kpse_format_info.program_enabled_p);
      DEBUGF1 ("  program enable level = %d\n", kpse_format_info.program_enable_level);
    }
#endif /* KPSE_DEBUG */

  return kpse_format_info.path;
}

static hash_table_type db; /* The hash table for all the ls-R's.  */
/* SMALL: The old size of the hash table was 7603, with the assumption
   that a minimal ls-R bas about 3500 entries.  But a typical ls-R will
   be more like double that size.  */
#ifndef DB_HASH_SIZE
#define DB_HASH_SIZE 15991
#endif
#ifndef DB_NAME
#define DB_NAME "ls-R"
#endif

static hash_table_type alias_db;
#ifndef ALIAS_NAME
#define ALIAS_NAME "aliases"
#endif
#ifndef ALIAS_HASH_SIZE
#define ALIAS_HASH_SIZE 1009
#endif

static str_list_type db_dir_list;

/* If DIRNAME contains any element beginning with a `.' (that is more
   than just `./'), return true.  This is to allow ``hidden''
   directories -- ones that don't get searched.  */

static bool
ignore_dir_p (const char *dirname)
{
  const char *dot_pos = dirname;
  
  while ((dot_pos = strchr (dot_pos + 1, '.'))) {
    /* If / before and no / after, skip it. */
    if (IS_DIR_SEP (dot_pos[-1]) && dot_pos[1] && !IS_DIR_SEP (dot_pos[1]))
      return true;
  }
  
  return false;
}

/* Allocate in increments of this size.  */
#define BLOCK_SIZE 75

static char *
read_line (FILE *f)
{
  int c;
  unsigned limit = BLOCK_SIZE;
  unsigned loc = 0;
  char *line = (char *) xmalloc (limit);
  
  while ((c = getc (f)) != EOF && c != '\n' && c != '\r')
    {
      line[loc] = c;
      loc++;
      
      /* By testing after the assignment, we guarantee that we'll always
         have space for the null we append below.  We know we always
         have room for the first char, since we start with BLOCK_SIZE.  */
      if (loc == limit)
        {
          limit += BLOCK_SIZE;
          line = (char *) xrealloc (line, limit);
        }
    }
  
  /* If we read anything, return it.  This can't represent a last
     ``line'' which doesn't end in a newline, but so what.  */
  if (c != EOF)
    {
      /* Terminate the string.  We can't represent nulls in the file,
         either.  Again, it doesn't matter.  */
      line[loc] = 0;
      /* Absorb LF of a CRLF pair. */
      if (c == '\r') {
          c = getc (f);
          if (c != '\n')
              ungetc (c, f);
      }
    }
  else /* At end of file.  */
    {
      free (line);
      line = NULL;
    }

  return line;
}

/* If no DB_FILENAME, return false (maybe they aren't using this feature).
   Otherwise, add entries from DB_FILENAME to TABLE, and return true.  */

static bool
db_build (hash_table_type *table, const char *db_filename)
{
  char *line;
  unsigned dir_count = 0, file_count = 0, ignore_dir_count = 0;
  unsigned len = strlen (db_filename) - sizeof (DB_NAME) + 1; /* Keep the /. */
  char *top_dir = (char *) xmalloc (len + 1);
  char *cur_dir = NULL; /* First thing in ls-R might be a filename.  */
  FILE *db_file = xfopen (db_filename, "r");
  
  strncpy (top_dir, db_filename, len);
  top_dir[len] = 0;
  
  if (db_file) {
    while ((line = read_line (db_file)) != NULL) {
      len = strlen (line);

      /* A line like `/foo:' = new dir foo.  Allow both absolute (/...)
         and explicitly relative (./...) names here.  It's a kludge to
         pass in the directory name with the trailing : still attached,
         but it doesn't actually hurt.  */
      if (len > 0 && line[len - 1] == ':' && kpse_absolute_p (line, true)) {
        /* New directory line.  */
        if (!ignore_dir_p (line)) {
          /* If they gave a relative name, prepend full directory name now.  */
          line[len - 1] = DIR_SEP;
          /* Skip over leading `./', it confuses `match' and is just a
             waste of space, anyway.  This will lose on `../', but `match'
             won't work there, either, so it doesn't matter.  */
          cur_dir = *line == '.' ? concat (top_dir, line + 2) : xstrdup (line);
          dir_count++;
        } else {
          cur_dir = NULL;
          ignore_dir_count++;
        }

      /* Ignore blank, `.' and `..' lines.  */
      } else if (*line != 0 && cur_dir   /* a file line? */
                 && !(*line == '.'
                      && (line[1] == '0' || (line[1] == '.' && line[2] == 0))))
       {/* Make a new hash table entry with a key of `line' and a data
           of `cur_dir'.  An already-existing identical key is ok, since
           a file named `foo' can be in more than one directory.  Share
           `cur_dir' among all its files (and hence never free it). */
        hash_insert (table, xstrdup (line), cur_dir);
        file_count++;

      } /* else ignore blank lines or top-level files
           or files in ignored directories*/

      free (line);
    }

    xfclose (db_file, db_filename);

    if (file_count == 0) {
      WARNING1 ("kpathsea: No usable entries in %s", db_filename);
      WARNING ("kpathsea: See the manual for how to generate ls-R");
      db_file = NULL;
    } else {
      str_list_add (&db_dir_list, xstrdup (top_dir));
    }

#ifdef KPSE_DEBUG
    if (KPSE_DEBUG_P (KPSE_DEBUG_HASH)) {
      /* Don't make this a debugging bit, since the output is so
         voluminous, and being able to specify -1 is too useful.
         Instead, let people who want it run the program under
         a debugger and change the variable that way.  */
      bool hash_summary_only = true;

      DEBUGF4 ("%s: %u entries in %d directories (%d hidden).\n",
               db_filename, file_count, dir_count, ignore_dir_count);
      DEBUGF ("ls-R hash table:");
      hash_print (*table, hash_summary_only);
      fflush (stderr);
    }
#endif /* KPSE_DEBUG */
  }

  free (top_dir);

  return db_file != NULL;
}


/* Insert FNAME into the hash table.  This is for files that get built
   during a run.  We wouldn't want to reread all of ls-R, even if it got
   rebuilt.  */

void
kpse_db_insert (const char *passed_fname)
{
  /* We might not have found ls-R, or even had occasion to look for it
     yet, so do nothing if we have no hash table.  */
  if (db.buckets) {
    const char *dir_part;
    char *fname = xstrdup (passed_fname);
    char *baseptr = xbasename (fname);
    const char *file_part = xstrdup (baseptr);

    *baseptr = '\0';  /* Chop off the filename.  */
    dir_part = fname; /* That leaves the dir, with the trailing /.  */

    hash_insert (&db, file_part, dir_part);
  }
}

/* Return true if FILENAME could be in PATH_ELT, i.e., if the directory
   part of FILENAME matches PATH_ELT.  Have to consider // wildcards, but
   $ and ~ expansion have already been done.  */
     
static bool
match (const char *filename, const char *path_elt)
{
  const char *original_filename = filename;
  bool matched = false;
  
  for (; *filename && *path_elt; filename++, path_elt++) {
    if (FILECHARCASEEQ (*filename, *path_elt)) /* normal character match */
      ;

    else if (IS_DIR_SEP (*path_elt)  /* at // */
             && original_filename < filename && IS_DIR_SEP (path_elt[-1])) {
      while (IS_DIR_SEP (*path_elt))
        path_elt++; /* get past second and any subsequent /'s */
      if (*path_elt == 0) {
        /* Trailing //, matches anything. We could make this part of the
           other case, but it seems pointless to do the extra work.  */
        matched = true;
        break;
      } else {
        /* Intermediate //, have to match rest of PATH_ELT.  */
        for (; !matched && *filename; filename++) {
          /* Try matching at each possible character.  */
          if (IS_DIR_SEP (filename[-1])
              && FILECHARCASEEQ (*filename, *path_elt))
            matched = match (filename, path_elt);
        }
        /* Prevent filename++ when *filename='\0'. */
        break;
      }
    }

    else /* normal character nonmatch, quit */
      break;
  }
  
  /* If we've reached the end of PATH_ELT, check that we're at the last
     component of FILENAME, we've matched.  */
  if (!matched && *path_elt == 0) {
    /* Probably PATH_ELT ended with `vf' or some such, and FILENAME ends
       with `vf/ptmr.vf'.  In that case, we'll be at a directory
       separator.  On the other hand, if PATH_ELT ended with a / (as in
       `vf/'), FILENAME being the same `vf/ptmr.vf', we'll be at the
       `p'.  Upshot: if we're at a dir separator in FILENAME, skip it.
       But if not, that's ok, as long as there are no more dir separators.  */
    if (IS_DIR_SEP (*filename))
      filename++;
      
    while (*filename && !IS_DIR_SEP (*filename))
      filename++;
    matched = *filename == 0;
  }
  
  return matched;
}


/* If DB_DIR is a prefix of PATH_ELT, return true; otherwise false.
   That is, the question is whether to try the db for a file looked up
   in PATH_ELT.  If PATH_ELT == ".", for example, the answer is no. If
   PATH_ELT == "/usr/local/lib/texmf/fonts//tfm", the answer is yes.
   
   In practice, ls-R is only needed for lengthy subdirectory
   comparisons, but there's no gain to checking PATH_ELT to see if it is
   a subdir match, since the only way to do that is to do a string
   search in it, which is all we do anyway.  */
   
static bool
elt_in_db (const char *db_dir,  const char *path_elt)
{
  bool found = false;

  while (!found && FILECHARCASEEQ (*db_dir++, *path_elt++)) {
    /* If we've matched the entire db directory, it's good.  */
    if (*db_dir == 0)
      found = true;
 
    /* If we've reached the end of PATH_ELT, but not the end of the db
       directory, it's no good.  */
    else if (*path_elt == 0)
      break;
  }

  return found;
}

/* If ALIAS_FILENAME exists, read it into TABLE.  */

static bool
alias_build (hash_table_type *table, const char *alias_filename)
{
  char *line, *real, *alias;
  unsigned count = 0;
  FILE *alias_file = xfopen (alias_filename, "r");

  if (alias_file) {
    while ((line = read_line (alias_file)) != NULL) {
      /* comments or empty */
      if (*line == 0 || *line == '%' || *line == '#') {
        ;
      } else {
        /* Each line should have two fields: realname aliasname.  */
        real = line;
        while (*real && isspace (*real))
          real++;
        alias = real;
        while (*alias && !isspace (*alias))
          alias++;
        *alias++ = 0;
        while (*alias && isspace (*alias)) 
          alias++;
        /* Is the check for errors strong enough?  Should we warn the user
           for potential errors?  */
        if (strlen (real) != 0 && strlen (alias) != 0) {
          hash_insert (table, xstrdup (alias), xstrdup (real));
          count++;
        }
      }
      free (line);
    }

#ifdef KPSE_DEBUG
    if (KPSE_DEBUG_P (KPSE_DEBUG_HASH)) {
      /* As with ls-R above ... */
      bool hash_summary_only = true;
      DEBUGF2 ("%s: %u aliases.\n", alias_filename, count);
      DEBUGF ("alias hash table:");
      hash_print (*table, hash_summary_only);
      fflush (stderr);
    }
#endif /* KPSE_DEBUG */

    xfclose (alias_file, alias_filename);
  }

  return alias_file != NULL;
}

/* Initialize the path for ls-R files, and read them all into the hash
   table `db'.  If no usable ls-R's are found, set db.buckets to NULL.  */

void
kpse_init_db (void)
{
  bool ok = false;
  const char *db_path = kpse_init_format ();
  char **db_files = kpse_all_path_search (db_path, DB_NAME);
  char **orig_db_files = db_files;

  /* Must do this after the path searching (which ends up calling
    kpse_db_search recursively), so db.buckets stays NULL.  */
  db = hash_create (DB_HASH_SIZE);

  while (db_files && *db_files) {
    if (db_build (&db, *db_files))
      ok = true;
    free (*db_files);
    db_files++;
  }
  
  if (!ok) {
    /* If db can't be built, leave `size' nonzero (so we don't
       rebuild it), but clear `buckets' (so we don't look in it).  */
    free (db.buckets);
    db.buckets = NULL;
  }

  free (orig_db_files);

  /* Add the content of any alias databases.  There may exist more than
     one alias file along DB_NAME files.  This duplicates the above code
     -- should be a function.  */
  ok = false;
  db_files = kpse_all_path_search (db_path, ALIAS_NAME);
  orig_db_files = db_files;

  alias_db = hash_create (ALIAS_HASH_SIZE);

  while (db_files && *db_files) {
    if (alias_build (&alias_db, *db_files))
      ok = true;
    free (*db_files);
    db_files++;
  }

  if (!ok) {
    free (alias_db.buckets);
    alias_db.buckets = NULL;
  }

  free (orig_db_files);
}

/* Avoid doing anything if this PATH_ELT is irrelevant to the databases. */

str_list_type *
kpse_db_search (const char *name, const char *orig_path_elt, bool all)
{
  char **db_dirs, **orig_dirs, **r;
  const char *last_slash;
  char *path_elt;
  bool done;
  str_list_type *ret = 0;
  unsigned e;
  char **aliases = NULL;
  bool relevant = false;
  
  /* If we failed to build the database (or if this is the recursive
     call to build the db path), quit.  */
  if (db.buckets == NULL)
    return NULL;
  
  /* When tex-glyph.c calls us looking for, e.g., dpi600/cmr10.pk, we
     won't find it unless we change NAME to just `cmr10.pk' and append
     `/dpi600' to PATH_ELT.  We are justified in using a literal `/'
     here, since that's what tex-glyph.c unconditionally uses in
     DPI_BITMAP_SPEC.  But don't do anything if the / begins NAME; that
     should never happen.  */
  last_slash = strrchr (name, '/');
  if (last_slash && last_slash != name) {
    unsigned len = last_slash - name + 1;
    char *dir_part = (char *) xmalloc (len);
    strncpy (dir_part, name, len - 1);
    dir_part[len - 1] = 0;
    path_elt = concat3 (orig_path_elt, "/", dir_part);
    name = last_slash + 1;
  } else
    path_elt = (char *) orig_path_elt;

  /* Don't bother doing any lookups if this `path_elt' isn't covered by
     any of database directories.  We do this not so much because the
     extra couple of hash lookups matter -- they don't -- but rather
     because we want to return NULL in this case, so path_search can
     know to do a disk search.  */
  for (e = 0; !relevant && e < STR_LIST_LENGTH (db_dir_list); e++) {
    relevant = elt_in_db (STR_LIST_ELT (db_dir_list, e), path_elt);
  }
  if (!relevant)
    return NULL;

  /* If we have aliases for this name, use them.  */
  if (alias_db.buckets)
    aliases = hash_lookup (alias_db, name);

  if (!aliases) {
    aliases = XTALLOC1 (char *);
    aliases[0] = NULL;
  }
  {  /* Push aliases up by one and insert the original name at the front.  */
    unsigned i;
    unsigned len = 1; /* Have NULL element already allocated.  */
    for (r = aliases; *r; r++)
      len++;
    XRETALLOC (aliases, len + 1, char *);
    for (i = len; i > 0; i--) {
      aliases[i] = aliases[i - 1];
    }
    aliases[0] = (char *) name;
  }

  done = false;
  for (r = aliases; !done && *r; r++) {
    char *atry = *r;

    /* We have an ls-R db.  Look up `atry'.  */
    orig_dirs = db_dirs = hash_lookup (db, atry);

    ret = XTALLOC1 (str_list_type);
    *ret = str_list_init ();

    /* For each filename found, see if it matches the path element.  For
       example, if we have .../cx/cmr10.300pk and .../ricoh/cmr10.300pk,
       and the path looks like .../cx, we don't want the ricoh file.  */
    while (!done && db_dirs && *db_dirs) {
      char *db_file = concat (*db_dirs, atry);
      bool matched = match (db_file, path_elt);

#ifdef KPSE_DEBUG
      if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
        DEBUGF3 ("db:match(%s,%s) = %d\n", db_file, path_elt, matched);
#endif

      /* We got a hit in the database.  Now see if the file actually
         exists, possibly under an alias.  */
      if (matched) {
        char *found = NULL;
        if (kpse_readable_file (db_file)) {
          found = db_file;
          
        } else {
          char **a;
          
          free (db_file); /* `db_file' wasn't on disk.  */
          
          /* The hit in the DB doesn't exist in disk.  Now try all its
             aliases.  For example, suppose we have a hierarchy on CD,
             thus `mf.bas', but ls-R contains `mf.base'.  Find it anyway.
             Could probably work around this with aliases, but
             this is pretty easy and shouldn't hurt.  The upshot is that
             if one of the aliases actually exists, we use that.  */
          for (a = aliases + 1; *a && !found; a++) {
            char *atry = concat (*db_dirs, *a);
            if (kpse_readable_file (atry))
              found = atry;
            else
              free (atry);
          }
        }
          
        /* If we have a real file, add it to the list, maybe done.  */
        if (found) {
          str_list_add (ret, found);
          if (!all && found)
            done = true;
        }
      } else { /* no match in the db */
        free (db_file);
      }
      

      /* On to the next directory, if any.  */
      db_dirs++;
    }

    /* This is just the space for the pointers, not the strings.  */
    if (orig_dirs && *orig_dirs)
      free (orig_dirs);
  }
  
  free (aliases);
  
  /* If we had to break up NAME, free the temporary PATH_ELT.  */
  if (path_elt != orig_path_elt)
    free (path_elt);

  return ret;
}

/* kdefault.c: Expand extra colons.  */

/* Check for leading colon first, then trailing, then doubled, since
   that is fastest.  Usually it will be leading or trailing.  */

char *
kpse_expand_default (const char *path, const char *fallback)
{
  unsigned path_length;
  char *expansion;
  
  /* The default path better not be null.  */
  assert (fallback);
  
  if (path == NULL)
    expansion = xstrdup (fallback);

  /* Solitary or leading :?  */
  else if (IS_ENV_SEP (*path))
    {
      expansion = path[1] == 0 ? xstrdup (fallback) : concat (fallback, path);
    }

  /* Sorry about the assignment in the middle of the expression, but
     conventions were made to be flouted and all that.  I don't see the
     point of calling strlen twice or complicating the logic just to
     avoid the assignment (especially now that I've pointed it out at
     such great length).  */
  else if (path[(path_length = strlen (path)) - 1] == ENV_SEP)
    expansion = concat (path, fallback);

  /* OK, not leading or trailing.  Check for doubled.  */
  else
    {
      const char *loc;

      /* What we'll return if we find none.  */
      expansion = xstrdup (path);

      for (loc = path; *loc; loc++)
        {
          if (IS_ENV_SEP (loc[0]) && IS_ENV_SEP (loc[1]))
            { /* We have a doubled colon.  */
              expansion = (char *) xmalloc (path_length + strlen (fallback) + 1);
              
              /* Copy stuff up to and including the first colon.  */
              strncpy (expansion, path, loc - path + 1);
              expansion[loc - path + 1] = 0;
              
              /* Copy in FALLBACK, and then the rest of PATH.  */
              strcat (expansion, fallback);
              strcat (expansion, loc + 1);

	      break;
            }
        }
    }
  
  return expansion;
}

/* elt-dirs.c: Translate a path element to its corresponding
   director{y,ies}.  */

/* To avoid giving prototypes for all the routines and then their real
   definitions, we give all the subroutines first.  The entry point is
   the last routine in the file.  */

/* Make a copy of DIR (unless it's null) and save it in L.  Ensure that
   DIR ends with a DIR_SEP for the benefit of later searches.  */

static void
dir_list_add (str_llist_type *l, const char *dir)
{
  char last_char = dir[strlen (dir) - 1];
  char *saved_dir
    = IS_DIR_SEP (last_char) || IS_DEVICE_SEP (last_char)
      ? xstrdup (dir)
      : concat (dir, DIR_SEP_STRING);
  
  str_llist_add (l, saved_dir);
}


/* If DIR is a directory, add it to the list L.  */

static void
checked_dir_list_add (str_llist_type *l, const char *dir)
{
  if (dir_p (dir))
    dir_list_add (l, dir);
}

/* The cache.  Typically, several paths have the same element; for
   example, /usr/local/lib/texmf/fonts//.  We don't want to compute the
   expansion of such a thing more than once.  Even though we also cache
   the dir_links call, that's not enough -- without this path element
   caching as well, the execution time doubles.  */

typedef struct
{
  const char *key;
  str_llist_type *value;
} cache_entry;

static cache_entry *the_cache = NULL;
static unsigned cache_length = 0;

/* Associate KEY with VALUE.  We implement the cache as a simple linear
   list, since it's unlikely to ever be more than a dozen or so elements
   long.  We don't bother to check here if PATH has already been saved;
   we always add it to our list.  We copy KEY but not VALUE; not sure
   that's right, but it seems to be all that's needed.  */

static void
cache (const char *key, str_llist_type *value)
{
  cache_length++;
  XRETALLOC (the_cache, cache_length, cache_entry);
  the_cache[cache_length - 1].key = xstrdup (key);
  the_cache[cache_length - 1].value = value;
}


/* To retrieve, just check the list in order.  */

static str_llist_type *
cached (const char *key)
{
  unsigned p;
  
  for (p = 0; p < cache_length; p++)
    {
      if (FILESTRCASEEQ (the_cache[p].key, key))
        return the_cache[p].value;
    }
  
  return NULL;
}

/* Handle the magic path constructs.  */

/* Declare recursively called routine.  */
static void expand_elt (str_llist_type *, const char *, unsigned);


/* POST is a pointer into the original element (which may no longer be
   ELT) to just after the doubled DIR_SEP, perhaps to the null.  Append
   subdirectories of ELT (up to ELT_LENGTH, which must be a /) to
   STR_LIST_PTR.  */

#ifdef WIN32
/* Shared across recursive calls, it acts like a stack. */
static char dirname[MAX_PATH];
#endif

static void
do_subdir (str_llist_type *str_list_ptr, const char *elt,
	   unsigned elt_length, const char *post)
{
#ifdef WIN32
  WIN32_FIND_DATA find_file_data;
  HANDLE hnd;
  int proceed;
#else
  DIR *dir;
  struct dirent *e;
#endif /* not WIN32 */
  fn_type name;
  
  /* Some old compilers don't allow aggregate initialization.  */
  name = fn_copy0 (elt, elt_length);
  
  assert (IS_DIR_SEP (elt[elt_length - 1])
          || IS_DEVICE_SEP (elt[elt_length - 1]));
  
#if defined (WIN32)
  strcpy(dirname, FN_STRING(name));
  strcat(dirname, "/*.*");         /* "*.*" or "*" -- seems equivalent. */
  hnd = FindFirstFile(dirname, &find_file_data);

  if (hnd == INVALID_HANDLE_VALUE) {
    fn_free(&name);
    return;
  }

  /* Include top level before subdirectories, if nothing to match.  */
  if (*post == 0)
    dir_list_add (str_list_ptr, FN_STRING (name));
  else {
    /* If we do have something to match, see if it exists.  For
       example, POST might be `pk/ljfour', and they might have a
       directory `$TEXMF/fonts/pk/ljfour' that we should find.  */
    fn_str_grow (&name, post);
    expand_elt (str_list_ptr, FN_STRING (name), elt_length);
    fn_shrink_to (&name, elt_length);
  }
  proceed = 1;
  while (proceed) {
    if (find_file_data.cFileName[0] != '.') {
      /* Construct the potential subdirectory name.  */
      fn_str_grow (&name, find_file_data.cFileName);
      if (find_file_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
	unsigned potential_len = FN_LENGTH (name);
	
	/* It's a directory, so append the separator.  */
	fn_str_grow (&name, DIR_SEP_STRING);

	do_subdir (str_list_ptr, FN_STRING (name),
		   potential_len, post);
      }
      fn_shrink_to (&name, elt_length);
    }
    proceed = FindNextFile (hnd, &find_file_data);
  }
  fn_free (&name);
  FindClose(hnd);

#else /* not WIN32 */

  /* If we can't open it, quit.  */
  dir = opendir (FN_STRING (name));
  if (dir == NULL)
    {
      fn_free (&name);
      return;
    }
  
  /* Include top level before subdirectories, if nothing to match.  */
  if (*post == 0)
    dir_list_add (str_list_ptr, FN_STRING (name));
  else
    { /* If we do have something to match, see if it exists.  For
         example, POST might be `pk/ljfour', and they might have a
         directory `$TEXMF/fonts/pk/ljfour' that we should find.  */
      fn_str_grow (&name, post);
      expand_elt (str_list_ptr, FN_STRING (name), elt_length);
      fn_shrink_to (&name, elt_length);
    }

  while ((e = readdir (dir)) != NULL)
    { /* If it begins with a `.', never mind.  (This allows ``hidden''
         directories that the algorithm won't find.)  */
      if (e->d_name[0] != '.')
        {
          int links;
          
          /* Construct the potential subdirectory name.  */
          fn_str_grow (&name, e->d_name);
          
          /* If we can't stat it, or if it isn't a directory, continue.  */
          links = dir_links (FN_STRING (name));

          if (links >= 0)
            { 
              unsigned potential_len = FN_LENGTH (name);
              
              /* It's a directory, so append the separator.  */
              fn_str_grow (&name, DIR_SEP_STRING);
              
              /* Should we recurse?  To see if the subdirectory is a
                 leaf, check if it has two links (one for . and one for
                 ..).  This means that symbolic links to directories do
                 not affect the leaf-ness.  This is arguably wrong, but
                 the only alternative I know of is to stat every entry
                 in the directory, and that is unacceptably slow.
                 
                 The #ifdef here makes all this configurable at
                 compile-time, so that if we're using VMS directories or
                 some such, we can still find subdirectories, even if it
                 is much slower.  */
#ifdef ST_NLINK_TRICK
#ifdef AMIGA
              /* With SAS/C++ 6.55 on the Amiga, `stat' sets the `st_nlink'
                 field to -1 for a file, or to 1 for a directory.  */
              if (links == 1)
#else
              if (links > 2)
#endif /* not AMIGA */
#endif /* not ST_NLINK_TRICK */
                /* All criteria are met; find subdirectories.  */
                do_subdir (str_list_ptr, FN_STRING (name),
                           potential_len, post);
#ifdef ST_NLINK_TRICK
              else if (*post == 0)
                /* Nothing to match, no recursive subdirectories to
                   look for: we're done with this branch.  Add it.  */
                dir_list_add (str_list_ptr, FN_STRING (name));
#endif
            }

          /* Remove the directory entry we just checked from `name'.  */
          fn_shrink_to (&name, elt_length);
        }
    }
  
  fn_free (&name);
  xclosedir (dir);
#endif /* not WIN32 */
}


/* Assume ELT is non-empty and non-NULL.  Return list of corresponding
   directories (with no terminating NULL entry) in STR_LIST_PTR.  Start
   looking for magic constructs at START.  */

static void
expand_elt (str_llist_type *str_list_ptr, const char *elt, unsigned start)
{
  const char *dir = elt + start;
  const char *post;
  
  while (*dir != 0)
    {
      if (IS_DIR_SEP (*dir))
        {
          /* If two or more consecutive /'s, find subdirectories.  */
          if (IS_DIR_SEP (dir[1]))
            {
	      for (post = dir + 1; IS_DIR_SEP (*post); post++) ;
              do_subdir (str_list_ptr, elt, dir - elt + 1, post);
	      return;
            }

          /* No special stuff at this slash.  Keep going.  */
        }
      
      dir++;
    }
  
  /* When we reach the end of ELT, it will be a normal filename.  */
  checked_dir_list_add (str_list_ptr, elt);
}

/* Here is the entry point.  Returns directory list for ELT.  */

str_llist_type *
kpse_element_dirs (const char *elt)
{
  str_llist_type *ret;

  /* If given nothing, return nothing.  */
  if (!elt || !*elt)
    return NULL;

  /* If we've already cached the answer for ELT, return it.  */
  ret = cached (elt);
  if (ret)
    return ret;

  /* We're going to have a real directory list to return.  */
  ret = XTALLOC1 (str_llist_type);
  *ret = NULL;

  /* We handle the hard case in a subroutine.  */
  expand_elt (ret, elt, 0);

  /* Remember the directory list we just found, in case future calls are
     made with the same ELT.  */
  cache (elt, ret);

#ifdef KPSE_DEBUG
  if (KPSE_DEBUG_P (KPSE_DEBUG_EXPAND))
    {
      DEBUGF1 ("path element %s =>", elt);
      if (ret)
        {
          str_llist_elt_type *e;
          for (e = *ret; e; e = STR_LLIST_NEXT (*e))
            fprintf (stderr, " %s", STR_LLIST (*e));
        }
      putc ('\n', stderr);
      fflush (stderr);
    }
#endif /* KPSE_DEBUG */

  return ret;
}

/* path-elt.c: Return the stuff between colons.  */

/* The static (but dynamically allocated) area we return the answer in,
   and how much we've currently allocated for it.  */
static char *elt = NULL;
static unsigned elt_alloc = 0;

/* The path we're currently working on.  */
static const char *path = NULL;

/* Upon entry, the static `path' is at the first (and perhaps last)
   character of the return value, or else NULL if we're at the end (or
   haven't been called).  I make no provision for caching the results;
   thus, we parse the same path over and over, on every lookup.  If that
   turns out to be a significant lose, it can be fixed, but I'm guessing
   disk accesses overwhelm everything else.  If ENV_P is true, use
   IS_ENV_SEP; else use IS_DIR_SEP.  */

static char *
element (const char *passed_path, bool env_p)
{
  const char *p;
  char *ret;
  int brace_level;
  unsigned len;
  
  if (passed_path)
    path = passed_path;
  /* Check if called with NULL, and no previous path (perhaps we reached
     the end).  */
  else if (!path)
    return NULL;
  
  /* OK, we have a non-null `path' if we get here.  */
  assert (path);
  p = path;
  
  /* Find the next colon not enclosed by braces (or the end of the path).  */
  brace_level = 0;
  while (*p != 0  && !(brace_level == 0
                       && (env_p ? IS_ENV_SEP (*p) : IS_DIR_SEP (*p)))) {
    if (*p == '{') ++brace_level;
    else if (*p == '}') --brace_level;
    ++p;
  }
   
  /* Return the substring starting at `path'.  */
  len = p - path;

  /* Make sure we have enough space (including the null byte).  */
  if (len + 1 > elt_alloc)
    {
      elt_alloc = len + 1;
      elt = (char *) xrealloc (elt, elt_alloc);
    }

  strncpy (elt, path, len);
  elt[len] = 0;
  ret = elt;

  /* If we are at the end, return NULL next time.  */
  if (path[len] == 0)
    path = NULL;
  else
    path += len + 1;

  return ret;
}

char *
kpse_path_element (const char *p)
{
  return element (p, true);
}

char *
kpse_filename_component (const char *p)
{
  return element (p, false);
}

/* xfopen.c: fopen and fclose with error checking.  */

/* These routines just check the return status from standard library
   routines and abort if an error happens.  */

FILE *
xfopen (const char *filename, const char *mode)
{
  FILE *f;
  
  assert (filename && mode);
  
  f = fopen (filename, mode);
  if (f == NULL)
    FATAL_PERROR (filename);

  return f;
}

void
xfclose (FILE *f, const char *filename)
{
  assert (f);
  
  if (fclose (f) == EOF)
    FATAL_PERROR (filename);
}

/* xftell.c: ftell with error checking.  */

unsigned long
xftell (FILE *f, char *filename)
{
  long where = ftell (f);

  if (where < 0)
    FATAL_PERROR (filename);

  return where;
}

#ifndef WIN32
void
xclosedir (DIR *d)
{
#ifdef CLOSEDIR_VOID
  closedir (d);
#else
  int ret = closedir (d);
  
  if (ret != 0)
    FATAL ("closedir failed");
#endif
}
#endif

/* xmalloc.c: malloc with error checking.  */

void *
xmalloc (unsigned size)
{
  void *new_mem = (void *) malloc (size);

  if (new_mem == NULL)
    {
      fprintf (stderr, "fatal: memory exhausted (xmalloc of %u bytes).\n",
               size);
      /* 1 means success on VMS, so pick a random number (ASCII `K').  */
      exit (75);
    }

  return new_mem;
}

/* xrealloc.c: realloc with error checking.  */

extern void *xmalloc (unsigned);

void *
xrealloc (void *old_ptr, unsigned size)
{
  void *new_mem;

  if (old_ptr == NULL)
    new_mem = xmalloc (size);
  else
    {
      new_mem = (void *) realloc (old_ptr, size);
      if (new_mem == NULL)
        {
          /* We used to print OLD_PTR here using %x, and casting its
             value to unsigned, but that lost on the Alpha, where
             pointers and unsigned had different sizes.  Since the info
             is of little or no value anyway, just don't print it.  */
          fprintf (stderr, "fatal: memory exhausted (realloc of %u bytes).\n",
                   size);
          /* 1 means success on VMS, so pick a random number (ASCII `B').  */
          exit (66);
        }
    }

  return new_mem;
}

/* xstrdup.c: strdup with error checking.  */

/* Return a copy of S in new storage.  */

char *
xstrdup (const char *s)
{
  char *new_string = (char *) xmalloc (strlen (s) + 1);
  return strcpy (new_string, s);
}

/* dir.c: directory operations.  */

/* Return true if FN is a directory or a symlink to a directory,
   false if not. */

int
dir_p (const char *fn)
{
#ifdef WIN32
  unsigned int fa = GetFileAttributes(fn);
  return (fa != 0xFFFFFFFF && (fa & FILE_ATTRIBUTE_DIRECTORY));
#else
  struct stat stats;
  return stat (fn, &stats) == 0 && S_ISDIR (stats.st_mode);
#endif
}

#ifndef WIN32

/* Return -1 if FN isn't a directory, else its number of links.
   Duplicate the call to stat; no need to incur overhead of a function
   call for that little bit of cleanliness. */

int
dir_links (const char *fn)
{
  static hash_table_type link_table;
  char **hash_ret;
  long ret;
  
  if (link_table.size == 0)
    link_table = hash_create (457);

#ifdef KPSE_DEBUG
  /* This is annoying, but since we're storing integers as pointers, we
     can't print them as strings.  */
  if (KPSE_DEBUG_P (KPSE_DEBUG_HASH))
    kpse_debug_hash_lookup_int = 1;
#endif

  hash_ret = hash_lookup (link_table, fn);
  
#ifdef KPSE_DEBUG
  if (KPSE_DEBUG_P (KPSE_DEBUG_HASH))
    kpse_debug_hash_lookup_int = 0;
#endif

  /* Have to cast the int we need to/from the const_string that the hash
     table stores for values. Let's hope an int fits in a pointer.  */
  if (hash_ret)
    ret = (long) *hash_ret;
  else
    {
      struct stat stats;
      ret = stat (fn, &stats) == 0 && S_ISDIR (stats.st_mode)
            ? stats.st_nlink : (unsigned) -1;

      /* It's up to us to copy the value.  */
      hash_insert (&link_table, xstrdup (fn), (const char *) ret);
      
#ifdef KPSE_DEBUG
      if (KPSE_DEBUG_P (KPSE_DEBUG_STAT))
        DEBUGF2 ("dir_links(%s) => %ld\n", fn, ret);
#endif
    }

  return ret;
}

#endif /* !WIN32 */

/* hash.c: hash table operations.  */

/* The hash function.  We go for simplicity here.  */

/* All our hash tables are related to filenames.  */
#ifdef MONOCASE_FILENAMES
#define TRANSFORM(x) toupper (x)
#else
#define TRANSFORM(x) (x)
#endif

static unsigned
hash (hash_table_type table, const char *key)
{
  unsigned n = 0;
  
  /* Our keys aren't often anagrams of each other, so no point in
     weighting the characters.  */
  while (*key != 0)
    n = (n + n + TRANSFORM (*key++)) % table.size;
  
  return n;
}

hash_table_type
hash_create (unsigned size) 
{
  /* hash_table_type ret; changed into "static ..." to work around gcc
     optimizer bug for Alpha.  */
  static hash_table_type ret;
  unsigned b;
  ret.buckets = XTALLOC (size, hash_element_type *);
  ret.size = size;
  
  /* calloc's zeroes aren't necessarily NULL, so be safe.  */
  for (b = 0; b <ret.size; b++)
    ret.buckets[b] = NULL;
    
  return ret;
}

/* Whether or not KEY is already in MAP, insert it and VALUE.  Do not
   duplicate the strings, in case they're being purposefully shared.  */

void
hash_insert (hash_table_type *table, const char *key, const char *value)
{
  unsigned n = hash (*table, key);
  hash_element_type *new_elt = XTALLOC1 (hash_element_type);

  new_elt->key = key;
  new_elt->value = value;
  new_elt->next = NULL;
  
  /* Insert the new element at the end of the list.  */
  if (!table->buckets[n])
    /* first element in bucket is a special case.  */
    table->buckets[n] = new_elt;
  else
    {
      hash_element_type *loc = table->buckets[n];
      while (loc->next)		/* Find the last element.  */
        loc = loc->next;
      loc->next = new_elt;	/* Insert the new one after.  */
    }
}

/* Look up STR in MAP.  Return a (dynamically-allocated) list of the
   corresponding strings or NULL if no match.  */ 

#ifdef KPSE_DEBUG
/* Print the hash values as integers if this is nonzero.  */
int kpse_debug_hash_lookup_int = 0; 
#endif

char **
hash_lookup (hash_table_type table, const char *key)
{
  hash_element_type *p;
  str_list_type ret;
  unsigned n = hash (table, key);
  ret = str_list_init ();
  
  /* Look at everything in this bucket.  */
  for (p = table.buckets[n]; p != NULL; p = p->next)
    if (FILESTRCASEEQ (key, p->key))
      /* Cast because the general str_list_type shouldn't force const data.  */
      str_list_add (&ret, (char *) p->value);
  
  /* If we found anything, mark end of list with null.  */
  if (STR_LIST (ret))
    str_list_add (&ret, NULL);

#ifdef KPSE_DEBUG
  if (KPSE_DEBUG_P (KPSE_DEBUG_HASH))
    {
      DEBUGF1 ("hash_lookup(%s) =>", key);
      if (!STR_LIST (ret))
        fputs (" (nil)\n", stderr);
      else
        {
          char **r;
          for (r = STR_LIST (ret); *r; r++)
            {
              putc (' ', stderr);
              if (kpse_debug_hash_lookup_int)
                fprintf (stderr, "%ld", (long) *r);
              else
                fputs (*r, stderr);
            }
          putc ('\n', stderr);
        }
      fflush (stderr);
    }
#endif

  return STR_LIST (ret);
}

/* We only print nonempty buckets, to decrease output volume.  */

void
hash_print (hash_table_type table, int summary_only)
{
  unsigned b;
  unsigned total_elements = 0, total_buckets = 0;
  
  for (b = 0; b < table.size; b++) {
    hash_element_type *bucket = table.buckets[b];

    if (bucket) {
      unsigned len = 1;
      hash_element_type *tb;

      total_buckets++;
      if (!summary_only) fprintf (stderr, "%4d ", b);

      for (tb = bucket->next; tb != NULL; tb = tb->next)
        len++;
      if (!summary_only) fprintf (stderr, ":%-5d", len);
      total_elements += len;

      if (!summary_only) {
        for (tb = bucket; tb != NULL; tb = tb->next)
          fprintf (stderr, " %s=>%s", tb->key, tb->value);
        putc ('\n', stderr);
      }
    }
  }
  
  fprintf (stderr,
          "%u buckets, %u nonempty (%u%%); %u entries, average chain %.1f.\n",
          table.size,
          total_buckets,
          100 * total_buckets / table.size,
          total_elements,
          total_buckets ? total_elements / (double) total_buckets : 0.0);
}

/* concat.c: dynamic string concatenation.  */

/* Return the concatenation of S1 and S2.  See `concatn.c' for a
   `concatn', which takes a variable number of arguments.  */

char *
concat (const char *s1, const char *s2)
{
  char *answer = (char *) xmalloc (strlen (s1) + strlen (s2) + 1);
  strcpy (answer, s1);
  strcat (answer, s2);

  return answer;
}

/* concat3.c: concatenate three strings.  */

char *
concat3 (const char *s1, const char *s2, const char *s3)
{
  char *answer
    = (char *) xmalloc (strlen (s1) + strlen (s2) + strlen (s3) + 1);
  strcpy (answer, s1);
  strcat (answer, s2);
  strcat (answer, s3);

  return answer;
}

/* concatn.c: Concatenate an arbitrary number of strings.  */

/* OK, it would be epsilon more efficient to compute the total length
   and then do the copying ourselves, but I doubt it matters in reality.  */

char *
concatn (const char *str1, ...)
{
  char *arg;
  char *ret;
  va_list ap;

  va_start (ap, str1);

  if (!str1)
    return NULL;
  
  ret = xstrdup (str1);
  
  while ((arg = va_arg (ap, char *)) != NULL)
    {
      char *temp = concat (ret, arg);
      free (ret);
      ret = temp;
    }
  va_end (ap);
  
  return ret;
}

/* debug.c: Help the user discover what's going on.  */

#ifdef KPSE_DEBUG

unsigned int kpathsea_debug = 0;

/* If the real definitions of fopen or fclose are macros, we lose -- the
   #undef won't restore them. */

FILE *
fopen (const char *filename, const char *mode)
{
#undef fopen
  FILE *ret = fopen (filename, mode);

  if (KPSE_DEBUG_P (KPSE_DEBUG_FOPEN))
    DEBUGF3 ("fopen(%s, %s) => 0x%lx\n", filename, mode, (unsigned long) ret);

  return ret;
}

int
fclose (FILE *f)
{
#undef fclose
  int ret = fclose (f);
  
  if (KPSE_DEBUG_P (KPSE_DEBUG_FOPEN))
    DEBUGF2 ("fclose(0x%lx) => %d\n", (unsigned long) f, ret);

  return ret;
}

#endif

/* find-suffix.c: return the stuff after a dot.  */

/* Return pointer to first character after `.' in last directory element
   of NAME.  If the name is `foo' or `/foo.bar/baz', we have no extension.  */

char *
find_suffix (const char *name)
{
  const char *slash_pos;
  char *dot_pos = strrchr (name, '.');
  
  if (dot_pos == NULL)
    return NULL;
  
  for (slash_pos = name + strlen (name);
       slash_pos > dot_pos && !IS_DIR_SEP (*slash_pos);
       slash_pos--)
    ;
  
  return slash_pos > dot_pos ? NULL : dot_pos + 1;
}

/* rm-suffix.c: remove any suffix.  */

/* Generic const warning -- see extend-fname.c.  */

char *
remove_suffix (const char *s)
{
  char *ret;
  const char *suffix = find_suffix (s);
  
  if (suffix)
    {
      /* Back up to before the dot.  */
      suffix--;
      ret = (char *) xmalloc (suffix - s + 1);
      strncpy (ret, s, suffix - s);
      ret[suffix - s] = 0;
    }
  else
    ret = (char *) s;
    
  return ret;
}

/* readable.c: check if a filename is a readable non-directory file.  */

/* Truncate any too-long components in NAME, returning the result.  It's
   too bad this is necessary.  See comments in readable.c for why.  */

static char *
kpse_truncate_filename (const char *name)
{
  unsigned c_len = 0;        /* Length of current component.  */
  unsigned ret_len = 0;      /* Length of constructed result.  */
  
  /* Allocate enough space.  */
  char *ret = (char *) xmalloc (strlen (name) + 1);

  for (; *name; name++)
    {
      if (IS_DIR_SEP (*name) || IS_DEVICE_SEP (*name))
        { /* At a directory delimiter, reset component length.  */
          c_len = 0;
        }
      else if (c_len > NAME_MAX)
        { /* If past the max for a component, ignore this character.  */
          continue;
        }

      /* Copy this character.  */
      ret[ret_len++] = *name;
      c_len++;
    }
  ret[ret_len] = 0;

  return ret;
}

/* If access can read FN, run stat (assigning to stat buffer ST) and
   check that fn is not a directory.  Don't check for just being a
   regular file, as it is potentially useful to read fifo's or some
   kinds of devices.  */

#ifdef WIN32
static inline bool
READABLE (const char *fn, struct stat&)
{
  return (GetFileAttributes(fn) != 0xFFFFFFFF
	  && !(GetFileAttributes(fn) & FILE_ATTRIBUTE_DIRECTORY));
}
#else
static inline bool
READABLE (const char *fn, struct stat& st)
{
  return (access (fn, R_OK) == 0
	  && stat (fn, &(st)) == 0
	  && !S_ISDIR (st.st_mode));
}
#endif

/* POSIX invented the brain-damage of not necessarily truncating
   filename components; the system's behavior is defined by the value of
   the symbol _POSIX_NO_TRUNC, but you can't change it dynamically!
   
   Generic const return warning.  See extend-fname.c.  */

char *
kpse_readable_file (const char *name)
{
  struct stat st;
  char *ret;
  
  if (READABLE (name, st)) {
    ret = (char *) name;

#ifdef ENAMETOOLONG
  } else if (errno == ENAMETOOLONG) {
    ret = kpse_truncate_filename (name);

    /* Perhaps some other error will occur with the truncated name, so
       let's call access again.  */
    if (!READABLE (ret, st))
      { /* Failed.  */
        if (ret != name) free (ret);
        ret = NULL;
      }
#endif /* ENAMETOOLONG */

  } else { /* Some other error.  */
    if (errno == EACCES) { /* Maybe warn them if permissions are bad.  */
      perror (name);
    }
    ret = NULL;
  }
  
  return ret;
}

/* absolute.c: Test if a filename is absolute or explicitly relative.  */

/* Sorry this is such a system-dependent mess, but I can't see any way
   to usefully generalize.  */

int
kpse_absolute_p (const char *filename, int relative_ok)
{
  int absolute = IS_DIR_SEP (*filename)
#ifdef DOSISH
                     /* Novell allows non-alphanumeric drive letters. */
                     || (*filename && IS_DEVICE_SEP (filename[1]))
#endif /* DOSISH */
#ifdef WIN32
                     /* UNC names */
                     || (*filename == '\\' && filename[1] == '\\')
#endif
		      ;
  int explicit_relative
    = relative_ok
      && (*filename == '.' && (IS_DIR_SEP (filename[1])
                         || (filename[1] == '.' && IS_DIR_SEP (filename[2]))));

  return absolute || explicit_relative;
}

/* str-list.c: define routines for string lists.  */

/* See the lib.h file for comments.  */

str_list_type
str_list_init (void)
{
  str_list_type ret;
  
  STR_LIST_LENGTH (ret) = 0;
  STR_LIST (ret) = NULL;
  
  return ret;
}

void
str_list_add (str_list_type *l, char *s)
{
  STR_LIST_LENGTH (*l)++;
  XRETALLOC (STR_LIST (*l), STR_LIST_LENGTH (*l), char *);
  STR_LIST_LAST_ELT (*l) = s;
}

/* May as well save some reallocations and do everything in a chunk
   instead of calling str_list_add on each element.  */
   
void
str_list_concat (str_list_type *target, str_list_type more)
{
  unsigned e;
  unsigned prev_len = STR_LIST_LENGTH (*target);

  STR_LIST_LENGTH (*target) += STR_LIST_LENGTH (more);
  XRETALLOC (STR_LIST (*target), STR_LIST_LENGTH (*target), char *);
  
  for (e = 0; e < STR_LIST_LENGTH (more); e++)
    STR_LIST_ELT (*target, prev_len + e) = STR_LIST_ELT (more, e);
}

/* Free the list (but not the elements within it).  */

void
str_list_free (str_list_type *l)
{
  if (STR_LIST (*l))
    {
      free (STR_LIST (*l));
      STR_LIST (*l) = NULL;
    }
}

/* str-llist.c: Implementation of a linked list of strings.  */

/* Add the new string STR to the end of the list L.  */

void
str_llist_add (str_llist_type *l, char *str)
{
  str_llist_elt_type *e;
  str_llist_elt_type *new_elt = XTALLOC1 (str_llist_elt_type);
  
  /* The new element will be at the end of the list.  */
  STR_LLIST (*new_elt) = str;
  STR_LLIST_MOVED (*new_elt) = 0;
  STR_LLIST_NEXT (*new_elt) = NULL;
  
  /* Find the current end of the list.  */
  for (e = *l; e && STR_LLIST_NEXT (*e); e = STR_LLIST_NEXT (*e))
    ;
  
  if (!e)
    *l = new_elt;
  else
    STR_LLIST_NEXT (*e) = new_elt;
}

/* Move an element towards the top. The idea is that when a file is
   found in a given directory, later files will likely be in that same
   directory, and looking for the file in all the directories in between
   is thus a waste.  */

void
str_llist_float (str_llist_type *l, str_llist_elt_type *mover)
{
  str_llist_elt_type *last_moved, *unmoved;
  
  /* If we've already moved this element, never mind.  */
  if (STR_LLIST_MOVED (*mover))
    return;
  
  /* Find the first unmoved element (to insert before).  We're
     guaranteed this will terminate, since MOVER itself is currently
     unmoved, and it must be in L (by hypothesis).  */
  for (last_moved = NULL, unmoved = *l; STR_LLIST_MOVED (*unmoved);
       last_moved = unmoved, unmoved = STR_LLIST_NEXT (*unmoved))
    ;

  /* If we are the first unmoved element, nothing to relink.  */
  if (unmoved != mover)
    { /* Remember `mover's current successor, so we can relink `mover's
         predecessor to it.  */
      str_llist_elt_type *before_mover;
      str_llist_elt_type *after_mover = STR_LLIST_NEXT (*mover);
      
      /* Find `mover's predecessor.  */
      for (before_mover = unmoved; STR_LLIST_NEXT (*before_mover) != mover;
           before_mover = STR_LLIST_NEXT (*before_mover))
        ;
      
      /* `before_mover' now links to `after_mover'.  */
      STR_LLIST_NEXT (*before_mover) = after_mover;

      /* Insert `mover' before `unmoved' and after `last_moved' (or at
         the head of the list).  */
      STR_LLIST_NEXT (*mover) = unmoved;
      if (!last_moved)
        *l = mover;
      else
        STR_LLIST_NEXT (*last_moved) = mover;
    }

  /* We've moved it.  */
  STR_LLIST_MOVED (*mover) = 1;
}

/* fn.c: arbitrarily long filenames (or just strings).  */

/* /usr/local/lib/texmf/fonts/public/cm/pk/ljfour/cmr10.300pk is 58
   chars, so ASCII `K' seems a good choice. */
#define CHUNK_SIZE 75

fn_type
fn_init (void)
{
  fn_type ret;
  
  FN_ALLOCATED (ret) = FN_LENGTH (ret) = 0;
  FN_STRING (ret) = NULL;
  
  return ret;
}

fn_type
fn_copy0 (const char *s, unsigned len)
{
  fn_type ret;
  
  FN_ALLOCATED (ret) = CHUNK_SIZE > len ? CHUNK_SIZE : len + 1;
  FN_STRING (ret) = (char *) xmalloc (FN_ALLOCATED (ret));
  
  strncpy (FN_STRING (ret), s, len);
  FN_STRING (ret)[len] = 0;
  FN_LENGTH (ret) = len + 1;
  
  return ret;
}

/* Don't think we ever try to free something that might usefully be
   empty, so give fatal error if nothing allocated.  */

void
fn_free (fn_type *f)
{
  assert (FN_STRING (*f) != NULL);
  free (FN_STRING (*f));
  FN_STRING (*f) = NULL;
  FN_ALLOCATED (*f) = 0;
  FN_LENGTH (*f) = 0;
}

/* An arithmetic increase seems more reasonable than geometric.  We
   don't increase the length member since it may be more convenient for
   the caller to add than subtract when appending the stuff that will
   presumably follow.  */

static void
grow (fn_type *f, unsigned len)
{
  while (FN_LENGTH (*f) + len > FN_ALLOCATED (*f))
    {
      FN_ALLOCATED (*f) += CHUNK_SIZE;
      XRETALLOC (FN_STRING (*f), FN_ALLOCATED (*f), char);
    }
}

void
fn_1grow (fn_type *f, char c)
{
  grow (f, 1);
  FN_STRING (*f)[FN_LENGTH (*f)] = c;
  FN_LENGTH (*f)++;
}

void
fn_grow (fn_type *f, void *source, unsigned len)
{
  grow (f, len);
  strncpy (FN_STRING (*f) + FN_LENGTH (*f), (char *) source, len);
  FN_LENGTH (*f) += len;
}

void
fn_str_grow (fn_type *f, const char *s)
{
  unsigned more_len = strlen (s);
  grow (f, more_len);
  strcat (FN_STRING (*f), s);
  FN_LENGTH (*f) += more_len;
}

void
fn_shrink_to (fn_type *f, unsigned loc)
{
  assert (FN_LENGTH (*f) > loc);
  FN_STRING (*f)[loc] = 0;
  FN_LENGTH (*f) = loc + 1;
}

/* variable.c: variable expansion.  */

/* Here's the simple one, when a program just wants a value.  */

char *
kpse_var_value (const char *var)
{
  char *ret = getenv (var);

  if (ret)
    ret = kpse_var_expand (ret);

#ifdef KPSE_DEBUG
  if (KPSE_DEBUG_P (KPSE_DEBUG_VARS))
    DEBUGF2("variable: %s = %s\n", var, ret ? ret : "(nil)");
#endif

  return ret;
}

/* We have to keep track of variables being expanded, otherwise
   constructs like TEXINPUTS = $TEXINPUTS result in an infinite loop.
   (Or indirectly recursive variables, etc.)  Our simple solution is to
   add to a list each time an expansion is started, and check the list
   before expanding.  */

typedef struct {
  const char *var;
  int expanding;
} expansion_type;
static expansion_type *expansions; /* The sole variable of this type.  */
static unsigned expansion_len = 0;

static void
expanding (const char *var, int xp)
{
  unsigned e;
  for (e = 0; e < expansion_len; e++) {
    if (STREQ (expansions[e].var, var)) {
      expansions[e].expanding = xp;
      return;
    }
  }

  /* New variable, add it to the list.  */
  expansion_len++;
  XRETALLOC (expansions, expansion_len, expansion_type);
  expansions[expansion_len - 1].var = xstrdup (var);
  expansions[expansion_len - 1].expanding = xp;
}


/* Return whether VAR is currently being expanding.  */

static int
expanding_p (const char *var)
{
  unsigned e;
  for (e = 0; e < expansion_len; e++) {
    if (STREQ (expansions[e].var, var))
      return expansions[e].expanding;
  }
  
  return 0;
}

/* Append the result of value of `var' to EXPANSION, where `var' begins
   at START and ends at END.  If `var' is not set, do not complain.
   This is a subroutine for the more complicated expansion function.  */

static void
expand (fn_type *expansion, const char *start, const char *end)
{
  char *value;
  unsigned len = end - start + 1;
  char *var = (char *) xmalloc (len + 1);
  strncpy (var, start, len);
  var[len] = 0;
  
  if (expanding_p (var)) {
    WARNING1 ("kpathsea: variable `%s' references itself (eventually)", var);
  } else {
    /* Check for an environment variable.  */
    value = getenv (var);

    if (value) {
      expanding (var, 1);
      value = kpse_var_expand (value);
      expanding (var, 0);
      fn_grow (expansion, value, strlen (value));
      free (value);
    }

    free (var);
  }
}

/* Can't think of when it would be useful to change these (and the
   diagnostic messages assume them), but ... */
#ifndef IS_VAR_START /* starts all variable references */
#define IS_VAR_START(c) ((c) == '$')
#endif
#ifndef IS_VAR_CHAR  /* variable name constituent */
#define IS_VAR_CHAR(c) (isalnum (c) || (c) == '_')
#endif
#ifndef IS_VAR_BEGIN_DELIMITER /* start delimited variable name (after $) */
#define IS_VAR_BEGIN_DELIMITER(c) ((c) == '{')
#endif
#ifndef IS_VAR_END_DELIMITER
#define IS_VAR_END_DELIMITER(c) ((c) == '}')
#endif


/* Maybe we should support some or all of the various shell ${...}
   constructs, especially ${var-value}.  */

char *
kpse_var_expand (const char *src)
{
  const char *s;
  char *ret;
  fn_type expansion;
  expansion = fn_init ();
  
  /* Copy everything but variable constructs.  */
  for (s = src; *s; s++) {
    if (IS_VAR_START (*s)) {
      s++;

      /* Three cases: `$VAR', `${VAR}', `$<anything-else>'.  */
      if (IS_VAR_CHAR (*s)) {
        /* $V: collect name constituents, then expand.  */
        const char *var_end = s;

        do {
          var_end++;
        } while (IS_VAR_CHAR (*var_end));

        var_end--; /* had to go one past */
        expand (&expansion, s, var_end);
        s = var_end;

      } else if (IS_VAR_BEGIN_DELIMITER (*s)) {
        /* ${: scan ahead for matching delimiter, then expand.  */
        const char *var_end = ++s;

        while (*var_end && !IS_VAR_END_DELIMITER (*var_end))
          var_end++;

        if (! *var_end) {
          WARNING1 ("%s: No matching } for ${", src);
          s = var_end - 1; /* will incr to null at top of loop */
        } else {
          expand (&expansion, s, var_end - 1);
          s = var_end; /* will incr past } at top of loop*/
        }

      } else {
        /* $<something-else>: error.  */
        WARNING2 ("%s: Unrecognized variable construct `$%c'", src, *s);
        /* Just ignore those chars and keep going.  */
      }
    } else
     fn_1grow (&expansion, *s);
  }
  fn_1grow (&expansion, 0);
          
  ret = FN_STRING (expansion);
  return ret;
}
