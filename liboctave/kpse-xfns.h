/* lib.h: declarations for common, low-level routines in kpathsea.

Copyright (C) 1992, 93, 94, 95, 96, 97 Free Software Foundation, Inc.
Copyright (C) 1993, 94, 95, 96 Karl Berry.
Copyright (C) 1997, 1998 Free Software Foundation, Inc.

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

#ifndef KPATHSEA_LIB_H
#define KPATHSEA_LIB_H

#if defined (__cplusplus)
extern "C" {
#endif

/* c-std.h: the first header files.  */

/* Header files that essentially all of our sources need, and
   that all implementations have.  We include these first, to help with
   NULL being defined multiple times.  */
#include <math.h>
#include <stdio.h>
#include <stdarg.h>

#include <sys/types.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <stdlib.h>

#ifdef WIN32
#include <malloc.h>
#endif /* not WIN32 */

#include <string.h>
#include <errno.h>
#include <assert.h>

/* c-dir.h: directory headers.  */

#ifdef WIN32

#include <direct.h>

#else /* not WIN32 */

/* Use struct dirent instead of struct direct.  */
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#define NAMLEN(dirent) strlen ((dirent)->d_name)
#else /* not DIRENT */
#define dirent direct
#define NAMLEN(dirent) ((dirent)->d_namlen)

#ifdef HAVE_SYS_NDIR_H
#include <sys/ndir.h>
#endif

#ifdef HAVE_SYS_DIR_H
#include <sys/dir.h>
#endif

#ifdef HAVE_NDIR_H
#include <ndir.h>
#endif

#endif /* not DIRENT */

#endif /* not WIN32 */

/* c-fopen.h: how to open files with fopen.  */

/* How to open a text file:  */
#ifndef FOPEN_A_MODE
#define FOPEN_A_MODE "a"
#endif

#ifndef FOPEN_R_MODE
#define FOPEN_R_MODE "r"
#endif

#ifndef FOPEN_W_MODE
#define FOPEN_W_MODE "w"
#endif

/* How to open a binary file for reading:  */
#ifndef FOPEN_RBIN_MODE
#if defined(DOS) || defined (OS2) || defined (WIN32) || defined (__DJGPP__) || defined (__CYGWIN32__)
#define	FOPEN_RBIN_MODE	"rb"
#else
#define	FOPEN_RBIN_MODE	"r"
#endif /* not (DOS or OS2 or WIN32 or __DJGPP__ or __CYGWIN32__) */
#endif /* not FOPEN_RBIN_MODE */

/* How to open a binary file for writing:  */
#ifndef FOPEN_WBIN_MODE
#if defined (DOS) || defined (OS2) || defined (WIN32) || defined (__DJGPP__) || defined (__CYGWIN32__)
#define FOPEN_WBIN_MODE "wb"
#else
#define	FOPEN_WBIN_MODE	"w"
#endif /* not (DOS or OS2 or WIN32 or DJGPP or CYGWIN32) */
#endif /* not FOPEN_WBIN_MODE */

/* How to open a binary file for appending:  */
#ifndef FOPEN_ABIN_MODE
#if defined (DOS) || defined (OS2) || defined (WIN32) || defined (__DJGPP__) || defined (__CYGWIN32__)
#define FOPEN_ABIN_MODE "ab"
#else
#define FOPEN_ABIN_MODE "a"
#endif /* not (DOS or OS2 or WIN32 or DJGPP or CYGWIN32) */
#endif /* not FOPEN_ABIN_MODE */

/* How to switch an already open file handle to binary mode.
   Used on DOSISH systems when we need to switch a standard
   stream, such as stdin or stdout, to binary mode.  */
#include <fcntl.h>
#ifdef DOSISH
#include <io.h>
#ifndef O_BINARY
#ifdef _O_BINARY
#define O_BINARY _O_BINARY
#endif
#endif
#if defined (__DJGPP__) || defined (WIN32) || defined (__CYGWIN32__)
#define SET_BINARY(f) setmode((f), O_BINARY)
#endif
#else  /* not DOSISH */
#ifndef O_BINARY
#define O_BINARY 0
#endif
#define SET_BINARY(f) 0
#endif /* not DOSISH */

/* c-namemx.h: define NAME_MAX, the maximum length of a single
   component in a filename.  No such limit may exist, or may vary
   depending on the filesystem.  */

#include <limits.h>

/* Most likely the system will truncate filenames if it is not POSIX,
   and so we can use the BSD value here.  */
#ifndef _POSIX_NAME_MAX
#define _POSIX_NAME_MAX 255
#endif

#ifndef NAME_MAX
#define NAME_MAX _POSIX_NAME_MAX
#endif

/* c-ctype.h: ASCII-safe versions of the <ctype.h> macros.  */

#include <ctype.h>

/* Be sure we have `isascii'.  */
#ifndef isascii
#define isascii(c) 1
#endif

#define ISALNUM(c) (isascii (c) && isalnum(c))
#define ISALPHA(c) (isascii (c) && isalpha(c))
#define ISASCII isascii
#define ISCNTRL(c) (isascii (c) && iscntrl(c))
#define ISDIGIT(c) (isascii (c) && isdigit (c))
#define ISGRAPH(c) (isascii (c) && isgraph(c))
#define ISLOWER(c) (isascii (c) && islower(c))
#define ISPRINT(c) (isascii (c) && isprint(c))
#define ISPUNCT(c) (isascii (c) && ispunct(c))
#define ISSPACE(c) (isascii (c) && isspace(c))
#define ISUPPER(c) (isascii (c) && isupper(c))
#define ISXDIGIT(c) (isascii (c) && isxdigit(c))
#define TOASCII toascii
#define TOLOWER(c) (ISUPPER (c) ? tolower (c) : (c))
#define TOUPPER(c) (ISLOWER (c) ? toupper (c) : (c))

/* This isn't part of the usual <ctype.h>, but it's useful sometimes.  */
#ifndef isblank
#define isblank(c) ((c) == ' ' || (c) == '\t')
#endif

/* Here's why this mess is necessary:

From: meyering@cs.utexas.edu (Jim Meyering)
Date: Wed, 25 Nov 1992 09:52:33 -0600
Subject: ss-921123: using isascii with <ctype.h> macros

  Yesterday some cursory regression testing found that GNU od
  (in an upcoming release of textutils) generated incorrect output
  when run on an SGI indigo because isprint ('\377') returned true.
  Of course, '\377' is not a printing character;  the problem lay
  in using isprint without first making sure its integer argument
  corresponded to an ascii code.

  MORAL: always guard uses of ctype macros with isascii if it's available.
  An obvious alternative is to avoid <ctype.h> and define and use your
  own versions of the ctype macros.

  A pretty clean approach to using <ctype.h> and isascii was
  suggested by David MacKenzie:

  #ifndef isascii
  #define isascii(c) 1
  #endif

  #define ISDIGIT(c) (isascii (c) && isdigit (c))
  #define ISPRINT(c) (isascii (c) && isprint (c))
  ...

  then, use ISDIGIT, etc. instead of isdigit, etc.  */
  
/* c-pathch.h: define the characters which separate components of
   filenames and environment variable paths.  */

/* What separates filename components?  */
#ifndef DIR_SEP
#ifdef DOSISH
/* Either \'s or 's work.  Wayne Sullivan's web2pc prefers /, so we'll
   go with that.  */
#define DIR_SEP '/'
#define DIR_SEP_STRING "/"
#define IS_DEVICE_SEP(ch) ((ch) == ':')
#define NAME_BEGINS_WITH_DEVICE(name) (*(name) && IS_DEVICE_SEP((name)[1]))
/* On DOS, it's good to allow both \ and / between directories.  */
#define IS_DIR_SEP(ch) ((ch) == '/' || (ch) == '\\')
#else
#define DIR_SEP '/'
#define DIR_SEP_STRING "/"
#endif /* not DOSISH */
#endif /* not DIR_SEP */

#ifndef IS_DIR_SEP
#define IS_DIR_SEP(ch) ((ch) == DIR_SEP)
#endif
#ifndef IS_DEVICE_SEP /* No `devices' on, e.g., Unix.  */
#define IS_DEVICE_SEP(ch) 0 
#endif
#ifndef NAME_BEGINS_WITH_DEVICE
#define NAME_BEGINS_WITH_DEVICE(name) 0 
#endif

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

#include <limits.h>

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

/* c-proto.h: macros to include or discard prototypes.  */

#if !defined(WIN32)
#define DllImport
#define __cdecl
#elif (defined(_DLL) && !defined(_IMPORT)) || !defined(_DLL)
#define DllImport
#else
#define DllImport __declspec(dllimport)
#endif

/* debug.h: Runtime tracing.  */

/* If NO_DEBUG is defined (not recommended), skip all this.  */
#ifndef NO_DEBUG

#if defined(WIN32)
#if defined(_DEBUG)
/* This was needed at some time for catching errors in pdftex. */
#include <crtdbg.h>
#define  SET_CRT_DEBUG_FIELD(a) \
            _CrtSetDbgFlag((a) | _CrtSetDbgFlag(_CRTDBG_REPORT_FLAG))
#define  CLEAR_CRT_DEBUG_FIELD(a) \
            _CrtSetDbgFlag(~(a) & _CrtSetDbgFlag(_CRTDBG_REPORT_FLAG))
#define  SETUP_CRTDBG \
   { _CrtSetReportMode( _CRT_WARN, _CRTDBG_MODE_FILE );    \
     _CrtSetReportFile( _CRT_WARN, _CRTDBG_FILE_STDOUT );  \
     _CrtSetReportMode( _CRT_ERROR, _CRTDBG_MODE_FILE );   \
     _CrtSetReportFile( _CRT_ERROR, _CRTDBG_FILE_STDOUT ); \
     _CrtSetReportMode( _CRT_ASSERT, _CRTDBG_MODE_FILE );  \
     _CrtSetReportFile( _CRT_ASSERT, _CRTDBG_FILE_STDOUT );\
   }
#else /* ! _DEBUG */
#define SET_CRT_DEBUG_FIELD(a) 
#define CLEAR_CRT_DEBUG_FIELD(a)
#define SETUP_CRTDBG
#endif /* _DEBUG */
#endif /* WIN32 */

/* OK, we'll have tracing support.  */
#define KPSE_DEBUG

/* Bit vector defining what we should trace.  */
extern DllImport unsigned kpathsea_debug;

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

/* c-stat.h: declarations for using stat(2).  */

#include <sys/types.h>

/* This is the symbol that X uses to determine if <sys/types.h> has been
   read, so we define it.  */
#define __TYPES__

#include <sys/stat.h>

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
#define alloca  _alloca
#define chdir   _chdir
#define chmod   _chmod
#define close   _close
#define creat   _creat
#define dup     _dup
#define dup2    _dup2
#define execlp  _execlp
#define execvp  _execvp
#define fdopen  _fdopen
#define fileno  _fileno
#define getpid  _getpid
#define getwd(dir)  GetCurrentDirectory(MAXPATHLEN, dir)
#define index   strchr
#define isatty  _isatty
#define itoa    _itoa
#define link    _link
#define lseek   _lseek
#define mkdir   _mkdir
#define mktemp  _mktemp
#define open    _open
#define pipe    _pipe
#if 0
#define popen	_popen
#define pclose	_pclose
#endif
#define read    _read
#define rmdir   _rmdir
#define setmode _setmode
#define spawnlp _spawnlp
#define stat    _stat
#define strcasecmp _stricmp
#define strdup  _strdup
#define strncasecmp _strnicmp
#define unlink  _unlink
#define umask	_umask
#define utime	_utime
#define write   _write

#define S_IFMT   _S_IFMT
#define S_IFDIR  _S_IFDIR
#define S_IFCHR  _S_IFCHR
#define S_IFIFO  _S_IFIFO
#define S_IFREG  _S_IFREG
#define S_IREAD  _S_IREAD
#define S_IWRITE _S_IWRITE
#define S_IEXEC  _S_IEXEC 
#define S_IXUSR  _S_IEXEC
#define S_IXGRP  _S_IEXEC
#define S_IXOTH  _S_IEXEC
#define S_IRUSR  _S_IREAD
#define S_IWUSR  _S_IWRITE
#define O_RDWR   _O_RDWR
#define O_CREAT  _O_CREAT
#define O_TRUNC  _O_TRUNC
#define O_RDONLY _O_RDONLY
#define O_WRONLY _O_WRONLY
#define O_APPEND _O_APPEND
#define O_TEXT   _O_TEXT
#define O_BINARY _O_BINARY

/* Define this so that winsock.h definitions don't get included when
   windows.h is...  For this to have proper effect, config.h must
   always be included before windows.h.  */ 
#define _WINSOCKAPI_    1

#include <windows.h>

/* Defines size_t and alloca ().  */
#include <malloc.h>

/* For proper declaration of environ.  */
#include <stdlib.h>
#include <io.h>
#include <fcntl.h>
#include <stdio.h>
#include <process.h>

/* Web2C takes care of ensuring that these are defined.  */
#ifdef max
#undef max
#undef min
#endif

/* Functions from win32lib.c */
extern FILE *popen(const char *, const char *);
extern int pclose(FILE *);

/* ============================================================ */

#endif /* WIN32 */

/* POSIX predicates for testing file attributes.  */

#if !defined (S_ISBLK) && defined (S_IFBLK)
#define	S_ISBLK(m) (((m) & S_IFMT) == S_IFBLK)
#endif
#if !defined (S_ISCHR) && defined (S_IFCHR)
#define	S_ISCHR(m) (((m) & S_IFMT) == S_IFCHR)
#endif
#if !defined (S_ISDIR) && defined (S_IFDIR)
#define	S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#endif
#if !defined (S_ISREG) && defined (S_IFREG)
#define	S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#endif
#if !defined (S_ISFIFO) && defined (S_IFIFO)
#define	S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)
#endif
#if !defined (S_ISLNK) && defined (S_IFLNK)
#define	S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)
#endif
#if !defined (S_ISSOCK) && defined (S_IFSOCK)
#define	S_ISSOCK(m) (((m) & S_IFMT) == S_IFSOCK)
#endif
#if !defined (S_ISMPB) && defined (S_IFMPB) /* V7 */
#define S_ISMPB(m) (((m) & S_IFMT) == S_IFMPB)
#define S_ISMPC(m) (((m) & S_IFMT) == S_IFMPC)
#endif
#if !defined (S_ISNWK) && defined (S_IFNWK) /* HP/UX */
#define S_ISNWK(m) (((m) & S_IFMT) == S_IFNWK)
#endif

/* readable.h: Is a file readable?  */

/* If NAME is readable and is a regular file, return it.  If the error is
   ENAMETOOLONG, truncate any too-long path components, and if the
   result is a readable file, return that.  Otherwise return NULL.  */
   
extern char *kpse_readable_file (const char *name);

/* absolute.h: Declare absolute filename predicate.  */

extern int kpse_absolute_p (const char *filename, int relative_ok);

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

#ifdef KPSE_DEBUG
/* How to print the hash results when debugging.  */
extern int kpse_debug_hash_lookup_int;
#endif

/* Create a hash table of size SIZE.  */
extern hash_table_type hash_create (unsigned size);

/* Insert the (KEY,VALUE) association into TABLE.  KEY may have more
   than one VALUE.  Neither KEY nor VALUE is copied.  */
extern void hash_insert (hash_table_type *table,  const char *key,
			 const char *value);

/* Remove the (KEY,VALUE) association from TABLE.  */
extern void hash_remove (hash_table_type *table,  const char *key,
			 const char *value);

/* Look up KEY in MAP, and return NULL-terminated list of all matching
   values (not copies), in insertion order.  If none, return NULL.  */
extern char **hash_lookup (hash_table_type table, const char *key);

/* Print TABLE to stderr.  */
extern void hash_print (hash_table_type table, int summary_only);

/* str-list.h: Declarations for string lists.  */

/* Lists of strings; used for, e.g., directory lists.  */

typedef struct
{
  unsigned length;
  char **list;
} str_list_type;

#define STR_LIST_LENGTH(l) ((l).length)
#define STR_LIST(l) ((l).list)
#define STR_LIST_ELT(l, n) STR_LIST (l)[n]
#define STR_LIST_LAST_ELT(l) STR_LIST_ELT (l, STR_LIST_LENGTH (l) - 1)

/* Return a new, empty, list.  */
extern str_list_type str_list_init (void);

/* Append the string S to the list L.  It's up to the caller to not
   deallocate S; we don't copy it.  Also up to the caller to terminate
   the list with a null entry.  */
extern void str_list_add (str_list_type *l, char *s);

/* Append all the elements from MORE to TARGET.  */
extern void str_list_concat (str_list_type * target, str_list_type more);

/* Free the space for the list elements (but not the list elements
   themselves).  */
extern void str_list_free (str_list_type *l);

/* str-llist.h: A linked list of strings,

It's pretty disgusting that both this and str-list exist; the reason is
that C cannot express iterators very well, and I don't want to change
all the for loops right now.  */

/* It's a little bizarre to be using the same type for the list and the
   elements of the list, but no reason not to in this case, I think --
   we never need a NULL string in the middle of the list, and an extra
   NULL/NULL element always at the end is inconsequential.  */

struct str_llist_elt
{
  char *str;
  int moved;
  struct str_llist_elt *next;
};
typedef struct str_llist_elt str_llist_elt_type;
typedef struct str_llist_elt *str_llist_type;

#define STR_LLIST(sl) ((sl).str)
#define STR_LLIST_MOVED(sl) ((sl).moved)
#define STR_LLIST_NEXT(sl) ((sl).next)


/* Add the new string E to the end of the list L.  */
extern void str_llist_add (str_llist_type *l, char *e);

/* Reorganize L so that E is below only other elements that have already
   been moved.  Set `moved' member for E.  */
extern void str_llist_float (str_llist_type *l, str_llist_elt_type *e);

/* xstat.h: stat with error checking.  */

/* Two files are indistinguishable if they are on the same device
   and have the same inode.  This checks two stat buffers for that.  Cf.
   the `same_file_p' routine in file-p.c, declared in kpathlib.h.  */
#define SAME_FILE_P(s1, s2) \
  ((s1).st_ino == (s2).st_ino && (s1).st_dev == (s2).st_dev)

/* Does stat(2) on PATH, and aborts if the stat fails.  */
extern struct stat xstat (const char *path);

/* Ditto, for lstat(2) (except that lstat might not exist).  */
#ifdef S_ISLNK
extern struct stat xlstat (const char *path);
#else
#define xlstat xstat
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


/* Create a new empty fn.  */
extern fn_type fn_init (void);

/* Create a new fn from the first LEN characters from S and a null.  */
extern fn_type fn_copy0 (const char *s,  unsigned len);

/* Free what's been allocated.  Can also just free the string if it's
   been extracted out.  Fatal error if nothing allocated in F.  */
extern void fn_free (fn_type *f);

/* Append the character C to the fn F.  Don't append trailing null.  */
extern void fn_1grow (fn_type *f, char c);

/* Append LENGTH bytes from SOURCE to F.  */
extern void fn_grow (fn_type *f, void *source, unsigned length);

/* Concatenate the component S to the fn F.  Assumes string currently in
   F is null terminated.  */
extern void fn_str_grow (fn_type *f, const char *s);

/* Add a null to F's string at position LOC, and update its length.
   Fatal error if LOC is past the end of the string.  */
extern void fn_shrink_to (fn_type *f, unsigned loc);

/* variable.h: Declare variable expander.  */

/* Return the (variable-expanded) environment variable value or config
   file value, or NULL.  */
extern char *kpse_var_value (const char *var);

/* Expand $VAR and ${VAR} references in SRC, returning the (always newly
   dynamically-allocated) result.  An unterminated ${ or any other
   character following $ produce error messages, and that part of SRC is
   ignored.  In the $VAR form, the variable name consists of consecutive
   letters, digits, and underscores.  In the ${VAR} form, the variable
   name consists of whatever is between the braces.
   
   In any case, ``expansion'' means calling `getenv'; if the variable is not
   set, look in texmf.cnf files for a definition.  If not set there, either,
   the expansion is the empty string (no error).  */
extern char *kpse_var_expand (const char *src);

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

/* Return a fresh copy of S1 followed by S2, et al.  */
extern DllImport char *concat (const char *s1, const char *s2);
extern DllImport char *concat3 (const char *, const char *, const char *);
extern DllImport char *concatn (const char *str1, ...);

/* A fresh copy of just S.  */
extern DllImport char *xstrdup (const char *s);

/* True if FILENAME1 and FILENAME2 are the same file.  If stat fails on
   either name, return false, no error message.
   Cf. `SAME_FILE_P' in xstat.h.  */
extern DllImport int same_file_p (const char *filename1,
				  const char *filename2);

#ifndef HAVE_BASENAME
/* Return NAME with any leading path stripped off.  This returns a
   pointer into NAME.  */
extern DllImport const char *basename (const char *name);
#endif /* not HAVE_BASENAME */

/* If NAME has a suffix, return a pointer to its first character (i.e.,
   the one after the `.'); otherwise, return NULL.  */
extern DllImport char *find_suffix (const char *name);

/* Return NAME with any suffix removed.  */
extern DllImport char *remove_suffix (const char *name);

/* Return S with the suffix SUFFIX, removing any suffix already present.
   For example, `make_suffix ("/foo/bar.baz", "quux")' returns
   `/foo/bar.quux'.  Returns a string allocated with malloc.  */
extern DllImport char *make_suffix (const char *s,  const char *suffix);

/* Return NAME with STEM_PREFIX prepended to the stem. For example,
   `make_prefix ("/foo/bar.baz", "x")' returns `/foo/xbar.baz'.
   Returns a string allocated with malloc.  */
extern DllImport char *make_prefix (char *stem_prefix, char *name);


/* Return the current working directory.  */
extern DllImport char *xgetcwd (void);

/* Returns true if FN is a directory or a symlink to a directory.  */
extern DllImport int dir_p (const char *fn);

/* If FN is a readable directory, return the number of links it has.
   Otherwise, return -1.  */
extern DllImport int dir_links (const char *fn);

/* Like their stdio counterparts, but abort on error, after calling
   perror(3) with FILENAME as its argument.  */
extern DllImport FILE *xfopen (const char *filename, const char *mode);
extern DllImport void xfclose (FILE *, const char *filename);

/* These call the corresponding function in the standard library, and
   abort if those routines fail.  Also, `xrealloc' calls `xmalloc' if
   OLD_ADDRESS is null.  */
extern DllImport void *xmalloc (unsigned size);
extern DllImport void *xrealloc (void *old_address, unsigned new_size);

extern DllImport char *xbasename (const char *name);

/* (Re)Allocate N items of type T using xmalloc/xrealloc.  */
#define XTALLOC(n, t) ((t *) xmalloc ((n) * sizeof (t)))
#define XTALLOC1(t) XTALLOC (1, t)
#define XRETALLOC(addr, n, t) ((addr) = (t *) xrealloc (addr, (n) * sizeof(t)))

#if defined (__cplusplus)
}
#endif

#endif /* not KPATHSEA_LIB_H */
