/* xfns.c: All the x* functions from kpathsearch in one file.

Copyright (C) 1992, 93, 94, 95, 96 Free Software Foundation, Inc.
Copyright (C) 1993, 94, 95, 96, 97, 98 Karl Berry.
Copyright (C) 1994, 95, 96, 97 Karl Berry & Olaf Weber.

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

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include <stdio.h>
#include <stdlib.h>

#include "kpse-xfns.h"

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

/* xfseek.c: fseek with error checking.  */

void
xfseek (FILE *f, long offset, int wherefrom, char *filename)
{
  if (fseek (f, offset, wherefrom) < 0)
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

/* xopendir.c: opendir and closedir with error checking.  */

#ifndef WIN32
DIR *
xopendir (char *dirname)
{
  DIR *d = opendir (dirname);

  if (d == NULL)
    FATAL_PERROR (dirname);

  return d;
}
#endif /* not WIN32 */

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

/* xstat.c: stat and (maybe) lstat with error checking.  */

struct stat
xstat (const char *path)
{
  struct stat s;
  
  if (stat (path, &s) != 0)
    FATAL_PERROR (path);
  
  return s;
}

/* If we don't have symbolic links, lstat is the same as stat, and
   a #define is made in the include file.  */

#ifdef S_ISLNK
struct stat
xlstat (const char *path)
{
  struct stat s;
  
  if (lstat (path, &s) != 0)
    FATAL_PERROR (path);
  
  return s;
}
#endif

/* xgetcwd.c: a from-scratch version of getwd.  Ideas from the tcsh 5.20
   source, apparently uncopyrighted.  */

#if ! (defined (HAVE_GETCWD) || defined (HAVE_GETWD))

static void
xchdir (char *dirname)
{
  if (chdir (dirname) != 0)
    FATAL_PERROR (dirname);
}

#endif /* not HAVE_GETCWD && not HAVE_GETWD */

/* Return the pathname of the current directory, or give a fatal error.  */

char *
xgetcwd (void)
{
  /* If the system provides getcwd, use it.  If not, use getwd if
     available.  But provide a way not to use getcwd: on some systems
     getcwd forks, which is expensive and may in fact be impossible for
     large programs like tex.  If your system needs this define and it
     is not detected by configure, let me know.
                                       -- Olaf Weber <infovore@xs4all.nl */
#if defined (HAVE_GETCWD) && !defined (GETCWD_FORKS)
  char *path = (char *) xmalloc (PATH_MAX + 1);
  
  if (getcwd (path, PATH_MAX + 1) == 0)
    {
      fprintf (stderr, "getcwd: %s", path);
      exit (1);
    }
  
  return path;
#elif defined (HAVE_GETWD)
  char *path = (char *) xmalloc (PATH_MAX + 1);
  
  if (getwd (path) == 0)
    {
      fprintf (stderr, "getwd: %s", path);
      exit (1);
    }
  
  return path;
#else /* not HAVE_GETCWD && not HAVE_GETWD */
  struct stat root_stat, cwd_stat;
  char *cwd_path = (char *) xmalloc (2); /* In case we assign "/" below.  */
  
  *cwd_path = 0;
  
  /* Find the inodes of the root and current directories.  */
  root_stat = xstat ("/");
  cwd_stat = xstat (".");

  /* Go up the directory hierarchy until we get to root, prepending each
     directory we pass through to `cwd_path'.  */
  while (!SAME_FILE_P (root_stat, cwd_stat))
    {
      struct dirent *e;
      DIR *parent_dir;
      int found = 0;
      
      xchdir ("..");
      parent_dir = xopendir (".");

      /* Look through the parent directory for the entry with the same
         inode, so we can get its name.  */
      while ((e = readdir (parent_dir)) != NULL && !found)
        {
          struct stat test_stat;
          test_stat = xlstat (e->d_name);
          
          if (SAME_FILE_P (test_stat, cwd_stat))
            {
              /* We've found it.  Prepend the pathname.  */
              char *temp = cwd_path;
              cwd_path = concat3 ("/", e->d_name, cwd_path);
              free (temp);
              
              /* Set up to test the next parent.  */
              cwd_stat = xstat (".");
              
              /* Stop reading this directory.  */
              found = 1;
            }
        }
      if (!found)
        FATAL2 ("No inode %d/device %d in parent directory",
                cwd_stat.st_ino, cwd_stat.st_dev);
      
      xclosedir (parent_dir);
    }
  
  /* If the current directory is the root, cwd_path will be the empty
     string, and we will have not gone through the loop.  */
  if (*cwd_path == 0)
    strcpy (cwd_path, "/");
  else
    /* Go back to where we were.  */
    xchdir (cwd_path);

#ifdef DOSISH
  /* Prepend the drive letter to CWD_PATH, since this technique
     never tells us what the drive is.
 
     Note that on MS-DOS/MS-Windows, the branch that works around
     missing `getwd' will probably only work for DJGPP (which does
     have `getwd'), because only DJGPP reports meaningful
     st_ino numbers.  But someday, somebody might need this...  */
  {
    char drive[3];
    char *temp = cwd_path;

    /* Make the drive letter lower-case, unless it is beyond Z: (yes,
       there ARE such drives, in case of Novell Netware on MS-DOS).  */
    drive[0] = root_stat.st_dev + (root_stat.st_dev < 26 ? 'a' : 'A');
    drive[1] = ':';
    drive[2] = '\0';

    cwd_path = concat (drive, cwd_path);
    free (temp);
  }
#endif

  return cwd_path;
#endif /* not HAVE_GETCWD && not HAVE_GETWD */
}

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

/* basename.c: return the last element in a path.  */

#ifndef HAVE_BASENAME

/* Return NAME with any leading path stripped off.  This returns a
   pointer into NAME.  For example, `basename ("/foo/bar.baz")'
   returns "bar.baz".  */

const char *
basename (const char *name)
{
  const char *base = NULL;
  unsigned len = strlen (name);
  
  for (len = strlen (name); len > 0; len--) {
    if (IS_DIR_SEP (name[len - 1]) || IS_DEVICE_SEP (name[len - 1])) {
      base = name + len;
      break;
    }
  }

  if (!base)
    base = name;
  
  return base;
}

#endif

char *
xbasename (const char *name)
{
  return (char *) basename (name);
}

/* file-p.c: file predicates.  */

/* Test whether FILENAME1 and FILENAME2 are actually the same file.  If
   stat fails on either of the names, we return false, without error.  */

int
same_file_p (const char *filename1, const char *filename2)
{
    struct stat sb1, sb2;
    /* These are put in variables only so the results can be inspected
       under gdb.  */
    int r1 = stat (filename1, &sb1);
    int r2 = stat (filename2, &sb2);

    return r1 == 0 && r2 == 0 ? SAME_FILE_P (sb1, sb2) : 0;
}

/* dir.c: directory operations.  */

/* Return true if FN is a directory or a symlink to a directory,
   false if not. */

int
dir_p (const char *fn)
{
#ifdef WIN32
  int fa = GetFileAttributes(fn);
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

/* Remove a (KEY, VALUE) pair.  */

void
hash_remove (hash_table_type *table, const char *key, const char *value)
{
  hash_element_type *p;
  hash_element_type *q;
  unsigned n = hash (*table, key);

  /* Find pair.  */
  for (q = NULL, p = table->buckets[n]; p != NULL; q = p, p = p->next)
    if (FILESTRCASEEQ (key, p->key) && STREQ (value, p->value))
      break;
  if (p) {
    /* We found something, remove it from the chain.  */
    if (q) q->next = p->next; else table->buckets[n] = p->next;
    /* We cannot dispose of the contents.  */
    free (p);
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

unsigned kpathsea_debug = 0;

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

/* libc replacement functions for win32.  */

/*
  This does make sense only under WIN32.
  Functions:
    - popen() rewritten
    - pclose() rewritten
    - stat() wrapper for _stat(), removing trailing slashes
  */

#ifdef WIN32

#include <fcntl.h>

/* The X library (among other things) defines `FALSE' and `TRUE', and so
   we only want to define them if necessary, for use by application code.  */
#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif /* FALSE */

struct _popen_elt {
  FILE *f;			/* File stream returned */
  HANDLE hp;			/* Handle of associated process */
  struct _popen_elt *next;	/* Next list element */
};

static struct _popen_elt _z = { NULL, 0, &_z };
static struct _popen_elt *_popen_list = &_z;

FILE *popen (const char *cmd, const char *mode)
{
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  SECURITY_ATTRIBUTES sa = { sizeof(SECURITY_ATTRIBUTES), NULL, TRUE };
  FILE *f = NULL;
  int fno, i;
  HANDLE child_in, child_out;
  HANDLE father_in, father_out;
  HANDLE father_in_dup, father_out_dup;
  HANDLE current_in, current_out;
  HANDLE current_pid;
  int binary_mode;
  char *new_cmd, *app_name = NULL;
  char *p, *q;
  struct _popen_elt *new_process;
  char pname[PATH_MAX], *fp;
  char *suffixes[] = { ".bat", ".cmd", ".com", ".exe", NULL };
  char **s;
  int go_on;

  /* We should look for the application name along the PATH,
     and decide to prepend "%COMSPEC% /c " or not to the command line.
     Do nothing for the moment. */

  /* Another way to do that would be to try CreateProcess first without
     invoking cmd, and look at the error code. If it fails because of
     command not found, try to prepend "cmd /c" to the cmd line.
     */

  /* Look for the application name */
  for (p = cmd; *p && isspace(*p); p++);
  if (*p == '"') {
    q = ++p;
    while(*p && *p != '"') p++;
    if (*p != '\0') {
      fprintf(stderr, "popen: malformed command (\" not terminated)\n");
      return NULL;
    }
  }
  else
    for (q = p; *p && !isspace(*p); p++);
  /* q points to the beginning of appname, p to the last + 1 char */
  if ((app_name = malloc(p - q + 1)) == NULL) {
    fprintf(stderr, "xpopen: malloc(app_name) failed.\n");
    return NULL;
  }
  strncpy(app_name, q, p - q );
  app_name[p - q] = '\0';
  pname[0] = '\0';
#ifdef TRACE
  fprintf(stderr, "popen: app_name = %s\n", app_name);
#endif

  /* Looking for appname on the path */
  for (s = suffixes, go_on = 1; go_on; *s++) {
    if (SearchPath(NULL,	/* Address of search path */
		   app_name,	/* Address of filename */
		   *s,		/* Address of extension */
		   PATH_MAX,	/* Size of destination buffer */
		   pname,	/* Address of destination buffer */
		   &fp)		/* File part of app_name */
      != 0) {
#ifdef TRACE
      fprintf(stderr, "%s found with suffix %s\n", app_name, *s);
#endif
      new_cmd = xstrdup(cmd);
      free(app_name);
      app_name = xstrdup(pname);
      break;
    }
    go_on = (*s != NULL);
  }
  if (go_on == 0) {
    /* the app_name was not found */
#ifdef TRACE
    fprintf(stderr, "%s not found, concatenating comspec\n", app_name);
#endif
    new_cmd = concatn(getenv("COMSPEC"), " /c ", cmd, NULL);
    free(app_name);
    app_name = NULL;
  }
  else {
  }
#ifdef TRACE
  fprintf(stderr, "popen: app_name = %s\n", app_name);
  fprintf(stderr, "popen: cmd_line = %s\n", new_cmd);
#endif

  current_in = GetStdHandle(STD_INPUT_HANDLE);
  current_out = GetStdHandle(STD_OUTPUT_HANDLE);
  current_pid = GetCurrentProcess();
  ZeroMemory( &si, sizeof(STARTUPINFO) );
  si.cb = sizeof(STARTUPINFO);
  si.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
  si.wShowWindow = SW_HIDE;

  if (strchr(mode, 'b'))
    binary_mode = _O_BINARY;
  else
    binary_mode = _O_TEXT;

  /* Opening the pipe for writing */
  if (strchr(mode, 'w')) {
    binary_mode |= _O_WRONLY;
    if (CreatePipe(&child_in, &father_out, &sa, 0) == FALSE) {
      fprintf(stderr, "popen: error CreatePipe\n");
      return NULL;
    }
#if 0
    if (SetStdHandle(STD_INPUT_HANDLE, child_in) == FALSE) {
      fprintf(stderr, "popen: error SetStdHandle child_in\n");
      return NULL;
    }
#endif
    si.hStdInput = child_in;
    si.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    si.hStdError = GetStdHandle(STD_ERROR_HANDLE);

    if (DuplicateHandle(current_pid, father_out, 
			current_pid, &father_out_dup, 
			0, FALSE, DUPLICATE_SAME_ACCESS) == FALSE) {
      fprintf(stderr, "popen: error DuplicateHandle father_out\n");
      return NULL;
    }
    CloseHandle(father_out);
    fno = _open_osfhandle((long)father_out_dup, binary_mode);
    f = _fdopen(fno, mode);
    i = setvbuf( f, NULL, _IONBF, 0 );
  }
  /* Opening the pipe for reading */
  else if (strchr(mode, 'r')) {
    binary_mode |= _O_RDONLY;
    if (CreatePipe(&father_in, &child_out, &sa, 0) == FALSE) {
      fprintf(stderr, "popen: error CreatePipe\n");
      return NULL;
    }
#if 0
    if (SetStdHandle(STD_OUTPUT_HANDLE, child_out) == FALSE) {
      fprintf(stderr, "popen: error SetStdHandle child_out\n");
      return NULL;
    }
#endif
    si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    si.hStdOutput = child_out;
    si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
    if (DuplicateHandle(current_pid, father_in, 
			current_pid, &father_in_dup, 
			0, FALSE, DUPLICATE_SAME_ACCESS) == FALSE) {
      fprintf(stderr, "popen: error DuplicateHandle father_in\n");
      return NULL;
    }
    CloseHandle(father_in);
    fno = _open_osfhandle((long)father_in_dup, binary_mode);
    f = _fdopen(fno, mode);
    i = setvbuf( f, NULL, _IONBF, 0 );
  }
  else {
    fprintf(stderr, "popen: invalid mode %s\n", mode);
    return NULL;
  }

  /* creating child process */
  if (CreateProcess(app_name,	/* pointer to name of executable module */
		    new_cmd,	/* pointer to command line string */
		    NULL,	/* pointer to process security attributes */
		    NULL,	/* pointer to thread security attributes */
		    TRUE,	/* handle inheritance flag */
		    CREATE_NEW_CONSOLE,		/* creation flags */
		    NULL,	/* pointer to environment */
		    NULL,	/* pointer to current directory */
		    &si,	/* pointer to STARTUPINFO */
		    &pi		/* pointer to PROCESS_INFORMATION */
		  ) == FALSE) {
    fprintf(stderr, "popen: CreateProcess %x\n", GetLastError());
    return NULL;
  }
  
#if 0
  /* Restoring saved values for stdin/stdout */
  if (SetStdHandle(STD_INPUT_HANDLE, current_in) == FALSE) 
    fprintf(stderr, "popen: error re-redirecting Stdin\n");  
  if (SetStdHandle(STD_OUTPUT_HANDLE, current_out) == FALSE) 
    fprintf(stderr, "popen: error re-redirecting Stdout\n");  
#endif  
   /* Only the process handle is needed */
  if (CloseHandle(pi.hThread) == FALSE) {
    fprintf(stderr, "popen: error closing thread handle\n");
    return NULL;
  }

  if (new_cmd) free(new_cmd);
  if (app_name) free(app_name);

#if 0
  /* This does not seem to make sense for console apps */
  while (1) {
    i = WaitForInputIdle(pi.hProcess, 5); /* Wait 5ms  */
    if (i == 0xFFFFFFFF) {
      fprintf(stderr, "popen: process can't initialize\n");
      return NULL;
    }
    else if (i == WAIT_TIMEOUT)
      fprintf(stderr, "popen: warning, process still not initialized\n");
    else
      break;
  }
#endif

  /* Add the pair (f, pi.hProcess) to the list */
  if ((new_process = malloc(sizeof(struct _popen_elt))) == NULL) {
    fprintf (stderr, "popen: malloc(new_process) error\n");
    return NULL;
  }
  /* Saving the FILE * pointer, access key for retrieving the process
     handle later on */
  new_process->f = f;
  /* Closing the unnecessary part of the pipe */
  if (strchr(mode, 'r')) {
    CloseHandle(child_out);
  }
  else if (strchr(mode, 'w')) {
    CloseHandle(child_in);
  }
  /* Saving the process handle */
  new_process->hp = pi.hProcess;
  /* Linking it to the list of popen() processes */
  new_process->next = _popen_list;
  _popen_list = new_process;

  return f;

}

int pclose (FILE *f)
{
  struct _popen_elt *p, *q;
  int exit_code;

  /* Look for f is the access key in the linked list */
  for (q = NULL, p = _popen_list; 
       p != &_z && p->f != f; 
       q = p, p = p->next);

  if (p == &_z) {
    fprintf(stderr, "pclose: error, file not found.");
    return -1;
  }

  /* Closing the FILE pointer */
  fclose(f);

  /* Waiting for the process to terminate */
  if (WaitForSingleObject(p->hp, INFINITE) != WAIT_OBJECT_0) {
    fprintf(stderr, "pclose: error, process still active\n");
    return -1;
  }

  /* retrieving the exit code */
  if (GetExitCodeProcess(p->hp, &exit_code) == 0) {
    fprintf(stderr, "pclose: can't get process exit code\n");
    return -1;
  }

  /* Closing the process handle, this will cause the system to
     remove the process from memory */
  if (CloseHandle(p->hp) == FALSE) {
    fprintf(stderr, "pclose: error closing process handle\n");
    return -1;
  }

  /* remove the elt from the list */
  if (q != NULL)
    q->next = p->next;
  else
    _popen_list = p->next;
  free(p);
    
  return exit_code;
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

/* make-suffix.c: unconditionally add a filename suffix.  */

/* Return a new string: S suffixed with SUFFIX, regardless of what it
   was before. This returns a newly allocated string.  */ 

char *
make_suffix (const char *s, const char *suffix)
{
  char *new_s;
  const char *dot_pos = strrchr (s, '.');
  const char *slash_pos;
  
  for (slash_pos = s + strlen (s) - 1; slash_pos > dot_pos && slash_pos > s;
       slash_pos--) {
    if (IS_DIR_SEP (*slash_pos))
      break;
  }

  if (dot_pos == NULL || slash_pos > dot_pos )
    new_s = concat3 (s, ".", suffix);
  else
    {
      unsigned past_dot_index = dot_pos + 1 - s;
      
      new_s = (char *) xmalloc (past_dot_index + strlen (suffix) + 1);
      strncpy (new_s, s, dot_pos + 1 - s);
      strcpy (new_s + past_dot_index, suffix);
    }

  return new_s;
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

#ifdef __DJGPP__
/* `stat' is way too expensive for such a simple job.  */
#define READABLE(fn, st) \
  (access (fn, R_OK) == 0 && access (fn, D_OK) == -1)
#elif WIN32
#define READABLE(fn, st) \
  (GetFileAttributes(fn) != 0xFFFFFFFF && \
   !(GetFileAttributes(fn) & FILE_ATTRIBUTE_DIRECTORY))
#else
#define READABLE(fn, st) \
  (access (fn, R_OK) == 0 && stat (fn, &(st)) == 0 && !S_ISDIR (st.st_mode))
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
#define IS_VAR_CHAR(c) (ISALNUM (c) || (c) == '_')
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
