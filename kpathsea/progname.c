/* progname.c: the executable name we were invoked as; general initialization.

Copyright (C) 1994, 96, 97 Karl Berry.

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

#include <kpathsea/config.h>
#include <kpathsea/absolute.h>
#include <kpathsea/c-pathch.h>
#include <kpathsea/c-stat.h>
#include <kpathsea/pathsearch.h>

#ifdef WIN32
#include <kpathsea/c-pathmx.h>
#endif

/* NeXT does not define the standard macros, but has the equivalent.
   WIN32 doesn't define them either, and doesn't have them.
   From: Gregor Hoffleit <flight@mathi.uni-heidelberg.de>.  */
#ifndef S_IXUSR
#ifdef WIN32
#define S_IXUSR 0
#define S_IXGRP 0
#define S_IXOTH 0
#else /* not WIN32 */
#define S_IXUSR 0100
#endif /* not WIN32 */
#endif /* not S_IXUSR */
#ifndef S_IXGRP
#define S_IXGRP 0010
#endif
#ifndef S_IXOTH
#define S_IXOTH 0001
#endif

#ifndef HAVE_PROGRAM_INVOCATION_NAME
/* Don't redefine the variables if glibc already has.  */
string program_invocation_name;
string program_invocation_short_name;
#endif

/* Return directory for NAME.  This is "." if NAME contains no directory
   separators (should never happen for selfdir), else whatever precedes
   the final directory separator, but with multiple separators stripped.
   For example, `my_dirname ("/foo//bar.baz")' returns "/foo".  Always
   return a new string.  */

static string
my_dirname P1C(const_string, name)
{
  string ret;
  unsigned loc = strlen (name);
  
  for (loc = strlen (name); loc > 0 && !IS_DIR_SEP (name[loc-1]); loc--)
    ;

  if (loc) {
    /* If have ///a, must return /, so don't strip off everything.  */
    unsigned limit = NAME_BEGINS_WITH_DEVICE (name) ? 3 : 1;
    while (loc > limit && IS_DIR_SEP (name[loc-1])
           && !IS_DEVICE_SEP (name[loc-1])) {
      loc--;
    } 
    ret = xmalloc (loc + 1);
    strncpy (ret, name, loc);
    ret[loc] = 0;
  } else {
    /* No directory separators at all, so return the current directory.
       The way this is used in kpathsea, this should never happen.  */
    ret = xstrdup (".");
  }
  
  return ret;
}

#ifndef WIN32
/* From a standalone program `ll' to expand symlinks written by Kimbo Mundy.
   Don't bother to compile if we don't have symlinks; thus we can assume
   / as the separator.  Also don't try to use basename, etc., or
   handle arbitrary filename length.  Mixed case function names because
   that's what kimbo liked.  */

#ifdef S_ISLNK
static int ll_verbose = 0;
static int ll_loop = 0;

#undef BSIZE
#define BSIZE 2048 /* sorry */


/* Read link FN into SYM.  */

static void
ReadSymLink (fn, sym)
     char *fn, *sym;
{
  register int n = readlink (fn, sym, BSIZE);
  if (n < 0) {
    perror (fn);
    exit (1);
  }
  sym[n] = 0;
}


/* Strip first component from S, and also return it in a static buffer.  */

static char *
StripFirst (s)
     register char *s;
{
  static char buf[BSIZE];
  register char *s1;

  /* Find the end of the first path element */
  for (s1 = s; *s1 && (*s1 != '/' || s1 == s); s1++)
    ;

  /* Copy it into buf and null-terminate it. */
  strncpy (buf, s, s1 - s);
  buf[s1 - s] = 0;

  /* Skip over the leading / (if any) */
  if (*s1 == '/')
    ++s1;

  /* Squeeze out the element */
  while ((*s++ = *s1++) != 0)
    ;

  return buf;
}


/* Strip last component from S, and also return it in a static buffer.  */

static char *
StripLast (s)
     register char *s;
{
  static char buf[BSIZE];
  register char *s1;

  for (s1 = s + strlen (s); s1 > s && *s1 != '/'; s1--)
    ;
  strcpy (buf, s1 + (*s1 == '/'));
  *s1 = 0;

  return buf;
}


/* Copy first path element from B to A, removing it from B.  */

static void 
CopyFirst (a, b)
     register char *a;
     char *b;
{
  register int length = strlen (a);

   if (length > 0 && a[length - 1] != '/') {
   a[length] = '/';
    a[length + 1] = 0;
  }
  strcat (a, StripFirst (b));
}

/* Returns NULL on error.  Prints intermediate results if global
   `ll_verbose' is nonzero.  */

#define EX(s)		(strlen (s) && strcmp (s, "/") ? "/" : "")
#define EXPOS		EX(post)
#define EXPRE		EX(pre)

static char *
expand_symlinks (s)
     char *s;
{
  static char pre[BSIZE];	/* return value */
  char post[BSIZE], sym[BSIZE], tmp[BSIZE], before[BSIZE];
  char *cp;
  char a;
  struct stat st;
  int done;

  /* Check for symlink loops.  It's difficult to check for all the
     possibilities ourselves, so let the kernel do it.  And make it
     conditional so that people can see where the infinite loop is
     being caused (see engtools#1536).  */
  if (!ll_loop) {
    FILE *f = fopen (s, "r");
    if (!f && errno == ELOOP) {
      /* Not worried about other errors, we'll get to them in due course.  */
      perror (s);
      return NULL;
    }
    fclose (f);
  }

  strcpy (post, s);
  strcpy (pre, "");

  while (strlen (post) != 0) {
    CopyFirst (pre, post);

    if (lstat (pre, &st) != 0) {
      fprintf (stderr, "lstat(%s) failed ...\n", pre);
      perror (pre);
      return NULL;
    }

    if (S_ISLNK (st.st_mode)) {
      ReadSymLink (pre, sym);

      if (!strncmp (sym, "/", 1)) {
        if (ll_verbose)
          printf ("[%s]%s%s -> [%s]%s%s\n", pre, EXPOS, post, sym, EXPOS,post);
        strcpy (pre, "");

      } else {
        a = pre[0];	/* handle links through the root */
        strcpy (tmp, StripLast (pre));
        if (!strlen (pre) && a == '/')
          strcpy (pre, "/");

        if (ll_verbose) {
          sprintf (before, "%s%s[%s]%s%s", pre, EXPRE, tmp, EXPOS, post);
          printf ("%s -> %s%s[%s]%s%s\n", before, pre, EXPRE, sym, EXPOS,post);
        }

        /* Strip "../" path elements from the front of sym; print
           new result if there were any such elements.  */
        done = 0;
        a = pre[0];	/* handle links through the root */
        while (!strncmp (sym, "..", 2)
               && (sym[2] == 0 || sym[2] == '/')
               && strlen (pre) != 0
               && strcmp (pre, ".")
               && strcmp (pre, "..")
               && (strlen (pre) < 3
                   || strcmp (pre + strlen (pre) - 3, "/.."))) {
          done = 1;
          StripFirst (sym);
          StripLast (pre);
        }

        if (done && ll_verbose) {
          for (cp = before; *cp;)
            *cp++ = ' ';
          if (strlen (sym))
            printf ("%s == %s%s%s%s%s\n", before, pre, EXPRE, sym, EXPOS,post);
          else
            printf ("%s == %s%s%s\n", before, pre, EXPOS, post);
        }
        if (!strlen (pre) && a == '/')
          strcpy (pre, "/");
      }

      if (strlen (post) != 0 && strlen (sym) != 0)
        strcat (sym, "/");

      strcat (sym, post);
      strcpy (post, sym);
    }
  }

  return pre;
}
#else /* not S_ISLNK */
#define expand_symlinks(s) (s)
#endif /* not S_ISLNK */

/* Remove .'s and ..'s in DIR, to avoid problems with relative symlinks
   as the program name, etc.  This does not canonicalize symlinks.  */

static string
remove_dots P1C(string, dir)
{
#ifdef AMIGA
  return dir;
#else
  string c;
  unsigned len;
  string ret = (string) ""; /* We always reassign.  */
  
  for (c = kpse_filename_component (dir); c;
       c = kpse_filename_component (NULL)) {
    if (STREQ (c, ".")) {
      /* If leading ., replace with cwd.  Else ignore.  */
      if (*ret == 0) {
        ret = xgetcwd ();
      }

    } else if (STREQ (c, "..")) {
      /* If leading .., start with my_dirname (cwd).  Else remove last
         component from ret, if any.  */
      if (*ret == 0) {
        string dot = xgetcwd ();
        ret = my_dirname (dot);
        free (dot);
      } else {
        unsigned last;
        for (last = strlen (ret);
             last > (NAME_BEGINS_WITH_DEVICE (ret) ? 2 : 0);
             last--) {
          if (IS_DIR_SEP (ret[last - 1])) {
            /* If we have `/../', that's the same as `/'.  */
            if (last > 1) {
              ret[last] = 0;
            }
            break;
          }
        }
      }

    } else {
      /* Not . or ..; just append.  Include a directory separator unless
         our string already ends with one.  This also changes all directory
         separators into the canonical DIR_SEP_STRING.  */
      string temp;
      len = strlen (ret);
      temp = concat3 (ret, ((len > 0 && ret[len - 1] == DIR_SEP)
                            || (NAME_BEGINS_WITH_DEVICE (c) && *ret == 0))
                           ? "" : DIR_SEP_STRING,
                      c);
      if (*ret)
        free (ret);
      ret = temp;
    }
  }
  
  /* Remove a trailing /, just in case it snuck in.  */
  len = strlen (ret);
  if (len > 0 && ret[len - 1] == DIR_SEP) {
    ret[len - 1] = 0;
  }

  return ret;
#endif /* not AMIGA */
}

/* Return directory ARGV0 comes from.  Check PATH if ARGV0 is not
   absolute.  */

static string
selfdir P1C(const_string, argv0)
{
  string ret = NULL;
  string self = NULL;
  
  if (kpse_absolute_p (argv0, true)) {
    self = xstrdup (argv0);
  } else {
#ifdef AMIGA
#include <dos.h>
#include <proto/dos.h>
#include <proto/exec.h>
    BPTR lock;
    struct DosLibrary *DOSBase
      = (struct DosLibrary *) OpenLibrary ("dos.library", 0L);
    assert (DOSBase);

    self = xmalloc (BUFSIZ);
    lock = findpath (argv0);
    if (lock != ((BPTR) -1)) {
      if (getpath (lock, self) == -1)
        *self = '\0';
      UnLock (lock);
    }
    CloseLibrary((struct Library *) DOSBase);
#else /* not AMIGA */
    string elt;
    struct stat s;

    /* Have to check PATH.  But don't call kpse_path_search since we don't
       want to search any ls-R's or do anything special with //'s.  */
    for (elt = kpse_path_element (getenv ("PATH")); !self && elt;
         elt = kpse_path_element (NULL)) {
      string name = concat3 (elt, DIR_SEP_STRING, argv0);

      /* In order to do this perfectly, we'd have to check the owner bits only
         if we are the file owner, and the group bits only if we belong
         to the file group.  That's a lot of work, though, and it's not
         likely that kpathsea will ever be used with a program that's
         only executable by some classes and not others.  See the
         `file_status' function in execute_cmd.c in bash for what's
         necessary if we were to do it right.  */
      if (stat (name, &s) == 0 && s.st_mode & (S_IXUSR|S_IXGRP|S_IXOTH)) {
        self = name;
      }
    }
#endif /* not AMIGA */
  }
  
  /* If argv0 is somehow dir/exename, `self' will still be NULL.  */
  if (!self)
    self = concat3 (".", DIR_SEP_STRING, argv0);
    
  ret = my_dirname (remove_dots (expand_symlinks (self)));

  free (self);
  
  return ret;
}
#endif /* not WIN32 */

void
kpse_set_progname P1C(const_string, progname)
{
  string ext, sdir, sdir_parent, sdir_grandparent;
  string s = getenv ("KPATHSEA_DEBUG");
  
  /* Set debugging stuff first, in case we end up doing debuggable stuff
     during this initialization.  */
  if (s) {
    kpathsea_debug |= atoi (s);
  }

#ifdef WIN32
  /* Win95 always gives the short filename for argv0, not the long one.
     There is only this way to catch it. It makes all the selfdir stuff
     useless for win32. */
  char path[PATH_MAX], *fp;
  HANDLE hnd;
  WIN32_FIND_DATA ffd;

  /* SearchPath() always give back an absolute directory */
  if (SearchPath(NULL, progname, ".exe", PATH_MAX, path, 
		 &fp) == 0)
    FATAL1("Can't determine where is the executable %s.\n", progname);
  if ((hnd = FindFirstFile(path, &ffd)) == NULL)
    FATAL1("The following path points to an invalid file : %s\n", path);
  progname = ffd.cFileName;
  /* slashify the dirname */
  assert(fp != NULL);
  *fp = '\0';
  for (fp = path; fp && *fp; fp++)
    if (IS_DIR_SEP(*fp)) *fp = DIR_SEP;
  /* sdir will be the directory where the executable resides, ie: c:/TeX/bin */
  sdir = my_dirname(path);
#endif /* WIN32 */

#ifndef HAVE_PROGRAM_INVOCATION_NAME
  program_invocation_name = xstrdup (progname);
#endif

  /* If configured --enable-shared and running from the build directory
     with the wrapper scripts (e.g., for make check), the binaries will
     be named foo.exe instead of foo.  Or possibly if we're running on a
     DOSISH system.  Although it's theoretically wrong to make
     `program_invocation_name' be anything but the real name, as a
     practical matter it seems simpler to strip off the .exe here.  Both
     cnf.c and texmfmp.c (and who knows what else in the future) would
     have to check for it otherwise.  */
  ext = find_suffix (program_invocation_name);
  if (ext && FILESTRCASEEQ (ext, "exe")) {
    program_invocation_name = remove_suffix (program_invocation_name);
  }

#ifndef HAVE_PROGRAM_INVOCATION_NAME
  program_invocation_short_name = (string) basename (program_invocation_name);
#endif

  /* SELFAUTODIR is actually the parent of the invocation directory,
     and SELFAUTOPARENT the grandparent.  This is how teTeX did it.  */
#ifndef WIN32
  sdir = selfdir (program_invocation_name);
#endif
  xputenv ("SELFAUTOLOC", sdir);
  sdir_parent = my_dirname (sdir);
  xputenv ("SELFAUTODIR", sdir_parent);
  sdir_grandparent = my_dirname (sdir_parent);
  xputenv ("SELFAUTOPARENT", sdir_grandparent);

  free (sdir);
  free (sdir_parent);
  free (sdir_grandparent);
}

#ifdef TEST
void
main (int argc, char **argv)
{
  puts (remove_dots ("/w/kpathsea"));
  puts (remove_dots ("/w//kpathsea"));
  puts (remove_dots ("/w/./kpathsea"));
  puts (remove_dots ("."));
  puts (remove_dots ("./"));
  puts (remove_dots ("./."));
  puts (remove_dots ("../kpathsea"));
  puts (remove_dots ("/../w/kpathsea"));
  puts (remove_dots ("/../w/kpathsea/."));
}
/*
Local variables:
standalone-compile-command: "gcc -g -I. -I.. -DTEST progname.c STATIC/libkpathsea.a"
End:
*/
#endif /* TEST */
