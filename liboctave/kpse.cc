/* pathsearch.c: look up a filename in a path.

Copyright (C) 1993, 94, 95, 96, 97 Karl Berry.
Copyright (C) 1993, 94, 95, 96, 97 Karl Berry & O. Weber.

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

#include <time.h> /* for `time' */

#ifdef __DJGPP__
#include <sys/stat.h>	/* for stat bits */
#endif

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
      log_file = fopen (log_name, FOPEN_A_MODE);
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

#ifdef __DJGPP__
  /* We will use `stat' heavily, so let's request for
     the fastest possible version of `stat', by telling
     it what members of struct stat do we really need.

     We need to set this on each call because this is a
     library function; the caller might need other options
     from `stat'.  Thus save the flags and restore them
     before exit.

     This call tells `stat' that we do NOT need to recognize
     executable files (neither by an extension nor by a magic
     signature); that we do NOT need time stamp of root directories;
     and that we do NOT need the write access bit in st_mode.

     Note that `kpse_set_progname' needs the EXEC bits,
     but it was already called by the time we get here.  */
  unsigned short save_djgpp_flags  = _djstat_flags;

  _djstat_flags = _STAT_EXEC_MAGIC | _STAT_EXEC_EXT
		  | _STAT_ROOT_TIME | _STAT_WRITEBIT;
#endif

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

#ifdef __DJGPP__
  /* Undo any side effects.  */
  _djstat_flags = save_djgpp_flags;
#endif

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

#ifdef __DJGPP__
  /* We will use `stat' heavily, so let's request for
     the fastest possible version of `stat', by telling
     it what members of struct stat do we really need.

     We need to set this on each call because this is a
     library function; the caller might need other options
     from `stat'.  Thus save the flags and restore them
     before exit.

     This call tells `stat' that we do NOT need to recognize
     executable files (neither by an extension nor by a magic
     signature); that we do NOT need time stamp of root directories;
     and that we do NOT need the write access bit in st_mode.

     Note that `kpse_set_progname' needs the EXEC bits,
     but it was already called by the time we get here.  */
  unsigned short save_djgpp_flags  = _djstat_flags;

  _djstat_flags = _STAT_EXEC_MAGIC | _STAT_EXEC_EXT
		  | _STAT_ROOT_TIME | _STAT_WRITEBIT;
#endif

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

#ifdef __DJGPP__
  /* Undo any side effects.  */
  _djstat_flags = save_djgpp_flags;
#endif

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
  FILE *db_file = fopen (db_filename, FOPEN_R_MODE);
  
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
  FILE *alias_file = fopen (alias_filename, FOPEN_R_MODE);

  if (alias_file) {
    while ((line = read_line (alias_file)) != NULL) {
      /* comments or empty */
      if (*line == 0 || *line == '%' || *line == '#') {
        ;
      } else {
        /* Each line should have two fields: realname aliasname.  */
        real = line;
        while (*real && ISSPACE (*real))
          real++;
        alias = real;
        while (*alias && !ISSPACE (*alias))
          alias++;
        *alias++ = 0;
        while (*alias && ISSPACE (*alias)) 
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

