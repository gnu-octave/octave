/* db.c: an external database to avoid filesystem lookups.

Copyright (C) 1994 Karl Berry.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <kpathsea/config.h>

#include <kpathsea/c-fopen.h>
#include <kpathsea/c-pathch.h>
#include <kpathsea/db.h>
#include <kpathsea/hash.h>
#include <kpathsea/line.h>
#include <kpathsea/readable.h>
#include <kpathsea/str-list.h>
#include <kpathsea/variable.h>

/* See comments in `read_files' in cnf.c.  */
string kpse_db_dir = NULL;

/* The hash table for ls-R.  */
static hash_table_type db;


/* If no DB_FILE, return false (maybe they aren't using this feature).
   Otherwise, build the db and return true.  */

static boolean
db_build P1C(hash_table_type *, table)
{
  string line;
  unsigned dir_count = 0, file_count = 0; /* for debugging */
  string cur_dir = NULL; /* First thing in ls-R might be a filename.  */
  string db_filename = concat3 (kpse_db_dir, DIR_SEP_STRING, KPSE_DB_NAME);
  FILE *db_file = fopen (db_filename, FOPEN_R_MODE);
  
  if (db_file)
    {
      while ((line = read_line (db_file)) != NULL)
        {
          unsigned len = strlen (line);
          /* A line like `/foo:' = new dir foo */
          if (IS_DIR_SEP (line[0]) && line[len - 1] == ':')
            {
              cur_dir = xstrdup (line);
              cur_dir[len - 1] = DIR_SEP;
              dir_count++;
            }
          else if (line[0] != 0 && cur_dir) /* other nonblank line */
            { /* New hash table entry with a key of `line' and a data of
                 `cur_dir'.  Already-existing identical keys are ok, since
                 a file named `foo' can be in more than one directory.
                 Since it doesn't hurt, share the directory name string
                 among all the files in the directory. */
              hash_insert (table, xstrdup (line), cur_dir);
              file_count++;
            }
          /* else ignore blank lines */

          free (line);
        }
      xfclose (db_file, db_filename);
      
      if (file_count == 0)
        {
          fprintf (stderr, "kpathsea: no usable entries in %s; see the\n", 
                   db_filename);
          fprintf (stderr, "kpathsea: manual for how to generate ls-R.\n");
        }

#ifdef DEBUG
      if (KPSE_DEBUG_P (KPSE_DEBUG_HASH))
        {
          /* Don't make this a debugging bit, since the output is so
             voluminous, and being able to specify -1 is too useful.
             Instead, let people who want it run the program under
             a debugger and change the variable that way.  */
          boolean print_hash_table = false;

          DEBUGF3 ("%u entries (in %d directories) read from %s.\n",
                   file_count, dir_count, db_filename);
          if (print_hash_table)
            {
              DEBUGF ("Hash table built from ls-R:");
              hash_print (*table);
            }
          fflush (stderr);
        }
#endif
    }

  free (db_filename);
  return db_file != NULL;
}


/* Insert FNAME into the hash table.  This is for files that get built
   during a run.  We wouldn't want to reread all of ls-R, even if it got
   rebuilt.  */

void
db_insert P1C(const_string, passed_fname)
{
  /* We should always have called `kpse_db_search' before this.  */
  assert (db.size > 0);
  
  /* But we might not have found ls-R; in that case, we'll have cleared
     the buckets (but left the size).  */
  if (db.buckets)
    {
      const_string dir_part;
      string fname = xstrdup (passed_fname);
      string baseptr = (string) basename (fname);
      const_string file_part = xstrdup (baseptr);
      
      *baseptr = '\0';  /* Chop off the filename.  */
      dir_part = fname; /* That leaves the dir, with the trailing /.  */

      hash_insert (&db, file_part, dir_part);
    }
}

/* Return true if FILENAME could be in PATH_ELT, i.e., if the directory
   part of FILENAME matches PATH_ELT.  Have to consider // wildcards, but
   $ and ~ expansion have already been done.  */
     
static boolean
match P2C(const_string, filename,  const_string, path_elt)
{
  const_string original_filename = filename;
  boolean matched = false;
  boolean done = false;
  
  for (; !done && *filename && *path_elt; filename++, path_elt++)
    {
      if (*filename == *path_elt) /* normal character match */
        ;

      else if (IS_DIR_SEP (*path_elt)  /* at // */
               && original_filename < filename && IS_DIR_SEP (path_elt[-1]))
        {
          path_elt++; /* get past second / */
          if (*path_elt == 0)
            { /* Have a trailing //, which matches anything. We
                 could make this part of the other case, but it seems
                 pointless to do the extra work.  */
              matched = true;
              done = true;
            }
          else
            { /* intermediate //, have to match rest of PATH_ELT */
              for (; !matched && *filename; filename++)
                { /* Try matching at each possible character.  */
                  if (*filename == *path_elt)
                    matched = match (filename, path_elt);
                }
            }
        }
      
      else /* normal character nonmatch, quit */
        done = true;
    }
  
  /* If we've reached the end of PATH_ELT, and we're at the last
     component of FILENAME, we've matched.  */
  if (!matched && *path_elt == 0 && IS_DIR_SEP (*filename))
    {
      filename++;
      while (*filename && !IS_DIR_SEP (*filename))
        filename++;
      matched = *filename == 0;
    }

  return matched;
}

/* Don't bother implementing a search path for the database itself.  We
   get multiple databases, sort of, with the $TEXMF value for DB_DIR.  */

str_list_type *
kpse_db_search P3C(const_string, name,  const_string, orig_path_elt,
                   boolean, all)
{
  string *db_dirs, *orig_dirs;
  const_string last_slash;
  string path_elt;
  boolean done;
  str_list_type *ret;
  
  /* Hash up the database if this is the first call.  */
  if (db.size == 0)
    {
      db = hash_create (7603); /* What the heck, sparse is ok.  */
      if (!db_build (&db))
        { /* If db can't be built, leave `size' nonzero (so we don't
             rebuild it), but clear `buckets' (so we don't look in it).  */
          free (db.buckets);
          db.buckets = NULL;
        }
    }
  
  /* If we failed to build the database, quit.  */
  if (db.buckets == NULL)
    return NULL;
  
  /* When tex-glyph.c calls us looking for, e.g., dpi600/cmr10.pk, we
     won't find it unless we change NAME to just `cmr10.pk' and append
     `/dpi600' to PATH_ELT.  We are justified in using a literal `/'
     here, since that's what tex-glyph.c unconditionally uses in
     DPI_BITMAP_SPEC.  But don't do anything if the / begins NAME; that
     should never happen.  */
  last_slash = strrchr (name, '/');
  if (last_slash && last_slash != name)
    {
      unsigned len = last_slash - name + 1;
      string dir_part = xmalloc (len);
      strncpy (dir_part, name, len - 1);
      dir_part[len - 1] = 0;
      path_elt = concat3 (orig_path_elt, "/", dir_part);
      name = last_slash + 1;
    }
  else
    path_elt = (string) orig_path_elt;

  /* We have a db.  Look up NAME.  */
  orig_dirs = db_dirs = hash_lookup (db, name);

  done = false;
  ret = XTALLOC1 (str_list_type);
  *ret = str_list_init ();
  
  /* For each filename found, see if it matches the path element.  For
     example, if we have ../cx/cmr10.300pk and .../ricoh/cmr10.300pk,
     and the path looks like .../cx, we don't want the ricoh file.  */
  while (!done && db_dirs && *db_dirs)
    {
      string db_file = concat (*db_dirs, name);
      
      if (match (db_file, path_elt) && kpse_readable_file (db_file))
        {
          str_list_add (ret, db_file);
          if (!all) done = true;
        }
      else
        free (db_file);
      
      /* On to the next directory, if any.  */
      db_dirs++;
    }

  /* This is just the space for the pointers, not the strings.  */
  if (orig_dirs && *orig_dirs) free (orig_dirs);
  
  /* If we had to break up NAME, free the temporary PATH_ELT.  */
  if (path_elt != orig_path_elt) free (path_elt);
  return ret;
}
