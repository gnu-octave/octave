/* pathsearch.h: mostly-generic path searching.

Copyright (C) 1993, 94, 96, 97 Karl Berry.

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

#ifndef KPATHSEA_PATHSEARCH_H
#define KPATHSEA_PATHSEARCH_H

#include <string>
#include "str-vec.h"

/* It's a little bizarre to be using the same type for the list and the
   elements of the list, but no reason not to in this case, I think --
   we never need a NULL string in the middle of the list, and an extra
   NULL/NULL element always at the end is inconsequential.  */

struct str_llist_elt
{
  std::string str;
  int moved;
  struct str_llist_elt *next;
};
typedef struct str_llist_elt str_llist_elt_type;
typedef struct str_llist_elt *str_llist_type;

#define STR_LLIST(sl) ((sl).str)
#define STR_LLIST_MOVED(sl) ((sl).moved)
#define STR_LLIST_NEXT(sl) ((sl).next)

/* If PATH is non-null, return its first element (as defined by
   IS_ENV_SEP).  If it's NULL, return the next element in the previous
   path, a la strtok.  Leading, trailing, or doubled colons result in
   the empty string.  When at the end of PATH, return NULL.  In any
   case, return a pointer to an area that may be overwritten on
   subsequent calls.  */
extern char *kpse_path_element (const char *path);

/* Like `kpse_path_element', but for filename components (using
   IS_DIR_SEP).  Uses same area as `kpse_path_element'.  */
extern char *kpse_filename_component (const char *path);


/* Given a path element ELT, return a pointer to a NULL-terminated list
   of the corresponding (existing) directory or directories, with
   trailing slashes, or NULL.  If ELT is the empty string, check the
   current working directory.
   
   It's up to the caller to expand ELT.  This is because this routine is
   most likely only useful to be called from `kpse_path_search', which
   has already assumed expansion has been done.  */
extern str_llist_type *kpse_element_dirs (const char *elt);


/* Call `kpse_expand' on NAME.  If the result is an absolute or
   explicitly relative filename, check whether it is a readable
   (regular) file.
   
   Otherwise, look in each of the directories specified in PATH (also do
   tilde and variable expansion on elements in PATH), using a prebuilt
   db (see db.h) if it's relevant for a given path element.
   
   If the prebuilt db doesn't exist, or if MUST_EXIST is true and NAME
   isn't found in the prebuilt db, look on the filesystem.  (I.e., if
   MUST_EXIST is false, and NAME isn't found in the db, do *not* look on
   the filesystem.)
   
   The caller must expand PATH. This is because it makes more sense to
   do this once, in advance, instead of for every search using it.
   
   In any case, return the complete filename if found, otherwise NULL.  */
extern std::string kpse_path_search (const std::string& path,
				     const std::string& name,
				     bool must_exist);


/* Like `kpse_path_search' with MUST_EXIST true, but return a list of
   all the filenames (or NULL if none), instead of taking the first.  */
extern string_vector kpse_all_path_search (const std::string& path,
					   const std::string&  name);

/* Search each element of PATH for each element in the list of NAMES.
   Return the first one found.  */
extern std::string kpse_path_find_first_of (const std::string& path,
					    const string_vector& names,
					    bool must_exist);

/* Like `kpse_path_find_first_of' with MUST_EXIST true, but return a
   list of all the filenames (or NULL if none), instead of taking the
   first.  */
extern string_vector kpse_all_path_find_first_of (const std::string& path,
						  const string_vector& names);

/* expand.h: general expansion.  */

/* Call kpse_var_expand and kpse_tilde_expand (in that order).  Result
   is always in fresh memory, even if no expansions were done.  */
extern std::string kpse_expand (const std::string& s);

/* Do brace expansion and call `kpse_expand' on each element of the
   result; return the final expansion (always in fresh memory, even if
   no expansions were done).  We don't call `kpse_expand_default'
   because there is a whole sequence of defaults to run through; see
   `kpse_init_format'.  */
extern std::string kpse_brace_expand (const char *path);

/* Do brace expansion and call `kpse_expand' on each argument of the
   result, then expand any `//' constructs.  The final expansion (always
   in fresh memory) is a path of all the existing directories that match
   the pattern. */
extern char *kpse_path_expand (const char *path);

/* default.h: Declare default path expander.  */

/* Replace a leading or trailing or doubled : in PATH with DFLT.  If
   no extra colons, return PATH.  Only one extra colon is replaced.
   DFLT may not be NULL.  */

extern char *kpse_expand_default (const char *path, const char *dflt);

/* db.h: lookups in an externally built db file.  */

/* Initialize the database.  Until this is called, no ls-R matches will
   be found.  */
extern void kpse_init_db (void);

/* Return list of matches for NAME in the ls-R file matching PATH_ELT.  If
   ALL is set, return (null-terminated list) of all matches, else just
   the first.  If no matches, return a pointer to an empty list.  If no
   databases can be read, or PATH_ELT is not in any of the databases,
   return NULL.  */
extern string_vector kpse_db_search (const std::string& name,
				     const std::string& path_elt, bool all);

/* Insert the filename FNAME into the database.
   Called by mktexpk et al.  */
extern void kpse_db_insert (const char *fname);

extern unsigned int kpathsea_debug;

#endif /* not KPATHSEA_PATHSEARCH_H */

