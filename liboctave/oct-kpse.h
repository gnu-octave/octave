/*

Copyright (C) 2000 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_kpse_h)
#define octave_kpse_h 1

#if !defined (OCTAVE_KPSE_SKIP_STRUCT_DECLS)
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
#endif

#ifdef __cplusplus
extern "C"
{
#endif

extern unsigned int kpathsea_debug;

extern str_llist_type *octave_kpse_element_dirs (const char *);

extern char *octave_kpse_path_search (const char *, const char *, int);

extern char **octave_kpse_all_path_search (const char *, const char *);

extern void octave_kpse_set_progname (const char *);

extern char *octave_kpse_expand_default (const char *, const char *);

extern char *octave_kpse_path_expand (const char *);

extern char *octave_kpse_path_element (const char *);

#ifdef __cplusplus
}
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
