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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <kpathsea/default.h>
#include <kpathsea/expand.h>
#include <kpathsea/pathsearch.h>
#include <kpathsea/progname.h>

#define OCTAVE_KPSE_SKIP_STRUCT_DECLS
#include "oct-kpse.h"
#undef OCTAVE_KPSE_SKIP_STRUCT_DECLS

str_llist_type *
octave_kpse_element_dirs (const char *elt)
{
  return kpse_element_dirs (elt);
}

char *
octave_kpse_path_search (const char *path, const char *name, int must_exist)
{
  return kpse_path_search (path, name, must_exist);
}

char **
octave_kpse_all_path_search (const char *path, const char *name)
{
  return kpse_all_path_search (path, name);
}

void
octave_kpse_set_progname (const char *name)
{
  kpse_set_progname (name);
}

char *
octave_kpse_expand_default (const char *path, const char *dflt)
{
  return kpse_expand_default (path, dflt);
}

char *
octave_kpse_path_expand (const char *path)
{
  return kpse_path_expand (path);
}

char *
octave_kpse_path_element (const char *path)
{
  return kpse_path_element (path);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
