// dynamic-ld.cc                                         -*- C++ -*-
/*

Copyright (C) 1993, 1994, 1995 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <strstream.h>

extern "C"
{
#ifdef WITH_DLD
#include <dld/dld.h>
#endif
}

#include "pathsearch.h"
#include "dynamic-ld.h"
#include "tree-const.h"
#include "user-prefs.h"
#include "variables.h"
#include "defaults.h"
#include "dirfns.h"
#include "octave.h"
#include "utils.h"
#include "error.h"

typedef builtin_function * (*Octave_builtin_fcn_struct_fcn)(void);

// XXX FIXME XXX -- should this list be in a user-level variable,
// with default taken from the environment?

#ifdef WITH_DLD

#ifndef STD_LIB_PATH
#define STD_LIB_PATH "/lib:/usr/lib:/usr/local/lib"
#endif

#ifndef OCTAVE_LIB_PATH
#define OCTAVE_LIB_PATH OCTAVE_LIBDIR ":" FLIB_PATH ":" CXXLIB_PATH 
#endif

static char *lib_dir_path = OCTAVE_LIB_PATH ":" STD_LIB_PATH;

// This is the list of interesting libraries that Octave is linked
// with.  Maybe it should include the readline, info, and kpathsea
// libraries.  Would there ever be a time that they would really be
// needed?

#ifndef SYSTEM_LIB_LIST
#define SYSTEM_LIB_LIST "libtermcap.a:libm.a" ":" CXXLIB_LIST
#endif

#ifndef OCTAVE_LIB_LIST
#define OCTAVE_LIB_LIST "liboctdld.a:liboctave.a:libcruft.a:libdld.a"
#endif

static char *lib_list = OCTAVE_LIB_LIST ":" FLIB_LIST ":" SYSTEM_LIB_LIST;

static char *
mangle_octave_builtin_name (const char *name)
{
  char *tmp = strconcat (name, "__FRC13Octave_objecti");
  char *retval = strconcat ("F", tmp);
  delete [] tmp;
  return retval;
}

static char *
mangle_octave_oct_file_name (const char *name)
{
  char *tmp = strconcat (name, "__Fv");
  char *retval = strconcat ("FS", tmp);
  delete [] tmp;
  return retval;
}

static void
octave_dld_init (void)
{
  static int initialized = 0;

  if (! initialized)
    {
      char *full_path = 0;

      char *tmp = dld_find_executable (raw_prog_name);
      if (tmp)
	{
	  full_path = make_absolute (tmp, the_current_working_directory);
	  free (tmp);
	}

      if (full_path)
	{
	  int status = dld_init (full_path);

	  if (status != 0)
	    error ("failed to load symbols from `%s'", full_path);
	  else
	    initialized = 1;
	}
      else
	{
	  error ("octave_dld_init: can't find full path to `%s'",
		 raw_prog_name);
	}
    }
}

static void
octave_list_undefined_symbols (ostream& os)
{
  char **list = dld_list_undefined_sym ();

  if (list)
    {
      os << "undefined symbols:\n\n";
      for (int i = 0; i < dld_undefined_sym_count; i++)
	os << list[i] << "\n";
      os << "\n";
    }
}

static void *
dld_octave_resolve_reference (const char *name, const char *file = 0)
{
  dld_create_reference (name);

  if (file)
    {
      if (dld_link (file) != 0)
	{
	  error ("failed to link file %s", file);
	  return 0;
	}

      if (dld_function_executable_p (name))
	return (void *) dld_get_func (name);
    }

// For each library, try to find it in a list of directories, then
// link to it.  It would have been nice to use the kpathsea functions
// here too, but calls to them can't be nested as they would need to
// be here...

  char **libs = pathstring_to_vector (lib_list);
  char **ptr = libs;
  char *lib_list_elt;

  while ((lib_list_elt = *ptr++))
    {
      char *lib = kpse_path_search (lib_dir_path, lib_list_elt,
				    kpathsea_true);

      if (lib && dld_link (lib) != 0)
	{
	  error ("failed to link library %s", lib);
	  return 0;
	}

      if (dld_function_executable_p (name))
	return (void *) dld_get_func (name);
    }

// If we get here, there was a problem.

  ostrstream output_buf;
  octave_list_undefined_symbols (output_buf);
  char *msg = output_buf.str ();
  error (msg);
  delete [] msg;

  return 0;
}

static Octave_builtin_fcn
dld_octave_builtin (const char *name)
{
  Octave_builtin_fcn retval = 0;

  char *mangled_name = mangle_octave_builtin_name (name);

  retval = (Octave_builtin_fcn) dld_octave_resolve_reference (mangled_name);

  delete [] mangled_name;

  return retval;
}

static int
dld_octave_oct_file (const char *name)
{
  char *oct_file = oct_file_in_path (name);

  if (oct_file)
    {
      char *mangled_name = mangle_octave_oct_file_name (name);

      Octave_builtin_fcn_struct_fcn f =
	(Octave_builtin_fcn_struct_fcn) dld_octave_resolve_reference
	  (mangled_name, oct_file);

      if (f)
	{
	  builtin_function *s = f ();

	  if (s)
	    {
	      install_builtin_function (s);
	      return 1;
	    }
	}

      delete [] oct_file;
    }

  return 0;
}

#endif

Octave_builtin_fcn
load_octave_builtin (const char *name)
{
#ifdef WITH_DLD
  return dld_octave_builtin (name);
#else
  return 0;
#endif
}

int
load_octave_oct_file (const char *name)
{
#ifdef WITH_DLD
  return dld_octave_oct_file (name);
#endif
  return 0;
}

void
init_dynamic_linker (void)
{
#ifdef WITH_DLD
  octave_dld_init ();
#endif
}

// OLD:

#if 0

void
octave_dld_tc2_unlink_by_symbol (const char *name, int hard)
{
// XXX FIXME XXX -- need to determine the name mangling scheme
// automatically, in case it changes, or is different on different
// systems, even if they have g++.

  char *mangled_fcn_name = strconcat (name, "__FRC13Octave_objecti");

  int status = dld_unlink_by_symbol (mangled_fcn_name, hard);

  if (status != 0)
    error ("octave_dld_tc2_unlink_by_symbol: %s", dld_strerror (0));

  delete [] mangled_fcn_name;
}

void
octave_dld_tc2_unlink_by_file (const char *name, int hard)
{
  int status = dld_unlink_by_file (name, hard);

  if (status != 0)
    error ("octave_dld_tc2_unlink_by_file: %s", dld_strerror (0));
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
