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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if defined (WITH_SHL)
#include <cerrno>
#include <cstring>
#endif

#include <strstream.h>

extern "C"
{
#if defined (WITH_DL)
#include <dlfcn.h>
#elif defined (WITH_SHL)
#include <dl.h>
#elif defined (WITH_DLD)
#include <dld/dld.h>
#endif
}

#include "defaults.h"
#include "dirfns.h"
#include "dynamic-ld.h"
#include "error.h"
#include "toplev.h"
#include "pathsearch.h"
#include "tree-const.h"
#include "user-prefs.h"
#include "utils.h"
#include "variables.h"

typedef builtin_function * (*Octave_builtin_fcn_struct_fcn)(void);

#if defined (WITH_DYNAMIC_LINKING)

// XXX FIXME XXX -- need to provide some way to ensure that functions
// that we are going to use will use the same naming convention as
// Octave's internal functions.  It needs to be simpler than the
// current DEFUN_DLD() macro, which assumes you know how to name the
// function, the struct, and the helper function.

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

#endif

#if defined (WITH_DL)

static void *
dl_resolve_octave_reference (const char *name, const char *file)
{
  void *retval = 0;

  // Dynamic linking with dlopen/dlsym doesn't require specification
  // of the libraries at runtime.  Instead, they are specified when
  // the .oct file is created.

  void *handle = dlopen (file, RTLD_LAZY);

  if (handle)
    {
      retval = dlsym (handle, name);

      if (! retval)
	{
	  const char *errmsg = dlerror ();

	  if (errmsg)
	    error("%s: `%s'", name, errmsg);
	  else
	    error("unable to link function `%s'", name);

	  dlclose (handle);
	}
    }
  else
    error ("%s: %s `%s'", dlerror (), file, name);

  return retval;
}

#elif defined (WITH_SHL)

static void *
shl_resolve_octave_reference (const char *name, const char *file)
{
  void *retval = 0;

  // Dynamic linking with shl_load/shl_findsym doesn't require
  // specification of the libraries at runtime.  Instead, they are
  // specified when the .oct file is created.

  void *handle = shl_load (file, BIND_DEFERRED, 0L);

  if (handle)
    {
      int status = shl_findsym ((shl_t *) &handle, name,
				TYPE_UNDEFINED, retval);

      if (status < 0)
	{
	  const char *errmsg = strerror (errno);

	  if (errmsg)
	    error("%s: `%s'", name, errmsg);
	  else
	    error("unable to link function `%s'", name);

	  retval = 0;
	}
    }
  else
    error ("%s: %s `%s'", strerror (errno), file, name);

  return retval;
}

#elif defined (WITH_DLD)

// Now that we have the code above to do dynamic linking with the
// dlopen/dlsym interface and Linux uses elf, I doubt that this code
// will be used very much.  Maybe it will be able to go away
// eventually.  Consider it unsupported...

// XXX FIXME XXX -- should this list be in a user-level variable,
// with default taken from the environment?

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
dld_resolve_octave_reference (const char *name, const char *file)
{
  dld_create_reference (name);

  if (file)
    {
      if (dld_link (file) != 0)
	{
	  error ("failed to link file `%s'", file);
	  return 0;
	}

      if (dld_function_executable_p (name))
	return (void *) dld_get_func (name);
    }

  // For each library, try to find it in a list of directories, then
  // link to it.  It would have been nice to use the kpathsea
  // functions here too, but calls to them can't be nested as they
  // would need to be here...

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

#endif

static void *
resolve_octave_reference (const char *name, const char *file = 0)
{
#if defined (WITH_DL)

  return dl_resolve_octave_reference (name, file);

#elif defined (WITH_SHL)

  return shl_resolve_octave_reference (name, file);

#elif defined (WITH_DLD)

  return dld_resolve_octave_reference (name, file);

#endif
}

Octave_builtin_fcn
load_octave_builtin (const char *name)
{
  Octave_builtin_fcn retval = 0;

#if defined (WITH_DYNAMIC_LINKING)

  char *mangled_name = mangle_octave_builtin_name (name);

  retval = (Octave_builtin_fcn) resolve_octave_reference (mangled_name);

  delete [] mangled_name;

#endif

  return retval;
}

int
load_octave_oct_file (const char *name)
{
  int retval = 0;

#if defined (WITH_DYNAMIC_LINKING)

  char *oct_file = oct_file_in_path (name);

  if (oct_file)
    {
      char *mangled_name = mangle_octave_oct_file_name (name);

      Octave_builtin_fcn_struct_fcn f =
	(Octave_builtin_fcn_struct_fcn) resolve_octave_reference
	  (mangled_name, oct_file);

      if (f)
	{
	  builtin_function *s = f ();

	  if (s)
	    {
	      install_builtin_function (s);
	      retval = 1;
	    }
	}

      delete [] oct_file;
    }

#else

  (void) name;

#endif

  return retval;
}

void
init_dynamic_linker (void)
{
#if defined (WITH_DLD)

  octave_dld_init ();

#endif
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
