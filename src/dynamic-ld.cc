/*

Copyright (C) 1996 John W. Eaton

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
#ifndef RTLD_LAZY
#define RTLD_LAZY 1
#endif
#elif defined (WITH_SHL)
#include <dl.h>
#endif
}

#include "defaults.h"
#include "dirfns.h"
#include "dynamic-ld.h"
#include "error.h"
#include "toplev.h"
#include "pathsearch.h"
#include "pt-const.h"
#include "utils.h"
#include "variables.h"

typedef builtin_function * (*Octave_builtin_fcn_struct_fcn)(void);

#if defined (WITH_DYNAMIC_LINKING)

// XXX FIXME XXX -- need to provide some way to ensure that functions
// that we are going to use will use the same naming convention as
// Octave's internal functions.  It needs to be simpler than the
// current DEFUN_DLD() macro, which assumes you know how to name the
// function, the struct, and the helper function.

static string
mangle_octave_oct_file_name (const string& name)
{
  string retval ("FS");
  retval.append (name);
  retval.append ("__Fv");
  return retval;
}

#if defined (WITH_DL)

static void *
dl_resolve_octave_reference (const string& name, const string& file)
{
  void *retval = 0;

  // Dynamic linking with dlopen/dlsym doesn't require specification
  // of the libraries at runtime.  Instead, they are specified when
  // the .oct file is created.

  void *handle = dlopen (file.c_str (), RTLD_LAZY);

  if (handle)
    {
      retval = dlsym (handle, name.c_str ());

      if (! retval)
	{
	  const char *errmsg = dlerror ();

	  if (errmsg)
	    error("%s: `%s'", name.c_str (), errmsg);
	  else
	    error("unable to link function `%s'", name.c_str ());

	  dlclose (handle);
	}
    }
  else
    error ("%s: %s `%s'", dlerror (), file.c_str (), name.c_str ());

  return retval;
}

#elif defined (WITH_SHL)

static void *
shl_resolve_octave_reference (const string& name, const string& file)
{
  void *retval = 0;

  // Dynamic linking with shl_load/shl_findsym doesn't require
  // specification of the libraries at runtime.  Instead, they are
  // specified when the .oct file is created.

  void *handle = shl_load (file.c_str (), BIND_DEFERRED, 0L);

  if (handle)
    {
      int status = shl_findsym ((shl_t *) &handle, name.c_str (),
				TYPE_UNDEFINED, retval);

      if (status < 0)
	{
	  const char *errmsg = strerror (errno);

	  if (errmsg)
	    error("%s: `%s'", name.c_str (), errmsg);
	  else
	    error("unable to link function `%s'", name.c_str ());

	  retval = 0;
	}
    }
  else
    error ("%s: %s `%s'", strerror (errno), file.c_str (), name.c_str ());

  return retval;
}

#endif
#endif

#if defined (WITH_DYNAMIC_LINKING)
static void *
resolve_octave_reference (const string& name, const string& file)
{
#if defined (WITH_DL)

  return dl_resolve_octave_reference (name, file);

#elif defined (WITH_SHL)

  return shl_resolve_octave_reference (name, file);

#endif
}
#endif

int
load_octave_oct_file (const string& name)
{
  int retval = 0;

#if defined (WITH_DYNAMIC_LINKING)

  string oct_file = oct_file_in_path (name);

  if (! oct_file.empty ())
    {
      string mangled_name = mangle_octave_oct_file_name (name);

      Octave_builtin_fcn_struct_fcn f =
	(Octave_builtin_fcn_struct_fcn) resolve_octave_reference
	  (mangled_name, oct_file);

      if (f)
	{
	  builtin_function *s = f ();

	  if (s)
	    {
	      install_builtin_function (*s);
	      retval = 1;
	    }
	}
    }

#else

  (void) name;

#endif

  return retval;
}

void
init_dynamic_linker (void)
{
  // Nothing to do anymore...
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
