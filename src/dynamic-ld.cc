// dynamic-ld.cc                                         -*- C++ -*-
/*

Copyright (C) 1993, 1994 John W. Eaton

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

extern "C"
{
#include "dld/dld.h"
}

#include "dynamic-ld.h"
#include "tree-const.h"
#include "user-prefs.h"
#include "octave.h"
#include "utils.h"
#include "error.h"

void
octave_dld_tc2_unlink_by_symbol (const char *name, int hard = 1)
{
// XXX FIXME XXX -- need to determine the name mangling scheme
// automatically, in case it changes, or is different on different
// systems, even if they have g++.
  char *mangled_fcn_name = strconcat (name, "__FRC13Octave_objecti");
  int status = dld_unlink_by_symbol (mangled_fcn_name, hard);
  if (status != 0)
    dld_perror ("octave_dld_tc2_unlink_by_symbol");
  delete [] mangled_fcn_name;
}

void
octave_dld_tc2_unlink_by_file (const char *name, int hard = 1)
{
  int status = dld_unlink_by_file (name, hard);
  if (status != 0)
    dld_perror ("octave_dld_tc2_unlink_by_file");
}

static void
octave_dld_init (void)
{
  static int initialized = 0;

  if (! initialized)
    {
      char *full_path = dld_find_executable (raw_prog_name);
      if (full_path)
	{
	  int status = dld_init (full_path);
	  if (status != 0)
	    {
	      dld_perror ("octave_dld_tc2_and_go");
	      error ("failed to load symbols from `%s'", full_path);
	    }
	  else
	    initialized = 1;
	}
      else
	error ("octave_dld_tc2_and_go: can't find full path to `%s'",
	       prog_name);
    }
}

/*
 * Look for object in path.  It should provide a definition for the
 * function we just marked as undefined.  If we find it, we\'ll also
 * try to load the remaining undefined symbols.
 */
static int
octave_dld_link (const char *object)
{
  char *file = file_in_path (object, 0);
  int status = dld_link (file);
  if (status != 0)
    dld_perror ("octave_dld_link");
    
  delete [] file;
  return status;
}

int
octave_dld_tc2_link (const char *object)
{
  int status = octave_dld_link (object);
  if (status == 0)
    {
// XXX FIXME XXX -- this obviously won't work everywhere...
      char *octave_lib = "/home/jwe/src/octave/sun4-dld/liboctave.a";
      status = octave_dld_link (octave_lib);
      if (status == 0)
	{
// XXX FIXME XXX -- this obviously won't work everywhere...
	  char *cruft_library = "/home/jwe/src/octave/sun4-dld/libcruft.a";
	  octave_dld_link (cruft_library);
	}
    }
  return status;
}

builtin_fcn_ptr
octave_dld_tc2 (const char *name, const char *fcn)
{
  builtin_fcn_ptr retval = 0;

  octave_dld_init ();

// XXX FIXME XXX -- need to determine the name mangling scheme
// automatically, in case it changes, or is different on different
// systems, even if they have g++.
  char *mangled_fcn_name = strconcat (fcn, "__FRC13Octave_objecti");

// See if the function has already been loaded.  If not, mark it as
// undefined.

  if (dld_get_func (mangled_fcn_name) == 0)
    dld_create_reference (mangled_fcn_name);

// XXX FIXME XXX -- this obviously won't work everywhere...
  char *octave_dld_library = "/home/jwe/src/octave/sun4-dld/liboctdld.a";
  int status = octave_dld_tc2_link (octave_dld_library);
  if (status == 0)
    {
// Return a pointer to the function we just loaded.  If we can\'t find
// it, this will return NULL.

      retval = (builtin_fcn_ptr) dld_get_func (mangled_fcn_name);
    }

  delete [] mangled_fcn_name;

  return retval;
    
}

Octave_object
octave_dld_tc2_and_go (const Octave_object& args, int nargout,
		       const char *name, const char *fcn)
{
  Octave_object retval;

  builtin_fcn_ptr fcn_to_call = octave_dld_tc2 (name, fcn);

  if (fcn_to_call)
    retval = (*fcn_to_call) (args, nargout);
  else
    error ("octave_dld_tc2_and_go: failed to load `%s'", name);

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
