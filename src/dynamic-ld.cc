/*

Copyright (C) 1996, 1997 John W. Eaton

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
#if defined (HAVE_DLFCN_H)
#include <dlfcn.h>
#else
extern void *dlopen (const char *, int);
extern const char *dlerror (void);
extern void *dlsym (void *, const char *);
extern int dlclose (void *);
#endif
#ifndef RTLD_LAZY
#define RTLD_LAZY 1
#endif
#elif defined (WITH_SHL)
#include <dl.h>
#endif
}

#include <defaults.h>
#include "dirfns.h"
#include "dynamic-ld.h"
#include "error.h"
#include "toplev.h"
#include "pathsearch.h"
#include "oct-obj.h"
#include "ov-builtin.h"
#include "ov.h"
#include "utils.h"
#include "variables.h"

#if defined (WITH_DL)

class
octave_dlopen_dynamic_loader : public octave_dynamic_loader
{
public:

  octave_dlopen_dynamic_loader (void)
    : octave_dynamic_loader () { }

  ~octave_dlopen_dynamic_loader (void) { }

private:

  octave_dynamic_loader::builtin_fcn_installer
  resolve_reference (const string& mangled_name, const string& file);

  // No copying!

  octave_dlopen_dynamic_loader (const octave_dlopen_dynamic_loader&);

  octave_dlopen_dynamic_loader&
  operator = (const octave_dlopen_dynamic_loader&);
};

octave_dynamic_loader::builtin_fcn_installer
octave_dlopen_dynamic_loader::resolve_reference (const string& name,
						 const string& file)
{
  octave_dynamic_loader::builtin_fcn_installer retval = 0;

  // Dynamic linking with dlopen/dlsym doesn't require specification
  // of the libraries at runtime.  Instead, they are specified when
  // the .oct file is created.

  void *handle = dlopen (file.c_str (), RTLD_LAZY);

  const char *nm = name.c_str ();

  if (handle)
    {
      void *tmp = dlsym (handle, nm);

      retval = X_CAST (octave_dynamic_loader::builtin_fcn_installer, tmp);

      if (! retval)
	{
	  const char *errmsg = dlerror ();

	  if (errmsg)
	    error("%s: `%s'", nm, errmsg);
	  else
	    error("unable to link function `%s'", nm);

	  dlclose (handle);
	}
    }
  else
    error ("%s: %s `%s'", dlerror (), file.c_str (), nm);

  return retval;
}

#elif defined (WITH_SHL)

class
octave_shl_load_dynamic_loader : public octave_dynamic_loader
{
public:

  octave_shl_load_dynamic_loader (void)
    : octave_dynamic_loader () { }

  ~octave_shl_load_dynamic_loader (void) { }

private:

  octave_dynamic_loader::builtin_fcn_installer
  resolve_reference (const string& mangled_name, const string& file);

  // No copying!

  octave_shl_load_dynamic_loader (const octave_shl_load_dynamic_loader&);

  octave_shl_load_dynamic_loader&
  operator = (const octave_shl_load_dynamic_loader&);
};

octave_dynamic_loader::builtin_fcn_installer
octave_shl_load_dynamic_loader::resolve_reference (const string& name,
						   const string& file)
{
  octave_dynamic_loader::builtin_fcn_installer retval = 0;

  // Dynamic linking with shl_load/shl_findsym doesn't require
  // specification of the libraries at runtime.  Instead, they are
  // specified when the .oct file is created.

  shl_t handle = shl_load (file.c_str (), BIND_DEFERRED, 0L);

  const char *nm = name.c_str ();

  if (handle)
    {
      // Don't use TYPE_PROCEDURE here.  The man page says that future
      // versions of HP-UX may not support it.

      int status = shl_findsym (&handle, nm, TYPE_UNDEFINED, &retval);

      if (status < 0)
	{
	  const char *errmsg = strerror (errno);

	  if (errmsg)
	    error("%s: `%s'", nm, errmsg);
	  else
	    error("unable to link function `%s'", nm);

	  retval = 0;
	}
    }
  else
    error ("%s: %s `%s'", strerror (errno), file.c_str (), nm);

  return retval;
}

#endif

octave_dynamic_loader *octave_dynamic_loader::instance = 0;

bool
octave_dynamic_loader::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    make_dynamic_loader ();

  if (! instance)
    {
      error ("unable to create command history object!");

      retval = false;
    }

  return retval;
}

void
octave_dynamic_loader::make_dynamic_loader (void)
{
#if defined (WITH_DL)
  instance = new octave_dlopen_dynamic_loader ();
#elif defined (WITH_SHL)
  instance = new octave_shl_load_dynamic_loader ();
#else
  instance = new octave_dynamic_loader ();
#endif
}

bool
octave_dynamic_loader::load_fcn_from_dot_oct_file (const string& fcn_name)
{
  if (! instance_ok ())
    make_dynamic_loader ();

  bool retval = false;

  string oct_file = oct_file_in_path (fcn_name);

  if (! oct_file.empty ())
    {
      string mangled_name = instance->mangle_name (fcn_name);

      builtin_fcn_installer f
	= instance->resolve_reference (mangled_name, oct_file);

      if (f)
	retval = f ();
    }

  return retval;
}

octave_dynamic_loader::builtin_fcn_installer
octave_dynamic_loader::resolve_reference (const string&, const string&)
{
  return 0;
}

string
octave_dynamic_loader::mangle_name (const string& name)
{
#if defined (CXX_PREPENDS_UNDERSCORE)
  string retval ("_FS");
#else
  string retval ("FS");
#endif
  retval.append (name);
  retval.append ("__Fv");
  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
