/*

Copyright (C) 1999 John W. Eaton

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

#include "file-stat.h"
#include "lo-error.h"
#include "oct-shlib.h"
#include "str-vec.h"

class
octave_base_shlib : public octave_shlib
{
public:

  octave_base_shlib (void)
    : octave_shlib (octave_xshlib ()), file (), fcn_names (),
      tm_loaded (static_cast<time_t> (0))
  { count = 1; }

  octave_base_shlib (const string& f)
    : octave_shlib (octave_xshlib ()), file (f), fcn_names (),
      tm_loaded (static_cast<time_t> (0))
  { count = 1; }

  ~octave_base_shlib (void) { }

  void open (const string&, bool = false) { }

  void *search (const string&, name_mangler = 0) { return 0; }

  void close (octave_shlib::close_hook = 0) { }

  bool remove (const string& fcn_name);

  bool is_open (void) const { return false; }

  bool is_out_of_date (void) const;

  int number_of_functions_loaded (void) const { return fcn_names.length (); }

  string file_name (void) const { return file; }

  octave_time time_loaded (void) const { return tm_loaded; }

protected:

  string file;

  string_vector fcn_names;

  octave_time tm_loaded;

  void stamp_time (bool warn_future = false);

  void add_to_fcn_names (const string& name);

  void do_close_hook (octave_shlib::close_hook = 0);

  void tabula_rasa (void);

  // No copying!

  octave_base_shlib (const octave_base_shlib&);

  octave_base_shlib& operator = (const octave_base_shlib&);
};

bool
octave_base_shlib::remove (const string& fcn_name)
{
  bool retval = false;

  int n = number_of_functions_loaded ();

  string_vector new_fcn_names (n);

  int k = 0;

  for (int i = 0; i < n; i++)
    {
      if (fcn_names(i) == fcn_name)
	retval = true;
      else
	new_fcn_names(k++) = fcn_names(i);
    }

  new_fcn_names.resize (k);

  fcn_names = new_fcn_names;

  return retval;
}

bool
octave_base_shlib::is_out_of_date (void) const
{
  file_stat fs (file);

  return fs.is_newer (tm_loaded);
}

void
octave_base_shlib::stamp_time (bool warn_future)
{
  tm_loaded.stamp ();

  if (warn_future)
    {
      file_stat fs (file);

      if (fs.is_newer (tm_loaded))
	(*current_liboctave_warning_handler)
	  ("timestamp on file %s is in the future", file.c_str ());
    }
}

void
octave_base_shlib::add_to_fcn_names (const string& name)
{
  int n = number_of_functions_loaded ();

  for (int i = 0; i < n; i++)
    if (fcn_names(i) == name)
      return;

  fcn_names.resize (n+1);

  fcn_names(n) = name;
}

void
octave_base_shlib::do_close_hook (octave_shlib::close_hook cl_hook)
{
  int n = number_of_functions_loaded ();

  for (int i = 0; i < n; i++)
    cl_hook (fcn_names(i));
}

void
octave_base_shlib::tabula_rasa (void)
{
  file = "";

  fcn_names.resize (0);

  tm_loaded = static_cast<time_t> (0);
}

#if defined (WITH_DL)

class
octave_dlopen_shlib : public octave_base_shlib
{
public:

  octave_dlopen_shlib (void);

  ~octave_dlopen_shlib (void);

  void open (const string& f, bool warn_future = false);

  void *search (const string& name, name_mangler mangler = 0);

  void close (octave_shlib::close_hook cl_hook = 0);

  bool is_open (void) const { return (library != 0); }

private:

  // No copying!

  octave_dlopen_shlib (const octave_dlopen_shlib&);

  octave_dlopen_shlib& operator = (const octave_dlopen_shlib&);

  void *library;
};

octave_dlopen_shlib::octave_dlopen_shlib (void)
  : octave_base_shlib (), library (0)
{
}

octave_dlopen_shlib::~octave_dlopen_shlib (void)
{
  close ();
}

void
octave_dlopen_shlib::open (const string& f, bool warn_future)
{
  if (! is_open ())
    {
      file = f;

      library = dlopen (file.c_str (), RTLD_LAZY);

      if (library)
	stamp_time (warn_future);
      else
	{
	  const char *msg = dlerror ();

	  if (msg)
	    (*current_liboctave_error_handler) ("%s", msg);
	}
    }
  else
    (*current_liboctave_error_handler)
      ("shared library %s is already open", file.c_str ());
}

void *
octave_dlopen_shlib::search (const string& name,
			     octave_shlib::name_mangler mangler)
{
  void *function = 0;

  if (is_open ())
    {
      string sym_name = name;

      if (mangler)
	sym_name = mangler (name);

      function = dlsym (library, sym_name.c_str ());

      if (function)
	add_to_fcn_names (name);
    }
  else
    (*current_liboctave_error_handler)
      ("shared library %s is not open", file.c_str ());

  return function;
}

void
octave_dlopen_shlib::close (octave_shlib::close_hook cl_hook)
{
  if (is_open ())
    {
      do_close_hook (cl_hook);

      dlclose (library);

      library = 0;

      tabula_rasa ();
    }
}

#elif defined (WITH_SHL)

class
octave_shl_load_shlib : public octave_base_shlib
{
public:

  octave_shl_load_shlib (void);

  ~octave_shl_load_shlib (void);

  void open (const string& f, bool warn_future = false);

  void *search (const string& name, name_mangler mangler = 0);

  void close (octave_shlib::close_hook cl_hook = 0);

  bool is_open (void) const { return { library != 0); }

private:

  // No copying!

  octave_shl_load_shlib (const octave_shl_load_shlib&);

  octave_shl_load_shlib& operator = (const octave_shl_load_shlib&);

  shl_t library;
};

octave_shl_load_shlib::octave_shl_load_shlib (void)
  : octave_base_shlib (), library (0)
{
}

octave_shl_load_shlib::~octave_shl_load_shlib (void)
{
  close ();
}

void
octave_shl_load_shlib::open (const string& f, bool warn_future)
{
  if (! is_open ())
    {
      file = f;

      library = shl_load (file.c_str (), BIND_DEFERRED, 0L);

      if (library)
	stamp_time (warn_future);
      else
	(*current_liboctave_error_handler) ("%s", strerror (errno));
    }
  else
    (*current_liboctave_error_handler)
      ("shared library %s is already open", file.c_str ());
}

void *
octave_shl_load_shlib::search (const string& name,
			       octave_shlib::name_mangler mangler)
{
  void *function = 0;

  if (is_open ())
    {
      string sym_name = name;

      if (mangler)
	sym_name = mangler (name);
	
      int status = shl_findsym (&library, sym_name.c_str (),
				TYPE_UNDEFINED, &function);

      if (status == 0)
	add_to_fcn_names (name);
    }
  else
    (*current_liboctave_error_handler)
      ("shared library %s is not open", file.c_str ());

  return function;
}

void
octave_shl_load_shlib::close (octave_shlib::close_hook cl_hook)
{
  if (is_open ())
    {
      do_close_hook (cl_hook);

      shl_unload (library);

      library = 0;

      tabula_rasa ();
    }
}

#endif

octave_shlib *
octave_shlib::make_shlib (void)
{
#if defined (WITH_DL)
  return new octave_dlopen_shlib ();
#elif defined (WITH_SHL)
  return new octave_shl_load_shlib ();
#else
  return new octave_base_shlib ();
#endif
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
