/*

Copyright (C) 1999, 2000, 2002, 2003, 2005, 2006, 2007, 2008 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <map>

#if defined (HAVE_SHL_LOAD_API)
#include <cerrno>
#include <cstring>
#endif

#if defined (HAVE_DYLD_API)
#include <mach-o/dyld.h>
#endif

extern "C"
{
#if defined (HAVE_DLOPEN_API)
#if defined (HAVE_DLFCN_H)
#include <dlfcn.h>
#else
extern void *dlopen (const char *, int);
extern const char *dlerror (void);
extern void *dlsym (void *, const char *);
extern int dlclose (void *);
#endif
#elif defined (HAVE_SHL_LOAD_API)
#include <dl.h>
#elif defined (HAVE_LOADLIBRARY_API)
#include <windows.h>
#undef min
#undef max
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

  octave_base_shlib (const std::string& f)
    : octave_shlib (octave_xshlib ()), file (f), fcn_names (),
      tm_loaded (static_cast<time_t> (0))
  { count = 1; }

  ~octave_base_shlib (void) { }

  void open (const std::string&) { }

  // FIXME -- instead of returning a direct pointer to a function,
  // perhaps this should return an object containing the pointer.
  // Then we could do the reference counting without relying on users
  // of this class to call remove at the appropriate time.  However,
  // when users accessed the internal pointer, they would still need
  // to know that the pointer is only valid while the container is
  // valid (similar to the std::string::c_str method).

  void *search (const std::string&, name_mangler = 0) { return 0; }

  void close (octave_shlib::close_hook = 0) { }

  bool remove (const std::string& fcn_name);

  bool is_open (void) const { return false; }

  bool is_out_of_date (void) const;

  size_t number_of_functions_loaded (void) const { return fcn_names.size (); }

  std::string file_name (void) const { return file; }

  octave_time time_loaded (void) const { return tm_loaded; }

protected:

  typedef std::map<std::string, size_t>::iterator fcn_names_iterator;
  typedef std::map<std::string, size_t>::const_iterator fcn_names_const_iterator;

  std::string file;

  std::map<std::string, size_t> fcn_names;

  octave_time tm_loaded;

  void stamp_time (void);

  void add_to_fcn_names (const std::string& name);

  void do_close_hook (octave_shlib::close_hook = 0);

  void tabula_rasa (void);

  // No copying!

  octave_base_shlib (const octave_base_shlib&);

  octave_base_shlib& operator = (const octave_base_shlib&);
};

bool
octave_base_shlib::remove (const std::string& fcn_name)
{
  bool retval = false;

  fcn_names_iterator p = fcn_names.find (fcn_name);

  if (p != fcn_names.end () && --(p->second) == 0)
    {
      fcn_names.erase (fcn_name);
      retval = true;
    }

  return retval;
}

bool
octave_base_shlib::is_out_of_date (void) const
{
  file_stat fs (file);

  return fs.is_newer (tm_loaded);
}

void
octave_base_shlib::stamp_time (void)
{
  tm_loaded.stamp ();

  file_stat fs (file);

  if (fs.is_newer (tm_loaded))
    (*current_liboctave_warning_with_id_handler)
      ("Octave:warn-future-time-stamp",
       "timestamp on file %s is in the future", file.c_str ());
}

void
octave_base_shlib::add_to_fcn_names (const std::string& name)
{
  fcn_names_iterator p = fcn_names.find (name);

  if (p == fcn_names.end ())
    fcn_names[name] = 1;
  else
    ++(p->second);
}

void
octave_base_shlib::do_close_hook (octave_shlib::close_hook cl_hook)
{
  for (fcn_names_iterator p = fcn_names.begin (); p != fcn_names.end (); p++)
    cl_hook (p->first);
}

void
octave_base_shlib::tabula_rasa (void)
{
  file = "";

  fcn_names.clear ();

  tm_loaded = static_cast<time_t> (0);
}

#if defined (HAVE_DLOPEN_API)

class
octave_dlopen_shlib : public octave_base_shlib
{
public:

  octave_dlopen_shlib (void);

  ~octave_dlopen_shlib (void);

  void open (const std::string& f);

  void *search (const std::string& name, name_mangler mangler = 0);

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
octave_dlopen_shlib::open (const std::string& f)
{
  if (! is_open ())
    {
      file = f;

      int flags = 0;

      // Use RTLD_NOW to resolve all symbols before dlopen returns.
      // By using this option, dlopen will detect errors and Octave
      // won't exit if there are unresolved symbols in the file we are
      // loading, and we may even get a useful diagnostic.
#if defined (RTLD_NOW)
      flags |= RTLD_NOW;
#endif

      library = dlopen (file.c_str (), flags);

      if (library)
	stamp_time ();
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
octave_dlopen_shlib::search (const std::string& name,
			     octave_shlib::name_mangler mangler)
{
  void *function = 0;

  if (is_open ())
    {
      std::string sym_name = name;

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
      if (cl_hook)
	do_close_hook (cl_hook);

      dlclose (library);

      library = 0;

      tabula_rasa ();
    }
}

#elif defined (HAVE_SHL_LOAD_API)

class
octave_shl_load_shlib : public octave_base_shlib
{
public:

  octave_shl_load_shlib (void);

  ~octave_shl_load_shlib (void);

  void open (const std::string& f);

  void *search (const std::string& name, name_mangler mangler = 0);

  void close (octave_shlib::close_hook cl_hook = 0);

  bool is_open (void) const { return (library != 0); }

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
octave_shl_load_shlib::open (const std::string& f)
{
  if (! is_open ())
    {
      file = f;

      library = shl_load (file.c_str (), BIND_IMMEDIATE, 0L);

      if (library)
	stamp_time ();
      else
	{
	  using namespace std;
	  (*current_liboctave_error_handler) ("%s", strerror (errno));
	}
    }
  else
    (*current_liboctave_error_handler)
      ("shared library %s is already open", file.c_str ());
}

void *
octave_shl_load_shlib::search (const std::string& name,
			       octave_shlib::name_mangler mangler)
{
  void *function = 0;

  if (is_open ())
    {
      std::string sym_name = name;

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
      if (cl_hook)
	do_close_hook (cl_hook);

      shl_unload (library);

      library = 0;

      tabula_rasa ();
    }
}

#elif defined (HAVE_LOADLIBRARY_API)

class
octave_w32_shlib: public octave_base_shlib
{
public:

  octave_w32_shlib (void);

  ~octave_w32_shlib (void);

  void open (const std::string& f);

  void *search (const std::string& name, name_mangler mangler = 0);

  void close (octave_shlib::close_hook cl_hook = 0);

  bool is_open (void) const { return (handle != 0); }

private:

  // No copying!

  octave_w32_shlib (const octave_w32_shlib&);

  octave_w32_shlib& operator = (const octave_w32_shlib&);

  HINSTANCE handle; 
};

octave_w32_shlib::octave_w32_shlib (void)
  : octave_base_shlib (), handle (0)
{
}

octave_w32_shlib::~octave_w32_shlib (void)
{
  close ();
}

void
octave_w32_shlib::open (const std::string& f)
{
  if (! is_open ())
    {
      file = f;

      handle = LoadLibrary (file.c_str ());

      if (handle)
	stamp_time ();
      else
	{
	  DWORD lastError = GetLastError ();
	  char *msg;

	  switch (lastError)
	    {
	    case ERROR_MOD_NOT_FOUND:
	    case ERROR_DLL_NOT_FOUND:
	      msg = "could not find library or dependents";
	      break;

	    case ERROR_INVALID_DLL:
	      msg = "library or its dependents are damaged";
	      break;

	    case ERROR_DLL_INIT_FAILED:
	      msg = "library initialization routine failed";
	      break;

	    default:
	      msg = "library open failed";
	    }

	  (*current_liboctave_error_handler) ("%s: %s", msg, file.c_str ());
	}
    }
  else
    (*current_liboctave_error_handler)
      ("shared library %s is already open", file.c_str ());
}

extern "C"
{
  void * octave_w32_search (HINSTANCE handle, const char * name);
}

void *
octave_w32_shlib::search (const std::string& name,
			  octave_shlib::name_mangler mangler)
{
  void *function = 0;

  if (is_open ())
    {
      std::string sym_name = name;

      if (mangler)
	sym_name = mangler (name);

      function = octave_w32_library_search (handle, sym_name.c_str ());

      if (function)
	add_to_fcn_names (name);
    }
  else
    (*current_liboctave_error_handler)
      ("shared library %s is not open", file.c_str ());

  return function;
}

void
octave_w32_shlib::close (octave_shlib::close_hook cl_hook)
{
  if (is_open ())
    {
      if (cl_hook)
	do_close_hook (cl_hook);

      FreeLibrary (handle);

      handle = 0;

      tabula_rasa ();
    }
}

#elif defined (HAVE_DYLD_API)

class
octave_dyld_shlib : public octave_base_shlib
{
public:

  octave_dyld_shlib (void);

  ~octave_dyld_shlib (void);

  void open (const std::string& f);

  void *search (const std::string& name, name_mangler mangler = 0);

  void close (octave_shlib::close_hook cl_hook = 0);

  bool is_open (void) const {return (isOpen); }

private:

  // No copying!

  octave_dyld_shlib (const octave_dyld_shlib&);

  octave_dyld_shlib& operator = (const octave_dyld_shlib&);

  bool isOpen;
  NSObjectFileImage img;
  NSModule handle;
};

octave_dyld_shlib::octave_dyld_shlib (void)
  : octave_base_shlib (), isOpen (false), handle (0)
{
}

octave_dyld_shlib::~octave_dyld_shlib (void)
{
  close ();
}

void
octave_dyld_shlib::open (const std::string& f)
{
  int returnCode;

  if (! is_open ())
    {
      file = f;

      returnCode = NSCreateObjectFileImageFromFile (file.c_str (), &img);

      if (NSObjectFileImageSuccess == returnCode)
	{
	  handle = NSLinkModule (img, file.c_str (),
				 (NSLINKMODULE_OPTION_RETURN_ON_ERROR
				  | NSLINKMODULE_OPTION_PRIVATE));
	  if (handle)
	    {
	      stamp_time ();
	      isOpen = true;
	    }
	  else
	    {
	      NSLinkEditErrors ler;
	      int lerno;
	      const char *file2;
	      const char *errstr = 0;

	      NSLinkEditError (&ler, &lerno, &file2, &errstr);

	      if (errstr)
		(*current_liboctave_error_handler)
		  ("%s: %s", file.c_str (), errstr);	
	    }
	}
      else
	{
	  (*current_liboctave_error_handler)
	    ("got NSObjectFileImageReturnCode %d", returnCode);

	  // FIXME -- should use NSLinkEditError () to get
	  // more info on what went wrong.
	}
    }
  else
    {
      (*current_liboctave_error_handler)
	("bundle %s is already open", file.c_str ());
    }
}

void *
octave_dyld_shlib::search (const std::string& name,
			   octave_shlib::name_mangler mangler)
{
  void *function = 0;

  if (is_open ())
    {
      std::string sym_name = name;

      if (mangler)
	sym_name = mangler (name);

      NSSymbol symbol = NSLookupSymbolInModule (handle, sym_name.c_str ());

      if (symbol)
	{
	  function = NSAddressOfSymbol (symbol);
	  add_to_fcn_names (name);
	}
    }
  else
    (*current_liboctave_error_handler)
      ("bundle %s is not open", file.c_str ());

  return function;
}

void
octave_dyld_shlib::close (octave_shlib::close_hook cl_hook)
{
  if (is_open ())
    {
      do_close_hook (cl_hook);

      NSUnLinkModule (handle, NSUNLINKMODULE_OPTION_RESET_LAZY_REFERENCES);

      handle = 0;

      if (NSDestroyObjectFileImage (img))
	isOpen = false;

      tabula_rasa ();
    }
}

#endif

octave_shlib *
octave_shlib::make_shlib (void)
{
#if defined (HAVE_DLOPEN_API)
  return new octave_dlopen_shlib ();
#elif defined (HAVE_SHL_LOAD_API)
  return new octave_shl_load_shlib ();
#elif defined (HAVE_LOADLIBRARY_API)
  return new octave_w32_shlib ();
#elif defined (HAVE_DYLD_API)
  return new octave_dyld_shlib ();
#else
  return new octave_base_shlib ();
#endif
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
