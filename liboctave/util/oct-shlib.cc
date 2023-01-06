////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1999-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <list>
#include <map>

extern "C"
{
#if defined (HAVE_DLOPEN_API)
#  if defined (HAVE_DLFCN_H)
#    include <dlfcn.h>
#  else
  extern void * dlopen (const char *, int);
  extern const char * dlerror (void);
  extern void * dlsym (void *, const char *);
  extern int dlclose (void *);
#  endif
#elif defined (HAVE_LOADLIBRARY_API)
#  define WIN32_LEAN_AND_MEAN 1
#  include <windows.h>
#  include <psapi.h>
#endif
}

#include "file-ops.h"
#include "file-stat.h"
#include "lo-error.h"
#include "oct-shlib.h"
#include "str-vec.h"

#if defined (HAVE_LOADLIBRARY_API)
#  include "lo-sysdep.h"
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

std::list<dynamic_library> possibly_unreferenced_dynamic_libraries;

void dynamic_library::delete_later (void)
{
  possibly_unreferenced_dynamic_libraries.push_back (*this);
}

int release_unreferenced_dynamic_libraries (void)
{
  possibly_unreferenced_dynamic_libraries.clear ();

  return 0;
}

dynamic_library::dynlib_rep::dynlib_rep (const std::string& f)
  : m_count (1), m_fcn_names (), m_file (f), m_time_loaded (),
    m_search_all_loaded (false)
{
  s_instances[f] = this;

  if (is_out_of_date ())
    (*current_liboctave_warning_with_id_handler)
      ("Octave:warn-future-time-stamp",
       "timestamp on file %s is in the future", m_file.c_str ());
}

bool
dynamic_library::dynlib_rep::is_out_of_date (void) const
{
  sys::file_stat fs (m_file);
  return (fs && fs.is_newer (m_time_loaded));
}

void
dynamic_library::dynlib_rep::fake_reload (void)
{
  // We can't actually reload the library, but we'll pretend we did.
  sys::file_stat fs (m_file);
  if (fs && fs.is_newer (m_time_loaded))
    {
      m_time_loaded = fs.mtime ();

      (*current_liboctave_warning_with_id_handler)
        ("Octave:library-reload",
         "library %s not reloaded due to existing references", m_file.c_str ());
    }
}

dynamic_library::dynlib_rep *
dynamic_library::dynlib_rep::get_instance (const std::string& f, bool fake)
{
  dynlib_rep *retval = nullptr;
  std::map<std::string, dynlib_rep *>::iterator p = s_instances.find (f);
  if (p != s_instances.end ())
    {
      retval = p->second;
      retval->m_count++;
      if (fake)
        retval->fake_reload ();
    }
  else
    retval = new_instance (f);

  return retval;
}

std::list<std::string>
dynamic_library::dynlib_rep::function_names (void) const
{
  std::list<std::string> retval;

  for (const auto& p : m_fcn_names)
    retval.push_back (p.first);

  return retval;
}

void
dynamic_library::dynlib_rep::add_fcn_name (const std::string& name)
{
  auto p = m_fcn_names.find (name);

  if (p == m_fcn_names.end ())
    m_fcn_names[name] = 1;
  else
    ++(p->second);
}

bool
dynamic_library::dynlib_rep::remove_fcn_name (const std::string& fcn_name)
{
  bool retval = false;

  auto p = m_fcn_names.find (fcn_name);

  if (p != m_fcn_names.end () && --(p->second) == 0)
    {
      m_fcn_names.erase (fcn_name);
      retval = true;
    }

  return retval;
}

std::map<std::string, dynamic_library::dynlib_rep *>
dynamic_library::dynlib_rep::s_instances;

dynamic_library::dynlib_rep dynamic_library::s_nil_rep;

#if defined (HAVE_DLOPEN_API)

class
octave_dlopen_shlib : public dynamic_library::dynlib_rep
{
public:

  octave_dlopen_shlib (const std::string& f);

  // No copying!

  octave_dlopen_shlib (const octave_dlopen_shlib&) = delete;

  octave_dlopen_shlib& operator = (const octave_dlopen_shlib&) = delete;

  ~octave_dlopen_shlib (void);

  void * search (const std::string& name,
                const dynamic_library::name_mangler& mangler
                = dynamic_library::name_mangler ());

  // FIXME: this is possibly redundant because failure to open a library will
  // normally throw an exception, avoiding the construction of an invalid
  // library.  Leave it here for possible future use.

  bool is_open (void) const
  {
    return (m_search_all_loaded || m_library != nullptr);
  }

private:

  void *m_library;
};

octave_dlopen_shlib::octave_dlopen_shlib (const std::string& f)
  : dynamic_library::dynlib_rep (f), m_library (nullptr)
{
  int flags = 0;

  // Use RTLD_NOW to resolve all symbols before dlopen returns.
  // By using this option, dlopen will detect errors and Octave
  // won't exit if there are unresolved symbols in the file we are
  // loading, and we may even get a useful diagnostic.
#  if defined (RTLD_NOW)
  flags |= RTLD_NOW;
#  endif

  // Use RTLD_GLOBAL to export symbols from loaded objects so they are
  // available to other subsequently loaded libraries.
#  if defined (RTLD_GLOBAL)
  flags |= RTLD_GLOBAL;
#  endif

  if (m_file.empty ())
    {
      m_search_all_loaded = true;
      return;
    }

  m_library = dlopen (m_file.c_str (), flags);

  if (! m_library)
    {
      const char *msg = dlerror ();

      if (msg)
        (*current_liboctave_error_handler)
          ("%s: failed to load\nIncompatible version or missing dependency?"
           "\n%s", m_file.c_str (), msg);
      else
        (*current_liboctave_error_handler)
          ("%s: failed to load\nIncompatible version or missing dependency?",
           m_file.c_str ());
    }
}

octave_dlopen_shlib::~octave_dlopen_shlib (void)
{
  if (m_library)
    dlclose (m_library);
}

void *
octave_dlopen_shlib::search (const std::string& name,
                             const dynamic_library::name_mangler& mangler)
{
  void *function = nullptr;

  if (! is_open ())
    (*current_liboctave_error_handler)
      ("shared library %s is not open", m_file.c_str ());

  std::string sym_name = name;

  if (mangler)
    sym_name = mangler (name);

  if (m_search_all_loaded)
    function = dlsym (RTLD_DEFAULT, sym_name.c_str ());
  else
    function = dlsym (m_library, sym_name.c_str ());

  return function;
}

#elif defined (HAVE_LOADLIBRARY_API)

class
octave_w32_shlib: public dynamic_library::dynlib_rep
{
public:

  octave_w32_shlib (const std::string& f);

  // No copying!

  octave_w32_shlib (const octave_w32_shlib&) = delete;

  octave_w32_shlib& operator = (const octave_w32_shlib&) = delete;

  ~octave_w32_shlib (void);

  void * search (const std::string& name,
                const dynamic_library::name_mangler& mangler
                = dynamic_library::name_mangler ());

  void * global_search (const std::string& sym_name);

  bool is_open (void) const
  {
    return (m_search_all_loaded || m_handle != nullptr);
  }

private:

  HINSTANCE m_handle;
};

octave_w32_shlib::octave_w32_shlib (const std::string& f)
  : dynamic_library::dynlib_rep (f), m_handle (nullptr)
{
  if (f.empty())
    {
      m_search_all_loaded = true;
      return;
    }

  std::string dir = sys::file_ops::dirname (f);
  std::wstring wdir = sys::u8_to_wstring (dir);
  SetDllDirectoryW (dir.empty ()
                    ? nullptr : wdir.c_str ());

  std::wstring wfile = sys::u8_to_wstring (m_file);
  m_handle = LoadLibraryW (wfile.c_str ());

  SetDllDirectoryW (nullptr);

  if (! m_handle)
    {
      DWORD last_error = GetLastError ();

      wchar_t *error_text = nullptr;
      FormatMessageW (FORMAT_MESSAGE_FROM_SYSTEM |
                      FORMAT_MESSAGE_ALLOCATE_BUFFER |
                      FORMAT_MESSAGE_IGNORE_INSERTS,
                      nullptr, last_error,
                      MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),
                      reinterpret_cast <wchar_t *> (&error_text), 0, nullptr);

      std::ostringstream err_str;
      err_str << "opening the library '" << m_file << "' failed (error "
              << last_error << "): ";
      if (error_text != nullptr)
        {
          err_str << sys::u8_from_wstring (error_text);
          LocalFree (error_text);
        }
      else
        err_str << "Unknown error.";

      (*current_liboctave_error_handler) ("%s", err_str.str ().c_str ());
    }
}

octave_w32_shlib::~octave_w32_shlib (void)
{
  if (m_handle)
    FreeLibrary (m_handle);
}

void *
octave_w32_shlib::global_search (const std::string& sym_name)
{
  void *function = nullptr;

  HANDLE proc = GetCurrentProcess ();

  if (! proc)
    (*current_liboctave_error_handler)
      ("Unable to get handle to own process.");

  std::size_t lib_num = 64;
  std::size_t size_lib = sizeof (HMODULE);
  HMODULE *h_libs;
  DWORD bytes_all_libs;
  bool got_libs;

  // Get a list of all the libraries in own process.
  h_libs = static_cast<HMODULE *> (malloc (size_lib*lib_num));
  got_libs = EnumProcessModules (proc, h_libs, size_lib*lib_num,
                                 &bytes_all_libs);
  int ii = 0;
  while (((size_lib*lib_num) < bytes_all_libs) && ii++ < 3)
    {
      lib_num = bytes_all_libs / size_lib;
      h_libs = static_cast<HMODULE *> (realloc (h_libs, bytes_all_libs));
      got_libs = EnumProcessModules (proc, h_libs, bytes_all_libs,
                                     &bytes_all_libs);
    }

  if (got_libs)
    {
      for (std::size_t i = 0; i < (bytes_all_libs / size_lib); i++)
        {
          // Check for function in library.
          function = reinterpret_cast<void *>
                     (GetProcAddress (h_libs[i], sym_name.c_str ()));

          if (function)
            break;
        }
    }

  // Release the handle to the process.
  CloseHandle (proc);

  return function;
}

void *
octave_w32_shlib::search (const std::string& name,
                          const dynamic_library::name_mangler& mangler)
{
  void *function = nullptr;

  if (! m_search_all_loaded && ! is_open ())
    (*current_liboctave_error_handler)
      ("shared library %s is not open", m_file.c_str ());

  std::string sym_name = name;

  if (mangler)
    sym_name = mangler (name);

  if (m_search_all_loaded)
    function = global_search (sym_name);
  else
    function = reinterpret_cast<void *> (GetProcAddress (m_handle,
                                         sym_name.c_str ()));

  return function;
}

#endif

dynamic_library::dynlib_rep *
dynamic_library::dynlib_rep::new_instance (const std::string& f)
{
#if defined (HAVE_DLOPEN_API)
  return new octave_dlopen_shlib (f);
#elif defined (HAVE_LOADLIBRARY_API)
  return new octave_w32_shlib (f);
#else
  (*current_liboctave_error_handler)
    ("support for dynamically loaded libraries was unavailable or disabled when liboctave was built");
#endif
}

OCTAVE_END_NAMESPACE(octave)
