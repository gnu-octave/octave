/*

Copyright (C) 1999-2017 John W. Eaton
Copyright (C) 2009 VZLU Prague

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <map>

#if defined (HAVE_SHL_LOAD_API)
#  include <cerrno>
#  include <cstring>
#endif

#if defined (HAVE_DYLD_API)
#  include <mach-o/dyld.h>
#endif

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
#elif defined (HAVE_SHL_LOAD_API)
#  include <dl.h>
#elif defined (HAVE_LOADLIBRARY_API)
#  define WIN32_LEAN_AND_MEAN 1
#  include <windows.h>
#endif
}

#include "file-ops.h"
#include "file-stat.h"
#include "lo-error.h"
#include "oct-shlib.h"
#include "str-vec.h"

namespace octave
{
  dynamic_library::dynlib_rep::dynlib_rep (const std::string& f)
    : count (1), file (f), tm_loaded (), fcn_names ()
  {
    instances[f] = this;

    if (is_out_of_date ())
      (*current_liboctave_warning_with_id_handler)
        ("Octave:warn-future-time-stamp",
         "timestamp on file %s is in the future", file.c_str ());
  }

  bool
  dynamic_library::dynlib_rep::is_out_of_date (void) const
  {
    sys::file_stat fs (file);
    return (fs && fs.is_newer (tm_loaded));
  }

  void
  dynamic_library::dynlib_rep::fake_reload (void)
  {
    // We can't actually reload the library, but we'll pretend we did.
    sys::file_stat fs (file);
    if (fs && fs.is_newer (tm_loaded))
      {
        tm_loaded = fs.mtime ();

        (*current_liboctave_warning_with_id_handler)
          ("Octave:library-reload",
           "library %s not reloaded due to existing references", file.c_str ());
      }
  }

  dynamic_library::dynlib_rep *
  dynamic_library::dynlib_rep::get_instance (const std::string& f, bool fake)
  {
    dynlib_rep *retval = nullptr;
    std::map<std::string, dynlib_rep *>::iterator p = instances.find (f);
    if (p != instances.end ())
      {
        retval = p->second;
        retval->count++;
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

    for (const auto& p : fcn_names)
      retval.push_back (p.first);

    return retval;
  }

  void
  dynamic_library::dynlib_rep::add_fcn_name (const std::string& name)
  {
    auto p = fcn_names.find (name);

    if (p == fcn_names.end ())
      fcn_names[name] = 1;
    else
      ++(p->second);
  }

  bool
  dynamic_library::dynlib_rep::remove_fcn_name (const std::string& fcn_name)
  {
    bool retval = false;

    auto p = fcn_names.find (fcn_name);

    if (p != fcn_names.end () && --(p->second) == 0)
      {
        fcn_names.erase (fcn_name);
        retval = true;
      }

    return retval;
  }

  std::map<std::string, dynamic_library::dynlib_rep *>
    dynamic_library::dynlib_rep::instances;

  dynamic_library::dynlib_rep dynamic_library::nil_rep;

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
                   dynamic_library::name_mangler mangler = nullptr);

    // FIXME: this is possibly redundant because failure to open a library will
    // normally throw an exception, avoiding the construction of an invalid
    // library.  Leave it here for possible future use.

    bool is_open (void) const { return (library != nullptr); }

  private:

    void *library;
  };

  octave_dlopen_shlib::octave_dlopen_shlib (const std::string& f)
    : dynamic_library::dynlib_rep (f), library (nullptr)
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

    library = dlopen (file.c_str (), flags);

    if (! library)
      {
        const char *msg = dlerror ();

        if (msg)
          (*current_liboctave_error_handler) ("%s: failed to load: %s",
                                              file.c_str (), msg);
        else
          (*current_liboctave_error_handler) ("%s: failed to load",
                                              file.c_str ());
      }
  }

  octave_dlopen_shlib::~octave_dlopen_shlib (void)
  {
    if (library)
      dlclose (library);
  }

  void *
  octave_dlopen_shlib::search (const std::string& name,
                               dynamic_library::name_mangler mangler)
  {
    void *function = nullptr;

    if (! is_open ())
      (*current_liboctave_error_handler)
        ("shared library %s is not open", file.c_str ());

    std::string sym_name = name;

    if (mangler)
      sym_name = mangler (name);

    function = dlsym (library, sym_name.c_str ());

    return function;
  }

#elif defined (HAVE_SHL_LOAD_API)

  class
  octave_shl_load_shlib : public dynamic_library::dynlib_rep
  {
  public:

    octave_shl_load_shlib (const std::string& f);

    // No copying!

    octave_shl_load_shlib (const octave_shl_load_shlib&) = delete;

    octave_shl_load_shlib& operator = (const octave_shl_load_shlib&) = delete;

    ~octave_shl_load_shlib (void);

    void * search (const std::string& name,
                   dynamic_library::name_mangler mangler = 0);

    bool is_open (void) const { return (library != 0); }

  private:

    shl_t library;
  };

  octave_shl_load_shlib::octave_shl_load_shlib (const std::string& f)
    : dynamic_library::dynlib_rep (f), library (0)
  {
    file = f;

    library = shl_load (file.c_str (), BIND_IMMEDIATE, 0L);

    if (! library)
      {
        using namespace std;  // FIXME: Why have this line?
        (*current_liboctave_error_handler) ("%s", std::strerror (errno));
      }
  }

  octave_shl_load_shlib::~octave_shl_load_shlib (void)
  {
    if (library)
      shl_unload (library);
  }

  void *
  octave_shl_load_shlib::search (const std::string& name,
                                 dynamic_library::name_mangler mangler)
  {
    void *function = nullptr;

    if (! is_open ())
      (*current_liboctave_error_handler)
        ("shared library %s is not open", file.c_str ());

    std::string sym_name = name;

    if (mangler)
      sym_name = mangler (name);

    int status = shl_findsym (&library, sym_name.c_str (),
                              TYPE_UNDEFINED, &function);

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
                   dynamic_library::name_mangler mangler = 0);

    bool is_open (void) const { return (handle != 0); }

  private:

    HINSTANCE handle;
  };

  static void
  set_dll_directory (const std::string& dir = "")
  {
    SetDllDirectory (dir.empty () ? 0 : dir.c_str ());
  }

  octave_w32_shlib::octave_w32_shlib (const std::string& f)
    : dynamic_library::dynlib_rep (f), handle (0)
  {
    std::string dir = sys::file_ops::dirname (f);

    set_dll_directory (dir);

    handle = LoadLibrary (file.c_str ());

    set_dll_directory ();

    if (! handle)
      {
        DWORD lastError = GetLastError ();
        const char *msg;

        switch (lastError)
          {
          case ERROR_MOD_NOT_FOUND:
          case ERROR_DLL_NOT_FOUND:
            msg = "could not find library or dependencies";
            break;

          case ERROR_INVALID_DLL:
            msg = "library or its dependencies are damaged";
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

  octave_w32_shlib::~octave_w32_shlib (void)
  {
    if (handle)
      FreeLibrary (handle);
  }

  void *
  octave_w32_shlib::search (const std::string& name,
                            dynamic_library::name_mangler mangler)
  {
    void *function = nullptr;

    if (! is_open ())
      (*current_liboctave_error_handler)
        ("shared library %s is not open", file.c_str ());

    std::string sym_name = name;

    if (mangler)
      sym_name = mangler (name);

    function = reinterpret_cast<void *> (GetProcAddress (handle,
                                                         sym_name.c_str ()));

    return function;
  }

#elif defined (HAVE_DYLD_API)

  class
  octave_dyld_shlib : public dynamic_library::dynlib_rep
  {
  public:

    octave_dyld_shlib (void);

    // No copying!

    octave_dyld_shlib (const octave_dyld_shlib&) = delete;

    octave_dyld_shlib& operator = (const octave_dyld_shlib&) = delete;

    ~octave_dyld_shlib (void);

    void open (const std::string& f);

    void * search (const std::string& name,
                   dynamic_library::name_mangler mangler = nullptr);

    void close (void);

    bool is_open (void) const {return (handle != 0); }

  private:

    NSObjectFileImage img;
    NSModule handle;
  };

  octave_dyld_shlib::octave_dyld_shlib (const std::string& f)
    : dynamic_library::dynlib_rep (f), handle (0)
  {
    int returnCode = NSCreateObjectFileImageFromFile (file.c_str (), &img);

    if (NSObjectFileImageSuccess != returnCode)
      {
        (*current_liboctave_error_handler)
          ("got NSObjectFileImageReturnCode %d", returnCode);

        // FIXME: should use NSLinkEditError () to get
        //        more info on what went wrong.
      }

    handle = NSLinkModule (img, file.c_str (),
                           (NSLINKMODULE_OPTION_RETURN_ON_ERROR
                            | NSLINKMODULE_OPTION_PRIVATE));
    if (! handle)
      {
        NSLinkEditErrors ler;
        int lerno;
        const char *file2;
        const char *errstr = nullptr;

        NSLinkEditError (&ler, &lerno, &file2, &errstr);

        if (! errstr)
          errstr = "unspecified error";

        (*current_liboctave_error_handler) ("%s: %s", file.c_str (), errstr);
      }
  }

  octave_dyld_shlib::~octave_dyld_shlib (void)
  {
    if (handle)
      NSUnLinkModule (handle, NSUNLINKMODULE_OPTION_RESET_LAZY_REFERENCES);

    NSDestroyObjectFileImage (img);
  }

  void *
  octave_dyld_shlib::search (const std::string& name,
                             dynamic_library::name_mangler mangler)
  {
    void *function = nullptr;

    if (! is_open ())
      (*current_liboctave_error_handler)
        ("bundle %s is not open", file.c_str ());

    std::string sym_name = name;

    if (mangler)
      sym_name = mangler (name);

    NSSymbol symbol = NSLookupSymbolInModule (handle, sym_name.c_str ());

    if (symbol)
      {
        function = NSAddressOfSymbol (symbol);
      }

    return function;
  }

#endif

  dynamic_library::dynlib_rep *
  dynamic_library::dynlib_rep::new_instance (const std::string& f)
  {
#if defined (HAVE_DLOPEN_API)
    return new octave_dlopen_shlib (f);
#elif defined (HAVE_SHL_LOAD_API)
    return new octave_shl_load_shlib (f);
#elif defined (HAVE_LOADLIBRARY_API)
    return new octave_w32_shlib (f);
#elif defined (HAVE_DYLD_API)
    return new octave_dyld_shlib (f);
#else
    (*current_liboctave_error_handler)
      ("support for dynamically loaded libraries was unavailable or disabled when liboctave was built");
#endif
  }
}
