/*

Copyright (C) 1993-2017 John W. Eaton

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

#include <iostream>
#include <list>

#include "file-stat.h"
#include "oct-env.h"
#include "oct-time.h"
#include "singleton-cleanup.h"

#include "defaults.h"
#include "defun.h"
#include "dynamic-ld.h"
#include "ov-fcn.h"
#include "ov-dld-fcn.h"
#include "ov-mex-fcn.h"
#include "parse.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

#define STRINGIFY(s) STRINGIFY1(s)
#define STRINGIFY1(s) #s

namespace octave
{
  void
  dynamic_loader::shlibs_list::append (const dynamic_library& shl)
  {
    lib_list.push_back (shl);
  }

  void
  dynamic_loader::shlibs_list::remove (dynamic_library& shl,
                                       dynamic_library::close_hook cl_hook)
  {
    for (iterator p = lib_list.begin (); p != lib_list.end (); p++)
      {
        if (*p == shl)
          {
            // Erase first to avoid potentially invalidating the pointer by the
            // following hooks.
            lib_list.erase (p);

            shl.close (cl_hook);

            break;
          }
      }
  }

  dynamic_library
  dynamic_loader::shlibs_list::find_file (const std::string& file_name) const
  {
    dynamic_library retval;

    for (const auto& lib : lib_list)
      {
        if (lib.file_name () == file_name)
          {
            retval = lib;
            break;
          }
      }

    return retval;
  }

  void
  dynamic_loader::shlibs_list::display (void) const
  {
    std::cerr << "current shared libraries:" << std::endl;
    for (const auto& lib : lib_list)
      std::cerr << "  " << lib.file_name () << std::endl;
  }

  dynamic_loader *dynamic_loader::instance = nullptr;

  bool dynamic_loader::doing_load = false;

  bool
  dynamic_loader::instance_ok (void)
  {
    bool retval = true;

    if (! instance)
      {
        instance = new dynamic_loader ();

        if (instance)
          singleton_cleanup_list::add (cleanup_instance);
      }

    if (! instance)
      error ("unable to create dynamic loader object!");

    return retval;
  }

  void
  dynamic_loader::do_clear_function (const std::string& fcn_name)
  {
    warning_with_id ("Octave:reload-forces-clear", "  %s", fcn_name.c_str ());

    symbol_table::clear_dld_function (fcn_name);
  }

  void
  dynamic_loader::do_clear (dynamic_library& oct_file)
  {
    if (oct_file.number_of_functions_loaded () > 1)
      {
        warning_with_id ("Octave:reload-forces-clear",
                         "reloading %s clears the following functions:",
                         oct_file.file_name ().c_str ());

        loaded_shlibs.remove (oct_file, do_clear_function);
      }
    else
      loaded_shlibs.remove (oct_file, symbol_table::clear_dld_function);
  }

  octave_function *
  dynamic_loader::do_load_oct (const std::string& fcn_name,
                               const std::string& file_name,
                               bool relative)
  {
    octave_function *retval = nullptr;

    unwind_protect frame;

    frame.protect_var (dynamic_loader::doing_load);

    doing_load = true;

    dynamic_library oct_file = loaded_shlibs.find_file (file_name);

    if (oct_file && oct_file.is_out_of_date ())
      do_clear (oct_file);

    if (! oct_file)
      {
        oct_file.open (file_name);

        if (oct_file)
          loaded_shlibs.append (oct_file);
      }

    if (! oct_file)
      error ("%s is not a valid shared library", file_name.c_str ());

    void *function = oct_file.search (fcn_name, name_mangler);

    if (! function)
      {
        // FIXME: can we determine this C mangling scheme
        // automatically at run time or configure time?

        function = oct_file.search (fcn_name, name_uscore_mangler);
      }

    if (function)
      {
        octave_dld_fcn_getter f
          = reinterpret_cast<octave_dld_fcn_getter> (function);

        retval = f (oct_file, relative);

        if (! retval)
          error ("failed to install .oct file function '%s'",
                 fcn_name.c_str ());
      }

    return retval;
  }

  octave_function *
  dynamic_loader::do_load_mex (const std::string& fcn_name,
                               const std::string& file_name,
                               bool /*relative*/)
  {
    octave_function *retval = nullptr;

    unwind_protect frame;

    frame.protect_var (dynamic_loader::doing_load);

    doing_load = true;

    dynamic_library mex_file = loaded_shlibs.find_file (file_name);

    if (mex_file && mex_file.is_out_of_date ())
      do_clear (mex_file);

    if (! mex_file)
      {
        mex_file.open (file_name);

        if (mex_file)
          loaded_shlibs.append (mex_file);
      }

    if (! mex_file)
      error ("%s is not a valid shared library", file_name.c_str ());

    void *function = nullptr;

    bool have_fmex = false;

    function = mex_file.search (fcn_name, mex_mangler);

    if (! function)
      {
        // FIXME: can we determine this C mangling scheme
        // automatically at run time or configure time?

        function = mex_file.search (fcn_name, mex_uscore_mangler);

        if (! function)
          {
            function = mex_file.search (fcn_name, mex_f77_mangler);

            if (function)
              have_fmex = true;
          }
      }

    if (function)
      retval = new octave_mex_function (function, have_fmex, mex_file, fcn_name);
    else
      error ("failed to install .mex file function '%s'", fcn_name.c_str ());

    return retval;
  }

  bool
  dynamic_loader::do_remove_oct (const std::string& fcn_name,
                                 dynamic_library& shl)
  {
    bool retval = false;

    // We don't need to do anything if this is called because we are in
    // the process of reloading a .oct file that has changed.

    if (! doing_load)
      {
        retval = shl.remove (fcn_name);

        if (shl.number_of_functions_loaded () == 0)
          loaded_shlibs.remove (shl);
      }

    return retval;
  }

  bool
  dynamic_loader::do_remove_mex (const std::string& fcn_name,
                                 dynamic_library& shl)
  {
    bool retval = false;

    // We don't need to do anything if this is called because we are in
    // the process of reloading a .oct file that has changed.

    if (! doing_load)
      {
        retval = shl.remove (fcn_name);

        if (shl.number_of_functions_loaded () == 0)
          loaded_shlibs.remove (shl);
      }

    return retval;
  }

  octave_function *
  dynamic_loader::load_oct (const std::string& fcn_name,
                            const std::string& file_name,
                            bool relative)
  {
    return (instance_ok ())
      ? instance->do_load_oct (fcn_name, file_name, relative) : 0;
  }

  octave_function *
  dynamic_loader::load_mex (const std::string& fcn_name,
                            const std::string& file_name,
                            bool relative)
  {
    return (instance_ok ())
      ? instance->do_load_mex (fcn_name, file_name, relative) : 0;
  }

  bool
  dynamic_loader::remove_oct (const std::string& fcn_name,
                              dynamic_library& shl)
  {
    return (instance_ok ()) ? instance->do_remove_oct (fcn_name, shl) : false;
  }

  bool
  dynamic_loader::remove_mex (const std::string& fcn_name,
                              dynamic_library& shl)
  {
    return (instance_ok ()) ? instance->do_remove_mex (fcn_name, shl) : false;
  }

  std::string
  dynamic_loader::name_mangler (const std::string& name)
  {
    return "G" + name;
  }

  std::string
  dynamic_loader::name_uscore_mangler (const std::string& name)
  {
    return "_G" + name;
  }

  std::string
  dynamic_loader::mex_mangler (const std::string&)
  {
    return "mexFunction";
  }

  std::string
  dynamic_loader::mex_uscore_mangler (const std::string&)
  {
    return "_mexFunction";
  }

  std::string
  dynamic_loader::mex_f77_mangler (const std::string&)
  {
    return STRINGIFY (F77_FUNC (mexfunction, MEXFUNCTION));
  }
}
