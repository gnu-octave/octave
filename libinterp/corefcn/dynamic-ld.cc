////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#include <iostream>
#include <list>

#include "file-stat.h"
#include "oct-env.h"
#include "oct-time.h"

#include "defun.h"
#include "dynamic-ld.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "ov-fcn.h"
#include "ov-dld-fcn.h"
#include "ov-mex-fcn.h"
#include "parse.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

#define STRINGIFY(s) STRINGIFY1(s)
#define STRINGIFY1(s) #s

OCTAVE_BEGIN_NAMESPACE(octave)

void
dynamic_loader::shlibs_list::append (const dynamic_library& shl)
{
  m_lib_list.push_back (shl);
}

std::list<std::string>
dynamic_loader::shlibs_list::remove (dynamic_library& shl)
{
  std::list<std::string> removed_fcns;

  for (auto p = m_lib_list.begin (); p != m_lib_list.end (); p++)
    {
      if (*p == shl)
        {
          m_lib_list.erase (p);

          removed_fcns = shl.close ();

          break;
        }
    }

  return removed_fcns;
}

dynamic_library
dynamic_loader::shlibs_list::find_file (const std::string& file_name) const
{
  dynamic_library retval;

  for (const auto& lib : m_lib_list)
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
  for (const auto& lib : m_lib_list)
    std::cerr << "  " << lib.file_name () << std::endl;
}

void
dynamic_loader::clear_function (const std::string& fcn_name)
{
  warning_with_id ("Octave:reload-forces-clear", "  %s", fcn_name.c_str ());

  // FIXME: is there a way to avoid this?  Can we manage the list of
  // functions that are loaded in the symbol table completely outside
  // of the dynamic_loader class?

  symbol_table& symtab = m_interpreter.get_symbol_table ();

  symtab.clear_dld_function (fcn_name);
}

void
dynamic_loader::clear (dynamic_library& oct_file)
{
  if (oct_file.number_of_functions_loaded () > 1)
    {
      warning_with_id ("Octave:reload-forces-clear",
                       "reloading %s clears the following functions:",
                       oct_file.file_name ().c_str ());

      std::list<std::string> removed_fcns = m_loaded_shlibs.remove (oct_file);

      for (const auto& fcn_name : removed_fcns)
        clear_function (fcn_name);
    }
  else
    {
      std::list<std::string> removed_fcns = m_loaded_shlibs.remove (oct_file);

      // FIXME: is there a way to avoid this?  Can we manage the list
      // of functions that are loaded in the symbol table completely
      // outside of the dynamic_loader class?

      symbol_table& symtab = m_interpreter.get_symbol_table ();

      for (const auto& fcn_name : removed_fcns)
        symtab.clear_dld_function (fcn_name);
    }
}

octave_function *
dynamic_loader::load_oct (const std::string& fcn_name,
                          const std::string& file_name,
                          bool relative)
{
  octave_function *retval = nullptr;

  unwind_protect_var<bool> restore_var (m_doing_load, true);

  dynamic_library oct_file = m_loaded_shlibs.find_file (file_name);

  if (oct_file && oct_file.is_out_of_date ())
    clear (oct_file);

  if (! oct_file)
    {
      oct_file.open (file_name);

      if (oct_file)
        m_loaded_shlibs.append (oct_file);
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

void *
dynamic_loader::try_load_mex (dynamic_library& mex_file,
                              const std::string& fcn_name, bool& have_fmex)
{
  // FCN_NAME is not used here, the mangler functions always return
  // some form of "mexFunction".

  have_fmex = false;

  void *function = mex_file.search (fcn_name, mex_mangler);

  if (! function)
    {
      // FIXME: Can we determine this C mangling scheme
      //        automatically at run time or configure time?

      function = mex_file.search (fcn_name, mex_uscore_mangler);

      if (! function)
        {
          function = mex_file.search (fcn_name, mex_f77_mangler);

          if (function)
            have_fmex = true;
        }
    }

  return function;
}

octave_function *
dynamic_loader::load_mex (const std::string& fcn_name,
                          const std::string& file_name,
                          bool /*relative*/)
{
  unwind_protect_var<bool> restore_var (m_doing_load, true);

  dynamic_library mex_file = m_loaded_shlibs.find_file (file_name);

  if (mex_file && mex_file.is_out_of_date ())
    clear (mex_file);

  if (! mex_file)
    {
      mex_file.open (file_name);

      if (mex_file)
        m_loaded_shlibs.append (mex_file);
    }

  if (! mex_file)
    error ("%s is not a valid shared library", file_name.c_str ());

  bool have_fmex = false;

  void *function = try_load_mex (mex_file, fcn_name, have_fmex);

  if (! function)
    error ("failed to install .mex file function '%s'", fcn_name.c_str ());

  void *symbol = mex_file.search ("__mx_has_interleaved_complex__");

  bool interleaved = symbol != nullptr;

  return new octave_mex_function (function, interleaved, have_fmex,
                                  mex_file, fcn_name);
}

bool
dynamic_loader::remove_oct (const std::string& fcn_name,
                            dynamic_library& shl)
{
  bool retval = false;

  // We don't need to do anything if this is called because we are in
  // the process of reloading a .oct file that has changed.

  if (! m_doing_load)
    {
      retval = shl.remove (fcn_name);

      if (shl.number_of_functions_loaded () == 0)
        m_loaded_shlibs.remove (shl);
    }

  return retval;
}

bool
dynamic_loader::remove_mex (const std::string& fcn_name,
                            dynamic_library& shl)
{
  // Use the same procedure as for oct files.
  return remove_oct (fcn_name, shl);
}

std::string
dynamic_loader::name_mangler (const std::string& name)
{
  return 'G' + name;
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

OCTAVE_END_NAMESPACE(octave)
