////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2023 The Octave Project Developers
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

#include <algorithm>
#include <iomanip>

#include "cdef-class.h"
#include "cdef-manager.h"
#include "cdef-utils.h"
#include "errwarn.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "load-path.h"
#include "ov-builtin.h"
#include "ov-classdef.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "parse.h"
#include "pt-assign.h"
#include "pt-classdef.h"
#include "pt-idx.h"
#include "pt-misc.h"
#include "pt-stmt.h"

// Define to 1 to enable debugging statements.
#define DEBUG_TRACE 0

OCTAVE_BEGIN_NAMESPACE(octave)

void
cdef_package::cdef_package_rep::install_class (const cdef_class& cls,
    const std::string& nm)
{
  m_class_map[nm] = cls;

  m_member_count++;
}

void
cdef_package::cdef_package_rep::install_function (const octave_value& fcn,
    const std::string& nm)
{
  m_function_map[nm] = fcn;
}

void
cdef_package::cdef_package_rep::install_package (const cdef_package& pack,
    const std::string& nm)
{
  m_package_map[nm] = pack;

  m_member_count++;
}

template <typename T1, typename T2>
Cell
map2Cell (const std::map<T1, T2>& m)
{
  Cell retval (1, m.size ());
  int i = 0;

  for (auto it = m.begin (); it != m.end (); ++it, ++i)
    retval(i) = to_ov (it->second);

  return retval;
}

Cell
cdef_package::cdef_package_rep::get_classes (void) const
{
  return map2Cell (m_class_map);
}

Cell
cdef_package::cdef_package_rep::get_functions (void) const
{
  return map2Cell (m_function_map);
}

Cell
cdef_package::cdef_package_rep::get_packages (void) const
{
  return map2Cell (m_package_map);
}

octave_value
cdef_package::cdef_package_rep::find (const std::string& nm)
{
  std::string symbol_name = get_name () + '.' + nm;

  interpreter& interp = __get_interpreter__ ();

  return interp.find (symbol_name);
}

octave_value_list
cdef_package::cdef_package_rep::meta_subsref
(const std::string& type, const std::list<octave_value_list>& idx,
 int nargout)
{
  octave_value_list retval;

  switch (type[0])
    {
    case '.':
      {
        if (idx.front ().length () != 1)
          error ("invalid meta.package indexing");

        std::string nm = idx.front ()(
                           0).xstring_value ("invalid meta.package indexing, expected a symbol name");

#if DEBUG_TRACE
        std::cerr << "meta.package query: " << nm << std::endl;
#endif

        octave_value o = find (nm);

        if (! o.is_defined ())
          error ("member '%s' in package '%s' does not exist",
                 nm.c_str (), get_name ().c_str ());

        if (o.is_function ())
          {
            octave_function *fcn = o.function_value ();

            // NOTE: the case where the package query is the last
            // part of this subsref index is handled in the parse
            // tree, because there is some logic to handle magic
            // "end" that makes it impossible to execute the
            // function call at this stage.

            if (type.size () > 1
                && ! fcn->accepts_postfix_index (type[1]))
              {
                octave_value_list tmp_args;

                retval = feval (o, tmp_args, nargout);
              }
            else
              retval(0) = o;

            if (type.size () > 1 && idx.size () > 1)
              retval = retval(0).next_subsref (nargout, type,
                                               idx, 1);
          }
        else if (type.size () > 1 && idx.size () > 1)
          retval = o.next_subsref (nargout, type, idx, 1);
        else
          retval(0) = o;
      }
      break;

    default:
      error ("invalid meta.package indexing");
      break;
    }

  return retval;
}

void
cdef_package::cdef_package_rep::meta_release (void)
{
  // FIXME: Do we really want to unregister the package, as it
  //        could still be referenced by classes or sub-packages?
  //        If the package object is recreated later on, it won't
  //        match the one already referenced by those classes or
  //        sub-packages.

  cdef_manager& cdm = __get_cdef_manager__ ();

  // Don't delete the "meta" package.
  if (this != cdm.meta ().get_rep ())
    cdm.unregister_package (wrap ());
}

OCTAVE_END_NAMESPACE(octave)
