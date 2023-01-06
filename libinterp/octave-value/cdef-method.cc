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
#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_NORETURN static
void
err_method_access (const std::string& from, const cdef_method& meth)
{
  octave_value acc = meth.get ("Access");
  std::string acc_s;

  if (acc.is_string ())
    acc_s = acc.string_value ();
  else
    acc_s = "class-restricted";

  error ("%s: method '%s' has %s access and cannot be run in this context",
         from.c_str (), meth.get_name ().c_str (), acc_s.c_str ());
}

void
cdef_method::cdef_method_rep::check_method (void)
{
  if (is_external ())
    {
      if (is_dummy_method (m_function))
        {
          load_path& lp = __get_load_path__ ();

          std::string name = get_name ();
          std::string cls_name = m_dispatch_type;
          std::string pack_name;

          std::size_t pos = cls_name.rfind ('.');

          if (pos != std::string::npos)
            {
              pack_name = cls_name.substr (0, pos);
              cls_name = cls_name.substr (pos + 1);
            }

          std::string dir_name;
          std::string file_name = lp.find_method (cls_name, name,
                                                  dir_name, pack_name);

          if (! file_name.empty ())
            {
              octave_value ov_fcn
                = load_fcn_from_file (file_name, dir_name,
                                      m_dispatch_type, pack_name);

              if (ov_fcn.is_defined ())
                {
                  m_function = ov_fcn;

                  make_function_of_class (m_dispatch_type, m_function);
                }
            }
        }
      else
        {
          // FIXME: check out-of-date status
        }

      if (is_dummy_method (m_function))
        error ("no definition found for method '%s' of class '%s'",
               get_name ().c_str (), m_dispatch_type.c_str ());
    }
}

octave_value_list
cdef_method::cdef_method_rep::execute (const octave_value_list& args,
                                       int nargout, bool do_check_access,
                                       const std::string& who)
{
  octave_value_list retval;

  if (do_check_access && ! check_access ())
    err_method_access (who, wrap ());

  if (get ("Abstract").bool_value ())
    error ("%s: cannot execute abstract method",
           get ("Name").string_value ().c_str ());

  check_method ();

  if (m_function.is_defined ())
    retval = feval (m_function, args, nargout);

  return retval;
}

octave_value_list
cdef_method::cdef_method_rep::execute (const cdef_object& obj,
                                       const octave_value_list& args,
                                       int nargout, bool do_check_access,
                                       const std::string& who)
{
  octave_value_list retval;

  if (do_check_access && ! check_access ())
    err_method_access (who, wrap ());

  if (get ("Abstract").bool_value ())
    error ("%s: cannot execute abstract method",
           get ("Name").string_value ().c_str ());

  check_method ();

  if (m_function.is_defined ())
    {
      octave_value_list new_args;

      new_args.resize (args.length () + 1);

      new_args(0) = to_ov (obj);
      for (int i = 0; i < args.length (); i++)
        new_args(i+1) = args(i);

      retval = feval (m_function, new_args, nargout);
    }

  return retval;
}

bool
cdef_method::cdef_method_rep::is_constructor (void) const
{
  if (m_function.is_function())
    return m_function.function_value ()->is_classdef_constructor ();

  return false;
}

bool
cdef_method::cdef_method_rep::is_defined_in_class (const std::string& cname) const
{
  return (m_function.is_function ()
          ? m_function.function_value ()->dispatch_class () == cname
          : false);
}

std::string
cdef_method::cdef_method_rep::get_doc_string (void)
{
  check_method ();

  octave_function *fcn = m_function.function_value ();

  return fcn ? fcn->doc_string () : "";
}

bool
cdef_method::cdef_method_rep::check_access (void) const
{
  cdef_class cls (to_cdef (get ("DefiningClass")));

  return octave::check_access (cls, get ("Access"), get_name ());
}

octave_value_list
cdef_method::cdef_method_rep::meta_subsref
(const std::string& type, const std::list<octave_value_list>& idx,
 int nargout)
{
  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      retval = (execute (idx.front (),
                         type.length () > 1 ? 1 : nargout, true));
      break;

    default:
      error ("invalid meta.method indexing");
      break;
    }

  if (type.length () > 1 && idx.size () > 1 && ! retval.empty ())
    retval = retval(0).next_subsref (nargout, type, idx, 1);

  return retval;
}

OCTAVE_END_NAMESPACE(octave)
