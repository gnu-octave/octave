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
#include "pt-eval.h"
#include "pt-idx.h"
#include "pt-misc.h"
#include "pt-stmt.h"
#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static bool
is_method_executing (const octave_value& ov, const cdef_object& obj)
{
  tree_evaluator& tw = __get_evaluator__ ();

  octave_function *stack_fcn = tw.current_function ();

  octave_function *method_fcn = ov.function_value (true);

  // Does the top of the call stack match our target function?

  if (stack_fcn && stack_fcn == method_fcn)
    {
      octave_user_function *uf = method_fcn->user_function_value (true);

      // We can only check the context object for user-function (not builtin),
      // where we have access to the parameters (arguments and return values).
      // That's ok as there's no need to call this function for builtin
      // methods.

      if (uf)
        {
          // At this point, the method is executing, but we still need to
          // check the context object for which the method is executing.  For
          // methods, it's the first argument of the function; for ctors, it
          // is the first return value.

          tree_parameter_list *pl = uf->is_classdef_constructor ()
                                    ? uf->return_list ()
                                    : uf->parameter_list ();

          if (pl && pl->size () > 0)
            {
              tree_decl_elt *elt = pl->front ();

              octave_value arg0 = tw.evaluate (elt);

              if (arg0.is_defined () && arg0.type_name () == "object")
                {
                  cdef_object arg0_obj = to_cdef (arg0);

                  return obj.is (arg0_obj);
                }
            }
        }
    }

  return false;
}

octave_value
cdef_property::cdef_property_rep::get_value (const cdef_object& obj,
    bool do_check_access,
    const std::string& who) const
{
  octave_value retval;

  if (do_check_access && ! check_get_access ())
    err_property_access (who, false);

  if (! obj.is_constructed ())
    {
      cdef_class cls (to_cdef (get ("DefiningClass")));

      if (! obj.is_partially_constructed_for (cls))
        error ("cannot reference properties of class '%s' for non-constructed object",
               cls.get_name ().c_str ());
    }

  octave_value get_fcn = get ("GetMethod");

  // FIXME: should check whether we're already in get accessor method

  if (get_fcn.isempty () || is_method_executing (get_fcn, obj))
    retval = obj.get (get ("Name").string_value ());
  else
    {
      octave_value_list args;

      args(0) = to_ov (obj);

      args = feval (get_fcn, args, 1);

      retval = args(0);
    }

  return retval;
}

octave_value
cdef_property::cdef_property_rep::get_value (bool do_check_access,
    const std::string& who) const
{
  if (do_check_access && ! check_get_access ())
    err_property_access (who, false);

  return get ("DefaultValue");
}

bool
cdef_property::cdef_property_rep::is_recursive_set (const cdef_object& /* obj */) const
{
  // FIXME: implement
  return false;
}

OCTAVE_NORETURN void
cdef_property::cdef_property_rep::err_property_access
(const std::string& from, bool is_set) const
{
  octave_value acc = get (is_set ? "SetAccess" : "GetAccess");
  std::string acc_s;

  if (acc.is_string ())
    acc_s = acc.string_value ();
  else
    acc_s = "class-restricted";

  if (is_set)
    error ("%s: property '%s' has %s access and cannot be set in this context",
           from.c_str (), get_name ().c_str (), acc_s.c_str ());
  else
    error ("%s: property '%s' has %s access and cannot be obtained in this context",
           from.c_str (), get_name ().c_str (), acc_s.c_str ());
}

void
cdef_property::cdef_property_rep::set_value (cdef_object& obj,
    const octave_value& val,
    bool do_check_access,
    const std::string& who)
{
  if (do_check_access && ! check_set_access ())
    err_property_access (who, true);

  if (! obj.is_constructed ())
    {
      cdef_class cls (to_cdef (get ("DefiningClass")));

      if (! obj.is_partially_constructed_for (cls))
        error ("cannot reference properties of class '%s' for non-constructed object",
               cls.get_name ().c_str ());
    }

  octave_value set_fcn = get ("SetMethod");

  if (set_fcn.isempty () || is_method_executing (set_fcn, obj))
    obj.put (get ("Name").string_value (), val);
  else
    {
      octave_value_list args;

      args(0) = to_ov (obj);
      args(1) = val;

      if (obj.is_handle_object ())
        feval (set_fcn, args, 0);
      else
        {
          args = feval (set_fcn, args, 1);

          if (args.length () > 0 && args(0).is_defined ())
            {
              if (args(0).is_classdef_object ())
                {
                  cdef_object new_obj = to_cdef (args(0));

                  obj = new_obj;
                }
              else
                ::warning ("set-method of property '%s' returned a non-classdef object",
                           get_name ().c_str ());
            }
        }
    }
}

bool
cdef_property::cdef_property_rep::check_get_access (void) const
{
  cdef_class cls (to_cdef (get ("DefiningClass")));

  return check_access (cls, get ("GetAccess"), "", get_name (), false);

  return false;
}

bool
cdef_property::cdef_property_rep::check_set_access (void) const
{
  cdef_class cls (to_cdef (get ("DefiningClass")));

  return check_access (cls, get ("SetAccess"), "", get_name (), true);

  return false;
}

OCTAVE_END_NAMESPACE(octave)
