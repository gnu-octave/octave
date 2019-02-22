/*

Copyright (C) 2012-2019 Michael Goffioul

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

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

OCTAVE_NORETURN static
void
err_property_access (const std::string& from, const cdef_property& prop,
                     bool is_set = false)
{
  octave_value acc = (prop.get (is_set ? "SetAccess" : "GetAccess"));
  std::string acc_s;

  if (acc.is_string ())
    acc_s = acc.string_value ();
  else
    acc_s = "class-restricted";

  if (is_set)
    error ("%s: property `%s' has %s access and cannot be set in this context",
           from.c_str (), prop.get_name ().c_str (), acc_s.c_str ());
  else
    error ("%s: property `%s' has %s access and cannot be obtained in this context",
           from.c_str (), prop.get_name ().c_str (), acc_s.c_str ());
}

static bool
is_method_executing (const octave_value& ov, const cdef_object& obj)
{
  octave::tree_evaluator& tw
    = octave::__get_evaluator__ ("is_method_executing");

  octave::call_stack& cs = octave::__get_call_stack__ ("is_method_executing");

  octave_function *stack_fcn = cs.current ();

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

          octave::tree_parameter_list *pl = uf->is_classdef_constructor ()
            ? uf->return_list () : uf->parameter_list ();

          if (pl && pl->size () > 0)
            {
              octave::tree_decl_elt *elt = pl->front ();

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
                                             const std::string& who)
{
  octave_value retval;

  if (do_check_access && ! check_get_access ())
    err_property_access (who, wrap (), false);

  if (! obj.is_constructed ())
    {
      cdef_class cls (to_cdef (get ("DefiningClass")));

      if (! obj.is_partially_constructed_for (cls))
        error ("cannot reference properties of class `%s' for non-constructed object",
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

      args = octave::feval (get_fcn, args, 1);

      retval = args(0);
    }

  return retval;
}

octave_value
cdef_property::cdef_property_rep::get_value (bool do_check_access,
                                             const std::string& who)
{
  if (do_check_access && ! check_get_access ())
    err_property_access (who, wrap (), false);

  return get ("DefaultValue");
}

bool
cdef_property::cdef_property_rep::is_recursive_set (const cdef_object& /* obj */) const
{
  // FIXME: implement
  return false;
}

void
cdef_property::cdef_property_rep::set_value (cdef_object& obj,
                                             const octave_value& val,
                                             bool do_check_access,
                                             const std::string& who)
{
  if (do_check_access && ! check_set_access ())
    err_property_access (who, wrap (), true);

  if (! obj.is_constructed ())
    {
      cdef_class cls (to_cdef (get ("DefiningClass")));

      if (! obj.is_partially_constructed_for (cls))
        error ("cannot reference properties of class `%s' for non-constructed object",
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

      args = octave::feval (set_fcn, args, 1);

      if (args.length () > 0 && args(0).is_defined ())
        {
          if (args (0).is_classdef_object ())
            {
              cdef_object new_obj = to_cdef (args(0));

              obj = new_obj;
            }
          else
            ::warning ("set-method of property `%s' returned a non-classdef object",
                       get_name ().c_str ());
        }
    }
}

bool
cdef_property::cdef_property_rep::check_get_access (void) const
{
  cdef_class cls (to_cdef (get ("DefiningClass")));

  return ::check_access (cls, get ("GetAccess"), "",
                         get_name (), false);

  return false;
}

bool
cdef_property::cdef_property_rep::check_set_access (void) const
{
  cdef_class cls (to_cdef (get ("DefiningClass")));

  return ::check_access (cls, get ("SetAccess"), "",
                         get_name (), true);

  return false;
}
