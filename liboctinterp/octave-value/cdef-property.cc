////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2026 The Octave Project Developers
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

#include "cdef-class.h"
#include "cdef-manager.h"
#include "cdef-utils.h"
#include "errwarn.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "ov-builtin.h"
#include "ov-classdef.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "pt-assign.h"
#include "pt-classdef.h"
#include "pt-eval.h"
#include "pt-idx.h"
#include "pt-misc.h"
#include "pt-stmt.h"
#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

cdef_property::cdef_property_rep::cdef_property_rep (
  const cdef_object& defining_class,
  octave_value name,
  cdef_property_opt_properties opt_args)
  : cdef_meta_object_rep ()
{
  set_class (__get_cdef_manager__ ().meta_property ());

  if (! defining_class.ok ())
    error ("invalid property: DefiningClass not valid");

  put ("DefiningClass", to_ov (defining_class));

  // Empty names are reserved for metadata-array filler objects.
  if (! name.isempty ()
      && name.string_value () == defining_class.get ("Name").string_value ())
    error ("invalid property: cannot have the same name as the classdef itself");
  put ("Name", name);
  put ("Description", opt_args.description);
  put ("DetailedDescription", opt_args.detailed_description);
  put ("Validation", opt_args.validation);

  // Resolve Access -> GetAccess/SetAccess inheritance.
  // Empty string means "not explicitly set", so inherit from Access.
  auto is_unset = [] (const octave_value& val)
  {
    return val.is_string () && val.string_value ().empty ();
  };

  octave_value access = opt_args.access;
  octave_value get_access = opt_args.get_access;
  octave_value set_access = opt_args.set_access;

  const cdef_class cls (defining_class);
  validate_access (access, "Access", "property", name.string_value (), cls);
  if (! is_unset (get_access))
    validate_access (get_access, "GetAccess", "property", name.string_value (), cls);
  if (! is_unset (set_access))
    validate_access (set_access, "SetAccess", "property", name.string_value (), cls);

  if (! is_unset (get_access) || ! is_unset (set_access))
    {
      // If either GetAccess or SetAccess is set, then
      // Access cannot also be set.
      if (! access.is_string () || access.string_value () != "public")
        error ("invalid property: cannot set \'Access\' and \'%s\' on the same property",
               is_unset (get_access) ? "SetAccess" : "GetAccess");

      put ("GetAccess", is_unset (get_access) ? octave_value ("public")
                                              : get_access);
      put ("SetAccess", is_unset (set_access) ? octave_value ("public")
                                              : set_access);
    }
  else
    {
      put ("GetAccess", access);
      put ("SetAccess", access);
    }

  // Attributes
  put ("AbortSet", opt_args.abort_set);
  put ("Abstract", opt_args.abstract);
  put ("Constant", opt_args.constant);
  put ("Dependent", opt_args.dependent);
  put ("GetObservable", opt_args.get_observable);
  put ("GetMethod", opt_args.get_method);

  if (opt_args.default_value.is_defined ())
    {
      put ("HasDefault", true);
      put ("DefaultValue", opt_args.default_value);
    }
  else
    {
      put ("HasDefault", false);
      put ("DefaultValue", octave_value ());
    }

  put ("Hidden", opt_args.hidden);
  put ("NonCopyable", opt_args.non_copyable);
  put ("PartialMatchPriority", opt_args.partial_match_priority);
  put ("SetMethod", opt_args.set_method);
  put ("SetObservable", opt_args.set_observable);
  put ("Transient", opt_args.transient);
  put ("WeakHandle", opt_args.weak_handle);
}

void
cdef_property::cdef_property_rep::put (const std::string& pname,
                                       const octave_value& val)
{
  try
    {
      if (pname == "AbortSet")
        {
          auto coerced = val.bool_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "Abstract")
        {
          auto coerced = val.bool_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "Access")
        {
          if (val.is_string ())
            {
              auto coerced = val.string_value ();
              if (coerced == "public" || coerced == "private"
                  || coerced == "protected")
                m_map.assign (pname, coerced);
              else
                error ("Access must be 'public', 'private', 'protected', or a list of classes");
            }
          else if (is_access_list (val))
            m_map.assign (pname, val);
          else
            error ("Access must be 'public', 'private', 'protected', or a list of classes");
        }
      else if (pname == "Constant")
        {
          auto coerced = val.bool_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "DefaultValue")
        {
          // Can be any value or empty
          m_map.assign (pname, val);
        }
      else if (pname == "DefiningClass")
        {
          // Must be a classdef object wrapping a meta.class
          m_map.assign (pname, val);
        }
      else if (pname == "Dependent")
        {
          auto coerced = val.bool_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "Description")
        {
          auto coerced = val.string_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "DetailedDescription")
        {
          auto coerced = val.string_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "GetAccess")
        {
          if (val.is_string ())
            {
              auto coerced = val.string_value ();
              if (coerced == "public" || coerced == "private"
                  || coerced == "protected")
                m_map.assign (pname, coerced);
              else
                error ("GetAccess must be 'public', 'private', 'protected', or a list of classes");
            }
          else if (is_access_list (val))
            m_map.assign (pname, val);
          else
            error ("GetAccess must be 'public', 'private', 'protected', or a list of classes");
        }
      else if (pname == "GetMethod")
        {
          // Can be a function handle, function, or empty
          m_map.assign (pname, val);
        }
      else if (pname == "GetObservable")
        {
          auto coerced = val.bool_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "HasDefault")
        {
          auto coerced = val.bool_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "Hidden")
        {
          auto coerced = val.bool_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "Name")
        {
          auto coerced = val.string_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "NonCopyable")
        {
          auto coerced = val.bool_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "PartialMatchPriority")
        {
          auto coerced = val.int_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "SetAccess")
        {
          if (val.is_string ())
            {
              auto coerced = val.string_value ();
              if (coerced == "public" || coerced == "private"
                  || coerced == "protected" || coerced == "immutable")
                m_map.assign (pname, coerced);
              else
                error ("SetAccess must be 'public', 'private', 'protected', 'immutable', or a list of classes");
            }
          else if (is_access_list (val))
            m_map.assign (pname, val);
          else
            error ("SetAccess must be 'public', 'private', 'protected', 'immutable', or a list of classes");
        }
      else if (pname == "SetMethod")
        {
          octave_value access = get ("SetAccess");

          if ((val.is_function () || val.is_function_handle ())
              && access.is_string () && access.string_value () == "immutable")
            error ("immutable property '%s' cannot have a set method",
                   get_name ().c_str ());

          // Can be a function handle, function, or empty
          m_map.assign (pname, val);
        }
      else if (pname == "SetObservable")
        {
          auto coerced = val.bool_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "Transient")
        {
          auto coerced = val.bool_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "Validation")
        {
          // Array of validation objects or empty
          m_map.assign (pname, val);
        }
      else if (pname == "WeakHandle")
        {
          auto coerced = val.bool_value ();
          m_map.assign (pname, coerced);
        }
      else
        {
          error ("unknown property '%s' of meta.property", pname.c_str ());
        }
    }
  catch (const execution_exception& ee)
    {
      error ("put: setting '%s' on meta.property failed with error: %s",
             pname.c_str (), ee.message ().c_str ());
    }
}

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

  if ((! get_fcn.is_function () && ! get_fcn.is_function_handle ())
      || is_method_executing (get_fcn, obj))
    {
      retval = obj.get (get ("Name").string_value ());

      if (! retval.is_defined ())
        {
          retval = get ("DefaultValue");
        }
    }
  else
    {
      octave_value_list args;

      args(0) = to_ov (obj);

      interpreter& interp = __get_interpreter__ ();

      args = interp.feval (get_fcn, args, 1);

      retval = args(0);
    }

  return retval;
}

cdef_class
cdef_property::cdef_property_rep::get_defining_class () const
{
  return to_cdef (m_map.contents ("DefiningClass"));
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
  if (do_check_access)
    {
      if (! check_set_access ())
        err_property_access (who, true);

      octave_value access = get ("SetAccess");

      // Constructor access does not permit modifying another, already
      // constructed instance of the same class.
      if (access.is_string () && access.string_value () == "immutable"
          && obj.is_constructed_for (get_defining_class ()))
        err_property_access (who, true);
    }

  if (! obj.is_constructed ())
    {
      cdef_class cls (to_cdef (get ("DefiningClass")));

      if (! obj.is_partially_constructed_for (cls))
        error ("cannot reference properties of class '%s' for non-constructed object",
               cls.get_name ().c_str ());
    }

  octave_value set_fcn = get ("SetMethod");

  if ((! set_fcn.is_function () && ! set_fcn.is_function_handle ())
      || is_method_executing (set_fcn, obj))
    obj.put (get ("Name").string_value (), val);
  else
    {
      octave_value_list args;

      args(0) = to_ov (obj);
      args(1) = val;

      interpreter& interp = __get_interpreter__ ();

      if (obj.is_handle_object ())
        interp.feval (set_fcn, args, 0);
      else
        {
          args = interp.feval (set_fcn, args, 1);

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
cdef_property::cdef_property_rep::check_get_access () const
{
  cdef_class cls (to_cdef (get ("DefiningClass")));

  return check_access (cls, get ("GetAccess"), "", get_name (), false);

  return false;
}

bool
cdef_property::cdef_property_rep::check_set_access () const
{
  cdef_class cls (to_cdef (get ("DefiningClass")));

  return check_access (cls, get ("SetAccess"), "", get_name (), true);

  return false;
}

cdef_class
cdef_property::get_defining_class () const
{
  return get_rep ()->get_defining_class ();
}

void
cdef_property::make_property (cdef_class& cls, tree_evaluator& tw,
                              tree_classdef_properties_block* pb_p)
{
  std::map<std::string, octave_value> amap = {
    {"AbortSet", false},
    {"Abstract", false},
    {"Access", "public"},
    {"Constant", false},
    {"Dependent", false},
    {"Description", ""},
    {"DetailedDescription", ""},
    {"GetAccess", ""},
    {"GetObservable", false},
    {"Hidden", false},
    {"NonCopyable", false},
    {"PartialMatchPriority", 1},
    {"SetAccess", ""},
    {"SetObservable", false},
    {"Transient", false},
    {"WeakHandle", false}
  };

  if (pb_p->attribute_list ())
    {
      for (auto& attr_p : *pb_p->attribute_list ())
        {
          std::string aname = attr_p->ident ()->name ();
          auto it = amap.find (aname);

          if (it == amap.end ())
            error ("invalid attribute: attribute %s cannot be assigned in a properties block", aname.c_str ());

          it->second = compute_attribute_value (tw, attr_p);
        }
    }

  if (pb_p->element_list ())
    {
      for (auto& prop_p : *pb_p->element_list ())
        {
          std::string pname = prop_p->ident ()->name ();

          // Docstring of a property is separated into the
          // Description and DetailedDescription properties

          const std::string doc_string = prop_p->doc_string ();
          std::string description = "";
          std::string detailed_description = "";

          size_t pos = doc_string.find('\n');
          if (pos != std::string::npos)
            {
              description = doc_string.substr (0, pos);
              detailed_description = doc_string.substr (pos + 1);
            }
          else
              description = doc_string;

          // Setting attributes "Description" and "DetailedDescription"
          // should override the doc string written in the properties block.
          // Strip the whitespace in front of the description string.
          // Weirdly, the whitespace isn't stripped from the
          // detailed_description string

          const std::string whitespaces = " \t\n\r\f\v";
          size_t start = description.find_first_not_of (whitespaces);
          if (start != std::string::npos)
            description.erase (0, start);

          if (amap["Description"].string_value () != "")
            description = amap["Description"].string_value ();
          if (amap["DetailedDescription"].string_value () != "")
            detailed_description = amap["DetailedDescription"].string_value ();

          tree_expression *expr = prop_p->expression ();
          octave_value pdefault;

          if (expr)
            {
              octave_value pvalue = expr->evaluate (tw);
              pdefault = pvalue;
            }

          cdef_property_opt_properties opts {
            .abort_set = amap["AbortSet"],
            .abstract = amap["Abstract"],
            .access = amap["Access"],
            .constant = amap["Constant"],
            .default_value = pdefault,
            .dependent = amap["Dependent"],
            // Description and DetailedDescription might be
            // different for each property in a property
            // block, so it should not be set by the map
            .description = description,
            .detailed_description = detailed_description,
            .get_access = amap["GetAccess"],
            .get_observable = amap["GetObservable"],
            .hidden = amap["Hidden"],
            .non_copyable = amap["NonCopyable"],
            .partial_match_priority = amap["PartialMatchPriority"],
            .set_access = amap["SetAccess"],
            .set_observable = amap["SetObservable"],
            .transient = amap["Transient"],
            .weak_handle = amap["WeakHandle"]
          };

          cdef_property prop = cdef_property (cls, pname, opts);

          cls.install_property (prop);
        }
     }
}

OCTAVE_END_NAMESPACE(octave)
