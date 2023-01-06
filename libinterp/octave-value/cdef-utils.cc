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

#include "cdef-class.h"
#include "cdef-manager.h"
#include "cdef-method.h"
#include "cdef-package.h"
#include "cdef-property.h"
#include "cdef-utils.h"
#include "interpreter-private.h"
#include "ov-classdef.h"
#include "ov-usr-fcn.h"
#include "pt-eval.h"

OCTAVE_BEGIN_NAMESPACE(octave)

std::string
get_base_name (const std::string& nm)
{
  std::string::size_type pos = nm.find_last_of ('.');

  if (pos != std::string::npos)
    return nm.substr (pos + 1);

  return nm;
}

void
make_function_of_class (const std::string& class_name,
                        const octave_value& fcn)
{
  octave_function *of = fcn.function_value ();

  of->stash_dispatch_class (class_name);

  octave_user_function *uf = of->user_function_value (true);

  if (uf)
    {
      if (get_base_name (class_name) == uf->name ())
        uf->mark_as_classdef_constructor ();
      else
        uf->mark_as_classdef_method ();
    }
}

void
make_function_of_class (const cdef_class& cls, const octave_value& fcn)
{
  make_function_of_class (cls.get_name (), fcn);
}

cdef_class
lookup_class (const std::string& name, bool error_if_not_found,
              bool load_if_not_found)
{
  cdef_manager& cdm = __get_cdef_manager__ ();

  return cdm.find_class (name, error_if_not_found, load_if_not_found);
}

cdef_class
lookup_class (const cdef_class& cls)
{
  // FIXME: placeholder for the time being, the purpose
  //        is to centralized any class update activity here.

  return cls;
}

cdef_class
lookup_class (const octave_value& ov)
{
  if (ov.is_string())
    return lookup_class (ov.string_value ());
  else
    {
      cdef_class cls (to_cdef (ov));

      return lookup_class (cls);
    }

  return cdef_class ();
}

std::list<cdef_class>
lookup_classes (const Cell& cls_list)
{
  std::list<cdef_class> retval;

  for (int i = 0; i < cls_list.numel (); i++)
    {
      cdef_class c = lookup_class (cls_list(i));

      retval.push_back (c);
    }

  return retval;
}

octave_value
to_ov (const cdef_object& obj)
{
  if (obj.ok ())
    return octave_value (new octave_classdef (obj));
  else
    return octave_value (Matrix ());
}

octave_value
to_ov (const octave_value& ov)
{
  return ov;
}

cdef_object
to_cdef (const octave_value& val)
{
  if (val.type_name () != "object")
    error ("cannot convert '%s' into 'object'", val.type_name().c_str ());

  return dynamic_cast<octave_classdef *> (val.internal_rep ())->get_object ();
}

cdef_object&
to_cdef_ref (const octave_value& val)
{
  if (val.type_name () != "object")
    error ("cannot convert '%s' into 'object'", val.type_name().c_str ());

  return dynamic_cast<octave_classdef *> (val.internal_rep ())->get_object_ref ();
}

cdef_object
to_cdef (const cdef_object& obj)
{
  return obj;
}

octave_value
to_ov (const std::list<cdef_class>& class_list)
{
  Cell cls (class_list.size (), 1);
  int i = 0;

  for (const auto& cdef_cls : class_list)
    cls(i++) = to_ov (cdef_cls);

  return octave_value (cls);
}

bool
is_dummy_method (const octave_value& fcn)
{
  bool retval = false;

  if (fcn.is_defined ())
    {
      if (fcn.is_user_function ())
        {
          octave_user_function *uf = fcn.user_function_value (true);

          if (! uf || ! uf->body ())
            retval = true;
        }
    }
  else
    retval = true;

  return retval;
}

bool
is_superclass (const cdef_class& clsa, const cdef_class& clsb,
               bool allow_equal, int max_depth)
{
  bool retval = false;

  if (allow_equal && clsa == clsb)
    retval = true;
  else if (max_depth != 0)
    {
      Cell c = clsb.get ("SuperClasses").cell_value ();

      for (int i = 0; ! retval && i < c.numel (); i++)
        {
          octave_classdef *metacls = c(i).classdef_object_value ();
          std::string clsname = metacls->get_property (0, "Name").string_value ();
          cdef_class cls = lookup_class (clsname);

          retval = is_superclass (clsa, cls, true,
                                  max_depth < 0 ? max_depth : max_depth-1);
        }
    }

  return retval;
}

bool
is_strict_superclass (const cdef_class& clsa, const cdef_class& clsb)
{
  return is_superclass (clsa, clsb, false);
}

bool
is_direct_superclass (const cdef_class& clsa, const cdef_class& clsb)
{
  return is_superclass (clsa, clsb, false, 1);
}

cdef_package
lookup_package (const std::string& name, bool error_if_not_found,
                bool load_if_not_found)
{
  cdef_manager& cdm = __get_cdef_manager__ ();

  return cdm.find_package (name, error_if_not_found, load_if_not_found);
}

cdef_class
get_class_context (std::string& name, bool& in_constructor)
{
  name = "";
  in_constructor = false;

  cdef_class cls;

  // If the dispatch class is set in the current stack frame it
  // overrides whatever dispatch class there is for the currently
  // executing function so that function handles returned from class
  // methods will use the dispatch class of the class in which they
  // are defined instead of the class in which they are executing.

  tree_evaluator& tw = __get_evaluator__ ();

  std::string dispatch_class = tw.get_dispatch_class ();

  if (! dispatch_class.empty ())
    return lookup_class (dispatch_class);

  octave_function *fcn = tw.current_function ();

  if (fcn && (fcn->is_class_method ()
              || fcn->is_classdef_constructor ()
              || fcn->is_anonymous_function_of_class ()
              || (fcn->is_private_function ()
                  && ! fcn->dispatch_class ().empty ())))
    {
      cls = lookup_class (fcn->dispatch_class ());

      name = fcn->name ();
      in_constructor = fcn->is_classdef_constructor ();
    }

  return cls;
}

cdef_class
get_class_context (void)
{
  std::string dummy_string;
  bool dummy_bool;

  return get_class_context (dummy_string, dummy_bool);
}

bool
check_access (const cdef_class& cls, const octave_value& acc,
              const std::string& meth_name, const std::string& prop_name,
              bool is_prop_set)
{
  if (acc.is_string ())
    {
      std::string acc_s = acc.string_value ();

      if (acc_s == "public")
        return true;

      cdef_class ctx = get_class_context ();

      // The access is private or protected, this requires a
      // valid class context.

      if (ctx.ok ())
        {
          if (acc_s == "private")
            return (ctx == cls);
          else if (acc_s == "protected")
            {
              if (is_superclass (cls, ctx))
                // Calling a protected method in a superclass.
                return true;
              else if (is_strict_superclass (ctx, cls))
                {
                  // Calling a protected method or property in a derived
                  // class.  This is only allowed if the context class knows
                  // about it and has access to it.

                  if (! meth_name.empty ())
                    {
                      cdef_method m = ctx.find_method (meth_name);

                      if (m.ok ())
                        return check_access (ctx,
                                             m.get ("Access"), meth_name);

                      return false;
                    }
                  else if (! prop_name.empty ())
                    {
                      cdef_property p = ctx.find_property (prop_name);

                      if (p.ok ())
                        {
                          octave_value p_access = p.get (is_prop_set ?
                                                         "SetAccess" :
                                                         "GetAccess");

                          return check_access (ctx, p_access, meth_name,
                                               prop_name, is_prop_set);
                        }

                      return false;
                    }
                  else
                    panic_impossible ();
                }

              return false;
            }
          else
            panic_impossible ();
        }
    }
  else if (acc.isobject ())
    {
      cdef_class ctx = get_class_context ();

      // At this point, a class context is always required.
      if (ctx.ok ())
        {
          if (ctx == cls)
            return true;

          cdef_class acc_cls (to_cdef (acc));

          if (is_superclass (acc_cls, ctx))
            return true;
        }
    }
  else if (acc.iscell ())
    {
      Cell acc_c = acc.cell_value ();

      cdef_class ctx = get_class_context ();

      // At this point, a class context is always required.

      if (ctx.ok ())
        {
          if (ctx == cls)
            return true;

          for (int i = 0; i < acc.numel (); i++)
            {
              cdef_class acc_cls (to_cdef (acc_c(i)));

              if (is_superclass (acc_cls, ctx))
                return true;
            }
        }
    }
  else
    error ("invalid property/method access in class '%s'",
           cls.get_name ().c_str ());

  return false;
}

OCTAVE_END_NAMESPACE(octave)
