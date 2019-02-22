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

#include "call-stack.h"
#include "cdef-manager.h"
#include "cdef-utils.h"
#include "interpreter-private.h"
#include "ov-classdef.h"
#include "ov-usr-fcn.h"

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
        {
          uf->mark_as_class_constructor ();
          uf->mark_as_classdef_constructor ();
        }
      else
        uf->mark_as_class_method ();
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
  cdef_manager& cdm = octave::__get_cdef_manager__ ("lookup_class");

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
          cdef_class cls = lookup_class (c(i));

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
  cdef_manager& cdm = octave::__get_cdef_manager__ ("lookup_package");

  return cdm.find_package (name, error_if_not_found, load_if_not_found);
}

cdef_class
get_class_context (std::string& name, bool& in_constructor)
{
  cdef_class cls;

  octave::call_stack& cs = octave::__get_call_stack__ ("get_class_context");

  octave_function *fcn = cs.current ();

  in_constructor = false;

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
