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

#include <algorithm>
#include <vector>

#include "unwind-prot.h"

#include "cdef-class.h"
#include "cdef-manager.h"
#include "cdef-method.h"
#include "cdef-package.h"
#include "cdef-property.h"
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

#define OCTAVE_CDEF_CLASS_DEBUG 0
#if OCTAVE_CDEF_CLASS_DEBUG
#  include <iostream>
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

static octave_value
make_fcn_handle (const octave_value& fcn, const std::string& meth_name,
                 const std::string& class_name)
{
  octave_value retval;

  if (fcn.is_defined ())
    {
      // FCN_HANDLE: METHOD
      octave_fcn_handle *fh
        = new octave_fcn_handle (fcn, class_name, meth_name);

      retval = octave_value (fh);
    }

  return retval;
}

//! Initialize class attributes and inherit superclass methods.
//!
//! Local declarations are installed after this constructor returns.  A local
//! method may resolve an inherited ambiguity, so check_method_conflicts() must
//! run after all declarations have been installed.  Properties remain in
//! their defining classes and are collected when needed.
//!
//! make_meta_class() performs the final conflict check and marks the class
//! abstract if any effective method or property remains abstract.
cdef_class::cdef_class_rep::cdef_class_rep
 (std::string name,
  cdef_class_opt_properties opt_args)
  : cdef_meta_object_rep (), m_member_count (0), m_handle_class (false),
    m_meta (false)
{
  set_class (__get_cdef_manager__ ().meta_class ());

  // "put" handles type validation
  put ("Abstract", opt_args.abstract);
  put ("AllowedSubclasses", opt_args.allowed_subclasses);
  put ("Aliases", opt_args.aliases);
  put ("ConstructOnLoad", opt_args.construct_on_load);
  put ("ContainingPackage", Matrix ());
  put ("Description", opt_args.description);
  put ("DetailedDescription", opt_args.detailed_description);
  put ("Enumeration", opt_args.enumeration);
  put ("Hidden", opt_args.hidden);
  put ("InferiorClasses", opt_args.inferior_classes);
  put ("Name", name);
  put ("Namespace", opt_args.namespace_class);
  put ("RestrictsSubclassing", opt_args.restricts_subclassing);
  put ("Sealed", opt_args.sealed);

  // Sets the superclass list.
  //
  // First, we make sure that no inheritance cycles can occur.  As an
  // example, `foo < bar` and `bar < foo` cannot be possible.
  //
  // Each individual classdef flattens all the superclass method tables
  // into a single table held by the classdef's internal representation
  // itself.  Conflicts are recorded until local methods are installed.
  //
  // Another property that is checked is whether the current class is a
  // handle class.  If at least one superclass is a handle class, then
  // two cases can occur:
  //
  // (1) Success: All the non-handle superclasses are marked
  //     HandleCompatible.  The classdef is a valid handle class.
  //
  // (2) Failure: At least one of the non-handle superclasses is not
  //     HandleCompatible.  The classdef is not valid.

  m_handle_class = false;
  bool handle_compatible_sclasses = true;
  auto superclass_list =
    opt_args.superclass_list.classdef_object_value ()->get_object ();

  std::vector<cdef_object> sclass_all_properties;
  std::vector<std::string> sclass_names = {name};

  if (superclass_list.ok ())
    {
      for (octave_idx_type i = 0; i < superclass_list.numel (); i++)
        {
          std::string curr_sclass_name =
            superclass_list(i).get ("Name").string_value ();
          auto s_it = std::find (sclass_names.begin (), sclass_names.end (),
                                 curr_sclass_name);
          if (s_it != sclass_names.end ())
            error ("invalid superclass list: class '%s' is duplicated or inherits from itself", curr_sclass_name.c_str ());

          sclass_names.push_back (curr_sclass_name);

          auto superclass = cdef_class (superclass_list (i));
          m_implicit_ctor_list.push_back (superclass);

          if (superclass.is_handle_class ())
            m_handle_class = true;
          else
            handle_compatible_sclasses = handle_compatible_sclasses
                                         && superclass.get_handle_compatible ();

          if (superclass.get_sealed ())
            error ("inheriting from sealed class: classdef %s is Sealed, and cannot be inherited from", superclass.get_name ().c_str ());

          superclass.get_rep ()->check_method_conflicts ();

          // Look into the superclass method maps.  If we detect conflicting
          // methods, then check if their DefiningClass is the same.  If yes,
          // that's okay (diamond inheritance), otherwise defer the error until
          // local overrides are installed.
          auto superclass_m_map = superclass.get_method_map ();

          for (const auto& pair : superclass_m_map)
            {
              // Private superclass methods are not inherited.
              octave_value acc = pair.second.get ("Access");

              if (acc.is_string () && acc.string_value () == "private")
                continue;

              auto it = m_method_map.find (pair.first);

              if (it != m_method_map.end ())
                {
                  std::string nm_1 = pair.second.get_defining_class ().get_name ();
                  std::string nm_2 = it->second.get_defining_class ().get_name ();

                  if (nm_1 != nm_2)
                    m_method_conflicts.try_emplace (pair.first, nm_2, nm_1);
                }

              m_method_map.insert (pair);
            }
        }
    }

  // Because we've processed the superclasses, we can now see if the
  // class is handle compatible
  if (m_handle_class)
    {
      if (! handle_compatible_sclasses)
        error ("non handle-compatible class: cannot mix handle and non-HandleCompatible classes");

      put ("HandleCompatible", true);
    }
  else
    put ("HandleCompatible", opt_args.handle_compatible);

  put ("SuperclassList", opt_args.superclass_list);
}

void
cdef_class::cdef_class_rep::put (const std::string& pname,
                                 const octave_value& val)
{
  try
    {
      if (pname == "Abstract")
        {
          auto coerced = val.bool_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "Aliases")
        {
          // TODO: Need a way to coerce a string array
          m_map.assign (pname, val);
        }
      else if (pname == "AllowedSubclasses")
        {
          m_map.assign (pname, val);
        }
      else if (pname == "ConstructOnLoad")
        {
          auto coerced = val.bool_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "ContainingPackage")
        {
          // TODO: rename to Namespace"
          // Can be a meta.package object or empty
          m_map.assign (pname, val);
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
      else if (pname == "Enumeration")
        {
          auto coerced = val.bool_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "EnumerationMemberList")
        {
          m_map.assign (pname, val);
        }
      else if (pname == "Events" || pname == "EventList")
        {
          // TODO: check for meta.event
          m_map.assign (pname, val);
        }
      else if (pname == "HandleCompatible")
        {
          auto coerced = val.bool_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "Hidden")
        {
          auto coerced = val.bool_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "InferiorClasses")
        {
          if (val.iscell ())
            {
              Cell classes = val.cell_value ();
              Array<cdef_object> objects (dim_vector (classes.numel (), 1));

              for (octave_idx_type i = 0; i < classes.numel (); i++)
                {
                  if (! classes(i).is_classdef_object ()
                      || classes(i).class_name () != "meta.class"
                      || classes(i).numel () != 1)
                    error ("InferiorClasses must contain scalar meta.class objects");

                  objects(i) = to_cdef (classes(i));
                }

              m_map.assign
                (pname, to_ov (cdef_object
                              (objects, __get_cdef_manager__ ().meta_class ())));
            }
          else
            {
              cdef_object objects = to_cdef (val);

              // The class of an empty list is not yet available during
              // bootstrap, before meta.class itself has been created.
              if (objects.get_class ().ok ()
                  && objects.class_name () != "meta.class")
                error ("InferiorClasses must be a meta.class array");

              m_map.assign (pname, val);
            }
        }
      else if (pname == "Name")
        {
          auto coerced = val.string_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "Namespace")
        {
          m_map.assign (pname, val);
        }
      else if (pname == "RestrictsSubclassing")
        {
          auto coerced = val.bool_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "Sealed")
        {
          auto coerced = val.bool_value ();
          m_map.assign (pname, coerced);
        }
      else if (pname == "SuperClasses"
               || pname == "SuperclassList" || pname == "SuperClassList")
        {
          val.classdef_object_value ();

          m_map.assign ("SuperclassList", val);
        }
      else
        {
          error ("unknown property '%s' of meta.class", pname.c_str ());
        }
    }
  catch (const execution_exception& ee)
    {
      error ("put: setting '%s' on meta.class failed with error: %s",
             pname.c_str (), ee.message ().c_str ());
    }
}

octave_value
cdef_class::cdef_class_rep::get (const std::string& pname) const
{
  try
    {
      if (pname == "Abstract"              || pname == "Aliases"              ||
          pname == "AllowedSubclasses"     || pname == "ConstructOnLoad"      ||
          pname == "ContainingPackage"     || pname == "Description"          ||
          pname == "DetailedDescription"   || pname == "Enumeration"          ||
          pname == "EnumerationMemberList" || pname == "EventList"            ||
          pname == "HandleCompatible"      || pname == "Hidden"               ||
          pname == "InferiorClasses"       || pname == "Name"                 ||
          pname == "Namespace"             || pname == "RestrictsSubclassing" ||
          pname == "Sealed"                || pname == "SuperClasses"         ||
          pname == "SuperclassList"        || pname == "SuperClassList")
        {
          return cdef_object_scalar::get (pname);
        }
      else if (pname == "Methods" || pname == "MethodList")
        {
          return to_ov (get_methods ());
        }
      else if (pname == "Properties" || pname == "PropertyList")
        {
          return to_ov (get_properties ());
        }
      else
        {
          error ("unknown property '%s' of meta.class", pname.c_str ());
        }
    }
  catch (const execution_exception& ee)
    {
      error ("get: getting '%s' on meta.class failed with error: %s",
             pname.c_str (), ee.message ().c_str ());
    }
}

cdef_method
cdef_class::cdef_class_rep::find_method (const std::string& nm, bool local) const
{
  auto it = m_method_map.find (nm);

  if (it == m_method_map.end ())
    {
      // Private methods of a superclass are deliberately absent from this map
      // because they are not inherited.  They must nevertheless remain callable
      // from the methods of the class that declares them, even when the object
      // is an instance of a derived class, so fall back to the class context.
      // Callers outside that class never reach this path with a matching
      // context, so the methods stay private.
      //
      // Only a class that has superclasses can have such a method to recover.
      // Checking that first avoids unnecessary context lookups.

      octave_value sclist = get ("SuperclassList");

      if (! sclist.isempty ())
        {
          cdef_class ctx = get_class_context ();

          if (ctx.ok ())
            {
              cdef_class self = const_cast<cdef_class_rep *> (this)->wrap ();

              if (is_strict_superclass (ctx, self))
                {
                  cdef_method meth = ctx.find_method (nm, true);

                  if (meth.ok ())
                    {
                      octave_value acc = meth.get ("Access");

                      if (acc.is_string () && acc.string_value () == "private")
                        return meth;
                    }
                }
            }
        }

      return cdef_method ();
    }

  cdef_method meth = it->second;

  if (local && meth.get_defining_class ().get_name () != get_name ())
    return cdef_method ();

  return meth;
}

class ctor_analyzer : public tree_walker
{
public:

  ctor_analyzer () = delete;

  ctor_analyzer (const std::string& ctor, const std::string& obj)
    : tree_walker (), m_who (ctor), m_obj_name (obj) { }

  OCTAVE_DISABLE_COPY_MOVE (ctor_analyzer)

  ~ctor_analyzer () = default;

  void visit_statement (tree_statement& t)
  {
    if (t.is_expression ())
      t.expression ()->accept (*this);
  }

  void visit_simple_assignment (tree_simple_assignment& t)
  {
    t.right_hand_side ()->accept (*this);
  }

  void visit_multi_assignment (tree_multi_assignment& t)
  {
    t.right_hand_side ()->accept (*this);
  }

  void visit_index_expression (tree_index_expression& t)
  {
    t.expression ()->accept (*this);
  }

  std::list<cdef_class> get_constructor_list () const
  { return m_ctor_list; }

  // NO-OP

  void visit_anon_fcn_handle (tree_anon_fcn_handle&) { }
  void visit_argument_list (tree_argument_list&) { }
  void visit_binary_expression (tree_binary_expression&) { }
  void visit_break_command (tree_break_command&) { }
  void visit_colon_expression (tree_colon_expression&) { }
  void visit_continue_command (tree_continue_command&) { }
  void visit_decl_command (tree_decl_command&) { }
  void visit_decl_init_list (tree_decl_init_list&) { }
  void visit_decl_elt (tree_decl_elt&) { }
  void visit_simple_for_command (tree_simple_for_command&) { }
  void visit_complex_for_command (tree_complex_for_command&) { }
  void visit_octave_user_script (octave_user_script&) { }
  void visit_octave_user_function (octave_user_function&) { }
  void visit_function_def (tree_function_def&) { }
  void visit_identifier (tree_identifier&) { }
  void visit_if_clause (tree_if_clause&) { }
  void visit_if_command (tree_if_command&) { }
  void visit_if_command_list (tree_if_command_list&) { }
  void visit_switch_case (tree_switch_case&) { }
  void visit_switch_case_list (tree_switch_case_list&) { }
  void visit_switch_command (tree_switch_command&) { }
  void visit_matrix (tree_matrix&) { }
  void visit_cell (tree_cell&) { }
  void visit_no_op_command (tree_no_op_command&) { }
  void visit_constant (tree_constant&) { }
  void visit_fcn_handle (tree_fcn_handle&) { }
  void visit_parameter_list (tree_parameter_list&) { }
  void visit_postfix_expression (tree_postfix_expression&) { }
  void visit_prefix_expression (tree_prefix_expression&) { }
  void visit_return_command (tree_return_command&) { }
  void visit_try_catch_command (tree_try_catch_command&) { }
  void visit_unwind_protect_command (tree_unwind_protect_command&) { }
  void visit_while_command (tree_while_command&) { }
  void visit_do_until_command (tree_do_until_command&) { }

  void visit_superclass_ref (tree_superclass_ref& t)
  {
    if (t.method_name () == m_obj_name)
      {
        std::string class_name = t.class_name ();

        cdef_class cls = lookup_class (class_name, false);

        if (cls.ok ())
          m_ctor_list.push_back (cls);
      }
  }

private:

  // The name of the constructor being analyzed.
  std::string m_who;

  // The name of the first output argument of the constructor.
  std::string m_obj_name;

  // The list of superclass constructors that are explicitly called.
  std::list<cdef_class> m_ctor_list;
};

void
cdef_class::cdef_class_rep::install_method (const cdef_method& meth)
{
  std::string mname = meth.get_name ();

  auto it = m_method_map.find (mname);

  //  Check if a method with the same name already exists before
  //  installing a new one.
  //  Overriding a method is okay; otherwise error.
  if (it != m_method_map.end ())
    {
      const std::string& other_class_name = it->second.get_defining_class ().get_name ();

      if (other_class_name == get_name ())
        {
          if (it->second.is_external () && meth.is_external ())
            return;

          std::string class_file = file_name ();
          error ("duplicate method '%s' in class '%s' in file '%s'",
                 mname.c_str (), get_name ().c_str (), class_file.c_str ());
        }
    }

  // Check that it does not conflict with the property names
  //
  // There is one case where the method and the property names can match (see
  // bug #67361): If the classdef is defined within a class folder and the
  // method is externally defined, and the method signature is not in the
  // classdef file, then the method and a property can have the same name.
  cdef_property prop = find_property (mname);

  if (prop.ok () && ! meth.is_external ())
    {
      error ("property and method are both named '%s' in class '%s' in file '%s'",
             mname.c_str (), get_name ().c_str (), file_name ().c_str ());
    }

  if (! meth.get_function ().function_value (true))
    error ("invalid method: no underlying function");

  if (mname.size () > 4)
    {
      std::string prefix = mname.substr (0, 4);

      if (prefix == "get." || prefix == "set.")
        {
          std::string prop_name = mname.substr (4);

          auto pit = m_property_map.find (prop_name);
          if (pit == m_property_map.end ())
            error ("class '%s': %s method '%s' has no corresponding property",
                   get_name ().c_str (), prefix == "get." ? "get" : "set",
                   prop_name.c_str ());

          if (prefix == "get.")
            pit->second.put ("GetMethod",
                             make_fcn_handle (meth.get_function (), mname,
                                              get_name ()));
          else
            pit->second.put ("SetMethod",
                             make_fcn_handle (meth.get_function (), mname,
                                              get_name ()));

          return;
        }
    }

  m_method_map[mname] = meth;
  m_method_conflicts.erase (mname);

  m_member_count++;

  if (meth.get("Abstract").bool_value ())
    put ("Abstract", true);

  if (meth.is_constructor ())
    {
      // Analyze the constructor code to determine what superclass
      // constructors are called explicitly.

      octave_value ov_fcn = meth.get_function ();

      if (ov_fcn.is_defined ())
        {
          octave_user_function *uf = ov_fcn.user_function_value (true);

          if (uf)
            {
              tree_parameter_list *ret_list = uf->return_list ();
              tree_statement_list *body = uf->body ();

              if (! ret_list || ret_list->size () < 1)
                error ("%s: invalid constructor output arguments",
                       meth.get_name ().c_str ());

              std::string m_obj_name = ret_list->front ()->name ();
              ctor_analyzer a (meth.get_name (), m_obj_name);

              body->accept (a);

              std::list<cdef_class> explicit_ctor_list
                = a.get_constructor_list ();

              for (const auto& cdef_cls : explicit_ctor_list)
                  m_implicit_ctor_list.remove (cdef_cls);
            }
        }
    }
}

void
cdef_class::cdef_class_rep::check_method_conflicts () const
{
  if (! m_method_conflicts.empty ())
    {
      const auto& [name, classes] = *m_method_conflicts.begin ();
      error ("method %s: conflicting definitions in classes '%s' and '%s'",
             name.c_str (), classes.first.c_str (), classes.second.c_str ());
    }
}

cdef_object
cdef_class::cdef_class_rep::get_methods (bool include_ctor) const
{
  auto method_map = get_method_map (member_mode::all, include_ctor);

  Array<cdef_object> arr (dim_vector (method_map.size (), 1));
  octave_idx_type idx = 0;

  for (const auto& meth : method_map)
    arr(idx++) = meth.second;

  return cdef_object (arr, __get_cdef_manager__ ().meta_method ());
}

std::map<std::string, cdef_method>
cdef_class::cdef_class_rep::get_method_map (member_mode mode,
                                            bool include_ctor) const
{
  std::map<std::string, cdef_method> retval = m_method_map;
  std::string this_name = get_name ();

  if (mode == member_mode::declared)
    std::erase_if (retval, [this_name](const auto& pair) {
      return pair.second.get_defining_class ().get_name () != this_name;
    });
  else if (mode == member_mode::all_visible)
    std::erase_if (retval, [](const auto& pair) {
      octave_value acc = pair.second.get ("Access");
      octave_value hid = pair.second.get ("Hidden");

      return (! acc.is_string () || acc.string_value () != "public"
              || hid.bool_value ());
    });

  if (! include_ctor)
    std::erase_if (retval, [](const auto& pair) {
      return pair.second.is_constructor ();
    });

  return retval;
}

cdef_object
cdef_class::cdef_class_rep::get_superclass_list () const
{
  octave_value val = get ("SuperclassList");

  return val.classdef_object_value ()->get_object ();
}

cdef_property
cdef_class::cdef_class_rep::find_property (const std::string& nm) const
{
  auto it = m_property_map.find (nm);

  if (it != m_property_map.end ())
    {
      cdef_property prop = it->second;

      if (prop.ok ())
        return prop;
    }

  // Look into superclasses

  auto super_classes = get_superclass_list ();
  cdef_class cls1;
  int nfnd = 0;

  for (int i = 0; i < super_classes.numel (); i++)
    {
      cdef_class cls (super_classes(i));

      cdef_property prop = cls.find_property (nm);

      if (prop.ok ())
        {
          nfnd++;
          if (nfnd == 1)
            cls1 = cls;
          else if (nfnd == 2)
            // FIXME: This error is emitted when a property with conflicting
            //        definitions is attempted to be used.  Ideally, this
            //        error would be emitted on construction of the object.
            error ("property %s: conflicting definitions in classes '%s' and '%s'",
                   nm.c_str (), cls1.get_name ().c_str (),
                   cls.get_name ().c_str ());
        }
     }

  if (nfnd == 1)
    return cls1.find_property (nm);

  return cdef_property ();
}

void
cdef_class::cdef_class_rep::install_property (const cdef_property& prop)
{
  std::string prop_name = prop.get_name ();

  // Check that property name doesn't conflict with classdef name (bug #55767)
  if (prop_name == get_name ())
    error ("invalid property: A property cannot have the same name as the classdef it is in");

  // Check if a method with the same name already exists before installing a
  // new one.  We can check the current class's method map only because we know
  // the inherited public and protected superclass methods are all flattened
  // into the method map of the current classdef.
  auto meth_it = m_method_map.find (prop_name);

  if (meth_it != m_method_map.end ())
    {
      std::string class_file = file_name ();
      error ("property and method are both named '%s' in class '%s' in file '%s'",
             prop_name.c_str (), get_name ().c_str (), class_file.c_str ());
    }

  // Check if a property with the same name already exists before installing a
  // new one.  Unlike methods, properties of superclasses are not flattened
  // into the current class's property map, so we have to use a specialized
  // method to investigate for property conflicts.
  auto prop_match = find_property (prop_name);
  const bool declared_here = m_property_map.contains (prop_name);

  if (prop_match.ok ()
      && (declared_here || ! prop_match.get ("Abstract").bool_value ()))
    {
      std::string class_file = file_name ();

      // Matlab lets a subclass declare a property of the same name as a
      // property private to a superclass, and keeps both.  An object here
      // keeps one value for each property name, so it could not.
      octave_value get_acc = prop_match.get ("GetAccess");
      octave_value set_acc = prop_match.get ("SetAccess");

      if (! declared_here
          && get_acc.is_string () && get_acc.string_value () == "private"
          && set_acc.is_string () && set_acc.string_value () == "private")
        {
          cdef_class super_cls (to_cdef (prop_match.get ("DefiningClass")));

          error ("property '%s' in class '%s' in file '%s': Octave cannot declare a property private to superclass '%s' again",
                 prop_name.c_str (), get_name ().c_str (), class_file.c_str (),
                 super_cls.get_name ().c_str ());
        }

      error ("duplicate property '%s' in class '%s' in file '%s'",
             prop_name.c_str (), get_name ().c_str (), class_file.c_str ());
    }

  // If the current property is marked Abstract, then the whole class is marked
  // Abstract.  Only subclasses can be concrete from this point forward.
  if (prop.get ("Abstract").bool_value ())
    put ("Abstract", true);

  m_property_map[prop_name] = prop;

  // Register the insertion rank of this property
  m_property_rank_map[prop.get_name ()] = m_member_count;

  m_member_count++;
}

cdef_object
cdef_class::cdef_class_rep::get_properties () const
{
  auto property_map = get_property_map (member_mode::all);

  Array<cdef_object> arr (dim_vector (property_map.size (), 1));
  octave_idx_type idx = 0;

  for (const auto& nm_prop : property_map)
    arr(idx++) = nm_prop.second;

  return cdef_object (arr, __get_cdef_manager__ ().meta_property ());
}

std::map<property_key, cdef_property>
cdef_class::cdef_class_rep::get_property_map (member_mode mode) const
{
  std::map<property_key, cdef_property> props;
  std::set<std::string> names;
  std::set<const cdef_class_rep *> visited;
  unsigned int next_rank = 0;

  find_properties (props, names, visited, next_rank, mode);

  return props;
}

void
cdef_class::cdef_class_rep::find_properties
  (std::map<property_key, cdef_property>& props,
   std::set<std::string>& names,
   std::set<const cdef_class_rep *>& visited,
   unsigned int& next_rank, member_mode mode) const
{
  if (! visited.insert (this).second)
    return;

  const unsigned int offset = next_rank;

  for (const auto& it : m_property_map)
    {
      const std::string nm = it.second.get_name ();

      if (mode == member_mode::all_visible)
        {
          octave_value acc = it.second.get ("GetAccess");
          octave_value hid = it.second.get ("Hidden");

          if ((acc.is_string () && acc.string_value () == "private")
              || hid.bool_value ())
            continue;
        }

      const unsigned int rank = offset + m_property_rank_map.at (nm);

      if (names.insert (nm).second)
        props[std::make_pair (rank, nm)] = it.second;

      next_rank = std::max (next_rank, rank + 1);
    }

  if (mode == member_mode::declared)
    return;

  auto super_classes = get_superclass_list ();

  for (int i = 0; i < super_classes.numel (); i++)
    {
      cdef_class cls = lookup_class (super_classes(i));

      cls.get_rep ()->find_properties (props, names, visited, next_rank, mode);
    }
}

void
cdef_class::cdef_class_rep::find_names (std::set<std::string>& names,
                                        bool all)
{
  for (const auto& cls_fnmap : m_method_map)
    {
      if (! cls_fnmap.second.is_constructor ())
        {
          std::string nm = cls_fnmap.second.get_name ();

          if (! all)
            {
              octave_value acc = cls_fnmap.second.get ("Access");

              if (! acc.is_string()
                  || acc.string_value () != "public")
                continue;
            }

          names.insert (nm);
        }
    }

  for (const auto& pname_prop : m_property_map)
    {
      std::string nm = pname_prop.second.get_name ();

      if (! all)
        {
          octave_value acc = pname_prop.second.get ("GetAccess");

          if (! acc.is_string()
              || acc.string_value () != "public")
            continue;
        }

      names.insert (nm);
    }

  // Look into superclasses

  auto super_classes = get_superclass_list ();

  for (int i = 0; i < super_classes.numel (); i++)
    {
      cdef_class cls = lookup_class (super_classes(i));

      cls.get_rep ()->find_names (names, all);
    }
}

string_vector
cdef_class::cdef_class_rep::get_names ()
{
  std::set<std::string> names;

  find_names (names, false);

  string_vector v (names);

  return v.sort (true);
}

void
cdef_class::cdef_class_rep::delete_object (const cdef_object& obj)
{
  cdef_method dtor = find_method ("delete");

  // FIXME: would it be better to tell find_method above to not find
  // overloaded functions?

  if (dtor.ok () && dtor.is_defined_in_class (get_name ()))
    dtor.execute (obj, octave_value_list (), 0, true, "destructor");

  // FIXME: should we destroy corresponding properties here?

  // Call "delete" in super classes

  auto super_classes = get_superclass_list ();

  for (int i = 0; i < super_classes.numel (); i++)
    {
      cdef_class cls = lookup_class (super_classes(i));

      if (cls.get_name () != "handle")
        cls.delete_object (obj);
    }
}

octave_value_list
cdef_class::cdef_class_rep::meta_subsref (const std::string& type,
    const std::list<octave_value_list>& idx,
    int nargout)
{
  std::size_t skip = 1;

  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      // Constructor call

#if OCTAVE_CDEF_CLASS_DEBUG
      std::cerr << "constructor" << std::endl;
#endif

      retval = construct_object (idx.front (), nargout);
      break;

    case '.':
      {
        // Static method, constant (or property?)

#if OCTAVE_CDEF_CLASS_DEBUG
        std::cerr << "static method/property" << std::endl;
#endif

        if (idx.front ().length () != 1)
          error ("invalid meta.class indexing");

        std::string nm = idx.front ()(0).xstring_value ("invalid meta.class indexing, expected a method or property name");

        cdef_method meth = find_method (nm);

        if (meth.ok ())
          {
            if (! meth.is_static ())
              error ("method '%s' is not static", nm.c_str ());

            octave_value_list args;

            if (type.length () > 1 && idx.size () > 1 && type[1] == '(')
              {
                args = *(++(idx.begin ()));
                skip++;
              }

            retval = meth.execute (args, (type.length () > skip
                                          ? 1 : nargout), true,
                                   "meta.class");
          }
        else
          {
            cdef_property prop = find_property (nm);

            if (! prop.ok ())
              error ("no such method or property '%s'", nm.c_str ());

            if (! prop.is_constant ())
              error ("property '%s' is not constant", nm.c_str ());

            retval(0) = prop.get_value (true, "meta.class");
          }
      }
      break;

    default:
      error ("invalid meta.class indexing");
      break;
    }

  if (type.length () > skip && idx.size () > skip && ! retval.empty ())
    retval = retval(0).next_subsref (nargout, type, idx, skip);

  return retval;
}

void
cdef_class::cdef_class_rep::meta_release ()
{
  cdef_manager& cdm = __get_cdef_manager__ ();

  cdm.unregister_class (wrap ());
}

void
cdef_class::cdef_class_rep::initialize_object (cdef_object& obj)
{
  // Populate the object with default property values

  auto super_classes = get_superclass_list ();

  for (int i = 0; i < super_classes.numel (); i++)
    cdef_class (super_classes(i)).initialize_object (obj);

  for (const auto& pname_prop : m_property_map)
    {
      if (! pname_prop.second.get ("Dependent").bool_value ())
        {
          octave_value pvalue = pname_prop.second.get ("DefaultValue");

          if (pvalue.is_defined ())
            obj.put (pname_prop.first, pvalue);
          else
            obj.put (pname_prop.first, octave_value (Matrix ()));
        }
    }

  obj.mark_for_construction (wrap ());
}

octave_value_list
cdef_class::cdef_class_rep::run_constructor (cdef_object& obj,
    const octave_value_list& args,
    const int nargout,
    bool default_initialize)
{
  octave_value_list retval;

  if (! default_initialize)
    {
      octave_value_list empty_args;

      for (auto supcls : m_implicit_ctor_list)
        {
          supcls.run_constructor (obj, empty_args);
        }

      std::string cls_name = get_name ();
      std::string ctor_name = get_base_name (cls_name);

      cdef_method ctor = find_method (ctor_name);

      if (ctor.ok ())
        {
          octave_value_list ctor_args (args);

          ctor_args.prepend (to_ov (obj));
          retval = ctor.execute (ctor_args, nargout, true, "constructor");

          if (retval.length () < 1)
            error ("%s: invalid number of output arguments for classdef constructor",
                   ctor_name.c_str ());

          obj = to_cdef (retval(0));
        }
    }

  obj.mark_as_constructed (wrap ());

  if (retval.empty ())
    retval.resize (1);

  retval(0) = to_ov (obj);

  return retval;
}

octave_value
cdef_class::cdef_class_rep::get_method (const std::string& name) const
{
  auto p = m_method_map.find (name);

  if (p == m_method_map.end ())
    return octave_value ();

  return p->second.get_function ();
}

octave_value
cdef_class::cdef_class_rep::get_method (int line) const
{
  octave_value closest_match;
  int closest_match_end_line = std::numeric_limits<int>::max ();
  // Since we have a dynamic cast, performance could be an issue if this is
  // called from a critical path.  If performance is an issue, we can cache
  // an ordered version of the method map
  for (auto i = m_method_map.cbegin (); i != m_method_map.cend (); ++i)
    {
      const octave_value& fcn = i->second.get_function ();
      octave_user_code *user_code = fcn.user_code_value (true);

      // Line numbers refer to this classdef file.  Inherited methods and
      // methods defined in separate files have unrelated source positions.
      if (! user_code || user_code->fcn_file_name () != file_name ())
        continue;

      octave_user_function *pfcn
        = dynamic_cast<octave_user_function *> (user_code);

      if (! pfcn)
        continue;

      octave::filepos end_pos = pfcn->end_pos ();

      int end_line = end_pos.line ();

      if (line <= end_line && end_line <= closest_match_end_line
          && pfcn->is_defined () && pfcn->is_user_code ())
        {
          closest_match = fcn;
          closest_match_end_line = end_line;
        }
    }

  // repeat the same for set and get methods of properties
  for (auto i = m_property_map.cbegin (); i != m_property_map.cend (); ++i)
    {
      const octave_value& get_meth = i->second.get ("GetMethod");
      if (get_meth.is_function_handle ())
        {
          octave_user_function *pfcn
            = get_meth.user_function_value ();

          if (! pfcn)
            continue;

          octave::filepos end_pos = pfcn->end_pos ();

          int end_line = end_pos.line ();

          if (line <= end_line && end_line <= closest_match_end_line
              && pfcn->is_defined () && pfcn->is_user_code ())
            {
              closest_match = get_meth;
              closest_match_end_line = end_line;
            }
        }

      const octave_value& set_meth = i->second.get ("SetMethod");
      if (set_meth.is_function_handle ())
        {
          octave_user_function *pfcn
            = set_meth.user_function_value ();

          if (! pfcn)
            continue;

          octave::filepos end_pos = pfcn->end_pos ();

          int end_line = end_pos.line ();

          if (line <= end_line && end_line <= closest_match_end_line
              && pfcn->is_defined () && pfcn->is_user_code ())
            {
              closest_match = set_meth;
              closest_match_end_line = end_line;
            }
        }
    }

  return closest_match;
}

octave_value
cdef_class::cdef_class_rep::construct (const octave_value_list& args,
                                       const bool default_initialize)
{
  octave_value_list retval = construct_object (args, 1, default_initialize);

  return retval(0);
}

octave_value_list
cdef_class::cdef_class_rep::construct_object (const octave_value_list& args,
                                              const int nargout,
                                              const bool default_initialize)
{
  check_method_conflicts ();

  if (is_abstract ())
    error ("cannot instantiate object for abstract class '%s'",
           get_name ().c_str ());

  cdef_object obj;
  octave_value_list retval (std::max (nargout, 1));

  if (is_handle_class ())
    obj = adopt (new handle_cdef_object ());
  else
    obj = adopt (new value_cdef_object ());
  obj.set_class (wrap ());

  initialize_object (obj);

  retval = run_constructor (obj, args, nargout, default_initialize);

  return retval;
}

//! Construct an object to fill a gap when an object array grows.
//!
//! Ordinary classes use their zero-argument constructor.  Built-in metaclasses
//! instead receive internal placeholder metadata belonging to the active
//! interpreter.  This does not permit public construction of abstract classes.
cdef_object
cdef_class::cdef_class_rep::construct_array_element ()
{
  if (! is_meta_class ())
    return to_cdef (construct (octave_value_list ()));

  const cdef_class this_cls = wrap ();
  cdef_manager& cdm = __get_cdef_manager__ ();

  // Filler metadata must belong to the active interpreter, just like the
  // array's class.  Do not keep these objects in process-wide static caches.
  if (this_cls == cdm.meta_class ())
    return cdef_class ("");
  else if (this_cls == cdm.meta_property ())
    return cdef_property (cdef_class (""), "");
  else if (this_cls == cdm.meta_method ())
    return cdef_method (cdef_class (""), octave_value (), "");
  else if (this_cls == cdm.meta_package ())
    return cdef_package ("");
  else
    error ("expecting meta class, property, method, or package in cdef_class::cdef_class_rep::construct_array_element - please report this bug");
}

cdef_class
cdef_class::make_meta_class (interpreter& interp,
                             tree_classdef *t, bool is_at_folder)
{
  // Class creation

  std::string class_name = t->ident ()->name ();
  std::string full_class_name = class_name;
  if (! t->package_name ().empty ())
    full_class_name = t->package_name () + '.' + full_class_name;

  // Push a dummy scope frame on the call stack that corresponds to the scope
  // that was used when parsing classdef object.  Without this, we may pick up
  // stray values from the current scope when evaluating expressions found in
  // things like attribute lists.

  tree_evaluator& tw = interp.get_evaluator ();

  tw.push_dummy_scope (full_class_name);
  unwind_action pop_scope (&tree_evaluator::pop_scope, &tw);

  // Search the superclass list.  If a superclass is not loaded, load the
  // superclass before proceeding with initialization of the object.

  std::vector<cdef_object> growable_superclass_list;

  if (t->superclass_list ())
    {
      for (auto& scls : (*t->superclass_list ()))
        {
          std::string sclass_name = (scls)->class_name ();

#if OCTAVE_CDEF_CLASS_DEBUG
          std::cerr << "superclass: " << sclass_name << std::endl;
#endif

          cdef_class sclass = lookup_class (sclass_name);

          if (sclass.get ("Sealed").bool_value ())
            error ("'%s' cannot inherit from '%s', because it is sealed",
                   full_class_name.c_str (), sclass_name.c_str ());

          growable_superclass_list.push_back (sclass);
        }
    }

  Array<cdef_object> superclass_list
    (growable_superclass_list, dim_vector (growable_superclass_list.size (), 1));
  auto superclasses
    = to_ov (cdef_object (superclass_list,
                          __get_cdef_manager__ ().meta_class ()));

  // Docstring of a class is separated into the
  // Description and DetailedDescription properties

  const std::string doc_string = t->doc_string ();
  std::string description = "";
  std::string detailed_description = "";

  size_t pos = doc_string.find ('\n');
  if (pos != std::string::npos)
    {
      description = doc_string.substr (0, pos);

      // Strip the whitespace in front of the description string
      // Weirdly, the whitespace isn't stripped from the detailed_description
      // string
      const std::string whitespaces = " \t\n\r\f\v";
      size_t start = description.find_first_not_of (whitespaces);
      if (start != std::string::npos)
        description.erase (0, start);

      detailed_description = doc_string.substr (pos + 1);
    }

  // Classdef Attributes

  cdef_class_opt_properties opt_args;
  std::map<std::string, octave_value> amap = {
    {"Abstract", opt_args.abstract},
    {"AllowedSubclasses", opt_args.allowed_subclasses},
    {"ConstructOnLoad", opt_args.construct_on_load},
    {"Description", description},
    {"DetailedDescription", detailed_description},
    {"HandleCompatible", opt_args.handle_compatible},
    {"Hidden", opt_args.hidden},
    {"InferiorClasses", opt_args.inferior_classes},
    {"Sealed", opt_args.sealed},
  };

  if (t->attribute_list ())
    {
      for (const auto& attr : (*t->attribute_list ()))
        {
          std::string aname = attr->ident ()->name ();
          auto it = amap.find (aname);

          if (it == amap.end ())
            error ("invalid attribute: attribute %s cannot be assigned in a classdef block", aname.c_str ());

          it->second = compute_attribute_value (tw, attr);
        }
    }

  opt_args = cdef_class_opt_properties {
    .abstract = amap["Abstract"],
    .allowed_subclasses = amap["AllowedSubclasses"],
    .construct_on_load = amap["ConstructOnLoad"],
    .description = amap["Description"],
    .detailed_description = amap["DetailedDescription"],
    .handle_compatible = amap["HandleCompatible"],
    .hidden = amap["Hidden"],
    .inferior_classes = amap["InferiorClasses"],
    .sealed = amap["Sealed"],
    .superclass_list = superclasses,
  };

  cdef_class retval = cdef_class (full_class_name, opt_args);

  cdef_manager& cdm = __get_cdef_manager__ ();
  cdm.register_class (retval);

  bool complete = false;
  unwind_action unregister_incomplete ([&] ()
  {
    if (! complete)
      cdm.unregister_class (retval);
  });

  retval.file_name (t->file_name ());
  retval.doc_string (doc_string);

  // Package owning this class
  if (! t->package_name ().empty ())
    {
      cdef_package pack = cdm.find_package (t->package_name ());

      if (pack.ok ())
        retval.put ("ContainingPackage", to_ov (pack));
    }

  tree_classdef_body *b = t->body ();

  if (b)
    {
      // Property blocks

      // FIXME: default property expression should be able to call static
      //        methods of the class being constructed.  A restricted
      //        CLASSNAME symbol should be added to the scope before
      //        evaluating default value expressions.

      std::list<tree_classdef_properties_block *> pb_list
        = b->property_list ();

      for (auto& pb_p : pb_list)
        {
          cdef_property::make_property (retval, tw, pb_p);
        }

      // Method blocks
      //
      // Methods in a classdef can be defined in three ways:
      //   1) Method signature and body in classdef file,
      //   2) Only method signature in classdef file, body is defined in an
      //      external file in a class (@) folder,
      //   3) Method signature and body defined in an external class folder.
      //      Signature is not needed in the classdef file, the method is just
      //      treated as a default (no attribute) method.
      //
      // Cases (1) and (2) are parsed first.

      std::list<tree_classdef_methods_block *> mb_list = b->method_list ();

      std::vector<cdef_method> mths;
      for (auto& mb_p : mb_list)
        {
          auto blk_mths = cdef_method::make_method (retval, tw, mb_p);
          if (! is_at_folder)
            for (const auto& meth : blk_mths)
              if (meth.is_external () && ! meth.get ("Abstract").bool_value ())
                error ("external methods are only allowed in @-folders");
          mths.insert (mths.end (), blk_mths.begin (), blk_mths.end ());
        }

      if (is_at_folder)
        {
          // Look for all external methods visible on octave path at the
          // time of loading of the class.
          //
          // FIXME: This is an "extension" to Matlab behavior, which only
          // looks in the @-folder containing the original classdef file.
          // However, this is easier to implement it that way at the moment.
          load_path& lp = interp.get_load_path ();

          std::list<std::string> external_methods
            = lp.methods (full_class_name);

          for (const auto& mth_name : external_methods)
            {
              if (mth_name != class_name)
                {
                  // Search if the method signature was already defined
                  auto match_it = std::ranges::find_if (mths, [mth_name](cdef_method mth) {
                    return mth_name == mth.get_name ();
                  });

                  if (match_it != mths.end ())
                    {
                      if (match_it->get_function ().user_function_value ()->body ())
                        error ("invalid method: external method %s cannot be named the same as a classdef method", mth_name.c_str ());
                    }
                  else
                    {
                      // Create a dummy method that is used until the actual
                      // method is loaded.
                      octave_user_function *fcn = new octave_user_function ();
                      fcn->stash_function_name (mth_name);

                      cdef_method meth = cdef_method (retval,
                                                      octave_value (fcn),
                                                      mth_name);

                      mths.push_back (meth);
                    }
                }
            }
        }

        for (auto mth : mths)
          retval.install_method (mth);

    }

  retval.get_rep ()->check_method_conflicts ();

  // Abstractness depends on the effective members after local overrides.
  // An explicitly abstract superclass may have a concrete subclass, but
  // inherited abstract members must all have implementations.
  for (const auto& [name, method] : retval.get_method_map ())
    if (method.get ("Abstract").bool_value ())
      retval.put ("Abstract", true);

  for (const auto& [key, property] :
       retval.get_property_map (member_mode::all))
    if (property.get ("Abstract").bool_value ())
      retval.put ("Abstract", true);

  complete = true;

  return retval;
}

octave_value
cdef_class::get_method_function (const std::string& /* nm */)
{
  return octave_value (new octave_classdef_meta (*this));
}

OCTAVE_END_NAMESPACE(octave)
