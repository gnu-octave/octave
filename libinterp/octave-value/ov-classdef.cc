/*

Copyright (C) 2012-2018 Michael Goffioul

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

#include "call-stack.h"
#include "defun.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "load-path.h"
#include "ov-builtin.h"
#include "ov-classdef.h"
#include "ov-fcn-handle.h"
#include "ov-typeinfo.h"
#include "ov-usr-fcn.h"
#include "pt-assign.h"
#include "pt-classdef.h"
#include "pt-eval.h"
#include "pt-funcall.h"
#include "pt-idx.h"
#include "pt-misc.h"
#include "pt-stmt.h"
#include "pt-walk.h"
#include "symtab.h"

// Define to 1 to enable debugging statements.
#define DEBUG_TRACE 0

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

  error ("%s: method `%s' has %s access and cannot be run in this context",
         from.c_str (), meth.get_name ().c_str (), acc_s.c_str ());
}

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

static std::string
get_base_name (const std::string& nm)
{
  std::string::size_type pos = nm.find_last_of ('.');

  if (pos != std::string::npos)
    return nm.substr (pos + 1);

  return nm;
}

static void
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

static void
make_function_of_class (const cdef_class& cls, const octave_value& fcn)
{
  make_function_of_class (cls.get_name (), fcn);
}

static octave_value
make_fcn_handle (octave_builtin::fcn ff, const std::string& nm)
{
  octave_value fcn (new octave_builtin (ff, nm));

  octave_value fcn_handle (new octave_fcn_handle (fcn, nm));

  return fcn_handle;
}

static octave_value
make_fcn_handle (const octave_value& fcn, const std::string& nm)
{
  octave_value retval;

  if (fcn.is_defined ())
    retval = octave_value (new octave_fcn_handle (fcn, nm));

  return retval;
}

static cdef_class
lookup_class (const std::string& name, bool error_if_not_found = true,
              bool load_if_not_found = true)
{
  cdef_manager& cdm = octave::__get_cdef_manager__ ("lookup_class");

  return cdm.find_class (name, error_if_not_found, load_if_not_found);
}

static cdef_class
lookup_class (const cdef_class& cls)
{
  // FIXME: placeholder for the time being, the purpose
  //        is to centralized any class update activity here.

  return cls;
}

static cdef_class
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

static std::list<cdef_class>
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

static octave_value
to_ov (const std::list<cdef_class>& class_list)
{
  Cell cls (class_list.size (), 1);
  int i = 0;

  for (const auto& cdef_cls : class_list)
    cls(i++) = to_ov (cdef_cls);

  return octave_value (cls);
}

static bool
is_superclass (const cdef_class& clsa, const cdef_class& clsb,
               bool allow_equal = true, int max_depth = -1)
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

inline bool
is_strict_superclass (const cdef_class& clsa, const cdef_class& clsb)
{ return is_superclass (clsa, clsb, false); }

inline bool
is_direct_superclass (const cdef_class& clsa, const cdef_class& clsb)
{ return is_superclass (clsa, clsb, false, 1); }

static octave_value_list
class_get_properties (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () == 1 && args(0).type_name () == "object")
    {
      cdef_class cls (to_cdef (args(0)));

      retval(0) = cls.get_properties ();
    }

  return retval;
}

static cdef_class
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

inline cdef_class
get_class_context (void)
{
  std::string dummy_string;
  bool dummy_bool;

  return get_class_context (dummy_string, dummy_bool);
}

static bool
in_class_method (const cdef_class& cls)
{
  cdef_class ctx = get_class_context ();

  return (ctx.ok () && is_superclass (ctx, cls));
}

static bool
check_access (const cdef_class& cls, const octave_value& acc,
              const std::string& meth_name = "",
              const std::string& prop_name = "",
              bool is_prop_set = false)
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
                  // Calling a protected method or property in a derived class.
                  // This is only allowed if the context class knows about it
                  // and has access to it.

                  if (! meth_name.empty ())
                    {
                      cdef_method m = ctx.find_method (meth_name);

                      if (m.ok ())
                        return check_access (ctx, m.get ("Access"), meth_name);

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
    error ("invalid property/method access in class `%s'",
           cls.get_name ().c_str ());

  return false;
}

static bool
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

static octave_value_list
class_get_methods (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () == 1 && args(0).type_name () == "object")
    {
      cdef_class cls (to_cdef (args(0)));

      retval(0) = cls.get_methods ();
    }

  return retval;
}

static octave_value_list
class_get_superclasses (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () == 1 && args(0).type_name () == "object"
      && args(0).class_name () == "meta.class")
    {
      cdef_class cls (to_cdef (args(0)));

      Cell classes = cls.get ("SuperClasses").cell_value ();

      retval(0) = to_ov (lookup_classes (classes));
    }

  return retval;
}

static octave_value_list
class_get_inferiorclasses (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () == 1 && args(0).type_name () == "object"
      && args(0).class_name () == "meta.class")
    {
      cdef_class cls (to_cdef (args(0)));

      Cell classes = cls.get ("InferiorClasses").cell_value ();

      retval(0) = to_ov (lookup_classes (classes));
    }

  return retval;
}

static octave_value_list
class_fromName (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () != 1)
    error ("fromName: invalid number of parameters");

  std::string name = args(0).xstring_value ("fromName: CLASS_NAME must be a string");

  retval(0) = to_ov (lookup_class (name, false));

  return retval;
}

static octave_value_list
class_fevalStatic (const octave_value_list& args, int nargout)
{
  if (args.length () <= 1 || args(0).type_name () != "object")
    error ("fevalStatic: first argument must be a meta.class object");

  cdef_class cls (to_cdef (args(0)));

  std::string meth_name = args(1).xstring_value ("fevalStatic: method name must be a string");

  cdef_method meth = cls.find_method (meth_name);

  if (! meth.ok ())
    error ("fevalStatic: method not found: %s", meth_name.c_str ());

  if (! meth.is_static ())
    error ("fevalStatic: method `%s' is not static", meth_name.c_str ());

  return meth.execute (args.splice (0, 2), nargout, true, "fevalStatic");
}

static octave_value_list
class_getConstant (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () != 2 || args(0).type_name () != "object"
      || args(0).class_name () != "meta.class")
    error ("getConstant: first argument must be a meta.class object");

  cdef_class cls = to_cdef (args(0));

  std::string prop_name = args(1).xstring_value ("getConstant: property name must be a string");

  cdef_property prop = cls.find_property (prop_name);

  if (! prop.ok ())
    error ("getConstant: property not found: %s",
           prop_name.c_str ());

  if (! prop.is_constant ())
    error ("getConstant: property `%s' is not constant",
           prop_name.c_str ());

  retval(0) = prop.get_value (true, "getConstant");

  return retval;
}

#define META_CLASS_CMP(OP, CLSA, CLSB, FUN)                             \
  static octave_value_list                                              \
  class_ ## OP (const octave_value_list& args, int /* nargout */)       \
  {                                                                     \
    octave_value_list retval;                                           \
                                                                        \
    if (args.length () != 2                                             \
        || args(0).type_name () != "object"                             \
        || args(1).type_name () != "object"                             \
        || args(0).class_name () != "meta.class"                        \
        || args(1).class_name () != "meta.class")                       \
      error (#OP ": invalid arguments");                                \
                                                                        \
    cdef_class clsa = to_cdef (args(0));                                \
                                                                        \
    cdef_class clsb = to_cdef (args(1));                                \
                                                                        \
    retval(0) = FUN (CLSA, CLSB);                                       \
                                                                        \
    return retval;                                                      \
  }

META_CLASS_CMP (lt, clsb, clsa, is_strict_superclass)
META_CLASS_CMP (le, clsb, clsa, is_superclass)
META_CLASS_CMP (gt, clsa, clsb, is_strict_superclass)
META_CLASS_CMP (ge, clsa, clsb, is_superclass)
META_CLASS_CMP (eq, clsa, clsb, operator==)
META_CLASS_CMP (ne, clsa, clsb, operator!=)

octave_value_list
property_get_defaultvalue (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () == 1 && args(0).type_name () == "object")
    {
      cdef_property prop (to_cdef (args(0)));

      retval(0) = prop.get ("DefaultValue");

      if (! retval(0).is_defined ())
        error_with_id ("Octave:class:NotDefaultDefined",
                       "no default value for property `%s'",
                       prop.get_name ().c_str ());
    }

  return retval;
}

static octave_value_list
handle_delete (const octave_value_list& /* args */, int /* nargout */)
{
  octave_value_list retval;

  // FIXME: implement this.  Wait, what is this supposed to do?

  return retval;
}

cdef_class
cdef_manager::make_class (const std::string& name,
                          const std::list<cdef_class>& super_list)
{
  cdef_class cls (name, super_list);

  cls.set_class (meta_class ());

  cls.put ("Abstract", false);
  cls.put ("ConstructOnLoad", false);
  cls.put ("ContainingPackage", Matrix ());
  cls.put ("Description", "");
  cls.put ("DetailedDescription", "");
  cls.put ("Events", Cell ());
  cls.put ("Hidden", false);
  cls.put ("InferiorClasses", Cell ());
  cls.put ("Methods", Cell ());
  cls.put ("Properties", Cell ());
  cls.put ("Sealed", false);

  if (name == "handle")
    {
      cls.put ("HandleCompatible", true);
      cls.mark_as_handle_class ();
    }
  else if (super_list.empty ())
    {
      cls.put ("HandleCompatible", false);
    }
  else
    {
      bool all_handle_compatible = true;
      bool has_handle_class = false;

      for (const auto& cl : super_list)
        {
          all_handle_compatible = all_handle_compatible
                                  && cl.get ("HandleCompatible").bool_value ();
          has_handle_class = has_handle_class || cl.is_handle_class ();
        }

      if (has_handle_class && ! all_handle_compatible)
        error ("%s: cannot mix handle and non-HandleCompatible classes",
               name.c_str ());

      cls.put ("HandleCompatible", all_handle_compatible);
      if (has_handle_class)
        cls.mark_as_handle_class ();
    }

  if (! name.empty ())
    register_class (cls);

  return cls;
}

cdef_class
cdef_manager::make_class (const std::string& name,
                          const cdef_class& super)
{
  return make_class (name, std::list<cdef_class> (1, super));
}

cdef_class
cdef_manager::make_meta_class (const std::string& name,
                               const cdef_class& super)
{
  cdef_class cls = make_class (name, super);

  cls.put ("Sealed", true);
  cls.mark_as_meta_class ();

  return cls;
}

cdef_property
cdef_manager::make_property (const cdef_class& cls, const std::string& name,
                             const octave_value& get_method,
                             const std::string& get_access,
                             const octave_value& set_method,
                             const std::string& set_access)
{
  cdef_property prop (name);

  prop.set_class (meta_property ());

  prop.put ("Description", "");
  prop.put ("DetailedDescription", "");
  prop.put ("Abstract", false);
  prop.put ("Constant", false);
  prop.put ("GetAccess", get_access);
  prop.put ("SetAccess", set_access);
  prop.put ("Dependent", false);
  prop.put ("Transient", false);
  prop.put ("Hidden", false);
  prop.put ("GetObservable", false);
  prop.put ("SetObservable", false);
  prop.put ("GetMethod", get_method);
  prop.put ("SetMethod", set_method);
  prop.put ("DefiningClass", to_ov (cls));
  prop.put ("DefaultValue", octave_value ());
  prop.put ("HasDefault", false);

  std::string class_name = cls.get_name ();

  if (! get_method.isempty ())
    make_function_of_class (class_name, get_method);
  if (! set_method.isempty ())
    make_function_of_class (class_name, set_method);

  return prop;
}

cdef_property
cdef_manager::make_attribute (const cdef_class& cls, const std::string& name)
{
  return make_property (cls, name, Matrix (), "public", Matrix (), "private");
}

cdef_method
cdef_manager::make_method (const cdef_class& cls, const std::string& name,
                           const octave_value& fcn,
                           const std::string& m_access, bool is_static)
{
  cdef_method meth (name);

  meth.set_class (meta_method ());

  meth.put ("Abstract", false);
  meth.put ("Access", m_access);
  meth.put ("DefiningClass", to_ov (cls));
  meth.put ("Description", "");
  meth.put ("DetailedDescription", "");
  meth.put ("Hidden", false);
  meth.put ("Sealed", true);
  meth.put ("Static", is_static);

  if (fcn.is_defined ())
    make_function_of_class (cls, fcn);

  meth.set_function (fcn);

  if (is_dummy_method (fcn))
    meth.mark_as_external (cls.get_name ());

  return meth;
}

cdef_method
cdef_manager::make_method (const cdef_class& cls, const std::string& name,
                           octave_builtin::fcn ff,
                           const std::string& m_access, bool is_static)
{
  octave_value fcn (new octave_builtin (ff, name));

  return make_method (cls, name, fcn, m_access, is_static);
}

cdef_method
cdef_manager::make_method (const cdef_class& cls, const std::string& name,
                           octave_builtin::meth mm,
                           const std::string& m_access, bool is_static)
{
  octave_value fcn (new octave_builtin (mm, name));

  return make_method (cls, name, fcn, m_access, is_static);
}

cdef_package
cdef_manager::make_package (const std::string& nm, const std::string& parent)
{
  cdef_package pack (nm);

  pack.set_class (meta_package ());

  if (parent.empty ())
    pack.put ("ContainingPackage", Matrix ());
  else
    pack.put ("ContainingPackage", to_ov (find_package (parent)));

  if (! nm.empty ())
    register_package (pack);

  return pack;
}

//----------------------------------------------------------------------------

int octave_classdef::t_id (-1);

const std::string octave_classdef::t_name ("object");

void
octave_classdef::register_type (octave::type_info& ti)
{
  t_id = ti.register_type (octave_classdef::t_name, "<unknown>",
                           octave_value (new octave_classdef ()));
}

octave_value_list
octave_classdef::subsref (const std::string& type,
                          const std::list<octave_value_list>& idx,
                          int nargout)
{
  size_t skip = 0;
  octave_value_list retval;

  cdef_class cls = object.get_class ();

  if (! in_class_method (cls) && ! called_from_builtin ())
    {
      cdef_method meth = cls.find_method ("subsref");

      if (meth.ok ())
        {
          octave_value_list args;

          args(1) = make_idx_args (type, idx, "subsref");

          count++;
          args(0) = octave_value (this);

          retval = meth.execute (args, nargout, true, "subsref");

          return retval;
        }
    }

  // At this point, the default subsref mechanism must be used.

  retval = object.subsref (type, idx, nargout, skip, cdef_class ());

  if (type.length () > skip && idx.size () > skip)
    retval = retval(0).next_subsref (nargout, type, idx, skip);

  return retval;
}

octave_value
octave_classdef::subsref (const std::string& type,
                          const std::list<octave_value_list>& idx,
                          bool auto_add)
{
  size_t skip = 0;
  octave_value_list retval;

  // This variant of subsref is used to create temporary values when doing
  // assignment with multi-level indexing.  AFAIK this is only used for internal
  // purpose (not sure we should even implement this) and any overload subsref
  // should not be called.

  retval = object.subsref (type, idx, 1, skip, cdef_class (), auto_add);

  if (type.length () > skip && idx.size () > skip)
    retval = retval(0).next_subsref (1, type, idx, skip);

  return retval.length () > 0 ? retval(0) : octave_value ();
}

octave_value
octave_classdef::subsasgn (const std::string& type,
                           const std::list<octave_value_list>& idx,
                           const octave_value& rhs)
{
  octave_value retval;

  cdef_class cls = object.get_class ();

  if (! in_class_method (cls) && ! called_from_builtin ())
    {
      cdef_method meth = cls.find_method ("subsasgn");

      if (meth.ok ())
        {
          octave_value_list args;

          args(1) = make_idx_args (type, idx, "subsasgn");

          count++;
          args(0) = octave_value (this);
          args(2) = rhs;

          octave_value_list retlist;

          retlist = meth.execute (args, 1, true, "subsasgn");

          if (retlist.empty ())
            error ("overloaded method `subsasgn' did not return any value");

          retval = retlist(0);
        }
    }

  if (! retval.is_defined ())
    retval = object.subsasgn (type, idx, rhs);

  return retval;
}

octave_value
octave_classdef::undef_subsasgn (const std::string& type,
                                 const std::list<octave_value_list>& idx,
                                 const octave_value& rhs)
{
  if (type.length () == 1 && type[0] == '(')
    {
      object = object.make_array ();

      return subsasgn (type, idx, rhs);
    }
  else
    return octave_base_value::undef_subsasgn (type, idx, rhs);

  return octave_value ();
}

octave_idx_type
octave_classdef::numel (const octave_value_list& idx)
{
  octave_idx_type retval = -1;

  cdef_class cls = object.get_class ();

  if (! in_class_method (cls) && ! called_from_builtin ())
    {
      cdef_method meth = cls.find_method ("numel");

      if (meth.ok ())
        {
          octave_value_list args (idx.length () + 1, octave_value ());

          count++;
          args(0) = octave_value (this);

          for (octave_idx_type i = 0; i < idx.length (); i++)
            args(i+1) = idx(i);

          octave_value_list lv = meth.execute (args, 1, true, "numel");
          if (lv.length () != 1 || ! lv(0).is_scalar_type ())
            error ("@%s/numel: invalid return value", cls.get_name ().c_str ());

          retval = lv(0).idx_type_value (true);

          return retval;
        }
    }

  retval = octave_base_value::numel (idx);

  return retval;
}

void
octave_classdef::print (std::ostream& os, bool)
{
  print_raw (os);
}

void
octave_classdef::print_raw (std::ostream& os, bool) const
{
  indent (os);
  os << "<object ";
  if (object.is_array ())
    os << "array ";
  os << class_name () << '>';
  newline (os);
}

bool
octave_classdef::print_name_tag (std::ostream& os,
                                 const std::string& name) const
{
  return octave_base_value::print_name_tag (os, name);
}

void
octave_classdef::print_with_name (std::ostream& os, const std::string& name,
                                  bool print_padding)
{
  octave_base_value::print_with_name (os, name, print_padding);
}

bool
octave_classdef::is_instance_of (const std::string& cls_name) const
{
  cdef_class cls = lookup_class (cls_name, false, false);

  if (cls.ok ())
    return is_superclass (cls, object.get_class ());

  return false;
}

//----------------------------------------------------------------------------

class octave_classdef_meta : public octave_function
{
public:
  octave_classdef_meta (const cdef_meta_object& obj)
    : object (obj) { }

  ~octave_classdef_meta (void)
  { object.meta_release (); }

  bool is_classdef_meta (void) const { return true; }

  bool is_package (void) const { return object.is_package(); }

  octave_function * function_value (bool = false) { return this; }

  octave_value_list
  subsref (const std::string& type,
           const std::list<octave_value_list>& idx,
           int nargout)
  {
    return object.meta_subsref (type, idx, nargout);
  }

  octave_value_list call (octave::tree_evaluator&, int nargout,
                          const octave_value_list& args)
  {
    // Emulate ()-type meta subsref

    std::list<octave_value_list> idx (1, args);
    std::string type ("(");

    return subsref (type, idx, nargout);
  }

  bool accepts_postfix_index (char type) const
  { return object.meta_accepts_postfix_index (type); }

  bool
  is_classdef_constructor (const std::string& cname = "") const
  {
    bool retval = false;

    if (object.is_class ())
      {
        if (cname.empty ())
          retval = true;
        else
          {
            cdef_class cls (object);

            if (cls.get_name () == cname)
              retval = true;
          }
      }

    return retval;
  }

private:
  cdef_meta_object object;
};

//----------------------------------------------------------------------------

class octave_classdef_superclass_ref : public octave_function
{
public:
  octave_classdef_superclass_ref (const octave_value_list& a)
    : octave_function (), args (a) { }

  ~octave_classdef_superclass_ref (void) = default;

  bool is_classdef_superclass_ref (void) const { return true; }

  octave_function * function_value (bool = false) { return this; }

  octave_value_list
  call (octave::tree_evaluator&, int nargout, const octave_value_list& idx)
  {
    octave_value_list retval;

    std::string meth_name;
    bool in_constructor;
    cdef_class ctx;

    ctx = get_class_context (meth_name, in_constructor);

    if (! ctx.ok ())
      error ("superclass calls can only occur in methods or constructors");

    std::string mname = args(0).string_value ();
    std::string cname = args(1).string_value ();

    cdef_class cls = lookup_class (cname);

    if (in_constructor)
      {
        if (! is_direct_superclass (cls, ctx))
          error ("`%s' is not a direct superclass of `%s'",
                 cname.c_str (), ctx.get_name ().c_str ());

        if (! is_constructed_object (mname))
          error ("cannot call superclass constructor with variable `%s'",
                 mname.c_str ());

        octave::symbol_scope scope
          = octave::__require_current_scope__ ("octave_classdef_superclass_ref::call");

        octave_value sym = scope.varval (mname);

        cls.run_constructor (to_cdef_ref (sym), idx);

        retval(0) = sym;
      }
    else
      {
        if (mname != meth_name)
          error ("method name mismatch (`%s' != `%s')",
                 mname.c_str (), meth_name.c_str ());

        if (! is_strict_superclass (cls, ctx))
          error ("`%s' is not a superclass of `%s'",
                 cname.c_str (), ctx.get_name ().c_str ());

        // I see 2 possible implementations here:
        // 1) use cdef_object::subsref with a different class
        //    context; this avoids duplicating code, but
        //    assumes the object is always the first argument
        // 2) lookup the method manually and call
        //    cdef_method::execute; this duplicates part of
        //    logic in cdef_object::subsref, but avoid the
        //    assumption of 1)
        // Not being sure about the assumption of 1), I
        // go with option 2) for the time being.

        cdef_method meth = cls.find_method (meth_name, false);

        if (! meth.ok ())
          error ("no method `%s' found in superclass `%s'",
                 meth_name.c_str (), cname.c_str ());

        retval = meth.execute (idx, nargout, true,
                               meth_name);
      }

    return retval;
  }

private:
  bool is_constructed_object (const std::string nm)
  {
    octave::call_stack& cs
      = octave::__get_call_stack__ ("octave_classdef_superclass_ref::is_constructed_object");

    octave_function *of = cs.current ();

    if (of->is_classdef_constructor ())
      {
        octave_user_function *uf = of->user_function_value (true);

        if (uf)
          {
            octave::tree_parameter_list *ret_list = uf->return_list ();

            if (ret_list && ret_list->length () == 1)
              return (ret_list->front ()->name () == nm);
          }
      }

    return false;
  }

private:
  octave_value_list args;
};

//----------------------------------------------------------------------------

void
cdef_object_rep::release (const cdef_object& obj)
{
  // We need to be careful to keep a reference to the object if we are
  // calling the delete method.  The object is passed to the delete
  // method as an argument and if the count is already zero when we
  // do that, then we will increment the count while creating the
  // argument list for the delete method and then it will be decremented
  // back to zero and we'll find ourselves in an infinite loop.

  if (refcount - 1 > static_count ())
    {
      --refcount;
      return;
    }

  if (is_handle_object () && ! is_meta_object ())
    {
      octave::unwind_protect frame;

      // Clear interrupts.
      frame.protect_var (octave_interrupt_state);
      octave_interrupt_state = 0;

      // Disallow quit().
      frame.protect_var (quit_allowed);
      quit_allowed = false;

      interpreter_try (frame);

      try
        {
          // Call classdef "delete()" method on object
          get_class ().delete_object (obj);
        }
      catch (const octave::interrupt_exception&)
        {
          octave::interpreter::recover_from_exception ();

          warning ("interrupt occurred in handle class delete method");
        }
      catch (const octave::execution_exception&)
        {
          std::string msg = last_error_message ();
          warning ("error caught while executing handle class delete method:\n%s\n",
                   msg.c_str ());

        }
      catch (const octave::exit_exception&)
        {
          // This shouldn't happen since we disabled quit above.
          warning ("exit disabled while executing handle class delete method");
        }
      catch (...) // Yes, the black hole.  We're in a d-tor.
        {
          // This shouldn't happen, in theory.
          warning ("internal error: unhandled exception in handle class delete method");
        }
    }

  // Now it is safe to set the count to zero.
  refcount--;

  destroy ();
}

octave_map
cdef_object::map_value (void) const
{
  octave_map retval;

  warning_with_id ("Octave:classdef-to-struct",
                   "struct: converting a classdef object into a struct "
                   "overrides the access restrictions defined for properties. "
                   "All properties are returned, including private and "
                   "protected ones.");

  cdef_class cls = get_class ();

  if (cls.ok ())
    {
      std::map<std::string, cdef_property> props;

      props = cls.get_property_map (cdef_class::property_all);

      // FIXME: Why not const here?
      for (auto& prop_val : props)
        {
          if (is_array ())
            {
              Array<cdef_object> a_obj = array_value ();

              Cell cvalue (a_obj.dims ());

              for (octave_idx_type i = 0; i < a_obj.numel (); i++)
                cvalue (i) = prop_val.second.get_value (a_obj(i), false);

              retval.setfield (prop_val.first, cvalue);
            }
          else
            {
              Cell cvalue (dim_vector (1, 1),
                           prop_val.second.get_value (*this, false));

              retval.setfield (prop_val.first, cvalue);
            }
        }
    }

  return retval;
}

string_vector
cdef_object_rep::map_keys (void) const
{
  cdef_class cls = get_class ();

  if (cls.ok ())
    return cls.get_names ();

  return string_vector ();
}

octave_value_list
cdef_object_scalar::subsref (const std::string& type,
                             const std::list<octave_value_list>& idx,
                             int nargout, size_t& skip,
                             const cdef_class& context, bool auto_add)
{
  skip = 0;

  cdef_class cls = (context.ok () ? context : get_class ());

  octave_value_list retval;

  if (! cls.ok ())
    return retval;

  switch (type[0])
    {
    case '.':
      {
        std::string name = (idx.front ())(0).string_value ();

        cdef_method meth = cls.find_method (name);

        if (meth.ok ())
          {
            int _nargout = (type.length () > 2 ? 1 : nargout);

            octave_value_list args;

            skip = 1;

            if (type.length () > 1 && type[1] == '(')
              {
                std::list<octave_value_list>::const_iterator it = idx.begin ();

                args = *++it;

                skip++;
              }

            if (meth.is_static ())
              retval = meth.execute (args, _nargout, true, "subsref");
            else
              {
                refcount++;
                retval = meth.execute (cdef_object (this), args, _nargout,
                                       true, "subsref");
              }
          }

        if (skip == 0)
          {
            cdef_property prop = cls.find_property (name);

            if (! prop.ok ())
              error ("subsref: unknown method or property: %s", name.c_str ());

            if (prop.is_constant ())
              retval(0) = prop.get_value (true, "subsref");
            else
              {
                refcount++;
                retval(0) = prop.get_value (cdef_object (this),
                                            true, "subsref");
              }

            skip = 1;
          }
        break;
      }

    case '(':
      {
        const octave_value_list& ival = idx.front ();

        refcount++;
        cdef_object this_obj (this);

        if (ival.empty ())
          {
            skip++;
            retval(0) = to_ov (this_obj);
          }
        else
          {
            Array<cdef_object> arr (dim_vector (1, 1), this_obj);

            cdef_object new_obj = cdef_object (new cdef_object_array (arr));

            new_obj.set_class (get_class ());

            retval = new_obj.subsref (type, idx, nargout, skip, cls, auto_add);
          }
      }
      break;

    default:
      error ("object cannot be indexed with `%c'", type[0]);
      break;
    }

  return retval;
}

octave_value
cdef_object_scalar::subsasgn (const std::string& type,
                              const std::list<octave_value_list>& idx,
                              const octave_value& rhs)
{
  octave_value retval;

  cdef_class cls = get_class ();

  switch (type[0])
    {
    case '.':
      {
        std::string name = (idx.front ())(0).string_value ();

        cdef_property prop = cls.find_property (name);

        if (! prop.ok ())
          error ("subsasgn: unknown property: %s", name.c_str ());

        if (prop.is_constant ())
          error ("subsasgn: cannot assign constant property: %s",
                 name.c_str ());

        refcount++;

        cdef_object obj (this);

        if (type.length () == 1)
          {
            prop.set_value (obj, rhs, true, "subsasgn");

            retval = to_ov (obj);
          }
        else
          {
            octave_value val =
              prop.get_value (obj, true, "subsasgn");

            std::list<octave_value_list> args (idx);

            args.erase (args.begin ());

            val = val.assign (octave_value::op_asn_eq,
                              type.substr (1), args, rhs);

            if (val.class_name () != "object"
                || ! to_cdef (val).is_handle_object ())
              prop.set_value (obj, val, true, "subsasgn");

            retval = to_ov (obj);
          }
      }
      break;

    case '(':
      {
        refcount++;

        cdef_object this_obj (this);

        Array<cdef_object> arr (dim_vector (1, 1), this_obj);

        cdef_object new_obj = cdef_object (new cdef_object_array (arr));

        new_obj.set_class (get_class ());

        octave_value tmp = new_obj.subsasgn (type, idx, rhs);

        retval = tmp;
      }
      break;

    default:
      error ("subsasgn: object cannot be index with `%c'", type[0]);
      break;
    }

  return retval;
}

void
cdef_object_scalar::mark_for_construction (const cdef_class& cls)
{
  std::string cls_name = cls.get_name ();

  Cell supcls = cls.get ("SuperClasses").cell_value ();

  std::list<cdef_class> supcls_list = lookup_classes (supcls);

  ctor_list[cls] = supcls_list;
}

octave_value_list
cdef_object_array::subsref (const std::string& type,
                            const std::list<octave_value_list>& idx,
                            int /* nargout */, size_t& skip,
                            const cdef_class& /* context */, bool auto_add)
{
  octave_value_list retval;

  skip = 1;

  switch (type[0])
    {
    case '(':
      {
        const octave_value_list& ival = idx.front ();

        if (ival.empty ())
          {
            refcount++;
            retval(0) = to_ov (cdef_object (this));
            break;
          }

        bool is_scalar = true;
        Array<idx_vector> iv (dim_vector (1, ival.length ()));

        for (int i = 0; i < ival.length (); i++)
          {
            try
              {
                iv(i) = ival(i).index_vector ();
              }
            catch (octave::index_exception& e)
              {
                // Rethrow to allow more info to be reported later.
                e.set_pos_if_unset (ival.length (), i+1);
                throw;
              }

            is_scalar = is_scalar && iv(i).is_scalar ();
          }

        Array<cdef_object> ires = array.index (iv, auto_add);

        // If resizing is enabled (auto_add = true), it's possible
        // indexing was out-of-bound and the result array contains
        // invalid cdef_objects.

        if (auto_add)
          fill_empty_values (ires);

        if (is_scalar)
          retval(0) = to_ov (ires(0));
        else
          {
            cdef_object array_obj (new cdef_object_array (ires));

            array_obj.set_class (get_class ());

            retval(0) = to_ov (array_obj);
          }
      }
      break;

    case '.':
      if (type.size () == 1 && idx.size () == 1)
        {
          Cell c (dims ());

          octave_idx_type n = array.numel ();

          // dummy variables
          size_t dummy_skip;
          cdef_class dummy_cls;

          for (octave_idx_type i = 0; i < n; i++)
            {
              octave_value_list r = array(i).subsref (type, idx, 1, dummy_skip,
                                                      dummy_cls);

              if (r.length () > 0)
                c(i) = r(0);
            }

          retval(0) = octave_value (c, true);

          break;
        }
      OCTAVE_FALLTHROUGH;

    default:
      error ("can't perform indexing operation on array of %s objects",
             class_name ().c_str ());
      break;
    }

  return retval;
}

octave_value
cdef_object_array::subsasgn (const std::string& type,
                             const std::list<octave_value_list>& idx,
                             const octave_value& rhs)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      if (type.length () == 1)
        {
          cdef_object rhs_obj = to_cdef (rhs);

          if (rhs_obj.get_class () != get_class ())
            error ("can't assign %s object into array of %s objects.",
                   rhs_obj.class_name ().c_str (),
                   class_name ().c_str ());

          const octave_value_list& ival = idx.front ();
          bool is_scalar = true;
          Array<idx_vector> iv (dim_vector (1, ival.length ()));

          for (int i = 0; i < ival.length (); i++)
            {
              try
                {
                  iv(i) = ival(i).index_vector ();
                }
              catch (octave::index_exception& e)
                {
                  e.set_pos_if_unset (ival.length (), i+1);
                  throw;   // var name set in pt-idx.cc / pt-assign.cc
                }

              is_scalar = is_scalar && iv(i).is_scalar ();
            }

          Array<cdef_object> rhs_mat;

          if (! rhs_obj.is_array ())
            {
              rhs_mat = Array<cdef_object> (dim_vector (1, 1));
              rhs_mat(0) = rhs_obj;
            }
          else
            rhs_mat = rhs_obj.array_value ();

          octave_idx_type n = array.numel ();

          array.assign (iv, rhs_mat, cdef_object ());

          if (array.numel () > n)
            fill_empty_values ();

          refcount++;
          retval = to_ov (cdef_object (this));
        }
      else
        {
          const octave_value_list& ivl = idx.front ();

          // Fill in trailing singleton dimensions so that
          // array.index doesn't create a new blank entry (bug #46660).
          const octave_idx_type one = static_cast<octave_idx_type> (1);
          const octave_value_list& ival = ivl.length () >= 2
                                            ? ivl : ((array.dims ()(0) == 1)
                                                      ? ovl (one, ivl(0))
                                                      : ovl (ivl(0), one));

          bool is_scalar = true;

          Array<idx_vector> iv (dim_vector (1, ival.length ()));

          for (int i = 0; i < ival.length (); i++)
            {
              try
                {
                  iv(i) = ival(i).index_vector ();
                }
              catch (octave::index_exception& e)
                {
                  // Rethrow to allow more info to be reported later.
                  e.set_pos_if_unset (ival.length (), i+1);
                  throw;
                }

              is_scalar = is_scalar && iv(i).is_scalar ();

              if (! is_scalar)
                error ("subsasgn: invalid indexing for object array assignment"
                       ", the index must reference a single object in the "
                       "array.");
            }

          Array<cdef_object> a = array.index (iv, true);

          if (a.numel () != 1)
            error ("subsasgn: invalid indexing for object array assignment");

          cdef_object obj = a(0);

          int ignore_copies = 0;

          // If the object in 'a' is not valid, this means the index
          // was out-of-bound and we need to create a new object.

          if (! obj.ok ())
            obj = get_class ().construct_object (octave_value_list ());
          else
            // Optimize the subsasgn call to come.  There are 2 copies
            // that we can safely ignore:
            // - 1 in "array"
            // - 1 in "a"
            ignore_copies = 2;

          std::list<octave_value_list> next_idx (idx);

          next_idx.erase (next_idx.begin ());

          octave_value tmp = obj.subsasgn (type.substr (1), next_idx,
                                           rhs, ignore_copies);

          cdef_object robj = to_cdef (tmp);

          if (! robj.ok ()
              || robj.is_array ()
              || robj.get_class () != get_class ())
            error ("subasgn: invalid assignment into array of %s objects",
                   class_name ().c_str ());

          // Small optimization, when dealing with handle
          // objects, we don't need to re-assign the result
          // of subsasgn back into the array.

          if (! robj.is (a(0)))
            {
              Array<cdef_object> rhs_a (dim_vector (1, 1),
                                        robj);

              octave_idx_type n = array.numel ();

              array.assign (iv, rhs_a);

              if (array.numel () > n)
                fill_empty_values ();
            }

          refcount++;

          retval = to_ov (cdef_object (this));
        }
      break;

    default:
      error ("can't perform indexing operation on array of %s objects",
             class_name ().c_str ());
      break;
    }

  return retval;
}

void
cdef_object_array::fill_empty_values (Array<cdef_object>& arr)
{
  cdef_class cls = get_class ();

  cdef_object obj;

  int n = arr.numel ();

  for (int i = 0; i < n; i++)
    {
      if (! arr.xelem (i).ok ())
        {
          if (! obj.ok ())
            {
              obj = cls.construct_object (octave_value_list ());

              arr.xelem (i) = obj;
            }
          else
            arr.xelem (i) = obj.copy ();
        }
    }
}

bool
cdef_object_scalar::is_constructed_for (const cdef_class& cls) const
{
  return (is_constructed ()
          || ctor_list.find (cls) == ctor_list.end ());
}

bool
cdef_object_scalar::is_partially_constructed_for (const cdef_class& cls) const
{
  std::map< cdef_class, std::list<cdef_class>>::const_iterator it;

  if (is_constructed ())
    return true;
  else if ((it = ctor_list.find (cls)) == ctor_list.end ()
           || it->second.empty ())
    return true;

  for (const auto& cdef_cls : it->second)
    if (! is_constructed_for (cdef_cls))
      return false;

  return true;
}

inline void
cdef_object_scalar::mark_as_constructed (const cdef_class& cls)
{
  ctor_list.erase (cls);
}

handle_cdef_object::~handle_cdef_object (void)
{
#if DEBUG_TRACE
  std::cerr << "deleting " << get_class ().get_name ()
            << " object (handle)" << std::endl;
#endif
}

value_cdef_object::~value_cdef_object (void)
{
#if DEBUG_TRACE
  std::cerr << "deleting " << get_class ().get_name ()
            << " object (value)" << std::endl;
#endif
}

cdef_class::cdef_class_rep::cdef_class_rep (const std::list<cdef_class>& superclasses)
  : cdef_meta_object_rep (), member_count (0), handle_class (false),
    object_count (0), meta (false)
{
  put ("SuperClasses", to_ov (superclasses));
  implicit_ctor_list = superclasses;
}

cdef_method
cdef_class::cdef_class_rep::find_method (const std::string& nm, bool local)
{
  method_iterator it = method_map.find (nm);

  if (it == method_map.end ())
    {
      // FIXME: look into class directory
    }
  else
    {
      cdef_method& meth = it->second;

      // FIXME: check if method reload needed

      if (meth.ok ())
        return meth;
    }

  if (! local)
    {
      // Look into superclasses

      Cell super_classes = get ("SuperClasses").cell_value ();

      for (int i = 0; i < super_classes.numel (); i++)
        {
          cdef_class cls = lookup_class (super_classes(i));

          cdef_method meth = cls.find_method (nm);

          if (meth.ok ())
            return meth;
        }
    }

  return cdef_method ();
}

class ctor_analyzer : public octave::tree_walker
{
public:
  ctor_analyzer (const std::string& ctor, const std::string& obj)
    : octave::tree_walker (), who (ctor), obj_name (obj) { }

  void visit_statement_list (octave::tree_statement_list& t)
  {
    for (const auto& stmt_p : t)
      stmt_p->accept (*this);
  }

  void visit_statement (octave::tree_statement& t)
  {
    if (t.is_expression ())
      t.expression ()->accept (*this);
  }

  void visit_simple_assignment (octave::tree_simple_assignment& t)
  {
    t.right_hand_side ()->accept (*this);
  }

  void visit_multi_assignment (octave::tree_multi_assignment& t)
  {
    t.right_hand_side ()->accept (*this);
  }

  void visit_index_expression (octave::tree_index_expression& t)
  {
    t.expression ()->accept (*this);
  }

  void visit_funcall (octave::tree_funcall& t)
  {
    octave_value fcn = t.function ();

    if (fcn.is_function ())
      {
        octave_function *of = fcn.function_value (true);

        if (of)
          {
            if (of->name () == "__superclass_reference__")
              {
                octave_value_list args = t.arguments ();

                if (args(0).string_value () == obj_name)
                  {
                    std::string class_name = args(1).string_value ();

                    cdef_class cls = lookup_class (class_name, false);

                    if (cls.ok ())
                      ctor_list.push_back (cls);
                  }
              }
          }
      }
  }

  std::list<cdef_class> get_constructor_list (void) const
  { return ctor_list; }

  // NO-OP
  void visit_anon_fcn_handle (octave::tree_anon_fcn_handle&) { }
  void visit_argument_list (octave::tree_argument_list&) { }
  void visit_binary_expression (octave::tree_binary_expression&) { }
  void visit_break_command (octave::tree_break_command&) { }
  void visit_colon_expression (octave::tree_colon_expression&) { }
  void visit_continue_command (octave::tree_continue_command&) { }
  void visit_decl_command (octave::tree_decl_command&) { }
  void visit_decl_init_list (octave::tree_decl_init_list&) { }
  void visit_decl_elt (octave::tree_decl_elt&) { }
  void visit_simple_for_command (octave::tree_simple_for_command&) { }
  void visit_complex_for_command (octave::tree_complex_for_command&) { }
  void visit_octave_user_script (octave_user_script&) { }
  void visit_octave_user_function (octave_user_function&) { }
  void visit_function_def (octave::tree_function_def&) { }
  void visit_identifier (octave::tree_identifier&) { }
  void visit_if_clause (octave::tree_if_clause&) { }
  void visit_if_command (octave::tree_if_command&) { }
  void visit_if_command_list (octave::tree_if_command_list&) { }
  void visit_switch_case (octave::tree_switch_case&) { }
  void visit_switch_case_list (octave::tree_switch_case_list&) { }
  void visit_switch_command (octave::tree_switch_command&) { }
  void visit_matrix (octave::tree_matrix&) { }
  void visit_cell (octave::tree_cell&) { }
  void visit_no_op_command (octave::tree_no_op_command&) { }
  void visit_constant (octave::tree_constant&) { }
  void visit_fcn_handle (octave::tree_fcn_handle&) { }
  void visit_parameter_list (octave::tree_parameter_list&) { }
  void visit_postfix_expression (octave::tree_postfix_expression&) { }
  void visit_prefix_expression (octave::tree_prefix_expression&) { }
  void visit_return_command (octave::tree_return_command&) { }
  void visit_return_list (octave::tree_return_list&) { }
  void visit_try_catch_command (octave::tree_try_catch_command&) { }
  void visit_unwind_protect_command (octave::tree_unwind_protect_command&) { }
  void visit_while_command (octave::tree_while_command&) { }
  void visit_do_until_command (octave::tree_do_until_command&) { }

private:
  // The name of the constructor being analyzed.
  std::string who;

  // The name of the first output argument of the constructor.
  std::string obj_name;

  // The list of superclass constructors that are explicitly called.
  std::list<cdef_class> ctor_list;
};

void
cdef_class::cdef_class_rep::install_method (const cdef_method& meth)
{
  method_map[meth.get_name ()] = meth;

  member_count++;

  if (meth.is_constructor ())
    {
      // Analyze the constructor code to determine what superclass
      // constructors are called explicitly.

      octave_function *of = meth.get_function ().function_value (true);

      if (of)
        {
          octave_user_function *uf = of->user_function_value (true);

          if (uf)
            {
              octave::tree_parameter_list *ret_list = uf->return_list ();
              octave::tree_statement_list *body = uf->body ();

              if (! ret_list || ret_list->size () != 1)
                error ("%s: invalid constructor output arguments",
                       meth.get_name ().c_str ());

              std::string obj_name = ret_list->front ()->name ();
              ctor_analyzer a (meth.get_name (), obj_name);

              body->accept (a);

              std::list<cdef_class> explicit_ctor_list
                = a.get_constructor_list ();

              for (const auto& cdef_cls : explicit_ctor_list)
                {
#if DEBUG_TRACE
                  std::cerr << "explicit superclass constructor: "
                            << cdef_cls.get_name () << std::endl;
#endif

                  implicit_ctor_list.remove (cdef_cls);
                }
            }
        }
    }
}

void
cdef_class::cdef_class_rep::load_all_methods (void)
{
  // FIXME: re-scan class directory
}

Cell
cdef_class::cdef_class_rep::get_methods (void)
{
  std::map<std::string,cdef_method> meths;

  find_methods (meths, false);

  Cell c (meths.size (), 1);

  int idx = 0;

  for (const auto& nm_mthd : meths)
    c(idx++, 0) = to_ov (nm_mthd.second);

  return c;
}

void
cdef_class::cdef_class_rep::find_methods (std::map<std::string,
                                          cdef_method>& meths,
                                          bool only_inherited)
{
  load_all_methods ();

  method_const_iterator it;

  for (it = method_map.begin (); it != method_map.end (); ++it)
    {
      if (! it->second.is_constructor ())
        {
          std::string nm = it->second.get_name ();

          if (meths.find (nm) == meths.end ())
            {
              if (only_inherited)
                {
                  octave_value acc = it->second.get ("Access");

                  if (! acc.is_string ()
                      || acc.string_value () == "private")
                    continue;
                }

              meths[nm] = it->second;
            }
        }
    }

  // Look into superclasses

  Cell super_classes = get ("SuperClasses").cell_value ();

  for (int i = 0; i < super_classes.numel (); i++)
    {
      cdef_class cls = lookup_class (super_classes(i));

      cls.get_rep ()->find_methods (meths, true);
    }
}

cdef_property
cdef_class::cdef_class_rep::find_property (const std::string& nm)
{
  property_iterator it = property_map.find (nm);

  if (it != property_map.end ())
    {
      cdef_property& prop = it->second;

      if (prop.ok ())
        return prop;
    }

  // Look into superclasses

  Cell super_classes = get ("SuperClasses").cell_value ();

  for (int i = 0; i < super_classes.numel (); i++)
    {
      cdef_class cls = lookup_class (super_classes(i));

      cdef_property prop = cls.find_property (nm);

      if (prop.ok ())
        return prop;
    }

  return cdef_property ();
}

void
cdef_class::cdef_class_rep::install_property (const cdef_property& prop)
{
  property_map[prop.get_name ()] = prop;

  member_count++;
}

Cell
cdef_class::cdef_class_rep::get_properties (int mode)
{
  std::map<std::string,cdef_property> props;

  props = get_property_map (mode);

  Cell c (props.size (), 1);

  int idx = 0;

  for (const auto& pname_prop : props)
    c(idx++, 0) = to_ov (pname_prop.second);

  return c;
}

std::map<std::string, cdef_property>
cdef_class::cdef_class_rep::get_property_map (int mode)
{
  std::map<std::string,cdef_property> props;

  find_properties (props, mode);

  return props;
}

void
cdef_class::cdef_class_rep::find_properties (std::map<std::string,
                                             cdef_property>& props,
                                             int mode)
{
  property_const_iterator it;

  for (it = property_map.begin (); it != property_map.end (); ++it)
    {
      std::string nm = it->second.get_name ();

      if (props.find (nm) == props.end ())
        {
          if (mode == property_inherited)
            {
              octave_value acc = it->second.get ("GetAccess");

              if (! acc.is_string ()
                  || acc.string_value () == "private")
                continue;
            }

          props[nm] = it->second;
        }
    }

  // Look into superclasses

  Cell super_classes = get ("SuperClasses").cell_value ();

  for (int i = 0; i < super_classes.numel (); i++)
    {
      cdef_class cls = lookup_class (super_classes(i));

      cls.get_rep ()->find_properties (props,
                                       (mode == property_all
                                        ? property_all
                                        : property_inherited));
    }
}

void
cdef_class::cdef_class_rep::find_names (std::set<std::string>& names,
                                        bool all)
{
  load_all_methods ();

  for (const auto& cls_fnmap : method_map)
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

  for (const auto& pname_prop : property_map)
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

  Cell super_classes = get ("SuperClasses").cell_value ();

  for (int i = 0; i < super_classes.numel (); i++)
    {
      cdef_class cls = lookup_class (super_classes(i));

      cls.get_rep ()->find_names (names, all);
    }
}

string_vector
cdef_class::cdef_class_rep::get_names (void)
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

  if (dtor.ok ())
    dtor.execute (obj, octave_value_list (), 0, true, "destructor");

  // FIXME: should we destroy corresponding properties here?

  // Call "delete" in super classes

  Cell super_classes = get ("SuperClasses").cell_value ();

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
  size_t skip = 1;

  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      // Constructor call

#if DEBUG_TRACE
      std::cerr << "constructor" << std::endl;
#endif

      retval(0) = construct (idx.front ());
      break;

    case '.':
      {
        // Static method, constant (or property?)

#if DEBUG_TRACE
        std::cerr << "static method/property" << std::endl;
#endif

        if (idx.front ().length () != 1)
          error ("invalid meta.class indexing");

        std::string nm = idx.front ()(0).xstring_value ("invalid meta.class indexing, expected a method or property name");

        cdef_method meth = find_method (nm);

        if (meth.ok ())
          {
            if (! meth.is_static ())
              error ("method `%s' is not static", nm.c_str ());

            octave_value_list args;

            if (type.length () > 1 && idx.size () > 1
                && type[1] == '(')
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
              error ("no such method or property `%s'", nm.c_str ());

            if (! prop.is_constant ())
              error ("property `%s' is not constant", nm.c_str ());

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
cdef_class::cdef_class_rep::meta_release (void)
{
  cdef_manager& cdm
    = octave::__get_cdef_manager__ ("cdef_class::cdef_class_rep::meta_release");

  cdm.unregister_class (wrap ());
}

void
cdef_class::cdef_class_rep::initialize_object (cdef_object& obj)
{
  // Populate the object with default property values

  std::list<cdef_class> super_classes = lookup_classes (
                                          get ("SuperClasses").cell_value ());

  for (auto& cls : super_classes)
    cls.initialize_object (obj);

  for (const auto& pname_prop : property_map)
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

  refcount++;
  obj.mark_for_construction (cdef_class (this));
}

void
cdef_class::cdef_class_rep::run_constructor (cdef_object& obj,
                                             const octave_value_list& args)
{
  octave_value_list empty_args;

  for (const auto& cls : implicit_ctor_list)
    {
      cdef_class supcls = lookup_class (cls);

      supcls.run_constructor (obj, empty_args);
    }

  std::string cls_name = get_name ();
  std::string ctor_name = get_base_name (cls_name);

  cdef_method ctor = find_method (ctor_name);

  if (ctor.ok ())
    {
      octave_value_list ctor_args (args);
      octave_value_list ctor_retval;

      ctor_args.prepend (to_ov (obj));
      ctor_retval = ctor.execute (ctor_args, 1, true, "constructor");

      if (ctor_retval.length () != 1)
        error ("%s: invalid number of output arguments for classdef constructor",
               ctor_name.c_str ());

      obj = to_cdef (ctor_retval(0));
    }

  obj.mark_as_constructed (wrap ());
}

octave_value
cdef_class::cdef_class_rep::construct (const octave_value_list& args)
{
  cdef_object obj = construct_object (args);

  if (obj.ok ())
    return to_ov (obj);

  return octave_value ();
}

cdef_object
cdef_class::cdef_class_rep::construct_object (const octave_value_list& args)
{
  if (is_abstract ())
    error ("cannot instantiate object for abstract class `%s'",
           get_name ().c_str ());

  cdef_object obj;

  if (is_meta_class ())
    {
      // This code path is only used to create empty meta objects
      // as filler for the empty values within a meta object array.

      cdef_class this_cls = wrap ();

      static cdef_object empty_class;

      cdef_manager& cdm
        = octave::__get_cdef_manager__ ("cdef_class::cdef_class_rep::construct_object");

      if (this_cls == cdm.meta_class ())
        {
          if (! empty_class.ok ())
            empty_class = cdm.make_class ("", std::list<cdef_class> ());
          obj = empty_class;
        }
      else if (this_cls == cdm.meta_property ())
        {
          static cdef_property empty_property;

          if (! empty_class.ok ())
            empty_class = cdm.make_class ("", std::list<cdef_class> ());
          if (! empty_property.ok ())
            empty_property = cdm.make_property (empty_class, "");
          obj = empty_property;
        }
      else if (this_cls == cdm.meta_method ())
        {
          static cdef_method empty_method;

          if (! empty_class.ok ())
            empty_class = cdm.make_class ("", std::list<cdef_class> ());
          if (! empty_method.ok ())
            empty_method = cdm.make_method (empty_class, "", octave_value ());
          obj = empty_method;
        }
      else if (this_cls == cdm.meta_package ())
        {
          static cdef_package empty_package;

          if (! empty_package.ok ())
            empty_package = cdm.make_package ("");
          obj = empty_package;
        }
      else
        panic_impossible ();

      return obj;
    }
  else
    {
      if (is_handle_class ())
        obj = cdef_object (new handle_cdef_object ());
      else
        obj = cdef_object (new value_cdef_object ());
      obj.set_class (wrap ());

      initialize_object (obj);

      run_constructor (obj, args);

      return obj;
    }

  return cdef_object ();
}

static octave_value
compute_attribute_value (octave::tree_evaluator& tw,
                         octave::tree_classdef_attribute *t)
{
  octave::tree_expression *expr = t->expression ();

  if (expr)
    {
      if (expr->is_identifier ())
        {
          std::string s = expr->name ();

          if (s == "public")
            return std::string ("public");
          else if (s == "protected")
            return std::string ("protected");
          else if (s == "private")
            return std::string ("private");
        }

      return tw.evaluate (expr);
    }
  else
    return octave_value (true);
}

template <typename T>
static std::string
attribute_value_to_string (T *t, octave_value v)
{
  if (v.is_string ())
    return v.string_value ();
  else if (t->expression ())
    return t->expression ()->original_text ();
  else
    return "true";
}

cdef_class
cdef_class::make_meta_class (octave::interpreter& interp,
                             octave::tree_classdef *t, bool is_at_folder)
{
  cdef_class retval;
  std::string class_name, full_class_name;

  // Class creation

  class_name = full_class_name = t->ident ()->name ();
  if (! t->package_name ().empty ())
    full_class_name = t->package_name () + '.' + full_class_name;

#if DEBUG_TRACE
  std::cerr << "class: " << full_class_name << std::endl;
#endif

  std::list<cdef_class> slist;

  if (t->superclass_list ())
    {
      for (auto& scls : (*t->superclass_list ()))
        {
          std::string sclass_name = (scls)->class_name ();

#if DEBUG_TRACE
          std::cerr << "superclass: " << sclass_name << std::endl;
#endif

          cdef_class sclass = lookup_class (sclass_name);

          if (sclass.get ("Sealed").bool_value ())
            error ("`%s' cannot inherit from `%s', because it is sealed",
                   full_class_name.c_str (), sclass_name.c_str ());

          slist.push_back (sclass);
        }
    }

  cdef_manager& cdm
    = octave::__get_cdef_manager__ ("cdef_class::make_meta_class");

  retval = cdm.make_class (full_class_name, slist);

  // Package owning this class

  if (! t->package_name ().empty ())
    {
      cdef_package pack = cdm.find_package (t->package_name ());

      if (pack.ok ())
        retval.put ("ContainingPackage", to_ov (pack));
    }

  // Class attributes

  octave::tree_evaluator& tw = interp.get_evaluator ();

  if (t->attribute_list ())
    {
      for (const auto& attr : (*t->attribute_list ()))
        {
          std::string aname = attr->ident ()->name ();
          octave_value avalue = compute_attribute_value (tw, attr);

#if DEBUG_TRACE
          std::cerr << "class attribute: " << aname << " = "
                    << attribute_value_to_string (attr, avalue) << std::endl;
#endif

          retval.put (aname, avalue);
        }
    }

  octave::tree_classdef_body *b = t->body ();

  if (b)
    {
      // Keep track of the get/set accessor methods.  They will be used
      // later on when creating properties.

      std::map<std::string, octave_value> get_methods;
      std::map<std::string, octave_value> set_methods;

      // Method blocks

      std::list<octave::tree_classdef_methods_block *> mb_list = b->methods_list ();

      octave::load_path& lp = interp.get_load_path ();

      for (auto& mb_p : mb_list)
        {
          std::map<std::string, octave_value> amap;

#if DEBUG_TRACE
          std::cerr << "method block" << std::endl;
#endif

          // Method attributes

          if (mb_p->attribute_list ())
            {
              for (auto& attr_p : *mb_p->attribute_list ())
                {
                  std::string aname = attr_p->ident ()->name ();
                  octave_value avalue = compute_attribute_value (tw, attr_p);

#if DEBUG_TRACE
                  std::cerr << "method attribute: " << aname << " = "
                            << attribute_value_to_string (attr_p, avalue)
                            << std::endl;
#endif

                  amap[aname] = avalue;
                }
            }

          // Methods

          if (mb_p->element_list ())
            {
              for (auto& mtd : *mb_p->element_list ())
                {
                  std::string mname = mtd.function_value ()->name ();
                  std::string mprefix = mname.substr (0, 4);

                  if (mprefix == "get.")
                    get_methods[mname.substr (4)] =
                      make_fcn_handle (mtd, full_class_name + '>' + mname);
                  else if (mprefix == "set.")
                    set_methods[mname.substr (4)] =
                      make_fcn_handle (mtd, full_class_name + '>' + mname);
                  else
                    {
                      cdef_method meth = cdm.make_method (retval, mname, mtd);

#if DEBUG_TRACE
                      std::cerr << (mname == class_name ? "constructor"
                                                        : "method")
                                << ": " << mname << std::endl;
#endif

                      for (auto& attrnm_val : amap)
                        meth.put (attrnm_val.first, attrnm_val.second);

                      retval.install_method (meth);
                    }
                }
            }
        }

      if (is_at_folder)
        {
          // Look for all external methods visible on octave path at the
          // time of loading of the class.
          //
          // FIXME: This is an "extension" to Matlab behavior, which only looks
          // in the @-folder containing the original classdef file.  However,
          // this is easier to implement it that way at the moment.

          std::list<std::string> external_methods
            = lp.methods (full_class_name);

          for (const auto& mtdnm : external_methods)
            {
              // FIXME: should we issue a warning if the method is already
              // defined in the classdef file?

              if (mtdnm != class_name
                  && ! retval.find_method (mtdnm, true).ok ())
                {
                  // Create a dummy method that is used until the actual
                  // method is loaded.
                  octave_user_function *fcn = new octave_user_function ();

                  fcn->stash_function_name (mtdnm);

                  cdef_method meth
                    = cdm.make_method (retval, mtdnm, octave_value (fcn));

                  retval.install_method (meth);
                }
            }
        }

      // Property blocks

      // FIXME: default property expression should be able to call static
      //        methods of the class being constructed.  A restricted CLASSNAME
      //        symbol should be added to the scope before evaluating default
      //        value expressions.

      std::list<octave::tree_classdef_properties_block *> pb_list
        = b->properties_list ();

      for (auto& pb_p : pb_list)
        {
          std::map<std::string, octave_value> amap;

#if DEBUG_TRACE
          std::cerr << "property block" << std::endl;
#endif

          // Property attributes

          if (pb_p->attribute_list ())
            {
              for (auto& attr_p : *pb_p->attribute_list ())
                {
                  std::string aname = attr_p->ident ()->name ();
                  octave_value avalue = compute_attribute_value (tw, attr_p);

#if DEBUG_TRACE
                  std::cerr << "property attribute: " << aname << " = "
                            << attribute_value_to_string (attr_p, avalue)
                            << std::endl;
#endif

                  if (aname == "Access")
                    {
                      amap["GetAccess"] = avalue;
                      amap["SetAccess"] = avalue;
                    }
                  else
                    amap[aname] = avalue;
                }
            }

          // Properties

          if (pb_p->element_list ())
            {
              for (auto& prop_p : *pb_p->element_list ())
                {
                  std::string prop_name = prop_p->ident ()->name ();

                  cdef_property prop = cdm.make_property (retval, prop_name);

#if DEBUG_TRACE
                  std::cerr << "property: " << prop_p->ident ()->name ()
                            << std::endl;
#endif

                  octave::tree_expression *expr = prop_p->expression ();
                  if (expr)
                    {
                      octave_value pvalue = tw.evaluate (expr);

#if DEBUG_TRACE
                      std::cerr << "property default: "
                                << attribute_value_to_string (*pit, pvalue)
                                << std::endl;
#endif

                      prop.put ("DefaultValue", pvalue);
                    }

                  // Install property attributes.  This is done before assigning
                  // the property accessors so we can do validation by using
                  // cdef_property methods.

                  for (auto& attrnm_val : amap)
                    prop.put (attrnm_val.first, attrnm_val.second);

                  // Install property access methods, if any.  Remove the
                  // accessor methods from the temporary storage map, so we can
                  // detect which ones are invalid and do not correspond to a
                  // defined property.

                  std::map<std::string, octave_value>::iterator git =
                    get_methods.find (prop_name);

                  if (git != get_methods.end ())
                    {
                      make_function_of_class (retval, git->second);
                      prop.put ("GetMethod", git->second);
                      get_methods.erase (git);
                    }

                  std::map<std::string, octave_value>::iterator sit =
                    set_methods.find (prop_name);

                  if (sit != set_methods.end ())
                    {
                      make_function_of_class (retval, sit->second);
                      prop.put ("SetMethod", sit->second);
                      set_methods.erase (sit);
                    }

                  retval.install_property (prop);
                }
            }
        }
    }

  return retval;
}

octave_function*
cdef_class::get_method_function (const std::string& /* nm */)
{
  octave_classdef_meta *p = new octave_classdef_meta (*this);

  return p;
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

void
cdef_method::cdef_method_rep::check_method (void)
{
  if (is_external ())
    {
      if (is_dummy_method (function))
        {
          octave::load_path& lp
            = octave::__get_load_path__ ("cdef_method::cdef_method_rep::check_method");

          std::string name = get_name ();
          std::string cls_name = dispatch_type;
          std::string pack_name;

          size_t pos = cls_name.rfind ('.');

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
                = octave::load_fcn_from_file (file_name, dir_name,
                                              dispatch_type, pack_name);

              if (ov_fcn.is_defined ())
                {
                  function = ov_fcn;

                  make_function_of_class (dispatch_type, function);
                }
            }
        }
      else
        {
          // FIXME: check out-of-date status
        }

      if (is_dummy_method (function))
        error ("no definition found for method `%s' of class `%s'",
               get_name ().c_str (), dispatch_type.c_str ());
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

  if (function.is_defined ())
    retval = octave::feval (function, args, nargout);

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

  if (function.is_defined ())
    {
      octave_value_list new_args;

      new_args.resize (args.length () + 1);

      new_args(0) = to_ov (obj);
      for (int i = 0; i < args.length (); i++)
        new_args(i+1) = args(i);

      retval = octave::feval (function, new_args, nargout);
    }

  return retval;
}

bool
cdef_method::cdef_method_rep::is_constructor (void) const
{
  if (function.is_function())
    return function.function_value ()->is_classdef_constructor ();

  return false;
}

bool
cdef_method::cdef_method_rep::check_access (void) const
{
  cdef_class cls (to_cdef (get ("DefiningClass")));

  return ::check_access (cls, get ("Access"), get_name ());
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
      retval = (execute (idx.front (), type.length () > 1 ? 1 : nargout, true));
      break;

    default:
      error ("invalid meta.method indexing");
      break;
    }

  if (type.length () > 1 && idx.size () > 1 && ! retval.empty ())
    retval = retval(0).next_subsref (nargout, type, idx, 1);

  return retval;
}

static cdef_package
lookup_package (const std::string& name, bool error_if_not_found = true,
                bool load_if_not_found = true)
{
  cdef_manager& cdm = octave::__get_cdef_manager__ ("lookup_package");

  return cdm.find_package (name, error_if_not_found, load_if_not_found);
}

static octave_value_list
package_fromName (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () != 1)
    error ("fromName: invalid number of parameters");

  std::string name = args(0).xstring_value ("fromName: PACKAGE_NAME must be a string");

  retval(0) = to_ov (lookup_package (name, false));

  return retval;
}

static octave_value_list
package_get_classes (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval (1, Matrix ());

  if (args.length () == 1 && args(0).type_name () == "object"
      && args(0).class_name () == "meta.package")
    {
      cdef_package pack (to_cdef (args(0)));

      retval(0) = pack.get_classes ();
    }

  return retval;
}

static octave_value_list
package_get_functions (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval (1, Matrix ());

  if (args.length () == 0 && args(0).type_name () == "object"
      && args(0).class_name () == "meta.package")
    {
      cdef_package pack (to_cdef (args(0)));

      retval(0) = pack.get_functions ();
    }

  return retval;
}

static octave_value_list
package_get_packages (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval (1, Matrix ());

  if (args.length () == 0 && args(0).type_name () == "object"
      && args(0).class_name () == "meta.package")
    {
      cdef_package pack (to_cdef (args(0)));

      retval(0) = pack.get_packages ();
    }

  return retval;
}

static octave_value_list
package_getAllPackages (octave::interpreter& interp,
                        const octave_value_list& /* args */, int /* nargout */)
{
  std::map<std::string, cdef_package> toplevel_packages;

  octave::load_path& lp = interp.get_load_path ();

  std::list<std::string> names = lp.get_all_package_names ();

  cdef_manager& cdm = octave::__get_cdef_manager__ ("package_getAllPackages");

  toplevel_packages["meta"] = cdm.find_package ("meta", false, false);

  for (const auto& nm : names)
    toplevel_packages[nm] = cdm.find_package (nm, false, true);

  Cell c (toplevel_packages.size (), 1);

  int i = 0;

  for (const auto& nm_pkg : toplevel_packages)
    c(i++,0) = to_ov (nm_pkg.second);

  return octave_value_list (octave_value (c));
}

void
cdef_package::cdef_package_rep::install_class (const cdef_class& cls,
                                               const std::string& nm)
{
  class_map[nm] = cls;

  member_count++;
}

void
cdef_package::cdef_package_rep::install_function (const octave_value& fcn,
                                                  const std::string& nm)
{
  function_map[nm] = fcn;
}

void
cdef_package::cdef_package_rep::install_package (const cdef_package& pack,
                                                 const std::string& nm)
{
  package_map[nm] = pack;

  member_count++;
}

template <typename T1, typename T2>
Cell
map2Cell (const std::map<T1, T2>& m)
{
  Cell retval (1, m.size ());
  int i = 0;

  for (typename std::map<T1, T2>::const_iterator it = m.begin ();
       it != m.end (); ++it, ++i)
    {
      retval(i) = to_ov (it->second);
    }

  return retval;
}

Cell
cdef_package::cdef_package_rep::get_classes (void) const
{ return map2Cell (class_map); }

Cell
cdef_package::cdef_package_rep::get_functions (void) const
{ return map2Cell (function_map); }

Cell
cdef_package::cdef_package_rep::get_packages (void) const
{ return map2Cell (package_map); }

octave_value
cdef_package::cdef_package_rep::find (const std::string& nm)
{
  std::string symbol_name = get_name () + '.' + nm;

  octave::symbol_table& symtab
    = octave::__get_symbol_table__ ("cdef_package::cdef_package_rep::find");

  return symtab.find (symbol_name, octave_value_list (), true, false);
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

        std::string nm = idx.front ()(0).xstring_value ("invalid meta.package indexing, expected a symbol name");

#if DEBUG_TRACE
        std::cerr << "meta.package query: " << nm << std::endl;
#endif

        octave_value o = find (nm);

        if (! o.is_defined ())
          error ("member `%s' in package `%s' does not exist",
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

                retval = octave::feval (o, tmp_args, nargout);
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

  cdef_manager& cdm
    = octave::__get_cdef_manager__ ("cdef_package::cdef_package_rep::meta_release");

  // Don't delete the "meta" package.
  if (this != cdm.meta ().get_rep ())
    cdm.unregister_package (wrap ());
}

//----------------------------------------------------------------------------

cdef_manager::cdef_manager (octave::interpreter& interp)
  : m_interpreter (interp), m_all_classes (), m_all_packages (),
    m_meta_class (), m_meta_property (), m_meta_method (),
    m_meta_package (), m_meta ()
{
  octave::type_info& ti = m_interpreter.get_type_info ();

  octave_classdef::register_type (ti);

  // bootstrap
  cdef_class tmp_handle = make_class ("handle");

  m_meta_class = make_meta_class ("meta.class", tmp_handle);

  tmp_handle.set_class (m_meta_class);
  m_meta_class.set_class (m_meta_class);

  // meta classes
  m_meta_property = make_meta_class ("meta.property", tmp_handle);

  m_meta_method = make_meta_class ("meta.method", tmp_handle);

  m_meta_package = make_meta_class ("meta.package", tmp_handle);

  cdef_class tmp_meta_event
    = make_meta_class ("meta.event", tmp_handle);

  cdef_class tmp_meta_dynproperty
    = make_meta_class ("meta.dynamicproperty", tmp_handle);

  // meta.class properties
  m_meta_class.install_property
    (make_attribute (m_meta_class, "Abstract"));

  m_meta_class.install_property
    (make_attribute (m_meta_class, "ConstructOnLoad"));

  m_meta_class.install_property
    (make_property (m_meta_class, "ContainingPackage"));

  m_meta_class.install_property
    (make_property (m_meta_class, "Description"));

  m_meta_class.install_property
    (make_property (m_meta_class, "DetailedDescription"));

  m_meta_class.install_property
    (make_property (m_meta_class, "Events"));

  m_meta_class.install_property
    (make_attribute (m_meta_class, "HandleCompatible"));

  m_meta_class.install_property
    (make_attribute (m_meta_class, "Hidden"));

  m_meta_class.install_property
    (make_property (m_meta_class, "InferiorClasses",
                    make_fcn_handle (class_get_inferiorclasses,
                                     "meta.class>get.InferiorClasses"),
                    "public", Matrix (), "private"));

  m_meta_class.install_property
    (make_property (m_meta_class, "Methods",
                    make_fcn_handle (class_get_methods,
                                     "meta.class>get.Methods"),
                    "public", Matrix (), "private"));

  m_meta_class.install_property
    (make_property (m_meta_class, "MethodList",
                     make_fcn_handle (class_get_methods,
                                      "meta.class>get.MethodList"),
                    "public", Matrix (), "private"));

  m_meta_class.install_property (make_attribute (m_meta_class, "Name"));

  m_meta_class.install_property
    (make_property (m_meta_class, "Properties",
                    make_fcn_handle (class_get_properties,
                                     "meta.class>get.Properties"),
                    "public", Matrix (), "private"));

  m_meta_class.install_property
    (make_property (m_meta_class, "PropertyList",
                    make_fcn_handle (class_get_properties,
                                     "meta.class>get.PropertyList"),
                    "public", Matrix (), "private"));

  m_meta_class.install_property (make_attribute (m_meta_class, "Sealed"));

  m_meta_class.install_property
    (make_property (m_meta_class, "SuperClasses",
                    make_fcn_handle (class_get_superclasses,
                                     "meta.class>get.SuperClasses"),
                    "public", Matrix (), "private"));

  m_meta_class.install_property
    (make_property (m_meta_class, "SuperClassList",
                    make_fcn_handle (class_get_superclasses,
                                     "meta.class>get.SuperClassList"),
                    "public", Matrix (), "private"));

  // meta.class methods
  m_meta_class.install_method
    (make_method (m_meta_class, "fromName", class_fromName, "public", true));

  m_meta_class.install_method
    (make_method (m_meta_class, "fevalStatic", class_fevalStatic, "public",
                  false));

  m_meta_class.install_method
    (make_method (m_meta_class, "getConstant", class_getConstant, "public",
                  false));

  m_meta_class.install_method (make_method (m_meta_class, "eq", class_eq));
  m_meta_class.install_method (make_method (m_meta_class, "ne", class_ne));
  m_meta_class.install_method (make_method (m_meta_class, "lt", class_lt));
  m_meta_class.install_method (make_method (m_meta_class, "le", class_le));
  m_meta_class.install_method (make_method (m_meta_class, "gt", class_gt));
  m_meta_class.install_method (make_method (m_meta_class, "ge", class_ge));

  // meta.method properties
  m_meta_method.install_property
    (make_attribute (m_meta_method, "Abstract"));

  m_meta_method.install_property
    (make_attribute (m_meta_method, "Access"));

  m_meta_method.install_property
    (make_attribute (m_meta_method, "DefiningClass"));

  m_meta_method.install_property
    (make_attribute (m_meta_method, "Description"));

  m_meta_method.install_property
    (make_attribute (m_meta_method, "DetailedDescription"));

  m_meta_method.install_property
    (make_attribute (m_meta_method, "Hidden"));

  m_meta_method.install_property
    (make_attribute (m_meta_method, "Name"));

  m_meta_method.install_property
    (make_attribute (m_meta_method, "Sealed"));

  m_meta_method.install_property
    (make_attribute (m_meta_method, "Static"));

  // meta.property properties
  m_meta_property.install_property
    (make_attribute (m_meta_property, "Name"));

  m_meta_property.install_property
    (make_attribute (m_meta_property, "Description"));

  m_meta_property.install_property
    (make_attribute (m_meta_property, "DetailedDescription"));

  m_meta_property.install_property
    (make_attribute (m_meta_property, "Abstract"));

  m_meta_property.install_property
    (make_attribute (m_meta_property, "Constant"));

  m_meta_property.install_property
    (make_attribute (m_meta_property, "GetAccess"));

  m_meta_property.install_property
    (make_attribute (m_meta_property, "SetAccess"));

  m_meta_property.install_property
    (make_attribute (m_meta_property, "Dependent"));

  m_meta_property.install_property
    (make_attribute (m_meta_property, "Transient"));

  m_meta_property.install_property
    (make_attribute (m_meta_property, "Hidden"));

  m_meta_property.install_property
    (make_attribute (m_meta_property, "GetObservable"));

  m_meta_property.install_property
    (make_attribute (m_meta_property, "SetObservable"));

  m_meta_property.install_property
    (make_attribute (m_meta_property, "GetMethod"));

  m_meta_property.install_property
    (make_attribute (m_meta_property, "SetMethod"));

  m_meta_property.install_property
    (make_attribute (m_meta_property, "DefiningClass"));

  m_meta_property.install_property
    (make_property (m_meta_property, "DefaultValue",
                    make_fcn_handle (property_get_defaultvalue,
                                     "meta.property>get.DefaultValue"),
                    "public", Matrix (), "private"));

  m_meta_property.install_property
    (make_attribute (m_meta_property, "HasDefault"));

  // meta.property events
  // FIXME: add events

  // handle methods

  tmp_handle.install_method
    (make_method (tmp_handle, "delete", handle_delete));

  // meta.package properties

  m_meta_package.install_property
    (make_attribute (m_meta_package, "Name"));

  m_meta_package.install_property
    (make_property (m_meta_package, "ContainingPackage"));

  m_meta_package.install_property
    (make_property (m_meta_package, "ClassList",
                    make_fcn_handle (package_get_classes,
                                     "meta.package>get.ClassList"),
                    "public", Matrix (), "private"));

  m_meta_package.install_property
    (make_property (m_meta_package, "Classes",
                    make_fcn_handle (package_get_classes,
                                     "meta.package>get.Classes"),
                    "public", Matrix (), "private"));

  m_meta_package.install_property
    (make_property (m_meta_package, "FunctionList",
                    make_fcn_handle (package_get_functions,
                                     "meta.package>get.FunctionList"),
                    "public", Matrix (), "private"));

  m_meta_package.install_property
    (make_property (m_meta_package, "Functions",
                    make_fcn_handle (package_get_functions,
                                     "meta.package>get.Functions"),
                    "public", Matrix (), "private"));

  m_meta_package.install_property
    (make_property (m_meta_package, "PackageList",
                      make_fcn_handle (package_get_packages,
                                       "meta.package>get.PackageList"),
                    "public", Matrix (), "private"));

  m_meta_package.install_property
    (make_property (m_meta_package, "Packages",
                    make_fcn_handle (package_get_packages,
                                     "meta.package>get.Packages"),
                    "public", Matrix (), "private"));

  m_meta_package.install_method
    (make_method (m_meta_package, "fromName", package_fromName,
                  "public", true));

  m_meta_package.install_method
    (make_method (m_meta_package, "getAllPackages", package_getAllPackages,
                  "public", true));

  // create "meta" package
  cdef_package package_meta
    = m_meta
    = make_package ("meta");

  package_meta.install_class (m_meta_class, "class");
  package_meta.install_class (m_meta_property, "property");
  package_meta.install_class (m_meta_method, "method");
  package_meta.install_class (m_meta_package, "package");
  package_meta.install_class (tmp_meta_event, "event");
  package_meta.install_class (tmp_meta_dynproperty, "dynproperty");

  octave::symbol_table& symtab = m_interpreter.get_symbol_table ();

  // install built-in classes into the symbol table
  symtab.install_built_in_function
    ("meta.class",
     octave_value (m_meta_class.get_constructor_function ()));

  symtab.install_built_in_function
    ("meta.method",
     octave_value (m_meta_method.get_constructor_function ()));

  symtab.install_built_in_function
    ("meta.property",
     octave_value (m_meta_property.get_constructor_function ()));

  symtab.install_built_in_function
    ("meta.package",
     octave_value (m_meta_package.get_constructor_function ()));

// FIXME: meta.event and meta.dynproperty are not implemented
//        and should not be installed into symbol table.

//  symtab.install_built_in_function
//    ("meta.event",
//     octave_value (tmp_meta_event.get_constructor_function ()));

//  symtab.install_built_in_function
//    ("meta.dynproperty",
//     octave_value (tmp_meta_dynproperty.get_constructor_function ()));
}

cdef_class
cdef_manager::find_class (const std::string& name, bool error_if_not_found,
                          bool load_if_not_found)
{
  std::map<std::string, cdef_class>::iterator it = m_all_classes.find (name);

  if (it == m_all_classes.end ())
    {
      if (load_if_not_found)
        {
          octave_value ov_cls;

          size_t pos = name.rfind ('.');

          if (pos == std::string::npos)
            {
              octave::symbol_table& symtab
                = octave::__get_symbol_table__ ("cdef_manager::find_class");

              ov_cls = symtab.find (name);
            }
          else
            {
              std::string pack_name = name.substr (0, pos);

              cdef_package pack = find_package (pack_name, false, true);

              if (pack.ok ())
                ov_cls = pack.find (name.substr (pos+1));
            }

          if (ov_cls.is_defined ())
            it = m_all_classes.find (name);
        }
    }

  if (it == m_all_classes.end ())
    {
      if (error_if_not_found)
        error ("class not found: %s", name.c_str ());
    }
  else
    {
      cdef_class cls = it->second;

      if (! cls.is_builtin ())
        cls = lookup_class (cls);

      if (cls.ok ())
        return cls;
      else
        m_all_classes.erase (it);
    }

  return cdef_class ();
}

octave_function *
cdef_manager::find_method_symbol (const std::string& method_name,
                                  const std::string& class_name)
{
  octave_function *retval = nullptr;

  cdef_class cls = find_class (class_name, false, false);

  if (cls.ok ())
    {
      cdef_method meth = cls.find_method (method_name);

      if (meth.ok ())
        retval = new octave_classdef_meta (meth);
    }

  return retval;
}

cdef_package
cdef_manager::find_package (const std::string& name, bool error_if_not_found,
                            bool load_if_not_found)
{
  cdef_package retval;

  std::map<std::string, cdef_package>::const_iterator it
    = m_all_packages.find (name);

  if (it != m_all_packages.end ())
    {
      retval = it->second;

      if (! retval.ok ())
        error ("invalid package `%s'", name.c_str ());
    }
  else
    {
      octave::load_path& lp
        = octave::__get_load_path__ ("cdef_manager::find_package");

      if (load_if_not_found && lp.find_package (name))
        {
          size_t pos = name.find ('.');

          if (pos == std::string::npos)
            retval = make_package (name, "");
          else
            {
              std::string parent_name = name.substr (0, pos);

              retval = make_package (name, parent_name);
            }
        }
      else if (error_if_not_found)
        error ("unknown package `%s'", name.c_str ());
    }

  return retval;
}

octave_function *
cdef_manager::find_package_symbol (const std::string& pack_name)
{
  octave_function *retval = nullptr;

  cdef_package pack = find_package (pack_name, false);

  if (pack.ok ())
    retval = new octave_classdef_meta (pack);

  return retval;
}

//----------------------------------------------------------------------------

DEFUN (__meta_get_package__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __meta_get_package__ ()
Undocumented internal function.
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  std::string cname = args(0).xstring_value ("PACKAGE_NAME must be a string");

  return to_ov (lookup_package (cname));
}

DEFUN (__superclass_reference__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __superclass_reference__ ()
Undocumented internal function.
@end deftypefn */)
{
  return ovl (new octave_classdef_superclass_ref (args));
}

DEFUN (__meta_class_query__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __meta_class_query__ ()
Undocumented internal function.
@end deftypefn */)
{
#if DEBUG_TRACE
  std::cerr << "__meta_class_query__ ("
            << args(0).string_value () << ')'
            << std::endl;
#endif

  if (args.length () != 1)
    print_usage ();

  std::string cls = args(0).xstring_value ("CLASS_NAME must be a string");

  return to_ov (lookup_class (cls));
}

DEFUN (metaclass, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} metaclass (obj)
Returns the meta.class object corresponding to the class of @var{obj}.
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  cdef_object obj = to_cdef (args(0));

  return to_ov (obj.get_class ());
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
