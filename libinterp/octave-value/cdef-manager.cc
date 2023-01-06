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

#include "cdef-manager.h"
#include "cdef-utils.h"
#include "interpreter.h"
#include "ov-classdef.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static octave_value
make_fcn_handle (octave_builtin::fcn ff, const std::string& nm)
{
  octave_value fcn (new octave_builtin (ff, nm));

  return octave_value (new octave_fcn_handle (fcn));
}

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
    error ("fevalStatic: method '%s' is not static", meth_name.c_str ());

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
    error ("getConstant: property '%s' is not constant",
           prop_name.c_str ());

  retval(0) = prop.get_value (true, "getConstant");

  return retval;
}

#define META_CLASS_CMP(OP, CLSA, CLSB, FCN)                             \
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
    retval(0) = FCN (CLSA, CLSB);                                       \
                                                                        \
    return retval;                                                      \
  }

META_CLASS_CMP (lt, clsb, clsa, is_strict_superclass)
META_CLASS_CMP (le, clsb, clsa, is_superclass)
META_CLASS_CMP (gt, clsa, clsb, is_strict_superclass)
META_CLASS_CMP (ge, clsa, clsb, is_superclass)
META_CLASS_CMP (eq, clsa, clsb, operator==)
META_CLASS_CMP (ne, clsa, clsb, operator!=)

static octave_value_list
property_get_defaultvalue (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () == 1 && args(0).type_name () == "object")
    {
      cdef_property prop (to_cdef (args(0)));

      retval(0) = prop.get ("DefaultValue");

      if (! retval(0).is_defined ())
        error_with_id ("Octave:class:NoDefaultDefined",
                       "no default value for property '%s'",
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
package_getAllPackages (interpreter& interp,
                        const octave_value_list& /* args */,
                        int /* nargout */)
{
  std::map<std::string, cdef_package> toplevel_packages;

  load_path& lp = interp.get_load_path ();

  std::list<std::string> names = lp.get_all_package_names ();

  cdef_manager& cdm = interp.get_cdef_manager ();

  toplevel_packages["meta"] = cdm.find_package ("meta", false, false);

  for (const auto& nm : names)
    toplevel_packages[nm] = cdm.find_package (nm, false, true);

  Cell c (toplevel_packages.size (), 1);

  int i = 0;

  for (const auto& nm_pkg : toplevel_packages)
    c(i++, 0) = to_ov (nm_pkg.second);

  return octave_value_list (octave_value (c));
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

cdef_manager::cdef_manager (interpreter& interp)
  : m_interpreter (interp), m_all_classes (), m_all_packages (),
    m_meta_class (), m_meta_property (), m_meta_method (),
    m_meta_package (), m_meta ()
{
  type_info& ti = m_interpreter.get_type_info ();

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
  (make_property (m_meta_class, "SuperclassList",
                  make_fcn_handle (class_get_superclasses,
                                   "meta.class>get.SuperclassList"),
                  "public", Matrix (), "private"));

  // FIXME: Matlab supports this property under "SuperclassList".
  //        Octave, however, has supported this under "SuperClassList".
  //        Alias the property.  Remove in Octave version 8.1.
  m_meta_class.install_property
  (make_property (m_meta_class, "SuperClassList",
                  make_fcn_handle (class_get_superclasses,
                                   "meta.class>get.SuperclassList"),
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

  symbol_table& symtab = m_interpreter.get_symbol_table ();

  // install built-in classes into the symbol table
  symtab.install_built_in_function
  ("meta.class", m_meta_class.get_constructor_function ());

  symtab.install_built_in_function
  ("meta.method", m_meta_method.get_constructor_function ());

  symtab.install_built_in_function
  ("meta.property", m_meta_property.get_constructor_function ());

  symtab.install_built_in_function
  ("meta.package", m_meta_package.get_constructor_function ());

  // FIXME: meta.event and meta.dynproperty are not implemented
  //        and should not be installed into symbol table.

  //  symtab.install_built_in_function
  //    ("meta.event", tmp_meta_event.get_constructor_function ());

  //  symtab.install_built_in_function
  //    ("meta.dynproperty", tmp_meta_dynproperty.get_constructor_function ());
}

cdef_class
cdef_manager::find_class (const std::string& name, bool error_if_not_found,
                          bool load_if_not_found)
{
  auto it = m_all_classes.find (name);

  if (it == m_all_classes.end ())
    {
      if (load_if_not_found)
        {
          octave_value ov_cls;

          std::size_t pos = name.rfind ('.');

          if (pos == std::string::npos)
            ov_cls = m_interpreter.find (name);
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

octave_value
cdef_manager::find_method_symbol (const std::string& method_name,
                                  const std::string& class_name)
{
  cdef_class cls = find_class (class_name, false, false);

  if (cls.ok ())
    {
      cdef_method meth = cls.find_method (method_name);

      if (meth.ok ())
        return octave_value (new octave_classdef_meta (meth));
    }

  return octave_value ();
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
        error ("invalid package '%s'", name.c_str ());
    }
  else
    {
      load_path& lp = m_interpreter.get_load_path ();

      if (load_if_not_found && lp.find_package (name))
        {
          std::size_t pos = name.rfind ('.');

          if (pos == std::string::npos)
            retval = make_package (name, "");
          else
            {
              std::string parent_name = name.substr (0, pos);

              retval = make_package (name, parent_name);
            }
        }
      else if (error_if_not_found)
        error ("unknown package '%s'", name.c_str ());
    }

  return retval;
}

octave_value
cdef_manager::find_package_symbol (const std::string& pack_name)
{
  cdef_package pack = find_package (pack_name, false);

  if (pack.ok ())
    return octave_value (new octave_classdef_meta (pack));

  return octave_value ();
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

octave_value
cdef_manager::find_method (const std::string& class_name,
                           const std::string& name) const
{
  cdef_class cls = lookup_class (class_name);

  return cls.get_method (name);
}

OCTAVE_END_NAMESPACE(octave)
