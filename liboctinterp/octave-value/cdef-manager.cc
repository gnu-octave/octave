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

#include "cdef-manager.h"
#include "cdef-utils.h"
#include "interpreter.h"
#include "interpreter-private.h"
#include "ov-classdef.h"
#include "unwind-prot.h"


OCTAVE_BEGIN_NAMESPACE(octave)

static octave_value
make_fcn_handle (octave_builtin::fcn ff, const std::string& nm)
{
  octave_value fcn (new octave_builtin (ff, nm));

  return octave_value (new octave_fcn_handle (fcn));
}

static octave_value_list
class_get_properties (const octave_value_list& args, int)
{
  octave_value_list retval;

  if (args.length () == 1 && args(0).type_name () == "object")
    {
      cdef_class cls (to_cdef (args(0)));

      retval(0) = cls.get ("PropertyList");
    }

  return retval;
}

static octave_value_list
class_get_methods (const octave_value_list& args, int)
{
  octave_value_list retval;

  if (args.length () == 1 && args(0).type_name () == "object")
    {
      cdef_class cls (to_cdef (args(0)));

      retval(0) = cls.get ("MethodList");
    }

  return retval;
}

static octave_value_list
class_get_superclasses (const octave_value_list& args, int)
{
  octave_value_list retval;

  if (args.length () == 1 && args(0).type_name () == "object"
      && args(0).class_name () == "meta.class")
    {
      cdef_class cls (to_cdef (args(0)));

      // Pass the object itself rather than its Array: the Array overload of
      // to_ov drops an empty list to [], losing the meta.class type.
      retval(0) = cls.get ("SuperclassList");
    }

  return retval;
}

static octave_value_list
class_get_inferiorclasses (const octave_value_list& args, int)
{
  octave_value_list retval;

  if (args.length () == 1 && args(0).type_name () == "object"
      && args(0).class_name () == "meta.class")
    {
      cdef_class cls (to_cdef (args(0)));

      retval(0) = cls.get ("InferiorClasses");
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

  if (args.length () == 1 && args(0).type_name () == "object"
      && args(0).class_name () == "meta.package")
    {
      cdef_package pack (to_cdef (args(0)));

      retval(0) = pack.get_functions ();
    }

  return retval;
}

//! Return immediate subpackages for meta.package.PackageList and its Packages
//! alias.  Return an empty matrix if the argument is not a meta.package object.
static octave_value_list
package_get_packages (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval (1, Matrix ());

  if (args.length () == 1 && args(0).type_name () == "object"
      && args(0).class_name () == "meta.package")
    {
      cdef_package pack (to_cdef (args(0)));

      retval(0) = pack.get_packages ();
    }

  return retval;
}

static octave_value_list
package_getAllPackages (const octave_value_list& /* args */, int /* nargout */)
{
  std::map<std::string, cdef_package> toplevel_packages;

  interpreter& interp = __get_interpreter__ ();

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
  : m_interpreter (interp), m_all_classes (), m_all_packages ()
{ }

void
cdef_manager::initialize ()
{
  type_info& ti = m_interpreter.get_type_info ();

  octave_classdef::register_type (ti);

  // Bootstrap -- start with "handle", then create "meta.metadata" and
  // "meta.class" because the latter two classes needs the former to be a
  // superclass.  Then, set the class of "handle" and "meta.metadata" to
  // "meta.class"

  cdef_class handle (
    "handle",
    cdef_class_opt_properties {
      .abstract = true,
      .construct_on_load = true,
      .handle_compatible = true
    }
  );

  handle.mark_as_handle_class ();
  register_class (handle);


  cdef_method delete_method (
    handle,
    make_fcn_handle (handle_delete, "delete"),
    "delete"
  );
  handle.install_method (delete_method);

  cdef_class_opt_properties metadata_properties {
    .abstract = true,
    .handle_compatible = true,
    .superclass_list = to_ov (handle)
  };

  m_meta_metadata = cdef_class (
    "meta.metadata",
    metadata_properties
  );
  m_meta_metadata.mark_as_meta_class ();
  register_class (m_meta_metadata);

  cdef_class_opt_properties metadata_class_properties {
    .abstract = true,
    .sealed = true,
    .superclass_list = to_ov (m_meta_metadata)
  };

  m_meta_class = cdef_class ("meta.class", metadata_class_properties);
  m_meta_class.mark_as_meta_class ();
  m_meta_class.set_class (m_meta_class);
  register_class (m_meta_class);

  handle.set_class (m_meta_class);
  m_meta_metadata.set_class (m_meta_class);

  // Create the rest of the octave.metadata.* classes

  m_meta_property = cdef_class (
    "meta.property",
    cdef_class_opt_properties {
      .abstract = true,
      .handle_compatible = true,
      .sealed = true,
      .superclass_list = to_ov (m_meta_metadata)
    }
  );
  m_meta_property.mark_as_meta_class ();
  m_meta_property.set_class (m_meta_class);
  register_class (m_meta_property);

  m_meta_method = cdef_class (
    "meta.method",
    cdef_class_opt_properties {
      .abstract = true,
      .handle_compatible = true,
      .sealed = true,
      .superclass_list = to_ov (m_meta_metadata)
    }
  );
  m_meta_method.mark_as_meta_class ();
  m_meta_method.set_class (m_meta_class);
  register_class (m_meta_method);

  // The delete method predates meta.method during bootstrap.
  delete_method.set_class (m_meta_method);

  // These operators compare metaclasses and their inheritance relationships.
  // They are not general handle-object comparisons.
  cdef_method eq (
    m_meta_class,
    make_fcn_handle (class_eq, "eq"),
    "eq"
  );
  m_meta_class.install_method (eq);

  cdef_method ne (
    m_meta_class,
    make_fcn_handle (class_ne, "ne"),
    "ne"
  );
  m_meta_class.install_method (ne);

  cdef_method lt (
    m_meta_class,
    make_fcn_handle (class_lt, "lt"),
    "lt"
  );
  m_meta_class.install_method (lt);

  cdef_method le (
    m_meta_class,
    make_fcn_handle (class_le, "le"),
    "le"
  );
  m_meta_class.install_method (le);

  cdef_method gt (
    m_meta_class,
    make_fcn_handle (class_gt, "gt"),
    "gt"
  );
  m_meta_class.install_method (gt);

  cdef_method ge (
    m_meta_class,
    make_fcn_handle (class_ge, "ge"),
    "ge"
  );
  m_meta_class.install_method (ge);

  m_meta_package = cdef_class (
    "meta.package",
    cdef_class_opt_properties {
      .abstract = true,
      .handle_compatible = true,
      .sealed = true,
      .superclass_list = to_ov (handle)
    }
  );
  m_meta_package.mark_as_meta_class ();
  m_meta_package.set_class (m_meta_class);
  register_class (m_meta_package);

  metadata_properties.sealed = true;

  cdef_class meta_event
    = cdef_class ("meta.event", metadata_properties);
  meta_event.mark_as_meta_class ();
  meta_event.set_class (m_meta_class);
  register_class (meta_event);

  cdef_class meta_dynamicproperty
    = cdef_class ("meta.dynamicproperty", metadata_properties);
  meta_dynamicproperty.mark_as_meta_class ();
  meta_dynamicproperty.set_class (m_meta_class);
  register_class (meta_dynamicproperty);

  m_meta_class.install_property (
    cdef_property (
      m_meta_class,
      "Abstract",
      cdef_property_opt_properties {
        .default_value = false,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_class.install_property (
    cdef_property (
      m_meta_class,
      "ConstructOnLoad",
      cdef_property_opt_properties {
        .default_value = false,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_class.install_property (
    cdef_property (
      m_meta_class,
      "ContainingPackage",
      cdef_property_opt_properties {
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_class.install_property (
    cdef_property (
      m_meta_class,
      "Description",
      cdef_property_opt_properties {
        .default_value = "",
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_class.install_property (
    cdef_property (
      m_meta_class,
      "DetailedDescription",
      cdef_property_opt_properties {
        .default_value = "",
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_class.install_property (
    cdef_property (
      m_meta_class,
      "Enumeration",
      cdef_property_opt_properties {
        .default_value = false,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_class.install_property (
    cdef_property (
      m_meta_class,
      "EventList",
      cdef_property_opt_properties {
        .default_value = Matrix(),
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_class.install_property (
    cdef_property (
      m_meta_class,
      "HandleCompatible",
      cdef_property_opt_properties {
        .default_value = false,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_class.install_property (
    cdef_property (
      m_meta_class,
      "Hidden",
      cdef_property_opt_properties {
        .default_value = false,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_class.install_property (
    cdef_property (
      m_meta_class,
      "InferiorClasses",
      cdef_property_opt_properties {
        .get_access = "public",
        .get_method = make_fcn_handle (
          class_get_inferiorclasses,
          "meta.class>get.InferiorClasses"
        ),
        .set_access = "private"
      }
    )
  );

  octave_value method_list_getter = make_fcn_handle (
    class_get_methods,
    "meta.class>get.MethodList"
  );

  m_meta_class.install_property (
    cdef_property (
      m_meta_class,
      "MethodList",
      cdef_property_opt_properties {
        .get_access = "public",
        .get_method = method_list_getter,
        .set_access = "private"
      }
    )
  );

  // Backwards compatibility for "Methods" on "meta.class"
  m_meta_class.install_property (
    cdef_property (
      m_meta_class,
      "Methods",
      cdef_property_opt_properties {
        .get_access = "public",
        .get_method = method_list_getter,
        .hidden = true,
        .set_access = "private"
      }
    )
  );

  m_meta_class.install_property (
    cdef_property (
      m_meta_class,
      "Name",
      cdef_property_opt_properties {
        .default_value = "",
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  octave_value property_list_getter = make_fcn_handle (
    class_get_properties,
    "meta.class>get.PropertyList"
  );

  m_meta_class.install_property (
    cdef_property (
      m_meta_class,
      "PropertyList",
      cdef_property_opt_properties {
        .default_value = false,
        .get_access = "public",
        .get_method = property_list_getter,
        .set_access = "private"
      }
    )
  );

  // Backwards compatibility for "Properties" on "meta.class"
  m_meta_class.install_property (
    cdef_property (
      m_meta_class,
      "Properties",
      cdef_property_opt_properties {
        .default_value = false,
        .get_access = "public",
        .get_method = property_list_getter,
        .hidden = true,
        .set_access = "private"
      }
    )
  );

  m_meta_class.install_property (
    cdef_property (
      m_meta_class,
      "RestrictsSubclassing",
      cdef_property_opt_properties {
        .default_value = false,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_class.install_property (
    cdef_property (
      m_meta_class,
      "Sealed",
      cdef_property_opt_properties {
        .default_value = false,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  octave_value superclass_getter = make_fcn_handle (
    class_get_superclasses,
    "meta.class>get.SuperclassList"
  );

  m_meta_class.install_property (
    cdef_property (
      m_meta_class,
      "SuperclassList",
      cdef_property_opt_properties {
        .default_value = false,
        .get_access = "public",
        .get_method = superclass_getter,
        .set_access = "private"
      }
    )
  );

  // Preserve the historical spellings as hidden aliases.
  for (const auto& name : {"SuperClasses", "SuperClassList", "Superclasses"})
    m_meta_class.install_property (
      cdef_property (
        m_meta_class,
        name,
        cdef_property_opt_properties {
          .default_value = false,
          .get_access = "public",
          .get_method = superclass_getter,
          .hidden = true,
          .set_access = "private"
        }
      )
    );

  // No private no-op constructor is installed for the meta classes.  Direct
  // construction is already refused by the Abstract check in
  // cdef_class_rep::construct_object, and a method named after the class
  // shadows any function of the same name for every object of that class:
  // installing one called "class" made class (?some_class) fail with an
  // access error instead of returning "meta.class".

  m_meta_class.install_method (
    cdef_method (
      m_meta_class,
      make_fcn_handle (class_fromName, "meta.class>fromName"),
      "fromName",
      cdef_method_opt_properties {
        .static_bool = true
      }
    )
  );

  // Not part of the public API, but this is the mechanism by which static
  // methods are called
  m_meta_class.install_method (
    cdef_method (
      m_meta_class,
      make_fcn_handle (class_fevalStatic, "meta.class>fevalStatic"),
      "fevalStatic",
      cdef_method_opt_properties {
        .hidden = true,
      }
    )
  );

  // Likewise with "fevalStatic", not part of the public API, but this is
  // the mechanism by which constant properties can be viewed without
  // instantiating the object
  m_meta_class.install_method (
    cdef_method (
      m_meta_class,
      make_fcn_handle (class_getConstant, "meta.class>getConstant"),
      "getConstant",
      cdef_method_opt_properties {
        .hidden = true,
      }
    )
  );

  cdef_method_opt_properties meta_method_default_vals;

  // meta.method properties
  m_meta_method.install_property (
    cdef_property (
      m_meta_method,
      "Abstract",
      cdef_property_opt_properties {
        .default_value = meta_method_default_vals.abstract,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_method.install_property (
    cdef_property (
      m_meta_method,
      "Access",
      cdef_property_opt_properties {
        .default_value = "public",
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_method.install_property (
    cdef_property (
      m_meta_method,
      "DefiningClass",
      cdef_property_opt_properties {
        .default_value = Matrix (),
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_method.install_property (
    cdef_property (
      m_meta_method,
      "Description",
      cdef_property_opt_properties {
        .default_value = meta_method_default_vals.description,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_method.install_property (
    cdef_property (
      m_meta_method,
      "DetailedDescription",
      cdef_property_opt_properties {
        .default_value = meta_method_default_vals.detailed_description,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_method.install_property (
    cdef_property (
      m_meta_method,
      "Hidden",
      cdef_property_opt_properties {
        .default_value = meta_method_default_vals.hidden,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_method.install_property (
    cdef_property (
      m_meta_method,
      "Name",
      cdef_property_opt_properties {
        .default_value = "",
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_method.install_property (
    cdef_property (
      m_meta_method,
      "Sealed",
      cdef_property_opt_properties {
        .default_value = meta_method_default_vals.sealed,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_method.install_property (
    cdef_property (
      m_meta_method,
      "Static",
      cdef_property_opt_properties {
        .default_value = meta_method_default_vals.static_bool,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  // Install properties
  cdef_property_opt_properties meta_property_default_vals;

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "AbortSet",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.abort_set,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "Abstract",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.abstract,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "Access",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.access,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "Constant",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.constant,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "DefaultValue",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.default_value,
        .get_access = "public",
        .get_method = make_fcn_handle (
          property_get_defaultvalue,
          "meta.property>get.DefaultValue"
        ),
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "DefiningClass",
      cdef_property_opt_properties {
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "Dependent",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.dependent,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "Description",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.description,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "DetailedDescription",
      cdef_property_opt_properties {
        .default_value
          = meta_property_default_vals.detailed_description,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "GetAccess",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.get_access,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "GetMethod",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.get_method,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "GetObservable",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.get_observable,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "HasDefault",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.has_default,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "Hidden",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.hidden,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "Name",
      cdef_property_opt_properties {
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "NonCopyable",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.non_copyable,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "PartialMatchPriority",
      cdef_property_opt_properties {
        .default_value
          = meta_property_default_vals.partial_match_priority,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "SetAccess",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.set_access,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "SetMethod",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.set_method,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "SetObservable",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.set_observable,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "Transient",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.transient,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "Validation",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.validation,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_property.install_property (
    cdef_property (
      m_meta_property,
      "WeakHandle",
      cdef_property_opt_properties {
        .default_value = meta_property_default_vals.weak_handle,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  // meta.property events
  // FIXME: add events

  // "meta.package" properties and methods
  cdef_package_opt_properties meta_package_default_vals;

  m_meta_package.install_property (
    cdef_property (
      m_meta_package,
      "ClassList",
      cdef_property_opt_properties {
        .get_access = "public",
        .get_method = make_fcn_handle (
          package_get_classes,
          "meta.package>get.ClassList"
        ),
        .set_access = "private"
      }
    )
  );

  // Backwards compatibility for "Classes" on "meta.package"
  m_meta_package.install_property (
    cdef_property (
      m_meta_package,
      "Classes",
      cdef_property_opt_properties {
        .get_access = "public",
        .get_method = make_fcn_handle (
          package_get_classes,
          "meta.package>get.ClassList"
        ),
        .hidden = true,
        .set_access = "private"
      }
    )
  );

  m_meta_package.install_property (
    cdef_property (
      m_meta_package,
      "ContainingPackage",
      cdef_property_opt_properties {
        .get_access = "public",
        .hidden = true,
        .set_access = "private"
      }
    )
  );

  m_meta_package.install_property (
    cdef_property (
      m_meta_package,
      "Description",
      cdef_property_opt_properties {
        .default_value = meta_package_default_vals.description,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_package.install_property (
    cdef_property (
      m_meta_package,
      "DetailedDescription",
      cdef_property_opt_properties {
        .default_value = meta_package_default_vals.detailed_description,
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_package.install_property (
    cdef_property (
      m_meta_package,
      "FunctionList",
      cdef_property_opt_properties {
        .get_access = "public",
        .get_method = make_fcn_handle (
          package_get_functions,
          "meta.package>get.FunctionList"
        ),
        .set_access = "private"
      }
    )
  );

  // Backwards compatibility for "Functions" on "meta.package"
  m_meta_package.install_property (
    cdef_property (
      m_meta_package,
      "Functions",
      cdef_property_opt_properties {
        .get_access = "public",
        .get_method = make_fcn_handle (
          package_get_functions,
          "meta.package>get.FunctionList"
        ),
        .hidden = true,
        .set_access = "private"
      }
    )
  );

  m_meta_package.install_property (
    cdef_property (
      m_meta_package,
      "PackageList",
      cdef_property_opt_properties {
        .get_access = "public",
        .get_method = make_fcn_handle (
          package_get_packages,
          "meta.package>get.PackageList"
        ),
        .set_access = "private"
      }
    )
  );

  // Backwards compatibility for "Packages" on "meta.package".
  m_meta_package.install_property (
    cdef_property (
      m_meta_package,
      "Packages",
      cdef_property_opt_properties {
        .get_access = "public",
        .get_method = make_fcn_handle (
          package_get_packages,
          "meta.package>get.Packages"
        ),
        .hidden = true,
        .set_access = "private"
      }
    )
  );

  m_meta_package.install_property (
    cdef_property (
      m_meta_package,
      "InnerNamespaces",
      cdef_property_opt_properties {
        .default_value = Matrix (),
        .get_access = "public",
        //.get_method = make_fcn_handle (
        //  package_get_classes,
        //  "meta.package>get.ClassList"
        //),
        .set_access = "private"
      }
    )
  );

  m_meta_package.install_property (
    cdef_property (
      m_meta_package,
      "Name",
      cdef_property_opt_properties {
        .default_value = "",
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_package.install_property (
    cdef_property (
      m_meta_package,
      "OuterNamespaces",
      cdef_property_opt_properties {
        .default_value = Matrix (),
        .get_access = "public",
        .set_access = "private"
      }
    )
  );

  m_meta_package.install_method (
    cdef_method (
      m_meta_package,
      make_fcn_handle (
        package_fromName,
        "meta.package>fromName"
      ),
      "fromName",
      cdef_method_opt_properties {
        .static_bool = true
      }
    )
  );

  m_meta_package.install_method (
    cdef_method (
      m_meta_package,
      make_fcn_handle (
        package_getAllPackages,
        "meta.package>getAllPackages"
      ),
      "getAllPackages",
      cdef_method_opt_properties {
        .static_bool = true
      }
    )
  );

  // create "meta" package
  m_meta = cdef_package ("meta");
  m_meta.set_class (meta_package ());
  register_package (m_meta);

  octave_value empty_class_list = to_ov (empty_meta_class (dim_vector (0, 1)));
  handle.put ("SuperclassList", empty_class_list);
  handle.put ("InferiorClasses", empty_class_list);
  m_meta_metadata.put ("InferiorClasses", empty_class_list);
  m_meta_class.put ("InferiorClasses", empty_class_list);
  m_meta_package.put ("InferiorClasses", empty_class_list);
  meta_event.put ("InferiorClasses", empty_class_list);
  meta_dynamicproperty.put ("InferiorClasses", empty_class_list);

  m_meta.install_class (m_meta_class, "class");
  m_meta.install_class (m_meta_property, "property");
  m_meta.install_class (m_meta_method, "method");
  m_meta.install_class (m_meta_package, "package");
  m_meta.install_class (meta_event, "event");
  m_meta.install_class (meta_dynamicproperty, "dynamicproperty");

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
          if (std::find (m_load_stack.begin (), m_load_stack.end (), name)
              != m_load_stack.end ())
            {
              std::string chain;
              for (const auto& nm : m_load_stack)
                chain += nm + " -> ";
              chain += name;

              error ("inheritance cycle detected while loading class '%s': %s",
                     name.c_str (), chain.c_str ());
            }

          m_load_stack.push_back (name);

          unwind_action pop_load_stack
            ([this] () { m_load_stack.pop_back (); });

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
  else if (load_if_not_found)
    {
      load_path& lp = m_interpreter.get_load_path ();

      if (lp.find_package (name))
        {
          std::size_t pos = name.rfind ('.');

          if (pos == std::string::npos)
            {
              retval = cdef_package (name);
              retval.put ("ContainingPackage", Matrix ());
              register_package (retval);
            }
          else
            {
              std::string parent_name = name.substr (0, pos);

              retval = cdef_package (name);
              retval.put ("ContainingPackage", to_ov (find_package (parent_name)));
              register_package (retval);
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

octave_value
cdef_manager::find_method (const std::string& class_name,
                           const std::string& name) const
{
  cdef_class cls = lookup_class (class_name);

  return cls.get_method (name);
}

OCTAVE_END_NAMESPACE(octave)
