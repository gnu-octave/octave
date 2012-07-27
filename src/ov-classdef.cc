/*

Copyright (C) 2012 Michael Goffioul

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <map>

#include "defun.h"
#include "ov-builtin.h"
#include "ov-classdef.h"
#include "ov-fcn-handle.h"
#include "ov-typeinfo.h"

static std::map<std::string, cdef_class> all_classes;
static std::map<std::string, cdef_package> all_packages;

static void
gripe_method_access (const std::string& from, const cdef_method& meth)
{
  error ("%s: method `%s' has %s access and cannot be run in this context",
	 from.c_str (), meth.get_name ().c_str (),
	 meth.get_access ().c_str ());
}

static void
gripe_property_access (const std::string& from, const cdef_property& prop,
		       bool is_set = false)
{
  if (is_set)
    error ("%s: property `%s' has %s access and cannot be set in this context",
	   from.c_str (), prop.get_name ().c_str (),
	   prop.get_set_access ().c_str ());
  else
    error ("%s: property `%s' has %s access and cannot be obtained in this context",
	   from.c_str (), prop.get_name ().c_str (),
	   prop.get_get_access ().c_str ());
}

static octave_value
make_fcn_handle (octave_builtin::fcn ff, const std::string& nm)
{
  octave_value fcn (new octave_builtin (ff, nm));

  octave_value fcn_handle (new octave_fcn_handle (fcn, nm));

  return fcn_handle;
}

inline octave_value_list
execute_ov (octave_value val, const octave_value_list& args, int nargout)
{
  std::list<octave_value_list> idx (1, args);

  std::string type ("(");

  return val.subsref (type, idx, nargout);
}

static bool
check_access (const std::string& req, const std::string& acc)
{
  if (req == "private")
    return true;
  else if (req == "protected")
    return (acc != "private");
  else
    return (acc == "public");
}

static std::string
get_base_name (const std::string& nm)
{
  std::string::size_type pos = nm.find_last_of ('.');

  if (pos != std::string::npos)
    return nm.substr (pos + 1);

  return nm;
}

static std::string
superclass_access (const std::string& acc)
{
  if (acc == "public")
    return acc;
  else
    return "protected";
}

static cdef_class
lookup_class (const std::string& name, bool error_if_not_found = true)
{
  std::map<std::string, cdef_class>::iterator it = all_classes.find (name);

  if (it == all_classes.end ())
    {
      // FIXME: should look into load-path
      if (error_if_not_found)
	error ("class not found: %s", name.c_str ());
    }
  else
    {
      cdef_class& cls = it->second;

      if (! cls.is_builtin ())
	{
	  // FIXME: check whether a class reload is needed
	}

      if (cls.ok ())
	return cls;
      else
	all_classes.erase (it);
    }

  return cdef_class ();
}

static Cell
lookup_classes (const Cell& cls_names)
{
  Cell cls (cls_names.numel (), 1);

  for (int i = 0; i < cls_names.numel (); i++)
    {
      cdef_class c = lookup_class (cls_names(i).string_value ());

      if (! error_state)
	cls(i) = to_ov (c);
      else
	return Cell ();
    }

  return cls;
}

static bool
is_superclass (const cdef_class& clsa, const cdef_class& clsb,
	       bool allow_equal = true)
{
  if (allow_equal && clsa == clsb)
    return true;
  else
    {
      Cell c = clsb.get ("SuperClasses").cell_value ();

      bool retval = false;

      for (int i = 0; ! retval && i < c.numel (); i++)
	{
	  cdef_class cls = lookup_class (c(i).string_value ());

	  if (! error_state)
	    retval = is_superclass (clsa, cls, true);
	}

      return retval;
    }
}

inline bool
is_strict_superclass (const cdef_class& clsa, const cdef_class& clsb)
{ return is_superclass (clsa, clsb, false); }

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

      retval(0) = lookup_classes (classes);
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

      retval(0) = lookup_classes (classes);
    }

  return retval;
}

static octave_value_list
class_fromName (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      std::string name = args(0).string_value ();

      if (! error_state)
	retval(0) = to_ov (lookup_class (name));
      else
	error ("fromName: invalid class name, expected a string value");
    }
  else
    error ("fromName: invalid number of parameters");

  return retval;
}

static octave_value_list
class_fevalStatic (const octave_value_list& args, int nargout)
{
  octave_value_list retval;

  if (args.length () > 1 && args(0).type_name () == "object")
    {
      cdef_class cls (to_cdef (args(0)));

      if (! error_state)
	{
	  std::string meth_name = args(1).string_value ();

	  if (! error_state)
	    {
	      cdef_method meth = cls.find_method (meth_name);

	      if (meth.ok ())
		{
		  // FIXME: can the context be something else?
		  if (meth.check_access ("public"))
		    {
		      if (meth.is_static ())
			retval = meth.execute (args.splice (0, 2), nargout);
		      else
			error ("fevalStatic: method `%s' is not static",
			       meth_name.c_str ());
		    }
		  else
		    gripe_method_access ("fevalStatic", meth);
		}
	      else
		error ("fevalStatic: method not found: %s",
		       meth_name.c_str ());
	    }
	  else
	    error ("fevalStatic: invalid method name, expected a string value");
	}
      error ("fevalStatic: invalid object, expected a meta.class object");
    }
  else
    error ("fevalStatic: invalid arguments");

  return retval;
}

static octave_value_list
class_getConstant (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () == 2 && args(0).type_name () == "object"
      && args(0).class_name () == "meta.class")
    {
      cdef_class cls = to_cdef (args(0));

      if (! error_state)
	{
	  std::string prop_name = args(1).string_value ();

	  if (! error_state)
	    {
	      cdef_property prop = cls.find_property (prop_name);

	      if (prop.ok ())
		{
		  // FIXME: can the context be something else?
		  if (prop.check_get_access ("public"))
		    {
		      if (prop.is_constant ())
			retval(0) = prop.get_value ();
		      else
			error ("getConstant: property `%s' is not constant",
			       prop_name.c_str ());
		    }
		  else
		    gripe_property_access ("getConstant", prop);
		}
	      else
		error ("getConstant: property not found: %s",
		       prop_name.c_str ());
	    }
	  else
	    error ("getConstant: invalid property name, expected a string value");
	}
      else
	error ("getConstant: invalid object, expected a meta.class object");
    }
  else
    error ("getConstant: invalid arguments");

  return retval;
}

#define META_CLASS_CMP(OP, CLSA, CLSB, FUN) \
static octave_value_list \
class_ ## OP (const octave_value_list& args, int /* nargout */) \
{ \
  octave_value_list retval; \
\
  if (args.length () == 2 \
      && args(0).type_name () == "object" && args(1).type_name () == "object" \
      && args(0).class_name () == "meta.class" && args(1).class_name () == "meta.class") \
    { \
      cdef_class clsa = to_cdef (args(0)); \
\
      cdef_class clsb = to_cdef (args(1)); \
\
      if (! error_state) \
	retval(0) = FUN (CLSA, CLSB); \
      else \
	error (#OP ": invalid objects, expected meta.class objects"); \
    } \
  else \
    error (#OP ": invalid arguments"); \
\
  return retval; \
}

META_CLASS_CMP (lt, clsb, clsa, is_strict_superclass)
META_CLASS_CMP (le, clsb, clsa, is_superclass)
META_CLASS_CMP (gt, clsa, clsb, is_strict_superclass)
META_CLASS_CMP (ge, clsa, clsb, is_superclass)
META_CLASS_CMP (eq, clsa, clsb, operator==)
META_CLASS_CMP (ne, clsa, clsb, operator!=)

static octave_value_list
handle_delete (const octave_value_list& /* args */, int /* nargout */)
{
  octave_value_list retval;

  // FIXME: implement this

  return retval;
}

static cdef_class
make_class (const std::string& name, const std::string& super = std::string())
{
  cdef_class cls ("meta.class");

  all_classes[name] = cls;
  cls.put ("ConstructOnLoad", false);
  cls.put ("ContainingPackage", Matrix ());
  cls.put ("Description", std::string ());
  cls.put ("DetailedDescription", std::string ());
  cls.put ("Events", Cell ());
  cls.put ("Hidden", false);
  cls.put ("InferiorClasses", Cell ());
  cls.put ("Methods", Cell ());
  cls.put ("Name", name);
  cls.put ("Properties", Cell ());
  cls.put ("Sealed", true);
  if (super.empty ())
    cls.put ("SuperClasses", Cell ());
  else
    cls.put ("SuperClasses", Cell (octave_value (super)));

  return cls;
}

static cdef_property
make_property (const cdef_object& cls, const std::string& name,
	       const octave_value& get_method = Matrix (),
	       const std::string& get_access = "public",
	       const octave_value& set_method = Matrix (),
	       const std::string& set_access = "public")
{
  // FIXME: what about default value?

  cdef_property prop ("meta.property");

  prop.put ("Name", name);
  prop.put ("Description", std::string ());
  prop.put ("DetailedDescription", std::string ());
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

  return prop;
}

inline cdef_property
make_attribute (const cdef_object& cls, const std::string& name)
{
  return make_property (cls, name, Matrix (), "public", Matrix (), "private");
}

static cdef_method
make_method (const cdef_object& cls, const std::string& name, const octave_value& fcn,
	     const std::string& m_access = "public", bool is_static = false)
{
  cdef_method meth ("meta.method");

  meth.put ("Abstract", false);
  meth.put ("Access", m_access);
  meth.put ("DefiningClass", to_ov (cls));
  meth.put ("Description", std::string ());
  meth.put ("DetailedDescription", std::string ());
  meth.put ("Hidden", false);
  meth.put ("Name", name);
  meth.put ("Sealed", true);
  meth.put ("Static", is_static);

  meth.set_function (fcn);

  return meth;
}

inline cdef_method
make_method (const cdef_object& cls, const std::string& name, octave_builtin::fcn ff,
	     const std::string& m_access = "public", bool is_static = false)
{
  octave_value fcn (new octave_builtin (ff, name));

  octave_value fcn_handle (new octave_fcn_handle (fcn, name));

  return make_method (cls, name, fcn_handle, m_access, is_static);
}

static cdef_package
make_package (const std::string& nm,
              const std::string& parent = std::string ())
{
  cdef_package pack ("meta.package");

  all_packages[nm] = pack;
  pack.put ("Name", nm);
  pack.put ("ContainingPackage", to_ov (all_packages[parent]));

  return pack;
}

DEFINE_OCTAVE_ALLOCATOR (octave_classdef);

int octave_classdef::t_id (-1);

const std::string octave_classdef::t_name ("object");

void
octave_classdef::register_type (void)
{
  t_id = octave_value_typeinfo::register_type
    (octave_classdef::t_name, "<unknown>", octave_value (new octave_classdef ()));
}

cdef_class
cdef_object_rep::get_class (void) const
{
  cdef_class cls = lookup_class (class_name ());

  return cls;
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
handle_cdef_object::subsref (const std::string& type,
			     const std::list<octave_value_list>& idx,
			     int nargout, int& skip)
{
  skip = 0;

  cdef_class cls = get_class ();

  octave_value_list retval;

  if (! cls.ok ())
    return retval;

  switch (type[0])
    {
    case '.':
	{
	  std::string name = (idx.front ())(0).string_value ();

	  // FIXME: get the right context; context should also
	  // be linked to the current executing class (if any)
	  // such that protected/private methods found in inherited
	  // classes are correctly resolved.
	  std::string context = "public";

	  cdef_method meth = cls.find_method (name);

	  if (meth.ok ())
	    {
	      if (meth.check_access (context))
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
		    retval = meth.execute (args, _nargout);
		  else
		    {
		      refcount++;
		      retval = meth.execute (cdef_object (this), args, _nargout);
		    }
		}
	      else
		gripe_method_access ("subsref", meth);
	    }

	  if (skip == 0 && ! error_state)
	    {
	      cdef_property prop = cls.find_property (name);

	      if (prop.ok ())
		{
		  if (prop.check_get_access (context))
		    {
		      refcount++;
		      retval(0) = prop.get_value (cdef_object (this));

		      skip = 1;
		    }
		  else
		    gripe_property_access ("subsref", prop);
		}
	      else
		error ("subsref: unknown method or property: %s", name.c_str ());
	    }
	  break;
	}
    default:
      error ("object cannot be indexed with `%c'", type[0]);
      break;
    }

  return retval;
}

cdef_method
cdef_class::cdef_class_rep::find_method (const std::string& nm)
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

  // Look into superclasses

  Cell super_classes = get ("SuperClasses").cell_value ();

  for (int i = 0; i < super_classes.numel (); i++)
    {
      cdef_class cls = lookup_class (super_classes(i).string_value ());

      if (! error_state)
	{
	  cdef_method meth = cls.find_method (nm);

	  if (meth.ok ())
	    return meth;
	}
    }

  return cdef_method ();
}

void
cdef_class::cdef_class_rep::install_method (const cdef_method& meth)
{
  method_map[meth.get_name ()] = meth;
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

  std::map<std::string,int> count;

  count["public"] = count["protected"] = count["private"] = 0;

  find_methods (meths, count);

  if (! error_state)
    {
      Cell c (count["public"] + count["protected"], 1);

      int idx = 0;

      for (std::map<std::string,cdef_method>::const_iterator it = meths.begin ();
	   it != meths.end (); ++it)
	if (::check_access ("protected", it->second.get_access ()))
	  c (idx++, 0) = to_ov (it->second);

      return c;
    }

  return Cell ();
}

void
cdef_class::cdef_class_rep::find_methods (std::map<std::string,cdef_method>& meths,
					  std::map<std::string,int>& count)
{
  load_all_methods ();

  method_const_iterator it;

  for (it = method_map.begin (); it != method_map.end (); ++it)
    {
      std::string nm = it->second.get_name ();

      if (meths.find (nm) == meths.end ())
	{
	  std::string acc = it->second.get_access ();

	  meths[nm] = it->second;
	  count[acc]++;
	}
    }

  // Look into superclasses

  Cell super_classes = get ("SuperClasses").cell_value ();

  for (int i = 0; i < super_classes.numel (); i++)
    {
      cdef_class cls = lookup_class (super_classes(i).string_value ());

      if (! error_state)
	cls.get_rep ()->find_methods (meths, count);
      else
	break;
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
      cdef_class cls = lookup_class (super_classes(i).string_value ());

      if (! error_state)
	{
	  cdef_property prop = cls.find_property (nm);

	  if (prop.ok ())
	    return prop;
	}
    }

  return cdef_property ();
}

void
cdef_class::cdef_class_rep::install_property (const cdef_property& prop)
{
  property_map[prop.get_name ()] = prop;
}

Cell
cdef_class::cdef_class_rep::get_properties (void)
{
  std::map<std::string,cdef_property> props;

  std::map<std::string,int> count;

  count["public"] = count["protected"] = count["private"] = 0;

  find_properties (props, count);

  if (! error_state)
    {
      Cell c (count["public"] + count["protected"], 1);

      int idx = 0;

      for (std::map<std::string,cdef_property>::const_iterator it = props.begin ();
	   it != props.end (); ++it)
	if (::check_access ("protected", it->second.get_get_access ()))
	  c (idx++, 0) = to_ov (it->second);

      return c;
    }

  return Cell ();
}

void
cdef_class::cdef_class_rep::find_properties (std::map<std::string,cdef_property>& props,
					     std::map<std::string,int>& count)
{
  property_const_iterator it;

  for (it = property_map.begin (); it != property_map.end (); ++it)
    {
      std::string nm = it->second.get_name ();

      if (props.find (nm) == props.end ())
	{
	  std::string acc = it->second.get_get_access ();

	  props[nm] = it->second;
	  count[acc]++;
	}
    }

  // Look into superclasses

  Cell super_classes = get ("SuperClasses").cell_value ();

  for (int i = 0; i < super_classes.numel (); i++)
    {
      cdef_class cls = lookup_class (super_classes(i).string_value ());

      if (! error_state)
	cls.get_rep ()->find_properties (props, count);
      else
	break;
    }
}

void
cdef_class::cdef_class_rep::find_names (std::map<std::string,std::string>& names,
					std::map<std::string,int>& count)
{
  load_all_methods ();

  for (method_const_iterator it = method_map.begin ();
       it != method_map.end(); ++it)
    {
      std::string nm = it->second.get_name ();

      if (names.find (nm) == names.end ())
	{
	  std::string acc = it->second.get_access ();

	  names[nm] = acc;
	  count[acc]++;
	}
    }

  for (property_const_iterator it = property_map.begin ();
       it != property_map.end (); ++it)
    {
      std::string nm = it->second.get_name ();

      if (names.find (nm) == names.end ())
	{
	  std::string acc = it->second.get_get_access ();

	  names[nm] = acc;
	  count[acc]++;
	}
    }

  // Look into superclasses

  Cell super_classes = get ("SuperClasses").cell_value ();

  for (int i = 0; i < super_classes.numel (); i++)
    {
      cdef_class cls = lookup_class (super_classes(i).string_value ());

      if (! error_state)
	cls.get_rep ()->find_names (names, count);
      else
	break;
    }
}

string_vector
cdef_class::cdef_class_rep::get_names (void)
{
  std::map<std::string,std::string> names;

  std::map<std::string,int> count;

  count["public"] = count["protected"] = count["private"] = 0;

  find_names (names, count);

  if (! error_state)
    {
      string_vector v (count["public"]);

      int idx = 0;
      for (std::map<std::string,std::string>::const_iterator it = names.begin ();
	   it != names.end (); ++it)
	{
	  if (it->second == "public")
	      v[idx++] = it->first;
	}

      return v.sort (true);
    }

  return string_vector ();
}

void
cdef_class::cdef_class_rep::delete_object (cdef_object obj)
{
  method_iterator it = method_map.find ("delete");

  if (it != method_map.end ())
    {
      std::string cls_name = obj.class_name ();

      obj.set_class_name (get ("Name").string_value ());

      it->second.execute (obj, octave_value_list (), 0);

      obj.set_class_name (cls_name);
    }

  // FIXME: should we destroy corresponding properties here?

  // Call "delete" in super classes

  Cell super_classes = get ("SuperClasses").cell_value ();

  for (int i = 0; i < super_classes.numel (); i++)
    {
      cdef_class cls = lookup_class (super_classes(i).string_value ());

      if (!error_state)
	cls.delete_object (obj);
    }
}

octave_value
cdef_property::cdef_property_rep::get_value (const cdef_object& obj)
{
  // FIXME: should check whether we're already in get accessor method

  octave_value retval;
 
  octave_value get_fcn = get ("GetMethod");

  std::string get_access = get ("GetAccess").string_value ();

  if (get_access != "public")
    {
      // FIXME: should check the current call stack
    }

  if (get_fcn.is_empty ())
    retval = obj.get (get ("Name").string_value ());
  else
    {
      octave_value_list args;

      args(0) = to_ov (obj);
      
      args = execute_ov (get_fcn, args, 1);

      if (! error_state)
	retval = args(0);
    }

  return retval;
}

bool
cdef_property::check_get_access (const std::string& req) const
{
  return ::check_access (req, get_get_access ());
}

bool
cdef_property::check_set_access (const std::string& req) const
{
  return ::check_access (req, get_set_access ());
}

void
cdef_method::cdef_method_rep::check_method (void)
{
  // FIXME: check whether re-load is needed
}

octave_value_list
cdef_method::cdef_method_rep::execute (const octave_value_list& args,
				       int nargout)
{
  octave_value_list retval;

  if (! get ("Abstract").bool_value ())
    {
      check_method ();

      if (function.is_defined ())
	{
	  retval = execute_ov (function, args, nargout);
	}
    }
  else
    error ("%s: cannot execute abstract method",
	   get ("Name").string_value ().c_str ());

  return retval;
}

octave_value_list
cdef_method::cdef_method_rep::execute (const cdef_object& obj,
				       const octave_value_list& args,
				       int nargout)
{
  octave_value_list retval;

  if (! get ("Abstract").bool_value ())
    {
      check_method ();

      octave_value_list new_args;

      if (function.is_defined ())
	{
	  new_args.resize (args.length () + 1);

	  new_args(0) = to_ov (obj);
	  for (int i = 0; i < args.length (); i++)
	    new_args(i+1) = args(i);

	  retval = execute_ov (function, new_args, nargout);
	}
    }
  else
    error ("%s: cannot execute abstract method",
	   get ("Name").string_value ().c_str ());

  return retval;
}

bool
cdef_method::check_access (const std::string& req) const
{
  return ::check_access (req, get_access ());
}

static cdef_package
lookup_package (const std::string& name)
{
  std::map<std::string, cdef_package>::const_iterator it = all_packages.find (name);

  if (it != all_packages.end ())
    {
      cdef_package pack = it->second;

      if (pack.ok ())
        return pack;
      else
        error ("invalid package: %s", name.c_str ());
    }
  else
    error ("package not found: %s", name.c_str ());

  return cdef_package ();
}

static octave_value_list
package_fromName (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      std::string name = args(0).string_value ();

      if (! error_state)
	retval(0) = to_ov (lookup_package (name));
      else
	error ("fromName: invalid package name, expected a string value");
    }
  else
    error ("fromName: invalid number of parameters");

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

void
cdef_package::cdef_package_rep::install_class (const cdef_class& cls,
                                               const std::string& nm)
{
  class_map[nm] = cls;
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
}

octave_value_list
cdef_package::cdef_package_rep::subsref (const std::string& type,
                                         const std::list<octave_value_list>& idx,
                                         int nargout, int& skip)
{
  return handle_cdef_object::subsref (type, idx, nargout, skip);
}

template<class T1, class T2>
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

octave_value_list
octave_classdef::subsref (const std::string& type,
			const std::list<octave_value_list>& idx,
			int nargout)
{
  int skip = 0;
  octave_value_list retval;

  // FIXME: should check "subsref" method first

  retval = object.subsref (type, idx, nargout, skip);

  if (! error_state)
    {
      if (type.length () > skip && idx.size () > skip)
	retval = retval(0).next_subsref (nargout, type, idx, skip);
    }

  return retval;
}

void
install_classdef (void)
{
  octave_classdef::register_type ();

  /* meta classes */
  cdef_class handle = make_class ("handle");
  cdef_class meta_class = make_class ("meta.class", "handle");
  cdef_class meta_property = make_class ("meta.property", "handle");
  cdef_class meta_method = make_class ("meta.method", "handle");
  cdef_class meta_event = make_class ("meta.event", "handle");
  cdef_class meta_package = make_class ("meta.package", "handle");
  cdef_class meta_dynproperty = make_class ("meta.dynamicproperty", "handle");

  /* meta.class properties */
  meta_class.install_property (make_attribute (meta_class, "ConstructOnLoad"));
  meta_class.install_property (make_property  (meta_class, "ContainingPackage"));
  meta_class.install_property (make_property  (meta_class, "Description"));
  meta_class.install_property (make_property  (meta_class, "DetailedDescription"));
  meta_class.install_property (make_property  (meta_class, "Events"));
  meta_class.install_property (make_attribute (meta_class, "Hidden"));
  meta_class.install_property
      (make_property (meta_class, "InferiorClasses",
		      make_fcn_handle (class_get_inferiorclasses, "meta.class>get.InferiorClasses"),
		      "public", Matrix (), "private"));
  meta_class.install_property
      (make_property  (meta_class, "Methods",
		       make_fcn_handle (class_get_methods, "meta.class>get.Methods"),
		       "public", Matrix (), "private"));
  meta_class.install_property
      (make_property  (meta_class, "MethodList",
		       make_fcn_handle (class_get_methods, "meta.class>get.MethodList"),
		       "public", Matrix (), "private"));
  meta_class.install_property (make_attribute (meta_class, "Name"));
  meta_class.install_property
      (make_property  (meta_class, "Properties",
		       make_fcn_handle (class_get_properties, "meta.class>get.Properties"),
		       "public", Matrix (), "private"));
  meta_class.install_property
      (make_property  (meta_class, "PropertyList",
		       make_fcn_handle (class_get_properties, "meta.class>get.PropertyList"),
		       "public", Matrix (), "private"));
  meta_class.install_property (make_attribute (meta_class, "Sealed"));
  meta_class.install_property
      (make_property (meta_class, "SuperClasses",
		      make_fcn_handle (class_get_superclasses, "meta.class>get.SuperClasses"),
		      "public", Matrix (), "private"));
  meta_class.install_property
      (make_property (meta_class, "SuperClassList",
		      make_fcn_handle (class_get_superclasses, "meta.class>get.SuperClassList"),
		      "public", Matrix (), "private"));
  /* meta.class methods */
  meta_class.install_method (make_method (meta_class, "fromName", class_fromName,
					  "public", true));
  meta_class.install_method (make_method (meta_class, "fevalStatic", class_fevalStatic,
					  "public", false));
  meta_class.install_method (make_method (meta_class, "getConstant", class_getConstant,
					  "public", false));
  meta_class.install_method (make_method (meta_class, "eq", class_eq));
  meta_class.install_method (make_method (meta_class, "ne", class_ne));
  meta_class.install_method (make_method (meta_class, "lt", class_lt));
  meta_class.install_method (make_method (meta_class, "le", class_le));
  meta_class.install_method (make_method (meta_class, "gt", class_gt));
  meta_class.install_method (make_method (meta_class, "ge", class_ge));

  /* meta.method properties */
  meta_method.install_property (make_attribute (meta_method, "Abstract"));
  meta_method.install_property (make_attribute (meta_method, "Access"));
  meta_method.install_property (make_attribute (meta_method, "DefiningClass"));
  meta_method.install_property (make_attribute (meta_method, "Description"));
  meta_method.install_property (make_attribute (meta_method, "DetailedDescription"));
  meta_method.install_property (make_attribute (meta_method, "Hidden"));
  meta_method.install_property (make_attribute (meta_method, "Name"));
  meta_method.install_property (make_attribute (meta_method, "Sealed"));
  meta_method.install_property (make_attribute (meta_method, "Static"));

  /* meta.property properties */
  meta_property.install_property (make_attribute (meta_property, "Name"));
  meta_property.install_property (make_attribute (meta_property, "Description"));
  meta_property.install_property (make_attribute (meta_property, "DetailedDescription"));
  meta_property.install_property (make_attribute (meta_property, "Abstract"));
  meta_property.install_property (make_attribute (meta_property, "Constant"));
  meta_property.install_property (make_attribute (meta_property, "GetAccess"));
  meta_property.install_property (make_attribute (meta_property, "SetAccess"));
  meta_property.install_property (make_attribute (meta_property, "Dependent"));
  meta_property.install_property (make_attribute (meta_property, "Transient"));
  meta_property.install_property (make_attribute (meta_property, "Hidden"));
  meta_property.install_property (make_attribute (meta_property, "GetObservable"));
  meta_property.install_property (make_attribute (meta_property, "SetObservable"));
  meta_property.install_property (make_attribute (meta_property, "GetMethod"));
  meta_property.install_property (make_attribute (meta_property, "SetMethod"));
  meta_property.install_property (make_attribute (meta_property, "DefiningClass"));
  /* meta.property events */
  // FIXME: add events

  /* handle methods */
  handle.install_method (make_method (handle, "delete", handle_delete));

  /* meta.package properties */
  meta_package.install_property (make_attribute (meta_package, "Name"));
  meta_package.install_property (make_property  (meta_package, "ContainingPackage"));
  meta_package.install_property
      (make_property (meta_package, "ClassList",
		      make_fcn_handle (package_get_classes, "meta.package>get.ClassList"),
		      "public", Matrix (), "private"));
  meta_package.install_property
      (make_property (meta_package, "Classes",
		      make_fcn_handle (package_get_classes, "meta.package>get.Classes"),
		      "public", Matrix (), "private"));
  meta_package.install_property
      (make_property (meta_package, "FunctionList",
		      make_fcn_handle (package_get_functions, "meta.package>get.FunctionList"),
		      "public", Matrix (), "private"));
  meta_package.install_property
      (make_property (meta_package, "Functions",
		      make_fcn_handle (package_get_functions, "meta.package>get.Functions"),
		      "public", Matrix (), "private"));
  meta_package.install_property
      (make_property (meta_package, "PackageList",
		      make_fcn_handle (package_get_packages, "meta.package>get.PackageList"),
		      "public", Matrix (), "private"));
  meta_package.install_property
      (make_property (meta_package, "Packages",
		      make_fcn_handle (package_get_packages, "meta.package>get.Packages"),
		      "public", Matrix (), "private"));
  meta_package.install_method (make_method (meta_package, "fromName", package_fromName,
                                            "public", true));

  /* create "meta" package */
  cdef_package package_meta = make_package ("meta");
  package_meta.install_class (meta_class,       "class");
  package_meta.install_class (meta_property,    "property");
  package_meta.install_class (meta_method,      "method");
  package_meta.install_class (meta_package,     "package");
  package_meta.install_class (meta_event,       "event");
  package_meta.install_class (meta_dynproperty, "dynproperty");
}

DEFUN (__meta_get_class__, args, , "")
{
  octave_value retval;

  if (args.length () == 1)
    {
      std::string cname = args(0).string_value ();

      if (! error_state)
	retval = to_ov (lookup_class (cname));
      else
	error ("invalid class name, expected a string value");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (__meta_get_package__, args, , "")
{
  octave_value retval;

  if (args.length () == 1)
    {
      std::string cname = args(0).string_value ();

      if (! error_state)
	retval = to_ov (lookup_package (cname));
      else
	error ("invalid package name, expected a string value");
    }
  else
    print_usage ();

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
