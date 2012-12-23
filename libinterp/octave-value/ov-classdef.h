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

#if !defined (octave_classdef_h)
#define octave_classdef_h 1

#include <string>

#include "oct-map.h"
#include "oct-refcount.h"
#include "ov-base.h"

class cdef_object;
class cdef_class;
class cdef_property;
class cdef_method;
class cdef_package;

class tree_classdef;

class
cdef_object_rep
{
public:
  friend class cdef_object;

public:
  cdef_object_rep (void)
      : refcount (1), cname () { }

  cdef_object_rep (const std::string& nm)
      : refcount (1), cname (nm) { }

  virtual ~cdef_object_rep (void) { }

  virtual cdef_class get_class (void) const;

  virtual void set_class (const cdef_object&)
    { error ("set_class: invalid object"); }

  virtual cdef_object_rep* clone (void) const
    {
      error ("clone: invalid object");
      return new cdef_object_rep ();
    }

  virtual void put (const std::string&, const octave_value&)
    { error ("put: invalid object"); }

  virtual octave_value get (const std::string&) const
    {
      error ("get: invalid object");
      return octave_value ();
    }

  virtual octave_value_list subsref (const std::string&,
				     const std::list<octave_value_list>&,
				     int, int&)
    {
      error ("subsref: invalid object");
      return octave_value_list ();
    }

  virtual string_vector map_keys(void) const;

  virtual bool is_valid (void) const { return false; }

  std::string class_name (void) const { return cname; }

  void set_class_name (const std::string& nm)
    { cname = nm; }

protected:
  /* reference count */
  octave_refcount<int> refcount;

  /* class name */
  std::string cname;
};

class
cdef_object
{
public:
  /* FIXME: use a null object */
  cdef_object (void)
      : rep (new cdef_object_rep ()) { }

  cdef_object (const cdef_object& obj)
    : rep (obj.rep)
    {
      rep->refcount++;
    }

  cdef_object (cdef_object_rep *r)
      : rep (r) { }

  virtual ~cdef_object (void)
    {
      if (--rep->refcount == 0)
	delete rep;
    }

  cdef_object& operator = (const cdef_object& obj)
    {
      if (rep != obj.rep)
	{
	  if (--rep->refcount == 0)
	    delete rep;

	  rep = obj.rep;
	  rep->refcount++;
	}

      return *this;
    }

  cdef_class get_class (void) const;

  void set_class_name (const std::string& nm)
    { rep->set_class_name (nm); }

  std::string class_name (void) const
    { return rep->class_name (); }

  cdef_object clone (void) const
    { return cdef_object (rep->clone ()); }

  void put (const std::string& pname, const octave_value& val)
    { rep->put (pname, val); }

  octave_value get (const std::string& pname) const
    { return rep->get (pname); }

  octave_value_list subsref (const std::string& type,
			     const std::list<octave_value_list>& idx,
			     int nargout, int& skip)
    { return rep->subsref (type, idx, nargout, skip); }

  string_vector map_keys (void) const { return rep->map_keys (); }

  const cdef_object_rep* get_rep (void) const { return rep; }

  bool ok (void) const { return rep->is_valid (); }

protected:
  cdef_object_rep* get_rep (void) { return rep; }

private:
  cdef_object_rep *rep;
};

class
handle_cdef_object : public cdef_object_rep
{
public:
  handle_cdef_object (void)
      : cdef_object_rep () { }

  handle_cdef_object (const std::string& nm)
      : cdef_object_rep (nm) { }

  cdef_object_rep* clone (void) const
    {
      handle_cdef_object *obj = const_cast<handle_cdef_object *> (this);
      obj->refcount++;
      return obj;
    }

  void put (const std::string& pname, const octave_value& val)
    { map.assign (pname, val); }

  octave_value get (const std::string& pname) const
    {
      Cell val = map.contents (pname);

      if (val.numel () > 0)
	return val(0, 0);
      else
	{
	  error ("get: unknown slot: %s", pname.c_str ());
	  return octave_value ();
	}
    }

  octave_value_list subsref (const std::string& type,
			     const std::list<octave_value_list>& idx,
			     int nargout, int& skip);

  bool is_valid (void) const { return true; }

protected:
  Octave_map map;
};

class
cdef_class : public cdef_object
{
private:

  class
  cdef_class_rep : public handle_cdef_object
  {
  public:
    cdef_class_rep (const std::string& nm)
	: handle_cdef_object (nm) { }

    cdef_method find_method (const std::string& nm);

    void install_method (const cdef_method& meth);

    Cell get_methods (void);

    cdef_property find_property (const std::string& nm);

    void install_property (const cdef_property& prop);

    Cell get_properties (void);

    string_vector get_names (void);

    void set_directory (const std::string& dir) { directory = dir; }

    std::string get_directory (void) const { return directory; }

    void delete_object (cdef_object obj);

  private:
    void load_all_methods (void);

    void find_names (std::map<std::string,std::string>& names,
		     std::map<std::string,int>& count);
    
    void find_properties (std::map<std::string,cdef_property>& props,
			  std::map<std::string,int>& count);
    
    void find_methods (std::map<std::string,cdef_method>& meths,
		       std::map<std::string,int>& count);

  private:
    std::string directory;

    std::map<std::string,cdef_method> method_map;

    std::map<std::string,cdef_property> property_map;

    typedef std::map<std::string,cdef_method>::iterator method_iterator;
    typedef std::map<std::string,cdef_method>::const_iterator method_const_iterator;
    typedef std::map<std::string,cdef_property>::iterator property_iterator;
    typedef std::map<std::string,cdef_property>::const_iterator property_const_iterator;
  };

public:
  // Create and invalid class object
  cdef_class (void)
      : cdef_object () { }

  cdef_class (const std::string& nm)
      : cdef_object (new cdef_class_rep (nm)) { }

  cdef_class (const cdef_class& cls)
      : cdef_object (cls) { }

  cdef_class (const cdef_object& obj)
      : cdef_object (obj)
    {
      // This should never happen...
      if (class_name () != "meta.class")
	error ("internal error: invalid assignment from %s to meta.class object",
	       class_name ().c_str ());
    }

  cdef_class& operator = (const cdef_class& cls)
    {
      cdef_object::operator= (cls);

      return *this;
    }

  cdef_class& operator = (const cdef_object& obj)
    {
      if (obj.class_name () == "meta.class")
	cdef_object::operator= (obj);
      else
	error ("internal error: invalid assignment from %s to meta.class object",
	       class_name ().c_str ());

      return *this;
    }

  cdef_method find_method (const std::string& nm);

  void install_method (const cdef_method& meth)
    { get_rep ()->install_method (meth); }

  Cell get_methods (void) { return get_rep ()->get_methods (); }

  cdef_property find_property (const std::string& nm);
  
  void install_property (const cdef_property& prop)
    { get_rep ()->install_property (prop); }

  Cell get_properties (void) { return get_rep ()->get_properties (); }

  string_vector get_names (void) { return get_rep ()->get_names (); }

  void set_directory (const std::string& dir)
    { get_rep ()->set_directory (dir); }

  std::string get_directory (void) const
    { return get_rep ()->get_directory (); }

  std::string get_name (void) const
    { return get ("Name").string_value (); }

  bool is_builtin (void) const
    { return get_directory ().empty (); }

  void delete_object (cdef_object obj)
    { get_rep ()->delete_object (obj); }

  static cdef_class make_meta_class (const tree_classdef* t);

private:
  cdef_class_rep* get_rep (void)
    { return dynamic_cast<cdef_class_rep *> (cdef_object::get_rep ()); }
  
  const cdef_class_rep* get_rep (void) const
    { return dynamic_cast<const cdef_class_rep *> (cdef_object::get_rep ()); }

  friend bool operator == (const cdef_class&, const cdef_class&);
  friend bool operator != (const cdef_class&, const cdef_class&);
};

inline bool
operator == (const cdef_class& clsa, const cdef_class& clsb)
// FIXME: is this really the right way to check class equality?
{ return (clsa.get_rep () == clsb.get_rep ()); }

inline bool
operator != (const cdef_class& clsa, const cdef_class& clsb)
{ return ! (clsa == clsb); }

class
cdef_property : public cdef_object
{
private:

  class
  cdef_property_rep : public handle_cdef_object
  {
  public:
    cdef_property_rep (const std::string& nm)
	: handle_cdef_object (nm) { }

    octave_value get_value (void) const { return default_value; }

    octave_value get_value (const cdef_object& obj);

    void set_value (const octave_value& val) { default_value = val; }

    void set_value (const cdef_object& obj, const octave_value& val);

  private:
    octave_value default_value;
  };

public:
  cdef_property (void) : cdef_object () { }

  cdef_property (const std::string& nm)
      : cdef_object (new cdef_property_rep (nm)) { }

  cdef_property (const cdef_property& prop)
      : cdef_object (prop) { }

  cdef_property (const cdef_object& obj)
      : cdef_object (obj)
    {
      // This should never happen...
      if (class_name () != "meta.property")
	error ("internal error: invalid assignment from %s to meta.property object",
	       class_name ().c_str ());
    }

  cdef_property& operator = (const cdef_property& prop)
    {
      cdef_object::operator= (prop);

      return *this;
    }

  octave_value get_value (const cdef_object& obj)
    { return get_rep ()->get_value (obj); }

  octave_value get_value (void) { return get_rep ()->get_value (); }

  void set_value (const cdef_object& obj, const octave_value& val)
    { get_rep ()->set_value (obj, val); }

  void set_value (const octave_value& val) { get_rep ()->set_value (val); }
 
  std::string get_get_access (void) const
    { return get ("GetAccess").string_value (); }
  
  std::string get_set_access (void) const
    { return get ("SetAccess").string_value (); }

  bool check_get_access (const std::string& acc) const;
  
  bool check_set_access (const std::string& acc) const;

  std::string get_name (void) const
    { return get ("Name").string_value (); }

  bool is_constant (void) const
    { return get ("Constant").bool_value (); }

private:
  cdef_property_rep* get_rep (void)
    { return dynamic_cast<cdef_property_rep *> (cdef_object::get_rep ()); }
  
  const cdef_property_rep* get_rep (void) const
    { return dynamic_cast<const cdef_property_rep *> (cdef_object::get_rep ()); }
};

class
cdef_method : public cdef_object
{
private:

  class
  cdef_method_rep : public handle_cdef_object
  {
  public:
    cdef_method_rep (const std::string& nm)
	: handle_cdef_object (nm) { }

    octave_value get_function (void) const { return function; }

    void set_function (const octave_value& fcn)
      { function = fcn; }

    octave_value_list execute (const octave_value_list& args, int nargout);

    octave_value_list execute (const cdef_object& obj,
			       const octave_value_list& args, int nargout);

  private:
    void check_method (void);

  private:
    octave_value function;
  };

public:
  cdef_method (void) : cdef_object () { }

  cdef_method (const std::string& nm)
      : cdef_object (new cdef_method_rep (nm)) { }

  cdef_method (const cdef_property& prop)
      : cdef_object (prop) { }

  cdef_method (const cdef_object& obj)
      : cdef_object (obj)
    {
      // This should never happen...
      if (class_name () != "meta.method")
	error ("internal error: invalid assignment from %s to meta.method object",
	       class_name ().c_str ());
    }

  cdef_method& operator = (const cdef_method& meth)
    {
      cdef_object::operator= (meth);

      return *this;
    }

  /* normal invokation */
  octave_value_list execute (const octave_value_list& args, int nargout)
    { return get_rep ()->execute (args, nargout); }

  /* dot-invokation: object is pushed as 1st argument */
  octave_value_list execute (const cdef_object& obj,
			     const octave_value_list& args, int nargout)
    { return get_rep ()->execute (obj, args, nargout); }

  std::string get_access (void) const
    { return get ("Access").string_value (); }

  bool check_access (const std::string& req) const;
  
  std::string get_name (void) const
    { return get ("Name").string_value (); }

  bool is_static (void) const
    { return get ("Static").bool_value (); }

  void set_function (const octave_value& fcn)
    { get_rep ()->set_function (fcn); }

private:
  cdef_method_rep* get_rep (void)
    { return dynamic_cast<cdef_method_rep *> (cdef_object::get_rep ()); }
  
  const cdef_method_rep* get_rep (void) const
    { return dynamic_cast<const cdef_method_rep *> (cdef_object::get_rep ()); }
};

inline cdef_class
cdef_object::get_class (void) const
{ return rep->get_class (); }

inline cdef_method
cdef_class::find_method (const std::string& nm)
{ return get_rep ()->find_method (nm); }

inline cdef_property
cdef_class::find_property (const std::string& nm)
{ return get_rep ()->find_property (nm); }

class
cdef_package : public cdef_object
{
private:

  class
  cdef_package_rep : public handle_cdef_object
  {
  public:
    cdef_package_rep (const std::string& nm)
      : handle_cdef_object (nm) { }

    void install_class (const cdef_class& cls, const std::string& nm);

    void install_function (const octave_value& fcn, const std::string& nm);

    void install_package (const cdef_package& pack, const std::string& nm);

    octave_value_list subsref (const std::string& type,
                               const std::list<octave_value_list>& idx,
                               int nargout, int& skip);

    Cell get_classes (void) const;

    Cell get_functions (void) const;

    Cell get_packages (void) const;

  private:
    std::map<std::string, cdef_class> class_map;
    std::map<std::string, octave_value> function_map;
    std::map<std::string, cdef_package> package_map;

    typedef std::map<std::string, cdef_class>::iterator class_iterator;
    typedef std::map<std::string, cdef_class>::const_iterator class_const_iterator;
    typedef std::map<std::string, octave_value>::iterator function_iterator;
    typedef std::map<std::string, octave_value>::const_iterator function_const_iterator;
    typedef std::map<std::string, cdef_package>::iterator package_iterator;
    typedef std::map<std::string, cdef_package>::const_iterator package_const_iterator;
  };

public:
  cdef_package (void) : cdef_object () { }

  cdef_package (const std::string& nm)
      : cdef_object (new cdef_package_rep (nm)) { }

  cdef_package (const cdef_object& obj)
      : cdef_object (obj)
    {
      // This should never happen...
      if (class_name () != "meta.package")
	error ("internal error: invalid assignment from %s to meta.package object",
	       class_name ().c_str ());
    }

  cdef_package& operator = (const cdef_package& pack)
    {
      cdef_object::operator= (pack);

      return *this;
    }

  void install_class (const cdef_class& cls, const std::string& nm)
    { get_rep ()->install_class (cls, nm); }

  void install_function (const octave_value& fcn, const std::string& nm)
    { get_rep ()->install_function (fcn, nm); }

  void install_package (const cdef_package& pack, const std::string& nm)
    { get_rep ()->install_package (pack, nm); }

  Cell get_classes (void) const
    { return get_rep ()->get_classes (); }

  Cell get_functions (void) const
    { return get_rep ()->get_functions (); }

  Cell get_packages (void) const
    { return get_rep ()->get_packages (); }

private:
  cdef_package_rep* get_rep (void)
    { return dynamic_cast<cdef_package_rep *> (cdef_object::get_rep ()); }
  
  const cdef_package_rep* get_rep (void) const
    { return dynamic_cast<const cdef_package_rep *> (cdef_object::get_rep ()); }
};

class
octave_classdef : public octave_base_value
{
public:
  octave_classdef (void)
      : octave_base_value (), object () { }

  octave_classdef (const cdef_object& obj)
      : octave_base_value (), object (obj) { }

  octave_classdef (const octave_classdef& obj)
      : octave_base_value (obj), object (obj.object) { }

  octave_base_value* clone (void) const
    { return new octave_classdef (object.clone ()); }

  octave_base_value* empty_clone (void) const
    { return new octave_classdef (); }

  cdef_object get_object (void) const
    { return object; }

  bool is_defined (void) const { return true; }

  bool is_map (void) const { return true; }

  bool print_as_scalar (void) const { return true; }

  void print(std::ostream& os, bool pr_as_read_syntax = false) const
    {
      // FIXME: should call "display" method
      print_raw(os, pr_as_read_syntax);
      newline(os);
    }

  void print_raw(std::ostream& os, bool /* pr_as_read_syntax */ = false) const
    {
      os << object.class_name () << " object";
    }

  octave_value_list subsref (const std::string& type,
			     const std::list<octave_value_list>& idx, int nargout);

  octave_value subsref (const std::string& type,
			const std::list<octave_value_list>& idx)
    {
      octave_value_list retval = subsref (type, idx, 1);
      return (retval.length () > 0 ? retval(0) : octave_value ());
    }

  string_vector map_keys (void) const { return object.map_keys (); }

  dim_vector dims (void) const { return dim_vector (1, 1); }

private:
  cdef_object object;

private:
  DECLARE_OCTAVE_ALLOCATOR

public:
  int type_id (void) const { return t_id; }
  std::string type_name (void) const { return t_name; }
  std::string class_name (void) const { return object.class_name (); }

  static int static_type_id (void) { return t_id; }
  static std::string static_type_name (void) { return t_name; }
  static std::string static_class_name (void) { return "<unknown>"; }
  static void register_type (void);

private:
  static int t_id;

  static const std::string t_name;
};

inline octave_value
to_ov (const cdef_object& obj)
{
  if (obj.ok ())
    return octave_value (new octave_classdef (obj));
  else
    return octave_value (Matrix ());
}

inline octave_value
to_ov (const octave_value& ov)
{ return ov; }

inline cdef_object
to_cdef (const octave_value& val)
{
  if (val.type_name () == "object")
    return dynamic_cast<octave_classdef *> (val.internal_rep ())->get_object ();
  else
    {
      warning ("trying to cast non-object into object");
      return cdef_object ();
    }
}

inline cdef_object
to_cdef (const cdef_object& obj)
{ return obj; }

OCTINTERP_API void install_classdef (void);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
