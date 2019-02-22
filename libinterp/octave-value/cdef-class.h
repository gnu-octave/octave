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

#if ! defined (octave_cdef_class_h)
#define octave_cdef_class_h 1

#include "octave-config.h"

#include <map>
#include <set>
#include <string>

#include "oct-refcount.h"

#include "cdef-object.h"
#include "ov-base.h"
#include "ov-builtin.h"

class cdef_object;
class cdef_property;
class cdef_method;
class cdef_package;

namespace octave
{
  class interpreter;
  class tree_classdef;
  class type_info;
}

class
cdef_class : public cdef_meta_object
{
private:

  class
  cdef_class_rep : public cdef_meta_object_rep
  {
  public:
    cdef_class_rep (void)
      : cdef_meta_object_rep (), member_count (0), handle_class (false),
        object_count (0), meta (false)
    { }

    cdef_class_rep (const std::list<cdef_class>& superclasses);

    cdef_class_rep& operator = (const cdef_class_rep&) = delete;

    ~cdef_class_rep (void) = default;

    cdef_object_rep * copy (void) const { return new cdef_class_rep (*this); }

    bool is_class (void) const { return true; }

    std::string get_name (void) const
    { return get ("Name").string_value (); }

    void set_name (const std::string& nm) { put ("Name", nm); }

    bool is_abstract (void) const { return get ("Abstract").bool_value (); }

    bool is_sealed (void) const { return get ("Sealed").bool_value (); }

    cdef_method find_method (const std::string& nm, bool local = false);

    void install_method (const cdef_method& meth);

    Cell get_methods (void);

    std::map<std::string, cdef_method> get_method_map (bool only_inherited);

    cdef_property find_property (const std::string& nm);

    void install_property (const cdef_property& prop);

    Cell get_properties (int mode);

    std::map<std::string, cdef_property> get_property_map (int mode);

    string_vector get_names (void);

    void set_directory (const std::string& dir) { directory = dir; }

    std::string get_directory (void) const { return directory; }

    void delete_object (const cdef_object& obj);

    octave_value_list
    meta_subsref (const std::string& type,
                  const std::list<octave_value_list>& idx, int nargout);

    void meta_release (void);

    bool meta_accepts_postfix_index (char type) const
    { return (type == '(' || type == '.'); }

    octave_value construct (const octave_value_list& args);

    cdef_object construct_object (const octave_value_list& args);

    void initialize_object (cdef_object& obj);

    void run_constructor (cdef_object& obj, const octave_value_list& args);

    void mark_as_handle_class (void) { handle_class = true; }

    bool is_handle_class (void) const { return handle_class; }

    void register_object (void) { object_count++; }

    void unregister_object (void) { object_count--; }

    octave_idx_type static_count (void) const { return member_count; }

    void destroy (void)
    {
      if (member_count)
        {
          refcount++;
          cdef_class lock (this);

          member_count = 0;
          method_map.clear ();
          property_map.clear ();
        }
      else
        delete this;
    }

    void mark_as_meta_class (void) { meta = true; }

    bool is_meta_class (void) const { return meta; }

  private:

    void load_all_methods (void);

    void find_names (std::set<std::string>& names, bool all);

    void find_properties (std::map<std::string,cdef_property>& props,
                          int mode = 0);

    void find_methods (std::map<std::string, cdef_method>& meths,
                       bool only_inherited);

    cdef_class wrap (void)
    {
      refcount++;
      return cdef_class (this);
    }

    // The @-directory were this class is loaded from.
    // (not used yet)
    std::string directory;

    // The methods defined by this class.
    std::map<std::string,cdef_method> method_map;

    // The properties defined by this class.
    std::map<std::string,cdef_property> property_map;

    // The number of members in this class (methods, properties...)
    octave_idx_type member_count;

    // TRUE if this class is a handle class.  A class is a handle
    // class when the abstract "handle" class is one of its superclasses.
    bool handle_class;

    // The list of super-class constructors that are called implicitly by the
    // the classdef engine when creating an object.  These constructors are not
    // called explicitly by the class constructor.
    std::list<cdef_class> implicit_ctor_list;

    // The number of objects of this class.
    octave::refcount<octave_idx_type> object_count;

    // TRUE if this class is a built-in meta class.
    bool meta;

    // Utility iterator typedef's.
    typedef std::map<std::string,cdef_method>::iterator method_iterator;
    typedef std::map<std::string,cdef_method>::const_iterator method_const_iterator;
    typedef std::map<std::string,cdef_property>::iterator property_iterator;
    typedef std::map<std::string,cdef_property>::const_iterator property_const_iterator;

    cdef_class_rep (const cdef_class_rep& c)
      : cdef_meta_object_rep (c), directory (c.directory),
        method_map (c.method_map), property_map (c.property_map),
        member_count (c.member_count), handle_class (c.handle_class),
        implicit_ctor_list (c.implicit_ctor_list),
        object_count (c.object_count), meta (c.meta) { }
  };

public:
  // Create and invalid class object
  cdef_class (void)
    : cdef_meta_object () { }

  cdef_class (const std::string& nm, const std::list<cdef_class>& superclasses)
    : cdef_meta_object (new cdef_class_rep (superclasses))
  { get_rep ()->set_name (nm); }

  cdef_class (const cdef_class& cls)
    : cdef_meta_object (cls) { }

  cdef_class (const cdef_object& obj)
    : cdef_meta_object (obj)
  {
    // This should never happen...
    if (! is_class ())
      error ("internal error: invalid assignment from %s to meta.class object",
             class_name ().c_str ());
  }

  cdef_class& operator = (const cdef_class& cls)
  {
    cdef_object::operator = (cls);

    return *this;
  }

  ~cdef_class (void) = default;

  cdef_method find_method (const std::string& nm, bool local = false);

  void install_method (const cdef_method& meth)
  { get_rep ()->install_method (meth); }

  Cell get_methods (void) { return get_rep ()->get_methods (); }

  std::map<std::string, cdef_method>
  get_method_map (bool only_inherited = false)
  { return get_rep ()->get_method_map (only_inherited); }

  cdef_property find_property (const std::string& nm);

  void install_property (const cdef_property& prop)
  { get_rep ()->install_property (prop); }

  Cell get_properties (int mode = property_normal)
  { return get_rep ()->get_properties (mode); }

  std::map<std::string, cdef_property>
  get_property_map (int mode = property_normal)
  { return get_rep ()->get_property_map (mode); }

  string_vector get_names (void) { return get_rep ()->get_names (); }

  bool is_abstract (void) const { return get_rep ()->is_abstract (); }

  bool is_sealed (void) const { return get_rep ()->is_sealed (); }

  void set_directory (const std::string& dir)
  { get_rep ()->set_directory (dir); }

  std::string get_directory (void) const
  { return get_rep ()->get_directory (); }

  std::string get_name (void) const
  { return get_rep ()->get_name (); }

  bool is_builtin (void) const
  { return get_directory ().empty (); }

  void delete_object (const cdef_object& obj)
  { get_rep ()->delete_object (obj); }

  //! Analyze the tree_classdef tree and transform it to a cdef_class
  //!
  //! <b>All attribute validation should occur here.</b>
  //!
  //! Classdef attribute values can be given in the form of
  //! expressions.  These expressions must be evaluated before
  //! assigning them as attribute values.  Evaluating them as they are
  //! parsed causes trouble with possible recusion in the parser so we
  //! do it here.  For example
  //!
  //! @code
  //! classdef recursion_class
  //!   methods (Access = ?recursion_class)
  //!   endmethods
  //! endclassdef
  //! @endcode
  //!
  //! will fail because each attempt to compute the metaclass of
  //! recursion_class will cause recursion_class to be parsed again.

  static cdef_class
  make_meta_class (octave::interpreter& interp, octave::tree_classdef *t,
                   bool is_at_folder = false);

  octave_function * get_method_function (const std::string& nm);

  octave_function * get_constructor_function (void)
  { return get_method_function (get_name ()); }

  octave_value construct (const octave_value_list& args)
  { return get_rep ()->construct (args); }

  cdef_object construct_object (const octave_value_list& args)
  { return get_rep ()->construct_object (args); }

  void initialize_object (cdef_object& obj)
  { get_rep ()->initialize_object (obj); }

  void run_constructor (cdef_object& obj, const octave_value_list& args)
  { get_rep ()->run_constructor (obj, args); }

  void mark_as_handle_class (void)
  { get_rep ()->mark_as_handle_class (); }

  bool is_handle_class (void) const
  { return get_rep ()->is_handle_class (); }

  void mark_as_meta_class (void) { get_rep ()->mark_as_meta_class (); }

  bool is_meta_class (void) const { return get_rep ()->is_meta_class (); }

  void register_object (void) { get_rep ()->register_object (); }

  void unregister_object (void) { get_rep ()->unregister_object (); }

public:
  enum
  {
    property_normal,
    property_inherited,
    property_all
  };

private:
  cdef_class_rep * get_rep (void)
  { return dynamic_cast<cdef_class_rep *> (cdef_object::get_rep ()); }

  const cdef_class_rep * get_rep (void) const
  { return dynamic_cast<const cdef_class_rep *> (cdef_object::get_rep ()); }

  friend bool operator == (const cdef_class&, const cdef_class&);
  friend bool operator != (const cdef_class&, const cdef_class&);
  friend bool operator < (const cdef_class&, const cdef_class&);

  friend void install_classdef (octave::interpreter& interp);
};

inline bool
operator == (const cdef_class& clsa, const cdef_class& clsb)
// FIXME: is this really the right way to check class equality?
{ return (clsa.get_rep () == clsb.get_rep ()); }

inline bool
operator != (const cdef_class& clsa, const cdef_class& clsb)
{ return ! (clsa == clsb); }

// This is only to be able to use cdef_class as map keys.
inline bool
operator < (const cdef_class& clsa, const cdef_class& clsb)
{ return clsa.get_rep () < clsb.get_rep (); }

class
cdef_property : public cdef_meta_object
{
  friend class cdef_class;

private:

  class
  cdef_property_rep : public cdef_meta_object_rep
  {
  public:
    cdef_property_rep (void)
      : cdef_meta_object_rep () { }

    cdef_property_rep& operator = (const cdef_property_rep& p) = delete;

    ~cdef_property_rep (void) = default;

    cdef_object_rep * copy (void) const { return new cdef_property_rep (*this); }

    bool is_property (void) const { return true; }

    std::string get_name (void) const { return get("Name").string_value (); }

    void set_name (const std::string& nm) { put ("Name", nm); }

    bool is_constant (void) const { return get("Constant").bool_value (); }

    octave_value get_value (bool do_check_access = true,
                            const std::string& who = "");

    octave_value get_value (const cdef_object& obj,
                            bool do_check_access = true,
                            const std::string& who = "");

    void set_value (cdef_object& obj, const octave_value& val,
                    bool do_check_access = true,
                    const std::string& who = "");

    bool check_get_access (void) const;

    bool check_set_access (void) const;

  private:
    cdef_property_rep (const cdef_property_rep& p)
      : cdef_meta_object_rep (p) { }

    bool is_recursive_set (const cdef_object& obj) const;

    cdef_property wrap (void)
    {
      refcount++;
      return cdef_property (this);
    }
  };

public:
  cdef_property (void) : cdef_meta_object () { }

  cdef_property (const std::string& nm)
    : cdef_meta_object (new cdef_property_rep ())
  { get_rep ()->set_name (nm); }

  cdef_property (const cdef_property& prop)
    : cdef_meta_object (prop) { }

  cdef_property (const cdef_object& obj)
    : cdef_meta_object (obj)
  {
    // This should never happen...
    if (! is_property ())
      error ("internal error: invalid assignment from %s to meta.property object",
             class_name ().c_str ());
  }

  cdef_property& operator = (const cdef_property& prop)
  {
    cdef_object::operator = (prop);

    return *this;
  }

  ~cdef_property (void) = default;

  octave_value get_value (const cdef_object& obj, bool do_check_access = true,
                          const std::string& who = "")
  { return get_rep ()->get_value (obj, do_check_access, who); }

  octave_value get_value (bool do_check_access = true,
                          const std::string& who = "")
  { return get_rep ()->get_value (do_check_access, who); }

  void set_value (cdef_object& obj, const octave_value& val,
                  bool do_check_access = true,
                  const std::string& who = "")
  { get_rep ()->set_value (obj, val, do_check_access, who); }

  bool check_get_access (void) const
  { return get_rep ()->check_get_access (); }

  bool check_set_access (void) const
  { return get_rep ()->check_set_access (); }

  std::string get_name (void) const { return get_rep ()->get_name (); }

  bool is_constant (void) const { return get_rep ()->is_constant (); }

private:
  cdef_property_rep * get_rep (void)
  { return dynamic_cast<cdef_property_rep *> (cdef_object::get_rep ()); }

  const cdef_property_rep * get_rep (void) const
  { return dynamic_cast<const cdef_property_rep *> (cdef_object::get_rep ()); }
};

class
cdef_method : public cdef_meta_object
{
  friend class cdef_class;

private:

  class
  cdef_method_rep : public cdef_meta_object_rep
  {
  public:
    cdef_method_rep (void)
      : cdef_meta_object_rep (), function (), dispatch_type ()
    { }

    cdef_method_rep& operator = (const cdef_method_rep& m) = delete;

    ~cdef_method_rep (void) = default;

    cdef_object_rep * copy (void) const { return new cdef_method_rep(*this); }

    bool is_method (void) const { return true; }

    std::string get_name (void) const { return get("Name").string_value (); }

    void set_name (const std::string& nm) { put ("Name", nm); }

    bool is_static (void) const { return get("Static").bool_value (); }

    octave_value get_function (void) const { return function; }

    void set_function (const octave_value& fcn) { function = fcn; }

    bool check_access (void) const;

    bool is_external (void) const { return ! dispatch_type.empty (); }

    void mark_as_external (const std::string& dtype)
    { dispatch_type = dtype; }

    octave_value_list execute (const octave_value_list& args, int nargout,
                               bool do_check_access = true,
                               const std::string& who = "");

    octave_value_list execute (const cdef_object& obj,
                               const octave_value_list& args, int nargout,
                               bool do_check_access = true,
                               const std::string& who = "");

    bool is_constructor (void) const;

    octave_value_list
    meta_subsref (const std::string& type,
                  const std::list<octave_value_list>& idx, int nargout);

    bool meta_accepts_postfix_index (char type) const
    { return (type == '(' || type == '.'); }

  private:
    cdef_method_rep (const cdef_method_rep& m)
      : cdef_meta_object_rep (m), function (m.function),
        dispatch_type (m.dispatch_type)
    { }

    void check_method (void);

    cdef_method wrap (void)
    {
      refcount++;
      return cdef_method (this);
    }

    octave_value function;

    // When non-empty, the method is externally defined and this member
    // is used to cache the dispatch type to look for the method.
    std::string dispatch_type;
  };

public:
  cdef_method (void) : cdef_meta_object () { }

  cdef_method (const std::string& nm)
    : cdef_meta_object (new cdef_method_rep ())
  { get_rep ()->set_name (nm); }

  cdef_method (const cdef_method& meth)
    : cdef_meta_object (meth) { }

  cdef_method (const cdef_object& obj)
    : cdef_meta_object (obj)
  {
    // This should never happen...
    if (! is_method ())
      error ("internal error: invalid assignment from %s to meta.method object",
             class_name ().c_str ());
  }

  cdef_method& operator = (const cdef_method& meth)
  {
    cdef_object::operator = (meth);

    return *this;
  }

  ~cdef_method (void) = default;

  // normal invocation
  octave_value_list execute (const octave_value_list& args, int nargout,
                             bool do_check_access = true,
                             const std::string& who = "")
  { return get_rep ()->execute (args, nargout, do_check_access, who); }

  // dot-invocation: object is pushed as 1st argument
  octave_value_list execute (const cdef_object& obj,
                             const octave_value_list& args, int nargout,
                             bool do_check_access = true,
                             const std::string& who = "")
  { return get_rep ()->execute (obj, args, nargout, do_check_access, who); }

  bool check_access (void) const { return get_rep ()->check_access (); }

  std::string get_name (void) const { return get_rep ()->get_name (); }

  bool is_static (void) const { return get_rep ()->is_static (); }

  void set_function (const octave_value& fcn)
  { get_rep ()->set_function (fcn); }

  octave_value get_function (void) const
  { return get_rep ()->get_function (); }

  bool is_constructor (void) const
  { return get_rep ()->is_constructor (); }

  bool is_external (void) const { return get_rep ()->is_external (); }

  void mark_as_external (const std::string& dtype)
  { get_rep ()->mark_as_external (dtype); }

private:
  cdef_method_rep * get_rep (void)
  { return dynamic_cast<cdef_method_rep *> (cdef_object::get_rep ()); }

  const cdef_method_rep * get_rep (void) const
  { return dynamic_cast<const cdef_method_rep *> (cdef_object::get_rep ()); }
};

inline cdef_method
cdef_class::find_method (const std::string& nm, bool local)
{ return get_rep ()->find_method (nm, local); }

inline cdef_property
cdef_class::find_property (const std::string& nm)
{ return get_rep ()->find_property (nm); }

class
cdef_package : public cdef_meta_object
{
  friend class cdef_class;

private:

  class
  cdef_package_rep : public cdef_meta_object_rep
  {
  public:
    cdef_package_rep (void)
      : cdef_meta_object_rep (), member_count (0) { }

    cdef_package_rep& operator = (const cdef_package_rep&) = delete;

    ~cdef_package_rep (void) = default;

    cdef_object_rep * copy (void) const { return new cdef_package_rep (*this); }

    bool is_package (void) const { return true; }

    std::string get_name (void) const { return get("Name").string_value (); }

    void set_name (const std::string& nm) { put ("Name", nm); }

    void install_class (const cdef_class& cls, const std::string& nm);

    void install_function (const octave_value& fcn, const std::string& nm);

    void install_package (const cdef_package& pack, const std::string& nm);

    Cell get_classes (void) const;

    Cell get_functions (void) const;

    Cell get_packages (void) const;

    octave_idx_type static_count (void) const { return member_count; }

    void destroy (void)
    {
      if (member_count)
        {
          refcount++;
          cdef_package lock (this);

          member_count = 0;
          class_map.clear ();
          package_map.clear ();
        }
      else
        delete this;
    }

    octave_value_list
    meta_subsref (const std::string& type,
                  const std::list<octave_value_list>& idx, int nargout);

    void meta_release (void);

    bool meta_accepts_postfix_index (char type) const
    { return (type == '.'); }

    octave_value find (const std::string& nm);

  private:
    std::string full_name;
    std::map<std::string, cdef_class> class_map;
    std::map<std::string, octave_value> function_map;
    std::map<std::string, cdef_package> package_map;

    // The number of registered members in this package (classes, packages).
    // This only accounts for the members that back-reference to this package.
    octave_idx_type member_count;

    typedef std::map<std::string, cdef_class>::iterator class_iterator;
    typedef std::map<std::string, cdef_class>::const_iterator class_const_iterator;
    typedef std::map<std::string, octave_value>::iterator function_iterator;
    typedef std::map<std::string, octave_value>::const_iterator function_const_iterator;
    typedef std::map<std::string, cdef_package>::iterator package_iterator;
    typedef std::map<std::string, cdef_package>::const_iterator package_const_iterator;

    cdef_package_rep (const cdef_package_rep& p)
      : cdef_meta_object_rep (p), full_name (p.full_name),
        class_map (p.class_map), function_map (p.function_map),
        package_map (p.package_map), member_count (p.member_count)
    { }

    cdef_package wrap (void)
    {
      refcount++;
      return cdef_package (this);
    }
  };

public:
  cdef_package (void) : cdef_meta_object () { }

  cdef_package (const std::string& nm)
    : cdef_meta_object (new cdef_package_rep ())
  { get_rep ()->set_name (nm); }

  cdef_package (const cdef_package& pack)
    : cdef_meta_object (pack) { }

  cdef_package (const cdef_object& obj)
    : cdef_meta_object (obj)
  {
    // This should never happen...
    if (! is_package ())
      error ("internal error: invalid assignment from %s to meta.package object",
             class_name ().c_str ());
  }

  cdef_package& operator = (const cdef_package& pack)
  {
    cdef_object::operator = (pack);

    return *this;
  }

  ~cdef_package (void) = default;

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

  std::string get_name (void) const { return get_rep ()->get_name (); }

  octave_value find (const std::string& nm) { return get_rep ()->find (nm); }

private:
  cdef_package_rep * get_rep (void)
  { return dynamic_cast<cdef_package_rep *> (cdef_object::get_rep ()); }

  const cdef_package_rep * get_rep (void) const
  { return dynamic_cast<const cdef_package_rep *> (cdef_object::get_rep ()); }

  friend void install_classdef (octave::interpreter& interp);
};

#endif
