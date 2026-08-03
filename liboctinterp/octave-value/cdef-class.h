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

#if ! defined (octave_cdef_class_h)
#define octave_cdef_class_h 1

#include "octave-config.h"

#include <map>
#include <set>
#include <string>
#include <utility>

#include "oct-refcount.h"

#include "cdef-fwd.h"
#include "cdef-method.h"
#include "cdef-object.h"
#include "cdef-package.h"
#include "cdef-property.h"
#include "cdef-utils.h"
#include "error.h"
#include "ov-classdef.h"
#include "ov.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class interpreter;
class tree_classdef;

// When we want to preserve the order in which properties were first defined,
// we index them with a std::pair key made of:
// - first, their rank in the property list;
// - and second, their name.
typedef std::pair<unsigned int, std::string> property_key;

//! Holds optional properties of an `meta.Class`.
struct cdef_class_opt_properties
{
  //! Underlying type: `logical`
  //!
  //! Classdef attribute.  Classdefs marked as abstract cannot be instantiated.
  //! The Abstract attribute can also be applied to methods and properties.  If
  //! an installed method or a property in a classdef is marked as abstract,
  //! then the whole classdef is abstract as well.
  //!
  //! @todo Abstract properties are not functional yet.
  octave_value abstract = false;

  //! Underlying type: `meta.Class`
  //!
  //! Classdef attribute.  The AllowedSubclasses attribute implements a
  //! whitelist for classdefs that can be subclasses of the aforementioned
  //! classdef.
  octave_value allowed_subclasses = to_ov (empty_meta_class ());

  //! Underlying type: `char`
  //!
  //! The names that can refer to the classdef.
  //!
  //! @todo Not implemented yet.
  octave_value aliases = "";

  //! Underlying type: `logical`
  //!
  //! Classdef attribute.  With this attribute set to True, the constructor of
  //! the classdef is called upon loading the classdef data from a save file
  //! (irrespective of save file format).
  octave_value construct_on_load = false;

  //! Underlying type: `char`
  //!
  //! The initial line of the classdef docstring.  If `help <classdef_name>` is
  //! called, this is the first line of text displayed.
  //!
  //! @note This value can be directly set as a classdef attribute.  If it is,
  //! the value in the classdef attribute always overrides the written
  //! docstring.
  octave_value description = "";

  //! Underlying type: `char`
  //!
  //! The rest of the classdef docstring (see `description`).  If `help
  //! <classdef_name>` is called, this is everything after the first line.
  //!
  //! @note This value can be directly set as a classdef attribute.  If it is,
  //! the value in the classdef attribute always overrides the written
  //! docstring.
  octave_value detailed_description = "";

  //! @todo Not implemented yet.
  octave_value enumeration = false; // Not implemented

  //! @todo Not implemented yet.
  octave_value enumeration_member_list
      = to_ov (cdef_object (Array<cdef_object> ()));

  //! Underlying type: `meta.Event`
  //!
  //! @todo Not implemented yet.
  octave_value event_list = to_ov (cdef_object ());

  //! Underlying type: `logical`
  //!
  //! Classdef attribute.  Allows for a value class with this attribute and a
  //! handle class to be joint superclasses.  Without this attribute,
  //! subclassing from a value class and a handle class at the same time would
  //! lead to conflicting copy semantics.
  octave_value handle_compatible = false;

  //! Underlying type: `logical`
  //!
  //! Classdef attribute.  Used to hide the classdef from the `superclasses`
  //! function.
  octave_value hidden = false;

  //! Underlying type: `meta.Class`
  //!
  //! @todo Not implemented yet.
  octave_value inferior_classes = to_ov (empty_meta_class (dim_vector (0, 1)));

  //! Underlying type: `meta.Class`
  //!
  //! @todo Not implemented yet.
  octave_value namespace_class = to_ov (cdef_object ());

  //! Underlying type: `logical`
  //!
  //! The value of this property is true if either `Sealed = true` or
  //! `AllowedSubclasses` is not empty.
  //!
  //! @warning Setting this value to `true` upon classdef construction will not
  //! affect the behavior of the classdef.  It is meant to be a read-only
  //! property that is dependent upon the `Sealed` and the `AllowedSubclasses`
  //! attribute.
  //!
  //! @todo Not implemented yet.
  octave_value restricts_subclassing = false; // Not implemented

  //! Underlying type: `logical`
  //!
  //! Classdef attribute.  If a classdef is marked as sealed, then it cannot be
  //! subclassed by any classdef.
  octave_value sealed = false;

  //! Underlying type: `meta.Class`
  //!
  //! The list of classdefs that a classdef inherits from.
  octave_value superclass_list = to_ov (empty_meta_class (dim_vector (0, 1)));
};

//! Classdef class definition.
class OCTINTERP_API cdef_class : public cdef_meta_object
{
public:

  enum class member_mode
  {
    //! Only the members literally declared by this class.  For properties, it
    //! mirrors the contents of `m_property_map` and the superclass ancestry
    //! is not walked at all.  For methods, keeps only those whose
    //! DefiningClass is this class.
    declared,

    //! Every member visible externally from this class.  This is the set
    //! reported by `properties (obj)` and by `methods (obj)`.
    all_visible,

    //! Every member reachable from this class, declared or inherited,
    //! with no filtering whatsoever.
    all
  };

private:
  class cdef_class_rep : public cdef_meta_object_rep
  {
  public:
    OCTINTERP_API cdef_class_rep (std::string name, cdef_class_opt_properties);

    cdef_class_rep& operator = (const cdef_class_rep&) = delete;

    ~cdef_class_rep () = default;

    cdef_object_rep * copy () const { return new cdef_class_rep (*this); }

    OCTINTERP_API void put (const std::string& pname, const octave_value& val);

    OCTINTERP_API octave_value get (const std::string& pname) const;

    bool is_class () const { return true; }

    std::string get_name () const
    { return get ("Name").string_value (); }

    void set_name (const std::string& nm) { put ("Name", nm); }

    bool is_abstract () const { return get ("Abstract").bool_value (); }

    bool is_sealed () const { return get ("Sealed").bool_value (); }

    OCTINTERP_API cdef_method
    find_method (const std::string& nm, bool local = false) const;

    //! Install a method onto a classdef.
    //!
    //! If installing a `get.*` or a `set.*` method, install the property first.
    //! Otherwise, an error will be thrown.
    OCTINTERP_API void
    install_method (const cdef_method& meth);

    OCTINTERP_API cdef_object
    get_methods (bool include_ctor = false) const;

    OCTINTERP_API std::map<std::string, cdef_method>
    get_method_map (member_mode mode = member_mode::all,
                    bool include_ctor = false) const;

    OCTINTERP_API cdef_property find_property (const std::string& nm) const;

    OCTINTERP_API void install_property (const cdef_property& prop);

    OCTINTERP_API cdef_object get_properties () const;

    OCTINTERP_API std::map<property_key, cdef_property>
    get_property_map (member_mode mode = member_mode::declared) const;

    OCTINTERP_API string_vector get_names ();

    OCTINTERP_API cdef_object get_superclass_list () const;

    void set_directory (const std::string& dir) { m_directory = dir; }

    std::string get_directory () const { return m_directory; }

    OCTINTERP_API void delete_object (const cdef_object& obj);

    OCTINTERP_API octave_value_list
    meta_subsref (const std::string& type,
                  const std::list<octave_value_list>& idx, int nargout);

    OCTINTERP_API void meta_release ();

    bool meta_accepts_postfix_index (char type) const
    {
      return (type == '(' || type == '.');
    }

    OCTINTERP_API octave_value get_method (const std::string& name) const;

    OCTINTERP_API octave_value get_method (int line) const;

    OCTINTERP_API octave_value
    construct (const octave_value_list& args,
               const bool default_initialize = false);

    OCTINTERP_API octave_value_list
    construct_object (const octave_value_list& args, const int nargout = 1,
                      const bool default_initialize = false);

    OCTINTERP_API void initialize_object (cdef_object& obj);

    OCTINTERP_API octave_value_list
    run_constructor (cdef_object& obj, const octave_value_list& args,
                     const int nargout = 1, bool default_initialize = false);

    void mark_as_handle_class () { m_handle_class = true; }

    bool is_handle_class () const { return m_handle_class; }

    octave_idx_type static_count () const { return m_member_count; }

    void destroy ()
    {
      if (m_member_count)
        {
          m_count++;
          cdef_class lock (this);

          m_member_count = 0;
          m_method_map.clear ();
          m_property_map.clear ();
          m_property_rank_map.clear ();
        }
      else
        delete this;
    }

    void mark_as_meta_class () { m_meta = true; }

    bool is_meta_class () const { return m_meta; }

    void file_name (const std::string& nm) { m_file_name = nm; }

    std::string file_name () const { return m_file_name; }

  private:
    OCTINTERP_API void find_names (std::set<std::string>& names, bool all);

    cdef_class wrap ()
    {
      m_count++;
      return cdef_class (this);
    }

    // The @-directory were this class is loaded from (not used yet).

    std::string m_directory;

    std::string m_file_name;

    // The methods defined by this class.

    std::map<std::string, cdef_method> m_method_map;

    // The properties defined by this class.

    std::map<std::string, cdef_property> m_property_map;
    std::map<std::string, unsigned int> m_property_rank_map;

    // The number of members in this class (methods, properties...)

    octave_idx_type m_member_count;

    // TRUE if this class is a handle class.  A class is a handle class when
    // the abstract "handle" class is one of its superclasses.

    bool m_handle_class;

    //! The list of super-class constructors that are called implicitly by the
    //! classdef engine when creating an object.  These constructors are not
    //! called explicitly by the class constructor.

    std::list<cdef_class> m_implicit_ctor_list;

    // TRUE if this class is a built-in meta class.

    bool m_meta;

    // Utility iterator typedefs.

    typedef std::map<std::string, cdef_method>::iterator method_iterator;
    typedef std::map<std::string, cdef_method>::const_iterator
        method_const_iterator;
    typedef std::map<property_key, cdef_property>::iterator property_iterator;
    typedef std::map<property_key, cdef_property>::const_iterator
        property_const_iterator;

    cdef_class_rep (const cdef_class_rep& c) = default;
  };

public:
  //! Create an invalid class object.
  cdef_class () : cdef_meta_object () { }

  //! Constructor that should be called for user-defined classdefs.
  //!
  //! Superclasses (if any) should be passed in this constructor.  Superclasses
  //! cannot be added after construction -- this is because proper installment
  //! of immediate methods and properties relies on the presence of superclass
  //! methods and properties to have already been installed.
  cdef_class (octave_value name,
              cdef_class_opt_properties opt_args = cdef_class_opt_properties ())
    : cdef_meta_object (new cdef_class_rep (name.string_value (), opt_args))
  { }

  cdef_class (const cdef_class& cls) : cdef_meta_object (cls) { }

  cdef_class (const cdef_object& obj) : cdef_meta_object (obj)
  {
    // This should never happen...
    if (! is_class ())
      error ("internal error: invalid assignment from %s to meta.class object",
             class_name ().c_str ());
  }

  octave_value get (const std::string& pname) const
  {
    return get_rep ()->get (pname);
  }

  cdef_class& operator = (const cdef_class& cls)
  {
    cdef_object::operator = (cls);

    return *this;
  }

  ~cdef_class () = default;

  //! Find a method associated with the classdef.
  //!
  //! An empty `cdef_method` is returned if no method is found.
  OCTINTERP_API cdef_method find_method (const std::string& nm,
                                         bool local = false);

  //! Install a method on the class.  Installed methods can override superclass
  //! methods (with certain exceptions), but cannot conflict with another
  //! method installed in the same class.
  //!
  //! @note For 'get.*' methods, the property must be installed in the classdef
  //! prior to the method being installed.  Otherwise, an error is thrown.
  void install_method (const cdef_method& meth)
  {
    get_rep ()->install_method (meth);
  }

  cdef_object get_methods (bool include_ctor = false) const
  {
    return get_rep ()->get_methods (include_ctor);
  }

  std::map<std::string, cdef_method>
  get_method_map (member_mode mode = member_mode::all,
                  bool include_ctor = false) const
  {
    return get_rep ()->get_method_map (mode, include_ctor);
  }

  OCTINTERP_API cdef_property find_property (const std::string& nm);

  void install_property (const cdef_property& prop)
  {
    get_rep ()->install_property (prop);
  }

  cdef_object get_superclass_list () const
  {
    return get_rep ()->get_superclass_list ();
  }

  cdef_object get_properties () const
  {
    return get_rep ()->get_properties ();
  }

  std::map<property_key, cdef_property>
  get_property_map (member_mode mode = member_mode::declared) const
  {
    return get_rep ()->get_property_map (mode);
  }

  string_vector get_names () { return get_rep ()->get_names (); }

  bool is_abstract () const { return get_rep ()->is_abstract (); }

  bool is_sealed () const { return get_rep ()->is_sealed (); }

  void set_directory (const std::string& dir)
  {
    get_rep ()->set_directory (dir);
  }

  std::string get_directory () const { return get_rep ()->get_directory (); }

  std::string get_name () const { return get_rep ()->get_name (); }

  bool is_builtin () const { return get_directory ().empty (); }

  void delete_object (const cdef_object& obj)
  {
    get_rep ()->delete_object (obj);
  }

  //! Analyze the tree_classdef tree and transform it to a cdef_class
  //!
  //! <b>All attribute validation should occur here.</b>
  //!
  //! Classdef attribute values can be given in the form of expressions.  
  //! These expressions must be evaluated before assigning them as attribute
  //! values.  Evaluating them as they are parsed causes trouble with possible
  //! recursion in the parser so we do it here.  For example
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

  static OCTINTERP_API cdef_class make_meta_class (interpreter& interp,
                                                   tree_classdef *t,
                                                   bool is_at_folder = false);

  octave_value get_method (const std::string& nm) const
  {
    return get_rep ()->get_method (nm);
  }

  octave_value get_method (int ln) const { return get_rep ()->get_method (ln); }

  OCTINTERP_API octave_value get_method_function (const std::string& nm);

  octave_value get_constructor_function ()
  {
    return get_method_function (get_name ());
  }

  bool get_handle_compatible () const
  { return get_rep ()->get ("HandleCompatible").bool_value (); }

  bool get_sealed () const
  { return get_rep ()->get ("Sealed").bool_value (); }

  //! Wrapper for `construct_object` that returns only the constructed object.
  //! Ignores all other constructor return parameters.
  octave_value construct (const octave_value_list& args,
                          const bool default_initialize = false)
  {
    return get_rep ()->construct (args, default_initialize);
  }

  //! The proper method to call for object construction.
  //!
  //! @return The first index slot in the octave_value_list contains the
  //! constructed classdef object.  All the other slots contain (possible)
  //! extra constructor return outputs.
  octave_value_list construct_object (const octave_value_list& args,
                                      const int nargout = 1,
                                      const bool default_initialize = false)
  {
    return get_rep ()->construct_object (args, nargout, default_initialize);
  }

  void initialize_object (cdef_object& obj)
  {
    get_rep ()->initialize_object (obj);
  }

  //! The method to call to run a classdef's constructor.
  //!
  //! @param <obj> The classdef object being constructed is passed by
  //! reference.  If a derived classdef is calling a superclass constructor,
  //! then the derived `cdef_object` is being passed in, and not a brand
  //! new superclass `cdef_object`.
  //!
  //! @return The first slot in the `octave_value_list` is a copy of the
  //! `cdef_object` being passed in.
  octave_value_list run_constructor (cdef_object& obj,
                                     const octave_value_list& args,
                                     int nargout = 1,
                                     bool default_initialize = false)
  {
    return get_rep ()->run_constructor (obj, args, nargout, default_initialize);
  }

  void mark_as_handle_class ()
  {
    get_rep ()->mark_as_handle_class ();
  }

  bool is_handle_class () const
  {
    return get_rep ()->is_handle_class ();
  }

  void mark_as_meta_class () { get_rep ()->mark_as_meta_class (); }

  bool is_meta_class () const { return get_rep ()->is_meta_class (); }

  void file_name (const std::string& nm) { get_rep ()->file_name (nm); }

  std::string file_name () const { return get_rep ()->file_name (); }

private:

  cdef_class (cdef_class_rep *rep) : cdef_meta_object (rep) { }

  void put (const std::string& pname, const octave_value& val)
  {
    get_rep ()->put (pname, val);
  }

  cdef_class_rep * get_rep ()
  {
    cdef_object_rep *r = cdef_object::get_rep ();

    if (! r->is_class ())
      error ("internal error: invalid meta.class object");

    return static_cast<cdef_class_rep *> (r);
  }

  const cdef_class_rep *get_rep () const
  {
    const cdef_object_rep *r = cdef_object::get_rep ();

    if (! r->is_class ())
      error ("internal error: invalid meta.class object");

    return static_cast<const cdef_class_rep *> (r);
  }

  friend OCTINTERP_API bool operator == (const cdef_class&, const cdef_class&);
  friend OCTINTERP_API bool operator != (const cdef_class&, const cdef_class&);
  friend OCTINTERP_API bool operator < (const cdef_class&, const cdef_class&);

  friend void install_classdef (octave::interpreter& interp);

  friend class cdef_manager;
};

inline bool
operator == (const cdef_class& clsa, const cdef_class& clsb)
{
  // FIXME: is this really the right way to check class equality?
  return (clsa.get_rep () == clsb.get_rep ());
}

inline bool
operator != (const cdef_class& clsa, const cdef_class& clsb)
{
  return ! (clsa == clsb);
}

// This is only to be able to use cdef_class as map keys.

inline bool
operator < (const cdef_class& clsa, const cdef_class& clsb)
{
  return clsa.get_rep () < clsb.get_rep ();
}

inline cdef_method
cdef_class::find_method (const std::string& nm, bool local)
{
  return get_rep ()->find_method (nm, local);
}

inline cdef_property
cdef_class::find_property (const std::string& nm)
{
  return get_rep ()->find_property (nm);
}

OCTAVE_END_NAMESPACE(octave)

#endif
