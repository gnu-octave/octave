////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2020 The Octave Project Developers
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

#include "oct-refcount.h"

#include "cdef-method.h"
#include "cdef-object.h"
#include "cdef-package.h"
#include "cdef-property.h"
#include "error.h"
#include "ov.h"
#include "ovl.h"

namespace octave
{
  class interpreter;
  class tree_classdef;

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
          meta (false)
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

      Cell get_methods (bool include_ctor);

      std::map<std::string, cdef_method>
      get_method_map (bool only_inherited, bool include_ctor);

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
      {
        return (type == '(' || type == '.');
      }

      octave_value get_method (const std::string& name) const;

      octave_value construct (const octave_value_list& args);

      cdef_object construct_object (const octave_value_list& args);

      void initialize_object (cdef_object& obj);

      void run_constructor (cdef_object& obj, const octave_value_list& args);

      void mark_as_handle_class (void) { handle_class = true; }

      bool is_handle_class (void) const { return handle_class; }

      octave_idx_type static_count (void) const { return member_count; }

      void destroy (void)
      {
        if (member_count)
          {
            m_count++;
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

      void doc_string (const std::string& txt) { m_doc_string = txt; }

      std::string doc_string (void) const { return m_doc_string; }

    private:

      void load_all_methods (void);

      void find_names (std::set<std::string>& names, bool all);

      void find_properties (std::map<std::string,cdef_property>& props,
                            int mode = 0);

      void find_methods (std::map<std::string, cdef_method>& meths,
                         bool only_inherited, bool include_ctor = false);

      cdef_class wrap (void)
      {
        m_count++;
        return cdef_class (this);
      }

      // The @-directory were this class is loaded from.
      // (not used yet)

      std::string directory;

      std::string m_doc_string;

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

      // TRUE if this class is a built-in meta class.

      bool meta;

      // Utility iterator typedefs.

      typedef std::map<std::string,cdef_method>::iterator method_iterator;
      typedef std::map<std::string,cdef_method>::const_iterator method_const_iterator;
      typedef std::map<std::string,cdef_property>::iterator property_iterator;
      typedef std::map<std::string,cdef_property>::const_iterator property_const_iterator;

      cdef_class_rep (const cdef_class_rep& c) = default;
    };

  public:

    // Create an invalid class object.

    cdef_class (void) : cdef_meta_object () { }

    cdef_class (const std::string& nm, const std::list<cdef_class>& superclasses)
      : cdef_meta_object (new cdef_class_rep (superclasses))
    {
      get_rep ()->set_name (nm);
    }

    cdef_class (const cdef_class& cls) : cdef_meta_object (cls) { }

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
    {
      get_rep ()->install_method (meth);
    }

    Cell get_methods (bool include_ctor = false)
    {
      return get_rep ()->get_methods (include_ctor);
    }

    std::map<std::string, cdef_method>
    get_method_map (bool only_inherited = false, bool include_ctor = false)
    {
      return get_rep ()->get_method_map (only_inherited, include_ctor);
    }

    cdef_property find_property (const std::string& nm);

    void install_property (const cdef_property& prop)
    {
      get_rep ()->install_property (prop);
    }

    Cell get_properties (int mode = property_normal)
    {
      return get_rep ()->get_properties (mode);
    }

    std::map<std::string, cdef_property>
    get_property_map (int mode = property_normal)
    {
      return get_rep ()->get_property_map (mode);
    }

    string_vector get_names (void) { return get_rep ()->get_names (); }

    bool is_abstract (void) const { return get_rep ()->is_abstract (); }

    bool is_sealed (void) const { return get_rep ()->is_sealed (); }

    void set_directory (const std::string& dir)
    {
      get_rep ()->set_directory (dir);
    }

    std::string get_directory (void) const
    {
      return get_rep ()->get_directory ();
    }

    std::string get_name (void) const { return get_rep ()->get_name (); }

    bool is_builtin (void) const { return get_directory ().empty (); }

    void delete_object (const cdef_object& obj)
    {
      get_rep ()->delete_object (obj);
    }

    //! Analyze the tree_classdef tree and transform it to a cdef_class
    //!
    //! <b>All attribute validation should occur here.</b>
    //!
    //! Classdef attribute values can be given in the form of
    //! expressions.  These expressions must be evaluated before
    //! assigning them as attribute values.  Evaluating them as they are
    //! parsed causes trouble with possible recursion in the parser so we
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
    make_meta_class (interpreter& interp, tree_classdef *t,
                     bool is_at_folder = false);

    octave_value get_method (const std::string& nm) const
    {
      return get_rep ()->get_method (nm);
    }

    octave_value get_method_function (const std::string& nm);

    octave_value get_constructor_function (void)
    {
      return get_method_function (get_name ());
    }

    octave_value construct (const octave_value_list& args)
    {
      return get_rep ()->construct (args);
    }

    cdef_object construct_object (const octave_value_list& args)
    {
      return get_rep ()->construct_object (args);
    }

    void initialize_object (cdef_object& obj)
    {
      get_rep ()->initialize_object (obj);
    }

    void run_constructor (cdef_object& obj, const octave_value_list& args)
    {
      get_rep ()->run_constructor (obj, args);
    }

    void mark_as_handle_class (void)
    {
      get_rep ()->mark_as_handle_class ();
    }

    bool is_handle_class (void) const
    {
      return get_rep ()->is_handle_class ();
    }

    void mark_as_meta_class (void) { get_rep ()->mark_as_meta_class (); }

    bool is_meta_class (void) const { return get_rep ()->is_meta_class (); }

    void doc_string (const std::string& txt) { get_rep ()->doc_string (txt); }

    std::string doc_string (void) const { return get_rep ()->doc_string (); }

  public:

    enum
      {
       property_normal,
       property_inherited,
       property_all
      };

  private:

    cdef_class_rep * get_rep (void)
    {
      return dynamic_cast<cdef_class_rep *> (cdef_object::get_rep ());
    }

    const cdef_class_rep * get_rep (void) const
    {
      return dynamic_cast<const cdef_class_rep *> (cdef_object::get_rep ());
    }

    friend bool operator == (const cdef_class&, const cdef_class&);
    friend bool operator != (const cdef_class&, const cdef_class&);
    friend bool operator < (const cdef_class&, const cdef_class&);

    friend void install_classdef (interpreter& interp);
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
}

#endif
