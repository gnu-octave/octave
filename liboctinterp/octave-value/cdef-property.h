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

#if ! defined (octave_cdef_property_h)
#define octave_cdef_property_h 1

#include "octave-config.h"

#include <string>

#include "oct-refcount.h"

#include "cdef-method.h"
#include "cdef-object.h"
#include "error.h"
#include "ov.h"

OCTAVE_BEGIN_NAMESPACE(octave)

struct cdef_property_opt_properties {

  //! Type: `logical`
  //!
  //! @todo Not implemented yet.
  octave_value abort_set = false;

  //! Type: `logical`
  //!
  //! Property attribute.  Properties marked as abstract
  //!
  //! @todo Semi-implemented.  Marking at least one property as abstract will
  //! mark the class as abstract, but you can still assign default values to an
  //! abstract property as of right now.
  octave_value abstract = false;

  //! Type: `char`
  //!
  //! Property attribute.  If `Access` is `public`, then the property can be
  //! accessed and modified outside of classdef methods.  If `Access` is
  //! `private`, then the property can only be accessed or modified from within
  //! the classdef's methods.  If `Access` is `protected`, then the property is
  //! both `private` and can be accessed or modified from subclass methods.
  //!
  //! @note Access is stored separately in the metaclass as `GetAccess` and
  //! `SetAccess`.  If `Access` is passed in, then the value of `GetAccess` and
  //! `SetAccess` will copy `Access`'s value. `Access` cannot be set if either
  //! `GetAccess` or `SetAccess` is set.
  octave_value access = "public";

  //! Type: `logical`
  //!
  //! Property attribute.  If `Constant` is `true`, then the property cannot be
  //! modified after object construction.
  octave_value constant = false;

  //! Type: Any type
  //!
  //! Default values are assigned by writing an assignment expression in the
  //! `properties` block.  An undefined value means that no default was
  //! supplied; an empty matrix is a valid, explicitly supplied default.
  octave_value default_value = octave_value ();

  //! Type: `logical` 
  //!
  //! @todo Not implemented yet.
  octave_value dependent = false; // Not implemented
  
  //! Type: `char`
  //!
  //! The initial line of the property docstring.
  //! The rest of the classdef docstring.  If `help
  //! <classdef_name>.<property_name>` is called, this is everything after the
  //! first line.
  //!
  //! @note This value can be directly set as a classdef attribute.  If it is,
  //! the value in the property attribute always overrides the written
  //! docstring.
  octave_value description = "";

  //! Type: `char`
  //!
  //! The rest of the property docstring (see `description`).  If `help
  //! <classdef_name>.<property_name>` is called, this is everything after the
  //! first line.
  //!
  //! @note This value can be directly set as a property attribute.  If it is,
  //! the value in the property attribute always overrides the written
  //! docstring.
  octave_value detailed_description = "";

  //! Type: `char`
  //!
  //! Property attribute.  If `GetAccess` is `public`, then the property can be
  //! accessed outside of classdef methods.  If `GetAccess` is `private`, then
  //! the property can only be accessed from within the classdef's methods.  If
  //! `GetAccess` is `protected`, then the property is both `private` and can
  //! be accessed from subclass methods.
  //!
  //! @note The default visibility of `GetAccess` is `public`.  However, the
  //! empty string is passed in to distinguish between the default visibility
  //! and the intentional setting of `GetAccess` to `public`.
  //!
  //! @note See `Access` for related functionality. `GetAccess` and `Access`
  //! cannot be set jointly on the property.
  octave_value get_access = "";

  //! Type: `function_handle`
  //! 
  //! Sets a getter method on the property.
  //!
  //! @todo: Not fully implemented yet.
  octave_value get_method = Matrix ();

  //! Type: `function_handle`
  //! 
  //! Sets a getter method on the property.
  //!
  //! @todo: Not fully implemented yet.
  octave_value get_observable = false;
  octave_value has_default = false;
  octave_value hidden = false;
  octave_value non_copyable = false;
  octave_value partial_match_priority = 1;
  octave_value set_access = "";
  octave_value set_method = Matrix ();
  octave_value set_observable = false;
  octave_value transient = false;
  octave_value validation = to_ov (cdef_object ());
  octave_value weak_handle = false;
};

class tree_evaluator;
class tree_classdef_properties_block;

//! This class holds the metadata associated with a classdef property.
//! 
//! This class fulfills two purposes
//!     1) An instance of `octave.metadata.property`
//!     2) A set of methods that define how the metadata properties influence
//!        the evaluation, assignment and presentation of the underlying
//!        property value.
//!
//! This class does not hold the underlying value associated with the property
//! (except for the value in the properties' default state).  The value of the
//! property is stored in the object's property map.  Operations to access,
//! assign or view the property in the Octave interpreter will go through this
//! class in order to conform to the object-oriented semantics of Octave.
class OCTINTERP_API cdef_property : public cdef_meta_object
{
  friend class cdef_class;

private:

  class cdef_property_rep : public cdef_meta_object_rep
  {
  public:
    cdef_property_rep (const cdef_object& defining_class,
                       octave_value name,
                       cdef_property_opt_properties opt_args
                         = cdef_property_opt_properties ());

    cdef_property_rep& operator = (const cdef_property_rep& p) = delete;

    ~cdef_property_rep () = default;

    cdef_object_rep * copy () const
    {
      return new cdef_property_rep (*this);
    }

    OCTINTERP_API void
    put (const std::string& pname, const octave_value& val);

    bool is_property () const { return true; }

    OCTINTERP_API cdef_class get_defining_class () const;

    std::string get_name () const { return get ("Name").string_value (); }

    void set_name (const std::string& nm) { put ("Name", nm); }

    bool is_constant () const { return get ("Constant").bool_value (); }

    octave_value get_value (bool do_check_access = true,
                            const std::string& who = "") const;

    octave_value get_value (const cdef_object& obj,
                            bool do_check_access = true,
                            const std::string& who = "") const;

    void set_value (cdef_object& obj, const octave_value& val,
                    bool do_check_access = true,
                    const std::string& who = "");

    OCTINTERP_API bool check_get_access () const;

    OCTINTERP_API bool check_set_access () const;

  private:
    cdef_property_rep (const cdef_property_rep& p)
      : cdef_meta_object_rep (p)
    { }

    OCTINTERP_API bool is_recursive_set (const cdef_object& obj) const;

    cdef_property wrap ()
    {
      m_count++;
      return cdef_property (this);
    }

    OCTAVE_NORETURN OCTINTERP_API
    void err_property_access (const std::string& from,
                              bool is_set = false) const;
  };

public:

  cdef_property () : cdef_meta_object () { }

  cdef_property (const cdef_object& defining_class,
                 octave_value name,
                 cdef_property_opt_properties opt_args
                   = cdef_property_opt_properties ())
    : cdef_meta_object (new cdef_property_rep (defining_class, name, opt_args))
  { }

  cdef_property (const cdef_property& prop) : cdef_meta_object (prop) { }

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

  ~cdef_property () = default;

  OCTINTERP_API cdef_class get_defining_class () const;

  octave_value get_value (const cdef_object& obj, bool do_check_access = true,
                          const std::string& who = "") const
  {
    return get_rep ()->get_value (obj, do_check_access, who);
  }

  octave_value get_value (bool do_check_access = true,
                          const std::string& who = "") const
  {
    return get_rep ()->get_value (do_check_access, who);
  }

  void set_value (cdef_object& obj, const octave_value& val,
                  bool do_check_access = true,
                  const std::string& who = "")
  {
    get_rep ()->set_value (obj, val, do_check_access, who);
  }

  bool check_get_access () const
  {
    return get_rep ()->check_get_access ();
  }

  bool check_set_access () const
  {
    return get_rep ()->check_set_access ();
  }

  std::string get_name () const { return get_rep ()->get_name (); }

  bool is_constant () const { return get_rep ()->is_constant (); }

  //! Installs the properties from a parsed properties block onto a class.
  static OCTINTERP_API void
  make_property (cdef_class& cls, tree_evaluator& tw,
                 tree_classdef_properties_block* prop_blk);

private:

  cdef_property (cdef_property_rep *r) : cdef_meta_object (r) { }

  void put (const std::string& pname, const octave_value& val)
  {
    get_rep ()->put (pname, val);
  }

  cdef_property_rep * get_rep ()
  {
    cdef_object_rep *r = cdef_object::get_rep ();

    panic_if (! r->is_property ());

    return static_cast<cdef_property_rep *> (r);
  }

  const cdef_property_rep * get_rep () const
  {
    const cdef_object_rep *r = cdef_object::get_rep ();

    if (! r->is_property ())
      error ("internal error: invalid meta.property object");

    return static_cast<const cdef_property_rep *> (r);
  }
};

OCTAVE_END_NAMESPACE(octave)

#endif
