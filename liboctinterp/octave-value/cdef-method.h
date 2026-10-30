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

#if ! defined (octave_cdef_method_h)
#define octave_cdef_method_h 1

#include "octave-config.h"

#include <string>

#include "oct-refcount.h"

#include "cdef-fwd.h"
#include "cdef-object.h"
#include "cdef-utils.h"
#include "ov.h"

OCTAVE_BEGIN_NAMESPACE(octave)

struct cdef_method_opt_properties {
  octave_value abstract = false;
  octave_value access = "public";
  octave_value description = "";
  octave_value detailed_description = "";
  octave_value hidden = false;
  octave_value sealed = false;
  octave_value static_bool = false;
};

class tree_evaluator;
class tree_classdef_methods_block;

class OCTINTERP_API cdef_method : public cdef_meta_object
{
  friend class cdef_class;

private:

  class cdef_method_rep : public cdef_meta_object_rep
  {
  public:
    cdef_method_rep (
      const cdef_object& defining_class,
      octave_value fcn,
      octave_value name,
      cdef_method_opt_properties opt_args = cdef_method_opt_properties ());

    cdef_method_rep& operator = (const cdef_method_rep& m) = delete;

    ~cdef_method_rep () = default;

    cdef_object_rep * copy () const
    {
      return new cdef_method_rep (*this);
    }

    bool is_method () const { return true; }

    std::string get_name () const { return get ("Name").string_value (); }

    OCTINTERP_API void put (const std::string& pname, const octave_value& val);

    void set_name (const std::string& nm) { put ("Name", nm); }

    bool is_static () const { return get ("Static").bool_value (); }

    bool is_get_method () const
    {
      return get_name ().substr (0, 4) == "get.";
    }

    bool is_set_method () const
    {
      return get_name ().substr (0, 4) == "set.";
    }

    octave_value get_function () const { return m_function; }

    //! Return the function, resolving an external placeholder if @p load is
    //! true.  Report an error if its external definition cannot be found.
    octave_value get_function (bool load)
    {
      if (load)
        check_method ();

      return m_function;
    }

    void set_function (const octave_value& fcn) { m_function = fcn; }

    OCTINTERP_API std::string get_doc_string ();

    OCTINTERP_API bool check_access () const;

    bool is_external () const { return ! m_dispatch_type.empty (); }

    void mark_as_external (const std::string& dtype)
    {
      m_dispatch_type = dtype;
    }

    OCTINTERP_API octave_value_list
    execute (const octave_value_list& args, int nargout,
             bool do_check_access = true, const std::string& who = "");

    OCTINTERP_API octave_value_list
    execute (const cdef_object& obj,
             const octave_value_list& args, int nargout,
             bool do_check_access = true, const std::string& who = "");

    OCTINTERP_API bool is_constructor () const;

    bool get_hidden () const
    {
      return get ("Hidden").bool_value ();
    }

    OCTINTERP_API bool is_defined_in_class (const std::string& cname) const;

    octave_value_list
    meta_subsref (const std::string& type,
                  const std::list<octave_value_list>& idx, int nargout);

    bool meta_accepts_postfix_index (char type) const
    {
      return (type == '(' || type == '.');
    }

    OCTINTERP_API cdef_class get_defining_class () const;

  private:

    cdef_method_rep (const cdef_method_rep& m)
      : cdef_meta_object_rep (m),
        m_dispatch_type (m.m_dispatch_type), m_function (m.m_function)
    { }

    OCTINTERP_API void check_method ();

    cdef_method wrap ()
    {
      m_count++;
      return cdef_method (this);
    }

    // When non-empty, the method is externally defined and this member
    // is used to cache the dispatch type to look for the method.
    std::string m_dispatch_type;
    octave_value m_function;
  };

public:

  cdef_method () : cdef_meta_object () { }

  //! The canonical classdef method construction path.
  //!
  //! @param[in] defining_class The classdef that contains the method in
  //! question.
  //! @param[in] fcn The underlying function object that holds the function
  //! body.
  //! @param[name] The name of the method.  Must be non-empty and correspond to
  //! a valid property name.  If the name is prefixed with `get.` or `set.`,
  //! then the method becomes a property get or a property set method,
  //! respectively. 
  cdef_method (
    const cdef_object& defining_class,
    octave_value fcn,
    std::string name,
    cdef_method_opt_properties attrs = cdef_method_opt_properties ())
    : cdef_meta_object (new cdef_method_rep (defining_class, fcn, name, attrs))
  { }

  cdef_method (const cdef_method& meth) : cdef_meta_object (meth) { }

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

  ~cdef_method () = default;

  // normal invocation
  octave_value_list execute (const octave_value_list& args, int nargout,
                             bool do_check_access = true,
                             const std::string& who = "")
  {
    return get_rep ()->execute (args, nargout, do_check_access, who);
  }

  // dot-invocation: object is pushed as 1st argument
  octave_value_list execute (const cdef_object& obj,
                             const octave_value_list& args, int nargout,
                             bool do_check_access = true,
                             const std::string& who = "")
  {
    return get_rep ()->execute (obj, args, nargout, do_check_access, who);
  }

  OCTINTERP_API cdef_class get_defining_class () const;

  bool check_access () const { return get_rep ()->check_access (); }

  std::string get_name () const { return get_rep ()->get_name (); }

  bool is_static () const { return get_rep ()->is_static (); }

  void set_function (const octave_value& fcn)
  {
    get_rep ()->set_function (fcn);
  }

  bool get_hidden () const
  {
    return get_rep ()->get_hidden ();
  }

  octave_value get_function () const
  {
    return get_rep ()->get_function ();
  }

  //! Return the function, optionally loading its external definition first.
  //!
  //! Use @p load = true before inspecting a method body, since an external
  //! method may still be a placeholder until its first call.  Loading fails
  //! with an error if no external definition is found.
  octave_value get_function (bool load)
  {
    return get_rep ()->get_function (load);
  }

  std::string get_doc_string ()
  {
    return get_rep ()->get_doc_string ();
  }

  bool is_constructor () const
  {
    return get_rep ()->is_constructor ();
  }

  bool is_defined_in_class (const std::string& cname) const
  {
    return get_rep ()->is_defined_in_class (cname);
  }

  bool is_get_method () const { return get_rep ()->is_get_method (); }

  bool is_external () const { return get_rep ()->is_external (); }

  void mark_as_external (const std::string& dtype)
  {
    get_rep ()->mark_as_external (dtype);
  }

  //! Builds the method objects from a parsed methods block.
  static OCTINTERP_API std::vector<cdef_method>
  make_method (cdef_class& cls, tree_evaluator& tw,
               tree_classdef_methods_block* mth_blk);

private:

  cdef_method (cdef_method_rep *r) : cdef_meta_object (r) { }

  void put (const std::string& pname, const octave_value& val)
  {
    get_rep ()->put (pname, val);
  }

  cdef_method_rep * get_rep ()
  {
    cdef_object_rep *r = cdef_object::get_rep ();

    if (! r->is_method ())
      error ("internal error: invalid meta.method object");

    return static_cast<cdef_method_rep *> (r);
  }

  const cdef_method_rep * get_rep () const
  {
    const cdef_object_rep *r = cdef_object::get_rep ();

    if (! r->is_method ())
      error ("internal error: invalid meta.method object");

    return static_cast<const cdef_method_rep *> (r);
  }
};

OCTAVE_END_NAMESPACE(octave)

#endif
