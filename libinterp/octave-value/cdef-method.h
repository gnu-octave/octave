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

#if ! defined (octave_cdef_method_h)
#define octave_cdef_method_h 1

#include "octave-config.h"

#include <map>
#include <set>
#include <string>

#include "oct-refcount.h"

#include "cdef-object.h"
#include "ov.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class
OCTINTERP_API
cdef_method : public cdef_meta_object
{
  friend class cdef_class;

private:

  class
  cdef_method_rep : public cdef_meta_object_rep
  {
  public:

    cdef_method_rep (void)
      : cdef_meta_object_rep (), m_function (), m_dispatch_type ()
    { }

    cdef_method_rep& operator = (const cdef_method_rep& m) = delete;

    ~cdef_method_rep (void) = default;

    cdef_object_rep * copy (void) const { return new cdef_method_rep(*this); }

    bool is_method (void) const { return true; }

    std::string get_name (void) const { return get("Name").string_value (); }

    void set_name (const std::string& nm) { put ("Name", nm); }

    bool is_static (void) const { return get("Static").bool_value (); }

    octave_value get_function (void) const { return m_function; }

    void set_function (const octave_value& fcn) { m_function = fcn; }

    OCTINTERP_API std::string get_doc_string (void);

    OCTINTERP_API bool check_access (void) const;

    bool is_external (void) const { return ! m_dispatch_type.empty (); }

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

    OCTINTERP_API bool is_constructor (void) const;

    OCTINTERP_API bool is_defined_in_class (const std::string& cname) const;

    octave_value_list
    meta_subsref (const std::string& type,
                  const std::list<octave_value_list>& idx, int nargout);

    bool meta_accepts_postfix_index (char type) const
    {
      return (type == '(' || type == '.');
    }

  private:

    cdef_method_rep (const cdef_method_rep& m)
      : cdef_meta_object_rep (m), m_function (m.m_function),
        m_dispatch_type (m.m_dispatch_type)
    { }

    OCTINTERP_API void check_method (void);

    cdef_method wrap (void)
    {
      m_count++;
      return cdef_method (this);
    }

    octave_value m_function;

    // When non-empty, the method is externally defined and this member
    // is used to cache the dispatch type to look for the method.

    std::string m_dispatch_type;
  };

public:

  cdef_method (void) : cdef_meta_object () { }

  cdef_method (const std::string& nm)
    : cdef_meta_object (new cdef_method_rep ())
  {
    get_rep ()->set_name (nm);
  }

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

  ~cdef_method (void) = default;

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

  bool check_access (void) const { return get_rep ()->check_access (); }

  std::string get_name (void) const { return get_rep ()->get_name (); }

  bool is_static (void) const { return get_rep ()->is_static (); }

  void set_function (const octave_value& fcn)
  {
    get_rep ()->set_function (fcn);
  }

  octave_value get_function (void) const
  {
    return get_rep ()->get_function ();
  }

  std::string get_doc_string (void)
  {
    return get_rep ()->get_doc_string ();
  }

  bool is_constructor (void) const
  {
    return get_rep ()->is_constructor ();
  }

  bool is_defined_in_class (const std::string& cname) const
  {
    return get_rep ()->is_defined_in_class (cname);
  }

  bool is_external (void) const { return get_rep ()->is_external (); }

  void mark_as_external (const std::string& dtype)
  {
    get_rep ()->mark_as_external (dtype);
  }

private:

  cdef_method_rep * get_rep (void)
  {
    return dynamic_cast<cdef_method_rep *> (cdef_object::get_rep ());
  }

  const cdef_method_rep * get_rep (void) const
  {
    return dynamic_cast<const cdef_method_rep *> (cdef_object::get_rep ());
  }
};

OCTAVE_END_NAMESPACE(octave)

#endif
