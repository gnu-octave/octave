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

#if ! defined (octave_cdef_package_h)
#define octave_cdef_package_h 1

#include "octave-config.h"

#include <map>
#include <set>
#include <string>

#include "oct-refcount.h"

#include "cdef-fwd.h"
#include "cdef-object.h"
#include "ov.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class interpreter;

class
OCTINTERP_API
cdef_package : public cdef_meta_object
{
  friend class cdef_class;

private:

  class
  cdef_package_rep : public cdef_meta_object_rep
  {
  public:

    cdef_package_rep (void) : cdef_meta_object_rep (), m_member_count (0) { }

    cdef_package_rep& operator = (const cdef_package_rep&) = delete;

    ~cdef_package_rep (void) = default;

    cdef_object_rep * copy (void) const
    { return new cdef_package_rep (*this); }

    bool is_package (void) const { return true; }

    std::string get_name (void) const { return get("Name").string_value (); }

    void set_name (const std::string& nm) { put ("Name", nm); }

    OCTINTERP_API void
    install_class (const cdef_class& cls, const std::string& nm);

    OCTINTERP_API void
    install_function (const octave_value& fcn, const std::string& nm);

    OCTINTERP_API void
    install_package (const cdef_package& pack, const std::string& nm);

    OCTINTERP_API Cell get_classes (void) const;

    OCTINTERP_API Cell get_functions (void) const;

    OCTINTERP_API Cell get_packages (void) const;

    octave_idx_type static_count (void) const { return m_member_count; }

    void destroy (void)
    {
      if (m_member_count)
        {
          m_count++;
          cdef_package lock (this);

          m_member_count = 0;
          m_class_map.clear ();
          m_package_map.clear ();
        }
      else
        delete this;
    }

    OCTINTERP_API octave_value_list
    meta_subsref (const std::string& type,
                  const std::list<octave_value_list>& idx, int nargout);

    OCTINTERP_API void meta_release (void);

    bool meta_accepts_postfix_index (char type) const
    {
      return (type == '.');
    }

    OCTINTERP_API octave_value find (const std::string& nm);

  private:

    std::string m_full_name;
    std::map<std::string, cdef_class> m_class_map;
    std::map<std::string, octave_value> m_function_map;
    std::map<std::string, cdef_package> m_package_map;

    // The number of registered members in this package (classes, packages).
    // This only accounts for the members that back-reference to this package.
    octave_idx_type m_member_count;

    typedef std::map<std::string, cdef_class>::iterator class_iterator;
    typedef std::map<std::string, cdef_class>::const_iterator class_const_iterator;
    typedef std::map<std::string, octave_value>::iterator function_iterator;
    typedef std::map<std::string, octave_value>::const_iterator
      function_const_iterator;
    typedef std::map<std::string, cdef_package>::iterator package_iterator;
    typedef std::map<std::string, cdef_package>::const_iterator
      package_const_iterator;

    cdef_package_rep (const cdef_package_rep& p)
      : cdef_meta_object_rep (p), m_full_name (p.m_full_name),
        m_class_map (p.m_class_map), m_function_map (p.m_function_map),
        m_package_map (p.m_package_map), m_member_count (p.m_member_count)
    { }

    cdef_package wrap (void)
    {
      m_count++;
      return cdef_package (this);
    }
  };

public:

  cdef_package (void) : cdef_meta_object () { }

  cdef_package (const std::string& nm)
    : cdef_meta_object (new cdef_package_rep ())
  {
    get_rep ()->set_name (nm);
  }

  cdef_package (const cdef_package& pack) : cdef_meta_object (pack) { }

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
  {
    get_rep ()->install_class (cls, nm);
  }

  void install_function (const octave_value& fcn, const std::string& nm)
  {
    get_rep ()->install_function (fcn, nm);
  }

  void install_package (const cdef_package& pack, const std::string& nm)
  {
    get_rep ()->install_package (pack, nm);
  }

  Cell get_classes (void) const
  {
    return get_rep ()->get_classes ();
  }

  Cell get_functions (void) const
  {
    return get_rep ()->get_functions ();
  }

  Cell get_packages (void) const
  {
    return get_rep ()->get_packages ();
  }

  std::string get_name (void) const { return get_rep ()->get_name (); }

  octave_value find (const std::string& nm)
  {
    return get_rep ()->find (nm);
  }

private:

  cdef_package_rep * get_rep (void)
  {
    return dynamic_cast<cdef_package_rep *> (cdef_object::get_rep ());
  }

  const cdef_package_rep * get_rep (void) const
  {
    return dynamic_cast<const cdef_package_rep *> (cdef_object::get_rep ());
  }

  friend void install_classdef (octave::interpreter& interp);
};

OCTAVE_END_NAMESPACE(octave)

#endif
