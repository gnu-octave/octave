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

#if ! defined (octave_cdef_package_h)
#define octave_cdef_package_h 1

#include "octave-config.h"

#include <map>
#include <set>
#include <string>

#include "oct-refcount.h"

#include "cdef-object.h"
#include "ov.h"

namespace octave
{
  class interpreter;

  class
  cdef_package : public cdef_meta_object
  {
    friend class cdef_class;

  private:

    class
    cdef_package_rep : public cdef_meta_object_rep
    {
    public:

      cdef_package_rep (void) : cdef_meta_object_rep (), member_count (0) { }

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
            m_count++;
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
      {
        return (type == '.');
      }

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
      typedef std::map<std::string, octave_value>::const_iterator
        function_const_iterator;
      typedef std::map<std::string, cdef_package>::iterator package_iterator;
      typedef std::map<std::string, cdef_package>::const_iterator
        package_const_iterator;

      cdef_package_rep (const cdef_package_rep& p)
        : cdef_meta_object_rep (p), full_name (p.full_name),
          class_map (p.class_map), function_map (p.function_map),
          package_map (p.package_map), member_count (p.member_count)
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

    friend void install_classdef (interpreter& interp);
  };
}

#endif
