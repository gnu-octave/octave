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

#if ! defined (octave_cdef_property_h)
#define octave_cdef_property_h 1

#include "octave-config.h"

#include <map>
#include <set>
#include <string>

#include "oct-refcount.h"

#include "cdef-object.h"
#include "error.h"
#include "ov.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class
OCTINTERP_API
cdef_property : public cdef_meta_object
{
  friend class cdef_class;

private:

  class
  cdef_property_rep : public cdef_meta_object_rep
  {
  public:

    cdef_property_rep (void) : cdef_meta_object_rep () { }

    cdef_property_rep& operator = (const cdef_property_rep& p) = delete;

    ~cdef_property_rep (void) = default;

    cdef_object_rep * copy (void) const
    {
      return new cdef_property_rep (*this);
    }

    bool is_property (void) const { return true; }

    std::string get_name (void) const { return get("Name").string_value (); }

    void set_name (const std::string& nm) { put ("Name", nm); }

    bool is_constant (void) const { return get("Constant").bool_value (); }

    octave_value get_value (bool do_check_access = true,
                            const std::string& who = "") const;

    octave_value get_value (const cdef_object& obj,
                            bool do_check_access = true,
                            const std::string& who = "") const;

    void set_value (cdef_object& obj, const octave_value& val,
                    bool do_check_access = true,
                    const std::string& who = "");

    OCTINTERP_API bool check_get_access (void) const;

    OCTINTERP_API bool check_set_access (void) const;

  private:
    cdef_property_rep (const cdef_property_rep& p)
      : cdef_meta_object_rep (p)
    { }

    OCTINTERP_API bool is_recursive_set (const cdef_object& obj) const;

    cdef_property wrap (void)
    {
      m_count++;
      return cdef_property (this);
    }

    OCTINTERP_API OCTAVE_NORETURN
    void err_property_access (const std::string& from,
                              bool is_set = false) const;
  };

public:

  cdef_property (void) : cdef_meta_object () { }

  cdef_property (const std::string& nm)
    : cdef_meta_object (new cdef_property_rep ())
  {
    get_rep ()->set_name (nm);
  }

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

  ~cdef_property (void) = default;

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

  bool check_get_access (void) const
  {
    return get_rep ()->check_get_access ();
  }

  bool check_set_access (void) const
  {
    return get_rep ()->check_set_access ();
  }

  std::string get_name (void) const { return get_rep ()->get_name (); }

  bool is_constant (void) const { return get_rep ()->is_constant (); }

private:

  cdef_property_rep * get_rep (void)
  {
    return dynamic_cast<cdef_property_rep *> (cdef_object::get_rep ());
  }

  const cdef_property_rep * get_rep (void) const
  {
    return dynamic_cast<const cdef_property_rep *> (cdef_object::get_rep ());
  }
};

OCTAVE_END_NAMESPACE(octave)

#endif
