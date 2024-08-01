////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007-2024 The Octave Project Developers
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

#if ! defined (octave_ov_class_h)
#define octave_ov_class_h 1

#include "octave-config.h"

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "oct-map.h"
#include "ov-base.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class type_info;

OCTAVE_END_NAMESPACE(octave)

class octave_value_list;

// Data structures.

class OCTINTERP_API octave_class : public octave_base_value
{
public:

  octave_class ()
    : octave_base_value (), m_map (), m_c_name (),
      m_parent_list (), m_obsolete_copies (0)
  { }

  octave_class (const octave_map& m, const std::string& id)
    : octave_base_value (), m_map (m), m_c_name (id),
      m_parent_list (), m_obsolete_copies (0)
  { }

  octave_class (const octave_map& m, const std::string& id,
                const std::list<std::string>& plist)
    : octave_base_value (), m_map (m), m_c_name (id),
      m_parent_list (plist), m_obsolete_copies (0)
  { }

  OCTINTERP_API
  octave_class (const octave_map& m, const std::string& id,
                const octave_value_list& parents);

  octave_class (const octave_class& s)
    : octave_base_value (s), m_map (s.m_map), m_c_name (s.m_c_name),
      m_parent_list (s.m_parent_list), m_obsolete_copies (0)  { }

  ~octave_class () = default;

  octave_base_value * clone () const { return new octave_class (*this); }

  OCTINTERP_API octave_base_value * unique_clone ();

  octave_base_value * empty_clone () const
  {
    return new octave_class (octave_map (m_map.keys ()), m_c_name, m_parent_list);
  }

  OCTINTERP_API void
  break_closure_cycles (const std::shared_ptr<octave::stack_frame>& frame);

  OCTINTERP_API Cell dotref (const octave_value_list& idx);

  OCTINTERP_API Matrix size ();

  OCTINTERP_API octave_idx_type xnumel (const octave_value_list&);

  // We don't need to override all three forms of subsref.  The using
  // declaration will avoid warnings about partially-overloaded virtual
  // functions.
  using octave_base_value::subsref;

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx)
  {
    octave_value_list tmp = subsref (type, idx, 1);
    return tmp.length () > 0 ? tmp(0) : octave_value ();
  }

  OCTINTERP_API octave_value_list
  subsref (const std::string& type, const std::list<octave_value_list>& idx,
           int nargout);

  static OCTINTERP_API octave_value
  numeric_conv (const Cell& val, const std::string& type);

  void assign(const std::string& k, const octave_value& rhs)
  { m_map.assign (k, rhs); }

  OCTINTERP_API octave_value
  subsasgn (const std::string& type, const std::list<octave_value_list>& idx,
            const octave_value& rhs);

  OCTINTERP_API octave_value
  undef_subsasgn (const std::string& type,
                  const std::list<octave_value_list>& idx,
                  const octave_value& rhs);

  OCTINTERP_API octave::idx_vector
  index_vector (bool require_integers = false) const;

  dim_vector dims () const { return m_map.dims (); }

  OCTINTERP_API std::size_t byte_size () const;

  // This is the number of elements in each field.  The total number
  // of elements is numel () * nfields ().
  octave_idx_type numel () const
  {
    const dim_vector& dv = dims ();
    return dv.numel ();
  }

  octave_idx_type nfields () const { return m_map.nfields (); }

  std::size_t nparents () const { return m_parent_list.size (); }

  octave_value reshape (const dim_vector& new_dims) const
  {
    octave_class retval = octave_class (*this);
    retval.m_map = retval.map_value ().reshape (new_dims);
    return octave_value (new octave_class (retval));
  }

  octave_value resize (const dim_vector& dv, bool = false) const
  {
    octave_class retval = octave_class (*this);
    retval.m_map.resize (dv);
    return octave_value (new octave_class (retval));
  }

  bool is_defined () const { return true; }

  bool isstruct () const { return false; }

  bool isobject () const { return true; }

  OCTINTERP_API bool is_true () const;

  octave_map map_value () const { return m_map; }

  OCTINTERP_API string_vector map_keys () const;

  std::list<std::string> parent_class_name_list () const
  { return m_parent_list; }

  string_vector parent_class_names () const
  { return string_vector (m_parent_list); }

  OCTINTERP_API octave_base_value * find_parent_class (const std::string&);

  OCTINTERP_API octave_base_value * unique_parent_class (const std::string&);

  OCTINTERP_API bool is_instance_of (const std::string&) const;

  OCTINTERP_API string_vector string_vector_value (bool pad) const;

  OCTINTERP_API void print (std::ostream& os, bool pr_as_read_syntax = false);

  OCTINTERP_API void print_raw (std::ostream& os,
                                bool pr_as_read_syntax = false) const;

  OCTINTERP_API bool reconstruct_exemplar ();

  OCTINTERP_API static void clear_exemplar_map ();

  OCTINTERP_API bool reconstruct_parents ();

  OCTINTERP_API bool save_ascii (std::ostream& os);

  OCTINTERP_API bool load_ascii (std::istream& is);

  OCTINTERP_API bool save_binary (std::ostream& os, bool save_as_floats);

  OCTINTERP_API bool
  load_binary (std::istream& is, bool swap,
               octave::mach_info::float_format fmt);

  OCTINTERP_API bool
  save_hdf5 (octave_hdf5_id loc_id, const char *name, bool save_as_floats);

  OCTINTERP_API bool
  load_hdf5 (octave_hdf5_id loc_id, const char *name);

  OCTINTERP_API mxArray * as_mxArray (bool interleaved) const;

private:
  octave_map m_map;

public:
  int type_id () const { return s_t_id; }
  std::string type_name () const { return s_t_name; }
  std::string class_name () const { return m_c_name; }

  static int static_type_id () { return s_t_id; }
  static std::string static_type_name () { return s_t_name; }
  static std::string static_class_name () { return "<unknown>"; }
  static OCTINTERP_API void register_type (octave::type_info&);

private:
  static int s_t_id;

  static const std::string s_t_name;
  std::string m_c_name;
  std::list<std::string> m_parent_list;

  OCTINTERP_API bool in_class_method ();
  OCTINTERP_API std::string get_current_method_class ();

  OCTINTERP_API octave_value
  subsasgn_common (const octave_value& obj, const std::string& type,
                   const std::list<octave_value_list>& idx,
                   const octave_value& rhs);

  int m_obsolete_copies;

public:
  // The list of field names and parent classes defines a class.  We
  // keep track of each class that has been created so that we know
  class exemplar_info
  {
  public:

    exemplar_info () : m_field_names (), m_parent_class_names () { }

    OCTINTERP_API exemplar_info (const octave_value& obj);

    exemplar_info (const exemplar_info& x)
      : m_field_names (x.m_field_names),
        m_parent_class_names (x.m_parent_class_names) { }

    exemplar_info& operator = (const exemplar_info& x)
    {
      if (&x != this)
        {
          m_field_names = x.m_field_names;
          m_parent_class_names = x.m_parent_class_names;
        }
      return *this;
    }

    octave_idx_type nfields () const { return m_field_names.numel (); }

    std::size_t nparents () const { return m_parent_class_names.size (); }

    string_vector fields () const { return m_field_names; }

    std::list<std::string> parents () const { return m_parent_class_names; }

    OCTINTERP_API bool compare (const octave_value& obj) const;

  private:

    string_vector m_field_names;
    std::list<std::string> m_parent_class_names;
  };

  // A map from class names to lists of fields.
  static std::map<std::string, exemplar_info> exemplar_map;

  typedef std::map<std::string, exemplar_info>::iterator
    exemplar_iterator;
  typedef std::map<std::string, exemplar_info>::const_iterator
    exemplar_const_iterator;
};

#endif
