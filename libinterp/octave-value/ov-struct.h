////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#if ! defined (octave_ov_struct_h)
#define octave_ov_struct_h 1

#include "octave-config.h"

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "oct-map.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

class octave_value_list;

// Data structures.

class
octave_struct : public octave_base_value
{
public:

  octave_struct (void)
    : octave_base_value (), m_map () { }

  octave_struct (const octave_map& m)
    : octave_base_value (), m_map (m) { }

  octave_struct (const octave_struct& s)
    : octave_base_value (), m_map (s.m_map) { }

  ~octave_struct (void) = default;

  octave_base_value * clone (void) const { return new octave_struct (*this); }
  octave_base_value * empty_clone (void) const { return new octave_struct (); }

  void break_closure_cycles (const std::shared_ptr<octave::stack_frame>& frame);

  octave_base_value * try_narrowing_conversion (void);

  Cell dotref (const octave_value_list& idx, bool auto_add = false);

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx)
  {
    octave_value_list tmp = subsref (type, idx, 1);
    return tmp.length () > 0 ? tmp(0) : octave_value ();
  }

  octave_value_list subsref (const std::string&,
                             const std::list<octave_value_list>&, int);

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx,
                        bool auto_add);

  static octave_value numeric_conv (const octave_value& val,
                                    const std::string& type);

  octave_value subsasgn (const std::string& type,
                         const std::list<octave_value_list>& idx,
                         const octave_value& rhs);

  octave_value squeeze (void) const { return m_map.squeeze (); }

  octave_value permute (const Array<int>& vec, bool inv = false) const
  { return m_map.permute (vec, inv); }

  octave_value do_index_op (const octave_value_list& idx,
                            bool resize_ok = false);

  dim_vector dims (void) const { return m_map.dims (); }

  std::size_t byte_size (void) const;

  // This is the number of elements in each field.  The total number
  // of elements is numel () * nfields ().
  octave_idx_type numel (void) const
  {
    return m_map.numel ();
  }

  octave_idx_type nfields (void) const { return m_map.nfields (); }

  octave_value reshape (const dim_vector& new_dims) const
  { return m_map.reshape (new_dims); }

  octave_value resize (const dim_vector& dv, bool fill = false) const
  { octave_map tmap = m_map; tmap.resize (dv, fill); return tmap; }

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  bool isstruct (void) const { return true; }

  builtin_type_t builtin_type (void) const { return btyp_struct; }

  octave_map map_value (void) const { return m_map; }

  string_vector map_keys (void) const { return m_map.fieldnames (); }

  bool isfield (const std::string& field_name) const
  { return m_map.isfield (field_name); }

  void print (std::ostream& os, bool pr_as_read_syntax = false);

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (std::ostream& os, const std::string& name) const;

  std::string edit_display (const float_display_format& fmt,
                            octave_idx_type i, octave_idx_type j) const;

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool save_as_floats);

  bool load_binary (std::istream& is, bool swap,
                    octave::mach_info::float_format fmt);

  bool save_hdf5 (octave_hdf5_id loc_id, const char *name, bool save_as_floats);

  bool load_hdf5 (octave_hdf5_id loc_id, const char *name);

  mxArray * as_mxArray (bool interleaved) const;

  octave_value
  fast_elem_extract (octave_idx_type n) const;

  bool
  fast_elem_insert (octave_idx_type n, const octave_value& x);

protected:

  // The associative array used to manage the structure data.
  octave_map m_map;

private:

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

class
octave_scalar_struct : public octave_base_value
{
public:

  octave_scalar_struct (void)
    : octave_base_value (), m_map () { }

  octave_scalar_struct (const octave_scalar_map& m)
    : octave_base_value (), m_map (m) { }

  octave_scalar_struct (const std::map<std::string, octave_value>& m)
    : octave_base_value (), m_map (m) { }

  octave_scalar_struct (const octave_scalar_struct& s)
    : octave_base_value (), m_map (s.m_map) { }

  ~octave_scalar_struct (void) = default;

  octave_base_value * clone (void) const
  { return new octave_scalar_struct (*this); }
  octave_base_value * empty_clone (void) const
  { return new octave_scalar_struct (); }

  void break_closure_cycles (const std::shared_ptr<octave::stack_frame>& frame);

  octave_value dotref (const octave_value_list& idx, bool auto_add = false);

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx);

  octave_value_list subsref (const std::string& type,
                             const std::list<octave_value_list>& idx, int);

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx,
                        bool auto_add);

  static octave_value numeric_conv (const octave_value& val,
                                    const std::string& type);

  octave_value subsasgn (const std::string& type,
                         const std::list<octave_value_list>& idx,
                         const octave_value& rhs);

  octave_value squeeze (void) const { return m_map; }

  octave_value permute (const Array<int>& vec, bool inv = false) const
  { return octave_map (m_map).permute (vec, inv); }

  octave_value do_index_op (const octave_value_list& idx,
                            bool resize_ok = false);

  dim_vector dims (void) const { static dim_vector dv (1, 1); return dv; }

  std::size_t byte_size (void) const;

  // This is the number of elements in each field.  The total number
  // of elements is numel () * nfields ().
  octave_idx_type numel (void) const
  {
    return 1;
  }

  octave_idx_type nfields (void) const { return m_map.nfields (); }

  octave_value reshape (const dim_vector& new_dims) const
  { return octave_map (m_map).reshape (new_dims); }

  octave_value resize (const dim_vector& dv, bool fill = false) const
  { octave_map tmap = m_map; tmap.resize (dv, fill); return tmap; }

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  bool isstruct (void) const { return true; }

  builtin_type_t builtin_type (void) const { return btyp_struct; }

  octave_map map_value (void) const { return m_map; }

  octave_scalar_map scalar_map_value (void) const { return m_map; }

  string_vector map_keys (void) const { return m_map.fieldnames (); }

  bool isfield (const std::string& field_name) const
  { return m_map.isfield (field_name); }

  void print (std::ostream& os, bool pr_as_read_syntax = false);

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (std::ostream& os, const std::string& name) const;

  std::string edit_display (const float_display_format& fmt,
                            octave_idx_type i, octave_idx_type j) const;

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool save_as_floats);

  bool load_binary (std::istream& is, bool swap,
                    octave::mach_info::float_format fmt);

  bool save_hdf5 (octave_hdf5_id loc_id, const char *name, bool save_as_floats);

  bool load_hdf5 (octave_hdf5_id loc_id, const char *name);

  mxArray * as_mxArray (bool interleaved) const;

  bool fast_elem_insert_self (void *where, builtin_type_t btyp) const;

protected:

  // The associative array used to manage the structure data.
  octave_scalar_map m_map;

private:

  octave_value to_array (void);

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
