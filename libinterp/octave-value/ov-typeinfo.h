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

#if ! defined (octave_ov_typeinfo_h)
#define octave_ov_typeinfo_h 1

#include "octave-config.h"

#include <string>

#include "Array.h"

#include "oct-map.h"
#include "ov.h"

class string_vector;

OCTAVE_BEGIN_NAMESPACE(octave)

class
OCTINTERP_API
type_info
{
public:

  typedef octave_value (*unary_class_op_fcn) (const octave_value&);

  typedef octave_value (*unary_op_fcn) (const octave_base_value&);

  typedef void (*non_const_unary_op_fcn) (octave_base_value&);

  typedef octave_value (*binary_class_op_fcn)
    (const octave_value&, const octave_value&);

  typedef octave_value (*binary_op_fcn)
    (const octave_base_value&, const octave_base_value&);

  typedef octave_value (*cat_op_fcn)
    (const octave_base_value&, const octave_base_value&,
   const Array<octave_idx_type>& ra_idx);

  typedef octave_value (*assign_op_fcn)
    (octave_base_value&, const octave_value_list&, const octave_base_value&);

  typedef octave_value (*assignany_op_fcn)
    (octave_base_value&, const octave_value_list&, const octave_value&);

  explicit type_info (int init_tab_sz = 16);

  // No copying!

  type_info (const type_info&) = delete;

  type_info& operator = (const type_info&) = delete;

  ~type_info (void) = default;

  // It is intentional that there is no install_type function.

  bool install_unary_class_op (octave_value::unary_op op,
                               unary_class_op_fcn f)
  {
    return register_unary_class_op (op, f, true);
  }

  bool install_unary_op (octave_value::unary_op op, int t, unary_op_fcn f)
  {
    return register_unary_op (op, t, f, true);
  }

  bool install_non_const_unary_op (octave_value::unary_op op, int t,
                                   non_const_unary_op_fcn f)
  {
    return register_non_const_unary_op (op, t, f, true);
  }

  bool install_binary_class_op (octave_value::binary_op op,
                                binary_class_op_fcn f)
  {
    return register_binary_class_op (op, f, true);
  }

  bool install_binary_op (octave_value::binary_op op, int t1, int t2,
                          binary_op_fcn f)
  {
    return register_binary_op (op, t1, t2, f, true);
  }

  bool install_binary_class_op (octave_value::compound_binary_op op,
                                binary_class_op_fcn f)
  {
    return register_binary_class_op (op, f, true);
  }

  bool install_binary_op (octave_value::compound_binary_op op,
                          int t_lhs, int t_rhs, binary_op_fcn f)
  {
    return register_binary_op (op, t_lhs, t_rhs, f, true);
  }

  bool install_cat_op (int t1, int t2, cat_op_fcn f)
  {
    return register_cat_op (t1, t2, f, true);
  }

  bool install_assign_op (octave_value::assign_op op,
                          int t_lhs, int t_rhs, assign_op_fcn f)
  {
    return register_assign_op (op, t_lhs, t_rhs, f, true);
  }

  bool install_assignany_op (octave_value::assign_op op, int t_lhs,
                             assignany_op_fcn f)
  {
    return register_assignany_op (op, t_lhs, f, true);
  }

  bool install_pref_assign_conv (int t_lhs, int t_rhs, int t_result)
  {
    return register_pref_assign_conv (t_lhs, t_rhs, t_result, true);
  }

  bool install_widening_op (int t, int t_result,
                            octave_base_value::type_conv_fcn f)
  {
    return register_widening_op (t, t_result, f, true);
  }

  int register_type (const std::string&, const std::string&,
                     const octave_value&, bool abort_on_duplicate = false);

  bool register_unary_class_op (octave_value::unary_op, unary_class_op_fcn,
                                bool abort_on_duplicate = false);

  bool register_unary_op (octave_value::unary_op, int, unary_op_fcn,
                          bool abort_on_duplicate = false);

  bool register_non_const_unary_op (octave_value::unary_op, int,
                                    non_const_unary_op_fcn,
                                    bool abort_on_duplicate = false);

  bool register_binary_class_op (octave_value::binary_op,
                                 binary_class_op_fcn,
                                 bool abort_on_duplicate = false);

  bool register_binary_op (octave_value::binary_op, int, int,
                           binary_op_fcn, bool abort_on_duplicate = false);

  bool register_binary_class_op (octave_value::compound_binary_op,
                                 binary_class_op_fcn,
                                 bool abort_on_duplicate = false);

  bool register_binary_op (octave_value::compound_binary_op, int, int,
                           binary_op_fcn, bool abort_on_duplicate = false);

  bool register_cat_op (int, int, cat_op_fcn,
                        bool abort_on_duplicate = false);

  bool register_assign_op (octave_value::assign_op, int, int, assign_op_fcn,
                           bool abort_on_duplicate = false);

  bool register_assignany_op (octave_value::assign_op, int, assignany_op_fcn,
                              bool abort_on_duplicate = false);

  bool register_pref_assign_conv (int, int, int,
                                  bool abort_on_duplicate = false);

  bool register_widening_op (int, int, octave_base_value::type_conv_fcn,
                             bool abort_on_duplicate = false);

  octave_value lookup_type (const std::string& nm);

  unary_class_op_fcn lookup_unary_class_op (octave_value::unary_op);

  unary_op_fcn lookup_unary_op (octave_value::unary_op, int);

  non_const_unary_op_fcn
  lookup_non_const_unary_op (octave_value::unary_op, int);

  binary_class_op_fcn lookup_binary_class_op (octave_value::binary_op);

  binary_op_fcn lookup_binary_op (octave_value::binary_op, int, int);

  binary_class_op_fcn
  lookup_binary_class_op (octave_value::compound_binary_op);

  binary_op_fcn
  lookup_binary_op (octave_value::compound_binary_op, int, int);

  cat_op_fcn lookup_cat_op (int, int);

  assign_op_fcn lookup_assign_op (octave_value::assign_op, int, int);

  assignany_op_fcn lookup_assignany_op (octave_value::assign_op, int);

  int lookup_pref_assign_conv (int, int);

  octave_base_value::type_conv_fcn lookup_widening_op (int, int);

  string_vector installed_type_names (void) const;

  octave_scalar_map installed_type_info (void) const;

  octave_scalar_map unary_ops_map (void) const;

  octave_scalar_map non_const_unary_ops_map (void) const;

  octave_scalar_map binary_ops_map (void) const;

  octave_scalar_map compound_binary_ops_map (void) const;

  octave_scalar_map assign_ops_map (void) const;

  octave_scalar_map assignany_ops_map (void) const;

private:

  int m_num_types;

  Array<std::string> m_types;

  Array<octave_value *> m_vals;

  Array<void *> m_unary_class_ops;

  Array<void *> m_unary_ops;

  Array<void *> m_non_const_unary_ops;

  Array<void *> m_binary_class_ops;

  Array<void *> m_binary_ops;

  Array<void *> m_compound_binary_class_ops;

  Array<void *> m_compound_binary_ops;

  Array<void *> m_cat_ops;

  Array<void *> m_assign_ops;

  Array<void *> m_assignany_ops;

  Array<int> m_pref_assign_conv;

  Array<void *> m_widening_ops;
};

OCTAVE_END_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(octave_value_typeinfo)

typedef octave::type_info::unary_class_op_fcn unary_class_op_fcn;

typedef octave::type_info::unary_op_fcn unary_op_fcn;

typedef octave::type_info::non_const_unary_op_fcn non_const_unary_op_fcn;

typedef octave::type_info::binary_class_op_fcn binary_class_op_fcn;

typedef octave::type_info::binary_op_fcn binary_op_fcn;

typedef octave::type_info::cat_op_fcn cat_op_fcn;

typedef octave::type_info::assign_op_fcn assign_op_fcn;

typedef octave::type_info::assignany_op_fcn assignany_op_fcn;

extern OCTINTERP_API int register_type (const std::string& t_name,
                                        const std::string& c_name,
                                        const octave_value& val);

extern OCTINTERP_API octave_value lookup_type (const std::string& nm);

extern OCTINTERP_API unary_class_op_fcn
lookup_unary_class_op (octave_value::unary_op op);

extern OCTINTERP_API unary_op_fcn
lookup_unary_op (octave_value::unary_op op, int t);

extern OCTINTERP_API non_const_unary_op_fcn
lookup_non_const_unary_op (octave_value::unary_op op, int t);

extern OCTINTERP_API binary_class_op_fcn
lookup_binary_class_op (octave_value::binary_op op);

extern OCTINTERP_API binary_op_fcn
lookup_binary_op (octave_value::binary_op op, int t1, int t2);

extern OCTINTERP_API binary_class_op_fcn
lookup_binary_class_op (octave_value::compound_binary_op op);

extern OCTINTERP_API binary_op_fcn
lookup_binary_op (octave_value::compound_binary_op op, int t1, int t2);

extern OCTINTERP_API cat_op_fcn lookup_cat_op (int t1, int t2);

extern OCTINTERP_API assign_op_fcn
lookup_assign_op (octave_value::assign_op op, int t_lhs, int t_rhs);

extern OCTINTERP_API assignany_op_fcn
lookup_assignany_op (octave_value::assign_op op, int t_lhs);

extern OCTINTERP_API int lookup_pref_assign_conv (int t_lhs, int t_rhs);

extern OCTINTERP_API octave_base_value::type_conv_fcn
lookup_widening_op (int t, int t_result);

extern OCTINTERP_API string_vector installed_type_names (void);

extern OCTINTERP_API octave_scalar_map installed_type_info (void);

OCTAVE_END_NAMESPACE(octave_value_typeinfo)

#endif
