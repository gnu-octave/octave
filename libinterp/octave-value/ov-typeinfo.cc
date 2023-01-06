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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <iostream>

#include "Array.h"

#include "defun.h"
#include "error.h"
#include "interpreter.h"
#include "interpreter-private.h"
#include "ov-typeinfo.h"
#include "ov.h"

OCTAVE_BEGIN_NAMESPACE(octave)

extern void install_ops (type_info& ti);

// FIXME: we should also store all class names and provide a
// way to list them (calling class with nargin == 0?).

static NDArray as_nd_array (const Array<int>& x)
{
  NDArray retval (x.dims ());

  for (int i = 0; i < x.numel (); i++)
    retval.xelem(i) = x(i);

  return retval;
}

static boolNDArray as_bool_nd_array (const Array<void *>& x)
{
  boolNDArray retval (x.dims ());

  for (octave_idx_type i = 0; i < x.numel (); i++)
    retval.xelem (i) = x(i);

  return retval;
}

type_info::type_info (int init_tab_sz)
  : m_num_types (0), m_types (dim_vector (init_tab_sz, 1), ""),
    m_vals (dim_vector (init_tab_sz, 1)),
    m_unary_class_ops (dim_vector (octave_value::num_unary_ops, 1), nullptr),
    m_unary_ops (dim_vector (octave_value::num_unary_ops, init_tab_sz), nullptr),
    m_non_const_unary_ops (dim_vector (octave_value::num_unary_ops, init_tab_sz), nullptr),
    m_binary_class_ops (dim_vector (octave_value::num_binary_ops, 1), nullptr),
    m_binary_ops (dim_vector (octave_value::num_binary_ops, init_tab_sz, init_tab_sz), nullptr),
    m_compound_binary_class_ops (dim_vector (octave_value::num_compound_binary_ops, 1), nullptr),
    m_compound_binary_ops (dim_vector (octave_value::num_compound_binary_ops, init_tab_sz, init_tab_sz),
                           nullptr),
    m_cat_ops (dim_vector (init_tab_sz, init_tab_sz), nullptr),
    m_assign_ops (dim_vector (octave_value::num_assign_ops, init_tab_sz, init_tab_sz), nullptr),
    m_assignany_ops (dim_vector (octave_value::num_assign_ops, init_tab_sz), nullptr),
    m_pref_assign_conv (dim_vector (init_tab_sz, init_tab_sz), -1),
    m_widening_ops (dim_vector (init_tab_sz, init_tab_sz), nullptr)
{
  install_types (*this);

  install_ops (*this);
}

int type_info::register_type (const std::string& t_name,
                              const std::string& /* c_name */,
                              const octave_value& val,
                              bool abort_on_duplicate)
{
  int i = 0;

  for (i = 0; i < m_num_types; i++)
    {
      if (t_name == m_types (i))
        {
          if (abort_on_duplicate)
            {
              std::cerr << "duplicate type " << t_name << std::endl;
              abort ();
            }

          warning ("duplicate type %s\n", t_name.c_str ());

          return i;
        }
    }

  int len = m_types.numel ();

  if (i == len)
    {
      len *= 2;

      m_types.resize (dim_vector (len, 1), "");

      m_vals.resize (dim_vector (len, 1), nullptr);

      m_unary_ops.resize
      (dim_vector (octave_value::num_unary_ops, len), nullptr);

      m_non_const_unary_ops.resize
      (dim_vector (octave_value::num_unary_ops, len), nullptr);

      m_binary_ops.resize
      (dim_vector (octave_value::num_binary_ops, len, len), nullptr);

      m_compound_binary_ops.resize
      (dim_vector (octave_value::num_compound_binary_ops, len, len),
       nullptr);

      m_cat_ops.resize (dim_vector (len, len), nullptr);

      m_assign_ops.resize
      (dim_vector (octave_value::num_assign_ops, len, len), nullptr);

      m_assignany_ops.resize
      (dim_vector (octave_value::num_assign_ops, len), nullptr);

      m_pref_assign_conv.resize (dim_vector (len, len), -1);

      m_widening_ops.resize (dim_vector (len, len), nullptr);
    }

  m_types (i) = t_name;

  // Yes, this object is intentionally not deleted in the destructor
  // so that we avoid a crash on exit for user-defined data types.
  // See bug #53156.  If that problem is properly fixed, then this
  // could be stored as an object instead of a pointer to an object
  // allocated with new.

  m_vals(i) = new octave_value (val);

  m_num_types++;

  return i;
}

bool type_info::register_unary_class_op (octave_value::unary_op op,
    type_info::unary_class_op_fcn f,
    bool abort_on_duplicate)
{
  if (lookup_unary_class_op (op))
    {
      std::string op_name = octave_value::unary_op_as_string (op);

      if (abort_on_duplicate)
        {
          std::cerr << "duplicate unary operator '" << op_name
                    << "' for class dispatch" << std::endl;
          abort ();
        }

      warning ("duplicate unary operator '%s' for class dispatch",
               op_name.c_str ());
    }

  m_unary_class_ops.checkelem (static_cast<int> (op))
    = reinterpret_cast<void *> (f);

  return false;
}

bool type_info::register_unary_op (octave_value::unary_op op, int t,
                                   unary_op_fcn f, bool abort_on_duplicate)
{
  if (lookup_unary_op (op, t))
    {
      std::string op_name = octave_value::unary_op_as_string (op);
      std::string type_name = m_types(t);

      if (abort_on_duplicate)
        {
          std::cerr << "duplicate unary operator '" << op_name
                    << "' for type '" << type_name << "'" << std::endl;
          abort ();
        }

      warning ("duplicate unary operator '%s' for type '%s'",
               op_name.c_str (), type_name.c_str ());
    }

  m_unary_ops.checkelem (static_cast<int> (op), t) = reinterpret_cast<void *> (f);

  return false;
}

bool
type_info::register_non_const_unary_op (octave_value::unary_op op, int t,
                                        type_info::non_const_unary_op_fcn f,
                                        bool abort_on_duplicate)
{
  if (lookup_non_const_unary_op (op, t))
    {
      std::string op_name = octave_value::unary_op_as_string (op);
      std::string type_name = m_types(t);

      if (abort_on_duplicate)
        {
          std::cerr << "duplicate unary operator '" << op_name
                    << "' for type '" << type_name << "'" << std::endl;
          abort ();
        }

      warning ("duplicate unary operator '%s' for type '%s'",
               op_name.c_str (), type_name.c_str ());
    }

  m_non_const_unary_ops.checkelem (static_cast<int> (op), t)
    = reinterpret_cast<void *> (f);

  return false;
}

bool
type_info::register_binary_class_op (octave_value::binary_op op,
                                     type_info::binary_class_op_fcn f,
                                     bool abort_on_duplicate)
{
  if (lookup_binary_class_op (op))
    {
      std::string op_name = octave_value::binary_op_as_string (op);

      if (abort_on_duplicate)
        {

          std::cerr << "duplicate binary operator '" << op_name
                    << "' for class dispatch" << std::endl;
          abort ();
        }

      warning ("duplicate binary operator '%s' for class dispatch",
               op_name.c_str ());
    }

  m_binary_class_ops.checkelem (static_cast<int> (op))
    = reinterpret_cast<void *> (f);

  return false;
}

bool type_info::register_binary_op (octave_value::binary_op op,
                                    int t1, int t2,
                                    type_info::binary_op_fcn f,
                                    bool abort_on_duplicate)
{
  if (lookup_binary_op (op, t1, t2))
    {
      std::string op_name = octave_value::binary_op_as_string (op);
      std::string t1_name = m_types(t1);
      std::string t2_name = m_types(t2);

      if (abort_on_duplicate)
        {
          std::cerr << "duplicate binary operator '" << op_name
                    << "' for types '" << t1_name << "' and '"
                    << t2_name << "'" << std::endl;
          abort ();
        }

      warning ("duplicate binary operator '%s' for types '%s' and '%s'",
               op_name.c_str (), t1_name.c_str (), t1_name.c_str ());
    }

  m_binary_ops.checkelem (static_cast<int> (op), t1, t2)
    = reinterpret_cast<void *> (f);

  return false;
}

bool
type_info::register_binary_class_op (octave_value::compound_binary_op op,
                                     type_info::binary_class_op_fcn f,
                                     bool abort_on_duplicate)
{
  if (lookup_binary_class_op (op))
    {
      std::string op_name = octave_value::binary_op_fcn_name (op);

      if (abort_on_duplicate)
        {
          std::cerr << "duplicate compound binary operator '"
                    << op_name << "' for class dispatch" << std::endl;
          abort ();
        }

      warning ("duplicate compound binary operator '%s' for class dispatch",
               op_name.c_str ());
    }

  m_compound_binary_class_ops.checkelem (static_cast<int> (op))
    = reinterpret_cast<void *> (f);

  return false;
}

bool type_info::register_binary_op (octave_value::compound_binary_op op,
                                    int t1, int t2,
                                    type_info::binary_op_fcn f,
                                    bool abort_on_duplicate)
{
  if (lookup_binary_op (op, t1, t2))
    {
      std::string op_name = octave_value::binary_op_fcn_name (op);
      std::string t1_name = m_types(t1);
      std::string t2_name = m_types(t2);

      if (abort_on_duplicate)
        {
          std::cerr << "duplicate compound binary operator '"
                    << op_name << "' for types '" << t1_name
                    << "' and '" << t2_name << "'" << std::endl;
          abort ();
        }

      warning ("duplicate compound binary operator '%s' for types '%s' and '%s'",
               op_name.c_str (), t1_name.c_str (), t1_name.c_str ());
    }

  m_compound_binary_ops.checkelem (static_cast<int> (op), t1, t2)
    = reinterpret_cast<void *> (f);

  return false;
}

bool type_info::register_cat_op (int t1, int t2, type_info::cat_op_fcn f,
                                 bool abort_on_duplicate)
{
  if (lookup_cat_op (t1, t2))
    {
      std::string t1_name = m_types(t1);
      std::string t2_name = m_types(t2);

      if (abort_on_duplicate)
        {
          std::cerr << "duplicate concatenation operator for types '"
                    << t1_name << "' and '" << t2_name << "'" << std::endl;
          abort ();
        }

      warning ("duplicate concatenation operator for types '%s' and '%s'",
               t1_name.c_str (), t1_name.c_str ());
    }

  m_cat_ops.checkelem (t1, t2) = reinterpret_cast<void *> (f);

  return false;
}

bool type_info::register_assign_op (octave_value::assign_op op,
                                    int t_lhs, int t_rhs,
                                    type_info::assign_op_fcn f,
                                    bool abort_on_duplicate)
{
  if (lookup_assign_op (op, t_lhs, t_rhs))
    {
      std::string op_name = octave_value::assign_op_as_string (op);
      std::string t_lhs_name = m_types(t_lhs);
      std::string t_rhs_name = m_types(t_rhs);

      if (abort_on_duplicate)
        {
          std::cerr << "duplicate assignment operator '"
                    << op_name << "' for types '" << t_lhs_name
                    << "' and '" << t_rhs_name << "'" << std::endl;
          abort ();
        }

      warning ("duplicate assignment operator '%s' for types '%s' and '%s'",
               op_name.c_str (), t_lhs_name.c_str (), t_rhs_name.c_str ());
    }

  m_assign_ops.checkelem (static_cast<int> (op), t_lhs, t_rhs)
    = reinterpret_cast<void *> (f);

  return false;
}

bool type_info::register_assignany_op (octave_value::assign_op op, int t_lhs,
                                       type_info::assignany_op_fcn f,
                                       bool abort_on_duplicate)
{
  if (lookup_assignany_op (op, t_lhs))
    {
      std::string op_name = octave_value::assign_op_as_string (op);
      std::string t_lhs_name = m_types(t_lhs);

      if (abort_on_duplicate)
        {
          std::cerr << "duplicate assignment operator '" << op_name
                    << "' for types '" << t_lhs_name << "'" << std::endl;
          abort ();
        }

      warning ("duplicate assignment operator '%s' for types '%s'",
               op_name.c_str (), t_lhs_name.c_str ());
    }

  m_assignany_ops.checkelem (static_cast<int> (op), t_lhs)
    = reinterpret_cast<void *> (f);

  return false;
}

bool type_info::register_pref_assign_conv (int t_lhs, int t_rhs,
    int t_result,
    bool abort_on_duplicate)
{
  if (lookup_pref_assign_conv (t_lhs, t_rhs) >= 0)
    {
      std::string t_lhs_name = m_types(t_lhs);
      std::string t_rhs_name = m_types(t_rhs);

      if (abort_on_duplicate)
        {
          std::cerr << "overriding assignment conversion for types '"
                    << t_lhs_name << "' and '" << t_rhs_name << "'"
                    << std::endl;
          abort ();
        }

      warning ("overriding assignment conversion for types '%s' and '%s'",
               t_lhs_name.c_str (), t_rhs_name.c_str ());
    }

  m_pref_assign_conv.checkelem (t_lhs, t_rhs) = t_result;

  return false;
}

bool type_info::register_widening_op (int t, int t_result,
                                      octave_base_value::type_conv_fcn f,
                                      bool abort_on_duplicate)
{
  if (lookup_widening_op (t, t_result))
    {
      std::string t_name = m_types(t);
      std::string t_result_name = m_types(t_result);

      if (abort_on_duplicate)
        {
          std::cerr << "overriding widening op for '" << t_name
                    << "' to '" << t_result_name << "'" << std::endl;
          abort ();
        }

      warning ("overriding widening op for '%s' to '%s'",
               t_name.c_str (), t_result_name.c_str ());
    }

  m_widening_ops.checkelem (t, t_result) = reinterpret_cast<void *> (f);

  return false;
}

octave_value type_info::lookup_type (const std::string& nm)
{
  octave_value retval;

  for (int i = 0; i < m_num_types; i++)
    {
      if (nm == m_types(i))
        {
          retval = *m_vals(i);
          retval.make_unique ();
          break;
        }
    }

  return retval;
}

type_info::unary_class_op_fcn
type_info::lookup_unary_class_op (octave_value::unary_op op)
{
  void *f = m_unary_class_ops.checkelem (static_cast<int> (op));
  return reinterpret_cast<type_info::unary_class_op_fcn> (f);
}

type_info::unary_op_fcn
type_info::lookup_unary_op (octave_value::unary_op op, int t)
{
  void *f = m_unary_ops.checkelem (static_cast<int> (op), t);
  return reinterpret_cast<type_info::unary_op_fcn> (f);
}

type_info::non_const_unary_op_fcn
type_info::lookup_non_const_unary_op (octave_value::unary_op op, int t)
{
  void *f = m_non_const_unary_ops.checkelem (static_cast<int> (op), t);
  return reinterpret_cast<type_info::non_const_unary_op_fcn> (f);
}

type_info::binary_class_op_fcn
type_info::lookup_binary_class_op (octave_value::binary_op op)
{
  void *f = m_binary_class_ops.checkelem (static_cast<int> (op));
  return reinterpret_cast<type_info::binary_class_op_fcn> (f);
}

type_info::binary_op_fcn
type_info::lookup_binary_op (octave_value::binary_op op, int t1, int t2)
{
  void *f = m_binary_ops.checkelem (static_cast<int> (op), t1, t2);
  return reinterpret_cast<type_info::binary_op_fcn> (f);
}

type_info::binary_class_op_fcn
type_info::lookup_binary_class_op (octave_value::compound_binary_op op)
{
  void *f = m_compound_binary_class_ops.checkelem (static_cast<int> (op));
  return reinterpret_cast<type_info::binary_class_op_fcn> (f);
}

type_info::binary_op_fcn
type_info::lookup_binary_op (octave_value::compound_binary_op op,
                             int t1, int t2)
{
  void *f = m_compound_binary_ops.checkelem (static_cast<int> (op), t1, t2);
  return reinterpret_cast<type_info::binary_op_fcn> (f);
}

type_info::cat_op_fcn
type_info::lookup_cat_op (int t1, int t2)
{
  void *f = m_cat_ops.checkelem (t1, t2);
  return reinterpret_cast<type_info::cat_op_fcn> (f);
}

type_info::assign_op_fcn
type_info::lookup_assign_op (octave_value::assign_op op,
                             int t_lhs, int t_rhs)
{
  void *f = m_assign_ops.checkelem (static_cast<int> (op), t_lhs, t_rhs);
  return reinterpret_cast<type_info::assign_op_fcn> (f);
}

type_info::assignany_op_fcn
type_info::lookup_assignany_op (octave_value::assign_op op, int t_lhs)
{
  void *f = m_assignany_ops.checkelem (static_cast<int> (op), t_lhs);
  return reinterpret_cast<type_info::assignany_op_fcn> (f);
}

int
type_info::lookup_pref_assign_conv (int t_lhs, int t_rhs)
{
  return m_pref_assign_conv.checkelem (t_lhs, t_rhs);
}

octave_base_value::type_conv_fcn
type_info::lookup_widening_op (int t, int t_result)
{
  void *f = m_widening_ops.checkelem (t, t_result);
  return reinterpret_cast<octave_base_value::type_conv_fcn> (f);
}

string_vector
type_info::installed_type_names (void) const
{
  string_vector retval (m_num_types);

  for (int i = 0; i < m_num_types; i++)
    retval(i) = m_types(i);

  return retval;
}

octave_scalar_map
type_info::unary_ops_map (void) const
{
  octave_scalar_map retval;

  int len = std::min (static_cast<int> (m_non_const_unary_ops.columns ()),
                      m_num_types);

  dim_vector tab_dims (1, len);

  for (int j = 0; j < octave_value::num_unary_ops; j++)
    {
      boolNDArray tab (tab_dims);

      for (int i = 0; i < len; i++)
        tab.xelem (i) = (m_unary_ops(j, i) != nullptr);

      octave_value::unary_op op_id = static_cast<octave_value::unary_op> (j);

      retval.setfield (octave_value::unary_op_as_string (op_id), tab);
    }

  return retval;
}

octave_scalar_map
type_info::non_const_unary_ops_map (void) const
{
  octave_scalar_map retval;

  int len = std::min (static_cast<int> (m_non_const_unary_ops.columns ()),
                      m_num_types);

  dim_vector tab_dims (1, len);

  for (int j = 0; j < octave_value::num_unary_ops; j++)
    {
      boolNDArray tab (tab_dims);

      for (int i = 0; i < len; i++)
        tab.xelem (i) = (m_non_const_unary_ops(j, i) != nullptr);

      octave_value::unary_op op_id = static_cast<octave_value::unary_op> (j);

      retval.setfield (octave_value::unary_op_as_string (op_id), tab);
    }

  return retval;
}

octave_scalar_map
type_info::binary_ops_map (void) const
{
  octave_scalar_map retval;

  int len = std::min (static_cast<int> (m_binary_ops.columns ()),
                      m_num_types);

  dim_vector tab_dims (len, len);

  for (int k = 0; k < octave_value::num_binary_ops; k++)
    {
      boolNDArray tab (tab_dims);

      for (int j = 0; j < len; j++)
        for (int i = 0; i < len; i++)
          tab.xelem (j, i) = (m_binary_ops(k, j, i) != nullptr);

      octave_value::binary_op op_id = static_cast<octave_value::binary_op> (k);

      retval.setfield (octave_value::binary_op_as_string (op_id), tab);
    }

  return retval;
}

octave_scalar_map
type_info::compound_binary_ops_map (void) const
{
  octave_scalar_map retval;

  int len = std::min (static_cast<int> (m_compound_binary_ops.columns ()),
                      m_num_types);

  dim_vector tab_dims (len, len);

  for (int k = 0; k < octave_value::num_compound_binary_ops; k++)
    {
      boolNDArray tab (tab_dims);

      for (int j = 0; j < len; j++)
        for (int i = 0; i < len; i++)
          tab.xelem (j, i) = (m_compound_binary_ops(k, j, i) != nullptr);

      octave_value::compound_binary_op op_id
        = static_cast<octave_value::compound_binary_op> (k);

      retval.setfield (octave_value::binary_op_fcn_name (op_id), tab);
    }

  return retval;
}

octave_scalar_map
type_info::assign_ops_map (void) const
{
  octave_scalar_map retval;

  int len = std::min (static_cast<int> (m_assign_ops.columns ()),
                      m_num_types);

  dim_vector tab_dims (len, len);

  for (int k = 0; k < octave_value::num_assign_ops; k++)
    {
      boolNDArray tab (tab_dims);

      for (int j = 0; j < len; j++)
        for (int i = 0; i < len; i++)
          tab.xelem (j, i) = (m_assign_ops(k, j, i) != nullptr);

      octave_value::assign_op op_id = static_cast<octave_value::assign_op> (k);

      retval.setfield (octave_value::assign_op_as_string (op_id), tab);
    }

  return retval;
}

octave_scalar_map
type_info::assignany_ops_map (void) const
{
  octave_scalar_map retval;

  int len = std::min (static_cast<int> (m_assignany_ops.columns ()),
                      m_num_types);

  dim_vector tab_dims (1, len);

  for (int j = 0; j < octave_value::num_assign_ops; j++)
    {
      boolNDArray tab (tab_dims);

      for (int i = 0; i < len; i++)
        tab.xelem (i) = (m_assignany_ops(j, i) != nullptr);

      octave_value::assign_op op_id = static_cast<octave_value::assign_op> (j);

      retval.setfield (octave_value::assign_op_as_string (op_id), tab);
    }

  return retval;
}

octave_scalar_map
type_info::installed_type_info (void) const
{
  octave_scalar_map retval;

  retval.setfield ("types", octave_value (Cell (installed_type_names ())));
  retval.setfield ("unary_ops", unary_ops_map ());
  retval.setfield ("non_const_unary_ops", non_const_unary_ops_map ());
  retval.setfield ("binary_ops", binary_ops_map ());
  retval.setfield ("compound_binary_ops", compound_binary_ops_map ());
  retval.setfield ("cat_ops", as_bool_nd_array (m_cat_ops));
  retval.setfield ("assign_ops", assign_ops_map ());
  retval.setfield ("assignany_ops", assignany_ops_map ());
  retval.setfield ("pref_assign_conv", as_nd_array (m_pref_assign_conv));
  retval.setfield ("widening_ops", as_bool_nd_array (m_widening_ops));

  return retval;
}

OCTAVE_END_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(octave_value_typeinfo)

int register_type (const std::string& t_name, const std::string& c_name,
                   const octave_value& val)
{
  octave::type_info& type_info = octave::__get_type_info__ ();

  return type_info.register_type (t_name, c_name, val);
}

octave_value lookup_type (const std::string& nm)
{
  octave::type_info& type_info = octave::__get_type_info__ ();

  return type_info.lookup_type (nm);
}

unary_class_op_fcn lookup_unary_class_op (octave_value::unary_op op)
{
  octave::type_info& type_info = octave::__get_type_info__ ();

  return type_info.lookup_unary_class_op (op);
}

unary_op_fcn lookup_unary_op (octave_value::unary_op op, int t)
{
  octave::type_info& type_info = octave::__get_type_info__ ();

  return type_info.lookup_unary_op (op, t);
}

non_const_unary_op_fcn
lookup_non_const_unary_op (octave_value::unary_op op, int t)
{
  octave::type_info& type_info = octave::__get_type_info__ ();

  return type_info.lookup_non_const_unary_op (op, t);
}

binary_class_op_fcn
lookup_binary_class_op (octave_value::binary_op op)
{
  octave::type_info& type_info = octave::__get_type_info__ ();

  return type_info.lookup_binary_class_op (op);
}

binary_op_fcn
lookup_binary_op (octave_value::binary_op op, int t1, int t2)
{
  octave::type_info& type_info = octave::__get_type_info__ ();

  return type_info.lookup_binary_op (op, t1, t2);
}

binary_class_op_fcn
lookup_binary_class_op (octave_value::compound_binary_op op)
{
  octave::type_info& type_info = octave::__get_type_info__ ();

  return type_info.lookup_binary_class_op (op);
}

binary_op_fcn
lookup_binary_op (octave_value::compound_binary_op op, int t1, int t2)
{
  octave::type_info& type_info = octave::__get_type_info__ ();

  return type_info.lookup_binary_op (op, t1, t2);
}

cat_op_fcn lookup_cat_op (int t1, int t2)
{
  octave::type_info& type_info = octave::__get_type_info__ ();

  return type_info.lookup_cat_op (t1, t2);
}

assign_op_fcn
lookup_assign_op (octave_value::assign_op op, int t_lhs, int t_rhs)
{
  octave::type_info& type_info = octave::__get_type_info__ ();

  return type_info.lookup_assign_op (op, t_lhs, t_rhs);
}

assignany_op_fcn
lookup_assignany_op (octave_value::assign_op op, int t_lhs)
{
  octave::type_info& type_info = octave::__get_type_info__ ();

  return type_info.lookup_assignany_op (op, t_lhs);
}

int lookup_pref_assign_conv (int t_lhs, int t_rhs)
{
  octave::type_info& type_info = octave::__get_type_info__ ();

  return type_info.lookup_pref_assign_conv (t_lhs, t_rhs);
}

octave_base_value::type_conv_fcn
lookup_widening_op (int t, int t_result)
{
  octave::type_info& type_info = octave::__get_type_info__ ();

  return type_info.lookup_widening_op (t, t_result);
}

string_vector installed_type_names (void)
{
  octave::type_info& type_info = octave::__get_type_info__ ();

  return type_info.installed_type_names ();
}

octave_scalar_map installed_type_info (void)
{
  octave::type_info& type_info = octave::__get_type_info__ ();

  return type_info.installed_type_info ();
}

OCTAVE_END_NAMESPACE(octave_value_typeinfo)

OCTAVE_BEGIN_NAMESPACE(octave)

DEFMETHOD (typeinfo, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{typestr} =} typeinfo (@var{expr})
@deftypefnx {} {@var{cstr} =} typeinfo ()

Return the type of the expression @var{expr}, as a string.

If @var{expr} is omitted, return a cell array of strings containing all the
currently installed data types.
@seealso{class, isa}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  if (nargin == 0)
    {
      type_info& type_info = interp.get_type_info ();

      return ovl (Cell (type_info.installed_type_names ()));
    }
  else
    return ovl (args(0).type_name ());
}

/*
%!assert (iscellstr (typeinfo ()))

%!assert (typeinfo ({"cell"}), "cell")

%!assert (typeinfo (1), "scalar")
%!assert (typeinfo (double (1)), "scalar")
%!assert (typeinfo (i), "complex scalar")

%!assert (typeinfo ([1, 2]), "matrix")
%!assert (typeinfo (double ([1, 2])), "matrix")
%!assert (typeinfo (diag ([1, 2])), "diagonal matrix")
%!assert (typeinfo ([i, 2]), "complex matrix")
%!assert (typeinfo (diag ([i, 2])), "complex diagonal matrix")

%!test
%! if (optimize_range ())
%!   assert (typeinfo (1:2), "double_range")
%! else
%!   assert (typeinfo (1:2), "matrix")
%! endif

%!assert (typeinfo (false), "bool")
%!assert (typeinfo ([true, false]), "bool matrix")

%!assert (typeinfo ("string"), "string")
%!assert (typeinfo ('string'), "sq_string")

%!assert (typeinfo (int8 (1)), "int8 scalar")
%!assert (typeinfo (int16 (1)), "int16 scalar")
%!assert (typeinfo (int32 (1)), "int32 scalar")
%!assert (typeinfo (int64 (1)), "int64 scalar")
%!assert (typeinfo (uint8 (1)), "uint8 scalar")
%!assert (typeinfo (uint16 (1)), "uint16 scalar")
%!assert (typeinfo (uint32 (1)), "uint32 scalar")
%!assert (typeinfo (uint64 (1)), "uint64 scalar")

%!assert (typeinfo (int8 ([1,2])), "int8 matrix")
%!assert (typeinfo (int16 ([1,2])), "int16 matrix")
%!assert (typeinfo (int32 ([1,2])), "int32 matrix")
%!assert (typeinfo (int64 ([1,2])), "int64 matrix")
%!assert (typeinfo (uint8 ([1,2])), "uint8 matrix")
%!assert (typeinfo (uint16 ([1,2])), "uint16 matrix")
%!assert (typeinfo (uint32 ([1,2])), "uint32 matrix")
%!assert (typeinfo (uint64 ([1,2])), "uint64 matrix")

%!assert (typeinfo (sparse ([true, false])), "sparse bool matrix")
%!assert (typeinfo (logical (sparse (i * eye (10)))), "sparse bool matrix")
%!assert (typeinfo (sparse ([1,2])), "sparse matrix")
%!assert (typeinfo (sparse (eye (10))), "sparse matrix")
%!assert (typeinfo (sparse ([i,2])), "sparse complex matrix")
%!assert (typeinfo (sparse (i * eye (10))), "sparse complex matrix")

%!test
%! s(2).a = 1;
%! assert (typeinfo (s), "struct");

%!test
%! s.a = 1;
%! assert (typeinfo (s), "scalar struct");

## FIXME: This doesn't work as a test for comma-separated list
%!#test
%! clist = {1, 2, 3};
%! assert (typeinfo (clist{:}), "cs-list");

%!assert (typeinfo (@sin), "function handle")
%!assert (typeinfo (@(x) x), "function handle")

%!assert (typeinfo (single (1)), "float scalar")
%!assert (typeinfo (single (i)), "float complex scalar")
%!assert (typeinfo (single ([1, 2])), "float matrix")

%!assert (typeinfo (single (diag ([1, 2]))), "float diagonal matrix")
%!assert (typeinfo (diag (single ([1, 2]))), "float diagonal matrix")
%!assert (typeinfo (single (diag ([i, 2]))), "float complex diagonal matrix")
%!assert (typeinfo (diag (single ([i, 2]))), "float complex diagonal matrix")

%!assert (typeinfo (eye(3)(:,[1 3 2])), "permutation matrix")
%!test
%! [l, u, p] = lu (rand (3));
%! assert (typeinfo (p), "permutation matrix");

%!assert (typeinfo ([]), "null_matrix")
%!assert (typeinfo (""), "null_string")
%!assert (typeinfo (''), "null_sq_string")

%!test
%! cvar = onCleanup (@() "");
%! assert (typeinfo (cvar), "onCleanup");

%!testif HAVE_JAVA; usejava ("jvm")
%! x = javaObject ("java.lang.StringBuffer");
%! assert (typeinfo (x), "octave_java");

## Test input validation
%!error typeinfo ("foo", 1)
*/

DEFMETHOD (__dump_typeinfo__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __dump_typeinfo__ ()
Undocumented internal function.
@end deftypefn */)
{
  if (args.length () > 0)
    print_usage ();

  type_info& type_info = interp.get_type_info ();

  return ovl (type_info.installed_type_info ());
}

OCTAVE_END_NAMESPACE(octave)
