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

#if ! defined (octave_ov_vm_h)
#define octave_ov_vm_h 1

#include "octave-config.h"

#include "ov.h"
#include "load-path.h"

#if defined (OCTAVE_ENABLE_BYTECODE_EVALUATOR)
// octave_value_vm is to be used only by the VM
// and need to have the same bit representation as
// an octave_value.
//
// A octave_value_vm object might not be assigned
// to itself or have a nullptr m_rep when being
// assigned to.

class octave_value_vm
{
public:
  octave_value_vm ()
      : m_rep (octave_value::nil_rep ())
  {
      m_rep->m_count++;
  }

  octave_value_vm (octave_base_value *rep, bool count_add1 = true)
    : m_rep (rep)
  {
    if (count_add1)
      m_rep->m_count++;
  }

  octave_value_vm (const octave_value_vm& a)
      : m_rep (a.m_rep)
  {
      m_rep->m_count++;
  }
  octave_value_vm (const octave_value& a)
      : m_rep (a.m_rep)
  {
      m_rep->m_count++;
  }

  octave_value_vm (octave_value_vm&& a)
  : m_rep (a.m_rep)
  {
      a.m_rep = nullptr;
  }
  octave_value_vm (octave_value&& a)
  : m_rep (a.m_rep)
  {
      a.m_rep = nullptr;
  }

  ~octave_value_vm () __attribute__ ((always_inline))
  {
      // Because we define a move constructor and a move assignment
      // operator, rep may be a nullptr here.  We should only need to
      // protect the move assignment operator in a similar way.

    if (m_rep && --m_rep->m_count == 0)
      delete m_rep;
  }

  octave_value_vm& operator = (const octave_value_vm& a)
  {
    if (--m_rep->m_count == 0)
      delete m_rep;

    m_rep = a.m_rep;
    m_rep->m_count++;

    return *this;
  }

  octave_value_vm& operator = (octave_value_vm&& a)
  {
    if (--m_rep->m_count == 0)
      delete m_rep;

    m_rep = a.m_rep;
    a.m_rep = nullptr;

    return *this;
  }

  octave_value_vm& operator = (octave_value&& a)
  {
    if (--m_rep->m_count == 0)
      delete m_rep;

    m_rep = a.m_rep;
    a.m_rep = nullptr;

    return *this;
  }

  static void release_rep (octave_base_value *rep)
  {
    if (--rep->m_count == 0)
      delete rep;
  }

  void steal_ov_rep (octave_value &&ov)
  {
    if (m_rep && --m_rep->m_count == 0)
      delete m_rep;

    m_rep = ov.m_rep;
    ov.m_rep = nullptr;
  }

  octave_base_value & get_rep () { return *m_rep; }

  octave_value_vm& operator = (octave_base_value *rep)
  {
    if (--m_rep->m_count == 0)
      delete m_rep;

    m_rep = rep;

    return *this;
  }

  bool vm_need_dispatch_push () __attribute__ ((pure, always_inline, nothrow))
  { return m_rep->vm_need_dispatch_push (); }

  bool vm_need_dispatch_assign_rhs () __attribute__ ((pure, always_inline, nothrow))
  { return m_rep->vm_need_dispatch_assign_rhs (); }

  bool vm_need_dispatch_assign_lhs () __attribute__ ((pure, always_inline, nothrow))
  { return m_rep->vm_need_dispatch_assign_lhs (); }

  int type_id() const __attribute__ ((pure, always_inline, nothrow))
  { return m_rep->type_id (); }

  bool is_matrix_type () const __attribute__ ((pure, always_inline, nothrow))
  { return m_rep->is_matrix_type (); }

  octave_base_value *m_rep;
};

class
octave_cached_value : public octave_base_value
{
public:

  octave_cached_value ()
  {
    m_n_updated = octave::load_path::get_weak_n_updated ();
  }

  void set_cached_obj (octave_value cache_obj)
  {
    m_cached_object = cache_obj;
  }

  octave_value get_cached_value ()
  {
    return m_cached_object;
  }

  bool cache_is_valid ()
  {
    return m_n_updated == octave::load_path::get_weak_n_updated () && m_cached_object.is_defined ();
  }

  bool is_defined () const { return true; }


private:

  octave_value m_cached_object;
  octave_idx_type m_n_updated = 0;
};

// Class that is a wrapper around arguments and subsref type (i.e. '(','{' and '.')
// that are needed for building up the args for a classdef subsref. The object
// that will be called is set in the constructor of the wrapper.
//
// The reason it is in an octave_value is to make unwinding of the VM stack easier.
//
// INDEX_STRUCT_SUBCALL adds one set of args and type for each execution of itself,
// and the last INDEX_STRUCT_SUBCALL makes a subsref of the classdef object with all
// the args.
class octave_vm_chainargs_wrapper : public octave_base_value
{
public:
  octave_vm_chainargs_wrapper (octave_value obj_to_call) : m_obj_to_call (obj_to_call) {}

  // Invalid to call after steal
  void append_args (octave_value_list &&ovl)
  {
    m_idxs.push_back (ovl);
  }

  // Invalid to call after steal
  void append_type (char type)
  {
    m_types.push_back (type);
  }

  bool is_vm_chainargs_wrapper () const { return true; }

  // Only callable once
  std::list<octave_value_list> && steal_idxs () { return std::move (m_idxs); }
  std::string && steal_types () { return std::move (m_types); }
  octave_value && steal_obj_to_call () { return std::move (m_obj_to_call); }

private:
  std::list<octave_value_list> m_idxs;
  std::string m_types;
  octave_value m_obj_to_call;
};

#endif

#endif
