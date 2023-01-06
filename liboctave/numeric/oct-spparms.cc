////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1998-2023 The Octave Project Developers
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

#include <ostream>

#include "Array.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "oct-spparms.h"
#include "singleton-cleanup.h"

OCTAVE_BEGIN_NAMESPACE(octave)

sparse_params *sparse_params::s_instance = nullptr;

bool sparse_params::instance_ok (void)
{
  bool retval = true;

  if (! s_instance)
    {
      s_instance = new sparse_params ();
      singleton_cleanup_list::add (cleanup_instance);
    }

  return retval;
}

void sparse_params::defaults (void)
{
  if (instance_ok ())
    s_instance->do_defaults ();
}

void sparse_params::tight (void)
{
  if (instance_ok ())
    s_instance->do_tight ();
}

string_vector sparse_params::get_keys (void)
{
  return instance_ok () ? s_instance->do_get_keys () : string_vector ();
}

ColumnVector sparse_params::get_vals (void)
{
  return instance_ok () ? s_instance->do_get_vals () : ColumnVector ();
}

bool sparse_params::set_vals (const Array<double>& vals)
{
  return instance_ok () ? s_instance->do_set_vals (vals) : false;
}

bool sparse_params::set_key (const std::string& key, const double& val)
{
  return instance_ok () ? s_instance->do_set_key (key, val) : false;
}

double sparse_params::get_key (const std::string& key)
{
  return (instance_ok ()
          ? s_instance->do_get_key (key) : numeric_limits<double>::NaN ());
}

double sparse_params::get_bandden (void)
{
  return instance_ok () ? s_instance->do_get_bandden () : 0.0;
}

void sparse_params::print_info (std::ostream& os, const std::string& prefix)
{
  if (instance_ok ())
    s_instance->do_print_info (os, prefix);
}

void sparse_params::do_defaults (void)
{
  m_params(0) = 0;      // spumoni
  m_params(1) = 1;      // ths_rel
  m_params(2) = 1;      // ths_abs
  m_params(3) = 0;      // exact_d
  m_params(4) = 3;      // supernd
  m_params(5) = 3;      // rreduce
  m_params(6) = 0.5;    // wh_frac
  m_params(7) = 1;      // autommd
  m_params(8) = 1;      // autoamd
  m_params(9) = 0.1;    // piv_tol
  m_params(10) = 0.5;   // bandden
  m_params(11) = 1;     // umfpack
  m_params(12) = 0.001; // sym_tol
}

void sparse_params::do_tight (void)
{
  m_params(0) = 0;      // spumoni
  m_params(1) = 1;      // ths_rel
  m_params(2) = 0;      // ths_abs
  m_params(3) = 1;      // exact_d
  m_params(4) = 1;      // supernd
  m_params(5) = 1;      // rreduce
  m_params(6) = 0.5;    // wh_frac
  m_params(7) = 1;      // autommd
  m_params(8) = 1;      // autoamd
  m_params(9) = 0.1;    // piv_tol
  m_params(10) = 0.5;   // bandden
  m_params(11) = 1;     // umfpack
  m_params(12) = 0.001; // sym_tol
}

void sparse_params::init_keys (void)
{
  m_keys(0) = "spumoni";
  m_keys(1) = "ths_rel";
  m_keys(2) = "ths_abs";
  m_keys(3) = "exact_d";
  m_keys(4) = "supernd";
  m_keys(5) = "rreduce";
  m_keys(6) = "wh_frac";
  m_keys(7) = "autommd";
  m_keys(8) = "autoamd";
  m_keys(9) = "piv_tol";
  m_keys(10) = "bandden";
  m_keys(11) = "umfpack";
  m_keys(12) = "sym_tol";
}

double sparse_params::do_get_bandden (void)
{
  return m_params(10);
}

bool sparse_params::do_set_vals (const Array<double>& vals)
{
  octave_idx_type len = vals.numel ();

  if (len > OCTAVE_SPARSE_CONTROLS_SIZE)
    (*current_liboctave_error_handler)
      ("sparse_params::do_set_vals: too many values");

  for (int i = 0; i < len; i++)
    m_params(i) = vals(i);

  return true;
}

bool sparse_params::do_set_key (const std::string& key, const double& val)
{
  for (int i = 0; i < OCTAVE_SPARSE_CONTROLS_SIZE; i++)
    {
      if (m_keys (i) == key)
        {
          m_params(i) = val;
          return true;
        }
    }

  return false;
}

double sparse_params::do_get_key (const std::string& key)
{
  for (int i = 0; i < OCTAVE_SPARSE_CONTROLS_SIZE; i++)
    {
      if (m_keys (i) == key)
        return m_params(i);
    }

  return numeric_limits<double>::NaN ();
}

void sparse_params::do_print_info (std::ostream& os,
                                   const std::string& prefix) const
{
  for (int i = 0; i < OCTAVE_SPARSE_CONTROLS_SIZE; i++)
    os << prefix << m_keys(i) << ": " << m_params(i) << "\n";
}

OCTAVE_END_NAMESPACE(octave)
