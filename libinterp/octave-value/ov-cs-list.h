////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2002-2023 The Octave Project Developers
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

#if ! defined (octave_ov_cs_list_h)
#define octave_ov_cs_list_h 1

#include "octave-config.h"

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "mx-base.h"
#include "str-vec.h"

#include "Cell.h"
#include "error.h"
#include "ovl.h"
#include "ov-typeinfo.h"

// Lists.

class
octave_cs_list : public octave_base_value
{
public:

  octave_cs_list (void)
    : octave_base_value (), m_list () { }

  octave_cs_list (const octave_value_list& l)
    : octave_base_value (), m_list (l) { }

  octave_cs_list (const Cell& c);

  octave_cs_list (const octave_cs_list& l)
    : octave_base_value (), m_list (l.m_list) { }

  ~octave_cs_list (void) = default;

  octave_base_value * clone (void) const { return new octave_cs_list (*this); }
  octave_base_value * empty_clone (void) const { return new octave_cs_list (); }

  dim_vector dims (void) const { return dim_vector (1, m_list.length ()); }

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  bool is_cs_list (void) const { return true; }

  octave_value_list list_value (void) const { return m_list; }

  // We don't need to override all three forms of subsref.  The using
  // declaration will avoid warnings about partially-overloaded virtual
  // functions.
  using octave_base_value::subsref;

  OCTINTERP_API octave_value
  subsref (const std::string& type, const std::list<octave_value_list>& idx);

  OCTINTERP_API octave_value_list
  subsref (const std::string& type, const std::list<octave_value_list>& idx,
           int);

private:

  // The list of Octave values.
  octave_value_list m_list;

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
