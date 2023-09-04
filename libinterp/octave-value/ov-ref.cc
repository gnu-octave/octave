////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007-2023 The Octave Project Developers
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

#include "ov.h"
#include "ov-ref.h"


#include "interpreter.h"
#include "interpreter-private.h"

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_value_ref_global,
                                     "global value reference",
                                     "global value reference");
DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_value_ref_persistent,
                                     "global value persistent",
                                     "global value persistent");

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_value_ref_vmlocal,
                                     "local vm value reference",
                                     "local vm value reference");

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_value_ref_ptr,
                                     "local vm value pointer",
                                     "local vm value pointer");

void
octave_value_ref::maybe_call_dtor ()
{
  ref ().m_rep->maybe_call_dtor ();
}

bool
octave_value_ref::is_defined () const
{
  return const_cast<octave_value_ref*> (this)->ref ().m_rep->is_defined ();
}

bool
octave_value_ref::is_maybe_function () const
{
  return const_cast<octave_value_ref*> (this)->ref ().m_rep->is_maybe_function ();
}

octave_base_value *
octave_value_ref::unique_clone ()
{
  return ref ().m_rep->unique_clone ();
}

octave_value
octave_value_ref::simple_subsasgn (char type, octave_value_list& idx, const octave_value& rhs)
{
  octave_value ans = ref ().m_rep->simple_subsasgn (type, idx, rhs);
  ref () = ans;
  return octave_value {this, true};
}

octave_value
octave_value_ref::subsasgn (const std::string& type,
                            const std::list<octave_value_list>& idx,
                            const octave_value& rhs)
{
  octave_value ans = ref ().m_rep->subsasgn (type, idx, rhs);
  ref () = ans;
  return octave_value {this, true};
}

octave_value
octave_value_ref_global::deref ()
{
  auto &interp = octave::__get_interpreter__();
  return interp.global_varval (m_name);
}

void
octave_value_ref_global::set_value (octave_value val)
{
  auto &interp = octave::__get_interpreter__();
  interp.global_assign (m_name, val);
}

octave_value&
octave_value_ref_global::ref ()
{
  auto& tw = octave::__get_evaluator__();
  return tw.global_varref (m_name);
}

octave_value
octave_value_ref_persistent::deref ()
{
  return m_scope.persistent_varval (m_offset);
}

void
octave_value_ref_persistent::set_value (octave_value val)
{
  octave_value &ov_pers = m_scope.persistent_varref (m_offset);
  ov_pers = val;
}

octave_value &
octave_value_ref_persistent::ref ()
{
  return m_scope.persistent_varref (m_offset);
}

octave_value &
octave_value_ref_vmlocal::ref ()
{
  return m_frame->varref (m_sym);
}

octave_value
octave_value_ref_vmlocal::deref ()
{
  return m_frame->varval (m_sym);
}

void
octave_value_ref_vmlocal::set_value (octave_value val)
{
  m_frame->varref (m_sym) = val;
}

octave_value &
octave_value_ref_ptr::ref ()
{
  if (m_pov->is_ref ())
    return m_pov->ref_rep ()->ref ();
  return *m_pov;
}

octave_value
octave_value_ref_ptr::deref ()
{
  if (m_pov->is_ref ())
    return m_pov->ref_rep ()->deref ();
  return *m_pov;
}

void
octave_value_ref_ptr::set_value (octave_value val)
{
  if (m_pov->is_ref ())
    m_pov->ref_rep ()->set_value (val);
  else
    *m_pov = val;
}
