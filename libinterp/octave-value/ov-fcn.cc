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

#include "unwind-prot.h"

#include "error.h"
#include "ovl.h"
#include "ov-fcn.h"
#include "pt-eval.h"

#include "error.h"
#include "interpreter-private.h"
#include "symtab.h"
#include "interpreter.h"
#include "lo-array-errwarn.h"

#include "pt-bytecode-walk.h"

octave_base_value *
octave_function::clone () const
{
  panic_impossible ();
}

octave_base_value *
octave_function::empty_clone () const
{
  panic_impossible ();
}

octave_value_list
octave_function::call (octave::tree_evaluator& tw, int nargout,
                       const octave_value_list& args)
{
#if defined (OCTAVE_ENABLE_BYTECODE_EVALUATOR)
  octave_user_function *usr = this->user_function_value(true);
  if (octave::vm::maybe_compile_or_compiled (usr))
    return octave::vm::call (tw, nargout, args, usr);
#endif

  tw.push_stack_frame (this);

  octave::unwind_action act ([&tw] () { tw.pop_stack_frame (); });

  return execute (tw, nargout, args);
}

#if defined (OCTAVE_ENABLE_BYTECODE_EVALUATOR)

bool
octave_fcn_cache::has_cached_function (void *pbeg, void *pend) const
{
  octave::stack_element *beg = static_cast<octave::stack_element *> (pbeg);
  octave::stack_element *end = static_cast<octave::stack_element *> (pend);

  if (m_n_updated == 0)
    return false;

  unsigned vec_n = m_cached_args.size ();

  unsigned n_args = end - beg;
  if (n_args != vec_n)
    return false;

  for (unsigned i = 0; i < n_args; i++)
    {
      if (beg[i].ov.type_id () != m_cached_args [i])
        return false;
    }

  return true;
}

void
octave_fcn_cache::set_cached_function (octave_value ov,
                                       const octave_value_list &args,
                                       octave_idx_type current_n_updated)
{
  clear_cached_function ();

  if (!ov.is_defined ())
    return;
  // Arbitrary limit on how many args we keep track of in caches.
  if (args.length () > 32)
    return;

  // We need to keep a reference to the metaobject for as long as the function is alive
  if (ov.is_classdef_meta ())
    m_cached_object = ov;

  std::vector<int> v_types;

  for (int i = 0; i < args.length (); i++)
    {
      // FIXME: We don't cache methods or functions with class object
      // arguments. Classes need some kind of unique simple key for this
      // simple approach.
      if (args(i).isobject())
        return;

      v_types.push_back (args (i).type_id ());
    }

  m_cached_args = v_types;
  m_cached_function = ov;

  m_n_updated = current_n_updated;
}

octave_value
octave_fcn_cache::
get_cached_obj ()
{
  octave_function *fcn = nullptr;

  octave_idx_type current_n_updated = octave::load_path::get_weak_n_updated ();
  if (has_cached_function (nullptr, nullptr))
    {
      if (m_n_updated == current_n_updated)
        return m_cached_function;
      else
        clear_cached_function ();
    }

  if (! fcn)
    {
      octave::interpreter& interp =
        octave::__get_interpreter__ ();

      octave::symbol_table& symtab = interp.get_symbol_table ();
      octave_value val = symtab.find_function (m_fcn_name, octave_value_list {});

      if (val.is_function ())
        {
          fcn = val.function_value (true);
          set_cached_function (val, octave_value_list {}, current_n_updated);
          return val;
        }

      val = symtab.find_function (m_fcn_name);
      if (val.is_function ())
        {
          return val;
        }
    }

  return {};
}

octave_function *
octave_fcn_cache::
get_cached_fcn_internal (const octave_value_list& args)
{
  clear_cached_function ();

  octave_function *fcn = nullptr;
  octave_idx_type current_n_updated = octave::load_path::get_weak_n_updated ();

  octave::interpreter& interp =
    octave::__get_interpreter__ ();

  octave::symbol_table& symtab = interp.get_symbol_table ();
  octave_value val = symtab.find_function (m_fcn_name, args);

  if (val.is_function ())
    {
      fcn = val.function_value (true);
      set_cached_function (val, args, current_n_updated);
      return fcn;
    }

  val = symtab.find_function (m_fcn_name);
  if (val.is_function ())
    {
      return val.function_value (true);
    }

  return fcn;
}

octave_function *
octave_fcn_cache::
get_cached_fcn_if_fresh ()
{
  octave_idx_type current_n_updated = octave::load_path::get_weak_n_updated ();
  if (m_n_updated == current_n_updated)
    return get_cached_fcn ();
  return nullptr;
}

octave_function *
octave_fcn_cache::
get_cached_fcn (const octave_value_list& args)
{
  octave_idx_type current_n_updated = octave::load_path::get_weak_n_updated ();
  if (OCTAVE_LIKELY (has_cached_function (args)))
    if (OCTAVE_LIKELY (m_n_updated == current_n_updated))
      return m_cached_function.function_value (true);

  return get_cached_fcn_internal (args);
}

octave_function *
octave_fcn_cache::
get_cached_fcn (void *pbeg, void *pend)
{
  octave_idx_type current_n_updated = octave::load_path::get_weak_n_updated ();
  if (OCTAVE_LIKELY (has_cached_function (pbeg, pend)))
    if (OCTAVE_LIKELY (m_n_updated == current_n_updated))
      return m_cached_function.function_value (true);

  octave::stack_element *beg = static_cast<octave::stack_element *> (pbeg);
  octave::stack_element *end = static_cast<octave::stack_element *> (pend);

  octave_value_list args;
  for (; beg != end; beg++)
    {
      if (OCTAVE_UNLIKELY (beg->ov.is_cs_list ()))
        args.append (beg->ov.list_value ());
      else
        args.append (beg->ov);
    }

  return get_cached_fcn_internal (args);
}

octave_value_list
octave_fcn_cache::
call (octave::tree_evaluator& tw,
      octave_function *fcn,
      const octave_value_list& args,
      int nargout)
{
  try
    {
      return fcn->call (tw, nargout, args);
    }
  catch (octave::index_exception& ie)
    {
      error ("Proper error message here for function calls");
      // Maybe return the octave_function pointer?
      //tw.final_index_error (ie, m_expr);
    }
}

#endif
