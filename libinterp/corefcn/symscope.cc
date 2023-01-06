////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#include <sstream>

#include "file-ops.h"

#include "fcn-info.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "ov-fcn.h"
#include "ov-usr-fcn.h"
#include "symrec.h"
#include "symscope.h"
#include "utils.h"

OCTAVE_BEGIN_NAMESPACE(octave)

symbol_record symbol_scope_rep::insert_local (const std::string& name)
{
  symbol_record sym (name);

  insert_symbol_record (sym);

  return sym;
}

void symbol_scope_rep::insert_symbol_record (symbol_record& sr)
{
  std::size_t data_offset = num_symbols ();
  std::string name = sr.name ();

  sr.set_data_offset (data_offset);

  m_symbols[name] = sr;
}

symbol_record symbol_scope_rep::insert (const std::string& name)
{
  table_iterator p = m_symbols.find (name);

  if (p == m_symbols.end ())
    {
      symbol_record ret (name);

      std::size_t data_offset = num_symbols ();

      ret.set_data_offset (data_offset);

      auto t_parent = m_parent.lock ();

      std::size_t offset = 0;

      if (is_nested () && t_parent
          && t_parent->look_nonlocal (name, offset, ret))
        return m_symbols[name] = ret;
      else
        {
          if (m_is_static)
            ret.mark_added_static ();

          return m_symbols[name] = ret;
        }
    }
  else
    return p->second;
}

std::list<octave_value> symbol_scope_rep::localfunctions (void) const
{
  std::list<octave_value> retval;

  // Find the subfunctions of this function (which should be the
  // primary parent function for this scope).

  // 1) m_subfunction_names contains only valid subfunctions
  // 2) m_subfunctions contains both nested functions and subfunctions

  // loop over them.

  for (const auto& nm : m_subfunction_names)
    {
      auto nm_fcn_iter = m_subfunctions.find (nm);

      if (nm_fcn_iter != m_subfunctions.end ())
        {
          octave_value ov_fcn = nm_fcn_iter->second;
          octave_user_code *fcn = ov_fcn.user_code_value ();

          if (! fcn)
            continue;

          symbol_scope scope = fcn->scope ();

          std::list<std::string> plst = scope.parent_fcn_names ();

          octave_fcn_handle *fh = new octave_fcn_handle (ov_fcn, nm, plst);

          retval.push_back (octave_value (fh));
        }
    }

  return retval;
}

octave_value
symbol_scope_rep::dump (void) const
{
  std::map<std::string, octave_value> m
  = {{ "name", m_name },
    { "nesting_depth", m_nesting_depth },
    { "is_static", m_is_static },
    { "symbols", dump_symbols_map () },
    { "subfunction_names", string_vector (m_subfunction_names) },
    { "subfunctions", dump_function_map (m_subfunctions) }
  };

  return octave_value (m);
}

octave_value
symbol_scope_rep::dump_symbols_map (void) const
{
  std::map<std::string, octave_value> info_map;

  for (const auto& nm_sr : m_symbols)
    {
      std::string nm = nm_sr.first;
      symbol_record sr = nm_sr.second;
      info_map[nm] = sr.dump ();
    }

  return octave_value (info_map);
}

std::list<symbol_record> symbol_scope_rep::symbol_list (void) const
{
  std::list<symbol_record> retval;

  for (const auto& nm_sr : m_symbols)
    retval.push_back (nm_sr.second);

  return retval;
}

octave_value
symbol_scope_rep::find_subfunction (const std::string& name) const
{
  subfunctions_const_iterator p = m_subfunctions.find (name);

  if (p != m_subfunctions.end ())
    return p->second;

  auto t_parent = m_parent.lock ();

  if (t_parent)
    return t_parent->find_subfunction (name);

  return octave_value ();
}

void
symbol_scope_rep::mark_subfunctions_in_scope_as_private (const std::string& class_name)
{
  for (auto& nm_sf : m_subfunctions)
    {
      octave_function *fcn = nm_sf.second.function_value ();

      if (fcn)
        fcn->mark_as_private_function (class_name);
    }
}

std::list<std::string>
symbol_scope_rep::parent_fcn_names (void) const
{
  std::list<std::string> retval;

  auto pscope = parent_scope_rep ();

  while (pscope)
    {
      retval.push_back (pscope->fcn_name ());

      pscope = pscope->parent_scope_rep ();
    }

  return retval;
}

void
symbol_scope_rep::set_parent (const std::shared_ptr<symbol_scope_rep>& parent)
{
  m_parent = std::weak_ptr<symbol_scope_rep> (parent);
}

void
symbol_scope_rep::set_primary_parent (const std::shared_ptr<symbol_scope_rep>& parent)
{
  m_primary_parent = std::weak_ptr<symbol_scope_rep> (parent);
}

void
symbol_scope_rep::cache_dir_name (const std::string& name)
{
  m_dir_name = sys::canonicalize_file_name (name);
}

bool
symbol_scope_rep::is_relative (const std::shared_ptr<symbol_scope_rep>& scope) const
{
  if (is_nested ())
    {
      // Since is_nested is true, the following should always return a
      // valid scope.

      auto t_parent = m_parent.lock ();

      if (t_parent)
        {
          // SCOPE is the parent of this scope: this scope is a child
          // of SCOPE.

          if (t_parent == scope)
            return true;
        }

      auto t_primary_parent = m_primary_parent.lock ();

      if (t_primary_parent)
        {
          // SCOPE is the primary parent of this scope: this scope is a
          // child (or grandchild) of SCOPE.
          if (t_primary_parent == scope)
            return true;

          // SCOPE and this scope share the same primary parent: they are
          // siblings (or cousins)
          auto scope_primary_parent = scope->primary_parent_scope_rep ();
          if (t_primary_parent == scope_primary_parent)
            return true;
        }
    }

  return false;
}

void symbol_scope_rep::mark_as_variable (const std::string& nm)
{
  table_iterator p = m_symbols.find (nm);

  if (p != m_symbols.end ())
    p->second.mark_as_variable ();
}

void symbol_scope_rep::mark_as_variables (const std::list<std::string>& lst)
{
  for (const auto& nm : lst)
    mark_as_variable (nm);
}

bool symbol_scope_rep::is_variable (const std::string& nm) const
{
  table_const_iterator p = m_symbols.find (nm);

  // FIXME: maybe we should also mark formal parameters as variables?

  if (p != m_symbols.end () && p->second.is_variable ())
    return true;

  if (is_nested ())
    {
      auto t_parent = m_parent.lock ();

      return t_parent ? t_parent->is_variable (nm) : false;
    }

  return false;
}

void symbol_scope_rep::update_nest (void)
{
  auto t_parent = m_parent.lock ();

  if (t_parent)
    {
      // fix bad symbol_records
      for (auto& nm_sr : m_symbols)
        {
          symbol_record& ours = nm_sr.second;

          std::size_t offset = 0;

          if (! ours.is_formal () && is_nested ())
            t_parent->look_nonlocal (nm_sr.first, offset, ours);
        }

      // The scopes of nested functions are static.
      if (is_nested ())
        m_is_static = true;
    }
  else if (m_children.size ())
    {
      // Parents of nested functions have static scopes.
      m_is_static = true;
    }

  for (auto& scope_obj : m_children)
    scope_obj.update_nest ();
}

bool symbol_scope_rep::look_nonlocal (const std::string& name,
                                      std::size_t offset,
                                      symbol_record& result)
{
  offset++;

  table_iterator p = m_symbols.find (name);

  if (p == m_symbols.end ())
    {
      auto t_parent = m_parent.lock ();

      if (is_nested () && t_parent)
        return t_parent->look_nonlocal (name, offset, result);
    }
  else
    {
      // Add scope offsets because the one we found may be used in
      // this scope but initially from another parent scope beyond
      // that.  The parent offset will already point to the first
      // occurrence because we do the overall nesting update from the
      // parent function down through the lists of all children.

      std::size_t t_frame_offset = offset + p->second.frame_offset ();
      std::size_t t_data_offset = p->second.data_offset ();

      result.set_frame_offset (t_frame_offset);
      result.set_data_offset (t_data_offset);

      return true;
    }

  return false;
}

std::list<octave_value> symbol_scope::localfunctions (void) const
{
  if (! m_rep)
    return std::list<octave_value> ();

  if (is_primary_fcn_scope ())
    return m_rep->localfunctions ();

  std::shared_ptr<symbol_scope_rep> ppsr
    = m_rep->primary_parent_scope_rep ();

  if (! ppsr)
    return std::list<octave_value> ();

  return ppsr->localfunctions ();
}

OCTAVE_END_NAMESPACE(octave)
