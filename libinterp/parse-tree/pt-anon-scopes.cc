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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "error.h"
#include "pt-all.h"
#include "pt-anon-scopes.h"

// TODO: make sure that if(f->scope()) is checked if necessary

OCTAVE_BEGIN_NAMESPACE(octave)

tree_anon_scopes::tree_anon_scopes (tree_anon_fcn_handle& anon_fh)
  : tree_walker (), m_params (), m_vars ()
{
  visit_anon_fcn_handle (anon_fh);
}

void
tree_anon_scopes::visit_anon_fcn_handle (tree_anon_fcn_handle& afh)
{
  tree_parameter_list *param_list = afh.parameter_list ();
  tree_expression *expr = afh.expression ();

  // Collect names of parameters.

  if (param_list)
    {
      std::list<std::string> pnames = param_list->variable_names ();

      for (const auto& nm : pnames)
        m_params.insert (nm);

      // Hmm, should this be included in the list returned from
      // tree_parameter_list::variable_names?
      if (param_list->takes_varargs ())
        m_params.insert ("varargin");
    }

  // Further walk the tree to find free variables in this expression
  // and any nested definitions of additional anonymous functions.

  if (expr)
    expr->accept (*this);
}

// The rest of visit_... methods is only for walking the tree.  Many of
// them, in particular all methods for commands, are not applicable to
// anonymous functions.  Only parts of the tree are walked which could
// contain further (nested) anonymous function definitions (so
// e.g. identifiers and left hand sides of assignments are ignored).

void
tree_anon_scopes::visit_identifier (tree_identifier& id)
{
  std::string nm = id.name ();

  if (m_params.find (nm) == m_params.end ())
    m_vars.insert (nm);
}

void
tree_anon_scopes::visit_parameter_list (tree_parameter_list&)
{
  // In visit_anon_fcn_handle we only accept/visit the body of
  // anonymous function definitions, not the parameter list.

  panic_impossible ();
}

void
tree_anon_scopes::visit_statement (tree_statement& stmt)
{
  tree_command *cmd = stmt.command ();

  if (cmd)
    panic_impossible ();
  else
    {
      tree_expression *expr = stmt.expression ();

      if (expr)
        expr->accept (*this);
    }
}

void
tree_anon_scopes::visit_statement_list (tree_statement_list& lst)
{
  for (auto& p : lst)
    {
      tree_statement *elt = p;

      if (elt)
        elt->accept (*this);
    }
}

OCTAVE_END_NAMESPACE(octave)

