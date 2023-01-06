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

#include "defun.h"
#include "error.h"
#include "pt-cmd.h"
#include "ov.h"
#include "oct-lvalue.h"
#include "pt-bp.h"
#include "pt-decl.h"
#include "pt-exp.h"
#include "pt-id.h"
#include "pt-walk.h"
#include "utils.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Declarations (global, static, etc.).

tree_decl_elt::tree_decl_elt (tree_identifier *i, tree_expression *e)
  : type (unknown), m_id (i), m_expr (e)
{
  if (! m_id)
    error ("tree_decl_elt: invalid ID");
}

tree_decl_elt::~tree_decl_elt (void)
{
  delete m_id;
  delete m_expr;
}

tree_decl_elt *
tree_decl_elt::dup (symbol_scope& scope) const
{
  return new tree_decl_elt (m_id->dup (scope),
                            m_expr ? m_expr->dup (scope) : nullptr);
}

// Initializer lists for declaration statements.

// Declaration commands (global, static).

tree_decl_command::tree_decl_command (const std::string& n,
                                      tree_decl_init_list *t, int l, int c)
  : tree_command (l, c), m_cmd_name (n), m_init_list (t)
{
  if (t)
    {
      if (m_cmd_name == "global")
        mark_global ();
      else if (m_cmd_name == "persistent")
        mark_persistent ();
      else
        error ("tree_decl_command: unknown decl type: %s",
               m_cmd_name.c_str ());
    }
}

tree_decl_command::~tree_decl_command (void)
{
  delete m_init_list;
}

OCTAVE_END_NAMESPACE(octave)
