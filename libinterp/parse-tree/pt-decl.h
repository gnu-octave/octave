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

#if ! defined (octave_pt_decl_h)
#define octave_pt_decl_h 1

#include "octave-config.h"

#include <list>
#include <string>

#include "base-list.h"
#include "oct-lvalue.h"
#include "pt-cmd.h"
#include "pt-id.h"
#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class symbol_scope;
class tree_evaluator;
class tree_expression;
class tree_identifier;

// List of expressions that make up a declaration statement.

class tree_decl_elt
{
public:

  enum decl_type
  {
    unknown,
    global,
    persistent
  };

  tree_decl_elt (tree_identifier *i, tree_expression *e = nullptr);

  // No copying!

  tree_decl_elt (const tree_decl_elt&) = delete;

  tree_decl_elt& operator = (const tree_decl_elt&) = delete;

  ~tree_decl_elt (void);

  void mark_as_formal_parameter (void)
  {
    m_id->mark_as_formal_parameter ();
  }

  bool lvalue_ok (void) { return m_id->lvalue_ok (); }

  octave_lvalue lvalue (tree_evaluator& tw)
  {
    return m_id->lvalue (tw);
  }

  void mark_global (void) { type = global; }
  bool is_global (void) const { return type == global; }

  void mark_persistent (void) { type = persistent; }
  bool is_persistent (void) const { return type == persistent; }

  tree_identifier * ident (void) { return m_id; }

  std::string name (void) const { return m_id->name (); }

  tree_expression * expression (void) { return m_expr; }

  tree_decl_elt * dup (symbol_scope& scope) const;

  void accept (tree_walker& tw)
  {
    tw.visit_decl_elt (*this);
  }

private:

  decl_type type;

  // An identifier to tag with the declared property.
  tree_identifier *m_id;

  // An initializer expression (may be zero);
  tree_expression *m_expr;
};

class tree_decl_init_list : public base_list<tree_decl_elt *>
{
public:

  tree_decl_init_list (void) { }

  tree_decl_init_list (tree_decl_elt *t) { append (t); }

  // No copying!

  tree_decl_init_list (const tree_decl_init_list&) = delete;

  tree_decl_init_list& operator = (const tree_decl_init_list&) = delete;

  ~tree_decl_init_list (void)
  {
    while (! empty ())
      {
        auto p = begin ();
        delete *p;
        erase (p);
      }
  }

  void mark_global (void)
  {
    for (tree_decl_elt *elt : *this)
      elt->mark_global ();
  }

  void mark_persistent (void)
  {
    for (tree_decl_elt *elt : *this)
      elt->mark_persistent ();
  }

  std::list<std::string> variable_names (void) const
  {
    std::list<std::string> retval;

    for (const tree_decl_elt *elt : *this)
      {
        std::string nm = elt->name ();

        if (! nm.empty ())
          retval.push_back (nm);
      }

    return retval;
  }

  void accept (tree_walker& tw)
  {
    tw.visit_decl_init_list (*this);
  }
};

// Base class for declaration commands -- global, static, etc.

class tree_decl_command : public tree_command
{
public:

  tree_decl_command (const std::string& n, int l = -1, int c = -1)
    : tree_command (l, c), m_cmd_name (n), m_init_list (nullptr) { }

  tree_decl_command (const std::string& n, tree_decl_init_list *t,
                     int l = -1, int c = -1);

  // No copying!

  tree_decl_command (const tree_decl_command&) = delete;

  tree_decl_command& operator = (const tree_decl_command&) = delete;

  ~tree_decl_command (void);

  void mark_global (void)
  {
    if (m_init_list)
      m_init_list->mark_global ();
  }

  void mark_persistent (void)
  {
    if (m_init_list)
      m_init_list->mark_persistent ();
  }

  tree_decl_init_list * initializer_list (void) { return m_init_list; }

  std::string name (void) const { return m_cmd_name; }

  void accept (tree_walker& tw)
  {
    tw.visit_decl_command (*this);
  }

private:

  // The name of this command -- global, static, etc.
  std::string m_cmd_name;

  // The list of variables or initializers in this declaration command.
  tree_decl_init_list *m_init_list;
};

OCTAVE_END_NAMESPACE(octave)

#endif
