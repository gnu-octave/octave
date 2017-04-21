/*

Copyright (C) 1996-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_pt_idx_h)
#define octave_pt_idx_h 1

#include "octave-config.h"

#include <list>

class octave_map;
class octave_value;
class octave_value_list;
class octave_lvalue;

#include "str-vec.h"

#include "pt-exp.h"
#include "pt-walk.h"
#include "symtab.h"

namespace octave
{
  class tree_argument_list;
  class tree_evaluator;

  // Index expressions.

  class tree_index_expression : public tree_expression
  {
  public:

    tree_index_expression (tree_expression *e = 0, tree_argument_list *lst = 0,
                           int l = -1, int c = -1, char t = '(');

    tree_index_expression (tree_expression *e, const std::string& n,
                           int l = -1, int c = -1);

    tree_index_expression (tree_expression *e, tree_expression* df,
                           int l = -1, int c = -1);

    // No copying!

    tree_index_expression (const tree_index_expression&) = delete;

    tree_index_expression& operator = (const tree_index_expression&) = delete;

    ~tree_index_expression (void);

    bool has_magic_end (void) const;

    void append (tree_argument_list *lst = 0, char t = '(');

    void append (const std::string& n);

    void append (tree_expression *df);

    bool is_index_expression (void) const { return true; }

    std::string name (void) const;

    tree_expression *expression (void) { return expr; }

    std::list<tree_argument_list *> arg_lists (void) { return args; }

    std::string type_tags (void) { return type; }

    std::list<string_vector> arg_names (void) { return arg_nm; }

    std::list<tree_expression *> dyn_fields (void) { return dyn_field; }

    bool lvalue_ok (void) const { return expr->lvalue_ok (); }

    bool rvalue_ok (void) const { return true; }

    octave_lvalue lvalue (tree_evaluator *tw);

    tree_index_expression *dup (symbol_table::scope_id scope,
                                symbol_table::context_id context) const;

    void accept (tree_walker& tw)
    {
      tw.visit_index_expression (*this);
    }

    std::string
    get_struct_index
    (tree_evaluator *tw, std::list<string_vector>::const_iterator p_arg_nm,
     std::list<tree_expression *>::const_iterator p_dyn_field) const;

  private:

    // The LHS of this index expression.
    tree_expression *expr;

    // The indices (only valid if type == paren || type == brace).
    std::list<tree_argument_list *> args;

    // The type of this index expression.
    std::string type;

    // The names of the arguments.  Used for constant struct element
    // references.
    std::list<string_vector> arg_nm;

    // The list of dynamic field names, if any.
    std::list<tree_expression *> dyn_field;

    tree_index_expression (int l, int c);

    octave_map make_arg_struct (void) const;
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED ("use 'octave::tree_index_expression' instead")
typedef octave::tree_index_expression tree_index_expression;

#endif

#endif
