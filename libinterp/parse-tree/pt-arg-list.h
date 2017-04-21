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

#if ! defined (octave_pt_arg_list_h)
#define octave_pt_arg_list_h 1

#include "octave-config.h"

#include <list>

class octave_value_list;
class octave_lvalue;

#include "str-vec.h"

#include "base-list.h"
#include "pt-walk.h"
#include "symtab.h"

namespace octave
{
  class tree_evaluator;
  class tree_expression;

  // Argument lists.  Used to hold the list of expressions that are the
  // arguments in a function call or index expression.

  class tree_argument_list : public octave::base_list<tree_expression *>
  {
  public:

    typedef tree_expression* element_type;

    tree_argument_list (void)
      : list_includes_magic_end (false), list_includes_magic_tilde (false),
        simple_assign_lhs (false) { }

    tree_argument_list (tree_expression *t)
      : list_includes_magic_end (false), list_includes_magic_tilde (false),
        simple_assign_lhs (false)
    { append (t); }

    // No copying!

    tree_argument_list (const tree_argument_list&) = delete;

    tree_argument_list& operator = (const tree_argument_list&) = delete;

    ~tree_argument_list (void);

    bool has_magic_end (void) const;

    bool has_magic_tilde (void) const
    { return list_includes_magic_tilde; }

    tree_expression *remove_front (void)
    {
      iterator p = begin ();
      tree_expression *retval = *p;
      erase (p);
      return retval;
    }

    void append (const element_type& s);

    void mark_as_simple_assign_lhs (void) { simple_assign_lhs = true; }

    bool is_simple_assign_lhs (void) { return simple_assign_lhs; }

    bool all_elements_are_constant (void) const;

    bool is_valid_lvalue_list (void) const;

    octave_value_list convert_to_const_vector (tree_evaluator *tw,
                                               const octave_value *object = 0);

    std::list<octave_lvalue> lvalue_list (tree_evaluator *tw);

    string_vector get_arg_names (void) const;

    std::list<std::string> variable_names (void) const;

    tree_argument_list *dup (symbol_table::scope_id scope,
                             symbol_table::context_id context) const;

    void accept (tree_walker& tw)
    {
      tw.visit_argument_list (*this);
    }

  private:

    bool list_includes_magic_end;

    bool list_includes_magic_tilde;

    bool simple_assign_lhs;
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

// tree_argument_list is derived from a template.

#endif

#endif
