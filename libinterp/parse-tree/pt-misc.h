/*

Copyright (C) 1994-2017 John W. Eaton

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

#if ! defined (octave_pt_misc_h)
#define octave_pt_misc_h 1

#include "octave-config.h"

#include "base-list.h"
#include "pt-decl.h"
#include "pt-walk.h"
#include "symtab.h"

namespace octave
{
  class tree_identifier;
  class tree_index_expression;

  // Parameter lists.  Used to hold the list of input and output
  // parameters in a function definition.  Elements are identifiers
  // only.

  class tree_parameter_list : public octave::base_list<tree_decl_elt *>
  {
  public:

    enum in_or_out
      {
        in = 1,
        out = 2
      };

    tree_parameter_list (void)
      : marked_for_varargs (0) { }

    tree_parameter_list (tree_decl_elt *t)
      : marked_for_varargs (0) { append (t); }

    tree_parameter_list (tree_identifier *id)
      : marked_for_varargs (0) { append (new tree_decl_elt (id)); }

    // No copying!

    tree_parameter_list (const tree_parameter_list&) = delete;

    tree_parameter_list& operator = (const tree_parameter_list&) = delete;

    ~tree_parameter_list (void);

    void mark_as_formal_parameters (void);

    void mark_varargs (void) { marked_for_varargs = 1; }

    void mark_varargs_only (void) { marked_for_varargs = -1; }

    bool takes_varargs (void) const { return marked_for_varargs != 0; }

    bool varargs_only (void) { return (marked_for_varargs < 0); }

    bool is_defined (void);

    std::list<std::string> variable_names (void) const;

    tree_parameter_list * dup (symbol_table::scope& scope) const;

    void accept (tree_walker& tw)
    {
      tw.visit_parameter_list (*this);
    }

  private:

    int marked_for_varargs;
  };

  // Return lists.  Used to hold the right hand sides of multiple
  // assignment expressions.

  class tree_return_list : public octave::base_list<tree_index_expression *>
  {
  public:

    tree_return_list (void) { }

    tree_return_list (tree_index_expression *t) { append (t); }

    // No copying!

    tree_return_list (const tree_return_list&) = delete;

    tree_return_list& operator = (const tree_return_list&) = delete;

    ~tree_return_list (void);

    void accept (tree_walker& tw)
    {
      tw.visit_return_list (*this);
    }
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

// tree_parameter_list is derived from a template.
// tree_return_list is derived from a template.

#endif

#endif
