/*

Copyright (C) 1993-2017 John W. Eaton

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

#if ! defined (octave_pt_const_h)
#define octave_pt_const_h 1

#include "octave-config.h"

#include <iosfwd>
#include <string>

class octave_value_list;

#include "ov.h"
#include "pt-bp.h"
#include "pt-exp.h"
#include "pt-walk.h"
#include "symtab.h"

namespace octave
{
  class tree_constant : public tree_expression
  {
  public:

    tree_constant (int l = -1, int c = -1)
      : tree_expression (l, c), val (), orig_text () { }

    tree_constant (const octave_value& v, int l = -1, int c = -1)
      : tree_expression (l, c), val (v), orig_text () { }

    tree_constant (const octave_value& v, const std::string& ot,
                   int l = -1, int c = -1)
      : tree_expression (l, c), val (v), orig_text (ot) { }

    // No copying!

    tree_constant (const tree_constant&) = delete;

    tree_constant& operator = (const tree_constant&) = delete;

    ~tree_constant (void) = default;

    bool has_magic_end (void) const { return false; }

    // Type.  It would be nice to eliminate the need for this.

    bool is_constant (void) const { return true; }

    void maybe_mutate (void) { val.maybe_mutate (); }

    void print (std::ostream& os, bool pr_as_read_syntax = false,
                bool pr_orig_txt = true);

    void print_raw (std::ostream& os, bool pr_as_read_syntax = false,
                    bool pr_orig_txt = true);

    bool rvalue_ok (void) const { return true; }

    octave_value value (void) { return val; }

    tree_expression * dup (symbol_table::scope& scope) const;

    void accept (tree_walker& tw)
    {
      tw.visit_constant (*this);
    }

    // Store the original text corresponding to this constant for later
    // pretty printing.

    void stash_original_text (const std::string& s) { orig_text = s; }

    std::string original_text (void) const { return orig_text; }

  private:

    // The actual value that this constant refers to.
    octave_value val;

    // The original text form of this constant.
    std::string orig_text;
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED (4.4, "use 'octave::tree_constant' instead")
typedef octave::tree_constant tree_constant;

#endif

#endif
