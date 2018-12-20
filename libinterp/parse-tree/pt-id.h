/*

Copyright (C) 1996-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_pt_id_h)
#define octave_pt_id_h 1

#include "octave-config.h"

#include <iosfwd>
#include <string>

class octave_value;
class octave_value_list;
class octave_function;

#include "oct-lvalue.h"
#include "pt-bp.h"
#include "pt-exp.h"
#include "pt-walk.h"
#include "symscope.h"

namespace octave
{
  class tree_evaluator;

  // Symbols from the symbol table.

  class tree_identifier : public tree_expression
  {
    friend class tree_index_expression;

  public:

    tree_identifier (int l = -1, int c = -1)
      : tree_expression (l, c), m_sym () { }

    tree_identifier (const symbol_record& s,
                     int l = -1, int c = -1)
      : tree_expression (l, c), m_sym (s) { }

    // No copying!

    tree_identifier (const tree_identifier&) = delete;

    tree_identifier& operator = (const tree_identifier&) = delete;

    ~tree_identifier (void) = default;

    bool has_magic_end (void) const { return (name () == "end"); }

    bool is_identifier (void) const { return true; }

    std::string name (void) const { return m_sym.name (); }

    bool is_defined (symbol_record::context_id context)
    {
      return m_sym.is_defined (context);
    }

    virtual bool is_variable (symbol_record::context_id context) const
    {
      return m_sym.is_variable (context);
    }

    virtual bool is_black_hole (void) { return false; }

    // Try to find a definition for an identifier.  Here's how:
    //
    //   * If the identifier is already defined and is a function defined
    //     in an function file that has been modified since the last time
    //     we parsed it, parse it again.
    //
    //   * If the identifier is not defined, try to find a builtin
    //     variable or an already compiled function with the same name.
    //
    //   * If the identifier is still undefined, try looking for an
    //     function file to parse.
    //
    //   * On systems that support dynamic linking, we prefer .oct files,
    //     then .mex files, then .m files.

    octave_value
    do_lookup (symbol_record::context_id context,
               const octave_value_list& args = octave_value_list ())
    {
      return m_sym.find (context, args);
    }

    void link_to_global (const symbol_scope& global_scope,
                         const symbol_record& global_sym);

    void mark_persistent (void) { m_sym.init_persistent (); }

    void mark_as_formal_parameter (void) { m_sym.mark_formal (); }

    // We really need to know whether this symbol referst to a variable
    // or a function, but we may not know that yet.

    bool lvalue_ok (void) const { return true; }

    octave_lvalue lvalue (tree_evaluator&);

    void eval_undefined_error (void);

    void static_workspace_error (void)
    {
      error (R"(can not add variable "%s" to a static workspace)",
             name ().c_str ());
    }

    tree_identifier * dup (symbol_scope& scope) const;

    void accept (tree_walker& tw)
    {
      tw.visit_identifier (*this);
    }

    symbol_record symbol (void) const { return m_sym; }

  private:

    // The symbol record that this identifier references.
    symbol_record m_sym;
  };

  class tree_black_hole : public tree_identifier
  {
  public:

    tree_black_hole (int l = -1, int c = -1)
      : tree_identifier (l, c) { }

    std::string name (void) const { return "~"; }

    bool is_variable (symbol_record::context_id) const { return false; }

    bool is_black_hole (void) { return true; }

    tree_black_hole * dup (symbol_scope&) const
    {
      return new tree_black_hole;
    }

    octave_lvalue lvalue (tree_evaluator&)
    {
      octave_lvalue retval;
      retval.mark_black_hole ();
      return retval;
    }
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED (4.4, "use 'octave::tree_identifier' instead")
typedef octave::tree_identifier tree_identifier;

OCTAVE_DEPRECATED (4.4, "use 'octave::tree_black_hole' instead")
typedef octave::tree_black_hole tree_black_hole;

#endif

#endif
