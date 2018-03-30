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

#if ! defined (octave_pt_loop_h)
#define octave_pt_loop_h 1

#include "octave-config.h"

class octave_value;

#include "pt-cmd.h"
#include "pt-walk.h"
#include "pt-jit.h"

namespace octave
{
  class jit_info;
  class tree_argument_list;
  class tree_expression;
  class tree_statement_list;

  // While.

  class tree_while_command : public tree_command
  {
  public:

    tree_while_command (int l = -1, int c = -1)
      : tree_command (l, c), m_expr (nullptr), m_list (nullptr),
        m_lead_comm (nullptr), m_trail_comm (nullptr)
#if defined (HAVE_LLVM)
      , m_compiled (nullptr)
#endif
    { }

    tree_while_command (tree_expression *e,
                        comment_list *lc = nullptr,
                        comment_list *tc = nullptr,
                        int l = -1, int c = -1)
      : tree_command (l, c), m_expr (e), m_list (nullptr),
        m_lead_comm (lc), m_trail_comm (tc)
#if defined (HAVE_LLVM)
      , m_compiled (nullptr)
#endif
    { }

    tree_while_command (tree_expression *e, tree_statement_list *lst,
                        comment_list *lc = nullptr,
                        comment_list *tc = nullptr,
                        int l = -1, int c = -1)
      : tree_command (l, c), m_expr (e), m_list (lst), m_lead_comm (lc),
        m_trail_comm (tc)
#if defined (HAVE_LLVM)
      , m_compiled (nullptr)
#endif
    { }

    // No copying!

    tree_while_command (const tree_while_command&) = delete;

    tree_while_command& operator = (const tree_while_command&) = delete;

    ~tree_while_command (void);

    tree_expression * condition (void) { return m_expr; }

    tree_statement_list * body (void) { return m_list; }

    comment_list * leading_comment (void) { return m_lead_comm; }

    comment_list * trailing_comment (void) { return m_trail_comm; }

    void accept (tree_walker& tw)
    {
      tw.visit_while_command (*this);
    }

#if defined (HAVE_LLVM)
    // some functions use by tree_jit
    jit_info * get_info (void) const { return m_compiled; }

    void stash_info (jit_info *jinfo) { m_compiled = jinfo; }
#endif

  protected:

    // Expression to test.
    tree_expression *m_expr;

    // List of commands to execute.
    tree_statement_list *m_list;

    // Comment preceding WHILE token.
    comment_list *m_lead_comm;

    // Comment preceding ENDWHILE token.
    comment_list *m_trail_comm;

  private:

#if defined (HAVE_LLVM)
    // compiled version of the loop
    jit_info *m_compiled;
#endif
  };

  // Do-Until.

  class tree_do_until_command : public tree_while_command
  {
  public:

    tree_do_until_command (int l = -1, int c = -1)
      : tree_while_command (l, c)
    { }

    tree_do_until_command (tree_expression *e,
                           comment_list *lc = nullptr,
                           comment_list *tc = nullptr,
                           int l = -1, int c = -1)
      : tree_while_command (e, lc, tc, l, c)
    { }

    tree_do_until_command (tree_expression *e, tree_statement_list *lst,
                           comment_list *lc = nullptr,
                           comment_list *tc = nullptr,
                           int l = -1, int c = -1)
      : tree_while_command (e, lst, lc, tc, l, c)
    { }

    // No copying!

    tree_do_until_command (const tree_do_until_command&) = delete;

    tree_do_until_command& operator = (const tree_do_until_command&) = delete;

    ~tree_do_until_command (void) = default;

    void accept (tree_walker& tw)
    {
      tw.visit_do_until_command (*this);
    }
  };

  // For.

  class tree_simple_for_command : public tree_command
  {
  public:

    tree_simple_for_command (int l = -1, int c = -1)
      : tree_command (l, c), m_parallel (false), m_lhs (nullptr),
        m_expr (nullptr), m_maxproc (nullptr), m_list (nullptr),
        m_lead_comm (nullptr), m_trail_comm (nullptr)
#if defined (HAVE_LLVM)
      , m_compiled (nullptr)
#endif
    { }

    tree_simple_for_command (bool parallel_arg, tree_expression *le,
                             tree_expression *re,
                             tree_expression *maxproc_arg,
                             tree_statement_list *lst,
                             comment_list *lc = nullptr,
                             comment_list *tc = nullptr,
                             int l = -1, int c = -1)
      : tree_command (l, c), m_parallel (parallel_arg), m_lhs (le),
        m_expr (re), m_maxproc (maxproc_arg), m_list (lst),
        m_lead_comm (lc), m_trail_comm (tc)
#if defined (HAVE_LLVM)
      , m_compiled (0)
#endif
    { }

    // No copying!

    tree_simple_for_command (const tree_simple_for_command&) = delete;

    tree_simple_for_command& operator = (const tree_simple_for_command&) = delete;

    ~tree_simple_for_command (void);

    bool in_parallel (void) { return m_parallel; }

    tree_expression * left_hand_side (void) { return m_lhs; }

    tree_expression * control_expr (void) { return m_expr; }

    tree_expression * maxproc_expr (void) { return m_maxproc; }

    tree_statement_list * body (void) { return m_list; }

    comment_list * leading_comment (void) { return m_lead_comm; }

    comment_list * trailing_comment (void) { return m_trail_comm; }

    void accept (tree_walker& tw)
    {
      tw.visit_simple_for_command (*this);
    }

#if defined (HAVE_LLVM)
    // some functions use by tree_jit
    jit_info * get_info (void) const
    {
      return m_compiled;
    }

    void stash_info (jit_info *jinfo)
    {
      m_compiled = jinfo;
    }
#endif

  private:
    // TRUE means operate in parallel (subject to the value of the
    // maxproc expression).
    bool m_parallel;

    // Expression to modify.
    tree_expression *m_lhs;

    // Expression to evaluate.
    tree_expression *m_expr;

    // Expression to tell how many processors should be used (only valid
    // if parallel is TRUE).
    tree_expression *m_maxproc;

    // List of commands to execute.
    tree_statement_list *m_list;

    // Comment preceding FOR token.
    comment_list *m_lead_comm;

    // Comment preceding ENDFOR token.
    comment_list *m_trail_comm;

#if defined (HAVE_LLVM)
    // compiled version of the loop
    jit_info *m_compiled;
#endif
  };

  class tree_complex_for_command : public tree_command
  {
  public:

    tree_complex_for_command (int l = -1, int c = -1)
      : tree_command (l, c), m_lhs (nullptr), m_expr (nullptr),
        m_list (nullptr), m_lead_comm (nullptr), m_trail_comm (nullptr)
    { }

    tree_complex_for_command (tree_argument_list *le, tree_expression *re,
                              tree_statement_list *lst,
                              comment_list *lc = nullptr,
                              comment_list *tc = nullptr,
                              int l = -1, int c = -1)
      : tree_command (l, c), m_lhs (le), m_expr (re), m_list (lst),
        m_lead_comm (lc), m_trail_comm (tc)
    { }

    // No copying!

    tree_complex_for_command (const tree_complex_for_command&) = delete;

    tree_complex_for_command& operator = (const tree_complex_for_command&) = delete;

    ~tree_complex_for_command (void);

    tree_argument_list * left_hand_side (void) { return m_lhs; }

    tree_expression * control_expr (void) { return m_expr; }

    tree_statement_list * body (void) { return m_list; }

    comment_list * leading_comment (void) { return m_lead_comm; }

    comment_list * trailing_comment (void) { return m_trail_comm; }

    void accept (tree_walker& tw)
    {
      tw.visit_complex_for_command (*this);
    }

  private:

    // Expression to modify.
    tree_argument_list *m_lhs;

    // Expression to evaluate.
    tree_expression *m_expr;

    // List of commands to execute.
    tree_statement_list *m_list;

    // Comment preceding FOR token.
    comment_list *m_lead_comm;

    // Comment preceding ENDFOR token.
    comment_list *m_trail_comm;
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED (4.4, "use 'octave::tree_while_command' instead")
typedef octave::tree_while_command tree_while_command;

OCTAVE_DEPRECATED (4.4, "use 'octave::tree_do_until_command' instead")
typedef octave::tree_do_until_command tree_do_until_command;

OCTAVE_DEPRECATED (4.4, "use 'octave::tree_simple_for_command' instead")
typedef octave::tree_simple_for_command tree_simple_for_command;

OCTAVE_DEPRECATED (4.4, "use 'octave::tree_complex_for_command' instead")
typedef octave::tree_complex_for_command tree_complex_for_command;

#endif

#endif
