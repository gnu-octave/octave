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

#include <cctype>

#include "comment-list.h"
#include "error.h"
#include "ov-usr-fcn.h"
#include "pr-output.h"
#include "pt-all.h"

OCTAVE_BEGIN_NAMESPACE(octave)

void
tree_print_code::visit_anon_fcn_handle (tree_anon_fcn_handle& afh)
{
  indent ();

  print_parens (afh, "(");

  m_os << "@";

  tree_parameter_list *param_list = afh.parameter_list ();

  if (param_list)
    param_list->accept (*this);

  print_fcn_handle_body (afh.expression ());

  print_parens (afh, ")");
}

void
tree_print_code::visit_argument_list (tree_argument_list& lst)
{
  auto p = lst.begin ();

  while (p != lst.end ())
    {
      tree_expression *elt = *p++;

      if (elt)
        {
          elt->accept (*this);

          if (p != lst.end ())
            m_os << ", ";
        }
    }
}

void
tree_print_code::visit_arguments_block (tree_arguments_block&)
{
  indent ();

  // FIXME
  m_os << "arguments ... endarguments";
}

void
tree_print_code::visit_args_block_attribute_list (tree_args_block_attribute_list&)
{
  panic_impossible ();
}

void
tree_print_code::visit_args_block_validation_list (tree_args_block_validation_list&)
{
  panic_impossible ();
}

void
tree_print_code::visit_arg_validation (tree_arg_validation&)
{
  panic_impossible ();
}

void
tree_print_code::visit_arg_size_spec (tree_arg_size_spec&)
{
  panic_impossible ();
}

void
tree_print_code::visit_arg_validation_fcns (tree_arg_validation_fcns&)
{
  panic_impossible ();
}

void
tree_print_code::visit_binary_expression (tree_binary_expression& expr)
{
  indent ();

  print_parens (expr, "(");

  tree_expression *op1 = expr.lhs ();

  if (op1)
    op1->accept (*this);

  m_os << ' ' << expr.oper () << ' ';

  tree_expression *op2 = expr.rhs ();

  if (op2)
    op2->accept (*this);

  print_parens (expr, ")");
}

void
tree_print_code::visit_break_command (tree_break_command&)
{
  indent ();

  m_os << "break";
}

void
tree_print_code::visit_colon_expression (tree_colon_expression& expr)
{
  indent ();

  print_parens (expr, "(");

  tree_expression *op1 = expr.base ();

  if (op1)
    op1->accept (*this);

  // Stupid syntax.

  tree_expression *op3 = expr.increment ();

  if (op3)
    {
      m_os << ':';
      op3->accept (*this);
    }

  tree_expression *op2 = expr.limit ();

  if (op2)
    {
      m_os << ':';
      op2->accept (*this);
    }

  print_parens (expr, ")");
}

void
tree_print_code::visit_continue_command (tree_continue_command&)
{
  indent ();

  m_os << "continue";
}

void
tree_print_code::visit_decl_command (tree_decl_command& cmd)
{
  indent ();

  m_os << cmd.name () << ' ';

  tree_decl_init_list *init_list = cmd.initializer_list ();

  if (init_list)
    init_list->accept (*this);
}

void
tree_print_code::visit_decl_init_list (tree_decl_init_list& lst)
{
  auto p = lst.begin ();

  while (p != lst.end ())
    {
      tree_decl_elt *elt = *p++;

      if (elt)
        {
          elt->accept (*this);

          if (p != lst.end ())
            m_os << ", ";
        }
    }
}

void
tree_print_code::visit_decl_elt (tree_decl_elt& cmd)
{
  tree_identifier *id = cmd.ident ();

  if (id)
    id->accept (*this);

  tree_expression *expr = cmd.expression ();

  if (expr)
    {
      m_os << " = ";

      expr->accept (*this);
    }
}

void
tree_print_code::visit_simple_for_command (tree_simple_for_command& cmd)
{
  print_comment_list (cmd.leading_comment ());

  indent ();

  m_os << (cmd.in_parallel () ? "parfor " : "for ");

  tree_expression *lhs = cmd.left_hand_side ();

  tree_expression *maxproc = cmd.maxproc_expr ();

  if (maxproc)
    m_os << '(';

  if (lhs)
    lhs->accept (*this);

  m_os << " = ";

  tree_expression *expr = cmd.control_expr ();

  if (expr)
    expr->accept (*this);

  if (maxproc)
    {
      m_os << ", ";
      maxproc->accept (*this);
      m_os << ')';
    }

  newline ();

  tree_statement_list *list = cmd.body ();

  if (list)
    {
      increment_indent_level ();

      list->accept (*this);

      decrement_indent_level ();
    }

  print_indented_comment (cmd.trailing_comment ());

  indent ();

  m_os << (cmd.in_parallel () ? "endparfor" : "endfor");
}

void
tree_print_code::visit_complex_for_command (tree_complex_for_command& cmd)
{
  print_comment_list (cmd.leading_comment ());

  indent ();

  m_os << "for [";
  m_nesting.push ('[');

  tree_argument_list *lhs = cmd.left_hand_side ();

  if (lhs)
    lhs->accept (*this);

  m_nesting.pop ();
  m_os << "] = ";

  tree_expression *expr = cmd.control_expr ();

  if (expr)
    expr->accept (*this);

  newline ();

  tree_statement_list *list = cmd.body ();

  if (list)
    {
      increment_indent_level ();

      list->accept (*this);

      decrement_indent_level ();
    }

  print_indented_comment (cmd.trailing_comment ());

  indent ();

  m_os << "endfor";
}

void
tree_print_code::visit_spmd_command (tree_spmd_command& cmd)
{
  print_comment_list (cmd.leading_comment ());

  indent ();

  m_os << "spmd";

  newline ();

  tree_statement_list *list = cmd.body ();

  if (list)
    {
      increment_indent_level ();

      list->accept (*this);

      decrement_indent_level ();
    }

  print_indented_comment (cmd.trailing_comment ());

  indent ();

  m_os << "endspmd";
}

void
tree_print_code::visit_octave_user_script (octave_user_script& fcn)
{
  reset ();

  tree_statement_list *cmd_list = fcn.body ();

  if (cmd_list)
    cmd_list->accept (*this);
}

void
tree_print_code::visit_octave_user_function (octave_user_function& fcn)
{
  reset ();

  visit_octave_user_function_header (fcn);

  tree_statement_list *cmd_list = fcn.body ();

  if (cmd_list)
    {
      increment_indent_level ();

      cmd_list->accept (*this);

      // endfunction will decrement the indent level.
    }

  visit_octave_user_function_trailer (fcn);
}

void
tree_print_code::visit_octave_user_function_header (octave_user_function& fcn)
{
  comment_list *leading_comment = fcn.leading_comment ();

  if (leading_comment)
    {
      print_comment_list (leading_comment);
      newline ();
    }

  indent ();

  m_os << "function ";

  tree_parameter_list *ret_list = fcn.return_list ();

  if (ret_list)
    {
      ret_list->accept (*this);

      m_os << " = ";
    }
  std::string fcn_name = fcn.name ();

  m_os << (fcn_name.empty () ? "(empty)" : fcn_name) << ' ';

  tree_parameter_list *param_list = fcn.parameter_list ();

  if (param_list)
    param_list->accept (*this);

  newline ();
}

void
tree_print_code::visit_octave_user_function_trailer (octave_user_function& fcn)
{
  print_indented_comment (fcn.trailing_comment ());

  newline ();
}

void
tree_print_code::visit_function_def (tree_function_def& fdef)
{
  indent ();

  octave_value fcn = fdef.function ();

  octave_function *f = fcn.function_value ();

  if (f)
    f->accept (*this);
}

void
tree_print_code::visit_identifier (tree_identifier& id)
{
  indent ();

  print_parens (id, "(");

  std::string nm = id.name ();
  m_os << (nm.empty () ? "(empty)" : nm);

  print_parens (id, ")");
}

void
tree_print_code::visit_if_clause (tree_if_clause& cmd)
{
  tree_expression *expr = cmd.condition ();

  if (expr)
    expr->accept (*this);

  newline ();

  tree_statement_list *list = cmd.commands ();

  if (list)
    {
      increment_indent_level ();

      list->accept (*this);

      decrement_indent_level ();
    }
}

void
tree_print_code::visit_if_command (tree_if_command& cmd)
{
  print_comment_list (cmd.leading_comment ());

  indent ();

  m_os << "if ";

  tree_if_command_list *list = cmd.cmd_list ();

  if (list)
    list->accept (*this);

  print_indented_comment (cmd.trailing_comment ());

  indent ();

  m_os << "endif";
}

void
tree_print_code::visit_if_command_list (tree_if_command_list& lst)
{
  auto p = lst.begin ();

  bool first_elt = true;

  while (p != lst.end ())
    {
      tree_if_clause *elt = *p++;

      if (elt)
        {
          if (! first_elt)
            {
              print_indented_comment (elt->leading_comment ());

              indent ();

              if (elt->is_else_clause ())
                m_os << "else";
              else
                m_os << "elseif ";
            }

          elt->accept (*this);
        }

      first_elt = false;
    }
}

void
tree_print_code::visit_index_expression (tree_index_expression& expr)
{
  indent ();

  print_parens (expr, "(");

  tree_expression *e = expr.expression ();

  if (e)
    e->accept (*this);

  std::list<tree_argument_list *> arg_lists = expr.arg_lists ();
  std::string type_tags = expr.type_tags ();
  std::list<string_vector> arg_names = expr.arg_names ();
  std::list<tree_expression *> dyn_fields = expr.dyn_fields ();

  int n = type_tags.length ();

  auto p_arg_lists = arg_lists.begin ();
  auto p_arg_names = arg_names.begin ();
  auto p_dyn_fields = dyn_fields.begin ();

  for (int i = 0; i < n; i++)
    {
      switch (type_tags[i])
        {
        case '(':
          {
            char nc = m_nesting.top ();
            if ((nc == '[' || nc == '{') && expr.paren_count () == 0)
              m_os << '(';
            else
              m_os << " (";
            m_nesting.push ('(');

            tree_argument_list *l = *p_arg_lists;
            if (l)
              l->accept (*this);

            m_nesting.pop ();
            m_os << ')';
          }
          break;

        case '{':
          {
            char nc = m_nesting.top ();
            if ((nc == '[' || nc == '{') && expr.paren_count () == 0)
              m_os << '{';
            else
              m_os << " {";
            // We only care about whitespace inside [] and {} when we
            // are defining matrix and cell objects, not when indexing.
            m_nesting.push ('(');

            tree_argument_list *l = *p_arg_lists;
            if (l)
              l->accept (*this);

            m_nesting.pop ();
            m_os << '}';
          }
          break;

        case '.':
          {
            std::string fn = (*p_arg_names)(0);
            if (fn.empty ())
              {
                tree_expression *df = *p_dyn_fields;

                if (df)
                  {
                    m_nesting.push ('(');
                    m_os << ".(";
                    df->accept (*this);
                    m_os << ")";
                    m_nesting.pop ();
                  }
              }
            else
              m_os << '.' << fn;
          }
          break;

        default:
          panic_impossible ();
        }

      p_arg_lists++;
      p_arg_names++;
      p_dyn_fields++;
    }

  print_parens (expr, ")");
}

void
tree_print_code::visit_matrix (tree_matrix& lst)
{
  indent ();

  print_parens (lst, "(");

  m_os << '[';
  m_nesting.push ('[');

  auto p = lst.begin ();

  while (p != lst.end ())
    {
      tree_argument_list *elt = *p++;

      if (elt)
        {
          elt->accept (*this);

          if (p != lst.end ())
            m_os << "; ";
        }
    }

  m_nesting.pop ();
  m_os << ']';

  print_parens (lst, ")");
}

void
tree_print_code::visit_cell (tree_cell& lst)
{
  indent ();

  print_parens (lst, "(");

  m_os << '{';
  m_nesting.push ('{');

  auto p = lst.begin ();

  while (p != lst.end ())
    {
      tree_argument_list *elt = *p++;

      if (elt)
        {
          elt->accept (*this);

          if (p != lst.end ())
            m_os << "; ";
        }
    }

  m_nesting.pop ();
  m_os << '}';

  print_parens (lst, ")");
}

void
tree_print_code::visit_multi_assignment (tree_multi_assignment& expr)
{
  indent ();

  print_parens (expr, "(");

  tree_argument_list *lhs = expr.left_hand_side ();

  if (lhs)
    {
      int len = lhs->length ();

      if (len > 1)
        {
          m_os << '[';
          m_nesting.push ('[');
        }

      lhs->accept (*this);

      if (len > 1)
        {
          m_nesting.pop ();
          m_os << ']';
        }
    }

  m_os << ' ' << expr.oper () << ' ';

  tree_expression *rhs = expr.right_hand_side ();

  if (rhs)
    rhs->accept (*this);

  print_parens (expr, ")");
}

void
tree_print_code::visit_no_op_command (tree_no_op_command& cmd)
{
  if (cmd.is_end_of_fcn_or_script () && m_curr_print_indent_level > 1)
    decrement_indent_level ();

  indent ();

  m_os << cmd.original_command ();
}

void
tree_print_code::visit_constant (tree_constant& val)
{
  indent ();

  print_parens (val, "(");

  val.print_raw (m_os, true, m_print_original_text);

  print_parens (val, ")");
}

void
tree_print_code::visit_fcn_handle (tree_fcn_handle& fh)
{
  indent ();

  print_parens (fh, "(");

  fh.print_raw (m_os, true, m_print_original_text);

  print_parens (fh, ")");
}

void
tree_print_code::visit_parameter_list (tree_parameter_list& lst)
{
  bool is_input_list = lst.is_input_list ();

  if (is_input_list)
    {
      m_os << '(';
      m_nesting.push ('(');
    }
  else
    {
      int len = lst.length ();
      if (lst.takes_varargs ())
        len++;

      if (len != 1)
        {
          m_os << '[';
          m_nesting.push ('[');
        }
    }

  auto p = lst.begin ();

  while (p != lst.end ())
    {
      tree_decl_elt *elt = *p++;

      if (elt)
        {
          elt->accept (*this);

          if (p != lst.end () || lst.takes_varargs ())
            m_os << ", ";
        }
    }

  if (lst.takes_varargs ())
    m_os << lst.varargs_symbol_name ();

  if (is_input_list)
    {
      m_nesting.pop ();
      m_os << ')';
    }
  else
    {
      int len = lst.length ();
      if (lst.takes_varargs ())
        len++;

      if (len != 1)
        {
          m_nesting.pop ();
          m_os << ']';
        }
    }
}

void
tree_print_code::visit_postfix_expression (tree_postfix_expression& expr)
{
  indent ();

  print_parens (expr, "(");

  tree_expression *e = expr.operand ();

  if (e)
    e->accept (*this);

  m_os << expr.oper ();

  print_parens (expr, ")");
}

void
tree_print_code::visit_prefix_expression (tree_prefix_expression& expr)
{
  indent ();

  print_parens (expr, "(");

  m_os << expr.oper ();

  tree_expression *e = expr.operand ();

  if (e)
    e->accept (*this);

  print_parens (expr, ")");
}

void
tree_print_code::visit_return_command (tree_return_command&)
{
  indent ();

  m_os << "return";
}

void
tree_print_code::visit_simple_assignment (tree_simple_assignment& expr)
{
  indent ();

  print_parens (expr, "(");

  tree_expression *lhs = expr.left_hand_side ();

  if (lhs)
    lhs->accept (*this);

  m_os << ' ' << expr.oper () << ' ';

  tree_expression *rhs = expr.right_hand_side ();

  if (rhs)
    rhs->accept (*this);

  print_parens (expr, ")");
}

void
tree_print_code::visit_statement (tree_statement& stmt)
{
  print_comment_list (stmt.comment_text ());

  tree_command *cmd = stmt.command ();

  if (cmd)
    {
      cmd->accept (*this);

      newline ();
    }
  else
    {
      tree_expression *expr = stmt.expression ();

      if (expr)
        {
          expr->accept (*this);

          if (! stmt.print_result ())
            {
              m_os << ';';
              newline (" ");
            }
          else
            newline ();
        }
    }
}

void
tree_print_code::visit_statement_list (tree_statement_list& lst)
{
  for (tree_statement *elt : lst)
    {
      if (elt)
        elt->accept (*this);
    }
}

void
tree_print_code::visit_switch_case (tree_switch_case& cs)
{
  print_comment_list (cs.leading_comment ());

  indent ();

  if (cs.is_default_case ())
    m_os << "otherwise";
  else
    m_os << "case ";

  tree_expression *label = cs.case_label ();

  if (label)
    label->accept (*this);

  newline ();

  tree_statement_list *list = cs.commands ();

  if (list)
    {
      increment_indent_level ();

      list->accept (*this);

      newline ();

      decrement_indent_level ();
    }
}

void
tree_print_code::visit_switch_command (tree_switch_command& cmd)
{
  print_comment_list (cmd.leading_comment ());

  indent ();

  m_os << "switch ";

  tree_expression *expr = cmd.switch_value ();

  if (expr)
    expr->accept (*this);

  newline ();

  tree_switch_case_list *list = cmd.case_list ();

  if (list)
    {
      increment_indent_level ();

      list->accept (*this);

      decrement_indent_level ();
    }

  print_indented_comment (cmd.leading_comment ());

  indent ();

  m_os << "endswitch";
}

void
tree_print_code::visit_try_catch_command (tree_try_catch_command& cmd)
{
  print_comment_list (cmd.leading_comment ());

  indent ();

  m_os << "try";

  newline ();

  tree_statement_list *try_code = cmd.body ();
  tree_identifier *expr_id = cmd.identifier ();

  if (try_code)
    {
      increment_indent_level ();

      try_code->accept (*this);

      decrement_indent_level ();
    }

  print_indented_comment (cmd.middle_comment ());

  indent ();

  m_os << "catch";

  if (expr_id)
    {
      m_os << ' ';
      expr_id->accept (*this);
    }

  newline ();

  tree_statement_list *catch_code = cmd.cleanup ();

  if (catch_code)
    {
      increment_indent_level ();

      catch_code->accept (*this);

      decrement_indent_level ();
    }

  print_indented_comment (cmd.trailing_comment ());

  indent ();

  m_os << "end_try_catch";
}

void
tree_print_code::visit_unwind_protect_command (tree_unwind_protect_command& cmd)
{
  print_comment_list (cmd.leading_comment ());

  indent ();

  m_os << "unwind_protect";

  newline ();

  tree_statement_list *unwind_protect_code = cmd.body ();

  if (unwind_protect_code)
    {
      increment_indent_level ();

      unwind_protect_code->accept (*this);

      decrement_indent_level ();
    }

  print_indented_comment (cmd.middle_comment ());

  indent ();

  m_os << "unwind_protect_cleanup";

  newline ();

  tree_statement_list *cleanup_code = cmd.cleanup ();

  if (cleanup_code)
    {
      increment_indent_level ();

      cleanup_code->accept (*this);

      decrement_indent_level ();
    }

  print_indented_comment (cmd.trailing_comment ());

  indent ();

  m_os << "end_unwind_protect";
}

void
tree_print_code::visit_while_command (tree_while_command& cmd)
{
  print_comment_list (cmd.leading_comment ());

  indent ();

  m_os << "while ";

  tree_expression *expr = cmd.condition ();

  if (expr)
    expr->accept (*this);

  newline ();

  tree_statement_list *list = cmd.body ();

  if (list)
    {
      increment_indent_level ();

      list->accept (*this);

      decrement_indent_level ();
    }

  print_indented_comment (cmd.trailing_comment ());

  indent ();

  m_os << "endwhile";
}

void
tree_print_code::visit_do_until_command (tree_do_until_command& cmd)
{
  print_comment_list (cmd.leading_comment ());

  indent ();

  m_os << "do";

  newline ();

  tree_statement_list *list = cmd.body ();

  if (list)
    {
      increment_indent_level ();

      list->accept (*this);

      decrement_indent_level ();
    }

  print_indented_comment (cmd.trailing_comment ());

  indent ();

  m_os << "until ";

  tree_expression *expr = cmd.condition ();

  if (expr)
    expr->accept (*this);

  newline ();
}

void
tree_print_code::visit_superclass_ref (tree_superclass_ref& scr)
{
  m_os << scr.method_name () << "@" << scr.class_name ();
}

void
tree_print_code::visit_metaclass_query (tree_metaclass_query& mcq)
{
  m_os << "?" << mcq.class_name ();
}

void
tree_print_code::print_fcn_handle_body (tree_expression *e)
{
  if (e)
    {
      m_suppress_newlines++;
      e->accept (*this);
      m_suppress_newlines--;
    }
}

// Each print_code() function should call this before printing anything.

void
tree_print_code::indent (void)
{
  panic_unless (m_curr_print_indent_level >= 0);

  if (m_beginning_of_line)
    {
      m_os << m_prefix;

      m_os << std::string (m_curr_print_indent_level, ' ');

      m_beginning_of_line = false;
    }
}

// All print_code() functions should use this to print new lines.

void
tree_print_code::newline (const char *alt_txt)
{
  if (m_suppress_newlines)
    m_os << alt_txt;
  else
    {
      // Print prefix for blank lines.
      indent ();

      m_os << "\n";

      m_beginning_of_line = true;
    }
}

// For resetting print_code state.

void
tree_print_code::reset (void)
{
  m_beginning_of_line = true;
  m_curr_print_indent_level = 0;
  while (m_nesting.top () != 'n')
    m_nesting.pop ();
}

void
tree_print_code::print_parens (const tree_expression& expr, const char *txt)
{
  int n = expr.paren_count ();

  for (int i = 0; i < n; i++)
    m_os << txt;
}

void
tree_print_code::print_comment_elt (const comment_elt& elt)
{
  bool printed_something = false;

  bool prev_char_was_newline = false;

  std::string comment = elt.text ();

  std::size_t len = comment.length ();

  std::size_t i = 0;

  while (i < len && comment[i++] == '\n')
    ; // Skip leading new lines.
  i--;

  while (i < len)
    {
      char c = comment[i++];

      if (c == '\n')
        {
          if (prev_char_was_newline)
            {
              printed_something = true;

              indent ();

              m_os << "##";
            }

          newline ();

          prev_char_was_newline = true;
        }
      else
        {
          if (m_beginning_of_line)
            {
              printed_something = true;

              indent ();

              m_os << "##";

              if (! (isspace (c) || c == '!'))
                m_os << ' ';
            }

          m_os << static_cast<char> (c);

          prev_char_was_newline = false;
        }
    }

  if (printed_something && ! m_beginning_of_line)
    newline ();
}

void
tree_print_code::print_comment_list (comment_list *comment_list)
{
  if (comment_list)
    {
      auto p = comment_list->begin ();

      while (p != comment_list->end ())
        {
          comment_elt elt = *p++;

          print_comment_elt (elt);

          if (p != comment_list->end ())
            newline ();
        }
    }
}

void
tree_print_code::print_indented_comment (comment_list *comment_list)
{
  increment_indent_level ();

  print_comment_list (comment_list);

  decrement_indent_level ();
}

OCTAVE_END_NAMESPACE(octave)
