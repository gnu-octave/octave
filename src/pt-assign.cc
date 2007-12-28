/*

Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2005,
              2006, 2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <set>

#include "defun.h"
#include "error.h"
#include "input.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "pager.h"
#include "ov.h"
#include "pt-arg-list.h"
#include "pt-bp.h"
#include "pt-assign.h"
#include "pt-walk.h"
#include "utils.h"
#include "variables.h"

// Simple assignment expressions.

static const char *former_built_in_variables[] =
{
  "DEFAULT_EXEC_PATH",
  "DEFAULT_LOADPATH",
  "EDITOR",
  "EXEC_PATH",
  "FFTW_WISDOM_PROGRAM",
  "IMAGEPATH",
  "INFO_FILE",
  "INFO_PROGRAM",
  "LOADPATH",
  "MAKEINFO_PROGRAM",
  "PAGER",
  "PS1",
  "PS2",
  "PS4",
  "__kluge_procbuf_delay__",
  "ans",
  "automatic_replot",
  "beep_on_error",
  "completion_append_char",
  "crash_dumps_octave_core",
  "current_script_file_name",
  "debug_on_error",
  "debug_on_interrupt",
  "debug_on_warning",
  "debug_symtab_lookups",
  "default_save_format",
  "echo_executing_commands",
  "fixed_point_format",
  "gnuplot_binary",
  "gnuplot_command_axes",
  "gnuplot_command_end",
  "gnuplot_command_plot",
  "gnuplot_command_replot",
  "gnuplot_command_splot",
  "gnuplot_command_title",
  "gnuplot_command_using",
  "gnuplot_command_with",
  "gnuplot_has_frames",
  "history_file",
  "history_size",
  "ignore_function_time_stamp",
  "max_recursion_depth",
  "octave_core_file_format",
  "octave_core_file_limit",
  "octave_core_file_name",
  "output_max_field_width",
  "output_precision",
  "page_output_immediately",
  "page_screen_output",
  "print_answer_id_name",
  "print_empty_dimensions",
  "print_rhs_assign_val",
  "save_header_format_string",
  "save_precision",
  "saving_history",
  "sighup_dumps_octave_core",
  "sigterm_dumps_octave_core",
  "silent_functions",
  "split_long_rows",
  "string_fill_char",
  "struct_levels_to_print",
  "suppress_verbose_help_message",
  "variables_can_hide_functions",
  "warn_assign_as_truth_value",
  "warn_associativity_change",
  "warn_divide_by_zero",
  "warn_empty_list_elements",
  "warn_fortran_indexing",
  "warn_function_name_clash",
  "warn_future_time_stamp",
  "warn_imag_to_real",
  "warn_matlab_incompatible",
  "warn_missing_semicolon",
  "warn_neg_dim_as_zero",
  "warn_num_to_str",
  "warn_precedence_change",
  "warn_reload_forces_clear",
  "warn_resize_on_range_error",
  "warn_separator_insert",
  "warn_single_quote_string",
  "warn_str_to_num",
  "warn_undefined_return_values",
  "warn_variable_switch_label",
  "whos_line_format",
  0,
};

static void
maybe_warn_former_built_in_variable (const std::string& nm)
{
  static bool initialized = false;

  static std::set<std::string> vars;

  if (! initialized)
    {
      const char **p = former_built_in_variables;

      while (*p)
	vars.insert (*p++);
    }

  if (vars.find (nm) != vars.end ())
    warning_with_id ("Octave:built-in-variable-assignment",
		     "%s is no longer a built-in variable; please read the NEWS file or type `news' for details",
		     nm.c_str ());
}

tree_simple_assignment::tree_simple_assignment
  (tree_expression *le, tree_expression *re,
   bool plhs, int l, int c, octave_value::assign_op t)
    : tree_expression (l, c), lhs (le), rhs (re), preserve (plhs), etype (t)
{
  if (lhs)
    maybe_warn_former_built_in_variable (lhs->name ());
}

tree_simple_assignment::~tree_simple_assignment (void)
{
  if (! preserve)
    delete lhs;

  delete rhs;
}

octave_value_list
tree_simple_assignment::rvalue (int nargout)
{
  octave_value_list retval;

  MAYBE_DO_BREAKPOINT;

  if (nargout > 1)
    error ("invalid number of output arguments for expression X = RHS");
  else
    retval = rvalue ();

  return retval;
}

// FIXME -- this works, but it would look a little better if
// it were broken up into a couple of separate functions.

octave_value
tree_simple_assignment::rvalue (void)
{
  octave_value retval;

  if (error_state)
    return retval;

  if (rhs)
    {
      octave_value_list tmp = rhs->rvalue ();

      if (! (error_state || tmp.empty ()))
	{
	  octave_value rhs_val = tmp(0);

	  if (rhs_val.is_undefined ())
	    {
	      error ("value on right hand side of assignment is undefined");
	      eval_error ();
	    }
	  else if (rhs_val.is_cs_list ())
	    {
	      error ("invalid assignment of comma-separated list");
	      eval_error ();
	    }
	  else
	    {
	      octave_lvalue ult = lhs->lvalue ();

	      if (error_state)
		eval_error ();
	      else
		{
		  ult.assign (etype, rhs_val);

		  if (! error_state)
		    {
		      if (etype == octave_value::op_asn_eq)
			retval = rhs_val;
		      else
			retval = ult.value ();

		      if (print_result ())
			{
			  // We clear any index here so that we can
			  // get the new value of the referenced
			  // object below, instead of the indexed
			  // value (which should be the same as the
			  // right hand side value).

			  ult.clear_index ();

			  octave_value lhs_val = ult.value ();

			  if (! error_state)
			    lhs_val.print_with_name (octave_stdout,
						     lhs->name ());
			}
		    }
		  else
		    eval_error ();
		}
	    }
	}
      else
	eval_error ();
    }

  return retval;
}

void
tree_simple_assignment::eval_error (void)
{
  int l = line ();
  int c = column ();

  if (l != -1 && c != -1)
    ::error ("evaluating assignment expression near line %d, column %d",
	     l, c);
}

std::string
tree_simple_assignment::oper (void) const
{
  return octave_value::assign_op_as_string (etype);
}

tree_expression *
tree_simple_assignment::dup (symbol_table::scope_id scope)
{
  tree_simple_assignment *new_sa
    = new tree_simple_assignment (lhs ? lhs->dup (scope) : 0,
				  rhs ? rhs->dup (scope) : 0,
				  preserve, etype);

  new_sa->copy_base (*this);

  return new_sa;
}

void
tree_simple_assignment::accept (tree_walker& tw)
{
  tw.visit_simple_assignment (*this);
}

// Multi-valued assignment expressions.

tree_multi_assignment::tree_multi_assignment
  (tree_argument_list *lst, tree_expression *r,
   bool plhs, int l, int c, octave_value::assign_op t)
    : tree_expression (l, c), lhs (lst), rhs (r), preserve (plhs), etype (t)
{
  for (tree_argument_list::iterator p = lhs->begin (); p != lhs->end (); p++)
    {
      tree_expression *lhs_expr = *p;

      if (lhs_expr)
	maybe_warn_former_built_in_variable (lhs_expr->name ());
    }
}

tree_multi_assignment::~tree_multi_assignment (void)
{
  if (! preserve)
    delete lhs;

  delete rhs;
}

octave_value
tree_multi_assignment::rvalue (void)
{
  octave_value retval;

  octave_value_list tmp = rvalue (1);

  if (! tmp.empty ())
    retval = tmp(0);

  return retval;
}

// FIXME -- this works, but it would look a little better if
// it were broken up into a couple of separate functions.

octave_value_list
tree_multi_assignment::rvalue (int)
{
  octave_value_list retval;

  if (error_state)
    return retval;

  if (rhs)
    {
      std::list<octave_lvalue> lvalue_list = lhs->lvalue_list ();

      if (error_state)
	return retval;

      int n_out = 0;

      for (std::list<octave_lvalue>::const_iterator p = lvalue_list.begin ();
	   p != lvalue_list.end ();
	   p++)
	n_out += p->numel ();

      octave_value_list rhs_val = rhs->rvalue (n_out);

      if (error_state)
	return retval;

      if (rhs_val.empty ())
	{
	  if (n_out > 0)
	    {
	      error ("value on right hand side of assignment is undefined");
	      eval_error ();
	      return retval;
	    }
	}
      else
	{
	  octave_idx_type k = 0;

	  octave_idx_type n = rhs_val.length ();

	  if (n == 1)
	    {
	      octave_value tmp = rhs_val(0);

	      if (tmp.is_cs_list ())
		{
		  error ("invalid assignment of comma-separated list");
		  eval_error ();
		  return retval;
		}
	    }

	  retval.resize (n, octave_value ());

	  tree_argument_list::iterator q = lhs->begin ();

	  for (std::list<octave_lvalue>::iterator p = lvalue_list.begin ();
	       p != lvalue_list.end ();
	       p++)
	    {
	      tree_expression *lhs_elt = *q++;

	      octave_lvalue ult = *p;

	      octave_idx_type nel = ult.numel ();

	      if (nel > 1)
		{
		  if (k + nel <= n)
		    {
		      if (etype == octave_value::op_asn_eq)
			{
			  octave_value_list ovl (nel, octave_value ());

			  for (octave_idx_type j = 0; j < nel; j++)
			    ovl(j) = rhs_val(k+j);

			  ult.assign (etype, octave_value (ovl, true));

			  if (! error_state)
			    {
			      for (octave_idx_type j = 0; j < nel; j++)
				retval(k+j) = rhs_val(k+j);

			      k += nel;
			    }
			}
		      else
			{
			  std::string op = octave_value::assign_op_as_string (etype);
			  error ("operator %s unsupported for comma-separated list assignment",
				 op.c_str ());
			}
		    }
		  else
		    error ("some elements undefined in return list");
		}
	      else
		{
		  if (k < n)
		    {
		      ult.assign (etype, rhs_val(k));

		      if (! error_state)
			{
			  if (etype == octave_value::op_asn_eq)
			    retval(k) = rhs_val(k);
			  else
			    retval(k) = ult.value ();

			  k++;
			}
		    }
		  else
		    error ("element number %d undefined in return list", k+1);
		}

	      if (error_state)
		{
		  eval_error ();
		  break;
		}
	      else if (print_result ())
		{
		  // We clear any index here so that we can get
		  // the new value of the referenced object below,
		  // instead of the indexed value (which should be
		  // the same as the right hand side value).

		  ult.clear_index ();

		  octave_value lhs_val = ult.value ();

		  if (! error_state)
		    lhs_val.print_with_name (octave_stdout,
					     lhs_elt->name ());
		}

	      if (error_state)
		break;

	    }
	}
    }
  else
    eval_error ();

  return retval;
}

void
tree_multi_assignment::eval_error (void)
{
  int l = line ();
  int c = column ();

  if (l != -1 && c != -1)
    ::error ("evaluating assignment expression near line %d, column %d",
	     l, c);
}

std::string
tree_multi_assignment::oper (void) const
{
  return octave_value::assign_op_as_string (etype);
}

tree_expression *
tree_multi_assignment::dup (symbol_table::scope_id scope)
{
  tree_multi_assignment *new_ma
    = new tree_multi_assignment (lhs ? lhs->dup (scope) : 0,
				 rhs ? rhs->dup (scope) : 0,
				 preserve, etype);

  new_ma->copy_base (*this);

  return new_ma;
}

void
tree_multi_assignment::accept (tree_walker& tw)
{
  tw.visit_multi_assignment (*this);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
