/*

Copyright (C) 1996-2015 John W. Eaton

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

#include "error.h"
#include "oct-obj.h"
#include "pager.h"
#include "ov.h"
#include "pt-bp.h"
#include "pt-colon.h"
#include "pt-walk.h"

// Colon expressions.

tree_colon_expression *
tree_colon_expression::append (tree_expression *t)
{
  tree_colon_expression *retval = 0;

  if (op_base)
    {
      if (op_limit)
        {
          if (op_increment)
            ::error ("invalid colon expression");
          else
            {
              // Stupid syntax:
              //
              // base : limit
              // base : increment : limit

              op_increment = op_limit;
              op_limit = t;
            }
        }
      else
        op_limit = t;

      retval = this;
    }
  else
    ::error ("invalid colon expression");

  return retval;
}

octave_value_list
tree_colon_expression::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("invalid number of output arguments for colon expression");
  else
    retval = rvalue1 (nargout);

  return retval;
}

octave_value
tree_colon_expression::make_range (const Matrix& m_base,
                                   const Matrix& m_limit,
                                   const Matrix& m_increment,
                                   bool result_is_str, bool dq_str) const
{
  octave_value retval;

  bool base_empty = m_base.is_empty ();
  bool limit_empty = m_limit.is_empty ();
  bool increment_empty = m_increment.is_empty ();

  if (base_empty || limit_empty || increment_empty)
    retval = Range ();
  else
    {
      Range r (m_base(0), m_limit(0), m_increment(0));

      // For compatibility with Matlab, don't allow the range used in
      // a FOR loop expression to be converted to a Matrix.

      retval = octave_value (r, is_for_cmd_expr ());

      if (result_is_str)
        retval = retval.convert_to_str (false, true, dq_str ? '"' : '\'');
    }

  return retval;
}

octave_value
tree_colon_expression::make_range (const octave_value& ov_base,
                                   const octave_value& ov_limit,
                                   const octave_value& ov_increment) const
{
  octave_value retval;

  if (ov_base.is_object () || ov_limit.is_object () ||
      ov_increment.is_object ())
    {
      octave_value_list tmp1;
      tmp1(2) = ov_limit;
      tmp1(1) = ov_increment;
      tmp1(0) = ov_base;

      octave_value fcn = symbol_table::find_function ("colon", tmp1);

      if (fcn.is_defined ())
        {
          octave_value_list tmp2 = fcn.do_multi_index_op (1, tmp1);

          if (! error_state)
            retval = tmp2 (0);
        }
      else
        ::error ("can not find overloaded colon function");
    }
  else
    {
      bool result_is_str = (ov_base.is_string () && ov_limit.is_string ());
      bool dq_str = (ov_base.is_dq_string () || ov_limit.is_dq_string ());

      Matrix m_base = ov_base.matrix_value (true);

      if (error_state)
        eval_error ("invalid base value in colon expression");
      else
        {
          Matrix m_limit = ov_limit.matrix_value (true);

          if (error_state)
            eval_error ("invalid limit value in colon expression");
          else
            {
              Matrix m_increment = ov_increment.matrix_value (true);

              if (error_state)
                eval_error ("invalid increment value in colon expression");
              else
                retval = make_range (m_base, m_limit, m_increment,
                                     result_is_str, dq_str);
            }
        }
    }

  return retval;
}

octave_value
tree_colon_expression::rvalue1 (int)
{
  octave_value retval;

  if (error_state || ! op_base || ! op_limit)
    return retval;

  octave_value ov_base = op_base->rvalue1 ();

  if (error_state || ov_base.is_undefined ())
    eval_error ("invalid base value in colon expression");
  else
    {
      octave_value ov_limit = op_limit->rvalue1 ();

      if (error_state || ov_limit.is_undefined ())
        eval_error ("invalid limit value in colon expression");
      else if (ov_base.is_object () || ov_limit.is_object ())
        {
          octave_value_list tmp1;

          if (op_increment)
            {
              octave_value ov_increment = op_increment->rvalue1 ();

              if (error_state || ov_increment.is_undefined ())
                eval_error ("invalid increment value in colon expression");
              else
                {
                  tmp1(2) = ov_limit;
                  tmp1(1) = ov_increment;
                  tmp1(0) = ov_base;
                }
            }
          else
            {
              tmp1(1) = ov_limit;
              tmp1(0) = ov_base;
            }

          if (!error_state)
            {
              octave_value fcn = symbol_table::find_function ("colon", tmp1);

              if (fcn.is_defined ())
                {
                  octave_value_list tmp2 = fcn.do_multi_index_op (1, tmp1);

                  if (! error_state)
                    retval = tmp2 (0);
                }
              else
                ::error ("can not find overloaded colon function");
            }
        }
      else
        {
          octave_value ov_increment = 1.0;

          if (op_increment)
            {
              ov_increment = op_increment->rvalue1 ();

              if (error_state || ov_increment.is_undefined ())
                eval_error ("invalid increment value in colon expression");
            }

          if (! error_state)
            retval = make_range (ov_base, ov_limit, ov_increment);
        }
    }

  return retval;
}

void
tree_colon_expression::eval_error (const std::string& s) const
{
  ::error ("%s", s.c_str ());
}

int
tree_colon_expression::line (void) const
{
  return (op_base ? op_base->line ()
          : (op_increment ? op_increment->line ()
             : (op_limit ? op_limit->line ()
                : -1)));
}

int
tree_colon_expression::column (void) const
{
  return (op_base ? op_base->column ()
          : (op_increment ? op_increment->column ()
             : (op_limit ? op_limit->column ()
                : -1)));
}

tree_expression *
tree_colon_expression::dup (symbol_table::scope_id scope,
                            symbol_table::context_id context) const
{
  tree_colon_expression *new_ce = new
    tree_colon_expression (op_base ? op_base->dup (scope, context) : 0,
                           op_limit ? op_limit->dup (scope, context) : 0,
                           op_increment ? op_increment->dup (scope, context)
                                        : 0,
                           line (), column ());

  new_ce->copy_base (*new_ce);

  return new_ce;
}

void
tree_colon_expression::accept (tree_walker& tw)
{
  tw.visit_colon_expression (*this);
}
