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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "Cell.h"
#include "error.h"
#include "interpreter-private.h"
#include "oct-map.h"
#include "ovl.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "pt-arg-list.h"
#include "pt-eval.h"
#include "pt-id.h"
#include "pt-idx.h"
#include "utils.h"
#include "variables.h"
#include "errwarn.h"

namespace octave
{
  // Index expressions.

  tree_index_expression::tree_index_expression (int l, int c)
    : tree_expression (l, c), m_expr (nullptr), m_args (0), m_type (),
      m_arg_nm (), m_dyn_field () { }

  tree_index_expression::tree_index_expression (tree_expression *e,
                                                tree_argument_list *lst,
                                                int l, int c, char t)
    : tree_expression (l, c), m_expr (e), m_args (0), m_type (),
      m_arg_nm (), m_dyn_field ()
  {
    append (lst, t);
  }

  tree_index_expression::tree_index_expression (tree_expression *e,
                                                const std::string& n,
                                                int l, int c)
    : tree_expression (l, c), m_expr (e), m_args (0), m_type (),
      m_arg_nm (), m_dyn_field ()
  {
    append (n);
  }

  tree_index_expression::tree_index_expression (tree_expression *e,
                                                tree_expression *df,
                                                int l, int c)
    : tree_expression (l, c), m_expr (e), m_args (0), m_type (),
      m_arg_nm (), m_dyn_field ()
  {
    append (df);
  }

  void
  tree_index_expression::append (tree_argument_list *lst, char t)
  {
    m_args.push_back (lst);
    m_type.append (1, t);
    m_arg_nm.push_back (lst ? lst->get_arg_names () : string_vector ());
    m_dyn_field.push_back (static_cast<tree_expression *> (nullptr));

    if (lst && lst->has_magic_tilde ())
      error ("invalid use of empty argument (~) in index expression");
  }

  void
  tree_index_expression::append (const std::string& n)
  {
    m_args.push_back (static_cast<tree_argument_list *> (nullptr));
    m_type += '.';
    m_arg_nm.push_back (n);
    m_dyn_field.push_back (static_cast<tree_expression *> (nullptr));
  }

  void
  tree_index_expression::append (tree_expression *df)
  {
    m_args.push_back (static_cast<tree_argument_list *> (nullptr));
    m_type += '.';
    m_arg_nm.push_back ("");
    m_dyn_field.push_back (df);
  }

  tree_index_expression::~tree_index_expression (void)
  {
    delete m_expr;

    while (! m_args.empty ())
      {
        auto p = m_args.begin ();
        delete *p;
        m_args.erase (p);
      }

    while (! m_dyn_field.empty ())
      {
        auto p = m_dyn_field.begin ();
        delete *p;
        m_dyn_field.erase (p);
      }
  }

  bool
  tree_index_expression::has_magic_end (void) const
  {
    for (const tree_argument_list *elt : m_args)
      {
        if (elt && elt->has_magic_end ())
          return true;
      }

    return false;
  }

  // This is useful for printing the name of the variable in an indexed
  // assignment.

  std::string
  tree_index_expression::name (void) const
  {
    return m_expr->name ();
  }

  static inline octave_value_list
  make_value_list (tree_evaluator& tw,
                   tree_argument_list *m_args,
                   const string_vector& m_arg_nm, const octave_value *object,
                   bool rvalue = true)
  {
    octave_value_list retval;

    if (m_args)
      {
        if (rvalue && object && m_args->has_magic_end ()
            && object->is_undefined ())
          err_invalid_inquiry_subscript ();

        retval = tw.convert_to_const_vector (m_args, object);
      }

    octave_idx_type n = retval.length ();

    if (n > 0)
      retval.stash_name_tags (m_arg_nm);

    return retval;
  }

  std::string
  tree_index_expression::get_struct_index
  (tree_evaluator& tw,
   std::list<string_vector>::const_iterator p_arg_nm,
   std::list<tree_expression *>::const_iterator p_dyn_field) const
  {
    std::string fn = (*p_arg_nm)(0);

    if (fn.empty ())
      {
        tree_expression *df = *p_dyn_field;

        if (df)
          {
            octave_value t = tw.evaluate (df);

            fn = t.xstring_value ("dynamic structure field names must be strings");
          }
        else
          panic_impossible ();
      }

    return fn;
  }

  // Final step of processing an indexing error.  Add the name of the
  // variable being indexed, if any, then issue an error.  (Will this also
  // be needed by pt-lvalue, which calls subsref?)

  static void
  final_index_error (index_exception& e, const tree_expression *expr)
  {
    std::string extra_message;

    symbol_table& symtab = __get_symbol_table__ ("final_index_error");

    symbol_record::context_id context = symtab.current_context ();

    if (expr->is_identifier ()
        && dynamic_cast<const tree_identifier *> (expr)->is_variable (context))
      {
        std::string var = expr->name ();

        e.set_var (var);

        octave_value fcn = symtab.find_function (var);

        if (fcn.is_function ())
          {
            octave_function *fp = fcn.function_value ();

            if (fp && fp->name () == var)
              extra_message = " (note: variable '" + var + "' shadows function)";
          }
      }

    std::string msg = e.message () + extra_message;

    error_with_id (e.err_id (), msg.c_str ());
  }

  octave_lvalue
  tree_index_expression::lvalue (tree_evaluator& tw)
  {
    octave_lvalue retval;

    std::list<octave_value_list> idx;
    std::string tmp_type;

    int n = m_args.size ();

    auto p_args = m_args.begin ();
    auto p_arg_nm = m_arg_nm.begin ();
    auto p_dyn_field = m_dyn_field.begin ();

    retval = m_expr->lvalue (tw);

    octave_value tmp = retval.value ();

    octave_idx_type tmpi = 0;
    std::list<octave_value_list> tmpidx;

    for (int i = 0; i < n; i++)
      {
        if (retval.numel () != 1)
          err_indexed_cs_list ();

        if (tmpi < i)
          {
            try
              {
                tmp = tmp.subsref (m_type.substr (tmpi, i-tmpi), tmpidx, true);
              }
            catch (index_exception& e)  // problems with range, invalid type etc.
              {
                final_index_error (e, m_expr);
              }

            tmpidx.clear ();
          }

        switch (m_type[i])
          {
          case '(':
            {
              octave_value_list tidx
                = make_value_list (tw, *p_args, *p_arg_nm, &tmp, false);

              idx.push_back (tidx);

              if (i < n - 1)
                {
                  if (m_type[i+1] != '.')
                    error ("() must be followed by . or close the index chain");

                  tmpidx.push_back (tidx);
                  tmpi = i+1;
                }
            }
            break;

          case '{':
            {
              octave_value_list tidx
                = make_value_list (tw, *p_args, *p_arg_nm, &tmp, false);

              if (tmp.is_undefined ())
                {
                  if (tidx.has_magic_colon ())
                    err_invalid_inquiry_subscript ();

                  tmp = Cell ();
                }
              else if (tmp.is_zero_by_zero ()
                       && (tmp.is_matrix_type () || tmp.is_string ()))
                {
                  tmp = Cell ();
                }

              retval.numel (tmp.numel (tidx));

              idx.push_back (tidx);
              tmpidx.push_back (tidx);
              tmpi = i;
            }
            break;

          case '.':
            {
              octave_value tidx = get_struct_index (tw, p_arg_nm, p_dyn_field);

              bool autoconv = (tmp.is_zero_by_zero ()
                               && (tmp.is_matrix_type () || tmp.is_string ()
                                   || tmp.iscell ()));

              if (i > 0 && m_type[i-1] == '(')
                {
                  octave_value_list pidx = idx.back ();

                  // Use octave_map, not octave_scalar_map so that the
                  // dimensions are 0x0, not 1x1.
                  if (tmp.is_undefined ())
                    {
                      if (pidx.has_magic_colon ())
                        err_invalid_inquiry_subscript ();

                      tmp = octave_map ();
                    }
                  else if (autoconv)
                    tmp = octave_map ();

                  retval.numel (tmp.numel (pidx));

                  tmpi = i-1;
                  tmpidx.push_back (tidx);
                }
              else
                {
                  if (tmp.is_undefined () || autoconv)
                    {
                      tmpi = i+1;
                      tmp = octave_value ();
                    }
                  else
                    {
                      retval.numel (tmp.numel (octave_value_list ()));

                      tmpi = i;
                      tmpidx.push_back (tidx);
                    }
                }

              idx.push_back (tidx);
            }
            break;

          default:
            panic_impossible ();
          }

        if (idx.back ().empty ())
          error ("invalid empty index list");

        p_args++;
        p_arg_nm++;
        p_dyn_field++;
      }

    retval.set_index (m_type, idx);

    return retval;
  }

  tree_index_expression *
  tree_index_expression::dup (symbol_scope& scope) const
  {
    tree_index_expression *new_idx_expr
      = new tree_index_expression (line (), column ());

    new_idx_expr->m_expr = (m_expr ? m_expr->dup (scope) : nullptr);

    std::list<tree_argument_list *> new_args;

    for (const tree_argument_list *elt : m_args)
      new_args.push_back (elt ? elt->dup (scope) : nullptr);

    new_idx_expr->m_args = new_args;

    new_idx_expr->m_type = m_type;

    new_idx_expr->m_arg_nm = m_arg_nm;

    std::list<tree_expression *> new_dyn_field;

    for (const tree_expression *elt : m_dyn_field)
      new_dyn_field.push_back (elt ? elt->dup (scope) : nullptr);

    new_idx_expr->m_dyn_field = new_dyn_field;

    new_idx_expr->copy_base (*this);

    return new_idx_expr;
  }
}

/*
%!test
%! clear x;
%! clear y;
%! y = 3;
%! x(y(end)) = 1;
%! assert (x, [0, 0, 1]);
%! clear x;
%! clear y;
%! y = {3};
%! x(y{end}) = 1;
%! assert (x, [0, 0, 1]);

%!test
%! x = {1, 2, 3};
%! [x{:}] = deal (4, 5, 6);
%! assert (x, {4, 5, 6});

%!test
%! [x.a, x.b.c] = deal (1, 2);
%! assert (x.a == 1 && x.b.c == 2);

%!test
%! [x.a, x(2).b] = deal (1, 2);
%! assert (x(1).a == 1 && isempty (x(2).a) && isempty (x(1).b) && x(2).b == 2);

%!test
%! x = struct (zeros (0, 1), {"a", "b"});
%! x(2).b = 1;
%! assert (x(2).b == 1);

%!test
%! x = struct (zeros (0, 1), {"a", "b"});
%! x(2).b = 1;
%! assert (x(2).b == 1);
*/
