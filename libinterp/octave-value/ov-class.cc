////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007-2023 The Octave Project Developers
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

#include <istream>
#include <memory>
#include <ostream>

#include "Array-util.h"
#include "byte-swap.h"
#include "oct-locbuf.h"
#include "lo-mappers.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "file-ops.h"
#include "errwarn.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "load-path.h"
#include "ls-hdf5.h"
#include "ls-oct-text.h"
#include "ls-oct-binary.h"
#include "ls-utils.h"
#include "mxarray.h"
#include "oct-lvalue.h"
#include "oct-hdf5.h"
#include "ov-class.h"
#include "ov-fcn.h"
#include "ov-typeinfo.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "parse.h"
#include "pr-output.h"
#include "unwind-prot.h"
#include "variables.h"


int octave_class::t_id (-1);

const std::string octave_class::t_name ("class");

void
octave_class::register_type (octave::type_info& ti)
{
  t_id = ti.register_type (octave_class::t_name, "<unknown>",
                           octave_value (new octave_class ()));
}

octave_class::octave_class (const octave_map& m, const std::string& id,
                            const octave_value_list& parents)
  : octave_base_value (), m_map (m), c_name (id), m_obsolete_copies (0)
{
  octave_idx_type n = parents.length ();

  for (octave_idx_type idx = 0; idx < n; idx++)
    {
      octave_value parent = parents(idx);

      if (! parent.isobject ())
        error ("parents must be objects");

      std::string pcnm = parent.class_name ();

      if (find_parent_class (pcnm))
        error ("duplicate class in parent tree");

      m_parent_list.push_back (pcnm);

      octave_idx_type nel = m_map.numel ();
      octave_idx_type p_nel = parent.numel ();

      if (nel == 0)
        {
          if (p_nel == 0)
            {
              // No elements in MAP or the parent class object,
              // so just add the field name.

              m_map.assign (pcnm, Cell (m_map.dims ()));
            }
          else if (p_nel == 1)
            {
              if (m_map.nfields () == 0)
                {
                  // No elements or fields in MAP, but the
                  // parent is class object with one element.
                  // Resize to match size of parent class and
                  // make the parent a field in MAP.

                  m_map.resize (parent.dims ());

                  m_map.assign (pcnm, parent);
                }
              else
                {
                  // No elements in MAP, but we have at least
                  // one field.  So don't resize, just add the
                  // field name.

                  m_map.assign (pcnm, Cell (m_map.dims ()));
                }
            }
          else if (m_map.nfields () == 0)
            {
              // No elements or fields in MAP and more than one
              // element in the parent class object, so we can
              // resize MAP to match parent dimsenions, then
              // distribute the elements of the parent object to
              // the elements of MAP.

              dim_vector parent_dims = parent.dims ();

              m_map.resize (parent_dims);

              Cell c (parent_dims);

              octave_map pmap = parent.map_value ();

              std::list<std::string> plist
                = parent.parent_class_name_list ();

              for (octave_idx_type i = 0; i < p_nel; i++)
                c(i) = octave_value (pmap.index (i), pcnm, plist);

              m_map.assign (pcnm, c);
            }
          else
            error ("class: parent class dimension mismatch");
        }
      else if (nel == 1 && p_nel == 1)
        {
          // Simple assignment.

          m_map.assign (pcnm, parent);
        }
      else
        {
          if (p_nel == 1)
            {
              // Broadcast the scalar parent class object to
              // each element of MAP.

              Cell pcell (m_map.dims (), parent);

              m_map.assign (pcnm, pcell);
            }
          else if (nel == p_nel)
            {
              // FIXME: is there a better way to do this?

              // The parent class object has the same number of
              // elements as the map we are using to create the
              // new object, so distribute those elements to
              // each element of the new object by first
              // splitting the elements of the parent class
              // object into a cell array with one element per
              // cell.  Then do the assignment all at once.

              Cell c (parent.dims ());

              octave_map pmap = parent.map_value ();

              std::list<std::string> plist
                = parent.parent_class_name_list ();

              for (octave_idx_type i = 0; i < p_nel; i++)
                c(i) = octave_value (pmap.index (i), pcnm, plist);

              m_map.assign (pcnm, c);
            }
          else
            error ("class: parent class dimension mismatch");
        }
    }

  octave::symbol_table& symtab = octave::__get_symbol_table__ ();

  symtab.add_to_parent_map (id, m_parent_list);
}

octave_base_value *
octave_class::unique_clone (void)
{
  if (count == m_obsolete_copies)
    {
      // All remaining copies are obsolete.  We don't actually need to clone.
      count++;
      return this;
    }
  else
    {
      // In theory, this shouldn't be happening, but it's here just in case.
      if (count < m_obsolete_copies)
        m_obsolete_copies = 0;

      return clone ();
    }
}

std::string
octave_class::get_current_method_class (void)
{
  std::string retval = class_name ();

  if (nparents () > 0)
    {
      octave::tree_evaluator& tw = octave::__get_evaluator__ ();

      octave_function *fcn = tw.current_function ();

      // Here we are just looking to see if FCN is a method or constructor
      // for any class, not specifically this one.
      if (fcn && (fcn->is_class_method () || fcn->is_class_constructor ()))
        retval = fcn->dispatch_class ();
    }

  return retval;
}

OCTAVE_NORETURN static
void
err_invalid_index1 (void)
{
  error ("invalid index for class");
}

OCTAVE_NORETURN static
void
err_invalid_index_for_assignment (void)
{
  error ("invalid index for class assignment");
}

OCTAVE_NORETURN static
void
err_invalid_index_type (const std::string& nm, char t)
{
  error ("%s cannot be indexed with %c", nm.c_str (), t);
}

void
octave_class::break_closure_cycles (const std::shared_ptr<octave::stack_frame>& frame)
{
  for (octave_idx_type j = 0; j < m_map.nfields (); j++)
    {
      Cell& c = m_map.contents (j);

      for (octave_idx_type i = 0; i < c.numel (); i++)
        c(i).break_closure_cycles (frame);
    }
}

Cell
octave_class::dotref (const octave_value_list& idx)
{
  panic_if (idx.length () != 1);

  std::string method_class = get_current_method_class ();

  // Find the class in which this method resides before attempting to access
  // the requested field.

  octave_base_value *obvp = find_parent_class (method_class);

  if (obvp == nullptr)
    error ("malformed class");

  octave_map my_map = (obvp != this) ? obvp->map_value () : m_map;

  std::string nm = idx(0).xstring_value ("invalid index for class");

  octave_map::const_iterator p = my_map.seek (nm);

  if (p == my_map.end ())
    error ("class has no member '%s'", nm.c_str ());

  return my_map.contents (p);
}

Matrix
octave_class::size (void)
{
  if (in_class_method () || called_from_builtin ())
    return octave_base_value::size ();

  Matrix retval (1, 2, 1.0);

  octave::symbol_table& symtab = octave::__get_symbol_table__ ();

  octave_value meth = symtab.find_method ("size", class_name ());

  if (meth.is_defined ())
    {
      count++;
      octave_value_list args (1, octave_value (this));

      octave_value_list lv = octave::feval (meth.function_value (), args, 1);
      if (lv.length () <= 0
          || ! lv(0).is_matrix_type () || ! lv(0).dims ().isvector ())
        error ("@%s/size: invalid return value", class_name ().c_str ());

      retval = lv(0).matrix_value ();
    }
  else
    {
      dim_vector dv = dims ();

      int nd = dv.ndims ();

      retval.resize (1, nd);

      for (int i = 0; i < nd; i++)
        retval(i) = dv(i);
    }

  return retval;
}

octave_idx_type
octave_class::xnumel (const octave_value_list& idx)
{
  if (in_class_method () || called_from_builtin ())
    return octave_base_value::xnumel (idx);

  octave_idx_type retval = -1;
  const std::string cn = class_name ();

  octave::symbol_table& symtab = octave::__get_symbol_table__ ();

  octave_value meth = symtab.find_method ("numel", cn);

  if (meth.is_defined ())
    {
      octave_value_list args (idx.length () + 1, octave_value ());

      count++;
      args(0) = octave_value (this);

      for (octave_idx_type i = 0; i < idx.length (); i++)
        args(i+1) = idx(i);

      octave_value_list lv = octave::feval (meth.function_value (), args, 1);
      if (lv.length () != 1 || ! lv(0).is_scalar_type ())
        error ("@%s/numel: invalid return value", cn.c_str ());

      retval = lv(0).idx_type_value (true);
    }
  else
    retval = octave_base_value::xnumel (idx);

  return retval;
}

octave_value_list
octave_class::subsref (const std::string& type,
                       const std::list<octave_value_list>& idx,
                       int nargout)
{
  octave_value_list retval;

  if (in_class_method () || called_from_builtin ())
    {
      // FIXME: this block of code is the same as the body of
      // octave_struct::subsref.  Maybe it could be shared instead of
      // duplicated.

      int skip = 1;

      switch (type[0])
        {
        case '(':
          {
            if (type.length () > 1 && type[1] == '.')
              {
                auto p = idx.begin ();
                octave_value_list key_idx = *++p;

                Cell tmp = dotref (key_idx);

                Cell t = tmp.index (idx.front ());

                retval(0) = (t.numel () == 1 ? t(0)
                             : octave_value (t, true));

                // We handled two index elements, so tell
                // next_subsref to skip both of them.
                skip++;
              }
            else
              retval(0) = octave_value (m_map.index (idx.front ()),
                                        c_name, m_parent_list);
          }
          break;

        case '.':
          {
            if (m_map.numel () > 0)
              {
                Cell t = dotref (idx.front ());

                retval(0) = (t.numel () == 1) ? t(0) : octave_value (t, true);
              }
          }
          break;

        case '{':
          err_invalid_index_type (type_name (), type[0]);
          break;

        default:
          panic_impossible ();
        }

      // FIXME: perhaps there should be an
      // octave_value_list::next_subsref member function?  See also
      // octave_user_function::subsref.

      if (idx.size () > 1)
        retval = retval(0).next_subsref (nargout, type, idx, skip);
    }
  else
    {
      octave::symbol_table& symtab = octave::__get_symbol_table__ ();

      octave_value meth = symtab.find_method ("subsref", class_name ());

      if (meth.is_defined ())
        {
          octave_value_list args;

          args(1) = make_idx_args (type, idx, "subsref");

          count++;
          args(0) = octave_value (this);

          // FIXME: for Matlab compatibility, let us attempt to set up a proper
          // value for nargout at least in the simple case where the
          // cs-list-type expression - i.e., {} or ().x, is the leading one.
          // Note that Octave does not actually need this, since it will
          // be able to properly react to varargout a posteriori.
          bool maybe_cs_list_query = (type[0] == '.' || type[0] == '{'
                                      || (type.length () > 1 && type[0] == '('
                                          && type[1] == '.'));

          int true_nargout = nargout;

          if (maybe_cs_list_query)
            {
              // Set up a proper nargout for the subsref call by calling numel.
              octave_value_list tmp;
              if (type[0] != '.') tmp = idx.front ();
              true_nargout = xnumel (tmp);
            }

          retval = octave::feval (meth.function_value (), args, true_nargout);

          // Since we're handling subsref, if the list has more than one
          // element, return it as a comma-separated list so that we can
          // pass it to the evaluator
          if (retval.length () > 1)
            retval = octave_value (retval);
        }
      else
        {
          if (type.length () == 1 && type[0] == '(')
            retval(0) = octave_value (m_map.index (idx.front ()), c_name,
                                      m_parent_list);
          else
            err_invalid_index1 ();
        }
    }

  return retval;
}

octave_value
octave_class::numeric_conv (const Cell& val, const std::string& type)
{
  octave_value retval;

  if (val.numel () != 1)
    err_invalid_index_for_assignment ();

  retval = val(0);

  if (type.length () > 0 && type[0] == '.' && ! retval.isstruct ())
    retval = octave_map ();

  return retval;
}

octave_value
octave_class::subsasgn (const std::string& type,
                        const std::list<octave_value_list>& idx,
                        const octave_value& rhs)
{
  count++;
  return subsasgn_common (octave_value (this), type, idx, rhs);
}

octave_value
octave_class::undef_subsasgn (const std::string& type,
                              const std::list<octave_value_list>& idx,
                              const octave_value& rhs)
{
  // For compatibility with Matlab, pass [] as the first argument to the
  // the subsasgn function when the LHS of an indexed assignment is
  // undefined.

  return subsasgn_common (Matrix (), type, idx, rhs);
}

octave_value
octave_class::subsasgn_common (const octave_value& obj,
                               const std::string& type,
                               const std::list<octave_value_list>& idx,
                               const octave_value& rhs)
{
  octave_value retval;

  if (! (in_class_method () || called_from_builtin ()))
    {
      octave::symbol_table& symtab = octave::__get_symbol_table__ ();

      octave_value meth = symtab.find_method ("subsasgn", class_name ());

      if (meth.is_defined ())
        {
          octave_value_list args;

          if (rhs.is_cs_list ())
            {
              octave_value_list lrhs = rhs.list_value ();
              args.resize (2 + lrhs.length ());
              for (octave_idx_type i = 0; i < lrhs.length (); i++)
                args(2+i) = lrhs(i);
            }
          else
            args(2) = rhs;

          args(1) = make_idx_args (type, idx, "subsasgn");
          args(0) = obj;

          // Now comes the magic.  Count copies with me:
          // 1. myself (obsolete)
          // 2. the copy inside args (obsolete)
          // 3. the copy in method's symbol table (working)
          // ... possibly more (not obsolete).
          //
          // So we mark 2 copies as obsolete and hold our fingers crossed.
          // But prior to doing that, check whether the routine is amenable
          // to the optimization.
          // It is essential that the handling function doesn't store extra
          // copies anywhere.  If it does, things will not break but the
          // optimization won't work.

          octave_value_list tmp;

          if (m_obsolete_copies == 0 && meth.is_user_function ()
              && meth.user_function_value ()->subsasgn_optimization_ok ())
            {
              octave::unwind_protect_var<int> restore_var (m_obsolete_copies);
              m_obsolete_copies = 2;

              tmp = octave::feval (meth.function_value (), args);
            }
          else
            tmp = octave::feval (meth.function_value (), args);

          // FIXME: Should the subsasgn method be able to return
          //        more than one value?

          if (tmp.length () > 1)
            error ("@%s/subsasgn returned more than one value",
                   class_name ().c_str ());

          else
            retval = tmp(0);

          return retval;
        }
    }

  // Find the class in which this method resides before
  // attempting to do the indexed assignment.

  std::string method_class = get_current_method_class ();

  octave_base_value *obvp = unique_parent_class (method_class);
  if (obvp != this)
    {

      if (! obvp)
        error ("malformed class");

      obvp->subsasgn (type, idx, rhs);

      count++;
      retval = octave_value (this);

      return retval;
    }

  // FIXME: this block of code is the same as the body of
  // octave_struct::subsasgn.  Maybe it could be shared instead of
  // duplicated.

  int n = type.length ();

  octave_value t_rhs = rhs;

  if (n > 1 && ! (type.length () == 2 && type[0] == '(' && type[1] == '.'))
    {
      switch (type[0])
        {
        case '(':
          {
            if (type.length () > 1 && type[1] == '.')
              {
                auto p = idx.begin ();
                octave_value_list t_idx = *p;

                octave_value_list key_idx = *++p;

                panic_if (key_idx.length () != 1);

                std::string key = key_idx(0).xstring_value ("invalid index for class assignment");

                octave_value u;

                if (! m_map.contains (key))
                  u = octave_value::empty_conv (type.substr (2), rhs);
                else
                  {
                    Cell map_val = m_map.contents (key);

                    Cell map_elt = map_val.index (idx.front (), true);

                    u = numeric_conv (map_elt, type.substr (2));
                  }

                std::list<octave_value_list> next_idx (idx);

                // We handled two index elements, so subsasgn to
                // needs to skip both of them.

                next_idx.erase (next_idx.begin ());
                next_idx.erase (next_idx.begin ());

                u.make_unique ();

                t_rhs = u.subsasgn (type.substr (2), next_idx, rhs);
              }
            else
              err_invalid_index_for_assignment ();
          }
          break;

        case '.':
          {
            octave_value_list key_idx = idx.front ();

            panic_if (key_idx.length () != 1);

            std::string key = key_idx(0).string_value ();

            std::list<octave_value_list> next_idx (idx);

            next_idx.erase (next_idx.begin ());

            std::string next_type = type.substr (1);

            Cell tmpc (1, 1);
            auto pkey = m_map.seek (key);
            if (pkey != m_map.end ())
              {
                m_map.contents (pkey).make_unique ();
                tmpc = m_map.contents (pkey);
              }

            // FIXME: better code reuse?
            if (tmpc.numel () != 1)
              err_indexed_cs_list ();

            octave_value& tmp = tmpc(0);

            if (! tmp.is_defined () || tmp.is_zero_by_zero ())
              {
                tmp = octave_value::empty_conv (next_type, rhs);
                tmp.make_unique (); // probably a no-op.
              }
            else
              // optimization: ignore copy still stored inside our map.
              tmp.make_unique (1);

            t_rhs = tmp.subsasgn (next_type, next_idx, rhs);
          }
          break;

        case '{':
          err_invalid_index_type (type_name (), type[0]);
          break;

        default:
          panic_impossible ();
        }
    }

  switch (type[0])
    {
    case '(':
      {
        if (n > 1 && type[1] == '.')
          {
            auto p = idx.begin ();
            octave_value_list key_idx = *++p;

            panic_if (key_idx.length () != 1);

            std::string key = key_idx(0).xstring_value ("assignment to class element failed");

            m_map.assign (idx.front (), key, t_rhs);

            count++;
            retval = octave_value (this);
          }
        else
          {
            if (t_rhs.isobject () || t_rhs.isstruct ())
              {
                octave_map rhs_map = t_rhs.xmap_value ("invalid class assignment");

                m_map.assign (idx.front (), rhs_map);

                count++;
                retval = octave_value (this);
              }
            else
              {
                if (! t_rhs.isempty ())
                  error ("invalid class assignment");

                m_map.delete_elements (idx.front ());

                count++;
                retval = octave_value (this);
              }
          }
      }
      break;

    case '.':
      {
        octave_value_list key_idx = idx.front ();

        panic_if (key_idx.length () != 1);

        std::string key = key_idx(0).string_value ();

        if (t_rhs.is_cs_list ())
          {
            Cell tmp_cell = Cell (t_rhs.list_value ());

            // The shape of the RHS is irrelevant, we just want
            // the number of elements to agree and to preserve the
            // shape of the left hand side of the assignment.

            if (numel () == tmp_cell.numel ())
              tmp_cell = tmp_cell.reshape (dims ());

            m_map.setfield (key, tmp_cell);
          }
        else
          {
            Cell tmp_cell(1, 1);
            tmp_cell(0) = t_rhs.storable_value ();
            m_map.setfield (key, tmp_cell);
          }

        count++;
        retval = octave_value (this);
      }
      break;

    case '{':
      err_invalid_index_type (type_name (), type[0]);
      break;

    default:
      panic_impossible ();
    }

  return retval;
}

octave::idx_vector
octave_class::index_vector (bool require_integers) const
{
  octave::symbol_table& symtab = octave::__get_symbol_table__ ();

  octave_value meth = symtab.find_method ("subsindex", class_name ());

  if (! meth.is_defined ())
    error ("no subsindex method defined for class %s",
           class_name ().c_str ());

  octave_value_list args;
  args(0) = octave_value (new octave_class (m_map, c_name, m_parent_list));

  octave_value_list tmp = octave::feval (meth.function_value (), args, 1);

  if (tmp(0).isobject ())
    error ("subsindex function must return a valid index vector");

  // Index vector returned by subsindex is zero based
  // (why this inconsistency Mathworks?), and so we must
  // add one to the value returned as the index_vector method
  // expects it to be one based.
  return octave::binary_op (octave_value::op_add, tmp (0),
                            octave_value (1.0)).index_vector (require_integers);
}

std::size_t
octave_class::byte_size (void) const
{
  // Neglect the size of the fieldnames.

  std::size_t retval = 0;

  for (auto it = m_map.cbegin (); it != m_map.cend (); it++)
    {
      std::string key = m_map.key (it);

      octave_value val = octave_value (m_map.contents (it));

      retval += val.byte_size ();
    }

  return retval;
}

bool
octave_class::is_true (void) const
{
  bool retval = false;

  octave::symbol_table& symtab = octave::__get_symbol_table__ ();

  octave_value meth = symtab.find_method ("logical", class_name ());

  if (meth.is_defined ())
    {
      octave_value in = new octave_class (*this);

      octave_value_list tmp = octave::feval (meth.function_value (), in, 1);
      retval = tmp(0).is_true ();
    }

  return retval;
}

string_vector
octave_class::map_keys (void) const
{
  err_wrong_type_arg ("octave_class::map_keys()", type_name ());
}

octave_base_value *
octave_class::find_parent_class (const std::string& parent_class_name)
{
  octave_base_value *retval = nullptr;

  if (parent_class_name == class_name ())
    retval = this;
  else
    {
      for (auto& par : m_parent_list)
        {
          octave_map::const_iterator smap = m_map.seek (par);

          const Cell& tmp = m_map.contents (smap);

          octave_value vtmp = tmp(0);

          octave_base_value *obvp = vtmp.internal_rep ();

          retval = obvp->find_parent_class (parent_class_name);

          if (retval)
            break;
        }
    }

  return retval;
}

octave_base_value *
octave_class::unique_parent_class (const std::string& parent_class_name)
{
  octave_base_value *retval = nullptr;

  if (parent_class_name == class_name ())
    retval = this;
  else
    {
      for (auto& par : m_parent_list)
        {
          auto smap = m_map.seek (par);

          Cell& tmp = m_map.contents (smap);

          octave_value& vtmp = tmp(0);

          octave_base_value *obvp = vtmp.internal_rep ();

          // Use find_parent_class first to avoid uniquifying if not necessary.
          retval = obvp->find_parent_class (parent_class_name);

          if (retval)
            {
              vtmp.make_unique ();
              obvp = vtmp.internal_rep ();
              retval = obvp->unique_parent_class (parent_class_name);

              break;
            }
        }
    }

  return retval;
}

bool
octave_class::is_instance_of (const std::string& cls_name) const
{
  bool retval = false;

  if (cls_name == class_name ())
    retval = true;
  else
    {
      for (auto& par : m_parent_list)
        {
          octave_map::const_iterator smap = m_map.seek (par);

          const Cell& tmp = m_map.contents (smap);

          const octave_value& vtmp = tmp(0);

          retval = vtmp.is_instance_of (cls_name);

          if (retval)
            break;
        }
    }

  return retval;
}

string_vector
octave_class::string_vector_value (bool pad) const
{
  string_vector retval;

  octave::symbol_table& symtab = octave::__get_symbol_table__ ();

  octave_value meth = symtab.find_method ("char", class_name ());

  if (! meth.is_defined ())
    error ("no char method defined for class %s", class_name ().c_str ());

  octave_value_list args;
  args(0) = octave_value (new octave_class (m_map, c_name, m_parent_list));

  octave_value_list tmp = octave::feval (meth.function_value (), args, 1);

  if (tmp.length () >= 1)
    {
      if (! tmp(0).is_string ())
        error ("cname/char method did not return a string");

      retval = tmp(0).string_vector_value (pad);
    }

  return retval;
}

void
octave_class::print (std::ostream& os, bool)
{
  print_raw (os);
}

void
octave_class::print_raw (std::ostream& os, bool) const
{
  indent (os);
  os << "  <class " << class_name () << '>';
  newline (os);
}

// Loading a class properly requires an exemplar map entry for success.
// If we don't have one, we attempt to create one by calling the constructor
// with no arguments.
bool
octave_class::reconstruct_exemplar (void)
{
  bool retval = false;

  octave_class::exemplar_const_iterator it
    = octave_class::exemplar_map.find (c_name);

  if (it != octave_class::exemplar_map.end ())
    retval = true;
  else
    {
      octave::interpreter& interp = octave::__get_interpreter__ ();

      octave::symbol_table& symtab = interp.get_symbol_table ();

      octave_value ctor = symtab.find_method (c_name, c_name);

      bool have_ctor = false;

      if (ctor.is_defined () && ctor.is_function ())
        {
          octave_function *fcn = ctor.function_value ();

          if (fcn && fcn->is_class_constructor (c_name))
            have_ctor = true;

          // Something has gone terribly wrong if
          // symbol_table::find_method (c_name, c_name) does not return
          // a class constructor for the class c_name...
          panic_unless (have_ctor);
        }

      if (have_ctor)
        {
          octave::unwind_protect frame;

          // Simulate try/catch.

          octave::interpreter_try (frame);

          bool execution_error = false;

          octave_value_list result;

          try
            {
              result = octave::feval (ctor, ovl (), 1);
            }
          catch (const octave::execution_exception&)
            {
              interp.recover_from_exception ();

              execution_error = true;
            }

          if (! execution_error && result.length () == 1)
            retval = true;
        }
      else
        warning ("no constructor for class %s", c_name.c_str ());
    }

  return retval;
}

void
octave_class::clear_exemplar_map (void)
{
  exemplar_map.clear ();
}

//  Load/save does not provide enough information to reconstruct the
//  class inheritance structure.  reconstruct_parents () attempts to
//  do so.  If successful, a "true" value is returned.
//
//  Note that we don't check the loaded object structure against the
//  class structure here so the user's loadobj method has a chance
//  to do its magic.
bool
octave_class::reconstruct_parents (void)
{
  bool retval = true;
  bool might_have_inheritance = false;
  std::string dbgstr = "dork";

  // First, check to see if there might be an issue with inheritance.
  for (auto it = m_map.cbegin (); it != m_map.cend (); it++)
    {
      std::string key = m_map.key (it);
      Cell        val = m_map.contents (it);
      if (val(0).isobject ())
        {
          dbgstr = "blork";
          if (key == val(0).class_name ())
            {
              might_have_inheritance = true;
              dbgstr = "cork";
              break;
            }
        }
    }

  if (might_have_inheritance)
    {
      octave_class::exemplar_const_iterator it
        = octave_class::exemplar_map.find (c_name);

      if (it == octave_class::exemplar_map.end ())
        retval = false;
      else
        {
          octave_class::exemplar_info exmplr = it->second;
          m_parent_list = exmplr.parents ();
          for (auto& par : m_parent_list)
            {
              dbgstr = par;
              bool dbgbool = m_map.contains (par);
              if (! dbgbool)
                {
                  retval = false;
                  break;
                }
            }
        }
    }

  return retval;
}

bool
octave_class::save_ascii (std::ostream& os)
{
  os << "# classname: " << class_name () << "\n";
  octave_map m;

  octave::load_path& lp = octave::__get_load_path__ ();

  if (lp.find_method (class_name (), "saveobj") != "")
    {
      octave_value in = new octave_class (*this);
      octave_value_list tmp = octave::feval ("saveobj", in, 1);

      m = tmp(0).map_value ();
    }
  else
    m = map_value ();

  os << "# length: " << m.nfields () << "\n";

  auto i = m.begin ();
  while (i != m.end ())
    {
      octave_value val = m_map.contents (i);

      bool b = save_text_data (os, val, m.key (i), false, 0);

      if (! b)
        return ! os.fail ();

      i++;
    }

  return true;
}

bool
octave_class::load_ascii (std::istream& is)
{
  octave_idx_type len = 0;
  std::string classname;

  if (! extract_keyword (is, "classname", classname) || classname.empty ())
    error ("load: failed to extract name of class");

  if (! extract_keyword (is, "length", len) || len < 0)
    error ("load: failed to extract number of elements in class");

  if (len > 0)
    {
      octave_map m (m_map);

      for (octave_idx_type j = 0; j < len; j++)
        {
          octave_value t2;
          bool dummy;

          // recurse to read cell elements
          std::string nm
            = read_text_data (is, "", dummy, t2, j);

          if (! is)
            break;

          Cell tcell = (t2.iscell () ? t2.xcell_value ("load: internal error loading class elements") : Cell (
                          t2));

          m.assign (nm, tcell);
        }

      if (! is)
        error ("load: failed to load class");

      c_name = classname;
      reconstruct_exemplar ();

      m_map = m;

      if (! reconstruct_parents ())
        warning ("load: unable to reconstruct object inheritance");

      octave::load_path& lp = octave::__get_load_path__ ();

      if (lp.find_method (classname, "loadobj") != "")
        {
          octave_value in = new octave_class (*this);
          octave_value_list tmp = octave::feval ("loadobj", in, 1);

          m_map = tmp(0).map_value ();
        }
    }
  else if (len == 0)
    {
      m_map = octave_map (dim_vector (1, 1));
      c_name = classname;
    }
  else
    panic_impossible ();

  return true;
}

bool
octave_class::save_binary (std::ostream& os, bool save_as_floats)
{
  int32_t classname_len = class_name ().length ();

  os.write (reinterpret_cast<char *> (&classname_len), 4);
  os << class_name ();

  octave_map m;

  octave::load_path& lp = octave::__get_load_path__ ();

  if (lp.find_method (class_name (), "saveobj") != "")
    {
      octave_value in = new octave_class (*this);
      octave_value_list tmp = octave::feval ("saveobj", in, 1);

      m = tmp(0).map_value ();
    }
  else
    m = map_value ();

  int32_t len = m.nfields ();
  os.write (reinterpret_cast<char *> (&len), 4);

  auto i = m.begin ();
  while (i != m.end ())
    {
      octave_value val = m_map.contents (i);

      bool b = save_binary_data (os, val, m.key (i), "", 0, save_as_floats);

      if (! b)
        return ! os.fail ();

      i++;
    }

  return true;
}

bool
octave_class::load_binary (std::istream& is, bool swap,
                           octave::mach_info::float_format fmt)
{
  bool success = true;

  int32_t classname_len;

  is.read (reinterpret_cast<char *> (&classname_len), 4);
  if (! is)
    return false;
  else if (swap)
    swap_bytes<4> (&classname_len);

  {
    OCTAVE_LOCAL_BUFFER (char, classname, classname_len+1);
    classname[classname_len] = '\0';
    if (! is.read (reinterpret_cast<char *> (classname), classname_len))
      return false;
    c_name = classname;
  }
  reconstruct_exemplar ();

  int32_t len;
  if (! is.read (reinterpret_cast<char *> (&len), 4))
    return false;
  if (swap)
    swap_bytes<4> (&len);

  if (len > 0)
    {
      octave_map m (m_map);

      for (octave_idx_type j = 0; j < len; j++)
        {
          octave_value t2;
          bool dummy;
          std::string doc;

          // recurse to read cell elements
          std::string nm = read_binary_data (is, swap, fmt, "",
                                             dummy, t2, doc);

          if (! is)
            break;

          Cell tcell = (t2.iscell () ? t2.xcell_value ("load: internal error loading class elements") : Cell (
                          t2));

          m.assign (nm, tcell);
        }

      if (is)
        {
          m_map = m;

          if (! reconstruct_parents ())
            warning ("load: unable to reconstruct object inheritance");

          octave::load_path& lp = octave::__get_load_path__ ();

          if (lp.find_method (c_name, "loadobj") != "")
            {
              octave_value in = new octave_class (*this);
              octave_value_list tmp = octave::feval ("loadobj", in, 1);

              m_map = tmp(0).map_value ();
            }
        }
      else
        {
          warning ("load: failed to load class");
          success = false;
        }
    }
  else if (len == 0)
    m_map = octave_map (dim_vector (1, 1));
  else
    panic_impossible ();

  return success;
}

bool
octave_class::save_hdf5 (octave_hdf5_id loc_id, const char *name,
                         bool save_as_floats)
{
#if defined (HAVE_HDF5)

  hsize_t hdims[3];
  hid_t group_hid = -1;
  hid_t type_hid = -1;
  hid_t space_hid = -1;
  hid_t class_hid = -1;
  hid_t data_hid = -1;
  octave_map m;
  octave_map::iterator i;

  octave::load_path& lp = octave::__get_load_path__ ();

#if defined (HAVE_HDF5_18)
  group_hid = H5Gcreate (loc_id, name, octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                         octave_H5P_DEFAULT);
#else
  group_hid = H5Gcreate (loc_id, name, 0);
#endif
  if (group_hid < 0)
    goto error_cleanup;

  // Add the class name to the group
  type_hid = H5Tcopy (H5T_C_S1); H5Tset_size (type_hid, c_name.length () + 1);
  if (type_hid < 0)
    goto error_cleanup;

  hdims[0] = 0;
  space_hid = H5Screate_simple (0, hdims, nullptr);
  if (space_hid < 0)
    goto error_cleanup;
#if defined (HAVE_HDF5_18)
  class_hid = H5Dcreate (group_hid, "classname",  type_hid, space_hid,
                         octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                         octave_H5P_DEFAULT);
#else
  class_hid = H5Dcreate (group_hid, "classname",  type_hid, space_hid,
                         octave_H5P_DEFAULT);
#endif
  if (class_hid < 0 || H5Dwrite (class_hid, type_hid, octave_H5S_ALL,
                                 octave_H5S_ALL, octave_H5P_DEFAULT,
                                 c_name.c_str ()) < 0)
    goto error_cleanup;

#if defined (HAVE_HDF5_18)
  data_hid = H5Gcreate (group_hid, "value", octave_H5P_DEFAULT,
                        octave_H5P_DEFAULT, octave_H5P_DEFAULT);
#else
  data_hid = H5Gcreate (group_hid, "value", 0);
#endif
  if (data_hid < 0)
    goto error_cleanup;

  if (lp.find_method (class_name (), "saveobj") != "")
    {
      octave_value in = new octave_class (*this);
      octave_value_list tmp = octave::feval ("saveobj", in, 1);

      m = tmp(0).map_value ();
    }
  else
    m = map_value ();

  // recursively add each element of the class to this group
  i = m.begin ();
  while (i != m.end ())
    {
      octave_value val = m_map.contents (i);

      bool retval2 = add_hdf5_data (data_hid, val, m.key (i), "", false,
                                    save_as_floats);

      if (! retval2)
        break;

      i++;
    }

error_cleanup:

  if (data_hid > 0)
    H5Gclose (data_hid);

  if (class_hid > 0)
    H5Dclose (class_hid);

  if (space_hid > 0)
    H5Sclose (space_hid);

  if (type_hid > 0)
    H5Tclose (type_hid);

  if (group_hid > 0)
    H5Gclose (group_hid);

  return true;

#else
  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);
  octave_unused_parameter (save_as_floats);

  warn_save ("hdf5");

  return false;
#endif
}

bool
octave_class::load_hdf5 (octave_hdf5_id loc_id, const char *name)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  hid_t group_hid = -1;
  hid_t data_hid = -1;
  hid_t type_hid = -1;
  hid_t type_class_hid = -1;
  hid_t space_hid = -1;
  hid_t subgroup_hid = -1;
  hid_t st_id = -1;

  hdf5_callback_data dsub;

  herr_t retval2 = 0;
  octave_map m (dim_vector (1, 1));
  int current_item = 0;
  hsize_t num_obj = 0;
  int slen = 0;
  hsize_t rank = 0;

#if defined (HAVE_HDF5_18)
  group_hid = H5Gopen (loc_id, name, octave_H5P_DEFAULT);
#else
  group_hid = H5Gopen (loc_id, name);
#endif
  if (group_hid < 0)
    goto error_cleanup;

#if defined (HAVE_HDF5_18)
  data_hid = H5Dopen (group_hid, "classname", octave_H5P_DEFAULT);
#else
  data_hid = H5Dopen (group_hid, "classname");
#endif

  if (data_hid < 0)
    goto error_cleanup;

  type_hid = H5Dget_type (data_hid);

  type_class_hid = H5Tget_class (type_hid);

  if (type_class_hid != H5T_STRING)
    goto error_cleanup;

  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    goto error_cleanup;

  slen = H5Tget_size (type_hid);
  if (slen < 0)
    goto error_cleanup;

  // do-while loop here to prevent goto crossing initialization of classname
  do
    {
      OCTAVE_LOCAL_BUFFER (char, classname, slen);

      // create datatype for (null-terminated) string to read into:
      st_id = H5Tcopy (H5T_C_S1);
      H5Tset_size (st_id, slen);

      if (H5Dread (data_hid, st_id, octave_H5S_ALL, octave_H5S_ALL,
                   octave_H5P_DEFAULT, classname)
          < 0)
        {
          H5Tclose (st_id);
          H5Dclose (data_hid);
          H5Gclose (group_hid);
          return false;
        }

      H5Tclose (st_id);
      H5Dclose (data_hid);
      data_hid = -1;

      c_name = classname;
    }
  while (0);
  reconstruct_exemplar ();

#if defined (HAVE_HDF5_18)
  subgroup_hid = H5Gopen (group_hid, name, octave_H5P_DEFAULT);
#else
  subgroup_hid = H5Gopen (group_hid, name);
#endif
  H5Gget_num_objs (subgroup_hid, &num_obj);
  H5Gclose (subgroup_hid);

  while (current_item < static_cast<int> (num_obj)
         && (retval2 = hdf5_h5g_iterate (group_hid, name, &current_item,
                                         &dsub)) > 0)
    {
      octave_value t2 = dsub.tc;

      Cell tcell = (t2.iscell () ? t2.xcell_value ("load: internal error loading class elements") : Cell (
                      t2));

      m.assign (dsub.name, tcell);

    }

  if (retval2 >= 0)
    {
      m_map = m;

      if (! reconstruct_parents ())
        warning ("load: unable to reconstruct object inheritance");

      octave::load_path& lp = octave::__get_load_path__ ();

      if (lp.find_method (c_name, "loadobj") != "")
        {
          octave_value in = new octave_class (*this);
          octave_value_list tmp = octave::feval ("loadobj", in, 1);

          m_map = tmp(0).map_value ();
          retval = true;
        }
    }

error_cleanup:
  if (data_hid > 0)
    H5Dclose (data_hid);

  if (data_hid > 0)
    H5Gclose (group_hid);

#else
  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  warn_load ("hdf5");
#endif

  return retval;
}

mxArray *
octave_class::as_mxArray (bool) const
{
  err_wrong_type_arg ("octave_class::as_mxArray ()", type_name ());
}

bool
octave_class::in_class_method (void)
{
  octave::tree_evaluator& tw = octave::__get_evaluator__ ();

  octave_function *fcn = tw.current_function ();

  return (fcn
          && (fcn->is_class_method ()
              || fcn->is_class_constructor ()
              || fcn->is_anonymous_function_of_class ()
              || fcn->is_private_function_of_class (class_name ()))
          && find_parent_class (fcn->dispatch_class ()));
}

octave_class::exemplar_info::exemplar_info (const octave_value& obj)
  : m_field_names (), m_parent_class_names ()
{
  if (! obj.isobject ())
    error ("invalid call to exemplar_info constructor");

  octave_map m = obj.map_value ();
  m_field_names = m.keys ();

  m_parent_class_names = obj.parent_class_name_list ();
}

// A map from class names to lists of fields.
std::map<std::string, octave_class::exemplar_info> octave_class::exemplar_map;

bool
octave_class::exemplar_info::compare (const octave_value& obj) const
{

  if (! obj.isobject ())
    error ("invalid comparison of class exemplar to non-class object");

  if (nfields () != obj.nfields ())
    error ("mismatch in number of fields");

  octave_map obj_map = obj.map_value ();
  string_vector obj_fnames = obj_map.keys ();
  string_vector fnames = fields ();

  for (octave_idx_type i = 0; i < nfields (); i++)
    {
      if (obj_fnames[i] != fnames[i])
        error ("mismatch in field names");
    }

  if (nparents () != obj.nparents ())
    error ("mismatch in number of parent classes");

  const std::list<std::string> obj_parents
    = obj.parent_class_name_list ();
  const std::list<std::string> pnames = parents ();

  auto p = obj_parents.begin ();
  auto q = pnames.begin ();

  while (p != obj_parents.end ())
    {
      if (*p++ != *q++)
        error ("mismatch in parent classes");
    }

  return true;
}

OCTAVE_BEGIN_NAMESPACE(octave)

DEFMETHOD (class, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{classname} =} class (@var{obj})
@deftypefnx {} {@var{cls} =} class (@var{s}, @var{classname})
@deftypefnx {} {@var{cls} =} class (@var{s}, @var{classname}, @var{parent1}, @dots{})
Return the class of the object @var{obj}, or create a class with fields from
structure @var{s} and name (string) @var{classname}.

Additional arguments name a list of parent classes from which the new class is
derived.
@seealso{typeinfo, isa}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin == 0)
    print_usage ();

  octave_value retval;

  if (nargin == 1)
    // Called for class of object
    retval = args(0).class_name ();
  else
    {
      // Called as class constructor
      std::string id = args(1).xstring_value ("class: ID (class name) must be a string");

      tree_evaluator& tw = interp.get_evaluator ();

      octave_function *fcn = tw.caller_function ();

      if (! fcn)
        error ("class: invalid call from outside class constructor or method");

      if (! fcn->is_class_constructor (id) && ! fcn->is_class_method (id))
        error ("class: '%s' is invalid as a class name in this context",
               id.c_str ());

      octave_map m = args(0).xmap_value ("class: S must be a valid structure");

      if (nargin == 2)
        retval
          = octave_value (new octave_class (m, id, std::list<std::string> ()));
      else
        {
          octave_value_list parents = args.slice (2, nargin-2);

          retval = octave_value (new octave_class (m, id, parents));
        }

      octave_class::exemplar_const_iterator it
        = octave_class::exemplar_map.find (id);

      if (it == octave_class::exemplar_map.end ())
        octave_class::exemplar_map[id] = octave_class::exemplar_info (retval);
      else if (! it->second.compare (retval))
        error ("class: object of class '%s' does not match previously constructed objects",
               id.c_str ());
    }

  return retval;
}

/*
%!assert (class (1.1), "double")
%!assert (class (single (1.1)), "single")
%!assert (class (uint8 (1)), "uint8")
%!testif HAVE_JAVA; usejava ("jvm")
%! jobj = javaObject ("java.lang.StringBuffer");
%! assert (class (jobj), "java.lang.StringBuffer");

## Test Input Validation
%!error class ()
*/

DEFUN (isa, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isa (@var{obj}, @var{classname})
Return true if @var{obj} is an object from the class @var{classname}.

@var{classname} may also be one of the following class categories:

@table @asis
@item @qcode{"float"}
Floating point value comprising classes @qcode{"double"} and
@qcode{"single"}.

@item @qcode{"integer"}
Integer value comprising classes (u)int8, (u)int16, (u)int32, (u)int64.

@item @qcode{"numeric"}
Numeric value comprising either a floating point or integer value.
@end table

If @var{classname} is a cell array of string, a logical array of the same
size is returned, containing true for each class to which @var{obj}
belongs to.

@seealso{class, typeinfo}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  octave_value obj = args(0);  // not const because of find_parent_class ()
  std::string obj_cls = obj.class_name ();
  Array<std::string> clsnames = args(
                                  1).xcellstr_value ("isa: CLASSNAME must be a string or cell array of strings");

  boolNDArray matches (clsnames.dims (), false);

  for (octave_idx_type idx = 0; idx < clsnames.numel (); idx++)
    {
      std::string cls = clsnames(idx);
      if (obj_cls == cls
          || (cls == "float"   && obj.isfloat   ())
          || (cls == "integer" && obj.isinteger ())
          || (cls == "numeric" && obj.isnumeric ())
          || obj.is_instance_of (cls))
        matches(idx) = true;
    }

  return ovl (matches);
}

/*
%!assert (isa ("char", "float"), false)
%!assert (isa (logical (1), "float"), false)
%!assert (isa (double (13), "float"), true)
%!assert (isa (single (13), "float"), true)
%!assert (isa (int8 (13), "float"), false)
%!assert (isa (int16 (13), "float"), false)
%!assert (isa (int32 (13), "float"), false)
%!assert (isa (int64 (13), "float"), false)
%!assert (isa (uint8 (13), "float"), false)
%!assert (isa (uint16 (13), "float"), false)
%!assert (isa (uint32 (13), "float"), false)
%!assert (isa (uint64 (13), "float"), false)
%!assert (isa ("char", "numeric"), false)
%!assert (isa (logical (1), "numeric"), false)
%!assert (isa (double (13), "numeric"), true)
%!assert (isa (single (13), "numeric"), true)
%!assert (isa (int8 (13), "numeric"), true)
%!assert (isa (int16 (13), "numeric"), true)
%!assert (isa (int32 (13), "numeric"), true)
%!assert (isa (int64 (13), "numeric"), true)
%!assert (isa (uint8 (13), "numeric"), true)
%!assert (isa (uint16 (13), "numeric"), true)
%!assert (isa (uint32 (13), "numeric"), true)
%!assert (isa (uint64 (13), "numeric"), true)
%!assert (isa (uint8 (13), "integer"), true)
%!assert (isa (double (13), "integer"), false)
%!assert (isa (single (13), "integer"), false)
%!assert (isa (single (13), {"integer", "float", "single"}), [false true true])

%!assert (isa (double (13), "double"))
%!assert (isa (single (13), "single"))
%!assert (isa (int8 (13), "int8"))
%!assert (isa (int16 (13), "int16"))
%!assert (isa (int32 (13), "int32"))
%!assert (isa (int64 (13), "int64"))
%!assert (isa (uint8 (13), "uint8"))
%!assert (isa (uint16 (13), "uint16"))
%!assert (isa (uint32 (13), "uint32"))
%!assert (isa (uint64 (13), "uint64"))
%!assert (isa ("string", "char"))
%!assert (isa (true, "logical"))
%!assert (isa (false, "logical"))
%!assert (isa ({1, 2}, "cell"))
%!assert (isa ({1, 2}, {"numeric", "integer", "cell"}), [false false true])

%!testif HAVE_JAVA; usejava ("jvm")
%! ## The first and last assert() are equal on purpose.  The assert() in
%! ## the middle with an invalid class name will cause the java code to
%! ## throw exceptions which we then must clear properly (or all other calls
%! ## will fail).  So we test this too.
%! assert (isa (javaObject ("java.lang.Double", 10), "java.lang.Number"));
%! assert (isa (javaObject ("java.lang.Double", 10), "not_a_class"), false);
%! assert (isa (javaObject ("java.lang.Double", 10), "java.lang.Number"));

%!test
%! a.b = 1;
%! assert (isa (a, "struct"));
*/

DEFUN (__parent_classes__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{Ccls} =} __parent_classes__ (@var{x})
Undocumented internal function.
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_value arg = args(0);

  if (arg.isobject ())
    return ovl (Cell (arg.parent_class_names ()));
  else
    return ovl (Cell ());
}

DEFUN (isobject, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isobject (@var{x})
Return true if @var{x} is a class object.
@seealso{class, typeinfo, isa, ismethod, isprop}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).isobject ());
}

static bool
is_built_in_class (const std::string& cn)
{
  static std::set<std::string> built_in_class_names;

  if (built_in_class_names.empty ())
    {
      built_in_class_names.insert ("double");
      built_in_class_names.insert ("single");
      built_in_class_names.insert ("cell");
      built_in_class_names.insert ("struct");
      built_in_class_names.insert ("logical");
      built_in_class_names.insert ("char");
      built_in_class_names.insert ("function handle");
      built_in_class_names.insert ("int8");
      built_in_class_names.insert ("uint8");
      built_in_class_names.insert ("int16");
      built_in_class_names.insert ("uint16");
      built_in_class_names.insert ("int32");
      built_in_class_names.insert ("uint32");
      built_in_class_names.insert ("int64");
      built_in_class_names.insert ("uint64");
    }

  return built_in_class_names.find (cn) != built_in_class_names.end ();
}

DEFMETHOD (superiorto, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} superiorto (@var{class_name}, @dots{})
When called from a class constructor, mark the object currently constructed
as having a higher precedence than @var{class_name}.

More that one such class can be specified in a single call.  This function
may @emph{only} be called from a class constructor.
@seealso{inferiorto}
@end deftypefn */)
{
  tree_evaluator& tw = interp.get_evaluator ();

  octave_function *fcn = tw.caller_function ();

  if (! fcn || ! fcn->is_class_constructor ())
    error ("superiorto: invalid call from outside class constructor");

  for (int i = 0; i < args.length (); i++)
    {
      std::string inf_class = args(i).xstring_value ("superiorto: CLASS_NAME must be a string");

      // User defined classes always have higher precedence
      // than built-in classes
      if (is_built_in_class (inf_class))
        break;

      symbol_table& symtab = interp.get_symbol_table ();

      std::string sup_class = fcn->name ();
      if (! symtab.set_class_relationship (sup_class, inf_class))
        error ("superiorto: opposite precedence already set for %s and %s",
               sup_class.c_str (), inf_class.c_str ());
    }

  return ovl ();
}

DEFMETHOD (inferiorto, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} inferiorto (@var{class_name}, @dots{})
When called from a class constructor, mark the object currently constructed
as having a lower precedence than @var{class_name}.

More that one such class can be specified in a single call.  This function
may @emph{only} be called from a class constructor.
@seealso{superiorto}
@end deftypefn */)
{
  tree_evaluator& tw = interp.get_evaluator ();

  octave_function *fcn = tw.caller_function ();

  if (! fcn || ! fcn->is_class_constructor ())
    error ("inferiorto: invalid call from outside class constructor");

  for (int i = 0; i < args.length (); i++)
    {
      std::string sup_class = args(i).xstring_value ("inferiorto: CLASS_NAME must be a string");

      if (is_built_in_class (sup_class))
        error ("inferiorto: cannot give user-defined class lower "
               "precedence than built-in class");

      symbol_table& symtab = interp.get_symbol_table ();

      std::string inf_class = fcn->name ();
      if (! symtab.set_class_relationship (sup_class, inf_class))
        error ("inferiorto: opposite precedence already set for %s and %s",
               inf_class.c_str (), sup_class.c_str ());
    }

  return ovl ();
}

// The following classes allow us to define "inline" function objects as
// legacy @class objects (as they appear to be in Matlab) while
// preserving the is_inline_function and function_value methods that
// were previously available in the octave_fcn_inline class.  However,
// inline function objects no longer behave as octave_fcn_handle objects
// so calling is_function_handle for them no longer returns true.  I see
// no reasonable way to preserve that behavior.  The goal here is to
// allow most code that used the old octave_inline_fcn object to
// continue to work while eliminating the octave_inline_fcn class that
// was derived from the octave_fcn_handle class.  Making that change
// appears to be necessary to properly fix function handle behavior and
// improve Matlab compatibility.  It's unfortunate if this change causes
// trouble, but I see no better fix.  Ultimately, we should replace all
// uses of "inline" function objects with anonymous functions.

class octave_inline;

// The following class can be removed once the
// octave_value::function_value method is removed.

class
octave_inline_fcn : public octave_function
{
public:

  octave_inline_fcn (octave_inline *obj) : m_inline_obj (obj) { }

  // No copying!

  octave_inline_fcn (const octave_inline_fcn& ob) = delete;

  octave_inline_fcn& operator = (const octave_inline_fcn& ob) = delete;

  ~octave_inline_fcn (void) = default;

  // Override default call method because we ultimately use feval to
  // execute the inline function and that will push a stack frame.

  octave_value_list
  call (tree_evaluator& tw, int nargout = 0,
        const octave_value_list& args = octave_value_list ())
  {
    return execute (tw, nargout, args);
  }

  octave_value_list
  execute (tree_evaluator& tw, int nargout = 0,
           const octave_value_list& args = octave_value_list ());

private:

  octave_inline *m_inline_obj;
};

// Once the octave_inline_fcn class is removed, we should also be able
// to eliminate the octave_inline class below and replace the
// octave_value::is_inline_function method with
//
// bool octave_value::is_inline_function (void) const
// {
//   return class_name () == "inline";
// }

class
octave_inline : public octave_class
{
public:

  octave_inline (const octave_map& m)
    : octave_class (m, "inline"), m_fcn_obj (new octave_inline_fcn (this))
  { }

  octave_inline (const octave_inline&) = default;

  ~octave_inline (void) = default;

  octave_base_value * clone (void) const { return new octave_inline (*this); }

  octave_base_value * empty_clone (void) const
  {
    return new octave_inline (octave_map (map_keys ()));
  }

  bool is_inline_function (void) const { return true; }

  octave_function * function_value (bool)
  {
    return m_fcn_obj.get ();
  }

private:

  std::shared_ptr<octave_inline_fcn> m_fcn_obj;
};

octave_value_list
octave_inline_fcn::execute (tree_evaluator& tw, int nargout,
                            const octave_value_list& args)
{
  interpreter& interp = tw.get_interpreter ();

  return interp.feval (octave_value (m_inline_obj, true), args, nargout);
}


DEFUN (__inline_ctor__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{inline_obj} =} __inline_ctor__ (@var{prop_struct})
Implement final construction for inline objects.
@end deftypefn */)
{
  // Input validation has already been done in input.m.
  return octave_value (new octave_inline (args(0).map_value ()));
}

OCTAVE_END_NAMESPACE(octave)
