/*

Copyright (C) 2007, 2008, 2009 John W. Eaton
Copyright (C) 2009 VZLU Prague

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

#include "Array-util.h"
#include "byte-swap.h"
#include "oct-locbuf.h"
#include "lo-mappers.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "file-ops.h"
#include "gripes.h"
#include "load-path.h"
#include "ls-hdf5.h"
#include "ls-oct-ascii.h"
#include "ls-oct-binary.h"
#include "ls-utils.h"
#include "oct-lvalue.h"
#include "ov-class.h"
#include "ov-fcn.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "parse.h"
#include "pr-output.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "variables.h"

DEFINE_OCTAVE_ALLOCATOR(octave_class);

int octave_class::t_id (-1);

const std::string octave_class::t_name ("class");

void
octave_class::register_type (void)
{
  t_id = octave_value_typeinfo::register_type
    (octave_class::t_name, "<unknown>", octave_value (new octave_class ()));
}

octave_class::octave_class (const Octave_map& m, const std::string& id, 
                            const octave_value_list& parents)
  : octave_base_value (), map (m), c_name (id), obsolete_copies (0)
{
  octave_idx_type n = parents.length ();

  for (octave_idx_type idx = 0; idx < n; idx++)
    {
      octave_value parent = parents(idx);

      if (! parent.is_object ())
        error ("parents must be objects");
      else
        {
          std::string cnm = parent.class_name ();

          if (find_parent_class (cnm))
            error ("duplicate class in parent tree");
          else
            {
              parent_list.push_back (cnm);

              map.assign (cnm, parent);
            }
        }
    }

  if (! error_state)
    symbol_table::add_to_parent_map (id, parent_list);
}

octave_base_value *
octave_class::unique_clone (void)
{ 
  if (count == obsolete_copies)
    {
      // All remaining copies are obsolete. We don't actually need to clone.
      count++;
      return this;
    }
  else
    { 
      // In theory, this shouldn't be happening, but it's here just in case.
      if (count < obsolete_copies)
        obsolete_copies = 0;

      return clone (); 
    }
}

static std::string
get_current_method_class (void)
{
  std::string retval;

  octave_function *fcn = octave_call_stack::current ();

  if (fcn->is_class_method () || fcn->is_class_constructor ())
    retval = fcn->dispatch_class ();

  return retval;
}

static void
gripe_invalid_index1 (void)
{
  error ("invalid index for class");
}

static void
gripe_invalid_index_for_assignment (void)
{
  error ("invalid index for class assignment");
}

static void
gripe_invalid_index_type (const std::string& nm, char t)
{
  error ("%s cannot be indexed with %c", nm.c_str (), t);
}

static void
gripe_failed_assignment (void)
{
  error ("assignment to class element failed");
}

static inline octave_value_list
sanitize (const octave_value_list& ovl)
{
  octave_value_list retval = ovl;

  for (octave_idx_type i = 0; i < ovl.length (); i++)
    {
      if (retval(i).is_magic_colon ())
        retval(i) = ":";
    }

  return retval;
}

static inline octave_value
make_idx_args (const std::string& type,
               const std::list<octave_value_list>& idx,
               const std::string& who)
{
  octave_value retval;

  size_t len = type.length ();

  if (len == idx.size ())
    {
      Cell type_field (1, len);
      Cell subs_field (1, len);

      std::list<octave_value_list>::const_iterator p = idx.begin ();

      for (size_t i = 0; i < len; i++)
        {
          char t = type[i];

          switch (t)
            {
            case '(':
              type_field(i) = "()";
              subs_field(i) = Cell (sanitize (*p++));
              break;

            case '{':
              type_field(i) = "{}";
              subs_field(i) = Cell (sanitize (*p++));
              break;

            case '.':
              {
                type_field(i) = ".";

                octave_value_list vlist = *p++;

                if (vlist.length () == 1)
                  {
                    octave_value val = vlist(0);

                    if (val.is_string ())
                      subs_field(i) = val;
                    else
                      {
                        error ("expecting character string argument for `.' index");
                        return retval;
                      }
                  }
                else
                  {
                    error ("expecting single argument for `.' index");
                    return retval;
                  }
              }
              break;

            default:
              panic_impossible ();
              break;
            }
        }

      Octave_map m;

      m.assign ("type", type_field);
      m.assign ("subs", subs_field);

      retval = m;
    }
  else
    error ("invalid index for %s", who.c_str ());

  return retval;
}

Cell
octave_class::dotref (const octave_value_list& idx)
{
  Cell retval;

  assert (idx.length () == 1);

  std::string method_class = get_current_method_class ();

  // Find the class in which this method resides before attempting to access
  // the requested field.

  octave_base_value *obvp
    = method_class.empty () ? 0 : find_parent_class (method_class);

  Octave_map my_map;

  my_map = obvp ? obvp->old_map_value () : map;

  std::string nm = idx(0).string_value ();

  if (! error_state)
    {
      Octave_map::const_iterator p = my_map.seek (nm);

      if (p != my_map.end ())
        retval = my_map.contents (p);
      else
        error ("class has no member `%s'", nm.c_str ());
    }
  else
    gripe_invalid_index1 ();

  return retval;
}

static bool
called_from_builtin (void)
{
  octave_function *fcn = octave_call_stack::caller ();

  // FIXME -- we probably need a better check here, or some other
  // mechanism to avoid overloaded functions when builtin is used.
  // For example, what if someone overloads the builtin function?
  // Also, are there other places where using builtin is not properly
  // avoiding dispatch?

  return (fcn && fcn->name () == "builtin");
}

Matrix
octave_class::size (void)
{
  if (in_class_method () || called_from_builtin ())
    return octave_base_value::size ();

  Matrix retval (1, 2, 1.0);
  octave_value meth = symbol_table::find_method ("size", class_name ());

  if (meth.is_defined ())
    {
      count++;
      octave_value_list args (1, octave_value (this));

      octave_value_list lv = feval (meth.function_value (), args, 1);
      if (lv.length () > 0 && lv(0).is_matrix_type () && lv(0).dims ().is_vector ())
        retval = lv(0).matrix_value ();
      else
        error ("@%s/size: invalid return value", class_name ().c_str ());
    }

  return retval;
}

octave_idx_type
octave_class::numel (const octave_value_list& idx)
{
  if (in_class_method () || called_from_builtin ())
    return octave_base_value::numel (idx);

  octave_idx_type retval = -1;
  const std::string cn = class_name ();

  octave_value meth = symbol_table::find_method ("numel", cn);

  if (meth.is_defined ())
    {
      octave_value_list args (idx.length () + 1, octave_value ());

      count++;
      args(0) = octave_value (this);

      for (octave_idx_type i = 0; i < idx.length (); i++)
        args(i+1) = idx(i);

      octave_value_list lv = feval (meth.function_value (), args, 1);
      if (lv.length () == 1 && lv(0).is_scalar_type ())
        retval = lv(0).idx_type_value (true);
      else
        error ("@%s/numel: invalid return value", cn.c_str ());
    }
  else
    retval = octave_base_value::numel (idx);

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
      // FIXME -- this block of code is the same as the body of
      // octave_struct::subsref.  Maybe it could be shared instead of
      // duplicated.

      int skip = 1;

      switch (type[0])
        {
        case '(':
          {
            if (type.length () > 1 && type[1] == '.')
              {
                std::list<octave_value_list>::const_iterator p = idx.begin ();
                octave_value_list key_idx = *++p;

                Cell tmp = dotref (key_idx);

                if (! error_state)
                  {
                    Cell t = tmp.index (idx.front ());

                    retval(0) = (t.length () == 1) ? t(0) : octave_value (t, true);

                    // We handled two index elements, so tell
                    // next_subsref to skip both of them.

                    skip++;
                  }
              }
            else
              retval(0) = octave_value (map.index (idx.front ()),
                                        class_name ());
          }
          break;

        case '.':
          {
            if (map.numel() > 0)
              {
                Cell t = dotref (idx.front ());

                retval(0) = (t.length () == 1) ? t(0) : octave_value (t, true);
              }
          }
          break;

        case '{':
          gripe_invalid_index_type (type_name (), type[0]);
          break;

        default:
          panic_impossible ();
        }

      // FIXME -- perhaps there should be an
      // octave_value_list::next_subsref member function?  See also
      // octave_user_function::subsref.

      if (idx.size () > 1)
        retval = retval(0).next_subsref (nargout, type, idx, skip);
    }
  else
    {
      octave_value meth = symbol_table::find_method ("subsref", class_name ());

      if (meth.is_defined ())
        {
          octave_value_list args;

          args(1) = make_idx_args (type, idx, "subsref");

          if (error_state)
            return octave_value_list ();

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
              true_nargout = numel (tmp);
            }

          retval = feval (meth.function_value (), args, true_nargout);

          // Since we're handling subsref, return the list in the first value
          // if it has more than one element, to be able to pass through
          // rvalue1 calls. 
          if (retval.length () > 1)
            retval = octave_value (retval, true);
        }
      else
        {
          if (type.length () == 1 && type[0] == '(')
            retval(0) = octave_value (map.index (idx.front ()), class_name ());
          else
            gripe_invalid_index1 ();
        }
    }

  return retval;
}

octave_value
octave_class::numeric_conv (const Cell& val, const std::string& type)
{
  octave_value retval;

  if (val.length () == 1)
    {
      retval = val(0);

      if (type.length () > 0 && type[0] == '.' && ! retval.is_map ())
        retval = Octave_map ();
    }
  else
    gripe_invalid_index_for_assignment ();

  return retval;
}

octave_value
octave_class::subsasgn (const std::string& type,
                        const std::list<octave_value_list>& idx,
                        const octave_value& rhs)
{
  octave_value retval;

  if (! (in_class_method () || called_from_builtin ()))
    {
      octave_value meth = symbol_table::find_method ("subsasgn", class_name ());

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

          if (error_state)
            return octave_value_list ();

          count++;
          args(0) = octave_value (this);

          // Now comes the magic. Count copies with me:
          // 1. myself (obsolete)
          // 2. the copy inside args (obsolete)
          // 3. the copy in method's symbol table (working)
          // ... possibly more (not obsolete).
          //
          // So we mark 2 copies as obsolete and hold our fingers crossed.
          // But prior to doing that, check whether the routine is amenable
          // to the optimization.
          // It is essential that the handling function doesn't store extra
          // copies anywhere. If it does, things will not break but the
          // optimization won't work.

          octave_value_list tmp;

          if (obsolete_copies == 0 && meth.is_user_function ()
              && meth.user_function_value ()->subsasgn_optimization_ok ())
            {
              unwind_protect frame;
              frame.protect_var (obsolete_copies);
              obsolete_copies = 2;

              tmp = feval (meth.function_value (), args);
            }
          else
            tmp = feval (meth.function_value (), args);

          // FIXME -- should the subsasgn method be able to return
          // more than one value?

          if (tmp.length () > 1)
            error ("expecting single return value from @%s/subsasgn",
                   class_name().c_str ());

          else
            retval = tmp(0);

          return retval;
        }
    }

  // FIXME -- this block of code is the same as the body of
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
                std::list<octave_value_list>::const_iterator p = idx.begin ();
                octave_value_list t_idx = *p;

                octave_value_list key_idx = *++p;

                assert (key_idx.length () == 1);

                std::string key = key_idx(0).string_value ();

                if (! error_state)
                  {
                    octave_value u;

                    if (! map.contains (key))
                      u = octave_value::empty_conv (type.substr (2), rhs);
                    else
                      {
                        Cell map_val = map.contents (key);

                        Cell map_elt = map_val.index (idx.front (), true);

                        u = numeric_conv (map_elt, type.substr (2));
                      }

                    if (! error_state)
                      {
                        std::list<octave_value_list> next_idx (idx);

                        // We handled two index elements, so subsasgn to
                        // needs to skip both of them.

                        next_idx.erase (next_idx.begin ());
                        next_idx.erase (next_idx.begin ());

                        u.make_unique ();

                        t_rhs = u.subsasgn (type.substr (2), next_idx, rhs);
                      }
                  }
                else
                  gripe_invalid_index_for_assignment ();
              }
            else
              gripe_invalid_index_for_assignment ();
          }
          break;

        case '.':
          {
            octave_value_list key_idx = idx.front ();

            assert (key_idx.length () == 1);

            std::string key = key_idx(0).string_value ();

            std::list<octave_value_list> next_idx (idx);

            next_idx.erase (next_idx.begin ());

            std::string next_type = type.substr (1);

            Cell tmpc (1, 1);
            Octave_map::iterator pkey = map.seek (key);
            if (pkey != map.end ())
              {
                pkey->second.make_unique ();
                tmpc = pkey->second;
              }

            // FIXME: better code reuse?
            if (! error_state)
              {
                if (tmpc.numel () == 1)
                  {
                    octave_value& tmp = tmpc(0);

                    if (! tmp.is_defined () || tmp.is_zero_by_zero ())
                      {
                        tmp = octave_value::empty_conv (next_type, rhs);
                        tmp.make_unique (); // probably a no-op.
                      }
                    else
                      // optimization: ignore the copy still stored inside our map.
                      tmp.make_unique (1);

                    if (! error_state)
                      t_rhs = tmp.subsasgn (next_type, next_idx, rhs);
                  }
                else
                  gripe_indexed_cs_list ();
              }
          }
          break;

        case '{':
          gripe_invalid_index_type (type_name (), type[0]);
          break;

        default:
          panic_impossible ();
        }
    }

  if (! error_state)
    {
      switch (type[0])
        {
        case '(':
          {
            if (n > 1 && type[1] == '.')
              {
                std::list<octave_value_list>::const_iterator p = idx.begin ();
                octave_value_list key_idx = *++p;

                assert (key_idx.length () == 1);

                std::string key = key_idx(0).string_value ();

                if (! error_state)
                  {
                    map.assign (idx.front (), key, t_rhs);

                    if (! error_state)
                      {
                        count++;
                        retval = octave_value (this);
                      }
                    else
                      gripe_failed_assignment ();
                  }
                else
                  gripe_failed_assignment ();
              }
            else
              {
                if (t_rhs.is_object () || t_rhs.is_map ())
                  {
                    Octave_map rhs_map = t_rhs.old_map_value ();

                    if (! error_state)
                      {
                        map.assign (idx.front (), rhs_map);

                        if (! error_state)
                          {
                            count++;
                            retval = octave_value (this);
                          }
                        else
                          gripe_failed_assignment ();
                      }
                    else
                      error ("invalid class assignment");
                  }
                else
                  {
                    if (t_rhs.is_empty ())
                      {
                        map.maybe_delete_elements (idx.front());

                        if (! error_state)
                          {
                            count++;
                            retval = octave_value (this);
                          }
                        else
                          gripe_failed_assignment ();
                      }
                    else
                      error ("invalid class assignment");
                  }
              }
          }
          break;

        case '.':
          {
            // Find the class in which this method resides before 
            // attempting to access the requested field.

            std::string method_class = get_current_method_class ();

            octave_base_value *obvp = unique_parent_class (method_class);

            if (obvp)
              {
                octave_value_list key_idx = idx.front ();

                assert (key_idx.length () == 1);

                std::string key = key_idx(0).string_value ();

                if (! error_state)
                  {
                    obvp->assign (key, t_rhs);

                    if (! error_state)
                      {
                        count++;
                        retval = octave_value (this);
                      }
                    else
                      gripe_failed_assignment ();
                  }
                else
                  gripe_failed_assignment ();
              }
            else
              error ("malformed class");
          }
          break;

        case '{':
          gripe_invalid_index_type (type_name (), type[0]);
          break;

        default:
          panic_impossible ();
        }
    }
  else
    gripe_failed_assignment ();

  return retval;
}

idx_vector
octave_class::index_vector (void) const
{
  idx_vector retval;

  octave_value meth = symbol_table::find_method ("subsindex", class_name ());

  if (meth.is_defined ())
    {
      octave_value_list args;
      args(0) = octave_value (new octave_class (map, c_name));

      octave_value_list tmp = feval (meth.function_value (), args, 1);

      if (!error_state && tmp.length () >= 1)
        {
          if (tmp(0).is_object())
            error ("subsindex function must return a valid index vector");
          else
            // Index vector returned by subsindex is zero based 
            // (why this inconsistency Mathworks?), and so we must
            // add one to the value returned as the index_vector method
            // expects it to be one based.
            retval = do_binary_op (octave_value::op_add, tmp (0), 
                                   octave_value (1.0)).index_vector ();
        }
    }
  else
    error ("no subsindex method defined for class %s",
           class_name().c_str ());

  return retval;
}

size_t
octave_class::byte_size (void) const
{
  // Neglect the size of the fieldnames.

  size_t retval = 0;

  for (Octave_map::const_iterator p = map.begin (); p != map.end (); p++)
    {
      std::string key = map.key (p);

      octave_value val = octave_value (map.contents (p));

      retval += val.byte_size ();
    }

  return retval;
}

string_vector
octave_class::map_keys (void) const
{
  string_vector retval;
  gripe_wrong_type_arg ("octave_class::map_keys()", type_name ());
  return retval;
}

octave_base_value *
octave_class::find_parent_class (const std::string& parent_class_name)
{
  octave_base_value* retval = 0;

  if (parent_class_name == class_name ())
    retval = this;
  else
    {
      for (std::list<std::string>::iterator pit = parent_list.begin ();
           pit != parent_list.end ();
           pit++)
        {
          Octave_map::const_iterator smap = map.seek (*pit);

          const Cell& tmp = smap->second;

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
  octave_base_value* retval = 0;

  if (parent_class_name == class_name ())
    retval = this;
  else
    {
      for (std::list<std::string>::iterator pit = parent_list.begin ();
           pit != parent_list.end ();
           pit++)
        {
          Octave_map::iterator smap = map.seek (*pit);

          Cell& tmp = smap->second;

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

void
octave_class::print (std::ostream& os, bool) const
{
  print_raw (os);
}

void
octave_class::print_raw (std::ostream& os, bool) const
{
  unwind_protect frame;

  frame.protect_var (Vstruct_levels_to_print);

  indent (os);
  os << "  <class " << class_name () << ">";
  newline (os);
}

bool
octave_class::print_name_tag (std::ostream& os, const std::string& name) const
{
  bool retval = false;

  indent (os);
  os << name << " =";
  newline (os);
  newline (os);

  return retval;
}

void
octave_class::print_with_name (std::ostream& os, const std::string& name, 
                               bool)
{
  octave_value fcn = symbol_table::find_method ("display", class_name ());

  if (fcn.is_defined ())
    {
      octave_value_list args;

      count++;
      args(0) = octave_value (this);
      
      string_vector arg_names (1);

      arg_names[0] = name;

      args.stash_name_tags (arg_names);

      feval (fcn.function_value (), args);
    }
  else
    {
      indent (os);
      os << name << " = <class " << class_name () << ">";
      newline (os);
    }
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
      octave_value ctor = symbol_table::find_method (c_name, c_name);

      if (ctor.is_defined ())
        {
          octave_value_list result = feval (ctor, 1);

          if (result.length () == 1)
            retval = true;
          else
            warning ("call to constructor for class %s failed", c_name.c_str ());
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
  bool retval = true, might_have_inheritance = false;
  std::string dbgstr = "dork";

  // First, check to see if there might be an issue with inheritance.
  for (Octave_map::const_iterator p = map.begin (); p != map.end (); p++)
    {
      std::string  key = map.key (p);
      Cell         val = map.contents (p);
      if ( val(0).is_object() )
        {
          dbgstr = "blork";
          if( key == val(0).class_name() )
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
          parent_list = exmplr.parents ();
          for (std::list<std::string>::iterator pit = parent_list.begin ();
               pit != parent_list.end ();
               pit++)
            {
              dbgstr = *pit;
              bool dbgbool = map.contains (*pit);
              if (!dbgbool)
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
  Octave_map m;
  if (load_path::find_method (class_name (), "saveobj") != std::string ())
    {
      octave_value in = new octave_class (*this);
      octave_value_list tmp = feval ("saveobj", in, 1);
      if (! error_state)
        m = tmp(0).old_map_value ();
      else
        return false;
    }
  else
    m = old_map_value ();

  os << "# length: " << m.nfields () << "\n";

  Octave_map::iterator i = m.begin ();
  while (i != m.end ())
    {
      octave_value val = map.contents (i);

      bool b = save_ascii_data (os, val, m.key (i), false, 0);
      
      if (! b)
        return os;

      i++;
    }

  return true;
}

bool 
octave_class::load_ascii (std::istream& is)
{
  octave_idx_type len = 0;
  std::string classname;
  bool success = true;

  if (extract_keyword (is, "classname", classname) && classname != "")
    {
      if (extract_keyword (is, "length", len) && len >= 0)
        {
          if (len > 0)
            {
              Octave_map m (map);

              for (octave_idx_type j = 0; j < len; j++)
                {
                  octave_value t2;
                  bool dummy;

                  // recurse to read cell elements
                  std::string nm
                    = read_ascii_data (is, std::string (), dummy, t2, j);

                  if (! is)
                    break;

                  Cell tcell = t2.is_cell () ? t2.cell_value () : Cell (t2);

                  if (error_state)
                    {
                      error ("load: internal error loading class elements");
                      return false;
                    }

                  m.assign (nm, tcell);
                }

              if (is) 
                {
                  c_name = classname;
                  reconstruct_exemplar ();

                  map = m;
                  
                  if (! reconstruct_parents ())
                    warning ("load: unable to reconstruct object inheritance");
                  else
                    {
                      if (load_path::find_method (classname, "loadobj")
                          != std::string ())
                        {
                          octave_value in = new octave_class (*this);
                          octave_value_list tmp = feval ("loadobj", in, 1);

                          if (! error_state)
                            map = tmp(0).old_map_value ();
                          else
                            success = false;
                        }
                    }
                }
              else
                {
                  error ("load: failed to load class");
                  success = false;
                }
            }
          else if (len == 0 )
            {
              map = Octave_map (dim_vector (1, 1));
              c_name = classname;
            }
          else
            panic_impossible ();
        }
      else 
        {
          error ("load: failed to extract number of elements in class");
          success = false;
        }
    }
  else
    {
      error ("load: failed to extract name of class");
      success = false;
    }

  return success;
}

bool 
octave_class::save_binary (std::ostream& os, bool& save_as_floats)
{
  int32_t classname_len = class_name().length ();

  os.write (reinterpret_cast<char *> (&classname_len), 4);
  os << class_name ();

  Octave_map m;
  if (load_path::find_method (class_name (), "saveobj") != std::string ())
    {
      octave_value in = new octave_class (*this);
      octave_value_list tmp = feval ("saveobj", in, 1);
      if (! error_state)
        m = tmp(0).old_map_value ();
      else
        return false;
    }
  else
    m = old_map_value ();

  int32_t len = m.nfields();
  os.write (reinterpret_cast<char *> (&len), 4);
  
  Octave_map::iterator i = m.begin ();
  while (i != m.end ())
    {
      octave_value val = map.contents (i);

      bool b = save_binary_data (os, val, m.key (i), "", 0, save_as_floats);
      
      if (! b)
        return os;

      i++;
    }

  return true;
}

bool 
octave_class::load_binary (std::istream& is, bool swap,
                            oct_mach_info::float_format fmt)
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
      Octave_map m (map);

      for (octave_idx_type j = 0; j < len; j++)
        {
          octave_value t2;
          bool dummy;
          std::string doc;

          // recurse to read cell elements
          std::string nm = read_binary_data (is, swap, fmt, std::string (), 
                                             dummy, t2, doc);

          if (! is)
            break;

          Cell tcell = t2.is_cell () ? t2.cell_value () : Cell (t2);
 
          if (error_state)
            {
              error ("load: internal error loading class elements");
              return false;
            }

          m.assign (nm, tcell);
        }

      if (is) 
        {
          map = m;

          if (! reconstruct_parents ())
            warning ("load: unable to reconstruct object inheritance");
          else
            {
              if (load_path::find_method (c_name, "loadobj") != std::string ())
                {
                  octave_value in = new octave_class (*this);
                  octave_value_list tmp = feval ("loadobj", in, 1);

                  if (! error_state)
                    map = tmp(0).old_map_value ();
                  else
                    success = false;
                }
            }
        }
      else
        {
          warning ("load: failed to load class");
          success = false;
        }
    }
  else if (len == 0 )
    map = Octave_map (dim_vector (1, 1));
  else
    panic_impossible ();

  return success;
}

#if defined (HAVE_HDF5)

bool
octave_class::save_hdf5 (hid_t loc_id, const char *name, bool save_as_floats)
{
  hsize_t hdims[3];
  hid_t group_hid = -1;
  hid_t type_hid = -1;
  hid_t space_hid = -1;
  hid_t class_hid = -1;
  hid_t data_hid = -1;
  Octave_map m;
  Octave_map::iterator i;

#if HAVE_HDF5_18
  group_hid = H5Gcreate (loc_id, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
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
  space_hid = H5Screate_simple (0 , hdims, 0);
  if (space_hid < 0)
    goto error_cleanup;
#if HAVE_HDF5_18
  class_hid = H5Dcreate (group_hid, "classname",  type_hid, space_hid,
                         H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  class_hid = H5Dcreate (group_hid, "classname",  type_hid, space_hid,
                         H5P_DEFAULT);
#endif
  if (class_hid < 0 || H5Dwrite (class_hid, type_hid, H5S_ALL, H5S_ALL, 
                                    H5P_DEFAULT, c_name.c_str ()) < 0)
    goto error_cleanup;

#if HAVE_HDF5_18
  data_hid = H5Gcreate (group_hid, "value", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  data_hid = H5Gcreate (group_hid, "value", 0);
#endif
  if (data_hid < 0)
    goto error_cleanup;

  if (load_path::find_method (class_name (), "saveobj") != std::string ())
    {
      octave_value in = new octave_class (*this);
      octave_value_list tmp = feval ("saveobj", in, 1);
      if (! error_state)
        m = tmp(0).old_map_value ();
      else
        goto error_cleanup;
    }
  else
    m = old_map_value ();

  // recursively add each element of the class to this group
  i = m.begin ();
  while (i != m.end ())
    {
      octave_value val = map.contents (i);

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
}

bool 
octave_class::load_hdf5 (hid_t loc_id, const char *name)
{
  bool retval = false;

  hid_t group_hid = -1;
  hid_t data_hid = -1;
  hid_t type_hid = -1;
  hid_t type_class_hid = -1;
  hid_t space_hid = -1;
  hid_t subgroup_hid = -1; 
  hid_t st_id = -1;

  hdf5_callback_data dsub;

  herr_t retval2 = 0;
  Octave_map m (dim_vector (1, 1));
  int current_item = 0;
  hsize_t num_obj = 0;
  int slen = 0;
  hsize_t rank = 0;

#if HAVE_HDF5_18
  group_hid = H5Gopen (loc_id, name, H5P_DEFAULT);
#else
  group_hid = H5Gopen (loc_id, name);
#endif
  if (group_hid < 0)
    goto error_cleanup;

#if HAVE_HDF5_18
  data_hid = H5Dopen (group_hid, "classname", H5P_DEFAULT);
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

      if (H5Dread (data_hid, st_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, 
                   classname) < 0)
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

#if HAVE_HDF5_18
  subgroup_hid = H5Gopen (group_hid, name, H5P_DEFAULT); 
#else
  subgroup_hid = H5Gopen (group_hid, name); 
#endif
  H5Gget_num_objs (subgroup_hid, &num_obj);
  H5Gclose (subgroup_hid);

  while (current_item < static_cast<int> (num_obj)
         && (retval2 = H5Giterate (group_hid, name, &current_item,
                                   hdf5_read_next_data, &dsub)) > 0)
    {
      octave_value t2 = dsub.tc;

      Cell tcell = t2.is_cell () ? t2.cell_value () : Cell (t2);
 
      if (error_state)
        {
          error ("load: internal error loading class elements");
          return false;
        }

      m.assign (dsub.name, tcell);

    }

  if (retval2 >= 0)
    {
      map = m;

      if (!reconstruct_parents ())
        warning ("load: unable to reconstruct object inheritance");
      else
        {
          if (load_path::find_method (c_name, "loadobj") != std::string ())
            {
              octave_value in = new octave_class (*this);
              octave_value_list tmp = feval ("loadobj", in, 1);

              if (! error_state)
                {
                  map = tmp(0).old_map_value ();
                  retval = true;
                }
              else
                retval = false;
            }
          else
            retval = true;
        }
    }
  
 error_cleanup:
  if (data_hid > 0)
    H5Dclose (data_hid);

  if (data_hid > 0)
    H5Gclose (group_hid);

  return retval;
}

#endif

mxArray *
octave_class::as_mxArray (void) const
{
  gripe_wrong_type_arg ("octave_class::as_mxArray ()", type_name ());

  return 0;
}

bool
octave_class::in_class_method (void)
{
  octave_function *fcn = octave_call_stack::current ();

  return (fcn
          && (fcn->is_class_method ()
              || fcn->is_class_constructor ()
              || fcn->is_private_function_of_class (class_name ()))
          && find_parent_class (fcn->dispatch_class ()));
}

octave_class::exemplar_info::exemplar_info (const octave_value& obj)
  : field_names (), parent_class_names ()
{
  if (obj.is_object ())
    {
      Octave_map m = obj.old_map_value ();
      field_names = m.keys ();

      parent_class_names = obj.parent_class_name_list ();
    }
  else
    error ("invalid call to exmplar_info constructor");
}


// A map from class names to lists of fields.
std::map<std::string, octave_class::exemplar_info> octave_class::exemplar_map;

bool
octave_class::exemplar_info::compare (const octave_value& obj) const
{
  bool retval = true;

  if (obj.is_object ())
    {
      if (nfields () == obj.nfields ())
        {
          Octave_map obj_map = obj.old_map_value ();
          string_vector obj_fnames = obj_map.keys ();
          string_vector fnames = fields ();

          for (octave_idx_type i = 0; i < nfields (); i++)
            {
              if (obj_fnames[i] != fnames[i])
                {
                  retval = false;
                  error ("mismatch in field names");
                  break;
                }
            }

          if (nparents () == obj.nparents ())
            {
              std::list<std::string> obj_parents
                = obj.parent_class_name_list ();
              std::list<std::string> pnames = parents ();

              std::list<std::string>::const_iterator p = obj_parents.begin ();
              std::list<std::string>::const_iterator q = pnames.begin ();

              while (p != obj_parents.end ())
                {
                  if (*p++ != *q++)
                    {
                      retval = false;
                      error ("mismatch in parent classes");
                      break;
                    }
                }
            }
          else
            {
              retval = false;
              error ("mismatch in number of parent classes");
            }
        }
      else
        {
          retval = false;
          error ("mismatch in number of fields");
        }
    }
  else
    {
      retval = false;
      error ("invalid comparison of class exemplar to non-class object");
    }

  return retval;
}

DEFUN (class, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} class (@var{expr})\n\
@deftypefnx {Built-in Function} {} class (@var{s}, @var{id})\n\
@deftypefnx {Built-in Function} {} class (@var{s}, @var{id}, @var{p}, @dots{})\n\
Return the class of the expression @var{expr} or create a class with\n\
fields from structure @var{s} and name (string) @var{id}.  Additional\n\
arguments name a list of parent classes from which the new class is\n\
derived.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    print_usage ();
  else if (nargin == 1)
    retval = args(0).class_name ();
  else
    {
      octave_function *fcn = octave_call_stack::caller ();

      if (fcn && fcn->is_class_constructor ())
        {
          Octave_map m = args(0).old_map_value ();

          if (! error_state)
            {
              std::string id = args(1).string_value ();

              if (! error_state)
                {
                  if (nargin == 2)
                    retval = octave_value (new octave_class (m, id));
                  else
                    {
                      octave_value_list parents = args.slice (2, nargin-2);

                      retval = octave_value (new octave_class (m, id, parents));
                    }

                  if (! error_state)
                    {
                      octave_class::exemplar_const_iterator it
                        = octave_class::exemplar_map.find (id);

                      if (it == octave_class::exemplar_map.end ())
                        octave_class::exemplar_map[id]
                          = octave_class::exemplar_info (retval);
                      else if (! it->second.compare (retval))
                        error ("class: object of class `%s' does not match previously constructed objects", id.c_str ());
                    }
                }
              else
                error ("class: expecting character string as second argument");
            }
          else
            error ("class: expecting structure as first argument");
        }
      else
        error ("class: invalid call from outside class constructor");
    }

  return retval;
}

DEFUN (__isa_parent__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __isa_parent__ (@var{class}, @var{name})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 2)
    {
      octave_value cls = args(0);
      octave_value nm = args(1);

      if (! error_state)
        {
          if (cls.find_parent_class (nm.string_value ()))
            retval = true;
        }
      else
        error ("__isa_parent__: expecting arguments to be character strings");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (__parent_classes__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __parent_classes__ (@var{x})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval = Cell ();

  if (args.length () == 1)
    {
      octave_value arg = args(0);

      if (arg.is_object ())
        retval = Cell (arg.parent_class_names ());
    }
  else
    print_usage ();

  return retval;
}

DEFUN (isobject, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isobject (@var{x})\n\
Return true if @var{x} is a class object.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_object ();
  else
    print_usage ();

  return retval;
}

DEFUN (ismethod, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ismethod (@var{x}, @var{method})\n\
Return true if @var{x} is a class object and the string @var{method}\n\
is a method of this class.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 2)
    {
      octave_value arg = args(0);

      std::string class_name;

      if (arg.is_object ())
        class_name = arg.class_name ();
      else if (arg.is_string ())
        class_name = arg.string_value ();
      else
        error ("ismethod: expecting object or class name as first argument");

      if (! error_state)
        {
          std::string method = args(1).string_value ();

          if (! error_state)
            {
              if (load_path::find_method (class_name, method) != std::string ())
                retval = true;
              else
                retval = false;
            }
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (methods, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} methods (@var{x})\n\
@deftypefnx {Built-in Function} {} methods (\"classname\")\n\
Return a cell array containing the names of the methods for the\n\
object @var{x} or the named class.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      octave_value arg = args(0);

      std::string class_name;

      if (arg.is_object ())
        class_name = arg.class_name ();
      else if (arg.is_string ())
        class_name = arg.string_value ();
      else
        error ("methods: expecting object or class name as argument");

      if (! error_state)
        {
          string_vector sv = load_path::methods (class_name);

          if (nargout == 0)
            {
              octave_stdout << "Methods for class " << class_name << ":\n\n";

              sv.list_in_columns (octave_stdout);

              octave_stdout << std::endl;
            }
          else
            retval = Cell (sv);
        }         
    }
  else
    print_usage ();

  return retval;
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

DEFUN (superiorto, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} superiorto (@var{class_name}, @dots{})\n\
When called from a class constructor, mark the object currently\n\
constructed as having a higher precedence than @var{class_name}.\n\
More that one such class can be specified in a single call.\n\
This function may only be called from a class constructor.\n\
@end deftypefn")
{
  octave_value retval;

  octave_function *fcn = octave_call_stack::caller ();

  if (fcn && fcn->is_class_constructor ())
    {
      for (int i = 0; i < args.length(); i++)
        {
          std::string class_name = args(i).string_value ();

          if (! error_state)
            {
              if (! is_built_in_class (class_name))
                {
                  std::string this_class_name = fcn->name ();

                  if (! symbol_table::set_class_relationship (this_class_name,
                                                              class_name))
                    {
                      error ("superiorto: precedence already set for %s and %s",
                             this_class_name.c_str (), class_name.c_str ());
                      break;
                    }
                }
              else
                {
                  // User defined classes always have higher precedence
                  // than built-in classes.
                }
            }
          else
            {
              error ("superiorto: expecting argument to be class name");
              break;
            }
        }
    }
  else
    error ("superiorto: invalid call from outside class constructor");

  return retval;
}

DEFUN (inferiorto, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} inferiorto (@var{class_name}, @dots{})\n\
When called from a class constructor, mark the object currently\n\
constructed as having a lower precedence than @var{class_name}.\n\
More that one such class can be specified in a single call.\n\
This function may only be called from a class constructor.\n\
@end deftypefn")
{
  octave_value retval;

  octave_function *fcn = octave_call_stack::caller ();

  if (fcn && fcn->is_class_constructor ())
    {
      for (int i = 0; i < args.length(); i++)
        {
          std::string class_name = args(i).string_value ();

          if (! error_state)
            {
              if (! is_built_in_class (class_name))
                {
                  std::string this_class_name = fcn->name ();

                  symbol_table::set_class_relationship (class_name,
                                                        this_class_name);

                  if (! symbol_table::set_class_relationship (this_class_name,
                                                              class_name))
                    {
                      error ("inferiorto: precedence already set for %s and %s",
                             this_class_name.c_str (), class_name.c_str ());
                      break;
                    }
                }
              else
                {
                  error ("inferiorto: cannot give user-defined class lower precedence than built-in class");
                  break;
                }
            }
          else
            {
              error ("inferiorto: expecting argument to be class name");
              break;
            }
        }
    }
  else
    error ("inferiorto: invalid call from outside class constructor");

  return retval;
}
