////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#include <iostream>

#include <sstream>

#include "oct-time.h"

#include "bp-table.h"
#include "defun.h"
#include "fcn-info.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "load-path.h"
#include "ov-classdef.h"
#include "ov-fcn.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "parse.h"
#include "pt-pr-code.h"
#include "symscope.h"
#include "symtab.h"

OCTAVE_BEGIN_NAMESPACE(octave)

symbol_table::symbol_table (interpreter& interp)
  : m_interpreter (interp), m_fcn_table (), m_class_precedence_table (),
    m_parent_map ()
{
  install_builtins ();
}

symbol_scope symbol_table::current_scope (void) const
{
  tree_evaluator& tw = m_interpreter.get_evaluator ();

  return tw.get_current_scope ();
}

bool symbol_table::is_built_in_function_name (const std::string& name)
{
  octave_value val = find_built_in_function (name);

  return val.is_defined ();
}

octave_value
symbol_table::find_scoped_function (const std::string& name,
                                    const symbol_scope& search_scope)
{
  if (name.empty ())
    return octave_value ();

  fcn_table_const_iterator p = m_fcn_table.find (name);

  if (p != m_fcn_table.end ())
    return p->second.find_scoped_function (search_scope);
  else
    {
      fcn_info finfo (name);

      octave_value fcn = finfo.find_scoped_function (search_scope);

      if (fcn.is_defined ())
        m_fcn_table[name] = finfo;

      return fcn;
    }
}

octave_value
symbol_table::find_private_function (const std::string& dir_name,
                                     const std::string& name)
{
  if (name.empty ())
    return octave_value ();

  fcn_table_const_iterator p = m_fcn_table.find (name);

  if (p != m_fcn_table.end ())
    return p->second.find_private_function (dir_name);
  else
    {
      fcn_info finfo (name);

      octave_value fcn = finfo.find_private_function (dir_name);

      if (fcn.is_defined ())
        m_fcn_table[name] = finfo;

      return fcn;
    }
}

// FIXME: this function only finds legacy class methods, not
// classdef methods.

octave_value symbol_table::find_method (const std::string& name,
                                        const std::string& dispatch_type)
{
  if (name.empty ())
    return octave_value ();

  fcn_table_const_iterator p = m_fcn_table.find (name);

  if (p != m_fcn_table.end ())
    return p->second.find_method (dispatch_type);
  else
    {
      fcn_info finfo (name);

      octave_value fcn = finfo.find_method (dispatch_type);

      if (fcn.is_defined ())
        m_fcn_table[name] = finfo;

      return fcn;
    }
}

octave_value symbol_table::find_built_in_function (const std::string& name)
{
  fcn_table_const_iterator p = m_fcn_table.find (name);

  if (p != m_fcn_table.end ())
    return p->second.find_built_in_function ();
  else
    {
      fcn_info finfo (name);

      octave_value fcn = finfo.find_built_in_function ();

      if (fcn.is_defined ())
        m_fcn_table[name] = finfo;

      return fcn;
    }
}

octave_value symbol_table::find_autoload (const std::string& name)
{
  if (name.empty ())
    return octave_value ();

  auto p = m_fcn_table.find (name);

  if (p != m_fcn_table.end ())
    return p->second.find_autoload ();
  else
    {
      fcn_info finfo (name);

      octave_value fcn = finfo.find_autoload ();

      if (fcn.is_defined ())
        m_fcn_table[name] = finfo;

      return fcn;
    }
}

octave_value
symbol_table::builtin_find (const std::string& name,
                            const symbol_scope& search_scope_arg)
{
  if (name.empty ())
    return octave_value ();

  fcn_table_iterator p = m_fcn_table.find (name);

  symbol_scope search_scope
    = (search_scope_arg ? search_scope_arg : current_scope ());

  if (p != m_fcn_table.end ())
    return p->second.builtin_find (search_scope);
  else
    {
      fcn_info finfo (name);

      octave_value fcn = finfo.builtin_find (search_scope);

      if (fcn.is_defined ())
        m_fcn_table[name] = finfo;

      return fcn;
    }

  return octave_value ();
}

octave_value
symbol_table::fcn_table_find (const std::string& name,
                              const octave_value_list& args,
                              const symbol_scope& search_scope_arg)
{
  if (name.empty ())
    return octave_value ();

  fcn_table_iterator p = m_fcn_table.find (name);

  symbol_scope search_scope
    = (search_scope_arg ? search_scope_arg : current_scope ());

  if (p != m_fcn_table.end ())
    return p->second.find (search_scope, args);
  else
    {
      fcn_info finfo (name);

      octave_value fcn = finfo.find (search_scope, args);

      if (fcn.is_defined ())
        m_fcn_table[name] = finfo;

      return fcn;
    }

  return octave_value ();
}

octave_value
symbol_table::find_function (const std::string& name,
                             const symbol_scope& search_scope_arg)
{
  if (name.empty ())
    return octave_value ();

  if (name[0] == '@')
    {
      std::size_t pos = name.find_first_of ('/');

      if (pos == std::string::npos)
        return octave_value ();

      std::string method = name.substr (pos+1);
      std::string dispatch_type = name.substr (1, pos-1);

      return find_method (method, dispatch_type);
    }
  else
    {
      symbol_scope search_scope
        = (search_scope_arg ? search_scope_arg : current_scope ());

      return find_function (name, ovl (), search_scope);
    }
}

octave_value
symbol_table::find_function (const std::string& name,
                             const octave_value_list& args,
                             const symbol_scope& search_scope)
{
  if (name.empty ())
    return octave_value ();

  return fcn_table_find (name, args, search_scope);
}

octave_value
symbol_table::find_user_function (const std::string& name)
{
  if (name.empty ())
    return octave_value ();

  auto p = m_fcn_table.find (name);

  if (p != m_fcn_table.end ())
    return p->second.find_user_function ();
  else
    {
      fcn_info finfo (name);

      octave_value fcn = finfo.find_user_function ();

      if (fcn.is_defined ())
        m_fcn_table[name] = finfo;

      return fcn;
    }
}

octave_value symbol_table::find_cmdline_function (const std::string& name)
{
  if (name.empty ())
    return octave_value ();

  auto p = m_fcn_table.find (name);

  if (p != m_fcn_table.end ())
    return p->second.find_cmdline_function ();
  else
    {
      fcn_info finfo (name);

      octave_value fcn = finfo.find_cmdline_function ();

      if (fcn.is_defined ())
        m_fcn_table[name] = finfo;

      return fcn;
    }
}

void symbol_table::install_cmdline_function (const std::string& name,
    const octave_value& fcn)
{
  auto p = m_fcn_table.find (name);

  if (p != m_fcn_table.end ())
    {
      fcn_info& finfo = p->second;

      finfo.install_cmdline_function (fcn);
    }
  else
    {
      fcn_info finfo (name);

      finfo.install_cmdline_function (fcn);

      m_fcn_table[name] = finfo;
    }
}

// Install local function FCN named NAME.  FILE_NAME is the name of
// the file containing the local function.

void symbol_table::install_local_function (const std::string& name,
    const octave_value& fcn,
    const std::string& file_name)
{
  auto p = m_fcn_table.find (name);

  if (p != m_fcn_table.end ())
    {
      fcn_info& finfo = p->second;

      finfo.install_local_function (fcn, file_name);
    }
  else
    {
      fcn_info finfo (name);

      finfo.install_local_function (fcn, file_name);

      m_fcn_table[name] = finfo;
    }
}

void symbol_table::install_user_function (const std::string& name,
    const octave_value& fcn)
{
  auto p = m_fcn_table.find (name);

  if (p != m_fcn_table.end ())
    {
      fcn_info& finfo = p->second;

      finfo.install_user_function (fcn);
    }
  else
    {
      fcn_info finfo (name);

      finfo.install_user_function (fcn);

      m_fcn_table[name] = finfo;
    }
}

// FIXME: should we ensure that FCN really is a built-in function
// object?
void symbol_table::install_built_in_function (const std::string& name,
    const octave_value& fcn)
{
  auto p = m_fcn_table.find (name);

  if (p != m_fcn_table.end ())
    {
      fcn_info& finfo = p->second;

      finfo.install_built_in_function (fcn);
    }
  else
    {
      fcn_info finfo (name);

      finfo.install_built_in_function (fcn);

      m_fcn_table[name] = finfo;
    }
}

// This is written as two separate functions instead of a single
// function with default values so that it will work properly with
// unwind_protect.

void symbol_table::clear_functions (bool force)
{
  auto p = m_fcn_table.begin ();

  while (p != m_fcn_table.end ())
    (p++)->second.clear (force);
}

void symbol_table::clear_function (const std::string& name)
{
  clear_user_function (name);
}

void symbol_table::clear_function_pattern (const std::string& pat)
{
  glob_match pattern (pat);

  auto p = m_fcn_table.begin ();

  while (p != m_fcn_table.end ())
    {
      if (pattern.match (p->first))
        (p++)->second.clear_user_function ();
      else
        p++;
    }
}

void symbol_table::clear_function_regexp (const std::string& pat)
{
  regexp pattern (pat);

  auto p = m_fcn_table.begin ();

  while (p != m_fcn_table.end ())
    {
      if (pattern.is_match (p->first))
        (p++)->second.clear_user_function ();
      else
        p++;
    }
}

void symbol_table::clear_user_function (const std::string& name)
{
  auto p = m_fcn_table.find (name);

  if (p != m_fcn_table.end ())
    {
      fcn_info& finfo = p->second;

      finfo.clear_user_function ();
    }
  // FIXME: is this necessary, or even useful?
  // else
  //   error ("clear: no such function '%s'", name.c_str ());
}

// This clears oct and mex files, including autoloads.
void symbol_table::clear_dld_function (const std::string& name)
{
  auto p = m_fcn_table.find (name);

  if (p != m_fcn_table.end ())
    {
      fcn_info& finfo = p->second;

      finfo.clear_autoload_function ();
      finfo.clear_user_function ();
    }
}

void symbol_table::clear_mex_functions (void)
{
  auto p = m_fcn_table.begin ();

  while (p != m_fcn_table.end ())
    (p++)->second.clear_mex_function ();
}

// Insert INF_CLASS in the set of class names that are considered
// inferior to SUP_CLASS.  Return FALSE if INF_CLASS is currently
// marked as superior to SUP_CLASS.

bool symbol_table::set_class_relationship (const std::string& sup_class,
    const std::string& inf_class)
{
  if (is_superiorto (inf_class, sup_class))
    return false;

  // If sup_class doesn't have an entry in the precedence table,
  // this will automatically create it, and associate to it a
  // singleton set {inf_class} of inferior classes.
  m_class_precedence_table[sup_class].insert (inf_class);

  return true;
}

// Has class A been marked as superior to class B?  Also returns
// TRUE if B has been marked as inferior to A, since we only keep
// one table, and convert inferiorto information to a superiorto
// relationship.  Two calls are required to determine whether there
// is no relationship between two classes:
//
//  if (symbol_table::is_superiorto (a, b))
//    // A is superior to B, or B has been marked inferior to A.
//  else if (symbol_table::is_superiorto (b, a))
//    // B is superior to A, or A has been marked inferior to B.
//  else
//    // No relation.

bool symbol_table::is_superiorto (const std::string& a, const std::string& b)
{
  class_precedence_table_const_iterator p = m_class_precedence_table.find (a);
  // If a has no entry in the precedence table, return false
  if (p == m_class_precedence_table.end ())
    return false;

  const std::set<std::string>& inferior_classes = p->second;
  std::set<std::string>::const_iterator q = inferior_classes.find (b);
  return (q != inferior_classes.end ());
}

void symbol_table::alias_built_in_function (const std::string& alias,
    const std::string& name)
{
  octave_value fcn = find_built_in_function (name);

  if (fcn.is_defined ())
    {
      fcn_info finfo (alias);

      finfo.install_built_in_function (fcn);

      m_fcn_table[alias] = finfo;
    }
  else
    panic ("alias: '%s' is undefined", name.c_str ());
}

void symbol_table::install_built_in_dispatch (const std::string& name,
    const std::string& klass)
{
  auto p = m_fcn_table.find (name);

  if (p != m_fcn_table.end ())
    {
      fcn_info& finfo = p->second;

      finfo.install_built_in_dispatch (klass);
    }
  else
    error ("install_built_in_dispatch: '%s' is undefined", name.c_str ());
}

std::list<std::string> symbol_table::user_function_names (void)
{
  std::list<std::string> retval;

  for (const auto& nm_finfo : m_fcn_table)
    {
      if (nm_finfo.second.is_user_function_defined ())
        retval.push_back (nm_finfo.first);
    }

  if (! retval.empty ())
    retval.sort ();

  return retval;
}

std::list<std::string> symbol_table::built_in_function_names (void)
{
  std::list<std::string> retval;

  for (const auto& nm_finfo : m_fcn_table)
    {
      octave_value fcn = nm_finfo.second.find_built_in_function ();

      if (fcn.is_defined ())
        retval.push_back (nm_finfo.first);
    }

  if (! retval.empty ())
    retval.sort ();

  return retval;
}

std::list<std::string> symbol_table::cmdline_function_names (void)
{
  std::list<std::string> retval;

  for (const auto& nm_finfo : m_fcn_table)
    {
      octave_value fcn = nm_finfo.second.find_cmdline_function ();

      if (fcn.is_defined ())
        retval.push_back (nm_finfo.first);
    }

  if (! retval.empty ())
    retval.sort ();

  return retval;
}

template <template <typename, typename...> class C, typename V,
          typename... A>
static octave_value
dump_container_map (const std::map<std::string, C<V, A...>>& container_map)
{
  if (container_map.empty ())
    return octave_value (Matrix ());

  std::map<std::string, octave_value> info_map;

  for (const auto& nm_container : container_map)
    {
      std::string nm = nm_container.first;
      const C<V, A...>& container = nm_container.second;
      info_map[nm] = Cell (container);
    }

  return octave_value (info_map);
}

octave_value symbol_table::dump (void) const
{
  std::map<std::string, octave_value> m
  = {{ "function_info", dump_fcn_table_map () },
    { "precedence_table", dump_container_map (m_class_precedence_table) },
    { "parent_classes", dump_container_map (m_parent_map) }
  };

  return octave_value (m);
}

void symbol_table::add_to_parent_map (const std::string& classname,
                                      const std::list<std::string>& parent_list)
{
  m_parent_map[classname] = parent_list;
}

std::list<std::string> symbol_table::parent_classes (const std::string& dispatch_type)
{
  std::list<std::string> retval;

  const_parent_map_iterator it = m_parent_map.find (dispatch_type);

  if (it != m_parent_map.end ())
    retval = it->second;

  for (const auto& nm : retval)
    {
      // Search for parents of parents and append them to the list.

      // FIXME: should we worry about a circular inheritance graph?

      std::list<std::string> parents = parent_classes (nm);

      if (! parents.empty ())
        retval.insert (retval.end (), parents.begin (), parents.end ());
    }

  return retval;
}

void symbol_table::cleanup (void)
{
  clear_functions ();

  m_fcn_table.clear ();
  m_class_precedence_table.clear ();
  m_parent_map.clear ();
}

fcn_info *symbol_table::get_fcn_info (const std::string& name)
{
  auto p = m_fcn_table.find (name);
  return p != m_fcn_table.end () ? &p->second : nullptr;
}

octave_value symbol_table::dump_fcn_table_map (void) const
{
  if (m_fcn_table.empty ())
    return octave_value (Matrix ());

  std::map<std::string, octave_value> info_map;

  for (const auto& nm_finfo : m_fcn_table)
    {
      std::string nm = nm_finfo.first;
      const fcn_info& finfo = nm_finfo.second;
      info_map[nm] = finfo.dump ();
    }

  return octave_value (info_map);
}

DEFMETHOD (__dump_symtab_info__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{S} =} __dump_symtab_info__ ()
@deftypefnx {} {@var{S} =} __dump_symtab_info__ (@var{function})
Return a structure with information from the symbol table.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  symbol_table& symtab = interp.get_symbol_table ();

  if (nargin == 0)
    return symtab.dump ();
  else
    {
      std::string fname = args(
                            0).xstring_value ("__dump_symtab_info__: argument must be a function name");

      fcn_info *finfo = symtab.get_fcn_info (fname);

      if (finfo)
        return finfo->dump ();
    }

  return ovl ();
}

DEFMETHOD (__get_cmdline_fcn_txt__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{str} =} __get_cmdline_fcn_txt__ (@var{name})
Undocumented internal function.
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  std::string name = args(
                       0).xstring_value ("__get_cmdline_fcn_txt__: first argument must be function name");

  symbol_table& symtab = interp.get_symbol_table ();

  octave_value ov = symtab.find_cmdline_function (name);

  octave_user_function *f = ov.user_function_value ();

  octave_value_list retval;

  if (f)
    {
      std::ostringstream buf;

      tree_print_code tpc (buf);

      f->accept (tpc);

      retval = ovl (buf.str ());
    }

  return retval;
}

// FIXME: should we have functions like this in Octave?
//
// DEFMETHOD (set_variable, interp, args, , "set_variable (NAME, VALUE)")
// {
//   if (args.length () != 2)
//     print_usage ();
//
//   std::string name = args(0).xstring_value ("set_variable: variable NAME must be a string");
//
//   symbol_table& symtab = interp.get_symbol_table ();
//
//   symtab.assign (name, args(1));
//
//   return ovl ();
// }
//
// DEFMETHOD (variable_value, interp, args, , "VALUE = variable_value (NAME)")
// {
//   if (args.length () != 1)
//     print_usage ();
//
//   octave_value retval;
//
//   std::string name = args(0).xstring_value ("variable_value: variable NAME must be a string");
//
//   symbol_table& symtab = interp.get_symbol_table ();
//
//   retval = symtab.varval (name);
//
//   if (retval.is_undefined ())
//     error ("variable_value: '%s' is not a variable in the current scope",
//            name.c_str ());
//
//   return retval;
// }

/*
bug #34497: 'clear -f' does not work for command line functions

This test relies on bar being a core function that is implemented in an m-file.
If the first assert fails, this is no longer the case and the tests need to be
updated to use some other function.

%!assert <34497> (! strcmp (which ("bar"), ""))

%!function x = bar ()
%!  x = 5;
%!endfunction
%!test
%! assert (bar == 5);
%! assert (strcmp (which ("bar"), "command-line function"));
%! clear -f bar;
%! assert (! strcmp (which ("bar"), ""));

%!function x = bar ()
%!  x = 5;
%!endfunction
%!test
%! assert (bar == 5);
%! assert (strcmp (which ("bar"), "command-line function"));
%! clear bar;
%! assert (! strcmp (which ("bar"), ""));
*/

OCTAVE_END_NAMESPACE(octave)

