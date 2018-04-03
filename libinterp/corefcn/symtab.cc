/*

Copyright (C) 1993-2018 John W. Eaton
Copyright (C) 2009 VZLU Prague, a.s.

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

#include <sstream>

#include "file-ops.h"
#include "file-stat.h"
#include "oct-env.h"
#include "oct-time.h"

#include "bp-table.h"
#include "defun.h"
#include "dirfns.h"
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
#include "symrec.h"
#include "symscope.h"
#include "symtab.h"
#include "unwind-prot.h"
#include "utils.h"

// Should Octave always check to see if function files have changed
// since they were last compiled?
static int Vignore_function_time_stamp = 1;

namespace octave
{
  static void
  split_name_with_package (const std::string& name, std::string& fname,
                           std::string& pname)
  {
    size_t pos = name.rfind ('.');

    fname.clear ();
    pname.clear ();

    if (pos != std::string::npos)
      {
        fname = name.substr (pos + 1);
        pname = name.substr (0, pos);
      }
    else
      fname = name;
  }

  // Check the load path to see if file that defined this is still
  // visible.  If the file is no longer visible, then erase the
  // definition and move on.  If the file is visible, then we also
  // need to check to see whether the file has changed since the
  // function was loaded/parsed.  However, this check should only
  // happen once per prompt (for files found from relative path
  // elements, we also check if the working directory has changed
  // since the last time the function was loaded/parsed).
  //
  // FIXME: perhaps this should be done for all loaded functions when
  // the prompt is printed or the directory has changed, and then we
  // would not check for it when finding symbol definitions.

  static inline bool
  load_out_of_date_fcn (const std::string& ff, const std::string& dir_name,
                        octave_value& function,
                        const std::string& dispatch_type = "",
                        const std::string& package_name = "")
  {
    bool retval = false;

    octave_value ov_fcn
      = octave::load_fcn_from_file (ff, dir_name, dispatch_type,
                                    package_name);

    if (ov_fcn.is_defined ())
      {
        retval = true;

        function = ov_fcn;
      }
    else
      function = octave_value ();

    return retval;
  }

  bool
  out_of_date_check (octave_value& function,
                     const std::string& dispatch_type,
                     bool check_relative)
  {
    bool retval = false;

    octave_function *fcn = function.function_value (true);

    if (fcn)
      {
        // FIXME: we need to handle subfunctions properly here.

        if (! fcn->is_subfunction ())
          {
            std::string ff = fcn->fcn_file_name ();

            if (! ff.empty ())
              {
                sys::time tc = fcn->time_checked ();

                bool relative = check_relative && fcn->is_relative ();

                if (tc <= Vlast_prompt_time
                    || (relative && tc < Vlast_chdir_time))
                  {
                    bool clear_breakpoints = false;
                    std::string nm = fcn->name ();
                    std::string pack = fcn->package_name ();
                    std::string canonical_nm = fcn->canonical_name ();

                    bool is_same_file = false;

                    std::string file;
                    std::string dir_name;

                    if (check_relative)
                      {
                        int nm_len = nm.length ();

                        if (sys::env::absolute_pathname (nm)
                            && ((nm_len > 4
                                 && (nm.substr (nm_len-4) == ".oct"
                                     || nm.substr (nm_len-4) == ".mex"))
                                || (nm_len > 2
                                    && nm.substr (nm_len-2) == ".m")))
                          file = nm;
                        else
                          {
                            // We don't want to make this an absolute name,
                            // because load_fcn_file looks at the name to
                            // decide whether it came from a relative lookup.

                            if (! dispatch_type.empty ())
                              {
                                load_path& lp = __get_load_path__ ("out_of_date_check");

                                file = lp.find_method (dispatch_type, nm,
                                                       dir_name, pack);

                                if (file.empty ())
                                  {
                                    std::string s_name;
                                    std::string s_pack;

                                    symbol_table& symtab
                                      = __get_symbol_table__ ("out_of_date_check");

                                    const std::list<std::string>& plist
                                      = symtab.parent_classes (dispatch_type);

                                    std::list<std::string>::const_iterator it
                                      = plist.begin ();

                                    while (it != plist.end ())
                                      {
                                        split_name_with_package (*it, s_name,
                                                                 s_pack);

                                        file = lp.find_method (*it, nm, dir_name,
                                                               s_pack);
                                        if (! file.empty ())
                                          {
                                            pack = s_pack;
                                            break;
                                          }

                                        it++;
                                      }
                                  }
                              }

                            // Maybe it's an autoload?
                            if (file.empty ())
                              file = lookup_autoload (nm);

                            if (file.empty ())
                              {
                                load_path& lp = __get_load_path__ ("out_of_date_check");
                                file = lp.find_fcn (nm, dir_name, pack);
                              }
                          }

                        if (! file.empty ())
                          is_same_file = same_file (file, ff);
                      }
                    else
                      {
                        is_same_file = true;
                        file = ff;
                      }

                    if (file.empty ())
                      {
                        // Can't see this function from current
                        // directory, so we should clear it.

                        function = octave_value ();

                        clear_breakpoints = true;
                      }
                    else if (is_same_file)
                      {
                        // Same file.  If it is out of date, then reload it.

                        sys::time ottp = fcn->time_parsed ();
                        time_t tp = ottp.unix_time ();

                        fcn->mark_fcn_file_up_to_date (sys::time ());

                        if (! (Vignore_function_time_stamp == 2
                               || (Vignore_function_time_stamp
                                   && fcn->is_system_fcn_file ())))
                          {
                            sys::file_stat fs (ff);

                            if (fs)
                              {
                                if (fs.is_newer (tp))
                                  {
                                    retval = load_out_of_date_fcn (ff, dir_name,
                                                                   function,
                                                                   dispatch_type,
                                                                   pack);

                                    clear_breakpoints = true;
                                  }
                              }
                            else
                              {
                                function = octave_value ();

                                clear_breakpoints = true;
                              }
                          }
                      }
                    else
                      {
                        // Not the same file, so load the new file in
                        // place of the old.

                        retval = load_out_of_date_fcn (file, dir_name, function,
                                                       dispatch_type, pack);

                        clear_breakpoints = true;
                      }

                    // If the function has been replaced then clear any
                    // breakpoints associated with it
                    if (clear_breakpoints)
                      {
                        octave::bp_table& bptab
                          = octave::__get_bp_table__ ("out_of_date_check");

                        bptab.remove_all_breakpoints_in_file (canonical_nm,
                                                              true);
                      }
                  }
              }
          }
      }

    return retval;
  }

  void
  symbol_table::clear_global (const std::string& name)
  {
    m_global_scope.clear_variable (name);
  }

  void
  symbol_table::clear_global_pattern (const std::string& pattern)
  {
    m_global_scope.clear_variable_pattern (pattern);
  }

  // Insert INF_CLASS in the set of class names that are considered
  // inferior to SUP_CLASS.  Return FALSE if INF_CLASS is currently
  // marked as superior to SUP_CLASS.

  bool
  symbol_table::set_class_relationship (const std::string& sup_class,
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

  bool
  symbol_table::is_superiorto (const std::string& a, const std::string& b)
  {
    class_precedence_table_const_iterator p = m_class_precedence_table.find (a);
    // If a has no entry in the precedence table, return false
    if (p == m_class_precedence_table.end ())
      return false;

    const std::set<std::string>& inferior_classes = p->second;
    std::set<std::string>::const_iterator q = inferior_classes.find (b);
    return (q != inferior_classes.end ());
  }

  octave_value
  symbol_table::find (const std::string& name, const octave_value_list& args,
                      bool skip_variables, bool local_funcs)
  {
    return (m_current_scope
            ? m_current_scope.find (name, args, skip_variables, local_funcs)
            : octave_value ());
  }

  octave_value
  symbol_table::builtin_find (const std::string& name)
  {
    fcn_table_iterator p = m_fcn_table.find (name);

    if (p != m_fcn_table.end ())
      return p->second.builtin_find ();
    else
      {
        fcn_info finfo (name);

        octave_value fcn = finfo.builtin_find ();

        if (fcn.is_defined ())
          m_fcn_table[name] = finfo;

        return fcn;
      }

    return octave_value ();
  }

  octave_value
  symbol_table::fcn_table_find (const std::string& name,
                                const octave_value_list& args, bool local_funcs)
  {
    fcn_table_iterator p = m_fcn_table.find (name);

    if (p != m_fcn_table.end ())
      return p->second.find (args, local_funcs);
    else
      {
        fcn_info finfo (name);

        octave_value fcn = finfo.find (args, local_funcs);

        if (fcn.is_defined ())
          m_fcn_table[name] = finfo;

        return fcn;
      }

    return octave_value ();
  }

  octave_value
  symbol_table::find_function (const std::string& name,
                               const octave_value_list& args, bool local_funcs)
  {
    octave_value retval;

    if (! name.empty () && name[0] == '@')
      {
        // Look for a class specific function.
        std::string dispatch_type =
          name.substr (1, name.find_first_of (sys::file_ops::dir_sep_str ()) - 1);

        std::string method;
        size_t pos = name.find_last_of (sys::file_ops::dir_sep_str ());
        if (pos != std::string::npos)
          method = name.substr (pos + 1);

        retval = find_method (method, dispatch_type);
      }
    else
      {
        size_t pos = name.find_first_of ('>');

        if (pos == std::string::npos)
          retval = find (name, args, true, local_funcs);
        else
          {
            std::string fcn_scope = name.substr (0, pos);
            symbol_scope stored_scope = m_current_scope;
            m_current_scope = m_top_scope;
            octave_value parent = find_function (name.substr (0, pos),
                                                 octave_value_list (), false);

            if (parent.is_defined ())
              {
                octave_function *parent_fcn = parent.function_value ();

                if (parent_fcn)
                  {
                    m_current_scope = parent_fcn->scope ();

                    if (m_current_scope && m_current_scope != m_top_scope)
                      retval = find_function (name.substr (pos + 1), args);
                  }
              }

            m_current_scope = stored_scope;
          }
      }

    return retval;
  }

  // look for @class/method>subfunction
  octave_value
  symbol_table::find_submethod (const std::string& name,
                                const std::string& dispatch_type)
  {
    octave_value fcn;

    std::string full_name = '@' + dispatch_type +
      sys::file_ops::dir_sep_str () + name;
    size_t pos = full_name.find_first_of ('>');

    if (pos != std::string::npos)
      {
        std::string fcn_scope = full_name.substr (0, pos);
        symbol_scope stored_scope = m_current_scope;
        m_current_scope = m_top_scope;
        octave_value parent = find_function (full_name.substr (0, pos),
                                             octave_value_list (), false);
        if (parent.is_defined ())
          {
            octave_function *parent_fcn = parent.function_value ();

            if (parent_fcn)
              {
                m_current_scope = parent_fcn->scope ();

                if (m_current_scope && m_current_scope != m_top_scope)
                  fcn = find_function (full_name.substr (pos + 1),
                                       octave_value_list ());
              }
          }

        m_current_scope = stored_scope;
      }

    return fcn;
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

  octave_value
  symbol_table::dump (void) const
  {
    std::map<std::string, octave_value> m
      = {{ "function_info", dump_fcn_table_map () },
         { "precedence_table", dump_container_map (m_class_precedence_table) },
         { "parent_classes", dump_container_map (m_parent_map) }};

    return octave_value (m);
  }

  void
  symbol_table::cleanup (void)
  {
    clear_all (true);

    m_fcn_table.clear ();
    m_class_precedence_table.clear ();
    m_parent_map.clear ();
  }

  octave_value
  symbol_table::dump_fcn_table_map (void) const
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
}

DEFUN (ignore_function_time_stamp, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} ignore_function_time_stamp ()
@deftypefnx {} {@var{old_val} =} ignore_function_time_stamp (@var{new_val})
Query or set the internal variable that controls whether Octave checks
the time stamp on files each time it looks up functions defined in
function files.

If the internal variable is set to @qcode{"system"}, Octave will not
automatically recompile function files in subdirectories of
@file{@var{octave-home}/lib/@var{version}} if they have changed since they were last compiled, but will recompile other function files in the search path if they change.

If set to @qcode{"all"}, Octave will not recompile any function files
unless their definitions are removed with @code{clear}.

If set to @qcode{"none"}, Octave will always check time stamps on files to
determine whether functions defined in function files need to recompiled.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  octave_value retval;

  if (nargout > 0 || nargin == 0)
    {
      switch (Vignore_function_time_stamp)
        {
        case 1:
          retval = "system";
          break;

        case 2:
          retval = "all";
          break;

        default:
          retval = "none";
          break;
        }
    }

  if (nargin == 1)
    {
      std::string sval = args(0).xstring_value ("ignore_function_time_stamp: first argument must be a string");

      if (sval == "all")
        Vignore_function_time_stamp = 2;
      else if (sval == "system")
        Vignore_function_time_stamp = 1;
      else if (sval == "none")
        Vignore_function_time_stamp = 0;
      else
        error (R"(ignore_function_time_stamp: argument must be one of "all", "system", or "none")");
    }

  return retval;
}

/*
%!shared old_state
%! old_state = ignore_function_time_stamp ();
%!test
%! state = ignore_function_time_stamp ("all");
%! assert (state, old_state);
%! assert (ignore_function_time_stamp (), "all");
%! state = ignore_function_time_stamp ("system");
%! assert (state, "all");
%! assert (ignore_function_time_stamp (), "system");
%! ignore_function_time_stamp (old_state);

## Test input validation
%!error (ignore_function_time_stamp ("all", "all"))
%!error (ignore_function_time_stamp ("UNKNOWN_VALUE"))
%!error (ignore_function_time_stamp (42))
*/

DEFMETHOD (__current_scope__, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {[@var{scope}, @var{context}] =} __current_scope__ ()
Return the current scope and context as integers.
@seealso{__dump_symtab_info__}
@end deftypefn */)
{
  octave::symbol_table& symtab = interp.get_symbol_table ();

  octave::symbol_scope scope = symtab.current_scope ();

  std::string nm = scope ? scope.name () : "<unknown>";

  return ovl (nm, symtab.current_context ());
}

DEFMETHOD (__dump_symtab_info__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} __dump_symtab_info__ ()
@deftypefnx {} {} __dump_symtab_info__ (@var{function})
Undocumented internal function.
@seealso{__current_scope__}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  octave::symbol_table& symtab = interp.get_symbol_table ();

  if (nargin == 0)
    return symtab.dump ();
  else
    {
      std::string fname = args(0).xstring_value ("__dump_symtab_info__: argument must be a function name");

      octave::fcn_info *finfo = symtab.get_fcn_info (fname);

      if (finfo)
        return finfo->dump ();
    }

  return ovl ();
}

DEFMETHOD (__get_cmdline_fcn_txt__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __get_cmdline_fcn_txt__ (@var{name})
Undocumented internal function.
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  std::string name = args(0).xstring_value ("__get_cmd_line_function_text__: first argument must be function name");

  octave::symbol_table& symtab = interp.get_symbol_table ();

  octave_value ov = symtab.find_cmdline_function (name);

  octave_user_function *f = ov.user_function_value ();

  octave_value_list retval;

  if (f)
    {
      std::ostringstream buf;

      octave::tree_print_code tpc (buf);

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
//   octave::symbol_table& symtab = interp.get_symbol_table ();
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
//   octave::symbol_table& symtab = interp.get_symbol_table ();
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
