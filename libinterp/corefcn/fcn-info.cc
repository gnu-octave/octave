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

#include "file-ops.h"
#include "file-stat.h"
#include "oct-env.h"

#include "defun.h"
#include "fcn-info.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "load-path.h"
#include "ov-fcn.h"
#include "ov-usr-fcn.h"
#include "parse.h"
#include "symscope.h"
#include "symtab.h"
#include "utils.h"

// Should Octave always check to see if function files have changed
// since they were last compiled?
static int Vignore_function_time_stamp = 1;

OCTAVE_BEGIN_NAMESPACE(octave)

octave_value
fcn_info::fcn_info_rep::load_private_function (const std::string& dir_name)
{
  octave_value retval;

  load_path& lp = __get_load_path__ ();

  std::string file_name = lp.find_private_fcn (dir_name, name);

  if (file_name.empty ())
    return retval;

  octave_value ov_fcn = load_fcn_from_file (file_name, dir_name);

  if (ov_fcn.is_undefined ())
    return retval;

  octave_function *tmpfcn = ov_fcn.function_value ();

  if (! tmpfcn)
    return retval;

  std::string class_name;

  std::size_t pos = dir_name.find_last_of (sys::file_ops::dir_sep_chars ());

  if (pos != std::string::npos)
    {
      std::string tmp = dir_name.substr (pos+1);

      if (tmp[0] == '@')
        class_name = tmp.substr (1);
    }

  tmpfcn->mark_as_private_function (class_name);

  private_functions[sys::canonicalize_file_name (dir_name)] = ov_fcn;

  return ov_fcn;
}

octave_value
fcn_info::fcn_info_rep::load_class_constructor (void)
{
  octave_value retval;

  std::string dir_name;

  load_path& lp = __get_load_path__ ();

  std::string file_name = lp.find_method (name, name, dir_name, package_name);

  if (! file_name.empty ())
    {
      octave_value ov_fcn
        = load_fcn_from_file (file_name, dir_name, name,
                              package_name);

      if (ov_fcn.is_defined ())
        {
          // Note: ov_fcn may be an octave_classdef_meta object instead
          // of the actual constructor function.

          retval = ov_fcn;

          class_constructors[name] = retval;
          class_methods[name] = retval;
        }
    }
  else
    {
      // Classdef constructors can be defined anywhere in the path, not
      // necessarily in @-folders.  Look for a normal function and load it.
      // If the loaded function is a classdef constructor, store it as such
      // and restore function_on_path to its previous value.

      octave_value old_function_on_path = function_on_path;

      octave_value maybe_cdef_ctor = find_user_function ();

      if (maybe_cdef_ctor.is_defined ())
        {
          octave_function *fcn = maybe_cdef_ctor.function_value (true);

          if (fcn && fcn->is_classdef_constructor ())
            {
              retval = maybe_cdef_ctor;

              class_constructors[name] = retval;
              class_methods[name] = retval;

              function_on_path = old_function_on_path;
            }
        }
    }

  return retval;
}

octave_value
fcn_info::fcn_info_rep::load_class_method (const std::string& dispatch_type)
{
  octave_value retval;

  if (full_name () == dispatch_type)
    retval = load_class_constructor ();
  else
    {
      cdef_manager& cdm = __get_cdef_manager__ ();

      retval = cdm.find_method_symbol (name, dispatch_type);

      if (! retval.is_defined ())
        {
          std::string dir_name;

          load_path& lp = __get_load_path__ ();

          std::string file_name = lp.find_method (dispatch_type, name,
                                                  dir_name);

          if (! file_name.empty ())
            {
              octave_value ov_fcn
                = load_fcn_from_file (file_name, dir_name,
                                      dispatch_type);

              if (ov_fcn.is_defined ())
                {
                  octave_function *tmpfcn = ov_fcn.function_value ();

                  if (tmpfcn && tmpfcn->is_class_method (dispatch_type))
                    {
                      retval = ov_fcn;

                      class_methods[dispatch_type] = retval;
                    }
                }
            }

          if (retval.is_undefined ())
            {
              // Search parent classes

              symbol_table& symtab = __get_symbol_table__ ();

              const std::list<std::string>& plist
                = symtab.parent_classes (dispatch_type);

              auto it = plist.begin ();

              while (it != plist.end ())
                {
                  retval = find_method (*it);

                  if (retval.is_defined ())
                    {
                      class_methods[dispatch_type] = retval;
                      break;
                    }

                  it++;
                }
            }

          if (retval.is_undefined ())
            {
              // Search for built-in functions that are declared to
              // handle specific types.

              if (built_in_function.is_defined ())
                {
                  octave_function *fcn = built_in_function.function_value ();

                  if (fcn && fcn->handles_dispatch_class (dispatch_type))
                    {
                      retval = built_in_function;

                      class_methods[dispatch_type] = retval;
                    }
                }
            }
        }
    }

  return retval;
}

// :-) JWE, can you parse this? Returns a 2D array with second dimension equal
// to btyp_num_types (static constant).  Only the leftmost dimension can be
// variable in C/C++.  Typedefs are boring.

static builtin_type_t (* build_sup_table (void))[btyp_num_types]
{
  static builtin_type_t sup_table[btyp_num_types][btyp_num_types];
  for (int i = 0; i < btyp_num_types; i++)
    for (int j = 0; j < btyp_num_types; j++)
      {
        builtin_type_t ityp = static_cast<builtin_type_t> (i);
        builtin_type_t jtyp = static_cast<builtin_type_t> (j);
        // FIXME: Is this really right?
        bool use_j
        = (jtyp == btyp_func_handle || ityp == btyp_bool
           || (btyp_isarray (ityp)
               && (! btyp_isarray (jtyp)
                   || (btyp_isinteger (jtyp) && ! btyp_isinteger (ityp))
                   || ((ityp == btyp_double || ityp == btyp_complex
                        || ityp == btyp_char)
                       && (jtyp == btyp_float
                           || jtyp == btyp_float_complex)))));

        sup_table[i][j] = (use_j ? jtyp : ityp);
      }

  return sup_table;
}

std::string
get_dispatch_type (const octave_value_list& args,
                   builtin_type_t& builtin_type)
{
  static builtin_type_t (*sup_table)[btyp_num_types] = build_sup_table ();
  std::string dispatch_type;

  int n = args.length ();

  if (n > 0)
    {
      int i = 0;
      builtin_type = args(0).builtin_type ();
      if (builtin_type != btyp_unknown)
        {
          for (i = 1; i < n; i++)
            {
              builtin_type_t bti = args(i).builtin_type ();
              if (bti != btyp_unknown)
                builtin_type = sup_table[builtin_type][bti];
              else
                {
                  builtin_type = btyp_unknown;
                  break;
                }
            }
        }

      if (builtin_type == btyp_unknown)
        {
          // There's a non-builtin class in the argument list.
          dispatch_type = args(i).class_name ();

          symbol_table& symtab = __get_symbol_table__ ();

          for (int j = i+1; j < n; j++)
            {
              octave_value arg = args(j);

              if (arg.builtin_type () == btyp_unknown)
                {
                  std::string cname = arg.class_name ();

                  // Only switch to type of ARG if it is marked superior
                  // to the current DISPATCH_TYPE.
                  if (! symtab.is_superiorto (dispatch_type, cname)
                      && symtab.is_superiorto (cname, dispatch_type))
                    dispatch_type = cname;
                }
            }
        }
      else
        dispatch_type = btyp_class_name[builtin_type];
    }
  else
    builtin_type = btyp_unknown;

  return dispatch_type;
}

std::string
get_dispatch_type (const octave_value_list& args)
{
  builtin_type_t builtin_type;
  return get_dispatch_type (args, builtin_type);
}

// Find function definition according to the following precedence list:
//
//   nested functions (and subfunctions)
//   local functions in the current file
//   private function
//   class method
//   class constructor
//   command-line function
//   autoload function
//   functions on the load_path (current directory is always first)
//   package (FIXME: does this belong here?)
//   built-in function

octave_value
fcn_info::fcn_info_rep::find (const symbol_scope& scope,
                              const octave_value_list& args)
{
  symbol_scope search_scope
    = (scope
       ? scope : __get_current_scope__ ());

  octave_value retval = xfind (search_scope, args);

  if (retval.is_undefined ())
    {
      // It is possible that the user created a file on the fly since
      // the last prompt or chdir, so try updating the load path and
      // searching again.

      load_path& lp = __get_load_path__ ();

      lp.update ();

      retval = xfind (search_scope, args);
    }

  return retval;
}


static void
split_name_with_package (const std::string& name, std::string& fname,
                         std::string& pname)
{
  std::size_t pos = name.rfind ('.');

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
load_out_of_date_fcn (const std::string& file_name,
                      const std::string& dir_name_arg,
                      octave_value& function,
                      const std::string& dispatch_type = "",
                      const std::string& package_name = "")
{
  bool retval = false;

  std::string dir_name = dir_name_arg;

  if (dir_name.empty ())
    {
      std::size_t pos = file_name.find_last_of (sys::file_ops::dir_sep_chars ());

      dir_name = file_name.substr (0, pos);
    }

  // FIXME: do the following job of determining private status and
  // class membership in a separate function?

  std::size_t pos = dir_name.find_last_of (sys::file_ops::dir_sep_chars ());

  bool is_private_fcn
    = pos != std::string::npos && dir_name.substr (pos+1) == "private";

  if (is_private_fcn)
    dir_name = dir_name.substr (0, pos);

  std::string class_name;

  pos = dir_name.find_last_of (sys::file_ops::dir_sep_chars ());

  if (pos != std::string::npos)
    {
      std::string tmp = dir_name.substr (pos+1);

      if (tmp[0] == '@')
        class_name = tmp.substr (1);
    }

  octave_value ov_fcn
    = load_fcn_from_file (file_name, dir_name, dispatch_type,
                          package_name);

  if (ov_fcn.is_defined ())
    {
      retval = true;

      octave_function *fcn = ov_fcn.function_value ();

      if (is_private_fcn)
        fcn->mark_as_private_function (class_name);

      function = ov_fcn;
    }
  else
    function = octave_value ();

  return retval;
}

static bool
out_of_date_check (octave_value& function,
                   const std::string& dispatch_type = "",
                   bool check_relative = true)
{
  bool retval = false;

  octave_function *fcn = function.function_value (true);

  if (fcn)
    {
      // FIXME: we need to handle subfunctions properly here.

      if (! (fcn->is_subfunction () || fcn->is_anonymous_function ()))
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
                              load_path& lp = __get_load_path__ ();

                              file = lp.find_method (dispatch_type, nm,
                                                     dir_name, pack);

                              if (file.empty ())
                                {
                                  std::string s_name;
                                  std::string s_pack;

                                  symbol_table& symtab = __get_symbol_table__ ();

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
                            {
                              tree_evaluator& tw = __get_evaluator__ ();

                              file = tw.lookup_autoload (nm);
                            }

                          if (file.empty ())
                            {
                              load_path& lp = __get_load_path__ ();
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
                      OCTAVE_TIME_T tp = ottp.unix_time ();

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
                      bp_table& bptab = __get_bp_table__ ();

                      bptab.remove_all_breakpoints_from_function (canonical_nm,
                          true);
                    }
                }
            }
        }
    }

  return retval;
}

octave_value
fcn_info::fcn_info_rep::find_scoped_function (const symbol_scope& search_scope)
{
  if (search_scope)
    {
      // Subfunction.

      octave_value fcn = search_scope.find_subfunction (name);

      if (fcn.is_defined ())
        return fcn;

      // Local function.

      std::string fcn_file = search_scope.fcn_file_name ();

      // For anonymous functions we look at the parent scope so that if
      // they were defined within class methods and use local functions
      // (helper functions) we can still use those anonymous functions

      if (! fcn_file.empty ())
        {
          auto r = local_functions.find (fcn_file);

          if (r != local_functions.end ())
            {
              // We shouldn't need an out-of-date check here since
              // local functions may ultimately be called only from
              // a primary function or method defined in the same
              // file.

              return r->second;
            }
        }

      // Private function.

      return find_private_function (search_scope.dir_name ());
    }

  return octave_value ();
}

octave_value
fcn_info::fcn_info_rep::find_private_function (const std::string& dir_name)
{
  if (! dir_name.empty ())
    {
      auto q = private_functions.find (dir_name);

      if (q == private_functions.end ())
        {
          octave_value val = load_private_function (dir_name);

          if (val.is_defined ())
            return val;
        }
      else
        {
          octave_value& fval = q->second;

          if (fval.is_defined ())
            out_of_date_check (fval, "", false);

          if (fval.is_defined ())
            return fval;
          else
            {
              octave_value val = load_private_function (dir_name);

              if (val.is_defined ())
                return val;
            }
        }
    }

  return octave_value ();
}

octave_value
fcn_info::fcn_info_rep::find_method (const octave_value_list& args)
{
  if (! args.empty ())
    {
      std::string dispatch_type = get_dispatch_type (args);

      return find_method (dispatch_type);
    }

  return octave_value ();
}

octave_value
fcn_info::fcn_info_rep::xfind (const symbol_scope& search_scope,
                               const octave_value_list& args)
{
  // Subfunction, local function, or private function.

  octave_value fcn;

  fcn = find_scoped_function (search_scope);

  if (fcn.is_defined ())
    return fcn;

  // Class methods.

  fcn = find_method (args);

  if (fcn.is_defined ())
    return fcn;

  // Class constructors.  The class name and function name are the same.

  auto q = class_constructors.find (name);

  if (q == class_constructors.end ())
    {
      octave_value val = load_class_constructor ();

      if (val.is_defined ())
        return val;
    }
  else
    {
      octave_value& fval = q->second;

      if (fval.is_defined ())
        out_of_date_check (fval, name);

      if (fval.is_defined ())
        return fval;
      else
        {
          octave_value val = load_class_constructor ();

          if (val.is_defined ())
            return val;
        }
    }

  // Command-line function.

  if (cmdline_function.is_defined ())
    return cmdline_function;

  // Autoload?

  fcn = find_autoload ();

  if (fcn.is_defined ())
    return fcn;

  // Function on the path.

  fcn = find_user_function ();

  if (fcn.is_defined ())
    return fcn;

  // Package

  fcn = find_package ();

  if (fcn.is_defined ())
    return fcn;

  // Built-in function (might be undefined).

  return built_in_function;
}

// Find the definition of NAME according to the following precedence
// list:
//
//   built-in function
//   function on the path
//   autoload function
//   command-line function
//   private function
//   subfunction

// This function is used to implement the "builtin" function, which
// searches for "built-in" functions.  In Matlab, "builtin" only
// returns functions that are actually built-in to the interpreter.
// But since the list of built-in functions is different in Octave and
// Matlab, we also search up the precedence list until we find
// something that matches.  Note that we are only searching by name,
// so class methods and constructors are skipped.

octave_value
fcn_info::fcn_info_rep::builtin_find (const symbol_scope& scope)
{
  symbol_scope search_scope
    = (scope
       ? scope : __get_current_scope__ ());

  octave_value retval = x_builtin_find (search_scope);

  if (! retval.is_defined ())
    {
      // It is possible that the user created a file on the fly since
      // the last prompt or chdir, so try updating the load path and
      // searching again.

      load_path& lp = __get_load_path__ ();

      lp.update ();

      retval = x_builtin_find (search_scope);
    }

  return retval;
}

octave_value
fcn_info::fcn_info_rep::x_builtin_find (const symbol_scope& search_scope)
{
  // Built-in function.
  if (built_in_function.is_defined ())
    return built_in_function;

  // Function on the path.

  octave_value fcn = find_user_function ();

  if (fcn.is_defined ())
    return fcn;

  // Autoload?

  fcn = find_autoload ();

  if (fcn.is_defined ())
    return fcn;

  // Command-line function.

  if (cmdline_function.is_defined ())
    return cmdline_function;

  // Private function, local function, or subfunction.

  if (search_scope)
    {
      // Private function.

      std::string dir_name = search_scope.dir_name ();

      if (! dir_name.empty ())
        {
          auto q = private_functions.find (dir_name);

          if (q == private_functions.end ())
            {
              octave_value val = load_private_function (dir_name);

              if (val.is_defined ())
                return val;
            }
          else
            {
              octave_value& fval = q->second;

              if (fval.is_defined ())
                out_of_date_check (fval);

              if (fval.is_defined ())
                return fval;
              else
                {
                  octave_value val = load_private_function (dir_name);

                  if (val.is_defined ())
                    return val;
                }
            }
        }

      // Local function.

      std::string fcn_file = search_scope.fcn_file_name ();

      if (! fcn_file.empty ())
        {
          auto r = local_functions.find (fcn_file);

          if (r != local_functions.end ())
            {
              // We shouldn't need an out-of-date check here since local
              // functions may ultimately be called only from a primary
              // function or method defined in the same file.

              return r->second;
            }
        }

      // Subfunction.  I think it only makes sense to check for
      // subfunctions if we are currently executing a function defined
      // from a .m file.

      octave_value val = search_scope.find_subfunction (name);

      if (val.is_defined ())
        return val;
    }

  return octave_value ();
}

octave_value
fcn_info::fcn_info_rep::find_method (const std::string& dispatch_type)
{
  octave_value retval;

  auto q = class_methods.find (dispatch_type);

  if (q == class_methods.end ())
    {
      octave_value val = load_class_method (dispatch_type);

      if (val.is_defined ())
        return val;
    }
  else
    {
      octave_value& fval = q->second;

      if (fval.is_defined ())
        out_of_date_check (fval, dispatch_type);

      if (fval.is_defined ())
        return fval;
      else
        {
          octave_value val = load_class_method (dispatch_type);

          if (val.is_defined ())
            return val;
        }
    }

  return retval;
}

octave_value
fcn_info::fcn_info_rep::find_autoload (void)
{
  // Autoloaded function.

  if (autoload_function.is_defined ())
    out_of_date_check (autoload_function);

  if (! autoload_function.is_defined ())
    {
      tree_evaluator& tw = __get_evaluator__ ();

      std::string file_name = tw.lookup_autoload (name);

      if (! file_name.empty ())
        {
          std::size_t pos = file_name.find_last_of (sys::file_ops::dir_sep_chars ());

          std::string dir_name = file_name.substr (0, pos);

          octave_value ov_fcn
            = load_fcn_from_file (file_name, dir_name, "", "", name, true);

          if (ov_fcn.is_defined ())
            autoload_function = octave_value (ov_fcn);
        }
    }

  return autoload_function;
}

octave_value
fcn_info::fcn_info_rep::find_user_function (void)
{
  // Function on the path.

  if (function_on_path.is_defined ())
    out_of_date_check (function_on_path);

  if (function_on_path.is_undefined ())
    {
      std::string dir_name;

      load_path& lp = __get_load_path__ ();


      std::string file_name = lp.find_fcn (name, dir_name, package_name);

      if (! file_name.empty ())
        {
          octave_value ov_fcn
            = load_fcn_from_file (file_name, dir_name, "", package_name);

          if (ov_fcn.is_defined ())
            function_on_path = ov_fcn;
        }
    }

  return function_on_path;
}

octave_value
fcn_info::fcn_info_rep::find_package (void)
{
  // FIXME: implement correct way to check out of date package
  //if (package.is_defined ())
  //  out_of_date_check (package);

  if (package.is_undefined ())
    {
      cdef_manager& cdm = __get_cdef_manager__ ();

      package = cdm.find_package_symbol (full_name ());
    }

  return package;
}

void
fcn_info::fcn_info_rep::install_built_in_dispatch (const std::string& klass)
{
  if (built_in_function.is_defined ())
    {
      octave_function *fcn = built_in_function.function_value ();

      if (fcn)
        {
          if (fcn->handles_dispatch_class (klass))
            warning ("install_built_in_dispatch: '%s' already defined for class '%s'",
                     name.c_str (), klass.c_str ());
          else
            fcn->push_dispatch_class (klass);
        }
    }
  else
    error ("install_built_in_dispatch: '%s' is not a built-in function",
           name.c_str ());
}

octave_value
fcn_info::fcn_info_rep::dump (void) const
{
  std::map<std::string, octave_value> m
  = {{ "name", full_name () },
    { "package", package.dump () },
    { "local_functions", dump_function_map (local_functions) },
    { "private_functions", dump_function_map (private_functions) },
    { "class_methods", dump_function_map (class_methods) },
    { "class_constructors", dump_function_map (class_constructors) },
    { "cmdline_function", cmdline_function.dump () },
    { "autoload_function", autoload_function.dump () },
    { "function_on_path", function_on_path.dump () },
    { "built_in_function", built_in_function.dump () }
  };

  return octave_value (m);
}

octave_value
dump_function_map (const std::map<std::string, octave_value>& fcn_map)
{
  if (fcn_map.empty ())
    return octave_value (Matrix ());

  std::map<std::string, octave_value> info_map;

  for (const auto& nm_fcn : fcn_map)
    {
      std::string nm = nm_fcn.first;
      const octave_value& fcn = nm_fcn.second;
      info_map[nm] = fcn.dump ();
    }

  return octave_value (info_map);
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
@file{@var{octave-home}/share/@var{version}/m} if they have changed since
they were last compiled, but will recompile other function files in the
search path if they change.

If set to @qcode{"all"}, Octave will not recompile any function files
unless their definitions are removed with @code{clear}.

If set to @qcode{"none"}, Octave will always check time stamps on files
to determine whether functions defined in function files need to
recompiled.
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
      std::string sval = args(
                           0).xstring_value ("ignore_function_time_stamp: first argument must be a string");

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
%!error ignore_function_time_stamp ("all", "all")
%!error ignore_function_time_stamp ("UNKNOWN_VALUE")
%!error ignore_function_time_stamp (42)
*/

OCTAVE_END_NAMESPACE(octave)
