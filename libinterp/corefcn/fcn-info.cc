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

#include "file-ops.h"

#include "fcn-info.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "load-path.h"
#include "ov-fcn.h"
#include "ov-usr-fcn.h"
#include "parse.h"
#include "symrec.h"
#include "symscope.h"
#include "symtab.h"

namespace octave
{
  octave_value
  fcn_info::fcn_info_rep::load_private_function (const std::string& dir_name)
  {
    octave_value retval;

    load_path& lp
      = __get_load_path__ ("fcn_info::fcn_info_rep::load_private_function");

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

    size_t pos = dir_name.find_last_of (sys::file_ops::dir_sep_chars ());

    if (pos != std::string::npos)
      {
        std::string tmp = dir_name.substr (pos+1);

        if (tmp[0] == '@')
          class_name = tmp.substr (1);
      }

    tmpfcn->mark_as_private_function (class_name);

    private_functions[dir_name] = ov_fcn;

    return ov_fcn;
  }

  octave_value
  fcn_info::fcn_info_rep::load_class_constructor (void)
  {
    octave_value retval;

    std::string dir_name;

    load_path& lp
      = __get_load_path__ ("fcn_info::fcn_info_rep::load_class_constructor");

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
        cdef_manager& cdm
          = __get_cdef_manager__ ("fcn_info::fcn_info_rep::load_class_method");

        octave_function *cm = cdm.find_method_symbol (name, dispatch_type);

        if (cm)
          retval = octave_value (cm);

        if (! retval.is_defined ())
          {
            std::string dir_name;

            load_path& lp = __get_load_path__ ("fcn_info::fcn_info_rep::load_class_method");

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

                symbol_table& symtab
                  = __get_symbol_table__ ("fcn_info::fcn_info_rep::load_class_method");

                const std::list<std::string>& plist =
                  symtab.parent_classes (dispatch_type);

                std::list<std::string>::const_iterator it = plist.begin ();

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

  static builtin_type_t (*build_sup_table (void))[btyp_num_types]
  {
    static builtin_type_t sup_table[btyp_num_types][btyp_num_types];
    for (int i = 0; i < btyp_num_types; i++)
      for (int j = 0; j < btyp_num_types; j++)
        {
          builtin_type_t ityp = static_cast<builtin_type_t> (i);
          builtin_type_t jtyp = static_cast<builtin_type_t> (j);
          // FIXME: Is this really right?
          bool use_j =
            (jtyp == btyp_func_handle || ityp == btyp_bool
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

            symbol_table& symtab = __get_symbol_table__ ("get_dispatch_type");

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
  //   private function
  //   class method
  //   class constructor
  //   command-line function
  //   autoload function
  //   function on the path
  //   built-in function
  //
  // Matlab documentation states that constructors have higher precedence
  // than methods, but that does not seem to be the case.

  octave_value
  fcn_info::fcn_info_rep::find (const octave_value_list& args, bool local_funcs)
  {
    octave_value retval = xfind (args, local_funcs);

    if (retval.is_undefined ())
      {
        // It is possible that the user created a file on the fly since
        // the last prompt or chdir, so try updating the load path and
        // searching again.

        load_path& lp = __get_load_path__ ("fcn_info::fcn_info_rep::find");

        lp.update ();

        retval = xfind (args, local_funcs);
      }

    return retval;
  }

  octave_value
  fcn_info::fcn_info_rep::xfind (const octave_value_list& args,
                                 bool local_funcs)
  {
    if (local_funcs)
      {
        symbol_scope curr_scope
          = __get_current_scope__ ("fcn_info::fcn_info_rep::xfind");

        octave_user_function *current_fcn
          = curr_scope ? curr_scope.function () : nullptr;

        // Local function.

        if (current_fcn)
          {
            std::string fcn_file = current_fcn->fcn_file_name ();

            // For anonymous functions we look at the parent scope so that if
            // they were defined within class methods and use local functions
            // (helper functions) we can still use those anonymous functions

            if (current_fcn->is_anonymous_function ())
              {
                if (fcn_file.empty ()
                    && curr_scope.parent_scope ()
                    && curr_scope.parent_scope ()->function () != nullptr)
                  fcn_file
                    = curr_scope.parent_scope ()->function ()->fcn_file_name();
              }

            if (! fcn_file.empty ())
              {
                str_val_iterator r = local_functions.find (fcn_file);

                if (r != local_functions.end ())
                  {
                    // We shouldn't need an out-of-date check here since
                    // local functions may ultimately be called only from
                    // a primary function or method defined in the same
                    // file.

                    return r->second;
                  }
              }
          }

        // Private function.

        if (current_fcn)
          {
            std::string dir_name = current_fcn->dir_name ();

            if (! dir_name.empty ())
              {
                str_val_iterator q = private_functions.find (dir_name);

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
          }
      }

    // Class methods.

    if (! args.empty ())
      {
        std::string dispatch_type = get_dispatch_type (args);

        octave_value fcn = find_method (dispatch_type);

        if (fcn.is_defined ())
          return fcn;
      }

    // Class constructors.  The class name and function name are the same.

    str_val_iterator q = class_constructors.find (name);

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

    octave_value fcn = find_autoload ();

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
  fcn_info::fcn_info_rep::builtin_find (void)
  {
    octave_value retval = x_builtin_find ();

    if (! retval.is_defined ())
      {
        // It is possible that the user created a file on the fly since
        // the last prompt or chdir, so try updating the load path and
        // searching again.

        load_path& lp = __get_load_path__ ("fcn_info::fcn_info_rep::builtin_find");

        lp.update ();

        retval = x_builtin_find ();
      }

    return retval;
  }

  octave_value
  fcn_info::fcn_info_rep::x_builtin_find (void)
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

    // Private function.

    symbol_scope curr_scope
      = __get_current_scope__ ("fcn_info::fcn_info_rep::x_builtin_find");

    octave_user_function *current_fcn = curr_scope ? curr_scope.function () : nullptr;

    if (current_fcn)
      {
        std::string dir_name = current_fcn->dir_name ();

        if (! dir_name.empty ())
          {
            str_val_iterator q = private_functions.find (dir_name);

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
      }

    // Local function.

    if (current_fcn)
      {
        std::string fcn_file = current_fcn->fcn_file_name ();

        if (! fcn_file.empty ())
          {
            str_val_iterator r = local_functions.find (fcn_file);

            if (r != local_functions.end ())
              {
                // We shouldn't need an out-of-date check here since local
                // functions may ultimately be called only from a primary
                // function or method defined in the same file.

                return r->second;
              }
          }
      }

    // Subfunction.  I think it only makes sense to check for
    // subfunctions if we are currently executing a function defined
    // from a .m file.

    if (curr_scope)
      {
        octave_value val = curr_scope.find_subfunction (name);

        if (val.is_defined ())
          return val;
      }

    return octave_value ();
  }

  octave_value
  fcn_info::fcn_info_rep::find_method (const std::string& dispatch_type)
  {
    octave_value retval;

    str_val_iterator q = class_methods.find (dispatch_type);

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
        std::string file_name = lookup_autoload (name);

        if (! file_name.empty ())
          {
            size_t pos = file_name.find_last_of (sys::file_ops::dir_sep_chars ());

            std::string dir_name = file_name.substr (0, pos);

            octave_value ov_fcn
              = load_fcn_from_file (file_name, dir_name, "", "",
                                            name, true);

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

        load_path& lp = __get_load_path__ ("fcn_info::fcn_info_rep::find_user_function");


        std::string file_name = lp.find_fcn (name, dir_name, package_name);

        if (! file_name.empty ())
          {
            octave_value ov_fcn
              = load_fcn_from_file (file_name, dir_name, "",
                                            package_name);

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
        cdef_manager& cdm
          = __get_cdef_manager__ ("fcn_info::fcn_info_rep::find_package");

        octave_function *fcn = cdm.find_package_symbol (full_name ());

        if (fcn)
          package = octave_value (fcn);
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
         { "built_in_function", built_in_function.dump () }};

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
}
