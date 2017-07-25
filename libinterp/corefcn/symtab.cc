/*

Copyright (C) 1993-2017 John W. Eaton
Copyright (C) 2009 VZLU Prague, a.s.

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

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
#include "input.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "load-path.h"
#include "ov-classdef.h"
#include "ov-fcn.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "parse.h"
#include "pt-pr-code.h"
#include "symtab.h"
#include "unwind-prot.h"
#include "utils.h"

// Should Octave always check to see if function files have changed
// since they were last compiled?
static int Vignore_function_time_stamp = 1;

namespace octave
{
  octave_value symbol_table::dummy_octave_value;

  void
  symbol_table::symbol_record::symbol_record_rep::clear (scope *sid)
  {
    if (! (is_hidden () || is_inherited ())
        && sid == decl_scope ())
      {
        if (is_global ())
          unmark_global ();

        if (is_persistent ())
          {
            sid->persistent_assign (name, varval ());

            unmark_persistent ();
          }

        assign (octave_value ());
      }
  }

  void
  symbol_table::symbol_record::symbol_record_rep::init_persistent (void)
  {
    symbol_table::scope *scope
      = __require_current_scope__ ("symbol_table::symbol_record::symbol_record_rep::init_persistent");

    if (! is_defined ())
      {
        mark_persistent ();

        assign (scope->persistent_varval (name));
      }
    // FIXME: this causes trouble with recursive calls.
    // else
    //   error ("unable to declare existing variable persistent");
  }

  void
  symbol_table::symbol_record::symbol_record_rep::erase_persistent (void)
  {
    unmark_persistent ();

    symbol_table::scope *scope
      = __require_current_scope__ ("symbol_table::symbol_record::symbol_record_rep::erase_persistent");

    scope->erase_persistent (name);
  }

  symbol_table::symbol_record::symbol_record_rep *
  symbol_table::symbol_record::symbol_record_rep::dup (scope *new_scope) const
  {
    return new symbol_record_rep (new_scope, name, varval (), storage_class);
  }

  octave_value
  symbol_table::symbol_record::symbol_record_rep::dump (void) const
  {
    std::map<std::string, octave_value> m
      = {{ "name", name },
         { "local", is_local () },
         { "automatic", is_automatic () },
         { "formal", is_formal () },
         { "hidden", is_hidden () },
         { "inherited", is_inherited () },
         { "global", is_global () },
         { "persistent", is_persistent () }};

    octave_value val = varval ();

    if (val.is_defined ())
      m["value"] = val;

    return octave_value (m);
  }

  octave_value&
  symbol_table::symbol_record::symbol_record_rep::xglobal_varref (void)
  {
    symbol_table& symtab
      = __get_symbol_table__ ("symbol_table::symbol_record::symbol_record_rep::xglobal_varref");

    symbol_table::global_symbols_iterator p
      = symtab.m_global_symbols.find (name);

    return (p == symtab.m_global_symbols.end ()
            ? symtab.m_global_symbols[name] : p->second);
  }

  octave_value&
  symbol_table::symbol_record::symbol_record_rep::xpersistent_varref (void)
  {
    symbol_table::scope *scope
      = __get_current_scope__ ("symbol_table::symbol_record::symbol_record_rep::xpersistent_varref");

    return scope ? scope->persistent_varref (name) : dummy_octave_value;
  }

  octave_value
  symbol_table::symbol_record::symbol_record_rep::xglobal_varval (void) const
  {
    symbol_table& symtab
      = __get_symbol_table__ ("symbol_table::symbol_record::symbol_record_rep::xglobal_varval");

    return symtab.global_varval (name);
  }

  octave_value
  symbol_table::symbol_record::symbol_record_rep::xpersistent_varval (void) const
  {
    symbol_table::scope *scope
      = __get_current_scope__ ("symbol_table::symbol_record::symbol_record_rep::xpersistent_varval");

    return scope ? scope->persistent_varval (name) : octave_value ();
  }

  symbol_table::symbol_record::symbol_record (void)
    : rep (new symbol_record_rep (__get_current_scope__ ("symbol_record"),
                                  "", octave_value (), local))

  { }

  octave_value
  symbol_table::symbol_record::find (const octave_value_list& args) const
  {
    octave_value retval;

    symbol_table& symtab
      = __get_symbol_table__ ("symbol_table::symbol_record::find");

    if (is_global ())
      retval = symtab.global_varval (name ());
    else
      {
        retval = varval ();

        if (retval.is_undefined ())
          {
#if 0
            // Use cached fcn_info pointer if possible.
            if (rep->finfo)
              retval = rep->finfo->find (args);
            else
#endif
              {
                retval = symtab.find_function (name (), args);

                if (retval.is_defined ())
                  return retval;
#if 0
                {
                  rep->finfo = symtab.get_fcn_info (name ());
                }
#endif
              }
          }
      }

    return retval;
  }

  symbol_table::symbol_record
  symbol_table::dummy_symbol_record (static_cast<symbol_table::scope*> (nullptr));

  symbol_table::symbol_reference::symbol_reference (const symbol_record& record)
    : m_scope (nullptr), m_context (0), m_sym (record)
  {
    m_scope = __get_current_scope__ ("symbol_reference");
  }

  void
  symbol_table::symbol_reference::update (void) const
  {
    symbol_table::scope *curr_scope
      = __get_current_scope__ ("symbol_reference::update");

    if (curr_scope && (m_scope != curr_scope || ! m_sym.is_valid ()))
      {
        m_scope = curr_scope;
        m_sym = m_scope->insert (m_sym.name ());  // ???
      }

    m_context = m_scope ? m_scope->current_context () : 0;
  }
}

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

namespace octave
{
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
                      bp_table::remove_all_breakpoints_in_file (canonical_nm,
                                                                true);
                  }
              }
          }
      }

    return retval;
  }

  octave_value
  symbol_table::fcn_info::fcn_info_rep::load_private_function
  (const std::string& dir_name)
  {
    octave_value retval;

    load_path& lp
      = __get_load_path__ ("symbol_table::fcn_info::fcn_info_rep::load_private_function");

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
  symbol_table::fcn_info::fcn_info_rep::load_class_constructor (void)
  {
    octave_value retval;

    std::string dir_name;

    load_path& lp
      = __get_load_path__ ("symbol_table::fcn_info::fcn_info_rep::load_class_constructor");

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
  symbol_table::fcn_info::fcn_info_rep::load_class_method
  (const std::string& dispatch_type)
  {
    octave_value retval;

    if (full_name () == dispatch_type)
      retval = load_class_constructor ();
    else
      {
        cdef_manager& cdm
          = __get_cdef_manager__ ("symbol_table::fcn_info::fcn_info_rep::load_class_method");

        octave_function *cm = cdm.find_method_symbol (name, dispatch_type);

        if (cm)
          retval = octave_value (cm);

        if (! retval.is_defined ())
          {
            std::string dir_name;

            load_path& lp = __get_load_path__ ("symbol_table::fcn_info::fcn_info_rep::load_class_method");

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
                  = __get_symbol_table__ ("symbol_table::fcn_info::fcn_info_rep::load_class_method");

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

namespace octave
{
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
  symbol_table::fcn_info::fcn_info_rep::find (const octave_value_list& args,
                                              bool local_funcs)
  {
    octave_value retval = xfind (args, local_funcs);

    if (retval.is_undefined ())
      {
        // It is possible that the user created a file on the fly since
        // the last prompt or chdir, so try updating the load path and
        // searching again.

        load_path& lp = __get_load_path__ ("symbol_table::fcn_info::fcn_info_rep::find");

        lp.update ();

        retval = xfind (args, local_funcs);
      }

    return retval;
  }

  octave_value
  symbol_table::fcn_info::fcn_info_rep::xfind (const octave_value_list& args,
                                               bool local_funcs)
  {
    if (local_funcs)
      {
        symbol_table::scope *scope
          = __get_current_scope__ ("symbol_table::fcn_info::fcn_info_rep::xfind");

        octave_user_function *current_fcn = scope ? scope->function () : nullptr;

        // Local function.

        if (current_fcn)
          {
            std::string fcn_file = current_fcn->fcn_file_name ();

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
  symbol_table::fcn_info::fcn_info_rep::builtin_find (void)
  {
    octave_value retval = x_builtin_find ();

    if (! retval.is_defined ())
      {
        // It is possible that the user created a file on the fly since
        // the last prompt or chdir, so try updating the load path and
        // searching again.

        load_path& lp = __get_load_path__ ("symbol_table::fcn_info::fcn_info_rep::builtin_find");

        lp.update ();

        retval = x_builtin_find ();
      }

    return retval;
  }

  octave_value
  symbol_table::fcn_info::fcn_info_rep::x_builtin_find (void)
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

    symbol_table::scope *scope
      = __get_current_scope__ ("symbol_table::fcn_info::fcn_info_rep::x_builtin_find");

    octave_user_function *current_fcn = scope ? scope->function () : nullptr;

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

    symbol_table::scope *curr_scope
      = __get_current_scope__ ("symbol_table::fcn_info::fcn_info_rep::x_builtin_find");

    if (curr_scope)
      {
        octave_value val = curr_scope->find_subfunction (name);

        if (val.is_defined ())
          return val;
      }

    return octave_value ();
  }

  octave_value
  symbol_table::fcn_info::fcn_info_rep::find_method
  (const std::string& dispatch_type)
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
  symbol_table::fcn_info::fcn_info_rep::find_autoload (void)
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
  symbol_table::fcn_info::fcn_info_rep::find_user_function (void)
  {
    // Function on the path.

    if (function_on_path.is_defined ())
      out_of_date_check (function_on_path);

    if (function_on_path.is_undefined ())
      {
        std::string dir_name;

        load_path& lp = __get_load_path__ ("symbol_table::fcn_info::fcn_info_rep::find_user_function");


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
  symbol_table::fcn_info::fcn_info_rep::find_package (void)
  {
    // FIXME: implement correct way to check out of date package
    //if (package.is_defined ())
    //  out_of_date_check (package);

    if (package.is_undefined ())
      {
        cdef_manager& cdm
          = __get_cdef_manager__ ("symbol_table::fcn_info::fcn_info_rep::find_package");

        octave_function *fcn = cdm.find_package_symbol (full_name ());

        if (fcn)
          package = octave_value (fcn);
      }

    return package;
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

  void
  symbol_table::fcn_info::fcn_info_rep::install_built_in_dispatch (const std::string& klass)
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
}

static octave_value
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

namespace octave
{
  octave_value
  symbol_table::fcn_info::fcn_info_rep::dump (void) const
  {
    std::map<std::string, octave_value> m
      = {{ "name", full_name () },
         { "refcount", count.value () },
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
  symbol_table::find (const std::string& name, const octave_value_list& args,
                      bool skip_variables, bool local_funcs)
  {
    return (m_current_scope
            ? m_current_scope->find (name, args, skip_variables, local_funcs)
            : octave_value ());
  }

  octave_value
  symbol_table::builtin_find (const std::string& name)
  {
    return (m_current_scope
            ? m_current_scope->builtin_find (name) : octave_value ());
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
        size_t pos = name.find_first_of (Vfilemarker);

        if (pos == std::string::npos)
          retval = find (name, args, true, local_funcs);
        else
          {
            std::string fcn_scope = name.substr (0, pos);
            scope *stored_scope = m_current_scope;
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

    std::string full_name = "@" + dispatch_type +
      sys::file_ops::dir_sep_str () + name;
    size_t pos = full_name.find_first_of (Vfilemarker);

    if (pos != std::string::npos)
      {
        std::string fcn_scope = full_name.substr (0, pos);
        scope *stored_scope = m_current_scope;
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

namespace octave
{
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

    m_global_symbols.clear ();
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

  octave_value
  symbol_table::scope::find (const std::string& name,
                             const octave_value_list& args,
                             bool skip_variables, bool local_funcs)
  {
    octave_value retval;

    // Variable.

    symbol_table& symtab
      = __get_symbol_table__ ("symbol_table::scope::find");

    if (! skip_variables)
      {
        table_iterator p = m_symbols.find (name);

        if (p != m_symbols.end ())
          {
            symbol_record sr = p->second;

            if (sr.is_global ())
              return symtab.global_varval (name);
            else
              {
                octave_value val = sr.varval ();

                if (val.is_defined ())
                  return val;
              }
          }
      }

    if (local_funcs)
      {
        // Subfunction.  I think it only makes sense to check for
        // subfunctions if we are currently executing a function defined
        // from a .m file.

        octave_value fcn = find_subfunction (name);

        if (fcn.is_defined ())
          return fcn;
      }

    fcn_table_iterator p = symtab.m_fcn_table.find (name);

    if (p != symtab.m_fcn_table.end ())
      return p->second.find (args, local_funcs);
    else
      {
        fcn_info finfo (name);

        octave_value fcn = finfo.find (args, local_funcs);

        if (fcn.is_defined ())
          symtab.m_fcn_table[name] = finfo;

        return fcn;
      }

    return retval;
  }

  octave_value
  symbol_table::scope::builtin_find (const std::string& name)
  {
    octave_value retval;

    symbol_table& symtab
      = __get_symbol_table__ ("symbol_table::scope::find");

    fcn_table_iterator p = symtab.m_fcn_table.find (name);

    if (p != symtab.m_fcn_table.end ())
      return p->second.builtin_find ();
    else
      {
        fcn_info finfo (name);

        octave_value fcn = finfo.builtin_find ();

        if (fcn.is_defined ())
          symtab.m_fcn_table[name] = finfo;

        return fcn;
      }

    return retval;
  }

  symbol_table::symbol_record&
  symbol_table::scope::insert (const std::string& name, bool force_add)
  {
    table_iterator p = m_symbols.find (name);

    if (p == m_symbols.end ())
      {
        symbol_table::symbol_record ret (this, name);

        if (m_is_nested && m_parent && m_parent->look_nonlocal (name, ret))
          return m_symbols[name] = ret;
        else
          {
            if (m_is_static && ! force_add)
              ret.mark_added_static ();

            return m_symbols[name] = ret;
          }
      }
    else
      return p->second;
  }

  void
  symbol_table::scope::clear_global (const std::string& name)
  {
    table_iterator p = m_symbols.find (name);

    if (p != m_symbols.end ())
      {
        symbol_table::symbol_record& sr = p->second;

        if (sr.is_global ())
          sr.unmark_global ();
      }

    symbol_table& symtab
      = __get_symbol_table__ ("symbol_table::scope::clear_global");

    global_symbols_iterator q = symtab.m_global_symbols.find (name);

    if (q != symtab.m_global_symbols.end ())
      symtab.m_global_symbols.erase (q);

  }

  void
  symbol_table::scope::clear_global_pattern (const std::string& pat)
  {
    glob_match pattern (pat);

    for (auto& nm_sr : m_symbols)
      {
        symbol_table::symbol_record& sr = nm_sr.second;

        if (sr.is_global () && pattern.match (sr.name ()))
          sr.unmark_global ();
      }

    symbol_table& symtab
      = __get_symbol_table__ ("symbol_table::scope::clear_global_pattern");

    global_symbols_iterator q = symtab.m_global_symbols.begin ();

    while (q != symtab.m_global_symbols.end ())
      {
        if (pattern.match (q->first))
          symtab.m_global_symbols.erase (q++);
        else
          q++;
      }
  }

  std::list<workspace_element>
  symbol_table::scope::workspace_info (void) const
  {
    std::list<workspace_element> retval;

    for (const auto& nm_sr : m_symbols)
      {
        std::string nm = nm_sr.first;
        symbol_record sr = nm_sr.second;

        if (! sr.is_hidden ())
          {
            octave_value val = sr.varval ();

            if (val.is_defined ())
              {
                // FIXME: fix size for objects, see kluge in variables.cc
                //dim_vector dv = val.dims ();
                octave_value tmp = val;
                Matrix sz = tmp.size ();
                dim_vector dv = dim_vector::alloc (sz.numel ());
                for (octave_idx_type i = 0; i < dv.ndims (); i++)
                  dv(i) = sz(i);

                char storage = ' ';
                if (sr.is_global ())
                  storage = 'g';
                else if (sr.is_persistent ())
                  storage = 'p';
                else if (sr.is_automatic ())
                  storage = 'a';
                else if (sr.is_formal ())
                  storage = 'f';
                else if (sr.is_hidden ())
                  storage = 'h';
                else if (sr.is_inherited ())
                  storage = 'i';

                std::ostringstream buf;
                val.short_disp (buf);
                std::string short_disp_str = buf.str ();

                workspace_element elt (storage, nm, val.class_name (),
                                       short_disp_str, dv.str (),
                                       val.iscomplex ());

                retval.push_back (elt);
              }
          }
      }

    return retval;
  }

  octave_value
  symbol_table::scope::dump (void) const
  {
    std::map<std::string, octave_value> m
      = {{ "name", m_name },
         { "symbols", dump_symbols_map () },
         { "persistent_variables", m_persistent_symbols },
         { "subfunctions", dump_function_map (m_subfunctions) }};

    return octave_value (m);
  }

  octave_value
  symbol_table::scope::dump_symbols_map (void) const
  {
    std::map<std::string, octave_value> info_map;

    for (const auto& nm_sr : m_symbols)
      {
        std::string nm = nm_sr.first;
        const symbol_table::symbol_record& sr = nm_sr.second;
        info_map[nm] = sr.dump ();
      }

    return octave_value (info_map);
  }

  void
  symbol_table::scope::install_subfunction (const std::string& name,
                                            const octave_value& fval,
                                            bool is_nested)
  {
    m_subfunctions[name] = fval;

    // This can be simpler once the scope object is stored in the function
    // object...
    octave_user_function *fcn = fval.user_function_value ();

    scope *fcn_scope = fcn->scope ();

    fcn_scope->set_parent (this);

    if (is_nested)
      {
        m_children.push_back (fcn_scope);

        fcn->mark_as_nested_function ();

        fcn_scope->m_is_nested = true;
      }

  }

  octave_value
  symbol_table::scope::find_subfunction (const std::string& name) const
  {
    subfunctions_const_iterator p = m_subfunctions.find (name);

    if (p != m_subfunctions.end ())
      return p->second;

    if (m_parent)
      return m_parent->find_subfunction (name);

    return octave_value ();
  }

  void
  symbol_table::scope::mark_subfunctions_in_scope_as_private (const std::string& class_name)
  {
    for (auto& nm_sf : m_subfunctions)
      {
        octave_function *fcn = nm_sf.second.function_value ();

        if (fcn)
          fcn->mark_as_private_function (class_name);
      }
  }

  void
  symbol_table::scope::update_nest (void)
  {
    if (m_parent)
      {
        // fix bad symbol_records
        for (auto& nm_sr : m_symbols)
          {
            symbol_record& ours = nm_sr.second;
            symbol_record parents;

            if (! ours.is_formal ()
                && m_is_nested && m_parent->look_nonlocal (nm_sr.first, parents))
              {
                if (ours.is_global () || ours.is_persistent ())
                  error ("global and persistent may only be used in the topmost level in which a nested variable is used");

                if (! ours.is_formal ())
                  {
                    ours.invalidate ();
                    nm_sr.second = parents;
                  }
              }
            else
              ours.set_curr_fcn (m_fcn);
          }
      }
    else if (m_children.size ())
      {
        m_is_static = true;
        for (auto& nm_sr : m_symbols)
          nm_sr.second.set_curr_fcn (m_fcn);
      }

    for (auto& symtab_p : m_children)
      symtab_p->update_nest ();
  }

  bool
  symbol_table::scope::look_nonlocal (const std::string& name,
                                      symbol_table::symbol_record& result)
  {
    table_iterator p = m_symbols.find (name);
    if (p == m_symbols.end ())
      {
        if (m_is_nested && m_parent)
          return m_parent->look_nonlocal (name, result);
      }
    else if (! p->second.is_automatic ())
      {
        result = p->second;
        return true;
      }

    return false;
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
        error ("ignore_function_time_stamp: argument must be one of \"all\", \"system\", or \"none\"");
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
@deftypefn {} {[@var{scope}, @var{context}]} __current_scope__ ()
Return the current scope and context as integers.
@seealso{__dump_symtab_info__}
@end deftypefn */)
{
  octave::symbol_table& symtab = interp.get_symbol_table ();

  return ovl (symtab.current_scope (), symtab.current_context ());
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

      octave::symbol_table::fcn_info *finfo = symtab.get_fcn_info (fname);

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
