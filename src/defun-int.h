/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_defun_int_h)
#define octave_defun_int_h 1

#include <string>

#include "ov-builtin.h"
#include "ov-dld-fcn.h"
#include "ov-mapper.h"
#include "symtab.h"
#include "version.h"

class octave_value;

extern void print_usage (const std::string& nm, bool just_usage = false);

extern void check_version (const std::string& version, const std::string& fcn);

extern void
install_builtin_mapper (octave_mapper *mf);

extern void
install_builtin_function (octave_builtin::fcn f, const std::string& name,
			  const std::string& doc, bool is_text_fcn = false,
			  bool can_hide_function = true);

extern void
install_builtin_variable (const std::string& n, const octave_value& v,
			  bool p, bool e,
			  symbol_record::change_function chg_fcn,
			  const std::string& h);

extern void
install_builtin_constant (const std::string& n, const octave_value& v,
			  bool p, const std::string& h);

extern void
install_dld_function (octave_dld_function::fcn f, const std::string& name,
		      const octave_shlib& shl,
		      const std::string& doc, bool is_text_fcn = false);

extern void
alias_builtin (const std::string& alias, const std::string& name);

#define DECLARE_FUNX(name, args_name, nargout_name) \
  octave_value_list \
  name (const octave_value_list& args_name, int nargout_name)

#define DECLARE_FUN(name, args_name, nargout_name) \
  DECLARE_FUNX (F ## name, args_name, nargout_name)

// Define the code that will be used to insert the new function into
// the symbol table.  We look for this name instead of the actual
// function so that we can easily install the doc std::string too.

typedef bool (*octave_dld_fcn_installer) (const octave_shlib&);

#define DEFINE_FUN_INSTALLER_FUN(name, doc) \
  DEFINE_FUN_INSTALLER_FUN2(name, doc, CXX_ABI)

#define DEFINE_FUN_INSTALLER_FUN2(name, doc, cxx_abi) \
  DEFINE_FUN_INSTALLER_FUN3(name, doc, cxx_abi)

#define DEFINE_FUN_INSTALLER_FUN3(name, doc, cxx_abi) \
  extern "C" \
  bool \
  FS ## name ## _ ## cxx_abi (const octave_shlib& shl) \
  { \
    check_version (OCTAVE_API_VERSION, #name); \
    install_dld_function (F ## name, #name, shl, doc); \
    return error_state ? false : true; \
  }

// Define a builtin variable.
//
//   name is the name of the variable, unquoted.
//
//   defn is the initial value for the variable.
//
//   protect is a flag that says whether it should be possible to give
//     the variable a new value.
//
//   eternal is a flag that says whether it should be possible to
//     clear the variable.  Most builtin variables are eternal, and
//     cannot be cleared.
//
//   chg_fcn is a pointer to a function that should be called whenever
//     this variable is given a new value.  It can be 0 if there is no
//     function to call.  See also the code in user-prefs.cc.
//
//   doc is the simple help text for this variable.

#define DEFVAR(name, defn, chg_fcn, doc) \
  DEFVAR_INTERNAL (#name, SBV_ ## name, defn, false, chg_fcn, doc)

// Define a builtin constant `name' (which may be redefined, but will
// retain its original value when cleared) and also an alias to it
// called `__name__' (which may not be redefined).

#define DEFCONST(name, defn, doc) \
  DEFCONST_INTERNAL (name, defn, doc)

// This one can be used when `name' cannot be used directly (if it is
// already defined as a macro).  In that case, name is already a
// quoted string, and the name of the structure must to be passed too.

#define DEFCONSTX(name, sname, defn, doc) \
  DEFCONSTX_INTERNAL (name, sname, defn, doc)

// MAKE_BUILTINS is defined to extract function names and related
// information and create the *.df files that are eventually used to
// create the builtins.cc file.

#if defined (MAKE_BUILTINS)

// Generate code to install name in the symbol table.  The script
// mkdefs will create a .def file for every .cc file that uses DEFUN,
// or DEFCMD.

#define DEFUN_INTERNAL(name, args_name, nargout_name, is_text_fcn, doc) \
  BEGIN_INSTALL_BUILTIN \
    XDEFUN_INTERNAL (name, args_name, nargout_name, is_text_fcn, doc) \
  END_INSTALL_BUILTIN

#define DEFCONSTFUN_INTERNAL(name, args_name, nargout_name, is_text_fcn, doc) \
  BEGIN_INSTALL_BUILTIN \
    XDEFCONSTFUN_INTERNAL (name, args_name, nargout_name, is_text_fcn, doc) \
  END_INSTALL_BUILTIN

#define DEFUNX_INTERNAL(name, fname, args_name, nargout_name, \
			is_text_fcn, doc) \
  BEGIN_INSTALL_BUILTIN \
    XDEFUNX_INTERNAL (name, fname, args_name, nargout_name, is_text_fcn, doc) \
  END_INSTALL_BUILTIN

// Generate code to install name in the symbol table.  The script
// mkdefs will create a .def file for every .cc file that uses
// DEFUN_DLD.

#define DEFUN_DLD_INTERNAL(name, args_name, nargout_name, is_text_fcn, doc) \
  BEGIN_INSTALL_BUILTIN \
    XDEFUN_DLD_INTERNAL (name, args_name, nargout_name, is_text_fcn, doc) \
  END_INSTALL_BUILTIN

// Generate code for making another name for an existing function.

#define DEFALIAS_INTERNAL(alias, name) \
  BEGIN_INSTALL_BUILTIN \
    XDEFALIAS_INTERNAL(alias, name) \
  END_INSTALL_BUILTIN

#define DEFVAR_INTERNAL(name, sname, defn, protect, chg_fcn, doc) \
  BEGIN_INSTALL_BUILTIN \
    XDEFVAR_INTERNAL(name, sname, defn, protect, chg_fcn, doc) \
  END_INSTALL_BUILTIN

#define DEFCONST_INTERNAL(name, defn, doc) \
  BEGIN_INSTALL_BUILTIN \
    XDEFCONST_INTERNAL(name, defn, doc) \
  END_INSTALL_BUILTIN

#define DEFCONSTX_INTERNAL(name, sname, defn, doc) \
  BEGIN_INSTALL_BUILTIN \
    XDEFCONST_INTERNAL(name, defn, doc) \
  END_INSTALL_BUILTIN

#define DEFUN_MAPPER_INTERNAL(name, ch_map, d_b_map, c_b_map, d_d_map, \
			      d_c_map, c_c_map, lo, hi, \
			      ch_map_flag, can_ret_cmplx_for_real, doc) \
  BEGIN_INSTALL_BUILTIN \
    XDEFUN_MAPPER_INTERNAL(name, ch_map, d_b_map, c_b_map, d_d_map, \
		           d_c_map, c_c_map, lo, hi, \
			   ch_map_flag, can_ret_cmplx_for_real, doc) \
  END_INSTALL_BUILTIN

#else /* ! MAKE_BUILTINS */

// Generate the first line of the function definition.  This ensures
// that the internal functions all have the same signature.

#define DEFUN_INTERNAL(name, args_name, nargout_name, is_text_fcn, doc) \
  DECLARE_FUN (name, args_name, nargout_name)

#define DEFCONSTFUN_INTERNAL(name, args_name, nargout_name, is_text_fcn, doc) \
  DECLARE_FUN (name, args_name, nargout_name)

#define DEFUNX_INTERNAL(name, fname, args_name, nargout_name, \
			is_text_fcn, doc) \
  DECLARE_FUNX (fname, args_name, nargout_name)

// No definition is required for an alias.

#define DEFALIAS_INTERNAL(alias, name)

// How builtin variables are actually installed.

#define DEFVAR_INTERNAL(name, sname, defn, protect, chg_fcn, doc) \
  install_builtin_variable (name, octave_value (defn), protect, \
			    (chg_fcn != 0), chg_fcn, doc)

// How builtin variables are actually installed.

#define INSTALL_CONST(name, sname, defn, protect, doc) \
  install_builtin_constant (name, octave_value (defn), protect, doc)

#define DEFCONST_INTERNAL(name, defn, doc) \
  INSTALL_CONST (#name, SBV_ ## name, defn, false, doc);

#define DEFCONSTX_INTERNAL(name, sname, defn, doc) \
  INSTALL_CONST (name, sname, defn, false, doc);

// How mapper functions are actually installed.

// XXX FIXME XXX -- Really want to avoid the following casts, since
// (as always with casts) it may mask some real errors...

#define DEFUN_MAPPER_INTERNAL(name, ch_map, d_b_map, c_b_map, d_d_map, \
			      d_c_map, c_c_map, lo, hi, \
			      ch_map_flag, can_ret_cmplx_for_real, doc) \
  install_builtin_mapper \
    (new octave_mapper \
     (X_CAST (octave_mapper::ch_mapper, ch_map), \
      X_CAST (octave_mapper::d_b_mapper, d_b_map), \
      X_CAST (octave_mapper::c_b_mapper, c_b_map), \
      X_CAST (octave_mapper::d_d_mapper, d_d_map), \
      X_CAST (octave_mapper::d_c_mapper, d_c_map), \
      X_CAST (octave_mapper::c_c_mapper, c_c_map), \
      lo, hi, ch_map_flag, can_ret_cmplx_for_real, #name, doc))

#endif /* ! MAKE_BUILTINS */

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
