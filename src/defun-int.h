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
#include "ov-mapper.h"
#include "symtab.h"
#include "version.h"

class octave_value;

extern void print_usage (const string& nm, bool just_usage = false);

extern void check_version (const string& version, const string& fcn);

extern void
install_builtin_mapper (octave_mapper *mf);

extern void
install_builtin_function (octave_builtin::fcn f, const string& name,
			  const string& doc, bool is_text_fcn = false);

extern void
install_builtin_variable (const string& n, const octave_value& v,
			  bool p, bool e,
			  symbol_record::change_function chg_fcn,
			  const string& h);

extern void
install_builtin_constant (const string& n, const octave_value& v,
			  bool p, const string& h);

extern void
alias_builtin (const string& alias, const string& name);

// MAKE_BUILTINS is defined to extract function names and related
// information and create the *.def files that are eventually used to
// create the buitlins.cc file.

#ifdef MAKE_BUILTINS

// Generate code to install name in the symbol table.  The script
// mkdefs will create a .def file for every .cc file that uses DEFUN,
// DEFUN_TEXT, or DEFUN_DLD.

#define DEFUN_INTERNAL(name, args_name, nargout_name, is_text_fcn, doc) \
  BEGIN_INSTALL_BUILTIN \
    extern DECLARE_FUN (name, args_name, nargout_name); \
    install_builtin_function (F ## name, #name, doc, is_text_fcn); \
  END_INSTALL_BUILTIN

// Generate code for making another name for an existing function.

#define DEFALIAS_INTERNAL(alias, name) \
  BEGIN_INSTALL_BUILTIN \
  alias_builtin (#alias, #name); \
  END_INSTALL_BUILTIN

#else /* ! MAKE_BUILTINS */

// Generate the first line of the function definition.  This ensures
// that the internal functions all have the same signature.

#define DEFUN_INTERNAL(name, args_name, nargout_name, is_text_fcn, doc) \
  DECLARE_FUN (name, args_name, nargout_name)

// No definition is required for an alias.

#define DEFALIAS_INTERNAL(name, alias)

#endif /* ! MAKE_BUILTINS */

// Define the code that will be used to insert the new function into
// the symbol table.

#define DEFINE_FUN_INSTALLER_FUN(name, doc) \
  bool \
  FS ## name (void) \
  { \
    static bool installed = false; \
    if (! installed) \
      { \
	check_version (OCTAVE_VERSION, #name); \
	install_builtin_function (F ## name, #name, doc); \
	installed = true; \
      } \
    return installed; \
  }

#define DECLARE_FUN(name, args_name, nargout_name) \
  octave_value_list \
  F ## name (const octave_value_list& args_name, int nargout_name)

// How builtin variables are actually installed.

#define DEFVAR_INTERNAL(name, sname, defn, protect, chg_fcn, doc) \
  install_builtin_variable (name, octave_value (defn), protect, \
			    (chg_fcn != 0), chg_fcn, doc)

// How builtin variables are actually installed.

#define DEFCONST_INTERNAL(name, sname, defn, protect, doc) \
  install_builtin_constant (name, octave_value (defn), protect, doc)

// How mapper functions are actually installed.

#define DEFUN_MAPPER_INTERNAL(name, ch_map, d_b_map, c_b_map, d_d_map, \
			      d_c_map, c_c_map, lo, hi, \
			      can_ret_cmplx_for_real, doc) \
  install_builtin_mapper \
    (new octave_mapper (ch_map, d_b_map, c_b_map, d_d_map, d_c_map, \
			c_c_map, lo, hi, \
			can_ret_cmplx_for_real, #name))

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
