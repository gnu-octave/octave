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

#if !defined (octave_defun_h)
#define octave_defun_h 1

#if defined (octave_defun_dld_h)
#error defun.h and defun-dld.h both included in same file!
#endif

#include "defun-int.h"

// Define a builtin variable.
//
//   name is the name of the variable, unquoted.
//
//   defn is the initial value for the variable.
//
//   inst_as_fcn is a flag that says whether to install the variable as
//     if it were a function (allowing the name to also be used as a
//     variable by users, but recover its original definition if cleared).
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

#define DEFVAR(name, defn, inst_as_fcn, chg_fcn, doc) \
  DEFVAR_INTERNAL (#name, SBV_ ## name, defn, inst_as_fcn, false, chg_fcn, doc)

// Define a builtin-constant `__name__', and a corresponding variable
// `name' that can be redefined.  (The variable is actually installed
// in the symbol table as a function, so that it doesn't work like a
// global variable when redefined.)

#define DEFCONST(name, defn, doc) \
  DEFVAR_INTERNAL (#name, SBV_ ## name, defn, true, false, 0, doc); \
  DEFVAR_INTERNAL ("__" ## #name ## "__", XSBV_ ## name, defn, false, \
		   true, 0, doc)

// This one can be used when `name' cannot be used directly (if it is
// already defined as a macro).  In that case, name is already a
// quoted string, and the name of the structure has to be passed too.

#define DEFCONSTX(name, sname, defn, doc) \
  DEFVAR_INTERNAL (name, sname, defn, true, false, 0, doc); \
  DEFVAR_INTERNAL ("__" ## name ## "__", X ## sname, defn, false, true, \
		   0, doc)

// Define a builtin function.
//
//   name is the name of the function, unqouted.
//
//   args_name is the name of the octave_value_list variable used to pass
//     the argument list to this function.
//
//   nargout_name is the name of the int variable used to pass the
//     number of output arguments this function is expected to produce.
//
//   doc is the simple help text for the function.

#define DEFUN(name, args_name, nargout_name, doc) \
  DEFUN_INTERNAL (name, args_name, nargout_name, false, doc)

// Define a builtin text-style function.
//
// This is like DEFUN, except that it defines a function that can be
// called from the Octave language without using parenthesis to
// surround the arguments). 

#define DEFUN_TEXT(name, args_name, nargout_name, doc) \
  DEFUN_INTERNAL (name, args_name, nargout_name, true, doc)

// Define a mapper function.
//
//   name is the name of the function, unquoqted.
//
//   ch_map is a pointer to a function that should be called for
//     integer arguments that are expected to creat integer results.
//     (It's a kluge to handle character mappers like isalpha.)
//
//   d_d_map is a pointer to a function that should be called for real
//     arguments that are expected to create real results.
//
//   d_c_map is a pointer to a function that should be called for real
//     arguments that are expected to create complex results.
//
//   c_c_map is a pointer to a function that should be called for
//     complex arguments that are expected to create complex results.
//
//   lo is the lower bound of the range for which real arguments can
//     become complex.  (e.g., lo == -Inf for sqrt).
//
//   hi is the upper bound of the range for which real arguments can
//     become complex.  (e.g., hi == 0 for sqrt).
//
//   can_ret_cmplx_for_real is a flag that says whether this function
//     can create a complex number given a real-valued  argument
//     (e.g., sqrt (-1)).
//
//   doc is the simple help text for the function.

#define DEFUN_MAPPER(name, ch_map, d_d_map, d_c_map, c_c_map, \
		     lo, hi, can_ret_cmplx_for_real, doc) \
  DEFUN_MAPPER_INTERNAL(name, ch_map, d_d_map, d_c_map, c_c_map, \
			lo, hi, can_ret_cmplx_for_real, doc)

// Make alias another name for the existing function name.  This macro
// must be used in the same file where name is defined, after the
// definition for name.

#define DEFALIAS(name, alias) \
  DEFALIAS_INTERNAL (name, alias)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
