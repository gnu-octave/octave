// defun.h                                                 -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#if !defined (octave_defun_h)
#define octave_defun_h 1

#if defined (octave_defun_dld_h)
#error defun.h and defun-dld.h both included in same file!
#endif

#include "defun-int.h"

// Define a builtin variable.
//
//   name is the name of the variable, as a string.
//
//   sname is the name of the structure that is used to hold
//     information about the variable, and that is passed to
//     install_builtin_variable to register it in the symbol table.
//     By convention, it is constructed by prefixing name with the
//     character S.
//
//   ins_as_fcn is a flag that says whether to install the variable as
//     if it were a function (allowing the name to also be used as a
//     variable by users, but recover its original definition if cleared).
//
//   eternal is a flag that says whether it should be possible to
//     clear the variable.  Most builtin variables are eternal, and
//     cannot be cleared.
//
//   sv_fcn is a pointer to a function that should be called whenever
//     this variable is given a new value.  It can be 0 if there is no
//     function to call.  See also the code in user-prefs.cc.
//
//   doc is the simple help text for this variable.

#define DEFVAR(name, sname, defn, inst_as_fcn, protect, \
	       eternal, sv_fcn, doc) \
  do \
    { \
      builtin_variable sname = \
	{ \
	  name, \
	  new tree_constant (defn), \
	  inst_as_fcn, \
	  protect, \
	  eternal, \
	  sv_fcn, \
	  doc, \
	}; \
      install_builtin_variable (&sname); \
    } \
  while (0)

// Define a builtin function.
//
//   name is the name of the function, as a string.
//
//   fname is the name of the C++ function.  By convention, it is
//     constructed by prefixing name with the character F.
//
//   sname is the name of the structure that is used to hold
//     information about the function, and that is passed to
//     install_builtin_function to register the function in the symbol
//     table.  By convention, it is constructed by prefixing name with
//     the character S.
//
//   nargin_max is the maximum number of arguments this function can
//     accept. XXX FIXME XXX -- is this really used now?
//
//   nargout_max is the maximum number of outputs this function can
//   produce.  XXX FIXME XXX -- is this really used now?
//
//   doc is the simple help text for the function.

#define DEFUN(name, fname, sname, nargin_max, nargout_max, doc) \
  DEFUN_INTERNAL (name, fname, sname, nargin_max, nargout_max, 0, doc)

// Define a builtin text-style function.
//
// This is like DEFUN, except that it defines a function that can be
// called from the Octave language without using parenthesis to
// surround the arguments). 

#define DEFUN_TEXT(name, fname, sname, nargin_max, nargout_max, doc) \
  DEFUN_INTERNAL (name, fname, sname, nargin_max, nargout_max, 1, doc)

// Define a mapper function.
//
//   name is the name of the function as a string
//
//   sname is the name of the structure that is used to hold
//     information about the function, and that is passed to
//     install_builtin_mapper to register the function in the symbol
//     table.  By convention, it is constructed by prefixing name with
//     the character S.
//
//   can_ret_cmplx_for_real is a flag that says whether this function
//     can create a complex number given a real-valued  argument
//     (e.g., sqrt (-1)).
//
//   lo is the lower bound of the range for which real arguments can
//     become complex.  (e.g., lo == -Inf for sqrt).
//
//   hi is the upper bound of the range for which real arguments can
//     become complex.  (e.g., hi == 0 for sqrt).
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
//   doc is the simple help text for the function.

#define DEFUN_MAPPER(name, sname, can_ret_cmplx_for_real, lo, hi, \
		     d_d_map, d_c_map, c_c_map, doc) \
  do \
    { \
      builtin_mapper_function sname = \
	{ \
	  name, \
	  can_ret_cmplx_for_real, \
	  lo, \
	  hi, \
	  d_d_map, \
	  d_c_map, \
	  c_c_map, \
	  doc, \
	}; \
      install_builtin_mapper (&sname); \
    } \
  while (0)

// Make alias another name for the existing function name.  This macro
// must be used in the same file where name is defined, after the
// definition for name.

#define DEFALIAS(name, alias) DEFALIAS_INTERNAL (name, alias)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
