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

#define DEFUN(name, fname, sname, nargin_max, nargout_max, doc) \
  DEFUN_INTERNAL (name, fname, sname, nargin_max, nargout_max, 0, doc)

#define DEFUN_TEXT(name, fname, sname, nargin_max, nargout_max, doc) \
  DEFUN_INTERNAL (name, fname, sname, nargin_max, nargout_max, 1, doc)

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

#define DEFALIAS(alias, name) DEFALIAS_INTERNAL (alias, name)

#ifdef MAKE_BUILTINS

#define DEFUN_INTERNAL(name, fname, sname, nargin_max, nargout_max, \
		       is_text_fcn, doc) \
  BEGIN_INSTALL_BUILTIN \
    extern DECLARE_FUN(fname); \
    static builtin_function sname = \
      { name, nargin_max, nargout_max, is_text_fcn, fname, doc }; \
    install_builtin_function (&sname); \
  END_INSTALL_BUILTIN

#else /* ! MAKE_BUILTINS */

#define DEFUN_INTERNAL(name, fname, sname, nargin_max, nargout_max, \
		       is_text_fcn, doc) \
  DECLARE_FUN(fname)

#endif /* ! MAKE_BUILTINS */

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
