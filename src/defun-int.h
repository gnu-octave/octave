/*

Copyright (C) 1996 John W. Eaton

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
    DEFINE_FUN_STRUCT (name, is_text_fcn, doc); \
    install_builtin_function (S ## name); \
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

// Define the structure that will be used to insert this function into
// the symbol table.

#define DEFINE_FUN_STRUCT(name, is_text_fcn, doc) \
  static builtin_function S ## name (#name, is_text_fcn, F ## name, doc)

#define DEFINE_FUN_STRUCT_FUN(name) \
  builtin_function& \
  FS ## name (void) \
  { \
    return S ## name; \
  }

#define DECLARE_FUN(name, args_name, nargout_name) \
  Octave_object F ## name (const Octave_object& args_name, int nargout_name)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
