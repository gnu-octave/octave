// defun-int.h                                          -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#define DEFUN_INTERNAL(name, fname, sname, unused_arg_flags, \
		       is_text_fcn, doc) \
  BEGIN_INSTALL_BUILTIN \
    extern DECLARE_FUN_ ## unused_arg_flags (fname); \
    DEFINE_FUN_STRUCT (name, fname, sname, is_text_fcn, doc); \
    install_builtin_function (&sname); \
  END_INSTALL_BUILTIN

// Generate code for making another name for an existing function.

#define DEFALIAS_INTERNAL(alias, name) \
  BEGIN_INSTALL_BUILTIN \
  alias_builtin (#alias, #name); \
  END_INSTALL_BUILTIN

#else /* ! MAKE_BUILTINS */

// Generate the first line of the function definition.  This ensures
// that the internal functions all have the same signature.

#define DEFUN_INTERNAL(name, fname, sname, unused_arg_flags, \
		       is_text_fcn, doc) \
  DECLARE_FUN_ ## unused_arg_flags (fname)

// No definition is required for an alias.

#define DEFALIAS_INTERNAL(name, alias)

#endif /* ! MAKE_BUILTINS */

// Define the structure that will be used to insert this function into
// the symbol table.

#define DEFINE_FUN_STRUCT(name, fname, sname, is_text_fcn, doc) \
  static builtin_function sname = \
    { name, is_text_fcn, fname, doc }

#define DEFINE_FUN_STRUCT_FUN(sname, fsname) \
  builtin_function * \
  fsname (void) \
  { \
    return &sname; \
  }

// Declare an internal function named fname.  This is the interface
// used by all internal functions in Octave that are also callable
// from the Octave language.  The funny suffixes are used to help us
// avoid warnings from g++ about unused arguments.

#define DECLARE_FUN_00(fname) \
  Octave_object fname (const Octave_object&, int)

#define DECLARE_FUN_01(fname) \
  Octave_object fname (const Octave_object&, int nargout)

#define DECLARE_FUN_10(fname) \
  Octave_object fname (const Octave_object& args, int)

#define DECLARE_FUN_11(fname) \
  Octave_object fname (const Octave_object& args, int nargout)

#define DECLARE_FUN_(fname) DECLARE_FUN_11 (fname)

// XXX FIXME XXX -- eliminate the need for these in the functions that
// use them?

#define DEFINE_ARGV(fcn_name) \
  int argc = args.length () + 1; \
  int save_argc = argc; \
  char **argv = make_argv (args, fcn_name); \
  char **save_argv = argv; \
  if (error_state) \
    return retval

#define DELETE_ARGV \
  do \
    { \
      while (--save_argc >= 0) \
	delete [] save_argv[save_argc]; \
      delete [] save_argv; \
    } \
  while (0)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
