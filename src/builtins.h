// Builtin function support.                               -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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

#if !defined (_builtins_h)
#define _builtins_h 1

#ifdef __GNUG__
#pragma interface
#endif

class Complex;
struct help_list;

#ifndef MAPPER_FCN_TYPEDEFS
#define MAPPER_FCN_TYPEDEFS 1

typedef double (*d_d_Mapper)(double);
typedef double (*d_c_Mapper)(const Complex&);
typedef Complex (*c_c_Mapper)(const Complex&);

#endif

struct Mapper_fcn
{
  int neg_arg_complex;
  d_d_Mapper d_d_mapper;
  d_c_Mapper d_c_mapper;
  c_c_Mapper c_c_mapper;
};

struct builtin_mapper_functions
{
  char *name;
  int nargin_max;
  int nargout_max;
  int neg_arg_complex;
  d_d_Mapper d_d_mapper;
  d_c_Mapper d_c_mapper;
  c_c_Mapper c_c_mapper;
  char *help_string;
};

#ifndef SV_FUNCTION_TYPEDEFS
#define SV_FUNCTION_TYPEDEFS 1

typedef int (*sv_Function)(void);

#endif

struct builtin_string_variables
{
  char *name;
  char *value;
  sv_Function sv_function;
  char *help_string;
};

extern void install_builtins (void);
extern int is_text_function_name (const char *s);

extern help_list *builtin_mapper_functions_help (void);
extern help_list *builtin_general_functions_help (void);
extern help_list *builtin_text_functions_help (void);
extern help_list *builtin_variables_help (void);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
