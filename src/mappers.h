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

#if !defined (octave_mappers_h)
#define octave_mappers_h 1

#include <string>

#include "lo-mappers.h"
#include "oct-cmplx.h"

typedef int (*ch_Mapper)(int);
typedef double (*d_d_Mapper)(double);
typedef double (*d_c_Mapper)(const Complex&);
typedef Complex (*c_c_Mapper)(const Complex&);

// ch_mapper is a kluge.
//
// If can_return_complex_for_real_arg is 1, lower_limit and
// upper_limit specify the range of values for which a real arg
// returns a real value.  Outside that range, we have to convert args
// to complex, and call the complex valued function.
//
// If can_return_complex_for_real_arg is 0, lower_limit and
// upper_limit are ignored.

struct builtin_mapper_function
{
  builtin_mapper_function (ch_Mapper ch = 0, d_d_Mapper dd = 0,
			   d_c_Mapper dc = 0, c_c_Mapper cc = 0,
			   double l = 0.0, double u = 0.0, int f = 0,
			   const string n = string (),
			   const string& h = string ())
    : ch_mapper (ch), d_d_mapper (dd), d_c_mapper (dc), c_c_mapper (cc),
      lower_limit (l), upper_limit (u), flag (f),
      name (n), help_string (h) { }

  builtin_mapper_function (const builtin_mapper_function& mf)
    : ch_mapper (mf.ch_mapper), d_d_mapper (mf.d_d_mapper),
      d_c_mapper (mf.d_c_mapper), c_c_mapper (mf.c_c_mapper),
      lower_limit (mf.lower_limit), upper_limit (mf.upper_limit),
      flag (mf.flag), name (mf.name), help_string (mf.help_string) { }

  builtin_mapper_function& operator = (const builtin_mapper_function& mf)
    {
      if (&mf != this)
	{
	  ch_mapper = mf.ch_mapper;
	  d_d_mapper = mf.d_d_mapper;
	  d_c_mapper = mf.d_c_mapper;
	  c_c_mapper = mf.c_c_mapper;
	  lower_limit = mf.lower_limit;
	  upper_limit = mf.upper_limit;
	  flag = mf.flag;
	  name = mf.name;
	  help_string = mf.help_string;
	}
      return *this;
    }

  ~builtin_mapper_function (void) { }

  ch_Mapper ch_mapper;
  d_d_Mapper d_d_mapper;
  d_c_Mapper d_c_mapper;
  c_c_Mapper c_c_mapper;
  double lower_limit;
  double upper_limit;

  // For ch_mapper:
  //
  //   0  =>  this function returns a matrix of ones and zeros
  //   1  =>  this function returns a numeric matrix (any values)
  //   2  =>  this function returns a string array
  //
  // For other mappers, nonzero means that this function can return a
  // complex value for some real arguments.
  int flag;

  string name;
  string help_string;
};

extern void install_mapper_functions (void);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
