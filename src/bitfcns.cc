/*

Copyright (C) 2004 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "str-vec.h"
#include "quit.h"

#include "defun.h"
#include "error.h"
#include "ov.h"
#include "ov-uint64.h"


// XXX FIXME XXX -- could probably eliminate some code duplication by
// clever use of templates.

#define BITOP(OP, FNAME) \
 \
  octave_value retval; \
 \
  int nargin = args.length (); \
 \
  if (nargin == 2) \
    { \
      uint64NDArray x = args(0).uint64_array_value (); \
      uint64NDArray y = args(1).uint64_array_value (); \
 \
      if (! error_state) \
	{ \
	  int nelx = x.numel (); \
	  int nely = y.numel (); \
 \
	  bool is_scalar_op = (nelx == 1 || nely == 1); \
 \
	  dim_vector dvx = x.dims (); \
	  dim_vector dvy = y.dims (); \
 \
	  bool is_array_op = (dvx == dvy); \
 \
	  if (is_array_op || is_scalar_op) \
	    { \
	      uint64NDArray result; \
 \
	      if (nelx != 1) \
		result.resize (dvx); \
	      else \
		result.resize (dvy); \
 \
	      for (int i = 0; i < nelx; i++) \
		if (is_scalar_op) \
		  for (int k = 0; k < nely; k++) \
		    result(i+k) = x(i) OP y(k); \
		else \
		  result(i) = x(i) OP y(i); \
 \
		retval = result; \
	    } \
	  else \
	    error ("%s: size of x and y must match, or one operand must be a scalar", FNAME); \
        } \
      else \
        error ("%s: expecting uint64 arguments", FNAME); \
    } \
  else \
    print_usage (FNAME); \
 \
  return retval

DEFUN (bitand, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} bitand (@var{x}, @var{y})\n\
calculates the bitwise AND of nonnegative integers.\n\
@var{x}, @var{y} must be in range [0..bitmax]\n\
@end deftypefn\n\
@seealso{bitor, bitxor, bitset, bitget, bitcmp, bitshift, bitmax}")
{
  BITOP (&, "bitand");
}

DEFUN (bitor, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} bitor (@var{x}, @var{y})\n\
calculates the bitwise OR of nonnegative integers.\n\
@var{x}, @var{y} must be in range [0..bitmax]\n\
@end deftypefn\n\
@seealso{bitor, bitxor, bitset, bitget, bitcmp, bitshift, bitmax}")
{
  BITOP (|, "bitor");
}

DEFUN (bitxor, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} bitxor (@var{x}, @var{y})\n\
calculates the bitwise XOR of nonnegative integers.\n\
@var{x}, @var{y} must be in range [0..bitmax]\n\
@end deftypefn\n\
@seealso{bitand, bitor, bitset, bitget, bitcmp, bitshift, bitmax}")
{
  BITOP (^, "bitxor");
}

DEFUN (bitcmp, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} bitcmp (@var{a}, @var{k})\n\
returns the @var{k}-bit complement of integers in @var{a}. If\n\
@var{k} is omitted k = log2(bitmax) is assumed.\n\
\n\
@example\n\
bitcmp (7, 4)\n\
@result{} 8\n\
dec2bin (11)\n\
@result{} 1011\n\
dec2bin (bitcmp (11))\n\
@result{} 11111111111111111111111111110100\n\
@end example\n\
\n\
@end deftypefn\n\
@seealso{bitand, bitor, bitxor, bitset, bitget, bitcmp, bitshift, bitmax}")
{
  octave_value retval;
  error ("not implemented");
  return retval;
}

DEFUN (bitget, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Function File} {} bitget (@var{a}, @var{n})\n\
returns the status of bit(s) @var{n} of unsigned integers in @var{a}\n\
the lowest significant bit is @var{n} = 1.\n\
\n\
@example\n\
bitget (100,8:-1:1)\n\
@result{} 0  1  1  0  0  1  0  0\n\
@end example\n\
@end deftypefn\n\
@seealso{bitand, bitor, bitxor, bitset, bitcmp, bitshift, bitmax}")
{
  octave_value retval;
  error ("not implemented");
  return retval;
}

DEFUN (bitset, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Function File} {} bitset (@var{a}, @var{n})\n\
@deftypefnx {Function File} {} bitset (@var{a}, @var{n}, @var{v})\n\
sets or resets bit(s) @var{n} of unsigned integers in @var{a}.\n\
@var{v} = 0 resets and @var{v} = 1 sets the bits.\n\
The lowest significant bit is: @var{n} = 1\n\
\n\
@example\n\
dec2bin (bitset (10, 1))\n\
@result{} 1011\n\
@end example\n\
\n\
@end deftypefn\n\
@seealso{bitand, bitor, bitxor, bitget, bitcmp, bitshift, bitmax}")
{
  octave_value retval;
  error ("not implemented");
  return retval;
}

#define DO_BITSHIFT(T) \
  do \
    { \
      T ## NDArray m = m_arg.T ## _array_value (); \
 \
      if (! error_state) \
	{ \
          double d1, d2; \
 \
          if (n.all_integers (d1, d2)) \
            { \
	      int m_nel = m.numel (); \
	      int n_nel = n.numel (); \
 \
	      bool is_scalar_op = (m_nel == 1 || n_nel == 1); \
 \
	      dim_vector m_dv = m.dims (); \
	      dim_vector n_dv = n.dims (); \
 \
	      bool is_array_op = (m_dv == n_dv); \
 \
	      if (is_array_op || is_scalar_op) \
		{ \
		  T ## NDArray result; \
 \
		  if (m_nel != 1) \
		    result.resize (m_dv); \
		  else \
		    result.resize (n_dv); \
 \
		  for (int i = 0; i < m_nel; i++) \
		    if (is_scalar_op) \
		      for (int k = 0; k < n_nel; k++) \
			result(i+k) = bitshift (m(i), static_cast<int> (n(k))); \
		    else \
		      result(i) = bitshift (m(i), static_cast<int> (n(i))); \
 \
		  retval = result; \
		} \
	      else \
		error ("bitshift: size of A and N must match, or one operand must be a scalar"); \
	    } \
          else \
            error ("bitshift: expecting second argument to be integer"); \
        } \
    } \
  while (0)

DEFUN (bitshift, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Function File} {} bitshift (@var{a}, @var{k})\n\
@deftypefnx {Function File} {} bitshift (@var{a}, @var{k}, @var{n})\n\
return a @var{k} bit shift of @var{n}- digit unsigned\n\
integers in @var{a}. A positive @var{k} leads to a left shift.\n\
A negative value to a right shift. If @var{N} is omitted it defaults\n\
to log2(bitmax)+1. \n\
@var{N} must be in range [1,log2(bitmax)+1] usually [1,33]\n\
\n\
@example\n\
bitshift (eye (3), 1))\n\
@result{}\n\
@group\n\
2 0 0\n\
0 2 0\n\
0 0 2\n\
@end group\n\
\n\
bitshift (10, [-2, -1, 0, 1, 2])\n\
@result{} 2   5  10  20  40\n\
\n\
bitshift ([1, 10], 2, [3,4])\n\
@result{} 4  8\n\
@end example\n\
@end deftypefn\n\
@seealso{bitand, bitor, bitxor, bitset, bitget, bitcmp, bitmax}")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      NDArray n = args(1).array_value ();

      octave_value m_arg = args(0);

      std::string cname = m_arg.class_name ();

      if (cname == "uint8")
	DO_BITSHIFT (uint8);
      else if (cname == "uint16")
	DO_BITSHIFT (uint16);
      else if (cname == "uint32")
	DO_BITSHIFT (uint32);
      else if (cname == "uint64")
	DO_BITSHIFT (uint64);
      else
	error ("bitshift: not defined for %s objects", cname.c_str ());
    }
  else
    print_usage ("bitshift");

  return retval;
}

DEFUN (bitmax, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} bitmax (@var{x}, @var{y})\n\
@end deftypefn")
{
  octave_value retval;
  error ("not implemented");
  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
