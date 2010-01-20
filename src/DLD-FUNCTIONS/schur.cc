/*

Copyright (C) 1996, 1997, 1999, 2000, 2004, 2005, 2006, 2007, 2008, 2009
              John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>

#include "CmplxSCHUR.h"
#include "dbleSCHUR.h"
#include "fCmplxSCHUR.h"
#include "floatSCHUR.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN_DLD (schur, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{s} =} schur (@var{a})\n\
@deftypefnx {Loadable Function} {[@var{u}, @var{s}] =} schur (@var{a}, @var{opt})\n\
@cindex Schur decomposition\n\
The Schur decomposition is used to compute eigenvalues of a\n\
square matrix, and has applications in the solution of algebraic\n\
Riccati equations in control (see @code{are} and @code{dare}).\n\
@code{schur} always returns\n\
@tex\n\
$S = U^T A U$\n\
@end tex\n\
@ifnottex\n\
@code{s = u' * a * u}\n\
@end ifnottex\n\
where\n\
@tex\n\
$U$\n\
@end tex\n\
@ifnottex\n\
@code{u}\n\
@end ifnottex\n\
 is a unitary matrix\n\
@tex\n\
($U^T U$ is identity)\n\
@end tex\n\
@ifnottex\n\
(@code{u'* u} is identity)\n\
@end ifnottex\n\
and\n\
@tex\n\
$S$\n\
@end tex\n\
@ifnottex\n\
@code{s}\n\
@end ifnottex\n\
is upper triangular.  The eigenvalues of\n\
@tex\n\
$A$ (and $S$)\n\
@end tex\n\
@ifnottex\n\
@code{a} (and @code{s})\n\
@end ifnottex\n\
are the diagonal elements of\n\
@tex\n\
$S$.\n\
@end tex\n\
@ifnottex\n\
@code{s}.\n\
@end ifnottex\n\
If the matrix\n\
@tex\n\
$A$\n\
@end tex\n\
@ifnottex\n\
@code{a}\n\
@end ifnottex\n\
is real, then the real Schur decomposition is computed, in which the\n\
matrix\n\
@tex\n\
$U$\n\
@end tex\n\
@ifnottex\n\
@code{u}\n\
@end ifnottex\n\
is orthogonal and\n\
@tex\n\
$S$\n\
@end tex\n\
@ifnottex\n\
@code{s}\n\
@end ifnottex\n\
is block upper triangular\n\
with blocks of size at most\n\
@tex\n\
$2\\times 2$\n\
@end tex\n\
@ifnottex\n\
@code{2 x 2}\n\
@end ifnottex\n\
along the diagonal.  The diagonal elements of\n\
@tex\n\
$S$\n\
@end tex\n\
@ifnottex\n\
@code{s}\n\
@end ifnottex\n\
(or the eigenvalues of the\n\
@tex\n\
$2\\times 2$\n\
@end tex\n\
@ifnottex\n\
@code{2 x 2}\n\
@end ifnottex\n\
blocks, when\n\
appropriate) are the eigenvalues of\n\
@tex\n\
$A$\n\
@end tex\n\
@ifnottex\n\
@code{a}\n\
@end ifnottex\n\
and\n\
@tex\n\
$S$.\n\
@end tex\n\
@ifnottex\n\
@code{s}.\n\
@end ifnottex\n\
\n\
The eigenvalues are optionally ordered along the diagonal according to\n\
the value of @code{opt}.  @code{opt = \"a\"} indicates that all\n\
eigenvalues with negative real parts should be moved to the leading\n\
block of\n\
@tex\n\
$S$\n\
@end tex\n\
@ifnottex\n\
@code{s}\n\
@end ifnottex\n\
(used in @code{are}), @code{opt = \"d\"} indicates that all eigenvalues\n\
with magnitude less than one should be moved to the leading block of\n\
@tex\n\
$S$\n\
@end tex\n\
@ifnottex\n\
@code{s}\n\
@end ifnottex\n\
(used in @code{dare}), and @code{opt = \"u\"}, the default, indicates that\n\
no ordering of eigenvalues should occur.  The leading\n\
@tex\n\
$k$\n\
@end tex\n\
@ifnottex\n\
@code{k}\n\
@end ifnottex\n\
columns of\n\
@tex\n\
$U$\n\
@end tex\n\
@ifnottex\n\
@code{u}\n\
@end ifnottex\n\
always span the\n\
@tex\n\
$A$-invariant\n\
@end tex\n\
@ifnottex\n\
@code{a}-invariant\n\
@end ifnottex\n\
subspace corresponding to the\n\
@tex\n\
$k$\n\
@end tex\n\
@ifnottex\n\
@code{k}\n\
@end ifnottex\n\
leading eigenvalues of\n\
@tex\n\
$S$.\n\
@end tex\n\
@ifnottex\n\
@code{s}.\n\
@end ifnottex\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2 || nargout > 2)
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);

  std::string ord;

  if (nargin == 2)
    {
      ord = args(1).string_value (); 

      if (error_state)
        {
          error ("schur: expecting string as second argument");
          return retval;
        }
    }

  char ord_char = ord.empty () ? 'U' : ord[0];

  if (ord_char != 'U' && ord_char != 'A' && ord_char != 'D'
      && ord_char != 'u' && ord_char != 'a' && ord_char != 'd')
    {
      warning ("schur: incorrect ordered schur argument `%c'",
               ord.c_str ());
      return retval;
    }

  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  int arg_is_empty = empty_arg ("schur", nr, nc);

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return octave_value_list (2, Matrix ());

  if (nr != nc)
    {
      gripe_square_matrix_required ("schur");
      return retval;
    }

  if (arg.is_single_type ())
    {
      if (arg.is_real_type ())
        {
          FloatMatrix tmp = arg.float_matrix_value ();

          if (! error_state)
            {
              if (nargout == 0 || nargout == 1)
                {
                  FloatSCHUR result (tmp, ord, false);
                  retval(0) = result.schur_matrix ();
                }
              else
                {
                  FloatSCHUR result (tmp, ord, true);
                  retval(1) = result.schur_matrix ();
                  retval(0) = result.unitary_matrix ();
                }
            }
        }
      else if (arg.is_complex_type ())
        {
          FloatComplexMatrix ctmp = arg.float_complex_matrix_value ();

          if (! error_state)
            {
 
              if (nargout == 0 || nargout == 1)
                {
                  FloatComplexSCHUR result (ctmp, ord, false);
                  retval(0) = result.schur_matrix ();
                }
              else
                {
                  FloatComplexSCHUR result (ctmp, ord, true);
                  retval(1) = result.schur_matrix ();
                  retval(0) = result.unitary_matrix ();
                }
            }
        }
    }
  else
    {
      if (arg.is_real_type ())
        {
          Matrix tmp = arg.matrix_value ();

          if (! error_state)
            {
              if (nargout == 0 || nargout == 1)
                {
                  SCHUR result (tmp, ord, false);
                  retval(0) = result.schur_matrix ();
                }
              else
                {
                  SCHUR result (tmp, ord, true);
                  retval(1) = result.schur_matrix ();
                  retval(0) = result.unitary_matrix ();
                }
            }
        }
      else if (arg.is_complex_type ())
        {
          ComplexMatrix ctmp = arg.complex_matrix_value ();

          if (! error_state)
            {
 
              if (nargout == 0 || nargout == 1)
                {
                  ComplexSCHUR result (ctmp, ord, false);
                  retval(0) = result.schur_matrix ();
                }
              else
                {
                  ComplexSCHUR result (ctmp, ord, true);
                  retval(1) = result.schur_matrix ();
                  retval(0) = result.unitary_matrix ();
                }
            }
        }
      else
        {
          gripe_wrong_type_arg ("schur", arg);
        }
    }
 
  return retval; 
}

/*

%!test
%! a = [1, 2, 3; 4, 5, 9; 7, 8, 6];
%! [u, s] = schur (a);
%! assert(u' * a * u, s, sqrt (eps));

%!test
%! a = single([1, 2, 3; 4, 5, 9; 7, 8, 6]);
%! [u, s] = schur (a);
%! assert(u' * a * u, s, sqrt (eps('single')));

%!test
%! fail("schur ([1, 2; 3, 4], 2)","warning");

%!error <Invalid call to schur.*> schur ();
%!error schur ([1, 2, 3; 4, 5, 6]);

 */


/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
