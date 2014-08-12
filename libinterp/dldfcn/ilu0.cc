/**
 * Copyright (C) 2014 Eduardo Ramos Fernández <eduradical951@gmail.com>
 *
 * This file is part of Octave.
 *
 * Octave is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * Octave is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * Octave; see the file COPYING.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun-dld.h"
#include "parse.h"

/* 
 * That function implements the IKJ and JKI variants of gaussian elimination to
 * perform the ILUTP decomposition. The behaviour is controlled by milu
 * parameter. If milu = ['off'|'col'] the JKI version is performed taking
 * advantage of CCS format of the input matrix. If milu = 'row' the input matrix
 * has to be transposed to obtain the equivalent CRS structure so we can work
 * efficiently with rows. In this case IKJ version is used.
 */

template <typename octave_matrix_t, typename T>
void ilu_0 (octave_matrix_t& sm, const std::string milu = "off") {

  const octave_idx_type n = sm.cols ();
  OCTAVE_LOCAL_BUFFER (octave_idx_type, iw, n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, uptr, n);
  octave_idx_type j1, j2, jrow, jw, i, k, jj;
  T tl, r;

  char opt;
  enum {OFF, ROW, COL};
  if (milu == "row")
    {
      opt = ROW;
      sm = sm.transpose ();
    }
  else if (milu == "col")
    opt = COL;
  else
    opt = OFF;

  octave_idx_type* cidx = sm.cidx ();
  octave_idx_type* ridx = sm.ridx ();
  T* data = sm.data ();
  for (i = 0; i < n; i++)
    iw[i] = -1;
  for (k = 0; k < n; k++)
    {
      j1 = cidx[k];
      j2 = cidx[k+1] - 1;
      octave_idx_type j;
      for (j = j1; j <= j2; j++)
        {
          iw[ridx[j]] = j;
        }
      r = 0;
      j = j1;
      jrow = ridx[j];
      while ((jrow < k) && (j <= j2)) 
        {
          if (opt == ROW)
            {
              tl = data[j] / data[uptr[jrow]];
              data[j] = tl;
            }
          for (jj = uptr[jrow] + 1; jj < cidx[jrow+1]; jj++)
            {
              jw = iw[ridx[jj]];
              if (jw != -1)
                if (opt == ROW)
                  data[jw] -= tl * data[jj];
                else
                  data[jw] -= data[j] * data[jj];

              else
                // That is for the milu='row'
                if (opt == ROW)
                  r += tl * data[jj];
                else if (opt == COL)
                  r += data[j] * data[jj];
            }
          j++;
          jrow = ridx[j];
        }
      uptr[k] = j;
      if(opt != OFF)
        data[uptr[k]] -= r;
      if (opt != ROW)
        for (jj = uptr[k] + 1; jj < cidx[k+1]; jj++)
          data[jj] /=  data[uptr[k]];
      if (k != jrow)
        {
          error ("ilu0: Your input matrix has a zero in the diagonal.");
          break;
        }

      if (data[j] == T(0))
        {
          error ("ilu0: There is a pivot equal to zero.");
          break;
        }
      for(i = j1; i <= j2; i++)
        iw[ridx[i]] = -1;
    }
  if (opt == ROW)
    sm = sm.transpose ();
}

DEFUN_DLD (ilu0, args, nargout, "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {[@var{L}, @var{U}] =} ilu0 (@var{A})\n\
@deftypefnx  {Loadable Function} {[@var{L}, @var{U}] =} ilu0 (@var{A}, @var{milu})\n\
\n\
NOTE: No pivoting is performed.\n\
\n\
Computes the incomplete LU-factorization (ILU) with 0-order level of fill of \
@var{A}.\n\
\n\
@code{[@var{L}, @var{U}] = ilu0 (@var{A})} computes the zero fill-in ILU-\
factorization ILU(0) of @var{A}, such that @code{@var{L} * @var{U}} is an \
approximation of the square sparse matrix @var{A}. Parameter @var{milu} = \
['off'|'row'|'col'] set if no row nor column sums are preserved, row sums \
are preserved or column sums are preserved respectively.\n\
\n\
For a full description of ILU0 and its options see ilu documentation.\n\
\n\
For more information about the algorithms themselves see:\n\
\n\
[1] Saad, Yousef: Iterative Methods for Sparse Linear Systems. Second Edition. \
Minneapolis, Minnesota: Siam 2003.\n\
\n\
    @seealso{ilu, ilutp, iluc, ichol}\n\
    @end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();
  std::string milu;
 

  if (nargout > 2 || nargin < 1 || nargin > 2)
    {
      print_usage ();
      return retval;
    }

  if (args (0).is_empty ())
    {
      retval (0) = octave_value (SparseMatrix());
      retval (1) = octave_value (SparseMatrix());
      return retval;
    }

  if (args (0).is_scalar_type () || !args (0).is_sparse_type ())
    error ("ilu0: 1. parameter must be a sparse square matrix.");

  if (nargin == 2)
    {
      milu = args (1).string_value ();
      if (error_state || !(milu == "row" || milu == "col" || milu == "off"))
        error (
          "ilu0: 2. parameter must be 'row', 'col' or 'off' character string.");
      // maybe resolve milu to a numerical value / enum type already here!
    }


  if (!error_state)
    {
      // In ILU0 algorithm the zero-pattern of the input matrix is preserved so
      // it's structure does not change during the algorithm. The same input
      // matrix is used to build the output matrix due to that fact.
      octave_value_list param_list;
      if (!args (0).is_complex_type ())
        {
          SparseMatrix sm = args (0).sparse_matrix_value ();
          ilu_0 <SparseMatrix, double> (sm, milu);
          if (!error_state)
            {
              param_list.append (sm);
              retval (1) = octave_value (
                feval ("triu", param_list)(0).sparse_matrix_value ()); 
              SparseMatrix eye = feval ("speye",
                octave_value_list (
                  octave_value (sm.cols ())))(0).sparse_matrix_value ();
              param_list.append (-1);
              retval (0) = octave_value (
                eye + feval ("tril", param_list)(0).sparse_matrix_value ()); 

            }
        }
      else
        {
          SparseComplexMatrix sm = args (0).sparse_complex_matrix_value ();
          ilu_0 <SparseComplexMatrix, Complex> (sm, milu);
          if (!error_state)
            {
              param_list.append (sm);
              retval (1) = octave_value (
                feval ("triu", param_list)(0).sparse_complex_matrix_value ()); 
              SparseComplexMatrix eye = feval ("speye",
                octave_value_list (
                  octave_value (sm.cols ())))(0).sparse_complex_matrix_value ();
              param_list.append (-1);
              retval (0) = octave_value (eye +
                feval ("tril", param_list)(0).sparse_complex_matrix_value ()); 
           }
        }

    }

  return retval;
}

/* Test cases for real numbers.
%!shared n_tiny, n_small, n_medium, n_large, A_tiny, A_small, A_medium, A_large
%! n_tiny = 5;
%! n_small = 40;
%! n_medium = 600;
%! n_large = 10000;
%! A_tiny = spconvert ([1 4 2 3 3 4 2 5; 1 1 2 3 4 4 5 5; 1 2 3 4 5 6 7 8]');
%! A_small = sprand (n_small, n_small, 1/n_small) + speye (n_small);
%! A_medium = sprand (n_medium, n_medium, 1/n_medium) + speye (n_medium);
%! A_large = sprand (n_large, n_large, 1/n_large/10) + speye (n_large);
%!# Input validation tests
%!test 
%!error [L,U] = ilu0(A_tiny, 1);
%!error [L,U] = ilu0(A_tiny, [1, 2]);
%!error [L,U] = ilu0(A_tiny, '');
%!error [L,U] = ilu0(A_tiny, 'foo');
%! [L,U] = ilu0 ([]);
%! assert (isempty (L), logical (1));
%! assert (isempty (U), logical (1));
%!error [L,U] = ilu0 (0);
%!error [L,U] = ilu0 (sparse (0));
%! [L,U] = ilu0 (sparse (2));
%! assert (L, sparse (1));
%! assert (U, sparse (2));
%!test 
%! [L,U] = ilu0 (A_tiny);
%! assert (norm (A_tiny - L * U, "fro") / norm (A_tiny, "fro"), 0, n_tiny*eps);
%!test 
%! [L,U] = ilu0 (A_small);
%! assert (norm (A_small - L * U, "fro") / norm (A_small, "fro"), 0, 1);
%!test 
%! [L,U] = ilu0 (A_medium);
%! assert (norm (A_medium - L * U, "fro") / norm (A_medium, "fro"), 0, 1);
%!test 
%! [L,U] = ilu0 (A_large);
%! assert (norm (A_large - L * U, "fro") / norm (A_large, "fro"), 0, 1);
*/

/* Test cases for complex numbers
%!shared n_tiny, n_small, n_medium, n_large, A_tiny, A_small, A_medium, A_large
%! n_tiny = 5;
%! n_small = 40;
%! n_medium = 600;
%! n_large = 10000;
%! A_tiny = spconvert([1 4 2 3 3 4 2 5; 1 1 2 3 4 4 5 5; 1 2 3 4 5 6 7 8]');
%! A_tiny(1,1) += 1i;
%! A_small = sprand(n_small, n_small, 1/n_small) + ...
%!   i * sprand(n_small, n_small, 1/n_small) + speye (n_small);
%! A_medium = sprand(n_medium, n_medium, 1/n_medium) + ...
%!   i * sprand(n_medium, n_medium, 1/n_medium) + speye (n_medium);
%! A_large = sprand(n_large, n_large, 1/n_large/10) + ...
%!   i * sprand(n_large, n_large, 1/n_large/10) + speye (n_large);
%!test 
%! [L,U] = ilu0 ([]);
%! assert (isempty (L), logical (1));
%! assert (isempty (U), logical (1));
%!error [L,U] = ilu0 (0+0i);
%!error [L,U] = ilu0 (0i);
%!error [L,U] = ilu0 (sparse (0+0i));
%!error [L,U] = ilu0 (sparse (0i));
%!test 
%! [L,U] = ilu0 (sparse (2+0i));
%! assert (L, sparse (1));
%! assert (U, sparse (2));
%! [L,U] = ilu0 (sparse (2+2i));
%! assert (L, sparse (1));
%! assert (U, sparse (2+2i));
%! [L,U] = ilu0 (sparse (2i));
%! assert (L, sparse (1));
%! assert (U, sparse (2i));
%!test 
%! [L,U] = ilu0 (A_tiny);
%! assert (norm (A_tiny - L * U, "fro") / norm (A_tiny, "fro"), 0, n_tiny*eps);
%!test 
%! [L,U] = ilu0 (A_small);
%! assert (norm (A_small - L * U, "fro") / norm (A_small, "fro"), 0, 1);
%!test 
%! [L,U] = ilu0 (A_medium);
%! assert (norm (A_medium - L * U, "fro") / norm (A_medium, "fro"), 0, 1);
%!test 
%! [L,U] = ilu0 (A_large);
%! assert (norm (A_large - L * U, "fro") / norm (A_large, "fro"), 0, 1);
*/

