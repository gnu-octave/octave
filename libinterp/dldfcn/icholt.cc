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

// Secondary functions specialiced for complex or real case used
// in icholt algorithms.
template < typename T > inline T
ichol_mult_complex (T a, T b)
{
  b.imag (-std::imag (b));
  return a * b;
}

template < typename T > inline bool
ichol_checkpivot_complex (T pivot)
{
  if (pivot.imag () != 0)
    {
      error ("icholt: Non-real pivot encountered. \
              The matrix must be hermitian");
      return false;
    }
  else if (pivot.real () < 0)
    {
      error ("icholt: Non-positive pivot encountered.");
      return false;
    }
  return true;

}

template < typename T > inline bool
ichol_checkpivot_real (T pivot)
{
  if (pivot < T (0))
    {
      error ("icholt: Non-positive pivot encountered.");
      return false;
    }
  return true;
}

template < typename T> inline T 
ichol_mult_real (T a, T b)
{
  return a * b;
}


template <typename octave_matrix_t, typename T,  T (*ichol_mult) (T, T), 
          bool (*ichol_checkpivot) (T)>
void ichol_t (const octave_matrix_t& sm, octave_matrix_t& L, const T* cols_norm,
              const T droptol, const std::string michol = "off")
              
{

  const octave_idx_type n = sm.cols ();
  octave_idx_type j, jrow, jend, jjrow, jw, i, k, jj, Llist_len, total_len, w_len,
                  max_len, ind;

  char opt;
  enum {OFF, ON};
  if (michol == "on")
    opt = ON;
  else
    opt = OFF;

  // Input matrix pointers
  octave_idx_type* cidx = sm.cidx ();
  octave_idx_type* ridx = sm.ridx ();
  T* data = sm.data ();

  // Output matrix data structures. Because it is not known the 
  // final zero pattern of the output matrix due to fill-in elements,
  // an heuristic approach has been adopted for memory allocation. The 
  // size of ridx_out_l and data_out_l is incremented 10% of their actual
  // size (nnz(A) in the beginning).  If that amount is less than n, their
  // size is just incremented in n elements. This way the number of
  // reallocations decrease throughout the process, obtaining a good performance.
  max_len = sm.nnz ();
  max_len += (0.1 * max_len) > n ? 0.1 * max_len : n;
  Array <octave_idx_type> cidx_out_l (dim_vector (n + 1,1));
  octave_idx_type* cidx_l = cidx_out_l.fortran_vec ();
  Array <octave_idx_type> ridx_out_l (dim_vector (max_len ,1));
  octave_idx_type* ridx_l = ridx_out_l.fortran_vec ();
  Array <T> data_out_l (dim_vector (max_len, 1));
  T* data_l = data_out_l.fortran_vec ();

  // Working arrays
  OCTAVE_LOCAL_BUFFER (T, w_data, n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, Lfirst, n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, Llist, n);
  OCTAVE_LOCAL_BUFFER (T, col_drops, n);
  std::vector <octave_idx_type> vec;
  vec.resize (n);


  T zero = T (0);
  cidx_l[0] = cidx[0];
  for (i = 0; i < n; i++)
    {
      Llist[i] = -1;
      Lfirst[i] = -1;
      w_data[i] = 0;
      col_drops[i] = zero;
      vec[i] = 0;
    }

  total_len = 0;
  for (k = 0; k < n; k++)
    {
      ind = 0;
      for (j = cidx[k]; j < cidx[k+1]; j++)
        {
          w_data[ridx[j]] = data[j];
          if (ridx[j] != k)
            {
              vec[ind] = ridx[j];
              ind++;
            }
        }
      jrow = Llist[k];
      while (jrow != -1) 
        {
          jjrow = Lfirst[jrow];
          jend = cidx_l[jrow+1];
          for (jj = jjrow; jj < jend; jj++)
            {
              j = ridx_l[jj];
              // If the element in the j position of the row is zero,
              // then it will become non-zero, so we add it to the 
              // vector that keeps track of non-zero elements in the working row.
              if (w_data[j] == zero)
                {
                  vec[ind] = j; 
                  ind++;
                }
              w_data[j] -=  ichol_mult (data_l[jj], data_l[jjrow]);

            }
          // Update the actual column first element and update the 
          // linked list of the jrow row.
          if ((jjrow + 1) < jend)
            {
              Lfirst[jrow]++;
              j = jrow;
              jrow = Llist[jrow];
              Llist[j] = Llist[ridx_l[Lfirst[j]]];
              Llist[ridx_l[Lfirst[j]]] = j;
            }
          else
            jrow = Llist[jrow];
        }

      // Resizing output arrays
      if ((max_len - total_len) < n)
        {
          max_len += (0.1 * max_len) > n ? 0.1 * max_len : n;
          data_out_l.resize (dim_vector (max_len, 1));
          data_l = data_out_l.fortran_vec ();
          ridx_out_l.resize (dim_vector (max_len, 1));
          ridx_l = ridx_out_l.fortran_vec ();
        }
      
      // The sorting of the non-zero elements of the working column can be
      // handled in a couple of ways. The most efficient two I found, are 
      // keeping the elements in an ordered binary search tree dinamically 
      // or keep them unsorted in a vector and at the end of the outer 
      // iteration order them. The last approach exhibit lower execution 
      // times.   
      std::sort (vec.begin (), vec.begin () + ind);

      data_l[total_len] = w_data[k];
      ridx_l[total_len] = k;
      w_len = 1;

      // Extract then non-zero elements of working column and drop the
      // elements that are lower than droptol * cols_norm[k].
      for (i = 0; i < ind ; i++)
        {
          jrow = vec[i];
          if (w_data[jrow] != zero)
            {
              if (std::abs (w_data[jrow]) < (droptol * cols_norm[k]))
                {
                  if (opt == ON)
                    {
                      col_drops[k] += w_data[jrow];
                      col_drops[jrow] += w_data[jrow];
                    }
                }
              else
                {
                  data_l[total_len + w_len] = w_data[jrow];
                  ridx_l[total_len + w_len] = jrow;
                  w_len++;
                }
              vec[i] = 0;
            }
          w_data[jrow] = zero;
        }

      // Compensate column sums --> michol option
      if (opt == ON)
        data_l[total_len] += col_drops[k];

      if (data_l[total_len] == zero)
        {
          error ("icholt: There is a pivot equal to zero.");
          break;
        }
      else if (!ichol_checkpivot (data_l[total_len]))
        break;

      // Once the elements are dropped and compensation of columns 
      // sums are done, scale the elements by the pivot.
      data_l[total_len] = std::sqrt (data_l[total_len]);
      for (jj = total_len + 1; jj < (total_len + w_len); jj++)
        data_l[jj] /=  data_l[total_len];
      total_len += w_len;
      cidx_l[k+1] = cidx_l[k] - cidx_l[0] + w_len;

      // Update Llist and Lfirst with the k-column information.
      if (k < (n - 1)) 
        {
          Lfirst[k] = cidx_l[k];
          if ((Lfirst[k] + 1) < cidx_l[k+1])
            {
              Lfirst[k]++;
              jjrow = ridx_l[Lfirst[k]];
              Llist[k] = Llist[jjrow];
              Llist[jjrow] = k;
            }
        }
        
      }

  if (! error_state)
    {
      // Build the output matrices
      L = octave_matrix_t (n, n, total_len);
      for (i = 0; i <= n; i++)
        L.cidx (i) = cidx_l[i];
      for (i = 0; i < total_len; i++)
        {
          L.ridx (i) = ridx_l[i];
          L.data (i) = data_l[i];
        }
    }

}

DEFUN_DLD (icholt, args, nargout, "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{L} =} icholt (@var{A}, @var{droptol}, @var{michol})\n\
\n\
Computes the thresholded Incomplete Cholesky factorization [ICT] of A \
which must be an square hermitian matrix in the complex case and a symmetric \
positive definite matrix in the real one. \
\n\
@code{[@var{L}] = icholt (@var{A}, @var{droptol}, @var{michol})} \
computes the ICT of @var{A}, such that @code{@var{L} * @var{L}'} is an \
approximation of the square sparse hermitian matrix @var{A}. @var{droptol} is \
a non-negative scalar used as a drop tolerance when performing ICT. Elements \
which are smaller in magnitude than @code{@var{droptol} * norm(@var{A}(j:end, j), 1)} \
, are dropped from the resulting factor @var{L}. The parameter @var{michol} \
decides whether the Modified IC(0) should be performed. This compensates the \
main diagonal of @var{L}, such that @code{@var{A} * @var{e} = @var{L} * @var{L}' \
 * @var{e}} with @code{@var{e} = ones (size (@var{A}, 2), 1))} holds. \n\
\n\
For more information about the algorithms themselves see:\n\
\n\
[1] Saad, Yousef. \"Preconditioning Techniques.\" Iterative Methods for Sparse Linear \
Systems. PWS Publishing Company, 1996. \
\n\
\n\
[2] Jones, Mark T. and Plassmann, Paul E.: An Improved Incomplete Cholesky \
Factorization (1992). \
\n\
@seealso{ichol, ichol0, chol, ilu}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();
  // Default values of parameters
  std::string michol = "off";
  double droptol = 0;
 

  if (nargout > 1 || nargin < 1 || nargin > 3)
    {
      print_usage ();
      return retval;
    }

  if (args (0).is_scalar_type () || !args (0).is_sparse_type ())
    error ("icholt: 1. parameter must be a sparse square matrix.");

  if (args (0).is_empty ())
    {
      retval (0) = octave_value (SparseMatrix ());
      return retval;
    }

  if (! error_state && (nargin >= 2))
    {
      droptol = args (1).double_value ();
      if (error_state || (droptol < 0) || ! args (1).is_real_scalar ())
        error ("icholt: 2. parameter must be a positive real scalar.");
    }

  if (! error_state && (nargin == 3))
    {
      michol = args (2).string_value ();
      if (error_state || !(michol == "on" || michol == "off"))
        error ("icholt: 3. parameter must be 'on' or 'off' character string.");
    }

  if (!error_state)
    {
      octave_value_list param_list;
      if (! args (0).is_complex_type ())
        {
          Array <double> cols_norm;
          SparseMatrix L;
          param_list.append (args (0).sparse_matrix_value ());
          SparseMatrix sm_l = feval ("tril", 
                                     param_list) (0).sparse_matrix_value (); 
          param_list (0) = sm_l;
          param_list (1) = 1;
          param_list (2) = "cols";
          cols_norm = feval ("norm", param_list) (0).vector_value ();
          param_list.clear ();
          ichol_t <SparseMatrix, 
                   double, ichol_mult_real, ichol_checkpivot_real> 
                   (sm_l, L, cols_norm.fortran_vec (), droptol, michol);
          if (! error_state)
            retval (0) = octave_value (L);
        }
      else
        {
          Array <Complex> cols_norm;
          SparseComplexMatrix L;
          param_list.append (args (0).sparse_complex_matrix_value ());
          SparseComplexMatrix sm_l = feval ("tril", 
                                            param_list) (0).sparse_complex_matrix_value (); 
          param_list (0) = sm_l;
          param_list (1) = 1;
          param_list (2) = "cols";
          cols_norm = feval ("norm", param_list) (0).complex_vector_value ();
          param_list.clear ();
          ichol_t <SparseComplexMatrix, 
                   Complex, ichol_mult_complex, ichol_checkpivot_complex> 
                   (sm_l, L, cols_norm.fortran_vec (), Complex (droptol), michol);
          if (! error_state)
            retval (0) = octave_value (L);
        }

    }

  return retval;
}

/*
%% Real matrices
%!shared A_1, A_2, A_3, A_4, A_5
%! A_1 = [ 0.37, -0.05,  -0.05,  -0.07;
%!        -0.05,  0.116,  0.0,   -0.05;
%!        -0.05,  0.0,    0.116, -0.05;
%!        -0.07, -0.05,  -0.05,   0.202];
%! A_1 = sparse(A_1);
%!
%! A_2 = gallery ('poisson', 30);
%!
%! A_3 = gallery ('tridiag', 50);
%!
%! nx = 400; ny = 200;
%! hx = 1 / (nx + 1); hy = 1 / (ny + 1);
%! Dxx = spdiags ([ones(nx, 1), -2 * ones(nx, 1), ones(nx, 1)], [-1 0 1 ], nx, nx) / (hx ^ 2);
%! Dyy = spdiags ([ones(ny, 1), -2 * ones(ny, 1), ones(ny, 1)], [-1 0 1 ], ny, ny) / (hy ^ 2);
%! A_4 = -kron (Dxx, speye (ny)) - kron (speye (nx), Dyy);
%! A_4 = sparse (A_4);
%!
%! A_5 = [ 0.37, -0.05,          -0.05,  -0.07;
%!        -0.05,  0.116,          0.0,   -0.05 + 0.05i;
%!        -0.05,  0.0,            0.116, -0.05;
%!        -0.07, -0.05 - 0.05i,  -0.05,   0.202];
%! A_5 = sparse(A_5);
%! A_6 = [ 0.37,    -0.05 - i, -0.05,  -0.07;
%!        -0.05 + i, 0.116,     0.0,   -0.05;
%!        -0.05,     0.0,       0.116, -0.05;
%!        -0.07,    -0.05,     -0.05,   0.202];
%! A_6 = sparse(A_6);
%! A_7 = A_5;
%! A_7(1) = 2i;
%!
%!test
%!error icholt ([]);
%!error icholt ([],[]);
%!error icholt ([],[],[]);
%!error [~] = icholt ([],[],[]);
%!error [L] = icholt ([],[],[]);
%!error [L] = icholt ([], 1e-4, 1);
%!error [L] = icholt (A_1, [], 'off');
%!error [L] = icholt (A_1, 1e-4, []);
%!error [L, E] = icholt (A_1, 1e-4, 'off');
%!error [L] = icholt (A_1, 1e-4, 'off', A_1);
%!error icholt (sparse (0), 1e-4, 'off');
%!error icholt (sparse (-0), 1e-4, 'off');
%!error icholt (sparse (-1), 1e-4, 'off');
%!error icholt (sparse (i), 1e-4, 'off');
%!error icholt (sparse (-i), 1e-4, 'off');
%!error icholt (sparse (1 + 1i), 1e-4, 'off');
%!error icholt (sparse (1 - 1i), 1e-4, 'off');
%!
%!test
%! L = icholt (sparse (1), 1e-4, 'off');
%! assert (L, sparse (1));
%! L = icholt (sparse (4), 1e-4, 'off');
%! assert (L, sparse (2));
%!
%!test
%! L = icholt (A_1, 1e-4, 'off');
%! assert (norm (A_1 - L*L', 'fro') / norm (A_1, 'fro'), eps, eps);
%! L = icholt (A_1, 1e-4, 'on');
%! assert (norm (A_1 - L*L', 'fro') / norm (A_1, 'fro'), eps, eps);
%!
%!test
%! L = icholt (A_2, 1e-4, 'off');
%! assert (norm (A_2 - L*L', 'fro') / norm (A_2, 'fro'), 1e-4, 1e-4);
%! L = icholt (A_2, 1e-4, 'on');
%! assert (norm (A_2 - L*L', 'fro') / norm (A_2, 'fro'), 3e-4, 1e-4);
%!
%!test
%! L = icholt (A_3, 1e-4, 'off');
%! assert (norm (A_3 - L*L', 'fro') / norm (A_3, 'fro'), eps, eps);
%! L = icholt (A_3, 1e-4, 'on');
%! assert (norm (A_3 - L*L', 'fro') / norm (A_3, 'fro'), eps, eps);
%!
%!test
%! L = icholt (A_4, 1e-4, 'off');
%! assert (norm (A_4 - L*L', 'fro') / norm (A_4, 'fro'), 2e-4, 1e-4);
%! L = icholt (A_4, 1e-4, 'on');
%! assert (norm (A_4 - L*L', 'fro') / norm (A_4, 'fro'), 7e-4, 1e-4);
%!
%% Complex matrices
%!test
%! L = ichol0 (A_5, 'off');
%! assert (norm (A_5 - L*L', 'fro') / norm (A_5, 'fro'), 1e-2, 1e-2);
%! L = ichol0 (A_5, 'on');
%! assert (norm (A_5 - L*L', 'fro') / norm (A_5, 'fro'), 2e-2, 1e-2);
%% Negative pivot 
%!error ichol0 (A_6, 'off');
%% Complex entry in the diagonal
%!error ichol0 (A_7, 'off');
*/


