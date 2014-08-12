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

template <typename octave_matrix_t, typename T>
void ilu_crout (octave_matrix_t& sm_l, octave_matrix_t& sm_u,
                octave_matrix_t& L, octave_matrix_t& U, T* cols_norm,
                T* rows_norm, const T droptol = 0,
                const std::string milu = "off")
{

  // Map the strings into chars to faster comparation inside loops
  #define ROW  1
  #define COL  2
  #define OFF  0
  char opt;
  if (milu == "row")
    opt = ROW;
  else if (milu == "col")
    opt = COL;
  else
    opt = OFF;

  octave_idx_type jrow, i, j, k, jj, total_len_l, total_len_u, max_len_u,
                  max_len_l, w_len_u, w_len_l, cols_list_len, rows_list_len;

  const octave_idx_type n = sm_u.cols ();
  sm_u = sm_u.transpose ();

  max_len_u = sm_u.nnz ();
  max_len_u += (0.1 * max_len_u) > n ? 0.1 * max_len_u : n;
  max_len_l = sm_l.nnz ();
  max_len_l += (0.1 * max_len_l) > n ? 0.1 * max_len_l : n;
  // Extract pointers to the arrays for faster access inside loops
  octave_idx_type* cidx_in_u = sm_u.cidx ();
  octave_idx_type* ridx_in_u = sm_u.ridx ();
  T* data_in_u = sm_u.data ();
  octave_idx_type* cidx_in_l = sm_l.cidx ();
  octave_idx_type* ridx_in_l = sm_l.ridx ();
  T* data_in_l = sm_l.data ();
  T tl, pivot;

  // L output arrays
  Array <octave_idx_type> ridx_out_l (dim_vector (max_len_l, 1));
  octave_idx_type* ridx_l = ridx_out_l.fortran_vec ();
  Array <T> data_out_l (dim_vector (max_len_l, 1));
  T* data_l = data_out_l.fortran_vec ();

  // U output arrays
  Array <octave_idx_type> ridx_out_u (dim_vector (max_len_u, 1));
  octave_idx_type* ridx_u = ridx_out_u.fortran_vec ();
  Array <T> data_out_u (dim_vector (max_len_u, 1));
  T* data_u = data_out_u.fortran_vec ();

  // Working arrays
  OCTAVE_LOCAL_BUFFER (octave_idx_type, cidx_l, n + 1);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, cidx_u, n + 1);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, cols_list, n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, rows_list, n);
  OCTAVE_LOCAL_BUFFER (T, w_data_l, n);
  OCTAVE_LOCAL_BUFFER (T, w_data_u, n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, Ufirst, n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, Lfirst, n);
  OCTAVE_LOCAL_BUFFER (T, cr_sum, n);

  T zero = T (0);
  
  cidx_u[0] = cidx_in_u[0];
  cidx_l[0] = cidx_in_l[0];
  for (i = 0; i < n; i++)
    {
      w_data_u[i] = zero;
      w_data_l[i] = zero;
      cr_sum[i] = zero;
    }

  total_len_u = 0;
  total_len_l = 0;
  cols_list_len = 0;
  rows_list_len = 0;

  for (k = 0; k < n; k++)
    {
      // Load the working column and working row 
      for (i = cidx_in_l[k]; i < cidx_in_l[k+1]; i++)
        w_data_l[ridx_in_l[i]] = data_in_l[i];

      for (i = cidx_in_u[k]; i < cidx_in_u[k+1]; i++)
        w_data_u[ridx_in_u[i]] = data_in_u[i];

      // Update U working row
      for (j = 0; j < rows_list_len; j++)
        {
          if ((Ufirst[rows_list[j]] != -1))
            for (jj = Ufirst[rows_list[j]]; jj < cidx_u[rows_list[j]+1]; jj++)
              {
                jrow = ridx_u[jj];
                w_data_u[jrow] -= data_u[jj] * data_l[Lfirst[rows_list[j]]];
              }
        }
      // Update L working column
      for (j = 0; j < cols_list_len; j++)
        {
          if (Lfirst[cols_list[j]] != -1)
            for (jj = Lfirst[cols_list[j]]; jj < cidx_l[cols_list[j]+1]; jj++)
              {
                jrow = ridx_l[jj];
                w_data_l[jrow] -= data_l[jj] * data_u[Ufirst[cols_list[j]]];
              }
        }

      if ((max_len_u - total_len_u) < n)
        {
          max_len_u += (0.1 * max_len_u) > n ? 0.1 * max_len_u : n;
          data_out_u.resize (dim_vector (max_len_u, 1));
          data_u = data_out_u.fortran_vec ();
          ridx_out_u.resize (dim_vector (max_len_u, 1));
          ridx_u = ridx_out_u.fortran_vec ();
        }

      if ((max_len_l - total_len_l) < n)
        {
          max_len_l += (0.1 * max_len_l) > n ? 0.1 * max_len_l : n;
          data_out_l.resize (dim_vector (max_len_l, 1));
          data_l = data_out_l.fortran_vec ();
          ridx_out_l.resize (dim_vector (max_len_l, 1));
          ridx_l = ridx_out_l.fortran_vec ();
        }

      // Expand the working row into the U output data structures
      w_len_l = 0;
      data_u[total_len_u] = w_data_u[k];
      ridx_u[total_len_u] = k;
      w_len_u = 1;
      for (i = k + 1; i < n; i++)
        {
          if (w_data_u[i] != zero)
            {
              if (std::abs (w_data_u[i]) < (droptol * rows_norm[k]))
                {
                  if (opt == ROW)
                    cr_sum[k] += w_data_u[i];
                  else if (opt == COL)
                    cr_sum[i] += w_data_u[i];
                }
              else
                {
                  data_u[total_len_u + w_len_u] = w_data_u[i];
                  ridx_u[total_len_u + w_len_u] = i;
                  w_len_u++;
                }
            }

          // Expand the working column into the L output data structures
          if (w_data_l[i] != zero)
            {
              if (std::abs (w_data_l[i]) < (droptol * cols_norm[k]))
                {
                  if (opt == COL)
                    cr_sum[k] += w_data_l[i];
                  else if (opt == ROW)
                    cr_sum[i] += w_data_l[i];
                }
              else
                {
                  data_l[total_len_l + w_len_l] = w_data_l[i];
                  ridx_l[total_len_l + w_len_l] = i;
                  w_len_l++;
                }
            }
          w_data_u[i] = zero;
          w_data_l[i] = zero;
        }

      // Compensate row and column sums --> milu option
      if (opt == COL || opt == ROW)
        data_u[total_len_u] += cr_sum[k];

      // Check if the pivot is zero
      if (data_u[total_len_u] == zero)
        {
              error ("iluc: There is a pivot equal to zero.");
              break;
        }
      
      // Scale the elements in L by the pivot
      for (i = total_len_l ; i < (total_len_l + w_len_l); i++)
        data_l[i] /= data_u[total_len_u];


      total_len_u += w_len_u;
      cidx_u[k+1] = cidx_u[k] - cidx_u[0] + w_len_u;
      total_len_l += w_len_l;
      cidx_l[k+1] = cidx_l[k] - cidx_l[0] + w_len_l;

      // The tricky part of the algorithm. The arrays pointing to the first
      // working element of each column in the next iteration (Lfirst) or
      // the first working element of each row (Ufirst) are updated. Also the
      // arrays working as lists cols_list and rows_list are filled with indexes
      // pointing to Ufirst and Lfirst respectively.
      // TODO: Maybe the -1 indicating in Ufirst and Lfirst, that no elements
      // have to be considered in a certain column or row in next iteration, can
      // be removed. It feels safer to me using such an indicator.
      if (k < (n - 1))
        {
          if (w_len_u > 0)
            Ufirst[k] = cidx_u[k];
          else
            Ufirst[k] = -1;
          if (w_len_l > 0)
            Lfirst[k] = cidx_l[k];
          else
            Lfirst[k] = -1;
          cols_list_len = 0;
          rows_list_len = 0;
          for (i = 0; i <= k; i++)
            {
              if (Ufirst[i] != -1)
                {
                  jj = ridx_u[Ufirst[i]];
                  if (jj < (k + 1))
                    {
                      if (Ufirst[i] < (cidx_u[i+1]))
                        {
                          Ufirst[i]++;
                          if (Ufirst[i] == cidx_u[i+1])
                            Ufirst[i] = -1;
                          else
                            jj = ridx_u[Ufirst[i]];
                        }
                    }
                  if (jj == (k + 1)) 
                    {
                      cols_list[cols_list_len] = i;
                      cols_list_len++;
                    }
                }

              if (Lfirst[i] != -1)
                {
                  jj = ridx_l[Lfirst[i]];
                  if (jj < (k + 1))
                    if(Lfirst[i] < (cidx_l[i+1]))
                      {
                        Lfirst[i]++;
                        if (Lfirst[i] == cidx_l[i+1])
                          Lfirst[i] = -1;
                        else
                          jj = ridx_l[Lfirst[i]];
                      }
                  if (jj == (k + 1)) 
                    {
                      rows_list[rows_list_len] = i;
                      rows_list_len++;
                    }
                }
            }
        }
    }

  if (!error_state)
    {
      // Build the output matrices
      L = octave_matrix_t (n, n, total_len_l);
      U = octave_matrix_t (n, n, total_len_u);
      for (i = 0; i <= n; i++)
        L.cidx (i) = cidx_l[i];
      for (i = 0; i < total_len_l; i++)
        {
          L.ridx (i) = ridx_l[i];
          L.data (i) = data_l[i];
        }
      for (i = 0; i <= n; i++)
        U.cidx (i) = cidx_u[i];
      for (i = 0; i < total_len_u; i++)
        {
          U.ridx (i) = ridx_u[i];
          U.data (i) = data_u[i];
        }
      U = U.transpose ();
    }
}

DEFUN_DLD (iluc, args, nargout, "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {[@var{L}, @var{U}] =} iluc (@var{A})\n\
@deftypefnx {Loadable Function} {[@var{L}, @var{U}] =} iluc (@var{A}, @var{droptol}, \
@var{milu})\n\
\n\
Computes the crout version incomplete LU-factorization (ILU) with threshold of @var{A}.\n\
\n\
NOTE: No pivoting is performed.\n\
\n\
@code{[@var{L}, @var{U}] = iluc (@var{A})} computes the default crout version\n\
ILU-factorization with threshold ILUT of @var{A}, such that \
@code{@var{L} * @var{U}} is an approximation of the square sparse matrix \
@var{A}. This version of ILU algorithms is significantly faster than ILUT or ILU(0). \
Parameter @code{@var{droptol}>=0} is the scalar double threshold. All elements \
@code{x<=@var{droptol}} will be dropped in the factorization. Parameter @var{milu} \
= ['off'|'row'|'col'] set if no row nor column sums are preserved, row sums are \
preserved or column sums are preserved respectively.\n\
\n\
For a full description of ILUC behaviour and its options see ilu documentation.\n\
\n\
For more information about the algorithms themselves see:\n\
\n\
[1] Saad, Yousef: Iterative Methods for Sparse Linear Systems. Second Edition. \
Minneapolis, Minnesota: Siam 2003.\n\
\n\
@seealso{ilu, ilu0, ilutp, ichol}\n\
@end deftypefn")
{

  octave_value_list retval;
  int nargin = args.length ();
  std::string milu = "off";
  double droptol = 0;
  double thresh = 0;

  if (nargout != 2 || nargin < 1 || nargin > 3)
    {
      print_usage ();
      return retval;
    }

  // To be matlab compatible 
  if (args (0).is_empty ())
    {
      retval (0) = octave_value (SparseMatrix());
      retval (1) = octave_value (SparseMatrix());
      return retval;
    }

  if (args (0).is_scalar_type () || !args (0).is_sparse_type ())
    error ("iluc: 1. parameter must be a sparse square matrix.");

  if (! error_state && (nargin >= 2))
    {
      droptol = args (1).double_value ();
      if (error_state || (droptol < 0) || ! args (1).is_real_scalar ())
        error ("iluc: 2. parameter must be a positive real scalar.");
    }

  if (! error_state && (nargin == 3))
    {
      milu = args (2).string_value ();
      if (error_state || !(milu == "row" || milu == "col" || milu == "off"))
        error ("iluc: 3. parameter must be 'row', 'col' or 'off' character string.");
    }

  if (! error_state)
    {
      octave_value_list param_list;
      if (!args (0).is_complex_type ())
        {
          Array<double> cols_norm, rows_norm;
          param_list.append (args (0).sparse_matrix_value ());
          SparseMatrix sm_u =  feval ("triu", param_list)(0).sparse_matrix_value (); 
          param_list.append (-1);
          SparseMatrix sm_l =  feval ("tril", param_list)(0).sparse_matrix_value (); 
          param_list (1) = "rows";
          rows_norm = feval ("norm", param_list)(0).vector_value ();
          param_list (1) = "cols";
          cols_norm = feval ("norm", param_list)(0).vector_value ();
          param_list.clear ();
          SparseMatrix U;
          SparseMatrix L;
          ilu_crout <SparseMatrix, double> (sm_l, sm_u, L, U, cols_norm.fortran_vec (), 
                                            rows_norm.fortran_vec (), droptol, milu);
          if (! error_state)
            {
              param_list.append (octave_value (L.cols ()));
              SparseMatrix eye = feval ("speye", param_list)(0).sparse_matrix_value ();
              retval (0) = octave_value (L + eye);
              retval (1) = octave_value (U);
            }
        }
      else
        {
          Array<Complex> cols_norm, rows_norm;
          param_list.append (args (0).sparse_complex_matrix_value ());
          SparseComplexMatrix sm_u =  feval("triu", 
                                            param_list)(0).sparse_complex_matrix_value (); 
          param_list.append (-1);
          SparseComplexMatrix sm_l =  feval("tril", 
                                            param_list)(0).sparse_complex_matrix_value (); 
          param_list (1) = "rows";
          rows_norm = feval ("norm", param_list)(0).complex_vector_value ();
          param_list (1) = "cols";
          cols_norm = feval ("norm", param_list)(0).complex_vector_value ();
          param_list.clear ();
          SparseComplexMatrix U;
          SparseComplexMatrix L;
          ilu_crout < SparseComplexMatrix, Complex > 
                    (sm_l, sm_u, L, U, cols_norm.fortran_vec () , 
                     rows_norm.fortran_vec (), Complex (droptol), milu);
          if (! error_state)
            {
              param_list.append (octave_value (L.cols ()));
              SparseComplexMatrix eye = feval ("speye", 
                                                param_list)(0).sparse_complex_matrix_value ();
              retval (0) = octave_value (L + eye);
              retval (1) = octave_value (U);
            }
        }


    }

  return retval;
}


/* Test cases for complex numbers
%!shared n_tiny, n_small, n_medium, n_large, A_tiny, A_small, A_medium, A_large
%! n_tiny = 5;
%! n_small = 40;
%! n_medium = 600;
%! n_large = 10000;
%! A_tiny = spconvert([1 4 2 3 3 4 2 5; 1 1 2 3 4 4 5 5; 1 2 3 4 5 6 7 8]');
%! A_tiny(1,1) += 1i;
%! A_small = sprand(n_small, n_small, 1/n_small) + i * sprand(n_small, n_small, 1/n_small) + speye (n_small);
%! A_medium = sprand(n_medium, n_medium, 1/n_medium) + i * sprand(n_medium, n_medium, 1/n_medium) + speye (n_medium);
%! A_large = sprand(n_large, n_large, 1/n_large/10) + i * sprand(n_large, n_large, 1/n_large/10) + speye (n_large);
%!# Input validation tests
%!test 
%!error [L,U] = iluc(A_tiny, -1);
%!error [L,U] = iluc(A_tiny, [1,2]);
%!error [L,U] = iluc(A_tiny, 2i);
%!error [L,U] = iluc(A_tiny, 1, 'foo');
%!error [L,U] = iluc(A_tiny, 1, '');
%!error [L,U] = iluc(A_tiny, 1, 1);
%!error [L,U] = iluc(A_tiny, 1, [1,2]);
%! [L,U] = iluc ([]);
%! assert (isempty (L), logical (1));
%! assert (isempty (U), logical (1));
%!error [L,U] = iluc (0+0i);
%!error [L,U] = iluc (0i);
%!error [L,U] = iluc (sparse (0+0i));
%!error [L,U] = iluc (sparse (0i));
%! [L,U] = iluc (sparse (2+0i));
%! assert (L, sparse (1));
%! assert (U, sparse (2));
%! [L,U] = iluc (sparse (2+2i));
%! assert (L, sparse (1));
%! assert (U, sparse (2+2i));
%! [L,U] = iluc (sparse (2i));
%! assert (L, sparse (1));
%! assert (U, sparse (2i));
%!# Output tests
%!test 
%! [L,U] = iluc (A_tiny);
%! assert (norm (A_tiny - L * U, "fro") / norm (A_tiny, "fro"), 0, n_tiny*eps);
%!test 
%! [L,U] = iluc (A_small);
%! assert (norm (A_small - L * U, "fro") / norm (A_small, "fro"), 0, 1);
%!test 
%! [L,U] = iluc (A_medium);
%! assert (norm (A_medium - L * U, "fro") / norm (A_medium, "fro"), 0, 1);
%!test 
%! [L,U] = iluc (A_large);
%! assert (norm (A_large - L * U, "fro") / norm (A_large, "fro"), 0, 1);
*/

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
%!test 
%! [L,U] = iluc ([]);
%! assert (isempty (L), logical (1));
%! assert (isempty (U), logical (1));
%!error [L,U] = iluc (0);
%!error [L,U] = iluc (sparse (0));
%!test 
%! [L,U] = iluc (sparse (2));
%! assert (L, sparse (1));
%! assert (U, sparse (2));
%!test 
%! [L,U] = iluc (A_tiny);
%! assert (norm (A_tiny - L * U, "fro") / norm (A_tiny, "fro"), 0, n_tiny*eps);
%!test 
%! [L,U] = iluc (A_small);
%! assert (norm (A_small - L * U, "fro") / norm (A_small, "fro"), 0, 1);
%!test 
%! [L,U] = iluc (A_medium);
%! assert (norm (A_medium - L * U, "fro") / norm (A_medium, "fro"), 0, 1);
%!test 
%! [L,U] = iluc (A_large);
%! assert (norm (A_large - L * U, "fro") / norm (A_large, "fro"), 0, 1);
*/
