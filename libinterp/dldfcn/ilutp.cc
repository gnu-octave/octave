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


// That function implements the IKJ and JKI variants of gaussian elimination 
// to perform the ILUTP decomposition. The behaviour is controlled by milu 
// parameter. If milu = ['off'|'col'] the JKI version is performed taking 
// advantage of CCS format of the input matrix. Row pivoting is performed. 
// If milu = 'row' the input matrix has to be transposed to obtain the 
// equivalent CRS structure so we can work efficiently with rows. In that
// case IKJ version is used and column pivoting is performed.

template <typename octave_matrix_t, typename T>
void ilu_tp (octave_matrix_t& sm, octave_matrix_t& L, octave_matrix_t& U, 
             octave_idx_type nnz_u, octave_idx_type nnz_l, T* cols_norm,  
             Array <octave_idx_type>& perm_vec, const T droptol = T(0),
             const T thresh = T(0), const  std::string milu = "off", 
             const double udiag = 0)
  {
  
  // Map the strings into chars to faster comparation inside loops
  enum {OFF, ROW, COL};
  char opt;
  if (milu == "row")
    opt = ROW;
  else if (milu == "col")
    opt = COL;
  else
    opt = OFF;
  
  const octave_idx_type n = sm.cols ();

  // That is necessary for the JKI (milu = "row") variant.
  if (opt == ROW)
    sm = sm.transpose();

  // Extract pointers to the arrays for faster access inside loops
  octave_idx_type* cidx_in = sm.cidx ();
  octave_idx_type* ridx_in = sm.ridx ();
  T* data_in = sm.data ();
  octave_idx_type jrow, i, j, k, jj, c, total_len_l, total_len_u, p_perm, res, 
                  max_ind, max_len_l, max_len_u;
  T tl, aux, maximum;

  max_len_u = nnz_u;
  max_len_u += (0.1 * max_len_u) > n ? 0.1 * max_len_u : n;
  max_len_l = nnz_l;
  max_len_l += (0.1 * max_len_l) > n ? 0.1 * max_len_l : n;

  Array <octave_idx_type> cidx_out_l (dim_vector (n + 1, 1));
  octave_idx_type* cidx_l = cidx_out_l.fortran_vec ();
  Array <octave_idx_type> ridx_out_l (dim_vector (max_len_l, 1));
  octave_idx_type* ridx_l = ridx_out_l.fortran_vec ();
  Array <T> data_out_l (dim_vector (max_len_l ,1));
  T* data_l = data_out_l.fortran_vec ();
  // Data for U
  Array <octave_idx_type> cidx_out_u (dim_vector (n + 1, 1));
  octave_idx_type* cidx_u = cidx_out_u.fortran_vec ();
  Array <octave_idx_type> ridx_out_u (dim_vector (max_len_u, 1));
  octave_idx_type* ridx_u = ridx_out_u.fortran_vec ();
  Array <T> data_out_u (dim_vector (max_len_u, 1));
  T* data_u = data_out_u.fortran_vec();

  // Working arrays and permutation arrays
  octave_idx_type w_len_u, w_len_l;
  T total_sum, partial_col_sum, partial_row_sum;
  std::set <octave_idx_type> iw_l;
  std::set <octave_idx_type> iw_u;
  std::set <octave_idx_type>::iterator it, it2;
  OCTAVE_LOCAL_BUFFER (T, w_data, n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, iperm, n);
  octave_idx_type* perm = perm_vec.fortran_vec ();
  OCTAVE_LOCAL_BUFFER (octave_idx_type, uptr, n);


  T zero = T(0);
  cidx_l[0] = cidx_in[0];
  cidx_u[0] = cidx_in[0];
  /**
  for (i = 0; i < ; i++)
    {
      ridx_u[i] = 0;
      data_u[i] = 0;
      ridx_l[i] = 0;
      data_l[i] = 0;
    }
**/
  for (i = 0; i < n; i++)
    {
      w_data[i] = 0;
      perm[i] = i;
      iperm[i] = i;
    }
  total_len_u = 0;
  total_len_l = 0;

  for (k = 0; k < n; k++)
    {

      for (j = cidx_in[k]; j < cidx_in[k+1]; j++)
        {
          p_perm = iperm[ridx_in[j]];
          w_data[iperm[ridx_in[j]]] = data_in[j];
          if (p_perm > k)
            iw_l.insert (ridx_in[j]);
          else
            iw_u.insert (p_perm);
        }

      it = iw_u.begin ();
      jrow = *it;
      total_sum = zero;
      while ((jrow < k) && (it != iw_u.end ())) 
        {
          if (opt == COL)
            partial_col_sum = w_data[jrow];
          if (w_data[jrow] != zero)
            {
              if (opt == ROW)
                {
                  partial_row_sum = w_data[jrow];
                  tl = w_data[jrow] / data_u[uptr[jrow]];
                }
              for (jj = cidx_l[jrow]; jj < cidx_l[jrow+1]; jj++)
                {
                  p_perm = iperm[ridx_l[jj]];
                  aux = w_data[p_perm];
                  if (opt == ROW)
                    {
                      w_data[p_perm] -= tl * data_l[jj];
                      partial_row_sum += tl * data_l[jj];
                    }
                  else
                    {
                      tl = data_l[jj] * w_data[jrow]; 
                      w_data[p_perm] -= tl;
                      if (opt == COL)
                        partial_col_sum += tl;
                    }

                  if ((aux == zero) && (w_data[p_perm] != zero))
                    {
                      if (p_perm > k)
                        iw_l.insert (ridx_l[jj]);
                      else
                        iw_u.insert (p_perm);
                    }
                }

                // Drop element from the U part in IKJ and L part in JKI 
                // variant (milu = [col|off])
                if ((std::abs (w_data[jrow]) < (droptol * cols_norm[k])) 
                    && (w_data[jrow] != zero))
                  {
                    if (opt == COL)
                      total_sum += partial_col_sum;
                    else if (opt == ROW)
                      total_sum += partial_row_sum;
                    w_data[jrow] = zero;
                    it2 = it;
                    it++;
                    iw_u.erase (it2);
                    jrow = *it;
                    continue;
                  }
                else 
                  // This is the element scaled by the pivot in the actual iteration
                  if (opt == ROW)
                    w_data[jrow] = tl;
            }
          jrow = *(++it);
        }

      // Search for the pivot and update iw_l and iw_u if the pivot is not the
      // diagonal element
      if ((thresh > zero) && (k < (n-1)))
        {
          maximum = std::abs (w_data[k]) / thresh;
          max_ind = perm[k];
          for (it = iw_l.begin (); it != iw_l.end (); ++it) 
            {
              p_perm = iperm[*it];
              if (std::abs (w_data[p_perm]) > maximum)
                {
                  maximum = std::abs (w_data[p_perm]);
                  max_ind = *it;
                  it2 = it; 
                }
            }
          // If the pivot is not the diagonal element update all.
          p_perm = iperm[max_ind];
          if (max_ind != perm[k])
            {
              iw_l.erase (it2);
              if (w_data[k] != zero)
                iw_l.insert (perm[k]);
              else
                  iw_u.insert (k);
              // Swap data and update permutation vectors
              aux = w_data[k];
              iperm[perm[p_perm]] = k;
              iperm[perm[k]] = p_perm;
              c = perm[k];
              perm[k] = perm[p_perm];
              perm[p_perm] = c;
              w_data[k] = w_data[p_perm];
              w_data[p_perm] = aux;
            }
          
      }              

      // Drop elements in the L part in the IKJ and from the U part in the JKI
      // version.
      it = iw_l.begin ();
      while (it != iw_l.end ()) 
        {
          p_perm = iperm[*it];
          if (droptol > zero)
            if (std::abs (w_data[p_perm]) < (droptol * cols_norm[k]))
              {
                if (opt != OFF)
                  total_sum += w_data[p_perm];
                w_data[p_perm] = zero;
                it2 = it;
                it++;
                iw_l.erase (it2);
                continue;
              }

          it++;
        }

      // If milu =[row|col] sumation is preserved --> Compensate diagonal element.
      if (opt != OFF)
        {
          if ((total_sum > zero) && (w_data[k] == zero))
            iw_u.insert (k);
          w_data[k] += total_sum;
        }
          


      // Check if the pivot is zero and if udiag is active.
      // NOTE: If the pivot == 0 and udiag is active, then if milu = [col|row]
      //       will not preserve the row sum for that column/row.
      if (w_data[k] == zero)
        {
          if (udiag == 1)
            {
              w_data[k] = droptol;
              iw_u.insert (k);
            }
          else
            {
              error ("ilutp: There is a pivot equal to zero.");
              break;
            }
        }

      // Scale the elements on the L part for IKJ version (milu = [col|off])  
      if (opt != ROW)
        for (it = iw_l.begin (); it != iw_l.end (); ++it) 
          {
              p_perm = iperm[*it];
              w_data[p_perm] = w_data[p_perm] / w_data[k];
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

      // Expand working vector into U.
      w_len_u = 0;
      for (it = iw_u.begin (); it != iw_u.end (); ++it)
        {
          if (w_data[*it] != zero)
            {
              data_u[total_len_u + w_len_u] = w_data[*it];
              ridx_u[total_len_u + w_len_u] = *it;
              w_len_u++;
            }
          w_data[*it] = 0;
        }
      total_len_u += w_len_u;
      if (opt == ROW)
        uptr[k] = total_len_u -1;
      cidx_u[k+1] = cidx_u[k] - cidx_u[0] + w_len_u;

      // Expand working vector into L.
      w_len_l = 0;
      for (it = iw_l.begin (); it != iw_l.end (); ++it)
        {
          p_perm = iperm[*it];
          if (w_data[p_perm] != zero)
            {
              data_l[total_len_l + w_len_l] = w_data[p_perm];
              ridx_l[total_len_l + w_len_l] = *it;
              w_len_l++;
            }
          w_data[*it] = 0;
        }
      total_len_l += w_len_l;
      cidx_l[k+1] = cidx_l[k] - cidx_l[0] + w_len_l;

      iw_l.clear ();
      iw_u.clear ();
    }

  if (!error_state)
    {
      octave_matrix_t *L_ptr; 
      octave_matrix_t *U_ptr;
      octave_matrix_t diag (n, n, n);
      
      // L and U are interchanged if milu = 'row'. It is a matter
      // of nomenclature to re-use code with both IKJ and JKI
      // versions of the algorithm.
      if (opt == ROW)
        {
          L_ptr = &U;
          U_ptr = &L;
          L = octave_matrix_t (n, n, total_len_u - n);
          U = octave_matrix_t (n, n, total_len_l);
        }
      else
        {
          L_ptr = &L;
          U_ptr = &U;
          L = octave_matrix_t (n, n, total_len_l);
          U = octave_matrix_t (n, n, total_len_u);
        }

      for (i = 0; i <= n; i++)
        {
          L_ptr->cidx (i) = cidx_l[i];
          U_ptr->cidx (i) = cidx_u[i];
          if (opt == ROW)
            U_ptr->cidx (i) -= i;
        }

      for (octave_idx_type i = 0; i < n; i++) 
        {
          if (opt == ROW)
            diag.elem (i,i) = data_u[uptr[i]];
          j = cidx_l[i];

          while (j < cidx_l[i+1])
            {
              L_ptr->ridx (j) = ridx_l[j];
              L_ptr->data (j) = data_l[j];
              j++;
            }
          j = cidx_u[i];

          while (j < cidx_u[i+1])
            {
              c = j;
              if (opt == ROW)
                {
                  // The diagonal is removed from from L if milu = 'row'
                  // That is because is convenient to have it inside 
                  // the L part to carry out the process.
                  if (ridx_u[j] == i)
                    {
                      j++;
                      continue;
                    }
                  else
                    c -= i;
                }
              U_ptr->data (c) = data_u[j];
              U_ptr->ridx (c) = ridx_u[j];
              j++;
            }
        }

      if (opt == ROW) 
        {
          U = U.transpose ();
          // The diagonal, conveniently permuted is added to U
          U += diag.index (idx_vector::colon, perm_vec);
          L = L.transpose ();
        }
    }
}

DEFUN_DLD (ilutp, args, nargout, "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {[@var{L}, @var{U}] =} ilutp (@var{A})\n\
@deftypefnx {Loadable Function} {[@var{L}, @var{U}] =} ilutp (@var{A}, \
@var{droptol}, @var{thresh}, @var{milu}, @var{udiag})\n\
@deftypefnx {Loadable Function} {[@var{L}, @var{U}, @var{P}] =} ilutp (@var{A})\n\
@deftypefnx {Loadable Function} {[@var{L}, @var{U}, @var{P}] =} ilutp \
(@var{A}, @var{droptol}, @var{thresh}, @var{milu}, @var{udiag})\n\
\n\
Computes the incomplete LU-factorization (ILU) with threshold and pivoting.\n\
@code{[@var{L}, @var{U}] = ilutp (@var{A})} computes the default version of\n\
ILU-factorization with threshold ILUT of @var{A}, such that \
@code{@var{L} * @var{U}} is an approximation of the square sparse matrix \
@var{A}. Pivoting is performed. Parameter @var{droptol} controls the fill-in of \
output matrices. Default @var{droptol} = 0. Parameter @var{milu} = ['off'|'row'|'col'] \
set if no row nor column sums are preserved, row sums are preserved or column sums are \
preserved respectively. There are also additional diferences in the output matrices \
depending on @var{milu} parameter. Default milu = 'off'. @var{thresh} controls the \
selection of the pivot. Default @var{thresh} = 0. Parameter @var{udiag} indicates if \
there will be replacement of 0s in the upper triangular factor with the value of \
@var{droptol}. Default @var{udiag} = 0.\n\
\n\
For a full description of ILUTP behaviour and its options see ilu documentation.\n\
\n\
For more information about the algorithms themselves see:\n\n\
[1] Saad, Yousef: Iterative Methods for Sparse Linear Systems. Second Edition. \
Minneapolis, Minnesota: Siam 2003.\n\
\n\
@seealso{ilu, ilu0, iluc, ichol}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();
  std::string milu = "";
  double droptol, thresh;
  double udiag;


  if (nargout < 2 || nargout > 3 || nargin < 1 || nargin > 5)
    {
      print_usage ();
      return retval;
    }

  // To be matlab compatible 
  if (args (0).is_empty ())
    {
      retval (0) = octave_value (SparseMatrix ());
      retval (1) = octave_value (SparseMatrix ());
      if (nargout == 3)
        retval (2) = octave_value (SparseMatrix ()); 
      return retval;
    }

  if (args (0).is_scalar_type () || !args (0).is_sparse_type () )
    error ("ilutp: 1. parameter must be a sparse square matrix.");

  if (! error_state && (nargin >= 2))
    {
      droptol = args (1).double_value ();
      if (error_state || (droptol < 0) || ! args (1).is_real_scalar ())
        error ("ilutp: 2. parameter must be a positive scalar.");
    }

  if (! error_state && (nargin >= 3))
    {
      thresh = args (2).double_value ();
      if (error_state || !args (2).is_real_scalar () || (thresh < 0) || thresh > 1)
        error ("ilutp: 3. parameter must be a scalar 0 <= thresh <= 1.");
    }

  if (! error_state && (nargin >= 4))
    {
      milu = args (3).string_value ();
      if (error_state || !(milu == "row" || milu == "col" || milu == "off"))
        error ("ilutp: 3. parameter must be 'row', 'col' or 'off' character string.");
    }

  if (! error_state && (nargin == 5))
    {
      udiag = args (4).double_value ();
      if (error_state || ! args (4).is_real_scalar () || ((udiag != 0) 
          && (udiag != 1)))
        error ("ilutp: 5. parameter must be a scalar with value 1 or 0.");
    }

  if (! error_state)
    {
      octave_value_list param_list;
      octave_idx_type nnz_u, nnz_l;
      if (!args (0).is_complex_type ())
        {
          Array <double> rc_norm;
          SparseMatrix sm = args (0).sparse_matrix_value ();
          param_list.append (sm);
          nnz_u =  (feval ("triu", param_list)(0).sparse_matrix_value ()).nnz (); 
          param_list.append (-1);
          nnz_l =  (feval ("tril", param_list)(0).sparse_matrix_value ()).nnz (); 
          if (milu == "row")
            param_list (1) = "rows";
          else
            param_list (1) = "cols";
          rc_norm = feval ("norm", param_list)(0).vector_value ();
          param_list.clear ();
          Array <octave_idx_type> perm (dim_vector (sm.cols (), 1)); 
          SparseMatrix U;
          SparseMatrix L;
          ilu_tp <SparseMatrix, double> (sm, L, U, nnz_u, nnz_l, rc_norm.fortran_vec (),
                                         perm, droptol, thresh, milu, udiag);
          if (! error_state)
            {
              param_list.append (octave_value (L.cols ()));
              SparseMatrix eye = feval ("speye", param_list)(0).sparse_matrix_value ();
              if (milu == "row")
                {
                  retval (0) = octave_value (L + eye);
                  if (nargout == 2) 
                    retval (1) = octave_value (U);
                  else if (nargout == 3)
                    {
                     retval (1) = octave_value (U.index (idx_vector::colon, perm));
                     retval (2) = octave_value (eye.index (idx_vector::colon, perm));
                    }
                }
              else
                {
                  retval (1) = octave_value (U);
                  if (nargout == 2) 
                    retval (0) = octave_value (L + eye.index (perm, idx_vector::colon));
                  else if (nargout == 3)
                    {
                      retval (0) = octave_value (L.index (perm, idx_vector::colon)  + eye);
                      retval (2) = octave_value (eye.index (perm, idx_vector::colon));
                    }
                }
            }
        }
      else
        {
          Array <Complex> rc_norm;
          SparseComplexMatrix sm = args (0).sparse_complex_matrix_value ();
          param_list.append (sm);
          nnz_u =  feval ("triu", param_list)(0).sparse_complex_matrix_value ().nnz (); 
          param_list.append (-1);
          nnz_l =  feval ("tril", param_list)(0).sparse_complex_matrix_value ().nnz (); 
          if (milu == "row")
            param_list (1) = "rows";
          else
            param_list (1) = "cols";
          rc_norm = feval ("norm", param_list)(0).complex_vector_value ();
          Array <octave_idx_type> perm (dim_vector (sm.cols (), 1)); 
          param_list.clear ();
          SparseComplexMatrix U;
          SparseComplexMatrix L;
          ilu_tp < SparseComplexMatrix, Complex> 
                  (sm, L, U, nnz_u, nnz_l, rc_norm.fortran_vec (), perm, 
                   Complex (droptol), Complex (thresh), milu, udiag);

          if (! error_state)
            {
              param_list.append (octave_value (L.cols ()));
              SparseComplexMatrix eye = feval ("speye",
                                               param_list)(0).sparse_complex_matrix_value ();
              if (milu == "row")
                {
                  retval (0) = octave_value (L + eye);
                  if (nargout == 2) 
                    retval (1) = octave_value (U);
                  else if (nargout == 3)
                    {
                     retval (1) = octave_value (U.index (idx_vector::colon, perm));
                     retval (2) = octave_value (eye.index (idx_vector::colon, perm));
                    }
                }
              else
                {
                  retval (1) = octave_value (U);
                  if (nargout == 2) 
                    retval (0) = octave_value (L + eye.index (perm, idx_vector::colon)) ;
                  else if (nargout == 3)
                    {
                      retval (0) = octave_value (L.index (perm, idx_vector::colon)  + eye);
                      retval (2) = octave_value (eye.index (perm, idx_vector::colon));
                    }
                }
            }
        }

    }

  return retval;
}

/* Test cases
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
%!error [L,U] = ilutp(A_tiny, -1);
%!error [L,U] = ilutp(A_tiny, [1,2]);
%!error [L,U] = ilutp(A_tiny, 2i);
%!error [L,U] = ilutp(A_tiny, 1, -1);
%!error [L,U] = ilutp(A_tiny, 1, 2);
%!error [L,U] = ilutp(A_tiny, 1, [1, 0]);
%!error [L,U] = ilutp(A_tiny, 1, 1, 'foo');
%!error [L,U] = ilutp(A_tiny, 1, 1, '');
%!error [L,U] = ilutp(A_tiny, 1, 1, 1);
%!error [L,U] = ilutp(A_tiny, 1, 1, [1,2]);
%!error [L,U] = ilutp(A_tiny, 1, 1, 'off', 0.5);
%!error [L,U] = ilutp(A_tiny, 1, 1, 'off', -1);
%!error [L,U] = ilutp(A_tiny, 1, 1, 'off', 2);
%!error [L,U] = ilutp(A_tiny, 1, 1, 'off', [1 ,0]);
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
