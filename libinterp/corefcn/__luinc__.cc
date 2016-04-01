/*

Copyright (C) 2005-2015 David Bateman

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
#  include "config.h"
#endif

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "utils.h"
#include "oct-map.h"

#include "MatrixType.h"
#include "sparse-lu.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

// FIXME: Deprecated in 4.0 and should be removed in 4.4.
DEFUN (__luinc__, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {} {[@var{L}, @var{U}, @var{P}, @var{Q}] =} __luinc__ (@var{A}, '0')\n\
@deftypefnx {} {[@var{L}, @var{U}, @var{P}, @var{Q}] =} __luinc__ (@var{A}, @var{droptol})\n\
@deftypefnx {} {[@var{L}, @var{U}, @var{P}, @var{Q}] =} __luinc__ (@var{A}, @var{opts})\n\
Internal implementation of @code{luinc}.\n\
\n\
See documentation for @code{luinc}.\n\
@seealso{luinc}\n\
@end deftypefn")
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  if (! args(0).is_sparse_type ())
    error ("luinc: matrix A must be sparse");

  bool zero_level = false;
  bool milu = false;
  bool udiag = false;
  Matrix thresh;
  double droptol = -1.0;
  bool vecout = false;

  if (args(1).is_string ())
    {
      if (args(1).string_value () == "0")
        zero_level = true;
      else
        error ("luinc: unrecognized string argument");
    }
  else if (args(1).is_map ())
    {
      octave_scalar_map map = args(1).xscalar_map_value ("luinc: OPTS must be a scalar structure");

      octave_value tmp;

      tmp = map.getfield ("droptol");
      if (tmp.is_defined ())
        droptol = tmp.double_value ();

      tmp = map.getfield ("milu");
      if (tmp.is_defined ())
        {
          double val = tmp.double_value ();

          milu = (val == 0.0 ? false : true);
        }

      tmp = map.getfield ("udiag");
      if (tmp.is_defined ())
        {
          double val = tmp.double_value ();

          udiag = (val == 0.0 ? false : true);
        }

      tmp = map.getfield ("thresh");
      if (tmp.is_defined ())
        {
          thresh = tmp.matrix_value ();

          if (thresh.numel () == 1)
            {
              thresh.resize (1, 2);
              thresh(1) = thresh(0);
            }
          else if (thresh.numel () != 2)
            error ("luinc: THRESH must be a 1 or 2-element vector");
        }
    }
  else
    droptol = args(1).double_value ();

  if (nargin == 3)
    {
      std::string tmp = args(2).string_value ();

      if (tmp == "vector")
        vecout = true;
      else
        error ("luinc: unrecognized string argument");
    }

  // FIXME: Add code for zero-level factorization
  if (zero_level)
    error ("luinc: zero-level factorization not implemented");

  octave_value_list retval;

  if (args(0).is_real_type ())
    {
      SparseMatrix sm = args(0).sparse_matrix_value ();
      octave_idx_type sm_nr = sm.rows ();
      octave_idx_type sm_nc = sm.cols ();
      ColumnVector Qinit (sm_nc);

      for (octave_idx_type i = 0; i < sm_nc; i++)
        Qinit(i) = i;

      switch (nargout)
        {
        case 0:
        case 1:
        case 2:
          {
            sparse_lu<SparseMatrix> fact (sm, Qinit, thresh, false, true, droptol,
                           milu, udiag);

            SparseMatrix P = fact.Pr ();
            SparseMatrix L = P.transpose () * fact.L ();

            retval(1)
              = octave_value (fact.U (), MatrixType (MatrixType::Upper));

            retval(0)
              = octave_value (L, MatrixType (MatrixType::Permuted_Lower,
                                             sm_nr, fact.row_perm ()));
          }
          break;

        case 3:
          {
            sparse_lu<SparseMatrix> fact (sm, Qinit, thresh, false, true, droptol,
                           milu, udiag);

            if (vecout)
              retval(2) = fact.Pr_vec ();
            else
              retval(2) = fact.Pr_mat ();

            retval(1)
              = octave_value (fact.U (), MatrixType (MatrixType::Upper));

            retval(0)
              = octave_value (fact.L (), MatrixType (MatrixType::Lower));
          }
          break;

        case 4:
        default:
          {
            sparse_lu<SparseMatrix> fact (sm, Qinit, thresh, false, false, droptol,
                           milu, udiag);

            if (vecout)
              {
                retval(3) = fact.Pc_vec ();
                retval(2) = fact.Pr_vec ();
              }
            else
              {
                retval(3) = fact.Pc_mat ();
                retval(2) = fact.Pr_mat ();
              }

            retval(1)
              = octave_value (fact.U (), MatrixType (MatrixType::Upper));

            retval(0)
              = octave_value (fact.L (), MatrixType (MatrixType::Lower));
          }
          break;
        }
    }
  else
    {
      SparseComplexMatrix sm = args(0).sparse_complex_matrix_value ();
      octave_idx_type sm_nr = sm.rows ();
      octave_idx_type sm_nc = sm.cols ();
      ColumnVector Qinit (sm_nc);

      for (octave_idx_type i = 0; i < sm_nc; i++)
        Qinit(i) = i;

      switch (nargout)
        {
        case 0:
        case 1:
        case 2:
          {
            sparse_lu<SparseComplexMatrix> fact (sm, Qinit, thresh, false, true,
                                  droptol, milu, udiag);

            SparseMatrix P = fact.Pr ();
            SparseComplexMatrix L = P.transpose () * fact.L ();

            retval(1)
              = octave_value (fact.U (), MatrixType (MatrixType::Upper));

            retval(0)
              = octave_value (L, MatrixType (MatrixType::Permuted_Lower,
                                             sm_nr, fact.row_perm ()));
          }
          break;

        case 3:
          {
            sparse_lu<SparseComplexMatrix> fact (sm, Qinit, thresh, false, true,
                                  droptol, milu, udiag);

            if (vecout)
              retval(2) = fact.Pr_vec ();
            else
              retval(2) = fact.Pr_mat ();

            retval(1)
              = octave_value (fact.U (), MatrixType (MatrixType::Upper));

            retval(0)
              = octave_value (fact.L (), MatrixType (MatrixType::Lower));
          }
          break;

        case 4:
        default:
          {
            sparse_lu<SparseComplexMatrix> fact (sm, Qinit, thresh, false, false,
                                  droptol, milu, udiag);

            if (vecout)
              {
                retval(3) = fact.Pc_vec ();
                retval(2) = fact.Pr_vec ();
              }
            else
              {
                retval(3) = fact.Pc_mat ();
                retval(2) = fact.Pr_mat ();
              }

            retval(1)
              = octave_value (fact.U (), MatrixType (MatrixType::Upper));

            retval(0)
              = octave_value (fact.L (), MatrixType (MatrixType::Lower));
          }
          break;
        }
    }

  return retval;
}

