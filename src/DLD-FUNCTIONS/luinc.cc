/*

Copyright (C) 2005, 2006, 2007, 2008 David Bateman

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

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"
#include "oct-map.h"

#include "MatrixType.h"
#include "SparseCmplxLU.h"
#include "SparsedbleLU.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

DEFUN_DLD (luinc, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {[@var{l}, @var{u}, @var{p}, @var{q}] =} luinc (@var{a}, '0')\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{u}, @var{p}, @var{q}] =} luinc (@var{a}, @var{droptol})\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{u}, @var{p}, @var{q}] =} luinc (@var{a}, @var{opts})\n\
@cindex LU decomposition\n\
Produce the incomplete LU factorization of the sparse matrix @var{a}.\n\
Two types of incomplete factorization are possible, and the type\n\
is determined by the second argument to @dfn{luinc}.\n\
\n\
Called with a second argument of '0', the zero-level incomplete\n\
LU factorization is produced.  This creates a factorization of @var{a}\n\
where the position of the non-zero arguments correspond to the same\n\
positions as in the matrix @var{a}.\n\
\n\
Alternatively, the fill-in of the incomplete LU factorization can\n\
be controlled through the variable @var{droptol} or the structure\n\
@var{opts}.  The @sc{umfpack} multifrontal factorization code by Tim A.\n\
Davis is used for the incomplete LU factorization, (availability\n\
@url{http://www.cise.ufl.edu/research/sparse/umfpack/})\n\
\n\
@var{droptol} determines the values below which the values in the LU\n\
factorization are dropped and replaced by zero.  It must be a positive\n\
scalar, and any values in the factorization whose absolute value are\n\
less than this value are dropped, expect if leaving them increase the\n\
sparsity of the matrix.  Setting @var{droptol} to zero results in a\n\
complete LU factorization which is the default.\n\
\n\
@var{opts} is a structure containing one or more of the fields\n\
\n\
@table @code\n\
@item droptol\n\
The drop tolerance as above.  If @var{opts} only contains @code{droptol}\n\
then this is equivalent to using the variable @var{droptol}.\n\
\n\
@item milu\n\
A logical variable flagging whether to use the modified incomplete LU\n\
factorization.  In the case that @code{milu} is true, the dropped values\n\
are subtracted from the diagonal of the matrix U of the factorization.\n\
The default is @code{false}.\n\
\n\
@item udiag\n\
A logical variable that flags whether zero elements on the diagonal of U\n\
should be replaced with @var{droptol} to attempt to avoid singular\n\
factors.  The default is @code{false}.\n\
\n\
@item thresh\n\
Defines the pivot threshold in the interval [0,1].  Values outside that\n\
range are ignored.\n\
@end table\n\
\n\
All other fields in @var{opts} are ignored.  The outputs from @dfn{luinc}\n\
are the same as for @dfn{lu}.\n\
\n\
Given the string argument 'vector', @dfn{luinc} returns the values of @var{p}\n\
@var{q} as vector values.\n\
@seealso{sparse, lu}\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value_list retval;

  if (nargin == 0)
    print_usage ();
  else if (nargin < 2 || nargin > 3)
    error ("luinc: incorrect number of arguments");
  else
    {
      bool zero_level = false;
      bool milu = false;
      bool udiag = false;
      Matrix thresh;
      double droptol = -1.;
      bool vecout;

      if (args(1).is_string ())
        {
          if (args(1).string_value () == "0")
            zero_level = true;
          else
            error ("luinc: unrecognized string argument");
        }
      else if (args(1).is_map ())
        {
          Octave_map map = args(1).map_value ();

          if (map.contains ("droptol"))
            droptol = map.contents ("droptol")(0).double_value ();

          if (map.contains ("milu"))
            {
              double tmp = map.contents ("milu")(0).double_value ();

              milu = (tmp == 0. ? false : true);
            }

          if (map.contains ("udiag"))
            {
              double tmp = map.contents ("udiag")(0).double_value ();

              udiag = (tmp == 0. ? false : true);
            }

          if (map.contains ("thresh"))
            {
              thresh = map.contents ("thresh")(0).matrix_value ();

              if (thresh.nelem () == 1)
                {
                  thresh.resize(1,2);
                  thresh(1) = thresh(0);
                }
              else if (thresh.nelem () != 2)
                error ("chol: expecting 2 element vector for thresh");
            }
        }
      else
        droptol = args(1).double_value ();

      if (nargin == 3)
        {
          std::string tmp = args(2).string_value ();

          if (! error_state )
            {
              if (tmp.compare ("vector") == 0)
                vecout = true;
              else
                error ("luinc: unrecognized string argument");
            }
        }

      // FIXME Add code for zero-level factorization
      if (zero_level)
        error ("luinc: zero-level factorization not implemented");

      if (!error_state)
        {
          if (args(0).type_name () == "sparse matrix") 
            {
              SparseMatrix sm = args(0).sparse_matrix_value ();
              octave_idx_type sm_nr = sm.rows ();
              octave_idx_type sm_nc = sm.cols ();
              ColumnVector Qinit (sm_nc);

              for (octave_idx_type i = 0; i < sm_nc; i++)
                Qinit (i) = i;

              if (! error_state)
                {
                  switch (nargout)
                    {
                    case 0:
                    case 1:
                    case 2:
                      {
                        SparseLU fact (sm, Qinit, thresh, false, true, droptol,
                                       milu, udiag);

                        if (! error_state)
                          {
                            SparseMatrix P = fact.Pr ();
                            SparseMatrix L = P.transpose () * fact.L ();
                            retval(1) = octave_value (fact.U (),
                                                      MatrixType (MatrixType::Upper));
                            retval(0) = octave_value (L, MatrixType 
                                                      (MatrixType::Permuted_Lower, 
                                                       sm_nr, fact.row_perm ()));
                          }
                      }
                      break;

                    case 3:
                      {
                        SparseLU fact (sm, Qinit, thresh, false, true, droptol,
                                       milu, udiag);

                        if (! error_state)
                          {
                            if (vecout)
                              retval(2) = fact.Pr_vec ();
                            else
                              retval(2) = fact.Pr ();
                            retval(1) = octave_value (fact.U (),
                                                      MatrixType (MatrixType::Upper));
                            retval(0) = octave_value (fact.L (),
                                                      MatrixType (MatrixType::Lower));
                          }
                      }
                      break;

                    case 4:
                    default:
                      {
                        SparseLU fact (sm, Qinit, thresh, false, false, droptol,
                                       milu, udiag);

                        if (! error_state)
                          {
                            if (vecout)
                              {
                                retval(3) = fact.Pc_vec ();
                                retval(2) = fact.Pr_vec ();
                              }
                            else
                              {
                                retval(3) = fact.Pc ();
                                retval(2) = fact.Pr ();
                              }
                            retval(1) = octave_value (fact.U (),
                                                      MatrixType (MatrixType::Upper));
                            retval(0) = octave_value (fact.L (),
                                                      MatrixType (MatrixType::Lower));
                          }
                      }
                      break;
                    }
                }
            }
          else if (args(0).type_name () == "sparse complex matrix") 
            {
              SparseComplexMatrix sm = 
                args(0).sparse_complex_matrix_value ();
              octave_idx_type sm_nr = sm.rows ();
              octave_idx_type sm_nc = sm.cols ();
              ColumnVector Qinit (sm_nc);

              for (octave_idx_type i = 0; i < sm_nc; i++)
                Qinit (i) = i;

              if (! error_state)
                {
                  switch (nargout)
                    {
                    case 0:
                    case 1:
                    case 2:
                      {
                        SparseComplexLU fact (sm, Qinit, thresh, false, true, 
                                              droptol, milu, udiag);


                        if (! error_state)
                          {
                            SparseMatrix P = fact.Pr ();
                            SparseComplexMatrix L = P.transpose () * fact.L ();
                            retval(1) = octave_value (fact.U (),
                                                      MatrixType (MatrixType::Upper));
                            retval(0) = octave_value (L, MatrixType 
                                                      (MatrixType::Permuted_Lower, 
                                                       sm_nr, fact.row_perm ()));
                          }
                      }
                      break;

                    case 3:
                      {
                        SparseComplexLU fact (sm, Qinit, thresh, false, true,
                                              droptol, milu, udiag);

                        if (! error_state)
                          {
                            if (vecout)
                              retval(2) = fact.Pr_vec ();
                            else
                              retval(2) = fact.Pr ();
                            retval(1) = octave_value (fact.U (),
                                                      MatrixType (MatrixType::Upper));
                            retval(0) = octave_value (fact.L (),
                                                      MatrixType (MatrixType::Lower));
                          }
                      }
                      break;

                    case 4:
                    default:
                      {
                        SparseComplexLU fact (sm, Qinit, thresh, false, false,
                                              droptol, milu, udiag);

                        if (! error_state)
                          {
                            if (vecout)
                              {
                                retval(3) = fact.Pc_vec ();
                                retval(2) = fact.Pr_vec ();
                              }
                            else
                              {
                                retval(3) = fact.Pc ();
                                retval(2) = fact.Pr ();
                              }
                            retval(1) = octave_value (fact.U (),
                                                      MatrixType (MatrixType::Upper));
                            retval(0) = octave_value (fact.L (),
                                                      MatrixType (MatrixType::Lower));
                          }
                      }
                      break;
                    }
                }
            }
          else
            error ("luinc: first argument must be sparse");
        }
    }

  return retval;
}

/*

%!testif HAVE_UMFPACK
%! a=sparse([1,2,0,0;0,1,2,0;1e-14,0,3,0;0,0,0,1]);
%! [l,u]=luinc(a,1e-10);
%! assert(l*u, sparse([1,2,0,0;0,1,2,0;0,0,3,0;0,0,0,1]),1e-10);
%! opts.droptol=1e-10;
%! [l,u]=luinc(a,opts);
%! assert(l*u, sparse([1,2,0,0;0,1,2,0;0,0,3,0;0,0,0,1]),1e-10);

%!testif HAVE_UMFPACK
%! a=sparse([1i,2,0,0;0,1,2,0;1e-14,0,3,0;0,0,0,1]);
%! [l,u]=luinc(a,1e-10);
%! assert(l*u, sparse([1i,2,0,0;0,1,2,0;0,0,3,0;0,0,0,1]),1e-10);
%! opts.droptol=1e-10;
%! [l,u]=luinc(a,opts);
%! assert(l*u, sparse([1i,2,0,0;0,1,2,0;0,0,3,0;0,0,0,1]),1e-10);

*/
