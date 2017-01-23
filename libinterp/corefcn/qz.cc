/*

Copyright (C) 1998-2016 A. S. Hodel

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

// Generalized eigenvalue balancing via LAPACK

// Author: A. S. Hodel <scotte@eng.auburn.edu>

#undef DEBUG
#undef DEBUG_SORT
#undef DEBUG_EIG

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cfloat>

#include <iostream>
#include <iomanip>

#include "f77-fcn.h"
#include "lo-lapack-proto.h"
#include "lo-math.h"
#include "qr.h"
#include "quit.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "oct-map.h"
#include "ov.h"
#include "pager.h"
#if defined (DEBUG) || defined (DEBUG_SORT)
#  include "pr-output.h"
#endif
#include "symtab.h"
#include "utils.h"
#include "variables.h"

typedef F77_INT (*sort_function) (const F77_INT& LSIZE,
                                  const double& ALPHA, const double& BETA,
                                  const double& S, const double& P);

extern "C"
{
  // Van Dooren's code (netlib.org: toms/590) for reordering
  // GEP.  Only processes Z, not Q.
  F77_RET_T
  F77_FUNC (dsubsp, DSUBSP) (const F77_INT& NMAX, const F77_INT& N,
                             F77_DBLE* A, F77_DBLE* B, F77_DBLE* Z,
                             sort_function, const F77_DBLE& EPS,
                             F77_INT& NDIM, F77_INT& FAIL, F77_INT* IND);
}

// fcrhp, fin, fout, folhp:
// Routines for ordering of generalized eigenvalues.
// Return 1 if test is passed, 0 otherwise.
//   fin:  |lambda| < 1
//   fout: |lambda| >= 1
//   fcrhp: real(lambda) >= 0
//   folhp: real(lambda) < 0

static F77_INT
fcrhp (const F77_INT& lsize, const double& alpha, const double& beta,
       const double& s, const double&)
{
  if (lsize == 1)
    return (alpha * beta >= 0 ? 1 : -1);
  else
    return (s >= 0 ? 1 : -1);
}

static F77_INT
fin (const F77_INT& lsize, const double& alpha, const double& beta,
     const double&, const double& p)
{
  F77_INT retval;

  if (lsize == 1)
    retval = (fabs (alpha) < fabs (beta) ? 1 : -1);
  else
    retval = (fabs (p) < 1 ? 1 : -1);

#if defined (DEBUG)
  std::cout << "qz: fin: retval=" << retval << std::endl;
#endif

  return retval;
}

static F77_INT
folhp (const F77_INT& lsize, const double& alpha, const double& beta,
       const double& s, const double&)
{
  if (lsize == 1)
    return (alpha * beta < 0 ? 1 : -1);
  else
    return (s < 0 ? 1 : -1);
}

static F77_INT
fout (const F77_INT& lsize, const double& alpha, const double& beta,
      const double&, const double& p)
{
  if (lsize == 1)
    return (fabs (alpha) >= fabs (beta) ? 1 : -1);
  else
    return (fabs (p) >= 1 ? 1 : -1);
}

//FIXME: Matlab does not produce lambda as the first output argument.
//       Compatibility problem?
DEFUN (qz, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{lambda} =} qz (@var{A}, @var{B})
@deftypefnx {} {@var{lambda} =} qz (@var{A}, @var{B}, @var{opt})
QZ@tie{}decomposition of the generalized eigenvalue problem
(@math{A x = s B x}).

There are three ways to call this function:
@enumerate
@item @code{@var{lambda} = qz (@var{A}, @var{B})}

Computes the generalized eigenvalues
@tex
$\lambda$
@end tex
@ifnottex
@var{lambda}
@end ifnottex
of @math{(A - s B)}.

@item @code{[AA, BB, Q, Z, V, W, @var{lambda}] = qz (@var{A}, @var{B})}

Computes QZ@tie{}decomposition, generalized eigenvectors, and generalized
eigenvalues of @math{(A - s B)}
@tex
$$ AV = BV{ \rm diag }(\lambda) $$
$$ W^T A = { \rm diag }(\lambda)W^T B $$
$$ AA = Q^T AZ, BB = Q^T BZ $$
@end tex
@ifnottex

@example
@group

A * V = B * V * diag (@var{lambda})
W' * A = diag (@var{lambda}) * W' * B
AA = Q * A * Z, BB = Q * B * Z

@end group
@end example

@end ifnottex
with @var{Q} and @var{Z} orthogonal (unitary)= @var{I}

@item @code{[AA,BB,Z@{, @var{lambda}@}] = qz (@var{A}, @var{B}, @var{opt})}

As in form [2], but allows ordering of generalized eigenpairs for, e.g.,
solution of discrete time algebraic Riccati equations.  Form 3 is not
available for complex matrices, and does not compute the generalized
eigenvectors @var{V}, @var{W}, nor the orthogonal matrix @var{Q}.

@table @var
@item opt
for ordering eigenvalues of the @nospell{GEP} pencil.  The leading block of
the revised pencil contains all eigenvalues that satisfy:

@table @asis
@item @qcode{"N"}
= unordered (default)

@item @qcode{"S"}
= small: leading block has all |lambda| @leq{} 1

@item @qcode{"B"}
= big: leading block has all |lambda| @geq{} 1

@item @qcode{"-"}
= negative real part: leading block has all eigenvalues
in the open left half-plane

@item @qcode{"+"}
= non-negative real part: leading block has all eigenvalues
in the closed right half-plane
@end table
@end table
@end enumerate

Note: @code{qz} performs permutation balancing, but not scaling
(@pxref{XREFbalance}).  The order of output arguments was selected for
compatibility with @sc{matlab}.
@seealso{eig, balance, lu, chol, hess, qr, qzhess, schur, svd}
@end deftypefn */)
{
  volatile int nargin = args.length ();

#if defined (DEBUG)
  std::cout << "qz: nargin = " << nargin
            << ", nargout = " << nargout << std::endl;
#endif

  if (nargin < 2 || nargin > 3 || nargout > 7)
    print_usage ();

  if (nargin == 3 && (nargout < 3 || nargout > 4))
    error ("qz: invalid number of output arguments for form [3] call");

#if defined (DEBUG)
  std::cout << "qz: determine ordering option" << std::endl;
#endif

  // Determine ordering option.
  volatile char ord_job = 0;
  static double safmin;

  if (nargin == 2)
    ord_job = 'N';
  else
    {
      std::string tmp = args(2).xstring_value ("qz: OPT must be a string");

      if (! tmp.empty ())
        ord_job = tmp[0];

      if (! (ord_job == 'N' || ord_job == 'n'
             || ord_job == 'S' || ord_job == 's'
             || ord_job == 'B' || ord_job == 'b'
             || ord_job == '+' || ord_job == '-'))
        error ("qz: invalid order option");

      // overflow constant required by dlag2
      F77_FUNC (xdlamch, XDLAMCH) (F77_CONST_CHAR_ARG2 ("S", 1),
                                   safmin
                                   F77_CHAR_ARG_LEN (1));

#if defined (DEBUG_EIG)
      std::cout << "qz: initial value of safmin="
                << setiosflags (std::ios::scientific)
                << safmin << std::endl;
#endif

      // Some machines (e.g., DEC alpha) get safmin = 0;
      // for these, use eps instead to avoid problems in dlag2.
      if (safmin == 0)
        {
#if defined (DEBUG_EIG)
          std::cout << "qz: DANGER WILL ROBINSON: safmin is 0!" << std::endl;
#endif

          F77_FUNC (xdlamch, XDLAMCH) (F77_CONST_CHAR_ARG2 ("E", 1),
                                       safmin
                                       F77_CHAR_ARG_LEN (1));

#if defined (DEBUG_EIG)
          std::cout << "qz: safmin set to "
                    << setiosflags (std::ios::scientific)
                    << safmin << std::endl;
#endif
        }
    }

#if defined (DEBUG)
  std::cout << "qz: check argument 1" << std::endl;
#endif

  // Argument 1: check if it's okay dimensioned.
  F77_INT nn = octave::to_f77_int (args(0).rows ());
  F77_INT nc = octave::to_f77_int (args(0).columns ());

#if defined (DEBUG)
  std::cout << "argument 1 dimensions: ("
            << nn << "," << nc << ")"
            << std::endl;
#endif

  octave_value_list retval;

  if (args(0).is_empty ())
    {
      warn_empty_arg ("qz: parameter 1; continuing");
      return octave_value_list (2, Matrix ());
    }
  else if (nc != nn)
    err_square_matrix_required ("qz", "A");

  // Argument 1: dimensions look good; get the value.
  Matrix aa;
  ComplexMatrix caa;

  if (args(0).is_complex_type ())
    caa = args(0).complex_matrix_value ();
  else
    aa = args(0).matrix_value ();

#if defined (DEBUG)
  std::cout << "qz: check argument 2" << std::endl;
#endif

  // Extract argument 2 (bb, or cbb if complex).
  F77_INT b_nr = octave::to_f77_int (args(1).rows ());
  F77_INT b_nc = octave::to_f77_int (args(1).columns ());

  if (nn != b_nc || nn != b_nr)
    err_nonconformant ();

  Matrix bb;
  ComplexMatrix cbb;

  if (args(1).is_complex_type ())
    cbb = args(1).complex_matrix_value ();
  else
    bb = args(1).matrix_value ();

  // Both matrices loaded, now let's check what kind of arithmetic:
  // declared volatile to avoid compiler warnings about long jumps,
  // vforks.

  volatile int complex_case
    = (args(0).is_complex_type () || args(1).is_complex_type ());

  if (nargin == 3 && complex_case)
    error ("qz: cannot re-order complex qz decomposition");

  // First, declare variables used in both the real and complex case.
  Matrix QQ(nn,nn), ZZ(nn,nn), VR(nn,nn), VL(nn,nn);
  RowVector alphar(nn), alphai(nn), betar(nn);
  ComplexRowVector xalpha(nn), xbeta(nn);
  ComplexMatrix CQ(nn,nn), CZ(nn,nn), CVR(nn,nn), CVL(nn,nn);
  F77_INT ilo, ihi, info;
  char compq = (nargout >= 3 ? 'V' : 'N');
  char compz = ((nargout >= 4 || nargin == 3)? 'V' : 'N');

  // Initialize Q, Z to identity if we need either of them.
  if (compq == 'V' || compz == 'V')
    for (F77_INT ii = 0; ii < nn; ii++)
      for (F77_INT jj = 0; jj < nn; jj++)
        {
          octave_quit ();

          QQ(ii,jj) = ZZ(ii,jj) = (ii == jj ? 1.0 : 0.0);
        }

  // Always perform permutation balancing.
  const char bal_job = 'P';
  RowVector lscale (nn), rscale (nn), work (6 * nn), rwork (nn);

  if (complex_case)
    {
#if defined (DEBUG)
      if (compq == 'V')
        std::cout << "qz: performing balancing; CQ=" << std::endl
                  << CQ << std::endl;
#endif
      if (args(0).is_real_type ())
        caa = ComplexMatrix (aa);

      if (args(1).is_real_type ())
        cbb = ComplexMatrix (bb);

      if (compq == 'V')
        CQ = ComplexMatrix (QQ);

      if (compz == 'V')
        CZ = ComplexMatrix (ZZ);

      F77_XFCN (zggbal, ZGGBAL,
                (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                 nn, F77_DBLE_CMPLX_ARG (caa.fortran_vec ()), nn,
                 F77_DBLE_CMPLX_ARG (cbb.fortran_vec ()),
                 nn, ilo, ihi, lscale.fortran_vec (),
                 rscale.fortran_vec (), work.fortran_vec (), info
                 F77_CHAR_ARG_LEN (1)));
    }
  else
    {
#if defined (DEBUG)
      if (compq == 'V')
        std::cout << "qz: performing balancing; QQ=" << std::endl
                  << QQ << std::endl;
#endif

      F77_XFCN (dggbal, DGGBAL,
                (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                 nn, aa.fortran_vec (), nn, bb.fortran_vec (),
                 nn, ilo, ihi, lscale.fortran_vec (),
                 rscale.fortran_vec (), work.fortran_vec (), info
                 F77_CHAR_ARG_LEN (1)));
    }

  // Since we just want the balancing matrices, we can use dggbal
  // for both the real and complex cases; left first

#if 0
  if (compq == 'V')
    {
      F77_XFCN (dggbak, DGGBAK,
                (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                 F77_CONST_CHAR_ARG2 ("L", 1),
                 nn, ilo, ihi, lscale.data (), rscale.data (),
                 nn, QQ.fortran_vec (), nn, info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)));

#if defined (DEBUG)
      if (compq == 'V')
        std::cout << "qz: balancing done; QQ=" << std::endl << QQ << std::endl;
#endif
  }

  // then right
  if (compz == 'V')
    {
      F77_XFCN (dggbak, DGGBAK,
                (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                 F77_CONST_CHAR_ARG2 ("R", 1),
                 nn, ilo, ihi, lscale.data (), rscale.data (),
                 nn, ZZ.fortran_vec (), nn, info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)));

#if defined (DEBUG)
      if (compz == 'V')
        std::cout << "qz: balancing done; ZZ=" << std::endl << ZZ << std::endl;
#endif
    }
#endif

  static char qz_job;
  qz_job = (nargout < 2 ? 'E' : 'S');

  if (complex_case)
    {
      // Complex case.

      // The QR decomposition of cbb.
      octave::math::qr<ComplexMatrix> cbqr (cbb);
      // The R matrix of QR decomposition for cbb.
      cbb = cbqr.R ();
      // (Q*)caa for following work.
      caa = (cbqr.Q ().hermitian ()) * caa;
      CQ = CQ * cbqr.Q ();

      F77_XFCN (zgghrd, ZGGHRD,
                (F77_CONST_CHAR_ARG2 (&compq, 1),
                 F77_CONST_CHAR_ARG2 (&compz, 1),
                 nn, ilo, ihi, F77_DBLE_CMPLX_ARG (caa.fortran_vec ()),
                 nn, F77_DBLE_CMPLX_ARG (cbb.fortran_vec ()), nn,
                 F77_DBLE_CMPLX_ARG (CQ.fortran_vec ()), nn,
                 F77_DBLE_CMPLX_ARG (CZ.fortran_vec ()), nn, info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)));

      ComplexRowVector cwork (1 * nn);

      F77_XFCN (zhgeqz, ZHGEQZ,
                (F77_CONST_CHAR_ARG2 (&qz_job, 1),
                 F77_CONST_CHAR_ARG2 (&compq, 1),
                 F77_CONST_CHAR_ARG2 (&compz, 1),
                 nn, ilo, ihi,
                 F77_DBLE_CMPLX_ARG (caa.fortran_vec ()), nn,
                 F77_DBLE_CMPLX_ARG (cbb.fortran_vec ()), nn,
                 F77_DBLE_CMPLX_ARG (xalpha.fortran_vec ()),
                 F77_DBLE_CMPLX_ARG (xbeta.fortran_vec ()),
                 F77_DBLE_CMPLX_ARG (CQ.fortran_vec ()), nn,
                 F77_DBLE_CMPLX_ARG (CZ.fortran_vec ()), nn,
                 F77_DBLE_CMPLX_ARG (cwork.fortran_vec ()), nn, rwork.fortran_vec (), info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)));

      if (compq == 'V')
        {
          // Left eigenvector.
          F77_XFCN (zggbak, ZGGBAK,
                    (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                     F77_CONST_CHAR_ARG2 ("L", 1),
                     nn, ilo, ihi, lscale.data (), rscale.data (),
                     nn, F77_DBLE_CMPLX_ARG (CQ.fortran_vec ()), nn, info
                     F77_CHAR_ARG_LEN (1)
                     F77_CHAR_ARG_LEN (1)));
        }

      // Right eigenvector.
      if (compz == 'V')
        {
          F77_XFCN (zggbak, ZGGBAK,
                    (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                     F77_CONST_CHAR_ARG2 ("R", 1),
                     nn, ilo, ihi, lscale.data (), rscale.data (),
                     nn, F77_DBLE_CMPLX_ARG (CZ.fortran_vec ()), nn, info
                     F77_CHAR_ARG_LEN (1)
                     F77_CHAR_ARG_LEN (1)));
        }

    }
  else
    {
#if defined (DEBUG)
      std::cout << "qz: peforming qr decomposition of bb" << std::endl;
#endif

      // Compute the QR factorization of bb.
      octave::math::qr<Matrix> bqr (bb);

#if defined (DEBUG)
      std::cout << "qz: qr (bb) done; now peforming qz decomposition"
                << std::endl;
#endif

      bb = bqr.R ();

#if defined (DEBUG)
      std::cout << "qz: extracted bb" << std::endl;
#endif

      aa = (bqr.Q ()).transpose () * aa;

#if defined (DEBUG)
      std::cout << "qz: updated aa " << std::endl;
      std::cout << "bqr.Q () = " << std::endl << bqr.Q () << std::endl;

      if (compq == 'V')
        std::cout << "QQ =" << QQ << std::endl;
#endif

      if (compq == 'V')
        QQ = QQ * bqr.Q ();

#if defined (DEBUG)
      std::cout << "qz: precursors done..." << std::endl;
#endif

#if defined (DEBUG)
      std::cout << "qz: compq = " << compq << ", compz = " << compz
                << std::endl;
#endif

      // Reduce to generalized Hessenberg form.
      F77_XFCN (dgghrd, DGGHRD,
                (F77_CONST_CHAR_ARG2 (&compq, 1),
                 F77_CONST_CHAR_ARG2 (&compz, 1),
                 nn, ilo, ihi, aa.fortran_vec (),
                 nn, bb.fortran_vec (), nn, QQ.fortran_vec (), nn,
                 ZZ.fortran_vec (), nn, info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)));

      // Check if just computing generalized eigenvalues or if we're
      // actually computing the decomposition.

      // Reduce to generalized Schur form.
      F77_XFCN (dhgeqz, DHGEQZ,
                (F77_CONST_CHAR_ARG2 (&qz_job, 1),
                 F77_CONST_CHAR_ARG2 (&compq, 1),
                 F77_CONST_CHAR_ARG2 (&compz, 1),
                 nn, ilo, ihi, aa.fortran_vec (), nn, bb.fortran_vec (),
                 nn, alphar.fortran_vec (), alphai.fortran_vec (),
                 betar.fortran_vec (), QQ.fortran_vec (), nn,
                 ZZ.fortran_vec (), nn, work.fortran_vec (), nn, info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)));

      if (compq == 'V')
        {
          F77_XFCN (dggbak, DGGBAK,
                    (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                     F77_CONST_CHAR_ARG2 ("L", 1),
                     nn, ilo, ihi, lscale.data (), rscale.data (),
                     nn, QQ.fortran_vec (), nn, info
                     F77_CHAR_ARG_LEN (1)
                     F77_CHAR_ARG_LEN (1)));

#if defined (DEBUG)
          if (compq == 'V')
            std::cout << "qz: balancing done; QQ=" << std::endl
                      << QQ << std::endl;
#endif
        }

      // then right
      if (compz == 'V')
        {
          F77_XFCN (dggbak, DGGBAK,
                    (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                     F77_CONST_CHAR_ARG2 ("R", 1),
                     nn, ilo, ihi, lscale.data (), rscale.data (),
                     nn, ZZ.fortran_vec (), nn, info
                     F77_CHAR_ARG_LEN (1)
                     F77_CHAR_ARG_LEN (1)));

#if defined (DEBUG)
          if (compz == 'V')
            std::cout << "qz: balancing done; ZZ=" << std::endl
                      << ZZ << std::endl;
#endif
        }

    }

  // Order the QZ decomposition?
  if (! (ord_job == 'N' || ord_job == 'n'))
    {
      if (complex_case)
        // Probably not needed, but better be safe.
        error ("qz: cannot re-order complex qz decomposition");

#if defined (DEBUG_SORT)
      std::cout << "qz: ordering eigenvalues: ord_job = "
                << ord_job << std::endl;
#endif

      // Declared static to avoid vfork/long jump compiler complaints.
      static sort_function sort_test;
      sort_test = 0;

      switch (ord_job)
        {
        case 'S':
        case 's':
          sort_test = &fin;
          break;

        case 'B':
        case 'b':
          sort_test = &fout;
          break;

        case '+':
          sort_test = &fcrhp;
          break;

        case '-':
          sort_test = &folhp;
          break;

        default:
          // Invalid order option (should never happen, since we
          // checked the options at the top).
          panic_impossible ();
          break;
        }

      double inf_norm;

      F77_XFCN (xdlange, XDLANGE,
                (F77_CONST_CHAR_ARG2 ("I", 1),
                 nn, nn, aa.data (), nn, work.fortran_vec (), inf_norm
                 F77_CHAR_ARG_LEN (1)));

      double eps = std::numeric_limits<double>::epsilon () * inf_norm * nn;

#if defined (DEBUG_SORT)
      std::cout << "qz: calling dsubsp: aa=" << std::endl;
      octave_print_internal (std::cout, aa, 0);
      std::cout << std::endl << "bb="  << std::endl;
      octave_print_internal (std::cout, bb, 0);
      if (compz == 'V')
        {
          std::cout << std::endl << "ZZ="  << std::endl;
          octave_print_internal (std::cout, ZZ, 0);
        }
      std::cout << std::endl;
      std::cout << "alphar = " << std::endl;
      octave_print_internal (std::cout, (Matrix) alphar, 0);
      std::cout << std::endl << "alphai = " << std::endl;
      octave_print_internal (std::cout, (Matrix) alphai, 0);
      std::cout << std::endl << "beta = " << std::endl;
      octave_print_internal (std::cout, (Matrix) betar, 0);
      std::cout << std::endl;
#endif

      Array<F77_INT> ind (dim_vector (nn, 1));

      F77_INT ndim, fail;

      F77_XFCN (dsubsp, DSUBSP,
                (nn, nn, aa.fortran_vec (), bb.fortran_vec (),
                 ZZ.fortran_vec (), sort_test, eps, ndim, fail,
                 ind.fortran_vec ()));

#if defined (DEBUG)
      std::cout << "qz: back from dsubsp: aa=" << std::endl;
      octave_print_internal (std::cout, aa, 0);
      std::cout << std::endl << "bb="  << std::endl;
      octave_print_internal (std::cout, bb, 0);
      if (compz == 'V')
        {
          std::cout << std::endl << "ZZ="  << std::endl;
          octave_print_internal (std::cout, ZZ, 0);
        }
      std::cout << std::endl;
#endif

      // Manually update alphar, alphai, betar.
      static F77_INT jj;

      jj = 0;
      while (jj < nn)
        {
#if defined (DEBUG_EIG)
          std::cout << "computing gen eig #" << jj << std::endl;
#endif

          // Number of zeros in this block.
          static F77_INT zcnt;

          if (jj == (nn-1))
            zcnt = 1;
          else if (aa(jj+1,jj) == 0)
            zcnt = 1;
          else zcnt = 2;

          if (zcnt == 1)
            {
              // Real zero.
#if defined (DEBUG_EIG)
              std::cout << "  single gen eig:" << std::endl;
              std::cout << "  alphar(" << jj << ") = " << aa(jj,jj)
                        << std::endl;
              std::cout << "  betar(" << jj << ") = " << bb(jj,jj)
                        << std::endl;
              std::cout << "  alphai(" << jj << ") = 0" << std::endl;
#endif

              alphar(jj) = aa(jj,jj);
              alphai(jj) = 0;
              betar(jj) = bb(jj,jj);
            }
          else
            {
              // Complex conjugate pair.
#if defined (DEBUG_EIG)
              std::cout << "qz: calling dlag2:" << std::endl;
              std::cout << "safmin="
                        << setiosflags (std::ios::scientific)
                        << safmin << std::endl;

              for (F77_INT idr = jj; idr <= jj+1; idr++)
                {
                  for (F77_INT idc = jj; idc <= jj+1; idc++)
                    {
                      std::cout << "aa(" << idr << "," << idc << ")="
                                << aa(idr,idc) << std::endl;
                      std::cout << "bb(" << idr << "," << idc << ")="
                                << bb(idr,idc) << std::endl;
                    }
                }
#endif

              // FIXME: probably should be using
              // fortran_vec instead of &aa(jj,jj) here.

              double scale1, scale2, wr1, wr2, wi;
              const double *aa_ptr = aa.data () + jj * nn + jj;
              const double *bb_ptr = bb.data () + jj * nn + jj;
              F77_XFCN (dlag2, DLAG2,
                        (aa_ptr, nn, bb_ptr, nn, safmin,
                         scale1, scale2, wr1, wr2, wi));

#if defined (DEBUG_EIG)
              std::cout << "dlag2 returns: scale1=" << scale1
                        << "\tscale2=" << scale2 << std::endl
                        << "\twr1=" << wr1 << "\twr2=" << wr2
                        << "\twi=" << wi << std::endl;
#endif

              // Just to be safe, check if it's a real pair.
              if (wi == 0)
                {
                  alphar(jj) = wr1;
                  alphai(jj) = 0;
                  betar(jj) = scale1;
                  alphar(jj+1) = wr2;
                  alphai(jj+1) = 0;
                  betar(jj+1) = scale2;
                }
              else
                {
                  alphar(jj) = alphar(jj+1) = wr1;
                  alphai(jj) = -(alphai(jj+1) = wi);
                  betar(jj)  = betar(jj+1) = scale1;
                }
            }

          // Advance past this block.
          jj += zcnt;
        }

#if defined (DEBUG_SORT)
      std::cout << "qz: back from dsubsp: aa=" << std::endl;
      octave_print_internal (std::cout, aa, 0);
      std::cout << std::endl << "bb="  << std::endl;
      octave_print_internal (std::cout, bb, 0);

      if (compz == 'V')
        {
          std::cout << std::endl << "ZZ="  << std::endl;
          octave_print_internal (std::cout, ZZ, 0);
        }
      std::cout << std::endl << "qz: ndim=" << ndim << std::endl
                << "fail=" << fail << std::endl;
      std::cout << "alphar = " << std::endl;
      octave_print_internal (std::cout, (Matrix) alphar, 0);
      std::cout << std::endl << "alphai = " << std::endl;
      octave_print_internal (std::cout, (Matrix) alphai, 0);
      std::cout << std::endl << "beta = " << std::endl;
      octave_print_internal (std::cout, (Matrix) betar, 0);
      std::cout << std::endl;
#endif
    }

  // Compute generalized eigenvalues?
  ComplexColumnVector gev;

  if (nargout < 2 || nargout == 7 || (nargin == 3 && nargout == 4))
    {
      if (complex_case)
        {
          F77_INT cnt = 0;

          for (F77_INT ii = 0; ii < nn; ii++)
            cnt++;

          ComplexColumnVector tmp (cnt);

          cnt = 0;
          for (F77_INT ii = 0; ii < nn; ii++)
            tmp(cnt++) = xalpha(ii) / xbeta(ii);

          gev = tmp;
        }
      else
        {
#if defined (DEBUG)
          std::cout << "qz: computing generalized eigenvalues" << std::endl;
#endif

          // Return finite generalized eigenvalues.
          F77_INT cnt = 0;

          for (F77_INT ii = 0; ii < nn; ii++)
            if (betar(ii) != 0)
              cnt++;

          ComplexColumnVector tmp (cnt);

          cnt = 0;
          for (F77_INT ii = 0; ii < nn; ii++)
            if (betar(ii) != 0)
              tmp(cnt++) = Complex(alphar(ii), alphai(ii))/betar(ii);

          gev = tmp;
        }
    }

  // Right, left eigenvector matrices.
  if (nargout >= 5)
    {
      // Which side to compute?
      char side = (nargout == 5 ? 'R' : 'B');
      // Compute all of them and backtransform
      char howmny = 'B';
      // Dummy pointer; select is not used.
      F77_INT *select = 0;

      if (complex_case)
        {
          CVL = CQ;
          CVR = CZ;
          ComplexRowVector cwork2 (2 * nn);
          RowVector rwork2 (8 * nn);
          F77_INT m;

          F77_XFCN (ztgevc, ZTGEVC,
                    (F77_CONST_CHAR_ARG2 (&side, 1),
                     F77_CONST_CHAR_ARG2 (&howmny, 1),
                     select, nn, F77_DBLE_CMPLX_ARG (caa.fortran_vec ()), nn,
                     F77_DBLE_CMPLX_ARG (cbb.fortran_vec ()),
                     nn, F77_DBLE_CMPLX_ARG (CVL.fortran_vec ()), nn,
                     F77_DBLE_CMPLX_ARG (CVR.fortran_vec ()), nn, nn,
                     m, F77_DBLE_CMPLX_ARG (cwork2.fortran_vec ()), rwork2.fortran_vec (), info
                     F77_CHAR_ARG_LEN (1)
                     F77_CHAR_ARG_LEN (1)));
        }
      else
        {
#if defined (DEBUG)
          std::cout << "qz: computing generalized eigenvectors" << std::endl;
#endif

          VL = QQ;
          VR = ZZ;
          F77_INT m;

          F77_XFCN (dtgevc, DTGEVC,
                    (F77_CONST_CHAR_ARG2 (&side, 1),
                     F77_CONST_CHAR_ARG2 (&howmny, 1),
                     select, nn, aa.fortran_vec (), nn, bb.fortran_vec (),
                     nn, VL.fortran_vec (), nn, VR.fortran_vec (), nn, nn,
                     m, work.fortran_vec (), info
                     F77_CHAR_ARG_LEN (1)
                     F77_CHAR_ARG_LEN (1)));

          // Now construct the complex form of VV, WW.
          F77_INT jj = 0;

          while (jj < nn)
            {
              octave_quit ();

              // See if real or complex eigenvalue.

              // Column increment; assume complex eigenvalue.
              int cinc = 2;

              if (jj == (nn-1))
                // Single column.
                cinc = 1;
              else if (aa(jj+1,jj) == 0)
                cinc = 1;

              // Now copy the eigenvector (s) to CVR, CVL.
              if (cinc == 1)
                {
                  for (F77_INT ii = 0; ii < nn; ii++)
                    CVR(ii,jj) = VR(ii,jj);

                  if (side == 'B')
                    for (F77_INT ii = 0; ii < nn; ii++)
                      CVL(ii,jj) = VL(ii,jj);
                }
              else
                {
                  // Double column; complex vector.

                  for (F77_INT ii = 0; ii < nn; ii++)
                    {
                      CVR(ii,jj) = Complex (VR(ii,jj), VR(ii,jj+1));
                      CVR(ii,jj+1) = Complex (VR(ii,jj), -VR(ii,jj+1));
                    }

                  if (side == 'B')
                    for (F77_INT ii = 0; ii < nn; ii++)
                      {
                        CVL(ii,jj) = Complex (VL(ii,jj), VL(ii,jj+1));
                        CVL(ii,jj+1) = Complex (VL(ii,jj), -VL(ii,jj+1));
                      }
                }

              // Advance to next eigenvectors (if any).
              jj += cinc;
            }
        }
    }

  switch (nargout)
    {
    case 7:
      retval(6) = gev;

    case 6:
      // Return eigenvectors.
      retval(5) = CVL;

    case 5:
      // Return eigenvectors.
      retval(4) = CVR;

    case 4:
      if (nargin == 3)
        {
#if defined (DEBUG)
          std::cout << "qz: sort: retval(3) = gev = " << std::endl;
          octave_print_internal (std::cout, gev);
          std::cout << std::endl;
#endif
          retval(3) = gev;
        }
      else
        {
          if (complex_case)
            retval(3) = CZ;
          else
            retval(3) = ZZ;
        }

    case 3:
      if (nargin == 3)
        {
          if (complex_case)
            retval(2) = CZ;
          else
            retval(2) = ZZ;
        }
      else
        {
          if (complex_case)
            retval(2) = CQ.hermitian ();
          else
            retval(2) = QQ.transpose ();
        }

    case 2:
      {
        if (complex_case)
          {
#if defined (DEBUG)
            std::cout << "qz: retval(1) = cbb = " << std::endl;
            octave_print_internal (std::cout, cbb, 0);
            std::cout << std::endl << "qz: retval(0) = caa = " <<std::endl;
            octave_print_internal (std::cout, caa, 0);
            std::cout << std::endl;
#endif
            retval(1) = cbb;
            retval(0) = caa;
          }
        else
          {
#if defined (DEBUG)
            std::cout << "qz: retval(1) = bb = " << std::endl;
            octave_print_internal (std::cout, bb, 0);
            std::cout << std::endl << "qz: retval(0) = aa = " <<std::endl;
            octave_print_internal (std::cout, aa, 0);
            std::cout << std::endl;
#endif
            retval(1) = bb;
            retval(0) = aa;
          }
      }
      break;

    case 1:
    case 0:
#if defined (DEBUG)
      std::cout << "qz: retval(0) = gev = " << gev << std::endl;
#endif
      retval(0) = gev;
      break;

    default:
      error ("qz: too many return arguments");
      break;
  }

#if defined (DEBUG)
  std::cout << "qz: exiting (at long last)" << std::endl;
#endif

  return retval;
}

/*
%!shared a, b, c
%! a = [1 2; 0 3];
%! b = [1 0; 0 0];
%! c = [0 1; 0 0];
%!assert (qz (a,b), 1)
%!assert (isempty (qz (a,c)))

## Exaple 7.7.3 in Golub & Van Loan
%!test
%! a = [ 10  1  2;
%!        1  2 -1;
%!        1  1  2];
%! b = reshape (1:9,3,3);
%! [aa, bb, q, z, v, w, lambda] = qz (a, b);
%! sz = length (lambda);
%! observed = (b * v * diag ([lambda;0])) (:, 1:sz);
%! assert ((a*v)(:, 1:sz), observed, norm (observed) * 1e-14);
%! observed = (diag ([lambda;0]) * w' * b) (1:sz, :);
%! assert ((w'*a)(1:sz, :) , observed, norm (observed) * 1e-13);
%! assert (q * a * z, aa, norm (aa) * 1e-14);
%! assert (q * b * z, bb, norm (bb) * 1e-14);

%!test
%! A = [0, 0, -1, 0; 1, 0, 0, 0; -1, 0, -2, -1; 0, -1, 1, 0];
%! B = [0, 0, 0, 0; 0, 1, 0, 0; 0, 0, 1, 0; 0, 0, 0, 1];
%! [AA, BB, Q, Z1] = qz (A, B);
%! [AA, BB, Z2] = qz (A, B, '-');
%! assert (Z1, Z2);
*/
