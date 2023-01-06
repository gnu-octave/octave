////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1998-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

// Generalized eigenvalue balancing via LAPACK

// Originally written by A. S. Hodel <scotte@eng.auburn.edu>, but is
// substantially different with the change to use LAPACK.

#undef DEBUG
#undef DEBUG_SORT
#undef DEBUG_EIG

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cctype>
#include <cmath>

#if defined (DEBUG_EIG)
#  include <iomanip>
#endif

#include "f77-fcn.h"
#include "lo-lapack-proto.h"
#include "qr.h"
#include "quit.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#if defined (DEBUG) || defined (DEBUG_SORT)
#  include "pager.h"
#  include "pr-output.h"
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

// FIXME: Matlab does not produce lambda as the first output argument.
// Compatibility problem?

DEFUN (qz, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{lambda} =} qz (@var{A}, @var{B})
@deftypefnx {} {[@var{AA}, @var{BB}, @var{Q}, @var{Z}, @var{V}, @var{W}, @var{lambda}] =} qz (@var{A}, @var{B})
@deftypefnx {} {[@var{AA}, @var{BB}, @var{Z}] =} qz (@var{A}, @var{B}, @var{opt})
@deftypefnx {} {[@var{AA}, @var{BB}, @var{Z}, @var{lambda}] =} qz (@var{A}, @var{B}, @var{opt})
Compute the QZ@tie{}decomposition of a generalized eigenvalue problem.

The generalized eigenvalue problem is defined as

@tex
$$A x = \lambda B x$$
@end tex
@ifnottex

@math{A x = @var{lambda} B x}

@end ifnottex

There are three calling forms of the function:

@enumerate
@item @code{@var{lambda} = qz (@var{A}, @var{B})}

Compute the generalized eigenvalues
@tex
$\lambda.$
@end tex
@ifnottex
@var{lambda}.
@end ifnottex

@item @code{[@var{AA}, @var{BB}, @var{Q}, @var{Z}, @var{V}, @var{W}, @var{lambda}] = qz (@var{A}, @var{B})}

Compute QZ@tie{}decomposition, generalized eigenvectors, and generalized
eigenvalues.
@tex
$$ AA = Q^T AZ, BB = Q^T BZ $$
$$ AV = BV{ \rm diag }(\lambda) $$
$$ W^T A = { \rm diag }(\lambda)W^T B $$
@end tex
@ifnottex

@example
@group

@var{AA} = @var{Q} * @var{A} * @var{Z}, @var{BB} = @var{Q} * @var{B} * @var{Z}
@var{A} * @var{V} = @var{B} * @var{V} * diag (@var{lambda})
@var{W}' * @var{A} = diag (@var{lambda}) * @var{W}' * @var{B}

@end group
@end example

@end ifnottex
with @var{Q} and @var{Z} orthogonal (unitary for complex case).

@item @code{[@var{AA}, @var{BB}, @var{Z} @{, @var{lambda}@}] = qz (@var{A}, @var{B}, @var{opt})}

As in form 2 above, but allows ordering of generalized eigenpairs for, e.g.,
solution of discrete time algebraic @nospell{Riccati} equations.  Form 3 is not
available for complex matrices, and does not compute the generalized
eigenvectors @var{V}, @var{W}, nor the orthogonal matrix @var{Q}.

@table @var
@item opt
for ordering eigenvalues of the @nospell{GEP} pencil.  The leading block of
the revised pencil contains all eigenvalues that satisfy:

@table @asis
@item @qcode{"N"}
unordered (default)

@item @qcode{"S"}
small: leading block has all
@tex
$|\lambda| < 1$
@end tex
@ifnottex
|@var{lambda}| < 1
@end ifnottex

@item @qcode{"B"}
big: leading block has all
@tex
$|\lambda| \geq 1$
@end tex
@ifnottex
|@var{lambda}| @geq{} 1
@end ifnottex

@item @qcode{"-"}
negative real part: leading block has all eigenvalues in the open left
half-plane

@item @qcode{"+"}
non-negative real part: leading block has all eigenvalues in the closed right
half-plane
@end table
@end table
@end enumerate

Note: @code{qz} performs permutation balancing, but not scaling
(@pxref{XREFbalance,,@code{balance}}), which may be lead to less accurate
results than @code{eig}.  The order of output arguments was selected for
compatibility with @sc{matlab}.
@seealso{eig, gsvd, balance, chol, hess, lu, qr, qzhess, schur}
@end deftypefn */)
{
  int nargin = args.length ();

#if defined (DEBUG)
  octave_stdout << "qz: nargin = " << nargin
                << ", nargout = " << nargout << std::endl;
#endif

  if (nargin < 2 || nargin > 3 || nargout > 7)
    print_usage ();

  if (nargin == 3 && (nargout < 3 || nargout > 4))
    error ("qz: invalid number of output arguments for form 3 call");

#if defined (DEBUG)
  octave_stdout << "qz: determine ordering option" << std::endl;
#endif

  // Determine ordering option.

  char ord_job = 'N';
  double safmin = 0.0;

  if (nargin == 3)
    {
      std::string opt = args(2).xstring_value ("qz: OPT must be a string");

      if (opt.empty ())
        error ("qz: OPT must be a non-empty string");

      ord_job = std::toupper (opt[0]);

      std::string valid_opts = "NSB+-";

      if (valid_opts.find_first_of (ord_job) == std::string::npos)
        error ("qz: invalid order option '%c'", ord_job);

      // overflow constant required by dlag2
      F77_FUNC (xdlamch, XDLAMCH) (F77_CONST_CHAR_ARG2 ("S", 1),
                                   safmin
                                   F77_CHAR_ARG_LEN (1));

#if defined (DEBUG_EIG)
      octave_stdout << "qz: initial value of safmin="
                    << setiosflags (std::ios::scientific)
                    << safmin << std::endl;
#endif

      // Some machines (e.g., DEC alpha) get safmin = 0;
      // for these, use eps instead to avoid problems in dlag2.
      if (safmin == 0)
        {
#if defined (DEBUG_EIG)
          octave_stdout << "qz: DANGER WILL ROBINSON: safmin is 0!"
                        << std::endl;
#endif

          F77_FUNC (xdlamch, XDLAMCH) (F77_CONST_CHAR_ARG2 ("E", 1),
                                       safmin
                                       F77_CHAR_ARG_LEN (1));

#if defined (DEBUG_EIG)
          octave_stdout << "qz: safmin set to "
                        << setiosflags (std::ios::scientific)
                        << safmin << std::endl;
#endif
        }
    }

#if defined (DEBUG)
  octave_stdout << "qz: check matrix A" << std::endl;
#endif

  // Matrix A: check dimensions.
  F77_INT nn = to_f77_int (args(0).rows ());
  F77_INT nc = to_f77_int (args(0).columns ());

#if defined (DEBUG)
  octave_stdout << "Matrix A dimensions: (" << nn << ',' << nc << ')'
                << std::endl;
#endif

  if (args(0).isempty ())
    {
      warn_empty_arg ("qz: A");
      return octave_value_list (2, Matrix ());
    }
  else if (nc != nn)
    err_square_matrix_required ("qz", "A");

  // Matrix A: get value.
  Matrix aa;
  ComplexMatrix caa;

  if (args(0).iscomplex ())
    caa = args(0).complex_matrix_value ();
  else
    aa = args(0).matrix_value ();

#if defined (DEBUG)
  octave_stdout << "qz: check matrix B" << std::endl;
#endif

  // Extract argument 2 (bb, or cbb if complex).
  F77_INT b_nr = to_f77_int (args(1).rows ());
  F77_INT b_nc = to_f77_int (args(1).columns ());

  if (nn != b_nc || nn != b_nr)
    ::err_nonconformant ();

  Matrix bb;
  ComplexMatrix cbb;

  if (args(1).iscomplex ())
    cbb = args(1).complex_matrix_value ();
  else
    bb = args(1).matrix_value ();

  // Both matrices loaded, now check whether to calculate complex or real val.

  bool complex_case = (args(0).iscomplex () || args(1).iscomplex ());

  if (nargin == 3 && complex_case)
    error ("qz: cannot re-order complex qz decomposition");

  // First, declare variables used in both the real and complex cases.
  // FIXME: There are a lot of excess variables declared.
  //        Probably a better way to handle this.
  Matrix QQ (nn, nn), ZZ (nn, nn), VR (nn, nn), VL (nn, nn);
  RowVector alphar (nn), alphai (nn), betar (nn);
  ComplexRowVector xalpha (nn), xbeta (nn);
  ComplexMatrix CQ (nn, nn), CZ (nn, nn), CVR (nn, nn), CVL (nn, nn);
  F77_INT ilo, ihi, info;
  char comp_q = (nargout >= 3 ? 'V' : 'N');
  char comp_z = ((nargout >= 4 || nargin == 3)? 'V' : 'N');

  // Initialize Q, Z to identity matrix if either is needed
  if (comp_q == 'V' || comp_z == 'V')
    {
      double *QQptr = QQ.fortran_vec ();
      double *ZZptr = ZZ.fortran_vec ();
      std::fill_n (QQptr, QQ.numel (), 0.0);
      std::fill_n (ZZptr, ZZ.numel (), 0.0);
      for (F77_INT i = 0; i < nn; i++)
        {
          QQ(i, i) = 1.0;
          ZZ(i, i) = 1.0;
        }
    }

  // Always perform permutation balancing.
  const char bal_job = 'P';
  RowVector lscale (nn), rscale (nn), work (6 * nn), rwork (nn);

  if (complex_case)
    {
#if defined (DEBUG)
      if (comp_q == 'V')
        octave_stdout << "qz: performing balancing; CQ =\n" << CQ << std::endl;
#endif
      if (args(0).isreal ())
        caa = ComplexMatrix (aa);

      if (args(1).isreal ())
        cbb = ComplexMatrix (bb);

      if (comp_q == 'V')
        CQ = ComplexMatrix (QQ);

      if (comp_z == 'V')
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
      if (comp_q == 'V')
        octave_stdout << "qz: performing balancing; QQ =\n" << QQ << std::endl;
#endif

      F77_XFCN (dggbal, DGGBAL,
                (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                 nn, aa.fortran_vec (), nn, bb.fortran_vec (),
                 nn, ilo, ihi, lscale.fortran_vec (),
                 rscale.fortran_vec (), work.fortran_vec (), info
                 F77_CHAR_ARG_LEN (1)));
    }

  // Only permutation balance above is done.  Skip scaling balance.

#if 0
  // Since we just want the balancing matrices, we can use dggbal
  // for both the real and complex cases; left first

  if (comp_q == 'V')
    {
      F77_XFCN (dggbak, DGGBAK,
                (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                 F77_CONST_CHAR_ARG2 ("L", 1),
                 nn, ilo, ihi, lscale.data (), rscale.data (),
                 nn, QQ.fortran_vec (), nn, info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)));

#if defined (DEBUG)
      if (comp_q == 'V')
        octave_stdout << "qz: balancing done; QQ =\n" << QQ << std::endl;
#endif
    }

  // then right
  if (comp_z == 'V')
    {
      F77_XFCN (dggbak, DGGBAK,
                (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                 F77_CONST_CHAR_ARG2 ("R", 1),
                 nn, ilo, ihi, lscale.data (), rscale.data (),
                 nn, ZZ.fortran_vec (), nn, info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)));

#if defined (DEBUG)
      if (comp_z == 'V')
        octave_stdout << "qz: balancing done; ZZ=\n" << ZZ << std::endl;
#endif
    }
#endif

  char qz_job = (nargout < 2 ? 'E' : 'S');

  if (complex_case)
    {
      // Complex case.

      // The QR decomposition of cbb.
      math::qr<ComplexMatrix> cbqr (cbb);
      // The R matrix of QR decomposition for cbb.
      cbb = cbqr.R ();
      // (Q*)caa for following work.
      caa = (cbqr.Q ().hermitian ()) * caa;
      CQ = CQ * cbqr.Q ();

      F77_XFCN (zgghrd, ZGGHRD,
                (F77_CONST_CHAR_ARG2 (&comp_q, 1),
                 F77_CONST_CHAR_ARG2 (&comp_z, 1),
                 nn, ilo, ihi, F77_DBLE_CMPLX_ARG (caa.fortran_vec ()),
                 nn, F77_DBLE_CMPLX_ARG (cbb.fortran_vec ()), nn,
                 F77_DBLE_CMPLX_ARG (CQ.fortran_vec ()), nn,
                 F77_DBLE_CMPLX_ARG (CZ.fortran_vec ()), nn, info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)));

      ComplexRowVector cwork (nn);

      F77_XFCN (zhgeqz, ZHGEQZ,
                (F77_CONST_CHAR_ARG2 (&qz_job, 1),
                 F77_CONST_CHAR_ARG2 (&comp_q, 1),
                 F77_CONST_CHAR_ARG2 (&comp_z, 1),
                 nn, ilo, ihi,
                 F77_DBLE_CMPLX_ARG (caa.fortran_vec ()), nn,
                 F77_DBLE_CMPLX_ARG (cbb.fortran_vec ()), nn,
                 F77_DBLE_CMPLX_ARG (xalpha.fortran_vec ()),
                 F77_DBLE_CMPLX_ARG (xbeta.fortran_vec ()),
                 F77_DBLE_CMPLX_ARG (CQ.fortran_vec ()), nn,
                 F77_DBLE_CMPLX_ARG (CZ.fortran_vec ()), nn,
                 F77_DBLE_CMPLX_ARG (cwork.fortran_vec ()), nn,
                 rwork.fortran_vec (), info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)));

      if (comp_q == 'V')
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

      if (comp_z == 'V')
        {
          // Right eigenvector.
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
      octave_stdout << "qz: performing qr decomposition of bb" << std::endl;
#endif

      // Compute the QR factorization of bb.
      math::qr<Matrix> bqr (bb);

#if defined (DEBUG)
      octave_stdout << "qz: qr (bb) done; now performing qz decomposition"
                    << std::endl;
#endif

      bb = bqr.R ();

#if defined (DEBUG)
      octave_stdout << "qz: extracted bb" << std::endl;
#endif

      aa = (bqr.Q ()).transpose () * aa;

#if defined (DEBUG)
      octave_stdout << "qz: updated aa " << std::endl;
      octave_stdout << "bqr.Q () =\n" << bqr.Q () << std::endl;

      if (comp_q == 'V')
        octave_stdout << "QQ =" << QQ << std::endl;
#endif

      if (comp_q == 'V')
        QQ = QQ * bqr.Q ();

#if defined (DEBUG)
      octave_stdout << "qz: precursors done..." << std::endl;
#endif

#if defined (DEBUG)
      octave_stdout << "qz: comp_q = " << comp_q << ", comp_z = " << comp_z
                    << std::endl;
#endif

      // Reduce to generalized Hessenberg form.
      F77_XFCN (dgghrd, DGGHRD,
                (F77_CONST_CHAR_ARG2 (&comp_q, 1),
                 F77_CONST_CHAR_ARG2 (&comp_z, 1),
                 nn, ilo, ihi, aa.fortran_vec (),
                 nn, bb.fortran_vec (), nn, QQ.fortran_vec (), nn,
                 ZZ.fortran_vec (), nn, info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)));

      // Check if just computing generalized eigenvalues,
      // or if we're actually computing the decomposition.

      // Reduce to generalized Schur form.
      F77_XFCN (dhgeqz, DHGEQZ,
                (F77_CONST_CHAR_ARG2 (&qz_job, 1),
                 F77_CONST_CHAR_ARG2 (&comp_q, 1),
                 F77_CONST_CHAR_ARG2 (&comp_z, 1),
                 nn, ilo, ihi, aa.fortran_vec (), nn, bb.fortran_vec (),
                 nn, alphar.fortran_vec (), alphai.fortran_vec (),
                 betar.fortran_vec (), QQ.fortran_vec (), nn,
                 ZZ.fortran_vec (), nn, work.fortran_vec (), nn, info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)));

      if (comp_q == 'V')
        {
          F77_XFCN (dggbak, DGGBAK,
                    (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                     F77_CONST_CHAR_ARG2 ("L", 1),
                     nn, ilo, ihi, lscale.data (), rscale.data (),
                     nn, QQ.fortran_vec (), nn, info
                     F77_CHAR_ARG_LEN (1)
                     F77_CHAR_ARG_LEN (1)));

#if defined (DEBUG)
          if (comp_q == 'V')
            octave_stdout << "qz: balancing done; QQ=\n" << QQ << std::endl;
#endif
        }

      // then right
      if (comp_z == 'V')
        {
          F77_XFCN (dggbak, DGGBAK,
                    (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                     F77_CONST_CHAR_ARG2 ("R", 1),
                     nn, ilo, ihi, lscale.data (), rscale.data (),
                     nn, ZZ.fortran_vec (), nn, info
                     F77_CHAR_ARG_LEN (1)
                     F77_CHAR_ARG_LEN (1)));

#if defined (DEBUG)
          if (comp_z == 'V')
            octave_stdout << "qz: balancing done; ZZ=\n" << ZZ << std::endl;
#endif
        }

    }

  // Order the QZ decomposition?
  if (ord_job != 'N')
    {
      if (complex_case)
        // Probably not needed, but better be safe.
        error ("qz: cannot re-order complex QZ decomposition");

#if defined (DEBUG_SORT)
      octave_stdout << "qz: ordering eigenvalues: ord_job = "
                    << ord_job << std::endl;
#endif

      Array<F77_LOGICAL> select (dim_vector (nn, 1));

      for (int j = 0; j < nn; j++)
        {
          switch (ord_job)
            {
            case 'S':
              select(j) = alphar(j)*alphar(j) + alphai(j)*alphai(j) < betar(j)*betar(j);
              break;

            case 'B':
              select(j) = alphar(j)*alphar(j) + alphai(j)*alphai(j) >= betar(j)*betar(j);
              break;

            case '+':
              select(j) = alphar(j) * betar(j) >= 0;
              break;

            case '-':
              select(j) = alphar(j) * betar(j) < 0;
              break;

            default:
              // Invalid order option
              // (should never happen since options were checked at the top).
              panic_impossible ();
              break;
            }
        }

      F77_LOGICAL wantq = 0, wantz = (comp_z == 'V');
      F77_INT ijob = 0, mm, lrwork3 = 4*nn+16, liwork = nn;
      F77_DBLE pl, pr;
      RowVector rwork3(lrwork3);
      Array<F77_INT> iwork (dim_vector (liwork, 1));

      F77_XFCN (dtgsen, DTGSEN,
                (ijob, wantq, wantz,
                 select.fortran_vec (), nn,
                 aa.fortran_vec (), nn,
                 bb.fortran_vec (), nn,
                 alphar.fortran_vec (),
                 alphai.fortran_vec (),
                 betar.fortran_vec (),
                 nullptr, nn,
                 ZZ.fortran_vec (), nn,
                 mm,
                 pl, pr,
                 nullptr,
                 rwork3.fortran_vec (), lrwork3,
                 iwork.fortran_vec (), liwork,
                 info));

#if defined (DEBUG_SORT)
      octave_stdout << "qz: back from dtgsen: aa =\n";
      octave_print_internal (octave_stdout, aa);
      octave_stdout << "\nbb =\n";
      octave_print_internal (octave_stdout, bb);
      if (comp_z == 'V')
        {
          octave_stdout << "\nZZ =\n";
          octave_print_internal (octave_stdout, ZZ);
        }
      octave_stdout << "\nqz: info=" << info;
      octave_stdout << "\nalphar =\n";
      octave_print_internal (octave_stdout, Matrix (alphar));
      octave_stdout << "\nalphai =\n";
      octave_print_internal (octave_stdout, Matrix (alphai));
      octave_stdout << "\nbeta =\n";
      octave_print_internal (octave_stdout, Matrix (betar));
      octave_stdout << std::endl;
#endif
    }

  // Compute the generalized eigenvalues as well?
  ComplexColumnVector gev;

  if (nargout < 2 || nargout == 7 || (nargin == 3 && nargout == 4))
    {
      if (complex_case)
        {
          ComplexColumnVector tmp (nn);

          for (F77_INT i = 0; i < nn; i++)
            tmp(i) = xalpha(i) / xbeta(i);

          gev = tmp;
        }
      else
        {
#if defined (DEBUG)
          octave_stdout << "qz: computing generalized eigenvalues" << std::endl;
#endif

          // Return finite generalized eigenvalues.
          ComplexColumnVector tmp (nn);

          for (F77_INT i = 0; i < nn; i++)
            {
              if (betar(i) != 0)
                tmp(i) = Complex (alphar(i), alphai(i)) / betar(i);
              else
                tmp(i) = numeric_limits<double>::Inf ();
            }

          gev = tmp;
        }
    }

  // Right, left eigenvector matrices.
  if (nargout >= 5)
    {
      // Which side to compute?
      char side = (nargout == 5 ? 'R' : 'B');
      // Compute all of them and backtransform
      char howmany = 'B';
      // Dummy pointer; select is not used.
      F77_INT *select = nullptr;

      if (complex_case)
        {
          CVL = CQ;
          CVR = CZ;
          ComplexRowVector cwork2 (2 * nn);
          RowVector rwork2 (8 * nn);
          F77_INT m;

          F77_XFCN (ztgevc, ZTGEVC,
                    (F77_CONST_CHAR_ARG2 (&side, 1),
                     F77_CONST_CHAR_ARG2 (&howmany, 1),
                     select, nn, F77_DBLE_CMPLX_ARG (caa.fortran_vec ()), nn,
                     F77_DBLE_CMPLX_ARG (cbb.fortran_vec ()),
                     nn, F77_DBLE_CMPLX_ARG (CVL.fortran_vec ()), nn,
                     F77_DBLE_CMPLX_ARG (CVR.fortran_vec ()), nn, nn,
                     m, F77_DBLE_CMPLX_ARG (cwork2.fortran_vec ()),
                     rwork2.fortran_vec (), info
                     F77_CHAR_ARG_LEN (1)
                     F77_CHAR_ARG_LEN (1)));
        }
      else
        {
#if defined (DEBUG)
          octave_stdout << "qz: computing generalized eigenvectors" << std::endl;
#endif

          VL = QQ;
          VR = ZZ;
          F77_INT m;

          F77_XFCN (dtgevc, DTGEVC,
                    (F77_CONST_CHAR_ARG2 (&side, 1),
                     F77_CONST_CHAR_ARG2 (&howmany, 1),
                     select, nn, aa.fortran_vec (), nn, bb.fortran_vec (),
                     nn, VL.fortran_vec (), nn, VR.fortran_vec (), nn, nn,
                     m, work.fortran_vec (), info
                     F77_CHAR_ARG_LEN (1)
                     F77_CHAR_ARG_LEN (1)));

          // Now construct the complex form of VV, WW.
          F77_INT j = 0;

          while (j < nn)
            {
              octave_quit ();

              // See if real or complex eigenvalue.

              // Column increment; assume complex eigenvalue.
              int cinc = 2;

              if (j == (nn-1))
                // Single column.
                cinc = 1;
              else if (aa(j+1, j) == 0)
                cinc = 1;

              // Now copy the eigenvector (s) to CVR, CVL.
              if (cinc == 1)
                {
                  for (F77_INT i = 0; i < nn; i++)
                    CVR(i, j) = VR(i, j);

                  if (side == 'B')
                    for (F77_INT i = 0; i < nn; i++)
                      CVL(i, j) = VL(i, j);
                }
              else
                {
                  // Double column; complex vector.

                  for (F77_INT i = 0; i < nn; i++)
                    {
                      CVR(i, j) = Complex (VR(i, j), VR(i, j+1));
                      CVR(i, j+1) = Complex (VR(i, j), -VR(i, j+1));
                    }

                  if (side == 'B')
                    for (F77_INT i = 0; i < nn; i++)
                      {
                        CVL(i, j) = Complex (VL(i, j), VL(i, j+1));
                        CVL(i, j+1) = Complex (VL(i, j), -VL(i, j+1));
                      }
                }

              // Advance to next eigenvectors (if any).
              j += cinc;
            }
        }
    }

  octave_value_list retval (nargout);

  switch (nargout)
    {
    case 7:
      retval(6) = gev;
      OCTAVE_FALLTHROUGH;

    case 6:
      // Return left eigenvectors.
      retval(5) = CVL;
      OCTAVE_FALLTHROUGH;

    case 5:
      // Return right eigenvectors.
      retval(4) = CVR;
      OCTAVE_FALLTHROUGH;

    case 4:
      if (nargin == 3)
        {
#if defined (DEBUG)
          octave_stdout << "qz: sort: retval(3) = gev =\n";
          octave_print_internal (octave_stdout, ComplexMatrix (gev));
          octave_stdout << std::endl;
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
      OCTAVE_FALLTHROUGH;

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
      OCTAVE_FALLTHROUGH;

    case 2:
      {
        if (complex_case)
          {
#if defined (DEBUG)
            octave_stdout << "qz: retval(1) = cbb =\n";
            octave_print_internal (octave_stdout, cbb);
            octave_stdout << "\nqz: retval(0) = caa =\n";
            octave_print_internal (octave_stdout, caa);
            octave_stdout << std::endl;
#endif
            retval(1) = cbb;
            retval(0) = caa;
          }
        else
          {
#if defined (DEBUG)
            octave_stdout << "qz: retval(1) = bb =\n";
            octave_print_internal (octave_stdout, bb);
            octave_stdout << "\nqz: retval(0) = aa =\n";
            octave_print_internal (octave_stdout, aa);
            octave_stdout << std::endl;
#endif
            retval(1) = bb;
            retval(0) = aa;
          }
      }
      break;

    case 1:
    case 0:
#if defined (DEBUG)
      octave_stdout << "qz: retval(0) = gev = " << gev << std::endl;
#endif
      retval(0) = gev;
      break;

    default:
      error ("qz: too many return arguments");
      break;
    }

#if defined (DEBUG)
  octave_stdout << "qz: exiting (at long last)" << std::endl;
#endif

  return retval;
}

/*
%!test
%! A = [1 2; 0 3];
%! B = [1 0; 0 0];
%! C = [0 1; 0 0];
%! assert (qz (A,B), [1; Inf]);
%! assert (qz (A,C), [Inf; Inf]);

## Example 7.7.3 in Golub & Van Loan
%!test
%! a = [ 10  1  2;
%!        1  2 -1;
%!        1  1  2 ];
%! b = reshape (1:9, 3,3);
%! [aa, bb, q, z, v, w, lambda] = qz (a, b);
%! assert (q * a * z, aa, norm (aa) * 1e-14);
%! assert (q * b * z, bb, norm (bb) * 1e-14);
%! is_finite = abs (lambda) < 1 / eps (max (a(:)));
%! observed = (b * v * diag (lambda))(:,is_finite);
%! assert (observed, (a*v)(:,is_finite), norm (observed) * 1e-14);
%! observed = (diag (lambda) * w' * b)(is_finite,:);
%! assert (observed, (w'*a)(is_finite,:), norm (observed) * 1e-13);

%!test
%! A = [0, 0, -1, 0; 1, 0, 0, 0; -1, 0, -2, -1; 0, -1, 1, 0];
%! B = [0, 0, 0, 0; 0, 1, 0, 0; 0, 0, 1, 0; 0, 0, 0, 1];
%! [AA, BB, Q, Z1] = qz (A, B);
%! [AA, BB, Z2] = qz (A, B, "-");
%! assert (Z1, Z2);

%!test
%! A = [ -1.03428  0.24929  0.43205 -0.12860;
%!        1.16228  0.27870  2.12954  0.69250;
%!       -0.51524 -0.34939 -0.77820  2.13721;
%!       -1.32941  2.11870  0.72005  1.00835 ];
%! B = [  1.407302 -0.632956 -0.360628  0.068534;
%!        0.149898  0.298248  0.991777  0.023652;
%!        0.169281 -0.405205 -1.775834  1.511730;
%!        0.717770  1.291390 -1.766607 -0.531352 ];
%! [AA, BB, Z, lambda] = qz (A, B, "+");
%! assert (all (real (lambda(1:3)) >= 0));
%! assert (real (lambda(4) < 0));
%! [AA, BB, Z, lambda] = qz (A, B, "-");
%! assert (real (lambda(1) < 0));
%! assert (all (real (lambda(2:4)) >= 0));
%! [AA, BB, Z, lambda] = qz (A, B, "B");
%! assert (all (abs (lambda(1:3)) >= 1));
%! assert (abs (lambda(4) < 1));
%! [AA, BB, Z, lambda] = qz (A, B, "S");
%! assert (abs (lambda(1) < 1));
%! assert (all (abs (lambda(2:4)) >= 1));
*/

OCTAVE_END_NAMESPACE(octave)
