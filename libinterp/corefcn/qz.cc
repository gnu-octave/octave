////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1998-2024 The Octave Project Developers
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

DEFUN (qz, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {[@var{AA}, @var{BB}, @var{Q}, @var{Z}, @var{V}, @var{W}] =} qz (@var{A}, @var{B})
@deftypefnx {} {[@var{AA}, @var{BB}, @var{Q}, @var{Z}, @var{V}, @var{W}] =} qz (@var{A}, @var{B}, @var{opt})
Compute the QZ@tie{}decomposition of a generalized eigenvalue problem.

The generalized eigenvalue problem is defined as

@tex
$$A x = \lambda B x$$
@end tex
@ifnottex

@math{A x = @var{lambda} B x}

@end ifnottex

There are two calling forms of the function:

@enumerate
@item @code{[@var{AA}, @var{BB}, @var{Q}, @var{Z}, @var{V}, @var{W}, @var{lambda}] = qz (@var{A}, @var{B})}

Compute the complex QZ@tie{}decomposition, generalized eigenvectors, and
generalized eigenvalues.
@tex
$$ AA = Q^T AZ, BB = Q^T BZ $$
$$ { \rm diag }(BB)AV = BV{ \rm diag }(AA) $$
$$ { \rm diag }(BB) W^T A = { \rm diag }(AA) W^T B $$
@end tex
@ifnottex

@example
@group

@var{AA} = @var{Q} * @var{A} * @var{Z}, @var{BB} = @var{Q} * @var{B} * @var{Z}
@var{A} * @var{V} * diag (diag (@var{BB})) = @var{B} * @var{V} * diag (diag (@var{AA}))
diag (diag (@var{BB})) * @var{W}' * @var{A} = diag (diag (@var{AA})) * @var{W}' * @var{B}

@end group
@end example

@end ifnottex
with @var{AA} and @var{BB} upper triangular, and @var{Q} and @var{Z}
unitary.  The matrices @var{V} and @var{W} respectively contain the right
and left generalized eigenvectors.

@item @code{[@var{AA}, @var{BB}, @var{Z} @{, @var{lambda}@}] = qz (@var{A}, @var{B}, @var{opt})}

The @var{opt} argument must be equal to either @qcode{"real"} or
@qcode{"complex"}.  If it is equal to @qcode{"complex"}, then this
calling form is equivalent to the first one with only two input
arguments.

If @var{opt} is equal to @qcode{"real"}, then the real QZ decomposition
is computed.  In particular, @var{AA} is only guaranteed to be
quasi-upper triangular with 1-by-1 and 2-by-2 blocks on the diagonal,
and @var{Q} and @var{Z} are orthogonal.  The identities mentioned above
for right and left generalized eigenvectors are only verified if
@var{AA} is upper triangular (i.e., when all the generalized eigenvalues
are real, in which case the real and complex QZ coincide).

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

  if (nargin < 2 || nargin > 3 || nargout > 6)
    print_usage ();

  bool complex_case = true;

  if (nargin == 3)
    {
      std::string opt = args(2).xstring_value ("qz: OPT must be a string");

      if (opt == "real")
        {
          if (args(0).iscomplex () || args(1).iscomplex ())
            {
              warning ("qz: ignoring 'real' option with complex matrices");
              complex_case = true;
            }
          else
            complex_case = false;
        }
      else if (opt == "complex")
        complex_case = true;
      else
        error ("qz: OPT must be 'real' or 'complex'");
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

  if (nc != nn)
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

  // First, declare variables used in both the real and complex cases.
  // FIXME: There are a lot of excess variables declared.
  //        Probably a better way to handle this.
  Matrix QQ (nn, nn), ZZ (nn, nn), VR (nn, nn), VL (nn, nn);
  RowVector alphar (nn), alphai (nn), betar (nn);
  ComplexRowVector xalpha (nn), xbeta (nn);
  ComplexMatrix CQ (nn, nn), CZ (nn, nn), CVR (nn, nn), CVL (nn, nn);
  F77_INT ilo, ihi, info;
  char comp_q = (nargout >= 3 ? 'V' : 'N');
  char comp_z = (nargout >= 4 ? 'V' : 'N');

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
        }
    }

  octave_value_list retval (nargout);

  switch (nargout)
    {
    case 6:
      // Return left eigenvectors.
      if (complex_case)
        retval(5) = CVL;
      else
        retval(5) = VL;
      OCTAVE_FALLTHROUGH;

    case 5:
      // Return right eigenvectors.
      if (complex_case)
        retval(4) = CVR;
      else
        retval(4) = VR;
      OCTAVE_FALLTHROUGH;

    case 4:
      if (complex_case)
        retval(3) = CZ;
      else
        retval(3) = ZZ;
      OCTAVE_FALLTHROUGH;

    case 3:
      if (complex_case)
        retval(2) = CQ.hermitian ();
      else
        retval(2) = QQ.transpose ();
      OCTAVE_FALLTHROUGH;

    case 2:
    case 1:
    case 0:
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

    }

  // FIXME: The API for qz changed in version 9.
  // These warnings can be removed in Octave version 11.
  if (nargout == 1)
    {
      warning_with_id ("Octave:qz:single-arg-out",
                       "qz: requesting a single output argument no longer returns eigenvalues since version 9");
      disable_warning ("Octave:qz:single-arg-out");
    }

  if (nargin == 2 && args(0).isreal () && args(1).isreal ()
      && retval(0).iscomplex ())
    {
      warning_with_id ("Octave:qz:complex-default",
                       "qz: returns the complex QZ by default on real matrices since version 9");
      disable_warning ("Octave:qz:complex-default");
    }

#if defined (DEBUG)
  octave_stdout << "qz: exiting (at long last)" << std::endl;
#endif

  return retval;
}

/*
## Example 7.7.3 in Golub & Van Loan (3rd ed.; not present in 4th ed.)
## The generalized eigenvalues are real, so the real and complex QZ coincide.
%!test
%! a = [ 10  1  2;
%!        1  2 -1;
%!        1  1  2 ];
%! b = reshape (1:9, 3,3);
%! [aa, bb, q, z, v, w] = qz (a, b);
%! assert (isreal (aa) && isreal (bb) && isreal (q) && isreal (z) ...
%!         && isreal (v) && isreal (w));
%! assert (istriu (aa) && istriu (bb));
%! assert (q * q', eye(3), 1e-15);
%! assert (z * z', eye(3), 1e-15);
%! assert (q * a * z, aa, norm (aa) * 1e-14);
%! assert (q * b * z, bb, norm (bb) * 1e-14);
%! assert (a * v * diag (diag (bb)), b * v * diag (diag (aa)), 1e-13);
%! assert (diag (diag (bb)) * w' * a, diag (diag (aa)) * w' * b, 1e-13);

## An example with real matrices where some generalized eigenvalues are
## complex, so the real and complex QZ differ.
%!test
%! A = [ -1.03428  0.24929  0.43205 -0.12860;
%!        1.16228  0.27870  2.12954  0.69250;
%!       -0.51524 -0.34939 -0.77820  2.13721;
%!       -1.32941  2.11870  0.72005  1.00835 ];
%! B = [  1.407302 -0.632956 -0.360628  0.068534;
%!        0.149898  0.298248  0.991777  0.023652;
%!        0.169281 -0.405205 -1.775834  1.511730;
%!        0.717770  1.291390 -1.766607 -0.531352 ];
%! [CAA, CBB, CQ, CZ, CV, CW] = qz (A, B, 'complex');
%! assert (iscomplex (CAA) && iscomplex (CBB) && iscomplex (CQ) ...
%!         && iscomplex (CZ) && iscomplex (CV) && iscomplex (CW));
%! assert (istriu (CAA) && istriu (CBB));
%! assert (CQ * CQ', eye(4), 1e-14);
%! assert (CZ * CZ', eye(4), 1e-14);
%! assert (CQ * A * CZ, CAA, norm (CAA) * 1e-14);
%! assert (CQ * B * CZ, CBB, norm (CBB) * 1e-14);
%! assert (A * CV * diag (diag (CBB)), B * CV * diag (diag (CAA)), 1e-13);
%! assert (diag (diag (CBB)) * CW' * A, diag (diag (CAA)) * CW' * B, 1e-13);
%! [AA, BB, Q, Z, V, W] = qz (A, B, 'real');
%! assert (isreal (AA) && isreal (BB) && isreal (Q) && isreal (Z) ...
%!         && isreal (V) && isreal (W));
%! assert (istriu (BB));
%! assert (Q * Q', eye(4), 1e-14);
%! assert (Z * Z', eye(4), 1e-14);
%! assert (Q * A * Z, AA, norm (AA) * 1e-14);
%! assert (Q * B * Z, BB, norm (BB) * 1e-14);

## Test input validation
%!error <Invalid call> qz ()
%!error <Invalid call> qz (1)
%!error <Invalid call> qz (1,2,3,4)
%!error <Invalid call> [r1,r2,r3,r4,r5,r6,r7] = qz (1,2)
%!error <OPT must be a string> qz (1,2, 3)
%!error <OPT must be 'real' or 'complex'> qz (1,2, 'foobar')
%!warning <ignoring 'real' option with complex matrices> qz (2i, 3, 'real');
%!warning <ignoring 'real' option with complex matrices> qz (2, 3i, 'real');

*/

OCTAVE_END_NAMESPACE(octave)
