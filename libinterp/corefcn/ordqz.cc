////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2020-2023 The Octave Project Developers
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

// Generalized eigenvalue reordering via LAPACK

// Originally written by M. Koehler <koehlerm(AT)mpi-magdeburg.mpg.de>

#undef DEBUG

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cctype>
#include <cmath>

#include "f77-fcn.h"
#include "lo-lapack-proto.h"
#include "qr.h"
#include "quit.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"


#if defined (DEBUG)
#  include "pager.h"
#  include "pr-output.h"
#endif


OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (ordqz, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {[@var{AR}, @var{BR}, @var{QR}, @var{ZR}] =} ordqz (@var{AA}, @var{BB}, @var{Q}, @var{Z}, @var{keyword})
@deftypefnx {} {[@var{AR}, @var{BR}, @var{QR}, @var{ZR}] =} ordqz (@var{AA}, @var{BB}, @var{Q}, @var{Z}, @var{select})
Reorder the QZ@tie{}decomposition of a generalized eigenvalue problem.

The generalized eigenvalue problem is defined as

@tex
$$A x = \lambda B x$$
@end tex
@ifnottex

@math{A x = @var{lambda} B x}

@end ifnottex

Its generalized Schur decomposition is computed using the @code{qz} algorithm:

@code{[@var{AA}, @var{BB}, @var{Q}, @var{Z}] = qz (@var{A}, @var{B})}

where @var{AA}, @var{BB}, @var{Q}, and @var{Z} fulfill
@tex
$$ AA = Q \cdot A \cdot Z, BB = Q \cdot B \cdot Z $$
@end tex
@ifnottex

@example
@group

@var{AA} = @var{Q} * @var{A} * @var{Z}, @var{BB} = @var{Q} * @var{B} * @var{Z}

@end group
@end example

@end ifnottex

The @code{ordqz} function computes a unitary transformation @var{QR} and
@var{ZR} such that the order of the eigenvalue on the diagonal of @var{AA} and
@var{BB} is changed.  The resulting reordered matrices @var{AR} and @var{BR}
fulfill:

@tex
$$ A_R = Q_R \cdot A \cdot Z_R, B_R = Q_R \cdot B \cdot Z_R $$
@end tex
@ifnottex

@example
@group

@var{AR} = @var{QR} * @var{A} * @var{ZR}, @var{BR} = @var{QR} * @var{B} * @var{ZR}

@end group
@end example

@end ifnottex

The function can either be called with the @var{keyword} argument which
selects the eigenvalues in the top left block of @var{AR} and @var{BR} in the
following way:

@table @asis
@item @qcode{"S"}, @nospell{@qcode{"udi"}}
small: leading block has all
@tex
$|\lambda| < 1$
@end tex
@ifnottex
|@var{lambda}| < 1
@end ifnottex

@item @qcode{"B"}, @nospell{@qcode{"udo"}}
big: leading block has all
@tex
$|\lambda| \geq 1$
@end tex
@ifnottex
|@var{lambda}| @geq{} 1
@end ifnottex

@item @qcode{"-"}, @nospell{@qcode{"lhp"}}
negative real part: leading block has all eigenvalues in the open left
half-plane

@item @qcode{"+"}, @nospell{@qcode{"rhp"}}
non-negative real part: leading block has all eigenvalues in the closed right
half-plane
@end table

If a logical vector @var{select} is given instead of a keyword the @code{ordqz}
function reorders all eigenvalues @code{k} to the left block for which
@code{select(k)} is true.

Note: The keywords are compatible with the ones from @code{qr}.

@seealso{eig, ordeig, qz, schur, ordschur}
@end deftypefn */)
{
  enum { LHP, RHP, UDI, UDO, VEC, NONE } select_mode = NONE;

  if (args.length () != 5)
    print_usage ();

  // Check select argument
  if (args(4).is_string())
    {
      std::string opts = args(4).string_value ();
      std::for_each (opts.begin (), opts.end (),
      [] (char& c) { c = std::tolower (c); });
      if (opts == "lhp" || opts == "-")
        select_mode = LHP;
      else if (opts == "rhp" || opts == "+")
        select_mode = RHP;
      else if (opts == "udi" || opts == "s")
        select_mode = UDI;
      else if (opts == "udo" || opts == "b")
        select_mode = UDO;
      else
        error_with_id ("Octave:ordqz:unknown-keyword",
                       "ordqz: unknown KEYWORD, possible values: "
                       "lhp, rhp, udi, udo");
    }
  else if (args(4).isreal () || args(4).isinteger () || args(4).islogical ())
    {
      if (args(4).rows () > 1 && args(4).columns () > 1)
        error_with_id ("Octave:ordqz:select-not-vector",
                       "ordqz: SELECT argument must be a vector");
      select_mode = VEC;
    }
  else
    error_with_id ("Octave:ordqz:unknown-arg",
                   "ordqz: OPT must be string or a logical vector");

  if (nargout > 4)
    error_with_id ("Octave:ordqz:nargout",
                   "ordqz: at most four output arguments possible");

  // Matrix A: check dimensions.
  F77_INT nn = to_f77_int (args(0).rows ());
  F77_INT nc = to_f77_int (args(0).columns ());

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

  // Extract argument 3 (qq, or cqq if complex).
  F77_INT q_nr = to_f77_int (args(2).rows ());
  F77_INT q_nc = to_f77_int (args(2).columns ());

  if (nn != q_nc || nn != q_nr)
    ::err_nonconformant ();

  Matrix qq;
  ComplexMatrix cqq;

  if (args(2).iscomplex ())
    cqq = args(2).complex_matrix_value ().hermitian ();
  else
    qq = args(2).matrix_value ().transpose ();

  // Extract argument 4 (zz, or czz if complex).
  F77_INT z_nr = to_f77_int (args(3).rows ());
  F77_INT z_nc = to_f77_int (args(3).columns ());

  if (nn != z_nc || nn != z_nr)
    ::err_nonconformant ();

  Matrix zz;
  ComplexMatrix czz;

  if (args(3).iscomplex ())
    czz = args(3).complex_matrix_value ();
  else
    zz = args(3).matrix_value ();

  bool complex_case = (args(0).iscomplex () || args(1).iscomplex ()
                       || args(2).iscomplex () || args(3).iscomplex ());

  if (select_mode == VEC && args(4).rows () != nn && args(4).columns () != nn)
    error_with_id ("Octave:ordqz:numel_select",
                   "ordqz: SELECT vector has the wrong number of elements");

  Array<double> select_array (dim_vector (nn, 1));
  if (select_mode == VEC)
    select_array = args(4).vector_value ();

  Array<F77_LOGICAL> select (dim_vector (nn, 1));

  if (complex_case)
    {
      // Complex
      if (args(0).isreal ())
        caa = ComplexMatrix (aa);
      if (args(1).isreal ())
        cbb = ComplexMatrix (bb);
      if (args(2).isreal ())
        cqq = ComplexMatrix (qq);
      if (args(3).isreal ())
        czz = ComplexMatrix (zz);

      ComplexRowVector alpha (dim_vector (nn, 1));
      ComplexRowVector beta  (dim_vector (nn, 1));
      octave_idx_type k;

      for (k = 0; k < nn-1; k++)
        {
          if (caa(k+1, k) != 0.0)
            error_with_id ("Octave:ordqz:unsupported_AA",
                           "ordqz: quasi upper triangular matrices are not "
                           "allowed with complex data");
        }

      for (k = 0; k < nn; k++)
        {
          alpha(k) = caa(k, k);
          beta(k)  = cbb(k, k);
        }

      for (k = 0; k < nn; k++)
        {
          switch (select_mode)
            {
            case LHP:
              select(k) = real (alpha(k) * beta(k)) < 0;
              break;
            case RHP:
              select(k) = real (alpha(k) * beta(k)) > 0;
              break;
            case UDI:
              if (beta(k) != 0.0)
                select(k) = abs (alpha(k)/beta(k)) < 1.0;
              else
                select(k) = false;
              break;
            case UDO:
              if (beta(k) != 0.0)
                select(k) = abs (alpha(k)/beta(k)) > 1.0;
              else
                select(k) = true;
              break;
            case VEC:
              if (select_array(k) != 0.0)
                select(k) = true;
              else
                select(k) = false;
              break;
            default:
              // default: case just here to suppress compiler warning.
              panic_impossible ();
            }
        }

      F77_LOGICAL wantq, wantz;
      wantq = 1,  wantz = 1;
      F77_INT ijob, mm, lrwork3, liwork, info;
      ijob = 0,  lrwork3 = 1,  liwork = 1;
      F77_DBLE pl, pr;
      ComplexRowVector work3 (lrwork3);
      Array<F77_INT> iwork (dim_vector (liwork, 1));

      F77_XFCN (ztgsen, ZTGSEN,
                (ijob, wantq, wantz,
                 select.fortran_vec (), nn,
                 F77_DBLE_CMPLX_ARG (caa.fortran_vec ()), nn,
                 F77_DBLE_CMPLX_ARG (cbb.fortran_vec ()), nn,
                 F77_DBLE_CMPLX_ARG (alpha.fortran_vec ()),
                 F77_DBLE_CMPLX_ARG (beta.fortran_vec ()),
                 F77_DBLE_CMPLX_ARG (cqq.fortran_vec ()), nn,
                 F77_DBLE_CMPLX_ARG (czz.fortran_vec ()), nn,
                 mm,
                 pl, pr,
                 nullptr,
                 F77_DBLE_CMPLX_ARG (work3.fortran_vec ()), lrwork3,
                 iwork.fortran_vec (), liwork,
                 info));
      if (info != 0)
        error_with_id ("Octave:ordqz:ztgsen_failed",
                       "ordqz: failed to reorder eigenvalues");
    }
  else
    {
      // Extract eigenvalues
      RowVector alphar (dim_vector (nn, 1));
      RowVector alphai (dim_vector (nn, 1));
      RowVector beta (dim_vector (nn, 1));

      octave_idx_type k;

      k = 0;
      while (k < nn)
        {
#ifdef DEBUG
          octave_stdout << "ordqz: k = " << k  << " nn = " << nn << " \n";
#endif
          if ((k < nn-1 && aa(k+1, k) == 0.0) || k == nn-1)
            {
              alphar(k) = aa(k, k);
              alphai(k) = 0.0;
              beta(k)   = bb(k, k);
              k++;
            }
          else
            {
              double ar[2], ai[2], b[2], work[4];
              char qz_job = 'E';
              char comp_q = 'N';
              char comp_z = 'N';
              F77_INT nl = 2;
              F77_INT ilo = 1;
              F77_INT ihi = 2;
              F77_INT lwork = 4;
              F77_INT info = 0;
              double *aa_vec = aa.fortran_vec ();
              double *bb_vec = bb.fortran_vec ();

              F77_XFCN (dhgeqz, DHGEQZ,
                        (F77_CONST_CHAR_ARG2 (&qz_job, 1),
                         F77_CONST_CHAR_ARG2 (&comp_q, 1),
                         F77_CONST_CHAR_ARG2 (&comp_z, 1),
                         nl, ilo, ihi,
                         &aa_vec[k+k*nn], nn,
                         &bb_vec[k+k*nn], nn,
                         ar, ai, b,
                         nullptr, nn,
                         nullptr, nn, work, lwork, info
                         F77_CHAR_ARG_LEN (1)
                         F77_CHAR_ARG_LEN (1)
                         F77_CHAR_ARG_LEN (1)));
              if (info != 0)
                error("ordqz: failed to extract eigenvalues");

              alphar(k)   = ar[0];
              alphar(k+1) = ar[1];
              alphai(k)   = ai[0];
              alphai(k+1) = ai[1];
              beta(k)   = b[0];
              beta(k+1) = b[1];

              k += 2;
            }

        }

      for (k = 0; k < nn; k++)
        {
          switch (select_mode)
            {
            case LHP:
              select(k) = alphar(k) * beta(k) < 0;
              break;
            case RHP:
              select(k) = alphar(k) * beta(k) > 0;
              break;
            case UDI:
              select(k) = alphar(k)*alphar(k)
                          + alphai(k)*alphai(k) < beta(k)*beta(k);
              break;
            case UDO:
              select(k) = alphar(k)*alphar(k)
                          + alphai(k)*alphai(k) > beta(k)*beta(k);
              break;
            case VEC:
              if (select_array(k) != 0.0)
                select(k) = true;
              else
                select(k) = false;
              break;
            default:
              // default: case just here to suppress compiler warning.
              panic_impossible();
            }
        }

      F77_LOGICAL wantq, wantz;
      wantq = 1,  wantz = 1;
      F77_INT ijob, mm, lrwork3, liwork, info;
      ijob = 0,  lrwork3 = 4*nn+16,  liwork = nn;
      F77_DBLE pl, pr;
      RowVector rwork3 (lrwork3);
      Array<F77_INT> iwork (dim_vector (liwork, 1));

      F77_XFCN (dtgsen, DTGSEN,
                (ijob, wantq, wantz,
                 select.fortran_vec (), nn,
                 aa.fortran_vec (), nn,
                 bb.fortran_vec (), nn,
                 alphar.fortran_vec (),
                 alphai.fortran_vec (),
                 beta.fortran_vec (),
                 qq.fortran_vec (), nn,
                 zz.fortran_vec (), nn,
                 mm,
                 pl, pr,
                 nullptr,
                 rwork3.fortran_vec (), lrwork3,
                 iwork.fortran_vec (), liwork,
                 info));
      if (info != 0)
        error("ordqz: failed to reorder eigenvalues");
    }

  octave_value_list retval (nargout);
  switch (nargout)
    {
    case 4:
      if (complex_case)
        retval(3) = czz;
      else
        retval(3) = zz;
      OCTAVE_FALLTHROUGH;
    case 3:
      if (complex_case)
        retval(2) = cqq.hermitian();
      else
        retval(2) = qq.transpose();
      OCTAVE_FALLTHROUGH;
    case 2:
      if (complex_case)
        retval(1) = cbb;
      else
        retval(1) = bb;
      OCTAVE_FALLTHROUGH;
    case 1:
      if (complex_case)
        retval(0) = caa;
      else
        retval(0) = aa;
      break;
    case 0:
      if (complex_case)
        retval(0) = caa;
      else
        retval(0) = aa;
      break;
    }

  return retval;
}


/*
%!shared A, B, AA, BB, QQ, ZZ, AC, BC, AAC, BBC, QQC, ZZC, select, selectc
%! A = [ -1.03428  0.24929  0.43205 -0.12860;
%!        1.16228  0.27870  2.12954  0.69250;
%!       -0.51524 -0.34939 -0.77820  2.13721;
%!       -1.32941  2.11870  0.72005  1.00835 ];
%! B = [  1.407302 -0.632956 -0.360628  0.068534;
%!        0.149898  0.298248  0.991777  0.023652;
%!        0.169281 -0.405205 -1.775834  1.511730;
%!        0.717770  1.291390 -1.766607 -0.531352 ];
%! AC = [ 0.4577 + 0.7199i   0.1476 + 0.6946i   0.6202 + 0.2092i   0.7559 + 0.2759i;
%!        0.5868 + 0.7275i   0.9174 + 0.8781i   0.6741 + 0.1985i   0.4320 + 0.7023i;
%!        0.2408 + 0.6359i   0.2959 + 0.8501i   0.3904 + 0.5613i   0.5000 + 0.1428i;
%!        0.8177 + 0.8581i   0.2583 + 0.8970i   0.7706 + 0.5451i   0.1068 + 0.1650i];
%! BC = [ 0.089898 + 0.209257i   0.157769 + 0.311387i   0.018926 + 0.622517i   0.058825 + 0.374647i;
%!        0.009367 + 0.098211i   0.736087 + 0.095797i   0.973192 + 0.583765i   0.434018 + 0.461909i;
%!        0.880784 + 0.868215i   0.032839 + 0.569461i   0.873437 + 0.266081i   0.739426 + 0.362017i;
%!        0.121649 + 0.115111i   0.426695 + 0.492222i   0.247670 + 0.034414i   0.771629 + 0.078153i];
%! [AA, BB, QQ, ZZ] = qz (A, B);
%! [AAC, BBC, QQC, ZZC] = qz (AC, BC);
%! select = [0 0 1 1];
%! selectc = [0 0 0 1];

%!test
%! [AAX, BBX, QQX, ZZX] = ordqz (AA, BB, QQ, ZZ, "rhp");
%! assert (all (real (eig (AAX(1:3,1:3), BBX(1:3,1:3))) >= 0));
%! assert (all (real (eig (AAX(4:4,4:4), BBX(4:4,4:4))) < 0));
%! assert (norm (QQX'*AAX*ZZX' - A, "fro"), 0, 1e-12);
%! assert (norm (QQX'*BBX*ZZX' - B, "fro"), 0, 1e-12);

%!test
%! [AAX, BBX, QQX, ZZX] = ordqz (AA, BB, QQ, ZZ, "+");
%! assert (all (real (eig (AAX(1:3,1:3), BBX(1:3,1:3))) >= 0));
%! assert (all (real (eig (AAX(4:4,4:4), BBX(4:4,4:4))) < 0));

%!test
%! [AAX, BBX, QQX, ZZX] = ordqz (AA, BB, QQ, ZZ, "lhp");
%! assert (all (real (eig (AAX(2:4,2:4), BBX(2:4,2:4))) >= 0));
%! assert (all (real (eig (AAX(1:1,1:1), BBX(1:1,1:1))) < 0));
%! assert (norm (QQX'*AAX*ZZX' - A, "fro"), 0, 1e-12);
%! assert (norm (QQX'*BBX*ZZX' - B, "fro"), 0, 1e-12);

%!test
%! [AAX, BBX, QQX, ZZX] = ordqz (AA, BB, QQ, ZZ, "-");
%! assert (all (real (eig (AAX(2:4,2:4), BBX(2:4,2:4))) >= 0));
%! assert (all (real (eig (AAX(1:1,1:1), BBX(1:1,1:1))) < 0));

%!test
%! [AAX, BBX, QQX, ZZX] = ordqz (AA, BB, QQ, ZZ, "udi");
%! assert (all (abs (eig (AAX(1:1,1:1), BBX(1:1,1:1))) < 1));
%! assert (all (abs (eig (AAX(2:4,2:4), BBX(2:4,2:4))) > 1));
%! assert (norm (QQX'*AAX*ZZX' - A, "fro"), 0, 1e-12);
%! assert (norm (QQX'*BBX*ZZX' - B, "fro"), 0, 1e-12);

%!test
%! [AAX, BBX, QQX, ZZX] = ordqz (AA, BB, QQ, ZZ, "S");
%! assert (all (abs (eig (AAX(1:1,1:1), BBX(1:1,1:1))) < 1));
%! assert (all (abs (eig (AAX(2:4,2:4), BBX(2:4,2:4))) > 1));

%!test
%! [AAX, BBX, QQX, ZZX] = ordqz (AA, BB, QQ, ZZ, "udo");
%! assert (all (abs (eig (AAX(1:3,1:3), BBX(1:3,1:3))) >= 1));
%! assert (all (abs (eig (AAX(4:4,4:4), BBX(4:4,4:4))) < 1));
%! assert (norm (QQX'*AAX*ZZX' - A, "fro"), 0, 1e-12);
%! assert (norm (QQX'*BBX*ZZX' - B, "fro"), 0, 1e-12);

%!test
%! [AAX, BBX, QQX, ZZX] = ordqz (AA, BB, QQ, ZZ, "B");
%! assert (all (abs (eig (AAX(1:3,1:3), BBX(1:3,1:3))) >= 1));
%! assert (all (abs (eig (AAX(4:4,4:4), BBX(4:4,4:4))) < 1));

%!test
%! [AAX, BBX, QQX, ZZX] = ordqz (AA, BB, QQ, ZZ, select);
%! assert (all (iscomplex (eig (AAX(1:2,1:2), BBX(1:2,1:2)))));
%! assert (norm (QQX'*AAX*ZZX' - A, "fro"), 0, 1e-12);
%! assert (norm (QQX'*BBX*ZZX' - B, "fro"), 0, 1e-12);

%!test
%! [AACX, BBCX, QQCX, ZZCX] = ordqz (AAC, BBC, QQC, ZZC, "rhp");
%! assert (all (real (eig (AACX(1:2,1:2), BBCX(1:2,1:2))) >= 0));
%! assert (all (real (eig (AACX(3:4,3:4), BBCX(3:4,3:4))) < 0));
%! assert (norm (QQCX'*AACX*ZZCX' - AC, "fro"), 0, 1e-12);
%! assert (norm (QQCX'*BBCX*ZZCX' - BC, "fro"), 0, 1e-12);

%!test
%! [AACX, BBCX, QQCX, ZZCX] = ordqz (AAC, BBC, QQC, ZZC, "lhp");
%! assert (all (real (eig (AACX(1:2,1:2), BBCX(1:2,1:2))) < 0));
%! assert (all (real (eig (AACX(3:4,3:4), BBCX(3:4,3:4))) >= 0));
%! assert (norm (QQCX'*AACX*ZZCX' - AC, "fro"), 0, 1e-12);
%! assert (norm (QQCX'*BBCX*ZZCX' - BC, "fro"), 0, 1e-12);

%!test
%! [AACX, BBCX, QQCX, ZZCX] = ordqz (AAC, BBC, QQC, ZZC, "udi");
%! assert (all (abs (eig (AACX(1:2,1:2), BBCX(1:2,1:2))) < 1));
%! assert (all (abs (eig (AACX(3:4,3:4), BBCX(3:4,3:4))) >= 1));
%! assert (norm (QQCX'*AACX*ZZCX' - AC, "fro"), 0, 1e-12);
%! assert (norm (QQCX'*BBCX*ZZCX' - BC, "fro"), 0, 1e-12);

%!test
%! [AACX, BBCX, QQCX, ZZCX] = ordqz (AAC, BBC, QQC, ZZC, "udo");
%! assert (all (abs (eig (AACX(1:2,1:2), BBCX(1:2,1:2))) >= 1));
%! assert (all (abs (eig (AACX(3:4,3:4), BBCX(3:4,3:4))) < 1));
%! assert (norm (QQCX'*AACX*ZZCX' - AC, "fro"), 0, 1e-12);
%! assert (norm (QQCX'*BBCX*ZZCX' - BC, "fro"), 0,  1e-12);

%!test
%! [AACX, BBCX, QQCX, ZZCX] = ordqz (AAC, BBC, QQC, ZZC, selectc);
%! ev = abs (eig (AACX(1:1,1:1), BBCX(1:1,1:1)));
%! assert(ev > 0.6 && ev < 0.7);
%! assert (norm (QQCX'*AACX*ZZCX' - AC, "fro"), 0, 1e-12);
%! assert (norm (QQCX'*BBCX*ZZCX' - BC, "fro"), 0, 1e-12);

%!test
%! A = toeplitz ([1,2,3,4]);
%! [B, A] = qr (A);
%! B = B';
%! [AA, BB, Q, Z] = qz (A, B);
%! [AAS, BBS, QS, ZS] = ordqz (AA, BB, Q, Z, "lhp");
%! E2 = ordeig (AAS, BBS);
%! ECOMP = [-3.414213562373092; -1.099019513592784;
%!          -0.5857864376269046; 9.099019513592784];
%! assert (norm (ECOMP - E2, "Inf"), 0, 1e-8);

## Test input validation
%!error <Invalid call> ordqz ()
%!error <Invalid call> ordqz (eye (2))
%!error <Invalid call> ordqz (eye (2), eye (2))
%!error <Invalid call> ordqz (eye (2), eye (2), eye (2))
%!error <Invalid call> ordqz (eye (2), eye (2), eye (2), eye (2))
%!error id=Octave:ordqz:unknown-keyword
%! ordqz (eye (2), eye (2), eye (2), eye (2), "foobar");
%!error id=Octave:ordqz:select-not-vector
%! ordqz (eye (2), eye (2), eye (2), eye (2), eye (2));
%!error id=Octave:ordqz:unknown-arg
%! ordqz (eye (2), eye (2), eye (2), eye (2), {"foobar"});
%!error id=Octave:ordqz:nargout
%! [a,b,c,d,e] = ordqz (eye (2), eye (2), eye (2), eye (2), "udi");
%!warning <A: argument is empty matrix> ordqz ([], [], [], [], "udi");
%!error <A must be a square matrix> ordqz (ones (1,2), [], [], [], "udi");
%!error <nonconformant matrices>
%! ordqz (eye (3), eye (2), eye (2), eye (2), "udi");
%!error <nonconformant matrices>
%! ordqz (eye (2), eye (3), eye (2), eye (2), "udi");
%!error <nonconformant matrices>
%! ordqz (eye (2), eye (2), eye (3), eye (2), "udi");
%!error <nonconformant matrices>
%! ordqz (eye (2), eye (2), eye (2), eye (3), "udi");
%!error <SELECT vector .* wrong number of elements>
%! ordqz (eye (2), eye (2), eye (2), eye (2), ones (1,5));
%!error <quasi upper triangular matrices are not allowed with complex data>
%! AA = zeros (2, 2);
%! AA(2,1) = i;
%! ordqz (AA, eye (2), eye (2), eye (2), "udi");

*/

OCTAVE_END_NAMESPACE(octave)
