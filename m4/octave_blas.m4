dnl --------------------------------------------------------------------
dnl
dnl Copyright (C) 2022-2023 The Octave Project Developers
dnl
dnl See the file COPYRIGHT.md in the top-level directory of this
dnl distribution or <https://octave.org/copyright/>.
dnl
dnl This file is part of Octave.
dnl
dnl Octave is free software: you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.
dnl
dnl Octave is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with Octave; see the file COPYING.  If not, see
dnl <https://www.gnu.org/licenses/>.
dnl
dnl --------------------------------------------------------------------
dnl
dnl Check for BLAS library and, if valid, determine integer size of library
dnl
AC_DEFUN([OCTAVE_BLAS], [
  AC_PREREQ(2.50)
  dnl Call reference macro to find BLAS library
  AX_BLAS

  if test "$cross_compiling" = yes ; then
    dnl Assume BLAS library exists when cross compiling
    ax_blas_ok=yes
    AC_MSG_CHECKING([BLAS can be called from Fortran])
    AC_MSG_RESULT([yes assumed for cross compilation])
    dnl Assume generic 4-byte BLAS library if not specified
    AC_MSG_CHECKING([BLAS library integer size])
    if test -z "$ax_cv_blas_integer_size" ; then
      ax_cv_blas_integer_size=4
    fi
    AC_MSG_RESULT([$ax_cv_blas_integer_size assumed for cross compilation])
  elif test $ax_blas_ok = yes; then
    ac_octave_save_LIBS="$LIBS"
    LIBS="$BLAS_LIBS $LIBS"
    AC_LANG_PUSH(Fortran 77)
    ## Check BLAS library integer size.
    ## If it does not appear to be 8 bytes, we assume it is 4 bytes.
    ## FIXME: this may fail with options like -ftrapping-math.
    AC_CACHE_CHECK([BLAS library integer size],
      [ax_cv_blas_integer_size],
      [AC_RUN_IFELSE([AC_LANG_PROGRAM(,[[
      integer*8 two, n
      integer*4 n2(2)
      double precision d, a(1), b(1), ddot
      equivalence (n, n2)

      a(1) = 1.0
      b(1) = 1.0

c Generate 2**32 + 1 in an 8-byte integer.  Whether we have a big
c endian or little endian system, both 4-byte words of this value
c should be 1.

      two = 2
      n = (two ** 32) + 1

c Check that our expectation about the type conversions are correct.

      if (n2(1) .ne. 1 .or. n2(2) .ne. 1) then
        print *, 'invalid assumption about integer type conversion'
        stop 2
      endif

*     print *, n, n2(1), n2(2)
*     print *, a(1), b(1)

c DDOT will either see 1 or a large value for N.  Since INCX and INCY
c are both 0, we will never increment the index, so A and B only need to
c have a single element.  If N is interpreted as 1 (BLAS compiled with
c 4-byte integers) then the result will be 1.  If N is interpreted as a
c large value (BLAS compiled with 8-byte integers) then the result will
c be the summation a(1)*b(1) 2^32+1 times.  This will also take some
c time to compute, but at least for now it is the unusual case so we are
c much more likely to exit quickly after detecting that the BLAS library
c was compiled with 4-byte integers.

      d = ddot (n, a, 0, b, 0)

*     print *, a(1), b(1), d

c Success (0 exit status) means we detected BLAS compiled with
c 8-byte integers.

      if (d .eq. 1.0) then
        stop 1
      endif

        ]])],
        ax_cv_blas_integer_size=8,
        ax_cv_blas_integer_size=4)
      ])

  AC_LANG_POP(Fortran 77)
  LIBS="$ac_octave_save_LIBS"
fi

])
