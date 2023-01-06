#!/bin/sh

########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

cat <<EOF
## !!! DO NOT EDIT !!!
## THIS IS AN AUTOMATICALLY GENERATED FILE
## modify mk-conv-tst.sh to generate the tests you need.


%!shared r,dq,sq,b,bm,dm,cdm,fdm,fcdm,pm,sm,sbm,scm,s,m,cs,cm,fs,fm,fcs,fcm,i8s,i16s,i32s,i64s,i8m,i16m,i32m,i64m,ui8s,ui16s,ui32s,ui64s,ui8m,ui16m,ui32m,ui64m
%!test
%! r = 1:5;
%! dq = "foo";
%! sq = 'bar';
%! b = true;
%! bm = rand (5) > 0.5;
%! dm = eye (5);
%! cdm = eye (5) * i;
%! fdm = eye (5, "single");
%! fcdm = eye (5, "single") * single (i);
%! [~, ~, pm] = qr (rand (5));
%! sm = sprand (5, 5, 0.1);
%! sbm = sparse (rand (5) > 0.5);
%! scm = sprand (5, 5, 0.1) * i;
%! s = rand ();
%! m = rand (5);
%! cs = 13 * i;
%! cm = rand (5) * i;
%! fs = rand ("single");
%! fm = rand (5, "single");
%! fcs = rand ("single") * single (i);
%! fcm = rand (5, "single") * single (i);
%! i8s = int8 (rand () * 10);
%! i16s = int16 (rand () * 10);
%! i32s = int32 (rand () * 10);
%! i64s = int64 (rand () * 10);
%! i8m = int8 (rand (5) * 10);
%! i16m = int16 (rand (5) * 10);
%! i32m = int32 (rand (5) * 10);
%! i64m = int64 (rand (5) * 10);
%! ui8s = uint8 (rand () * 10);
%! ui16s = uint16 (rand () * 10);
%! ui32s = uint32 (rand () * 10);
%! ui64s = uint64 (rand () * 10);
%! ui8m = uint8 (rand (5) * 10);
%! ui16m = uint16 (rand (5) * 10);
%! ui32m = uint32 (rand (5) * 10);
%! ui64m = uint64 (rand (5) * 10);
%!
%!test
%! if (optimize_range ())
%!   assert (typeinfo (r), "double_range")
%! else
%!   assert (typeinfo (r), "matrix")
%! endif
%!assert (typeinfo (dq), "string")
%!assert (typeinfo (sq), "sq_string")
%!assert (typeinfo (b), "bool")
%!assert (typeinfo (bm), "bool matrix")
%!assert (typeinfo (dm), "diagonal matrix")
%!assert (typeinfo (cdm), "complex diagonal matrix")
%!assert (typeinfo (fdm), "float diagonal matrix")
%!assert (typeinfo (fcdm), "float complex diagonal matrix")
%!assert (typeinfo (pm), "permutation matrix")
%!assert (typeinfo (sm), "sparse matrix")
%!assert (typeinfo (sbm), "sparse bool matrix")
%!assert (typeinfo (scm), "sparse complex matrix")
%!assert (typeinfo (s), "scalar")
%!assert (typeinfo (m), "matrix")
%!assert (typeinfo (cs), "complex scalar")
%!assert (typeinfo (cm), "complex matrix")
%!assert (typeinfo (fs), "float scalar")
%!assert (typeinfo (fm), "float matrix")
%!assert (typeinfo (fcs), "float complex scalar")
%!assert (typeinfo (fcm), "float complex matrix")
%!assert (typeinfo (i8s), "int8 scalar")
%!assert (typeinfo (i16s), "int16 scalar")
%!assert (typeinfo (i32s), "int32 scalar")
%!assert (typeinfo (i64s), "int64 scalar")
%!assert (typeinfo (i8m), "int8 matrix")
%!assert (typeinfo (i16m), "int16 matrix")
%!assert (typeinfo (i32m), "int32 matrix")
%!assert (typeinfo (i64m), "int64 matrix")
%!assert (typeinfo (ui8s), "uint8 scalar")
%!assert (typeinfo (ui16s), "uint16 scalar")
%!assert (typeinfo (ui32s), "uint32 scalar")
%!assert (typeinfo (ui64s), "uint64 scalar")
%!assert (typeinfo (ui8m), "uint8 matrix")
%!assert (typeinfo (ui16m), "uint16 matrix")
%!assert (typeinfo (ui32m), "uint32 matrix")
%!assert (typeinfo (ui64m), "uint64 matrix")
%!
%!assert (class (r), "double")
%!assert (class (dq), "char")
%!assert (class (sq), "char")
%!assert (class (b), "logical")
%!assert (class (bm), "logical")
%!assert (class (dm), "double")
%!assert (class (cdm), "double")
%!assert (class (fdm), "single")
%!assert (class (fcdm), "single")
%!assert (class (pm), "double")
%!assert (class (sm), "double")
%!assert (class (sbm), "logical")
%!assert (class (scm), "double")
%!assert (class (s), "double")
%!assert (class (m), "double")
%!assert (class (cs), "double")
%!assert (class (cm), "double")
%!assert (class (fs), "single")
%!assert (class (fm), "single")
%!assert (class (fcs), "single")
%!assert (class (fcm), "single")
%!assert (class (i8s), "int8")
%!assert (class (i16s), "int16")
%!assert (class (i32s), "int32")
%!assert (class (i64s), "int64")
%!assert (class (i8m), "int8")
%!assert (class (i16m), "int16")
%!assert (class (i32m), "int32")
%!assert (class (i64m), "int64")
%!assert (class (ui8s), "uint8")
%!assert (class (ui16s), "uint16")
%!assert (class (ui32s), "uint32")
%!assert (class (ui64s), "uint64")
%!assert (class (ui8m), "uint8")
%!assert (class (ui16m), "uint16")
%!assert (class (ui32m), "uint32")
%!assert (class (ui64m), "uint64")
EOF

## There are no single or integer sparse matrices.

sparse_vars="sm sbm scm"
sparse_conversions="double logical"

for c in $sparse_conversions; do
  echo ""
  for v in $sparse_vars; do
    echo "%!assert (class ($c ($v)), \"$c\")"
  done
done

char_vars="dq sq"
bool_vars="b bm"
real_vars="r s m fs fm dm fdm pm"
complex_vars="cs cm fcs fcm cdm fcdm"
signed_int_vars="i8s i16s i32s i64s i8m i16m i32m i64m"
unsigned_int_vars="ui8s ui16s ui32s ui64s ui8m ui16m ui32m ui64m"
int_vars="$signed_int_vars $unsigned_int_vars"

bool_conversions="logical"
float_conversions="double single"
int_conversions="int8 int16 int32 int64 uint8 uint16 uint32 uint64"
all_conversions="$bool_conversions $float_conversions $int_conversions"

## Strings can't be converted to logical.

for v in $char_vars; do
  echo ""
  for c in $float_conversions $int_conversions; do
    echo "%!assert (class ($c ($v)), \"$c\")"
  done
done

## Octave doesn't have complex integers.

for v in $complex_vars; do
  echo ""
  for c in $bool_conversions $float_conversions; do
    echo "%!assert (class ($c ($v)), \"$c\")"
  done
done

## Logical and real-value numeric values should be convertible to any
## double, single, logical or integer type.

for v in $bool_vars $real_vars $int_vars; do
  echo ""
  for c in $all_conversions; do
    echo "%!assert (class ($c ($v)), \"$c\")"
  done
done
