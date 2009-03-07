/*

Copyright (C) 1996, 1999, 2000, 2003, 2005, 2006, 2007, 2008,
              2009 John W. Eaton

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

#if !defined (octave_MArray_defs_h)
#define octave_MArray_defs_h 1

// Nothing like a little CPP abuse to brighten everyone's day.

#define DO_VS_OP(r, l, v, OP, s) \
  if (l > 0) \
    { \
      for (octave_idx_type i = 0; i < l; i++) \
	r[i] = v[i] OP s; \
    }

#define DO_SV_OP(r, l, s, OP, v) \
  if (l > 0) \
    { \
      for (octave_idx_type i = 0; i < l; i++) \
	r[i] = s OP v[i]; \
    }

#define DO_VV_OP(r, l, x, OP, y) \
  if (l > 0) \
    { \
      for (octave_idx_type i = 0; i < l; i++) \
	r[i] = x[i] OP y[i]; \
    }

#define NEG_V(r, l, x) \
  if (l > 0) \
    { \
      for (octave_idx_type i = 0; i < l; i++) \
	r[i] = -x[i]; \
    }

#define DO_VS_OP2(T, a, OP, s) \
  octave_idx_type l = a.length (); \
  if (l > 0) \
    { \
      T *tmp = a.fortran_vec (); \
      for (octave_idx_type i = 0; i < l; i++) \
	tmp[i] OP s; \
    }

#define DO_VV_OP2(T, a, OP, b) \
  do \
    { \
      T *a_tmp = a.fortran_vec (); \
      const T *b_tmp = b.data (); \
      for (octave_idx_type i = 0; i < l; i++) \
	a_tmp[i] OP b_tmp[i]; \
    } \
  while (0)

// Instantiate the OP= operators.
#define MARRAY_OP_ASSIGN_DEFS(A_T, E_T, RHS_T, API) \
  MARRAY_OP_ASSIGN_DECLS (A_T, E_T, template, API, , RHS_T)

// Instantiate the unary operators.
#define MARRAY_UNOP_DEFS(A_T, E_T, API) \
  MARRAY_UNOP_DECLS (A_T, E_T, template, API, )

// Instantiate the binary operators.
#define MARRAY_BINOP_DEFS(A_T, E_T, API) \
  MARRAY_BINOP_DECLS (A_T, E_T, template, API, , A_T<E_T>, E_T) \
  MARRAY_BINOP_DECLS (A_T, E_T, template, API, , E_T, A_T<E_T>) \
  MARRAY_AA_BINOP_DECLS (A_T, E_T, template, API, )

#define MDIAGARRAY2_BINOP_DEFS(A_T, E_T, API) \
  MDIAGARRAY2_DAS_BINOP_DECLS (A_T, E_T, template, API, , A_T<E_T>, E_T) \
  MDIAGARRAY2_SDA_BINOP_DECLS (A_T, E_T, template, API, , E_T, A_T<E_T>) \
  MDIAGARRAY2_DADA_BINOP_DECLS (A_T, E_T, template, API, )

// The following macros are for external use.

// Instantiate all the MArray friends for MArray element type T.
#define INSTANTIATE_MARRAY_FRIENDS(T, API) \
  MARRAY_OP_ASSIGN_DEFS (MArray, T, T, API) \
  MARRAY_OP_ASSIGN_DEFS (MArray, T, MArray<T>, API) \
  MARRAY_UNOP_DEFS (MArray, T, API) \
  MARRAY_BINOP_DEFS (MArray, T, API)

// Instantiate all the MArray2 friends for MArray2 element type T.
#define INSTANTIATE_MARRAY2_FRIENDS(T, API) \
  MARRAY_OP_ASSIGN_DEFS (MArray2, T, T, API) \
  MARRAY_OP_ASSIGN_DEFS (MArray2, T, MArray2<T>, API) \
  MARRAY_UNOP_DEFS (MArray2, T, API) \
  MARRAY_BINOP_DEFS (MArray2, T, API)

// Instantiate all the MArrayN friends for MArrayN element type T.
#define INSTANTIATE_MARRAYN_FRIENDS(T, API) \
  MARRAY_OP_ASSIGN_DEFS (MArrayN, T, T, API) \
  MARRAY_OP_ASSIGN_DEFS (MArrayN, T, MArrayN<T>, API) \
  MARRAY_UNOP_DEFS (MArrayN, T, API) \
  MARRAY_BINOP_DEFS (MArrayN, T, API)

// Instantiate all the MDiagArray2 friends for MDiagArray2 element type T.
#define INSTANTIATE_MDIAGARRAY2_FRIENDS(T, API) \
  MARRAY_OP_ASSIGN_DEFS (MDiagArray2, T, MDiagArray2<T>, API) \
  MARRAY_UNOP_DEFS (MDiagArray2, T, API) \
  MDIAGARRAY2_BINOP_DEFS (MDiagArray2, T, API)

// Now we have all the definitions we need.

#endif
