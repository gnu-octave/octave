/*

Copyright (C) 1996 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_mx_op_defs_h)
#define octave_mx_op_defs_h 1

#include "mx-inlines.cc"

#define BIN_OP_DECL(R, OP, X, Y) \
  extern R OP (const X&, const Y&)

#define MS_OP_DECLS(R, M, S) \
  BIN_OP_DECL (R, operator +, M, S); \
  BIN_OP_DECL (R, operator -, M, S); \
  BIN_OP_DECL (R, operator *, M, S); \
  BIN_OP_DECL (R, operator /, M, S);

#define SM_OP_DECLS(R, S, M) \
  BIN_OP_DECL (R, operator +, S, M); \
  BIN_OP_DECL (R, operator -, S, M); \
  BIN_OP_DECL (R, operator *, S, M); \
  BIN_OP_DECL (R, operator /, S, M);

#define MM_OP_DECLS(R, M1, M2) \
  BIN_OP_DECL (R, operator +, M1, M2); \
  BIN_OP_DECL (R, operator -, M1, M2); \
  BIN_OP_DECL (R, product,    M1, M2); \
  BIN_OP_DECL (R, quotient,   M1, M2);

#define SDM_OP_DECLS(R, S, DM) \
  BIN_OP_DECL (R, operator +, S, DM); \
  BIN_OP_DECL (R, operator -, S, DM);

#define DMS_OP_DECLS(R, DM, S) \
  BIN_OP_DECL (R, operator +, DM, S); \
  BIN_OP_DECL (R, operator -, DM, S);

#define MDM_OP_DECLS(R, M, DM) \
  BIN_OP_DECL (R, operator +, M, DM); \
  BIN_OP_DECL (R, operator -, M, DM); \
  BIN_OP_DECL (R, operator *, M, DM);

#define DMM_OP_DECLS(R, DM, M) \
  BIN_OP_DECL (R, operator +, DM, M); \
  BIN_OP_DECL (R, operator -, DM, M); \
  BIN_OP_DECL (R, operator *, DM, M);

#define DMDM_OP_DECLS(R, DM1, DM2) \
  BIN_OP_DECL (R, operator +, DM1, DM2); \
  BIN_OP_DECL (R, operator -, DM1, DM2); \
  BIN_OP_DECL (R, product, DM1, DM2);

#define MS_OP(R, OP, M, S, F) \
  R \
  OP (const M& m, const S& s) \
  { \
    int nr = m.rows (); \
    int nc = m.cols (); \
 \
    R r (nr, nc); \
 \
    if (nr > 0 && nc > 0) \
      F ## _vs (r.fortran_vec (), m.data (), nr * nc, s); \
 \
    return r; \
  }

#define MS_OPS(R, M, S) \
  MS_OP (R, operator +, M, S, add) \
  MS_OP (R, operator -, M, S, subtract) \
  MS_OP (R, operator *, M, S, multiply) \
  MS_OP (R, operator /, M, S, divide)

#define SM_OP(R, OP, S, M, F) \
  R \
  OP (const S& s, const M& m) \
  { \
    int nr = m.rows (); \
    int nc = m.cols (); \
 \
    R r (nr, nc); \
 \
    if (nr > 0 && nc > 0) \
      F ## _sv (r.fortran_vec (), s, m.data (), nr * nc); \
 \
    return r; \
  }

#define SM_OPS(R, S, M) \
  SM_OP (R, operator +, S, M, add) \
  SM_OP (R, operator -, S, M, subtract) \
  SM_OP (R, operator *, S, M, multiply) \
  SM_OP (R, operator /, S, M, divide)

#define MM_OP(R, OP, M1, M2, F) \
  R \
  OP (const M1& m1, const M2& m2) \
  { \
    R r; \
 \
    int m1_nr = m1.rows (); \
    int m1_nc = m1.cols (); \
 \
    int m2_nr = m2.rows (); \
    int m2_nc = m2.cols (); \
 \
    if (m1_nr != m2_nr || m1_nc != m2_nc) \
      gripe_nonconformant (#OP, m1_nr, m1_nc, m2_nr, m2_nc); \
    else \
      { \
	r.resize (m1_nr, m1_nc); \
 \
	if (m1_nr > 0 && m1_nc > 0) \
	  F ## _vv (r.fortran_vec (), m1.data (), m2.data (), m1_nr * m1_nc); \
      } \
 \
    return r; \
  }

#define MM_OPS(R, M1, M2) \
  MM_OP (R, operator +, M1, M2, add) \
  MM_OP (R, operator -, M1, M2, subtract) \
  MM_OP (R, product,    M1, M2, multiply) \
  MM_OP (R, quotient,   M1, M2, divide)

#define SDM_OP(R, OP, S, DM, OPEQ) \
  R \
  OP (const S& s, const DM& dm) \
  { \
    int nr = dm.rows (); \
    int nc = dm.cols (); \
 \
    R r (nr, nc, s); \
 \
    int len = dm.length (); \
 \
    for (int i = 0; i < len; i++) \
      r.elem (i, i) OPEQ dm.elem (i, i); \
 \
    return r; \
}

#define SDM_OPS(R, S, DM) \
  SDM_OP (R, operator +, S, DM, +=) \
  SDM_OP (R, operator -, S, DM, -=)

#define DMS_OP(R, OP, DM, S, SGN) \
  R \
  OP (const DM& dm, const S& s) \
  { \
    int nr = dm.rows (); \
    int nc = dm.cols (); \
 \
    R r (nr, nc, SGN s); \
 \
    int len = dm.length (); \
 \
    for (int i = 0; i < len; i++) \
      r.elem (i, i) += dm.elem (i, i); \
 \
    return r; \
  }

#define DMS_OPS(R, DM, S) \
  DMS_OP (R, operator +, DM, S, ) \
  DMS_OP (R, operator -, DM, S, -)

#define MDM_OP(R, OP, M, DM, OPEQ) \
R \
OP (const M& m, const DM& dm) \
{ \
  R r; \
 \
  int m_nr = m.rows (); \
  int m_nc = m.cols (); \
 \
  int dm_nr = dm.rows (); \
  int dm_nc = dm.cols (); \
 \
  if (m_nr != dm_nr || m_nc != dm_nc) \
    gripe_nonconformant (#OP, m_nr, m_nc, dm_nr, dm_nc); \
  else \
    { \
      r.resize (m_nr, m_nc); \
 \
      if (m_nr > 0 && m_nc > 0) \
	{ \
	  r = m; \
 \
	  int len = dm.length (); \
 \
	  for (int i = 0; i < len; i++) \
	    r.elem (i, i) OPEQ dm.elem (i, i); \
	} \
    } \
 \
  return r; \
}

#define MDM_MULTIPLY_OP(R, M, DM) \
R \
operator * (const M& m, const DM& dm) \
{ \
  R r; \
 \
  int m_nr = m.rows (); \
  int m_nc = m.cols (); \
 \
  int dm_nr = dm.rows (); \
  int dm_nc = dm.cols (); \
 \
  if (m_nc != dm_nr) \
    gripe_nonconformant ("operator *", m_nr, m_nc, dm_nr, dm_nc); \
  else \
    { \
      r.resize (m_nr, dm_nc, 0.0); \
 \
      if (m_nr > 0 && m_nc > 0 && dm_nc == 0) \
	{ \
	  for (int j = 0; j < dm.length (); j++) \
	    { \
	      if (dm.elem (j, j) == 1.0) \
		{ \
		  for (int i = 0; i < m_nr; i++) \
		    r.elem (i, j) = m.elem (i, j); \
		} \
	      else if (dm.elem (j, j) != 0.0) \
		{ \
		  for (int i = 0; i < m_nr; i++) \
		    r.elem (i, j) = dm.elem (j, j) * m.elem (i, j); \
		} \
	    } \
	} \
    } \
 \
  return r; \
}

#define MDM_OPS(R, M, DM) \
  MDM_OP (R, operator +, M, DM, +=) \
  MDM_OP (R, operator -, M, DM, -=) \
  MDM_MULTIPLY_OP (R, M, DM)

// XXX FIXME XXX -- DM - M will not give the correct result.

#define DMM_OP(R, OP, DM, M, OPEQ) \
R \
OP (const DM& dm, const M& m) \
{ \
  R r; \
 \
  int dm_nr = dm.rows (); \
  int dm_nc = dm.cols (); \
 \
  int m_nr = m.rows (); \
  int m_nc = m.cols (); \
 \
  if (dm_nr != m_nr || dm_nc != m_nc) \
    gripe_nonconformant (#OP, dm_nr, dm_nc, m_nr, m_nc); \
  else \
    { \
      if (m_nr > 0 && m_nc > 0) \
	{ \
	  r = m; \
 \
	  int len = dm.length (); \
 \
	  for (int i = 0; i < len; i++) \
	    r.elem (i, i) OPEQ dm.elem (i, i); \
	} \
      else \
	r.resize (m_nr, m_nc); \
    } \
 \
  return r; \
}

#define DMM_MULTIPLY_OP(R, DM, M) \
R \
operator * (const DM& dm, const M& m) \
{ \
  R r; \
 \
  int dm_nr = dm.rows (); \
  int dm_nc = dm.cols (); \
 \
  int m_nr = m.rows (); \
  int m_nc = m.cols (); \
 \
  if (dm_nc != m_nr) \
    gripe_nonconformant ("operator *", dm_nr, dm_nc, m_nr, m_nc); \
  else \
    { \
      r.resize (dm_nr, m_nc, 0.0); \
 \
      if (dm_nr > 0 && dm_nc > 0 && m_nc > 0) \
	{ \
	  for (int i = 0; i < dm.length (); i++) \
	    { \
	      if (dm.elem (i, i) == 1.0) \
		{ \
		  for (int j = 0; j < m_nc; j++) \
		    r.elem (i, j) = m.elem (i, j); \
		} \
	      else if (dm.elem (i, i) != 0.0) \
		{ \
		  for (int j = 0; j < m_nc; j++) \
		    r.elem (i, j) = dm.elem (i, i) * m.elem (i, j); \
		} \
	    } \
	} \
    } \
 \
  return r; \
}

#define DMM_OPS(R, DM, M) \
  DMM_OP (R, operator +, DM, M, +=) \
  DMM_OP (R, operator -, DM, M, -=) \
  DMM_MULTIPLY_OP(R, DM, M)

#define MM_OP(R, OP, M1, M2, F) \
  R \
  OP (const M1& m1, const M2& m2) \
  { \
    R r; \
 \
    int m1_nr = m1.rows (); \
    int m1_nc = m1.cols (); \
 \
    int m2_nr = m2.rows (); \
    int m2_nc = m2.cols (); \
 \
    if (m1_nr != m2_nr || m1_nc != m2_nc) \
      gripe_nonconformant (#OP, m1_nr, m1_nc, m2_nr, m2_nc); \
    else \
      { \
	r.resize (m1_nr, m1_nc); \
 \
	if (m1_nr > 0 && m1_nc > 0) \
	  F ## _vv (r.fortran_vec (), m1.data (), m2.data (), m1_nr * m1_nc); \
      } \
 \
    return r; \
  }

#define DMDM_OP(R, OP, DM1, DM2, F) \
  R \
  OP (const DM1& dm1, const DM2& dm2) \
  { \
    R r; \
 \
    int dm1_nr = dm1.rows (); \
    int dm1_nc = dm1.cols (); \
 \
    int dm2_nr = dm2.rows (); \
    int dm2_nc = dm2.cols (); \
 \
    if (dm1_nr != dm2_nr || dm1_nc != dm2_nc) \
      gripe_nonconformant (#OP, dm1_nr, dm1_nc, dm2_nr, dm2_nc); \
    else \
      { \
	r.resize (dm1_nr, dm1_nc); \
 \
	if (dm1_nr > 0 && dm1_nc > 0) \
	  F ## _vv (r.fortran_vec (), dm1.data (), dm2.data (), \
		    dm1_nr * dm2_nc); \
      } \
 \
    return r; \
  }

#define DMDM_OPS(R, DM1, DM2) \
  DMDM_OP (R, operator +, DM1, DM2, add) \
  DMDM_OP (R, operator -, DM1, DM2, subtract) \
  DMDM_OP (R, product,    DM1, DM2, multiply)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
