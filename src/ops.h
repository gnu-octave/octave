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

#if !defined (octave_ops_h)
#define octave_ops_h 1

extern void install_ops (void);

#define INSTALL_UNOP(op, t, f) \
  octave_value_typeinfo::register_unary_op \
    (octave_value::op, t::static_type_id (), f);

#define INSTALL_BINOP(op, t1, t2, f) \
  octave_value_typeinfo::register_binary_op \
    (octave_value::op, t1::static_type_id (), t2::static_type_id (), f);

#define INSTALL_ASSIGNOP(t1, t2, f) \
  octave_value_typeinfo::register_assign_op \
    (t1::static_type_id (), t2::static_type_id (), f);

#define INSTALL_ASSIGNCONV(t1, t2, tr) \
  octave_value_typeinfo::register_pref_assign_conv \
    (t1::static_type_id (), t2::static_type_id (), tr::static_type_id ());

#define INSTALL_WIDENOP(t1, t2, f) \
  octave_value_typeinfo::register_widening_op \
    (t1::static_type_id (), t2::static_type_id (), f);

#define BOOL_OP1(xt, xn, get_x, yt, yn, get_y) \
  xt xn = get_x; \
  yt yn = get_y;

#define BOOL_OP2(x) \
  int nr = x.rows (); \
  int nc = x.columns ();

#define BOOL_OP3(test) \
  Matrix retval (nr, nc); \
  for (int j = 0; j < nc; j++) \
    for (int i = 0; i < nr; i++) \
      retval (i, j) = test; \
  return retval;

#define SC_MX_BOOL_OP(st, sn, get_s, mt, mn, get_m, test, empty_result) \
  do \
    { \
      BOOL_OP1 (st, sn, get_s, mt, mn, get_m) \
      BOOL_OP2 (mn) \
      if (nr == 0|| nc == 0) \
        return empty_result; \
      BOOL_OP3 (test) \
    } \
  while (0)

#define MX_SC_BOOL_OP(mt, mn, get_m, st, sn, get_s, test, empty_result) \
  do \
    { \
      BOOL_OP1 (mt, mn, get_m, st, sn, get_s) \
      BOOL_OP2 (mn) \
      if (nr == 0|| nc == 0) \
        return empty_result; \
      BOOL_OP3 (test) \
    } \
  while (0)

#define MX_MX_BOOL_OP(m1t, m1n, get_m1, m2t, m2n, get_m2, test, op, \
		      empty_result) \
  do \
    { \
      BOOL_OP1 (m1t, m1n, get_m1, m2t, m2n, get_m2) \
      int m1_nr = m1n.rows (); \
      int m1_nc = m1n.cols (); \
      int m2_nr = m2n.rows (); \
      int m2_nc = m2n.cols (); \
      if (m1_nr != m2_nr || m1_nc != m2_nc) \
	{ \
	  gripe_nonconformant ("operator " op, m1_nr, m1_nc, m2_nr, m2_nc); \
	  return Matrix (); \
	} \
      if (m1_nr == 0 || m1_nc == 0) \
	return empty_result; \
      BOOL_OP2 (m1n) \
      BOOL_OP3 (test) \
    } \
  while (0)

#define CAST_BINOP_ARGS(t1, t2) \
  t1 v1 = DYNAMIC_CAST (t1, a1); \
  t2 v2 = DYNAMIC_CAST (t2, a2);

#define CAST_CONV_ARG(t) \
  t v = DYNAMIC_CAST (t, a);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
