// tc-inlines.h                                          -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

// Just a coupla more helper functions.

static inline int
tree_to_mat_idx (double x)
{
  if (x > 0)
    return ((int) (x + 0.5) - 1);
  else
    return ((int) (x - 0.5) - 1);
}

static inline int
range_max_check (int i, int imax)
{
  i++;
  if (i > imax)
    {
      error ("matrix index = %d exceeds maximum dimension = %d", i, imax);
      return -1;
    }
  return 0;
}

static inline int
range_max_check (int i, int j, int nr, int nc)
{
  int status = 0;
  i++;
  if (i > nr)
    {
      error ("matrix row index = %d exceeds maximum row dimension = %d",
	     i, nr);
      status = -1;
    }

  j++;
  if (j > nc)
    {
      error ("matrix column index = %d exceeds maximum column dimension = %d",
	     j, nc);
      status = -1;
    }
  return status;
}

static inline int
indexed_assign_conforms (int lhs_nr, int lhs_nc, int rhs_nr, int rhs_nc)
{
  return (lhs_nr == rhs_nr && lhs_nc == rhs_nc);
}

static inline int
is_one_zero (const Range& r)
{
  double b = r.base ();
  double l = r.limit ();
  return (r.nelem () == 2 && NINT (b) == 1 && NINT (l) == 0);
}

static inline int
is_zero_one (const Range& r)
{
  double b = r.base ();
  double l = r.limit ();
  return (r.nelem () == 2 && NINT (b) == 0 && NINT (l) == 1);
}

static inline int
index_check (int i, char *rc)
{
  if (i < 0)
    {
      error ("invalid %s index = %d", rc, i+1);
      return -1;
    }
  return 0;
}

static inline int
index_check (const Range& r, char *rc)
{
  if (r.nelem () < 1)
    {
      error ("range invalid as %s index", rc);
      return -1;
    }

  int imin = tree_to_mat_idx (r.min ());

  if (imin < 0)
    {
      error ("invalid %s index = %d", rc, imin+1);
      return -1;
    }

  return 0;
}

static inline int
fortran_row (int i, int nr)
{
  int r;
  r = i % nr;
  if (r == 0)
    r = nr;
  return r;
}

static inline int
fortran_column (int i, int nr)
{
  int c;
  int r;
  r = fortran_row (i, nr);
  c = (i - r) / nr + 1;
  return c;
}

// How about a few macros?

#define TC_REP tree_constant::tree_constant_rep

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

#ifndef ABS
#define ABS(x) (((x) < 0) ? (-x) : (x))
#endif

// The following are used by some of the functions in the
// tree_constant_rep class that must deal with real and complex
// matrices.  This was not done with overloaded or virtual functions
// from the Matrix class because there is no clean way to do that --
// the necessary functions (like elem) need to return values of
// different types...

// Given a tree_constant, and the names to be used for the real and
// complex matrix and their dimensions, declare a real or complex
// matrix, and initialize it from the tree_constant.  Note that m, cm,
// nr, and nc must not be previously declared, and they must not be
// expressions.  Since only one of the matrices will be defined after
// this macro is used, only one set of dimesions is declared.

// This macro only makes sense inside a friend or member function of
// the tree_constant_rep class

#define REP_RHS_MATRIX(tc,m,cm,nr,nc) \
  int nr = 0; \
  int nc = 0; \
  Matrix m; \
  ComplexMatrix cm; \
  if ((tc).const_type () == TC_REP::complex_matrix_constant) \
    { \
      cm = (tc).complex_matrix_value (); \
      nr = (cm).rows (); \
      nc = (cm).columns (); \
    } \
  else if ((tc).const_type () == TC_REP::matrix_constant) \
    { \
      m = (tc).matrix_value (); \
      nr = (m).rows (); \
      nc = (m).columns (); \
    } \
  else \
    panic_impossible ();

// Assign a real or complex value to a tree_constant.
//
// This macro only makes sense inside a friend or member function of
// the tree_constant_rep class.

#define REP_ELEM_ASSIGN(i,j,rval,cval,real_type) \
  do \
    { \
      if (type_tag == TC_REP::matrix_constant) \
        { \
          if (real_type) \
            matrix->elem ((i), (j)) = (rval); \
          else \
            panic_impossible (); \
        } \
      else \
        { \
          if (real_type) \
            complex_matrix->elem ((i), (j)) = (rval); \
          else \
            complex_matrix->elem ((i), (j)) = (cval); \
        } \
    } \
  while (0)

// Given a real and complex matrix and row and column dimensions,
// declare both and size one of them.  Only one of the matrices should
// be used after this macro has been used.

// This macro only makes sense inside a friend or member function of
// the tree_constant_rep class.

#define CRMATRIX(m,cm,nr,nc) \
  Matrix m; \
  ComplexMatrix cm; \
  if (type_tag == TC_REP::matrix_constant) \
    (m).resize ((nr), (nc)); \
  else if (type_tag == complex_matrix_constant) \
    (cm).resize ((nr), (nc)); \
  else \
    panic_impossible (); \

// Assign a real or complex matrix to a tree constant.

// This macro only makes sense inside a friend or member function of
// the tree_constant_rep class.

#define ASSIGN_CRMATRIX_TO(tc,m,cm) \
  do \
    { \
      if (type_tag == matrix_constant) \
        tc = tree_constant (m); \
      else \
        tc = tree_constant (cm); \
    } \
  while (0)

// Assign an element of this tree_constant_rep's real or complex
// matrix to another real or complex matrix.

// This macro only makes sense inside a friend or member function of
// the tree_constant_rep class.

#define CRMATRIX_ASSIGN_REP_ELEM(m,cm,i1,j1,i2,j2) \
  do \
    { \
      if (type_tag == matrix_constant) \
        (m).elem ((i1), (j1)) = matrix->elem ((i2), (j2)); \
      else \
        (cm).elem ((i1), (j1)) = complex_matrix->elem ((i2), (j2)); \
    } \
  while (0)

// Assign a value to an element of a real or complex matrix.  Assumes
// that the lhs and rhs are either both real or both complex types.

#define CRMATRIX_ASSIGN_ELEM(m,cm,i,j,rval,cval,real_type) \
  do \
    { \
      if (real_type) \
        (m).elem ((i), (j)) = (rval); \
      else \
        (cm).elem ((i), (j)) = (cval); \
    } \
  while (0)


// One more...

static inline int
valid_scalar_indices (const Octave_object& args)
{
  int nargin = args.length ();

  return ((nargin == 2
	   && args(1).valid_as_scalar_index ()
	   && args(0).valid_as_scalar_index ())
	  || (nargin == 1
	      && args(0).valid_as_scalar_index ()));
}

static inline int
valid_zero_index (const Octave_object& args)
{
  int nargin = args.length ();

  return ((nargin == 2
	   && args(1).valid_as_zero_index ()
	   && args(0).valid_as_zero_index ())
	  || (nargin == 1
	      && args(0).valid_as_zero_index ()));
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
