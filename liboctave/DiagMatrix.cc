// DiagMatrix manipulations.                             -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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

// I\'m not sure how this is supposed to work if the .h file declares
// several classes, each of which is defined in a separate file...
//
// #ifdef __GNUG__
// #pragma implementation "Matrix.h"
// #endif

#include "Matrix.h"
#include "mx-inlines.cc"
#include "lo-error.h"

/*
 * Diagonal Matrix class.
 */

DiagMatrix::DiagMatrix (int n)
{
  if (n < 0)
    {
      (*current_liboctave_error_handler)
	("can't create matrix with negative dimensions");
      nr = 0;
      nc = 0;
      len = 0;
      data = (double *) NULL;
      return;
    }

  nr = n;
  nc = n;
  len = n;
  if (len > 0)
    data = new double [len];
  else
    data = (double *) NULL;
}

DiagMatrix::DiagMatrix (int n, double val)
{
  if (n < 0)
    {
      (*current_liboctave_error_handler)
	("can't create matrix with negative dimensions");
      nr = 0;
      nc = 0;
      len = 0;
      data = (double *) NULL;
      return;
    }

  nr = n;
  nc = n;
  len = n;
  if (len > 0)
    {
      data = new double [len];
      copy (data, len, val);
    }
  else
    data = (double *) NULL;
}

DiagMatrix::DiagMatrix (int r, int c)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't create matrix with negative dimensions");
      nr = 0;
      nc = 0;
      len = 0;
      data = (double *) NULL;
      return;
    }

  nr = r;
  nc = c;
  len = r < c ? r : c;
  if (len > 0)
    data = new double [len];
  else
    data = (double *) NULL;
}

DiagMatrix::DiagMatrix (int r, int c, double val)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't create matrix with negative dimensions");
      nr = 0;
      nc = 0;
      len = 0;
      data = (double *) NULL;
      return;
    }

  nr = r;
  nc = c;
  len = r < c ? r : c;
  if (len > 0)
    {
      data = new double [len];
      copy (data, len, val);
    }
  else
    data = (double *) NULL;
}

DiagMatrix::DiagMatrix (const RowVector& a)
{
  nr = a.len;
  nc = nr;
  len = nr;
  if (len > 0)
    {
      data = new double [len];
      copy (data, a.data, len);
    }
  else
    data = (double *) NULL;
}

DiagMatrix::DiagMatrix (const ColumnVector& a)
{
  nr = a.len;
  nc = nr;
  len = nr;
  if (len > 0)
    {
      data = new double [len];
      copy (data, a.data, len);
    }
  else
    data = (double *) NULL;
}

DiagMatrix::DiagMatrix (const DiagMatrix& a)
{
  nr = a.nr;
  nc = a.nc;
  len = a.len;
  if (len > 0)
    {
      data = new double [len];
      copy (data, a.data, len);
    }
  else
    data = (double *) NULL;
}

DiagMatrix::DiagMatrix (double a)
{
  nr = 1;
  nc = 1;
  len = 1;
  data = new double [1];
  data[0] = a;
}

DiagMatrix&
DiagMatrix::operator = (const DiagMatrix& a)
{
  if (this != &a)
    {
      delete [] data;
      nr = a.nr;
      nc = a.nc;
      len = a.len;
      if (len > 0)
	{
	  data = new double [len];
	  copy (data, a.data, len);
	}
      else
	data = (double *) NULL;
    }
  return *this;
}

double&
DiagMatrix::checkelem (int r, int c)
{
#ifndef NO_RANGE_CHECK
  if (r < 0 || r >= nr || c < 0 || c >= nc)
    {
      (*current_liboctave_error_handler) ("range error");
      static double foo = 0.0;
      return foo;
    }
#endif

  return elem (r, c);
}

double
DiagMatrix::checkelem (int r, int c) const
{
#ifndef NO_RANGE_CHECK
  if (r < 0 || r >= nr || c < 0 || c >= nc)
    {
      (*current_liboctave_error_handler) ("range error");
      return 0.0;
    }
#endif

  return elem (r, c);
}

DiagMatrix&
DiagMatrix::resize (int r, int c)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimensions");
      return *this;
    }

  int new_len = r < c ? r : c;
  double *new_data = (double *) NULL;
  if (new_len > 0)
    {
      new_data = new double [new_len];

      int min_len = new_len < len ? new_len : len;

      for (int i = 0; i < min_len; i++)
	new_data[i] = data[i];
    }

  delete [] data;
  nr = r;
  nc = c;
  len = new_len;
  data = new_data;

  return *this;
}

DiagMatrix&
DiagMatrix::resize (int r, int c, double val)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimensions");
      return *this;
    }

  int new_len = r < c ? r : c;
  double *new_data = (double *) NULL;
  if (new_len > 0)
    {
      new_data = new double [new_len];

      int min_len = new_len < len ? new_len : len;

      for (int i = 0; i < min_len; i++)
	new_data[i] = data[i];

      for (i = min_len; i < new_len; i++)
	new_data[i] = val;
    }

  delete [] data;
  nr = r;
  nc = c;
  len = new_len;
  data = new_data;

  return *this;
}

int
DiagMatrix::operator == (const DiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    return 0;

  return equal (data, a.data, len);
}

int
DiagMatrix::operator != (const DiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    return 1;

  return !equal (data, a.data, len);
}

DiagMatrix&
DiagMatrix::fill (double val)
{
  copy (data, len, val);
  return *this;
}

DiagMatrix&
DiagMatrix::fill (double val, int beg, int end)
{
  if (beg < 0 || end >= len || end < beg)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  if (end > beg)
    copy (data+beg, beg-end, val);
  return *this;
}

DiagMatrix&
DiagMatrix::fill (const ColumnVector& a)
{
  if (a.len != len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  copy (data, a.data, len);
  return *this;
}

DiagMatrix&
DiagMatrix::fill (const RowVector& a)
{
  if (a.len != len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  copy (data, a.data, len);
  return *this;
}

DiagMatrix&
DiagMatrix::fill (const ColumnVector& a, int beg)
{
  if (beg < 0 || beg + a.len >= len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  copy (data+beg, a.data, a.len);
  return *this;
}

DiagMatrix&
DiagMatrix::fill (const RowVector& a, int beg)
{
  if (beg < 0 || beg + a.len >= len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  copy (data+beg, a.data, a.len);
  return *this;
}

DiagMatrix
DiagMatrix::transpose (void) const
{
  return DiagMatrix (dup (data, len), nc, nr);
}

Matrix
DiagMatrix::extract (int r1, int c1, int r2, int c2) const
{
  if (r1 > r2) { int tmp = r1; r1 = r2; r2 = tmp; }
  if (c1 > c2) { int tmp = c1; c1 = c2; c2 = tmp; }

  int new_r = r2 - r1 + 1;
  int new_c = c2 - c1 + 1;

  Matrix result (new_r, new_c);

  for (int j = 0; j < new_c; j++)
    for (int i = 0; i < new_r; i++)
      result.data[new_r*j+i] = elem (r1+i, c1+j);

  return result;
}

// extract row or column i.

RowVector
DiagMatrix::row (int i) const
{
  if (i < 0 || i >= nr)
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return RowVector (); 
    }

  RowVector retval (nc, 0.0);
  if (nr <= nc || (nr > nc && i < nc))
    retval.data [i] = data[i];

  return retval;
}

RowVector
DiagMatrix::row (char *s) const
{
  if (s == (char *) NULL)
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return RowVector (); 
    }

  char c = *s;
  if (c == 'f' || c == 'F')
    return row (0);
  else if (c == 'l' || c == 'L')
    return row (nr - 1);
  else
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return RowVector (); 
    }
}

ColumnVector
DiagMatrix::column (int i) const
{
  if (i < 0 || i >= nc)
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return ColumnVector (); 
    }

  ColumnVector retval (nr, 0.0);
  if (nr >= nc || (nr < nc && i < nr))
    retval.data [i] = data[i];

  return retval;
}

ColumnVector
DiagMatrix::column (char *s) const
{
  if (s == (char *) NULL)
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return ColumnVector (); 
    }

  char c = *s;
  if (c == 'f' || c == 'F')
    return column (0);
  else if (c == 'l' || c == 'L')
    return column (nc - 1);
  else
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return ColumnVector (); 
    }
}

DiagMatrix
DiagMatrix::inverse (int &info) const
{
  if (nr != nc)
    {
      (*current_liboctave_error_handler) ("inverse requires square matrix");
      return DiagMatrix ();
    }

  info = 0;
  double *tmp_data = dup (data, len);
  for (int i = 0; i < len; i++)
    {
      if (data[i] == 0.0)
	{
	  info = -1;
	  copy (tmp_data, data, len); // Restore contents.
	  break;
	}
      else
	{
	  tmp_data[i] = 1.0 / data[i];
	}
    }

  return DiagMatrix (tmp_data, nr, nc);
}

DiagMatrix
DiagMatrix::inverse (void) const
{
  int info;
  return inverse (info);
}

// diagonal matrix by scalar -> matrix operations

Matrix
DiagMatrix::operator + (double s) const
{
  Matrix tmp (nr, nc, s);
  return *this + tmp;
}

Matrix
DiagMatrix::operator - (double s) const
{
  Matrix tmp (nr, nc, -s);
  return *this + tmp;
}

ComplexMatrix
DiagMatrix::operator + (const Complex& s) const
{
  ComplexMatrix tmp (nr, nc, s);
  return *this + tmp;
}

ComplexMatrix
DiagMatrix::operator - (const Complex& s) const
{
  ComplexMatrix tmp (nr, nc, -s);
  return *this + tmp;
}

// diagonal matrix by scalar -> diagonal matrix operations

DiagMatrix
DiagMatrix::operator * (double s) const
{
  return DiagMatrix (multiply (data, len, s), nr, nc);
}

DiagMatrix
DiagMatrix::operator / (double s) const
{
  return DiagMatrix (divide (data, len, s), nr, nc);
}

ComplexDiagMatrix
DiagMatrix::operator * (const Complex& s) const
{
  return ComplexDiagMatrix (multiply (data, len, s), nr, nc);
}

ComplexDiagMatrix
DiagMatrix::operator / (const Complex& s) const
{
  return ComplexDiagMatrix (divide (data, len, s), nr, nc);
}

// scalar by diagonal matrix -> matrix operations

Matrix
operator + (double s, const DiagMatrix& a)
{
  return a + s;
}

Matrix
operator - (double s, const DiagMatrix& a)
{
  return -a + s;
}

// scalar by diagonal matrix -> diagonal matrix operations

DiagMatrix
operator * (double s, const DiagMatrix& a)
{
  return DiagMatrix (multiply (a.data, a.len, s), a.nr, a.nc);
}

DiagMatrix
operator / (double s, const DiagMatrix& a)
{
  return DiagMatrix (divide (s, a.data, a.len), a.nr, a.nc);
}

// diagonal matrix by column vector -> column vector operations

ColumnVector
DiagMatrix::operator * (const ColumnVector& a) const
{
  if (nc != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return ColumnVector ();
    }

  if (nc == 0 || nr == 0)
    return ColumnVector (0);

  ColumnVector result (nr);

  for (int i = 0; i < a.len; i++)
    result.data[i] = a.data[i] * data[i];

  for (i = a.len; i < nr; i++)
    result.data[i] = 0.0;

  return result;
}

ComplexColumnVector
DiagMatrix::operator * (const ComplexColumnVector& a) const
{
  if (nc != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return ColumnVector ();
    }

  if (nc == 0 || nr == 0)
    return ComplexColumnVector (0);

  ComplexColumnVector result (nr);

  for (int i = 0; i < a.len; i++)
    result.data[i] = a.data[i] * data[i];

  for (i = a.len; i < nr; i++)
    result.data[i] = 0.0;

  return result;
}

// diagonal matrix by diagonal matrix -> diagonal matrix operations

DiagMatrix
DiagMatrix::operator + (const DiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix addition attempted");
      return DiagMatrix ();
    }

  if (nc == 0 || nr == 0)
    return DiagMatrix (nr, nc);

  return DiagMatrix (add (data, a.data, len), nr , nc);
}

DiagMatrix
DiagMatrix::operator - (const DiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix subtraction attempted");
      return DiagMatrix ();
    }

  if (nc == 0 || nr == 0)
    return DiagMatrix (nr, nc);

  return DiagMatrix (subtract (data, a.data, len), nr, nc);
}

DiagMatrix
DiagMatrix::operator * (const DiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return DiagMatrix ();
    }

  if (nc == 0 || nr == 0)
    return DiagMatrix (nr, nc);

  return DiagMatrix (multiply (data, a.data, len), nr, nc);
}

ComplexDiagMatrix
DiagMatrix::operator + (const ComplexDiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix addition attempted");
      return ComplexDiagMatrix ();
    }

  if (nc == 0 || nr == 0)
    return ComplexDiagMatrix (nr, nc);

  return ComplexDiagMatrix (add (data, a.data, len), nr , nc);
}

ComplexDiagMatrix
DiagMatrix::operator - (const ComplexDiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix subtraction attempted");
      return ComplexDiagMatrix ();
    }

  if (nc == 0 || nr == 0)
    return ComplexDiagMatrix (nr, nc);

  return ComplexDiagMatrix (subtract (data, a.data, len), nr, nc);
}

ComplexDiagMatrix
DiagMatrix::operator * (const ComplexDiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return ComplexDiagMatrix ();
    }

  if (nc == 0 || nr == 0)
    return ComplexDiagMatrix (nr, nc);

  return ComplexDiagMatrix (multiply (data, a.data, len), nr, nc);
}

DiagMatrix
DiagMatrix::product (const DiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix product attempted");
      return DiagMatrix ();
    }

  if (nc == 0 || nr == 0)
    return DiagMatrix (nr, nc);

  return DiagMatrix (multiply (data, a.data, len), nr, nc);
}

DiagMatrix
DiagMatrix::quotient (const DiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix quotient attempted");
      return DiagMatrix ();
    }

  if (nc == 0 || nr == 0)
    return DiagMatrix (nr, nc);

  return DiagMatrix (divide (data, a.data, len), nr, nc);
}

ComplexDiagMatrix
DiagMatrix::product (const ComplexDiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix product attempted");
      return ComplexDiagMatrix ();
    }

  if (nc == 0 || nr == 0)
    return ComplexDiagMatrix (nr, nc);

  return ComplexDiagMatrix (multiply (data, a.data, len), nr, nc);
}

ComplexDiagMatrix
DiagMatrix::quotient (const ComplexDiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix quotient attempted");
      return ComplexDiagMatrix ();
    }

  if (nc == 0 || nr == 0)
    return ComplexDiagMatrix (nr, nc);

  return ComplexDiagMatrix (divide (data, a.data, len), nr, nc);
}

DiagMatrix&
DiagMatrix::operator += (const DiagMatrix& a)
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix += operation attempted");
      return *this;
    }

  if (nc == 0 || nr == 0)
    return *this;

  add2 (data, a.data, len);
  return *this;
}

DiagMatrix&
DiagMatrix::operator -= (const DiagMatrix& a)
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix -= operation attempted");
      return *this;
    }

  if (nr == 0 || nc == 0)

  subtract2 (data, a.data, len);
  return *this;
}

// diagonal matrix by matrix -> matrix operations

Matrix
DiagMatrix::operator + (const Matrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix addition attempted");
      return Matrix ();
    }

  if (nr == 0 || nc == 0)
    return Matrix (nr, nc);

  Matrix result (a);
  for (int i = 0; i < len; i++)
    result.elem (i, i) += data[i];

  return result;
}

Matrix
DiagMatrix::operator - (const Matrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix subtraction attempted");
      return Matrix ();
    }

  if (nr == 0 || nc == 0)
    return Matrix (nr, nc);

  Matrix result (-a);
  for (int i = 0; i < len; i++)
    result.elem (i, i) += data[i];

  return result;
}

Matrix
DiagMatrix::operator * (const Matrix& a) const
{
  if (nc != a.nr)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return Matrix ();
    }

  if (nr == 0 || nc == 0 || a.nc == 0)
    return Matrix (nr, a.nc, 0.0);

  Matrix c (nr, a.nc);

  for (int i = 0; i < len; i++)
    {
      if (data[i] == 1.0)
	{
	  for (int j = 0; j < a.nc; j++)
	    c.elem (i, j) = a.elem (i, j);
	}
      else if (data[i] == 0.0)
	{
	  for (int j = 0; j < a.nc; j++)
	    c.elem (i, j) = 0.0;
	}
      else
	{
	  for (int j = 0; j < a.nc; j++)
	    c.elem (i, j) = data[i] * a.elem (i, j);
	}
    }

  if (nr > nc)
    {
      for (int j = 0; j < a.nc; j++)
	for (int i = a.nr; i < nr; i++)
	  c.elem (i, j) = 0.0;
    }

  return c;
}

ComplexMatrix
DiagMatrix::operator + (const ComplexMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix addition attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (a);
  for (int i = 0; i < len; i++)
    result.elem (i, i) += data[i];

  return result;
}

ComplexMatrix
DiagMatrix::operator - (const ComplexMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix subtraction attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (-a);
  for (int i = 0; i < len; i++)
    result.elem (i, i) += data[i];

  return result;
}

ComplexMatrix
DiagMatrix::operator * (const ComplexMatrix& a) const
{
  if (nc != a.nr)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0 || a.nc == 0)
    return ComplexMatrix (nr, nc, 0.0);

  ComplexMatrix c (nr, a.nc);

  for (int i = 0; i < len; i++)
    {
      if (data[i] == 1.0)
	{
	  for (int j = 0; j < a.nc; j++)
	    c.elem (i, j) = a.elem (i, j);
	}
      else if (data[i] == 0.0)
	{
	  for (int j = 0; j < a.nc; j++)
	    c.elem (i, j) = 0.0;
	}
      else
	{
	  for (int j = 0; j < a.nc; j++)
	    c.elem (i, j) = data[i] * a.elem (i, j);
	}
    }

  if (nr > nc)
    {
      for (int j = 0; j < a.nc; j++)
	for (int i = a.nr; i < nr; i++)
	  c.elem (i, j) = 0.0;
    }

  return c;
}

// unary operations

DiagMatrix
DiagMatrix::operator - (void) const
{
  return DiagMatrix (negate (data, len), nr, nc);
}

ColumnVector
DiagMatrix::diag (void) const
{
  return diag (0);
}

// Could be optimized...

ColumnVector
DiagMatrix::diag (int k) const
{
  int nnr = nr;
  int nnc = nc;
  if (k > 0)
    nnc -= k;
  else if (k < 0)
    nnr += k;

  ColumnVector d;

  if (nnr > 0 && nnc > 0)
    {
      int ndiag = (nnr < nnc) ? nnr : nnc;

      d.resize (ndiag);

      if (k > 0)
	{
	  for (int i = 0; i < ndiag; i++)
	    d.elem (i) = elem (i, i+k);
	}
      else if ( k < 0)
	{
	  for (int i = 0; i < ndiag; i++)
	    d.elem (i) = elem (i-k, i);
	}
      else
	{
	  for (int i = 0; i < ndiag; i++)
	    d.elem (i) = elem (i, i);
	}
    }
  else
    cerr << "diag: requested diagonal out of range\n";

  return d;
}

ostream&
operator << (ostream& os, const DiagMatrix& a)
{
  double ZERO = 0.0;
//  int field_width = os.precision () + 7;
  for (int i = 0; i < a.nr; i++)
    {
      for (int j = 0; j < a.nc; j++)
	{
	  if (i == j)
	    os << " " /* setw (field_width) */ << a.data[i];
	  else
	    os << " " /* setw (field_width) */ << ZERO;
	}
      os << "\n";
    }
  return os;
}

/*
 * Complex Diagonal Matrix class
 */

ComplexDiagMatrix::ComplexDiagMatrix (int n)
{
  if (n < 0)
    {
      (*current_liboctave_error_handler)
	("can't create matrix with negative dimensions");
      nr = 0;
      nc = 0;
      len = 0;
      data = (Complex *) NULL;
      return;
    }

  nr = n;
  nc = n;
  len = n;
  if (len > 0)
    data = new Complex [len];
  else
    data = (Complex *) NULL;
}

ComplexDiagMatrix::ComplexDiagMatrix (int n, double val)
{
  if (n < 0)
    {
      (*current_liboctave_error_handler)
	("can't create matrix with negative dimensions");
      nr = 0;
      nc = 0;
      len = 0;
      data = (Complex *) NULL;
      return;
    }

  nr = n;
  nc = n;
  len = n;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, len, val);
    }
  else
    data = (Complex *) NULL;
}

ComplexDiagMatrix::ComplexDiagMatrix (int n, const Complex& val)
{
  if (n < 0)
    {
      (*current_liboctave_error_handler)
	("can't create matrix with negative dimensions");
      nr = 0;
      nc = 0;
      len = 0;
      data = (Complex *) NULL;
      return;
    }

  nr = n;
  nc = n;
  len = n;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, len, val);
    }
  else
    data = (Complex *) NULL;
}

ComplexDiagMatrix::ComplexDiagMatrix (int r, int c)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't create matrix with negative dimensions");
      nr = 0;
      nc = 0;
      len = 0;
      data = (Complex *) NULL;
      return;
    }

  nr = r;
  nc = c;
  len = r < c ? r : c;
  if (len > 0)
    data = new Complex [len];
  else
    data = (Complex *) NULL;
}

ComplexDiagMatrix::ComplexDiagMatrix (int r, int c, double val)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't create matrix with negative dimensions");
      nr = 0;
      nc = 0;
      len = 0;
      data = (Complex *) NULL;
      return;
    }

  nr = r;
  nc = c;
  len = r < c ? r : c;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, len, val);
    }
  else
    data = (Complex *) NULL;
}

ComplexDiagMatrix::ComplexDiagMatrix (int r, int c, const Complex& val)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't create matrix with negative dimensions");
      nr = 0;
      nc = 0;
      len = 0;
      data = (Complex *) NULL;
      return;
    }

  nr = r;
  nc = c;
  len = r < c ? r : c;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, len, val);
    }
  else
    data = (Complex *) NULL;
}

ComplexDiagMatrix::ComplexDiagMatrix (const RowVector& a)
{
  nr = a.len;
  nc = nr;
  len = nr;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, a.data, len);
    }
  else
    data = (Complex *) NULL;
}

ComplexDiagMatrix::ComplexDiagMatrix (const ComplexRowVector& a)
{
  nr = a.len;
  nc = nr;
  len = nr;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, a.data, len);
    }
  else
    data = (Complex *) NULL;
}

ComplexDiagMatrix::ComplexDiagMatrix (const ColumnVector& a)
{
  nr = a.len;
  nc = nr;
  len = nr;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, a.data, len);
    }
  else
    data = (Complex *) NULL;
}

ComplexDiagMatrix::ComplexDiagMatrix (const ComplexColumnVector& a)
{
  nr = a.len;
  nc = nr;
  len = nr;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, a.data, len);
    }
  else
    data = (Complex *) NULL;
}

ComplexDiagMatrix::ComplexDiagMatrix (const DiagMatrix& a)
{
  nr = a.nr;
  nc = a.nc;
  len = a.len;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, a.data, len);
    }
  else
    data = (Complex *) NULL;
}

ComplexDiagMatrix::ComplexDiagMatrix (const ComplexDiagMatrix& a)
{
  nr = a.nr;
  nc = a.nc;
  len = a.len;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, a.data, len);
    }
  else
    data = (Complex *) NULL;
}

ComplexDiagMatrix::ComplexDiagMatrix (double a)
{
  nr = 1;
  nc = 1;
  len = 1;
  data = new Complex [1];
  data[0] = a;
}

ComplexDiagMatrix::ComplexDiagMatrix (const Complex& a)
{
  nr = 1;
  nc = 1;
  len = 1;
  data = new Complex [1];
  data[0] = Complex (a);
}

ComplexDiagMatrix&
ComplexDiagMatrix::operator = (const DiagMatrix& a)
{
  delete [] data;
  nr = a.nr;
  nc = a.nc;
  len = a.len;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, a.data, len);
    }
  else
    data = (Complex *) NULL;

  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::operator = (const ComplexDiagMatrix& a)
{
  if (this != &a)
    {
      delete [] data;
      nr = a.nr;
      nc = a.nc;
      len = a.len;
      if (len > 0)
	{
	  data = new Complex [len];
	  copy (data, a.data, len);
	}
      else
	data = (Complex *) NULL;
    }
  return *this;
}

Complex&
ComplexDiagMatrix::checkelem (int r, int c)
{
#ifndef NO_RANGE_CHECK
  if (r < 0 || r >= nr || c < 0 || c >= nc)
    {
      (*current_liboctave_error_handler) ("range error");
      static Complex foo (0.0);
      return foo;
    }
#endif

  return elem (r, c);
}

Complex
ComplexDiagMatrix::checkelem (int r, int c) const
{
#ifndef NO_RANGE_CHECK
  if (r < 0 || r >= nr || c < 0 || c >= nc)
    {
      (*current_liboctave_error_handler) ("range error");
      return Complex (0.0);
    }
#endif

  return elem (r, c);
}

ComplexDiagMatrix&
ComplexDiagMatrix::resize (int r, int c)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimensions");
      return *this;
    }

  int new_len = r < c ? r : c;
  Complex *new_data = (Complex *) NULL;
  if (new_len > 0)
    {
      new_data = new Complex [new_len];

      int min_len = new_len < len ? new_len : len;

      for (int i = 0; i < min_len; i++)
	new_data[i] = data[i];
    }

  delete [] data;
  nr = r;
  nc = c;
  len = new_len;
  data = new_data;

  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::resize (int r, int c, double val)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimensions");
      return *this;
    }

  int new_len = r < c ? r : c;
  Complex *new_data = (Complex *) NULL;
  if (new_len > 0)
    {
      new_data = new Complex [new_len];

      int min_len = new_len < len ? new_len : len;

      for (int i = 0; i < min_len; i++)
	new_data[i] = data[i];

      for (i = min_len; i < new_len; i++)
	new_data[i] = val;
    }

  delete [] data;
  nr = r;
  nc = c;
  len = new_len;
  data = new_data;

  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::resize (int r, int c, const Complex& val)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimensions");
      return *this;
    }

  int new_len = r < c ? r : c;
  Complex *new_data = (Complex *) NULL;
  if (new_len > 0)
    {
      new_data = new Complex [new_len];

      int min_len = new_len < len ? new_len : len;

      for (int i = 0; i < min_len; i++)
	new_data[i] = data[i];

      for (i = min_len; i < new_len; i++)
	new_data[i] = val;
    }

  delete [] data;
  nr = r;
  nc = c;
  len = new_len;
  data = new_data;

  return *this;
}

int
ComplexDiagMatrix::operator == (const ComplexDiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    return 0;

  return equal (data, a.data, len);
}

int
ComplexDiagMatrix::operator != (const ComplexDiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    return 1;

  return !equal (data, a.data, len);
}

ComplexDiagMatrix
ComplexDiagMatrix::hermitian (void) const
{
  return ComplexDiagMatrix (conj_dup (data, len), nc, nr);
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (double val)
{
  copy (data, len, val);
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const Complex& val)
{
  copy (data, len, val);
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (double val, int beg, int end)
{
  if (beg < 0 || end >= len || end < beg)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  if (end > beg)
    copy (data+beg, beg-end, val);
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const Complex& val, int beg, int end)
{
  if (beg < 0 || end >= len || end < beg)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  if (end > beg)
    copy (data+beg, beg-end, val);
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const ColumnVector& a)
{
  if (a.len != len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  copy (data, a.data, len);
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const ComplexColumnVector& a)
{
  if (a.len != len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  copy (data, a.data, len);
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const RowVector& a)
{
  if (a.len != len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  copy (data, a.data, len);
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const ComplexRowVector& a)
{
  if (a.len != len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  copy (data, a.data, len);
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const ColumnVector& a, int beg)
{
  if (beg < 0 || beg + a.len >= len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  copy (data+beg, a.data, a.len);
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const ComplexColumnVector& a, int beg)
{
  if (beg < 0 || beg + a.len >= len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  copy (data+beg, a.data, a.len);
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const RowVector& a, int beg)
{
  if (beg < 0 || beg + a.len >= len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  copy (data+beg, a.data, a.len);
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::fill (const ComplexRowVector& a, int beg)
{
  if (beg < 0 || beg + a.len >= len)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  copy (data+beg, a.data, a.len);
  return *this;
}

ComplexDiagMatrix
ComplexDiagMatrix::transpose (void) const
{
  return ComplexDiagMatrix (dup (data, len), nc, nr);
}

DiagMatrix
real (const ComplexDiagMatrix& a)
{
  DiagMatrix retval;
  if (a.len > 0)
    retval = DiagMatrix (real_dup (a.data, a.len), a.nr, a.nc);
  return retval;
}

DiagMatrix
imag (const ComplexDiagMatrix& a)
{
  DiagMatrix retval;
  if (a.len > 0)
    retval = DiagMatrix (imag_dup (a.data, a.len), a.nr, a.nc);
  return retval;
}

ComplexDiagMatrix
conj (const ComplexDiagMatrix& a)
{
  ComplexDiagMatrix retval;
  if (a.len > 0)
    retval = ComplexDiagMatrix (conj_dup (a.data, a.len), a.nr, a.nc);
  return retval;
}

// resize is the destructive analog for this one

ComplexMatrix
ComplexDiagMatrix::extract (int r1, int c1, int r2, int c2) const
{
  if (r1 > r2) { int tmp = r1; r1 = r2; r2 = tmp; }
  if (c1 > c2) { int tmp = c1; c1 = c2; c2 = tmp; }

  int new_r = r2 - r1 + 1;
  int new_c = c2 - c1 + 1;

  ComplexMatrix result (new_r, new_c);

  for (int j = 0; j < new_c; j++)
    for (int i = 0; i < new_r; i++)
      result.data[new_r*j+i] = elem (r1+i, c1+j);

  return result;
}

// extract row or column i.

ComplexRowVector
ComplexDiagMatrix::row (int i) const
{
  if (i < 0 || i >= nr)
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return RowVector (); 
    }

  ComplexRowVector retval (nc, 0.0);
  if (nr <= nc || (nr > nc && i < nc))
    retval.data [i] = data[i];

  return retval;
}

ComplexRowVector
ComplexDiagMatrix::row (char *s) const
{
  if (s == (char *) NULL)
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return ComplexRowVector (); 
    }

  char c = *s;
  if (c == 'f' || c == 'F')
    return row (0);
  else if (c == 'l' || c == 'L')
    return row (nr - 1);
  else
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return ComplexRowVector ();
    }
}

ComplexColumnVector
ComplexDiagMatrix::column (int i) const
{
  if (i < 0 || i >= nc)
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return ColumnVector (); 
    }

  ComplexColumnVector retval (nr, 0.0);
  if (nr >= nc || (nr < nc && i < nr))
    retval.data [i] = data[i];

  return retval;
}

ComplexColumnVector
ComplexDiagMatrix::column (char *s) const
{
  if (s == (char *) NULL)
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return ColumnVector (); 
    }

  char c = *s;
  if (c == 'f' || c == 'F')
    return column (0);
  else if (c == 'l' || c == 'L')
    return column (nc - 1);
  else
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return ColumnVector (); 
    }
}

ComplexDiagMatrix
ComplexDiagMatrix::inverse (int& info) const
{
  if (nr != nc)
    {
      (*current_liboctave_error_handler) ("inverse requires square matrix");
      return DiagMatrix ();
    }

  info = 0;
  for (int i = 0; i < len; i++)
    {
      if (data[i] == 0.0)
	{
	  info = -1;
	  return *this;
	}
      else
	data[i] = 1.0 / data[i];
    }

  return *this;
}

ComplexDiagMatrix
ComplexDiagMatrix::inverse (void) const
{
  int info;
  return inverse (info);
}

// diagonal matrix by scalar -> matrix operations

ComplexMatrix
ComplexDiagMatrix::operator + (double s) const
{
  ComplexMatrix tmp (nr, nc, s);
  return *this + tmp;
}

ComplexMatrix
ComplexDiagMatrix::operator - (double s) const
{
  ComplexMatrix tmp (nr, nc, -s);
  return *this + tmp;
}

ComplexMatrix
ComplexDiagMatrix::operator + (const Complex& s) const
{
  ComplexMatrix tmp (nr, nc, s);
  return *this + tmp;
}

ComplexMatrix
ComplexDiagMatrix::operator - (const Complex& s) const
{
  ComplexMatrix tmp (nr, nc, -s);
  return *this + tmp;
}

// diagonal matrix by scalar -> diagonal matrix operations

ComplexDiagMatrix
ComplexDiagMatrix::operator * (double s) const
{
  return ComplexDiagMatrix (multiply (data, len, s), nr, nc);
}

ComplexDiagMatrix
ComplexDiagMatrix::operator / (double s) const
{
  return ComplexDiagMatrix (divide (data, len, s), nr, nc);
}

ComplexDiagMatrix
ComplexDiagMatrix::operator * (const Complex& s) const
{
  return ComplexDiagMatrix (multiply (data, len, s), nr, nc);
}

ComplexDiagMatrix
ComplexDiagMatrix::operator / (const Complex& s) const
{
  return ComplexDiagMatrix (divide (data, len, s), nr, nc);
}

// scalar by diagonal matrix -> matrix operations

ComplexMatrix
operator + (double s, const ComplexDiagMatrix& a)
{
  return a + s;
}

ComplexMatrix
operator - (double s, const ComplexDiagMatrix& a)
{
  return -a + s;
}

ComplexMatrix
operator + (const Complex& s, const ComplexDiagMatrix& a)
{
  return a + s;
}

ComplexMatrix
operator - (const Complex& s, const ComplexDiagMatrix& a)
{
  return -a + s;
}

// scalar by diagonal matrix -> diagonal matrix operations

ComplexDiagMatrix
operator * (double s, const ComplexDiagMatrix& a)
{
  return ComplexDiagMatrix (multiply (a.data, a.len, s), a.nr, a.nc);
}

ComplexDiagMatrix
 operator / (double s, const ComplexDiagMatrix& a)
{
  return ComplexDiagMatrix (divide (s, a.data, a.len), a.nr, a.nc);
}

ComplexDiagMatrix
 operator * (const Complex& s, const ComplexDiagMatrix& a)
{
  return ComplexDiagMatrix (multiply (a.data, a.len, s), a.nr, a.nc);
}

ComplexDiagMatrix
operator / (const Complex& s, const ComplexDiagMatrix& a)
{
  return ComplexDiagMatrix (divide (s, a.data, a.len), a.nr, a.nc);
}

// diagonal matrix by column vector -> column vector operations

ComplexColumnVector
ComplexDiagMatrix::operator * (const ColumnVector& a) const
{
  if (nc != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix muliplication attempted");
      return ComplexColumnVector ();
    }

  if (nc == 0 || nr == 0)
    return ComplexColumnVector (0);

  ComplexColumnVector result (nr);

  for (int i = 0; i < a.len; i++)
    result.data[i] = a.data[i] * data[i];

  for (i = a.len; i < nr; i++)
    result.data[i] = 0.0;

  return result;
}

ComplexColumnVector
ComplexDiagMatrix::operator * (const ComplexColumnVector& a) const
{
  if (nc != a.len)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix muliplication attempted");
      return ComplexColumnVector ();
    }

  if (nc == 0 || nr == 0)
    return ComplexColumnVector (0);

  ComplexColumnVector result (nr);

  for (int i = 0; i < a.len; i++)
    result.data[i] = a.data[i] * data[i];

  for (i = a.len; i < nr; i++)
    result.data[i] = 0.0;

  return result;
}

// diagonal matrix by diagonal matrix -> diagonal matrix operations

ComplexDiagMatrix
ComplexDiagMatrix::operator + (const DiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix addition attempted");
      return ComplexDiagMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexDiagMatrix (nr, nc);

  return ComplexDiagMatrix (add (data, a.data, len), nr , nc);
}

ComplexDiagMatrix
ComplexDiagMatrix::operator - (const DiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix subtraction attempted");
      return ComplexDiagMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexDiagMatrix (nr, nc);

  return ComplexDiagMatrix (subtract (data, a.data, len), nr, nc);
}

ComplexDiagMatrix
ComplexDiagMatrix::operator * (const DiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return ComplexDiagMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexDiagMatrix (nr, nc);

  return ComplexDiagMatrix (multiply (data, a.data, len), nr, nc);
}

ComplexDiagMatrix
ComplexDiagMatrix::operator + (const ComplexDiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix addition attempted");
      return ComplexDiagMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexDiagMatrix (nr, nc);

  return ComplexDiagMatrix (add (data, a.data, len), nr , nc);
}

ComplexDiagMatrix
ComplexDiagMatrix::operator - (const ComplexDiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix subtraction attempted");
      return ComplexDiagMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexDiagMatrix (nr, nc);

  return ComplexDiagMatrix (subtract (data, a.data, len), nr, nc);
}

ComplexDiagMatrix
ComplexDiagMatrix::operator * (const ComplexDiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return ComplexDiagMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexDiagMatrix (nr, nc);

  return ComplexDiagMatrix (multiply (data, a.data, len), nr, nc);
}

ComplexDiagMatrix
ComplexDiagMatrix::product (const DiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix product attempted");
      return ComplexDiagMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexDiagMatrix (nr, nc);

  return ComplexDiagMatrix (multiply (data, a.data, len), nr, nc);
}

ComplexDiagMatrix
ComplexDiagMatrix::quotient (const DiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix quotient attempted");
      return ComplexDiagMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexDiagMatrix (nr, nc);

  return ComplexDiagMatrix (divide (data, a.data, len), nr, nc);
}

ComplexDiagMatrix
ComplexDiagMatrix::product (const ComplexDiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix product attempted");
      return ComplexDiagMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexDiagMatrix (nr, nc);

  return ComplexDiagMatrix (multiply (data, a.data, len), nr, nc);
}

ComplexDiagMatrix
ComplexDiagMatrix::quotient (const ComplexDiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix quotient attempted");
      return ComplexDiagMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexDiagMatrix (nr, nc);

  return ComplexDiagMatrix (divide (data, a.data, len), nr, nc);
}

ComplexDiagMatrix&
ComplexDiagMatrix::operator += (const DiagMatrix& a)
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix += operation attempted");
      return *this;
    }

  if (nr == 0 || nc == 0)
    return *this;

  add2 (data, a.data, len);
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::operator -= (const DiagMatrix& a)
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix -= operation attempted");
      return *this;
    }

  if (nr == 0 || nc == 0)
    return *this;

  subtract2 (data, a.data, len);
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::operator += (const ComplexDiagMatrix& a)
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix += operation attempted");
      return *this;
    }

  if (nr == 0 || nc == 0)
    return *this;

  add2 (data, a.data, len);
  return *this;
}

ComplexDiagMatrix&
ComplexDiagMatrix::operator -= (const ComplexDiagMatrix& a)
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix -= operation attempted");
      return *this;
    }

  if (nr == 0 || nc == 0)
    return *this;

  subtract2 (data, a.data, len);
  return *this;
}

// diagonal matrix by matrix -> matrix operations

ComplexMatrix
ComplexDiagMatrix::operator + (const Matrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix addition attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (a);
  for (int i = 0; i < len; i++)
    result.elem (i, i) += data[i];

  return result;
}

ComplexMatrix
ComplexDiagMatrix::operator - (const Matrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix subtraction attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (-a);
  for (int i = 0; i < len; i++)
    result.elem (i, i) += data[i];

  return result;
}

ComplexMatrix
ComplexDiagMatrix::operator * (const Matrix& a) const
{
  if (nc != a.nr)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0 || a.nc == 0)
    return ComplexMatrix (nr, a.nc, 0.0);

  ComplexMatrix c (nr, a.nc);

  for (int i = 0; i < len; i++)
    {
      if (data[i] == 1.0)
	{
	  for (int j = 0; j < a.nc; j++)
	    c.elem (i, j) = a.elem (i, j);
	}
      else if (data[i] == 0.0)
	{
	  for (int j = 0; j < a.nc; j++)
	    c.elem (i, j) = 0.0;
	}
      else
	{
	  for (int j = 0; j < a.nc; j++)
	    c.elem (i, j) = data[i] * a.elem (i, j);
	}
    }

  if (nr > nc)
    {
      for (int j = 0; j < a.nc; j++)
	for (int i = a.nr; i < nr; i++)
	  c.elem (i, j) = 0.0;
    }

  return c;
}

ComplexMatrix
ComplexDiagMatrix::operator + (const ComplexMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix addition attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (a);
  for (int i = 0; i < len; i++)
    result.elem (i, i) += data[i];

  return result;
}

ComplexMatrix
ComplexDiagMatrix::operator - (const ComplexMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix subtraction attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (-a);
  for (int i = 0; i < len; i++)
    result.elem (i, i) += data[i];

  return result;
}

ComplexMatrix
ComplexDiagMatrix::operator * (const ComplexMatrix& a) const
{
  if (nc != a.nr)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0 || a.nc == 0)
    return ComplexMatrix (nr, a.nc, 0.0);

  ComplexMatrix c (nr, a.nc);

  for (int i = 0; i < len; i++)
    {
      if (data[i] == 1.0)
	{
	  for (int j = 0; j < a.nc; j++)
	    c.elem (i, j) = a.elem (i, j);
	}
      else if (data[i] == 0.0)
	{
	  for (int j = 0; j < a.nc; j++)
	    c.elem (i, j) = 0.0;
	}
      else
	{
	  for (int j = 0; j < a.nc; j++)
	    c.elem (i, j) = data[i] * a.elem (i, j);
	}
    }

  if (nr > nc)
    {
      for (int j = 0; j < a.nc; j++)
	for (int i = a.nr; i < nr; i++)
	  c.elem (i, j) = 0.0;
    }

  return c;
}

// unary operations

ComplexDiagMatrix
ComplexDiagMatrix::operator - (void) const
{
  return ComplexDiagMatrix (negate (data, len), nr, nc);
}

ComplexColumnVector
ComplexDiagMatrix::diag (void) const
{
  return diag (0);
}

// Could be optimized...

ComplexColumnVector
ComplexDiagMatrix::diag (int k) const
{
  int nnr = nr;
  int nnc = nc;
  if (k > 0)
    nnc -= k;
  else if (k < 0)
    nnr += k;

  ComplexColumnVector d;

  if (nnr > 0 && nnc > 0)
    {
      int ndiag = (nnr < nnc) ? nnr : nnc;

      d.resize (ndiag);

      if (k > 0)
	{
	  for (int i = 0; i < ndiag; i++)
	    d.elem (i) = elem (i, i+k);
	}
      else if ( k < 0)
	{
	  for (int i = 0; i < ndiag; i++)
	    d.elem (i) = elem (i-k, i);
	}
      else
	{
	  for (int i = 0; i < ndiag; i++)
	    d.elem (i) = elem (i, i);
	}
    }
  else
    cerr << "diag: requested diagonal out of range\n";

  return d;
}

// i/o

ostream&
operator << (ostream& os, const ComplexDiagMatrix& a)
{
  Complex ZERO (0.0);
//  int field_width = os.precision () + 7;
  for (int i = 0; i < a.nr; i++)
    {
      for (int j = 0; j < a.nc; j++)
	{
	  if (i == j)
	    os << " " /* setw (field_width) */ << a.data[i];
	  else
	    os << " " /* setw (field_width) */ << ZERO;
	}
      os << "\n";
    }
  return os;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
