// Template array classes                              -*- C++ -*-
/*

Copyright (C) 1993 John W. Eaton

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

// Written by John C. Campbell <jcc@che.utexas.edu>.

#if !defined (_Array_h)
#define _Array_h 1

#include <iostream.h>
#include <assert.h>

template <class T> class Array;  

template <class T>
class ArrayRep
{
  friend class Array<T>;

public:

  ArrayRep  (void);
  ArrayRep  (int);
  ArrayRep  (const ArrayRep<T>& a);

  ~ArrayRep (void);
  
  int length (void) const;
  
  T& elem (int n);
  T& checkelem (int n);
  T& operator () (int n);
  
  T elem (int n) const;
  T checkelem (int n) const;
  T operator () (int n) const;
  
private:
  
  T *data;
  int len;
  int count;
};

template <class T>
class Array
{
public:
  
  Array (void);
  Array (int);
  Array (int n, T val);
  Array (const Array<T>& a);

  ~Array (void);

  Array<T>& operator = (const Array<T>& a);
  
  int length (void) const;

  T& elem (int n);
  T& checkelem (int n);
  T& operator () (int n);

  T elem (int n) const;
  T checkelem (int n) const;
  T operator () (int n) const;

protected:

  ArrayRep<T> *rep;
};

template <class T>
class Array2 : public Array<T>
{
public:

  Array2 (void);
  Array2 (int n, int m);
  Array2 (int n, int m, T val);
  Array2 (const Array2<T>& a);

  Array2<T>& operator = (const Array2<T>& a);

  int dim1 (void) const;
  int dim2 (void) const;
 
  T& elem (int i, int j);
  T& checkelem (int i, int j); 
  T& operator () (int i, int j);
  
  T elem (int i, int j) const;
  T checkelem (int i, int j) const;
  T operator () (int i, int j) const;

protected:
  
  int d1;
  int d2;
};

template <class T>
class Array3 : public Array2<T>
{
public:

  Array3 (void);
  Array3 (int n, int m, int k);
  Array3 (int n, int m, int k, T val);
  Array3 (const Array3<T>& a);

  Array3<T>& operator = (const Array3<T>& a);

  int dim3 (void) const;

  T& elem (int i, int j, int k);
  T& checkelem (int i, int j, int k); 
  T& operator()(int i,int j,int k);
  
  T elem (int i, int j, int k) const;
  T checkelem(int i,int j,int k)const;
  T operator()(int i,int j,int k) const;

protected:
  
  int d3;
};

template <class T>
class DiagArray : public Array<T>
{
public:
  
  DiagArray (void);
  DiagArray (int n): Array<T> (n) {}
  DiagArray (int r, int c);
  DiagArray (int r, int c, T val);
  DiagArray (const Array<T>& a);
  DiagArray (const DiagArray<T>& a);

  DiagArray<T>& operator = (const DiagArray<T>& a);

  int rows (void) const;
  int cols (void) const;
  int columns (void) const;

  T& elem (int r, int c);
  T& checkelem (int r, int c);
  T& operator () (int r, int c);

  T elem (int r, int c) const;
  T checkelem (int r, int c) const;
  T operator () (int r, int c) const;

protected:

  int nr;
  int nc;
};

#ifdef __GNUG__
#include "Array.cc"
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
