/*

Copyright (C) 2004 David Bateman
Copyright (C) 1998-2004 Andy Adler

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

*/

#if !defined (octave_oct_spparms_h)
#define octave_oct_spparms_h 1

#include <cassert>
#include <cstddef>

#include <iostream>

#include "str-vec.h"
#include "dColVector.h"

#define OCTAVE_SPARSE_CONTROLS_SIZE 12

class
SparseParams
{
 public:
  SparseParams (void) : params (ColumnVector (OCTAVE_SPARSE_CONTROLS_SIZE)), 
    keys (string_vector (OCTAVE_SPARSE_CONTROLS_SIZE)) 
    { defaults (); init_keys (); }
  
  void defaults (void);

  void tight (void);
  
  SparseParams& operator = (const SparseParams& a);
  
  double& operator () (int n) { return params (n); }
  double operator () (int n) const { return params (n); }

  string_vector get_keys (void) const { return keys; }

  ColumnVector get_vals (void) const { return params; }

  bool set_key (const std::string key, const double& val);

  double get_key (const std::string key);

  void print_info (std::ostream& os, const std::string& prefix) const;
  
 private:
  void init_keys (void);

  ColumnVector params;

  string_vector keys;
};

extern SparseParams Voctave_sparse_controls;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
