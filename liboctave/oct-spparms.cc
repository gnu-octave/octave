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
along with this program; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#include "config.h"
#include "lo-ieee.h"

#include "oct-spparms.h"

SparseParams Voctave_sparse_controls;

void
SparseParams::defaults (void)
{
  Voctave_sparse_controls (0) = 0;    // spumoni
  Voctave_sparse_controls (1) = 1;    // ths_rel
  Voctave_sparse_controls (2) = 1;    // ths_abs
  Voctave_sparse_controls (3) = 0;    // exact_d
  Voctave_sparse_controls (4) = 3;    // supernd
  Voctave_sparse_controls (5) = 3;    // rreduce
  Voctave_sparse_controls (6) = 0.5;  // wh_frac
  Voctave_sparse_controls (7) = 1;    // autommd
  Voctave_sparse_controls (8) = 1;    // autoamd
  Voctave_sparse_controls (9) = 0.1;  // piv_tol
  Voctave_sparse_controls (10) = 0.5; // bandden
  Voctave_sparse_controls (11) = 1;   // umfpack
}

void
SparseParams::tight (void)
{
  Voctave_sparse_controls (0) = 0;    // spumoni
  Voctave_sparse_controls (1) = 1;    // ths_rel
  Voctave_sparse_controls (2) = 0;    // ths_abs
  Voctave_sparse_controls (3) = 1;    // exact_d
  Voctave_sparse_controls (4) = 1;    // supernd
  Voctave_sparse_controls (5) = 1;    // rreduce
  Voctave_sparse_controls (6) = 0.5;  // wh_frac
  Voctave_sparse_controls (7) = 1;    // autommd
  Voctave_sparse_controls (8) = 1;    // autoamd
  Voctave_sparse_controls (9) = 0.1;  // piv_tol
  Voctave_sparse_controls (10) = 0.5; // bandden
  Voctave_sparse_controls (11) = 1;   // umfpack
}
  
void
SparseParams::init_keys (void)
{
  keys (0) = "spumoni";
  keys (1) = "ths_rel";
  keys (2) = "ths_abs";
  keys (3) = "exact_d";
  keys (4) = "supernd";
  keys (5) = "rreduce";
  keys (6) = "wh_frac";
  keys (7) = "autommd";
  keys (8) = "autoamd";
  keys (9) = "piv_tol";
  keys (10) = "bandden";
  keys (11) = "umfpack";
}

SparseParams& 
SparseParams::operator = (const SparseParams& a)
{
  for (int i = 0; i < OCTAVE_SPARSE_CONTROLS_SIZE; i++)
    params (i) = a.params (i);

  return *this;
}

bool
SparseParams::set_key (const std::string key, const double& val)
{
  for (int i = 0; i < OCTAVE_SPARSE_CONTROLS_SIZE; i++)
    if (keys (i) == key)
      {
	params(i) = val;
	return true;
      }
  return false;
}

double
SparseParams::get_key (const std::string key)
{
  for (int i = 0; i < OCTAVE_SPARSE_CONTROLS_SIZE; i++)
    if (keys (i) == key)
	return params(i);

  return octave_NaN;
}

void
SparseParams::print_info (std::ostream& os, const std::string& prefix) const
{
  for (int i = 0; i < OCTAVE_SPARSE_CONTROLS_SIZE; i++)
    os << prefix << keys(i) << ": " << params(i) << "\n";
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
