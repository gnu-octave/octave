/*

Copyright (C) 1998 John W. Eaton

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

#if !defined (octave_base_matrix_h)
#define octave_base_matrix_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cstdlib>

#include <string>

class ostream;

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// Real matrix values.

template <class MT> class
octave_base_matrix : public octave_base_value
{
public:

  octave_base_matrix (void)
    : octave_base_value () { }

  octave_base_matrix (const MT& m)
    : octave_base_value (), matrix (m) { }

  octave_base_matrix (const octave_base_matrix& m)
    : octave_base_value (), matrix (m.matrix) { }

  ~octave_base_matrix (void) { }

  octave_value *clone (void) { return new octave_base_matrix (*this); }

  int rows (void) const { return matrix.rows (); }
  int columns (void) const { return matrix.columns (); }

  int length (void) const
  {
    int r = rows ();
    int c = columns ();

    return r > c ? r : c;
  }

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  virtual bool print_as_scalar (void) const;

  void print (ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (ostream& os, const string& name) const;

protected:

  MT matrix;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
