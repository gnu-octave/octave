// oct-obj.cc                                            -*- C++ -*-
/*

Copyright (C) 1994, 1995 John W. Eaton

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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "oct-obj.h"

// XXX FIXME XXX -- these can probably go in oct-obj.h now.

Octave_object::Octave_object (double d)
  : Array<tree_constant> (1, tree_constant (d)) { }

Octave_object::Octave_object (const Matrix& m)
  : Array<tree_constant> (1, tree_constant (m)) { }

Octave_object::Octave_object (const DiagMatrix& d)
  : Array<tree_constant> (1, tree_constant (d)) { }

Octave_object::Octave_object (const RowVector& v, int pcv)
  : Array<tree_constant> (1, tree_constant (v, pcv)) { }

Octave_object::Octave_object (const ColumnVector& v, int pcv)
  : Array<tree_constant> (1, tree_constant (v, pcv)) { }

Octave_object::Octave_object (const Complex& c)
  : Array<tree_constant> (1, tree_constant (c)) { }

Octave_object::Octave_object (const ComplexMatrix& m)
  : Array<tree_constant> (1, tree_constant (m)) { }

Octave_object::Octave_object (const ComplexDiagMatrix& d)
  : Array<tree_constant> (1, tree_constant (d)) { }

Octave_object::Octave_object (const ComplexRowVector& v, int pcv)
  : Array<tree_constant> (1, tree_constant (v, pcv)) { }

Octave_object::Octave_object (const ComplexColumnVector& v, int pcv)
  : Array<tree_constant> (1, tree_constant (v, pcv)) { }

Octave_object::Octave_object (const char *s)
  : Array<tree_constant> (1, tree_constant (s)) { }

Octave_object::Octave_object (const string& s)
  : Array<tree_constant> (1, tree_constant (s)) { }

Octave_object::Octave_object (double base, double limit, double inc)
  : Array<tree_constant> (1, tree_constant (base, limit, inc)) { }

Octave_object::Octave_object (const Range& r)
  : Array<tree_constant> (1, tree_constant (r)) { }

tree_constant&
Octave_object::operator () (int n)
{
  maybe_resize (n);
  return Array<tree_constant>::operator () (n);
}

tree_constant
Octave_object::operator () (int n) const
{
  return Array<tree_constant>::operator () (n);
}

tree_constant&
Octave_object::elem (int n)
{
  maybe_resize (n);
  return Array<tree_constant>::elem (n);
}

tree_constant
Octave_object::elem (int n) const
{
  return Array<tree_constant>::operator () (n);
}

void
Octave_object::maybe_resize (int n)
{
  if (n >= length ())
    resize (n + 1, Matrix ());
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
