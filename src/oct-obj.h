// oct-obj.h                                            -*- C -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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

#if !defined (octave_oct_obj_h)
#define octave_oct_obj_h 1

#include "Array.h"

class tree_constant;

class Octave_object : public Array<tree_constant>
{
public:

  Octave_object (void) : Array<tree_constant> () { }
  Octave_object (int n) : Array<tree_constant> (n) { }
  Octave_object (int n, const tree_constant& val)
    : Array<tree_constant> (n, val) { }

  Octave_object (const Octave_object& obj) : Array<tree_constant> (obj) { }

  Octave_object& operator = (const Octave_object& obj)
    {
      Array<tree_constant>::operator = (obj);
      return *this;
    }

#if 0
// For now, translate the index, since it will be somewhat difficult
// to fix this without some major (and relatively risky) surgery.

  tree_constant& elem (int n)
    { return Array<tree_constant>::elem (n - 1); }

  tree_constant& checkelem (int n);
    { return Array<tree_constant>::checkelem (n - 1); }

  tree_constant& operator () (int n);
    { return Array<tree_constant>::operator () (n - 1); }

// No checking.
  tree_constant& xelem (int n);
    { return Array<tree_constant>::xelem (n - 1); }

  tree_constant elem (int n) const;
    { return Array<tree_constant>::elem (n - 1); }

  tree_constant checkelem (int n) const;
    { return Array<tree_constant>::checkelem (n - 1); }

  tree_constant operator () (int n) const;
    { return Array<tree_constant>::operator () (n - 1); }
#endif
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
