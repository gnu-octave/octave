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

#if !defined (octave_tree_mvr_h)
#define octave_tree_mvr_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class tree_constant;
class Octave_object;

#include "pt-exp-base.h"

// A base class for objects that can be return multiple values

class
tree_multi_val_ret : public tree_expression
{
public:
  tree_multi_val_ret (int l = -1, int c = -1) : tree_expression (l, c) { }

  tree_multi_val_ret (int l = -1, int c = -1, tree_expression::type et)
    : tree_expression (l, c, et) { }

  ~tree_multi_val_ret (void) { }

  bool is_multi_val_ret_expression (void) const
    { return true; }

  tree_constant eval (bool print);

  virtual Octave_object eval (bool print, int nargout,
			      const Octave_object& args) = 0;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
