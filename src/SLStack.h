/*

Copyright (C) 1996, 1997 John W. Eaton

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

/*

The classes in this file are derived from the old `genclass' version
of SLStack from libg++, originally:

  Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

and distributed under the terms of the GNU Library General Public
License as published by the Free Software Foundation.

*/

#if !defined (_SLStack_h)
#define _SLStack_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include "SLList.h"
#include "Stack.h"

template <class T>
class
SLStack : public Stack<T>
{
private:

  SLList<T> p;

public:

  SLStack (void) : p () { }

  SLStack (const SLStack<T>& s) : p (s.p) { }

  ~SLStack (void) { }

  SLStack<T>& operator = (const SLStack<T>& s);

  void push (const T& item) { p.prepend (item); }

  T pop (void) { return p.remove_front (); }

  T& top (void) { return p.front (); }

  void del_top (void) { p.del_front (); }

  int empty (void) { return p.empty (); }

  int full (void) { return 0; }

  int length (void) { return p.length (); }

  void clear (void) { p.clear (); }

  int OK (void) { return p.OK (); }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
