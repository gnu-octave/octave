// Stack.h                                                -*- C++ -*-
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

/*

The classes in this file are derived from the old `genclass' version
of Stack from libg++, originally:

  Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

and distributed under the terms of the GNU Library General Public
License as published by the Free Software Foundation.

*/

#if !defined (_Stack_h)
#define _Stack_h 1

template <class T>
class
Stack
{
 public:
  inline Stack (void) { }
  inline virtual ~Stack (void) { }

  virtual void push (const T& item) = 0;

  virtual T pop (void) = 0;
  virtual T& top (void) = 0; 

  virtual void del_top (void) = 0;

  virtual int empty (void) = 0;
  virtual int full (void) = 0;
  virtual int length (void) = 0;

  virtual void clear (void) = 0;

  void error (const char *msg);
  virtual int OK (void) = 0;
};

#endif
