// This may look like C code, but it is really -*- C++ -*-
/*
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#if !defined (_SLStack_h)
#define _SLStack_h 1

#if defined (__GNUG__) && defined (USE_EXTERNAL_TEMPLATES)
#pragma interface
#endif

#include "SLList.h"
#include "Stack.h"

template <class T>
class SLStack : public Stack<T>
{
 private:
  SLList<T> p;

 public:
  SLStack (void);
  SLStack (const SLStack<T>& s);
  ~SLStack (void);

  void operator = (const SLStack<T>&);

  void push (const T& item);
  T pop (void);
  T& top (void);
  void del_top (void);

  int empty (void);
  int full (void);
  int length (void);

  void clear (void);

  int OK (void);
};

#if defined (__GNUG__) && ! defined (USE_EXTERNAL_TEMPLATES)
#include "SLStack.cc"
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
