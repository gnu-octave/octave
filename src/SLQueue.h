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


#if !defined (_SLQueue_h)
#define _SLQueue_h 1

#include "SLList.h"
#include "Queue.h"

template <class T>
class
SLQueue : public Queue<T>
{
 private:
  SLList<T> p;

 public:
  SLQueue (void) : p () { }

  SLQueue (const SLQueue<T>& q) : p (q.p) { }

  ~SLQueue (void) { }

  void operator = (const SLQueue<T>& s) { p = s.p; }

  void enq (const T& item) { p.append (item); }

  T deq (void) { return p.remove_front (); }

  T& front (void) { return p.front (); }

  void del_front (void) { p.del_front (); }

  void clear (void) { p.clear (); }

  int empty (void) const { return p.empty (); }

  int full (void) const { return 0; }

  int length (void) const { return p.length (); }
               
  int OK (void) { return p.OK (); }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
