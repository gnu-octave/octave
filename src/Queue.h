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


#ifndef _Queue_h
#define _Queue_h 1

#if defined (__GNUG__) && defined (USE_EXTERNAL_TEMPLATES)
//#pragma interface
#endif

template <class T>
class
Queue
{
public:
  Queue (void) { }

  virtual ~Queue (void) { }

  virtual void enq (const T& item) = 0;

  virtual T deq (void) = 0;

  virtual T& front (void) = 0;

  virtual void del_front (void) = 0;

  virtual void clear (void) = 0;

  virtual int empty (void) const = 0;

  virtual int full (void) const = 0;

  virtual int length (void) const = 0;

  void error (const char*);
  
  virtual int OK (void) = 0;
};

#endif
