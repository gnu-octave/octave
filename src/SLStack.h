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


#ifndef _SLStack_h
#ifdef __GNUG__
//#pragma interface
#endif
#define _SLStack_h 1

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

template <class T>
inline SLStack<T>::SLStack (void) : p () { }

template <class T>
inline SLStack<T>::SLStack (const SLStack<T>& a) : p (a.p) { }

template <class T>
inline SLStack<T>::~SLStack (void) { }

template <class T>
inline void
SLStack<T>::push (const T& item)
{
  p.prepend (item);
}

template <class T>
inline T
SLStack<T>::pop (void)
{
  return p.remove_front ();
}

template <class T>
inline T&
SLStack<T>::top (void)
{
  return p.front ();
}

template <class T>
inline void
SLStack<T>::del_top (void)
{
  p.del_front ();
}

template <class T>
inline void
SLStack<T>::operator = (const SLStack<T>& s)
{
  p = s.p;
}

template <class T>
inline int
SLStack<T>::empty (void)
{
  return p.empty ();
}

template <class T>
inline int
SLStack<T>::full (void)
{
  return 0;
}

template <class T>
inline int
SLStack<T>::length (void)
{
  return p.length ();
}

template <class T>
inline int
SLStack<T>::OK (void)
{
  return p.OK ();
}

template <class T>
inline void
SLStack<T>::clear (void)
{
  p.clear ();
}

#endif
