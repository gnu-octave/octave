// SLStack.cc                                           -*- C++ -*-
/*

Copyright (C) 1993, 1994, 1995 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "SLStack.h"

template <class T>
SLStack<T>::SLStack (void) : p ()
{
}

template <class T>
SLStack<T>::SLStack (const SLStack<T>& a) : p (a.p)
{
}

template <class T>
SLStack<T>::~SLStack (void)
{
}

template <class T>
void
SLStack<T>::push (const T& item)
{
  p.prepend (item);
}

template <class T>
T
SLStack<T>::pop (void)
{
  return p.remove_front ();
}

template <class T>
T&
SLStack<T>::top (void)
{
  return p.front ();
}

template <class T>
void
SLStack<T>::del_top (void)
{
  p.del_front ();
}

template <class T>
SLStack<T>&
SLStack<T>::operator = (const SLStack<T>& s)
{
  p = s.p;
  return *this;
}

template <class T>
int
SLStack<T>::empty (void)
{
  return p.empty ();
}

template <class T>
int
SLStack<T>::full (void)
{
  return 0;
}

template <class T>
int
SLStack<T>::length (void)
{
  return p.length ();
}

template <class T>
int
SLStack<T>::OK (void)
{
  return p.OK ();
}

template <class T>
void
SLStack<T>::clear (void)
{
  p.clear ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
