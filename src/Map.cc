// Map.cc                                               -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

The classes in this file are derived from the old `genclass' versions
of Map and CHMap from libg++, originally:

  Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

and distributed under the terms of the GNU Library General Public
License as published by the Free Software Foundation.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <iostream.h>

#include "Map.h"

static unsigned int
hash (const char *str)
{
  unsigned h = 0;
  while (*str)
    h = h * 33 + *str++;
  return h;
}

template <class C>
Pix
Map<C>::seek (const char *item) const
{
  for (Pix i = first (); i != 0 && strcmp (key (i), item) != 0; next (i))
    ; // Skip items until match found.

  return i;
}

template <class C>
int
Map<C>::owns (Pix idx) const
{
  if (idx == 0)
    return 0;

  for (Pix i = first (); i != 0; next (i))
    if (i == idx)
      return 1;

  return 0;
}

template <class C>
void
Map<C>::clear (void)
{
  Pix i = first ();
  while (i != 0)
    {
      del (key (i));
      i = first ();
    }
}

template <class C>
int
Map<C>::contains (const char *item) const
{
  return seek (item) != 0;
}

template <class C>
void
Map<C>::error (const char* msg) const
{
  cerr << "Map: " << msg << "\n";
}

// CHMap class.

// The nodes are linked together serially via a version of a trick
// used in some vtables: odd pointers are actually links to the next
// table entry.  Not terrible, but not wonderful either.

template <class C>
static int
goodCHptr (CHNode<C> *t)
{
  return ((((unsigned) t) & 1) == 0);
}

// This sucks, but avoids g++ 2.6.0 `type unification failed' errors.

static void *
index_to_CHptr (int i)
{
  return (void *) ((i << 1) + 1);
}

template <class C>
static int
CHptr_to_index (CHNode<C> *t)
{
  return ((unsigned) t) >> 1;
}

template <class C>
CHMap<C>::CHMap (const C& dflt, unsigned int sz) : Map<C> (dflt)
{
  tab = new CHNode<C>* [size = sz];
  for (unsigned int i = 0; i < size; ++i)
    tab[i] = (CHNode<C> *) index_to_CHptr (i+1);
  count = 0;
}

template <class C>
CHMap<C>::CHMap (const CHMap& a) : Map<C> (a.def)
{
  tab = new CHNode<C>* [size = a.size];
  for (unsigned int i = 0; i < size; ++i)
    tab[i] = (CHNode<C> *) index_to_CHptr (i+1);
  count = 0;
  for (Pix p = a.first (); p; a.next (p))
    (*this) [a.key (p)] = a.contents (p);
}

template <class C>
Pix
CHMap<C>::seek (const char *key) const
{
  unsigned int h = hash (key) % size;

  for (CHNode<C> *t = tab[h]; goodCHptr (t); t = t->tl)
    if (strcmp (key, t->hd) == 0)
      return Pix (t);

  return 0;
}

template <class C>
C&
CHMap<C>::operator [] (const char *item)
{
  unsigned int h = hash (item) % size;

  for (CHNode<C> *t = tab[h]; goodCHptr (t); t = t->tl)
    if (strcmp (item, t->hd) == 0)
      return t->cont;

  t = new CHNode<C> (item, def, tab[h]);
  tab[h] = t;
  ++count;
  return t->cont;
}

template <class C>
void
CHMap<C>::del (const char *key)
{
  unsigned int h = hash (key) % size;

  CHNode<C> *t = tab[h];
  CHNode<C> *trail = t;
  while (goodCHptr (t))
    {
      if (strcmp (key, t->hd) == 0)
	{
	  if (trail == t)
	    tab[h] = t->tl;
	  else
	    trail->tl = t->tl;
	  delete t;
	  --count;
	  return;
	}
      trail = t;
      t = t->tl;
    }
}

template <class C>
void
CHMap<C>::clear (void)
{
  for (unsigned int i = 0; i < size; ++i)
    {
      CHNode<C> *p = tab[i];
      tab[i] = (CHNode<C> *) index_to_CHptr (i+1);
      while (goodCHptr (p))
	{
	  CHNode<C> *nxt = p->tl;
	  delete p;
	  p = nxt;
	}
    }
  count = 0;
}

template <class C>
Pix
CHMap<C>::first (void) const
{
  for (unsigned int i = 0; i < size; ++i)
    if (goodCHptr (tab[i]))
      return Pix (tab[i]);
  return 0;
}

template <class C>
void
CHMap<C>::next (Pix& p) const
{
  CHNode<C> *t = ((CHNode<C> *) p)->tl;
  if (goodCHptr (t))
    p = Pix (t);
  else
    {
      for (unsigned int i = CHptr_to_index (t); i < size; ++i)
	{
	  if (goodCHptr (tab[i]))
	    {
	      p =  Pix (tab[i]);
	      return;
	    }
	}
      p = 0;
    }
}

template <class C>
int
CHMap<C>::OK (void) const
{
  int v = tab != 0;
  int n = 0;

  for (unsigned int i = 0; i < size; ++i)
    {
      for (CHNode<C> *p = tab[i]; goodCHptr (p); p = p->tl)
	++n;

      v &= CHptr_to_index (p) == i + 1;
    }

  v &= count == n;

  if (! v)
    error ("invariant failure");

  return v;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
