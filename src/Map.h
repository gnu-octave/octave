// octave-map.h                                    -*- C++ -*-
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

The classes in this file are derived from the old `genclass' versions
of Map and CHMap from libg++, originally:

  Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

and distributed under the terms of the GNU Library General Public
License as published by the Free Software Foundation.

*/

#if ! defined (octave_Map_h)
#define octave_Map_h 1

#if defined (__GNUG__) && defined (USE_EXTERNAL_TEMPLATES)
#pragma interface
#endif

#include <Pix.h>

#include "utils.h"

template <class C>
class Map
{
protected:
  int count;
  C def;

public:
  Map (const C& dflt) : def (dflt) { count = 0; }

  virtual ~Map (void) { }

  int length (void) const { return count; }	// current number of items
  int empty (void) const { return count == 0; }

  virtual int contains (const char *key) const;	// is key mapped?

  virtual void clear (void);			// delete all items
	      
  virtual C& operator [] (const char *key) = 0;	// access contents by key
	      
  virtual void del (const char *key) = 0;	// delete entry
	      
  virtual Pix first (void) const = 0;		// Pix of first item or 0
  virtual void next (Pix& i) const = 0;		// advance to next or 0
  virtual const char *key (Pix i) const = 0;	// access key at i
  virtual C& contents (Pix i) const = 0;	// access contents at i

  virtual int owns (Pix i) const;		// is i a valid Pix  ?
  virtual Pix seek (const char *key) const;	// Pix of key

  C& dflt (void) { return def; }		// access default val

  void  error (const char* msg) const;

  virtual int OK (void) const = 0;		// rep invariant
};

template <class C>
struct CHNode
{
  CHNode *tl;
  char *hd;
  C cont;

  CHNode (void) : tl (0), hd (0) { }

  CHNode (const char *h, const C& c, CHNode *t = 0)
    : tl (t), cont (c)
      { hd = strsave (h); }

  ~CHNode (void)
    { delete [] hd; }
};

#ifndef DEFAULT_INITIAL_CAPACITY
#define DEFAULT_INITIAL_CAPACITY 8
#endif

template <class C>
class CHMap : public Map<C>
{
protected:
  CHNode<C> **tab;
  unsigned int size;

public:
  CHMap (const C& dflt, unsigned int sz = DEFAULT_INITIAL_CAPACITY);

  CHMap (const CHMap& a);

  ~CHMap (void)
    {
      clear ();
      delete tab;
    }

  C& operator [] (const char *key);

  void del (const char *key);

  Pix first (void) const;
  void next (Pix& i) const;

  const char *key (Pix p) const
    {
      if (p == 0)
	error ("null Pix");

      return ((CHNode<C> *) p)->hd;
    }

  C& contents (Pix p) const
    {
      if (p == 0)
	error ("null Pix");

     return ((CHNode<C> *) p)->cont;
   }

  Pix seek (const char *key) const;

  int contains (const char *key) const
    {
      return seek (key) != 0;
    }

  void clear (void);
  int  OK (void) const;
};

#if defined (__GNUG__) && ! defined (USE_EXTERNAL_TEMPLATES)
#include "Map.cc"
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
