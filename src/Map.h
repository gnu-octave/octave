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

#include <string>

#include <Pix.h>

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

  virtual int contains (const string& key) const;  // is key mapped?

  virtual void clear (void);			// delete all items
	      
  virtual C& operator [] (const string& key) = 0;  // access contents by key
	      
  virtual void del (const string& key) = 0;	// delete entry
	      
  virtual Pix first (void) const = 0;		// Pix of first item or 0
  virtual void next (Pix& i) const = 0;		// advance to next or 0
  virtual string key (Pix i) const = 0;		// access key at i
  virtual C& contents (Pix i) const = 0;	// access contents at i

  virtual int owns (Pix i) const;		// is i a valid Pix  ?
  virtual Pix seek (const string& key) const;	// Pix of key

  C& dflt (void) { return def; }		// access default val

  void error (const string& msg) const;

  virtual int OK (void) const = 0;		// rep invariant
};

template <class C>
struct CHNode
{
  CHNode *tl;
  string hd;
  C cont;

  CHNode (void) : tl (0), hd (), cont () { }

  CHNode (const string& h, const C& c, CHNode *t = 0)
    : tl (t), hd (h), cont (c) { }

  ~CHNode (void) { }
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

  C& operator [] (const string& key);

  void del (const string& key);

  Pix first (void) const;
  void next (Pix& i) const;

  string key (Pix p) const
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

  Pix seek (const string& key) const;

  int contains (const string& key) const
    {
      return seek (key) != 0;
    }

  void clear (void);
  int  OK (void) const;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
