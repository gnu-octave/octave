// tree-const.cc                                         -*- C++ -*-
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <iostream.h>

#include "tree-const.h"
#include "error.h"
#include "gripes.h"
#include "user-prefs.h"
#include "oct-map.h"

// The following three variables could be made static members of the
// tree_constant class.

// Pointer to the blocks of memory we manage.
static tree_constant *newlist;

// Multiplier for allocating new blocks.
static const int newlist_grow_size = 128;

// Pointer to the last element of the last block allocated.
static tree_constant *newlist_tail = 0;

Octave_map
tree_constant::map_value (void) const
{
  return rep->map_value ();
}

tree_constant::~tree_constant (void)
{
#if defined (MDEBUG)
  cerr << "~tree_constant: rep: " << rep
       << " rep->count: " << rep->count << "\n";
#endif

  if (--rep->count <= 0)
    {
      delete rep;
      rep = 0;
    }
}

void *
tree_constant::operator new (size_t size)
{
  assert (size == sizeof (tree_constant));

  if (! newlist)
    {
      int block_size = newlist_grow_size * sizeof (tree_constant);
      newlist = (tree_constant *) new char [block_size];

      for (int i = 0; i < newlist_grow_size - 1; i++)
	newlist[i].freeptr = &newlist[i+1];

      newlist[i].freeptr = 0;

      if (newlist_tail)
	newlist_tail->freeptr = newlist;

      newlist_tail = &newlist[i];
    }

  tree_constant *tmp = newlist;
  newlist = newlist->freeptr;
  return tmp;
}

void
tree_constant::operator delete (void *p, size_t size)
{
  tree_constant *tmp = (tree_constant *) p;
  tmp->freeptr = newlist;
  newlist = tmp;
}

// Simple assignment.

tree_constant
tree_constant::operator = (const tree_constant& a)
{
  if (rep != a.rep)
    {
      if (--rep->count <= 0)
	delete rep;
      rep = a.rep;
      rep->count++;
    }
  return *this;  
}

tree_constant
tree_constant::lookup_map_element (SLList<char*>& list)
{
  tree_constant retval;

  tree_constant_rep *tmp_rep = rep;

  Pix p = list.first ();
  while (p)
    {
      char *elt = list (p);

      list.next (p);

      tree_constant tmp = tmp_rep->lookup_map_element (elt);

      if (error_state)
	break;

      tmp_rep = tmp.rep;

      if (! p)
	retval = tmp;
    }

  return retval;
}

// Simple structure assignment.

void
tree_constant::make_unique (void)
{
  if (rep->count > 1)
    {
      --rep->count;
      rep = new tree_constant_rep (*rep);
      rep->count = 1;
    }

  if (rep->is_map ())
    {
      for (Pix p = rep->a_map->first (); p != 0; rep->a_map->next (p))
	{
	  rep->a_map->contents (p) . make_unique ();
	}
    }
}

tree_constant::tree_constant_rep *
tree_constant::make_unique_map (void)
{
  if (! rep->is_map ())
    {
      if (--rep->count <= 0)
	delete rep;

      Octave_map m;
      rep = new tree_constant_rep (m);
      rep->count = 1;
    }

  make_unique ();

  return rep;
}

tree_constant
tree_constant::assign_map_element (SLList<char*>& list,
				   tree_constant& rhs)
{
  tree_constant_rep *tmp_rep = make_unique_map ();

  if (rhs.is_map ())
    rhs.make_unique ();

  Pix p = list.first ();
  while (p)
    {
      char *elt = list (p);

      list.next (p);

      tree_constant& tmp = tmp_rep->lookup_map_element (elt, 1);

      if (! p)
	{
	  tmp = rhs;
	  return tmp;
	}

      tmp_rep = tmp.make_unique_map ();
    }

  return tree_constant ();
}

// Indexed structure assignment.

tree_constant
tree_constant::assign_map_element (SLList<char*>& list,
				   tree_constant& rhs,
				   const Octave_object& args)
{
  tree_constant_rep *tmp_rep = make_unique_map ();

  if (rhs.is_map ())
    rhs.make_unique ();

  Pix p = list.first ();
  while (p)
    {
      char *elt = list (p);

      list.next (p);

      tree_constant& tmp = tmp_rep->lookup_map_element (elt, 1);

      if (! p)
	{
	  tmp.assign (rhs, args);
	  return tmp;
	}

      tmp_rep = tmp.make_unique_map ();
    }

  return tree_constant ();
}

void
tree_constant::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  if (rep)
    rep->print_code (os);

  if (in_parens)
    os << ")";
}

// Construct return vector of empty matrices.  Return empty matrices
// and/or gripe when appropriate.

Octave_object
vector_of_empties (int nargout, const char *fcn_name)
{
  Octave_object retval;

// Got an empty argument, check if should gripe/return empty values.

  int flag = user_pref.propagate_empty_matrices;
  if (flag != 0)
    {
      if (flag < 0)
	gripe_empty_arg (fcn_name, 0);

      Matrix m;
      retval.resize (nargout ? nargout : 1);
      for (int i = 0; i < nargout; i++)
	retval(i) = m;
    }
  else
    gripe_empty_arg (fcn_name, 1);

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
