/*

Copyright (C) 1996, 1997 John W. Eaton

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

#if !defined (octave_tree_indirect_ref_h)
#define octave_tree_indirect_ref_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

#include <string>

class octave_value;
class octave_value_list;
class tree_identifier;
class tree_walker;

#include "pt-mvr-base.h"

// Indirect references to values (structure references).

class
tree_indirect_ref : public tree_multi_val_ret
{
public:

  tree_indirect_ref (int l = -1, int c = -1)
    : tree_multi_val_ret (l, c), id (0), indir (0), nm (),
      preserve_ident (false), preserve_indir (false),
      maybe_do_ans_assign (false) { }

  tree_indirect_ref (tree_identifier *i, int l = -1, int c = -1)
    : tree_multi_val_ret (l, c), id (i), indir (0), nm (),
      preserve_ident (false), preserve_indir (false),
      maybe_do_ans_assign (false) { }

  tree_indirect_ref (tree_indirect_ref *i, const string& n,
		     int l = -1, int c = -1)
    : tree_multi_val_ret (l, c), id (0), indir (i), nm (n),
      preserve_ident (false), preserve_indir (false),
      maybe_do_ans_assign (false) { }

  ~tree_indirect_ref (void);

  bool is_indirect_ref (void) const
    { return true; }

  bool is_identifier_only (void) const
    { return (id && nm.empty ()); }

  tree_identifier *ident (void)
    { return id; }

  tree_indirect_ref *indirect (void)
    { return indir; }

  void preserve_identifier (void)
    { preserve_ident = true; }

  void preserve_indirect (void)
    { preserve_indir = true; }

  void mark_for_possible_ans_assign (void);

  string name (void) const;

  octave_value eval (bool print = false);

  octave_value_list
  eval (bool print, int nargout, const octave_value_list& args);

  octave_value value (void) const;
  octave_value& reference (void);

  string elt_name (void)
    { return nm; }

  void accept (tree_walker& tw);

private:

  // The identifier for this structure reference.  For example, in
  // a.b.c, a is the id.
  tree_identifier *id;

  // This element just points to another indirect reference.
  tree_indirect_ref *indir;

  // The sub-element name.
  string nm;

  // True if we should not delete the identifier.
  bool preserve_ident;

  // True if we should not delete the indirect reference.
  bool preserve_indir;

  // True if we should consider assigning the result of evaluating
  // this identifier to the built-in variable ans.
  bool maybe_do_ans_assign;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
