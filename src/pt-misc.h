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

#if !defined (octave_tree_misc_h)
#define octave_tree_misc_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include <SLList.h>

class octave_value;
class octave_value_list;

class tree_identifier;
class tree_index_expression;
class tree_va_return_list;

class tree_walker;

// Parameter lists.  Used to hold the list of input and output
// parameters in a function definition.  Elements are identifiers
// only.

class
tree_parameter_list
{
public:

  tree_parameter_list (void)
    : lst (), marked_for_varargs (0) { }

  tree_parameter_list (tree_identifier *t)
    : lst (), marked_for_varargs (0) { lst.append (t); }

  ~tree_parameter_list (void);

  int length (void) const { return lst.length (); }

  void append (tree_identifier *&s) { lst.append (s); }
  void append (tree_identifier * const &s) { lst.append (s); }

  tree_identifier *&operator () (Pix p) { return lst (p); }

  tree_identifier * const &operator () (Pix p) const { return lst (p); }

  Pix first (void) const { return lst.first (); }

  void next (Pix& p) const { return lst.next (p); }

  void mark_as_formal_parameters (void);

  void mark_varargs (void) { marked_for_varargs = 1; }

  bool takes_varargs (void) const { return marked_for_varargs != 0; }

  void mark_varargs_only (void) { marked_for_varargs = -1; }

  bool varargs_only (void) { return (marked_for_varargs < 0); }

  void initialize_undefined_elements (octave_value& val);

  void define_from_arg_vector (const octave_value_list& args);

  void clear (void);

  bool is_defined (void);

  octave_value_list convert_to_const_vector (tree_va_return_list *vr_list);

  void accept (tree_walker& tw);

private:

  // The list of identifiers in the parameter list.
  SLList<tree_identifier *> lst;

  int marked_for_varargs;

  // No copying!

  tree_parameter_list (const tree_parameter_list&);

  tree_parameter_list& operator = (const tree_parameter_list&);
};

// Return lists.  Used to hold the right hand sides of multiple
// assignment expressions.

class
tree_return_list
{
public:

  tree_return_list (void)
    : lst () { }

  tree_return_list (tree_index_expression *t)
    : lst () { lst.append (t); }

  ~tree_return_list (void);

  void append (tree_index_expression *&s) { lst.append (s); }
  void append (tree_index_expression * const &s) { lst.append (s); }

  tree_index_expression *&operator () (Pix p) { return lst (p); }

  tree_index_expression * const &operator () (Pix p) const { return lst (p); }

  Pix first (void) const { return lst.first (); }

  void next (Pix& p) const { return lst.next (p); }

  void accept (tree_walker& tw);

private:

  // The list of expressions in the return list.
  SLList<tree_index_expression *> lst;

  // No copying!

  tree_return_list (const tree_return_list&);

  tree_return_list& operator = (const tree_return_list&);
};

class
tree_va_return_list
{
public:

  tree_va_return_list (void) : lst () { }

  ~tree_va_return_list (void) { }

  int length (void) const { return lst.length (); }

  void clear (void) { lst.clear (); }

  int empty (void) const { return lst.empty (); }

  void append (octave_value& s) { lst.append (s); }
  void append (const octave_value& s) { lst.append (s); }

  octave_value& operator () (Pix p) { return lst (p); }

  const octave_value& operator () (Pix p) const { return lst (p); }

  Pix first (void) const { return lst.first (); }

  void next (Pix& p) const { return lst.next (p); }

private:

  // The list of values in the va return list.
  SLList<octave_value> lst;

  // No copying!

  tree_va_return_list (const tree_va_return_list&);

  tree_va_return_list& operator = (const tree_va_return_list&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
