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

#if !defined (octave_tree_decl_h)
#define octave_tree_decl_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <SLList.h>

class tree_expression;
class tree_identifier;

class tree_walker;

#include <string>

#include "pt-cmd.h"

// List of expressions that make up a declaration statement.

class
tree_decl_elt
{
public:

  typedef void (*eval_fcn) (tree_decl_elt &, bool);

  tree_decl_elt (tree_identifier *i = 0, tree_expression *e = 0)
    : id (i), expr (e) { }

  ~tree_decl_elt (void);

  void eval (void);

  tree_identifier *ident (void) { return id; }

  tree_expression *expression (void) { return expr; }

  void accept (tree_walker& tw);

private:

  // An identifier to tag with the declared property.
  tree_identifier *id;

  // An initializer expression (may be zero);
  tree_expression *expr;
};

class
tree_decl_init_list : public SLList<tree_decl_elt *>
{
public:

  tree_decl_init_list (void)
    : SLList<tree_decl_elt *> () { }

  tree_decl_init_list (tree_decl_elt *t)
    : SLList<tree_decl_elt *> () { append (t); }

  ~tree_decl_init_list (void)
    {
      while (! empty ())
	{
	  tree_decl_elt *t = remove_front ();
	  delete t;
	}
    }

  void eval (tree_decl_elt::eval_fcn, bool);

  void accept (tree_walker& tw);
};

// Base class for declaration commands -- global, static, etc.

class
tree_decl_command : public tree_command
{
public:

  tree_decl_command (const string& n, int l = -1, int c = -1)
    : tree_command (l, c), cmd_name (n), initialized (false), init_list (0) { }

  tree_decl_command (const string& n, tree_decl_init_list *t,
		     int l = -1, int c = -1)
    : tree_command (l, c), cmd_name (n), initialized (false), init_list (t) { }

  ~tree_decl_command (void);

  tree_decl_init_list *initializer_list (void) { return init_list; }

  void accept (tree_walker& tw);

  string name (void) { return cmd_name; }

protected:

  // The name of this command -- global, static, etc.
  string cmd_name;

  // TRUE if this command has been evaluated.
  bool initialized;

  // The list of variables or initializers in this declaration command.
  tree_decl_init_list *init_list;
};

// Global.

class
tree_global_command : public tree_decl_command
{
public:

  tree_global_command (int l = -1, int c = -1)
    : tree_decl_command ("global", l, c) { }

  tree_global_command (tree_decl_init_list *t, int l = -1, int c = -1)
    : tree_decl_command ("global", t, l, c) { }

  ~tree_global_command (void) { }

  void eval (void);
};

// Static.

class
tree_static_command : public tree_decl_command
{
public:

  tree_static_command (int l = -1, int c = -1)
    : tree_decl_command ("static", l, c) { }

  tree_static_command (tree_decl_init_list *t, int l = -1, int c = -1)
    : tree_decl_command ("static", t, l, c) { }

  ~tree_static_command (void) { }

  void eval (void);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
