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

class tree_expression;
class tree_identifier;

class tree_walker;

#include <string>

#include "base-list.h"
#include "pt-cmd.h"

// List of expressions that make up a declaration statement.

class
tree_decl_elt
{
public:

  typedef void (*eval_fcn) (tree_decl_elt &);

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

  // No copying!

  tree_decl_elt (const tree_decl_elt&);

  tree_decl_elt& operator = (const tree_decl_elt&);
};

class
tree_decl_init_list : public octave_base_list<tree_decl_elt *>
{
public:

  tree_decl_init_list (void) { }

  tree_decl_init_list (tree_decl_elt *t) { append (t); }

  ~tree_decl_init_list (void)
    {
      while (! empty ())
	{
	  iterator p = begin ();
	  delete *p;
	  erase (p);
	}
    }

  void eval (tree_decl_elt::eval_fcn);

  void accept (tree_walker& tw);

private:

  // No copying!

  tree_decl_init_list (const tree_decl_init_list&);

  tree_decl_init_list& operator = (const tree_decl_init_list&);
};

// Base class for declaration commands -- global, static, etc.

class
tree_decl_command : public tree_command
{
public:

  tree_decl_command (const std::string& n, int l = -1, int c = -1)
    : tree_command (l, c), cmd_name (n), initialized (false), init_list (0) { }

  tree_decl_command (const std::string& n, tree_decl_init_list *t,
		     int l = -1, int c = -1)
    : tree_command (l, c), cmd_name (n), initialized (false), init_list (t) { }

  ~tree_decl_command (void);

  tree_decl_init_list *initializer_list (void) { return init_list; }

  void accept (tree_walker& tw);

  std::string name (void) { return cmd_name; }

protected:

  // The name of this command -- global, static, etc.
  std::string cmd_name;

  // TRUE if this command has been evaluated.
  bool initialized;

  // The list of variables or initializers in this declaration command.
  tree_decl_init_list *init_list;

private:

  // No copying!

  tree_decl_command (const tree_decl_command&);

  tree_decl_command& operator = (const tree_decl_command&);
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

private:

  static void do_init (tree_decl_elt& elt);

  // No copying!

  tree_global_command (const tree_global_command&);

  tree_global_command& operator = (const tree_global_command&);
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

private:

  static void do_init (tree_decl_elt& elt);

  // No copying!

  tree_static_command (const tree_static_command&);

  tree_static_command& operator = (const tree_static_command&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
