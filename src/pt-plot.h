// tree-plot.h                                         -*- C++ -*-
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

#if !defined (octave_tree_plot_h)
#define octave_tree_plot_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class tree_plot_limits;
class tree_plot_range;
class tree_subplot;
class tree_subplot_using;
class tree_subplot_style;
class tree_subplot_list;
class tree_plot_command;

class ostream;

#include "tree-base.h"

class
tree_plot_command : public tree_command
{
 public:
  tree_plot_command (void);
  tree_plot_command (tree_subplot_list *plt, int nd);
  tree_plot_command (tree_subplot_list *plt, tree_plot_limits *rng, int nd);

  ~tree_plot_command (void);

  tree_constant eval (int print);

 private:
  int ndim;
  tree_plot_limits *range;
  tree_subplot_list *plot_list;
};

class
tree_subplot_list : public tree
{
 public:
  tree_subplot_list (void);
  tree_subplot_list (tree_expression *data);
  tree_subplot_list (tree_subplot_list *t);
  tree_subplot_list (tree_subplot_using *u, tree_expression *t,
		     tree_subplot_style *s);

  ~tree_subplot_list (void);

  tree_subplot_list *set_data (tree_expression *data);

  tree_subplot_list *chain (tree_subplot_list *t);

  tree_subplot_list *reverse (void);

  tree_subplot_list *next_elem (void);

  tree_constant eval (int print);
//  tree_constant *eval (int print, int nargout);

  int print (int ndim, ostrstream& plot_buf);

 private:
  tree_expression *plot_data;
  tree_subplot_using *using;
  tree_expression *title;
  tree_subplot_style *style;
  tree_subplot_list *next;
};

class
tree_plot_limits : public tree
{
 public:
  tree_plot_limits (void);
  tree_plot_limits (tree_plot_range *xlim);
  tree_plot_limits (tree_plot_range *xlim, tree_plot_range *ylim);
  tree_plot_limits (tree_plot_range *xlim, tree_plot_range *ylim,
		    tree_plot_range *zlim);

  ~tree_plot_limits (void);

  tree_constant eval (int print);

  void print (int print, ostrstream& plot_buf);

 private:
  tree_plot_range *x_range;
  tree_plot_range *y_range;
  tree_plot_range *z_range;
};

class
tree_plot_range : public tree
{
 public:
  tree_plot_range (void);
  tree_plot_range (tree_expression *l, tree_expression *u);

  ~tree_plot_range (void);

  tree_constant eval (int print);

  void print (ostrstream& plot_buf);

 private:
  tree_expression *lower;
  tree_expression *upper;
};

class
tree_subplot_using : public tree
{
 public:
  tree_subplot_using (void);
  tree_subplot_using (tree_expression *fmt);

  ~tree_subplot_using (void);

  tree_subplot_using *set_format (tree_expression *fmt);

  tree_subplot_using *add_qualifier (tree_expression *t);

  tree_constant eval (int print);

  int print (int ndim, int n_max, ostrstream& plot_buf);

 private:
  int qualifier_count;
  tree_expression *x[4];
  tree_expression *scanf_fmt;
};

class
tree_subplot_style : public tree
{
 public:
  tree_subplot_style (void);
  tree_subplot_style (char *s);
  tree_subplot_style (char *s, tree_expression *lt);
  tree_subplot_style (char *s, tree_expression *lt, tree_expression *pt);

  ~tree_subplot_style (void);

  tree_constant eval (int print);

  int print (ostrstream& plot_buf);

 private:
  char *style;
  tree_expression *linetype;
  tree_expression *pointtype;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
