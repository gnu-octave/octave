// tree-plot.h                                         -*- C++ -*-
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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_tree_plot_h)
#define octave_tree_plot_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <iostream.h>

class tree_command;
class tree_plot_command;
class plot_limits;
class plot_range;
class subplot_using;
class subplot_style;
class subplot;
class subplot_list;

#include <SLList.h>

#include "dColVector.h"

#include "idx-vector.h"
#include "tree-cmd.h"
#include "tree-expr.h"

class
tree_plot_command : public tree_command
{
public:
  tree_plot_command (void);
  tree_plot_command (subplot_list *plt, int nd);
  tree_plot_command (subplot_list *plt, plot_limits *rng, int nd);

  ~tree_plot_command (void);

  void eval (void);

  void print_code (ostream& os);

private:
  int ndim;
  plot_limits *range;
  subplot_list *plot_list;
};

class
plot_limits : public tree_print_code
{
public:
  plot_limits (void);
  plot_limits (plot_range *xlim);
  plot_limits (plot_range *xlim, plot_range *ylim);
  plot_limits (plot_range *xlim, plot_range *ylim,
		    plot_range *zlim);

  ~plot_limits (void);

  void print (int print, ostrstream& plot_buf);

  void print_code (ostream& os);

private:
  plot_range *x_range;
  plot_range *y_range;
  plot_range *z_range;
};

class
plot_range : public tree_print_code
{
public:
  plot_range (void);
  plot_range (tree_expression *l, tree_expression *u);

  ~plot_range (void);

  void print (ostrstream& plot_buf);

  void print_code (ostream& os);

private:
  tree_expression *lower;
  tree_expression *upper;
};

class
subplot_using : public tree_print_code
{
public:
  subplot_using (void);
  subplot_using (tree_expression *fmt);

  ~subplot_using (void);

  subplot_using *set_format (tree_expression *fmt);

  subplot_using *add_qualifier (tree_expression *t);

  int eval (int ndim, int n_max);

  ColumnVector values (int ndim, int n_max = 0);

  int print (int ndim, int n_max, ostrstream& plot_buf);

  void print_code (ostream& os);

private:
  int qualifier_count;
  tree_expression *x[4];
  tree_expression *scanf_fmt;
  ColumnVector val;
};

class
subplot_style : public tree_print_code
{
public:
  subplot_style (void);
  subplot_style (char *s);
  subplot_style (char *s, tree_expression *lt);
  subplot_style (char *s, tree_expression *lt, tree_expression *pt);

  ~subplot_style (void);

  int print (ostrstream& plot_buf);

  int errorbars (void);

  void print_code (ostream& os);

private:
  char *style;
  tree_expression *linetype;
  tree_expression *pointtype;
};

class
subplot : public tree_print_code
{
public:
  subplot (void);
  subplot (tree_expression *data);
  subplot (subplot_using *u, tree_expression *t, subplot_style *s);

  ~subplot (void);

  void set_data (tree_expression *data);

  tree_constant extract_plot_data (int ndim, tree_constant& data);

  int handle_plot_data (int ndim, ostrstream& plot_buf);

  int print (int ndim, ostrstream& plot_buf);

  void print_code (ostream& os);

private:
  tree_expression *plot_data;
  subplot_using *using_clause;
  tree_expression *title_clause;
  subplot_style *style_clause;
};

class
subplot_list : public SLList<subplot *>, public tree_print_code
{
 public:
  subplot_list (void) : SLList<subplot *> (), tree_print_code () { }

  subplot_list (subplot *t) : SLList<subplot *> (), tree_print_code ()
    { append (t); }

  ~subplot_list (void)
    {
      while (! empty ())
	{
	  subplot *t = remove_front ();
	  delete t;
	}
    }

  int print (int ndim, ostrstream& plot_buf);

  void print_code (ostream& os);
};

extern char *save_in_tmp_file (tree_constant& t, int ndim = 2,
			       int parametric = 0);

extern void mark_for_deletion (const char *filename);

extern void cleanup_tmp_files (void);

extern void close_plot_stream (void);

extern void do_external_plotter_cd (const char *newdir);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
