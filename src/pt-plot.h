// pt-plot.h                                         -*- C++ -*-
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

class ostream;
class ostrstream;

class tree_command;
class tree_plot_command;
class plot_limits;
class plot_range;
class subplot_using;
class subplot_style;
class subplot;
class subplot_list;

#include <string>

#include <SLList.h>

#include "dColVector.h"

#include "idx-vector.h"
#include "pt-cmd.h"
#include "pt-exp.h"

class
tree_plot_command : public tree_command
{
public:
  tree_plot_command (subplot_list *plt = 0, plot_limits *rng = 0, int nd = 0)
    : tree_command (), ndim (nd), range (rng), plot_list (plt) { }

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
  plot_limits (plot_range *xlim = 0, plot_range *ylim = 0,
	       plot_range *zlim = 0)
    : tree_print_code (), x_range (xlim), y_range (ylim), z_range (zlim) { }

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
  plot_range (tree_expression *l = 0, tree_expression *u = 0)
    : tree_print_code (), lower (l), upper (u) { }

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
  subplot_using (void)
    {
      qualifier_count = 0;
      x[0] = x[1] = x[2] = x[3] = 0;
      scanf_fmt = 0;
    }

  subplot_using (tree_expression *fmt) : val (4, -1)
    {
      qualifier_count = 0;
      x[0] = x[1] = x[2] = x[3] = 0;
      scanf_fmt = fmt;
    }

  ~subplot_using (void);

  subplot_using *set_format (tree_expression *fmt)
    {
      scanf_fmt = fmt;
      return this;
    }

  subplot_using *add_qualifier (tree_expression *t)
    {
      if (qualifier_count < 4)
	x[qualifier_count] = t;

      qualifier_count++;

      return this;
    }

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
  subplot_style (void)
    : tree_print_code (), style (0), linetype (0), pointtype (0) { }

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
  subplot (tree_expression *data = 0)
    : tree_print_code (), plot_data (data), using_clause (0),
      title_clause (0), style_clause (0) { }

  subplot (subplot_using *u, tree_expression *t, subplot_style *s)
    : tree_print_code (), plot_data (0), using_clause (u),
      title_clause (t), style_clause (s) { }

  ~subplot (void);

  subplot *set_data (tree_expression *data)
    {
      plot_data = data;
      return this;
    }

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

  ~subplot_list (void);

  int print (int ndim, ostrstream& plot_buf);

  void print_code (ostream& os);
};

extern char *save_in_tmp_file (tree_constant& t, int ndim = 2,
			       int parametric = 0);

extern void mark_for_deletion (const string&);

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
