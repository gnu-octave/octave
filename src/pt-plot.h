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

#if !defined (octave_tree_plot_h)
#define octave_tree_plot_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include <iostream>

#include "lo-sstream.h"

class tree_expression;
class tree_plot_command;
class plot_limits;
class plot_range;
class subplot_using;
class subplot_style;
class subplot_axes;
class subplot;
class subplot_list;

class tree_walker;

#include <string>

#include <SLList.h>

#include "dColVector.h"

#include "pt-cmd.h"

class
tree_plot_command : public tree_command
{
public:

  tree_plot_command (subplot_list *plt = 0, plot_limits *rng = 0, int nd = 0)
    : tree_command (), ndim (nd), range (rng), plot_list (plt) { }

  ~tree_plot_command (void);

  void eval (void);

  int num_dimensions (void) { return ndim; }

  plot_limits *limits (void) { return range; }

  subplot_list *subplots (void) { return plot_list; }

  void accept (tree_walker& tw);

private:

  // The number of dimensions.  1 indicates a replot command.
  int ndim;

  // The data ranges for the plot.
  plot_limits *range;

  // The list of plots for this plot command.  For example, the
  // command "plot sin(x), cos(x)" has two subplot commands.
  subplot_list *plot_list;

  // No copying!

  tree_plot_command (const tree_plot_command&);

  tree_plot_command& operator = (const tree_plot_command&);
};

class
plot_limits
{
public:

  plot_limits (plot_range *xlim = 0, plot_range *ylim = 0,
	       plot_range *zlim = 0)
    : x_range (xlim), y_range (ylim), z_range (zlim) { }

  ~plot_limits (void);

  void print (int ndim, OSSTREAM& plot_buf);

  plot_range *x_limits (void) { return x_range; }
  plot_range *y_limits (void) { return y_range; }
  plot_range *z_limits (void) { return z_range; }

  void accept (tree_walker& tw);

private:

  // Specified limits of the x, y, and z axes we should display for
  // this plot.
  plot_range *x_range;
  plot_range *y_range;
  plot_range *z_range;

  // No copying!

  plot_limits (const plot_limits&);

  plot_limits& operator = (const plot_limits&);
};

class
plot_range
{
public:

  plot_range (tree_expression *l = 0, tree_expression *u = 0)
    : lower (l), upper (u) { }

  ~plot_range (void);

  void print (OSSTREAM& plot_buf);

  tree_expression *lower_bound (void) { return lower; }

  tree_expression *upper_bound (void) { return upper; }

  void accept (tree_walker& tw);

private:

  // A range can specify a lower or upper bound or both.  If neither
  // is specified, the range to display is determined from the data.
  tree_expression *lower;
  tree_expression *upper;

  // No copying!

  plot_range (const plot_range&);

  plot_range& operator = (const plot_range&);
};

class
subplot_using
{
public:

  subplot_using (tree_expression *fmt = 0)
    : qual_count (0), scanf_fmt (fmt), val (4, -1)
      {
	x[0] = x[1] = x[2] = x[3] = 0;
      }

  ~subplot_using (void);

  subplot_using *set_format (tree_expression *fmt)
    {
      scanf_fmt = fmt;
      return this;
    }

  subplot_using *add_qualifier (tree_expression *t)
    {
      if (qual_count < 4)
	x[qual_count] = t;

      qual_count++;

      return this;
    }

  int eval (int ndim, int n_max);

  ColumnVector values (int ndim, int n_max = 0);

  int print (int ndim, int n_max, OSSTREAM& plot_buf);

  int qualifier_count (void) { return qual_count; }

  tree_expression **qualifiers (void) { return x; }

  tree_expression *scanf_format (void) { return scanf_fmt; }

  void accept (tree_walker& tw);

private:

  // The number of using qualifiers (in "using 1:2", 1 and 2 are the
  // qualifiers).
  int qual_count;

  // An optional scanf-style format.  This is parsed and stored but
  // not currently used.
  tree_expression *scanf_fmt;

  // This is a cache for evaluated versions of the qualifiers stored
  // in x.
  ColumnVector val;

  // A vector to hold using qualifiers.
  tree_expression *x[4];

  // No copying!

  subplot_using (const subplot_using&);

  subplot_using& operator = (const subplot_using&);
};

class
subplot_style
{
public:

  subplot_style (const std::string& s = std::string (),
		 tree_expression *lt = 0, tree_expression *pt = 0)
    : sp_style (s), sp_linetype (lt), sp_pointtype (pt) { }

  ~subplot_style (void);

  int print (OSSTREAM& plot_buf);

  bool columns_ok (int nc);

  std::string style (void) { return sp_style; }

  tree_expression *linetype (void) { return sp_linetype; }

  tree_expression *pointtype (void) { return sp_pointtype; }

  void accept (tree_walker& tw);

private:

  // The style we are using: `lines', `points', etc.
  std::string sp_style;

  // The number of the line type to use.
  tree_expression *sp_linetype;

  // The number of the point type to use.
  tree_expression *sp_pointtype;

  // No copying!

  subplot_style (const subplot_style&);

  subplot_style& operator = (const subplot_style&);
};

class
subplot_axes
{
public:

  subplot_axes (const std::string& s = std::string ())
    : sp_axes (s) { }

  ~subplot_axes (void) { }

  int print (OSSTREAM& plot_buf);

  std::string axes (void) { return sp_axes; }

  void accept (tree_walker& tw);

private:

  // The axes we are using: `x1y1', `x1y2', etc.
  std::string sp_axes;

  // No copying!

  subplot_axes (const subplot_axes&);

  subplot_axes& operator = (const subplot_axes&);
};

class
subplot
{
public:

  subplot (tree_expression *data = 0)
    : sp_plot_data (data), sp_using_clause (0), sp_title_clause (0),
      sp_style_clause (0), sp_axes_clause (0) { }

  ~subplot (void);

  subplot *add_data (tree_expression *data)
    {
      sp_plot_data = data;
      return this;
    }

  subplot *add_clause (subplot_using *u)
    {
      if (! sp_using_clause)
	{
	  sp_using_clause = u;
	  return this;
	}
      else
	return 0;
    }

  subplot *add_clause (tree_expression *t)
    {
      if (! sp_title_clause)
	{
	  sp_title_clause = t;
	  return this;
	}
      else
	return 0;
    }

  subplot *add_clause (subplot_style *s)
    {
      if (! sp_style_clause)
	{
	  sp_style_clause = s;
	  return this;
	}
      else
	return 0;
    }

  subplot *add_clause (subplot_axes *a)
    {
      if (! sp_axes_clause)
	{
	  sp_axes_clause = a;
	  return this;
	}
      else
	return 0;
    }

  octave_value extract_plot_data (int ndim, octave_value& data);

  int handle_plot_data (int ndim, OSSTREAM& plot_buf);

  int print (int ndim, OSSTREAM& plot_buf);

  tree_expression *plot_data (void) { return sp_plot_data; }

  subplot_using *using_clause (void) { return sp_using_clause; }

  tree_expression *title_clause (void) { return sp_title_clause; }

  subplot_style *style_clause (void) { return sp_style_clause; }

  subplot_axes *axes_clause (void) { return sp_axes_clause; }

  void accept (tree_walker& tw);

private:

  // The data to plot.
  tree_expression *sp_plot_data;

  // The `using' option
  subplot_using *sp_using_clause;

  // The `title' option
  tree_expression *sp_title_clause;

  // The `style' option
  subplot_style *sp_style_clause;

  // The `axes' option
  subplot_axes *sp_axes_clause;

  // No copying!

  subplot (const subplot&);

  subplot& operator = (const subplot&);
};

class
subplot_list
{
public:

  subplot_list (void)
    : lst () { }

  subplot_list (subplot *t)
    : lst () { lst.append (t); }

  ~subplot_list (void);

  void append (subplot *&s) { lst.append (s); }
  void append (subplot * const &s) { lst.append (s); }

  subplot *&operator () (Pix p) { return lst (p); }

  subplot * const &operator () (Pix p) const { return lst (p); }

  Pix first (void) const { return lst.first (); }

  void next (Pix& p) const { return lst.next (p); }

  int print (int ndim, OSSTREAM& plot_buf);

  void accept (tree_walker& tw);

private:

  // The list of subplot commands.
  SLList<subplot *> lst;

  // No copying!

  subplot_list (const subplot_list&);

  subplot_list& operator = (const subplot_list&);
};

extern std::string save_in_tmp_file (octave_value& t, int ndim = 2,
				bool parametric = false);

extern void mark_for_deletion (const std::string&);

extern void cleanup_tmp_files (void);

extern void close_plot_stream (void);

extern void do_external_plotter_cd (const std::string& newdir);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
