// tree-plot.cc                                         -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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
#include <fstream.h>
#include <strstream.h>

#include "error.h"
#include "utils.h"
#include "tree.h"

extern "C"
{
  char *tilde_expand (char *s); /* From readline's tilde.c */
}

// The number of lines we\'ve plotted so far.
static int plot_line_count;

// Is this a parametric plot?  Makes a difference for 3D plotting.
int parametric_plot = 0;

/*
 * Plotting, eh?
 */

tree_plot_command::tree_plot_command (void)
{
  range = (tree_plot_limits *) NULL;
  plot_list = (tree_subplot_list *) NULL;
  ndim = 0;
}

tree_plot_command::tree_plot_command (tree_subplot_list *plt, int nd)
{
  range = (tree_plot_limits *) NULL;
  plot_list = plt;
  ndim = nd;
}

tree_plot_command::tree_plot_command (tree_subplot_list *plt,
				      tree_plot_limits *rng, int nd)
{
  range = rng;
  plot_list = plt;
  ndim = nd;
}

tree_plot_command::~tree_plot_command (void)
{
  delete range;
  delete plot_list;
}

tree_constant
tree_plot_command::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  ostrstream plot_buf;

  switch (ndim)
    {
    case 2:
      plot_buf << "plot";
      break;
    case 3:
      plot_buf << "splot";
      break;
    default:
      panic_impossible ();
      break;
    }

  if (range != (tree_plot_limits *) NULL)
    range->print (ndim, plot_buf);

  if (error_state)
    return retval;

  plot_line_count = 0;
  tree_subplot_list *ptr = plot_list;
  for ( ; ptr != NULL_TREE ; ptr = ptr->next_elem ())
    {
      plot_line_count++;

      if (ptr != plot_list)
	plot_buf << ",\\\n  ";

      int status = ptr->print (ndim, plot_buf);
      if (status < 0)
	return retval;
    }

  plot_buf << "\n" << ends;

// Just testing...
//  char *message = plot_buf.str ();
//  cout << "[*]" << message << "[*]\n";

  if (parametric_plot && ndim == 2)
    {
      warning ("can't make 2D parametric plot -- setting noparametric...");
      send_to_plot_stream ("set noparametric\n");
      char *message = plot_buf.str ();
      send_to_plot_stream (message);
      delete [] message;
      send_to_plot_stream ("set parametric\n");
    }
  else
    {
      char *message = plot_buf.str ();
      send_to_plot_stream (message);
      delete [] message;
    }

  return retval;
}

tree_subplot_list::tree_subplot_list (void)
{
  plot_data = NULL_TREE;
  using = (tree_subplot_using *) NULL;
  title = NULL_TREE;
  style = (tree_subplot_style *) NULL;
  next = (tree_subplot_list *) NULL;
}

tree_subplot_list::tree_subplot_list (tree *data)
{
  plot_data = data;
  using = (tree_subplot_using *) NULL;
  title = NULL_TREE;
  style = (tree_subplot_style *) NULL;
  next = (tree_subplot_list *) NULL;
}

tree_subplot_list::tree_subplot_list (tree_subplot_list *t)
{
  plot_data = t->plot_data;
  using = t->using;
  title = t->title;
  style = t->style;
  next = t->next;
}

tree_subplot_list::tree_subplot_list (tree_subplot_using *u, tree *t,
				      tree_subplot_style *s)
{
  plot_data = NULL_TREE;
  using = u;
  title = t;
  style = s;
  next = (tree_subplot_list *) NULL;
}

tree_subplot_list::~tree_subplot_list (void)
{
  delete plot_data;
  delete using;
  delete title;
  delete style;
  delete next;
}

tree_subplot_list *
tree_subplot_list::set_data (tree *data)
{
  plot_data = data;
  return this;
}

tree_subplot_list *
tree_subplot_list::chain (tree_subplot_list *t)
{
  tree_subplot_list *tmp = new tree_subplot_list (t);
  tmp->next = this;
  return tmp;
}

tree_subplot_list *
tree_subplot_list::reverse (void)
{
  tree_subplot_list *list = this;
  tree_subplot_list *next;
  tree_subplot_list *prev = (tree_subplot_list *) NULL;

  while (list != (tree_subplot_list *) NULL)
    {
      next = list->next;
      list->next = prev;
      prev = list;
      list = next;
    }
  return prev;
}

tree_subplot_list *
tree_subplot_list::next_elem (void)
{
  return next;
}

tree_constant
tree_subplot_list::eval (int print)
{
  return plot_data->eval (0);
}

int
tree_subplot_list::print (int ndim, ostrstream& plot_buf)
{
  int nc = 0;
  if (plot_data != NULL_TREE)
    {
      tree_constant data = plot_data->eval (0);
      if (! error_state && data.is_defined ())
	{
	  char *file = (char *) NULL;
	  if (data.is_string_type ())
	    {
	      file = tilde_expand (data.string_value ());
	      ifstream ftmp (file);
	      if (ftmp)
		{
		  plot_buf << " \"" << file << '"';
		  free (file);
		  goto have_existing_file_or_command;
		}
	      else
		{
		  free (file);
		  file = (char *) NULL;

// Opening as a file failed.  Let's try passing it along as a plot
// command.
		  plot_buf << " " << data.string_value ();
		  goto have_existing_file_or_command;
		}
	    }

	  nc = data.columns ();
	  switch (ndim)
	    {
	    case 2:
	      file = save_in_tmp_file (data, ndim);
	      break;
	    case 3:
	      file = save_in_tmp_file (data, ndim, parametric_plot);
	      break;
	    default:
	      panic_impossible ();
	      break;
	    }

	  if (file)
	    {
	      mark_for_deletion (file);
	      plot_buf << " \"" << file << '"';
	    }
	}
      else
	return -1;
    }
  else
    return -1;

 have_existing_file_or_command:

  if (using != (tree_subplot_using *) NULL)
    {
      int status = using->print (ndim, nc, plot_buf);
      if (status < 0)
	return -1;
    }

  if (title != NULL_TREE)
    {
      tree_constant tmp = title->eval (0);
      if (! error_state && tmp.is_string_type ())
	plot_buf << " title " << '"' << tmp.string_value () << '"';
      else
	{
	  warning ("line title must be a string");
	  plot_buf << " title " << '"' << "line " << plot_line_count << '"';
	}
    }
  else
    plot_buf << " title " << '"' << "line " << plot_line_count << '"';

  if (style != (tree_subplot_style *) NULL)
    {
      int status = style->print (plot_buf);
      if (status < 0)
	return -1;
    }

  return 0;
}

tree_plot_limits::tree_plot_limits (void)
{
  x_range = (tree_plot_range *) NULL;
  y_range = (tree_plot_range *) NULL;
  z_range = (tree_plot_range *) NULL;
}

tree_plot_limits::tree_plot_limits (tree_plot_range *xlim)
{
  x_range = xlim;
  y_range = (tree_plot_range *) NULL;
  z_range = (tree_plot_range *) NULL;
}

tree_plot_limits::tree_plot_limits (tree_plot_range *xlim,
				    tree_plot_range *ylim)
{
  x_range = xlim;
  y_range = ylim;
  z_range = (tree_plot_range *) NULL;
}

tree_plot_limits::tree_plot_limits (tree_plot_range *xlim,
				    tree_plot_range *ylim,
				    tree_plot_range *zlim)
{
  x_range = xlim;
  y_range = ylim;
  z_range = zlim;
}

tree_plot_limits::~tree_plot_limits (void)
{
  delete x_range;
  delete y_range;
  delete z_range;
}

tree_constant
tree_plot_limits::eval (int print)
{
  tree_constant retval;
  return retval;
}

void
tree_plot_limits::print (int ndim, ostrstream& plot_buf)
{
  if (ndim  == 2 || ndim == 3)
    {
      if (x_range != (tree_plot_range *) NULL)
	x_range->print (plot_buf);
      else
	return;

      if (y_range != (tree_plot_range *) NULL)
	y_range->print (plot_buf);
      else
	return;
    }

  if (ndim == 3 && z_range != (tree_plot_range *) NULL)
    z_range->print (plot_buf);
}

tree_plot_range::tree_plot_range (void)
{
  lower = NULL_TREE;
  upper = NULL_TREE;
}

tree_plot_range::tree_plot_range (tree *l, tree *u)
{
  lower = l;
  upper = u;
}

tree_plot_range::~tree_plot_range (void)
{
  delete lower;
  delete upper;
}

tree_constant
tree_plot_range::eval (int print)
{
  tree_constant retval;
  return retval;
}

void
tree_plot_range::print (ostrstream& plot_buf)
{
  plot_buf << " [";

  if (lower != NULL_TREE)
    {
      tree_constant lower_val = lower->eval (0);
      if (error_state)
	{
	  ::error ("evaluating lower bound of plot range");
	  return;
	}
      else
	{
	  double lo = lower_val.to_scalar ();
	  plot_buf << lo;
	}
    }

  plot_buf << ":";

  if (upper != NULL_TREE)
    {
      tree_constant upper_val = upper->eval (0);
      if (error_state)
	{
	  ::error ("evaluating upper bound of plot range");
	  return;
	}
      else
	{
	  double hi = upper_val.to_scalar ();
	  plot_buf << hi;
	}
    }

  plot_buf << "]";
}

tree_subplot_using::tree_subplot_using (void)
{
  qualifier_count = 0;
  x[0] = NULL_TREE;
  x[1] = NULL_TREE;
  x[2] = NULL_TREE;
  x[3] = NULL_TREE;
  scanf_fmt = NULL_TREE;
}

tree_subplot_using::tree_subplot_using (tree *fmt)
{
  qualifier_count = 0;
  x[0] = NULL_TREE;
  x[1] = NULL_TREE;
  x[2] = NULL_TREE;
  x[3] = NULL_TREE;
  scanf_fmt = fmt;
}

tree_subplot_using::~tree_subplot_using (void)
{
  delete scanf_fmt;
}

tree_subplot_using *
tree_subplot_using::set_format (tree *fmt)
{
  scanf_fmt = fmt;
  return this;
}

tree_subplot_using *
tree_subplot_using::add_qualifier (tree *t)
{
  if (qualifier_count < 4)
    x[qualifier_count] = t;

  qualifier_count++;

  return this;
}

tree_constant
tree_subplot_using::eval (int print)
{
  tree_constant retval;
  return retval;
}

int
tree_subplot_using::print (int ndim, int n_max, ostrstream& plot_buf)
{
  if ((ndim == 2 && qualifier_count > 4)
      || (ndim == 3 && qualifier_count > 3))
    return -1;

  for (int i = 0; i < qualifier_count; i++)
    {
      if (x[i] != NULL_TREE)
	{
	  tree_constant tmp = x[i]->eval (0);
	  if (error_state)
	    {
	      ::error ("evaluating plot using command");
	      return -1;
	    }

	  double val;
	  if (tmp.is_defined ())
	    {
	      val = tmp.to_scalar ();
	      if (i == 0)
		plot_buf << " using ";
	      else
		plot_buf << ":";

	      int n = NINT (val);

	      if (n > n_max || n < 1)
		{
		  ::error ("using: column %d out of range", n); 
		  return -1;
		}
	      else
		plot_buf << n;
	    }
	  else
	    return -1;
	}
      else
	return -1;
    }

  if (scanf_fmt != NULL_TREE)
    warning ("ignoring scanf format in plot command");

  return 0;
}

tree_subplot_style::tree_subplot_style (void)
{
  style = (char *) NULL;
  linetype = NULL_TREE;
  pointtype = NULL_TREE;
}

tree_subplot_style::tree_subplot_style (char *s)
{
  style = strsave (s);
  linetype = NULL_TREE;
  pointtype = NULL_TREE;
}

tree_subplot_style::tree_subplot_style (char *s, tree *lt)
{
  style = strsave (s);
  linetype = lt;
  pointtype = NULL_TREE;
}

tree_subplot_style::tree_subplot_style (char *s, tree *lt, tree *pt)
{
  style = strsave (s);
  linetype = lt;
  pointtype = pt;
}

tree_subplot_style::~tree_subplot_style (void)
{ 
  delete [] style;
  delete linetype;
  delete pointtype;
}

tree_constant
tree_subplot_style::eval (int print)
{
  tree_constant retval;
  return retval;
}

int
tree_subplot_style::print (ostrstream& plot_buf)
{
  if (style != (char *) NULL)
    {
      plot_buf << " with " << style;

      if (linetype != NULL_TREE)
	{
	  tree_constant tmp = linetype->eval (0);
	  if (! error_state && tmp.is_defined ())
	    {
	      double val = tmp.to_scalar ();
	      plot_buf << " " << NINT (val);
	    }
	  else
	    {
	      ::error ("evaluating plot style command");
	      return -1;
	    }
	}

      if (pointtype != NULL_TREE)
	{
	  tree_constant tmp = pointtype->eval (0);
	  if (! error_state && tmp.is_defined ())
	    {
	      double val = tmp.to_scalar ();
	      plot_buf << " " << NINT (val);
	    }
	  else
	    {
	      ::error ("evaluating plot style command");
	      return -1;
	    }
	}
    }
  else
    return -1;

  return 0;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
