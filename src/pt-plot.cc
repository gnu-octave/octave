// tree-plot.cc                                         -*- C++ -*-
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if defined (__GNUG__)
#pragma implementation
#endif

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <iostream.h>
#include <fstream.h>
#include <strstream.h>

#include "SLStack.h"
#include "procstream.h"

#include "user-prefs.h"
#include "tree-const.h"
#include "tree-plot.h"
#include "help.h"
#include "error.h"
#include "utils.h"
#include "tree.h"
#include "defun.h"

extern "C"
{
  char *tilde_expand (char *s); /* From readline's tilde.c */
}

// The number of lines we\'ve plotted so far.
int plot_line_count;

// Is this a parametric plot?  Makes a difference for 3D plotting.
int parametric_plot = 0;

// Should the graph window be cleared before plotting the next line?
int clear_before_plotting = 1;

// List of files to delete when we exit or crash.
static SLStack <char *> tmp_files;

// Pipe to gnuplot.
static oprocstream plot_stream;

/*
 * Plotting, eh?
 */

tree_plot_command::tree_plot_command (void)
{
  range = 0;
  plot_list = 0;
  ndim = 0;
}

tree_plot_command::tree_plot_command (tree_subplot_list *plt, int nd)
{
  range = 0;
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
    case 1:
      if (plot_line_count == 0)
	{
	  if (plot_list)
	    plot_buf << "plot";
	  else
	    {
	      ::error ("replot: must have something to plot");
	      return retval;
	    }
	}
      else
	plot_buf << "replot";
      break;
    case 2:
      if (clear_before_plotting || plot_line_count == 0)
	{
	  plot_line_count = 0;
	  plot_buf << "plot";
	}
      else
	plot_buf << "replot";
      break;
    case 3:
      {
	plot_line_count = 0;
	plot_buf << "splot";
      }
      break;
    default:
      panic_impossible ();
      break;
    }

  if (range)
    {
      if (plot_line_count == 0)
	range->print (ndim, plot_buf);
      else
	warning ("can't specify new plot ranges with `replot' or while\
 hold is on");
    }

  if (error_state)
    return retval;

  for (tree_subplot_list *ptr = plot_list; ptr; ptr = ptr->next_elem ())
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
  plot_data = 0;
  using = 0;
  title = 0;
  style = 0;
  next = 0;
}

tree_subplot_list::tree_subplot_list (tree_expression *data)
{
  plot_data = data;
  using = 0;
  title = 0;
  style = 0;
  next = 0;
}

tree_subplot_list::tree_subplot_list (tree_subplot_list *t)
{
  plot_data = t->plot_data;
  using = t->using;
  title = t->title;
  style = t->style;
  next = t->next;
}

tree_subplot_list::tree_subplot_list (tree_subplot_using *u,
				      tree_expression *t,
				      tree_subplot_style *s)
{
  plot_data = 0;
  using = u;
  title = t;
  style = s;
  next = 0;
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
tree_subplot_list::set_data (tree_expression *data)
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
  tree_subplot_list *prev = 0;

  while (list)
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
  if (plot_data)
    {
      tree_constant data = plot_data->eval (0);
      if (! error_state && data.is_defined ())
	{
	  char *file = 0;
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
		  file = 0;

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

  if (using)
    {
      int status = using->print (ndim, nc, plot_buf);
      if (status < 0)
	return -1;
    }

  if (title)
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

  if (style)
    {
      int status = style->print (plot_buf);
      if (status < 0)
	return -1;
    }

  return 0;
}

tree_plot_limits::tree_plot_limits (void)
{
  x_range = 0;
  y_range = 0;
  z_range = 0;
}

tree_plot_limits::tree_plot_limits (tree_plot_range *xlim)
{
  x_range = xlim;
  y_range = 0;
  z_range = 0;
}

tree_plot_limits::tree_plot_limits (tree_plot_range *xlim,
				    tree_plot_range *ylim)
{
  x_range = xlim;
  y_range = ylim;
  z_range = 0;
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
      if (x_range)
	x_range->print (plot_buf);
      else
	return;

      if (y_range)
	y_range->print (plot_buf);
      else
	return;
    }

  if (ndim == 3 && z_range)
    z_range->print (plot_buf);
}

tree_plot_range::tree_plot_range (void)
{
  lower = 0;
  upper = 0;
}

tree_plot_range::tree_plot_range (tree_expression *l, tree_expression *u)
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

  if (lower)
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

  if (upper)
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
  x[0] = 0;
  x[1] = 0;
  x[2] = 0;
  x[3] = 0;
  scanf_fmt = 0;
}

tree_subplot_using::tree_subplot_using (tree_expression *fmt)
{
  qualifier_count = 0;
  x[0] = 0;
  x[1] = 0;
  x[2] = 0;
  x[3] = 0;
  scanf_fmt = fmt;
}

tree_subplot_using::~tree_subplot_using (void)
{
  delete scanf_fmt;
}

tree_subplot_using *
tree_subplot_using::set_format (tree_expression *fmt)
{
  scanf_fmt = fmt;
  return this;
}

tree_subplot_using *
tree_subplot_using::add_qualifier (tree_expression *t)
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
      if (x[i])
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

	      if (n < 1 || n_max > 0 && n > n_max)
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

  if (scanf_fmt)
    warning ("ignoring scanf format in plot command");

  return 0;
}

tree_subplot_style::tree_subplot_style (void)
{
  style = 0;
  linetype = 0;
  pointtype = 0;
}

tree_subplot_style::tree_subplot_style (char *s)
{
  style = strsave (s);
  linetype = 0;
  pointtype = 0;
}

tree_subplot_style::tree_subplot_style (char *s, tree_expression *lt)
{
  style = strsave (s);
  linetype = lt;
  pointtype = 0;
}

tree_subplot_style::tree_subplot_style (char *s, tree_expression *lt,
					tree_expression *pt)
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
  if (style)
    {
      plot_buf << " with " << style;

      if (linetype)
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

      if (pointtype)
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

char *
save_in_tmp_file (tree_constant& t, int ndim, int parametric)
{
  char *name = strsave (tmpnam (0));
  if (name)
    {
      ofstream file (name);
      if (file)
	{
	  switch (ndim)
	    {
	    case 2:
	      t.save (file);
	      break;
	    case 3:
	      t.save_three_d (file, parametric);
	      break;
	    default:
	      panic_impossible ();
	      break;
	    }
	}
      else
	{
	  error ("couldn't open temporary output file `%s'", name);
	  delete [] name;
	  name = 0;
	}
    }
  return name;
}

void
mark_for_deletion (const char *filename)
{
  char *tmp = strsave (filename);
  tmp_files.push (tmp);
}

void
cleanup_tmp_files (void)
{
  while (! tmp_files.empty ())
    {
      char *filename = tmp_files.pop ();
      unlink (filename);
      delete [] filename;
    }
}

int
send_to_plot_stream (const char *cmd)
{
// From sighandlers.cc:
  extern int pipe_handler_error_count;

  static int initialized = 0;

  if (! plot_stream.is_open ())
    {
      plot_line_count = 0;

      char *plot_prog = user_pref.gnuplot_binary;
      if (plot_prog)
	{
	  plot_stream.open (plot_prog);
	  if (! plot_stream.is_open ())
	    {
	      warning ("plot: unable to open pipe to `%s'",
		       plot_prog);

	      if (strcmp (plot_prog, "gnuplot") != 0)
		{
		  warning ("having trouble finding plotting program.");
		  warning ("trying again with `gnuplot'");
		  goto last_chance;
		}
	    }
	}
      else
	{
	last_chance:

	  plot_stream.open ("gnuplot");

	  if (! plot_stream.is_open ())
	    {
	      error ("plot: unable to open pipe to `%s'", plot_prog);
	      return -1;
	    }
	}
    }

  if (! initialized)
    {
      initialized = 1;
      plot_stream << "set data style lines\n";
    }

  plot_stream << cmd;
  plot_stream.flush ();
  pipe_handler_error_count = 0;

  return 0;
}

void
close_plot_stream (void)
{
  if (plot_stream.is_open ())
    plot_stream.close ();

  plot_line_count = 0;
}

DEFUN ("closeplot", Fcloseplot, Scloseplot, 1, 0,
  "closeplot (): close the stream to plotter")
{
  Octave_object retval;
  close_plot_stream ();
  return retval;
}

DEFUN_TEXT ("hold", Fhold, Shold, -1, 1,
  "hold [on|off]\n\
\n\
determine whether the plot window is cleared before the next line is\n\
drawn.  With no argument, toggle the current state.") 
{
  Octave_object retval;

  DEFINE_ARGV("hold");

  switch (argc)
    {
    case 1:
      clear_before_plotting = ! clear_before_plotting;
      break;
    case 2:
      if (strcasecmp (argv[1], "on") == 0)
	clear_before_plotting = 0;
      else if (strcasecmp (argv[1], "off") == 0)
	clear_before_plotting = 1;
      else
	print_usage ("hold");
      break;
    default:
      print_usage ("hold");
      break;
    }

  DELETE_ARGV;

  return retval;
}

DEFUN ("purge_tmp_files", Fpurge_tmp_files, Spurge_tmp_files, 5, 1,
  "delete temporary data files used for plotting")
{
  Octave_object retval;
  cleanup_tmp_files ();
  return retval;
}

DEFUN_TEXT ("set", Fset, Sset, -1, 1,
  "set [options]\n\
\n\
set plotting options")
{
  Octave_object retval;

  DEFINE_ARGV("set");

  ostrstream plot_buf;

  if (argc > 1)
    {
      if (almost_match ("parametric", argv[1], 3))
	parametric_plot = 1;
      else if (almost_match ("noparametric", argv[1], 5))
	parametric_plot = 0;
    }

  for (int i = 0; i < argc; i++)
    plot_buf << argv[i] << " ";

  plot_buf << "\n" << ends;

  char *plot_command = plot_buf.str ();
  send_to_plot_stream (plot_command);

  delete [] plot_command;

  DELETE_ARGV;

  return retval;
}

/*
 * Set plotting options.
 */
DEFUN_TEXT ("show", Fshow, Sshow, -1, 1,
  "show [options]\n\
\n\
show plotting options")
{
  Octave_object retval;

  DEFINE_ARGV("show");

  ostrstream plot_buf;

  for (int i = 0; i < argc; i++)
    plot_buf << argv[i] << " ";

  plot_buf << "\n" << ends;

  char *plot_command = plot_buf.str ();
  send_to_plot_stream (plot_command);

  delete [] plot_command;

  DELETE_ARGV;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
