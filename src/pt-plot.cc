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
#include <string.h>
#include <iostream.h>
#include <strstream.h>
#include <fstream.h>

#include "SLStack.h"
#include "procstream.h"

#include "user-prefs.h"
#include "tree-base.h"
#include "tree-expr.h"
#include "tree-cmd.h"
#include "tree-const.h"
#include "tree-plot.h"
#include "load-save.h"
#include "help.h"
#include "error.h"
#include "utils.h"
#include "defun.h"

extern "C"
{
#include <readline/tilde.h>
}

// The number of lines we\'ve plotted so far.
int plot_line_count = 0;

// Is this a parametric plot?  Makes a difference for 3D plotting.
int parametric_plot = 0;

// Should the graph window be cleared before plotting the next line?
int clear_before_plotting = 1;

// List of files to delete when we exit or crash.
static SLStack <char *> tmp_files;

// Pipe to gnuplot.
static oprocstream plot_stream;

static void
open_plot_stream (void)
{
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
	    error ("plot: unable to open pipe to `%s'", plot_prog);
	}
    }

  if (! initialized)
    {
      initialized = 1;
      plot_stream << "set data style lines\n";
    }
}

static int
send_to_plot_stream (const char *cmd)
{
// From sighandlers.cc:
  extern int pipe_handler_error_count;

  if (! plot_stream.is_open ())
    {
      open_plot_stream ();

      if (error_state)
	return -1;
    }

  if (plot_line_count == 0 && strncmp (cmd, "replot", 6) == 0)
    error ("replot: no previous plot");
  else
    {
      plot_stream << cmd;
      plot_stream.flush ();
      pipe_handler_error_count = 0;
    }

  return 0;
}

// Plotting, eh?

tree_plot_command::tree_plot_command (void) : tree_command ()
{
  range = 0;
  plot_list = 0;
  ndim = 0;
}

tree_plot_command::tree_plot_command (subplot_list *plt, int nd)
  : tree_command ()
{
  range = 0;
  plot_list = plt;
  ndim = nd;
}

tree_plot_command::tree_plot_command (subplot_list *plt,
				      plot_limits *rng, int nd)
  : tree_command ()
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

void
tree_plot_command::eval (void)
{
  if (error_state)
    return;

  open_plot_stream ();

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
	      return;
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
    return;

  if (plot_list)
    {
      int status = plot_list->print (ndim, plot_buf);

      if (status < 0)
	return;
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
}

void
tree_plot_command::print_code (ostream& os)
{
  print_code_indent (os);

  switch (ndim)
    {
    case 1:
      os << "replot";
      break;
    case 2:
      os << "gplot";
      break;
    case 3:
      os << "gsplot";
      break;
    default:
      panic_impossible ();
      break;
    }

  if (range)
    range->print_code (os);

  if (plot_list)
    plot_list->print_code (os);
}

plot_limits::plot_limits (void)
{
  x_range = 0;
  y_range = 0;
  z_range = 0;
}

plot_limits::plot_limits (plot_range *xlim)
{
  x_range = xlim;
  y_range = 0;
  z_range = 0;
}

plot_limits::plot_limits (plot_range *xlim,
				    plot_range *ylim)
{
  x_range = xlim;
  y_range = ylim;
  z_range = 0;
}

plot_limits::plot_limits (plot_range *xlim,
				    plot_range *ylim,
				    plot_range *zlim)
{
  x_range = xlim;
  y_range = ylim;
  z_range = zlim;
}

plot_limits::~plot_limits (void)
{
  delete x_range;
  delete y_range;
  delete z_range;
}

void
plot_limits::print (int ndim, ostrstream& plot_buf)
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

void
plot_limits::print_code (ostream& os)
{
  if (x_range)
    x_range->print_code (os);

  if (y_range)
    y_range->print_code (os);

  if (z_range)
    z_range->print_code (os);
}

plot_range::plot_range (void)
{
  lower = 0;
  upper = 0;
}

plot_range::plot_range (tree_expression *l, tree_expression *u)
{
  lower = l;
  upper = u;
}

plot_range::~plot_range (void)
{
  delete lower;
  delete upper;
}

void
plot_range::print (ostrstream& plot_buf)
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
	  double lo = lower_val.double_value ();
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
	  double hi = upper_val.double_value ();
	  plot_buf << hi;
	}
    }

  plot_buf << "]";
}

void
plot_range::print_code (ostream& os)
{
  os << " [";

  if (lower)
    lower->print_code (os);

  os << ":";

  if (upper)
    upper->print_code (os);

  os << "]";
}

subplot_using::subplot_using (void)
{
  qualifier_count = 0;
  x[0] = 0;
  x[1] = 0;
  x[2] = 0;
  x[3] = 0;
  scanf_fmt = 0;
}

subplot_using::subplot_using (tree_expression *fmt)
{
  qualifier_count = 0;
  x[0] = 0;
  x[1] = 0;
  x[2] = 0;
  x[3] = 0;
  scanf_fmt = fmt;
}

subplot_using::~subplot_using (void)
{
  delete scanf_fmt;
}

subplot_using *
subplot_using::set_format (tree_expression *fmt)
{
  scanf_fmt = fmt;
  return this;
}

subplot_using *
subplot_using::add_qualifier (tree_expression *t)
{
  if (qualifier_count < 4)
    x[qualifier_count] = t;

  qualifier_count++;

  return this;
}

int
subplot_using::print (int ndim, int n_max, ostrstream& plot_buf)
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
	      val = tmp.double_value ();
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

void
subplot_using::print_code (ostream& os)
{
  os << " using ";
  for (int i = 0; i < qualifier_count; i++)
    {
      if (i > 0)
	os << ":";

      if (x[i])
	x[i]->print_code (os);
    }
}

subplot_style::subplot_style (void)
{
  style = 0;
  linetype = 0;
  pointtype = 0;
}

subplot_style::subplot_style (char *s)
{
  style = strsave (s);
  linetype = 0;
  pointtype = 0;
}

subplot_style::subplot_style (char *s, tree_expression *lt)
{
  style = strsave (s);
  linetype = lt;
  pointtype = 0;
}

subplot_style::subplot_style (char *s, tree_expression *lt,
					tree_expression *pt)
{
  style = strsave (s);
  linetype = lt;
  pointtype = pt;
}

subplot_style::~subplot_style (void)
{ 
  delete [] style;
  delete linetype;
  delete pointtype;
}

int
subplot_style::print (ostrstream& plot_buf)
{
  if (style)
    {
      plot_buf << " with " << style;

      if (linetype)
	{
	  tree_constant tmp = linetype->eval (0);
	  if (! error_state && tmp.is_defined ())
	    {
	      double val = tmp.double_value ();
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
	      double val = tmp.double_value ();
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

void
subplot_style::print_code (ostream& os)
{
  os << " with " << style;

  if (linetype)
    {
      os << " ";
      linetype->print_code (os);
    }

  if (pointtype)
    {
      os << " ";
      pointtype->print_code (os);
    }
}

int
subplot::print (int ndim, ostrstream& plot_buf)
{
  int nc = 0;
  if (plot_data)
    {
      tree_constant data = plot_data->eval (0);
      if (! error_state && data.is_defined ())
	{
	  char *file = 0;
	  if (data.is_string ())
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
      if (! error_state && tmp.is_string ())
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

void
subplot::print_code (ostream& os)
{
  if (plot_data)
    {
      os << " ";
      plot_data->print_code (os);
    }

  if (using)
    using->print_code (os);

  if (title)
    title->print_code (os);

  if (style)
    style->print_code (os);
}

int
subplot_list::print (int ndim, ostrstream& plot_buf)
{
  int status = 0;

  for (Pix p = first (); p != 0; next (p))
    {
      subplot *elt = this->operator () (p);

      plot_line_count++;

      if (p != first ())
	plot_buf << ",\\\n  ";

      status = elt->print (ndim, plot_buf);

      if (status < 0)
	break;
    }

  return status;
}

void
subplot_list::print_code (ostream& os)
{
  Pix p = first ();

  while (p)
    {
      subplot *elt = this->operator () (p);

      next (p);

      if (elt)
	{
	  elt->print_code (os);

	  if (p)
	    os << ",";
	}
    }
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
	      save_ascii_data (file, t);
	      break;
	    case 3:
	      save_three_d (file, t, parametric);
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
