// pt-plot.cc                                         -*- C++ -*-
/*

Copyright (C) 1996 John W. Eaton

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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstring>

#include <string>

#include <fstream.h>
#include <iostream.h>
#include <strstream.h>

#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

#include "SLStack.h"
#include "procstream.h"

#include "file-ops.h"
#include "str-vec.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "load-save.h"
#include "mappers.h"
#include "oct-obj.h"
#include "pt-cmd.h"
#include "pt-exp.h"
#include "pt-plot.h"
#include "sysdep.h"
#include "user-prefs.h"
#include "utils.h"

// The number of lines we've plotted so far.
static int plot_line_count = 0;

// Is this a parametric plot?  Makes a difference for 3D plotting.
static bool parametric_plot = false;

// The gnuplot terminal type.
static char *gnuplot_terminal_type = 0;

// Should the graph window be cleared before plotting the next line?
static bool clear_before_plotting = true;

// List of files to delete when we exit or crash.
static SLStack <string> tmp_files;

// Pipe to gnuplot.
static oprocstream *plot_stream = 0;

// Use shortest possible abbreviations to minimize trouble caused by
// gnuplot's fixed-length command line buffer.

#ifndef GNUPLOT_COMMAND_PLOT  
#define GNUPLOT_COMMAND_PLOT   "pl"
#endif

#ifndef GNUPLOT_COMMAND_REPLOT 
#define GNUPLOT_COMMAND_REPLOT "rep"
#endif

#ifndef GNUPLOT_COMMAND_SPLOT 
#define GNUPLOT_COMMAND_SPLOT  "sp"
#endif

#ifndef GNUPLOT_COMMAND_USING
#define GNUPLOT_COMMAND_USING  "u"
#endif

#ifndef GNUPLOT_COMMAND_WITH 
#define GNUPLOT_COMMAND_WITH   "w"
#endif

#ifndef GNUPLOT_COMMAND_TITLE
#define GNUPLOT_COMMAND_TITLE  "t"
#endif

static void
open_plot_stream (void)
{
  static bool initialized = false;

  if (plot_stream && ! *plot_stream)
    {
      delete plot_stream;
      plot_stream = 0;
    }

  if (! plot_stream)
    {
      initialized = false;

      plot_line_count = 0;

      string plot_prog = user_pref.gnuplot_binary;

      if (! plot_prog.empty ())
	{
	  plot_stream = new oprocstream (plot_prog.c_str ());

	  if (plot_stream && ! *plot_stream)
	    {
	      delete plot_stream;
	      plot_stream = 0;
	    }

	  if (! plot_stream)
	    {
	      warning ("plot: unable to open pipe to `%s'",
		       plot_prog.c_str ());

	      if (plot_prog == "gnuplot")
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

	  plot_stream = new oprocstream ("gnuplot");

	  if (plot_stream && ! *plot_stream)
	    {
	      delete plot_stream;
	      plot_stream = 0;
	    }

	  if (! plot_stream)
	    error ("plot: unable to open pipe to `%s'",
		   plot_prog.c_str ());
	}
    }

  if (! error_state && plot_stream && *plot_stream && ! initialized)
    {
      initialized = true;
      *plot_stream << "set data style lines\n";

      if (gnuplot_terminal_type)
	*plot_stream << "set term " << gnuplot_terminal_type << "\n";
    }
}

static int
send_to_plot_stream (const char *cmd)
{
  // From sighandlers.cc:

  extern int pipe_handler_error_count;

  if (! (plot_stream && *plot_stream))
    {
      open_plot_stream ();

      if (error_state)
	return -1;
    }

  int replot_len = strlen (GNUPLOT_COMMAND_REPLOT);
  int splot_len = strlen (GNUPLOT_COMMAND_SPLOT);
  int plot_len = strlen (GNUPLOT_COMMAND_PLOT);

  bool is_replot = (strncmp (cmd, GNUPLOT_COMMAND_REPLOT, replot_len) == 0);
  bool is_splot = (strncmp (cmd, GNUPLOT_COMMAND_SPLOT, splot_len) == 0);
  bool is_plot = (strncmp (cmd, GNUPLOT_COMMAND_PLOT, plot_len) == 0);

  if (plot_line_count == 0 && is_replot)
    error ("replot: no previous plot");
  else
    {
      *plot_stream << cmd;

      if (! (is_replot || is_splot || is_plot)
	  && plot_line_count > 0
	  && user_pref.automatic_replot)
	*plot_stream << GNUPLOT_COMMAND_REPLOT << "\n";

      plot_stream->flush ();
      pipe_handler_error_count = 0;
    }

  return 0;
}

// Plotting, eh?

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
	    plot_buf << GNUPLOT_COMMAND_PLOT;
	  else
	    {
	      ::error ("replot: must have something to plot");
	      return;
	    }
	}
      else
	plot_buf << GNUPLOT_COMMAND_REPLOT;
      break;

    case 2:
      if (clear_before_plotting || plot_line_count == 0)
	{
	  plot_line_count = 0;
	  plot_buf << GNUPLOT_COMMAND_PLOT;
	}
      else
	plot_buf << GNUPLOT_COMMAND_REPLOT;
      break;

    case 3:
      if (clear_before_plotting || plot_line_count == 0)
	{
	  plot_line_count = 0;
	  plot_buf << GNUPLOT_COMMAND_SPLOT;
	}
      else
	plot_buf << GNUPLOT_COMMAND_REPLOT;
      break;

    default:
      gripe_2_or_3_dim_plot ();
      return;
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

      if (error_state || status < 0)
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
      os << "<unkown plot command>";
      break;
    }

  if (range)
    range->print_code (os);

  if (plot_list)
    plot_list->print_code (os);
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
      tree_constant lower_val = lower->eval (false);
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
      tree_constant upper_val = upper->eval (false);
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

subplot_using::~subplot_using (void)
{
  delete scanf_fmt;
}

int
subplot_using::eval (int ndim, int n_max)
{
  if ((ndim == 2 && qualifier_count > 4)
      || (ndim == 3 && qualifier_count > 3))
    return -1;

  if (qualifier_count > 0)
    val.resize (qualifier_count);

  for (int i = 0; i < qualifier_count; i++)
    {
      if (x[i])
	{
	  tree_constant tmp = x[i]->eval (false);
	  if (error_state)
	    {
	      ::error ("evaluating plot using command");
	      return -1;
	    }

	  double val_tmp;
	  if (tmp.is_defined ())
	    {
	      val_tmp = tmp.double_value ();

	      if (error_state)
		return -1;

	      if (xisnan (val_tmp))
		{
		  ::error ("NaN is invalid as a column specifier");
		  return -1;
		}

	      int n = NINT (val_tmp);

	      if (n < 1 || n_max > 0 && n > n_max)
		{
		  ::error ("using: column %d out of range", n); 
		  return -1;
		}
	      else
		val.elem (i) = n;
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

ColumnVector
subplot_using::values (int ndim, int n_max)
{
  int status = eval (ndim, n_max);

  if (status < 0)
    return -1;

  return val;
}

int
subplot_using::print (int ndim, int n_max, ostrstream& plot_buf)
{
  int status = eval (ndim, n_max);

  if (status < 0)
    return -1;

  for (int i = 0; i < qualifier_count; i++)
    {
      if (i == 0)
	plot_buf << " " << GNUPLOT_COMMAND_USING << " ";
      else
	plot_buf << ":";

      plot_buf << val.elem (i);
    }

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

subplot_style::subplot_style (const string& s)
{
  style = s;
  linetype = 0;
  pointtype = 0;
}

subplot_style::subplot_style (const string& s, tree_expression *lt)
{
  style = s;
  linetype = lt;
  pointtype = 0;
}

subplot_style::subplot_style (const string& s, tree_expression *lt,
			      tree_expression *pt)
{
  style = s;
  linetype = lt;
  pointtype = pt;
}

subplot_style::~subplot_style (void)
{
  delete linetype;
  delete pointtype;
}

int
subplot_style::print (ostrstream& plot_buf)
{
  if (! style.empty ())
    {
      plot_buf << " " << GNUPLOT_COMMAND_WITH << " " << style;

      if (linetype)
	{
	  tree_constant tmp = linetype->eval (false);
	  if (! error_state && tmp.is_defined ())
	    {
	      double val = tmp.double_value ();
	      if (xisnan (val))
		{
		  ::error ("NaN is invalid a plotting line style");
		  return -1;
		}
	      else
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
	  tree_constant tmp = pointtype->eval (false);
	  if (! error_state && tmp.is_defined ())
	    {
	      double val = tmp.double_value ();
	      if (xisnan (val))
		{
		  ::error ("NaN is invalid a plotting point style");
		  return -1;
		}
	      else
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

int
subplot_style::errorbars (void)
{
  return (almost_match ("errorbars", style, 1, 0)
	  || almost_match ("boxerrorbars", style, 5, 0));
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

subplot::~subplot (void)
{
  delete plot_data;
  delete using_clause;
  delete title_clause;
  delete style_clause;
}

tree_constant
subplot::extract_plot_data (int ndim, tree_constant& data)
{
  tree_constant retval;

  if (using_clause)
    {
      ColumnVector val = using_clause->values (ndim);

      Octave_object args;
      args(1) = val;
      args(0) = tree_constant::magic_colon_t;

      Octave_object tmp = data.eval (0, 1, args);
      retval = tmp(0);

      if (error_state)
	return tree_constant ();
    }
  else
    {
      retval = data;
    }

  if (ndim == 2 && style_clause && style_clause->errorbars ())
    {
      int nc = retval.columns ();

      if (nc < 3 || nc > 4)
	{
	  error ("plots with errorbars require 3 or 4 columns of data");
	  error ("but %d were provided", nc);
	  return tree_constant ();
	}
    }

  return retval;
}

int
subplot::handle_plot_data (int ndim, ostrstream& plot_buf)
{
  if (plot_data)
    {
      tree_constant data = plot_data->eval (false);

      if (! error_state && data.is_defined ())
	{
	  string file;

	  if (data.is_string ())
	    {
	      // Should really try to look at data file to determine
	      // n_max.  Can't do much about other arbitrary gnuplot
	      // commands though...

	      int n_max = 0;

	      file = oct_tilde_expand (data.string_value ());

	      ifstream ftmp (file.c_str ());

	      if (ftmp)
		{
		  plot_buf << " \"" << file << '"';
		}
	      else
		{
		  file = "";

		  // Opening as a file failed.  Let's try passing it
		  // along as a plot command.

		  plot_buf << " " << data.string_value ();
		}

	      if (using_clause)
		{
		  int status = using_clause->print (ndim, n_max, plot_buf);

		  if (status < 0)
		    return -1;
		}
	    }
	  else
	    {
	      // Eliminate the need for printing a using clause to
	      // plot_buf.

	      tree_constant tmp_data = extract_plot_data (ndim, data);

	      if (tmp_data.is_defined ())
		{
		  switch (ndim)
		    {
		    case 2:
		      file = save_in_tmp_file (tmp_data, ndim);
		      break;

		    case 3:
		      file = save_in_tmp_file (tmp_data, ndim,
					       parametric_plot);
		      break;

		    default:
		      gripe_2_or_3_dim_plot ();
		      break;
		    }

		  if (file.length () > 0)
		    {
		      mark_for_deletion (file);
		      plot_buf << " \"" << file << '"';
		    }
		}
	    }
	}
      else
	return -1;
    }
  else
    return -1;

  return 0;
}

int
subplot::print (int ndim, ostrstream& plot_buf)
{
  int status = handle_plot_data (ndim, plot_buf);

  if (status < 0)
    return -1;

  if (title_clause)
    {
      tree_constant tmp = title_clause->eval (false);
      if (! error_state && tmp.is_string ())
	plot_buf << " " << GNUPLOT_COMMAND_TITLE << " "
	  << '"' << tmp.string_value () << '"';
      else
	{
	  warning ("line title must be a string");
	  plot_buf << " " << GNUPLOT_COMMAND_TITLE << " "
	    << '"' << "line " << plot_line_count << '"';
	}
    }
  else
    plot_buf << " " << GNUPLOT_COMMAND_TITLE << " "
      << '"' << "line " << plot_line_count << '"';

  if (style_clause)
    {
      int status = style_clause->print (plot_buf);
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

  if (using_clause)
    using_clause->print_code (os);

  if (title_clause)
    title_clause->print_code (os);

  if (style_clause)
    style_clause->print_code (os);
}

subplot_list::~subplot_list (void)
{
  while (! empty ())
    {
      subplot *t = remove_front ();
      delete t;
    }
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

string
save_in_tmp_file (tree_constant& t, int ndim, bool parametric)
{
  string name = oct_tempnam ();

  if (! name.empty ())
    {
      ofstream file (name.c_str ());

      if (file)
	{
	  switch (ndim)
	    {
	    case 2:
	      save_ascii_data (file, t, name, 1);
	      break;

	    case 3:
	      save_three_d (file, t, parametric);
	      break;

	    default:
	      gripe_2_or_3_dim_plot ();
	      break;
	    }
	}
      else
	{
	  error ("couldn't open temporary output file `%s'", name.c_str ());
	  name.resize (0);
	}
    }

  return name;
}

void
mark_for_deletion (const string& file)
{
  tmp_files.push (file);
}

void
cleanup_tmp_files (void)
{
  while (! tmp_files.empty ())
    {
      string filename = tmp_files.pop ();
      unlink (filename.c_str ());
    }
}

void
close_plot_stream (void)
{
  if (plot_stream)
    {
      delete plot_stream;
      plot_stream = 0;
    }

  plot_line_count = 0;
}

void
do_external_plotter_cd (const string& newdir)
{
  if (plot_stream && *plot_stream)
    {
      ostrstream plot_buf;
      plot_buf << "cd \"" << newdir << "\"\n" << ends;
      char *message = plot_buf.str ();
      send_to_plot_stream (message);
      delete [] message;
    }
}

DEFUN ("clearplot", Fclearplot, Sclearplot, 00,
  "clearplot (): clear the plot window")
{
  Octave_object retval;
  send_to_plot_stream ("clear\n");

  // XXX FIXME XXX -- instead of just clearing these things, it would
  // be nice if we could reset things to a user-specified default
  // state.

  send_to_plot_stream ("set title\n");
  send_to_plot_stream ("set xlabel\n");
  send_to_plot_stream ("set ylabel\n");
  send_to_plot_stream ("set nogrid\n");
  send_to_plot_stream ("set nolabel\n");

  // This makes a simple `replot' not work after a `clearplot' command
  // has been issued.

  plot_line_count = 0;

  return retval;
}

DEFALIAS (clg, clearplot);

DEFUN ("closeplot", Fcloseplot, Scloseplot, 00,
  "closeplot (): close the stream to plotter")
{
  Octave_object retval;
  close_plot_stream ();
  return retval;
}

DEFUN_TEXT ("hold", Fhold, Shold, 10,
  "hold [on|off]\n\
\n\
determine whether the plot window is cleared before the next line is\n\
drawn.  With no argument, toggle the current state.") 
{
  Octave_object retval;

  int argc = args.length () + 1;

  string_vector argv = make_argv (args, "hold");

  if (error_state)
    return retval;

  switch (argc)
    {
    case 1:
      clear_before_plotting = ! clear_before_plotting;
      break;

    case 2:
      if (argv[1] == "on")
	clear_before_plotting = false;
      else if (argv[1] == "off")
	clear_before_plotting = true;
      else
	print_usage ("hold");
      break;

    default:
      print_usage ("hold");
      break;
    }

  return retval;
}

DEFUN ("ishold", Fishold, Sishold, 00,
  "ishold\n\
\n\
Return 1 if hold is on, otherwise return 0.")
{
  return (double) (! clear_before_plotting);
}

DEFUN ("purge_tmp_files", Fpurge_tmp_files, Spurge_tmp_files, 00,
  "delete temporary data files used for plotting")
{
  Octave_object retval;
  cleanup_tmp_files ();
  return retval;
}

DEFUN_TEXT ("set", Fset, Sset, 10,
  "set [options]\n\
\n\
set plotting options")
{
  Octave_object retval;

  int argc = args.length () + 1;

  string_vector argv = make_argv (args, "set");

  if (error_state)
    return retval;

  ostrstream plot_buf;

  if (argc > 1)
    {
      if (almost_match ("parametric", argv[1], 3))
	parametric_plot = true;
      else if (almost_match ("noparametric", argv[1], 5))
	parametric_plot = false;
      else if (almost_match ("term", argv[1], 1))
	{
	  delete [] gnuplot_terminal_type;
	  ostrstream buf;
	  for (int i = 2; i < argc; i++)
	    buf << argv[i] << " ";
	  buf << "\n" << ends;
	  gnuplot_terminal_type = buf.str ();
	}
    }

  for (int i = 0; i < argc; i++)
    plot_buf << argv[i] << " ";

  plot_buf << "\n" << ends;

  char *plot_command = plot_buf.str ();
  send_to_plot_stream (plot_command);

  delete [] plot_command;

  return retval;
}

DEFUN_TEXT ("show", Fshow, Sshow, 10,
  "show [options]\n\
\n\
show plotting options")
{
  Octave_object retval;

  int argc = args.length () + 1;

  string_vector argv = make_argv (args, "show");

  if (error_state)
    return retval;

  ostrstream plot_buf;

  for (int i = 0; i < argc; i++)
    plot_buf << argv[i] << " ";

  plot_buf << "\n" << ends;

  char *plot_command = plot_buf.str ();
  send_to_plot_stream (plot_command);

  delete [] plot_command;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
