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
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "SLStack.h"
#include "procstream.h"

#include "file-ops.h"
#include "lo-mappers.h"
#include "str-vec.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "load-save.h"
#include "oct-obj.h"
#include "pt-cmd.h"
#include "pt-exp.h"
#include "pt-plot.h"
#include "pt-walk.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "utils.h"
#include "variables.h"

// If TRUE, a replot command is issued automatically each time a plot
// changes in some way.
static bool Vautomatic_replot;

// The name of the shell command to execute to start gnuplot.
static string Vgnuplot_binary;

// TRUE if gnuplot appears to support multiple plot windows with X11.
static bool Vgnuplot_has_frames;

// TRUE if gnuplot appears to support multiplot.
static bool Vgnuplot_has_multiplot;

// The number of lines we've plotted so far.
static int plot_line_count = 0;

// Is this a parametric plot?  Makes a difference for 3D plotting.
static bool parametric_plot = false;

// The gnuplot terminal type.
static char *gnuplot_terminal_type = 0;

// Should the graph window be cleared before plotting the next line?
static bool clear_before_plotting = true;

// List of files to delete when we exit or crash.
//
// XXX FIXME XXX -- this should really be static, but that causes
// problems on some systems.
SLStack <string> tmp_files;

// Pipe to gnuplot.
static oprocstream *plot_stream = 0;

// ID of the plotter process.
static pid_t plot_stream_pid = 0;

// Use shortest possible abbreviations to minimize trouble caused by
// gnuplot's fixed-length command line buffer.

#ifndef GPLOT_CMD_PLOT  
#define GPLOT_CMD_PLOT   "pl"
#endif

#ifndef GPLOT_CMD_REPLOT 
#define GPLOT_CMD_REPLOT "cle;rep"
#endif

#ifndef GPLOT_CMD_SPLOT 
#define GPLOT_CMD_SPLOT  "sp"
#endif

#ifndef GPLOT_CMD_USING
#define GPLOT_CMD_USING  "u"
#endif

#ifndef GPLOT_CMD_WITH 
#define GPLOT_CMD_WITH   "w"
#endif

#ifndef GPLOT_CMD_TITLE
#define GPLOT_CMD_TITLE  "t"
#endif

#ifndef GPLOT_CMD_END
#define GPLOT_CMD_END "\n"
#endif

static void
plot_stream_death_handler (pid_t pid, int)
{
  close_plot_stream ();

  warning ("connection to external plotter (pid = %d) lost --", pid);
  warning ("please try your plot command(s) again");
}

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

      string plot_prog = Vgnuplot_binary;

      if (plot_prog.empty ())
	plot_prog = "gnuplot";

      // XXX FIXME XXX -- I'm not sure this is the right thing to do,
      // but without it, C-c at the octave prompt will kill gnuplot...

#if defined (HAVE_POSIX_SIGNALS)
      sigset_t set, oset;
      sigemptyset (&set);
      sigaddset (&set, SIGINT);
      sigprocmask (SIG_BLOCK, &set, &oset);
#else
     volatile octave_interrupt_handler old_interrupt_handler
	= octave_ignore_interrupts ();
#endif

      plot_stream = new oprocstream (plot_prog.c_str ());

      if (plot_stream)
	{
	  if (! *plot_stream)
	    {
	      delete plot_stream;
	      plot_stream = 0;

	      error ("plot: unable to open pipe to `%s'", plot_prog.c_str ());
	    }
	  else
	    {
	      plot_stream_pid = plot_stream->pid ();
    	      octave_child_list::insert (plot_stream_pid,
					 plot_stream_death_handler);
	    }
	}
      else
	error ("plot: unable to open pipe to `%s'", plot_prog.c_str ());

#if defined (HAVE_POSIX_SIGNALS)
      sigprocmask (SIG_SETMASK, &oset, 0);
#else
      octave_set_interrupt_handler (old_interrupt_handler);
#endif
    }

  if (! error_state && plot_stream && *plot_stream && ! initialized)
    {
      initialized = true;
      *plot_stream << "set data style lines\n";

      if (gnuplot_terminal_type)
	*plot_stream << "set term " << gnuplot_terminal_type << GPLOT_CMD_END;
    }
}

static int
send_to_plot_stream (const char *cmd)
{
  if (! (plot_stream && *plot_stream))
    {
      open_plot_stream ();

      if (error_state)
	return -1;
    }

  int replot_len = strlen (GPLOT_CMD_REPLOT);
  int splot_len = strlen (GPLOT_CMD_SPLOT);
  int plot_len = strlen (GPLOT_CMD_PLOT);

  bool is_replot = (strncmp (cmd, GPLOT_CMD_REPLOT, replot_len) == 0);
  bool is_splot = (strncmp (cmd, GPLOT_CMD_SPLOT, splot_len) == 0);
  bool is_plot = (strncmp (cmd, GPLOT_CMD_PLOT, plot_len) == 0);

  if (plot_line_count == 0 && is_replot)
    error ("replot: no previous plot");
  else
    {
      *plot_stream << cmd;

      if (! (is_replot || is_splot || is_plot)
	  && plot_line_count > 0
	  && Vautomatic_replot)
	*plot_stream << GPLOT_CMD_REPLOT << GPLOT_CMD_END;

      plot_stream->flush ();
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
	    plot_buf << GPLOT_CMD_PLOT;
	  else
	    {
	      ::error ("replot: must have something to plot");
	      return;
	    }
	}
      else
	plot_buf << GPLOT_CMD_REPLOT;
      break;

    case 2:
      if (clear_before_plotting || plot_line_count == 0)
	{
	  plot_line_count = 0;
	  plot_buf << GPLOT_CMD_PLOT;
	}
      else
	plot_buf << GPLOT_CMD_REPLOT;
      break;

    case 3:
      if (clear_before_plotting || plot_line_count == 0)
	{
	  plot_line_count = 0;
	  plot_buf << GPLOT_CMD_SPLOT;
	}
      else
	plot_buf << GPLOT_CMD_REPLOT;
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

  plot_buf << GPLOT_CMD_END << ends;

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
tree_plot_command::accept (tree_walker& tw)
{
  tw.visit_plot_command (*this);
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
plot_limits::accept (tree_walker& tw)
{
  tw.visit_plot_limits (*this);
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
      octave_value lower_val = lower->rvalue ();

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
      octave_value upper_val = upper->rvalue ();

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
plot_range::accept (tree_walker& tw)
{
  tw.visit_plot_range (*this);
}

subplot_using::~subplot_using (void)
{
  delete scanf_fmt;
}

int
subplot_using::eval (int ndim, int n_max)
{
  if ((ndim == 2 && qual_count > 4)
      || (ndim == 3 && qual_count > 3))
    return -1;

  if (qual_count > 0)
    val.resize (qual_count);

  for (int i = 0; i < qual_count; i++)
    {
      if (x[i])
	{
	  octave_value tmp = x[i]->rvalue ();

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
		val (i) = n;
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

  for (int i = 0; i < qual_count; i++)
    {
      if (i == 0)
	plot_buf << " " << GPLOT_CMD_USING << " ";
      else
	plot_buf << ":";

      plot_buf << val (i);
    }

  return 0;
}

void
subplot_using::accept (tree_walker& tw)
{
  tw.visit_subplot_using (*this);
}

subplot_style::~subplot_style (void)
{
  delete sp_linetype;
  delete sp_pointtype;
}

int
subplot_style::print (ostrstream& plot_buf)
{
  if (! sp_style.empty ())
    {
      plot_buf << " " << GPLOT_CMD_WITH << " " << sp_style;

      if (sp_linetype)
	{
	  octave_value tmp = sp_linetype->rvalue ();

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

      if (sp_pointtype)
	{
	  octave_value tmp = sp_pointtype->rvalue ();

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

bool
subplot_style::columns_ok (int nc)
{
  bool retval = true;

  if ((almost_match ("boxes", sp_style, 5, 0) && nc != 3)
      || (almost_match ("boxerrorbars", sp_style, 5, 0)
	  && (! (nc == 3 || nc == 4 || nc == 5)))
      || ((almost_match ("boxxyerrorbars", sp_style, 4, 0)
	   || almost_match ("xyerrorbars", sp_style, 2, 0))
	  && (! (nc == 4 || nc == 6 || nc == 7)))
      || ((almost_match ("candlesticks", sp_style, 1, 0)
	   || almost_match ("financebars", sp_style, 2, 0))
	  && nc != 5)
      || ((almost_match ("errorbars", sp_style, 1, 0)
	   || almost_match ("xerrorbars", sp_style, 1, 0)
	   || almost_match ("yerrorbars", sp_style, 1, 0))
	  && (! (nc == 3 || nc == 4))))
    {
      error
	("invalid number of data columns = %d specified for plot style `%s'",
	 nc, sp_style.c_str ());

      retval = false;
    }

  return retval;
}

void
subplot_style::accept (tree_walker& tw)
{
  tw.visit_subplot_style (*this);
}

subplot::~subplot (void)
{
  delete sp_plot_data;
  delete sp_using_clause;
  delete sp_title_clause;
  delete sp_style_clause;
}

octave_value
subplot::extract_plot_data (int ndim, octave_value& data)
{
  octave_value retval;

  if (sp_using_clause)
    {
      ColumnVector val = sp_using_clause->values (ndim);

      octave_value_list args;

      args(1) = val;
      args(0) = octave_value::magic_colon_t;

      retval = data.do_index_op (args);

      if (error_state)
	return octave_value ();
    }
  else
    {
      retval = data;
    }

  int nc = retval.columns ();

  if (ndim == 2 && sp_style_clause && ! sp_style_clause->columns_ok (nc))
    return octave_value ();

  return retval;
}

int
subplot::handle_plot_data (int ndim, ostrstream& plot_buf)
{
  if (sp_plot_data)
    {
      octave_value data = sp_plot_data->rvalue ();

      if (! error_state && data.is_defined ())
	{
	  string file;

	  if (data.is_string ())
	    {
	      // Should really try to look at data file to determine
	      // n_max.  Can't do much about other arbitrary gnuplot
	      // commands though...

	      int n_max = 0;

	      file = file_ops::tilde_expand (data.string_value ());

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

	      if (sp_using_clause)
		{
		  int status = sp_using_clause->print (ndim, n_max, plot_buf);

		  if (status < 0)
		    return -1;
		}
	    }
	  else
	    {
	      // Eliminate the need for printing a using clause to
	      // plot_buf.

	      octave_value tmp_data = extract_plot_data (ndim, data);

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

  if (sp_title_clause)
    {
      octave_value tmp = sp_title_clause->rvalue ();

      if (! error_state && tmp.is_string ())
	plot_buf << " " << GPLOT_CMD_TITLE << " "
	  << '"' << tmp.string_value () << '"';
      else
	{
	  warning ("line title must be a string");
	  plot_buf << " " << GPLOT_CMD_TITLE << " "
	    << '"' << "line " << plot_line_count << '"';
	}
    }
  else
    plot_buf << " " << GPLOT_CMD_TITLE << " "
      << '"' << "line " << plot_line_count << '"';

  if (sp_style_clause)
    {
      int status = sp_style_clause->print (plot_buf);
      if (status < 0)
	return -1;
    }

  return 0;
}

void
subplot::accept (tree_walker& tw)
{
  tw.visit_subplot (*this);
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
subplot_list::accept (tree_walker& tw)
{
  tw.visit_subplot_list (*this);
}

string
save_in_tmp_file (octave_value& t, int ndim, bool parametric)
{
  string name = file_ops::tempnam ("", "oct-");

  if (! name.empty ())
    {
      ofstream file (name.c_str ());

      if (file)
	{
	  switch (ndim)
	    {
	    case 2:
	      save_ascii_data (file, t, name, true);
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
  octave_child_list::remove (plot_stream_pid);

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
      plot_buf << "cd \"" << newdir << "\"" GPLOT_CMD_END << ends;
      char *message = plot_buf.str ();
      send_to_plot_stream (message);
      delete [] message;
    }
}

DEFUN (clearplot, , ,
  "clearplot (): clear the plot window")
{
  octave_value_list retval;
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

DEFUN (closeplot, , ,
  "closeplot (): close the stream to plotter")
{
  octave_value_list retval;
  close_plot_stream ();
  return retval;
}

DEFUN_TEXT (hold, args, ,
  "hold [on|off]\n\
\n\
determine whether the plot window is cleared before the next line is\n\
drawn.  With no argument, toggle the current state.") 
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("hold");

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

DEFUN (ishold, , ,
  "ishold\n\
\n\
Return 1 if hold is on, otherwise return 0.")
{
  return static_cast<double> (! clear_before_plotting);
}

DEFUN (purge_tmp_files, , ,
  "delete temporary data files used for plotting")
{
  octave_value_list retval;
  cleanup_tmp_files ();
  return retval;
}

DEFUN_TEXT (gset, args, ,
  "gset [options]\n\
\n\
set plotting options for gnuplot")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("set");

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
	  buf << GPLOT_CMD_END << ends;
	  gnuplot_terminal_type = buf.str ();
	}
    }

  for (int i = 0; i < argc; i++)
    plot_buf << argv[i] << " ";

  plot_buf << GPLOT_CMD_END << ends;

  char *plot_command = plot_buf.str ();
  send_to_plot_stream (plot_command);

  delete [] plot_command;

  return retval;
}

DEFUN_TEXT (set, args, nargout,
  "This command is has been replaced by `gset'.")
{
  warning ("set is obsolete -- use gset instead");
  return Fgset (args, nargout);
}

DEFUN_TEXT (gshow, args, ,
  "gshow [options]\n\
\n\
show plotting options")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("show");

  if (error_state)
    return retval;

  ostrstream plot_buf;

  for (int i = 0; i < argc; i++)
    plot_buf << argv[i] << " ";

  plot_buf << GPLOT_CMD_END << ends;

  char *plot_command = plot_buf.str ();
  send_to_plot_stream (plot_command);

  delete [] plot_command;

  return retval;
}

DEFUN_TEXT (show, args, nargout,
  "This command is has been replaced by `gshow'.")
{
  warning ("show is obsolete -- use gshow instead");
  return Fgshow (args, nargout);
}

static int
automatic_replot (void)
{
  Vautomatic_replot = check_preference ("automatic_replot");

  return 0;
}

int
gnuplot_binary (void)
{
  int status = 0;

  string s = builtin_string_variable ("gnuplot_binary");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("gnuplot_binary");
      status = -1;
    }
  else
    Vgnuplot_binary = s;

  return status;
}

static int
gnuplot_has_frames (void)
{
  Vgnuplot_has_frames = check_preference ("gnuplot_has_frames");

  return 0;
}

static int
gnuplot_has_multiplot (void)
{
  Vgnuplot_has_multiplot = check_preference ("gnuplot_has_multiplot");

  return 0;
}

void
symbols_of_pt_plot (void)
{
  DEFVAR (automatic_replot, 0.0, 0, automatic_replot,
    "if true, auto-insert a replot command when a plot changes");

  DEFVAR (gnuplot_binary, "gnuplot", 0, gnuplot_binary,
    "path to gnuplot binary");

#ifdef GNUPLOT_HAS_FRAMES
  double with_frames = 1.0;
#else
  double with_frames = 0.0;
#endif

  DEFVAR (gnuplot_has_frames, with_frames, 0, gnuplot_has_frames,
    "true if gnuplot supports multiple plot windows on X11, false otherwise");

#ifdef GNUPLOT_HAS_MULTIPLOT
  double with_multiplot = 1.0;
#else
  double with_multiplot = 0.0;
#endif

  DEFVAR (gnuplot_has_multiplot, with_multiplot, 0, gnuplot_has_multiplot,
    "true if gnuplot supports multiplot, false otherwise");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
