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

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstring>

#include <fstream>
#include <iostream>
#include <string>
#include <stack>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

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
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// If TRUE, a replot command is issued automatically each time a plot
// changes in some way.
static bool Vautomatic_replot;

// The name of the shell command to execute to start gnuplot.
static std::string Vgnuplot_binary;

// TRUE if gnuplot appears to support multiple plot windows with X11.
static bool Vgnuplot_has_frames;

// The number of lines we've plotted so far.
static int plot_line_count = 0;

// Is this a parametric plot?  Makes a difference for 3D plotting.
static bool parametric_plot = false;

// The gnuplot terminal type.
static std::string gnuplot_terminal_type;

// Should the graph window be cleared before plotting the next line?
static bool clear_before_plotting = true;

// List of files to delete when we exit or crash.
//
// XXX FIXME XXX -- this should really be static, but that causes
// problems on some systems.
std::stack <std::string> tmp_files;

// Pipe to gnuplot.
static oprocstream *plot_stream = 0;

// ID of the plotter process.
static pid_t plot_stream_pid = 0;

// Gnuplot command strings that we use.
static std::string Vgnuplot_command_plot;
static std::string Vgnuplot_command_replot;
static std::string Vgnuplot_command_splot;
static std::string Vgnuplot_command_using;
static std::string Vgnuplot_command_with;
static std::string Vgnuplot_command_axes;
static std::string Vgnuplot_command_title;
static std::string Vgnuplot_command_end;

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

      std::string plot_prog;

      if (Vgnuplot_binary.empty ())
	plot_prog = "gnuplot";
      else
        plot_prog = "\"" + Vgnuplot_binary + "\"";

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

      if (! gnuplot_terminal_type.empty ())
	*plot_stream << "set term " << gnuplot_terminal_type
		     << Vgnuplot_command_end; 
    }
}

static int
send_to_plot_stream (const std::string& cmd)
{
  if (! (plot_stream && *plot_stream))
    {
      open_plot_stream ();

      if (error_state)
	return -1;
    }

  int replot_len = Vgnuplot_command_replot.length ();
  int splot_len = Vgnuplot_command_splot.length ();
  int plot_len = Vgnuplot_command_plot.length ();

  bool is_replot = (Vgnuplot_command_replot == cmd.substr (0, replot_len));
  bool is_splot = (Vgnuplot_command_splot == cmd.substr (0, splot_len));
  bool is_plot = (Vgnuplot_command_plot == cmd.substr (0, plot_len));

  if (plot_line_count == 0 && is_replot)
    error ("replot: no previous plot");
  else
    {
      *plot_stream << cmd;

      if (! (is_replot || is_splot || is_plot)
	  && plot_line_count > 0
	  && Vautomatic_replot)
	*plot_stream << Vgnuplot_command_replot << Vgnuplot_command_end;

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

  OSSTREAM plot_buf;

  switch (ndim)
    {
    case 1:
      if (plot_line_count == 0)
	{
	  if (plot_list)
	    plot_buf << Vgnuplot_command_plot;
	  else
	    {
	      ::error ("replot: must have something to plot");
	      return;
	    }
	}
      else
	plot_buf << Vgnuplot_command_replot;
      break;

    case 2:
      if (clear_before_plotting || plot_line_count == 0)
	{
	  plot_line_count = 0;
	  plot_buf << Vgnuplot_command_plot;
	}
      else
	plot_buf << Vgnuplot_command_replot;
      break;

    case 3:
      if (clear_before_plotting || plot_line_count == 0)
	{
	  plot_line_count = 0;
	  plot_buf << Vgnuplot_command_splot;
	}
      else
	plot_buf << Vgnuplot_command_replot;
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

  plot_buf << Vgnuplot_command_end << OSSTREAM_ENDS;

  // Just testing...
  //  char *message = plot_buf.str ();
  //  std::cout << "[*]" << message << "[*]\n";

  std::string message = OSSTREAM_STR (plot_buf);

  if (parametric_plot && ndim == 2)
    {
      warning ("can't make 2D parametric plot -- setting noparametric...");
      send_to_plot_stream ("set noparametric\n");
      send_to_plot_stream (message);
      send_to_plot_stream ("set parametric\n");
    }
  else
    send_to_plot_stream (message);

  OSSTREAM_FREEZE (plot_buf);
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
plot_limits::print (int ndim, OSSTREAM& plot_buf)
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
plot_range::print (OSSTREAM& plot_buf)
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

  // XXX FIXME XXX -- is the following really right?
  if (status < 0)
    return ColumnVector (1, -1.0);

  return val;
}

int
subplot_using::print (int ndim, int n_max, OSSTREAM& plot_buf)
{
  int status = eval (ndim, n_max);

  if (status < 0)
    return -1;

  for (int i = 0; i < qual_count; i++)
    {
      if (i == 0)
	plot_buf << " " << Vgnuplot_command_using << " ";
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
subplot_style::print (OSSTREAM& plot_buf)
{
  if (! sp_style.empty ())
    {
      plot_buf << " " << Vgnuplot_command_with << " " << sp_style;

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

  if ((almost_match ("boxes", sp_style, 5, 0)
       && (! (nc == 2 || nc == 3)))
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

int
subplot_axes::print (OSSTREAM& plot_buf)
{
  if (! sp_axes.empty ())
    plot_buf << " " << Vgnuplot_command_axes << " " << sp_axes;

  return 0;
}

void
subplot_axes::accept (tree_walker& tw)
{
  tw.visit_subplot_axes (*this);
}

subplot::~subplot (void)
{
  delete sp_plot_data;
  delete sp_using_clause;
  delete sp_title_clause;
  delete sp_style_clause;
  delete sp_axes_clause;
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

      retval = data.single_subsref ("(", args);

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
subplot::handle_plot_data (int ndim, OSSTREAM& plot_buf)
{
  if (sp_plot_data)
    {
      octave_value data = sp_plot_data->rvalue ();

      if (! error_state && data.is_defined ())
	{
	  std::string file;

	  if (data.is_string ())
	    {
	      // Should really try to look at data file to determine
	      // n_max.  Can't do much about other arbitrary gnuplot
	      // commands though...

	      int n_max = 0;

	      file = file_ops::tilde_expand (data.string_value ());

	      std::ifstream ftmp (file.c_str ());

	      if (ftmp)
		{
		  plot_buf << " '" << file << "'";
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
		      plot_buf << " '" << file << "'";
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
subplot::print (int ndim, OSSTREAM& plot_buf)
{
  int status = handle_plot_data (ndim, plot_buf);

  if (status < 0)
    return -1;

  if (sp_axes_clause)
    {
      status = sp_axes_clause->print (plot_buf);

      if (status < 0)
	return -1;
    }

  if (sp_title_clause)
    {
      octave_value tmp = sp_title_clause->rvalue ();

      if (! error_state && tmp.is_string ())
	plot_buf << " " << Vgnuplot_command_title << " "
	  << '"' << tmp.string_value () << '"';
      else
	{
	  warning ("line title must be a string");
	  plot_buf << " " << Vgnuplot_command_title << " "
	    << '"' << "line " << plot_line_count << '"';
	}
    }
  else
    plot_buf << " " << Vgnuplot_command_title << " "
      << '"' << "line " << plot_line_count << '"';

  if (sp_style_clause)
    {
      status = sp_style_clause->print (plot_buf);

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

int
subplot_list::print (int ndim, OSSTREAM& plot_buf)
{
  int status = 0;

  for (iterator p = begin (); p != end (); p++)
    {
      subplot *elt = *p;

      plot_line_count++;

      if (p != begin ())
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

std::string
save_in_tmp_file (const octave_value& t, int ndim, bool parametric)
{
  std::string name = file_ops::tempnam ("", "oct-");

  if (! name.empty ())
    {
      std::ofstream file (name.c_str ());

      if (file)
	{
	  switch (ndim)
	    {
	    case 2:
	      save_ascii_data_for_plotting (file, t, name);
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
mark_for_deletion (const std::string& file)
{
  tmp_files.push (file);
}

void
cleanup_tmp_files (void)
{
  while (! tmp_files.empty ())
    {
      std::string filename = tmp_files.top ();
      tmp_files.pop ();
      unlink (filename.c_str ());
    }
}

void
close_plot_stream (void)
{
  octave_child_list::remove (plot_stream_pid);

  if (plot_stream)
    {
      send_to_plot_stream ("\nquit\n");
      delete plot_stream;
      plot_stream = 0;
    }

  plot_line_count = 0;
}

void
do_external_plotter_cd (const std::string& newdir)
{
  if (plot_stream && *plot_stream)
    {
      OSSTREAM plot_buf;
      plot_buf << "cd '" << newdir << "'" << Vgnuplot_command_end
	       << OSSTREAM_ENDS;
      send_to_plot_stream (OSSTREAM_STR (plot_buf));
      OSSTREAM_FREEZE (plot_buf);
    }
}

DEFUN (clearplot, , ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} clearplot\n\
@deftypefnx {Built-in Function} {} clg\n\
Clear the plot window and any titles or axis labels.  The name\n\
@code{clg} is aliased to @code{clearplot} for compatibility with\n\
@sc{Matlab}.\n\
\n\
The commands @kbd{gplot clear}, @kbd{gsplot clear}, and @kbd{replot\n\
clear} are equivalent to @code{clearplot}.  (Previously, commands like\n\
@kbd{gplot clear} would evaluate @code{clear} as an ordinary expression\n\
and clear all the visible variables.)\n\
@end deftypefn")
{
  octave_value_list retval;

  // We are clearing the plot window, so there is no need to redisplay
  // after each incremental change to the title, labels, etc.

  unwind_protect_bool (Vautomatic_replot);

  Vautomatic_replot = false;

  // XXX FIXME XXX -- instead of just clearing these things, it would
  // be nice if we could reset things to a user-specified default
  // state.

  send_to_plot_stream ("set title\n");
  send_to_plot_stream ("set xlabel\n");
  send_to_plot_stream ("set ylabel\n");
  send_to_plot_stream ("set nogrid\n");
  send_to_plot_stream ("set nolabel\n");

  // Clear the plot display last.

  send_to_plot_stream ("clear\n");

  // Setting plot_line_count to zero makes a simple `replot' not work
  // after a `clearplot' command has been issued.

  plot_line_count = 0;

  unwind_protect::run ();

  return retval;
}

DEFALIAS (clg, clearplot);

DEFUN (closeplot, , ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} closeplot\n\
Close stream to the @code{gnuplot} subprocess.  If you are using X11,\n\
this will close the plot window.\n\
@end deftypefn")
{
  octave_value_list retval;
  close_plot_stream ();
  return retval;
}

DEFCMD (hold, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} hold @var{args}\n\
Tell Octave to `hold' the current data on the plot when executing\n\
subsequent plotting commands.  This allows you to execute a series of\n\
plot commands and have all the lines end up on the same figure.  The\n\
default is for each new plot command to clear the plot device first.\n\
For example, the command\n\
\n\
@example\n\
hold on\n\
@end example\n\
\n\
@noindent\n\
turns the hold state on.  An argument of @code{off} turns the hold state\n\
off, and @code{hold} with no arguments toggles the current hold state.\n\
@end deftypefn")
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
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ishold\n\
Return 1 if the next line will be added to the current plot, or 0 if\n\
the plot device will be cleared before drawing the next line.\n\
@end deftypefn")
{
  return octave_value (! clear_before_plotting);
}

DEFUN (purge_tmp_files, , ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} purge_tmp_files\n\
Delete the temporary files created by the plotting commands.\n\
\n\
Octave creates temporary data files for @code{gnuplot} and then sends\n\
commands to @code{gnuplot} through a pipe.  Octave will delete the\n\
temporary files on exit, but if you are doing a lot of plotting you may\n\
want to clean up in the middle of a session.\n\
\n\
A future version of Octave will eliminate the need to use temporary\n\
files to hold the plot data.\n\
@end deftypefn")
{
  octave_value_list retval;
  cleanup_tmp_files ();
  return retval;
}


DEFUN (graw, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} graw (@var{string})\n\
Send @var{string} directly to gnuplot subprocess.\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 1 && args(0).is_string ())
    {
      std::string cmd = args(0).string_value ();

      if (! (plot_stream && *plot_stream))
	open_plot_stream ();

      if (! error_state)
	{
	  *plot_stream << cmd;

	  plot_stream->flush ();
	}
    }
  else
    print_usage ("graw");

  return retval;
}

DEFCMD (gset, args, ,
  "-*- texinfo -*-\n\
@deffn {Command} gset options\n\
Set plotting options for gnuplot\n\
@end deffn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("set");

  if (error_state)
    return retval;

  OSSTREAM plot_buf;

  if (argc > 1)
    {
      if (almost_match ("parametric", argv[1], 3))
	parametric_plot = true;
      else if (almost_match ("noparametric", argv[1], 5))
	parametric_plot = false;
      else if (almost_match ("term", argv[1], 1))
	{
	  gnuplot_terminal_type = "";
	  OSSTREAM buf;
	  int i;
	  for (i = 2; i < argc-1; i++)
	    buf << argv[i] << " ";
	  if (i < argc)
	    buf << argv[i];
	  buf << Vgnuplot_command_end << OSSTREAM_ENDS;
	  gnuplot_terminal_type = OSSTREAM_STR (buf);
	  OSSTREAM_FREEZE (buf);
	}
    }

  int i;
  for (i = 0; i < argc-1; i++)
    plot_buf << argv[i] << " ";

  if (i < argc)
    plot_buf << argv[i];

  plot_buf << Vgnuplot_command_end << OSSTREAM_ENDS;

  send_to_plot_stream (OSSTREAM_STR (plot_buf));

  OSSTREAM_FREEZE (plot_buf);

  return retval;
}

DEFCMD (set, args, nargout,
  "-*- texinfo -*-\n\
This command is has been replaced by @code{gset}.")
{
  warning ("set is obsolete -- use gset instead");
  return Fgset (args, nargout);
}

DEFCMD (gshow, args, ,
  "-*- texinfo -*-\n\
@deffn {Command} gshow options\n\
Show plotting options.\n\
@end deffn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("show");

  if (error_state)
    return retval;

  OSSTREAM plot_buf;

  int i;
  for (i = 0; i < argc-1; i++)
    plot_buf << argv[i] << " ";
  if (i < argc)
    plot_buf << argv[i];

  plot_buf << Vgnuplot_command_end << OSSTREAM_ENDS;

  send_to_plot_stream (OSSTREAM_STR (plot_buf));

  OSSTREAM_FREEZE (plot_buf);

  return retval;
}

DEFCMD (show, args, nargout,
  "-*- texinfo -*-\n\
This command is has been replaced by @code{gshow}.")
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

static int
set_string_var (std::string& var, const char *nm)
{
  int retval = 0;

  std::string s = builtin_string_variable (nm);

  if (s.empty ())
    {
      gripe_invalid_value_specified (nm);
      retval = -1;
    }
  else
    var = s;

  return retval;
}

static int
gnuplot_binary (void)
{
  return set_string_var (Vgnuplot_binary, "gnuplot_binary");
}

static int
gnuplot_command_plot (void)
{
  return set_string_var (Vgnuplot_command_plot, "gnuplot_command_plot");
}

static int
gnuplot_command_replot (void)
{
  return set_string_var (Vgnuplot_command_replot, "gnuplot_command_replot");
}

static int
gnuplot_command_splot (void)
{
  return set_string_var (Vgnuplot_command_splot, "gnuplot_command_splot");
}

static int
gnuplot_command_using (void)
{
  return set_string_var (Vgnuplot_command_using, "gnuplot_command_using");
}

static int
gnuplot_command_with (void)
{
  return set_string_var (Vgnuplot_command_with, "gnuplot_command_with");
}

static int
gnuplot_command_axes (void)
{
  return set_string_var (Vgnuplot_command_axes, "gnuplot_command_axes");
}

static int
gnuplot_command_title (void)
{
  return set_string_var (Vgnuplot_command_title, "gnuplot_command_title");
}

static int
gnuplot_command_end (void)
{
  return set_string_var (Vgnuplot_command_end, "gnuplot_command_end");
}

static int
gnuplot_has_frames (void)
{
  Vgnuplot_has_frames = check_preference ("gnuplot_has_frames");

  return 0;
}

void
symbols_of_pt_plot (void)
{
  DEFVAR (automatic_replot, true, automatic_replot,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} automatic_replot\n\
You can tell Octave to redisplay the plot each time anything about it\n\
changes by setting the value of the builtin variable\n\
@code{automatic_replot} to a nonzero value.  Although it is fairly\n\
inefficient, especially for large plots, the default value is 1 for\n\
compatibility with Matlab.\n\
@end defvr");

  DEFVAR (gnuplot_binary, GNUPLOT_BINARY, gnuplot_binary,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} gnuplot_binary\n\
The name of the program invoked by the plot command.  The default value\n\
is @code{\"gnuplot\"}.  @xref{Installation}.\n\
@end defvr");

  DEFVAR (gnuplot_command_plot, "pl", gnuplot_command_plot,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} gnuplot_command_plot\n\
@end defvr");

  DEFVAR (gnuplot_command_replot, "rep", gnuplot_command_replot,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} gnuplot_command_replot\n\
@end defvr");

  DEFVAR (gnuplot_command_splot, "sp", gnuplot_command_splot,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} gnuplot_command_splot\n\
@end defvr");

  DEFVAR (gnuplot_command_using, "u", gnuplot_command_using,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} gnuplot_command_using\n\
@end defvr");

  DEFVAR (gnuplot_command_with, "w", gnuplot_command_with,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} gnuplot_command_with\n\
@end defvr");

  DEFVAR (gnuplot_command_axes, "ax", gnuplot_command_axes,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} gnuplot_command_axes\n\
@end defvr");

  DEFVAR (gnuplot_command_title, "t", gnuplot_command_title,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} gnuplot_command_title\n\
@end defvr");

  DEFVAR (gnuplot_command_end, "\n", gnuplot_command_end,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} gnuplot_command_end\n\
@end defvr");

#if defined (GNUPLOT_HAS_FRAMES)
  bool with_frames = true;
#else
  bool with_frames = false;
#endif

  DEFVAR (gnuplot_has_frames, with_frames, gnuplot_has_frames,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} gnuplot_has_frames\n\
If the value of this variable is nonzero, Octave assumes that your copy\n\
of gnuplot has support for multiple frames that is included in recent\n\
3.6beta releases.  Its initial value is determined by configure, but it\n\
can be changed in your startup script or at the command line in case\n\
configure got it wrong, or if you upgrade your gnuplot installation.\n\
@end defvr");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
