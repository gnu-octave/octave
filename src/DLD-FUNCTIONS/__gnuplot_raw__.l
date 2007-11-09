/*

Copyright (C) 2004, 2005, 2006, 2007 John W. Eaton and Teemu Ikonen

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

%option prefix="gpt"
%option noyywrap

%{
// PKG_ADD: mark_as_rawcommand ("__gnuplot_plot__");
// PKG_ADD: mark_as_rawcommand ("__gnuplot_splot__");
// PKG_ADD: mark_as_rawcommand ("__gnuplot_replot__");

// PKG_ADD: mark_as_command ("__gnuplot_set__");
// PKG_ADD: mark_as_command ("__gnuplot_show__");

// PKG_ADD: atexit ("closeplot");

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <map>
#include <string>
#include <fstream>
#include <iostream>
#include <sstream>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "file-ops.h"
#include "oct-env.h"

#include "defun-dld.h"
#include "file-io.h"
#include "gripes.h"
#include "ls-oct-ascii.h"
#include "parse.h"
#include "procstream.h"
#include "sighandlers.h"
#include "utils.h"
#include "variables.h"

enum _toktype
  {
    START_PAREN = 1,
    END_PAREN,
    START_BRACE,
    END_BRACE,
    START_BRACKET,
    END_BRACKET,
    COLON,
    SEMICOLON,
    COMMA,
    QUOTE,
    DOLLAR,
    IDENT,
    NUMBER,
    BINOP,
    UNOP,
    STRING,
    OTHER,
    TITLE,
    USING,
    WITH,
    AXES,
    CLEAR
  };

typedef bool (*pred) (const int);

class
gpt_parse_error
{
public:
  gpt_parse_error (void) : msg () { }
  gpt_parse_error (std::string errmsg) : msg (errmsg) { }

  std::string msg;
};

static bool gpt_quote_is_transpose;
static bool gpt_allow_plotkw;
static int gpt_parens;
static int gpt_brackets;
static int gpt_braces;

static bool can_be_plotkw (void);

static int is_plot_keyword (const std::string& s);

static int handle_string (char delim);
static std::string strbuf;

%}

D	[0-9]
S	[ \t]
IDENT	([_a-zA-Z@][_a-zA-Z0-9]*)
EXPON	([DdEe][+-]?{D}+)
NUMBER  (({D}+\.?{D}*{EXPON}?)|(\.{D}+{EXPON}?)|(0[xX][0-9a-fA-F]+))
NOT	((\~)|(\!))
/* NOT is not strictly a binary operator, but is close enough for us. */
BINOP   (({NOT})|(\.?([\*/\\^+-]|\*\*)=?)|([<=~!>&|]=)|([=&|<>]{1,2})|(<<=)|(>>=)|(\.))
/* single quote (') transpose operator is handled separately. */
UNOP    ((\+\+)|(\-\-)|(\.'))

%%

"(" {
    gpt_quote_is_transpose = false;
    gpt_parens++;
    return START_PAREN;
    }

")" {
    gpt_quote_is_transpose = true;
    gpt_parens--;
    return END_PAREN;
    }

"{" {
    gpt_quote_is_transpose = false;
    gpt_braces++;
    return START_BRACE;
    }

"}" {
    gpt_quote_is_transpose = true;
    gpt_braces--;
    return END_BRACE;
    }

"[" {
    gpt_quote_is_transpose = false;
    gpt_brackets++;
    return START_BRACKET;
    }

"]" {
    gpt_quote_is_transpose = true;
    gpt_brackets--;
    return END_BRACKET;
    }

":" {
    gpt_quote_is_transpose = false;
    return COLON;
    }

";" {
    gpt_quote_is_transpose = false;
    return SEMICOLON;
    }

"," {
    gpt_quote_is_transpose = false;
    return COMMA;
    }

"'" {
    if (gpt_quote_is_transpose)
      {
        gpt_allow_plotkw = true;
        return QUOTE;
      }
    else
      {
        gpt_quote_is_transpose = true;
        gpt_allow_plotkw = true;
        return handle_string ('\'');
      }
    }

"$" {
    gpt_quote_is_transpose = false;
    return DOLLAR;
    }

"\"" {
    return handle_string ('"');
    }

{IDENT} {
    int itok = 0;
    if (can_be_plotkw () && (itok = is_plot_keyword (yytext)))
      {
        gpt_quote_is_transpose = false;
        gpt_allow_plotkw = true;
        return itok;
      }
    else if (std::string (yytext) == "function")
      throw gpt_parse_error ("function keyword not allowed in plot commands");
    else
      {
        gpt_quote_is_transpose = true;
        gpt_allow_plotkw = true;
        return IDENT;
      }
    }

{D}+/\.[\*/\\^'] | /* ' */
{NUMBER} {
    gpt_quote_is_transpose = true;
    gpt_allow_plotkw = true;
    return NUMBER;
    }

{BINOP} {
    gpt_quote_is_transpose = false;
    gpt_allow_plotkw = false;
    return BINOP;
    }

{UNOP} {
    gpt_quote_is_transpose = false;
    gpt_allow_plotkw = true;
    return UNOP;
    }

{S} { /* Ignore spaces and tabs outside of character strings. */ }

. {
    gpt_quote_is_transpose = false;
    gpt_allow_plotkw = true;
    warning ("unknown token = \"%s\" in plot command", yytext);
    return OTHER;
    }

%%

// ------------------------------------------------------------
// Interface to external gnuplot process(es), including gnuplot
// command parser.
// ------------------------------------------------------------

// The name of the shell command to execute to start gnuplot.
static std::string Vgnuplot_binary = GNUPLOT_BINARY;

// Append -title "Figure NN" to the gnuplot command?
static bool Vgnuplot_use_title_option = octave_env::have_x11_display ();

// Gnuplot command strings that we use.
static std::string Vgnuplot_command_plot = "pl";
static std::string Vgnuplot_command_replot = "rep";
static std::string Vgnuplot_command_splot = "sp";
static std::string Vgnuplot_command_using = "u";
static std::string Vgnuplot_command_with = "w";
static std::string Vgnuplot_command_axes = "ax";
static std::string Vgnuplot_command_title = "t";
static std::string Vgnuplot_command_end = "\n";

// Check if the parser state is such that a plot keyword can occur.

static bool
can_be_plotkw (void)
{
    return (gpt_allow_plotkw
	    && (gpt_braces == 0)
	    && (gpt_brackets == 0)
	    && (gpt_parens == 0));
}

// Check to see if a character string matches any one of the plot
// option keywords.  Don't match abbreviations for clear, since that's
// not a gnuplot keyword (users will probably only expect to be able
// to abbreviate actual gnuplot keywords).

static int
is_plot_keyword (const std::string& s)
{
  if (almost_match ("title", s, 1))
    return TITLE;
  else if (almost_match ("using", s, 1))
    return USING;
  else if (almost_match ("with", s, 1))
    return WITH;
  else if (almost_match ("axes", s, 2) || almost_match ("axis", s, 2))
    return AXES;
  else if ("clear" == s)
    return CLEAR;
  else
    return 0;
}

// This is used to handle character strings.  Kludge alert.

static int
handle_string (char delim)
{
  int c;
  bool escape_pending = false;

  strbuf = std::string (1, delim);

  while ((c = yyinput ()) != EOF)
    {
      if (c == '\\')
	{
	  if (escape_pending)
	    {
	      strbuf += static_cast<char> (c);
	      escape_pending = false;
	    }
	  else
	    {
	      strbuf += static_cast<char> (c);
	      escape_pending = true;
	    }
	  continue;
	}
      else if (c == '\n')
	{
	  error ("unterminated string constant");
	  break;
	}
      else if (c == delim)
	{
	  if (escape_pending)
	    strbuf += static_cast<char> (c);
	  else
	    {
	      c = yyinput ();

	      if (c == delim)
		{
		  strbuf += static_cast<char> (c);
		  strbuf += static_cast<char> (c);
		}
	      else
		{
		  yyunput (c, yytext);
		  strbuf += static_cast<char> (delim);
		  return STRING;
		}
	    }
	}
      else
	strbuf += static_cast<char> (c);

      escape_pending = false;
    }

  throw gpt_parse_error ("unterminated string");

  return 0;
}

// (Probably not necessesary, but current Matlab style plot functions
// break without this (they emit too short gnuplot commands))

static std::string
plot_style_token (const std::string& s)
{
  std::string retval;

  // FIXME -- specify minimum match length for these.
  static const char *plot_styles[] =
    {
      "boxes",
      "boxerrorbars",
      "boxxyerrorbars",
      "candlesticks",
      "dots",
      "errorbars",
      "financebars",
      "fsteps",
      "histeps",
      "impulses",
      "lines",
      "linespoints",
      "points",
      "steps",
      "vector",
      "xerrorbars",
      "xyerrorbars",
      "yerrorbars",
      0,
    };

  const char * const *tmp = plot_styles;
  while (*tmp)
    {
      if (almost_match (*tmp, s.c_str ()))
	{
	  retval = *tmp;
	  break;
	}

      tmp++;
    }

  return retval;
}

// Some predicates on tokens

// Return true for ":".

static bool
colonp (const int tok)
{
  return (tok == COLON);
}

// Return TRUE for "]".

static bool
endbracketp (const int tok)
{
  return (tok == END_BRACKET);
}

// Return TRUE for plot token, comma or end of input.

static bool
plottok_or_end_p (const int tok)
{
  return (tok == TITLE
	  || tok == USING
	  || tok == WITH
	  || tok == AXES
	  || tok == CLEAR
	  || tok == COMMA
	  || tok == 0);
}

// Equivalent to (colonp (tok) || plottok_or_end_p (tok)).

static bool
colon_plottok_or_end_p (const int tok)
{
  return (tok == COLON || plottok_or_end_p (tok));
}

// read until test is true and delimiters are balanced, or end of input.
// Return the last token in lasttok

static std::string
read_until (pred test, int& lasttok) throw (gpt_parse_error)
{
  int tok;

  // We have to maintain balanced delimiters per subexpression too.
  int brackets = 0;
  int parens = 0;
  int braces = 0;
  std::string s;

  tok = gptlex ();

  while (tok && ! (test (tok)
		   && brackets == 0
		   && parens == 0
		   && braces == 0))
    {
      switch (tok)
	{
	case START_BRACKET:
	  brackets++;
	  break;

	case END_BRACKET:
	  brackets--;
	  break;

	case START_PAREN:
	  parens++;
	  break;

	case END_PAREN:
	  parens--;
	  break;

	case START_BRACE:
	  braces++;
	  break;

	case END_BRACE:
	  braces--;
	  break;

	default:
	  break;
        }

      s += (tok == STRING ? strbuf : std::string (yytext)) + " ";

      tok = gptlex ();
    }

  // Throw error only if we've reached the end token and the test
  // doesn't accept it.

  if (! test (tok) && ! tok)
    throw gpt_parse_error ("unexpected end of input");

  lasttok = tok;

  return s;
}

// Eval the two expressions giving limits of range and print it.

static std::string
printrange (std::string starts, std::string ends)
{
  octave_value startv, endv;
  int status;
  std::string s;
  std::ostringstream range_buf;

  range_buf << "[";

  if (! starts.empty ())
    {
      startv = eval_string (starts, true, status);
      if (! startv.is_real_scalar ())
	throw gpt_parse_error ();
      startv.print_raw (range_buf);
    }

    range_buf << ":";

    if (! ends.empty ())
      {
	endv = eval_string (ends, true, status);
	if (! endv.is_real_scalar ())
	  throw gpt_parse_error ();
        endv.print_raw (range_buf);
      }

    range_buf << "]";

    s = range_buf.str ();

    return s;
}

// Handle plot parameters.

// Parse, evaluate and print colon separated expressions in the using
// plot parameter. The use of trailing format string is not supported.

static std::string
handle_using (int& lasttok)
{
  int tok;
  std::string expr_str;
  std::string retstr = Vgnuplot_command_using + " ";
  bool out = false;

  octave_value tmp_data;
  int status;
  while (! out)
    {
      expr_str = read_until (colon_plottok_or_end_p, tok);

      if (! expr_str.empty () &&  expr_str[0] == '(')
	retstr += expr_str;
      else
	{
	  tmp_data = eval_string (expr_str, true, status);
	  if (status != 0 || ! tmp_data.is_real_scalar ())
	    throw gpt_parse_error ();

	  std::ostringstream tmp_buf;
	  tmp_data.print_raw (tmp_buf);
	  retstr += tmp_buf.str ();
	}

      if (tok == COLON)
	retstr += ":";
      else
	out = true;
    }

  lasttok = tok;

  return retstr;
}

// Presently just passes the linewidth, pointtype etc. tokens as they are.

static std::string
handle_style (int& lasttok)
{
  std::string retstr = Vgnuplot_command_with + " ";
  std::string style;

  lasttok = gptlex ();

  if (lasttok != IDENT)
    throw gpt_parse_error ("expected plot style token");

  style = std::string (yytext);
  style = plot_style_token (style);

  if (! style.empty ())
    retstr += style;
  else
    retstr += std::string (yytext);

  // FIXME -- should evaluate the remaining tokens, but this
  // needs changes in the parser.
  retstr += " " + read_until (plottok_or_end_p, lasttok);

  return retstr;
}

// Axes has only one qualifier keyword, which is not evaluated.

static std::string
handle_axes (int& lasttok)
{
  return Vgnuplot_command_axes + " " + read_until (plottok_or_end_p, lasttok);
}

static void
write_data (std::ostream& os, const octave_value& val,
	    int ndim = 2, bool parametric = false,
	    const std::string& name = std::string ())
{
  switch (ndim)
    {
    case 2:
      save_ascii_data_for_plotting (os, val, name);
      break;

    case 3:
      save_three_d (os, val, parametric);
      break;

    default:
      gripe_2_or_3_dim_plot ();
      break;
    }
}

static void
write_inline_data (std::ostream& os, const octave_value& val,
		   int ndim = 2, bool parametric = false)
{
  write_data (os, val, ndim, parametric);
  os << "e" << std::endl;
}

static std::string
save_in_tmp_file (const octave_value& val, int ndim = 2,
		  bool parametric = false)
{
  std::string name = file_ops::tempnam ("", "oct-");

  if (! name.empty ())
    {
      std::ofstream file (name.c_str ());

      if (file)
	write_data (file, val, ndim, parametric, name);
      else
	{
	  error ("couldn't open temporary output file `%s'", name.c_str ());
	  name.resize (0);
	}
    }

  return name;
}

#define WARN_OBSOLETE(nm) \
  do \
    { \
      static bool warned = false; \
 \
      if (! warned) \
        { \
          warning ("%s: this function is obsolete and will be removed from a future version of Octave", nm); \
 \
          warned = true; \
        } \
    } \
  while (0)

DEFUN_DLD (__gnuplot_save_data__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __gnuplot_save_data__ (@var{data}, @var{ndim}, @var{parametric})\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin > 0 && nargin < 4)
    {
      WARN_OBSOLETE ("__gnuplot_save_data__");

      int ndim = 2;
      bool parametric = false;

      if (nargin > 1)
	{
	  ndim = args(1).int_value ();

	  if (! error_state)
	    {
	      if (nargin > 2)
		parametric = args(2).bool_value ();
	    }
	}

      if (! error_state)
	{
	  std::string file = save_in_tmp_file (args(0), ndim, parametric);

	  mark_for_deletion (file);

	  retval = file;
	}
    }
  else
    print_usage ();

  return retval;
}

static int
get_current_figure (void)
{
  int retval = 1;

  octave_value cf = get_global_value ("__current_figure__", true);

  if (cf.is_defined ())
    retval = cf.int_value ();
  else
    set_global_value ("__current_figure__", retval);

  return retval;
}

class
gnuplot
{
protected:

  gnuplot (void)
    : plot_line_count (0), parametric_plot (false), 
      use_title_option (Vgnuplot_use_title_option),
      gnuplot_exe (Vgnuplot_binary),
      gnuplot_terminal_type (), plot_stream () { }

public:

  ~gnuplot (void) { }

  static bool have_instance (void);

  static bool ensure_instance (void);

  static bool ensure_plot_stream (void);

  static void open (void);

  static void close (void);

  static void close_all (void);

  static bool plot_stream_event_handler (pid_t pid, int status);

  static void send (const std::string& cmd)
  {
    if (ensure_plot_stream ())
      instance->do_send (cmd);
  }

  static void send_raw (const std::string& cmd)
  {
    if (ensure_plot_stream ())
      instance->do_send_raw (cmd);
  }

  static void send_inline_data (const octave_value& val, int ndim,
				bool parametric)
  {
    if (ensure_plot_stream ())
      instance->do_send_inline_data (val, ndim, parametric);
  }

  static void clear (void)
  {
    if (ensure_plot_stream ())
      instance->do_clear ();
  }

  static void set (const string_vector& argv)
  {
    if (ensure_plot_stream ())
      instance->do_set (argv);
  }

  static void show (const string_vector& argv)
  {
    if (ensure_plot_stream ())
      instance->do_show (argv);
  }

  static void set_gnuplot_exe (const std::string& exe)
  {
    if (ensure_instance ())
      instance->do_set_gnuplot_exe (exe);
  }

  static void set_gnuplot_use_title_option (bool opt)
  {
    if (ensure_instance ())
      instance->do_set_gnuplot_use_title_option (opt);
  }

  // FIXME -- should only remove tmp files associated with
  // gnuplot?
  static void cleanup_tmp_files (void) { ::cleanup_tmp_files (); }

  static void plot (const string_vector& argv)
  {
    if (ensure_plot_stream ())
      instance->do_plot (argv);
  }

private:

  static gnuplot *instance;

  static std::map<int, gnuplot *> instance_map;

  // The number of lines we've plotted so far.
  int plot_line_count;

  // Is this a parametric plot?  Makes a difference for 3D plotting.
  bool parametric_plot;

  // Should we append '-title "TITLE"' to the gnuplot command?
  bool use_title_option;

  // The executable program to run.
  std::string gnuplot_exe;

  // The gnuplot terminal type.
  std::string gnuplot_terminal_type;

  // Pipe to gnuplot.
  oprocstream *plot_stream;

  pid_t pid (void) const;

  static gnuplot *lookup_by_pid (pid_t pid);

  void do_open (void);

  void do_close (void);

  void delete_plot_stream (void);

  void reset_plot_stream (void);

  void do_send (const std::string& cmd);

  void do_send_raw (const std::string& cmd);

  void do_send_inline_data (const octave_value& val, int ndim,
			    bool parametric);

  void do_clear (void);

  void do_set (const string_vector& argv);

  void do_show (const string_vector& argv);

  void do_set_gnuplot_exe (const std::string& exe) { gnuplot_exe = exe; }

  void do_set_gnuplot_use_title_option (bool opt) { use_title_option = opt; }

  void do_plot (const string_vector& argv);

  std::string
  makeplot (std::string caller, std::string args) throw (gpt_parse_error);

  std::string handle_title (int& lasttok);
};

gnuplot *gnuplot::instance = 0;

std::map<int, gnuplot *> gnuplot::instance_map;

bool
gnuplot::have_instance (void)
{
  int current_figure = get_current_figure ();

  if (instance_map.find (current_figure) != instance_map.end ())
    {
      instance = instance_map[current_figure];
      return true;
    }
  else
    return false;
}

bool
gnuplot::ensure_instance (void)
{
  if (! have_instance ())
    {
      instance = new gnuplot ();

      if (! instance)
	{
	  ::error ("unable to create gnuplot object!");

	  return false;
	}
      else
	instance_map[get_current_figure ()] = instance;
    }

  return true;
}

bool
gnuplot::ensure_plot_stream (void)
{
  if (ensure_instance ())
    {
      instance->do_open ();

      if (error_state)
	return false;
    }

  return true;
}

void
gnuplot::close (void)
{
  if (have_instance ())
    {
      instance->do_close ();

      instance_map.erase (get_current_figure ());
    }
}

void
gnuplot::close_all (void)
{
  for (std::map<int, gnuplot *>::const_iterator p = instance_map.begin ();
       p != instance_map.end ();
       p++)
    {
      gnuplot *elt = p->second;

      elt->do_close ();
    }
}

pid_t
gnuplot::pid (void) const
{
  return plot_stream ? plot_stream->pid () : -1;
}

gnuplot *
gnuplot::lookup_by_pid (pid_t pid)
{
  gnuplot *retval = 0;

  for (std::map<int, gnuplot *>::const_iterator p = instance_map.begin ();
       p != instance_map.end ();
       p++)
    {
      gnuplot *elt = p->second;

      if (elt && elt->pid () == pid)
	{
	  retval = elt;
	  break;
	}
    }

  return retval;
}

void
gnuplot::do_open (void)
{
  static bool initialized = false;

  if (plot_stream && ! *plot_stream)
    do_close ();

  if (! plot_stream)
    {
      initialized = false;

      plot_line_count = 0;

      std::string cmd;

      if (gnuplot_exe.empty ())
	cmd = "gnuplot";
      else
        cmd = "\"" + gnuplot_exe + "\"";

      // FIXME -- I'm not sure this is the right thing to do,
      // but without it, C-c at the octave prompt will kill gnuplot...

#if defined (HAVE_POSIX_SIGNALS)
      sigset_t nset, oset;
      sigemptyset (&nset);
      sigaddset (&nset, SIGINT);
      sigprocmask (SIG_BLOCK, &nset, &oset);
#else
      volatile octave_interrupt_handler old_interrupt_handler
	= octave_ignore_interrupts ();
#endif

      if (use_title_option)
	{
	  std::ostringstream buf;

	  buf << cmd << " -title \"Figure " << get_current_figure () << "\"";

	  cmd = buf.str ();
	}

      plot_stream = new oprocstream (cmd.c_str ());

      if (plot_stream && *plot_stream)
	octave_child_list::insert (plot_stream->pid (),
				   plot_stream_event_handler);
      else
	{
	  delete_plot_stream ();

	  error ("plot: unable to open pipe to `%s'", cmd.c_str ());
	}

#if defined (HAVE_POSIX_SIGNALS)
      sigprocmask (SIG_SETMASK, &oset, 0);
#else
      octave_set_interrupt_handler (old_interrupt_handler);
#endif
    }

  if (! error_state && plot_stream && *plot_stream && ! initialized)
    {
      initialized = true;

      do_send_raw ("set style data lines\n");

      if (! gnuplot_terminal_type.empty ())
	do_send_raw ("set term " + gnuplot_terminal_type
		     + Vgnuplot_command_end + "\n");
    }
}

void
gnuplot::delete_plot_stream (void)
{
  do_send_raw ("\nquit\n");

  delete plot_stream;
  plot_stream = 0;
}

void
gnuplot::reset_plot_stream (void)
{
  delete_plot_stream ();

  plot_line_count = 0;
  parametric_plot = false;
}

void
gnuplot::do_close (void)
{
  if (plot_stream)
    {
      octave_child_list::remove (plot_stream->pid ());

      delete_plot_stream ();
    }

  plot_line_count = 0;
}

bool
gnuplot::plot_stream_event_handler (pid_t pid, int status)
{
  bool retval = false;

  if (pid > 0)
    {
      if (WIFEXITED (status) || WIFSIGNALLED (status))
	{
	  gnuplot *plotter = gnuplot::lookup_by_pid (pid);

	  // We should only print a warning or request removal of the
	  // process from the child list if the process died
	  // unexpectedly.  If do_close is responsible for
	  // gnuplot's death, then plotter will be 0 here and we don't
	  // need to do anything.

	  if (plotter)
	    {
	      plotter->reset_plot_stream ();

	      warning ("connection to external plotter (pid = %d) lost --", pid);
	      // Request removal of this PID from the list of child
	      // processes.

	      retval = true;
	    }
	}
    }

  return retval;
}

void
gnuplot::do_send (const std::string& cmd)
{
  int replot_len = Vgnuplot_command_replot.length ();

  bool is_replot = (Vgnuplot_command_replot == cmd.substr (0, replot_len));

  if (! (plot_line_count == 0 && is_replot))
    do_send_raw (cmd);
}

void
gnuplot::do_send_raw (const std::string& cmd)
{
  if (plot_stream && *plot_stream)
    {
      *plot_stream << cmd;

      plot_stream->flush ();
    }
}

void
gnuplot::do_send_inline_data (const octave_value& val, int ndim,
			      bool parametric)
{
  if (plot_stream && *plot_stream)
    {
      write_inline_data (*plot_stream, val, ndim, parametric);

      plot_stream->flush ();
    }
}

void
gnuplot::do_clear (void)
{
  do_send_raw ("clear\n");

  // FIXME -- instead of just clearing these things, it would
  // be nice if we could reset things to a user-specified default
  // state.

  do_send_raw ("set title\n");
  do_send_raw ("set xlabel\n");
  do_send_raw ("set ylabel\n");
  do_send_raw ("set nogrid\n");
  do_send_raw ("set nolabel\n");

  // This makes a simple `replot' not work after a `clearplot' command
  // has been issued.

  plot_line_count = 0;
}

void
gnuplot::do_set (const string_vector& argv)
{
  int argc = argv.length ();

  std::ostringstream plot_buf;

  if (argc > 1)
    {
      if (almost_match ("parametric", argv[1], 3))
	parametric_plot = true;
      else if (almost_match ("noparametric", argv[1], 5))
	parametric_plot = false;
      else if (almost_match ("term", argv[1], 1))
	{
	  gnuplot_terminal_type = "";
	  std::ostringstream buf;
	  int i;
	  for (i = 2; i < argc-1; i++)
	    buf << argv[i] << " ";
	  if (i < argc)
	    buf << argv[i];
	  buf << Vgnuplot_command_end;
	  gnuplot_terminal_type = buf.str ();
	}
    }

  int i;
  for (i = 0; i < argc-1; i++)
    plot_buf << argv[i] << " ";

  if (i < argc)
    plot_buf << argv[i];

  plot_buf << Vgnuplot_command_end;

  do_send_raw (plot_buf.str ());
}

void
gnuplot::do_show (const string_vector& argv)
{
  int argc = argv.length ();

  std::ostringstream plot_buf;

  int i;
  for (i = 0; i < argc-1; i++)
    plot_buf << argv[i] << " ";
  if (i < argc)
    plot_buf << argv[i];

  plot_buf << Vgnuplot_command_end;

  do_send (plot_buf.str ());
}

void
gnuplot::do_plot (const string_vector& argv)
{
  std::string s;

  for (int i = 1; i < argv.length (); i++)
    s += argv[i] + " ";

  try
    {
      std::string cmd = makeplot (argv[0], s);

      do_send (cmd);
    }
  catch (gpt_parse_error& e)
    {
      if (e.msg.empty ())
	error ("could not parse plot command");
      else
	error (e.msg.c_str ());
    }
}

// Parse and evaluate parameter string and pass it to gnuplot pipe.

std::string
gnuplot::makeplot (std::string caller, std::string args)
  throw (gpt_parse_error)
{
  std::string retval;

  YY_BUFFER_STATE bstate;

  bstate = yy_scan_string (args.c_str ());
  yy_switch_to_buffer (bstate);
  std::string outstr;
  int ndim = 2;

  if (caller == "replot")
    {
      ndim = 1;
      outstr += Vgnuplot_command_replot + " ";
    }
  else if (caller == "plot")
    {
      ndim = 2;
      plot_line_count = 0;
      outstr += Vgnuplot_command_plot + " ";
    }
  else if (caller == "splot")
    {
      ndim = 3;
      plot_line_count = 0;
      outstr += Vgnuplot_command_splot + " ";
    }
  else
    throw gpt_parse_error ("unknown plot command");

  gpt_quote_is_transpose = false;
  gpt_allow_plotkw = false;
  gpt_parens = 0;
  gpt_braces = 0;
  gpt_brackets = 0;

  int tok;
  tok = gptlex ();
  if (plottok_or_end_p (tok) && caller != "replot")
    throw gpt_parse_error ("must have something to plot");

  while (tok)
    {
      bool title_set = false;
      bool using_set = false;
      bool style_set = false;
      bool axes_set = false;

      if (tok == START_BRACKET)
	{
	  if (caller == "replot")
	    throw gpt_parse_error ("can't specify new plot ranges with `replot' or while hold is on");

	  std::string xrange_start_str = read_until (colonp, tok);
	  std::string xrange_end_str = read_until (endbracketp, tok);
	  outstr += printrange (xrange_start_str, xrange_end_str) + " ";
	  tok = gptlex ();
	  if (tok == START_BRACKET)
	    {
	      std::string yrange_start_str = read_until (colonp, tok);
	      std::string yrange_end_str = read_until (endbracketp, tok);
	      outstr += printrange (yrange_start_str, yrange_end_str) + " ";
	      tok = gptlex ();
	      if (tok == START_BRACKET && caller == "splot")
		{
		  std::string zrange_start_str = read_until (colonp, tok);
		  std::string zrange_end_str = read_until (endbracketp, tok);
		  outstr += printrange (zrange_start_str, zrange_end_str) + " ";
		  tok = gptlex ();
                }
            }
        }

      if (plottok_or_end_p (tok))
	return std::string ();
      else
	{
	  std::string file;
	  plot_line_count++;

	  std::string plot_expr_str;
	  plot_expr_str += std::string (yytext) + " ";
	  plot_expr_str += read_until (plottok_or_end_p, tok);

	  int status = 0;
	  octave_value tmp_data = eval_string (plot_expr_str,
					       true, status);

	  if (status != 0 || ! tmp_data.is_defined ())
	    throw gpt_parse_error ();

	  std::ostringstream tmp_buf;
	  tmp_data.print_raw (tmp_buf);

	  if (tmp_data.is_string ())
	    {
	      file = file_ops::tilde_expand (tmp_data.string_value ());
	      // FIXME -- perhaps should check if the file exists?
	      outstr += file + " ";
	    }
	  else
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
		  outstr += "'" + file + "' ";
		}
	    }
        }

      std::string title_str;
      std::string using_str;
      std::string style_str;
      std::string axes_str;

      bool out = false;
      while (tok && ! out)
	{
	  switch (tok)
	    {
	    case COMMA:
	      out = true;
	      break;

	    case TITLE:
	      if (! title_set)
		title_str += handle_title (tok) + " ";
	      else
		throw gpt_parse_error ("only one title option may be specified");
	      title_set = true;
	      break;

	    case USING:
	      if (! using_set)
		using_str += handle_using (tok) + " ";
	      else
		throw gpt_parse_error ("only one using option may be specified");
	      using_set = true;
	      break;

	    case WITH:
	      if (! style_set)
		style_str += handle_style (tok) + " ";
	      else
		throw gpt_parse_error ("only one style option may be specified");
	      style_set = true;
	      break;

	    case AXES:
	      if (! axes_set)
		axes_str += handle_axes (tok) + " ";
	      else
		throw gpt_parse_error ("only one axes option may be specified");
	      axes_set = true;
	      break;

	    default:
	      tok = 0;
	      break;
            }
        }

        if (! title_set)
	  {
            std::ostringstream tmp_buf;
            tmp_buf << Vgnuplot_command_title << " \"line "
                    << plot_line_count << "\" ";
            title_str = tmp_buf.str ();
	    title_set = true;
        }

	// Plot parameters have to be output in this order.
	if (using_set)
	  outstr += using_str;

	if (axes_set)
	  outstr += axes_str;

	if (title_set)
	  outstr += title_str;

	if (style_set)
	  outstr += style_str;

	if (out)
	  {
	    // Saw comma on while loop.
            outstr += ", ";
            gpt_quote_is_transpose = false;
            gpt_allow_plotkw = false;
            gpt_parens = 0;
            gpt_braces = 0;
            gpt_brackets = 0;
            tok = gptlex ();
	  }
    }

  outstr += Vgnuplot_command_end;

  return outstr;
}

// Title has one string expression which is evaluated and printed to the
// gnuplot command string.

std::string
gnuplot::handle_title (int& lasttok)
{
  int tok;
  std::string retstr = Vgnuplot_command_title + " ";
  std::string title_expr_str;

  title_expr_str += read_until (plottok_or_end_p, tok);

  int status;
  octave_value tmp_data = eval_string (title_expr_str, true, status);

  if (status != 0 || ! tmp_data.is_defined ())
    throw gpt_parse_error ();

  std::ostringstream tmp_buf;
  if (tmp_data.is_string ())
    {
      tmp_buf << '"';
      tmp_data.print_raw (tmp_buf);
      tmp_buf << '"';
    }
  else
    {
      warning ("line title must be a string");
      tmp_buf << '"' << "line " << plot_line_count << '"';
    }

  retstr += tmp_buf.str ();

  lasttok = tok;

  return retstr;
}

// The static instance of this class is here so that
// gnuplot::close_all will be called when the .oct file is unloaded.

class
gnuplot_X
{
public:
  gnuplot_X (void) { }
  ~gnuplot_X (void) { gnuplot::close_all (); }
};

static gnuplot_X X;

// -----------------------
// User-callable functions
// -----------------------

DEFUN_DLD (gnuplot_binary, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{val} =} gnuplot_binary ()\n\
@deftypefnx {Loadable Function} {@var{old_val} =} gnuplot_binary (@var{new_val})\n\
Query or set the name of the program invoked by the plot command.\n\
The default value @code{\"gnuplot\"}.  @xref{Installation}.\n\
@end deftypefn")
{
  octave_value retval = SET_INTERNAL_VARIABLE (gnuplot_binary);

  if (! error_state && args.length () == 1)
    gnuplot::set_gnuplot_exe (Vgnuplot_binary);

  return retval;
}

DEFUN_DLD (gnuplot_command_plot, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{val} = } gnuplot_command_plot ()\n\
@deftypefnx {Loadable Function} {@var{old_val} = } gnuplot_command_plot (@var{new_val})\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}\n\
@end deftypefn")
{
  WARN_OBSOLETE ("gnuplot_command_plot");

  return SET_INTERNAL_VARIABLE (gnuplot_command_plot);
}

DEFUN_DLD (gnuplot_command_replot, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{val} =} gnuplot_command_replot ()\n\
@deftypefnx {Loadable Function} {@var{old_val} =} gnuplot_command_replot (@var{new_val})\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}\n\
@end deftypefn")
{
  WARN_OBSOLETE ("gnuplot_command_replot");

  return SET_INTERNAL_VARIABLE (gnuplot_command_replot);
}

DEFUN_DLD (gnuplot_command_splot, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{val} =} gnuplot_command_splot ()\n\
@deftypefnx {Loadable Function} {@var{old_val} =} gnuplot_command_splot (@var{new_val})\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}\n\
@end deftypefn")
{
  WARN_OBSOLETE ("gnuplot_command_splot");

  return SET_INTERNAL_VARIABLE (gnuplot_command_splot);
}

DEFUN_DLD (gnuplot_command_using, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{val} =} gnuplot_command_using ()\n\
@deftypefnx {Loadable Function} {@var{old_val} =} gnuplot_command_using (@var{new_val})\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}\n\
@end deftypefn")
{
  WARN_OBSOLETE ("gnuplot_command_using");

  return SET_INTERNAL_VARIABLE (gnuplot_command_using);
}

DEFUN_DLD (gnuplot_command_with, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{val} =} gnuplot_command_with ()\n\
@deftypefnx {Loadable Function} {@var{old_val} =} gnuplot_command_with (@var{new_val})\n\
of Octave.}\n\
@end deftypefn")
{
  WARN_OBSOLETE ("gnuplot_command_with");

  return SET_INTERNAL_VARIABLE (gnuplot_command_with);
}

DEFUN_DLD (gnuplot_command_axes, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{val} =} gnuplot_command_axes ()\n\
@deftypefnx {Loadable Function} {@var{old_val} =} gnuplot_command_axes (@var{new_val})\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}\n\
@end deftypefn")
{
  WARN_OBSOLETE ("gnuplot_command_axes");

  return SET_INTERNAL_VARIABLE (gnuplot_command_axes);
}

DEFUN_DLD (gnuplot_command_title, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{val} =} gnuplot_command_title ()\n\
@deftypefnx {Loadable Function} {@var{old_val} =} gnuplot_command_title (@var{new_val})\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}\n\
@end deftypefn")
{
  WARN_OBSOLETE ("gnuplot_command_title");

  return SET_INTERNAL_VARIABLE (gnuplot_command_title);
}

DEFUN_DLD (gnuplot_command_end, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{val} =} gnuplot_command_end ()\n\
@deftypefnx {Loadable Function} {@var{old_val} =} gnuplot_command_end (@var{new_val})\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}\n\
\n\
@end deftypefn")
{
  WARN_OBSOLETE ("gnuplot_command_end");

  return SET_INTERNAL_VARIABLE (gnuplot_command_end);
}

DEFUN_DLD (gnuplot_use_title_option, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{val} =} gnuplot_use_title_option ()\n\
@deftypefnx {Loadable Function} {@var{old_val} =} gnuplot_use_title_option (@var{new_val})\n\
If enabled, append @samp{-title \"Figure NN\"} to the gnuplot command.\n\
By default, this feature is enabled if the @code{DISPLAY} environment\n\
variable is set when Octave starts.\n\
\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}\n\
@end deftypefn")
{
  WARN_OBSOLETE ("gnuplot_command_use_title_option");

  return SET_INTERNAL_VARIABLE (gnuplot_use_title_option);
}

DEFUN_DLD (__clear_plot_window__, , ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} clearplot\n\
@deftypefnx {Loadable Function} {} clg\n\
Clear the plot window and any titles or axis labels.\n\
\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}\n\
@end deftypefn")
{
  WARN_OBSOLETE ("__clear_plot_window__");

  gnuplot::clear ();

  return octave_value_list ();
}

DEFUN_DLD (closeplot, , ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} closeplot\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}\n\
\n\
Close stream to the @code{gnuplot} subprocess.  If you are using X11,\n\
this will close the plot window.\n\
@seealso{close}\n\
@end deftypefn")
{
  // Don't use WARN_OBSOLETE here because closeplot is called
  // unconditionally from atexit and always printing the warning when
  // Octave exits could cause confusion.

  gnuplot::close ();

  return octave_value_list ();
}

DEFUN_DLD (purge_tmp_files, , ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} purge_tmp_files\n\
Delete the temporary files created by the plotting commands.\n\
\n\
Octave creates temporary data files for @code{gnuplot} and then sends\n\
commands to @code{gnuplot} through a pipe.  Octave will delete the\n\
temporary files on exit, but if you are doing a lot of plotting you may\n\
want to clean up in the middle of a session.\n\
\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}\n\
@end deftypefn")
{
  WARN_OBSOLETE ("purge_tmp_files");

  gnuplot::cleanup_tmp_files ();

  return octave_value_list ();
}
 
DEFUN_DLD (__gnuplot_raw__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __gnuplot_raw__ (@var{string})\n\
Send @var{string} directly to gnuplot subprocess.\n\
\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}\n\
@end deftypefn")
{
  if (args.length () == 1 && args(0).is_string ())
    {
      WARN_OBSOLETE ("__gnuplot_raw__");

      std::string cmd = args(0).string_value ();

      gnuplot::send_raw (cmd);
    }
  else
    print_usage ();

  return octave_value_list ();
}

DEFUN_DLD (__gnuplot_send_inline_data__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __gnuplot_send_inline_data__ (@var{data}, @var{ndim}, @var{parametric})\n\
Send @var{val} to gnuplot subprocess as inline data.\n\
\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin > 0 && nargin < 4)
    {
      WARN_OBSOLETE ("__gnuplot_send_inline_data__");

      int ndim = 2;
      bool parametric = false;

      if (nargin > 1)
	{
	  ndim = args(1).int_value ();

	  if (! error_state)
	    {
	      if (nargin > 2)
		parametric = args(2).bool_value ();
	    }
	}

      if (! error_state)
	gnuplot::send_inline_data (args(0), ndim, parametric);
    }
  else
    print_usage ();

  return retval;
}

DEFUN_DLD (__gnuplot_set__, args, ,
  "-*- texinfo -*-\n\
@deffn {Loadable Function} __gnuplot_set__ options\n\
Set plotting options for gnuplot\n\
\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}\n\
@end deffn")
{
  WARN_OBSOLETE ("__gnuplot_set__");

  string_vector argv = args.make_argv ("set");

  if (! error_state)
    gnuplot::set (argv);

  return octave_value_list ();
}

DEFUN_DLD (__gnuplot_show__, args, ,
  "-*- texinfo -*-\n\
@deffn {Loadable Function} __gnuplot_show__ options\n\
Show plotting options.\n\
\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}\n\
@end deffn")
{
  WARN_OBSOLETE ("__gnuplot_show__");

  string_vector argv = args.make_argv ("show");

  if (! error_state)
    gnuplot::show (argv);

  return octave_value_list ();
}

DEFUN_DLD (__gnuplot_plot__, args, ,
  "-*- texinfo -*-\n\
Plot with gnuplot.\n\
\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}")
{
  WARN_OBSOLETE ("__gnuplot_plot__");

  string_vector argv = args.make_argv ("plot");

  if (! error_state)
    gnuplot::plot (argv);

  return octave_value_list ();
}

DEFUN_DLD (__gnuplot_splot__, args, ,
  "-*- texinfo -*-\n\
Plot with gnuplot.\n\
\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}")
{
  WARN_OBSOLETE ("__gnuplot_splot__");

  string_vector argv = args.make_argv ("splot");

  if (! error_state)
    gnuplot::plot (argv);

  return octave_value_list ();
}

DEFUN_DLD (__gnuplot_replot__, args, ,
  "-*- texinfo -*-\n\
Plot with gnuplot.\n\
\n\
@strong{This function is obsolete and will be removed from a future version\n\
of Octave.}")
{
  WARN_OBSOLETE ("__gnuplot_replot__");

  string_vector argv = args.make_argv ("replot");

  if (! error_state)
    gnuplot::plot (argv);

  return octave_value_list ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
