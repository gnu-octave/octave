%option prefix="gpt"
%option noyywrap

%{
// PKG_ADD: mark_as_rawcommand ("gplot");
// PKG_ADD: mark_as_rawcommand ("gset");
// PKG_ADD: mark_as_rawcommand ("gsplot");

// PKG_ADD: mark_as_rawcommand ("replot");

// PKG_ADD: mark_as_command ("gshow");
// PKG_ADD: mark_as_command ("hold");
// PKG_ADD: mark_as_command ("set");
// PKG_ADD: mark_as_command ("show");

// PKG_ADD: __gplot_init__ ();

// PKG_ADD: atexit ("closeplot");

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>
#include <fstream>
#include <iostream>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "file-ops.h"

#include "defun-dld.h"
#include "file-io.h"
#include "gripes.h"
#include "load-save.h"
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

static int is_plot_keyword (const std::string& s);

static int handle_string (char delim);

static inline bool can_be_plotkw (void);

static bool gpt_quote_is_transpose;
static bool gpt_allow_plotkw;
static int gpt_parens;
static int gpt_brackets;
static int gpt_braces;

static int send_to_plot_stream (const std::string& cmd);

// needed by handle_string
static char string_buf[256];

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

"\"" {
    return handle_string ('"');
    }

{IDENT} {
    int itok;
    if (can_be_plotkw () && (itok = is_plot_keyword (yytext)))
      {
        gpt_quote_is_transpose = false;
        gpt_allow_plotkw = true;
        return itok;
      }
    else if (std::string (yytext) == "function")
      {
        throw gpt_parse_error ("The 'function' keyword is not allowed in plot commands.");
      }
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

// Pipe to gnuplot.
static oprocstream *plot_stream = 0;

// Gnuplot command strings that we use.
static std::string Vgnuplot_command_plot;
static std::string Vgnuplot_command_replot;
static std::string Vgnuplot_command_splot;
static std::string Vgnuplot_command_using;
static std::string Vgnuplot_command_with;
static std::string Vgnuplot_command_axes;
static std::string Vgnuplot_command_title;
static std::string Vgnuplot_command_end;

// (almost) copy-paste code from pt-plot.cc

static std::string
save_in_tmp_file (const octave_value& t, int ndim = 2, bool parametric = false)
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

static void
close_plot_stream (bool remove_from_child_list = true)
{
  if (plot_stream)
    {
      if (remove_from_child_list)
	octave_child_list::remove (plot_stream->pid ());

      send_to_plot_stream ("\nquit\n");

      delete plot_stream;
      plot_stream = 0;
    }

  plot_line_count = 0;
}

static bool
plot_stream_event_handler (pid_t pid, int status)
{
  bool retval = false;

  if (pid > 0)
    {
      if (WIFEXITED (status) || WIFSIGNALLED (status))
	{
	  close_plot_stream (false);

	  warning ("connection to external plotter (pid = %d) lost --", pid);
	  warning ("please try your plot command(s) again");

	  // Request removal of this PID from the list of child
	  // processes.

	  retval = true;
	}
    }

  return retval;
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
    	    octave_child_list::insert (plot_stream->pid (),
				       plot_stream_event_handler);
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

      *plot_stream << "set style data lines\n";

      if (! gnuplot_terminal_type.empty ())
	*plot_stream << "set term " << gnuplot_terminal_type
		     << Vgnuplot_command_end << "\n";
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
	{
	  *plot_stream << Vgnuplot_command_replot << Vgnuplot_command_end;
	}

      plot_stream->flush ();
    }

  return 0;
}

// Check if the parser state is such that a plot keyword can occur.
static inline bool
can_be_plotkw (void)
{
    return (gpt_allow_plotkw
	    && (gpt_braces == 0)
	    && (gpt_brackets == 0)
	    && (gpt_parens == 0));
}

// (Probably not necessesary, but current Matlab style plot functions
// break without this (they emit too short gnuplot commands))
static std::string
plot_style_token (const std::string& s)
{
  std::string retval;

  // XXX FIXME XXX -- specify minimum match length for these.
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

// This is used to handle single-quote delimited strings. Kludge alert.
static int
handle_string (char delim)
{
  int c;
  char *pos = string_buf;
  int escape_pending = 0;

  *pos++ = static_cast<char> (delim);
  while ((c = yyinput ()) != EOF)
    {
      if (c == '\\')
	{
	  if (escape_pending)
	    {
	      *pos++ = static_cast<char> (c);
	      escape_pending = 0;
	    }
	  else
	    {
		*pos++ = static_cast<char> (c);
		escape_pending = 1;
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
	    *pos++ = static_cast<char> (c);
	  else
	    {
	      c = yyinput ();
	      if (c == delim)
		{
		  *pos++ = static_cast<char> (c);
		  *pos++ = static_cast<char> (c);
		}
	      else
		{
		  yyunput (c, yytext);
		  *pos++ = static_cast<char> (delim);
		  *pos++ = '\0';
		  yytext = string_buf;
		  return STRING;
		}
	    }
	}
      else
	{
	  *pos++ = static_cast<char> (c);
	}

      escape_pending = 0;
    }

  throw gpt_parse_error ("Unterminated string?");

  return 0;
}

// Check to see if a character string matches any one of the plot
// option keywords.  Don't match abbreviations for clear, since that's
// not a gnuplot keyword (users will probably only expect to be able
// to abbreviate actual gnuplot keywords).

static int
is_plot_keyword (const std::string& s)
{
  const char *t = s.c_str ();
  if (almost_match ("title", t, 1))
    {
      return TITLE;
    }
  else if (almost_match ("using", t, 1))
    {
      return USING;
    }
  else if (almost_match ("with", t, 1))
    {
      return WITH;
    }
  else if (almost_match ("axes", t, 2) || almost_match ("axis", t, 2))
    {
      return AXES;
    }
  else if (strcmp ("clear", t) == 0)
    {
      return CLEAR;
    }
  else
    {
      return 0;
    }
}

// Some predicates on tokens

// Return true for ":".
static inline bool
colonp (const int tok)
{
  return (tok == COLON);
}

// Return TRUE for "]".
static inline bool
endbracketp (const int tok)
{
  return (tok == END_BRACKET);
}

// Return TRUE for plot token, comma or end of input.
static inline bool
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
static inline bool
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

      s += std::string (yytext) + " ";

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
  OSSTREAM range_buf;

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
    range_buf << OSSTREAM_ENDS;

    s = OSSTREAM_STR (range_buf);

    return s;
}

// Handle plot parameters.

// Title has one string expression which is evaluated and printed to the
// gnuplot command string.
static std::string
handle_title (int& lasttok)
{
  int tok;
  std::string retstr = Vgnuplot_command_title + " ";
  std::string title_expr_str;

  title_expr_str += read_until (plottok_or_end_p, tok);

  int status;
  octave_value tmp_data = eval_string (title_expr_str, true, status);

  if (status != 0 || ! tmp_data.is_defined ())
    throw gpt_parse_error ();

  OSSTREAM tmp_buf;
  if (tmp_data.is_string ())
    {
      tmp_buf << '"';
      tmp_data.print_raw (tmp_buf);
      tmp_buf << '"' << OSSTREAM_ENDS;
    }
  else
    {
      warning ("line title must be a string");
      tmp_buf << '"' << "line " << plot_line_count << '"';
    }

  retstr += OSSTREAM_STR (tmp_buf);

  lasttok = tok;

  return retstr;
}

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

      tmp_data = eval_string (expr_str, true, status);
      if (status != 0 || ! tmp_data.is_real_scalar ())
	throw gpt_parse_error ();

      OSSTREAM tmp_buf;
      tmp_data.print_raw (tmp_buf);
      tmp_buf << OSSTREAM_ENDS;
      retstr += OSSTREAM_STR (tmp_buf);

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

  // XXX FIXME XXX -- should evaluate the remaining tokens, but this
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

// Parse and evaluate parameter string and pass it to gnuplot pipe.
static int
makeplot (std::string caller, std::string args) throw (gpt_parse_error)
{
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
      if (clear_before_plotting || plot_line_count == 0)
	{
	  plot_line_count = 0;
	  outstr += Vgnuplot_command_plot + " ";
	}
      else
	outstr += Vgnuplot_command_replot + " ";
    }
  else if (caller == "splot")
    {
      ndim = 3;
      if (clear_before_plotting || plot_line_count == 0)
	{
	  plot_line_count = 0;
	  outstr += Vgnuplot_command_splot + " ";
	}
      else
	outstr += Vgnuplot_command_replot + " ";
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
	      if (tok == START_BRACKET && caller == "gsplot")
		{
		  std::string zrange_start_str = read_until (colonp, tok);
		  std::string zrange_end_str = read_until (endbracketp, tok);
		  outstr += printrange (zrange_start_str, zrange_end_str) + " ";
		  tok = gptlex ();
                }
            }
        }

      if (plottok_or_end_p (tok))
	throw gpt_parse_error ("must have something to plot");
      else
	{
	  std::string file;
	  plot_line_count++;

	  if (tok == STRING)
	    {
	      file = file_ops::tilde_expand (std::string (yytext));
	      // XXX FIXME XXX -- perhaps should check if the file exists?
	      outstr += file + " ";
	      // Discard junk after the string.
	      read_until (plottok_or_end_p, tok);
            }
	  else
	    {
	      std::string plot_expr_str;
	      plot_expr_str += std::string (yytext) + " ";
	      plot_expr_str += read_until (plottok_or_end_p, tok);

	      int status;
	      octave_value tmp_data = eval_string (plot_expr_str,
						   true, status);

	      if (status != 0 || ! tmp_data.is_defined ())
		throw gpt_parse_error ();

	      OSSTREAM tmp_buf;
	      tmp_data.print_raw (tmp_buf);
	      tmp_buf << OSSTREAM_ENDS;

	      if (tmp_data.is_string ())
		{
		  file = file_ops::tilde_expand (tmp_data.string_value ());
		  // XXX FIXME XXX -- perhaps should check if the file exists?
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
            OSSTREAM tmp_buf;
            tmp_buf << Vgnuplot_command_title << " \"line "
                    << plot_line_count << "\" " << OSSTREAM_ENDS;
            title_str = OSSTREAM_STR (tmp_buf);
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

  // Terrible kludge, but line count is destroyed when plot stream
  // is opened for the first time.
  int linesave = plot_line_count;
  send_to_plot_stream (outstr);
  plot_line_count = linesave;

  return 1;
}

static void
doplot (std::string caller, octave_value_list args)
{
  std::string s;
  int i = 0;

  while (i < args.length ())
    s += args (i++).string_value () + " ";

  try
    {
      makeplot (caller, s);
    }
  catch (gpt_parse_error e)
    {
      if (e.msg.empty ())
	error ("could not parse plot command");
      else
	error (e.msg.c_str ());
    }
}

DEFUN_DLD (gplot, args, ,
  "Plot with gnuplot.\n")
{
  doplot ("plot", args);
  return octave_value_list ();
}

DEFUN_DLD (gsplot, args, ,
  "Plot with gnuplot.\n")
{
  doplot ("splot", args);
  return octave_value_list ();
}

DEFUN_DLD (replot, args, ,
  "Plot with gnuplot.\n")
{
  doplot ("replot", args);
  return octave_value_list ();
}

DEFUN_DLD (clearplot, , ,
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

DEFUN_DLD (closeplot, , ,
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

DEFUN_DLD (hold, args, ,
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

DEFUN_DLD (ishold, , ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ishold\n\
Return 1 if the next line will be added to the current plot, or 0 if\n\
the plot device will be cleared before drawing the next line.\n\
@end deftypefn")
{
  return octave_value (! clear_before_plotting);
}

DEFUN_DLD (purge_tmp_files, , ,
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

DEFUN_DLD (graw, args, ,
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
    print_usage ("raw");

  return retval;
}

DEFUN_DLD (gset, args, ,
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

DEFUN_DLD (set, args, nargout,
  "-*- texinfo -*-\n\
This command is has been replaced by @code{gset}.")
{
  warning ("set is obsolete -- use gset instead");
  return Fgset (args, nargout);
}

DEFUN_DLD (gshow, args, ,
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

DEFUN_DLD (show, args, nargout,
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

DEFUN_DLD (__gplot_init__, , ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} __gplot_init__ ()\n\
@end deftypefn")
{
  octave_value_list retval;

  static bool gplot_initialized = false;

  if (gplot_initialized)
    return retval;

  gplot_initialized = true;

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

  return retval;
}
