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
along with GNU CC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

%s TEXT_FCN
%s MATRIX

%{
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cctype>
#include <cstring>

#include <string>

#include <strstream.h>

#include "SLStack.h"

// These would be alphabetical, but y.tab.h must be included before
// oct-gperf.h and y.tab.h must be included after token.h and the tree
// class declarations.  We can't include y.tab.h in oct-gperf.h
// because it may not be protected to allow it to be included multiple
// times.

#include "defun.h"
#include "error.h"
#include "input.h"
#include "lex.h"
#include "toplev.h"
#include "parse.h"
#include "symtab.h"
#include "token.h"
#include "pt-base.h"
#include "pt-cmd.h"
#include "ov.h"
#include "pt-exp.h"
#include "pt-mat.h"
#include "pt-misc.h"
#include "pt-plot.h"
#include "utils.h"
#include "variables.h"
#include <y.tab.h>
#include <oct-gperf.h>

#if ! (defined (FLEX_SCANNER) \
       && defined (YY_FLEX_MAJOR_VERSION) && YY_FLEX_MAJOR_VERSION >= 2 \
       && defined (YY_FLEX_MINOR_VERSION) && YY_FLEX_MINOR_VERSION >= 5)
#error lex.l requires flex version 2.5.4 or later
#endif

// Flags that need to be shared between the lexer and parser.
lexical_feedback lexer_flags;

// Stack to hold tokens so that we can delete them when the parser is
// reset and avoid growing forever just because we are stashing some
// information.  This has to appear before lex.h is included, because
// one of the macros defined there uses token_stack.
//
// XXX FIXME XXX -- this should really be static, but that causes
// problems on some systems.
SLStack <token*> token_stack;

// Did eat_whitespace() eat a space or tab, or a newline, or both?

typedef int yum_yum;

const yum_yum ATE_NOTHING = 0;
const yum_yum ATE_SPACE_OR_TAB = 1;
const yum_yum ATE_NEWLINE = 2;

// Is the closest nesting level a square brace or a paren?

class brace_paren_nesting_level : public SLStack <int>
{
public:

  brace_paren_nesting_level (void) : SLStack<int> () { }

  ~brace_paren_nesting_level (void) { }

  void brace (void) { push (BRACE); }
  bool is_brace (void) { return ! empty () && top () == BRACE; }

  void paren (void) { push (PAREN); }
  bool is_paren (void) { return ! empty () && top () == PAREN; }

  bool none (void) { return empty (); }

  void remove (void) { if (! empty ()) SLStack<int>::pop (); }

private:

  enum { BRACE = 1, PAREN = 2 };

  brace_paren_nesting_level (const brace_paren_nesting_level&);

  brace_paren_nesting_level& operator = (const brace_paren_nesting_level&);
};

static brace_paren_nesting_level nesting_level;

// Should whitespace in a literal matrix list be automatically
// converted to commas and semicolons?
//
//   user specifies   value of var
//   --------------   ------------
//   "ignore"              2
//   "traditional"         1
//   anything else         0
//
// Octave will never insert a comma in a literal matrix list if the
// user specifies "ignore".  For example, the statement [1 2] will
// result in an error instead of being treated the same as [1, 2], and
// the statement
//
//   [ 1, 2,
//     3, 4 ]
//
// will result in the vector [1 2 3 4] instead of a matrix.
//
// Traditional behavior makes Octave convert spaces to a comma between
// identifiers and `('.  For example, the statement
//
//   [eye (2)]
//
// will be parsed as
//
//   [eye, (2)]
//
// and will result in an error since the `eye' function will be
// called with no arguments.  To get around this, you would have to
// omit the space between `eye' and the `('.
//
// The default value is 0, which results in behavior that is the same
// as traditional, except that Octave does not convert spaces to a
// comma between identifiers and `('.  For example, the statement
//
//   [eye (2)]
//
// will result in a call to `eye' with the argument `2'.

static int Vwhitespace_in_literal_matrix;


// Forward declarations for functions defined at the bottom of this
// file.

static void do_string_escapes (char *s);
static void fixup_column_count (char *s);
static void do_comma_insert_check (void);
static int is_plot_keyword (const string& s);
static int is_keyword (const string& s);
static string plot_style_token (const string& s);
static symbol_record *lookup_identifier (const string& s);
static void grab_help_text (void);
static int match_any (char c, char *s);
static int next_token_is_bin_op (int spc_prev, char *yytext);
static int next_token_is_postfix_unary_op (int spc_prev, char *yytext);
static string strip_trailing_whitespace (char *s);
static void handle_number (char *yytext);
static int handle_string (char delim, int text_style = 0);
static int handle_close_brace (int spc_gobbled);
static int handle_identifier (const string& tok, int spc_gobbled);
static int have_continuation (int trailing_comments_ok = 1);
static int have_ellipsis_continuation (int trailing_comments_ok = 1);
static yum_yum eat_whitespace (void);
static yum_yum eat_continuation (void);

%}

D	[0-9]
S	[ \t]
NL	((\n)|(\r\n))
SNL	({S}|{NL})
EL	(\.\.\.)
BS	(\\)
CONT	({EL}|{BS})
Im	[iIjJ]
CCHAR	[#%]
COMMENT	({CCHAR}.*{NL})
SNLCMT	({SNL}|{COMMENT})
NOTEQ	((~=)|(!=)|(<>))
POW	((\*\*)|(\^))
EPOW	(\.{POW})
NOT	((\~)|(\!))
IDENT	([_a-zA-Z][_a-zA-Z0-9]*)
EXPON	([DdEe][+-]?{D}+)
NUMBER	(({D}+\.?{D}*{EXPON}?)|(\.{D}+{EXPON}?))
%%

%{
// Help and other text-style functions are a pain in the ass.  This
// stuff needs to be simplified.  May require some changes in the
// parser too.
%}

<TEXT_FCN>{NL} {
    BEGIN 0;
    current_input_column = 1;
    lexer_flags.quote_is_transpose = 0;
    lexer_flags.cant_be_identifier = 0;
    lexer_flags.convert_spaces_to_comma = 1;
    return '\n';
  }

<TEXT_FCN>[\;\,] {
    if (lexer_flags.doing_set && strcmp (yytext, ",") == 0)
      {
	TOK_PUSH_AND_RETURN (yytext, TEXT);
      }
    else
      {
	BEGIN 0;
	if (strcmp (yytext, ",") == 0)
	  TOK_RETURN (',');
	else
	  TOK_RETURN (';');
      }
  }

<TEXT_FCN>[\"\'] {
    current_input_column++;
    return handle_string (yytext[0], 1);
  }

<TEXT_FCN>[^ \t\n\;\,]*{S}* {
    string tok = strip_trailing_whitespace (yytext);
    TOK_PUSH_AND_RETURN (tok, TEXT);
  }

%{
// For this and the next two rules, we're looking at ']', and we
// need to know if the next token is `=' or `=='.
//
// It would have been so much easier if the delimiters were simply
// different for the expression on the left hand side of the equals
// operator.
//
// It's also a pain in the ass to decide whether to insert a comma
// after seeing a ']' character...
%}

<MATRIX>{SNLCMT}*\]{S}* {
    fixup_column_count (yytext);
    int c = yytext[yyleng-1];
    int cont_is_spc = eat_continuation ();
    int spc_gobbled = (cont_is_spc || c == ' ' || c == '\t');
    return handle_close_brace (spc_gobbled);
  }

%{
// Commas are element separators in matrix constants.  If we don't
// check for continuations here we can end up inserting too many
// commas.
%}

<MATRIX>{S}*\,{S}* {
    current_input_column += yyleng;
    int tmp = eat_continuation ();
    lexer_flags.quote_is_transpose = 0;
    lexer_flags.cant_be_identifier = 0;
    lexer_flags.convert_spaces_to_comma = 1;
    if (Vwhitespace_in_literal_matrix != 2
	&& (tmp & ATE_NEWLINE) == ATE_NEWLINE)
      unput (';');
    return (',');
  }

%{
// In some cases, spaces in matrix constants can turn into commas.
// If commas are required, spaces are not important in matrix
// constants so we just eat them.  If we don't check for continuations
// here we can end up inserting too many commas.
%}

<MATRIX>{S}+ {
    current_input_column += yyleng;
    if (Vwhitespace_in_literal_matrix != 2)
      {
	int tmp = eat_continuation ();
	int bin_op = next_token_is_bin_op (1, yytext);
	int postfix_un_op = next_token_is_postfix_unary_op (1, yytext);

	if (! (postfix_un_op || bin_op)
	    && nesting_level.is_brace ()
	    && lexer_flags.convert_spaces_to_comma)
	  {
	    lexer_flags.quote_is_transpose = 0;
	    lexer_flags.cant_be_identifier = 0;
	    lexer_flags.convert_spaces_to_comma = 1;
	    if ((tmp & ATE_NEWLINE) == ATE_NEWLINE)
	      unput (';');
	    return (',');
	  }
      }
  }

%{
// Semicolons are handled as row seprators in matrix constants.  If we
// don't eat whitespace here we can end up inserting too many
// semicolons.
%}

<MATRIX>{SNLCMT}*;{SNLCMT}* {
    fixup_column_count (yytext);
    eat_whitespace ();
    lexer_flags.quote_is_transpose = 0;
    lexer_flags.cant_be_identifier = 0;
    lexer_flags.convert_spaces_to_comma = 1;
    return ';';
  }

%{
// In some cases, new lines can also become row separators.  If we
// don't eat whitespace here we can end up inserting too many
// semicolons.
%}

<MATRIX>{SNLCMT}*\n{SNLCMT}* {
    fixup_column_count (yytext);
    eat_whitespace ();
    if (Vwhitespace_in_literal_matrix != 2)
      {
	lexer_flags.quote_is_transpose = 0;
	lexer_flags.cant_be_identifier = 0;
	lexer_flags.convert_spaces_to_comma = 1;

	if (nesting_level.none ())
	  return LEXICAL_ERROR;

	if (nesting_level.is_brace ())
	  return ';';
      }
  }

%{
// Open and close brace are handled differently if we are in the range
// part of a plot command.
//
%}

\[{S}* {
    nesting_level.brace ();

    current_input_column += yyleng;
    lexer_flags.quote_is_transpose = 0;
    lexer_flags.cant_be_identifier = 0;
    lexer_flags.convert_spaces_to_comma = 1;

    promptflag--;
    eat_whitespace ();

    if (lexer_flags.plotting && ! lexer_flags.past_plot_range)
      {
	lexer_flags.in_plot_range = 1;
	return OPEN_BRACE;
      }
    else
      {
	lexer_flags.braceflag++;
	BEGIN MATRIX;
	return '[';
      }
  }

\] {
    nesting_level.remove ();

    if (lexer_flags.plotting && ! lexer_flags.past_plot_range)
      {
	lexer_flags.in_plot_range = 0;
	TOK_RETURN (CLOSE_BRACE);
      }
    else
      TOK_RETURN (']');
  }

%{
// Imaginary numbers.
%}

{NUMBER}{Im} {
    handle_number (yytext);
    return IMAG_NUM;
  }

%{
// Real numbers.  Don't grab the `.' part of a dot operator as part of
// the constant.
%}

{D}+/\.[\*/\\^'] |
{NUMBER} {
    handle_number (yytext);
    return NUM;
  }

%{
// Eat whitespace.  Whitespace inside matrix constants is handled by
// the <MATRIX> start state code above.
%}

{S}* {
    current_input_column += yyleng;
  }

%{
// Continuation lines.  Allow comments after continuations.
%}

{CONT}{S}*{NL} |
{CONT}{S}*{COMMENT} {
    promptflag--;
    current_input_column = 1;
  }

%{
// An ellipsis not at the end of a line is not a continuation, but
// does have another meaning.
%}

{EL} {
    return ELLIPSIS;
  }

%{
// End of file.
%}

<<EOF>> {
    TOK_RETURN (END_OF_INPUT);
  }

%{
// Identifiers.  Truncate the token at the first space or tab but
// don't write directly on yytext.
%}

{IDENT}{S}* {
    string tok = strip_trailing_whitespace (yytext);
    int c = yytext[yyleng-1];
    int cont_is_spc = eat_continuation ();
    int spc_gobbled = (cont_is_spc || c == ' ' || c == '\t');
    return handle_identifier (tok, spc_gobbled);
  }

%{
// A new line character.  New line characters inside matrix constants
// are handled by the <MATRIX> start state code above.  If closest
// nesting is inside parentheses, don't return a row separator.
%}

{NL} {
    current_input_column = 1;
    lexer_flags.quote_is_transpose = 0;
    lexer_flags.cant_be_identifier = 0;
    lexer_flags.convert_spaces_to_comma = 1;

    if (nesting_level.none ())
      return '\n';

    if (nesting_level.is_brace ())
      return LEXICAL_ERROR;
  }

%{
// Single quote can either be the beginning of a string or a transpose
// operator. 
%}

"'" {
    current_input_column++;
    lexer_flags.convert_spaces_to_comma = 1;

    if (lexer_flags.quote_is_transpose)
      {
	do_comma_insert_check ();
	return QUOTE;
      }
    else
      return handle_string ('\'');
  }

%{
// Double quotes always begin strings.
%}

\" {
    current_input_column++;
    return handle_string ('"');
}

%{
// The colon operator is handled differently if we are in the range
// part of a plot command.
%}

":" {
    if (lexer_flags.plotting
	&& (lexer_flags.in_plot_range || lexer_flags.in_plot_using))
      BIN_OP_RETURN (COLON, 1);
    else
      BIN_OP_RETURN (':', 0);
  }

%{
// Gobble comments.  If closest nesting is inside parentheses, don't
// return a new line.
%} 

{CCHAR} {
    if (help_buf.empty ()
	&& lexer_flags.beginning_of_function
	&& nesting_level.none ())
      {
	grab_help_text ();
	lexer_flags.beginning_of_function = 0;
      }
    else
      {
	int c;
	while ((c = yyinput ()) != EOF && c != '\n')
	  ; // Eat comment.
      }

    current_input_column = 1;
    lexer_flags.quote_is_transpose = 0;
    lexer_flags.cant_be_identifier = 0;
    lexer_flags.convert_spaces_to_comma = 1;

    if (nesting_level.none ())
      return '\n';
    else if (nesting_level.is_brace ())
      return ';';
  }

%{
// Other operators.
%}

".+"		{ BIN_OP_RETURN (EPLUS, 0); }
".-"		{ BIN_OP_RETURN (EMINUS, 0); }
".*"		{ BIN_OP_RETURN (EMUL, 0); }
"./"		{ BIN_OP_RETURN (EDIV, 0); }
".\\"		{ BIN_OP_RETURN (ELEFTDIV, 0); }
{EPOW}		{ BIN_OP_RETURN (EPOW, 0); }
".'"		{ do_comma_insert_check (); BIN_OP_RETURN (TRANSPOSE, 1); }
"++"		{ do_comma_insert_check (); BIN_OP_RETURN (PLUS_PLUS, 1); }
"--"		{ do_comma_insert_check (); BIN_OP_RETURN (MINUS_MINUS, 1); }
"<="		{ BIN_OP_RETURN (EXPR_LE, 0); }
"=="		{ BIN_OP_RETURN (EXPR_EQ, 0); }
{NOTEQ}		{ BIN_OP_RETURN (EXPR_NE, 0); }
">="		{ BIN_OP_RETURN (EXPR_GE, 0); }
"|"		{ BIN_OP_RETURN (EXPR_OR, 0); }
"&"		{ BIN_OP_RETURN (EXPR_AND, 0); }
"<"		{ BIN_OP_RETURN (EXPR_LT, 0); }
">"		{ BIN_OP_RETURN (EXPR_GT, 0); }
"*"		{ BIN_OP_RETURN ('*', 0); }
"/"		{ BIN_OP_RETURN ('/', 0); }
"\\"		{ BIN_OP_RETURN (LEFTDIV, 0); }
";"		{ BIN_OP_RETURN (';', 1); }
","		{ BIN_OP_RETURN (',', 1); }
{POW}		{ BIN_OP_RETURN (POW, 0); }
"="		{ BIN_OP_RETURN ('=', 1); }
"||"		{ BIN_OP_RETURN (EXPR_OR_OR, 0); }
"&&"		{ BIN_OP_RETURN (EXPR_AND_AND, 0); }

{NOT} {
    if (lexer_flags.plotting && ! lexer_flags.in_plot_range)
      lexer_flags.past_plot_range = 1;
    BIN_OP_RETURN (EXPR_NOT, 0);
  }

"+" { 
    if (lexer_flags.plotting && ! lexer_flags.in_plot_range)
      lexer_flags.past_plot_range = 1;
    BIN_OP_RETURN ('+', 0);
  }

"-" { 
    if (lexer_flags.plotting && ! lexer_flags.in_plot_range)
      lexer_flags.past_plot_range = 1;
    BIN_OP_RETURN ('-', 0);
  }

"(" {
    if (lexer_flags.plotting && ! lexer_flags.in_plot_range)
      lexer_flags.past_plot_range = 1;
    nesting_level.paren ();
    promptflag--;
    TOK_RETURN ('(');
  }

")" {
    nesting_level.remove ();

    current_input_column++;
    lexer_flags.cant_be_identifier = 1;
    lexer_flags.quote_is_transpose = 1;
    lexer_flags.convert_spaces_to_comma = nesting_level.is_brace ();
    do_comma_insert_check ();
    return ')';
  }

"." {
    TOK_RETURN ('.');
  }

%{
// Unrecognized input is a lexical error.
%}

. {
    current_input_column++;

    error ("invalid character `%s' near line %d, column %d",
	   undo_string_escape (yytext[0]), input_line_number,
	   current_input_column);

    return LEXICAL_ERROR;
  }

%%

// GAG.
//
// If we're reading a matrix and the next character is '[', make sure
// that we insert a comma ahead of it.

void
do_comma_insert_check (void)
{
  int spc_gobbled = eat_continuation ();
  int c = yyinput ();
  yyunput (c, yytext);
  if (spc_gobbled)
    yyunput (' ', yytext);
  lexer_flags.do_comma_insert = (lexer_flags.braceflag && c == '[');
}

// Fix things up for errors or interrupts.  The parser is never called
// recursively, so it is always safe to reinitialize its state before
// doing any parsing.

void
reset_parser (void)
{
  // Start off on the right foot.
  BEGIN 0;
  error_state = 0;

  // We do want a prompt by default.
  promptflag = 1;

  // Error may have occurred inside some parentheses or braces.
  nesting_level.clear ();

  // Clear out the stack of token info used to track line and column
  // numbers.
  while (! token_stack.empty ())
    delete token_stack.pop ();

  // Can be reset by defining a function.
  if (! (reading_script_file || reading_fcn_file))
    {
      current_input_column = 1;
      input_line_number = current_command_number - 1;
    }

  // Only ask for input from stdin if we are expecting interactive
  // input.
  if (interactive && ! (reading_fcn_file || get_input_from_eval_string))
    yyrestart (stdin);

  // Clear the buffer for help text.
  help_buf.resize (0);

  // Reset other flags.
  lexer_flags.init ();
}

// Replace backslash escapes in a string with the real values.

static void
do_string_escapes (char *s)
{
  char *p1 = s;
  char *p2 = s;
  while (*p2 != '\0')
    {
      if (*p2 == '\\' && *(p2+1) != '\0')
	{
	  switch (*++p2)
	    {
	    case 'a':
	      *p1 = '\a';
	      break;

	    case 'b': // backspace
	      *p1 = '\b';
	      break;

	    case 'f': // formfeed
	      *p1 = '\f';
	      break;

	    case 'n': // newline
	      *p1 = '\n';
	      break;

	    case 'r': // carriage return
	      *p1 = '\r';
	      break;

	    case 't': // horizontal tab
	      *p1 = '\t';
	      break;

	    case 'v': // vertical tab
	      *p1 = '\v';
	      break;

	    case '\\': // backslash
	      *p1 = '\\';
	      break;

	    case '\'': // quote
	      *p1 = '\'';
	      break;

	    case '"': // double quote
	      *p1 = '"';
	      break;

	    default:
	      warning ("unrecognized escape sequence `\\%c' --\
 converting to `%c'", *p2, *p2);
	      *p1 = *p2;
	      break;
	    }
	}
      else
	{
	  *p1 = *p2;
	}

      p1++;
      p2++;
    }

  *p1 = '\0';
}

// If we read some newlines, we need figure out what column we're
// really looking at.

static void
fixup_column_count (char *s)
{
  char c;
  while ((c = *s++) != '\0')
    {
      if (c == '\n')
	  current_input_column = 1;
      else
	current_input_column++;
    }
}

// Include these so that we don't have to link to libfl.a.

#ifdef yywrap
#undef yywrap
#endif
static int
yywrap (void)
{
  return 1;
}

// Tell us all what the current buffer is.

YY_BUFFER_STATE
current_buffer (void)
{
  return YY_CURRENT_BUFFER;
}

// Create a new buffer.

YY_BUFFER_STATE
create_buffer (FILE *f)
{
  return yy_create_buffer (f, YY_BUF_SIZE);
}

// Start reading a new buffer.

void
switch_to_buffer (YY_BUFFER_STATE buf)
{
  yy_switch_to_buffer (buf);
}

// Delete a buffer.

void
delete_buffer (YY_BUFFER_STATE buf)
{
  yy_delete_buffer (buf);
}

// Restore a buffer (for unwind-prot).

void
restore_input_buffer (void *buf)
{
  switch_to_buffer ((YY_BUFFER_STATE) buf);
}

// Delete a buffer (for unwind-prot).

void
delete_input_buffer (void *buf)
{
  delete_buffer ((YY_BUFFER_STATE) buf);
}

// Check to see if a character string matches any of the possible line
// styles for plots.

static string
plot_style_token (const string& s)
{
  string retval;

  static char *plot_styles[] = 
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

  char **tmp = plot_styles;
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

// Check to see if a character string matches any one of the plot
// option keywords.  Don't match abbreviations for clear, since that's
// not a gnuplot keyword (users will probably only expect to be able
// to abbreviate actual gnuplot keywords).

static int
is_plot_keyword (const string& s)
{
  const char *t = s.c_str ();
  if (almost_match ("title", t))
    {
      return TITLE;
    }
  else if (almost_match ("using", t))
    {
      lexer_flags.in_plot_using = 1;
      return USING;
    }
  else if (almost_match ("with", t))
    {
      lexer_flags.in_plot_style = 1;
      return WITH;
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

// Handle keywords.  Could probably be more efficient...

static int
is_keyword (const string& s)
{
  if (lexer_flags.plotting && lexer_flags.in_plot_style)
    {
      string sty = plot_style_token (s);

      if (! sty.empty ())
	{
	  lexer_flags.in_plot_style = 0;
	  yylval.tok_val = new token (sty);
	  token_stack.push (yylval.tok_val);
	  return STYLE;
	}
    }

  int l = input_line_number;
  int c = current_input_column;

  int len = s.length ();

  const octave_kw *kw = octave_kw_lookup (s.c_str (), len);

  if (kw)
    {
      yylval.tok_val = 0;

      switch (kw->kw_id)
	{
	case all_va_args_kw:
	case break_kw:
	case catch_kw:
	case continue_kw:
	case else_kw:
	case elseif_kw:
	case global_kw:
	case return_kw:
	case unwind_protect_cleanup_kw:
 	  break;

	case end_kw:
	  yylval.tok_val = new token (token::simple_end, l, c);
	  break;

	case end_try_catch_kw:
	  yylval.tok_val = new token (token::try_catch_end, l, c);
	  break;

	case end_unwind_protect_kw:
	  yylval.tok_val = new token (token::unwind_protect_end, l, c);
	  break;

	case endfor_kw:
	  yylval.tok_val = new token (token::for_end, l, c);
	  break;

	case endfunction_kw:
	  yylval.tok_val = new token (token::function_end, l, c);
	  break;

	case endif_kw:
	  yylval.tok_val = new token (token::if_end, l, c);
	  break;

	case endwhile_kw:
	  yylval.tok_val = new token (token::while_end, l, c);
	  break;

	case for_kw:
	case while_kw:
	  promptflag--;
	  lexer_flags.looping++;
	  break;

	case if_kw:
	  promptflag--;
	  lexer_flags.iffing++;
	  break;

	case try_kw:
	case unwind_protect_kw:
	  promptflag--;
	  break;

	case gplot_kw:
	  lexer_flags.plotting = 1;
	  yylval.tok_val = new token (token::two_dee, l, c);
	  break;

	case gsplot_kw:
	  lexer_flags.plotting = 1;
	  yylval.tok_val = new token (token::three_dee, l, c);
	  break;

	case replot_kw:
	  lexer_flags.plotting = 1;
	  yylval.tok_val = new token (token::replot, l, c);
	  break;

	case function_kw:
	  if (lexer_flags.defining_func)
	    {
	      error ("function keyword invalid within a function body");

	      if ((reading_fcn_file || reading_script_file)
		  && ! curr_fcn_file_name.empty ())
		error ("defining new function near line %d of file `%s.m'",
		       input_line_number, curr_fcn_file_name.c_str ());
	      else
		error ("defining new function near line %d",
		       input_line_number);

	      return LEXICAL_ERROR;
	    }
	  else
	    {
	      tmp_local_sym_tab = new symbol_table ();
	      curr_sym_tab = tmp_local_sym_tab;
	      lexer_flags.defining_func = 1;
	      promptflag--;
	      lexer_flags.beginning_of_function = 1;
	      if (! (reading_fcn_file || reading_script_file))
		input_line_number = 1;
	    }
	  break;

	default:
	  panic_impossible ();
	}

      if (! yylval.tok_val)
	yylval.tok_val = new token (l, c);

      token_stack.push (yylval.tok_val);

      return kw->tok;
    }

  return 0;
}

// Try to find an identifier.  All binding to global or builtin
// variables occurs when expressions are evaluated.

static symbol_record *
lookup_identifier (const string& name)
{
  return curr_sym_tab->lookup (name, 1, 0);
}

static bool
is_variable (const string& name)
{
  symbol_record *sr = curr_sym_tab->lookup (name, 0, 0);

  return sr && sr->is_variable ();
}

static void
force_local_variable (const string& name)
{
  symbol_record *sr = curr_sym_tab->lookup (name, 1, 0);

  if (sr)
    sr->define (octave_value ());
}

// Grab the help text from an function file.  Always overwrites the
// current contents of help_buf.

// XXX FIXME XXX -- gobble_leading_white_space() in variables.cc
// duplicates some of this code!

static void
grab_help_text (void)
{
  help_buf.resize (0);

  bool begin_comment = true;
  bool in_comment = true;
  int c = 0;

  while ((c = yyinput ()) != EOF)
    {
      if (begin_comment)
	{
	  if (c == '%' || c == '#')
	    continue;
	  else
	    begin_comment = false;
	}	

      if (in_comment)
	{
	  help_buf += (char) c;

	  if (c == '\n')
	    in_comment = false;
	}
      else
	{
	  switch (c)
	    {
	    case '%':
	    case '#':
	      in_comment = true;
	      begin_comment = true;
	      break;

	    case ' ':
	    case '\t':
	      break;

	    default:
	      goto done;
	    }
	}
    }

 done:

  if (c)
    yyunput (c, yytext);
}

// Return 1 if the given character matches any character in the given
// string.

static int
match_any (char c, char *s)
{
  char tmp;
  while ((tmp = *s++) != '\0')
    {
      if (c == tmp)
	return 1;
    }
  return 0;
}

// Given information about the spacing surrounding an operator,
// return 1 if it looks like it should be treated as a binary
// operator.  For example,
//
//   [ 1 + 2 ]  or  [ 1+ 2]  or  [ 1+2 ]  ==> binary

static int
looks_like_bin_op (int spc_prev, int spc_next)
{
  return ((spc_prev && spc_next) || ! spc_prev);
}

// Try to determine if the next token should be treated as a postfix
// unary operator.  This is ugly, but it seems to do the right thing.

static int
next_token_is_postfix_unary_op (int spc_prev, char *yytext)
{
  int un_op = 0;

  int c0 = yyinput ();
  int c1 = yyinput ();

  yyunput (c1, yytext);
  yyunput (c0, yytext);

  int transpose = (c0 == '.' && c1 == '\'');
  int hermitian = (c0 == '\'');

  un_op = (transpose || (hermitian && ! spc_prev));

  return un_op;
}

// Try to determine if the next token should be treated as a binary
// operator.  This is even uglier, but it also seems to do the right
// thing.  Note that it is only necessary to check the spacing for `+'
// and `-', since those are the only tokens that can appear as unary
// ops too.
//
// Note that this never returns true for `.', even though it can be a
// binary operator (the structure reference thing).  The only time
// this appears to matter is for things like
//
//   [ a . b ]
//
// which probably doesn't occur that often, can be worked around by
// eliminating the whitespace, putting the expression in parentheses,
// or using `whitespace_in_literal_matrix = "ignored"', so I think it
// is an acceptable change.  It would be quite a bit harder to `fix'
// this.  (Well, maybe not.  the best fix would be to do away with the
// specialness of whitespace inside of `[ ... ]').
//
// However, we still do check for `.+', `.*', etc.

static int
next_token_is_bin_op (int spc_prev, char *yytext)
{
  int bin_op = 0;

  int c0 = yyinput ();

  switch (c0)
    {
    case '+':
    case '-':
      {
	int c1 = yyinput ();
	yyunput (c1, yytext);
	int spc_next = (c1 == ' ' || c1 == '\t');
	bin_op = looks_like_bin_op (spc_prev, spc_next);
      }
      break;

    case '.':
      {
	int c1 = yyinput ();
	yyunput (c1, yytext);
	bin_op = match_any (c1, "+-*/\\^");
      }
      break;

    case '/':
    case ':':
    case '\\':
    case '^':
    case '&':
    case '*':
    case '|':
    case '<':
    case '>':
    case '~':
    case '!':
    case '=':
      bin_op = 1;
      break;

    default:
      break;
    }

  yyunput (c0, yytext);

  return bin_op;
}

// Used to delete trailing white space from tokens.

static string
strip_trailing_whitespace (char *s)
{
  string retval = s;

  size_t pos = retval.find_first_of (" \t");

  if (pos != NPOS)
    retval.resize (pos);

  return retval;
}

// Discard whitespace, including comments and continuations.
//
// Return value is logical OR of the following values:
//
//  ATE_NOTHING      : no spaces to eat
//  ATE_SPACE_OR_TAB : space or tab in input
//  ATE_NEWLINE      : bare new line in input

static yum_yum
eat_whitespace (void)
{
  yum_yum retval = ATE_NOTHING;
  int in_comment = 0;
  int c;
  while ((c = yyinput ()) != EOF)
    {
      current_input_column++;

      switch (c)
	{
	case ' ':
	case '\t':
	  retval |= ATE_SPACE_OR_TAB;
	  break;

	case '\n':
	  retval |= ATE_NEWLINE;
	  in_comment = 0;
	  current_input_column = 0;
	  break;

	case '#':
	case '%':
	  in_comment = 1;
	  break;

	case '.':
	  if (in_comment)
	    break;
	  else
	    {
	      if (have_ellipsis_continuation ())
		break;
	      else
		goto done;
	    }

	case '\\':
	  if (in_comment)
	    break;
	  else
	    {
	      if (have_continuation ())
		break;
	      else
		goto done;
	    }

	default:
	  if (in_comment)
	    break;
	  else
	    goto done;
	}
    }

 done:
  yyunput (c, yytext);
  current_input_column--;
  return retval;
}

static void
handle_number (char *yytext)
{
  char *tmp = strsave (yytext);

  char *idx = strpbrk (tmp, "Dd");

  if (idx)
    *idx = 'e';

  double value;
  int nread = sscanf (tmp, "%lf", &value);

  delete [] tmp;

  // If yytext doesn't contain a valid number, we are in deep doo doo.

  assert (nread == 1);

  lexer_flags.quote_is_transpose = 1;
  lexer_flags.cant_be_identifier = 1;
  lexer_flags.convert_spaces_to_comma = 1;

  if (lexer_flags.plotting && ! lexer_flags.in_plot_range)
    lexer_flags.past_plot_range = 1;

  yylval.tok_val = new token (value, yytext, input_line_number,
			      current_input_column);

  token_stack.push (yylval.tok_val);

  current_input_column += yyleng;

  do_comma_insert_check ();
}

// We have seen a backslash and need to find out if it should be
// treated as a continuation character.  If so, this eats it, up to
// and including the new line character.
//
// Match whitespace only, followed by a comment character or newline.
// Once a comment character is found, discard all input until newline.
// If non-whitespace characters are found before comment
// characters, return 0.  Otherwise, return 1.

static int
have_continuation (int trailing_comments_ok)
{
  ostrstream buf;

  int in_comment = 0;
  char c;
  while ((c = yyinput ()) != EOF)
    {
      buf << (char) c;

      switch (c)
	{
	case ' ':
	case '\t':
	  break;

	case '%':
	case '#':
	  if (trailing_comments_ok)
	    in_comment = 1;
	  else
	    goto cleanup;
	  break;

	case '\n':
	  current_input_column = 0;
	  promptflag--;
	  return 1;

	default:
	  if (! in_comment)
	    goto cleanup;
	  break;
	}
    }

  yyunput (c, yytext);
  return 0;

 cleanup:
  buf << ends;
  char *s = buf.str ();
  if (s)
    {
      int len = strlen (s);
      while (len--)
	yyunput (s[len], yytext);
    }
  delete [] s;
  return 0;
}

// We have seen a `.' and need to see if it is the start of a
// continuation.  If so, this eats it, up to and including the new
// line character.

static int
have_ellipsis_continuation (int trailing_comments_ok)
{
  char c1 = yyinput ();
  if (c1 == '.')
    {
      char c2 = yyinput ();
      if (c2 == '.' && have_continuation (trailing_comments_ok))
	return 1;
      else
	{
	  yyunput (c2, yytext);
	  yyunput (c1, yytext);
	}
    }
  else
    yyunput (c1, yytext);

  return 0;
}

// See if we have a continuation line.  If so, eat it and the leading
// whitespace on the next line.
//
// Return value is the same as described for eat_whitespace().

static yum_yum
eat_continuation (void)
{
  int retval = ATE_NOTHING;
  int c = yyinput ();
  if ((c == '.' && have_ellipsis_continuation ())
      || (c == '\\' && have_continuation ()))
    retval = eat_whitespace ();
  else
    yyunput (c, yytext);

  return retval;
}

static int
handle_string (char delim, int text_style)
{
  ostrstream buf;

  int c;
  int escape_pending = 0;

  while ((c = yyinput ()) != EOF)
    {
      current_input_column++;

      if (c == '\\')
	{
	  if (escape_pending)
	    {
	      buf << (char) c;
	      escape_pending = 0;
	    }
	  else
	    {
	      if (have_continuation (0))
		escape_pending = 0;
	      else
		{
		  buf << (char) c;
		  escape_pending = 1;
		}
	    }
	  continue;
	}
      else if (c == '.')
	{
	  if (! have_ellipsis_continuation (0))
	    buf << (char) c;
	}
      else if (c == '\n')
	{
	  error ("unterminated string constant");
	  break;
	}
      else if (c == delim)
	{
	  if (escape_pending)
	    buf << (char) c;
	  else
	    {
	      c = yyinput ();
	      if (c == delim)
		buf << (char) c;
	      else
		{
		  yyunput (c, yytext);
		  buf << ends;
		  char *tok = buf.str ();
		  do_string_escapes (tok);

		  if (text_style && lexer_flags.doing_set)
		    {
		      if (tok)
			{
			  int len = strlen (tok) + 3;
			  char *tmp = tok;
			  tok = new char [len];
			  tok[0] = delim;
			  strcpy (tok+1, tmp);
			  tok[len-2] = delim;
			  tok[len-1] = '\0';
			  delete [] tmp;
			}
		    }
		  else
		    {
		      lexer_flags.quote_is_transpose = 1;
		      lexer_flags.cant_be_identifier = 1;
		      lexer_flags.convert_spaces_to_comma = 1;
		    }

		  yylval.tok_val = new token (tok);
		  delete [] tok;
		  token_stack.push (yylval.tok_val);
		  return TEXT;
		}
	    }
	}
      else
	{
	  buf << (char) c;
	}

      escape_pending = 0;
    }

  return LEXICAL_ERROR;
}

static int
handle_close_brace (int spc_gobbled)
{
  if (! nesting_level.none ())
    {
      nesting_level.remove ();
      lexer_flags.braceflag--;
    }

  if (lexer_flags.braceflag == 0)
    BEGIN 0;

  int c1 = yyinput ();
  if (c1 == '=')
    {
      lexer_flags.quote_is_transpose = 0;
      lexer_flags.cant_be_identifier = 0;
      lexer_flags.convert_spaces_to_comma = 1;

      int c2 = yyinput ();
      unput (c2);
      unput (c1);

      if (c2 != '=' && lexer_flags.maybe_screwed_again)
	return SCREW_TWO;
      else
	return ']';
    }
  else
    {
      unput (c1);

      if (lexer_flags.braceflag && Vwhitespace_in_literal_matrix != 2)
	{
	  int bin_op = next_token_is_bin_op (spc_gobbled, yytext);
	  int postfix_un_op = next_token_is_postfix_unary_op
	    (spc_gobbled, yytext);

	  int other_op = match_any (c1, ",;\n]");

	  if (! (postfix_un_op || bin_op || other_op)
	      && nesting_level.is_brace ()
	      && lexer_flags.convert_spaces_to_comma)
	    {
	      unput (',');
	      return ']';
	    }
	}
    }

  lexer_flags.quote_is_transpose = 1;
  lexer_flags.cant_be_identifier = 0;
  lexer_flags.convert_spaces_to_comma = 1;
  return ']';
}

static void
maybe_unput_comma (int spc_gobbled)
{
  if (Vwhitespace_in_literal_matrix != 2
      && nesting_level.is_brace ())
    {
      int bin_op = next_token_is_bin_op (spc_gobbled, yytext);

      int postfix_un_op = next_token_is_postfix_unary_op (spc_gobbled,
							  yytext);

      int c1 = yyinput ();
      int c2 = yyinput ();
      unput (c2);
      unput (c1);
      int sep_op = match_any (c1, ",;\n]");
      int dot_op = (c1 == '.'
		    && (isalpha (c2) || isspace (c2) || c2 == '_'));
      int index_op = (c1 == '('
		      && (Vwhitespace_in_literal_matrix == 0
			  || ! spc_gobbled));

      if (! (postfix_un_op || bin_op || sep_op || dot_op || index_op))
	unput (',');
    }
}

// Figure out exactly what kind of token to return when we have seen
// an identifier.  Handles keywords.

static int
handle_identifier (const string& tok, int spc_gobbled)
{
  // It is almost always an error for an identifier to be followed
  // directly by another identifier.  Special cases are handled
  // below.

  lexer_flags.cant_be_identifier = 1;

  // If we are expecting a structure element, we just want to return
  // TEXT_ID, which is a string that is also a valid identifier.  But
  // first, we have to decide whether to insert a comma.

  if (lexer_flags.looking_at_indirect_ref)
    {
      maybe_unput_comma (spc_gobbled);
      TOK_PUSH_AND_RETURN (tok, TEXT_ID);
    }

  // If we have a regular keyword, or a plot STYLE, return it.
  // Keywords can be followed by identifiers (TOK_RETURN handles
  // that).

  int kw_token = is_keyword (tok);
  if (kw_token)
    {
      if (kw_token == STYLE)
	{
	  current_input_column += yyleng;
	  lexer_flags.quote_is_transpose = 0;
	  lexer_flags.convert_spaces_to_comma = 1;
	  return kw_token;
	}
      else
	TOK_RETURN (kw_token);
    }

  // See if we have a plot keyword (title, using, with, or clear).

  if (lexer_flags.plotting)
    {
      // Yes, we really do need both of these plot_range variables.
      // One is used to mark when we are past all possiblity of a plot
      // range, the other is used to mark when we are actually between
      // the square brackets that surround the range.

      if (! lexer_flags.in_plot_range)
	lexer_flags.past_plot_range = 1;

      // Option keywords can't appear in parentheses or braces.

      int plot_option_kw = 0;
      if (nesting_level.none ())
	plot_option_kw = is_plot_keyword (tok);

      if (lexer_flags.cant_be_identifier && plot_option_kw)
	TOK_RETURN (plot_option_kw);
    }

  int c = yyinput ();
  yyunput (c, yytext);
  bool next_tok_is_eq = (c == '=');
  bool next_tok_is_paren = (c == '(');

  // Make sure we put the return values of a function in the symbol
  // table that is local to the function.

  if (next_tok_is_eq
      && lexer_flags.defining_func && lexer_flags.maybe_screwed)
    curr_sym_tab = tmp_local_sym_tab;

  // Kluge alert.
  //
  // If we are looking at a text style function, set up to gobble its
  // arguments.  If the following token is `=', force the symbol to be
  // inserted as a variable in the current symbol table.

  if (is_text_function_name (tok) && ! is_variable (tok))
    {
      if (next_tok_is_eq)
	force_local_variable (tok);
      else if (! next_tok_is_paren)
	{
	  if (tok == "gset")
	    lexer_flags.doing_set = 1;

	  BEGIN TEXT_FCN;
	}
    }

  // Find the token in the symbol table.

  yylval.tok_val = new token (lookup_identifier (tok),
			      input_line_number,
			      current_input_column);

  token_stack.push (yylval.tok_val);

  // After seeing an identifer, it is ok to convert spaces to a comma
  // (if needed).

  lexer_flags.convert_spaces_to_comma = 1;

  // If we are defining a function and we have not seen the parameter
  // list yet and the next token is `=', return a token that
  // represents the only return value for the function.  For example,
  //
  //   function SCREW = f (args);
  //
  // The variable maybe_screwed is reset in parse.y.

  if (next_tok_is_eq)
    {
      current_input_column += yyleng;
      if (lexer_flags.defining_func && lexer_flags.maybe_screwed)
	return SCREW;
      else
	return NAME;
    }

  // At this point, we are only dealing with identifiers that are not
  // followed by `=' (if the next token is `=', there is no need to
  // check to see if we should insert a comma (invalid syntax), or
  // allow a following `'' to be treated as a transpose (the next
  // token is `=', so it can't be `''.

  lexer_flags.quote_is_transpose = 1;
  do_comma_insert_check ();

  maybe_unput_comma (spc_gobbled);

  current_input_column += yyleng;
  return NAME;
}

// Print a warning if a function file that defines a function has
// anything other than comments and whitespace following the END token
// that matches the FUNCTION statement.

void
check_for_garbage_after_fcn_def (void)
{
  // By making a newline be the next character to be read, we will
  // force the parser to return after reading the function.  Calling
  // yyunput with EOF seems not to work...

  int in_comment = 0;
  int lineno = input_line_number;
  int c;
  while ((c = yyinput ()) != EOF)
    {
      switch (c)
	{
	case ' ':
	case '\t':
	case ';':
	case ',':
	  break;

	case '\n':
	  if (in_comment)
	    in_comment = 0;
	  break;

	case '%':
	case '#':
	  in_comment = 1;
	  break;

	default:
	  if (in_comment)
	    break;
	  else
	    {
	      warning ("ignoring trailing garbage after end of function\n\
         near line %d of file `%s.m'", lineno, curr_fcn_file_name.c_str ());
	      
	      yyunput ('\n', yytext);
	      return;
	    }
	}
    }
  yyunput ('\n', yytext);
}

void
lexical_feedback::init (void)
{
  // Not initially defining a function.
  beginning_of_function = 0;
  defining_func = 0;

  // Not initially defining a matrix list.
  braceflag = 0;

  // Next token can be identifier.
  cant_be_identifier = 0;

  // No need to do comma insert or convert spaces to comma at
  // beginning of input. 
  convert_spaces_to_comma = 1;
  do_comma_insert = 0;

  // Not initially doing any plotting or setting of plot attributes.
  doing_set = 0;
  in_plot_range = 0;
  in_plot_style = 0;
  in_plot_using = 0;
  past_plot_range = 0;
  plotting = 0;

  // Not initially inside a loop or if statement.
  iffing = 0;
  looping = 0;

  // Not initially looking at indirect references.
  looking_at_indirect_ref = 0;

  // Not initially screwed by `function [...] = f (...)' syntax.
  maybe_screwed = 0;
  maybe_screwed_again = 0;

  // Quote marks strings intially.
  quote_is_transpose = 0;
}

int
whitespace_in_literal_matrix (void)
{
  int pref = 0;
  string val = builtin_string_variable ("whitespace_in_literal_matrix");
  if (! val.empty ())
    {
      if (val.compare ("ignore", 0, 6) == 0)
	pref = 2;
      else if (val.compare ("traditional", 0, 11) == 0)
	pref = 1;
    }
  Vwhitespace_in_literal_matrix = pref;
  return 0;
}

void
symbols_of_lex (void)
{
  DEFVAR (whitespace_in_literal_matrix, "", 0, whitespace_in_literal_matrix,
    "control auto-insertion of commas and semicolons in literal matrices");
}

// Maybe someday...
//
// "+="		return ADD_EQ;
// "-="		return SUB_EQ;
// "*="		return MUL_EQ;
// "/="		return DIV_EQ;
// "\\="	return LEFTDIV_EQ;
// ".+="	return ADD_EQ;
// ".-="	return SUB_EQ;
// ".*="	return EMUL_EQ;
// "./="	return EDIV_EQ;
// ".\\="	return ELEFTDIV_EQ;

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
