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

#include <strstream>
#include <string>

#include "SLStack.h"

#include "cmd-edit.h"

// These would be alphabetical, but y.tab.h must be included before
// oct-gperf.h and y.tab.h must be included after token.h and the tree
// class declarations.  We can't include y.tab.h in oct-gperf.h
// because it may not be protected to allow it to be included multiple
// times.

#include "comment-list.h"
#include "defun.h"
#include "error.h"
#include "input.h"
#include "lex.h"
#include "ov.h"
#include "parse.h"
#include "pt-all.h"
#include "symtab.h"
#include "token.h"
#include "toplev.h"
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

// Is the closest nesting level a square bracket, squiggly brace or a paren?

class bracket_brace_paren_nesting_level : public SLStack <int>
{
public:

  bracket_brace_paren_nesting_level (void) : SLStack<int> () { }

  ~bracket_brace_paren_nesting_level (void) { }

  void bracket (void) { push (BRACKET); }
  bool is_bracket (void) { return ! empty () && top () == BRACKET; }

  void brace (void) { push (BRACE); }
  bool is_brace (void) { return ! empty () && top () == BRACE; }

  void paren (void) { push (PAREN); }
  bool is_paren (void) { return ! empty () && top () == PAREN; }

  bool none (void) { return empty (); }

  void remove (void) { if (! empty ()) SLStack<int>::pop (); }

private:

  enum { BRACKET = 1, BRACE = 2, PAREN = 3 };

  bracket_brace_paren_nesting_level (const bracket_brace_paren_nesting_level&);

  bracket_brace_paren_nesting_level&
  operator = (const bracket_brace_paren_nesting_level&);
};

static bracket_brace_paren_nesting_level nesting_level;

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

static bool Vwarn_separator_insert = false;

static bool Vwarn_single_quote_string = false;

// Forward declarations for functions defined at the bottom of this
// file.

static void fixup_column_count (char *s);
static void do_comma_insert_check (void);
static int is_plot_keyword (const std::string& s);
static int is_keyword (const std::string& s);
static std::string plot_style_token (const std::string& s);
static symbol_record *lookup_identifier (const std::string& s);
static void grab_help_text (void);
static bool match_any (char c, const char *s);
static bool next_token_is_sep_op (void);
static bool next_token_is_bin_op (bool spc_prev);
static bool next_token_is_postfix_unary_op (bool spc_prev);
static std::string strip_trailing_whitespace (char *s);
static void handle_number (void);
static int handle_string (char delim, int text_style = 0);
static int handle_close_bracket (int spc_gobbled);
static int handle_identifier (const std::string& tok, int spc_gobbled);
static bool have_continuation (bool trailing_comments_ok = true);
static bool have_ellipsis_continuation (bool trailing_comments_ok = true);
static void scan_for_comments (const char *);
static yum_yum eat_whitespace (void);
static yum_yum eat_continuation (void);
static void maybe_warn_separator_insert (char sep);
static void gripe_single_quote_string (void);

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
NUMBER	(({D}+\.?{D}*{EXPON}?)|(\.{D}+{EXPON}?)|(0[xX][0-9a-fA-F]+))
%%

%{
// Help and other text-style functions are a pain in the ass.  This
// stuff needs to be simplified.  May require some changes in the
// parser too.
%}

<TEXT_FCN>{NL} {
    BEGIN 0;
    current_input_column = 1;
    lexer_flags.quote_is_transpose = false;
    lexer_flags.cant_be_identifier = false;
    lexer_flags.convert_spaces_to_comma = true;
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
    return handle_string (yytext[0], true);
  }

<TEXT_FCN>[^ \t\n\;\,\"\'][^ \t\n\;\,]*{S}* {
    std::string tok = strip_trailing_whitespace (yytext);
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
    scan_for_comments (yytext);
    fixup_column_count (yytext);
    int c = yytext[yyleng-1];
    int cont_is_spc = eat_continuation ();
    int spc_gobbled = (cont_is_spc || c == ' ' || c == '\t');
    return handle_close_bracket (spc_gobbled);
  }

%{
// Commas are element separators in matrix constants.  If we don't
// check for continuations here we can end up inserting too many
// commas.
%}

<MATRIX>{S}*\,{S}* {
    current_input_column += yyleng;

    int tmp = eat_continuation ();

    lexer_flags.quote_is_transpose = false;
    lexer_flags.cant_be_identifier = false;
    lexer_flags.convert_spaces_to_comma = true;

    if ((tmp & ATE_NEWLINE) == ATE_NEWLINE)
      {
	maybe_warn_separator_insert (';');

	if (Vwhitespace_in_literal_matrix != 2)
	  unput (';');
      }

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

    int tmp = eat_continuation ();
    int bin_op = next_token_is_bin_op (true);
    int postfix_un_op = next_token_is_postfix_unary_op (true);

    if (! (postfix_un_op || bin_op)
	&& nesting_level.is_bracket ()
	&& lexer_flags.convert_spaces_to_comma)
      {
	if ((tmp & ATE_NEWLINE) == ATE_NEWLINE)
	  {
	    maybe_warn_separator_insert (';');

	    if (Vwhitespace_in_literal_matrix != 2)
	      unput (';');
	  }

	if (Vwhitespace_in_literal_matrix != 2)
	  {
	    lexer_flags.quote_is_transpose = false;
	    lexer_flags.cant_be_identifier = false;
	    lexer_flags.convert_spaces_to_comma = true;

	    maybe_warn_separator_insert (',');

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
    scan_for_comments (yytext);
    fixup_column_count (yytext);
    eat_whitespace ();
    lexer_flags.quote_is_transpose = false;
    lexer_flags.cant_be_identifier = false;
    lexer_flags.convert_spaces_to_comma = true;
    return ';';
  }

%{
// In some cases, new lines can also become row separators.  If we
// don't eat whitespace here we can end up inserting too many
// semicolons.
%}

<MATRIX>{S}*{COMMENT}{SNLCMT}* |
<MATRIX>{S}*{NL}{SNLCMT}* {
    scan_for_comments (yytext);
    fixup_column_count (yytext);
    eat_whitespace ();

    if (Vwhitespace_in_literal_matrix != 2)
      {
	lexer_flags.quote_is_transpose = false;
	lexer_flags.cant_be_identifier = false;
	lexer_flags.convert_spaces_to_comma = true;

	if (nesting_level.none ())
	  return LEXICAL_ERROR;
      }

    if (nesting_level.is_bracket ())
      {
	maybe_warn_separator_insert (';');

	if (Vwhitespace_in_literal_matrix != 2)
	  return ';';
      }
  }

%{
// Open and close bracket are handled differently if we are in the range
// part of a plot command.
//
%}

\[{S}* {
    nesting_level.bracket ();

    current_input_column += yyleng;
    lexer_flags.quote_is_transpose = false;
    lexer_flags.cant_be_identifier = false;
    lexer_flags.convert_spaces_to_comma = true;

    promptflag--;
    eat_whitespace ();

    if (lexer_flags.plotting && ! lexer_flags.past_plot_range)
      {
	lexer_flags.in_plot_range = true;
	return OPEN_BRACE;
      }
    else
      {
	lexer_flags.bracketflag++;
	BEGIN MATRIX;
	return '[';
      }
  }

\] {
    nesting_level.remove ();

    if (lexer_flags.plotting && ! lexer_flags.past_plot_range)
      {
	lexer_flags.in_plot_range = false;
	TOK_RETURN (CLOSE_BRACE);
      }
    else
      TOK_RETURN (']');
  }

%{
// Imaginary numbers.
%}

{NUMBER}{Im} {
    handle_number ();
    return IMAG_NUM;
  }

%{
// Real numbers.  Don't grab the `.' part of a dot operator as part of
// the constant.
%}

{D}+/\.[\*/\\^'] |
{NUMBER} {
    handle_number ();
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
    scan_for_comments (yytext);
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
    std::string tok = strip_trailing_whitespace (yytext);
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
    lexer_flags.quote_is_transpose = false;
    lexer_flags.cant_be_identifier = false;
    lexer_flags.convert_spaces_to_comma = true;

    if (nesting_level.none ())
      return '\n';

    if (nesting_level.is_bracket ())
      return LEXICAL_ERROR;
  }

%{
// Single quote can either be the beginning of a string or a transpose
// operator. 
%}

"'" {
    current_input_column++;
    lexer_flags.convert_spaces_to_comma = true;

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
      BIN_OP_RETURN (COLON, true);
    else
      BIN_OP_RETURN (':', false);
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
	lexer_flags.beginning_of_function = false;

	grab_help_text ();

	octave_comment_buffer::append (help_buf);
      }
    else
      {
	std::string buf;

	bool begin_comment = true;

	int c;
	while ((c = yyinput ()) != EOF && c != '\n')
	  {
	    if (begin_comment && (c == '#' || c == '%'))
	      ; /* Skip leading comment characters. */
	    else
	      buf += (char) c;
	  }

	octave_comment_buffer::append (buf);
      }

    current_input_column = 1;
    lexer_flags.quote_is_transpose = false;
    lexer_flags.cant_be_identifier = false;
    lexer_flags.convert_spaces_to_comma = true;

    if (nesting_level.none ())
      return '\n';
    else if (nesting_level.is_bracket ())
      return ';';
  }

%{
// Other operators.
%}

".+"	{ BIN_OP_RETURN (EPLUS, false); }
".-"	{ BIN_OP_RETURN (EMINUS, false); }
".*"	{ BIN_OP_RETURN (EMUL, false); }
"./"	{ BIN_OP_RETURN (EDIV, false); }
".\\"	{ BIN_OP_RETURN (ELEFTDIV, false); }
{EPOW}	{ BIN_OP_RETURN (EPOW, false); }
".'"	{ do_comma_insert_check (); BIN_OP_RETURN (TRANSPOSE, true); }
"++"	{ do_comma_insert_check (); BIN_OP_RETURN (PLUS_PLUS, true); }
"--"	{ do_comma_insert_check (); BIN_OP_RETURN (MINUS_MINUS, true); }
"<="	{ BIN_OP_RETURN (EXPR_LE, false); }
"=="	{ BIN_OP_RETURN (EXPR_EQ, false); }
{NOTEQ}	{ BIN_OP_RETURN (EXPR_NE, false); }
">="	{ BIN_OP_RETURN (EXPR_GE, false); }
"&"	{ BIN_OP_RETURN (EXPR_AND, false); }
"|"	{ BIN_OP_RETURN (EXPR_OR, false); }
"<"	{ BIN_OP_RETURN (EXPR_LT, false); }
">"	{ BIN_OP_RETURN (EXPR_GT, false); }
"*"	{ BIN_OP_RETURN ('*', false); }
"/"	{ BIN_OP_RETURN ('/', false); }
"\\"	{ BIN_OP_RETURN (LEFTDIV, false); }
";"	{ BIN_OP_RETURN (';', true); }
","	{ BIN_OP_RETURN (',', true); }
{POW}	{ BIN_OP_RETURN (POW, false); }
"="	{ BIN_OP_RETURN ('=', true); }
"&&"	{ BIN_OP_RETURN (EXPR_AND_AND, false); }
"||"	{ BIN_OP_RETURN (EXPR_OR_OR, false); }
"<<"	{ BIN_OP_RETURN (LSHIFT, false); }
">>"	{ BIN_OP_RETURN (RSHIFT, false); }

{NOT} {
    if (lexer_flags.plotting && ! lexer_flags.in_plot_range)
      lexer_flags.past_plot_range = true;
    BIN_OP_RETURN (EXPR_NOT, false);
  }

"+" { 
    if (lexer_flags.plotting && ! lexer_flags.in_plot_range)
      lexer_flags.past_plot_range = true;
    BIN_OP_RETURN ('+', false);
  }

"-" { 
    if (lexer_flags.plotting && ! lexer_flags.in_plot_range)
      lexer_flags.past_plot_range = true;
    BIN_OP_RETURN ('-', false);
  }

"(" {
    if (lexer_flags.plotting && ! lexer_flags.in_plot_range)
      lexer_flags.past_plot_range = true;
    nesting_level.paren ();
    promptflag--;
    TOK_RETURN ('(');
  }

")" {
    nesting_level.remove ();

    current_input_column++;
    lexer_flags.cant_be_identifier = true;
    lexer_flags.quote_is_transpose = true;
    lexer_flags.convert_spaces_to_comma = nesting_level.is_bracket ();
    do_comma_insert_check ();
    return ')';
  }

"." {
    TOK_RETURN ('.');
  }

"+="	{ BIN_OP_RETURN (ADD_EQ, false); }
"-="	{ BIN_OP_RETURN (SUB_EQ, false); }
"*="	{ BIN_OP_RETURN (MUL_EQ, false); }
"/="	{ BIN_OP_RETURN (DIV_EQ, false); }
"\\="	{ BIN_OP_RETURN (LEFTDIV_EQ, false); }
".+="	{ BIN_OP_RETURN (ADD_EQ, false); }
".-="	{ BIN_OP_RETURN (SUB_EQ, false); }
".*="	{ BIN_OP_RETURN (EMUL_EQ, false); }
"./="	{ BIN_OP_RETURN (EDIV_EQ, false); }
".\\="	{ BIN_OP_RETURN (ELEFTDIV_EQ, false); }
"&="	{ BIN_OP_RETURN (AND_EQ, false); }
"|="	{ BIN_OP_RETURN (OR_EQ, false); }
"<<="	{ BIN_OP_RETURN (LSHIFT_EQ, false); }
">>="	{ BIN_OP_RETURN (RSHIFT_EQ, false); }

"{" {
    nesting_level.brace ();
    promptflag--;
    TOK_RETURN ('{');
  }

"}" {
    nesting_level.remove ();

    current_input_column++;
    lexer_flags.cant_be_identifier = true;
    lexer_flags.quote_is_transpose = true;
    lexer_flags.convert_spaces_to_comma = nesting_level.is_bracket ();
    do_comma_insert_check (); // Is this really necessary?

    return '}';
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

  unput (c);

  if (spc_gobbled)
    unput (' ');

  lexer_flags.do_comma_insert = (lexer_flags.bracketflag && c == '[');
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
  warning_state = 0;

  // We do want a prompt by default.
  promptflag = 1;

  // Error may have occurred inside some brackets, braces, or parentheses.
  nesting_level.clear ();

  // Clear out the stack of token info used to track line and column
  // numbers.
  while (! token_stack.empty ())
    delete token_stack.pop ();

  // Can be reset by defining a function.
  if (! (reading_script_file || reading_fcn_file))
    {
      current_input_column = 1;
      input_line_number = command_editor::current_command_number () - 1;
    }

  // Only ask for input from stdin if we are expecting interactive
  // input.
  if ((interactive || forced_interactive)
      && ! (reading_fcn_file || get_input_from_eval_string
	    || input_from_startup_file))
    yyrestart (stdin);

  // Clear the buffer for help text.
  help_buf.resize (0);

  // Reset other flags.
  lexer_flags.init ();
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
int
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
  switch_to_buffer (static_cast<YY_BUFFER_STATE> (buf));
}

// Delete a buffer (for unwind-prot).

void
delete_input_buffer (void *buf)
{
  delete_buffer (static_cast<YY_BUFFER_STATE> (buf));
}

// Check to see if a character string matches any of the possible line
// styles for plots.

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

// Check to see if a character string matches any of the possible axes
// tags for plots.

static std::string
plot_axes_token (const std::string& s)
{
  std::string retval;

  // XXX FIXME XXX -- specify minimum match length for these.
  static const char *plot_axes[] = 
    {
      "x1y1",
      "x1y2",
      "x2y1",
      "x2y2",
      0,
    };

  const char **tmp = plot_axes;
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
is_plot_keyword (const std::string& s)
{
  const char *t = s.c_str ();
  if (almost_match ("title", t, 1))
    {
      return TITLE;
    }
  else if (almost_match ("using", t, 1))
    {
      lexer_flags.in_plot_using = true;
      return USING;
    }
  else if (almost_match ("with", t, 1))
    {
      lexer_flags.in_plot_style = true;
      return WITH;
    }
  else if (almost_match ("axes", t, 2) || almost_match ("axis", t, 2))
    {
      lexer_flags.in_plot_axes = true;
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

// Handle keywords.

static int
is_keyword (const std::string& s)
{
  if (lexer_flags.plotting)
    {
      if (lexer_flags.in_plot_style)
	{
	  std::string sty = plot_style_token (s);

	  if (! sty.empty ())
	    {
	      lexer_flags.in_plot_style = false;
	      yylval.tok_val = new token (sty);
	      token_stack.push (yylval.tok_val);
	      return STYLE;
	    }
	}
      else if (lexer_flags.in_plot_axes)
	{
	  std::string axes = plot_axes_token (s);

	  if (! axes.empty ())
	    {
	      lexer_flags.in_plot_axes = false;
	      yylval.tok_val = new token (axes);
	      token_stack.push (yylval.tok_val);
	      return AXES_TAG;
	    }
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
	case case_kw:
	case catch_kw:
	case continue_kw:
	case else_kw:
	case elseif_kw:
	case global_kw:
	case otherwise_kw:
	case return_kw:
	case static_kw:
	case until_kw:
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

	case endswitch_kw:
	  yylval.tok_val = new token (token::switch_end, l, c);
	  break;

	case endwhile_kw:
	  yylval.tok_val = new token (token::while_end, l, c);
	  break;

	case do_kw:
	case for_kw:
	case while_kw:
	  promptflag--;
	  lexer_flags.looping++;
	  break;

	case if_kw:
	case try_kw:
	case switch_kw:
	case unwind_protect_kw:
	  promptflag--;
	  break;

	case gplot_kw:
	  lexer_flags.plotting = true;
	  yylval.tok_val = new token (token::two_dee, l, c);
	  break;

	case gsplot_kw:
	  lexer_flags.plotting = true;
	  yylval.tok_val = new token (token::three_dee, l, c);
	  break;

	case replot_kw:
	  lexer_flags.plotting = true;
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
	      // Prepare for local symbols.

	      tmp_local_sym_tab = new symbol_table ();

	      promptflag--;

	      lexer_flags.defining_func = true;
	      lexer_flags.parsed_function_name = false;
	      lexer_flags.beginning_of_function = true;

	      if (! (reading_fcn_file || reading_script_file))
		input_line_number = 1;
	    }
	  break;

        case magic_file_kw:
	  {
	    if ((reading_fcn_file || reading_script_file)
		&& ! curr_fcn_file_full_name.empty ())
	      yylval.tok_val = new token (curr_fcn_file_full_name, l, c);
	    else
	      yylval.tok_val = new token ("stdin", l, c);
	  }
	  break;

        case magic_line_kw:
	  yylval.tok_val = new token (static_cast<double> (l), "", l, c);
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
lookup_identifier (const std::string& name)
{
  return curr_sym_tab->lookup (name, true);
}

static bool
is_variable (const std::string& name)
{
  symbol_record *sr = curr_sym_tab->lookup (name);

  return sr && sr->is_variable ();
}

static void
force_local_variable (const std::string& name)
{
  symbol_record *sr = curr_sym_tab->lookup (name, true);

  if (sr)
    sr->define (octave_value ());
}

// Grab the help text from an function file.  Always overwrites the
// current contents of help_buf.

// XXX FIXME XXX -- gobble_leading_white_space() in parse.y
// duplicates some of this code!

static void
grab_help_text (void)
{
  help_buf.resize (0);

  bool begin_comment = true;
  bool in_comment = true;
  bool discard_space = true;

  int c = 0;

  while ((c = yyinput ()) != EOF)
    {
      if (begin_comment)
	{
	  if (c == '%' || c == '#')
	    continue;
	  else if (discard_space && c == ' ')
	    {
	      discard_space = false;
	      continue;
	    }
	  else
	    begin_comment = false;
	}	

      if (in_comment)
	{
	  help_buf += (char) c;

	  if (c == '\n')
	    {
	      in_comment = false;
	      discard_space = true;
	    }
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
    unput (c);
}

// Return 1 if the given character matches any character in the given
// string.

static bool
match_any (char c, const char *s)
{
  char tmp;
  while ((tmp = *s++) != '\0')
    {
      if (c == tmp)
	return true;
    }
  return false;
}

// Given information about the spacing surrounding an operator,
// return 1 if it looks like it should be treated as a binary
// operator.  For example,
//
//   [ 1 + 2 ]  or  [ 1+ 2]  or  [ 1+2 ]  ==>  binary
//
//   [ 1 +2 ]  ==>  unary

static bool
looks_like_bin_op (bool spc_prev, int next_char)
{
  bool spc_next = (next_char == ' ' || next_char == '\t');

  return ((spc_prev && spc_next) || ! spc_prev);
}

// Recognize separators.  If the separator is a CRLF pair, it is
// replaced by a single LF.

static bool
next_token_is_sep_op (void)
{
  bool retval = false;

  int c1 = yyinput ();

  if (c1 == '\r')
    {
      int c2 = yyinput ();

      if (c2 == '\n')
	{
	  c1 = '\n';

	  retval = true;
	}
      else
	unput (c2);
    }
  else
    retval = match_any (c1, ",;\n]");

  unput (c1);

  return retval;
}

// Try to determine if the next token should be treated as a postfix
// unary operator.  This is ugly, but it seems to do the right thing.

static bool
next_token_is_postfix_unary_op (bool spc_prev)
{
  bool un_op = false;

  int c0 = yyinput ();

  if (c0 == '\'' && ! spc_prev)
    {
      un_op = true;
    }
  else if (c0 == '.')
    {
      int c1 = yyinput ();
      un_op = (c1 == '\'');
      unput (c1);
    }

  unput (c0);

  return un_op;
}

// Try to determine if the next token should be treated as a binary
// operator.
//
// This kluge exists because whitespace is not always ignored inside
// the square brackets that are used to create matrix objects (though
// spacing only really matters in the cases that can be interpreted
// either as binary ops or prefix unary ops: currently just +, -).
//
// Note that a line continuation directly following a + or - operator
// (e.g., the characters '[' 'a' ' ' '+' '\' LFD 'b' ']') will be
// parsed as a binary operator.

static bool
next_token_is_bin_op (bool spc_prev)
{
  bool bin_op = false;

  int c0 = yyinput ();

  switch (c0)
    {
    case '+':
    case '-':
      {
	int c1 = yyinput ();

	switch (c1)
	  {
	  case '+':
	  case '-':
	    // Unary ops, spacing doesn't matter.
	    break;

	  case '=':
	    // Binary ops, spacing doesn't matter.
	    bin_op = true;
	    break;

	  default:
	    // Could be either, spacing matters.
	    bin_op = looks_like_bin_op (spc_prev, c1);
	    break;
	  }

	unput (c1);
      }
      break;

    case ':':
    case '/':
    case '\\':
    case '^':
      // Always a binary op (may also include /=, \=, and ^=).
      bin_op = true;
      break;

    // .+ .- ./ .\ .^ .* .**
    case '.':
      {
	int c1 = yyinput ();

	if (match_any (c1, "+-/\\^*"))
	  // Always a binary op (may also include .+=, .-=, ./=, ...).
	  bin_op = true;
	else if (! isdigit (c1) && c1 != ' ' && c1 != '\t' && c1 != '.')
	  // A structure element reference is a binary op.
	  bin_op = true;

	unput (c1);
      }
      break;

    // = == & && | || * **
    case '=':
    case '&':
    case '|':
    case '*':
      // Always a binary op (may also include ==, &&, ||, **).
      bin_op = true;
      break;

    // < <= <> > >=
    case '<':
    case '>':
      // Always a binary op (may also include <=, <>, >=).
      bin_op = true;
      break;

    // ~= !=
    case '~':
    case '!':
      {
	int c1 = yyinput ();

	// ~ and ! can be unary ops, so require following =.
	if (c1 == '=')
	  bin_op = true;

	unput (c1);
      }
      break;

    default:
      break;
    }

  unput (c0);

  return bin_op;
}

// Used to delete trailing white space from tokens.

static std::string
strip_trailing_whitespace (char *s)
{
  std::string retval = s;

  size_t pos = retval.find_first_of (" \t");

  if (pos != NPOS)
    retval.resize (pos);

  return retval;
}

static void
scan_for_comments (const char *text)
{
  std::string comment_buf;

  bool in_comment = false;
  bool beginning_of_comment = false;

  int len = strlen (text);
  int i = 0;

  while (i < len)
    {
      char c = text[i++];

      switch (c)
	{
	case '%':
	case '#':
	  if (in_comment)
	    {
	      if (! beginning_of_comment)
		comment_buf += (char) c;
	    }
	  else
	    {
	      in_comment = true;
	      beginning_of_comment = true;
	    }
	  break;

	case '\n':
	  if (in_comment)
	    {
	      comment_buf += (char) c;
	      octave_comment_buffer::append (comment_buf);
	      comment_buf.resize (0);
	      in_comment = false;
	      beginning_of_comment = false;
	    }
	  break;

	case '\r':
	  if (in_comment)
	    comment_buf += (char) c;
	  if (i < len)
	    {
	      c = text[i++];

	      if (c == '\n')
		{
		  if (in_comment)
		    {
		      comment_buf += (char) c;
		      octave_comment_buffer::append (comment_buf);
		      in_comment = false;
		      beginning_of_comment = false;
		    }
		}
	    }

	default:
	  if (in_comment)
	    {
	      comment_buf += (char) c;
	      beginning_of_comment = false;
	    }
	  break;
	}
    }

  if (! comment_buf.empty ())
    octave_comment_buffer::append (comment_buf);
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

  std::string comment_buf;

  bool in_comment = false;
  bool beginning_of_comment = false;

  int c = 0;

  while ((c = yyinput ()) != EOF)
    {
      current_input_column++;

      switch (c)
	{
	case ' ':
	case '\t':
	  if (in_comment)
	    {
	      comment_buf += (char) c;
	      beginning_of_comment = false;
	    }
	  retval |= ATE_SPACE_OR_TAB;
	  break;

	case '\n':
	  retval |= ATE_NEWLINE;
	  if (in_comment)
	    {
	      comment_buf += (char) c;
	      octave_comment_buffer::append (comment_buf);
	      comment_buf.resize (0);
	      in_comment = false;
	      beginning_of_comment = false;
	    }
	  current_input_column = 0;
	  break;

	case '#':
	case '%':
	  if (in_comment)
	    {
	      if (! beginning_of_comment)
		comment_buf += (char) c;
	    }
	  else
	    {
	      in_comment = true;
	      beginning_of_comment = true;
	    }
	  break;

	case '.':
	  if (in_comment)
	    {
	      comment_buf += (char) c;
	      beginning_of_comment = false;
	      break;
	    }
	  else
	    {
	      if (have_ellipsis_continuation ())
		break;
	      else
		goto done;
	    }

	case '\\':
	  if (in_comment)
	    {
	      comment_buf += (char) c;
	      beginning_of_comment = false;
	      break;
	    }
	  else
	    {
	      if (have_continuation ())
		break;
	      else
		goto done;
	    }

	default:
	  if (in_comment)
	    {
	      comment_buf += (char) c;
	      beginning_of_comment = false;
	      break;
	    }
	  else
	    goto done;
	}
    }

  if (! comment_buf.empty ())
    octave_comment_buffer::append (comment_buf);

 done:
  unput (c);
  current_input_column--;
  return retval;
}

static inline bool
looks_like_hex (const char *s, int len)
{
  return (len > 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X'));
}

static void
handle_number (void)
{
  double value = 0.0;
  int nread = 0;

  if (looks_like_hex (yytext, strlen (yytext)))
    {
      unsigned long ival;

      nread = sscanf (yytext, "%lx", &ival);

      value = static_cast<double> (ival);
    }
  else
    {
      char *tmp = strsave (yytext);

      char *idx = strpbrk (tmp, "Dd");

      if (idx)
	*idx = 'e';

      nread = sscanf (tmp, "%lf", &value);

      delete [] tmp;
    }

  // If yytext doesn't contain a valid number, we are in deep doo doo.

  assert (nread == 1);

  lexer_flags.quote_is_transpose = 1;
  lexer_flags.cant_be_identifier = 1;
  lexer_flags.convert_spaces_to_comma = 1;

  if (lexer_flags.plotting && ! lexer_flags.in_plot_range)
    lexer_flags.past_plot_range = true;

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

static bool
have_continuation (bool trailing_comments_ok)
{
  std::ostrstream buf;

  std::string comment_buf;

  bool in_comment = false;
  bool beginning_of_comment = false;

  int c = 0;

  while ((c = yyinput ()) != EOF)
    {
      buf << (char) c;

      switch (c)
	{
	case ' ':
	case '\t':
	  if (in_comment)
	    {
	      comment_buf += (char) c;
	      beginning_of_comment = false;
	    }
	  break;

	case '%':
	case '#':
	  if (trailing_comments_ok)
	    {
	      if (in_comment)
		{
		  if (! beginning_of_comment)
		    comment_buf += (char) c;
		}
	      else
		{
		  in_comment = true;
		  beginning_of_comment = true;
		}
	    }
	  else
	    goto cleanup;
	  break;

	case '\n':
	  if (in_comment)
	    {
	      comment_buf += (char) c;
	      octave_comment_buffer::append (comment_buf);
	    }
	  current_input_column = 0;
	  promptflag--;
	  return true;

	case '\r':
	  if (in_comment)
	    comment_buf += (char) c;
	  c = yyinput ();
	  if (c == EOF)
	    break;
	  else if (c == '\n')
	    {
	      if (in_comment)
		{
		  comment_buf += (char) c;
		  octave_comment_buffer::append (comment_buf);
		}
	      current_input_column = 0;
	      promptflag--;
	      return true;
	    }

	default:
	  if (in_comment)
	    {
	      comment_buf += (char) c;
	      beginning_of_comment = false;
	    }
	  else
	    goto cleanup;
	  break;
	}
    }

  unput (c);
  return false;

cleanup:
  buf << std::ends;
  char *s = buf.str ();
  if (s)
    {
      int len = strlen (s);
      while (len--)
	unput (s[len]);
    }
  delete [] s;

  return false;
}

// We have seen a `.' and need to see if it is the start of a
// continuation.  If so, this eats it, up to and including the new
// line character.

static bool
have_ellipsis_continuation (bool trailing_comments_ok)
{
  char c1 = yyinput ();
  if (c1 == '.')
    {
      char c2 = yyinput ();
      if (c2 == '.' && have_continuation (trailing_comments_ok))
	return true;
      else
	{
	  unput (c2);
	  unput (c1);
	}
    }
  else
    unput (c1);

  return false;
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
    unput (c);

  return retval;
}

static int
handle_string (char delim, int text_style)
{
  std::ostrstream buf;

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
	      if (have_continuation (false))
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
	  if (! have_ellipsis_continuation (false))
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
		  unput (c);
		  buf << std::ends;
		  char *t = buf.str ();
		  std::string s = do_string_escapes (t);
		  delete [] t;

		  if (text_style && lexer_flags.doing_set)
		    {
		      s = std::string (1, delim) + s + std::string (1, delim);
		    }
		  else
		    {
		      lexer_flags.quote_is_transpose = true;
		      lexer_flags.cant_be_identifier = true;
		      lexer_flags.convert_spaces_to_comma = true;
		    }

		  yylval.tok_val = new token (s);
		  token_stack.push (yylval.tok_val);

		  if (delim == '\'')
		    gripe_single_quote_string ();

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

static bool
next_token_is_assign_op (void)
{
  bool retval = false;

  int c0 = yyinput ();

  switch (c0)
    {
    case '=':
      {
	int c1 = yyinput ();
	unput (c1);
	if (c1 != '=')
	  retval = true;
      }
      break;

    case '+':
    case '-':
    case '*':
    case '/':
    case '\\':
    case '&':
    case '|':
      {
	int c1 = yyinput ();
	unput (c1);
	if (c1 == '=')
	  retval = true;
      }
      break;

    case '.':
      {
	int c1 = yyinput ();
	if (match_any (c1, "+-*/\\"))
	  {
	    int c2 = yyinput ();
	    unput (c2);
	    if (c2 == '=')
	      retval = true;
	  }
	unput (c1);
      }
      break;

    case '>':
      {
	int c1 = yyinput ();
	if (c1 == '>')
	  {
	    int c2 = yyinput ();
	    unput (c2);
	    if (c2 == '=')
	      retval = true;
	  }
	unput (c1);
      }
      break;

    case '<':
      {
	int c1 = yyinput ();
	if (c1 == '<')
	  {
	    int c2 = yyinput ();
	    unput (c2);
	    if (c2 == '=')
	      retval = true;
	  }
	unput (c1);
      }
      break;

    default:
      break;
    }

  unput (c0);

  return retval;
}

static int
handle_close_bracket (int spc_gobbled)
{
  int retval = ']';

  if (! nesting_level.none ())
    {
      nesting_level.remove ();
      lexer_flags.bracketflag--;
    }

  if (lexer_flags.bracketflag == 0)
    BEGIN 0;

  if (next_token_is_assign_op () && ! lexer_flags.looking_at_return_list)
    {
      retval = CLOSE_BRACE;
    }
  else
    {
      int c1 = yyinput ();
      unput (c1);

      if (lexer_flags.bracketflag)
	{
	  int bin_op = next_token_is_bin_op (spc_gobbled);

	  int postfix_un_op = next_token_is_postfix_unary_op (spc_gobbled);

	  int sep_op = next_token_is_sep_op ();

	  if (! (postfix_un_op || bin_op || sep_op)
	      && nesting_level.is_bracket ()
	      && lexer_flags.convert_spaces_to_comma)
	    {
	      maybe_warn_separator_insert (',');

	      if (Vwhitespace_in_literal_matrix != 2)
		{
		  unput (',');
		  return ']';
		}
	    }
	}
    }

  lexer_flags.quote_is_transpose = true;
  lexer_flags.cant_be_identifier = false;
  lexer_flags.convert_spaces_to_comma = true;

  return retval;
}

static void
maybe_unput_comma (int spc_gobbled)
{
  if (nesting_level.is_bracket ())
    {
      int bin_op = next_token_is_bin_op (spc_gobbled);

      int postfix_un_op = next_token_is_postfix_unary_op (spc_gobbled);

      int c1 = yyinput ();
      int c2 = yyinput ();

      unput (c2);
      unput (c1);

      int sep_op = next_token_is_sep_op ();

      int dot_op = (c1 == '.'
		    && (isalpha (c2) || isspace (c2) || c2 == '_'));

      if (postfix_un_op || bin_op || sep_op || dot_op)
	return;

      int index_op = (c1 == '(');

      if (index_op)
	{
	  // If there is no space before the '(', we don't insert a comma.
	  if (! spc_gobbled)
	    return;

	  maybe_warn_separator_insert (',');

	  // If there is a space, we only insert a comma if we are
	  // trying to be Matlab-like.
	  if (Vwhitespace_in_literal_matrix == 1)
	    unput (',');
	}
      else
	{
	  maybe_warn_separator_insert (',');

	  if (Vwhitespace_in_literal_matrix != 2)
	    unput (',');
	}
    }
}

// Figure out exactly what kind of token to return when we have seen
// an identifier.  Handles keywords.

static int
handle_identifier (const std::string& tok, int spc_gobbled)
{
  // It is almost always an error for an identifier to be followed
  // directly by another identifier.  Special cases are handled
  // below.

  lexer_flags.cant_be_identifier = true;

  // If we are expecting a structure element, avoid recognizing
  // keywords and other special names and return STRUCT_ELT, which is
  // a string that is also a valid identifier.  But first, we have to
  // decide whether to insert a comma.

  if (lexer_flags.looking_at_indirect_ref)
    {
      do_comma_insert_check ();

      maybe_unput_comma (spc_gobbled);

      yylval.tok_val = new token (tok, input_line_number,
				  current_input_column);

      token_stack.push (yylval.tok_val);

      lexer_flags.cant_be_identifier = false;
      lexer_flags.quote_is_transpose = true;
      lexer_flags.convert_spaces_to_comma = true;

      current_input_column += yyleng;

      return STRUCT_ELT;
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
	  lexer_flags.quote_is_transpose = false;
	  lexer_flags.convert_spaces_to_comma = true;
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
	lexer_flags.past_plot_range = true;

      // Option keywords can't appear in brackets, braces, or parentheses.

      int plot_option_kw = 0;

      if (nesting_level.none ())
	plot_option_kw = is_plot_keyword (tok);
      
      if (lexer_flags.cant_be_identifier && plot_option_kw)
	TOK_RETURN (plot_option_kw);
    }

  int c1 = yyinput ();

  bool next_tok_is_dot = (c1 == '.');
  bool next_tok_is_paren = (c1 == '(');

  bool next_tok_is_eq = false;
  if (c1 == '=')
    {
      int c2 = yyinput ();
      unput (c2);

      if (c2 != '=')
	next_tok_is_eq = true;
    }

  unput (c1);

  // Make sure we put the return values of a function in the symbol
  // table that is local to the function.

  // If we are defining a function and we have not seen the function
  // name yet and the next token is `=', then this identifier must be
  // the only return value for the function and it belongs in the
  // local symbol table.

  if (next_tok_is_eq
      && lexer_flags.defining_func
      && ! lexer_flags.parsed_function_name)
    curr_sym_tab = tmp_local_sym_tab;

  // Kluge alert.
  //
  // If we are looking at a text style function, set up to gobble its
  // arguments.
  //
  // If the following token is `=', or if we are parsing a function
  // return list or function parameter list, or if we are looking at
  // something like [ab,cd] = foo (), force the symbol to be inserted
  // as a variable in the current symbol table.

  if (is_text_function_name (tok) && ! is_variable (tok))
    {
      if (next_tok_is_eq
	  || lexer_flags.looking_at_return_list
	  || lexer_flags.looking_at_parameter_list
	  || lexer_flags.looking_at_matrix_or_assign_lhs
	  || (next_tok_is_dot && next_token_is_bin_op (spc_gobbled)))
	{
	  force_local_variable (tok);
	}
      else if (! next_tok_is_paren)
	{
	  if (tok == "gset")
	    lexer_flags.doing_set = true;

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

  lexer_flags.convert_spaces_to_comma = true;

  if (! next_tok_is_eq)
    {
      lexer_flags.quote_is_transpose = true;

      do_comma_insert_check ();

      maybe_unput_comma (spc_gobbled);
    }

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
  // unput with EOF does not work.

  std::string comment_buf;

  bool in_comment = false;
  bool beginning_of_comment = true;

  int lineno = input_line_number;

  int c = 0;

  while ((c = yyinput ()) != EOF)
    {
      switch (c)
	{
	case ' ':
	case '\t':
	case ';':
	case ',':
	  if (in_comment)
	    {
	      comment_buf += (char) c;
	      beginning_of_comment = false;
	    }
	  break;

	case '\n':
	  if (in_comment)
	    {
	      comment_buf += (char) c;
	      octave_comment_buffer::append (comment_buf);
	      comment_buf.resize (0);
	      in_comment = false;
	      beginning_of_comment = false;
	    }
	  break;

	case '%':
	case '#':
	  if (in_comment)
	    {
	      if (! beginning_of_comment)
		comment_buf += (char) c;
	    }
	  else
	    {
	      in_comment = true;
	      beginning_of_comment = true;
	    }
	  break;

	default:
	  if (in_comment)
	    {
	      comment_buf += (char) c;
	      beginning_of_comment = false;
	      break;
	    }
	  else
	    {
	      warning ("ignoring trailing garbage after end of function\n\
         near line %d of file `%s.m'", lineno, curr_fcn_file_name.c_str ());
	      
	      unput ('\n');
	      return;
	    }
	}
    }

  if (! comment_buf.empty ())
    octave_comment_buffer::append (comment_buf);

  unput ('\n');
}

void
lexical_feedback::init (void)
{
  // Not initially defining a matrix list.
  bracketflag = 0;

  // Not initially inside a loop or if statement.
  looping = 0;

  // Not initially defining a function.
  beginning_of_function = false;
  defining_func = false;
  parsed_function_name = false;

  // Not parsing a function return or parameter list.
  looking_at_return_list = false;
  looking_at_parameter_list = false;

  // Next token can be identifier.
  cant_be_identifier = false;

  // No need to do comma insert or convert spaces to comma at
  // beginning of input. 
  convert_spaces_to_comma = true;
  do_comma_insert = false;

  // Not initially doing any plotting or setting of plot attributes.
  doing_set = false;
  in_plot_range = false;
  in_plot_style = false;
  in_plot_axes = false;
  in_plot_using = false;
  past_plot_range = false;
  plotting = false;

  // Not initially looking at indirect references.
  looking_at_indirect_ref = false;

  // Quote marks strings intially.
  quote_is_transpose = false;
}

static void
maybe_warn_separator_insert (char sep)
{
  std::string nm = curr_fcn_file_full_name;

  if (Vwarn_separator_insert)
    {
      if (nm.empty ())
	warning ("potential auto-insertion of `%c' near line %d",
		 sep, input_line_number);
      else
	warning ("potential auto-insertion of `%c' near line %d of file %s",
		 sep, input_line_number, nm.c_str ());
    }
}

static void
gripe_single_quote_string (void)
{
  std::string nm = curr_fcn_file_full_name;

  if (Vwarn_single_quote_string)
    {
      if (nm.empty ())
	warning ("single quote delimited string near line %d",
		 input_line_number);
      else
	warning ("single quote delimited string near line %d of file %s",
		 input_line_number, nm.c_str ());
    }
}

static int
warn_separator_insert (void)
{
  Vwarn_separator_insert = check_preference ("warn_separator_insert");

  return 0;
}

static int
warn_single_quote_string (void)
{
  Vwarn_single_quote_string = check_preference ("warn_single_quote_string");

  return 0;
}

static int
whitespace_in_literal_matrix (void)
{
  int pref = 0;

  std::string val = builtin_string_variable ("whitespace_in_literal_matrix");

  if (! val.empty ())
    {
      if (val == "ignore")
	pref = 2;
      else if (val == "traditional")
	pref = 1;
    }

  Vwhitespace_in_literal_matrix = pref;

  return 0;
}

void
symbols_of_lex (void)
{
  DEFVAR (warn_separator_insert, 0.0, warn_separator_insert,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} warn_separator_insert\n\
Print warning if commas or semicolons might be inserted\n\
automatically in literal matrices.\n\
@end defvr");

  DEFVAR (warn_single_quote_string, 0.0, warn_single_quote_string,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} warn_single_quote_string\n\
Print warning if a signle quote character is used to introduce a\n\
string constant.\n\
@end defvr");

  DEFVAR (whitespace_in_literal_matrix, "", whitespace_in_literal_matrix,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} whitespace_in_literal_matrix\n\
Control auto-insertion of commas and semicolons in literal matrices.\n\
@end defvr");

}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
