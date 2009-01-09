/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
              2002, 2003, 2004, 2005, 2006, 2007 John W. Eaton

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

%option prefix = "octave_"

%s COMMAND_START
%s MATRIX_START

%x SCRIPT_FILE_BEGIN

%x NESTED_FUNCTION_END
%x NESTED_FUNCTION_BEGIN

%{
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cctype>
#include <cstring>

#include <set>
#include <sstream>
#include <string>
#include <stack>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "cmd-edit.h"
#include "quit.h"
#include "lo-mappers.h"

// These would be alphabetical, but y.tab.h must be included before
// oct-gperf.h and y.tab.h must be included after token.h and the tree
// class declarations.  We can't include y.tab.h in oct-gperf.h
// because it may not be protected to allow it to be included multiple
// times.

#include "Cell.h"
#include "comment-list.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"
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

#define yylval octave_lval

// Arrange to get input via readline.

#ifdef YY_INPUT
#undef YY_INPUT
#endif
#define YY_INPUT(buf, result, max_size) \
  if ((result = octave_read (buf, max_size)) < 0) \
    YY_FATAL_ERROR ("octave_read () in flex scanner failed");

// Try to avoid crashing out completely on fatal scanner errors.
// The call to yy_fatal_error should never happen, but it avoids a
// `static function defined but not used' warning from gcc.

#ifdef YY_FATAL_ERROR
#undef YY_FATAL_ERROR
#endif
#define YY_FATAL_ERROR(msg) \
  do \
    { \
      error (msg); \
      OCTAVE_QUIT; \
      yy_fatal_error (msg); \
    } \
  while (0)

#define DISPLAY_TOK_AND_RETURN(tok) \
  do \
    { \
      int tok_val = tok; \
      if (Vdisplay_tokens) \
        display_token (tok_val); \
      return tok_val; \
    } \
  while (0)

#define COUNT_TOK_AND_RETURN(tok) \
  do \
    { \
      Vtoken_count++; \
      DISPLAY_TOK_AND_RETURN (tok); \
    } \
  while (0)

#define TOK_RETURN(tok) \
  do \
    { \
      current_input_column += yyleng; \
      lexer_flags.quote_is_transpose = false; \
      lexer_flags.convert_spaces_to_comma = true; \
      COUNT_TOK_AND_RETURN (tok); \
    } \
  while (0)

#define TOK_PUSH_AND_RETURN(name, tok) \
  do \
    { \
      yylval.tok_val = new token (name, input_line_number, \
				  current_input_column); \
      token_stack.push (yylval.tok_val); \
      TOK_RETURN (tok); \
    } \
  while (0)

#define BIN_OP_RETURN(tok, convert) \
  do \
    { \
      yylval.tok_val = new token (input_line_number, current_input_column); \
      token_stack.push (yylval.tok_val); \
      current_input_column += yyleng; \
      lexer_flags.quote_is_transpose = false; \
      lexer_flags.convert_spaces_to_comma = convert; \
      COUNT_TOK_AND_RETURN (tok); \
    } \
  while (0)

#define XBIN_OP_RETURN(tok, convert) \
  do \
    { \
	gripe_matlab_incompatible_operator (yytext); \
        BIN_OP_RETURN (tok, convert); \
    } \
  while (0)

// TRUE means that we have encountered EOF on the input stream.
bool parser_end_of_input = false;

// Flags that need to be shared between the lexer and parser.
lexical_feedback lexer_flags;

// Stack to hold tokens so that we can delete them when the parser is
// reset and avoid growing forever just because we are stashing some
// information.  This has to appear before lex.h is included, because
// one of the macros defined there uses token_stack.
//
// FIXME -- this should really be static, but that causes
// problems on some systems.
std::stack <token*> token_stack;

// Did eat_whitespace() eat a space or tab, or a newline, or both?

typedef int yum_yum;

const yum_yum ATE_NOTHING = 0;
const yum_yum ATE_SPACE_OR_TAB = 1;
const yum_yum ATE_NEWLINE = 2;

// Is the closest nesting level a square bracket, squiggly brace or a paren?

class bracket_brace_paren_nesting_level
{
public:

  bracket_brace_paren_nesting_level (void) : context () { }

  ~bracket_brace_paren_nesting_level (void) { }

  void bracket (void) { context.push (BRACKET); }
  bool is_bracket (void)
    { return ! context.empty () && context.top () == BRACKET; }

  void brace (void) {  context.push (BRACE); }
  bool is_brace (void)
    { return ! context.empty () && context.top () == BRACE; }

  void paren (void) {  context.push (PAREN); }
  bool is_paren (void)
    { return ! context.empty () && context.top () == PAREN; }

  bool is_bracket_or_brace (void)
    { return (! context.empty ()
	      && (context.top () == BRACKET || context.top () == BRACE)); }

  bool none (void) { return context.empty (); }

  void remove (void) { if (! context.empty ()) context.pop (); }

  void clear (void) { while (! context.empty ()) context.pop (); }

private:

  std::stack<int> context;

  static const int BRACKET;
  static const int BRACE;
  static const int PAREN;

  bracket_brace_paren_nesting_level (const bracket_brace_paren_nesting_level&);

  bracket_brace_paren_nesting_level&
  operator = (const bracket_brace_paren_nesting_level&);
};

const int bracket_brace_paren_nesting_level::BRACKET = 1;
const int bracket_brace_paren_nesting_level::BRACE = 2;
const int bracket_brace_paren_nesting_level::PAREN = 3;

static bracket_brace_paren_nesting_level nesting_level;

static bool Vdisplay_tokens = false;

static unsigned int Vtoken_count = 0;

// The start state that was in effect when the beginning of a block
// comment was noticed.
static int block_comment_nesting_level = 0;

// Forward declarations for functions defined at the bottom of this
// file.

static int text_yyinput (void);
static void xunput (char c, char *buf);
static void fixup_column_count (char *s);
static void do_comma_insert_check (void);
static int is_keyword_token (const std::string& s);
static void prep_for_function (void);
static void prep_for_nested_function (void);
static int process_comment (bool start_in_block, bool& eof);
static bool match_any (char c, const char *s);
static bool next_token_is_sep_op (void);
static bool next_token_is_bin_op (bool spc_prev);
static bool next_token_is_postfix_unary_op (bool spc_prev);
static std::string strip_trailing_whitespace (char *s);
static void handle_number (void);
static int handle_string (char delim, int text_style = 0);
static int handle_close_bracket (bool spc_gobbled, int bracket_type);
static int handle_identifier (void);
static bool have_continuation (bool trailing_comments_ok = true);
static bool have_ellipsis_continuation (bool trailing_comments_ok = true);
static void scan_for_comments (const char *);
static yum_yum eat_whitespace (void);
static yum_yum eat_continuation (void);
static void maybe_warn_separator_insert (char sep);
static void gripe_single_quote_string (void);
static void gripe_matlab_incompatible (const std::string& msg);
static void maybe_gripe_matlab_incompatible_comment (char c);
static void gripe_matlab_incompatible_continuation (void);
static void gripe_matlab_incompatible_operator (const std::string& op);
static void display_token (int tok);

%}

D	[0-9]
S	[ \t]
NL	((\n)|(\r)|(\r\n))
SNL	({S}|{NL})
EL	(\.\.\.)
BS	(\\)
CONT	({EL}|{BS})
Im	[iIjJ]
CCHAR	[#%]
COMMENT	({CCHAR}.*{NL})
SNLCMT	({SNL}|{COMMENT})
NOT	((\~)|(\!))
POW     ((\*\*)|(\^))
EPOW    (\.{POW})
IDENT	([_$a-zA-Z][_$a-zA-Z0-9]*)
EXPON	([DdEe][+-]?{D}+)
NUMBER	(({D}+\.?{D}*{EXPON}?)|(\.{D}+{EXPON}?)|(0[xX][0-9a-fA-F]+))
%%

<SCRIPT_FILE_BEGIN>. {
    BEGIN (INITIAL);
    xunput (yytext[0], yytext);
    COUNT_TOK_AND_RETURN (SCRIPT);
  }

<NESTED_FUNCTION_END>. {
    BEGIN (NESTED_FUNCTION_BEGIN);
    xunput (yytext[0], yytext);
    COUNT_TOK_AND_RETURN (';');
  }

<NESTED_FUNCTION_BEGIN>. {
    BEGIN (INITIAL);
    xunput (yytext[0], yytext);
    prep_for_nested_function ();
    COUNT_TOK_AND_RETURN (FCN);
  }

%{
// Help and other command-style functions are a pain in the ass.  This
// stuff needs to be simplified.  May require some changes in the
// parser too.
%}

<COMMAND_START>{NL} {
    BEGIN (INITIAL);
    input_line_number++;
    current_input_column = 1;
    lexer_flags.quote_is_transpose = false;
    lexer_flags.convert_spaces_to_comma = true;
    lexer_flags.doing_rawcommand = false;
    COUNT_TOK_AND_RETURN ('\n');
  }

<COMMAND_START>[\;\,] {
    if (lexer_flags.doing_rawcommand)
      TOK_PUSH_AND_RETURN (yytext, SQ_STRING);

    BEGIN (INITIAL);

    if (strcmp (yytext, ",") == 0)
      TOK_RETURN (',');
    else
      TOK_RETURN (';');
  }

<COMMAND_START>[\"\'] {
    current_input_column++;
    int tok = handle_string (yytext[0], true);
    COUNT_TOK_AND_RETURN (tok);
  }

<COMMAND_START>[^#% \t\r\n\;\,\"\'][^ \t\r\n\;\,]*{S}* {
    std::string tok = strip_trailing_whitespace (yytext);
    TOK_PUSH_AND_RETURN (tok, SQ_STRING);
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

// FIXME -- we need to handle block comments here.
%}

<MATRIX_START>{SNLCMT}*\]{S}* {
    scan_for_comments (yytext);
    fixup_column_count (yytext);
    int c = yytext[yyleng-1];
    int cont_is_spc = eat_continuation ();
    bool spc_gobbled = (cont_is_spc || c == ' ' || c == '\t');
    int tok_to_return = handle_close_bracket (spc_gobbled, ']');
    if (spc_gobbled)
      yyunput (' ', yytext);
    COUNT_TOK_AND_RETURN (tok_to_return);
  }

%{
// FIXME -- we need to handle block comments here.
%}

<MATRIX_START>{SNLCMT}*\}{S}* {
    scan_for_comments (yytext);
    fixup_column_count (yytext);
    int c = yytext[yyleng-1];
    int cont_is_spc = eat_continuation ();
    bool spc_gobbled = (cont_is_spc || c == ' ' || c == '\t');
    int tok_to_return = handle_close_bracket (spc_gobbled, '}');
    if (spc_gobbled)
      yyunput (' ', yytext);
    COUNT_TOK_AND_RETURN (tok_to_return);
  }

%{
// Commas are element separators in matrix constants.  If we don't
// check for continuations here we can end up inserting too many
// commas.
%}

<MATRIX_START>{S}*\,{S}* {
    current_input_column += yyleng;

    int tmp = eat_continuation ();

    lexer_flags.quote_is_transpose = false;
    lexer_flags.convert_spaces_to_comma = true;

    if ((tmp & ATE_NEWLINE) == ATE_NEWLINE)
      {
	maybe_warn_separator_insert (';');

	yyunput (';', yytext);
      }

    COUNT_TOK_AND_RETURN (',');
  }

%{
// In some cases, spaces in matrix constants can turn into commas.
// If commas are required, spaces are not important in matrix
// constants so we just eat them.  If we don't check for continuations
// here we can end up inserting too many commas.
%}

<MATRIX_START>{S}+ {
    current_input_column += yyleng;

    int tmp = eat_continuation ();
    bool bin_op = next_token_is_bin_op (true);
    bool postfix_un_op = next_token_is_postfix_unary_op (true);
    bool sep_op = next_token_is_sep_op ();

    if (! (postfix_un_op || bin_op || sep_op)
	&& nesting_level.is_bracket_or_brace ()
	&& lexer_flags.convert_spaces_to_comma)
      {
	if ((tmp & ATE_NEWLINE) == ATE_NEWLINE)
	  {
	    maybe_warn_separator_insert (';');

	    yyunput (';', yytext);
	  }

	lexer_flags.quote_is_transpose = false;
	lexer_flags.convert_spaces_to_comma = true;

	maybe_warn_separator_insert (',');

	COUNT_TOK_AND_RETURN (',');
      }
  }

%{
// Semicolons are handled as row seprators in matrix constants.  If we
// don't eat whitespace here we can end up inserting too many
// semicolons.

// FIXME -- we need to handle block comments here.
%}

<MATRIX_START>{SNLCMT}*;{SNLCMT}* {
    scan_for_comments (yytext);
    fixup_column_count (yytext);
    eat_whitespace ();
    lexer_flags.quote_is_transpose = false;
    lexer_flags.convert_spaces_to_comma = true;
    COUNT_TOK_AND_RETURN (';');
  }

%{
// In some cases, new lines can also become row separators.  If we
// don't eat whitespace here we can end up inserting too many
// semicolons.

// FIXME -- we need to handle block comments here.
%}

<MATRIX_START>{S}*{COMMENT}{SNLCMT}* |
<MATRIX_START>{S}*{NL}{SNLCMT}* {
    scan_for_comments (yytext);
    fixup_column_count (yytext);
    eat_whitespace ();

    lexer_flags.quote_is_transpose = false;
    lexer_flags.convert_spaces_to_comma = true;

    if (nesting_level.none ())
      return LEXICAL_ERROR;

    if (nesting_level.is_bracket_or_brace ())
      {
	maybe_warn_separator_insert (';');

	COUNT_TOK_AND_RETURN (';');
      }
  }

\[{S}* {
    nesting_level.bracket ();

    current_input_column += yyleng;
    lexer_flags.quote_is_transpose = false;
    lexer_flags.convert_spaces_to_comma = true;

    if (lexer_flags.defining_func && ! lexer_flags.parsed_function_name)
      lexer_flags.looking_at_return_list = true;
    else
      lexer_flags.looking_at_matrix_or_assign_lhs = true;

    promptflag--;
    eat_whitespace ();

    lexer_flags.bracketflag++;
    BEGIN (MATRIX_START);
    COUNT_TOK_AND_RETURN ('[');
  }

\] {
    nesting_level.remove ();

    TOK_RETURN (']');
  }

%{
// Imaginary numbers.
%}

{NUMBER}{Im} {
    handle_number ();
    COUNT_TOK_AND_RETURN (IMAG_NUM);
  }

%{
// Real numbers.  Don't grab the `.' part of a dot operator as part of
// the constant.
%}

{D}+/\.[\*/\\^'] |
{NUMBER} {
    handle_number ();
    COUNT_TOK_AND_RETURN (NUM);
  }

%{
// Eat whitespace.  Whitespace inside matrix constants is handled by
// the <MATRIX_START> start state code above.
%}

{S}* {
    current_input_column += yyleng;
  }

%{
// Continuation lines.  Allow comments after continuations.
%}

{CONT}{S}*{NL} |
{CONT}{S}*{COMMENT} {
    if (yytext[0] == '\\')
      gripe_matlab_incompatible_continuation ();
    scan_for_comments (yytext);
    promptflag--;
    input_line_number++;
    current_input_column = 1;
  }

%{
// End of file.
%}

<<EOF>> {
    if (block_comment_nesting_level != 0)
      {
	warning ("block comment open at end of input");

	if ((reading_fcn_file || reading_script_file)
	    && ! curr_fcn_file_name.empty ())
	  warning ("near line %d of file `%s.m'",
		   input_line_number, curr_fcn_file_name.c_str ());
      }

    TOK_RETURN (END_OF_INPUT);
  }

%{
// Identifiers.  Truncate the token at the first space or tab but
// don't write directly on yytext.
%}

{IDENT}{S}* {
    int id_tok = handle_identifier ();

    if (id_tok >= 0)
      COUNT_TOK_AND_RETURN (id_tok);
  }

%{
// Function handles.
%}

"@" {
    current_input_column++;
    lexer_flags.quote_is_transpose = false;
    lexer_flags.convert_spaces_to_comma = false;
    lexer_flags.looking_at_function_handle++;
    COUNT_TOK_AND_RETURN ('@');
  }

%{
// A new line character.  New line characters inside matrix constants
// are handled by the <MATRIX_START> start state code above.  If closest
// nesting is inside parentheses, don't return a row separator.
%}

{NL} {
    input_line_number++;
    current_input_column = 1;
    lexer_flags.quote_is_transpose = false;
    lexer_flags.convert_spaces_to_comma = true;
    if (nesting_level.none ())
      COUNT_TOK_AND_RETURN ('\n');
    else if (nesting_level.is_paren ())
      gripe_matlab_incompatible ("bare newline inside parentheses");
    else if (nesting_level.is_bracket_or_brace ())
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
	COUNT_TOK_AND_RETURN (QUOTE);
      }
    else
      {
	int tok = handle_string ('\'');
	COUNT_TOK_AND_RETURN (tok);
      }
  }

%{
// Double quotes always begin strings.
%}

\" {
    current_input_column++;
    int tok = handle_string ('"');
    COUNT_TOK_AND_RETURN (tok);
}

%{
// Gobble comments.  If closest nesting is inside parentheses, don't
// return a new line.
%} 

{CCHAR} {
    xunput (yytext[0], yytext);

    bool eof = false;
    int tok = process_comment (false, eof);

    if (eof)
      TOK_RETURN (END_OF_INPUT);
    else if (tok > 0)
      COUNT_TOK_AND_RETURN (tok);
  }

%{
// Block comments.
%}

^{S}*{CCHAR}\{{S}*{NL} {
    input_line_number++;
    current_input_column = 1;
    block_comment_nesting_level++;
    promptflag--;
    bool eof = false;
    process_comment (true, eof);
  }

%{
// Other operators.
%}

":"     { BIN_OP_RETURN (':', false); }

".+"	{ XBIN_OP_RETURN (EPLUS, false); }
".-"	{ XBIN_OP_RETURN (EMINUS, false); }
".*"	{ BIN_OP_RETURN (EMUL, false); }
"./"	{ BIN_OP_RETURN (EDIV, false); }
".\\"	{ BIN_OP_RETURN (ELEFTDIV, false); }
".^"	{ BIN_OP_RETURN (EPOW, false); }
".**"	{ XBIN_OP_RETURN (EPOW, false); }
".'"	{ do_comma_insert_check (); BIN_OP_RETURN (TRANSPOSE, true); }
"++"	{ do_comma_insert_check (); XBIN_OP_RETURN (PLUS_PLUS, true); }
"--"	{ do_comma_insert_check (); XBIN_OP_RETURN (MINUS_MINUS, true); }
"<="	{ BIN_OP_RETURN (EXPR_LE, false); }
"=="	{ BIN_OP_RETURN (EXPR_EQ, false); }
"~="	{ BIN_OP_RETURN (EXPR_NE, false); }
"!="	{ XBIN_OP_RETURN (EXPR_NE, false); }
">="	{ BIN_OP_RETURN (EXPR_GE, false); }
"&"	{ BIN_OP_RETURN (EXPR_AND, false); }
"|"	{ BIN_OP_RETURN (EXPR_OR, false); }
"<"	{ BIN_OP_RETURN (EXPR_LT, false); }
">"	{ BIN_OP_RETURN (EXPR_GT, false); }
"+"     { BIN_OP_RETURN ('+', false); }
"-"     { BIN_OP_RETURN ('-', false); }
"*"	{ BIN_OP_RETURN ('*', false); }
"/"	{ BIN_OP_RETURN ('/', false); }
"\\"	{ BIN_OP_RETURN (LEFTDIV, false); }
";"	{ BIN_OP_RETURN (';', true); }
","	{ BIN_OP_RETURN (',', true); }
"^"	{ BIN_OP_RETURN (POW, false); }
"**"	{ XBIN_OP_RETURN (POW, false); }
"="	{ BIN_OP_RETURN ('=', true); }
"&&"	{ BIN_OP_RETURN (EXPR_AND_AND, false); }
"||"	{ BIN_OP_RETURN (EXPR_OR_OR, false); }
"<<"	{ XBIN_OP_RETURN (LSHIFT, false); }
">>"	{ XBIN_OP_RETURN (RSHIFT, false); }

{NOT} {
    if (yytext[0] == '~')
      BIN_OP_RETURN (EXPR_NOT, false);
    else
      XBIN_OP_RETURN (EXPR_NOT, false);
  }

"(" {
    lexer_flags.looking_at_indirect_ref = false;
    nesting_level.paren ();
    promptflag--;
    TOK_RETURN ('(');
  }

")" {
    nesting_level.remove ();
    current_input_column++;
    lexer_flags.quote_is_transpose = true;
    lexer_flags.convert_spaces_to_comma = nesting_level.is_bracket_or_brace ();
    do_comma_insert_check ();
    COUNT_TOK_AND_RETURN (')');
  }

"."     { TOK_RETURN ('.'); }

"+="	{ XBIN_OP_RETURN (ADD_EQ, false); }
"-="	{ XBIN_OP_RETURN (SUB_EQ, false); }
"*="	{ XBIN_OP_RETURN (MUL_EQ, false); }
"/="	{ XBIN_OP_RETURN (DIV_EQ, false); }
"\\="	{ XBIN_OP_RETURN (LEFTDIV_EQ, false); }
".+="	{ XBIN_OP_RETURN (ADD_EQ, false); }
".-="	{ XBIN_OP_RETURN (SUB_EQ, false); }
".*="	{ XBIN_OP_RETURN (EMUL_EQ, false); }
"./="	{ XBIN_OP_RETURN (EDIV_EQ, false); }
".\\="	{ XBIN_OP_RETURN (ELEFTDIV_EQ, false); }
{POW}=  { XBIN_OP_RETURN (POW_EQ, false); }
{EPOW}= { XBIN_OP_RETURN (EPOW_EQ, false); }
"&="	{ XBIN_OP_RETURN (AND_EQ, false); }
"|="	{ XBIN_OP_RETURN (OR_EQ, false); }
"<<="	{ XBIN_OP_RETURN (LSHIFT_EQ, false); }
">>="	{ XBIN_OP_RETURN (RSHIFT_EQ, false); }

\{{S}* {
    nesting_level.brace ();

    current_input_column += yyleng;
    lexer_flags.quote_is_transpose = false;
    lexer_flags.convert_spaces_to_comma = true;

    promptflag--;
    eat_whitespace ();

    lexer_flags.braceflag++;
    BEGIN (MATRIX_START);
    COUNT_TOK_AND_RETURN ('{');
  }

"}" {
    nesting_level.remove ();

    TOK_RETURN ('}');
  }

%{
// Unrecognized input is a lexical error.
%}

. {
    // EOF happens here if we are parsing nested functions.

    xunput (yytext[0], yytext);

    int c = text_yyinput ();

    if (c != EOF)
      {
	current_input_column++;

	error ("invalid character `%s' (ASCII %d) near line %d, column %d",
	       undo_string_escape (static_cast<char> (c)), c,
	       input_line_number, current_input_column);

	return LEXICAL_ERROR;
      }
    else
      TOK_RETURN (END_OF_INPUT);
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

  int c = text_yyinput ();

  xunput (c, yytext);

  if (spc_gobbled)
    yyunput (' ', yytext);

  lexer_flags.do_comma_insert = (lexer_flags.bracketflag && c == '[');
}

// Fix things up for errors or interrupts.  The parser is never called
// recursively, so it is always safe to reinitialize its state before
// doing any parsing.

void
reset_parser (void)
{
  // Start off on the right foot.
  BEGIN (INITIAL);

  parser_end_of_input = false;
  end_tokens_expected = 0;

  while (! symtab_context.empty ())
    symtab_context.pop ();

  symbol_table::reset_parent_scope ();

  // We do want a prompt by default.
  promptflag = 1;

  // We are not in a block comment.
  block_comment_nesting_level = 0;

  // Error may have occurred inside some brackets, braces, or parentheses.
  nesting_level.clear ();

  // Clear out the stack of token info used to track line and column
  // numbers.
  while (! token_stack.empty ())
    {
      delete token_stack.top ();
      token_stack.pop ();
    }

  // Can be reset by defining a function.
  if (! (reading_script_file || reading_fcn_file))
    {
      current_input_column = 1;
      input_line_number = command_editor::current_command_number ();
    }

  // Only ask for input from stdin if we are expecting interactive
  // input.
  if ((interactive || forced_interactive)
      && ! (reading_fcn_file
	    || reading_script_file
	    || get_input_from_eval_string
	    || input_from_startup_file))
    yyrestart (stdin);

  // Clear the buffer for help text.
  while (! help_buf.empty ())
    help_buf.pop ();

  // Reset other flags.
  lexer_flags.init ();
}

static int
text_yyinput (void)
{
  int c = yyinput ();

  // Convert CRLF into just LF and single CR into LF.

  if (c == '\r')
    {
      c = yyinput ();

      if (c != '\n')
	{
	  yyunput (c, yytext);
	  c = '\n';
	}
    }

  if (c == '\n')
    input_line_number++;

  return c;
}

static void
xunput (char c, char *buf)
{
  if (c == '\n')
    input_line_number--;

  yyunput (c, buf);
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
        {
          input_line_number++;
          current_input_column = 1;
        }
      else
	current_input_column++;
    }
}

// Include these so that we don't have to link to libfl.a.

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

static void
prep_for_function (void)
{
  end_tokens_expected++;

  promptflag--;

  lexer_flags.defining_func = true;
  lexer_flags.parsed_function_name = false;

  if (! (reading_fcn_file || reading_script_file))
    input_line_number = 1;
}

static void
prep_for_nested_function (void)
{
  lexer_flags.parsing_nested_function = 1;
  help_buf.push (std::string ());
  prep_for_function ();
  // We're still only expecting one end token for this set of functions.
  end_tokens_expected--;
  yylval.tok_val = new token (input_line_number, current_input_column);
  token_stack.push (yylval.tok_val);
}

// Handle keywords.  Return -1 if the keyword should be ignored.

static int
is_keyword_token (const std::string& s)
{
  int l = input_line_number;
  int c = current_input_column;

  int len = s.length ();

  const octave_kw *kw = octave_kw_hash::in_word_set (s.c_str (), len);

  if (kw)
    {
      yylval.tok_val = 0;

      switch (kw->kw_id)
	{
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
	  if (lexer_flags.looking_at_object_index
	      || (lexer_flags.defining_func
		  && ! (lexer_flags.looking_at_return_list
			|| lexer_flags.parsed_function_name)))
	    return 0;
	  else
	    {
	      if (reading_fcn_file && end_tokens_expected == 1)
		return -1;
	      else
		{
		  yylval.tok_val = new token (token::simple_end, l, c);
		  end_tokens_expected--;
		}
	    }
	  break;

	case end_try_catch_kw:
	  end_tokens_expected--;
	  yylval.tok_val = new token (token::try_catch_end, l, c);
	  break;

	case end_unwind_protect_kw:
	  end_tokens_expected--;
	  yylval.tok_val = new token (token::unwind_protect_end, l, c);
	  break;

	case endfor_kw:
	  end_tokens_expected--;
	  yylval.tok_val = new token (token::for_end, l, c);
	  break;

	case endfunction_kw:
	  {
	    if (reading_fcn_file && end_tokens_expected == 1)
	      return -1;
	    else
	      {
		yylval.tok_val = new token (token::function_end, l, c);
		end_tokens_expected--;
	      }
	  }
	  break;

	case endif_kw:
	  end_tokens_expected--;
	  yylval.tok_val = new token (token::if_end, l, c);
	  break;

	case endswitch_kw:
	  end_tokens_expected--;
	  yylval.tok_val = new token (token::switch_end, l, c);
	  break;

	case endwhile_kw:
	  end_tokens_expected--;
	  yylval.tok_val = new token (token::while_end, l, c);
	  break;

	case for_kw:
	case while_kw:
	  end_tokens_expected++;
	  // Fall through...

	case do_kw:
	  promptflag--;
	  lexer_flags.looping++;
	  break;

	case if_kw:
	case try_kw:
	case switch_kw:
	case unwind_protect_kw:
	  end_tokens_expected++;
	  promptflag--;
	  break;

	case function_kw:
	  {
	    if (lexer_flags.defining_func)
	      {
		if (reading_fcn_file)
		  {
		    if (lexer_flags.parsing_nested_function)
		      {
			BEGIN (NESTED_FUNCTION_END);

			yylval.tok_val = new token (token::function_end, l, c);
			token_stack.push (yylval.tok_val);

			return END;
		      }
		    else
		      {
			prep_for_nested_function ();

			return FCN;
		      }
		  }
		else
		  {
		    error ("nested functions not implemented in this context");

		    if ((reading_fcn_file || reading_script_file)
			&& ! curr_fcn_file_name.empty ())
		      error ("near line %d of file `%s.m'",
			     input_line_number, curr_fcn_file_name.c_str ());
		    else
		      error ("near line %d", input_line_number);

		    return LEXICAL_ERROR;
		  }
	      }
	    else
	      prep_for_function ();
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

static bool
is_variable (const std::string& name)
{
  return (symbol_table::is_variable (name)
	  || (lexer_flags.pending_local_variables.find (name)
	      != lexer_flags.pending_local_variables.end ()));
}

void
force_local_variable (const std::string& name)
{
  octave_value& val = symbol_table::varref (name);

  if (! val.is_defined ())
    val = Matrix ();
}

static std::string
grab_block_comment (stream_reader& reader, bool& eof)
{
  std::string buf;

  bool at_bol = true;
  bool look_for_marker = false;

  bool warned_incompatible = false;

  int c = 0;

  while ((c = reader.getc ()) != EOF)
    {
      current_input_column++;

      if (look_for_marker)
	{
	  at_bol = false;
	  look_for_marker = false;

	  if (c == '{' || c == '}')
	    {
	      std::string tmp_buf (1, static_cast<char> (c));

	      int type = c;

	      bool done = false;

	      while ((c = reader.getc ()) != EOF && ! done)
		{
		  current_input_column++;

		  switch (c)
		    {
		    case ' ':
		    case '\t':
		      tmp_buf += static_cast<char> (c);
		      break;

		    case '\n':
		      {
			current_input_column = 0;
			at_bol = true;
			done = true;

			if (type == '{')
			  {
			    block_comment_nesting_level++;
			    promptflag--;
			  }
			else
			  {
			    block_comment_nesting_level--;
			    promptflag++;

			    if (block_comment_nesting_level == 0)
			      {
				buf += grab_comment_block (reader, true, eof);

				return buf;
			      }
			  }
		      }
		      break;

		    default:
		      at_bol = false;
		      tmp_buf += static_cast<char> (c);
		      buf += tmp_buf;
		      done = true;
		      break;
		    }
		}
	    }
	}

      if (at_bol && (c == '%' || c == '#'))
        {
          if (c == '#' && ! warned_incompatible)
	    {
	      warned_incompatible = true;
	      maybe_gripe_matlab_incompatible_comment (c);
	    }

	  at_bol = false;
	  look_for_marker = true;
	}
      else
	{
	  buf += static_cast<char> (c);

	  if (c == '\n')
	    {
	      current_input_column = 0;
	      at_bol = true;
	    }
	}
    }

  if (c == EOF)
    eof = true;

  return buf;
}

std::string
grab_comment_block (stream_reader& reader, bool at_bol,
		    bool& eof)
{
  std::string buf;

  // TRUE means we are at the beginning of a comment block.
  bool begin_comment = false;

  // TRUE means we are currently reading a comment block.
  bool in_comment = false;

  bool warned_incompatible = false;

  int c = 0;

  while ((c = reader.getc ()) != EOF)
    {
      current_input_column++;

      if (begin_comment)
	{
	  if (c == '%' || c == '#')
	    {
	      at_bol = false;
	      continue;
	    }
	  else if (at_bol && c == '{')
	    {
	      std::string tmp_buf (1, static_cast<char> (c));

	      bool done = false;

	      while ((c = reader.getc ()) != EOF && ! done)
		{
		  current_input_column++;

		  switch (c)
		    {
		    case ' ':
		    case '\t':
		      tmp_buf += static_cast<char> (c);
		      break;

		    case '\n':
		      {
			current_input_column = 0;
			at_bol = true;
			done = true;

			block_comment_nesting_level++;
			promptflag--;

			buf += grab_block_comment (reader, eof);

			in_comment = false;

			if (eof)
			  goto done;
		      }
		      break;

		    default:
		      at_bol = false;
		      tmp_buf += static_cast<char> (c);
		      buf += tmp_buf;
		      done = true;
		      break;
		    }
		}
	    }
	  else
	    {
	      at_bol = false;
	      begin_comment = false;
	    }
	}	

      if (in_comment)
	{
	  buf += static_cast<char> (c);

	  if (c == '\n')
	    {
	      at_bol = true;
	      current_input_column = 0;
	      in_comment = false;
	    }
	}
      else
	{
	  switch (c)
	    {
	    case ' ':
	    case '\t':
	      break;

	    case '#':
	      if (! warned_incompatible)
		{
		  warned_incompatible = true;
		  maybe_gripe_matlab_incompatible_comment (c);
		}
	      // fall through...

	    case '%':
	      in_comment = true;
	      begin_comment = true;
	      break;

	    default:
	      current_input_column--;
	      reader.ungetc (c);
	      goto done;
	    }
	}
    }

 done:

  if (c == EOF)
    eof = true;

  return buf;
}

class
flex_stream_reader : public stream_reader
{
public:
  flex_stream_reader (char *buf_arg) : stream_reader (), buf (buf_arg) { }

  int getc (void) { return ::text_yyinput (); }
  int ungetc (int c) { ::xunput (c, buf); return 0; }
  
private:
  char *buf;
};

static int
process_comment (bool start_in_block, bool& eof)
{
  eof = false;

  std::string help_txt;

  if (! help_buf.empty ())
    help_txt = help_buf.top ();

  flex_stream_reader flex_reader (yytext);

  // process_comment is only supposed to be called when we are not
  // initially looking at a block comment.

  std::string txt = start_in_block
    ? grab_block_comment (flex_reader, eof)
    : grab_comment_block (flex_reader, false, eof);

  if (help_txt.empty () && nesting_level.none ())
    {
      if (! help_buf.empty ())
	help_buf.pop ();

      help_buf.push (txt);
    }

  octave_comment_buffer::append (txt);

  current_input_column = 1;
  lexer_flags.quote_is_transpose = false;
  lexer_flags.convert_spaces_to_comma = true;

  if (YY_START == COMMAND_START)
    BEGIN (INITIAL);

  if (nesting_level.none ())
    {
      lexer_flags.doing_rawcommand = false;
      return '\n';
    }
  else if (nesting_level.is_bracket_or_brace ())
    return ';';
  else
    return 0;
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

  int c = text_yyinput ();

  retval = match_any (c, ",;\n]");

  xunput (c, yytext);

  return retval;
}

// Try to determine if the next token should be treated as a postfix
// unary operator.  This is ugly, but it seems to do the right thing.

static bool
next_token_is_postfix_unary_op (bool spc_prev)
{
  bool un_op = false;

  int c0 = text_yyinput ();

  if (c0 == '\'' && ! spc_prev)
    {
      un_op = true;
    }
  else if (c0 == '.')
    {
      int c1 = text_yyinput ();
      un_op = (c1 == '\'');
      xunput (c1, yytext);
    }
  else if (c0 == '+')
    {
      int c1 = text_yyinput ();
      un_op = (c1 == '+');
      xunput (c1, yytext);
    }
  else if (c0 == '-')
    {
      int c1 = text_yyinput ();
      un_op = (c1 == '-');
      xunput (c1, yytext);
    }

  xunput (c0, yytext);

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

  int c0 = text_yyinput ();

  switch (c0)
    {
    case '+':
    case '-':
      {
	int c1 = text_yyinput ();

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

	xunput (c1, yytext);
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
	int c1 = text_yyinput ();

	if (match_any (c1, "+-/\\^*"))
	  // Always a binary op (may also include .+=, .-=, ./=, ...).
	  bin_op = true;
	else if (! isdigit (c1) && c1 != ' ' && c1 != '\t' && c1 != '.')
	  // A structure element reference is a binary op.
	  bin_op = true;

	xunput (c1, yytext);
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
	int c1 = text_yyinput ();

	// ~ and ! can be unary ops, so require following =.
	if (c1 == '=')
	  bin_op = true;

	xunput (c1, yytext);
      }
      break;

    default:
      break;
    }

  xunput (c0, yytext);

  return bin_op;
}

// Used to delete trailing white space from tokens.

static std::string
strip_trailing_whitespace (char *s)
{
  std::string retval = s;

  size_t pos = retval.find_first_of (" \t");

  if (pos != std::string::npos)
    retval.resize (pos);

  return retval;
}

// FIXME -- we need to handle block comments here.

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
		comment_buf += static_cast<char> (c);
	    }
	  else
	    {
	      maybe_gripe_matlab_incompatible_comment (c);
	      in_comment = true;
	      beginning_of_comment = true;
	    }
	  break;

	case '\n':
	  if (in_comment)
	    {
	      comment_buf += static_cast<char> (c);
	      octave_comment_buffer::append (comment_buf);
	      comment_buf.resize (0);
	      in_comment = false;
	      beginning_of_comment = false;
	    }
	  break;

	default:
	  if (in_comment)
	    {
	      comment_buf += static_cast<char> (c);
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

// FIXME -- we need to handle block comments here.

static yum_yum
eat_whitespace (void)
{
  yum_yum retval = ATE_NOTHING;

  std::string comment_buf;

  bool in_comment = false;
  bool beginning_of_comment = false;

  int c = 0;

  while ((c = text_yyinput ()) != EOF)
    {
      current_input_column++;

      switch (c)
	{
	case ' ':
	case '\t':
	  if (in_comment)
	    {
	      comment_buf += static_cast<char> (c);
	      beginning_of_comment = false;
	    }
	  retval |= ATE_SPACE_OR_TAB;
	  break;

	case '\n':
	  retval |= ATE_NEWLINE;
	  if (in_comment)
	    {
	      comment_buf += static_cast<char> (c);
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
		comment_buf += static_cast<char> (c);
	    }
	  else
	    {
	      maybe_gripe_matlab_incompatible_comment (c);
	      in_comment = true;
	      beginning_of_comment = true;
	    }
	  break;

	case '.':
	  if (in_comment)
	    {
	      comment_buf += static_cast<char> (c);
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
	      comment_buf += static_cast<char> (c);
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
	      comment_buf += static_cast<char> (c);
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
  xunput (c, yytext);
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

  lexer_flags.quote_is_transpose = true;
  lexer_flags.convert_spaces_to_comma = true;

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

// FIXME -- we need to handle block comments here.

static bool
have_continuation (bool trailing_comments_ok)
{
  std::ostringstream buf;

  std::string comment_buf;

  bool in_comment = false;
  bool beginning_of_comment = false;

  int c = 0;

  while ((c = text_yyinput ()) != EOF)
    {
      buf << static_cast<char> (c);

      switch (c)
	{
	case ' ':
	case '\t':
	  if (in_comment)
	    {
	      comment_buf += static_cast<char> (c);
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
		    comment_buf += static_cast<char> (c);
		}
	      else
		{
		  maybe_gripe_matlab_incompatible_comment (c);
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
	      comment_buf += static_cast<char> (c);
	      octave_comment_buffer::append (comment_buf);
	    }
	  current_input_column = 0;
	  promptflag--;
	  gripe_matlab_incompatible_continuation ();
	  return true;

	default:
	  if (in_comment)
	    {
	      comment_buf += static_cast<char> (c);
	      beginning_of_comment = false;
	    }
	  else
	    goto cleanup;
	  break;
	}
    }

  xunput (c, yytext);
  return false;

cleanup:

  std::string s = buf.str ();

  int len = s.length ();
  while (len--)
    xunput (s[len], yytext);

  return false;
}

// We have seen a `.' and need to see if it is the start of a
// continuation.  If so, this eats it, up to and including the new
// line character.

static bool
have_ellipsis_continuation (bool trailing_comments_ok)
{
  char c1 = text_yyinput ();
  if (c1 == '.')
    {
      char c2 = text_yyinput ();
      if (c2 == '.' && have_continuation (trailing_comments_ok))
	return true;
      else
	{
	  xunput (c2, yytext);
	  xunput (c1, yytext);
	}
    }
  else
    xunput (c1, yytext);

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

  int c = text_yyinput ();

  if ((c == '.' && have_ellipsis_continuation ())
      || (c == '\\' && have_continuation ()))
    retval = eat_whitespace ();
  else
    xunput (c, yytext);

  return retval;
}

static int
handle_string (char delim, int text_style)
{
  std::ostringstream buf;

  int bos_line = input_line_number;
  int bos_col = current_input_column;

  int c;
  int escape_pending = 0;

  while ((c = text_yyinput ()) != EOF)
    {
      current_input_column++;

      if (c == '\\')
	{
	  if (delim == '\'' || escape_pending)
	    {
	      buf << static_cast<char> (c);
	      escape_pending = 0;
	    }
	  else
	    {
	      if (have_continuation (false))
		escape_pending = 0;
	      else
		{
		  buf << static_cast<char> (c);
		  escape_pending = 1;
		}
	    }
	  continue;
	}
      else if (c == '.')
	{
	  if (delim == '\'' || ! have_ellipsis_continuation (false))
	    buf << static_cast<char> (c);
	}
      else if (c == '\n')
	{
	  error ("unterminated string constant");
	  break;
	}
      else if (c == delim)
	{
	  if (escape_pending)
	    buf << static_cast<char> (c);
	  else
	    {
	      c = text_yyinput ();
	      if (c == delim)
		{
		  buf << static_cast<char> (c);		    
		  if (lexer_flags.doing_rawcommand)
		    buf << static_cast<char> (c);
		}
	      else
		{
		  std::string s;  
		  xunput (c, yytext);

		  if (lexer_flags.doing_rawcommand || delim == '\'')
		    s = buf.str ();
		  else
		    s = do_string_escapes (buf.str ());

		  if (text_style && lexer_flags.doing_rawcommand)
		    s = std::string (1, delim) + s + std::string (1, delim);
		  else
		    {
		      lexer_flags.quote_is_transpose = true;
		      lexer_flags.convert_spaces_to_comma = true;
		    }

		  yylval.tok_val = new token (s, bos_line, bos_col);
		  token_stack.push (yylval.tok_val);

		  if (delim == '"')
		    gripe_matlab_incompatible ("\" used as string delimiter");
		  else if (delim == '\'')
		    gripe_single_quote_string ();

		  return delim == '"' ? DQ_STRING : SQ_STRING;
		}
	    }
	}
      else
	{
	  buf << static_cast<char> (c);
	}

      escape_pending = 0;
    }

  return LEXICAL_ERROR;
}

static bool
next_token_is_assign_op (void)
{
  bool retval = false;

  int c0 = text_yyinput ();

  switch (c0)
    {
    case '=':
      {
	int c1 = text_yyinput ();
	xunput (c1, yytext);
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
	int c1 = text_yyinput ();
	xunput (c1, yytext);
	if (c1 == '=')
	  retval = true;
      }
      break;

    case '.':
      {
	int c1 = text_yyinput ();
	if (match_any (c1, "+-*/\\"))
	  {
	    int c2 = text_yyinput ();
	    xunput (c2, yytext);
	    if (c2 == '=')
	      retval = true;
	  }
	xunput (c1, yytext);
      }
      break;

    case '>':
      {
	int c1 = text_yyinput ();
	if (c1 == '>')
	  {
	    int c2 = text_yyinput ();
	    xunput (c2, yytext);
	    if (c2 == '=')
	      retval = true;
	  }
	xunput (c1, yytext);
      }
      break;

    case '<':
      {
	int c1 = text_yyinput ();
	if (c1 == '<')
	  {
	    int c2 = text_yyinput ();
	    xunput (c2, yytext);
	    if (c2 == '=')
	      retval = true;
	  }
	xunput (c1, yytext);
      }
      break;

    default:
      break;
    }

  xunput (c0, yytext);

  return retval;
}

static bool
next_token_is_index_op (void)
{
  int c = text_yyinput ();
  xunput (c, yytext);
  return c == '(' || c == '{';
}

static int
handle_close_bracket (bool spc_gobbled, int bracket_type)
{
  int retval = bracket_type;

  if (! nesting_level.none ())
    {
      nesting_level.remove ();

      if (bracket_type == ']')
	lexer_flags.bracketflag--;
      else if (bracket_type == '}')
	lexer_flags.braceflag--;
      else
	panic_impossible ();
    }

  if (lexer_flags.bracketflag == 0 && lexer_flags.braceflag == 0)
    BEGIN (INITIAL);

  if (bracket_type == ']'
      && next_token_is_assign_op ()
      && ! lexer_flags.looking_at_return_list)
    {
      retval = CLOSE_BRACE;
    }
  else if ((lexer_flags.bracketflag || lexer_flags.braceflag)
	   && lexer_flags.convert_spaces_to_comma
	   && (nesting_level.is_bracket ()
	       || (nesting_level.is_brace ()
		   && ! lexer_flags.looking_at_object_index)))
    {
      bool index_op = next_token_is_index_op ();

      // Don't insert comma if we are looking at something like
      //
      //   [x{i}{j}] or [x{i}(j)]
      //
      // but do if we are looking at
      //
      //   [x{i} {j}] or [x{i} (j)]

      if (spc_gobbled || ! (bracket_type == '}' && index_op))
	{
	  bool bin_op = next_token_is_bin_op (spc_gobbled);

	  bool postfix_un_op = next_token_is_postfix_unary_op (spc_gobbled);

	  bool sep_op = next_token_is_sep_op ();

	  if (! (postfix_un_op || bin_op || sep_op))
	    {
	      maybe_warn_separator_insert (',');

	      yyunput (',', yytext);
	      return retval;
	    }
	}
    }

  lexer_flags.quote_is_transpose = true;
  lexer_flags.convert_spaces_to_comma = true;

  return retval;
}

static void
maybe_unput_comma (int spc_gobbled)
{
  if (nesting_level.is_bracket ()
      || (nesting_level.is_brace ()
	  && ! lexer_flags.looking_at_object_index))
    {
      int bin_op = next_token_is_bin_op (spc_gobbled);

      int postfix_un_op = next_token_is_postfix_unary_op (spc_gobbled);

      int c1 = text_yyinput ();
      int c2 = text_yyinput ();

      xunput (c2, yytext);
      xunput (c1, yytext);

      int sep_op = next_token_is_sep_op ();

      int dot_op = (c1 == '.'
		    && (isalpha (c2) || isspace (c2) || c2 == '_'));

      if (postfix_un_op || bin_op || sep_op || dot_op)
	return;

      int index_op = (c1 == '(' || c1 == '{');

      // If there is no space before the indexing op, we don't insert
      // a comma.

      if (index_op && ! spc_gobbled)
	return;

      maybe_warn_separator_insert (',');

      yyunput (',', yytext);
    }
}

// Figure out exactly what kind of token to return when we have seen
// an identifier.  Handles keywords.  Return -1 if the identifier
// should be ignored.

static int
handle_identifier (void)
{
  std::string tok = strip_trailing_whitespace (yytext);

  int c = yytext[yyleng-1];

  int cont_is_spc = eat_continuation ();

  int spc_gobbled = (cont_is_spc || c == ' ' || c == '\t');

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

      lexer_flags.quote_is_transpose = true;
      lexer_flags.convert_spaces_to_comma = true;

      current_input_column += yyleng;

      return STRUCT_ELT;
    }

  int kw_token = is_keyword_token (tok);

  if (lexer_flags.looking_at_function_handle)
    {
      if (kw_token)
	{
	  error ("function handles may not refer to keywords");

	  return LEXICAL_ERROR;
	}
      else
	{
	  yylval.tok_val = new token (tok, input_line_number,
				      current_input_column);

	  token_stack.push (yylval.tok_val);

	  current_input_column += yyleng;
	  lexer_flags.quote_is_transpose = false;
	  lexer_flags.convert_spaces_to_comma = true;

	  return FCN_HANDLE;
	}
    }

  // If we have a regular keyword, return it.
  // Keywords can be followed by identifiers.

  if (kw_token)
    {
      if (kw_token >= 0)
	{
	  current_input_column += yyleng;
	  lexer_flags.quote_is_transpose = false;
	  lexer_flags.convert_spaces_to_comma = true;
	}

      return kw_token;
    }

  // See if we have a plot keyword (title, using, with, or clear).

  int c1 = text_yyinput ();

  bool next_tok_is_paren = (c1 == '(');

  bool next_tok_is_eq = false;
  if (c1 == '=')
    {
      int c2 = text_yyinput ();
      xunput (c2, yytext);

      if (c2 != '=')
	next_tok_is_eq = true;
    }

  xunput (c1, yytext);

  // Kluge alert.
  //
  // If we are looking at a text style function, set up to gobble its
  // arguments.
  //
  // If the following token is `=', or if we are parsing a function
  // return list or function parameter list, or if we are looking at
  // something like [ab,cd] = foo (), force the symbol to be inserted
  // as a variable in the current symbol table.

  if (is_command_name (tok) && ! is_variable (tok))
    {
      if (next_tok_is_eq
	  || lexer_flags.looking_at_return_list
	  || (lexer_flags.looking_at_parameter_list
	      && ! lexer_flags.looking_at_initializer_expression))
	{
	  force_local_variable (tok);
	}
      else if (lexer_flags.looking_at_matrix_or_assign_lhs)
	{
	  lexer_flags.pending_local_variables.insert (tok);
	}
      else if (! (next_tok_is_paren || lexer_flags.looking_at_object_index))
	{
	  BEGIN (COMMAND_START);
	}

      if (is_rawcommand_name (tok) && ! lexer_flags.looking_at_object_index)
	{
	  lexer_flags.doing_rawcommand = true;
	  BEGIN (COMMAND_START);
	}
    }

  // Find the token in the symbol table.  Beware the magic
  // transformation of the end keyword...

  if (tok == "end")
    tok = "__end__";    

  yylval.tok_val = new token (&(symbol_table::insert (tok)),
			      input_line_number, current_input_column);

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

void
lexical_feedback::init (void)
{
  // Not initially defining a matrix list.
  bracketflag = 0;

  // Not initially defining a cell array list.
  braceflag = 0;

  // Not initially inside a loop or if statement.
  looping = 0;

  // Not initially defining a function.
  defining_func = false;
  parsed_function_name = false;
  parsing_nested_function = 0;
  parsing_class_method = false;

  // Not initiallly looking at a function handle.
  looking_at_function_handle = 0;

  // Not parsing a function return or parameter list.
  looking_at_return_list = false;
  looking_at_parameter_list = false;

  // Not looking at an argument list initializer expression.
  looking_at_initializer_expression = false;

  // Not parsing a matrix or the left hand side of multi-value
  // assignment statement.
  looking_at_matrix_or_assign_lhs = false;

  // Not parsing an object index.
  looking_at_object_index = 0;

  // No need to do comma insert or convert spaces to comma at
  // beginning of input. 
  convert_spaces_to_comma = true;
  do_comma_insert = false;

  // Not initially doing any plotting or setting of plot attributes.
  doing_rawcommand = false;

  // Not initially looking at indirect references.
  looking_at_indirect_ref = false;

  // Quote marks strings intially.
  quote_is_transpose = false;

  // Set of identifiers that might be local variable names is empty.
  pending_local_variables.clear ();
}

bool
is_keyword (const std::string& s)
{
  return octave_kw_hash::in_word_set (s.c_str (), s.length ()) != 0;
}

DEFCMD (iskeyword, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} iskeyword (@var{name})\n\
Return true if @var{name} is an Octave keyword.  If @var{name}\n\
is omitted, return a list of keywords.\n\
@end deftypefn")
{
  octave_value retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("iskeyword");

  if (error_state)
    return retval;

  if (argc == 1)
    {
      string_vector lst (TOTAL_KEYWORDS);

      for (int i = 0; i < TOTAL_KEYWORDS; i++)
	lst[i] = wordlist[i].name;

      retval = Cell (lst.qsort ());
    }
  else if (argc == 2)
    {
      retval = is_keyword (argv[1]);
    }
  else
    print_usage ();

  return retval;
}

void
prep_lexer_for_script (void)
{
  BEGIN (SCRIPT_FILE_BEGIN);
}

static void
maybe_warn_separator_insert (char sep)
{
  std::string nm = curr_fcn_file_full_name;

  if (nm.empty ())
    warning_with_id ("Octave:separator-insert",
		     "potential auto-insertion of `%c' near line %d",
		     sep, input_line_number);
  else
    warning_with_id ("Octave:separator-insert",
		     "potential auto-insertion of `%c' near line %d of file %s",
		     sep, input_line_number, nm.c_str ());
}

static void
gripe_single_quote_string (void)
{
  std::string nm = curr_fcn_file_full_name;

  if (nm.empty ())
    warning_with_id ("Octave:single-quote-string",
		     "single quote delimited string near line %d",
		     input_line_number);
  else
    warning_with_id ("Octave:single-quote-string",
		     "single quote delimited string near line %d of file %s",
		     input_line_number, nm.c_str ());
}

static void
gripe_matlab_incompatible (const std::string& msg)
{
  warning_with_id ("Octave:matlab-incompatible",
		   "potential Matlab compatibility problem: %s",
		   msg.c_str ());
}

static void
maybe_gripe_matlab_incompatible_comment (char c)
{
  if (c == '#')
    gripe_matlab_incompatible ("# used as comment character");
}

static void
gripe_matlab_incompatible_continuation (void)
{
  gripe_matlab_incompatible ("\\ used as line continuation marker");
}

static void
gripe_matlab_incompatible_operator (const std::string& op)
{
  std::string t = op;
  int n = t.length ();
  if (t[n-1] == '\n')
    t.resize (n-1);
  gripe_matlab_incompatible (t + " used as operator");
}

static void
display_token (int tok)
{
  switch (tok)
    {
    case '=': std::cerr << "'='\n"; break;
    case ':': std::cerr << "':'\n"; break;
    case '-': std::cerr << "'-'\n"; break;
    case '+': std::cerr << "'+'\n"; break;
    case '*': std::cerr << "'*'\n"; break;
    case '/': std::cerr << "'/'\n"; break;
    case ADD_EQ: std::cerr << "ADD_EQ\n"; break;
    case SUB_EQ: std::cerr << "SUB_EQ\n"; break;
    case MUL_EQ: std::cerr << "MUL_EQ\n"; break;
    case DIV_EQ: std::cerr << "DIV_EQ\n"; break;
    case LEFTDIV_EQ: std::cerr << "LEFTDIV_EQ\n"; break;
    case POW_EQ: std::cerr << "POW_EQ\n"; break;
    case EMUL_EQ: std::cerr << "EMUL_EQ\n"; break;
    case EDIV_EQ: std::cerr << "EDIV_EQ\n"; break;
    case ELEFTDIV_EQ: std::cerr << "ELEFTDIV_EQ\n"; break;
    case EPOW_EQ: std::cerr << "EPOW_EQ\n"; break;
    case AND_EQ: std::cerr << "AND_EQ\n"; break;
    case OR_EQ: std::cerr << "OR_EQ\n"; break;
    case LSHIFT_EQ: std::cerr << "LSHIFT_EQ\n"; break;
    case RSHIFT_EQ: std::cerr << "RSHIFT_EQ\n"; break;
    case LSHIFT: std::cerr << "LSHIFT\n"; break;
    case RSHIFT: std::cerr << "RSHIFT\n"; break;
    case EXPR_AND_AND: std::cerr << "EXPR_AND_AND\n"; break;
    case EXPR_OR_OR: std::cerr << "EXPR_OR_OR\n"; break;
    case EXPR_AND: std::cerr << "EXPR_AND\n"; break;
    case EXPR_OR: std::cerr << "EXPR_OR\n"; break;
    case EXPR_NOT: std::cerr << "EXPR_NOT\n"; break;
    case EXPR_LT: std::cerr << "EXPR_LT\n"; break;
    case EXPR_LE: std::cerr << "EXPR_LE\n"; break;
    case EXPR_EQ: std::cerr << "EXPR_EQ\n"; break;
    case EXPR_NE: std::cerr << "EXPR_NE\n"; break;
    case EXPR_GE: std::cerr << "EXPR_GE\n"; break;
    case EXPR_GT: std::cerr << "EXPR_GT\n"; break;
    case LEFTDIV: std::cerr << "LEFTDIV\n"; break;
    case EMUL: std::cerr << "EMUL\n"; break;
    case EDIV: std::cerr << "EDIV\n"; break;
    case ELEFTDIV: std::cerr << "ELEFTDIV\n"; break;
    case EPLUS: std::cerr << "EPLUS\n"; break;
    case EMINUS: std::cerr << "EMINUS\n"; break;
    case QUOTE: std::cerr << "QUOTE\n"; break;
    case TRANSPOSE: std::cerr << "TRANSPOSE\n"; break;
    case PLUS_PLUS: std::cerr << "PLUS_PLUS\n"; break;
    case MINUS_MINUS: std::cerr << "MINUS_MINUS\n"; break;
    case POW: std::cerr << "POW\n"; break;
    case EPOW: std::cerr << "EPOW\n"; break;
    case NUM: std::cerr << "NUM\n"; break;
    case IMAG_NUM: std::cerr << "IMAG_NUM\n"; break;
    case STRUCT_ELT: std::cerr << "STRUCT_ELT\n"; break;
    case NAME: std::cerr << "NAME\n"; break;
    case END: std::cerr << "END\n"; break;
    case DQ_STRING: std::cerr << "DQ_STRING\n"; break;
    case SQ_STRING: std::cerr << "SQ_STRING\n"; break;
    case FOR: std::cerr << "FOR\n"; break;
    case WHILE: std::cerr << "WHILE\n"; break;
    case DO: std::cerr << "DO\n"; break;
    case UNTIL: std::cerr << "UNTIL\n"; break;
    case IF: std::cerr << "IF\n"; break;
    case ELSEIF: std::cerr << "ELSEIF\n"; break;
    case ELSE: std::cerr << "ELSE\n"; break;
    case SWITCH: std::cerr << "SWITCH\n"; break;
    case CASE: std::cerr << "CASE\n"; break;
    case OTHERWISE: std::cerr << "OTHERWISE\n"; break;
    case BREAK: std::cerr << "BREAK\n"; break;
    case CONTINUE: std::cerr << "CONTINUE\n"; break;
    case FUNC_RET: std::cerr << "FUNC_RET\n"; break;
    case UNWIND: std::cerr << "UNWIND\n"; break;
    case CLEANUP: std::cerr << "CLEANUP\n"; break;
    case TRY: std::cerr << "TRY\n"; break;
    case CATCH: std::cerr << "CATCH\n"; break;
    case GLOBAL: std::cerr << "GLOBAL\n"; break;
    case STATIC: std::cerr << "STATIC\n"; break;
    case FCN_HANDLE: std::cerr << "FCN_HANDLE\n"; break;
    case END_OF_INPUT: std::cerr << "END_OF_INPUT\n\n"; break;
    case LEXICAL_ERROR: std::cerr << "LEXICAL_ERROR\n\n"; break;
    case FCN: std::cerr << "FCN\n"; break;
    case CLOSE_BRACE: std::cerr << "CLOSE_BRACE\n"; break;
    case '\n': std::cerr << "\\n\n"; break;
    case '\r': std::cerr << "\\r\n"; break;
    case '\t': std::cerr << "TAB\n"; break;
    default:
      {
        if (tok < 256)
	  std::cerr << static_cast<char> (tok) << "\n";
	else
	  std::cerr << "UNKNOWN(" << tok << ")\n";
      }
      break;
    }
}

DEFUN (__display_tokens__, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __display_tokens__\n\
Query or set the internal variable that determines whether Octave's\n\
lexer displays tokens as they are read.\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (display_tokens);
}

DEFUN (__token_count__, , ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __token_count__\n\
Number of language tokens processed since Octave startup.\n\
@end deftypefn")
{
  return octave_value (Vtoken_count);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
