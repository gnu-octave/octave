/* lex.l                                                -*- C -*-

Copyright (C) 1992, 1993 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

%x COMMENT
%x NEW_MATRIX
%x HELP_FCN
%s TEXT_FCN
%s DQSTRING
%s STRING
%s MATRIX

%{

// Arrange to get input via readline.

#ifdef YY_INPUT
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
  if ((result = octave_read (buf, max_size)) < 0) \
    YY_FATAL_ERROR ("octave_read () in flex scanner failed");
#endif

// Try to avoid crashing out completely on fatal scanner errors.

#ifdef YY_FATAL_ERROR
#undef YY_FATAL_ERROR
#define YY_FATAL_ERROR(msg) \
  do \
    { \
      error (msg); \
      jump_to_top_level (); \
    } \
  while (0)
#endif

#include "input.h"

// The type of an END token.  This declaration is repeated in parse.y.
// It must appear before y.tab.h is included.
enum end_tok_type
  {
    simple_end,
    for_end,
    function_end,
    if_end,
    while_end,
  };

// The type of a PLOT token.  This declaration is repeated in parse.y.
// It must appear before y.tab.h is included.
enum plot_tok_type
  {
    two_dee = 2,
    three_dee = 3,
  };

#include "SLStack.h"

#include "variables.h"
#include "symtab.h"
#include "error.h"
#include "utils.h"
#include "tree.h"
#include "y.tab.h"
#include "parse.h"
#include "lex.h"

// Nonzero means we thing we are looking at the beginning of a
// function definition.
static int beginning_of_function = 0;

// Nonzero means we think we are looking at a set command.
static int doing_set = 0;

// GAG.  Stupid kludge so that [[1,2][3,4]] will work.
static do_comma_insert = 0;

// Brace level count.
static int braceflag = 0;

// Return transpose or start a string?
static int quote_is_transpose = 0;

// Nonzero means that we should convert spaces to a comma inside a
// matrix definition.
static int convert_spaces_to_comma = 1;

// Another context hack, this time for the plot command's `using',
// `title', and `with' keywords.
static int cant_be_identifier = 0;

// Is the closest nesting level a square brace or a paren?
//
//  1 -> brace, spaces are important (they can turn into commas)
//  0 -> paren, spaces are not important
//
static SLStack <int> in_brace_or_paren;

static void do_string_escapes (char *s);
static void fixup_column_count (char *s);
static int do_comma_insert_check (void);
static int is_plot_keyword (char *s);
static int is_keyword (char *s);
static char *plot_style_token (char *s);
static symbol_record *lookup_identifier (char *s);
static void grab_help_text (void);
static int match_any (char c, char *s);
static int next_token_is_bin_op (int spc_prev, char *yytext);
static int next_token_is_postfix_unary_op (int spc_prev, char *yytext);
static char *strip_trailing_whitespace (char *s);

#define DO_COMMA_INSERT_CHECK yyless (do_comma_insert_check ())

#define RETURN(token) \
  do \
    { \
      current_input_column += yyleng; \
      quote_is_transpose = 0; \
      cant_be_identifier = 0; \
      convert_spaces_to_comma = 1; \
      return (token); \
    } \
  while (0)

#define BIN_OP_RETURN(token) \
  do \
    { \
      current_input_column += yyleng; \
      quote_is_transpose = 0; \
      cant_be_identifier = 0; \
      convert_spaces_to_comma = 0; \
      return (token); \
    } \
  while (0)

%}

D	[0-9]
S	[ \t]
N	[\n]
SN	[ \t\n]
EL	(\.\.\.)
Im	[iIjJ]
QQ	(\'\')
ECHAR	(\\.)
QSTR	([^\n\'\\]*({QQ}|{ECHAR})*)
DQSTR	([^\n\"\\]*{ECHAR}*)
IDENT	([_a-zA-Z][_a-zA-Z0-9]*)
EXPON	([DdEe][+-]?{D}+)
%%

\%			|
\#			{
			  if (beginning_of_function)
			    {
			      grab_help_text ();
			      beginning_of_function = 0;
			    }

			  BEGIN COMMENT;
			  current_input_column += yyleng;
			}

<COMMENT>\n		{
			  BEGIN 0;
			  current_input_column = 0;
			  quote_is_transpose = 0;
			  cant_be_identifier = 0;
			  convert_spaces_to_comma = 1;
			  return '\n';
			}

<COMMENT><<EOF>>	{ RETURN (END_OF_INPUT); }

<COMMENT>.*$		{ current_input_column += yyleng; }

<NEW_MATRIX>[^ \t\n]	{
			  yyless (0);
			  BEGIN MATRIX;
			}

<NEW_MATRIX>{SN}*	{
			  fixup_column_count (yytext);
			  BEGIN MATRIX;
			}

<HELP_FCN>\n		|
<TEXT_FCN>\n		{
		          BEGIN 0;
			  current_input_column = 0;
			  quote_is_transpose = 0;
			  cant_be_identifier = 0;
			  convert_spaces_to_comma = 1;
			  return '\n';
			}

<TEXT_FCN>[\;\,]	{
			  if (doing_set)
			    {
			      yylval.string = strsave (yytext);
			      RETURN (TEXT);
			    }
			  else
			    {
			      BEGIN 0;
			      RETURN (',');
			    }
		        }

<HELP_FCN>[^ \t\n]*{S}*	    |
<TEXT_FCN>[^ \t\n\;\,]*{S}* {

			  static char *tok = (char *) NULL;
			  delete [] tok;
			  tok = strip_trailing_whitespace (yytext);

			  yylval.string = strsave (tok);
			  RETURN (TEXT);
			}

<TEXT_FCN>\'{QSTR}*[\n\'] {
			  if (yytext[yyleng-1] == '\n')
			    {
			      error ("unterminated string constant");
			      current_input_column = 0;
			      jump_to_top_level ();
			    }
			  else
			    {
			      int off1 = doing_set ? 0 : 1;
			      int off2 = doing_set ? 0 : 2;
			      yylval.string = strsave (&yytext[off1]);
			      yylval.string[yyleng-off2] = '\0';
			      current_input_column += yyleng;
			    }
			  do_string_escapes (yylval.string);
			  return TEXT;
			}

<TEXT_FCN>\"{DQSTR}*[\n\"] {
			  if (yytext[yyleng-1] == '\n')
			    {
			      error ("unterminated string constant");
			      current_input_column = 0;
			      jump_to_top_level ();
			    }
			  else
			    {
			      int off1 = doing_set ? 0 : 1;
			      int off2 = doing_set ? 0 : 2;
			      yylval.string = strsave (&yytext[off1]);
			      yylval.string[yyleng-off2] = '\0';
			      current_input_column += yyleng;
			    }
			  do_string_escapes (yylval.string);
			  return TEXT;
			}

<TEXT_FCN>{S}*		{ current_input_column += yyleng; }

<STRING>{QSTR}*[\n\']	{
			  if (braceflag)
			    BEGIN MATRIX;
			  else
			    BEGIN 0;

			  if (yytext[yyleng-1] == '\n')
			    {
			      error ("unterminated string constant");
			      current_input_column = 0;
			      jump_to_top_level ();
			    }
			  else
			    {
			      yylval.string = strsave (yytext);
			      yylval.string[yyleng-1] = '\0';
			      current_input_column += yyleng;
			    }
			  do_string_escapes (yylval.string);
			  quote_is_transpose = 1;
			  cant_be_identifier = 1;
			  convert_spaces_to_comma = 1;
			  return TEXT;
			}


<DQSTRING>{DQSTR}*[\n\"] {
			  if (braceflag)
			    BEGIN MATRIX;
			  else
			    BEGIN 0;

			  if (yytext[yyleng-1] == '\n')
			    {
			      error ("unterminated string constant");
			      current_input_column = 0;
			      jump_to_top_level ();
			    }
			  else
			    {
			      yylval.string = strsave (yytext);
			      yylval.string[yyleng-1] = '\0';
			      current_input_column += yyleng;
			    }
			  do_string_escapes (yylval.string);
			  quote_is_transpose = 1;
			  cant_be_identifier = 1;
			  convert_spaces_to_comma = 1;
			  return TEXT;
			}

<MATRIX>{SN}*\]{S}*/==	{

// For this and the next two rules, we're looking at ']', and we
// need to know if the next token is '='.
//
// All this so we can handle the bogus syntax 
//
//   [x,y]                % an expression by itself
//   [x,y] = expression   % assignment to a list of identifiers
//   [x,y] == expression  % test for equality
//
// It would have been so much easier if the delimiters were simply
// different for the expression on the left hand side of the equals
// operator.

			  in_brace_or_paren.pop ();
			  braceflag--;
			  if (braceflag == 0)
			    {
			      if (!defining_func)
				promptflag++;
			      BEGIN 0;
			    }
			  fixup_column_count (yytext);
			  quote_is_transpose = 0;
			  cant_be_identifier = 0;
			  convert_spaces_to_comma = 1;
			  return ']';
			}

<MATRIX>{SN}*\]{S}*/=	{
			  in_brace_or_paren.pop ();
			  braceflag--;
			  if (braceflag == 0)
			    {
			      BEGIN 0;
			      if (!defining_func)
				promptflag++;
			    }
			  fixup_column_count (yytext);
			  quote_is_transpose = 0;
			  cant_be_identifier = 0;
			  convert_spaces_to_comma = 1;
			  if (maybe_screwed_again)
			    return SCREW_TWO;
			  else
			    return ']';
			}

<MATRIX>{SN}*\]{S}*	{
			  fixup_column_count (yytext);

			  in_brace_or_paren.pop ();
			  braceflag--;
			  if (braceflag == 0)
			    {
			      if (!defining_func)
				promptflag++;
			      BEGIN 0;
			    }
			  else
			    {
			      int c0 = yytext[yyleng-1];
			      int spc_prev = (c0 == ' ' || c0 == '\t');
			      int bin_op = next_token_is_bin_op (spc_prev,
								 yytext);
			      int postfix_un_op
				= next_token_is_postfix_unary_op (spc_prev,
								  yytext);

			      int c1 = yyinput ();
			      unput (c1);
			      int other_op = match_any (c1, ",;\n]");

			      if (! (postfix_un_op || bin_op || other_op)
				     && in_brace_or_paren.top ()
				     && convert_spaces_to_comma)
				{
				  unput (',');
				  return ']';
				}
			    }

			  quote_is_transpose = 1;
			  cant_be_identifier = 0;
			  convert_spaces_to_comma = 1;
			  return ']';
			}

<MATRIX>{S}*\,{S}*	{ RETURN (','); }

<MATRIX>{S}+		{
			  int bin_op = next_token_is_bin_op (1, yytext);
			  int postfix_un_op
			    = next_token_is_postfix_unary_op (1, yytext);

 			  if (! (postfix_un_op || bin_op)
			      && in_brace_or_paren.top ()
			      && convert_spaces_to_comma)
			    RETURN (',');
			}

<MATRIX>{SN}*\;{SN}*	|
<MATRIX>{N}{SN}*	{
			  fixup_column_count (yytext);
			  quote_is_transpose = 0;
			  cant_be_identifier = 0;
			  convert_spaces_to_comma = 1;
			  return ';';
			}

\]			{
			  if (! in_brace_or_paren.empty ())
			    in_brace_or_paren.pop ();

			  if (plotting && ! past_plot_range)
			    {
			      in_plot_range = 0;
			      RETURN (CLOSE_BRACE);
			    }
			  else
			    RETURN (']');
			}

{D}+{EXPON}?{Im}	|
{D}+\.{D}*{EXPON}?{Im}	|
\.{D}+{EXPON}?{Im}	{
			  int nread = sscanf (yytext, "%lf", &(yylval.number));
			  assert (nread == 1);
			  quote_is_transpose = 1;
			  cant_be_identifier = 1;
			  convert_spaces_to_comma = 1;
			  if (plotting && ! in_plot_range)
			    past_plot_range = 1;
			  current_input_column += yyleng;
			  DO_COMMA_INSERT_CHECK;
			  return IMAG_NUM;
			}

{D}+{EXPON}?		|
{D}+\.{D}*{EXPON}?	|
\.{D}+{EXPON}?		|
			{
			  int nread = sscanf (yytext, "%lf", &(yylval.number));
			  assert (nread == 1);
			  quote_is_transpose = 1;
			  cant_be_identifier = 1;
			  convert_spaces_to_comma = 1;
			  if (plotting && ! in_plot_range)
			    past_plot_range = 1;
			  current_input_column += yyleng;
			  DO_COMMA_INSERT_CHECK;
			  return NUM;
			}

\[{S}*		{
		  in_brace_or_paren.push (1);
		  if (plotting && ! past_plot_range)
		    {
		      in_plot_range = 1;
		      RETURN (OPEN_BRACE);
		    }

		  if (do_comma_insert)
		    {
		      yyless (0);
		      do_comma_insert = 0;
		      quote_is_transpose = 0;
		      cant_be_identifier = 0;
		      convert_spaces_to_comma = 1;
		      return (',');
		    }
		  else
		    {
		      mlnm.push (1);
		      braceflag++;
		      promptflag--;
		      BEGIN NEW_MATRIX;
		      RETURN ('[');
		    }
		}

{S}*		{ current_input_column += yyleng; }

{EL}{S}*\n	{

// Line continuation.

		  promptflag--;
		  current_input_column = 0;
		}

<<EOF>>		RETURN (END_OF_INPUT);

{IDENT}{S}*	{

// Truncate the token at the first space or tab but don't write
// directly on yytext.

		  static char *tok = (char *) NULL;
		  delete [] tok;
		  tok = strip_trailing_whitespace (yytext);

		  int kw_token = is_keyword (tok);
		  if (kw_token)
		    RETURN (kw_token);

		  if (plotting && cant_be_identifier)
		    {
		      int plot_option_kw = is_plot_keyword (tok);
		      if (plot_option_kw)
			{
			  quote_is_transpose = 0;
			  cant_be_identifier = 0;
			  convert_spaces_to_comma = 1;
			  current_input_column += yyleng;
			  return plot_option_kw;
			}
		    }

		  if (plotting && ! in_plot_range)
		    past_plot_range = 1;

		  if (plotting && in_plot_style)
		    {
		      char *sty = plot_style_token (&tok[1]);
		      if (sty != (char *) NULL)
			{
			  yylval.string = strsave (sty);
			  if (in_plot_style)
			    {
			      in_plot_style = 0;
			      RETURN (STYLE);
			    }
			}
		    }

		  cant_be_identifier = 1;

// If we are looking at a text style function, set up to gobble its
// arguments.  These are also reserved words, but only because it
// would be very difficult to do anything intelligent with them if
// they were not reserved.

		  if (is_text_function_name (tok))
		    {
		      BEGIN TEXT_FCN;

		      if (strcmp (tok, "clear") == 0)
			return CLEAR;
		      else if (strcmp (tok, "help") == 0)
			BEGIN HELP_FCN;
		      else if (strcmp (tok, "set") == 0)
			doing_set = 1;
		    }

		  yylval.sym_rec = lookup_identifier (tok);

		  quote_is_transpose = 1;
		  current_input_column += yyleng;
		  DO_COMMA_INSERT_CHECK;

		  if (! in_brace_or_paren.empty ()
		      && in_brace_or_paren.top ())
		    {
		      int c0 = yytext[yyleng-1];
		      int spc_prev = (c0 == ' ' || c0 == '\t');
		      int bin_op = next_token_is_bin_op (spc_prev, yytext);

		      int postfix_un_op
			= next_token_is_postfix_unary_op (spc_prev, yytext);

		      int c1 = yyinput ();
		      unput (c1);
		      int other_op = match_any (c1, ",;\n](");

		      if (! (postfix_un_op || bin_op || other_op))
			unput (',');
		    }

		  convert_spaces_to_comma = 1;
		  return NAME;
		}

{IDENT}/{S}*=	{

// We've found an identifier followed by some space and an equals
// sign.  If we are working on a function definition and the previous
// token was `function', we have something like this
//
//    function x = y <list> end
//
// which is a function named y returning a variable named x.  The
// symbol y belongs in the global symbol table (nested function
// definitions are illegal) and the symbol x belongs in the 
// symbol table local to the function. 
//
// If we're not defining a function, this should function exactly like
// the case above.  I suppose it would be nice to avoid duplicating
// all the code, eh?

		  int kw_token = is_keyword (yytext);
		  if (kw_token)
		    RETURN (kw_token);

		  if (plotting && cant_be_identifier)
		    {
		      int plot_option_kw = is_plot_keyword (yytext);
		      if (plot_option_kw)
			{
			  quote_is_transpose = 0;
		  	  convert_spaces_to_comma = 1;
			  current_input_column += yyleng;
			  return plot_option_kw;
			}
		    }
		
		  cant_be_identifier = 1;

// If we are looking at a text style function, set up to gobble its
// arguments.  These are also reserved words, but only because it
// would be very difficult to do anything intelligent with them if
// they were not reserved.

		  if (is_text_function_name (yytext))
		    {
		      BEGIN TEXT_FCN;

		      if (strcmp (yytext, "clear") == 0)
			return CLEAR;
		      else if (strcmp (yytext, "help") == 0)
			BEGIN HELP_FCN;
		      else if (strcmp (yytext, "set") == 0)
			doing_set = 1;
		    }

		  if (defining_func && maybe_screwed)
		    curr_sym_tab = tmp_local_sym_tab;

		  yylval.sym_rec = lookup_identifier (yytext);

		  convert_spaces_to_comma = 1;
		  current_input_column += yyleng;
		  if (defining_func && maybe_screwed)
		    {
		      return SCREW;
		    }
		  else
		    {
		      quote_is_transpose = 1;
		      DO_COMMA_INSERT_CHECK;
		      return NAME;
		    }
		}

"\n"		{
		  quote_is_transpose = 0;
		  cant_be_identifier = 0;
		  current_input_column = 0;
		  convert_spaces_to_comma = 1;
		  return '\n';
		}

"'"		{
		  current_input_column++;
		  convert_spaces_to_comma = 1;

		  if (quote_is_transpose)
		    {
		      DO_COMMA_INSERT_CHECK;
		      return QUOTE;
		    }
		  else
		    BEGIN STRING;
		}

":"		{
		  if (plotting && (in_plot_range || in_plot_using))
		    RETURN (COLON);
		  else
		    BIN_OP_RETURN (':');
		}

\"		{ BEGIN DQSTRING; }
".**"		{ BIN_OP_RETURN (EPOW); }
".*"		{ BIN_OP_RETURN (EMUL); }
"./"		{ BIN_OP_RETURN (EDIV); }
".\\"		{ BIN_OP_RETURN (ELEFTDIV); }
".^"		{ BIN_OP_RETURN (EPOW); }
".'"		{ DO_COMMA_INSERT_CHECK; RETURN (TRANSPOSE); }
"++"		{ DO_COMMA_INSERT_CHECK; RETURN (PLUS_PLUS); }
"--"		{ DO_COMMA_INSERT_CHECK; RETURN (MINUS_MINUS); }
"<="		{ BIN_OP_RETURN (EXPR_LE); }
"=="		{ BIN_OP_RETURN (EXPR_EQ); }
"~="		{ BIN_OP_RETURN (EXPR_NE); }
"!="		{ BIN_OP_RETURN (EXPR_NE); }
"<>"		{ BIN_OP_RETURN (EXPR_NE); }
">="		{ BIN_OP_RETURN (EXPR_GE); }
"||"		{ BIN_OP_RETURN (EXPR_OR); }
"&&"		{ BIN_OP_RETURN (EXPR_AND); }
"|"		{ BIN_OP_RETURN (EXPR_OR); }
"&"		{ BIN_OP_RETURN (EXPR_AND); }
"!"		{
		  if (plotting && ! in_plot_range)
		    past_plot_range = 1;
		  RETURN (EXPR_NOT);
		}
"~"		{
		  if (plotting && ! in_plot_range)
		    past_plot_range = 1;
		  BIN_OP_RETURN (EXPR_NOT);
		}
"<"		{ BIN_OP_RETURN (EXPR_LT); }
">"		{ BIN_OP_RETURN (EXPR_GT); }
"+"		{ 
		  if (plotting && ! in_plot_range)
		    past_plot_range = 1;
		  BIN_OP_RETURN ('+');
		}
"-"		{
		  if (plotting && ! in_plot_range)
		    past_plot_range = 1;
		  BIN_OP_RETURN ('-');
		}
"**"		{ BIN_OP_RETURN (POW); }
"*"		{ BIN_OP_RETURN ('*'); }
"/"		{ BIN_OP_RETURN ('/'); }
"\\"		{ BIN_OP_RETURN (LEFTDIV); }
";"		{ RETURN (';'); }
","		{ RETURN (','); }
"^"		{ BIN_OP_RETURN (POW); }
"="		{ RETURN ('='); }
"("		{
		  if (plotting && ! in_plot_range)
		    past_plot_range = 1;
		  in_brace_or_paren.push (0);
		  RETURN ('(');
		}
")"		{
		  if (! in_brace_or_paren.empty ())
		    in_brace_or_paren.pop ();
		  DO_COMMA_INSERT_CHECK;
		  current_input_column++;
		  quote_is_transpose = 1;
		  return ')';
		}

.		{

// We return everything else as single character tokens, which should
// eventually result in a parse error.

		  RETURN (yytext[0]);
		}

%%

/*
 * GAG.
 *
 * If we're reading a matrix and the next character is '[', make sure
 * that we insert a comma ahead of it.
 */
int
do_comma_insert_check (void)
{
  int tmp_len = yyleng;
  int c = yyinput ();
  do_comma_insert = (braceflag && c == '[');
  return tmp_len;
}

/*
 * Fix things up for errors or interrupts.
 */
void
reset_parser (void)
{
  BEGIN 0;
  promptflag = 1;
  doing_set = 0;
  braceflag = 0;
  maybe_screwed = 0;
  maybe_screwed_again = 0;
  looping = 0;
  iffing = 0;
  ml.clear ();
  mlnm.clear ();
  defining_func = 0;
  curr_sym_tab = top_level_sym_tab;
  get_input_from_eval_string = 0;
  quote_is_transpose = 0;
  current_input_column = 0;
  do_comma_insert = 0;
  plotting = 0;
  past_plot_range = 0;
  in_plot_range = 0;
  in_plot_using = 0;
  in_plot_style = 0;
  cant_be_identifier = 0;
  convert_spaces_to_comma = 1;
  beginning_of_function = 0;
  in_brace_or_paren.clear ();
  yyrestart (stdin);
}

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
          warning ("unrecognized escape sequence `\\%c' -- converting to `%c'",
		   *p2, *p2);
	      *p1 = *p2;
	      break;
	    }
	}
      else if (*p2 == '\'' && *(p2+1) == '\'')
	{
	  *p1 = '\'';
	  p2++;
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

static void
fixup_column_count (char *s)
{
  char c;
  while ((c = *s++) != '\0')
    {
      if (c == '\n')
	  current_input_column = 0;
      else
	current_input_column++;
    }
}

#ifdef yywrap
#undef yywrap
#endif
int
yywrap (void)
{
  return 0;
}

/*
 * Tell us all what the current buffer is.
 */
YY_BUFFER_STATE
current_buffer (void)
{
  return YY_CURRENT_BUFFER;
}

/*
 * Create a new buffer.
 */
YY_BUFFER_STATE
create_buffer (FILE *f)
{
  return yy_create_buffer (f, YY_BUF_SIZE);
}

/*
 * Start reading a new buffer.
 */
void
switch_to_buffer (YY_BUFFER_STATE buf)
{
  yy_switch_to_buffer (buf);
}

/*
 * Delete a buffer.
 */
void
delete_buffer (YY_BUFFER_STATE buf)
{
  yy_delete_buffer (buf);
}

/*
 * Restore a buffer (for unwind-prot).
 */
void
restore_input_buffer (void *buf)
{
  switch_to_buffer ((YY_BUFFER_STATE) buf);
}

/*
 * Delete a buffer (for unwind-prot).
 */
void
delete_input_buffer (void *buf)
{
  delete_buffer ((YY_BUFFER_STATE) buf);
}

static char *plot_styles[] = 
  {
    "dots",
    "dots",
    "errorbars",
    "impulses",
    "lines",
    "linespoints",
    "points",
    (char *) NULL,
  };

static char *
plot_style_token (char *s)
{
  char **tmp = plot_styles;
  while (*tmp != (char *) NULL)
    {
      if (almost_match (*tmp, s))
	return *tmp;

      tmp++;
    }

  return (char *) NULL;
}

static int
is_plot_keyword (char *s)
{
  if (almost_match ("title", s))
    return TITLE;
  else if (almost_match ("using", s))
    { in_plot_using = 1; past_plot_range = 1; return USING; }
  else if (almost_match ("with", s))
    { in_plot_style = 1; past_plot_range = 1; return WITH; }
  else
    return 0;
}

/*
 * Handle keywords.  Could probably be more efficient...
 */
static int
is_keyword (char *s)
{
  if (plotting && in_plot_style)
    {
      char *sty = plot_style_token (s);
      if (sty != (char *) NULL)
	{
	  in_plot_style = 0;
	  yylval.string = strsave (sty);
	  return STYLE;
	}
    }

  int end_found = 0;
  if (strcmp ("break", s) == 0)
    return BREAK;
  else if (strcmp ("continue", s) == 0)
    return CONTINUE;
  else if (strcmp ("else", s) == 0)
    { return ELSE; }
  else if (strcmp ("elseif", s) == 0)
    { return ELSEIF; }
  else if (strcmp ("end", s) == 0)
    { end_found = 1; yylval.ettype = simple_end; }
  else if (strcmp ("endfor", s) == 0)
    { end_found = 1; yylval.ettype = for_end; }
  else if (strcmp ("endfunction", s) == 0)
    { end_found = 1; yylval.ettype = function_end; }
  else if (strcmp ("endif", s) == 0)
    { end_found = 1; yylval.ettype = if_end; }
  else if (strcmp ("endwhile", s) == 0)
    { end_found = 1; yylval.ettype = while_end; }
  else if (strcmp ("for", s) == 0)
    { promptflag--; looping++; return FOR; }
  else if (strcmp ("function", s) == 0)
    {
      if (defining_func)
	{
	  error ("sorry, nested functions are a no-no...");
	  jump_to_top_level ();
	}
      else
	{
	  tmp_local_sym_tab = new symbol_table ();
	  curr_sym_tab = tmp_local_sym_tab;
	  defining_func = 1;
	  promptflag--;
	  beginning_of_function = 1;
	  help_buf[0] = '\0';
	  return FCN;
	}
    }
  else if (strcmp ("global", s) == 0)
    return GLOBAL;
  else if (strcmp ("gplot", s) == 0)
    { plotting = 1; yylval.pttype = two_dee; return PLOT; }
  else if (strcmp ("gsplot", s) == 0)
    { plotting = 1; yylval.pttype = three_dee; return PLOT; }
  else if (strcmp ("if", s) == 0)
    { iffing++; promptflag--; return IF; }
  else if (strcmp ("return", s) == 0)
    return FUNC_RET;
  else if (strcmp ("while", s) == 0)
    { promptflag--; looping++; return WHILE; }

  if (end_found)
    {
      if (!defining_func && !looping)
	promptflag++;
      return END;
    }

  return 0;
}

static symbol_record *
lookup_identifier (char *name)
{
  symbol_record *lsr = curr_sym_tab->lookup (name, 0, 0);
  if (lsr != (symbol_record *) NULL)
    return lsr;

  symbol_record *gsr = global_sym_tab->lookup (name, 0, 0);
  if (curr_sym_tab == top_level_sym_tab && gsr != (symbol_record *) NULL)
    return gsr;

  return curr_sym_tab->lookup (name, 1, 0);
}

static void
grab_help_text (void)
{
  int max_len = HELP_BUF_LENGTH - 1;

  int in_comment = 1;
  int len = 0;
  int c;

  while ((c = yyinput ()) != EOF)
    {
      if (in_comment)
	{
	  help_buf[len++] = c;
	  if (c == '\n')
	    in_comment = 0;
	}
      else
	{
	  switch (c)
	    {
	    case '%':
	    case '#':
	      in_comment = 1;
	    case ' ':
	    case '\t':
	      break;
	    default:
	      goto done;
	    }
	}

      if (len > max_len)
	{
	  message ("grab_help_text",
		   "buffer overflow after caching %d characters",
		   max_len);

	  goto done;
	}
    }

 done:

// Make sure there's an end of line so yylex sees an end to the
// comment immediately.

  yyunput (c, yytext);
  if (c != '\n')
    yyunput ('\n', yytext);

  help_buf[len] =  '\0';
}

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

static int
looks_like_bin_op (int spc_prev, int spc_next)
{
  return ((spc_prev && spc_next) || ! (spc_prev || spc_next));
}

static int
next_char_is_space (void)
{
  int c = yyinput ();
  yyunput (c, yytext);
  return (c == ' ' || c == '\t');
}

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

static int
next_token_is_bin_op (int spc_prev, char *yytext)
{
  int bin_op = 0;
  int spc_next = 0;

  int c0 = yyinput ();
  int c1 = yyinput ();

  switch (c0)
    {
    case '+':  case '-':  case '/':
    case ':':  case '\\': case '^':
      spc_next = (c1 == ' ' || c1 == '\t');
      break;

    case '&':
      if (c1 == '&')
	spc_next = next_char_is_space ();
      else
	spc_next = (c1 == ' ' || c1 == '\t');
      break;

    case '*':
      if (c1 == '*')
	spc_next = next_char_is_space ();
      else
	spc_next = (c1 == ' ' || c1 == '\t');
      break;
	
    case '|':
      if (c1 == '|')
	spc_next = next_char_is_space ();
      else
	spc_next = (c1 == ' ' || c1 == '\t');
      break;

    case '<':
      if (c1 == '=' || c1 == '>')
	spc_next = next_char_is_space ();
      else
	spc_next = (c1 == ' ' || c1 == '\t');
      break;

    case '>':
      if (c1 == '=')
	spc_next = next_char_is_space ();
      else
	spc_next = (c1 == ' ' || c1 == '\t');
      break;

    case '~':  case '!':  case '=':
      if (c1 == '=')
	spc_next = next_char_is_space ();
      else
	goto done;
      break;

    case '.':
      if (c1 == '*')
	{
	  int c2 = yyinput ();
	  if (c2 == '*')
	    spc_next = next_char_is_space ();
	  else
	    spc_next = (c2 == ' ' || c2 == '\t');
	  yyunput (c2, yytext);
	}
      else if (c1 == '/' || c1 == '\\' || c1 == '^')
	spc_next = next_char_is_space ();
      else
	goto done;
      break;

    default:
      goto done;
    }

  bin_op = looks_like_bin_op (spc_prev, spc_next);

 done:
  yyunput (c1, yytext);
  yyunput (c0, yytext);

  return bin_op;
}

char *
strip_trailing_whitespace (char *s)
{
  char *retval = strsave (s);

  char *t = strchr (retval, ' ');
  if (t != (char *) NULL)
    *t = '\0';

  t = strchr (retval, '\t');
  if (t != (char *) NULL)
    *t = '\0';

  return retval;
}

void
check_for_garbage_after_fcn_def (void)
{
// By making a newline be the next character to be read, we will force
// the parser to return after reading the function.  Calling yyunput
// with EOF seems not to work...

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
         near line %d of file `%s.m'", lineno, curr_m_file_name);
	      
	      yyunput ('\n', yytext);
	      return;
	    }
	}
    }
  yyunput ('\n', yytext);
}

/* Maybe someday...

"+="		return ADD_EQ;
"-="		return SUB_EQ;
"*="		return MUL_EQ;
"/="		return DIV_EQ;
"\\="		return LEFTDIV_EQ;
".+="		return ADD_EQ;
".-="		return SUB_EQ;
".*="		return EMUL_EQ;
"./="		return EDIV_EQ;
".\\="		return ELEFTDIV_EQ;

*/
