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

#include "input.h"
#include "token.h"

#include "SLStack.h"

// Stack to hold tokens so that we can delete them when the parser is
// reset and avoid growing forever just because we are stashing some
// information.  This has to appear before lex.h is included, because
// one of the macros defined there uses token_stack.
static SLStack <token*> token_stack;

#include "variables.h"
#include "octave.h"
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

// Forward declarations for functions defined at the bottom of this
// file.

static void do_string_escapes (char *s);
static void fixup_column_count (char *s);
static void do_comma_insert_check (void);
static int is_plot_keyword (char *s);
static int is_keyword (char *s);
static char *plot_style_token (char *s);
static symbol_record *lookup_identifier (char *s);
static void grab_help_text (void);
static int match_any (char c, char *s);
static int next_token_is_bin_op (int spc_prev, char *yytext);
static int next_token_is_postfix_unary_op (int spc_prev, char *yytext);
static char *strip_trailing_whitespace (char *s);
static int handle_identifier (char *s, int next_tok_is_eq);

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
			  current_input_column = 1;
			  quote_is_transpose = 0;
			  cant_be_identifier = 0;
			  convert_spaces_to_comma = 1;
			  return '\n';
			}

<COMMENT><<EOF>>	{ TOK_RETURN (END_OF_INPUT); }

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
			  current_input_column = 1;
			  quote_is_transpose = 0;
			  cant_be_identifier = 0;
			  convert_spaces_to_comma = 1;
			  return '\n';
			}

<TEXT_FCN>[\;\,]	{
			  if (doing_set)
			    {
			      yylval.tok_val = new token (yytext);
			      token_stack.push (yylval.tok_val);
			      TOK_RETURN (TEXT);
			    }
			  else
			    {
			      BEGIN 0;
			      TOK_RETURN (',');
			    }
		        }

<HELP_FCN>[^ \t\n]*{S}*	    |
<TEXT_FCN>[^ \t\n\;\,]*{S}* {
			      static char *tok = (char *) NULL;
			      delete [] tok;
			      tok = strip_trailing_whitespace (yytext);
			      yylval.tok_val = new token (tok);
			      token_stack.push (yylval.tok_val);
			      TOK_RETURN (TEXT);
			    }

<TEXT_FCN>\'{QSTR}*[\n\'] {
			  if (yytext[yyleng-1] == '\n')
			    {
			      error ("unterminated string constant");
			      current_input_column = 1;
			      jump_to_top_level ();
			    }
			  else
			    {
			      static char *tok = (char *) NULL;
			      delete [] tok;
			      int off1 = doing_set ? 0 : 1;
			      int off2 = doing_set ? 0 : 2;
			      tok = strsave (&yytext[off1]);
			      tok[yyleng-off2] = '\0';
			      do_string_escapes (tok);
			      yylval.tok_val = new token (tok);
			      token_stack.push (yylval.tok_val);
			      current_input_column += yyleng;
			    }
			  return TEXT;
			}

<TEXT_FCN>\"{DQSTR}*[\n\"] {
			  if (yytext[yyleng-1] == '\n')
			    {
			      error ("unterminated string constant");
			      current_input_column = 1;
			      jump_to_top_level ();
			    }
			  else
			    {
			      static char *tok = (char *) NULL;
			      delete [] tok;
			      int off1 = doing_set ? 0 : 1;
			      int off2 = doing_set ? 0 : 2;
			      tok = strsave (&yytext[off1]);
			      tok[yyleng-off2] = '\0';
			      do_string_escapes (tok);
			      yylval.tok_val = new token (tok);
			      token_stack.push (yylval.tok_val);
			      current_input_column += yyleng;
			    }
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
			      current_input_column = 1;
			      jump_to_top_level ();
			    }
			  else
			    {
			      static char *tok = (char *) NULL;
			      delete [] tok;
			      tok = strsave (yytext);
			      tok[yyleng-1] = '\0';
			      do_string_escapes (tok);
			      yylval.tok_val = new token (tok);
			      token_stack.push (yylval.tok_val);
			      quote_is_transpose = 1;
			      cant_be_identifier = 1;
			      convert_spaces_to_comma = 1;
			      current_input_column += yyleng;
			    }
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
			      current_input_column = 1;
			      jump_to_top_level ();
			    }
			  else
			    {
			      static char *tok = (char *) NULL;
			      delete [] tok;
			      tok = strsave (yytext);
			      tok[yyleng-1] = '\0';
			      do_string_escapes (tok);
			      yylval.tok_val = new token (tok);
			      token_stack.push (yylval.tok_val);
			      quote_is_transpose = 1;
			      cant_be_identifier = 1;
			      convert_spaces_to_comma = 1;
			      current_input_column += yyleng;
			    }
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
			      if (! defining_func)
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
			      if (! defining_func)
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
			      if (! defining_func)
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

<MATRIX>{S}*\,{S}*	{ TOK_RETURN (','); }

<MATRIX>{S}+		{
			  int bin_op = next_token_is_bin_op (1, yytext);
			  int postfix_un_op
			    = next_token_is_postfix_unary_op (1, yytext);

 			  if (! (postfix_un_op || bin_op)
			      && in_brace_or_paren.top ()
			      && convert_spaces_to_comma)
			    TOK_RETURN (',');
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
			      TOK_RETURN (CLOSE_BRACE);
			    }
			  else
			    TOK_RETURN (']');
			}

{D}+{EXPON}?{Im}	|
{D}+\.{D}*{EXPON}?{Im}	|
\.{D}+{EXPON}?{Im}	{
			  double value;
			  int nread = sscanf (yytext, "%lf", &value);
			  assert (nread == 1);
			  quote_is_transpose = 1;
			  cant_be_identifier = 1;
			  convert_spaces_to_comma = 1;
			  if (plotting && ! in_plot_range)
			    past_plot_range = 1;
			  yylval.tok_val = new token (value,
						      input_line_number,
						      current_input_column);
			  token_stack.push (yylval.tok_val);
			  current_input_column += yyleng;
			  do_comma_insert_check ();
			  return IMAG_NUM;
			}

{D}+{EXPON}?		|
{D}+\.{D}*{EXPON}?	|
\.{D}+{EXPON}?		|
			{
			  double value;
			  int nread = sscanf (yytext, "%lf", &value);
			  assert (nread == 1);
			  quote_is_transpose = 1;
			  cant_be_identifier = 1;
			  convert_spaces_to_comma = 1;
			  if (plotting && ! in_plot_range)
			    past_plot_range = 1;
			  yylval.tok_val = new token (value,
						      input_line_number,
						      current_input_column);
			  token_stack.push (yylval.tok_val);
			  current_input_column += yyleng;
			  do_comma_insert_check ();
			  return NUM;
			}

\[{S}*		{
		  in_brace_or_paren.push (1);
		  if (plotting && ! past_plot_range)
		    {
		      in_plot_range = 1;
		      TOK_RETURN (OPEN_BRACE);
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
		      TOK_RETURN ('[');
		    }
		}

{S}*		{ current_input_column += yyleng; }

{EL}{S}*\n	{

// Line continuation.

		  promptflag--;
		  current_input_column = 1;
		}

<<EOF>>		TOK_RETURN (END_OF_INPUT);

{IDENT}{S}*	{

// Truncate the token at the first space or tab but don't write
// directly on yytext.

		  static char *tok = (char *) NULL;
		  delete [] tok;
		  tok = strip_trailing_whitespace (yytext);
		  return handle_identifier (tok, 0);
		}

{IDENT}/{S}*=	{ return handle_identifier (yytext, 1); }

"\n"		{
		  quote_is_transpose = 0;
		  cant_be_identifier = 0;
		  current_input_column = 1;
		  convert_spaces_to_comma = 1;
		  return '\n';
		}

"'"		{
		  current_input_column++;
		  convert_spaces_to_comma = 1;

		  if (quote_is_transpose)
		    {
		      do_comma_insert_check ();
		      return QUOTE;
		    }
		  else
		    BEGIN STRING;
		}

":"		{
		  if (plotting && (in_plot_range || in_plot_using))
		    BIN_OP_RETURN (COLON, 1);
		  else
		    BIN_OP_RETURN (':', 0);
		}

\"		{ BEGIN DQSTRING; }
".**"		{ BIN_OP_RETURN (EPOW, 0); }
".*"		{ BIN_OP_RETURN (EMUL, 0); }
"./"		{ BIN_OP_RETURN (EDIV, 0); }
".\\"		{ BIN_OP_RETURN (ELEFTDIV, 0); }
".^"		{ BIN_OP_RETURN (EPOW, 0); }
".'"		{ do_comma_insert_check (); BIN_OP_RETURN (TRANSPOSE, 1); }
"++"		{ do_comma_insert_check (); BIN_OP_RETURN (PLUS_PLUS, 1); }
"--"		{ do_comma_insert_check (); BIN_OP_RETURN (MINUS_MINUS, 1); }
"<="		{ BIN_OP_RETURN (EXPR_LE, 0); }
"=="		{ BIN_OP_RETURN (EXPR_EQ, 0); }
"~="		{ BIN_OP_RETURN (EXPR_NE, 0); }
"!="		{ BIN_OP_RETURN (EXPR_NE, 0); }
"<>"		{ BIN_OP_RETURN (EXPR_NE, 0); }
">="		{ BIN_OP_RETURN (EXPR_GE, 0); }
"||"		{ BIN_OP_RETURN (EXPR_OR, 0); }
"&&"		{ BIN_OP_RETURN (EXPR_AND, 0); }
"|"		{ BIN_OP_RETURN (EXPR_OR, 0); }
"&"		{ BIN_OP_RETURN (EXPR_AND, 0); }
"!"		{
		  if (plotting && ! in_plot_range)
		    past_plot_range = 1;
		  BIN_OP_RETURN (EXPR_NOT, 1);
		}
"~"		{
		  if (plotting && ! in_plot_range)
		    past_plot_range = 1;
		  BIN_OP_RETURN (EXPR_NOT, 0);
		}
"<"		{ BIN_OP_RETURN (EXPR_LT, 0); }
">"		{ BIN_OP_RETURN (EXPR_GT, 0); }
"+"		{ 
		  if (plotting && ! in_plot_range)
		    past_plot_range = 1;
		  BIN_OP_RETURN ('+', 0);
		}
"-"		{
		  if (plotting && ! in_plot_range)
		    past_plot_range = 1;
		  BIN_OP_RETURN ('-', 0);
		}
"**"		{ BIN_OP_RETURN (POW, 0); }
"*"		{ BIN_OP_RETURN ('*', 0); }
"/"		{ BIN_OP_RETURN ('/', 0); }
"\\"		{ BIN_OP_RETURN (LEFTDIV, 0); }
";"		{ BIN_OP_RETURN (';', 1); }
","		{ BIN_OP_RETURN (',', 1); }
"^"		{ BIN_OP_RETURN (POW, 0); }
"="		{ BIN_OP_RETURN ('=', 1); }
"("		{
		  if (plotting && ! in_plot_range)
		    past_plot_range = 1;
		  in_brace_or_paren.push (0);
		  TOK_RETURN ('(');
		}
")"		{
		  if (! in_brace_or_paren.empty ())
		    in_brace_or_paren.pop ();
		  do_comma_insert_check ();
		  current_input_column++;
		  quote_is_transpose = 1;
		  return ')';
		}

.		{

// We return everything else as single character tokens, which should
// eventually result in a parse error.

		  TOK_RETURN (yytext[0]);
		}

%%

/*
 * GAG.
 *
 * If we're reading a matrix and the next character is '[', make sure
 * that we insert a comma ahead of it.
 */
void
do_comma_insert_check (void)
{
  int c = yyinput ();
  yyunput (c, yytext);
  do_comma_insert = (braceflag && c == '[');
}

/*
 * Fix things up for errors or interrupts.  This could use a few
 * comments now, eh?
 */
void
reset_parser (void)
{
  BEGIN 0;
  error_state = 0;
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
  current_input_column = 1;
// Might have been reset by defining a function.
  input_line_number = current_command_number - 1;
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
  while (! token_stack.empty ())
    delete token_stack.pop ();
  yyrestart (stdin);
}

/*
 * Replace backslash escapes in a string with the real values.
 */
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

/*
 * If we read some newlines, we need figure out what column we're
 * really looking at.
 */
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

/*
 * Check to see if a character string matches any of the possible line
 * styles for plots.
 */
static char *
plot_style_token (char *s)
{
  static char *plot_styles[] = 
    {
      "dots",
      "errorbars",
      "impulses",
      "lines",
      "linespoints",
      "points",
      (char *) NULL,
    };

  char **tmp = plot_styles;
  while (*tmp != (char *) NULL)
    {
      if (almost_match (*tmp, s))
	return *tmp;

      tmp++;
    }

  return (char *) NULL;
}

/*
 * Check to see if a character string matches any one of the plot
 * option keywords. 
 */
static int
is_plot_keyword (char *s)
{
  if (almost_match ("title", s))
    {
      return TITLE;
    }
  else if (almost_match ("using", s))
    {
      in_plot_using = 1;
      past_plot_range = 1;
      return USING;
    }
  else if (almost_match ("with", s))
    {
      in_plot_style = 1;
      past_plot_range = 1;
      return WITH;
    }
  else
    {
      return 0;
    }
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
	  yylval.tok_val = new token (sty);
	  token_stack.push (yylval.tok_val);
	  return STYLE;
	}
    }

  int l = input_line_number;
  int c = current_input_column;

  int end_found = 0;
  if (strcmp ("break", s) == 0)
    {
      return BREAK;
    }
  else if (strcmp ("continue", s) == 0)
    {
      return CONTINUE;
    }
  else if (strcmp ("else", s) == 0)
    {
      return ELSE;
    }
  else if (strcmp ("elseif", s) == 0)
    {
      return ELSEIF;
    }
  else if (strcmp ("end", s) == 0)
    {
      end_found = 1;
      yylval.tok_val = new token (token::simple_end, l, c);
      token_stack.push (yylval.tok_val);
    }
  else if (strcmp ("endfor", s) == 0)
    {
      end_found = 1;
      yylval.tok_val = new token (token::for_end, l, c);
      token_stack.push (yylval.tok_val);
    }
  else if (strcmp ("endfunction", s) == 0)
    {
      end_found = 1;
      yylval.tok_val = new token (token::function_end, l, c);
      token_stack.push (yylval.tok_val);
    }
  else if (strcmp ("endif", s) == 0)
    {
      end_found = 1;
      yylval.tok_val = new token (token::if_end, l, c);
      token_stack.push (yylval.tok_val);
    }
  else if (strcmp ("endwhile", s) == 0)
    {
      end_found = 1;
      yylval.tok_val = new token (token::while_end, l, c);
      token_stack.push (yylval.tok_val);
    }
  else if (strcmp ("for", s) == 0)
    {
      promptflag--;
      looping++;
      return FOR;
    }
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
	  input_line_number = 1;
	  return FCN;
	}
    }
  else if (strcmp ("global", s) == 0)
    {
      return GLOBAL;
    }
  else if (strcmp ("gplot", s) == 0)
    {
      plotting = 1;
      yylval.tok_val = new token (token::two_dee, l, c);
      return PLOT;
    }
  else if (strcmp ("gsplot", s) == 0)
    {
      plotting = 1;
      yylval.tok_val = new token (token::three_dee, l, c);
      token_stack.push (yylval.tok_val);
      return PLOT;
    }
  else if (strcmp ("if", s) == 0)
    {
      iffing++;
      promptflag--;
      return IF;
    }
  else if (strcmp ("return", s) == 0)
    {
      return FUNC_RET;
    }
  else if (strcmp ("while", s) == 0)
    {
      promptflag--;
      looping++;
      return WHILE;
    }

  if (end_found)
    {
      if (! defining_func && ! looping)
	promptflag++;
      return END;
    }

  return 0;
}

/*
 * Try to find an identifier in one symbol table or another.
 */
static symbol_record *
lookup_identifier (char *name)
{
  if (curr_sym_tab == top_level_sym_tab)
    {
      symbol_record *lsr = curr_sym_tab->lookup (name, 0, 0);
      if (lsr != (symbol_record *) NULL)
	return lsr;

      symbol_record *gsr = global_sym_tab->lookup (name, 0, 0);
      if (gsr != (symbol_record *) NULL && ! (looping || iffing))
	return gsr;
    }

  return curr_sym_tab->lookup (name, 1, 0);
}

/*
 * Grab the help text from an M-file.
 */
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

/*
 * Return 1 if the given character matches any character in the given
 * string.
 */
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

/*
 * Given information about the spacing surrounding an operator,
 * return 1 if it looks like it should be treated as a binary
 * operator.  For example,
 *
 *   [ 1 + 2 ]  or  [ 1+2 ]  ==> binary
 *
 * The case of [ 1+ 2 ] should also be treated as a binary operator,
 * but it is handled by the caller.
 */
static int
looks_like_bin_op (int spc_prev, int spc_next)
{
  return ((spc_prev && spc_next) || ! (spc_prev || spc_next));
}

/*
 * Duh.
 */
static int
next_char_is_space (void)
{
  int c = yyinput ();
  yyunput (c, yytext);
  return (c == ' ' || c == '\t');
}

/*
 * Try to determine if the next token should be treated as a postfix
 * unary operator.  This is ugly, but it seems to do the right thing.
 */
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

/*
 * Try to determine if the next token should be treated as a binary
 * operator.  This is even uglier, but it also seems to do the right
 * thing.
 */
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

/*
 * Used to delete trailing white space from tokens.
 */
static char *
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

/*
 * Figure out exactly what kind of token to return when we have seen
 * an identifier.  Handles keywords.
 */
static int
handle_identifier (char *tok, int next_tok_is_eq)
{
// If we have a regular keyword, or a plot STYLE, return it.  STYLE is
// special only because it can't be followed by an identifier.

  int kw_token = is_keyword (tok);
  if (kw_token)
    {
      if (kw_token == STYLE)
	{
	  current_input_column += yyleng;
	  quote_is_transpose = 0;
	  cant_be_identifier = 1;
	  convert_spaces_to_comma = 1;
	  return kw_token;
	}
      else
	TOK_RETURN (kw_token);
    }

// See if we have a plot keyword (title, using, or with).

  int plot_option_kw = is_plot_keyword (tok);
  if (plotting && cant_be_identifier && plot_option_kw)
    TOK_RETURN (plot_option_kw);

// Yes, we really do need both of these plot_range variables.  One
// is used to mark when we are past all possiblity of a plot range,
// the other is used to mark when we are actually between the square
// brackets that surround the range.

  if (plotting && ! in_plot_range)
    past_plot_range = 1;

// It is always an error for an identifier to be followed directly by
// another identifier.

  cant_be_identifier = 1;

// If we are looking at a text style function, set up to gobble its
// arguments.  These are also reserved words, but only because it
// would be very difficult to do anything intelligent with them if
// they were not reserved.

  if (is_text_function_name (tok))
    {
      BEGIN TEXT_FCN;

      if (strcmp (tok, "clear") == 0)
	{
	  symbol_record *sr = global_sym_tab->lookup ("clear", 1, 0);

	  assert (sr != (symbol_record *) NULL);

	  yylval.tok_val = new token (sr, input_line_number,
				      current_input_column);

	  token_stack.push (yylval.tok_val);

	  return CLEAR;
	}
      else if (strcmp (tok, "help") == 0)
	BEGIN HELP_FCN;
      else if (strcmp (tok, "set") == 0)
	doing_set = 1;
    }

// Make sure we put the return values of a function in the symbol
// table that is local to the function.

  if (next_tok_is_eq && defining_func && maybe_screwed)
    curr_sym_tab = tmp_local_sym_tab;

// Find the token in the symbol table.

  yylval.tok_val = new token (lookup_identifier (tok),
			      input_line_number,
			      current_input_column);

  token_stack.push (yylval.tok_val);

// After seeing an identifer, it is ok to convert spaces to a comma
// (if needed).

  convert_spaces_to_comma = 1;
  current_input_column += yyleng;

// If we are defining a function and we have not seen the parameter
// list yet and the next token is `=', return a token that represents
// the only return value for the function.  For example,
//
//   function SCREW = f (args);
//
// The variable maybe_screwed is reset in parse.y.

  if (next_tok_is_eq)
    {
      if (defining_func && maybe_screwed)
	return SCREW;
      else
	return NAME;
    }

// At this point, we are only dealing with identifiers that are not
// followed by `=' (if the next token is `=', there is no need to
// check to see if we should insert a comma (invalid syntax), or allow
// a following `'' to be treated as a transpose (the next token is
// `=', so it can't be `''.

  quote_is_transpose = 1;
  do_comma_insert_check ();

// Check to see if we should insert a comma.

  if (! in_brace_or_paren.empty () && in_brace_or_paren.top ()) 
    {
      int c0 = yytext[yyleng-1];
      int spc_prev = (c0 == ' ' || c0 == '\t');
      int bin_op = next_token_is_bin_op (spc_prev, yytext);

      int postfix_un_op = next_token_is_postfix_unary_op (spc_prev,
							  yytext);

      int c1 = yyinput ();
      unput (c1);
      int other_op = match_any (c1, ",;\n](");

      if (! (postfix_un_op || bin_op || other_op))
	unput (',');
    }

  return NAME;
}

/*
 * Print a warning if an M-file that defines a function has anything
 * other than comments and whitespace following the END token that
 * matches the FUNCTION statement.
 */
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
