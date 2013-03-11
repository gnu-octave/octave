/*

Copyright (C) 1993-2012 John W. Eaton

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

/*
We are using the pure parser interface and the reentrant lexer
interface but the Octave parser and lexer are NOT properly
reentrant because both still use many global variables.  It should be
safe to create a parser object and call it while anotehr parser
object is active (to parse a callback function while the main
interactive parser is waiting for input, for example) if you take
care to properly save and restore (typically with an unwind_protect
object) relevant global values before and after the nested call.
*/

%option prefix = "octave_"
%option noyywrap
%option reentrant
%option bison-bridge

%top {
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

}

%s COMMAND_START
%s MATRIX_START

%x INPUT_FILE_START

%x BLOCK_COMMENT_START
%x LINE_COMMENT_START

%{

#include <cctype>
#include <cstring>

#include <iostream>
#include <set>
#include <sstream>
#include <string>
#include <stack>

#include <sys/types.h>
#include <unistd.h>

#include "cmd-edit.h"
#include "quit.h"
#include "lo-mappers.h"

// These would be alphabetical, but oct-parse.h must be included before
// oct-gperf.h and oct-parse.h must be included after token.h and the tree
// class declarations.  We can't include oct-parse.h in oct-gperf.h
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
#include "parse-private.h"
#include "pt-all.h"
#include "symtab.h"
#include "token.h"
#include "toplev.h"
#include "utils.h"
#include "variables.h"
#include <oct-parse.h>
#include <oct-gperf.h>

#if defined (GNULIB_NAMESPACE)
// Calls to the following functions appear in the generated output from
// flex without the namespace tag.  Redefine them so we will use them
// via the gnulib namespace.
#define fprintf GNULIB_NAMESPACE::fprintf
#define fwrite GNULIB_NAMESPACE::fwrite
#define isatty GNULIB_NAMESPACE::isatty
#define malloc GNULIB_NAMESPACE::malloc
#define realloc GNULIB_NAMESPACE::realloc
#endif

#if ! (defined (FLEX_SCANNER) \
       && defined (YY_FLEX_MAJOR_VERSION) && YY_FLEX_MAJOR_VERSION >= 2 \
       && defined (YY_FLEX_MINOR_VERSION) && YY_FLEX_MINOR_VERSION >= 5)
#error lex.l requires flex version 2.5.4 or later
#endif

#define YY_EXTRA_TYPE octave_lexer *
#define curr_lexer yyextra

// Arrange to get input via readline.

#ifdef YY_INPUT
#undef YY_INPUT
#endif
#define YY_INPUT(buf, result, max_size) \
  result = curr_lexer->read (buf, max_size)

// Try to avoid crashing out completely on fatal scanner errors.

#ifdef YY_FATAL_ERROR
#undef YY_FATAL_ERROR
#endif
#define YY_FATAL_ERROR(msg) \
  (yyget_extra (yyscanner))->fatal_error (msg)

#define CMD_OR_OP(PATTERN, TOK, COMPAT) \
 \
  do \
    { \
      curr_lexer->lexer_debug (PATTERN); \
 \
      if (curr_lexer->looks_like_command_arg ()) \
        { \
          yyless (0); \
          curr_lexer->push_start_state (COMMAND_START); \
        } \
      else \
        { \
          return curr_lexer->handle_op_internal (TOK, false, COMPAT); \
        } \
    } \
  while (0)

#define CMD_OR_UNARY_OP(PATTERN, TOK, COMPAT) \
 \
  do \
    { \
      curr_lexer->lexer_debug (PATTERN); \
 \
      if (curr_lexer->looks_like_command_arg ()) \
        { \
          yyless (0); \
          curr_lexer->push_start_state (COMMAND_START); \
        } \
      else \
        { \
          int tok \
            = (COMPAT \
               ? curr_lexer->handle_unary_op (TOK) \
               : curr_lexer->handle_incompatible_unary_op (TOK)); \
 \
          if (tok < 0) \
            { \
              yyless (0); \
              curr_lexer->xunput (','); \
              /* Adjust for comma that was not really in the input stream. */ \
              curr_lexer->current_input_column--; \
            } \
          else \
            { \
              return tok; \
            } \
        } \
    } \
  while (0)

static bool Vdisplay_tokens = false;

static unsigned int Vtoken_count = 0;

// Internal variable for lexer debugging state.
static bool lexer_debug_flag = false;

// Forward declarations for functions defined at the bottom of this
// file that are needed inside the lexer actions.

static std::string strip_trailing_whitespace (char *s);

%}

D       [0-9]
S       [ \t]
NL      ((\n)|(\r)|(\r\n))
CONT    ((\.\.\.)|(\\))
Im      [iIjJ]
CCHAR   [#%]
IDENT   ([_$a-zA-Z][_$a-zA-Z0-9]*)
EXPON   ([DdEe][+-]?{D}+)
NUMBER  (({D}+\.?{D}*{EXPON}?)|(\.{D}+{EXPON}?)|(0[xX][0-9a-fA-F]+))

ANY_INCLUDING_NL (.|{NL})

%%

%{
// Make script and function files start with a bogus token. This makes
// the parser go down a special path.
%}

<INPUT_FILE_START>{ANY_INCLUDING_NL} {
    curr_lexer->lexer_debug ("<INPUT_FILE_START>{ANY_INCLUDING_NL}");

    curr_lexer->xunput (yytext[0]);

    // May be reset later if we see "function" or "classdef" appears
    // as the first token.
    curr_lexer->reading_script_file = true;

    curr_lexer->pop_start_state ();

    return curr_lexer->show_token (INPUT_FILE);
  }

%{
// Help and other command-style functions.
%}

<COMMAND_START>{NL} {
    curr_lexer->lexer_debug ("<COMMAND_START>{NL}");

    curr_lexer->input_line_number++;
    curr_lexer->current_input_column = 1;

    curr_lexer->looking_for_object_index = false;
    curr_lexer->at_beginning_of_statement = true;

    curr_lexer->pop_start_state ();

    return curr_lexer->count_token ('\n');
  }

<COMMAND_START>[\;\,] {
    curr_lexer->lexer_debug ("<COMMAND_START>[\\;\\,]");

    curr_lexer->looking_for_object_index = false;
    curr_lexer->at_beginning_of_statement = true;

    curr_lexer->pop_start_state ();

    if (strcmp (yytext, ",") == 0)
      return curr_lexer->handle_token (',');
    else
      return curr_lexer->handle_token (';');
  }

<COMMAND_START>[\"\'] {
    curr_lexer->lexer_debug ("<COMMAND_START>[\\\"\\']");

    curr_lexer->at_beginning_of_statement = false;

    curr_lexer->current_input_column++;
    int tok = curr_lexer->handle_string (yytext[0]);

    return curr_lexer->count_token_internal (tok);
  }

<COMMAND_START>[^#% \t\r\n\;\,\"\'][^ \t\r\n\;\,]*{S}* {
    curr_lexer->lexer_debug ("<COMMAND_START>[^#% \\t\\r\\n\\;\\,\\\"\\'][^ \\t\\r\\n\\;\\,]*{S}*");

    std::string tok = strip_trailing_whitespace (yytext);

    curr_lexer->looking_for_object_index = false;
    curr_lexer->at_beginning_of_statement = false;

    return curr_lexer->handle_token (tok, SQ_STRING);
  }

<MATRIX_START>{S}* {
    curr_lexer->lexer_debug ("<MATRIX_START>{S}*");

    curr_lexer->mark_previous_token_trailing_space ();
  }

<MATRIX_START>{NL} {
    curr_lexer->lexer_debug ("<MATRIX_START>{NL}");

    curr_lexer->input_line_number++;
    curr_lexer->current_input_column = 1;

    if (curr_lexer->nesting_level.is_paren ())
      curr_lexer->gripe_matlab_incompatible ("bare newline inside parentheses");
    else
      {
        int tok = curr_lexer->previous_token_value ();

        if (! (tok == ';' || tok == '[' || tok == '{'))
          {
            curr_lexer->xunput (';');
            // Adjust for semicolon that was not really in the input stream.
            curr_lexer->current_input_column--;
          }
      }
  }

%{
// For this and the next two rules, we're looking at ']', and we
// need to know if the next token is '=' or '=='.
//
// It would have been so much easier if the delimiters were simply
// different for the expression on the left hand side of the equals
// operator.
//
// It's also a pain in the ass to decide whether to insert a comma
// after seeing a ']' character...

// FIXME -- we need to handle block comments here.
%}

<MATRIX_START>\] {
    curr_lexer->lexer_debug ("<MATRIX_START>\\]");

    curr_lexer->looking_at_object_index.pop_front ();

    curr_lexer->looking_for_object_index = true;
    curr_lexer->at_beginning_of_statement = false;

    int tok_to_return = curr_lexer->handle_close_bracket (']');

    return curr_lexer->count_token (']');
  }

%{
// FIXME -- we need to handle block comments here.
%}

<MATRIX_START>\} {
    curr_lexer->lexer_debug ("<MATRIX_START>\\}*");

    curr_lexer->looking_at_object_index.pop_front ();

    curr_lexer->looking_for_object_index = true;
    curr_lexer->at_beginning_of_statement = false;

    int tok_to_return = curr_lexer->handle_close_bracket ('}');

    return curr_lexer->count_token ('}');
  }

\[ {
    curr_lexer->lexer_debug ("\\[");

        bool unput_comma = false;

    if (curr_lexer->whitespace_is_significant ()
        && curr_lexer->space_follows_previous_token ())
      {
        int tok = curr_lexer->previous_token_value ();

        if (! (tok == '[' || tok == '{'
               || curr_lexer->previous_token_is_binop ()))
          unput_comma = true;
      }

    if (unput_comma)
      {
        yyless (0);
        curr_lexer->xunput (',');
        // Adjust for comma that was not really in the input stream.
        curr_lexer->current_input_column--;
      }
    else
      {
        curr_lexer->nesting_level.bracket ();

        curr_lexer->looking_at_object_index.push_front (false);

        curr_lexer->current_input_column += yyleng;
        curr_lexer->looking_for_object_index = false;
        curr_lexer->at_beginning_of_statement = false;

        if (curr_lexer->defining_func
            && ! curr_lexer->parsed_function_name.top ())
          curr_lexer->looking_at_return_list = true;
        else
          curr_lexer->looking_at_matrix_or_assign_lhs = true;

        curr_lexer->decrement_promptflag ();

        curr_lexer->bracketflag++;

        curr_lexer->push_start_state (MATRIX_START);

        return curr_lexer->count_token ('[');
      }
  }

\] {
    curr_lexer->lexer_debug ("\\]");

    curr_lexer->nesting_level.remove ();

    curr_lexer->looking_at_object_index.pop_front ();

    curr_lexer->looking_for_object_index = true;
    curr_lexer->at_beginning_of_statement = false;

    return curr_lexer->handle_token (']');
  }

%{
// Gobble comments.
%}

%{
// Start of a block comment.  If the comment marker appears immediately
// after a block of full-line comments, finish the full line comment
// block.
%}

^{S}*{CCHAR}\{{S}*{NL} {
    curr_lexer->lexer_debug ("^{S}*{CCHAR}\{{S}*{NL}");

    yyless (0);

    if (curr_lexer->start_state () == LINE_COMMENT_START)
      {
        if (! curr_lexer->comment_text.empty ())
          curr_lexer->finish_comment (octave_comment_elt::full_line);

        curr_lexer->pop_start_state ();
      }

    curr_lexer->push_start_state (BLOCK_COMMENT_START);

  }

<BLOCK_COMMENT_START>^{S}*{CCHAR}\{{S}*{NL} {
    curr_lexer->lexer_debug ("<BLOCK_COMMENT_START>^{S}*{CCHAR}\{{S}*{NL}");

    curr_lexer->input_line_number++;
    curr_lexer->current_input_column = 1;

    if (curr_lexer->block_comment_nesting_level)
      curr_lexer->comment_text = "\n";

    curr_lexer->block_comment_nesting_level++;
  }

%{
// End of a block comment.  If this block comment is nested inside
// another, wait for the outermost block comment block to be closed
// before storing the comment.
%}

<BLOCK_COMMENT_START>^{S}*{CCHAR}\}{S}*{NL} {
    curr_lexer->lexer_debug ("<BLOCK_COMMENT_START>^{S}*{CCHAR}\\}{S}*{NL}");

    curr_lexer->input_line_number++;
    curr_lexer->current_input_column = 1;

    if (curr_lexer->block_comment_nesting_level > 1)
      curr_lexer->comment_text = "\n";
    else
      curr_lexer->finish_comment (octave_comment_elt::block);

    curr_lexer->block_comment_nesting_level--;
    curr_lexer->pop_start_state ();
  }

%{
// Body of a block comment.
%}

<BLOCK_COMMENT_START>.*{NL} {
    curr_lexer->lexer_debug ("<BLOCK_COMMENT_START>.*{NL}");

    curr_lexer->input_line_number++;
    curr_lexer->current_input_column = 1;
    curr_lexer->comment_text += yytext;
  }

%{
// Full-line or end-of-line comment.
%}

{S}*{CCHAR}.*{NL} {
    curr_lexer->lexer_debug ("{S}*{CCHAR}.*{NL}");

    curr_lexer->push_start_state (LINE_COMMENT_START);
    yyless (0);
  }

<LINE_COMMENT_START>{S}*{CCHAR}.*{NL} {
    curr_lexer->lexer_debug ("<LINE_COMMENT_START>{S}*{CCHAR}.*{NL}");

    bool full_line_comment = curr_lexer->current_input_column == 1;
    curr_lexer->input_line_number++;
    curr_lexer->current_input_column = 1;

    bool have_space = false;
    size_t len = yyleng;
    size_t i = 0;
    while (i < len)
      {
        char c = yytext[i];
        if (c == ' ' || c == '\t')
          {
            have_space = true;
            i++;
          }
        else
          break;
      }

    while (i < len)
      {
        char c = yytext[i];
        if (c == '#' || c == '%')
          i++;
        else
          break;
      }
      
    curr_lexer->comment_text += &yytext[i];

    if (! full_line_comment)
      {
        if (have_space)
          curr_lexer->mark_previous_token_trailing_space ();

        curr_lexer->finish_comment (octave_comment_elt::end_of_line);

        curr_lexer->pop_start_state ();
      }
  }

%{
// End of a block of full-line comments.
%}

<LINE_COMMENT_START>{ANY_INCLUDING_NL} {
    curr_lexer->lexer_debug ("<LINE_COMMENT_START>{ANY_INCLUDING_NL}");

    curr_lexer->xunput (yytext[0]);

    curr_lexer->finish_comment (octave_comment_elt::full_line);  

    curr_lexer->pop_start_state ();
  }

%{
// Imaginary numbers.
%}

{NUMBER}{Im} {
    curr_lexer->lexer_debug ("{NUMBER}{Im}");

    int tok = curr_lexer->previous_token_value ();

    if (curr_lexer->whitespace_is_significant ()
        && curr_lexer->space_follows_previous_token ()
        && ! (tok == '[' || tok == '{'
              || curr_lexer->previous_token_is_binop ()))
      {
        yyless (0);
        unput (',');
      }
    else
      {
        curr_lexer->handle_number ();
        return curr_lexer->count_token_internal (IMAG_NUM);
      }
  }

%{
// Real numbers.  Don't grab the '.' part of a dot operator as part of
// the constant.
%}

{D}+/\.[\*/\\^\'] |
{NUMBER} {
    curr_lexer->lexer_debug ("{D}+/\\.[\\*/\\^\\']|{NUMBER}");

     int tok = curr_lexer->previous_token_value ();

     if (curr_lexer->whitespace_is_significant ()
         && curr_lexer->space_follows_previous_token ()
         && ! (tok == '[' || tok == '{'
               || curr_lexer->previous_token_is_binop ()))
       {
         yyless (0);
         unput (',');
       }
     else
       {
         curr_lexer->handle_number ();
         return curr_lexer->count_token_internal (NUM);
       }
  }

%{
// Eat whitespace.  Whitespace inside matrix constants is handled by
// the <MATRIX_START> start state code above.
%}

{S}* {
    curr_lexer->current_input_column += yyleng;

    curr_lexer->mark_previous_token_trailing_space ();
  }

%{
// Continuation lines.  Allow comments after continuations.
%}

{CONT}{S}*{NL} |
{CONT}{S}*{CCHAR}.*{NL} {
    curr_lexer->lexer_debug ("{CONT}{S}*{NL}|{CONT}{S}*{CCHAR}.*{NL}");

    curr_lexer->handle_continuation ();
  }

%{
// End of file.
%}

<<EOF>> {
   return curr_lexer->handle_end_of_input ();
  }

%{
// Identifiers.
%}

{IDENT} {
    curr_lexer->lexer_debug ("{IDENT}");

    int tok = curr_lexer->previous_token_value ();

    if (curr_lexer->whitespace_is_significant ()
        && curr_lexer->space_follows_previous_token ()
        && ! (tok == '[' || tok == '{'
              || curr_lexer->previous_token_is_binop ()))
      {
        yyless (0);
        unput (',');
      }
    else
      {
        if (! curr_lexer->looking_at_decl_list
            && curr_lexer->previous_token_may_be_command ())
          {
            yyless (0);
            curr_lexer->push_start_state (COMMAND_START);
          }
        else
          {
            int id_tok = curr_lexer->handle_identifier ();

            if (id_tok >= 0)
              return curr_lexer->count_token_internal (id_tok);
          }
      }
  }

%{
// Superclass method identifiers.
%}

{IDENT}@{IDENT} |
{IDENT}@{IDENT}.{IDENT} {
    curr_lexer->lexer_debug ("{IDENT}@{IDENT}|{IDENT}@{IDENT}.{IDENT}");

    int id_tok = curr_lexer->handle_superclass_identifier ();

    if (id_tok >= 0)
      {
        curr_lexer->looking_for_object_index = true;

        return curr_lexer->count_token_internal (SUPERCLASSREF);
      }
  }

%{
// Metaclass query
%}

\?{IDENT} |
\?{IDENT}\.{IDENT} {
    curr_lexer->lexer_debug ("\\?{IDENT}|\\?{IDENT}\\.{IDENT}");

    int id_tok = curr_lexer->handle_meta_identifier ();

    if (id_tok >= 0)
      {
        curr_lexer->looking_for_object_index = true;

        return curr_lexer->count_token_internal (METAQUERY);
      }
  }

"@" {
    curr_lexer->lexer_debug ("@");

    curr_lexer->current_input_column++;

    curr_lexer->looking_at_function_handle++;
    curr_lexer->looking_for_object_index = false;
    curr_lexer->at_beginning_of_statement = false;

    return curr_lexer->count_token ('@');
  }

%{
// A new line character.  New line characters inside matrix constants
// are handled by the <MATRIX_START> start state code above.  If closest
// nesting is inside parentheses, don't return a row separator.
%}

{NL} {
    curr_lexer->lexer_debug ("{NL}");

    curr_lexer->input_line_number++;
    curr_lexer->current_input_column = 1;

    if (curr_lexer->nesting_level.none ())
      {
        curr_lexer->at_beginning_of_statement = true;
        return curr_lexer->count_token ('\n');
      }
    else if (curr_lexer->nesting_level.is_paren ())
      {
        curr_lexer->at_beginning_of_statement = false;
        curr_lexer->gripe_matlab_incompatible ("bare newline inside parentheses");
      }
    else if (curr_lexer->nesting_level.is_bracket_or_brace ())
      return LEXICAL_ERROR;
  }

%{
// Single quote can either be the beginning of a string or a transpose
// operator.
%}

"'" {
    curr_lexer->lexer_debug ("'");

    if (curr_lexer->previous_token_may_be_command ()
        &&  curr_lexer->space_follows_previous_token ())
      {
        yyless (0);
        curr_lexer->push_start_state (COMMAND_START);
      }
    else
      {
        int tok = curr_lexer->previous_token_value ();

        bool transpose = false;

        if (curr_lexer->whitespace_is_significant ())
          {
            if (curr_lexer->space_follows_previous_token ())
              {
                if (tok == '[' || tok == '{'
                    || curr_lexer->previous_token_is_binop ())
                  {
                    curr_lexer->current_input_column++;
                    int retval = curr_lexer->handle_string ('\'');
                    return curr_lexer->count_token_internal (retval);
                  }
                else
                  {
                    yyless (0);
                    curr_lexer->xunput (',');
                    // Adjust for comma that was not really in the input stream.
                    curr_lexer->current_input_column--;
                  }
              }
            else
              {
                if (tok == '[' || tok == '{'
                    || curr_lexer->previous_token_is_binop ()
                    || curr_lexer->previous_token_is_keyword ())
                  {
                    curr_lexer->current_input_column++;
                    int retval = curr_lexer->handle_string ('\'');
                    return curr_lexer->count_token_internal (retval);
                  }
                else
                  return curr_lexer->count_token (QUOTE);
              }
          }
        else
          {
            if (! tok || tok == '[' || tok == '{' || tok == '('
                || curr_lexer->previous_token_is_binop ()
                || curr_lexer->previous_token_is_keyword ())
              {
                curr_lexer->current_input_column++;
                int retval = curr_lexer->handle_string ('\'');
                return curr_lexer->count_token_internal (retval);
              }
            else
              return curr_lexer->count_token (QUOTE);
          }
      }
  }

%{
// Double quotes always begin strings.
%}

\" {
    curr_lexer->lexer_debug ("\"");

    if (curr_lexer->previous_token_may_be_command ()
        &&  curr_lexer->space_follows_previous_token ())
      {
        yyless (0);
        curr_lexer->push_start_state (COMMAND_START);
      }
    else
      {
        int tok = curr_lexer->previous_token_value ();

        bool transpose = false;

        if (curr_lexer->whitespace_is_significant ())
          {
            if (curr_lexer->space_follows_previous_token ())
              {
                if (tok == '[' || tok == '{'
                    || curr_lexer->previous_token_is_binop ())
                  {
                    curr_lexer->current_input_column++;
                    int retval = curr_lexer->handle_string ('"');
                    return curr_lexer->count_token_internal (retval);
                  }
                else
                  {
                    yyless (0);
                    curr_lexer->xunput (',');
                    // Adjust for comma that was not really in the input stream.
                    curr_lexer->current_input_column--;
                  }
              }
            else
              {
                curr_lexer->current_input_column++;
                int retval = curr_lexer->handle_string ('"');
                return curr_lexer->count_token_internal (retval);
              }
          }
        else
          {
            curr_lexer->current_input_column++;
            int retval = curr_lexer->handle_string ('"');
            return curr_lexer->count_token_internal (retval);
          }
      }
  }

%{
// Other operators.
%}

":"   { CMD_OR_OP (":", ':', true); }
".+"  { CMD_OR_OP (".+", EPLUS, false); }
".-"  { CMD_OR_OP (".-", EMINUS, false); }
".*"  { CMD_OR_OP (".*", EMUL, true); }
"./"  { CMD_OR_OP ("./", EDIV, true); }
".\\" { CMD_OR_OP (".\\", ELEFTDIV, true); }
".^"  { CMD_OR_OP (".^", EPOW, true); }
".**" { CMD_OR_OP (".**", EPOW, false); }
"<="  { CMD_OR_OP ("<=", EXPR_LE, true); }
"=="  { CMD_OR_OP ("==", EXPR_EQ, true); }
"~="  { CMD_OR_OP ("~=", EXPR_NE, true); }
"!="  { CMD_OR_OP ("!=", EXPR_NE, false); }
">="  { CMD_OR_OP (">=", EXPR_GE, true); }
"&"   { CMD_OR_OP ("&", EXPR_AND, true); }
"|"   { CMD_OR_OP ("|", EXPR_OR, true); }
"<"   { CMD_OR_OP ("<", EXPR_LT, true); }
">"   { CMD_OR_OP (">", EXPR_GT, true); }
"*"   { CMD_OR_OP ("*", '*', true); }
"/"   { CMD_OR_OP ("/", '/', true); }

%{
// In Matlab, '\' may also trigger command syntax.
%}

"\\"  { return curr_lexer->handle_op ("\\", LEFTDIV); }

"^"   { CMD_OR_OP ("^", POW, true); }
"**"  { CMD_OR_OP ("**", POW, false); }
"&&"  { CMD_OR_OP ("&&", EXPR_AND_AND, true); }
"||"  { CMD_OR_OP ("||", EXPR_OR_OR, true); }
"<<"  { CMD_OR_OP ("<<", LSHIFT, false); }
">>"  { CMD_OR_OP (">>", RSHIFT, false); }

";" {
    bool at_beginning_of_statement
      = (! (curr_lexer->whitespace_is_significant ()
            || curr_lexer->looking_at_object_index.front ()));

    return curr_lexer->handle_op (";", ';', at_beginning_of_statement);
  }

"+" { CMD_OR_UNARY_OP ("+", '+', true); }
"-" { CMD_OR_UNARY_OP ("-", '-', true); }

"~" { CMD_OR_UNARY_OP ("~", EXPR_NOT, true); }
"!" { CMD_OR_UNARY_OP ("!", EXPR_NOT, false); }

"," {
    bool at_beginning_of_statement
      = (! (curr_lexer->whitespace_is_significant ()
            || curr_lexer->looking_at_object_index.front ()));

    return curr_lexer->handle_op (",", ',', at_beginning_of_statement);
  }

".'" {
    return curr_lexer->handle_op (".'", TRANSPOSE, false);
  }

"++" {
    curr_lexer->lexer_debug ("++");

    int tok = curr_lexer->handle_incompatible_unary_op (PLUS_PLUS, false);

    if (tok < 0)
      {
        yyless (0);
        curr_lexer->xunput (',');
        // Adjust for comma that was not really in the input stream.
        curr_lexer->current_input_column--;
      }
    else
      return tok;
  }

"--" {
    curr_lexer->lexer_debug ("--");

    int tok = curr_lexer->handle_incompatible_unary_op (MINUS_MINUS, false);

    if (tok < 0)
      {
        yyless (0);
        curr_lexer->xunput (',');
        // Adjust for comma that was not really in the input stream.
        curr_lexer->current_input_column--;
      }
    else
      return tok;
  }

"(" {
    curr_lexer->lexer_debug ("(");

    bool unput_comma = false;

    if (curr_lexer->whitespace_is_significant ()
        && curr_lexer->space_follows_previous_token ())
      {
        int tok = curr_lexer->previous_token_value ();

        if (! (tok == '[' || tok == '{'
               || curr_lexer->previous_token_is_binop ()))
          unput_comma = true;
      }

    if (unput_comma)
      {
        yyless (0);
        curr_lexer->xunput (',');
        // Adjust for comma that was not really in the input stream.
        curr_lexer->current_input_column--;
      }
    else
      {
        // If we are looking for an object index, then push TRUE for
        // looking_at_object_index.  Otherwise, just push whatever state
        // is current (so that we can pop it off the stack when we find
        // the matching close paren).

        curr_lexer->looking_at_object_index.push_front
          (curr_lexer->looking_for_object_index);

        curr_lexer->looking_at_indirect_ref = false;
        curr_lexer->looking_for_object_index = false;
        curr_lexer->at_beginning_of_statement = false;

        curr_lexer->nesting_level.paren ();
        curr_lexer->decrement_promptflag ();

        return curr_lexer->handle_token ('(');
      }
  }

")" {
    curr_lexer->lexer_debug (")");

    curr_lexer->nesting_level.remove ();
    curr_lexer->current_input_column++;

    curr_lexer->looking_at_object_index.pop_front ();

    curr_lexer->looking_for_object_index = true;
    curr_lexer->at_beginning_of_statement = false;

    if (curr_lexer->looking_at_anon_fcn_args)
      {
        curr_lexer->looking_at_anon_fcn_args = false;
        curr_lexer->nesting_level.anon_fcn_body ();
      }

    return curr_lexer->count_token (')');
  }

"." {
    curr_lexer->lexer_debug (".");

    curr_lexer->looking_for_object_index = false;
    curr_lexer->at_beginning_of_statement = false;

    return curr_lexer->handle_token ('.');
  }

%{
// = and op= operators.
%}

"="    { return curr_lexer->handle_op ("=", '='); }
"+="   { return curr_lexer->handle_incompatible_op ("+=", ADD_EQ); }
"-="   { return curr_lexer->handle_incompatible_op ("-=", SUB_EQ); }
"*="   { return curr_lexer->handle_incompatible_op ("*=", MUL_EQ); }
"/="   { return curr_lexer->handle_incompatible_op ("/=", DIV_EQ); }
"\\="  { return curr_lexer->handle_incompatible_op ("\\=", LEFTDIV_EQ); }
".+="  { return curr_lexer->handle_incompatible_op (".+=", ADD_EQ); }
".-="  { return curr_lexer->handle_incompatible_op (".-=", SUB_EQ); }
".*="  { return curr_lexer->handle_incompatible_op (".*=", EMUL_EQ); }
"./="  { return curr_lexer->handle_incompatible_op ("./=", EDIV_EQ); }
".\\=" { return curr_lexer->handle_incompatible_op (".\\=", ELEFTDIV_EQ); }
"^="   { return curr_lexer->handle_incompatible_op ("^=", POW_EQ); }
"**="  { return curr_lexer->handle_incompatible_op ("^=", POW_EQ); }
".^="  { return curr_lexer->handle_incompatible_op (".^=", EPOW_EQ); }
".**=" { return curr_lexer->handle_incompatible_op (".^=", EPOW_EQ); }
"&="   { return curr_lexer->handle_incompatible_op ("&=", AND_EQ); }
"|="   { return curr_lexer->handle_incompatible_op ("|=", OR_EQ); }
"<<="  { return curr_lexer->handle_incompatible_op ("<<=", LSHIFT_EQ); }
">>="  { return curr_lexer->handle_incompatible_op (">>=", RSHIFT_EQ); }

%{
// In Matlab, '{' may also trigger command syntax.
%}

"{" {
    curr_lexer->lexer_debug ("{");

    bool unput_comma = false;

    if (curr_lexer->whitespace_is_significant ()
        && curr_lexer->space_follows_previous_token ())
      {
        int tok = curr_lexer->previous_token_value ();

        if (! (tok == '[' || tok == '{'
               || curr_lexer->previous_token_is_binop ()))
          unput_comma = true;
      }

    if (unput_comma)
      {
        yyless (0);
        curr_lexer->xunput (',');
        // Adjust for comma that was not really in the input stream.
        curr_lexer->current_input_column--;
      }
    else
      {
        curr_lexer->nesting_level.brace ();

        curr_lexer->looking_at_object_index.push_front
          (curr_lexer->looking_for_object_index);

        curr_lexer->current_input_column += yyleng;
        curr_lexer->looking_for_object_index = false;
        curr_lexer->at_beginning_of_statement = false;

        curr_lexer->decrement_promptflag ();

        curr_lexer->braceflag++;

        curr_lexer->push_start_state (MATRIX_START);

        return curr_lexer->count_token ('{');
      }
  }

"}" {
    curr_lexer->lexer_debug ("}");

    curr_lexer->looking_at_object_index.pop_front ();

    curr_lexer->looking_for_object_index = true;
    curr_lexer->at_beginning_of_statement = false;

    curr_lexer->nesting_level.remove ();

    return curr_lexer->handle_token ('}');
  }

%{
// Unrecognized input is a lexical error.
%}

. {
    curr_lexer->lexer_debug (".");

    curr_lexer->xunput (yytext[0]);

    int c = curr_lexer->text_yyinput ();

    if (c != EOF)
      {
        curr_lexer->current_input_column++;

        error ("invalid character '%s' (ASCII %d) near line %d, column %d",
               undo_string_escape (static_cast<char> (c)), c,
               curr_lexer->input_line_number, curr_lexer->current_input_column);

        return LEXICAL_ERROR;
      }
    else
      return curr_lexer->handle_end_of_input ();
  }

%%

static void
display_character (char c)
{
  if (isgraph (c))
    std::cerr << c;
  else
    switch (c)
      {
      case 0:
        std::cerr << "NUL";
        break;

      case 1:
        std::cerr << "SOH";
        break;

      case 2:
        std::cerr << "STX";
        break;

      case 3:
        std::cerr << "ETX";
        break;

      case 4:
        std::cerr << "EOT";
        break;

      case 5:
        std::cerr << "ENQ";
        break;

      case 6:
        std::cerr << "ACK";
        break;

      case 7:
        std::cerr << "\\a";
        break;

      case 8:
        std::cerr << "\\b";
        break;

      case 9:
        std::cerr << "\\t";
        break;

      case 10:
        std::cerr << "\\n";
        break;

      case 11:
        std::cerr << "\\v";
        break;

      case 12:
        std::cerr << "\\f";
        break;

      case 13:
        std::cerr << "\\r";
        break;

      case 14:
        std::cerr << "SO";
        break;

      case 15:
        std::cerr << "SI";
        break;

      case 16:
        std::cerr << "DLE";
        break;

      case 17:
        std::cerr << "DC1";
        break;

      case 18:
        std::cerr << "DC2";
        break;

      case 19:
        std::cerr << "DC3";
        break;

      case 20:
        std::cerr << "DC4";
        break;

      case 21:
        std::cerr << "NAK";
        break;

      case 22:
        std::cerr << "SYN";
        break;

      case 23:
        std::cerr << "ETB";
        break;

      case 24:
        std::cerr << "CAN";
        break;

      case 25:
        std::cerr << "EM";
        break;

      case 26:
        std::cerr << "SUB";
        break;

      case 27:
        std::cerr << "ESC";
        break;

      case 28:
        std::cerr << "FS";
        break;

      case 29:
        std::cerr << "GS";
        break;

      case 30:
        std::cerr << "RS";
        break;

      case 31:
        std::cerr << "US";
        break;

      case 32:
        std::cerr << "SPACE";
        break;

      case 127:
        std::cerr << "DEL";
        break;
      }
}

void
cleanup_parser (void)
{
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

bool
is_keyword (const std::string& s)
{
  // Parsing function names like "set.property_name" inside
  // classdef-style class definitions is simplified by handling the
  // "set" and "get" portions of the names using the same mechanism as
  // is used for keywords.  However, they are not really keywords in
  // the language, so omit them from the list of possible keywords.

  return (octave_kw_hash::in_word_set (s.c_str (), s.length ()) != 0
          && ! (s == "set" || s == "get"));
}

DEFUN (iskeyword, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} iskeyword ()\n\
@deftypefnx {Built-in Function} {} iskeyword (@var{name})\n\
Return true if @var{name} is an Octave keyword.  If @var{name}\n\
is omitted, return a list of keywords.\n\
@seealso{isvarname, exist}\n\
@end deftypefn")
{
  octave_value retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("iskeyword");

  if (error_state)
    return retval;

  if (argc == 1)
    {
      // Neither set and get are keywords.  See the note in the
      // is_keyword function for additional details.

      string_vector lst (TOTAL_KEYWORDS);

      int j = 0;

      for (int i = 0; i < TOTAL_KEYWORDS; i++)
        {
          std::string tmp = wordlist[i].name;

          if (! (tmp == "set" || tmp == "get"))
            lst[j++] = tmp;
        }

      lst.resize (j);

      retval = Cell (lst.sort ());
    }
  else if (argc == 2)
    {
      retval = is_keyword (argv[1]);
    }
  else
    print_usage ();

  return retval;
}

/*

%!assert (iskeyword ("for"))
%!assert (iskeyword ("fort"), false)
%!assert (iskeyword ("fft"), false)

*/

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

DEFUN (__display_tokens__, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __display_tokens__ ()\n\
Query or set the internal variable that determines whether Octave's\n\
lexer displays tokens as they are read.\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (display_tokens);
}

DEFUN (__token_count__, , ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __token_count__ ()\n\
Number of language tokens processed since Octave startup.\n\
@end deftypefn")
{
  return octave_value (Vtoken_count);
}

DEFUN (__lexer_debug_flag__, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{old_val} =} __lexer_debug_flag__ (@var{new_val}))\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  retval = set_internal_variable (lexer_debug_flag, args, nargout,
                                  "__lexer_debug_flag__");

  return retval;
}

lexical_feedback::~lexical_feedback (void)
{
  tokens.clear ();
}

void
lexical_feedback::init (void)
{
  // The closest paren, brace, or bracket nesting is not an object
  // index.
  looking_at_object_index.push_front (false);
}

void
lexical_feedback::reset (void)
{
  end_of_input = false;
  at_beginning_of_statement = true;
  looking_at_anon_fcn_args = false;
  looking_at_return_list = false;
  looking_at_parameter_list = false;
  looking_at_decl_list = false;
  looking_at_initializer_expression = false;
  looking_at_matrix_or_assign_lhs = false;
  looking_for_object_index = false; 
  looking_at_indirect_ref = false;
  parsing_class_method = false;
  maybe_classdef_get_set_method = false;
  parsing_classdef = false;
  force_script = false;
  reading_fcn_file = false;
  reading_script_file = false;
  reading_classdef_file = false;
  input_line_number = 1;
  current_input_column = 1;
  bracketflag = 0;
  braceflag = 0;
  looping = 0;
  defining_func = 0;
  looking_at_function_handle = 0;
  block_comment_nesting_level = 0;
  token_count = 0;
  current_input_line = "";
  comment_text = "";
  help_text = "";
  fcn_file_name = "";
  fcn_file_full_name = "";
  looking_at_object_index.clear ();
  looking_at_object_index.push_front (false);

  while (! parsed_function_name.empty ())
    parsed_function_name.pop ();

  nesting_level.reset ();

  tokens.clear ();
}

int
lexical_feedback::previous_token_value (void) const
{
  const token *tok = tokens.front ();
  return tok ? tok->token_value () : 0;
}

bool
lexical_feedback::previous_token_value_is (int tok_val) const
{
  const token *tok = tokens.front ();
  return tok ? tok->token_value_is (tok_val) : false;
}

void
lexical_feedback::mark_previous_token_trailing_space (void)
{
  token *tok = tokens.front ();
  if (tok && ! previous_token_value_is ('\n'))
    tok->mark_trailing_space ();
}

bool
lexical_feedback::space_follows_previous_token (void) const
{
  const token *tok = tokens.front ();
  return tok ? tok->space_follows_token () : false;
}

bool
lexical_feedback::previous_token_is_binop (void) const
{
  int tok = previous_token_value ();

  return (tok == '+' || tok == '-' || tok == '@'
          || tok == ',' || tok == ';' || tok == '*' || tok == '/'
          || tok == ':' || tok == '=' || tok == ADD_EQ
          || tok == AND_EQ || tok == DIV_EQ || tok == EDIV
          || tok == EDIV_EQ || tok == ELEFTDIV || tok == ELEFTDIV_EQ
          || tok == EMINUS || tok == EMUL || tok == EMUL_EQ
          || tok == EPOW || tok == EPOW_EQ || tok == EXPR_AND
          || tok == EXPR_AND_AND || tok == EXPR_EQ || tok == EXPR_GE
          || tok == EXPR_GT || tok == EXPR_LE || tok == EXPR_LT
          || tok == EXPR_NE || tok == EXPR_NOT || tok == EXPR_OR
          || tok == EXPR_OR_OR || tok == LEFTDIV || tok == LEFTDIV_EQ
          || tok == LSHIFT || tok == LSHIFT_EQ || tok == MUL_EQ
          || tok == OR_EQ || tok == POW || tok == POW_EQ
          || tok == RSHIFT || tok == RSHIFT_EQ || tok == SUB_EQ);
}

bool
lexical_feedback::previous_token_is_keyword (void) const
{
  const token *tok = tokens.front ();
  return tok ? tok->is_keyword () : false;
}

bool
lexical_feedback::previous_token_may_be_command (void) const
{
  const token *tok = tokens.front ();
  return tok ? tok->may_be_command () : false;
}

static bool
looks_like_copyright (const std::string& s)
{
  bool retval = false;

  if (! s.empty ())
    {
      size_t offset = s.find_first_not_of (" \t");

      retval = (s.substr (offset, 9) == "Copyright" || s.substr (offset, 6) == "Author");
    }

  return retval;
}

void
octave_lexer::input_buffer::fill (const std::string& input, bool eof_arg)
{
  buffer = input;
  chars_left = buffer.length ();
  pos = buffer.c_str ();
  eof = eof_arg;
}

int
octave_lexer::input_buffer::copy_chunk (char *buf, size_t max_size)
{
  static const char * const eol = "\n";

  size_t len = max_size > chars_left ? chars_left : max_size;
  assert (len > 0);

  memcpy (buf, pos, len);

  chars_left -= len;
  pos += len;

  // Make sure input ends with a new line character.
  if (chars_left == 0 && buf[len-1] != '\n')
    {
      if (len < max_size)
        {
          // There is enough room to plug the newline character in
          // the buffer.
          buf[len++] = '\n';
        }
      else
        {
          // There isn't enough room to plug the newline character
          // in the buffer so arrange to have it returned on the next
          // call to octave_lexer::read.
          pos = eol;
          chars_left = 1;
        }
    }

  return len;
}

octave_lexer::~octave_lexer (void)
{
  yylex_destroy (scanner);
}

void
octave_lexer::init (void)
{
  yylex_init (&scanner);

  // Make octave_lexer object available through yyextra in
  // flex-generated lexer.
  yyset_extra (this, scanner);

  clear_start_state ();
}

// Inside Flex-generated functions, yyg is the scanner cast to its real
// type.  Some flex macros that we use in octave_lexer member functions
// (for example, BEGIN) use yyg.  If we could perform the actions of
// these macros with functions instead, we could eliminate the
// OCTAVE_YYG macro.

#define OCTAVE_YYG \
  struct yyguts_t *yyg = static_cast<struct yyguts_t*> (scanner)

void
octave_lexer::reset (void)
{
  // Start off on the right foot.
  clear_start_state ();

  parser_symtab_context.clear ();

  // We do want a prompt by default.
  promptflag (1);

  // Only ask for input from stdin if we are expecting interactive
  // input.

  if (! quitting_gracefully
      && (interactive || forced_interactive)
      && ! (reading_fcn_file
            || reading_classdef_file
            || reading_script_file
            || input_from_eval_string ()))
    yyrestart (stdin, scanner);

  input_reader.reset ();

  lexical_feedback::reset ();
}

void
octave_lexer::prep_for_file (void)
{
  reading_script_file = true;

  push_start_state (INPUT_FILE_START);
}

int
octave_lexer::read (char *buf, unsigned max_size)
{
  int status = 0;

  if (input_buf.empty ())
    {
      bool eof = false;
      current_input_line = input_reader.get_input (eof);
      input_buf.fill (current_input_line, eof);
    }

  if (! input_buf.empty ())
    status = input_buf.copy_chunk (buf, max_size);
  else
    {
      status = YY_NULL;

      if (! input_buf.at_eof ())
        fatal_error ("octave_lexer::read () in flex scanner failed");
    }

  return status;
}

int
octave_lexer::handle_end_of_input (void)
{
  lexer_debug ("<<EOF>>");

  if (block_comment_nesting_level != 0)
    {
      warning ("block comment open at end of input");

      if ((reading_fcn_file || reading_script_file || reading_classdef_file)
          && ! fcn_file_name.empty ())
        warning ("near line %d of file '%s.m'",
                 input_line_number, fcn_file_name.c_str ());
    }

  return handle_token (END_OF_INPUT);
}

char *
octave_lexer::flex_yytext (void)
{
  return yyget_text (scanner);
}

int
octave_lexer::flex_yyleng (void)
{
  return yyget_leng (scanner);
}

int
octave_lexer::text_yyinput (void)
{
  int c = yyinput (scanner);

  if (lexer_debug_flag)
    {
      std::cerr << "I: ";
      display_character (c);
      std::cerr << std::endl;
    }

  // Convert CRLF into just LF and single CR into LF.

  if (c == '\r')
    {
      c = yyinput (scanner);

      if (lexer_debug_flag)
        {
          std::cerr << "I: ";
          display_character (c);
          std::cerr << std::endl;
        }

      if (c != '\n')
        {
          xunput (c);
          c = '\n';
        }
    }

  return c;
}

void
octave_lexer::xunput (char c, char *buf)
{
  if (c != EOF)
    {
      if (lexer_debug_flag)
        {
          std::cerr << "U: ";
          display_character (c);
          std::cerr << std::endl;
        }

      yyunput (c, buf, scanner);
    }
}

void
octave_lexer::xunput (char c)
{
  char *yytxt = flex_yytext ();

  xunput (c, yytxt);
}

bool
octave_lexer::looking_at_space (void)
{
  int c = text_yyinput ();
  xunput (c);
  return (c == ' ' || c == '\t');
}

bool
octave_lexer::inside_any_object_index (void)
{
  bool retval = false;

  for (std::list<bool>::const_iterator i = looking_at_object_index.begin ();
       i != looking_at_object_index.end (); i++)
    {
      if (*i)
        {
          retval = true;
          break;
        }
    }

  return retval;
}

// Handle keywords.  Return -1 if the keyword should be ignored.

int
octave_lexer::is_keyword_token (const std::string& s)
{
  int l = input_line_number;
  int c = current_input_column;

  int len = s.length ();

  const octave_kw *kw = octave_kw_hash::in_word_set (s.c_str (), len);

  if (kw)
    {
      token *tok_val = 0;

      switch (kw->kw_id)
        {
        case break_kw:
        case catch_kw:
        case continue_kw:
        case else_kw:
        case otherwise_kw:
        case return_kw:
        case unwind_protect_cleanup_kw:
          at_beginning_of_statement = true;
          break;

        case static_kw:
          if ((reading_fcn_file || reading_script_file
               || reading_classdef_file)
              && ! fcn_file_full_name.empty ())
            warning_with_id ("Octave:deprecated-keyword",
                             "the 'static' keyword is obsolete and will be removed from a future version of Octave; please use 'persistent' instead; near line %d of file '%s'",
                             input_line_number,
                             fcn_file_full_name.c_str ());
          else
            warning_with_id ("Octave:deprecated-keyword",
                             "the 'static' keyword is obsolete and will be removed from a future version of Octave; please use 'persistent' instead; near line %d",
                             input_line_number);
          // fall through ...

        case persistent_kw:
        case global_kw:
          looking_at_decl_list = true;
          break;

        case case_kw:
        case elseif_kw:
        case until_kw:
          break;

        case end_kw:
          if (inside_any_object_index ()
              || (! reading_classdef_file
                  && (defining_func
                      && ! (looking_at_return_list
                            || parsed_function_name.top ()))))
            return 0;

          tok_val = new token (end_kw, token::simple_end, l, c);
          at_beginning_of_statement = true;
          break;

        case end_try_catch_kw:
          tok_val = new token (end_try_catch_kw, token::try_catch_end, l, c);
          at_beginning_of_statement = true;
          break;

        case end_unwind_protect_kw:
          tok_val = new token (end_unwind_protect_kw,
                               token::unwind_protect_end, l, c);
          at_beginning_of_statement = true;
          break;

        case endfor_kw:
          tok_val = new token (endfor_kw, token::for_end, l, c);
          at_beginning_of_statement = true;
          break;

        case endfunction_kw:
          tok_val = new token (endfunction_kw, token::function_end, l, c);
          at_beginning_of_statement = true;
          break;

        case endif_kw:
          tok_val = new token (endif_kw, token::if_end, l, c);
          at_beginning_of_statement = true;
          break;

        case endparfor_kw:
          tok_val = new token (endparfor_kw, token::parfor_end, l, c);
          at_beginning_of_statement = true;
          break;

        case endswitch_kw:
          tok_val = new token (endswitch_kw, token::switch_end, l, c);
          at_beginning_of_statement = true;
          break;

        case endwhile_kw:
          tok_val = new token (endwhile_kw, token::while_end, l, c);
          at_beginning_of_statement = true;
          break;

        case endclassdef_kw:
          tok_val = new token (endclassdef_kw, token::classdef_end, l, c);
          at_beginning_of_statement = true;
          break;

        case endenumeration_kw:
          tok_val = new token (endenumeration_kw, token::enumeration_end, l, c);
          at_beginning_of_statement = true;
          break;

        case endevents_kw:
          tok_val = new token (endevents_kw, token::events_end, l, c);
          at_beginning_of_statement = true;
          break;

        case endmethods_kw:
          tok_val = new token (endmethods_kw, token::methods_end, l, c);
          at_beginning_of_statement = true;
          break;

        case endproperties_kw:
          tok_val = new token (endproperties_kw, token::properties_end, l, c);
          at_beginning_of_statement = true;
          break;


        case for_kw:
        case parfor_kw:
        case while_kw:
          decrement_promptflag ();
          looping++;
          break;

        case do_kw:
          at_beginning_of_statement = true;
          decrement_promptflag ();
          looping++;
          break;

        case try_kw:
        case unwind_protect_kw:
          at_beginning_of_statement = true;
          decrement_promptflag ();
          break;

        case if_kw:
        case switch_kw:
          decrement_promptflag ();
          break;

        case get_kw:
        case set_kw:
          // 'get' and 'set' are keywords in classdef method
          // declarations.
          if (! maybe_classdef_get_set_method)
            return 0;
          break;

        case enumeration_kw:
        case events_kw:
        case methods_kw:
        case properties_kw:
          // 'properties', 'methods' and 'events' are keywords for
          // classdef blocks.
          if (! parsing_classdef)
            return 0;
          // fall through ...

        case classdef_kw:
          // 'classdef' is always a keyword.
          decrement_promptflag ();

          if (! force_script && token_count == 0 && input_from_file ())
            {
              reading_classdef_file = true;
              reading_script_file = false;
            }
          break;

        case function_kw:
          decrement_promptflag ();

          defining_func++;
          parsed_function_name.push (false);

          if (! force_script && token_count == 0 && input_from_file ())
            {
              reading_fcn_file = true;
              reading_script_file = false;
            }

          if (! (reading_fcn_file || reading_script_file
                 || reading_classdef_file))
            input_line_number = 1;
          break;

        case magic_file_kw:
          {
            if ((reading_fcn_file || reading_script_file
                 || reading_classdef_file)
                && ! fcn_file_full_name.empty ())
              tok_val = new token (magic_file_kw, true,
                                   fcn_file_full_name, l, c);
            else
              tok_val = new token (magic_file_kw, "stdin", l, c);
          }
          break;

        case magic_line_kw:
          tok_val = new token (magic_line_kw, static_cast<double> (l),
                               "", l, c);
          break;

        default:
          panic_impossible ();
        }

      if (! tok_val)
        tok_val = new token (kw->tok, true, l, c);

      push_token (tok_val);

      return kw->tok;
    }

  return 0;
}

bool
octave_lexer::whitespace_is_significant (void)
{
  return (nesting_level.is_bracket ()
          || (nesting_level.is_brace ()
              && ! looking_at_object_index.front ()));
}

static inline bool
looks_like_hex (const char *s, int len)
{
  return (len > 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X'));
}

void
octave_lexer::handle_number (void)
{
  double value = 0.0;
  int nread = 0;

  char *yytxt = flex_yytext ();

  if (looks_like_hex (yytxt, strlen (yytxt)))
    {
      unsigned long ival;

      nread = sscanf (yytxt, "%lx", &ival);

      value = static_cast<double> (ival);
    }
  else
    {
      char *tmp = strsave (yytxt);

      char *idx = strpbrk (tmp, "Dd");

      if (idx)
        *idx = 'e';

      nread = sscanf (tmp, "%lf", &value);

      delete [] tmp;
    }

  // If yytext doesn't contain a valid number, we are in deep doo doo.

  assert (nread == 1);

  looking_for_object_index = false;
  at_beginning_of_statement = false;

  push_token (new token (NUM, value, yytxt, input_line_number,
                         current_input_column));

  current_input_column += flex_yyleng ();
}

void
octave_lexer::handle_continuation (void)
{
  char *yytxt = flex_yytext ();
  int yylng = flex_yyleng ();

  int offset = 1;
  if (yytxt[0] == '\\')
    gripe_matlab_incompatible_continuation ();
  else
    offset = 3;

  bool have_space = false;
  while (offset < yylng)
    {
      char c = yytxt[offset];
      if (c == ' ' || c == '\t')
        {
          have_space = true;
          offset++;
        }
      else
        break;
    }

  if (have_space)
    mark_previous_token_trailing_space ();

  bool have_comment = false;
  while (offset < yylng)
    {
      char c = yytxt[offset];
      if (c == '#' || c == '%')
        {
          have_comment = true;
          offset++;
        }
      else
        break;
    }

  if (have_comment)
    {
      comment_text = &yytxt[offset];

      finish_comment (octave_comment_elt::end_of_line, true);
    }

  decrement_promptflag ();
  input_line_number++;
  current_input_column = 1;
}

void
octave_lexer::finish_comment (octave_comment_elt::comment_type typ,
                              bool looking_at_continuation)
{
  bool copyright = looks_like_copyright (comment_text);

  if (nesting_level.none () && help_text.empty ()
    && ! comment_text.empty () && ! copyright)
    help_text = comment_text;

  if (copyright)
    typ = octave_comment_elt::copyright;

  octave_comment_buffer::append (comment_text, typ);

  comment_text = "";

  at_beginning_of_statement = true;

  if (! looking_at_continuation)
    {
      xunput ('\n');
      // Adjust for newline that was not really in the input stream.
      input_line_number--;
    }
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

bool
octave_lexer::have_continuation (bool trailing_comments_ok)
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
          decrement_promptflag ();
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

  xunput (c);
  return false;

cleanup:

  std::string s = buf.str ();

  int len = s.length ();
  while (len--)
    xunput (s[len]);

  return false;
}

// We have seen a '.' and need to see if it is the start of a
// continuation.  If so, this eats it, up to and including the new
// line character.

bool
octave_lexer::have_ellipsis_continuation (bool trailing_comments_ok)
{
  char c1 = text_yyinput ();
  if (c1 == '.')
    {
      char c2 = text_yyinput ();
      if (c2 == '.' && have_continuation (trailing_comments_ok))
        return true;
      else
        {
          xunput (c2);
          xunput (c1);
        }
    }
  else
    xunput (c1);

  return false;
}

int
octave_lexer::handle_string (char delim)
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
                }
              else
                {
                  std::string s;
                  xunput (c);

                  if (delim == '\'')
                    s = buf.str ();
                  else
                    s = do_string_escapes (buf.str ());

                  if (delim == '"')
                    gripe_matlab_incompatible ("\" used as string delimiter");
                  else if (delim == '\'')
                    gripe_single_quote_string ();

                  looking_for_object_index = true;
                  at_beginning_of_statement = false;

                  int tok = delim == '"' ? DQ_STRING : SQ_STRING;

                  push_token (new token (tok, s, bos_line, bos_col));

                  return tok;
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

int
octave_lexer::handle_close_bracket (int bracket_type)
{
  int retval = bracket_type;

  if (! nesting_level.none ())
    {
      nesting_level.remove ();

      if (bracket_type == ']')
        bracketflag--;
      else if (bracket_type == '}')
        braceflag--;
      else
        panic_impossible ();
    }

  pop_start_state ();

  return retval;
}

bool
octave_lexer::looks_like_command_arg (void)
{
  bool space_before = space_follows_previous_token ();
  bool space_after = looking_at_space ();

  return (space_before && ! space_after
          && previous_token_may_be_command ());
}

int
octave_lexer::handle_superclass_identifier (void)
{
  std::string pkg;
  char *yytxt = flex_yytext ();
  std::string meth = strip_trailing_whitespace (yytxt);
  size_t pos = meth.find ("@");
  std::string cls = meth.substr (pos).substr (1);
  meth = meth.substr (0, pos - 1);

  pos = cls.find (".");
  if (pos != std::string::npos)
    {
      pkg = cls.substr (pos).substr (1);
      cls = cls.substr (0, pos - 1);
    }

  int kw_token = (is_keyword_token (meth) || is_keyword_token (cls)
                  || is_keyword_token (pkg));
  if (kw_token)
    {
      error ("method, class and package names may not be keywords");
      return LEXICAL_ERROR;
    }

  push_token (new token (SUPERCLASSREF,
                         meth.empty () ? 0 : &(symbol_table::insert (meth)),
                         cls.empty () ? 0 : &(symbol_table::insert (cls)),
                         pkg.empty () ? 0 : &(symbol_table::insert (pkg)),
                         input_line_number, current_input_column));

  current_input_column += flex_yyleng ();

  return SUPERCLASSREF;
}

int
octave_lexer::handle_meta_identifier (void)
{
  std::string pkg;
  char *yytxt = flex_yytext ();
  std::string cls = strip_trailing_whitespace (yytxt).substr (1);
  size_t pos = cls.find (".");

  if (pos != std::string::npos)
    {
      pkg = cls.substr (pos).substr (1);
      cls = cls.substr (0, pos - 1);
    }

  int kw_token = is_keyword_token (cls) || is_keyword_token (pkg);
  if (kw_token)
    {
       error ("class and package names may not be keywords");
      return LEXICAL_ERROR;
    }

  push_token (new token (METAQUERY,
                         cls.empty () ? 0 : &(symbol_table::insert (cls)),
                         pkg.empty () ? 0 : &(symbol_table::insert (pkg)),
                         input_line_number, current_input_column));

  current_input_column += flex_yyleng ();

  return METAQUERY;
}

// Figure out exactly what kind of token to return when we have seen
// an identifier.  Handles keywords.  Return -1 if the identifier
// should be ignored.

int
octave_lexer::handle_identifier (void)
{
  char *yytxt = flex_yytext ();

  std::string tok = yytxt;

  // If we are expecting a structure element, avoid recognizing
  // keywords and other special names and return STRUCT_ELT, which is
  // a string that is also a valid identifier.  But first, we have to
  // decide whether to insert a comma.

  if (looking_at_indirect_ref)
    {
      push_token (new token (STRUCT_ELT, tok, input_line_number,
                             current_input_column));

      looking_for_object_index = true;

      current_input_column += flex_yyleng ();

      return STRUCT_ELT;
    }

  // The is_keyword_token may reset
  // at_beginning_of_statement.  For example, if it sees
  // an else token, then the next token is at the beginning of a
  // statement.

  // May set at_beginning_of_statement to true.
  int kw_token = is_keyword_token (tok);

  // If we found a keyword token, then the beginning_of_statement flag
  // is already set.  Otherwise, we won't be at the beginning of a
  // statement.

  if (looking_at_function_handle)
    {
      if (kw_token)
        {
          error ("function handles may not refer to keywords");

          return LEXICAL_ERROR;
        }
      else
        {
          push_token (new token (FCN_HANDLE, tok, input_line_number,
                                 current_input_column));

          current_input_column += flex_yyleng ();
          looking_for_object_index = true;

          at_beginning_of_statement = false;

          return FCN_HANDLE;
        }
    }

  // If we have a regular keyword, return it.
  // Keywords can be followed by identifiers.

  if (kw_token)
    {
      if (kw_token >= 0)
        {
          current_input_column += flex_yyleng ();
          looking_for_object_index = false;
        }

      return kw_token;
    }

  // Find the token in the symbol table.  Beware the magic
  // transformation of the end keyword...

  if (tok == "end")
    tok = "__end__";

  token *tok_val = new token (NAME, &(symbol_table::insert (tok)),
                              input_line_number, current_input_column);

  if (at_beginning_of_statement
      && (! (tok == "e"
             || tok == "I" || tok == "i"
             || tok == "J" || tok == "j"
             || tok == "Inf" || tok == "inf"
             || tok == "NaN" || tok == "nan")))
    tok_val->mark_may_be_command ();

  push_token (tok_val);

  current_input_column += flex_yyleng ();

  if (tok != "__end__")
    looking_for_object_index = true;

  at_beginning_of_statement = false;

  return NAME;
}

void
octave_lexer::maybe_warn_separator_insert (char sep)
{
  std::string nm = fcn_file_full_name;

  if (nm.empty ())
    warning_with_id ("Octave:separator-insert",
                     "potential auto-insertion of '%c' near line %d",
                     sep, input_line_number);
  else
    warning_with_id ("Octave:separator-insert",
                     "potential auto-insertion of '%c' near line %d of file %s",
                     sep, input_line_number, nm.c_str ());
}

void
octave_lexer::gripe_single_quote_string (void)
{
  std::string nm = fcn_file_full_name;

  if (nm.empty ())
    warning_with_id ("Octave:single-quote-string",
                     "single quote delimited string near line %d",
                     input_line_number);
  else
    warning_with_id ("Octave:single-quote-string",
                     "single quote delimited string near line %d of file %s",
                     input_line_number, nm.c_str ());
}

void
octave_lexer::gripe_matlab_incompatible (const std::string& msg)
{
  std::string nm = fcn_file_full_name;

  if (nm.empty ())
    warning_with_id ("Octave:matlab-incompatible",
                     "potential Matlab compatibility problem: %s",
                     msg.c_str ());
  else
    warning_with_id ("Octave:matlab-incompatible",
                     "potential Matlab compatibility problem: %s near line %d offile %s",
                     msg.c_str (), input_line_number, nm.c_str ());
}

void
octave_lexer::maybe_gripe_matlab_incompatible_comment (char c)
{
  if (c == '#')
    gripe_matlab_incompatible ("# used as comment character");
}

void
octave_lexer::gripe_matlab_incompatible_continuation (void)
{
  gripe_matlab_incompatible ("\\ used as line continuation marker");
}

void
octave_lexer::gripe_matlab_incompatible_operator (const std::string& op)
{
  std::string t = op;
  int n = t.length ();
  if (t[n-1] == '\n')
    t.resize (n-1);
  gripe_matlab_incompatible (t + " used as operator");
}

void
octave_lexer::push_token (token *tok)
{
  YYSTYPE *lval = yyget_lval (scanner);
  lval->tok_val = tok;
  tokens.push (tok);
}

token *
octave_lexer::current_token (void)
{
  YYSTYPE *lval = yyget_lval (scanner);
  return lval->tok_val;
}

void
octave_lexer::display_token (int tok)
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

    case NUM:
    case IMAG_NUM:
      {
        token *tok_val = current_token ();
        std::cerr << (tok == NUM ? "NUM" : "IMAG_NUM")
                  << " [" << tok_val->number () << "]\n";
      }
      break;

    case STRUCT_ELT:
      {
        token *tok_val = current_token ();
        std::cerr << "STRUCT_ELT [" << tok_val->text () << "]\n";
      }
      break;

    case NAME:
      {
        token *tok_val = current_token ();
        symbol_table::symbol_record *sr = tok_val->sym_rec ();
        std::cerr << "NAME";
        if (sr)
          std::cerr << " [" << sr->name () << "]";
        std::cerr << "\n";
      }
      break;

    case END: std::cerr << "END\n"; break;

    case DQ_STRING:
    case SQ_STRING:
      {
        token *tok_val = current_token ();

        std::cerr << (tok == DQ_STRING ? "DQ_STRING" : "SQ_STRING")
                  << " [" << tok_val->text () << "]\n";
      }
      break;

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
    case PERSISTENT: std::cerr << "PERSISTENT\n"; break;
    case FCN_HANDLE: std::cerr << "FCN_HANDLE\n"; break;
    case END_OF_INPUT: std::cerr << "END_OF_INPUT\n\n"; break;
    case LEXICAL_ERROR: std::cerr << "LEXICAL_ERROR\n\n"; break;
    case FCN: std::cerr << "FCN\n"; break;
    case INPUT_FILE: std::cerr << "INPUT_FILE\n"; break;
    case SUPERCLASSREF: std::cerr << "SUPERCLASSREF\n"; break;
    case METAQUERY: std::cerr << "METAQUERY\n"; break;
    case GET: std::cerr << "GET\n"; break;
    case SET: std::cerr << "SET\n"; break;
    case PROPERTIES: std::cerr << "PROPERTIES\n"; break;
    case METHODS: std::cerr << "METHODS\n"; break;
    case EVENTS: std::cerr << "EVENTS\n"; break;
    case CLASSDEF: std::cerr << "CLASSDEF\n"; break;
    case '\n': std::cerr << "\\n\n"; break;
    case '\r': std::cerr << "\\r\n"; break;
    case '\t': std::cerr << "TAB\n"; break;
    default:
      {
        if (tok < 256 && tok > 31)
          std::cerr << static_cast<char> (tok) << "\n";
        else
          std::cerr << "UNKNOWN(" << tok << ")\n";
      }
      break;
    }
}

void
octave_lexer::fatal_error (const char *msg)
{
  error (msg);

  OCTAVE_QUIT;

  yy_fatal_error (msg, scanner);
}

void
octave_lexer::lexer_debug (const char *pattern)
{
  if (lexer_debug_flag)
    {
      std::cerr << std::endl;

      display_start_state ();

      std::cerr << "P: " << pattern << std::endl;
      std::cerr << "T: " << flex_yytext () << std::endl;
    }
}

void
octave_lexer::push_start_state (int state)
{
  OCTAVE_YYG;

  start_state_stack.push (state);

  BEGIN (start_state ());
}

void
octave_lexer::pop_start_state (void)
{
  OCTAVE_YYG;

  start_state_stack.pop ();

  BEGIN (start_state ());
}

void
octave_lexer::clear_start_state (void)
{
  while (! start_state_stack.empty ())
    start_state_stack.pop ();

  push_start_state (INITIAL);
}

void
octave_lexer::display_start_state (void) const
{
  std::cerr << "S: ";

  switch (start_state ())
    {
    case INITIAL:
      std::cerr << "INITIAL" << std::endl;
      break;

    case COMMAND_START:
      std::cerr << "COMMAND_START" << std::endl;
      break;

    case MATRIX_START:
      std::cerr << "MATRIX_START" << std::endl;
      break;

    case INPUT_FILE_START:
      std::cerr << "INPUT_FILE_BEGIN" << std::endl;
      break;

    case BLOCK_COMMENT_START:
      std::cerr << "BLOCK_COMMENT_START" << std::endl;
      break;

    case LINE_COMMENT_START:
      std::cerr << "LINE_COMMENT_START" << std::endl;
      break;

    default:
      std::cerr << "UNKNOWN START STATE!" << std::endl;
      break;
    }
}

int
octave_lexer::handle_op (const char *pattern, int tok, bool bos)
{
  lexer_debug (pattern);

  return handle_op_internal (tok, bos, true);
}

int
octave_lexer::handle_incompatible_op (const char *pattern, int tok, bool bos)
{
  lexer_debug (pattern);

  return handle_op_internal (tok, bos, false);
}

bool
octave_lexer::maybe_unput_comma_before_unary_op (int tok)
{
  int prev_tok = previous_token_value ();

  bool unput_comma = false;

  if (whitespace_is_significant () && space_follows_previous_token ())
    {
      int c = text_yyinput ();
      xunput (c);

      bool space_after = (c == ' ' || c == '\t');

      if (! (prev_tok == '[' || prev_tok == '{'
             || previous_token_is_binop ()
             || ((tok == '+' || tok == '-') && space_after)))
        unput_comma = true;
    }

  return unput_comma;
}

int
octave_lexer::handle_unary_op (int tok, bool bos)
{
  return maybe_unput_comma_before_unary_op (tok)
    ? -1 : handle_op_internal (tok, bos, true);
}

int
octave_lexer::handle_incompatible_unary_op (int tok, bool bos)
{
  return maybe_unput_comma_before_unary_op (tok)
    ? -1 : handle_op_internal (tok, bos, false);
}

int
octave_lexer::handle_op_internal (int tok, bool bos, bool compat)
{
  if (! compat)
    gripe_matlab_incompatible_operator (flex_yytext ());

  push_token (new token (tok, input_line_number, current_input_column));

  current_input_column += flex_yyleng ();
  looking_for_object_index = false;
  at_beginning_of_statement = bos;

  return count_token (tok);
}

int
octave_lexer::handle_token (const std::string& name, int tok)
{
  token *tok_val = new token (tok, name, input_line_number,
                              current_input_column);

  return handle_token (tok, tok_val);
}

int
octave_lexer::handle_token (int tok, token *tok_val)
{
  if (! tok_val)
    tok_val = new token (tok, input_line_number, current_input_column);

  push_token (tok_val);

  current_input_column += flex_yyleng ();

  return count_token_internal (tok);
}

int
octave_lexer::count_token (int tok)
{
  token *tok_val = new token (tok, input_line_number, current_input_column);

  push_token (tok_val);

  return count_token_internal (tok);
}

int
octave_lexer::count_token_internal (int tok)
{
  if (tok != '\n')
    {
      Vtoken_count++;
      token_count++;
    }

  return show_token (tok);
}

int
octave_lexer::show_token (int tok)
{
  if (Vdisplay_tokens)
    display_token (tok);

  if (lexer_debug_flag)
    {
      std::cerr << "R: ";
      display_token (tok);
      std::cerr << std::endl; 
    }

  return tok;
}
