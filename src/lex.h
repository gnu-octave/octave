// lex.h                                                 -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#if !defined (octave_lex_h)
#define octave_lex_h 1

// Arrange to get input via readline.

#ifdef YY_INPUT
#undef YY_INPUT
#endif
#define YY_INPUT(buf,result,max_size) \
  if ((result = octave_read (buf, max_size)) < 0) \
    YY_FATAL_ERROR ("octave_read () in flex scanner failed");

// Try to avoid crashing out completely on fatal scanner errors.

#ifdef YY_FATAL_ERROR
#undef YY_FATAL_ERROR
#endif
#define YY_FATAL_ERROR(msg) \
  do \
    { \
      error (msg); \
      jump_to_top_level (); \
    } \
  while (0)

#define TOK_RETURN(tok) \
  do \
    { \
      current_input_column += yyleng; \
      quote_is_transpose = 0; \
      cant_be_identifier = 0; \
      convert_spaces_to_comma = 1; \
      return (tok); \
    } \
  while (0)

#define TOK_PUSH_AND_RETURN(name,tok) \
  do \
    { \
      yylval.tok_val = new token (name, input_line_number, \
				  current_input_column); \
      token_stack.push (yylval.tok_val); \
      TOK_RETURN (tok); \
    } \
  while (0)

#define BIN_OP_RETURN(tok,convert) \
  do \
    { \
      yylval.tok_val = new token (input_line_number, current_input_column); \
      token_stack.push (yylval.tok_val); \
      current_input_column += yyleng; \
      quote_is_transpose = 0; \
      cant_be_identifier = 0; \
      convert_spaces_to_comma = convert; \
      return (tok); \
    } \
  while (0)

typedef struct yy_buffer_state *YY_BUFFER_STATE;

// Associate a buffer with a new file to read.
extern YY_BUFFER_STATE create_buffer (FILE *f);

// Report the current buffer.
extern YY_BUFFER_STATE current_buffer (void);

// Connect to new buffer buffer.
extern void switch_to_buffer (YY_BUFFER_STATE buf);

// Delete a buffer.
extern void delete_buffer (YY_BUFFER_STATE buf);

// Restore a buffer (for unwind-prot).
extern void restore_input_buffer (void *buf);

// Delete a buffer (for unwind-prot).
extern void delete_input_buffer (void *buf);

// See if a function file has extra garbage after the end statement.
extern void check_for_garbage_after_fcn_def (void);

// Return transpose or start a string?
extern int quote_is_transpose;

// Nonzero means we thing we are looking at the beginning of a
// function definition.
extern int beginning_of_function;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
