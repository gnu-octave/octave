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

#if !defined (octave_lex_h)
#define octave_lex_h 1

// Arrange to get input via readline.

#ifdef YY_INPUT
#undef YY_INPUT
#endif
#define YY_INPUT(buf, result, max_size) \
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
      lexer_flags.quote_is_transpose = false; \
      lexer_flags.cant_be_identifier = false; \
      lexer_flags.convert_spaces_to_comma = true; \
      return (tok); \
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
      lexer_flags.cant_be_identifier = true; \
      lexer_flags.convert_spaces_to_comma = convert; \
      return (tok); \
    } \
  while (0)

// XXX FIXME XXX -- these input buffer things should be members of an
// parser input stream class.

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
// This needs to be defined in lex.l so that it can use yyinput() but
// it must be called from parse.y.
extern void check_for_garbage_after_fcn_def (void);

// For communication between the lexer and parser.

class lexical_feedback
{
public:

  lexical_feedback (void) { init (); }

  ~lexical_feedback (void) { }

  void init (void);

  // Brace level count.
  int braceflag;

  // TRUE means we're in the middle of defining a loop.
  int looping;

  // TRUE means we think we are looking at the beginning of a
  // function definition.
  bool beginning_of_function;

  // Another context hack, this time for the plot command's `using',
  // `title', and `with' keywords.
  bool cant_be_identifier;

  // TRUE means that we should convert spaces to a comma inside a
  // matrix definition.
  bool convert_spaces_to_comma;

  // TRUE means we're in the middle of defining a function.
  bool defining_func;

  // TRUE means we're parsing the return list for a function.
  bool looking_at_return_list;

  // TRUE means we're parsing the parameter list for a function.
  bool looking_at_parameter_list;

  // TRUE means we're parsing a matrix or the left hand side of
  // multi-value assignment statement.
  bool looking_at_matrix_or_assign_lhs;

  // GAG.  Stupid kludge so that [[1,2][3,4]] will work.
  bool do_comma_insert;

  // TRUE means we think we are looking at a set command.
  bool doing_set;

  // TRUE means we're looking at the range part of a plot command.
  bool in_plot_range;

  // TRUE means we're looking at the using part of a plot command.
  bool in_plot_using;

  // TRUE means we're looking at the style part of a plot command.
  bool in_plot_style;

  // TRUE means we're looking at the axes part of a plot command.
  bool in_plot_axes;

  // TRUE means we're looking at an indirect reference to a
  // structure element.
  bool looking_at_indirect_ref;

  // TRUE means that we've already seen the name of this function.
  // Should only matter if defining_func is also TRUE.
  bool parsed_function_name;

  // TRUE means we've seen something that means we must be past the
  // range part of a plot command.
  bool past_plot_range;

  // TRUE means we're working on a plot command.
  bool plotting;

  // Return transpose or start a string?
  bool quote_is_transpose;

private:

  lexical_feedback (const lexical_feedback&);

  lexical_feedback& operator = (const lexical_feedback&);
};

// Flags that need to be shared between the lexer and parser.
extern lexical_feedback lexer_flags;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
