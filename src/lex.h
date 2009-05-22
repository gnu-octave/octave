/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2002,
              2003, 2004, 2005, 2006, 2007, 2008, 2009 John W. Eaton

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

#if !defined (octave_lex_h)
#define octave_lex_h 1

#include <list>

// FIXME -- these input buffer things should be members of a
// parser input stream class.

typedef struct yy_buffer_state *YY_BUFFER_STATE;

// Associate a buffer with a new file to read.
extern OCTINTERP_API YY_BUFFER_STATE create_buffer (FILE *f);

// Report the current buffer.
extern OCTINTERP_API YY_BUFFER_STATE current_buffer (void);

// Connect to new buffer buffer.
extern OCTINTERP_API void switch_to_buffer (YY_BUFFER_STATE buf);

// Delete a buffer.
extern OCTINTERP_API void delete_buffer (YY_BUFFER_STATE buf);

// Restore a buffer (for unwind-prot).
extern OCTINTERP_API void restore_input_buffer (void *buf);

// Delete a buffer (for unwind-prot).
extern OCTINTERP_API void delete_input_buffer (void *buf);

// Is the given string a keyword?
extern bool is_keyword (const std::string& s);

extern void prep_lexer_for_script (void);

// For communication between the lexer and parser.

class
lexical_feedback
{
public:

  lexical_feedback (void) { init (); }

  ~lexical_feedback (void) { }

  void init (void);

  // Square bracket level count.
  int bracketflag;

  // Curly brace level count.
  int braceflag;

  // TRUE means we're in the middle of defining a loop.
  int looping;

  // TRUE means that we should convert spaces to a comma inside a
  // matrix definition.
  bool convert_spaces_to_comma;

  // TRUE means we are at the beginning of a statement, where a
  // command name is possible.
  bool at_beginning_of_statement;

  // TRUE means we're in the middle of defining a function.
  bool defining_func;

  // Nonzero means we are parsing a function handle.
  int looking_at_function_handle;

  // TRUE means we're parsing the return list for a function.
  bool looking_at_return_list;

  // TRUE means we're parsing the parameter list for a function.
  bool looking_at_parameter_list;

  // TRUE means we're parsing a declaration list (global or
  // persistent).
  bool looking_at_decl_list;

  // TRUE means we are looking at the initializer expression for a
  // parameter list element.
  bool looking_at_initializer_expression;

  // TRUE means we're parsing a matrix or the left hand side of
  // multi-value assignment statement.
  bool looking_at_matrix_or_assign_lhs;

  // If the front of the list is TRUE, the closest paren, brace, or
  // bracket nesting is an index for an object.
  std::list<bool> looking_at_object_index;

  // Object index not possible until we've seen something.
  bool looking_for_object_index;

  // GAG.  Stupid kludge so that [[1,2][3,4]] will work.
  bool do_comma_insert;

  // TRUE means we're looking at an indirect reference to a
  // structure element.
  bool looking_at_indirect_ref;

  // TRUE means that we've already seen the name of this function.
  // Should only matter if defining_func is also TRUE.
  bool parsed_function_name;

  // Are we parsing a nested function?
  //   1 ==> Yes.
  //   0 ==> No.
  //  -1 ==> Yes, but it is the last one because we have seen EOF.
  int parsing_nested_function;

  // TRUE means we are parsing a class method.
  bool parsing_class_method;

  // Return transpose or start a string?
  bool quote_is_transpose;

  // Set of identifiers that might be local variable names.
  std::set<std::string> pending_local_variables;

private:

  lexical_feedback (const lexical_feedback&);

  lexical_feedback& operator = (const lexical_feedback&);
};

class
stream_reader
{
public:
  virtual int getc (void) = 0;
  virtual int ungetc (int c) = 0;

protected:
  stream_reader (void) { }
  ~stream_reader (void) { }

private:

  // No copying!
  stream_reader (const stream_reader&);
  stream_reader& operator = (const stream_reader&);
};

extern std::string
grab_comment_block (stream_reader& reader, bool at_bol, bool& eof);

// TRUE means that we have encountered EOF on the input stream.
extern bool parser_end_of_input;

// Flags that need to be shared between the lexer and parser.
extern lexical_feedback lexer_flags;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
