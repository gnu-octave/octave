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

#if !defined (octave_lex_h)
#define octave_lex_h 1

#include <list>
#include <set>
#include <stack>

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

extern OCTINTERP_API void clear_all_buffers (void);

extern OCTINTERP_API void cleanup_parser (void);

// Is the given string a keyword?
extern bool is_keyword (const std::string& s);

extern void prep_lexer_for_script_file (void);
extern void prep_lexer_for_function_file (void);

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

// Forward decl for lexical_feedback::token_stack.
class token;

// For communication between the lexer and parser.

class
lexical_feedback
{
public:

  // Did eat_whitespace or eat_continuation eat a space or tab, or a
  // newline, or both?
  //
  // Functions that return this type will return a logical OR of the
  // following values:
  //
  //  NO_WHITESPACE  no spaces to eat
  //  SPACE_OR_TAB   space or tab in input
  //  NEWLINE        bare new line in input

  enum whitespace_type
    {
      NO_WHITESPACE = 1,
      SPACE_OR_TAB = 2,
      NEWLINE = 4
    };


  // Track nesting of square brackets, curly braces, and parentheses.

  class bbp_nesting_level
  {
  private:

    enum bracket_type
      {
        BRACKET = 1,
        BRACE = 2,
        PAREN = 3
      };

  public:

    bbp_nesting_level (void) : context () { }

    bbp_nesting_level (const bbp_nesting_level& nl) : context (nl.context) { }

    bbp_nesting_level& operator = (const bbp_nesting_level& nl)
    {
      if (&nl != this)
        context = nl.context;

      return *this;
    }

    ~bbp_nesting_level (void) { }

    void bracket (void) { context.push (BRACKET); }

    bool is_bracket (void)
    {
      return ! context.empty () && context.top () == BRACKET;
    }

    void brace (void) { context.push (BRACE); }

    bool is_brace (void)
    {
      return ! context.empty () && context.top () == BRACE;
    }

    void paren (void) { context.push (PAREN); }

    bool is_paren (void)
    {
      return ! context.empty () && context.top () == PAREN;
    }

    bool is_bracket_or_brace (void)
    {
      return (! context.empty ()
              && (context.top () == BRACKET || context.top () == BRACE));
    }

    bool none (void) { return context.empty (); }

    void remove (void)
    {
      if (! context.empty ())
        context.pop ();
    }

    void clear (void)
    {
      while (! context.empty ())
        context.pop ();
    }

  private:

    std::stack<int> context;
  };

  lexical_feedback (void)
    : convert_spaces_to_comma (true), do_comma_insert (false),
      at_beginning_of_statement (true),
      looking_at_anon_fcn_args (false), looking_at_return_list (false),
      looking_at_parameter_list (false), looking_at_decl_list (false),
      looking_at_initializer_expression (false),
      looking_at_matrix_or_assign_lhs (false),
      looking_for_object_index (false), 
      looking_at_indirect_ref (false), parsing_class_method (false),
      maybe_classdef_get_set_method (false), parsing_classdef (false),
      quote_is_transpose (false), parser_end_of_input (false),
      input_line_number (1), current_input_column (1),
      bracketflag (0), braceflag (0),
      looping (0), defining_func (0), looking_at_function_handle (0),
      block_comment_nesting_level (0),
      looking_at_object_index (), parsed_function_name (),
      pending_local_variables (), nesting_level ()
  {
    init ();
  }

  ~lexical_feedback (void);

  void init (void)
  {
    // The closest paren, brace, or bracket nesting is not an object
    // index.
    looking_at_object_index.push_front (false);
  }

  void do_comma_insert_check (void);

  int text_yyinput (void);

  void xunput (char c, char *buf);

  void fixup_column_count (char *s);

  bool inside_any_object_index (void);

  int is_keyword_token (const std::string& s);

  bool is_variable (const std::string& name);

  std::string grab_block_comment (stream_reader& reader, bool& eof);

  std::string grab_comment_block (stream_reader& reader, bool at_bol,
                                  bool& eof);

  int process_comment (bool start_in_block, bool& eof);

  bool next_token_is_sep_op (void);

  bool next_token_is_postfix_unary_op (bool spc_prev);

  bool next_token_is_bin_op (bool spc_prev);

  void scan_for_comments (const char *text);

  int eat_whitespace (void);

  void handle_number (void);

  bool have_continuation (bool trailing_comments_ok = true);

  bool have_ellipsis_continuation (bool trailing_comments_ok = true);

  int eat_continuation (void);

  int handle_string (char delim);

  bool next_token_is_assign_op (void);

  bool next_token_is_index_op (void);

  int handle_close_bracket (bool spc_gobbled, int bracket_type);

  void maybe_unput_comma (int spc_gobbled);

  bool next_token_can_follow_bin_op (void);

  bool looks_like_command_arg (void);

  int handle_superclass_identifier (void);

  int handle_meta_identifier (void);

  int handle_identifier (void);

  void maybe_warn_separator_insert (char sep);

  void gripe_single_quote_string (void);

  void gripe_matlab_incompatible (const std::string& msg);

  void maybe_gripe_matlab_incompatible_comment (char c);

  void gripe_matlab_incompatible_continuation (void);

  void gripe_matlab_incompatible_operator (const std::string& op);

  void display_token (int tok);

  void lexer_debug (const char *pattern, const char *text);

  // TRUE means that we should convert spaces to a comma inside a
  // matrix definition.
  bool convert_spaces_to_comma;

  // GAG.  Stupid kludge so that [[1,2][3,4]] will work.
  bool do_comma_insert;

  // TRUE means we are at the beginning of a statement, where a
  // command name is possible.
  bool at_beginning_of_statement;

  // TRUE means we are parsing an anonymous function argument list.
  bool looking_at_anon_fcn_args;

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

  // Object index not possible until we've seen something.
  bool looking_for_object_index;

  // TRUE means we're looking at an indirect reference to a
  // structure element.
  bool looking_at_indirect_ref;

  // TRUE means we are parsing a class method in function or classdef file.
  bool parsing_class_method;

  // TRUE means we are parsing a class method declaration line in a
  // classdef file and can accept a property get or set method name.
  // For example, "get.PropertyName" is recognized as a function name.
  bool maybe_classdef_get_set_method;

  // TRUE means we are parsing a classdef file
  bool parsing_classdef;

  // Return transpose or start a string?
  bool quote_is_transpose;

  // TRUE means that we have encountered EOF on the input stream.
  bool parser_end_of_input;

  // The current input line number.
  int input_line_number;

  // The column of the current token.
  int current_input_column;

  // Square bracket level count.
  int bracketflag;

  // Curly brace level count.
  int braceflag;

  // TRUE means we're in the middle of defining a loop.
  int looping;

  // Nonzero means we're in the middle of defining a function.
  int defining_func;

  // Nonzero means we are parsing a function handle.
  int looking_at_function_handle;

  // Nestng level for blcok comments.
  int block_comment_nesting_level;

  // If the front of the list is TRUE, the closest paren, brace, or
  // bracket nesting is an index for an object.
  std::list<bool> looking_at_object_index;

  // If the top of the stack is TRUE, then we've already seen the name
  // of the current function.  Should only matter if
  // current_function_level > 0
  std::stack<bool> parsed_function_name;

  // Set of identifiers that might be local variable names.
  std::set<std::string> pending_local_variables;

  // Is the closest nesting level a square bracket, squiggly brace or
  // a paren?
  bbp_nesting_level nesting_level;

  // Stack to hold tokens so that we can delete them when the parser is
  // reset and avoid growing forever just because we are stashing some
  // information.
  std::stack <token*> token_stack;

  // For unwind protect.
  static void cleanup (lexical_feedback *lexer) { delete lexer; }

private:

  // No copying!

  lexical_feedback (const lexical_feedback&);

  lexical_feedback& operator = (const lexical_feedback&);
};

// The current state of the lexer.
extern lexical_feedback *curr_lexer;

#endif
