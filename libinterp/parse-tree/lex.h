////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_lex_h)
#define octave_lex_h 1

#include "octave-config.h"

#include <deque>
#include <list>
#include <set>
#include <stack>

#include "comment-list.h"
#include "filepos.h"
#include "input.h"
#include "symscope.h"
#include "token.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class interpreter;

// Is the given string a keyword?
extern bool iskeyword (const std::string& s);

// For communication between the lexer and parser.

class
lexical_feedback
{
public:

  // Track symbol table information when parsing functions.

  class symbol_table_context
  {
  public:

    symbol_table_context (interpreter& interp)
      : m_interpreter (interp), m_frame_stack () { }

    ~symbol_table_context (void) { clear (); }

    void clear (void);

    bool empty (void) const { return m_frame_stack.empty (); }

    std::size_t size (void) const { return m_frame_stack.size (); }

    void pop (void);

    void push (const symbol_scope& scope)
    {
      m_frame_stack.push_front (scope);
    }

    symbol_scope curr_scope (void) const;
    symbol_scope parent_scope (void) const;

  private:

    interpreter& m_interpreter;

    std::deque<symbol_scope> m_frame_stack;
  };

  // Track nesting of square brackets, curly braces, and parentheses.

  class bbp_nesting_level
  {
  private:

    enum bracket_type
    {
      BRACKET = 1,
      BRACE = 2,
      PAREN = 3,
      ANON_FCN_BODY = 4
    };

  public:

    bbp_nesting_level (void) : m_context () { }

    bbp_nesting_level (const bbp_nesting_level& nl)
      : m_context (nl.m_context)
    { }

    bbp_nesting_level& operator = (const bbp_nesting_level& nl)
    {
      if (&nl != this)
        m_context = nl.m_context;

      return *this;
    }

    ~bbp_nesting_level (void) = default;

    // Alias for clear function.
    void reset (void) { clear (); }

    void bracket (void) { m_context.push (BRACKET); }

    bool is_bracket (void)
    {
      return ! m_context.empty () && m_context.top () == BRACKET;
    }

    void brace (void) { m_context.push (BRACE); }

    bool is_brace (void)
    {
      return ! m_context.empty () && m_context.top () == BRACE;
    }

    void paren (void) { m_context.push (PAREN); }

    bool is_paren (void)
    {
      return ! m_context.empty () && m_context.top () == PAREN;
    }

    void anon_fcn_body (void) { m_context.push (ANON_FCN_BODY); }

    bool is_anon_fcn_body (void)
    {
      return ! m_context.empty () && m_context.top () == ANON_FCN_BODY;
    }

    bool is_bracket_or_brace (void)
    {
      return (! m_context.empty ()
              && (m_context.top () == BRACKET || m_context.top () == BRACE));
    }

    bool none (void) { return m_context.empty (); }

    void remove (void)
    {
      if (! m_context.empty ())
        m_context.pop ();
    }

    void clear (void)
    {
      while (! m_context.empty ())
        m_context.pop ();
    }

  private:

    std::stack<int> m_context;
  };

  class token_cache
  {
  public:

    // Store an "unlimited" number of tokens.

    // Tokens are allocated with new.  Delete them when they are
    // removed from the cache.
    //
    // One of the reasons for using this class instead of std::deque
    // directly is that we can ensure that memory is cleaned up
    // properly.  It's more tedious to do that with deque since the
    // deque destructor and clear method don't call delete on the
    // elements that it stores.  Another reason is that it makes it
    // easier to change the implementation later if needed.

    token_cache (void) : m_buffer () { }

    // No copying!

    token_cache (const token_cache&) = delete;

    token_cache& operator = (const token_cache&) = delete;

    ~token_cache (void) { clear (); }

    void push (token *tok)
    {
      m_buffer.push_front (tok);
    }

    void pop (void)
    {
      if (! empty ())
        {
          delete m_buffer.back ();
          m_buffer.pop_back ();
        }
    }

    // Direct access.
    token * at (std::size_t n)
    {
      return empty () ? nullptr : m_buffer.at (n);
    }

    const token * at (std::size_t n) const
    {
      return empty () ? nullptr : m_buffer.at (n);
    }

    // Most recently pushed.
    token * front (void)
    {
      return empty () ? nullptr : m_buffer.front ();
    }

    const token * front (void) const
    {
      return empty () ? nullptr : m_buffer.front ();
    }

    token * back (void)
    {
      return empty () ? nullptr : m_buffer.back ();
    }

    const token * back (void) const
    {
      return empty () ? nullptr : m_buffer.back ();
    }

    // Number of elements currently in the buffer.
    std::size_t size (void) const { return m_buffer.size (); }

    bool empty (void) const { return m_buffer.empty (); }

    void clear (void)
    {
      while (! empty ())
        pop ();
    }

  private:

    std::deque<token *> m_buffer;
  };

  lexical_feedback (interpreter& interp)
    : m_interpreter (interp),
      m_end_of_input (false),
      m_allow_command_syntax (true),
      m_at_beginning_of_statement (true),
      m_looking_at_anon_fcn_args (false),
      m_looking_at_return_list (false),
      m_looking_at_parameter_list (false),
      m_looking_at_decl_list (false),
      m_looking_at_matrix_or_assign_lhs (false),
      m_looking_for_object_index (false),
      m_looking_at_indirect_ref (false),
      m_arguments_is_keyword (false),
      m_classdef_element_names_are_keywords (false),
      m_parsing_anon_fcn_body (false),
      m_parsing_class_method (false),
      m_parsing_classdef (false),
      m_parsing_classdef_decl (false),
      m_parsing_classdef_superclass (false),
      m_maybe_classdef_get_set_method (false),
      m_parsing_classdef_get_method (false),
      m_parsing_classdef_set_method (false),
      m_quote_is_transpose (false),
      m_force_script (false),
      m_reading_fcn_file (false),
      m_reading_script_file (false),
      m_reading_classdef_file (false),
      m_buffer_function_text (false),
      m_bracketflag (0),
      m_braceflag (0),
      m_looping (0),
      m_defining_fcn (0),
      m_looking_at_function_handle (0),
      m_block_comment_nesting_level (0),
      m_command_arg_paren_count (0),
      m_token_count (0),
      m_filepos (1, 1),
      m_tok_beg (),
      m_tok_end (),
      m_string_text (),
      m_current_input_line (),
      m_comment_text (),
      m_help_text (),
      m_function_text (),
      m_fcn_file_name (),
      m_fcn_file_full_name (),
      m_dir_name (),
      m_package_name (),
      m_looking_at_object_index (),
      m_parsed_function_name (),
      m_symtab_context (interp),
      m_nesting_level (),
      m_tokens ()
  {
    init ();
  }

  // No copying!

  lexical_feedback (const lexical_feedback&) = delete;

  lexical_feedback& operator = (const lexical_feedback&) = delete;

  ~lexical_feedback (void);

  void init (void);

  void reset (void);

  int previous_token_value (void) const;

  bool previous_token_value_is (int tok_val) const;

  void mark_previous_token_trailing_space (void);

  bool space_follows_previous_token (void) const;

  bool previous_token_is_binop (void) const;

  bool previous_token_is_keyword (void) const;

  bool previous_token_may_be_command (void) const;

  void mark_as_variable (const std::string& nm);
  void mark_as_variables (const std::list<std::string>& lst);

  interpreter& m_interpreter;

  // true means that we have encountered eof on the input stream.
  bool m_end_of_input;

  // true means command syntax is allowed.
  bool m_allow_command_syntax;

  // true means we are at the beginning of a statement, where a
  // command name is possible.
  bool m_at_beginning_of_statement;

  // true means we are parsing an anonymous function argument list.
  bool m_looking_at_anon_fcn_args;

  // true means we're parsing the return list for a function.
  bool m_looking_at_return_list;

  // true means we're parsing the parameter list for a function.
  bool m_looking_at_parameter_list;

  // true means we're parsing a declaration list (global or
  // persistent).
  bool m_looking_at_decl_list;

  // true means we're parsing a matrix or the left hand side of
  // multi-value assignment statement.
  bool m_looking_at_matrix_or_assign_lhs;

  // object index not possible until we've seen something.
  bool m_looking_for_object_index;

  // true means we're looking at an indirect reference to a
  // structure element.
  bool m_looking_at_indirect_ref;

  // true means arguments is handled as keyword.
  bool m_arguments_is_keyword;

  // true means "properties", "methods", "events", and "enumeration"
  // are treated like keywords.
  bool m_classdef_element_names_are_keywords;

  // true means we are parsing the body of an anonymous function.
  bool m_parsing_anon_fcn_body;

  // true means we are parsing a class method in function or classdef file.
  bool m_parsing_class_method;

  // true means we are parsing a classdef file
  bool m_parsing_classdef;

  // true means we are parsing the initial classdef declaration
  // portion of classdef file, from the "classdef" token through the
  // optional list of superclasses.
  bool m_parsing_classdef_decl;

  // true means we are parsing the superclass part of a classdef
  // declaration.
  bool m_parsing_classdef_superclass;

  // true means we are parsing a class method declaration line in a
  // classdef file and can accept a property get or set method name.
  // for example, "get.propertyname" is recognized as a function name.
  bool m_maybe_classdef_get_set_method;

  // TRUE means we are parsing a classdef get.method.
  bool m_parsing_classdef_get_method;

  // TRUE means we are parsing a classdef set.method.
  bool m_parsing_classdef_set_method;

  // return transpose or start a string?
  bool m_quote_is_transpose;

  // TRUE means treat the current file as a script even if the first
  // token is "function" or "classdef".
  bool m_force_script;

  // TRUE means we're parsing a function file.
  bool m_reading_fcn_file;

  // TRUE means we're parsing a script file.
  bool m_reading_script_file;

  // TRUE means we're parsing a classdef file.
  bool m_reading_classdef_file;

  // TRUE means we should store the text of the function we are
  // parsing.
  bool m_buffer_function_text;

  // square bracket level count.
  int m_bracketflag;

  // curly brace level count.
  int m_braceflag;

  // true means we're in the middle of defining a loop.
  int m_looping;

  // nonzero means we're in the middle of defining a function.
  int m_defining_fcn;

  // nonzero means we are parsing a function handle.
  int m_looking_at_function_handle;

  // nestng level for block comments.
  int m_block_comment_nesting_level;

  // Parenthesis count for command argument parsing.
  int m_command_arg_paren_count;

  // Count of tokens recognized by this lexer since initialized or
  // since the last reset.
  std::size_t m_token_count;

  // The current position in the file (line and column).
  filepos m_filepos;

  // The positions of the beginning and end of the current token after
  // calling update_token_positions.  Also used apart from
  // update_token_positions to handle the beginning and end of
  // character strings.
  filepos m_tok_beg;
  filepos m_tok_end;

  // The current character string text.
  std::string m_string_text;

  // The current line of input.
  std::string m_current_input_line;

  // The current comment text.
  std::string m_comment_text;

  // The current help text.
  std::string m_help_text;

  // The text of functions entered on the command line.
  std::string m_function_text;

  // Simple name of function file we are reading.
  std::string m_fcn_file_name;

  // Full name of file we are reading.
  std::string m_fcn_file_full_name;

  // Directory name where this file was found.  May be relative.
  std::string m_dir_name;

  // Name of +package containing this file, if any.
  std::string m_package_name;

  // if the front of the list is true, the closest paren, brace, or
  // bracket nesting is an index for an object.
  std::list<bool> m_looking_at_object_index;

  // if the top of the stack is true, then we've already seen the name
  // of the current function.  should only matter if
  // current_function_level > 0
  std::stack<bool> m_parsed_function_name;

  // Track current symbol table scope and context.
  symbol_table_context m_symtab_context;

  // is the closest nesting level a square bracket, squiggly brace,
  // a paren, or an anonymous function body?
  bbp_nesting_level m_nesting_level;

  // Tokens generated by the lexer.
  token_cache m_tokens;
};

// base_lexer inherits from lexical_feedback because we will
// eventually have several different constructors and it is easier to
// initialize if everything is grouped in a parent class rather than
// listing all the members in the base_lexer class.

class
base_lexer : public lexical_feedback
{
public:

  // Handle buffering of input for lexer.

  class input_buffer
  {
  public:

    input_buffer (void)
      : m_buffer (), m_offset (0), m_chars_left (0), m_eof (false)
    { }

    void fill (const std::string& input, bool eof_arg);

    // Copy at most max_size characters to buf.
    int copy_chunk (char *buf, std::size_t max_size, bool by_lines = false);

    bool empty (void) const { return m_chars_left == 0; }

    bool at_eof (void) const { return m_eof; }

  private:

    std::string m_buffer;
    std::size_t m_offset;
    std::size_t m_chars_left;
    bool m_eof;
  };

  // Collect comment text.

  class
  comment_buffer
  {
  public:

    comment_buffer (void) : m_comment_list (nullptr) { }

    ~comment_buffer (void) { delete m_comment_list; }

    void append (const std::string& s, comment_elt::comment_type t)
    {
      if (! m_comment_list)
        m_comment_list = new comment_list ();

      m_comment_list->append (s, t);
    }

    // Caller is expected to delete the returned value.

    comment_list * get_comment (void)
    {
      comment_list *retval = m_comment_list;

      m_comment_list = nullptr;

      return retval;
    }

    void reset (void)
    {
      delete m_comment_list;

      m_comment_list = nullptr;
    }

  private:

    comment_list *m_comment_list;
  };

  base_lexer (interpreter& interp)
    : lexical_feedback (interp), m_scanner (nullptr), m_input_buf (),
      m_comment_buf ()
  {
    init ();
  }

  // No copying!

  base_lexer (const base_lexer&) = delete;

  base_lexer& operator = (const base_lexer&) = delete;

  virtual ~base_lexer (void);

  void init (void);

  virtual bool is_push_lexer (void) const { return false; }

  virtual void reset (void);

  void prep_for_file (void);

  void begin_string (int state);

  virtual int fill_flex_buffer (char *buf, unsigned int max_size) = 0;

  bool at_end_of_buffer (void) const { return m_input_buf.empty (); }

  bool at_end_of_file (void) const { return m_input_buf.at_eof (); }

  int handle_end_of_input (void);

  char * flex_yytext (void);

  int flex_yyleng (void);

  int text_yyinput (void);

  void xunput (char c, char *buf);

  void xunput (char c);

  void update_token_positions (int tok_len);

  bool looking_at_space (void);

  bool inside_any_object_index (void);

  int make_keyword_token (const std::string& s);

  bool fq_identifier_contains_keyword (const std::string& s);

  bool whitespace_is_significant (void);

  // We only provide specializations with base equal to 2, 10, or 16.
  template <int base>
  int handle_number (void);

  void handle_continuation (void);

  void finish_comment (comment_elt::comment_type typ);

  comment_list * get_comment (void) { return m_comment_buf.get_comment (); }

  int handle_close_bracket (int bracket_type);

  bool looks_like_command_arg (void);

  int handle_superclass_identifier (void);

  int handle_meta_identifier (void);

  int handle_fq_identifier (void);

  int handle_identifier (void);

  void maybe_warn_separator_insert (char sep);

  void warn_language_extension (const std::string& msg);

  void maybe_warn_language_extension_comment (char c);

  void warn_language_extension_continuation (void);

  void warn_language_extension_operator (const std::string& op);

  void warn_deprecated_syntax (const std::string& msg);

  void warn_deprecated_operator (const std::string& deprecated_op,
                                 const std::string& recommended_op,
                                 const std::string& version);

  void push_token (token *);

  token * current_token (void);

  std::size_t pending_token_count (void) const;

  void display_token (int tok);

  void fatal_error (const char *msg);

  bool debug_flag (void) const;

  bool display_tokens (void) const;

  void increment_token_count (void);

  void lexer_debug (const char *pattern);

  // Internal state of the flex-generated lexer.
  void *m_scanner;

  // Object that reads and buffers input.
  input_buffer m_input_buf;

  // Object that collects comment text.
  comment_buffer m_comment_buf;

  virtual std::string input_source (void) const { return "unknown"; }

  virtual bool input_from_terminal (void) const { return false; }

  virtual bool input_from_file (void) const { return false; }

  virtual bool input_from_eval_string (void) const { return false; }

  bool input_from_tmp_history_file (void);

  void push_start_state (int state);

  void pop_start_state (void);

  void clear_start_state (void);

  int start_state (void) const { return start_state_stack.top (); }

  void display_start_state (void) const;

  bool maybe_unput_comma_before_unary_op (int tok);

  int handle_op (int tok, bool bos = false, bool compat = true);

  int finish_command_arg (void);

  int handle_token (int tok, token *tok_val = nullptr);

  int count_token (int tok);

  int count_token_internal (int tok);

  int show_token (int tok);

protected:

  std::stack<int> start_state_stack;
};

class
lexer : public base_lexer
{
public:

  lexer (interpreter& interp)
    : base_lexer (interp), m_reader (interp), m_initial_input (true)
  { }

  lexer (FILE *file, interpreter& interp)
    : base_lexer (interp), m_reader (interp, file), m_initial_input (true)
  { }

  lexer (FILE *file, interpreter& interp, const std::string& encoding)
    : base_lexer (interp), m_reader (interp, file, encoding),
      m_initial_input (true)
  { }

  lexer (const std::string& eval_string, interpreter& interp)
    : base_lexer (interp), m_reader (interp, eval_string),
      m_initial_input (true)
  { }

  // No copying!

  lexer (const lexer&) = delete;

  lexer& operator = (const lexer&) = delete;

  void reset (void)
  {
    m_initial_input = true;

    base_lexer::reset ();
  }

  std::string input_source (void) const
  {
    return m_reader.input_source ();
  }

  bool input_from_terminal (void) const
  {
    return m_reader.input_from_terminal ();
  }

  bool input_from_file (void) const
  {
    return m_reader.input_from_file ();
  }

  bool input_from_eval_string (void) const
  {
    return m_reader.input_from_eval_string ();
  }

  int fill_flex_buffer (char *buf, unsigned int max_size);

  input_reader m_reader;

  // TRUE means we are filling the input buffer for the first time.
  // Otherwise, we are requesting more input to complete the parse
  // and, if printing a prompt, should use the secondary prompt
  // string.

  bool m_initial_input;
};

template <> int base_lexer::handle_number<2> ();
template <> int base_lexer::handle_number<10> ();
template <> int base_lexer::handle_number<16> ();

class
push_lexer : public base_lexer
{
public:

  push_lexer (interpreter& interp)
    : base_lexer (interp)
  {
    append_input ("", false);
  }

  push_lexer (const std::string& input, interpreter& interp)
    : base_lexer (interp)
  {
    append_input (input, false);
  }

  push_lexer (bool eof, interpreter& interp)
    : base_lexer (interp)
  {
    append_input ("", eof);
  }

  push_lexer (const std::string& input, bool eof, interpreter& interp)
    : base_lexer (interp)
  {
    append_input (input, eof);
  }

  // No copying!

  push_lexer (const push_lexer&) = delete;

  push_lexer& operator = (const push_lexer&) = delete;

  bool is_push_lexer (void) const { return true; }

  void append_input (const std::string& input, bool eof);

  std::string input_source (void) const { return "push buffer"; }

  int fill_flex_buffer (char *buf, unsigned int max_size);
};

OCTAVE_END_NAMESPACE(octave)

#endif
