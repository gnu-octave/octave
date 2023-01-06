////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdlib>
#include <cstring>

#include <string>

#include "cmd-edit.h"
#include "cmd-hist.h"
#include "file-ops.h"
#include "file-stat.h"
#include "lo-error.h"
#include "lo-utils.h"
#include "oct-env.h"
#include "oct-mutex.h"
#include "oct-time.h"
#include "quit.h"
#include "singleton-cleanup.h"
#include "strdup-wrapper.h"
#include "unistd-wrappers.h"

#if defined (USE_READLINE)
#include <cstdio>

#include "oct-rl-edit.h"
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

char * do_completer_word_break_hook ();

command_editor *command_editor::s_instance = nullptr;

std::set<command_editor::startup_hook_fcn> command_editor::m_startup_hook_set;

std::set<command_editor::pre_input_hook_fcn> command_editor::m_pre_input_hook_set;

std::set<command_editor::event_hook_fcn> command_editor::m_event_hook_set;

static mutex event_hook_lock;

#if defined (USE_READLINE)

class
gnu_readline : public command_editor
{
public:

  typedef command_editor::startup_hook_fcn startup_hook_fcn;

  typedef command_editor::pre_input_hook_fcn pre_input_hook_fcn;

  typedef command_editor::event_hook_fcn event_hook_fcn;

  typedef command_editor::completion_fcn completion_fcn;

  gnu_readline (void);

  ~gnu_readline (void) = default;

  void do_set_name (const std::string& n);

  std::string do_readline (const std::string& prompt, bool& eof);

  void do_set_input_stream (FILE *f);

  FILE * do_get_input_stream (void);

  void do_set_output_stream (FILE *f);

  FILE * do_get_output_stream (void);

  void do_redisplay (void);

  int do_terminal_rows (void);

  int do_terminal_cols (void);

  void do_clear_screen (bool skip_redisplay);

  void do_resize_terminal (void);

  void do_set_screen_size (int ht, int wd);

  std::string newline_chars (void);

  void do_restore_terminal_state (void);

  void do_blink_matching_paren (bool flag);

  bool do_erase_empty_line (bool flag);

  void do_set_basic_word_break_characters (const std::string& s);

  void do_set_completer_word_break_characters (const std::string& s);

  void do_set_basic_quote_characters (const std::string& s);

  void do_set_filename_quote_characters (const std::string& s);

  void do_set_completer_quote_characters (const std::string& s);

  void do_set_completion_append_character (char c);

  void do_set_completion_function (completion_fcn f);

  void do_set_quoting_function (quoting_fcn f);

  void do_set_dequoting_function (dequoting_fcn f);

  void do_set_char_is_quoted_function (char_is_quoted_fcn f);

  void do_set_user_accept_line_function (user_accept_line_fcn f);

  completion_fcn do_get_completion_function (void) const;

  quoting_fcn do_get_quoting_function (void) const;

  dequoting_fcn do_get_dequoting_function (void) const;

  char_is_quoted_fcn do_get_char_is_quoted_function (void) const;

  user_accept_line_fcn do_get_user_accept_line_function (void) const;

  string_vector
  do_generate_filename_completions (const std::string& text);

  std::string do_get_line_buffer (void) const;

  std::string do_get_current_line (void) const;

  char do_get_prev_char (int) const;

  void do_replace_line (const std::string& text, bool clear_undo);

  void do_kill_full_line (void);

  void do_insert_text (const std::string& text);

  void do_newline (void);

  void do_accept_line (void);

  bool do_undo (void);

  void do_clear_undo_list (void);

  void set_startup_hook (startup_hook_fcn f);

  void restore_startup_hook (void);

  void set_pre_input_hook (pre_input_hook_fcn f);

  void restore_pre_input_hook (void);

  void set_event_hook (event_hook_fcn f);

  void restore_event_hook (void);

  void do_restore_event_hook (void);

  void do_read_init_file (const std::string& file);

  void do_re_read_init_file (void);

  bool do_filename_completion_desired (bool);

  bool do_filename_quoting_desired (bool);

  bool do_prefer_env_winsize (bool);

  void do_interrupt (bool);

  void do_handle_interrupt_signal (void);

  static int operate_and_get_next (int, int);

  static int history_search_backward (int, int);

  static int history_search_forward (int, int);

private:

  static char * command_generator (const char *text, int state);

  static char * command_quoter (char *text, int match_type,
                               char *quote_pointer);

  static char * command_dequoter (char *text, int match_type);

  static int command_char_is_quoted (char *text, int index);

  static int command_accept_line (int count, int key);

  static char ** command_completer (const char *text, int start, int end);

  static char * do_completer_word_break_hook ();

  startup_hook_fcn m_previous_startup_hook;

  pre_input_hook_fcn m_previous_pre_input_hook;

  event_hook_fcn m_previous_event_hook;

  completion_fcn m_completion_function;

  quoting_fcn m_quoting_function;

  dequoting_fcn m_dequoting_function;

  char_is_quoted_fcn m_char_is_quoted_function;

  user_accept_line_fcn user_accept_line_function;

  static std::string s_completer_quote_characters;
};

std::string gnu_readline::s_completer_quote_characters = "";

gnu_readline::gnu_readline ()
  : command_editor (), m_previous_startup_hook (nullptr),
    m_previous_pre_input_hook (nullptr),
    m_previous_event_hook (nullptr), m_completion_function (nullptr),
    m_quoting_function (nullptr), m_dequoting_function (nullptr),
    m_char_is_quoted_function (nullptr), user_accept_line_function (nullptr)
{
  // FIXME: need interface to rl_add_defun, rl_initialize, and
  // a function to set rl_terminal_name

  std::string term = sys::env::getenv ("TERM");

  octave_rl_set_terminal_name (term.c_str ());

  octave_rl_initialize ();

  do_blink_matching_paren (true);

  // Bind operate-and-get-next.

  octave_rl_add_defun ("operate-and-get-next",
                       gnu_readline::operate_and_get_next,
                       octave_rl_ctrl ('O'));
}

void
gnu_readline::do_set_name (const std::string& nm)
{
  ::octave_rl_set_name (nm.c_str ());
}

std::string
gnu_readline::do_readline (const std::string& prompt, bool& eof)
{
  std::string retval;

  eof = false;

  const char *p = prompt.c_str ();

  char *line = ::octave_rl_readline (p);

  if (line)
    {
      retval = line;

      free (line);
    }
  else
    eof = true;

  return retval;
}

void
gnu_readline::do_set_input_stream (FILE *f)
{
  ::octave_rl_set_input_stream (f);
}

FILE *
gnu_readline::do_get_input_stream (void)
{
  return ::octave_rl_get_input_stream ();
}

void
gnu_readline::do_set_output_stream (FILE *f)
{
  ::octave_rl_set_output_stream (f);
}

FILE *
gnu_readline::do_get_output_stream (void)
{
  return ::octave_rl_get_output_stream ();
}

void
gnu_readline::do_redisplay (void)
{
  ::octave_rl_redisplay ();
}

// GNU readline handles SIGWINCH, so these values have a good chance
// of being correct even if the window changes size (they may be
// wrong if, for example, the luser changes the window size while the
// pager is running, and the signal is handled by the pager instead of
// us.

int
gnu_readline::do_terminal_rows (void)
{
  int sh = ::octave_rl_screen_height ();

  return sh > 0 ? sh : 24;
}

int
gnu_readline::do_terminal_cols (void)
{
  int sw = ::octave_rl_screen_width ();

  return sw > 0 ? sw : 80;
}

void
gnu_readline::do_clear_screen (bool skip_redisplay)
{
  ::octave_rl_clear_screen (skip_redisplay);
}

void
gnu_readline::do_resize_terminal (void)
{
  ::octave_rl_resize_terminal ();
}

void
gnu_readline::do_set_screen_size (int ht, int wd)
{
  ::octave_rl_set_screen_size (ht, wd);
}

std::string
gnu_readline::newline_chars (void)
{
  return "\r\n";
}

void
gnu_readline::do_restore_terminal_state (void)
{
  ::octave_rl_restore_terminal_state ();
}

void
gnu_readline::do_blink_matching_paren (bool flag)
{
  ::octave_rl_enable_paren_matching (flag ? 1 : 0);
}

bool
gnu_readline::do_erase_empty_line (bool flag)
{
  return ::octave_rl_erase_empty_line (flag ? 1 : 0);
}

void
gnu_readline::do_set_basic_word_break_characters (const std::string& s)
{
  ::octave_rl_set_basic_word_break_characters (s.c_str ());
}

void
gnu_readline::do_set_completer_word_break_characters (const std::string& s)
{
  ::octave_rl_set_completer_word_break_characters (s.c_str ());

  ::octave_rl_set_completion_word_break_hook
  (gnu_readline::do_completer_word_break_hook);

}

void
gnu_readline::do_set_basic_quote_characters (const std::string& s)
{
  ::octave_rl_set_basic_quote_characters (s.c_str ());
}

void
gnu_readline::do_set_filename_quote_characters (const std::string& s)
{
  ::octave_rl_set_filename_quote_characters (s.c_str ());
}

void
gnu_readline::do_set_completer_quote_characters (const std::string& s)
{
  s_completer_quote_characters = s;
}

void
gnu_readline::do_set_completion_append_character (char c)
{
  ::octave_rl_set_completion_append_character (c);
}

void
gnu_readline::do_set_completion_function (completion_fcn f)
{
  m_completion_function = f;

  rl_attempted_completion_fcn_ptr fp
    = (f ? gnu_readline::command_completer : nullptr);

  ::octave_rl_set_completion_function (fp);
}

void
gnu_readline::do_set_quoting_function (quoting_fcn f)
{
  m_quoting_function = f;

  rl_quoting_fcn_ptr fp
    = (f ? gnu_readline::command_quoter : nullptr);

  ::octave_rl_set_quoting_function (fp);
}

void
gnu_readline::do_set_dequoting_function (dequoting_fcn f)
{
  m_dequoting_function = f;

  rl_dequoting_fcn_ptr fp
    = (f ? gnu_readline::command_dequoter : nullptr);

  ::octave_rl_set_dequoting_function (fp);
}

void
gnu_readline::do_set_char_is_quoted_function (char_is_quoted_fcn f)
{
  m_char_is_quoted_function = f;

  rl_char_is_quoted_fcn_ptr fp
    = (f ? gnu_readline::command_char_is_quoted : nullptr);

  ::octave_rl_set_char_is_quoted_function (fp);
}

void
gnu_readline::do_set_user_accept_line_function (user_accept_line_fcn f)
{
  user_accept_line_function = f;

  if (f)
    octave_rl_add_defun ("accept-line", gnu_readline::command_accept_line,
                         ::octave_rl_ctrl ('M'));
  else
    octave_rl_add_defun ("accept-line", ::octave_rl_newline,
                         ::octave_rl_ctrl ('M'));
}

gnu_readline::completion_fcn
gnu_readline::do_get_completion_function (void) const
{
  return m_completion_function;
}

gnu_readline::quoting_fcn
gnu_readline::do_get_quoting_function (void) const
{
  return m_quoting_function;
}

gnu_readline::dequoting_fcn
gnu_readline::do_get_dequoting_function (void) const
{
  return m_dequoting_function;
}

gnu_readline::char_is_quoted_fcn
gnu_readline::do_get_char_is_quoted_function (void) const
{
  return m_char_is_quoted_function;
}

gnu_readline::user_accept_line_fcn
gnu_readline::do_get_user_accept_line_function (void) const
{
  return user_accept_line_function;
}

// True if the last "word" of the string line (delimited by delim) is
// an existing directory.  Used by do_completer_word_break_hook.

static bool
looks_like_filename (const char *line, char delim)
{
  bool retval = false;

  const char *s = strrchr (line, delim);

  if (s)
    {
      // Remove incomplete component.
      const char *f = strrchr (line, sys::file_ops::dir_sep_char ());

      if (f && (s[1] == '~' || f != s))
        {
          // For something like "A /b", f==s; don't assume a file.

          std::string candidate_filename = s+1;

          candidate_filename = candidate_filename.substr (0, f - s);

          // Handles any complete ~<username>, but doesn't expand usernames.

          if (candidate_filename[0] == '~')
            candidate_filename
              = sys::file_ops::tilde_expand (candidate_filename);

          sys::file_stat fs (candidate_filename);

          retval = fs.is_dir ();
        }
    }

  return retval;
}

// Decide whether to interpret partial commands like "abc/def" as a
// filename or division.  Return the set of delimiters appropriate for
// the decision.

char *
gnu_readline::do_completer_word_break_hook ()
{
  static char *dir_sep = octave_strdup_wrapper (R"( '")");

  std::string word;
  std::string line = get_line_buffer ();

  // For now, assume space or quote delimiter for file names.
  const char *l = line.c_str ();

  if (looks_like_filename (l, ' ') || looks_like_filename (l, '\'')
      || looks_like_filename (l, '"'))
    {
      ::octave_rl_set_completer_quote_characters
      (s_completer_quote_characters.c_str ());

      return dir_sep;
    }
  else
    {
      static char *word_break_chars = nullptr;

      ::octave_rl_set_completer_quote_characters ("");

      free (word_break_chars);

      word_break_chars
        = octave_strdup_wrapper (octave_rl_get_completer_word_break_characters ());

      return word_break_chars;
    }
}

string_vector
gnu_readline::do_generate_filename_completions (const std::string& text)
{
  string_vector retval;

  int n = 0;
  int count = 0;

  char *fn = nullptr;

  while (1)
    {
      fn = ::octave_rl_filename_completion_function (text.c_str (), count);

      if (fn)
        {
          if (count == n)
            {
              // Famous last words:  Most large directories will not
              // have more than a few hundred files, so we should not
              // resize too many times even if the growth is linear...
              n += 100;
              retval.resize (n);
            }

          retval[count++] = fn;

          free (fn);
        }
      else
        break;
    }

  retval.resize (count);

  return retval;
}

std::string
gnu_readline::do_get_line_buffer (void) const
{
  return ::octave_rl_line_buffer ();
}

std::string
gnu_readline::do_get_current_line (void) const
{
  std::string retval;
  char *buf = ::octave_rl_copy_line ();
  retval = buf;
  free (buf);
  return retval;
}

// Return the character (offset+1) to the left of the cursor,
// or '\0' if the cursor is at the start of the line.
char
gnu_readline::do_get_prev_char (int offset) const
{
  const char *buf = ::octave_rl_line_buffer ();
  int p = ::octave_rl_point ();

  return p > offset ? buf[p - offset - 1] : '\0';
}

void
gnu_readline::do_replace_line (const std::string& text, bool clear_undo)
{
  ::octave_rl_replace_line (text.c_str (), clear_undo);
}

void
gnu_readline::do_kill_full_line (void)
{
  ::octave_rl_kill_full_line ();
}

void
gnu_readline::do_insert_text (const std::string& text)
{
  ::octave_rl_insert_text (text.c_str ());
}

void
gnu_readline::do_newline (void)
{
  ::octave_rl_newline (1, '\n');
}

void
gnu_readline::do_accept_line (void)
{
  command_accept_line (1, '\n');
}

bool
gnu_readline::do_undo (void)
{
  return ::octave_rl_do_undo ();
}

void
gnu_readline::do_clear_undo_list ()
{
  ::octave_rl_clear_undo_list ();
}

void
gnu_readline::set_startup_hook (startup_hook_fcn f)
{
  m_previous_startup_hook = ::octave_rl_get_startup_hook ();

  if (f != m_previous_startup_hook)
    ::octave_rl_set_startup_hook (f);
}

void
gnu_readline::restore_startup_hook (void)
{
  ::octave_rl_set_startup_hook (m_previous_startup_hook);
}

void
gnu_readline::set_pre_input_hook (pre_input_hook_fcn f)
{
  m_previous_pre_input_hook = ::octave_rl_get_pre_input_hook ();

  if (f != m_previous_pre_input_hook)
    ::octave_rl_set_pre_input_hook (f);
}

void
gnu_readline::restore_pre_input_hook (void)
{
  ::octave_rl_set_pre_input_hook (m_previous_pre_input_hook);
}

void
gnu_readline::set_event_hook (event_hook_fcn f)
{
  m_previous_event_hook = octave_rl_get_event_hook ();

  ::octave_rl_set_event_hook (f);
}

void
gnu_readline::restore_event_hook (void)
{
  ::octave_rl_set_event_hook (m_previous_event_hook);
}

void
gnu_readline::do_read_init_file (const std::string& file)
{
  ::octave_rl_read_init_file (file.c_str ());
}

void
gnu_readline::do_re_read_init_file (void)
{
  ::octave_rl_re_read_init_file ();
}

bool
gnu_readline::do_filename_completion_desired (bool arg)
{
  return ::octave_rl_filename_completion_desired (arg);
}

bool
gnu_readline::do_filename_quoting_desired (bool arg)
{
  return ::octave_rl_filename_quoting_desired (arg);
}

bool
gnu_readline::do_prefer_env_winsize (bool arg)
{
  return ::octave_rl_prefer_env_winsize (arg);
}

void
gnu_readline::do_interrupt (bool arg)
{
  ::octave_rl_done (arg);
}

void
gnu_readline::do_handle_interrupt_signal (void)
{
  octave_signal_caught = 0;
  octave_interrupt_state = 0;

  ::octave_rl_recover_from_interrupt ();

  throw interrupt_exception ();
}

int
gnu_readline::operate_and_get_next (int /* count */, int /* c */)
{
  // Accept the current line.

  command_editor::accept_line ();

  // Find the current line, and find the next line to use.

  int x_where = command_history::where ();

  int x_length = command_history::length ();

  if ((command_history::is_stifled ()
       && (x_length >= command_history::max_input_history ()))
      || (x_where >= x_length - 1))
    command_history::set_mark (x_where);
  else
    command_history::set_mark (x_where + 1);

  command_editor::add_startup_hook (command_history::goto_mark);

  return 0;
}

int
gnu_readline::history_search_backward (int count, int c)
{
  return octave_rl_history_search_backward (count, c);
}

int
gnu_readline::history_search_forward (int count, int c)
{
  return octave_rl_history_search_forward (count, c);
}

char *
gnu_readline::command_generator (const char *text, int state)
{
  char *retval = nullptr;

  completion_fcn f = command_editor::get_completion_function ();

  std::string tmp = f (text, state);

  std::size_t len = tmp.length ();

  if (len > 0)
    {
      retval = static_cast<char *> (std::malloc (len+1));

      if (retval)
        strcpy (retval, tmp.c_str ());
    }

  return retval;
}

char *
gnu_readline::command_quoter (char *text, int matches, char *qcp)
{
  char *retval = nullptr;

  quoting_fcn f = command_editor::get_quoting_function ();

  std::string tmp = f (text, matches, *qcp);

  std::size_t len = tmp.length ();

  if (len > 0)
    {
      retval = static_cast<char *> (std::malloc (len+1));

      if (retval)
        strcpy (retval, tmp.c_str ());
    }

  return retval;
}

char *
gnu_readline::command_dequoter (char *text, int quote)
{
  char *retval = nullptr;

  dequoting_fcn f = command_editor::get_dequoting_function ();

  std::string tmp = f (text, quote);

  std::size_t len = tmp.length ();

  if (len > 0)
    {
      retval = static_cast<char *> (std::malloc (len+1));

      if (retval)
        strcpy (retval, tmp.c_str ());
    }

  return retval;
}

int
gnu_readline::command_char_is_quoted (char *text, int quote)
{
  char_is_quoted_fcn f = command_editor::get_char_is_quoted_function ();

  return f (text, quote);
}

int
gnu_readline::command_accept_line (int count, int key)
{
  user_accept_line_fcn f = command_editor::get_user_accept_line_function ();

  if (f)
    f (::octave_rl_line_buffer ());

  ::octave_rl_redisplay ();

  return ::octave_rl_newline (count, key);
}

char **
gnu_readline::command_completer (const char *text, int, int)
{
  char **matches
    = ::octave_rl_completion_matches (text, gnu_readline::command_generator);

  return matches;
}

#endif

class
default_command_editor : public command_editor
{
public:

  default_command_editor (void)
    : command_editor (), m_input_stream (stdin), m_output_stream (stdout) { }

  // No copying!

  default_command_editor (const default_command_editor&) = delete;

  default_command_editor& operator = (const default_command_editor&) = delete;

  ~default_command_editor (void) = default;

  std::string do_readline (const std::string& prompt, bool& eof);

  void do_set_input_stream (FILE *f);

  FILE * do_get_input_stream (void);

  void do_set_output_stream (FILE *f);

  FILE * do_get_output_stream (void);

  string_vector do_generate_filename_completions (const std::string& text);

  std::string do_get_line_buffer (void) const;

  std::string do_get_current_line (void) const;

  char do_get_prev_char (int) const;

  void do_replace_line (const std::string& text, bool clear_undo);

  void do_kill_full_line (void);

  void do_insert_text (const std::string& text);

  void do_newline (void);

  void do_accept_line (void);

private:

  FILE *m_input_stream;

  FILE *m_output_stream;
};

std::string
default_command_editor::do_readline (const std::string& prompt, bool& eof)
{
  std::fputs (prompt.c_str (), m_output_stream);
  std::fflush (m_output_stream);

  return fgetl (m_input_stream, eof);
}

void
default_command_editor::do_set_input_stream (FILE *f)
{
  m_input_stream = f;
}

FILE *
default_command_editor::do_get_input_stream (void)
{
  return m_input_stream;
}

void
default_command_editor::do_set_output_stream (FILE *f)
{
  m_output_stream = f;
}

FILE *
default_command_editor::do_get_output_stream (void)
{
  return m_output_stream;
}

string_vector
default_command_editor::do_generate_filename_completions (const std::string&)
{
  // FIXME
  return string_vector ();
}

std::string
default_command_editor::do_get_line_buffer (void) const
{
  return "";
}

std::string
default_command_editor::do_get_current_line (void) const
{
  // FIXME
  return "";
}

char
default_command_editor::do_get_prev_char (int) const
{
  return '\0';
}

void
default_command_editor::do_replace_line (const std::string&, bool)
{
  // FIXME
}

void
default_command_editor::do_kill_full_line (void)
{
  // FIXME
}

void
default_command_editor::do_insert_text (const std::string&)
{
  // FIXME
}

void
default_command_editor::do_newline (void)
{
  // FIXME
}

void
default_command_editor::do_accept_line (void)
{
  // FIXME
}

bool
command_editor::instance_ok (void)
{
  bool retval = true;

  if (! s_instance)
    {
      make_command_editor ();

      if (s_instance)
        {
          s_instance->set_event_hook (event_handler);

          singleton_cleanup_list::add (cleanup_instance);
        }
    }

  if (! s_instance)
    (*current_liboctave_error_handler)
      ("unable to create command history object!");

  return retval;
}

void
command_editor::make_command_editor (void)
{
#if defined (USE_READLINE)
  s_instance = new gnu_readline ();
#else
  s_instance = new default_command_editor ();
#endif
}

void
command_editor::force_default_editor (void)
{
  delete s_instance;
  s_instance = new default_command_editor ();
}

void
command_editor::set_initial_input (const std::string& text)
{
  if (instance_ok ())
    s_instance->m_initial_input = text;
}

int
command_editor::insert_initial_input (void)
{
  return instance_ok () ? s_instance->do_insert_initial_input () : 0;
}

int
command_editor::startup_handler (void)
{
  // Iterate over a copy of the set to avoid problems if a hook
  // function attempts to remove itself from the startup_hook_set.

  std::set<startup_hook_fcn> hook_set = m_startup_hook_set;

  for (startup_hook_fcn f : hook_set)
    {
      if (f)
        f ();
    }

  return 0;
}

int
command_editor::pre_input_handler (void)
{
  // Iterate over copy of the set to avoid problems if a hook function
  // attempts to remove itself from the pre_input_hook_set.

  std::set<pre_input_hook_fcn> hook_set = m_pre_input_hook_set;

  for (pre_input_hook_fcn f : hook_set)
    {
      if (f)
        f ();
    }

  return 0;
}

int
command_editor::event_handler (void)
{
  if (octave_interrupt_state)
    handle_interrupt_signal ();

  event_hook_lock.lock ();

  std::set<event_hook_fcn> hook_set (m_event_hook_set);

  event_hook_lock.unlock ();

  for (event_hook_fcn f : hook_set)
    {
      if (f)
        f ();
    }

  return 0;
}

void
command_editor::set_name (const std::string& n)
{
  if (instance_ok ())
    s_instance->do_set_name (n);
}

std::string
command_editor::readline (const std::string& prompt)
{
  bool eof;

  return readline (prompt, eof);
}

std::string
command_editor::readline (const std::string& prompt, bool& eof)
{
  std::string retval;

  if (instance_ok ())
    {
      if (! s_instance->m_initial_input.empty ())
        add_pre_input_hook (command_editor::insert_initial_input);

      retval = s_instance->do_readline (prompt, eof);
    }

  return retval;
}

void
command_editor::set_input_stream (FILE *f)
{
  if (instance_ok ())
    s_instance->do_set_input_stream (f);
}

FILE *
command_editor::get_input_stream (void)
{
  return instance_ok () ? s_instance->do_get_input_stream () : nullptr;
}

void
command_editor::set_output_stream (FILE *f)
{
  if (instance_ok ())
    s_instance->do_set_output_stream (f);
}

FILE *
command_editor::get_output_stream (void)
{
  return instance_ok () ? s_instance->do_get_output_stream () : nullptr;
}

void
command_editor::redisplay (void)
{
  if (instance_ok ())
    s_instance->do_redisplay ();
}

int
command_editor::terminal_rows (void)
{
  return instance_ok () ? s_instance->do_terminal_rows () : -1;
}

int
command_editor::terminal_cols (void)
{
  return instance_ok () ? s_instance->do_terminal_cols () : -1;
}

void
command_editor::clear_screen (bool skip_redisplay)
{
  if (instance_ok ())
    s_instance->do_clear_screen (skip_redisplay);
}

void
command_editor::resize_terminal (void)
{
  if (instance_ok ())
    s_instance->do_resize_terminal ();
}

void
command_editor::set_screen_size (int ht, int wd)
{
  if (instance_ok ())
    s_instance->do_set_screen_size (ht, wd);
}

std::string
command_editor::decode_prompt_string (const std::string& s)
{
  return instance_ok () ? s_instance->do_decode_prompt_string (s) : "";
}

int
command_editor::current_command_number (void)
{
  return instance_ok () ? s_instance->m_command_number : 0;
}

void
command_editor::reset_current_command_number (int n)
{
  if (instance_ok ())
    s_instance->m_command_number = n;
}

void
command_editor::increment_current_command_number (void)
{
  if (instance_ok ())
    s_instance->m_command_number++;
}

void
command_editor::restore_terminal_state (void)
{
  if (instance_ok ())
    s_instance->do_restore_terminal_state ();
}

void
command_editor::blink_matching_paren (bool flag)
{
  if (instance_ok ())
    s_instance->do_blink_matching_paren (flag);
}

bool
command_editor::erase_empty_line (bool flag)
{
  return instance_ok () ? s_instance->do_erase_empty_line (flag) : false;
}

void
command_editor::set_basic_word_break_characters (const std::string& s)
{
  if (instance_ok ())
    s_instance->do_set_basic_word_break_characters (s);
}

void
command_editor::set_completer_word_break_characters (const std::string& s)
{
  if (instance_ok ())
    s_instance->do_set_completer_word_break_characters (s);
}

void
command_editor::set_basic_quote_characters (const std::string& s)
{
  if (instance_ok ())
    s_instance->do_set_basic_quote_characters (s);
}

void
command_editor::set_filename_quote_characters (const std::string& s)
{
  if (instance_ok ())
    s_instance->do_set_filename_quote_characters (s);
}

void
command_editor::set_completer_quote_characters (const std::string& s)
{
  if (instance_ok ())
    s_instance->do_set_completer_quote_characters (s);
}

void
command_editor::set_completion_append_character (char c)
{
  if (instance_ok ())
    s_instance->do_set_completion_append_character (c);
}

void
command_editor::set_completion_function (completion_fcn f)
{
  if (instance_ok ())
    s_instance->do_set_completion_function (f);
}

void
command_editor::set_quoting_function (quoting_fcn f)
{
  if (instance_ok ())
    s_instance->do_set_quoting_function (f);
}

void
command_editor::set_dequoting_function (dequoting_fcn f)
{
  if (instance_ok ())
    s_instance->do_set_dequoting_function (f);
}

void
command_editor::set_char_is_quoted_function (char_is_quoted_fcn f)
{
  if (instance_ok ())
    s_instance->do_set_char_is_quoted_function (f);
}

void
command_editor::set_user_accept_line_function (user_accept_line_fcn f)
{
  if (instance_ok ())
    s_instance->do_set_user_accept_line_function (f);
}

command_editor::completion_fcn
command_editor::get_completion_function (void)
{
  return instance_ok () ? s_instance->do_get_completion_function () : nullptr;
}

command_editor::quoting_fcn
command_editor::get_quoting_function (void)
{
  return instance_ok () ? s_instance->do_get_quoting_function () : nullptr;
}

command_editor::dequoting_fcn
command_editor::get_dequoting_function (void)
{
  return instance_ok () ? s_instance->do_get_dequoting_function () : nullptr;
}

command_editor::char_is_quoted_fcn
command_editor::get_char_is_quoted_function (void)
{
  return (instance_ok ()
          ? s_instance->do_get_char_is_quoted_function () : nullptr);
}

command_editor::user_accept_line_fcn
command_editor::get_user_accept_line_function (void)
{
  return (instance_ok ()
          ? s_instance->do_get_user_accept_line_function () : nullptr);
}

string_vector
command_editor::generate_filename_completions (const std::string& text)
{
  return (instance_ok ()
          ? s_instance->do_generate_filename_completions (text)
          : string_vector ());
}

std::string
command_editor::get_line_buffer (void)
{
  return instance_ok () ? s_instance->do_get_line_buffer () : "";
}

std::string
command_editor::get_current_line (void)
{
  return instance_ok () ? s_instance->do_get_current_line () : "";
}

// Return the character (offset+1) to the left of the cursor,
// or '\0' if the cursor is at the start of the line.
char
command_editor::get_prev_char (int offset)
{
  return instance_ok () ? s_instance->do_get_prev_char (offset) : '\0';
}

void
command_editor::replace_line (const std::string& text, bool clear_undo)
{
  if (instance_ok ())
    s_instance->do_replace_line (text, clear_undo);
}

void
command_editor::kill_full_line (void)
{
  if (instance_ok ())
    s_instance->do_kill_full_line ();
}

void
command_editor::insert_text (const std::string& text)
{
  if (instance_ok ())
    s_instance->do_insert_text (text);
}

void
command_editor::newline (void)
{
  if (instance_ok ())
    s_instance->do_newline ();
}

void
command_editor::accept_line (void)
{
  if (instance_ok ())
    s_instance->do_accept_line ();
}

bool
command_editor::undo (void)
{
  return instance_ok () ? s_instance->do_undo () : false;
}

void
command_editor::clear_undo_list (void)
{
  if (instance_ok ())
    s_instance->do_clear_undo_list ();
}

void
command_editor::add_startup_hook (startup_hook_fcn f)
{
  if (instance_ok ())
    {
      m_startup_hook_set.insert (f);

      s_instance->set_startup_hook (startup_handler);
    }
}

void
command_editor::remove_startup_hook (startup_hook_fcn f)
{
  if (instance_ok ())
    {
      auto p = m_startup_hook_set.find (f);

      if (p != m_startup_hook_set.end ())
        m_startup_hook_set.erase (p);

      if (m_startup_hook_set.empty ())
        s_instance->restore_startup_hook ();
    }
}

void
command_editor::add_pre_input_hook (pre_input_hook_fcn f)
{
  if (instance_ok ())
    {
      m_pre_input_hook_set.insert (f);

      s_instance->set_pre_input_hook (pre_input_handler);
    }
}

void
command_editor::remove_pre_input_hook (pre_input_hook_fcn f)
{
  if (instance_ok ())
    {
      auto p = m_pre_input_hook_set.find (f);

      if (p != m_pre_input_hook_set.end ())
        m_pre_input_hook_set.erase (p);

      if (m_pre_input_hook_set.empty ())
        s_instance->restore_pre_input_hook ();
    }
}

void
command_editor::add_event_hook (event_hook_fcn f)
{
  autolock guard (event_hook_lock);

  m_event_hook_set.insert (f);
}

void
command_editor::remove_event_hook (event_hook_fcn f)
{
  autolock guard (event_hook_lock);

  auto p = m_event_hook_set.find (f);

  if (p != m_event_hook_set.end ())
    m_event_hook_set.erase (p);

}

void
command_editor::run_event_hooks (void)
{
  event_handler ();
}

void
command_editor::read_init_file (const std::string& file_arg)
{
  if (instance_ok ())
    {
      std::string file = sys::file_ops::tilde_expand (file_arg);

      s_instance->do_read_init_file (file);
    }
}

void
command_editor::re_read_init_file (void)
{
  if (instance_ok ())
    s_instance->do_re_read_init_file ();
}

bool
command_editor::filename_completion_desired (bool arg)
{
  return (instance_ok ()
          ? s_instance->do_filename_completion_desired (arg) : false);
}

bool
command_editor::filename_quoting_desired (bool arg)
{
  return (instance_ok ())
         ? s_instance->do_filename_quoting_desired (arg) : false;
}

bool
command_editor::prefer_env_winsize (bool arg)
{
  return instance_ok () ? s_instance->do_prefer_env_winsize (arg) : false;
}

bool
command_editor::interrupt (bool arg)
{
  bool retval;

  if (instance_ok ())
    {
      // Return the current interrupt state.
      retval = s_instance->m_interrupted;

      s_instance->do_interrupt (arg);

      s_instance->m_interrupted = arg;
    }
  else
    retval = false;

  return retval;
}

void
command_editor::interrupt_event_loop (bool arg)
{
  if (instance_ok ())
    s_instance->do_interrupt_event_loop (arg);
}

bool
command_editor::event_loop_interrupted (void)
{
  return instance_ok () ? s_instance->do_event_loop_interrupted  () : false;
}

void
command_editor::handle_interrupt_signal (void)
{
  if (instance_ok ())
    s_instance->do_handle_interrupt_signal ();
}

// Return a string which will be printed as a prompt.  The string may
// contain special characters which are decoded as follows:
//
//      \a      bell (ascii 07)
//      \d      the date
//      \e      escape (ascii 033)
//      \h      the hostname up to the first '.'
//      \H      the hostname
//      \n      CRLF
//      \r      CR
//      \s      the name of the shell (program)
//      \t      the time
//      \T      the time in 12-hour hh:mm:ss format
//      \@      the time in 12-hour hh:mm am/pm format
//      \A      the time in 24-hour hh:mm format
//      \u      your username
//      \w      the current working directory
//      \W      the last element of PWD
//      \!      the history number of this command
//      \#      the command number of this command
//      \$      a $ or a # if you are root
//      \nnn    character code nnn in octal
//      \\      a backslash
//      \[      begin a sequence of non-printing chars
//      \]      end a sequence of non-printing chars

std::string
command_editor::do_decode_prompt_string (const std::string& s)
{
  std::string retval;
  std::string tmpstr;
  std::size_t i = 0;
  std::size_t slen = s.length ();
  int c;

  while (i < slen)
    {
      c = s[i];

      i++;

      if (c == '\\')
        {
          c = s[i];

          switch (c)
            {
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
              // Maybe convert an octal number.
              {
                int n = read_octal (s.substr (i, 3));

                tmpstr = '\\';

                if (n != -1)
                  {
                    tmpstr[0] = n;
                    i += 2;   // i++ makes this += 3 later
                  }

                break;
              }

            case 'a':
              {
                tmpstr = '\a';

                break;
              }

            case 'd':
            case 't':
            case 'T':
            case '@':
            case 'A':
              // Make the current time/date into a string.
              {
                sys::localtime now;

                if (c == 'd')
                  tmpstr = now.strftime ("%a %b %d");
                else if (c == 't')
                  tmpstr = now.strftime ("%H:%M:%S");
                else if (c == 'T')
                  tmpstr = now.strftime ("%I:%M:%S");
                else if (c == '@')
                  tmpstr = now.strftime ("%I:%M %p");
                else // (c == 'A')
                  tmpstr = now.strftime ("%H:%M");

                break;
              }

            case 'e':
              {
                tmpstr = '\033';

                break;
              }

            case 'h':
              {
                tmpstr = sys::env::get_host_name ();

                std::size_t pos = tmpstr.find ('.');

                if (pos != std::string::npos)
                  tmpstr.resize (pos);

                break;
              }

            case 'H':
              {
                tmpstr = sys::env::get_host_name ();

                break;
              }

            case 'n':
              {
                tmpstr = newline_chars ();

                break;
              }

            case 'r':
              {
                tmpstr = '\r';

                break;
              }

            case 's':
              {
                tmpstr = sys::env::get_program_name ();
                tmpstr = sys::env::base_pathname (tmpstr);

                break;
              }

            case 'u':
              {
                tmpstr = sys::env::get_user_name ();

                break;
              }

            case 'w':
            case 'W':
              {
                try
                  {
                    tmpstr = sys::env::get_current_directory ();
                  }
                catch (const execution_exception&)
                  {
                    tmpstr = "";
                  }

                std::string home_dir = sys::env::get_home_directory ();

                if (c == 'W' && (home_dir.empty () || tmpstr != home_dir))
                  {
                    if (tmpstr != "/" && tmpstr != "//")
                      {
                        std::size_t pos = tmpstr.rfind ('/');

                        if (pos != std::string::npos && pos != 0)
                          tmpstr = tmpstr.substr (pos + 1);
                      }
                  }
                else
                  tmpstr = sys::env::polite_directory_format (tmpstr);

                break;
              }

            case '!':
              {
                char number_buffer[32];
                int num = command_history::current_number ();
                if (num > 0)
                  sprintf (number_buffer, "%d", num);
                else
                  strcpy (number_buffer, "!");
                tmpstr = number_buffer;

                break;
              }

            case '#':
              {
                char number_buffer[32];
                sprintf (number_buffer, "%d", m_command_number);
                tmpstr = number_buffer;

                break;
              }

            case '$':
              {
                tmpstr = (octave_geteuid_wrapper () == 0 ? '#' : '$');
                break;
              }

#if defined (USE_READLINE)
            case '[':
            case ']':
              {
                tmpstr.resize (1);

                tmpstr[0] = ((c == '[')
                             ? ::octave_rl_prompt_start_ignore ()
                             : ::octave_rl_prompt_end_ignore ());

                break;
              }
#endif

            case '\\':
              {
                tmpstr = '\\';

                break;
              }

            default:
              {
                tmpstr = "\\ ";
                tmpstr[1] = c;

                break;
              }
            }

          retval.append (tmpstr);
          i++;   // Move past processed escape character
        }
      else
        retval += c;
    }

  return retval;
}

int
command_editor::do_insert_initial_input (void)
{
  std::string input = m_initial_input;

  m_initial_input = "";

  do_insert_text (input);

  // Is it really right to redisplay here?
  do_redisplay ();

  return 0;
}

// Return the octal number parsed from STRING, or -1 to indicate that
// the string contained a bad number.

int
command_editor::read_octal (const std::string& s)
{
  int result = 0;
  int digits = 0;

  std::size_t i = 0;
  std::size_t slen = s.length ();

  while (i < slen && s[i] >= '0' && s[i] < '8')
    {
      digits++;
      result = (result * 8) + s[i] - '0';
      i++;
    }

  if (! digits || result > 0777 || i < slen)
    result = -1;

  return result;
}

void
command_editor::error (int err_num)
{
  (*current_liboctave_error_handler) ("%s", std::strerror (err_num));
}

void
command_editor::error (const std::string& s)
{
  (*current_liboctave_error_handler) ("%s", s.c_str ());
}

OCTAVE_END_NAMESPACE(octave)
