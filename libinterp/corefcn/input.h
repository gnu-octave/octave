/*

Copyright (C) 1993-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

// Use the GNU readline library for command line editing and hisory.

#if ! defined (octave_input_h)
#define octave_input_h 1

#include "octave-config.h"

#include <cstdio>

#include <string>

#include "oct-refcount.h"
#include "oct-time.h"
#include "ovl.h"
#include "pager.h"

class octave_value;
namespace octave
{
  class base_lexer;
}

extern OCTINTERP_API FILE * get_input_from_stdin (void);

// TRUE after a call to completion_matches.
extern bool octave_completion_matches_called;

// TRUE if the plotting system has requested a call to drawnow at
// the next user prompt.
extern OCTINTERP_API bool Vdrawnow_requested;

// TRUE if we are in debugging mode.
extern OCTINTERP_API bool Vdebugging;

// TRUE if we are not executing a command direct from debug> prompt.
extern OCTINTERP_API bool Vtrack_line_num;

extern std::string find_indexed_expression (const std::string& text);

extern void initialize_command_input (void);

extern bool octave_yes_or_no (const std::string& prompt);

extern octave_value do_keyboard (const octave_value_list& args
                                 = octave_value_list ());

extern void remove_input_event_hook_functions (void);

extern void set_default_prompts (void);

extern octave::sys::time Vlast_prompt_time;

namespace octave
{
  class
  base_reader
  {
  public:

    friend class input_reader;

    base_reader (base_lexer *lxr)
      : m_count (1), m_pflag (0), m_lexer (lxr)
    { }

    base_reader (const base_reader& x)
      : m_count (1), m_pflag (x.m_pflag), m_lexer (x.m_lexer)
    { }

    virtual ~base_reader (void) = default;

    virtual std::string get_input (bool& eof) = 0;

    virtual std::string input_source (void) const { return s_in_src; }

    void reset (void) { promptflag (1); }

    void increment_promptflag (void) { m_pflag++; }

    void decrement_promptflag (void) { m_pflag--; }

    int promptflag (void) const { return m_pflag; }

    int promptflag (int n)
    {
      int retval = m_pflag;
      m_pflag = n;
      return retval;
    }

    std::string octave_gets (bool& eof);

    virtual bool reading_fcn_file (void) const;

    virtual bool reading_classdef_file (void) const;

    virtual bool reading_script_file (void) const;

    virtual bool input_from_terminal (void) const { return false; }

    virtual bool input_from_file (void) const { return false; }

    virtual bool input_from_eval_string (void) const { return false; }

  private:

    refcount<int> m_count;

    int m_pflag;

    base_lexer *m_lexer;

    static const std::string s_in_src;
  };

  class
  input_reader
  {
  public:

    input_reader (base_lexer *lxr = nullptr);

    input_reader (FILE *file, base_lexer *lxr = nullptr);

    input_reader (const std::string& str, base_lexer *lxr = nullptr);

    input_reader (const input_reader& ir)
    {
      m_rep = ir.m_rep;
      m_rep->m_count++;
    }

    input_reader& operator = (const input_reader& ir)
    {
      if (&ir != this)
        {
          m_rep = ir.m_rep;
          m_rep->m_count++;
        }

      return *this;
    }

    ~input_reader (void)
    {
      if (--m_rep->m_count == 0)
        delete m_rep;
    }

    void reset (void) { return m_rep->reset (); }

    void increment_promptflag (void) { m_rep->increment_promptflag (); }

    void decrement_promptflag (void) { m_rep->decrement_promptflag (); }

    int promptflag (void) const { return m_rep->promptflag (); }

    int promptflag (int n) { return m_rep->promptflag (n); }

    std::string get_input (bool& eof)
    {
      return m_rep->get_input (eof);
    }

    std::string input_source (void) const
    {
      return m_rep->input_source ();
    }

    bool input_from_terminal (void) const
    {
      return m_rep->input_from_terminal ();
    }

    bool input_from_file (void) const
    {
      return m_rep->input_from_file ();
    }

    bool input_from_eval_string (void) const
    {
      return m_rep->input_from_eval_string ();
    }

  private:

    base_reader *m_rep;
  };
}

#endif
