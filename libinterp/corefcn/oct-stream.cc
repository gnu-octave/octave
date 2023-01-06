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

#include <cassert>
#include <cctype>
#include <cstring>

#include <algorithm>
#include <deque>
#include <fstream>
#include <limits>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

#include "Array.h"
#include "Cell.h"
#include "byte-swap.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-utils.h"
#include "oct-locbuf.h"
#include "octave-preserve-stream-state.h"
#include "quit.h"
#include "str-vec.h"
#include "strcase-wrappers.h"

#include "error.h"
#include "errwarn.h"
#include "input.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "octave.h"
#include "oct-iostrm.h"
#include "oct-stdstrm.h"
#include "oct-string.h"
#include "oct-stream.h"
#include "ov.h"
#include "ovl.h"
#include "pager.h"
#include "utils.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Programming Note: There are two very different error functions used
// in the stream code.  When invoked with "error (...)" the member
// function from stream or base_stream is called.  This
// function sets the error state on the stream AND returns control to
// the caller.  The caller must then return a value at the end of the
// function.  When invoked with "::error (...)" the exception-based
// error function from error.h is used.  This function will throw an
// exception and not return control to the caller.  BE CAREFUL and
// invoke the correct error function!

// Possible values for conv_err:
//
//   1 : not a real scalar
//   2 : value is NaN
//   3 : value is not an integer

static int
convert_to_valid_int (const octave_value& tc, int& conv_err)
{
  conv_err = 0;

  int retval = 0;

  double dval = 0.0;

  try
    {
      dval = tc.double_value ();
    }
  catch (const execution_exception&)
    {
      interpreter& interp = __get_interpreter__ ();

      interp.recover_from_exception ();

      conv_err = 1;
    }

  if (! conv_err)
    {
      if (! lo_ieee_isnan (dval))
        {
          int ival = math::nint (dval);

          if (ival == dval)
            retval = ival;
          else
            conv_err = 3;
        }
      else
        conv_err = 2;
    }

  return retval;
}

static octave_idx_type
get_size (double d, const std::string& who)
{
  octave_idx_type retval = -1;

  if (lo_ieee_isnan (d))
    ::error ("%s: NaN invalid as size specification", who.c_str ());

  if (math::isinf (d))
    retval = -1;
  else
    {
      if (d < 0.0)
        ::error ("%s: negative value invalid as size specification",
                 who.c_str ());

      static const double out_of_range_top
        = static_cast<double> (std::numeric_limits<octave_idx_type>::max ())
          + 1.;
      if (d >= out_of_range_top)
        ::error ("%s: dimension too large for Octave's index type",
                 who.c_str ());

      retval = math::nint_big (d);
    }

  return retval;
}

static void
get_size (const Array<double>& size,
          octave_idx_type& nr, octave_idx_type& nc,
          bool& one_elt_size_spec, const std::string& who)
{
  nr = -1;
  nc = -1;

  one_elt_size_spec = false;

  double dnr = -1.0;
  double dnc = -1.0;

  octave_idx_type sz_len = size.numel ();

  if (sz_len == 1)
    {
      one_elt_size_spec = true;

      dnr = size(0);

      dnc = (dnr == 0.0) ? 0.0 : 1.0;
    }
  else if (sz_len == 2)
    {
      dnr = size(0);

      if (math::isinf (dnr))
        ::error ("%s: infinite value invalid as size specification",
                 who.c_str ());

      dnc = size(1);
    }
  else
    ::error ("%s: invalid size specification (must be 2-D)", who.c_str ());

  nr = get_size (dnr, who);

  if (dnc >= 0.0)
    {
      nc = get_size (dnc, who);

      // Check for overflow.
      if (nr > 0 && nc > 0
          && nc > std::numeric_limits<octave_idx_type>::max () / nr)
        ::error ("%s: size too large for Octave's index type", who.c_str ());
    }
}

static std::string
expand_char_class (const std::string& s)
{
  std::string retval;

  std::size_t len = s.length ();

  std::size_t i = 0;

  while (i < len)
    {
      unsigned char c = s[i++];

      if (c == '-' && i > 1 && i < len
          && (   static_cast<unsigned char> (s[i-2])
                 <= static_cast<unsigned char> (s[i])))
        {
          // Add all characters from the range except the first (we
          // already added it below).

          for (c = s[i-2]+1; c < s[i]; c++)
            retval += c;
        }
      else
        {
          // Add the character to the class.  Only add '-' if it is
          // the last character in the class.

          if (c != '-' || i == len)
            retval += c;
        }
    }

  return retval;
}

class
scanf_format_elt
{
public:

  enum special_conversion
  {
    whitespace_conversion = 1,
    literal_conversion = 2,
    null = 3
  };

  scanf_format_elt (const std::string& txt = "", int w = 0, bool d = false,
                    char typ = '\0', char mod = '\0',
                    const std::string& ch_class = "")
    : text (txt), width (w), discard (d), type (typ),
      modifier (mod), char_class (ch_class)
  { }

  scanf_format_elt (const scanf_format_elt&) = default;

  scanf_format_elt& operator = (const scanf_format_elt&) = default;

  ~scanf_format_elt (void) = default;

  // The C-style format string.
  std::string text;

  // The maximum field width.
  int width;

  // TRUE if we are not storing the result of this conversion.
  bool discard;

  // Type of conversion -- 'd', 'i', 'o', 'u', 'x', 'e', 'f', 'g',
  // 'c', 's', 'p', '%', or '['.
  char type;

  // A length modifier -- 'h', 'l', or 'L'.
  char modifier;

  // The class of characters in a '[' format.
  std::string char_class;
};

class
scanf_format_list
{
public:

  scanf_format_list (const std::string& fmt = "");

  // No copying!

  scanf_format_list (const scanf_format_list&) = delete;

  scanf_format_list& operator = (const scanf_format_list&) = delete;

  ~scanf_format_list (void);

  octave_idx_type num_conversions (void) { return m_nconv; }

  // The length can be different than the number of conversions.
  // For example, "x %d y %d z" has 2 conversions but the length of
  // the list is 3 because of the characters that appear after the
  // last conversion.

  std::size_t length (void) const { return m_fmt_elts.size (); }

  const scanf_format_elt * first (void)
  {
    m_curr_idx = 0;
    return current ();
  }

  const scanf_format_elt * current (void) const
  {
    return length () > 0 ? m_fmt_elts[m_curr_idx] : nullptr;
  }

  const scanf_format_elt * next (bool cycle = true)
  {
    static scanf_format_elt dummy
    ("", 0, false, scanf_format_elt::null, '\0', "");

    m_curr_idx++;

    if (m_curr_idx >= length ())
      {
        if (cycle)
          m_curr_idx = 0;
        else
          return &dummy;
      }

    return current ();
  }

  void printme (void) const;

  bool ok (void) const { return (m_nconv >= 0); }

  operator bool () const { return ok (); }

  bool all_character_conversions (void);

  bool all_numeric_conversions (void);

private:

  void add_elt_to_list (int width, bool discard, char type, char modifier,
                        const std::string& char_class = "");

  void process_conversion (const std::string& s, std::size_t& i,
                           std::size_t n, int& width, bool& discard,
                           char& type, char& modifier);

  int finish_conversion (const std::string& s, std::size_t& i, std::size_t n,
                         int width, bool discard, char& type,
                         char modifier);

  //--------

  // Number of conversions specified by this format string, or -1 if
  // invalid conversions have been found.
  octave_idx_type m_nconv;

  // Index to current element;
  std::size_t m_curr_idx;

  // List of format elements.
  std::deque<scanf_format_elt *> m_fmt_elts;

  // Temporary buffer.
  std::ostringstream m_buf;

};

scanf_format_list::scanf_format_list (const std::string& s)
  : m_nconv (0), m_curr_idx (0), m_fmt_elts (), m_buf ()
{
  std::size_t n = s.length ();

  std::size_t i = 0;

  int width = 0;
  bool discard = false;
  char modifier = '\0';
  char type = '\0';

  bool have_more = true;

  while (i < n)
    {
      have_more = true;

      if (s[i] == '%')
        {
          // Process percent-escape conversion type.

          process_conversion (s, i, n, width, discard, type, modifier);

          have_more = (m_buf.tellp () != 0);
        }
      else if (isspace (s[i]))
        {
          type = scanf_format_elt::whitespace_conversion;

          width = 0;
          discard = false;
          modifier = '\0';
          m_buf << ' ';

          while (++i < n && isspace (s[i]))
            ; // skip whitespace

          add_elt_to_list (width, discard, type, modifier);

          have_more = false;
        }
      else
        {
          type = scanf_format_elt::literal_conversion;

          width = 0;
          discard = false;
          modifier = '\0';

          while (i < n && ! isspace (s[i]) && s[i] != '%')
            m_buf << s[i++];

          add_elt_to_list (width, discard, type, modifier);

          have_more = false;
        }

      if (m_nconv < 0)
        {
          have_more = false;
          break;
        }
    }

  if (have_more)
    add_elt_to_list (width, discard, type, modifier);

  m_buf.clear ();
  m_buf.str ("");
}

scanf_format_list::~scanf_format_list (void)
{
  std::size_t n = m_fmt_elts.size ();

  for (std::size_t i = 0; i < n; i++)
    {
      scanf_format_elt *elt = m_fmt_elts[i];
      delete elt;
    }
}

void
scanf_format_list::add_elt_to_list (int width, bool discard, char type,
                                    char modifier,
                                    const std::string& char_class)
{
  std::string text = m_buf.str ();

  if (! text.empty ())
    {
      scanf_format_elt *elt
        = new scanf_format_elt (text, width, discard, type,
                                modifier, char_class);

      m_fmt_elts.push_back (elt);
    }

  m_buf.clear ();
  m_buf.str ("");
}

void
scanf_format_list::process_conversion (const std::string& s, std::size_t& i,
                                       std::size_t n, int& width,
                                       bool& discard, char& type,
                                       char& modifier)
{
  width = 0;
  discard = false;
  modifier = '\0';
  type = '\0';

  m_buf << s[i++];

  bool have_width = false;

  while (i < n)
    {
      switch (s[i])
        {
        case '*':
          if (discard)
            m_nconv = -1;
          else
            {
              discard = true;
              m_buf << s[i++];
            }
          break;

        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
          if (have_width)
            m_nconv = -1;
          else
            {
              char c = s[i++];
              width = 10 * width + c - '0';
              have_width = true;
              m_buf << c;
              while (i < n && isdigit (s[i]))
                {
                  c = s[i++];
                  width = 10 * width + c - '0';
                  m_buf << c;
                }
            }
          break;

        case 'h': case 'l': case 'L':
          if (modifier != '\0')
            m_nconv = -1;
          else
            modifier = s[i++];
          break;

        // We accept X for compatibility with undocumented Matlab behavior.
        case 'd': case 'i': case 'o': case 'u': case 'x':
        case 'X':
          if (modifier == 'L')
            {
              m_nconv = -1;
              break;
            }
          goto fini;

        // We accept E and G for compatibility with undocumented
        // Matlab behavior.
        case 'e': case 'f': case 'g':
        case 'E': case 'G':
          if (modifier == 'h')
            {
              m_nconv = -1;
              break;
            }

          // No float or long double conversions, thanks.
          m_buf << 'l';

          goto fini;

        case 'c': case 's': case 'p': case '%': case '[':
          if (modifier != '\0')
            {
              m_nconv = -1;
              break;
            }
          goto fini;

        fini:
          {
            if (finish_conversion (s, i, n, width, discard,
                                   type, modifier) == 0)
              return;
          }
          break;

        default:
          m_nconv = -1;
          break;
        }

      if (m_nconv < 0)
        break;
    }

  m_nconv = -1;
}

int
scanf_format_list::finish_conversion (const std::string& s, std::size_t& i,
                                      std::size_t n, int width, bool discard,
                                      char& type, char modifier)
{
  int retval = 0;

  std::string char_class;

  std::size_t beg_idx = std::string::npos;
  std::size_t end_idx = std::string::npos;

  if (s[i] == '%')
    {
      type = '%';
      m_buf << s[i++];
    }
  else
    {
      type = s[i];

      if (s[i] == '[')
        {
          m_buf << s[i++];

          if (i < n)
            {
              beg_idx = i;

              if (s[i] == '^')
                {
                  type = '^';
                  m_buf << s[i++];

                  if (i < n)
                    {
                      beg_idx = i;

                      if (s[i] == ']')
                        m_buf << s[i++];
                    }
                }
              else if (s[i] == ']')
                m_buf << s[i++];
            }

          while (i < n && s[i] != ']')
            m_buf << s[i++];

          if (i < n && s[i] == ']')
            {
              end_idx = i-1;
              m_buf << s[i++];
            }

          if (s[i-1] != ']')
            retval = m_nconv = -1;
        }
      else
        m_buf << s[i++];

      m_nconv++;
    }

  if (m_nconv >= 0)
    {
      if (beg_idx != std::string::npos && end_idx != std::string::npos)
        char_class = expand_char_class (s.substr (beg_idx,
                                        end_idx - beg_idx + 1));

      add_elt_to_list (width, discard, type, modifier, char_class);
    }

  return retval;
}

void
scanf_format_list::printme (void) const
{
  std::size_t n = m_fmt_elts.size ();

  for (std::size_t i = 0; i < n; i++)
    {
      scanf_format_elt *elt = m_fmt_elts[i];

      std::cerr
          << "width:      " << elt->width << "\n"
          << "discard:    " << elt->discard << "\n"
          << "type:       ";

      if (elt->type == scanf_format_elt::literal_conversion)
        std::cerr << "literal text\n";
      else if (elt->type == scanf_format_elt::whitespace_conversion)
        std::cerr << "whitespace\n";
      else
        std::cerr << elt->type << "\n";

      std::cerr
          << "modifier:   " << elt->modifier << "\n"
          << "char_class: '" << undo_string_escapes (elt->char_class) << "'\n"
          << "text:       '" << undo_string_escapes (elt->text) << "'\n\n";
    }
}

bool
scanf_format_list::all_character_conversions (void)
{
  std::size_t n = m_fmt_elts.size ();

  if (n > 0)
    {
      for (std::size_t i = 0; i < n; i++)
        {
          scanf_format_elt *elt = m_fmt_elts[i];

          switch (elt->type)
            {
            case 'c': case 's': case '%': case '[': case '^':
            case scanf_format_elt::literal_conversion:
            case scanf_format_elt::whitespace_conversion:
              break;

            default:
              return false;
              break;
            }
        }

      return true;
    }
  else
    return false;
}

bool
scanf_format_list::all_numeric_conversions (void)
{
  std::size_t n = m_fmt_elts.size ();

  if (n > 0)
    {
      for (std::size_t i = 0; i < n; i++)
        {
          scanf_format_elt *elt = m_fmt_elts[i];

          switch (elt->type)
            {
            case 'd': case 'i': case 'o': case 'u': case 'x': case 'X':
            case 'e': case 'f': case 'g': case 'E': case 'G':
              break;

            default:
              return false;
              break;
            }
        }

      return true;
    }
  else
    return false;
}

class
printf_format_elt
{
public:

  printf_format_elt (const std::string& txt = "", int n = 0, int w = -1,
                     int p = -1, const std::string& f = "",
                     char typ = '\0', char mod = '\0')
    : text (txt), args (n), fw (w), prec (p), flags (f),
      type (typ), modifier (mod)
  { }

  printf_format_elt (const printf_format_elt&) = default;

  printf_format_elt& operator = (const printf_format_elt&) = default;

  ~printf_format_elt (void) = default;

  // The C-style format string.
  std::string text;

  // How many args do we expect to consume?
  int args;

  // Field width.
  int fw;

  // Precision.
  int prec;

  // Flags -- '-', '+', ' ', '0', or '#'.
  std::string flags;

  // Type of conversion -- 'd', 'i', 'o', 'x', 'X', 'u', 'c', 's',
  // 'f', 'e', 'E', 'g', 'G', 'p', or '%'
  char type;

  // A length modifier -- 'h', 'l', or 'L'.
  char modifier;
};

class
printf_format_list
{
public:

  printf_format_list (const std::string& fmt = "");

  // No copying!

  printf_format_list (const printf_format_list&) = delete;

  printf_format_list& operator = (const printf_format_list&) = delete;

  ~printf_format_list (void);

  octave_idx_type num_conversions (void) { return m_nconv; }

  const printf_format_elt * first (void)
  {
    m_curr_idx = 0;
    return current ();
  }

  const printf_format_elt * current (void) const
  {
    return length () > 0 ? m_fmt_elts[m_curr_idx] : nullptr;
  }

  std::size_t length (void) const { return m_fmt_elts.size (); }

  const printf_format_elt * next (bool cycle = true)
  {
    m_curr_idx++;

    if (m_curr_idx >= length ())
      {
        if (cycle)
          m_curr_idx = 0;
        else
          return nullptr;
      }

    return current ();
  }

  bool last_elt_p (void) { return (m_curr_idx + 1 == length ()); }

  void printme (void) const;

  bool ok (void) const { return (m_nconv >= 0); }

  operator bool () const { return ok (); }

private:

  void add_elt_to_list (int args, const std::string& flags, int fw,
                        int prec, char type, char modifier);

  void process_conversion (const std::string& s, std::size_t& i,
                           std::size_t n,
                           int& args, std::string& flags, int& fw,
                           int& prec, char& modifier, char& type);

  void finish_conversion (const std::string& s, std::size_t& i, int args,
                          const std::string& flags, int fw, int prec,
                          char modifier, char& type);

  //--------

  // Number of conversions specified by this format string, or -1 if
  // invalid conversions have been found.
  octave_idx_type m_nconv;

  // Index to current element;
  std::size_t m_curr_idx;

  // List of format elements.
  std::deque<printf_format_elt *> m_fmt_elts;

  // Temporary buffer.
  std::ostringstream m_buf;

};

printf_format_list::printf_format_list (const std::string& s)
  : m_nconv (0), m_curr_idx (0), m_fmt_elts (), m_buf ()
{
  std::size_t n = s.length ();

  std::size_t i = 0;

  int args = 0;
  std::string flags;
  int fw = -1;
  int prec = -1;
  char modifier = '\0';
  char type = '\0';

  bool have_more = true;
  bool empty_buf = true;

  if (n == 0)
    {
      printf_format_elt *elt
        = new printf_format_elt ("", args, fw, prec, flags, type, modifier);

      m_fmt_elts.push_back (elt);
    }
  else
    {
      while (i < n)
        {
          have_more = true;

          empty_buf = (m_buf.tellp () == 0);

          switch (s[i])
            {
            case '%':
              {
                if (empty_buf)
                  {
                    process_conversion (s, i, n, args, flags, fw, prec,
                                        modifier, type);

                    // If there is nothing in the buffer, then
                    // add_elt_to_list must have just been called, so we
                    // are already done with the current element and we
                    // don't need to call add_elt_to_list if this is our
                    // last trip through the loop.

                    have_more = (m_buf.tellp () != 0);
                  }
                else
                  add_elt_to_list (args, flags, fw, prec, type, modifier);
              }
              break;

            default:
              {
                args = 0;
                flags = "";
                fw = -1;
                prec = -1;
                modifier = '\0';
                type = '\0';
                m_buf << s[i++];
              }
              break;
            }

          if (m_nconv < 0)
            {
              have_more = false;
              break;
            }
        }

      if (have_more)
        add_elt_to_list (args, flags, fw, prec, type, modifier);

      m_buf.clear ();
      m_buf.str ("");
    }
}

printf_format_list::~printf_format_list (void)
{
  std::size_t n = m_fmt_elts.size ();

  for (std::size_t i = 0; i < n; i++)
    {
      printf_format_elt *elt = m_fmt_elts[i];
      delete elt;
    }
}

void
printf_format_list::add_elt_to_list (int args, const std::string& flags,
                                     int fw, int prec, char type,
                                     char modifier)
{
  std::string text = m_buf.str ();

  if (! text.empty ())
    {
      printf_format_elt *elt
        = new printf_format_elt (text, args, fw, prec, flags,
                                 type, modifier);

      m_fmt_elts.push_back (elt);
    }

  m_buf.clear ();
  m_buf.str ("");
}

void
printf_format_list::process_conversion (const std::string& s, std::size_t& i,
                                        std::size_t n, int& args,
                                        std::string& flags, int& fw,
                                        int& prec, char& modifier,
                                        char& type)
{
  args = 0;
  flags = "";
  fw = -1;
  prec = -1;
  modifier = '\0';
  type = '\0';

  m_buf << s[i++];

  bool nxt = false;

  while (i < n)
    {
      switch (s[i])
        {
        case '-': case '+': case ' ': case '0': case '#':
          flags += s[i];
          m_buf << s[i++];
          break;

        default:
          nxt = true;
          break;
        }

      if (nxt)
        break;
    }

  if (i < n)
    {
      if (s[i] == '*')
        {
          fw = -2;
          args++;
          m_buf << s[i++];
        }
      else
        {
          if (isdigit (s[i]))
            {
              int nn = 0;
              std::string tmp = s.substr (i);
              sscanf (tmp.c_str (), "%d%n", &fw, &nn);
            }

          while (i < n && isdigit (s[i]))
            m_buf << s[i++];
        }
    }

  if (i < n && s[i] == '.')
    {
      // nothing before the . means 0.
      if (fw == -1)
        fw = 0;

      // . followed by nothing is 0.
      prec = 0;

      m_buf << s[i++];

      if (i < n)
        {
          if (s[i] == '*')
            {
              prec = -2;
              args++;
              m_buf << s[i++];
            }
          else
            {
              if (isdigit (s[i]))
                {
                  int nn = 0;
                  std::string tmp = s.substr (i);
                  sscanf (tmp.c_str (), "%d%n", &prec, &nn);
                }

              while (i < n && isdigit (s[i]))
                m_buf << s[i++];
            }
        }
    }

  if (i < n)
    {
      // Accept and record modifier, but don't place it in the format
      // item text.  All integer conversions are handled as 64-bit
      // integers.

      switch (s[i])
        {
        case 'h': case 'l': case 'L':
          modifier = s[i++];
          break;

        default:
          break;
        }
    }

  if (i < n)
    finish_conversion (s, i, args, flags, fw, prec, modifier, type);
  else
    m_nconv = -1;
}

void
printf_format_list::finish_conversion (const std::string& s, std::size_t& i,
                                       int args, const std::string& flags,
                                       int fw, int prec, char modifier,
                                       char& type)
{
  switch (s[i])
    {
    case 'd': case 'i': case 'o': case 'x': case 'X':
    case 'u': case 'c':
      if (modifier == 'L')
        {
          m_nconv = -1;
          break;
        }
      goto fini;

    case 'f': case 'e': case 'E': case 'g': case 'G':
      if (modifier == 'h' || modifier == 'l')
        {
          m_nconv = -1;
          break;
        }
      goto fini;

    case 's': case 'p': case '%':
      if (modifier != '\0')
        {
          m_nconv = -1;
          break;
        }
      goto fini;

    fini:

      type = s[i];

      m_buf << s[i++];

      if (type != '%' || args != 0)
        m_nconv++;

      if (type != '%')
        args++;

      add_elt_to_list (args, flags, fw, prec, type, modifier);

      break;

    default:
      m_nconv = -1;
      break;
    }
}

void
printf_format_list::printme (void) const
{
  std::size_t n = m_fmt_elts.size ();

  for (std::size_t i = 0; i < n; i++)
    {
      printf_format_elt *elt = m_fmt_elts[i];

      std::cerr
          << "args:     " << elt->args << "\n"
          << "flags:    '" << elt->flags << "'\n"
          << "width:    " << elt->fw << "\n"
          << "prec:     " << elt->prec << "\n"
          << "type:     '" << elt->type << "'\n"
          << "modifier: '" << elt->modifier << "'\n"
          << "text:     '" << undo_string_escapes (elt->text) << "'\n\n";
    }
}

// Calculate x^n.  Used for ...e+nn so that, for example, 1e2 is
// exactly 100 and 5e-1 is 1/2

static double
pown (double x, unsigned int n)
{
  double retval = 1;

  for (unsigned int d = n; d; d >>= 1)
    {
      if (d & 1)
        retval *= x;
      x *= x;
    }

  return retval;
}

static Cell
init_inf_nan (void)
{
  Cell retval (dim_vector (1, 2));

  retval(0) = Cell (octave_value ("inf"));
  retval(1) = Cell (octave_value ("nan"));

  return retval;
}

// Delimited stream, optimized to read strings of characters separated
// by single-character delimiters.
//
// The reason behind this class is that octstream doesn't provide
// seek/tell, but the opportunity has been taken to optimise for the
// textscan workload.
//
// The function reads chunks into a 4kiB buffer, and marks where the
// last delimiter occurs.  Reads up to this delimiter can be fast.
// After that last delimiter, the remaining text is moved to the front
// of the buffer and the buffer is refilled.  This also allows cheap
// seek and tell operations within a "fast read" block.

class
delimited_stream
{
public:

  delimited_stream (std::istream& is, const std::string& delimiters,
                    int longest_lookahead, octave_idx_type bsize = 4096);

  delimited_stream (std::istream& is, const delimited_stream& ds);

  // No copying!

  delimited_stream (const delimited_stream&) = delete;

  delimited_stream& operator = (const delimited_stream&) = delete;

  ~delimited_stream (void);

  // Called when optimized sequence of get is finished.  Ensures that
  // there is a remaining delimiter in buf, or loads more data in.
  void field_done (void)
  {
    if (m_idx >= m_last)
      refresh_buf ();
  }

  // Load new data into buffer, and set eob, last, idx.
  // Return EOF at end of file, 0 otherwise.
  int refresh_buf (bool initialize = false);

  // Get a character, relying on caller to call field_done if
  // a delimiter has been reached.
  int get (void)
  {
    if (m_delimited)
      return eof () ? std::istream::traits_type::eof () : *m_idx++;
    else
      return get_undelim ();
  }

  // Get a character, checking for underrun of the buffer.
  int get_undelim (void);

  // Read character that will be got by the next get.
  // FIXME: This will not set EOF if delimited stream is at EOF and a peek
  // is attempted.  This does *NOT* behave like C++ input stream.
  // For a compatible peek function, use peek_undelim.  See bug #56917.
  int peek (void)
  { return eof () ? std::istream::traits_type::eof () : *m_idx; }

  // Read character that will be got by the next get.
  int peek_undelim (void);

  // Undo a 'get' or 'get_undelim'.  It is the caller's responsibility
  // to avoid overflow by calling putbacks only for a character got by
  // get() or get_undelim(), with no intervening
  // get, get_delim, field_done, refresh_buf, getline, read or seekg.
  void putback (char /*ch*/ = 0) { if (! eof ()) --m_idx; }

  int getline  (std::string& dest, char delim);

  // int skipline (char delim);

  char * read (char *buffer, int size, char *&new_start);

  // Return a position suitable to "seekg", valid only within this
  // block between calls to field_done.
  char * tellg (void) { return m_idx; }

  void seekg (char *old_idx) { m_idx = old_idx; }

  bool eof (void)
  {
    return (m_eob == m_buf + m_overlap && m_i_stream.eof ())
           || (m_flags & std::ios_base::eofbit);
  }

  operator const void *(void)
  { return (! eof () && ! m_flags) ? this : nullptr; }

  bool fail (void) { return m_flags & std::ios_base::failbit; }

  std::ios_base::iostate rdstate (void) { return m_flags; }

  void setstate (std::ios_base::iostate m) { m_flags = m_flags | m; }

  void clear (std::ios_base::iostate m
              = (std::ios_base::eofbit & ~std::ios_base::eofbit))
  {
    m_flags = m_flags & m;
  }

  // Report if any characters have been consumed.
  // (get, read, etc. not cancelled by putback or seekg)

  void progress_benchmark (void) { m_progress_marker = m_idx; }

  bool no_progress (void) { return m_progress_marker == m_idx; }

  // Number of characters remaining until end of stream if it is already
  // buffered. int_max otherwise.

  std::ptrdiff_t remaining (void)
  {
    if (m_eob < m_buf + m_bufsize)
      return m_eob - m_idx;
    else
      return std::numeric_limits<std::ptrdiff_t>::max ();
  }

private:

  // Number of characters to read from the file at once.
  int m_bufsize;

  // Stream to read from.
  std::istream& m_i_stream;

  // Temporary storage for a "chunk" of data.
  char *m_buf;

  // Current read pointer.
  char *m_idx;

  // Location of last delimiter in the buffer at buf (undefined if
  // delimited is false).
  char *m_last;

  // Position after last character in buffer.
  char *m_eob;

  // Overlap with old content when refreshing buffer.
  std::ptrdiff_t m_overlap;

  // True if there is delimiter in the buffer after idx.
  bool m_delimited;

  // Longest lookahead required.
  int m_longest;

  // Sequence of single-character delimiters.
  const std::string m_delims;

  // Position of start of buf in original stream.
  std::streampos m_buf_in_file;

  // Marker to see if a read consumes any characters.
  char *m_progress_marker;

  std::ios_base::iostate m_flags;
};

// Create a delimited stream, reading from is, with delimiters delims,
// and allowing reading of up to tellg + longest_lookeahead.  When is
// is at EOF, lookahead may be padded by ASCII nuls.

delimited_stream::delimited_stream (std::istream& is,
                                    const std::string& delimiters,
                                    int longest_lookahead,
                                    octave_idx_type bsize)
  : m_bufsize (bsize), m_i_stream (is), m_longest (longest_lookahead),
    m_delims (delimiters),
    m_flags (std::ios::failbit & ~std::ios::failbit) // can't cast 0
{
  m_buf = new char[m_bufsize];
  m_eob = m_buf + m_bufsize;
  m_idx = m_eob;                // refresh_buf shouldn't try to copy old data
  m_progress_marker = m_idx;
  refresh_buf (true);           // load the first batch of data
}

// Used to create a stream from a strstream from data read from a dstr.
delimited_stream::delimited_stream (std::istream& is,
                                    const delimited_stream& ds)
  : delimited_stream (is, ds.m_delims, ds.m_longest, ds.m_bufsize)
{ }

delimited_stream::~delimited_stream (void)
{
  // Seek to the correct position in i_stream.
  if (! eof ())
    {
      m_i_stream.clear ();
      m_i_stream.seekg (m_buf_in_file);
      m_i_stream.read (m_buf, m_idx - m_buf - m_overlap);
    }

  delete [] m_buf;
}

// Read a character from the buffer, refilling the buffer from the file
// if necessary.

int
delimited_stream::get_undelim (void)
{
  int retval;
  if (eof ())
    {
      setstate (std::ios_base::failbit);
      return std::istream::traits_type::eof ();
    }

  if (m_idx < m_eob)
    retval = *m_idx++;
  else
    {
      refresh_buf ();

      if (eof ())
        {
          setstate (std::ios_base::eofbit);
          retval = std::istream::traits_type::eof ();
        }
      else
        retval = *m_idx++;
    }

  if (m_idx >= m_last)
    m_delimited = false;

  return retval;
}

// Return the next character to be read without incrementing the
// pointer, refilling the buffer from the file if necessary.

int
delimited_stream::peek_undelim (void)
{
  int retval = get_undelim ();
  putback ();

  return retval;
}

// Copy remaining unprocessed data to the start of the buffer and load
// new data to fill it.  Return EOF if the file is at EOF before
// reading any data and all of the data that has been read has been
// processed.

int
delimited_stream::refresh_buf (bool initialize)
{
  if (eof ())
    return std::istream::traits_type::eof ();

  int retval;

  if (m_eob < m_idx)
    m_idx = m_eob;

  std::size_t old_remaining = m_eob - m_idx;
  std::size_t old_overlap = 0;

  if (initialize || (m_idx - m_buf <= 0))
    m_overlap = 0;
  else
    {
      old_overlap = m_overlap;
      // Retain the last 25 bytes in the buffer.  That should be more than enough
      // to putback an entire double precision floating point number in decimal
      // including 3 digit exponent and signs.  Do we ever need to putback more
      // than that?
      m_overlap = 25;
      // Assure we don't "underflow" with the overlap
      m_overlap = std::min (m_overlap, m_idx - m_buf - 1);
    }

  octave_quit ();                       // allow ctrl-C

  if (old_remaining + m_overlap > 0)
    {
      m_buf_in_file += (m_idx - old_overlap - m_buf);
      std::memmove (m_buf, m_idx - m_overlap, m_overlap + old_remaining);
    }
  else
    m_buf_in_file = m_i_stream.tellg ();  // record for destructor

  // where original idx would have been
  m_progress_marker -= m_idx - m_overlap - m_buf;
  m_idx = m_buf + m_overlap;

  int gcount;   // chars read
  if (! m_i_stream.eof ())
    {
      m_i_stream.read (m_buf + m_overlap + old_remaining,
                       m_bufsize - m_overlap - old_remaining);
      gcount = m_i_stream.gcount ();
    }
  else
    gcount = 0;

  m_eob = m_buf + m_overlap + old_remaining + gcount;
  m_last = m_eob;
  if (gcount == 0)
    {
      m_delimited = false;

      if (m_eob != m_buf + m_overlap)
        // no more data in file, but still some to go
        retval = 0;
      else
        // file and buffer are both done.
        retval = std::istream::traits_type::eof ();
    }
  else
    {
      m_delimited = true;

      for (m_last = m_eob - m_longest; m_last - m_buf - m_overlap >= 0;
           m_last--)
        {
          if (m_delims.find (*m_last) != std::string::npos)
            break;
        }

      if (m_last < m_buf + m_overlap)
        m_delimited = false;

      retval = 0;
    }

  // Ensure fast peek doesn't give valid char
  if (retval == std::istream::traits_type::eof ())
    *m_idx = '\0';    // FIXME: check that no TreatAsEmpty etc starts w. \0?

  return retval;
}

// Return a pointer to a block of data of size size, assuming that a
// sufficiently large buffer is available in buffer, if required.
// If called when delimited == true, and size is no greater than
// longest_lookahead then this will not call refresh_buf, so seekg
// still works.  Otherwise, seekg may be invalidated.

char *
delimited_stream::read (char *buffer, int size, char *&prior_tell)
{
  char *retval;

  if (m_eob - m_idx >= size)
    {
      retval = m_idx;
      m_idx += size;
      if (m_idx > m_last)
        m_delimited = false;
    }
  else
    {
      // If there was a tellg pointing to an earlier point than the current
      // read position, try to keep it in the active buffer.
      // In the current code, prior_tell==idx for each call,
      // so this is not necessary, just a precaution.

      if (m_eob - prior_tell + size < m_bufsize)
        {
          octave_idx_type gap = m_idx - prior_tell;
          m_idx = prior_tell;
          refresh_buf ();
          m_idx += gap;
        }
      else      // can't keep the tellg in range.  May skip some data.
        {
          refresh_buf ();
        }

      prior_tell = m_buf;

      if (m_eob - m_idx > size)
        {
          retval = m_idx;
          m_idx += size;
          if (m_idx > m_last)
            m_delimited = false;
        }
      else
        {
          if (size <= m_bufsize)          // small read, but reached EOF
            {
              retval = m_idx;
              memset (m_eob, 0, size + (m_idx - m_buf));
              m_idx += size;
            }
          else  // Reading more than the whole buf; return it in buffer
            {
              retval = buffer;
              // FIXME: read bufsize at a time
              int i;
              for (i = 0; i < size && ! eof (); i++)
                *buffer++ = get_undelim ();
              if (eof ())
                memset (buffer, 0, size - i);
            }
        }
    }

  return retval;
}

// Return in OUT an entire line, terminated by delim.  On input, OUT
// must have length at least 1.

int
delimited_stream::getline (std::string& out, char delim)
{
  int len = out.length ();
  int used = 0;
  int ch;
  while ((ch = get_undelim ()) != delim
         && ch != std::istream::traits_type::eof ())
    {
      out[used++] = ch;
      if (used == len)
        {
          len <<= 1;
          out.resize (len);
        }
    }
  out.resize (used);
  field_done ();

  return ch;
}

// A single conversion specifier, such as %f or %c.

class
textscan_format_elt
{
public:

  enum special_conversion
  {
    whitespace_conversion = 1,
    literal_conversion = 2
  };

  textscan_format_elt (const std::string& txt, int w = 0, int p = -1,
                       int bw = 0, bool dis = false, char typ = '\0',
                       const std::string& ch_class = std::string ())
    : text (txt), width (w), prec (p), bitwidth (bw),
      char_class (ch_class), type (typ), discard (dis),
      numeric (typ == 'd' || typ == 'u' || type == 'f' || type == 'n')
  { }

  textscan_format_elt (const textscan_format_elt& e)
    : text (e.text), width (e.width), prec (e.prec),
      bitwidth (e.bitwidth), char_class (e.char_class), type (e.type),
      discard (e.discard), numeric (e.numeric)
  { }

  textscan_format_elt& operator = (const textscan_format_elt& e)
  {
    if (this != &e)
      {
        text = e.text;
        width = e.width;
        prec = e.prec;
        bitwidth = e.bitwidth;
        discard = e.discard;
        type = e.type;
        numeric = e.numeric;
        char_class = e.char_class;
      }

    return *this;
  }

  // The C-style format string.
  std::string text;

  // The maximum field width.
  unsigned int width;

  // The maximum number of digits to read after the decimal in a
  // floating point conversion.
  int prec;

  // The size of the result.  For integers, bitwidth may be 8, 16, 34,
  // or 64.  For floating point values, bitwidth may be 32 or 64.
  int bitwidth;

  // The class of characters in a '[' or '^' format.
  std::string char_class;

  // Type of conversion
  //  -- 'd', 'u', 'f', 'n', 's', 'q', 'c', '%', 'C', 'D', '[' or '^'.
  char type;

  // TRUE if we are not storing the result of this conversion.
  bool discard;

  // TRUE if the type is 'd', 'u', 'f', 'n'
  bool numeric;
};

// The (parsed) sequence of format specifiers.

class textscan;

class
textscan_format_list
{
public:

  textscan_format_list (const std::string& fmt = std::string (),
                        const std::string& who = "textscan");
  // No copying!

  textscan_format_list (const textscan_format_list&) = delete;

  textscan_format_list& operator = (const textscan_format_list&) = delete;

  ~textscan_format_list (void);

  octave_idx_type num_conversions (void) const { return m_nconv; }

  // The length can be different than the number of conversions.
  // For example, "x %d y %d z" has 2 conversions but the length of
  // the list is 3 because of the characters that appear after the
  // last conversion.

  std::size_t numel (void) const { return m_fmt_elts.size (); }

  const textscan_format_elt * first (void)
  {
    m_curr_idx = 0;
    return current ();
  }

  const textscan_format_elt * current (void) const
  {
    return numel () > 0 ? m_fmt_elts[m_curr_idx] : nullptr;
  }

  const textscan_format_elt * next (bool cycle = true)
  {
    m_curr_idx++;

    if (m_curr_idx >= numel ())
      {
        if (cycle)
          m_curr_idx = 0;
        else
          return nullptr;
      }

    return current ();
  }

  void printme (void) const;

  bool ok (void) const { return (m_nconv >= 0); }

  operator const void *(void) const { return ok () ? this : nullptr; }

  // What function name should be shown when reporting errors.
  std::string who;

  // True if number of %f to be set from data file.
  bool set_from_first;

  // At least one conversion specifier is s,q,c, or [...].
  bool has_string;

  int read_first_row (delimited_stream& is, textscan& ts);

  std::list<octave_value> out_buf (void) const
  { return (m_output_container); }

private:

  void add_elt_to_list (unsigned int width, int prec, int bitwidth,
                        octave_value val_type, bool discard,
                        char type,
                        const std::string& char_class = std::string ());

  void process_conversion (const std::string& s, std::size_t& i,
                           std::size_t n);

  std::string parse_char_class (const std::string& pattern) const;

  int finish_conversion (const std::string& s, std::size_t& i, std::size_t n,
                         unsigned int width, int prec, int bitwidth,
                         octave_value& val_type,
                         bool discard, char& type);

  //--------

  // Number of conversions specified by this format string, or -1 if
  // invalid conversions have been found.
  octave_idx_type m_nconv;

  // Index to current element;
  std::size_t m_curr_idx;

  // List of format elements.
  std::deque<textscan_format_elt *> m_fmt_elts;

  // list holding column arrays of types specified by conversions
  std::list<octave_value> m_output_container;

  // Temporary buffer.
  std::ostringstream m_buf;

};

// Main class to implement textscan.  Read data and parse it
// according to a format.
//
// The calling sequence is
//
//   textscan scanner ();
//   scanner.scan (...);

class
OCTINTERP_API
textscan
{
public:

  textscan (const std::string& who_arg = "textscan",
            const std::string& encoding = "utf-8");

  // No copying!

  textscan (const textscan&) = delete;

  textscan& operator = (const textscan&) = delete;

  ~textscan (void) = default;

  octave_value scan (std::istream& isp, const std::string& fmt,
                     octave_idx_type ntimes,
                     const octave_value_list& options,
                     octave_idx_type& read_count);

private:

  friend class textscan_format_list;

  octave_value do_scan (std::istream& isp, textscan_format_list& fmt_list,
                        octave_idx_type ntimes);

  void parse_options (const octave_value_list& args,
                      textscan_format_list& fmt_list);

  int read_format_once (delimited_stream& isp, textscan_format_list& fmt_list,
                        std::list<octave_value>& retval,
                        Array<octave_idx_type> row, int& done_after);

  void scan_one (delimited_stream& is, const textscan_format_elt& fmt,
                 octave_value& ov, Array<octave_idx_type> row);

  // Methods to process a particular conversion specifier.
  double read_double (delimited_stream& is,
                      const textscan_format_elt& fmt) const;

  void scan_complex (delimited_stream& is, const textscan_format_elt& fmt,
                     Complex& val) const;

  int scan_bracket (delimited_stream& is, const std::string& pattern,
                    std::string& val) const;

  int scan_caret (delimited_stream& is, const std::string& pattern,
                  std::string& val) const;

  void scan_string (delimited_stream& is, const textscan_format_elt& fmt,
                    std::string& val) const;

  void scan_cstring (delimited_stream& is, const textscan_format_elt& fmt,
                     std::string& val) const;

  void scan_qstring (delimited_stream& is, const textscan_format_elt& fmt,
                     std::string& val);

  // Helper methods.
  std::string read_until (delimited_stream& is, const Cell& delimiters,
                          const std::string& ends) const;

  int lookahead (delimited_stream& is, const Cell& targets, int max_len,
                 bool case_sensitive = true) const;

  bool match_literal (delimited_stream& isp, const textscan_format_elt& elem);

  int skip_whitespace (delimited_stream& is, bool EOLstop = true);

  int skip_delim (delimited_stream& is);

  bool is_delim (unsigned char ch) const
  {
    return ((m_delim_table.empty ()
             && (isspace (ch) || ch == m_eol1 || ch == m_eol2))
            || m_delim_table[ch] != '\0');
  }

  bool isspace (unsigned int ch) const
  { return m_whitespace_table[ch & 0xff]; }

  // True if the only delimiter is whitespace.
  bool whitespace_delim (void) const { return m_delim_table.empty (); }

  //--------

  // What function name should be shown when reporting errors.
  std::string m_who;

  std::string m_encoding;

  std::string m_buf;

  // Three cases for delim_table and delim_list
  // 1. delim_table empty, delim_list empty:  whitespace delimiters
  // 2. delim_table = look-up table of delim chars, delim_list empty.
  // 3. delim_table non-empty, delim_list = Cell array of delim strings

  std::string m_whitespace_table;

  // delim_table[i] == '\0' if i is not a delimiter.
  std::string m_delim_table;

  // String of delimiter characters.
  std::string m_delims;

  Cell m_comment_style;

  // How far ahead to look to detect an open comment.
  int m_comment_len;

  // First character of open comment.
  int m_comment_char;

  octave_idx_type m_buffer_size;

  std::string m_date_locale;

  // 'inf' and 'nan' for formatted_double.
  Cell m_inf_nan;

  // Array of strings of delimiters.
  Cell m_delim_list;

  // Longest delimiter.
  int m_delim_len;

  octave_value m_empty_value;
  std::string m_exp_chars;
  int m_header_lines;
  Cell m_treat_as_empty;

  // Longest string to treat as "N/A".
  int m_treat_as_empty_len;

  std::string m_whitespace;

  short m_eol1;
  short m_eol2;
  short m_return_on_error;

  bool m_collect_output;
  bool multiple_delims_as_one;
  bool m_default_exp;

  octave_idx_type m_lines;
};

textscan_format_list::textscan_format_list (const std::string& s,
    const std::string& who_arg)
  : who (who_arg), set_from_first (false), has_string (false),
    m_nconv (0), m_curr_idx (0), m_fmt_elts (), m_buf ()
{
  std::size_t n = s.length ();

  std::size_t i = 0;

  unsigned int width = -1;    // Unspecified width = max (except %c)
  int prec = -1;
  int bitwidth = 0;
  bool discard = false;
  char type = '\0';

  bool have_more = true;

  if (s.empty ())
    {
      m_buf.clear ();
      m_buf.str ("");

      m_buf << "%f";

      bitwidth = 64;
      type = 'f';
      add_elt_to_list (width, prec, bitwidth, octave_value (NDArray ()),
                       discard, type);
      have_more = false;
      set_from_first = true;
      m_nconv = 1;
    }
  else
    {
      set_from_first = false;

      while (i < n)
        {
          have_more = true;

          if (s[i] == '%' && (i+1 == n || s[i+1] != '%'))
            {
              // Process percent-escape conversion type.

              process_conversion (s, i, n);

              // If there is nothing in the buffer, then add_elt_to_list
              // must have just been called, so we are already done with
              // the current element and we don't need to call
              // add_elt_to_list if this is our last trip through the
              // loop.

              have_more = (m_buf.tellp () != 0);
            }
          else if (isspace (s[i]))
            {
              while (++i < n && isspace (s[i]))
                /* skip whitespace */;

              have_more = false;
            }
          else
            {
              type = textscan_format_elt::literal_conversion;

              width = 0;
              prec = -1;
              bitwidth = 0;
              discard = true;

              while (i < n && ! isspace (s[i])
                     && (s[i] != '%' || (i+1 < n && s[i+1] == '%')))
                {
                  if (s[i] == '%')      // if double %, skip one
                    i++;
                  m_buf << s[i++];
                  width++;
                }

              add_elt_to_list (width, prec, bitwidth, octave_value (),
                               discard, type);

              have_more = false;
            }

          if (m_nconv < 0)
            {
              have_more = false;
              break;
            }
        }
    }

  if (have_more)
    add_elt_to_list (width, prec, bitwidth, octave_value (), discard, type);

  m_buf.clear ();
  m_buf.str ("");
}

textscan_format_list::~textscan_format_list (void)
{
  std::size_t n = numel ();

  for (std::size_t i = 0; i < n; i++)
    {
      textscan_format_elt *elt = m_fmt_elts[i];
      delete elt;
    }
}

void
textscan_format_list::add_elt_to_list (unsigned int width, int prec,
                                       int bitwidth, octave_value val_type,
                                       bool discard, char type,
                                       const std::string& char_class)
{
  std::string text = m_buf.str ();

  if (! text.empty ())
    {
      textscan_format_elt *elt
        = new textscan_format_elt (text, width, prec, bitwidth, discard,
                                   type, char_class);

      if (! discard)
        m_output_container.push_back (val_type);

      m_fmt_elts.push_back (elt);
    }

  m_buf.clear ();
  m_buf.str ("");
}

void
textscan_format_list::process_conversion (const std::string& s,
    std::size_t& i, std::size_t n)
{
  unsigned width = 0;
  int prec = -1;
  int bitwidth = 0;
  bool discard = false;
  octave_value val_type;
  char type = '\0';

  m_buf << s[i++];

  bool have_width = false;

  while (i < n)
    {
      switch (s[i])
        {
        case '*':
          if (discard)
            m_nconv = -1;
          else
            {
              discard = true;
              m_buf << s[i++];
            }
          break;

        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
          if (have_width)
            m_nconv = -1;
          else
            {
              char c = s[i++];
              width = width * 10 + c - '0';
              have_width = true;
              m_buf << c;
              while (i < n && isdigit (s[i]))
                {
                  c = s[i++];
                  width = width * 10 + c - '0';
                  m_buf << c;
                }

              if (i < n && s[i] == '.')
                {
                  m_buf << s[i++];
                  prec = 0;
                  while (i < n && isdigit (s[i]))
                    {
                      c = s[i++];
                      prec = prec * 10 + c - '0';
                      m_buf << c;
                    }
                }
            }
          break;

        case 'd': case 'u':
          {
            bool done = true;
            m_buf << (type = s[i++]);
            if (i < n)
              {
                if (s[i] == '8')
                  {
                    bitwidth = 8;
                    if (type == 'd')
                      val_type = octave_value (int8NDArray ());
                    else
                      val_type = octave_value (uint8NDArray ());
                    m_buf << s[i++];
                  }
                else if (s[i] == '1' && i+1 < n && s[i+1] == '6')
                  {
                    bitwidth = 16;
                    if (type == 'd')
                      val_type = octave_value (int16NDArray ());
                    else
                      val_type = octave_value (uint16NDArray ());
                    m_buf << s[i++];
                    m_buf << s[i++];
                  }
                else if (s[i] == '3' && i+1 < n && s[i+1] == '2')
                  {
                    done = false;       // use default size below
                    m_buf << s[i++];
                    m_buf << s[i++];
                  }
                else if (s[i] == '6' && i+1 < n && s[i+1] == '4')
                  {
                    bitwidth = 64;
                    if (type == 'd')
                      val_type = octave_value (int64NDArray ());
                    else
                      val_type = octave_value (uint64NDArray ());
                    m_buf << s[i++];
                    m_buf << s[i++];
                  }
                else
                  done = false;
              }
            else
              done = false;

            if (! done)
              {
                bitwidth = 32;
                if (type == 'd')
                  val_type = octave_value (int32NDArray ());
                else
                  val_type = octave_value (uint32NDArray ());
              }
            goto fini;
          }

        case 'f':
          m_buf << (type = s[i++]);
          bitwidth = 64;
          if (i < n)
            {
              if (s[i] == '3' && i+1 < n && s[i+1] == '2')
                {
                  bitwidth = 32;
                  val_type = octave_value (FloatNDArray ());
                  m_buf << s[i++];
                  m_buf << s[i++];
                }
              else if (s[i] == '6' && i+1 < n && s[i+1] == '4')
                {
                  val_type = octave_value (NDArray ());
                  m_buf << s[i++];
                  m_buf << s[i++];
                }
              else
                val_type = octave_value (NDArray ());
            }
          else
            val_type = octave_value (NDArray ());
          goto fini;

        case 'n':
          m_buf << (type = s[i++]);
          bitwidth = 64;
          val_type = octave_value (NDArray ());
          goto fini;

        case 's': case 'q': case '[': case 'c':
          if (! discard)
            val_type = octave_value (Cell ());
          m_buf << (type = s[i++]);
          has_string = true;
          goto fini;

        fini:
          {
            if (! have_width)
              {
                if (type == 'c')        // %c defaults to one character
                  width = 1;
                else
                  width = static_cast<unsigned int> (-1); // others: unlimited
              }

            if (finish_conversion (s, i, n, width, prec, bitwidth, val_type,
                                   discard, type) == 0)
              return;
          }
          break;

        default:
          error ("%s: '%%%c' is not a valid format specifier",
                 who.c_str (), s[i]);
        }

      if (m_nconv < 0)
        break;
    }

  m_nconv = -1;
}

// Parse [...] and [^...]
//
// Matlab does not expand expressions like A-Z, but they are useful, and
// so we parse them "carefully".  We treat '-' as a usual character
// unless both start and end characters are from the same class (upper
// case, lower case, numeric), or this is not the first '-' in the
// pattern.
//
// Keep both a running list of characters and a mask of which chars have
// occurred.  The first is efficient for patterns with few characters.
// The latter is efficient for [^...] patterns.

std::string
textscan_format_list::parse_char_class (const std::string& pattern) const
{
  int len = pattern.length ();
  if (len == 0)
    return "";

  std::string retval (256, '\0');
  std::string mask   (256, '\0');       // number of times chr has been seen

  int in = 0, out = 0;
  unsigned char ch, prev = 0;
  bool flip = false;

  ch = pattern[in];
  if (ch == '^')
    {
      in++;
      flip = true;
    }
  mask[pattern[in]] = '\1';
  retval[out++] = pattern[in++];        // even copy ']' if it is first

  bool prev_was_range = false;          // disallow "a-m-z" as a pattern
  bool prev_prev_was_range = false;
  for (; in < len; in++)
    {
      bool was_range = false;
      ch = pattern[in];
      if (ch == ']')
        break;

      if (prev == '-' && in > 1 && isalnum (ch) && ! prev_prev_was_range)
        {
          unsigned char start_of_range = pattern[in-2];
          if (start_of_range < ch
              && ((isupper (ch) && isupper (start_of_range))
                  || (islower (ch) && islower (start_of_range))
                  || (isdigit (ch) && isdigit (start_of_range))
                  || mask['-'] > 1))    // not the first '-'
            {
              was_range = true;
              out--;
              mask['-']--;
              for (int i = start_of_range; i <= ch; i++)
                {
                  if (mask[i] == '\0')
                    {
                      mask[i] = '\1';
                      retval[out++] = i;
                    }
                }
            }
        }
      if (! was_range)
        {
          if (mask[ch]++ == 0)
            retval[out++] = ch;
          else if (ch != '-')
            warning_with_id ("Octave:textscan-pattern",
                             "%s: [...] contains two '%c's",
                             who.c_str (), ch);

          if (prev == '-' && mask['-'] >= 2)
            warning_with_id
            ("Octave:textscan-pattern",
             "%s: [...] contains two '-'s outside range expressions",
             who.c_str ());
        }
      prev = ch;
      prev_prev_was_range = prev_was_range;
      prev_was_range = was_range;
    }

  if (flip)                             // [^...]
    {
      out = 0;
      for (int i = 0; i < 256; i++)
        if (! mask[i])
          retval[out++] = i;
    }

  retval.resize (out);

  return retval;
}

int
textscan_format_list::finish_conversion (const std::string& s, std::size_t& i,
    std::size_t n, unsigned int width,
    int prec, int bitwidth,
    octave_value& val_type, bool discard,
    char& type)
{
  int retval = 0;

  std::string char_class;

  std::size_t beg_idx = std::string::npos;
  std::size_t end_idx = std::string::npos;

  if (type != '%')
    {
      m_nconv++;
      if (type == '[')
        {
          if (i < n)
            {
              beg_idx = i;

              if (s[i] == '^')
                {
                  type = '^';
                  m_buf << s[i++];

                  if (i < n)
                    {
                      beg_idx = i;

                      if (s[i] == ']')
                        m_buf << s[i++];
                    }
                }
              else if (s[i] == ']')
                m_buf << s[i++];
            }

          while (i < n && s[i] != ']')
            m_buf << s[i++];

          if (i < n && s[i] == ']')
            {
              end_idx = i-1;
              m_buf << s[i++];
            }

          if (s[i-1] != ']')
            retval = m_nconv = -1;
        }
    }

  if (m_nconv >= 0)
    {
      if (beg_idx != std::string::npos && end_idx != std::string::npos)
        char_class = parse_char_class (s.substr (beg_idx,
                                       end_idx - beg_idx + 1));

      add_elt_to_list (width, prec, bitwidth, val_type, discard, type,
                       char_class);
    }

  return retval;
}

void
textscan_format_list::printme (void) const
{
  std::size_t n = numel ();

  for (std::size_t i = 0; i < n; i++)
    {
      textscan_format_elt *elt = m_fmt_elts[i];

      std::cerr
          << "width:      " << elt->width << "\n"
          << "digits      " << elt->prec << "\n"
          << "bitwidth:   " << elt->bitwidth << "\n"
          << "discard:    " << elt->discard << "\n"
          << "type:       ";

      if (elt->type == textscan_format_elt::literal_conversion)
        std::cerr << "literal text\n";
      else if (elt->type == textscan_format_elt::whitespace_conversion)
        std::cerr << "whitespace\n";
      else
        std::cerr << elt->type << "\n";

      std::cerr
          << "char_class: '" << undo_string_escapes (elt->char_class) << "'\n"
          << "text:       '" << undo_string_escapes (elt->text) << "'\n\n";
    }
}

// If FORMAT is explicitly "", it is assumed to be "%f" repeated enough
// times to read the first row of the file.  Set it now.

int
textscan_format_list::read_first_row (delimited_stream& is, textscan& ts)
{
  // Read first line and strip end-of-line, which may be two characters
  std::string first_line (20, ' ');

  is.getline (first_line, static_cast<char> (ts.m_eol2));

  if (! first_line.empty () && first_line.back () == ts.m_eol1)
    first_line.pop_back ();

  std::istringstream strstr (first_line);
  delimited_stream ds (strstr, is);

  dim_vector dv (1, 1);     // initial size of each output_container
  Complex val;
  octave_value val_type;
  m_nconv = 0;
  int max_empty = 1000;     // failsafe, if ds fails but not with eof
  int retval = 0;

  // read line, creating output_container as we go
  while (! ds.eof ())
    {
      bool already_skipped_delim = false;
      ts.skip_whitespace (ds, false);
      ds.progress_benchmark ();
      ts.scan_complex (ds, *m_fmt_elts[0], val);
      if (ds.fail ())
        {
          ds.clear (ds.rdstate () & ~std::ios::failbit);

          if (ds.eof ())
            break;

          // Unless this was a missing value (i.e., followed by a delimiter),
          // return with an error status.
          ts.skip_delim (ds);
          if (ds.no_progress ())
            {
              retval = 4;
              break;
            }
          already_skipped_delim = true;

          val = ts.m_empty_value.scalar_value ();

          if (! --max_empty)
            break;
        }

      if (val.imag () == 0)
        val_type = octave_value (NDArray (dv, val.real ()));
      else
        val_type = octave_value (ComplexNDArray (dv, val));

      m_output_container.push_back (val_type);

      if (! already_skipped_delim)
        ts.skip_delim (ds);

      if (ds.no_progress ())
        break;

      m_nconv++;
    }

  m_output_container.pop_front (); // discard empty element from constructor

  // Create fmt_list now that the size is known
  for (octave_idx_type i = 1; i < m_nconv; i++)
    m_fmt_elts.push_back (new textscan_format_elt (*m_fmt_elts[0]));

  return retval;             // May have returned 4 above.
}

textscan::textscan (const std::string& who_arg, const std::string& encoding)
  : m_who (who_arg), m_encoding (encoding), m_buf (), m_whitespace_table (),
    m_delim_table (), m_delims (), m_comment_style (), m_comment_len (0),
    m_comment_char (-2), m_buffer_size (0), m_date_locale (),
    m_inf_nan (init_inf_nan ()),
    m_empty_value (numeric_limits<double>::NaN ()),
    m_exp_chars ("edED"), m_header_lines (0), m_treat_as_empty (),
    m_treat_as_empty_len (0), m_whitespace (" \b\t"), m_eol1 ('\r'),
    m_eol2 ('\n'), m_return_on_error (1), m_collect_output (false),
    multiple_delims_as_one (false), m_default_exp (true), m_lines (0)
{ }

octave_value
textscan::scan (std::istream& isp, const std::string& fmt,
                octave_idx_type ntimes, const octave_value_list& options,
                octave_idx_type& count)
{
  textscan_format_list fmt_list (fmt);

  parse_options (options, fmt_list);

  octave_value result = do_scan (isp, fmt_list, ntimes);

  // FIXME: this is probably not the best way to get count.  The
  // position could easily be larger than octave_idx_type when using
  // 32-bit indexing.

  std::ios::iostate state = isp.rdstate ();
  isp.clear ();
  count = static_cast<octave_idx_type> (isp.tellg ());
  isp.setstate (state);

  return result;
}

octave_value
textscan::do_scan (std::istream& isp, textscan_format_list& fmt_list,
                   octave_idx_type ntimes)
{
  octave_value retval;

  if (fmt_list.num_conversions () == -1)
    error ("%s: invalid format specified", m_who.c_str ());

  if (fmt_list.num_conversions () == 0)
    error ("%s: no valid format conversion specifiers", m_who.c_str ());

  // skip the first header_lines
  std::string dummy;
  for (int i = 0; i < m_header_lines && isp; i++)
    getline (isp, dummy, static_cast<char> (m_eol2));

  // Create our own buffered stream, for fast get/putback/tell/seek.

  // First, see how far ahead it should let us look.
  int max_lookahead = std::max ({m_comment_len, m_treat_as_empty_len,
                                 m_delim_len, 3});  // 3 for NaN and Inf

  // Next, choose a buffer size to avoid reading too much, or too often.
  octave_idx_type buf_size = 4096;
  if (m_buffer_size)
    buf_size = m_buffer_size;
  else if (ntimes > 0)
    {
      // Avoid overflow of 80*ntimes...
      buf_size = std::min (buf_size, std::max (ntimes, 80 * ntimes));
      buf_size = std::max (buf_size, ntimes);
    }
  // Finally, create the stream.
  delimited_stream is (isp,
                       (m_delims.empty () ? m_whitespace + "\r\n"
                        : m_delims),
                       max_lookahead, buf_size);

  // Grow retval dynamically.  "size" is half the initial size
  // (FIXME: Should we start smaller if ntimes is large?)
  octave_idx_type size = ((ntimes < 8 && ntimes >= 0) ? ntimes : 1);
  Array<octave_idx_type> row_idx (dim_vector (1, 2));
  row_idx(1) = 0;

  int err = 0;
  octave_idx_type row = 0;

  if (multiple_delims_as_one)           // bug #44750?
    skip_delim (is);

  int done_after;  // Number of columns read when EOF seen.

  // If FORMAT explicitly "", read first line and see how many "%f" match
  if (fmt_list.set_from_first)
    {
      err = fmt_list.read_first_row (is, *this);
      m_lines = 1;

      done_after = fmt_list.numel () + 1;
      if (! err)
        row = 1;  // the above puts the first line into fmt_list.out_buf ()
    }
  else
    done_after = fmt_list.out_buf ().size () + 1;

  std::list<octave_value> out = fmt_list.out_buf ();

  // We will later merge adjacent columns of the same type.
  // Check now which columns to merge.
  // Reals may become complex, and so we can't trust types
  // after reading in data.
  // If the format was "", that conversion may already have happened,
  // so force all to be merged (as all are %f).
  bool merge_with_prev[fmt_list.numel ()];
  int conv = 0;
  if (m_collect_output)
    {
      int prev_type = -1;
      for (const auto& col : out)
        {
          if (col.type_id () == prev_type
              || (fmt_list.set_from_first && prev_type != -1))
            merge_with_prev[conv++] = true;
          else
            merge_with_prev[conv++] = false;

          prev_type = col.type_id ();
        }
    }

  // This should be caught by earlier code, but this avoids a possible
  // infinite loop below.
  if (fmt_list.num_conversions () == 0)
    error ("%s: No conversions specified", m_who.c_str ());

  // Read the data.  This is the main loop.
  if (! err)
    {
      for (/* row set ~30 m_lines above */;
                                          row < ntimes || ntimes == -1;
                                          row++)
        {
          if (row == 0 || row >= size)
            {
              size += (size+1);
              for (auto& col : out)
                col = col.resize (dim_vector (size, 1), 0);
            }

          row_idx(0) = row;
          err = read_format_once (is, fmt_list, out, row_idx, done_after);

          if ((err & ~1) > 0 || ! is || (m_lines >= ntimes && ntimes > -1))
            break;
        }
    }

  if ((err & 4) && ! m_return_on_error)
    error ("%s: Read error in field %d of row %" OCTAVE_IDX_TYPE_FORMAT,
           m_who.c_str (), done_after + 1, row + 1);

  // If file does not end in EOL, do not pad columns with NaN.
  bool uneven_columns = false;
  if (err & 4)
    uneven_columns = true;
  else if (isp.eof ())
    {
      isp.clear ();
      isp.seekg (-1, std::ios_base::end);
      int last_char = isp.get ();
      isp.setstate (isp.eofbit);
      uneven_columns = (last_char != m_eol1 && last_char != m_eol2);
    }

  // convert return value to Cell array
  Array<octave_idx_type> ra_idx (dim_vector (1, 2));

  // (err & 1) means "error, and no columns read this row
  // FIXME: This may redundant now that done_after=0 says the same
  if (err & 1)
    done_after = out.size () + 1;

  int valid_rows = (row == ntimes
                    ? ntimes
                    : ((err & 1) && (err & 8)) ? row : row+1);
  dim_vector dv (valid_rows, 1);

  ra_idx(0) = 0;
  int i = 0;
  if (! m_collect_output)
    {
      retval = Cell (dim_vector (1, out.size ()));
      for (auto& col : out)
        {
          // trim last columns if that was requested
          if (i == done_after && uneven_columns)
            dv = dim_vector (std::max (valid_rows - 1, 0), 1);

          ra_idx(1) = i;
          retval = cat_op (retval, octave_value (Cell (col.resize (dv, 0))),
                           ra_idx);
          i++;
        }
    }
  else  // group adjacent cells of the same type into a single cell
    {
      octave_value cur;                // current cell, accumulating columns
      octave_idx_type group_size = 0;  // columns in this cell
      int prev_type = -1;

      conv = 0;
      retval = Cell ();
      for (auto& col : out)
        {
          if (! merge_with_prev[conv++])  // including first time
            {
              if (prev_type != -1)
                {
                  ra_idx(1) = i++;
                  retval = cat_op (retval, octave_value (Cell (cur)), ra_idx);
                }
              cur = octave_value (col.resize (dv, 0));
              group_size = 1;
              prev_type = col.type_id ();
            }
          else
            {
              ra_idx(1) = group_size++;
              cur = cat_op (cur, octave_value (col.resize (dv, 0)), ra_idx);
            }
        }
      ra_idx(1) = i;
      retval = cat_op (retval, octave_value (Cell (cur)), ra_idx);
    }

  return retval;
}

// Read a double considering the "precision" field of FMT and the
// EXP_CHARS option of OPTIONS.

double
textscan::read_double (delimited_stream& is,
                       const textscan_format_elt& fmt) const
{
  int sign = 1;
  unsigned int width_left = fmt.width;
  double retval = 0;
  bool valid = false;         // syntactically correct double?

  int ch = is.peek_undelim ();

  if (ch == '+')
    {
      is.get ();
      ch = is.peek_undelim ();
      if (width_left)
        width_left--;
    }
  else if (ch == '-')
    {
      sign = -1;
      is.get ();
      ch = is.peek_undelim ();
      if (width_left)
        width_left--;
    }

  // Read integer part
  if (ch != '.')
    {
      if (ch >= '0' && ch <= '9')       // valid if at least one digit
        valid = true;
      while (width_left-- && is && (ch = is.get ()) >= '0' && ch <= '9')
        retval = retval * 10 + (ch - '0');
      width_left++;
    }

  // Read fractional part, up to specified precision
  if (ch == '.' && width_left)
    {
      double multiplier = 1;
      int precision = fmt.prec;
      int i;

      width_left--;                  // Consider width of '.'

      if (precision == -1)
        precision = 1<<30;           // FIXME: Should be MAXINT

      if (! valid)                   // if there was nothing before '.'...
        is.get ();                   // ...ch was a "peek", not "get".

      for (i = 0; i < precision; i++)
        {
          if (width_left-- && is && (ch = is.get ()) >= '0' && ch <= '9')
            retval += (ch - '0') * (multiplier *= 0.1);
          else
            {
              width_left++;
              break;
            }
        }

      // round up if we truncated and the next digit is >= 5
      if ((i == precision || ! width_left) && (ch = is.get ()) >= '5'
          && ch <= '9')
        retval += multiplier;

      if (i > 0)
        valid = true;           // valid if at least one digit after '.'
      else if (! valid)         // if there was nothing before and after '.'
        {
          is.putback (ch);
          ch = '.';
        }

      // skip remainder after '.', to field width, to look for exponent
      if (i == precision)
        while (width_left-- && is && (ch = is.get ()) >= '0' && ch <= '9')
          ;  // discard

      width_left++;
    }

  // look for exponent part in, e.g.,  6.023E+23
  bool used_exp = false;
  if (valid && width_left > 1 && m_exp_chars.find (ch) != std::string::npos)
    {
      int ch1 = is.peek_undelim ();
      if (ch1 == '-' || ch1 == '+' || (ch1 >= '0' && ch1 <= '9'))
        {
          // if 1.0e+$ or some such, this will set failbit, as we want
          width_left--;                         // count "E"
          int exp = 0;
          int exp_sign = 1;
          if (ch1 == '+')
            {
              width_left--;
              is.get ();
            }
          else if (ch1 == '-')
            {
              width_left--;
              exp_sign = -1;
              is.get ();
            }
          valid = false;
          while (width_left-- && is && (ch = is.get_undelim ()) >= '0' && ch <= '9')
            {
              exp = exp*10 + ch - '0';
              valid = true;
            }
          width_left++;
          if (ch != std::istream::traits_type::eof () && width_left)
            is.putback (ch);

          double multiplier = pown (10, exp);
          if (exp_sign > 0)
            retval *= multiplier;
          else
            retval /= multiplier;

          used_exp = true;
        }
    }
  is.clear ();
  if (! used_exp && ch != std::istream::traits_type::eof () && width_left)
    is.putback (ch);

  // Check for +/- inf and NaN
  if (! valid && width_left >= 3 && is.remaining () >= 3)
    {
      int i = lookahead (is, m_inf_nan, 3, false);  // false->case insensitive
      if (i == 0)
        {
          retval = numeric_limits<double>::Inf ();
          valid = true;
        }
      else if (i == 1)
        {
          retval = numeric_limits<double>::NaN ();
          valid = true;
        }
    }

  if (! valid)
    is.setstate (std::ios::failbit);
  else
    is.setstate (is.rdstate () & ~std::ios::failbit);

  return retval * sign;
}

// Read a single number: real, complex, inf, NaN, possibly with limited
// precision.  Calls to this should be preceded by skip_whitespace.
// Calling that inside scan_complex would violate its const declaration.

void
textscan::scan_complex (delimited_stream& is, const textscan_format_elt& fmt,
                        Complex& val) const
{
  double im = 0;
  double re = 0;
  bool as_empty = false;  // did we fail but match a "treat_as_empty" string?
  bool inf = false;

  int ch = is.peek_undelim ();
  if (ch == '+' || ch == '-')   // check for [+-][ij] with no coefficients
    {
      ch = is.get ();
      int ch2 = is.peek_undelim ();
      if (ch2 == 'i' || ch2 == 'j')
        {
          double value = 1;
          is.get ();
          // Check not -inf
          if (is.peek_undelim () == 'n')
            {
              char *pos = is.tellg ();
              std::ios::iostate state = is.rdstate ();

              is.get ();
              ch2 = is.get_undelim ();
              if (ch2 == 'f')
                {
                  inf = true;
                  re = (ch == '+' ? numeric_limits<double>::Inf ()
                        : -numeric_limits<double>::Inf ());
                  value = 0;
                }
              else
                {
                  is.clear (state);
                  // FIXME: Buffer might have refreshed.
                  //        pos might no longer be valid.
                  is.seekg (pos);   // reset to position before look-ahead
                }
            }

          im = (ch == '+') ? value : -value;
        }
      else
        is.putback (ch);
    }

  if (! im && ! inf)        // if not [+-][ij] or [+-]inf, read real normally
    {
      char *pos = is.tellg ();
      std::ios::iostate state = is.rdstate ();
      //re = read_value<double> (is);
      // FIXME: read_double might refresh the buffer.  So seekg might be off.
      re = read_double (is, fmt);

      // check for "treat as empty" string
      if (m_treat_as_empty.numel ()
          && (is.fail () || math::is_NaN_or_NA (Complex (re))
              || re == numeric_limits<double>::Inf ()))
        {

          for (int i = 0; i < m_treat_as_empty.numel (); i++)
            {
              if (ch == m_treat_as_empty (i).string_value ()[0])
                {
                  as_empty = true;  // first char matches, so read the lot
                  break;
                }
            }
          if (as_empty)             // if first char matched...
            {
              as_empty = false;     // ...look for the whole string

              is.clear (state);     // treat_as_empty "-" causes partial read
              is.seekg (pos);       // reset to position before failed read

              // treat_as_empty strings may be different sizes.
              // Read ahead longest, put it all back, then re-read the string
              // that matches.
              std::string look_buf (m_treat_as_empty_len, '\0');
              char *look = is.read (&look_buf[0], look_buf.size (), pos);

              is.clear (state);
              is.seekg (pos);        // reset to position before look-ahead
                                     // FIXME: is.read could invalidate pos

              for (int i = 0; i < m_treat_as_empty.numel (); i++)
                {
                  std::string s = m_treat_as_empty (i).string_value ();
                  if (! strncmp (s.c_str (), look, s.size ()))
                    {
                      as_empty = true;
                      // read just the right amount
                      is.read (&look_buf[0], s.size (), pos);
                      break;
                    }
                }
            }
        }

      if (! is.eof () && ! as_empty)
        {
          state = is.rdstate ();   // before tellg, since that fails at EOF

          ch = is.peek_undelim ();
          // ch == EOF if read failed; no need to chk fail
          if (ch == 'i' || ch == 'j')           // pure imaginary
            {
              is.get ();
              im = re;
              re = 0;
            }
          else if (ch == '+' || ch == '-')      // see if it is real+imag[ij]
            {
              // save stream state in case we have to restore it
              pos   = is.tellg ();
              state = is.rdstate ();

              //im = read_value<double> (is);
              // FIXME: read_double might refresh the buffer.
              //        So seekg might be off after this.
              im = read_double (is, fmt);
              if (is.fail ())
                im = 1;

              if (is.peek_undelim () == 'i' || is.peek_undelim () == 'j')
                is.get ();
              else
                {
                  im = 0;           // no valid imaginary part.  Restore state
                  is.clear (state); // eof shouldn't cause fail.
                  is.seekg (pos);
                }
            }
          else if (is.eof ())       // we've read enough to be a "valid" read
            is.clear (state);       // failed peek shouldn't cause fail
        }
    }
  if (as_empty)
    val = m_empty_value.scalar_value ();
  else
    val = Complex (re, im);
}

// Return in VAL the run of characters from IS NOT contained in PATTERN.

int
textscan::scan_caret (delimited_stream& is, const std::string& pattern,
                      std::string& val) const
{
  int c1 = std::istream::traits_type::eof ();
  std::ostringstream obuf;   // FIXME: is this optimized for growing?

  while (is && ((c1 = (is && ! is.eof ())
                      ? is.get_undelim ()
                      : std::istream::traits_type::eof ())
                != std::istream::traits_type::eof ())
         && pattern.find (c1) == std::string::npos)
    obuf << static_cast<char> (c1);

  val = obuf.str ();

  if (c1 != std::istream::traits_type::eof ())
    is.putback (c1);

  return c1;
}

// Read until one of the strings in DELIMITERS is found.  For
// efficiency, ENDS is a list of the last character of each delimiter.

std::string
textscan::read_until (delimited_stream& is, const Cell& delimiters,
                      const std::string& ends) const
{
  std::string retval ("");
  bool done = false;
  do
    {
      // find sequence ending with an ending char
      std::string next;
      scan_caret (is, ends.c_str (), next);
      retval = retval + next;   // FIXME: could use repeated doubling of size

      int last = (! is.eof ()
                  ? is.get_undelim () : std::istream::traits_type::eof ());

      if (last != std::istream::traits_type::eof ())
        {
          if (last == m_eol1 || last == m_eol2)
            break;

          retval = retval + static_cast<char> (last);
          for (int i = 0; i < delimiters.numel (); i++)
            {
              std::string delim = delimiters(i).string_value ();
              std::size_t start = (retval.length () > delim.length ()
                                   ? retval.length () - delim.length ()
                                   : 0);
              std::string may_match = retval.substr (start);
              if (may_match == delim)
                {
                  done = true;
                  retval = retval.substr (0, start);
                  if (start == 0)
                    is.putback (last);
                  break;
                }
            }
        }
    }
  while (! done && is && ! is.eof ());

  return retval;
}

// Read stream until either fmt.width chars have been read, or
// options.delimiter has been found.  Does *not* rely on fmt being 's'.
// Used by formats like %6f to limit to 6.

void
textscan::scan_string (delimited_stream& is, const textscan_format_elt& fmt,
                       std::string& val) const
{
  if (m_delim_list.isempty ())
    {
      unsigned int i = 0;
      unsigned int width = fmt.width;

      for (i = 0; i < width; i++)
        {
          // Grow string in an exponential fashion if necessary.
          if (i >= val.length ())
            val.append (std::max (val.length (),
                                  static_cast<std::size_t> (16)), '\0');

          int ch = is.get_undelim ();
          if (is_delim (ch) || ch == std::istream::traits_type::eof ())
            {
              is.putback (ch);
              break;
            }
          else
            val[i] = ch;
        }
      val = val.substr (0, i);          // trim pre-allocation
    }
  else  // Cell array of multi-character delimiters
    {
      std::string ends (m_delim_list.numel () + 2, '\0');
      int i;
      for (i = 0; i < m_delim_list.numel (); i++)
        {
          std::string tmp = m_delim_list(i).string_value ();
          ends[i] = tmp.back ();
        }
      ends[i++] = m_eol1;
      ends[i++] = m_eol2;
      val = textscan::read_until (is, m_delim_list, ends);
    }

  // convert from codepage
  if (m_encoding.compare ("utf-8"))
    val = string::u8_from_encoding ("textscan", val, m_encoding);
}

// Return in VAL the run of characters from IS contained in PATTERN.

int
textscan::scan_bracket (delimited_stream& is, const std::string& pattern,
                        std::string& val) const
{
  int c1 = std::istream::traits_type::eof ();
  std::ostringstream obuf;              // Is this optimized for growing?

  while (is && pattern.find (c1 = is.get_undelim ()) != std::string::npos)
    obuf << static_cast<char> (c1);

  val = obuf.str ();
  if (c1 != std::istream::traits_type::eof ())
    is.putback (c1);
  return c1;
}

// Return in VAL a string, either delimited by whitespace/delimiters, or
// enclosed in a pair of double quotes ("...").  Enclosing quotes are
// removed.  A consecutive pair "" is inserted into VAL as a single ".

void
textscan::scan_qstring (delimited_stream& is, const textscan_format_elt& fmt,
                        std::string& val)
{
  skip_whitespace (is);

  if (is.peek_undelim () != '"')
    scan_string (is, fmt, val);
  else
    {
      is.get ();
      scan_caret (is, R"(")", val);     // read everything until "
      is.get ();                        // swallow "

      while (is && is.peek_undelim () == '"')  // if double ",
        {
          // insert one in stream,
          is.get ();                           // keep looking for single "
          std::string val1;
          scan_caret (is, R"(")", val1);
          val = val + '"' + val1;
          is.get_undelim ();
        }
    }

  // convert from codepage
  if (m_encoding.compare ("utf-8"))
    val = string::u8_from_encoding ("textscan", val, m_encoding);
}

// Read from IS into VAL a string of the next fmt.width characters,
// including any whitespace or delimiters.

void
textscan::scan_cstring (delimited_stream& is, const textscan_format_elt& fmt,
                        std::string& val) const
{
  val.resize (fmt.width);

  for (unsigned int i = 0; is && i < fmt.width; i++)
    {
      int ch = is.get_undelim ();
      if (ch != std::istream::traits_type::eof ())
        val[i] = ch;
      else
        {
          val.resize (i);
          break;
        }
    }

  // convert from codepage
  if (m_encoding.compare ("utf-8"))
    val = string::u8_from_encoding ("textscan", val, m_encoding);
}

//  Read a single '%...' conversion and place it in position ROW of OV.

void
textscan::scan_one (delimited_stream& is, const textscan_format_elt& fmt,
                    octave_value& ov, Array<octave_idx_type> row)
{
  skip_whitespace (is);

  is.clear ();

  octave_value val;
  if (fmt.numeric)
    {
      if (fmt.type == 'f' || fmt.type == 'n')
        {
          Complex v;
          skip_whitespace (is);
          scan_complex (is, fmt, v);

          if (! fmt.discard && ! is.fail ())
            {
              if (fmt.bitwidth == 64)
                {
                  if (ov.isreal () && v.imag () == 0)
                    ov.internal_rep ()->fast_elem_insert (row(0), v.real ());
                  else
                    {
                      if (ov.isreal ())  // cat does type conversion
                        ov = cat_op (ov, octave_value (v), row);
                      else
                        ov.internal_rep ()->fast_elem_insert (row(0), v);
                    }
                }
              else
                {
                  if (ov.isreal () && v.imag () == 0)
                    ov.internal_rep ()->fast_elem_insert (row(0),
                                                          float (v.real ()));
                  else
                    {
                      if (ov.isreal ())  // cat does type conversion
                        ov = cat_op (ov, octave_value (v), row);
                      else
                        ov.internal_rep ()->fast_elem_insert (row(0),
                                                              FloatComplex (v));
                    }
                }
            }
        }
      else
        {
          double v;    // Matlab docs say 1e30 etc should be valid for %d and
                       // 1000 as a %d8 should be 127, so read as double.
                       // Some loss of precision for d64 and u64.
          skip_whitespace (is);
          v = read_double (is, fmt);
          if (! fmt.discard && ! is.fail ())
            switch (fmt.bitwidth)
              {
              case 64:
                switch (fmt.type)
                  {
                  case 'd':
                    {
                      octave_int64 vv = v;
                      ov.internal_rep ()->fast_elem_insert (row(0), vv);
                    }
                    break;

                  case 'u':
                    {
                      octave_uint64 vv = v;
                      ov.internal_rep ()->fast_elem_insert (row(0), vv);
                    }
                    break;
                  }
                break;

              case 32:
                switch (fmt.type)
                  {
                  case 'd':
                    {
                      octave_int32 vv = v;
                      ov.internal_rep ()->fast_elem_insert (row(0), vv);
                    }
                    break;

                  case 'u':
                    {
                      octave_uint32 vv = v;
                      ov.internal_rep ()->fast_elem_insert (row(0), vv);
                    }
                    break;
                  }
                break;

              case 16:
                if (fmt.type == 'd')
                  {
                    octave_int16 vv = v;
                    ov.internal_rep ()->fast_elem_insert (row(0), vv);
                  }
                else
                  {
                    octave_uint16 vv = v;
                    ov.internal_rep ()->fast_elem_insert (row(0), vv);
                  }
                break;

              case 8:
                if (fmt.type == 'd')
                  {
                    octave_int8 vv = v;
                    ov.internal_rep ()->fast_elem_insert (row(0), vv);
                  }
                else
                  {
                    octave_uint8 vv = v;
                    ov.internal_rep ()->fast_elem_insert (row(0), vv);
                  }
                break;
              }
        }

      if (is.fail () & ! fmt.discard)
        ov = cat_op (ov, m_empty_value, row);
    }
  else
    {
      std::string vv ("        ");      // initial buffer.  Grows as needed
      switch (fmt.type)
        {
        case 's':
          scan_string (is, fmt, vv);
          break;

        case 'q':
          scan_qstring (is, fmt, vv);
          break;

        case 'c':
          scan_cstring (is, fmt, vv);
          break;

        case '[':
          scan_bracket (is, fmt.char_class.c_str (), vv);
          break;

        case '^':
          scan_caret   (is, fmt.char_class.c_str (), vv);
          break;
        }

      if (! fmt.discard)
        ov.internal_rep ()->fast_elem_insert (row (0),
                                              Cell (octave_value (vv)));

      // FIXME: why does failbit get set at EOF, instead of eofbit?
      if (! vv.empty ())
        is.clear (is.rdstate () & ~std::ios_base::failbit);
    }

  is.field_done ();
}

// Read data corresponding to the entire format string once, placing the
// values in row ROW of retval.

int
textscan::read_format_once (delimited_stream& is,
                            textscan_format_list& fmt_list,
                            std::list<octave_value>& retval,
                            Array<octave_idx_type> row, int& done_after)
{
  const textscan_format_elt *elem = fmt_list.first ();
  auto out = retval.begin ();
  bool no_conversions = true;
  bool done = false;
  bool conversion_failed = false;       // Record for ReturnOnError
  bool nothing_worked = true;

  octave_quit ();

  for (std::size_t i = 0; i < fmt_list.numel (); i++)
    {
      bool this_conversion_failed = false;

      // Clear fail of previous numeric conversions.
      is.clear ();

      switch (elem->type)
        {
        case 'C':
        case 'D':
          warning ("%s: conversion %c not yet implemented",
                   m_who.c_str (), elem->type);
          break;

        case 'u':
        case 'd':
        case 'f':
        case 'n':
        case 's':
        case '[':
        case '^':
        case 'q':
        case 'c':
          scan_one (is, *elem, *out, row);
          break;

        case textscan_format_elt::literal_conversion :
          match_literal (is, *elem);
          break;

        default:
          error ("Unknown format element '%c'", elem->type);
        }

      if (! is.fail ())
        {
          if (! elem->discard)
            no_conversions = false;
        }
      else
        {
          is.clear (is.rdstate () & ~std::ios::failbit);

          if (! is.eof ())
            {
              if (m_delim_list.isempty ())
                {
                  if (! is_delim (is.peek_undelim ()))
                    this_conversion_failed = true;
                }
              else  // Cell array of multi-character delimiters
                {
                  char *pos = is.tellg ();
                  if (-1 == lookahead (is, m_delim_list, m_delim_len))
                    this_conversion_failed = true;
                  is.clear ();
                  is.seekg (pos);     // reset to position before look-ahead
                }
            }
        }

      if (! elem->discard)
        out++;

      elem = fmt_list.next ();
      char *pos = is.tellg ();

      // Skip delimiter before reading the next fmt conversion,
      // unless the fmt is a string literal which begins with a delimiter,
      // in which case the literal must match everything.  Bug #58008
      if (elem->type != textscan_format_elt::literal_conversion)
        skip_delim (is);
      else if (! is_delim (elem->text[0]))
        skip_delim (is);

      if (is.eof ())
        {
          if (! done)
            done_after = i+1;

          // note EOF, but process others to get empty_val.
          done = true;
        }

      if (this_conversion_failed)
        {
          if (is.tellg () == pos && ! conversion_failed)
            {
              // done_after = first failure
              done_after = i; // note fail, but parse others to get empty_val
              conversion_failed = true;
            }
          else
            this_conversion_failed = false;
        }
      else if (! done && ! conversion_failed)
        nothing_worked = false;
    }

  if (done)
    is.setstate (std::ios::eofbit);

  return no_conversions
         + (is.eof () ? 2 : 0)
         + (conversion_failed ? 4 : 0)
         + (nothing_worked ? 8 : 0);

}

void
textscan::parse_options (const octave_value_list& args,
                         textscan_format_list& fmt_list)
{
  int last = args.length ();
  int n = last;

  if (n & 1)
    error ("%s: %d parameters given, but only %d values",
           m_who.c_str (), n-n/2, n/2);

  m_delim_len = 1;
  bool have_delims = false;
  for (int i = 0; i < last; i += 2)
    {
      std::string param = args(i).xstring_value ("%s: Invalid parameter type <%s> for parameter %d",
                          m_who.c_str (),
                          args(i).type_name ().c_str (),
                          i/2 + 1);
      std::transform (param.begin (), param.end (), param.begin (), ::tolower);

      if (param == "delimiter")
        {
          bool invalid = true;
          if (args(i+1).is_string ())
            {
              invalid = false;
              have_delims = true;
              m_delims = args(i+1).string_value ();
              if (args(i+1).is_sq_string ())
                m_delims = do_string_escapes (m_delims);
            }
          else if (args(i+1).iscell ())
            {
              invalid = false;
              m_delim_list = args(i+1).cell_value ();
              m_delim_table = " ";  // non-empty, to flag non-default delim

              // Check that all elements are strings, and find max length
              for (int j = 0; j < m_delim_list.numel (); j++)
                {
                  if (! m_delim_list(j).is_string ())
                    invalid = true;
                  else
                    {
                      if (m_delim_list(j).is_sq_string ())
                        m_delim_list(j) = do_string_escapes (m_delim_list(j)
                                                             .string_value ());
                      octave_idx_type len = m_delim_list(j).string_value ()
                                            .length ();
                      m_delim_len = std::max (static_cast<int> (len),
                                              m_delim_len);
                    }
                }
            }
          if (invalid)
            error ("%s: Delimiters must be either a string or cell array of strings",
                   m_who.c_str ());
        }
      else if (param == "commentstyle")
        {
          if (args(i+1).is_string ())
            {
              // check here for names like "C++", "C", "shell", ...?
              m_comment_style = Cell (args(i+1));
            }
          else if (args(i+1).iscell ())
            {
              m_comment_style = args(i+1).cell_value ();
              int len = m_comment_style.numel ();
              if ((len >= 1 && ! m_comment_style (0).is_string ())
                  || (len >= 2 && ! m_comment_style (1).is_string ())
                  || (len >= 3))
                error ("%s: CommentStyle must be either a string or cell array of one or two strings",
                       m_who.c_str ());
            }
          else
            error ("%s: CommentStyle must be either a string or cell array of one or two strings",
                   m_who.c_str ());

          // How far ahead do we need to look to detect an open comment
          // and which character do we look for?
          if (m_comment_style.numel () >= 1)
            {
              m_comment_len  = m_comment_style (0).string_value ().size ();
              m_comment_char = m_comment_style (0).string_value ()[0];
            }
        }
      else if (param == "treatasempty")
        {
          bool invalid = false;
          if (args(i+1).is_string ())
            {
              m_treat_as_empty = Cell (args(i+1));
              m_treat_as_empty_len = args(i+1).string_value ().size ();
            }
          else if (args(i+1).iscell ())
            {
              m_treat_as_empty = args(i+1).cell_value ();
              for (int j = 0; j < m_treat_as_empty.numel (); j++)
                if (! m_treat_as_empty (j).is_string ())
                  invalid = true;
                else
                  {
                    int k = m_treat_as_empty (j).string_value ().size ();
                    if (k > m_treat_as_empty_len)
                      m_treat_as_empty_len = k;
                  }
            }
          if (invalid)
            error ("%s: TreatAsEmpty must be either a string or cell array of one or two strings",
                   m_who.c_str ());

          // FIXME: Ensure none is a prefix of a later one.  Sort by length?
        }
      else if (param == "collectoutput")
        {
          m_collect_output = args(i+1).xbool_value ("%s: CollectOutput must be logical or numeric",
                             m_who.c_str ());
        }
      else if (param == "emptyvalue")
        {
          m_empty_value = args(i+1).xscalar_value ("%s: EmptyValue must be numeric", m_who.c_str ());
        }
      else if (param == "headerlines")
        {
          m_header_lines = args(i+1).xscalar_value ("%s: HeaderLines must be numeric", m_who.c_str ());
        }
      else if (param == "bufsize")
        {
          m_buffer_size = args(i+1).xscalar_value ("%s: BufSize must be numeric", m_who.c_str ());
        }
      else if (param == "multipledelimsasone")
        {
          multiple_delims_as_one = args(i
                                        +1).xbool_value ("%s: MultipleDelimsAsOne must be logical or numeric", m_who.c_str ());
        }
      else if (param == "returnonerror")
        {
          m_return_on_error = args(i+1).xbool_value ("%s: ReturnOnError must be logical or numeric",
                              m_who.c_str ());
        }
      else if (param == "whitespace")
        {
          m_whitespace = args(i+1).xstring_value ("%s: Whitespace must be a character string",
                                                  m_who.c_str ());
        }
      else if (param == "expchars")
        {
          m_exp_chars = args(i+1).xstring_value ("%s: ExpChars must be a character string", m_who.c_str ());
          m_default_exp = false;
        }
      else if (param == "endofline")
        {
          bool valid = true;
          std::string s = args(i+1).xstring_value (R"(%s: EndOfLine must be at most one character or '\r\n')",
                          m_who.c_str ());
          if (args(i+1).is_sq_string ())
            s = do_string_escapes (s);
          int l = s.length ();
          if (l == 0)
            m_eol1 = m_eol2 = -2;
          else if (l == 1)
            m_eol1 = m_eol2 = s.c_str ()[0];
          else if (l == 2)
            {
              m_eol1 = s.c_str ()[0];
              m_eol2 = s.c_str ()[1];
              if (m_eol1 != '\r' || m_eol2 != '\n')    // Why limit it?
                valid = false;
            }
          else
            valid = false;

          if (! valid)
            error (R"(%s: EndOfLine must be at most one character or '\r\n')",
                   m_who.c_str ());
        }
      else
        error ("%s: unrecognized option '%s'", m_who.c_str (), param.c_str ());
    }

  // Remove any user-supplied delimiter from whitespace list
  for (unsigned int j = 0; j < m_delims.length (); j++)
    {
      m_whitespace.erase (std::remove (m_whitespace.begin (),
                                       m_whitespace.end (),
                                       m_delims[j]),
                          m_whitespace.end ());
    }
  for (int j = 0; j < m_delim_list.numel (); j++)
    {
      std::string delim = m_delim_list(j).string_value ();
      if (delim.length () == 1)
        m_whitespace.erase (std::remove (m_whitespace.begin (),
                                         m_whitespace.end (),
                                         delim[0]),
                            m_whitespace.end ());
    }

  m_whitespace_table = std::string (256, '\0');
  for (unsigned int i = 0; i < m_whitespace.length (); i++)
    m_whitespace_table[m_whitespace[i]] = '1';

  // For Matlab compatibility, add 0x20 to whitespace, unless
  // whitespace is explicitly ignored.
  if (! (m_whitespace.empty () && fmt_list.has_string))
    m_whitespace_table[' '] = '1';

  // Create look-up table of delimiters, based on 'delimiter'
  m_delim_table = std::string (256, '\0');
  if (m_eol1 >= 0 && m_eol1 < 256)
    m_delim_table[m_eol1] = '1';        // EOL is always a delimiter
  if (m_eol2 >= 0 && m_eol2 < 256)
    m_delim_table[m_eol2] = '1';        // EOL is always a delimiter
  if (! have_delims)
    for (unsigned int i = 0; i < 256; i++)
      {
        if (isspace (i))
          m_delim_table[i] = '1';
      }
  else
    for (unsigned int i = 0; i < m_delims.length (); i++)
      m_delim_table[m_delims[i]] = '1';
}

// Skip comments, and characters specified by the "Whitespace" option.
// If EOLstop == true, don't skip end of line.

int
textscan::skip_whitespace (delimited_stream& is, bool EOLstop)
{
  int c1 = std::istream::traits_type::eof ();
  bool found_comment = false;

  do
    {
      found_comment = false;
      int prev = -1;
      while (is
             && (c1 = is.get_undelim ()) != std::istream::traits_type::eof ()
             && ( ( (c1 == m_eol1 || c1 == m_eol2) && ++m_lines && ! EOLstop)
                  || isspace (c1)))
        {
          if (prev == m_eol1 && m_eol1 != m_eol2 && c1 == m_eol2)
            m_lines--;
          prev = c1;
        }

      if (c1 == m_comment_char)           // see if we match an open comment
        {
          // save stream state in case we have to restore it
          char *pos = is.tellg ();
          std::ios::iostate state = is.rdstate ();

          std::string tmp (m_comment_len, '\0');
          char *look = is.read (&tmp[0], m_comment_len-1, pos); // already read first char
          if (is && m_comment_style.numel () > 0
              && ! strncmp (m_comment_style(0).string_value ().substr (1).c_str (),
                            look, m_comment_len-1))
            {
              found_comment = true;

              std::string dummy;
              if (m_comment_style.numel () == 1)  // skip to end of line
                {
                  std::string eol (3, '\0');
                  eol[0] = m_eol1;
                  eol[1] = m_eol2;

                  scan_caret (is, eol, dummy);
                  c1 = is.get_undelim ();
                  if (c1 == m_eol1 && m_eol1 != m_eol2
                      && is.peek_undelim () == m_eol2)
                    is.get_undelim ();
                  m_lines++;
                }
              else      // matching pair
                {
                  std::string end_c = m_comment_style(1).string_value ();
                  // last char of end-comment sequence
                  std::string last = end_c.substr (end_c.size () - 1);
                  std::string may_match ("");
                  do
                    {
                      // find sequence ending with last char
                      scan_caret (is, last, dummy);
                      is.get_undelim ();        // (read LAST itself)

                      may_match = may_match + dummy + last;
                      if (may_match.length () > end_c.length ())
                        {
                          std::size_t start = may_match.length ()
                                              - end_c.length ();
                          may_match = may_match.substr (start);
                        }
                    }
                  while (may_match != end_c && is && ! is.eof ());
                }
            }
          else  // wasn't really a comment; restore state
            {
              is.clear (state);
              is.seekg (pos);
            }
        }
    }
  while (found_comment);

  if (c1 != std::istream::traits_type::eof ())
    is.putback (c1);

  return c1;
}

// See if the next few characters match one of the strings in target.
// For efficiency, MAX_LEN is the cached longest length of any target.
// Return -1 if none is found, or the index of the match.

int
textscan::lookahead (delimited_stream& is, const Cell& targets, int max_len,
                     bool case_sensitive) const
{
  // target strings may be different sizes.
  // Read ahead longest, put it all back, then re-read the string
  // that matches.

  char *pos = is.tellg ();

  std::string tmp (max_len, '\0');
  char *look = is.read (&tmp[0], tmp.size (), pos);

  is.clear ();
  is.seekg (pos);              // reset to position before read
                               // FIXME: pos may be corrupted by is.read

  int i;
  int (*compare)(const char *, const char *, std::size_t);
  compare = (case_sensitive ? strncmp : octave_strncasecmp);

  for (i = 0; i < targets.numel (); i++)
    {
      std::string s = targets (i).string_value ();
      if (! (*compare) (s.c_str (), look, s.size ()))
        {
          is.read (&tmp[0], s.size (), pos); // read just the right amount
          break;
        }
    }

  if (i == targets.numel ())
    i = -1;

  return i;
}

// Skip delimiters -- multiple if MultipleDelimsAsOne specified.
int
textscan::skip_delim (delimited_stream& is)
{
  int c1 = skip_whitespace (is);  // Stop once EOL is read
  if (m_delim_list.numel () == 0)   // single character delimiter
    {
      if (is_delim (c1) || c1 == m_eol1 || c1 == m_eol2)
        {
          is.get ();
          if (c1 == m_eol1 && is.peek_undelim () == m_eol2)
            is.get ();          // if \r\n, skip the \n too.

          if (multiple_delims_as_one)
            {
              int prev = -1;
              // skip multiple delims.
              // Increment lines for each end-of-line seen;
              // Decrement for \r\n
              while (is && ((c1 = is.get_undelim ())
                            != std::istream::traits_type::eof ())
                     && (((c1 == m_eol1 || c1 == m_eol2) && ++m_lines)
                         || isspace (c1) || is_delim (c1)))
                {
                  if (prev == m_eol1 && m_eol1 != m_eol2 && c1 == m_eol2)
                    m_lines--;
                  prev = c1;
                }
              if (c1 != std::istream::traits_type::eof ())
                is.putback (c1);
            }
        }
    }
  else                                  // multi-character delimiter
    {
      int first_match;

      if (c1 == m_eol1 || c1 == m_eol2
          || (-1 != (first_match = lookahead (is, m_delim_list, m_delim_len))))
        {
          if (c1 == m_eol1)
            {
              is.get_undelim ();
              if (is.peek_undelim () == m_eol2)
                is.get_undelim ();
            }
          else if (c1 == m_eol2)
            {
              is.get_undelim ();
            }

          if (multiple_delims_as_one)
            {
              int prev = -1;
              // skip multiple delims.
              // Increment lines for each end-of-line seen;
              // decrement for \r\n.
              while (is && ((c1 = skip_whitespace (is))
                            != std::istream::traits_type::eof ())
                     && (((c1 == m_eol1 || c1 == m_eol2) && ++m_lines)
                         || -1 != lookahead (is, m_delim_list, m_delim_len)))
                {
                  if (prev == m_eol1 && m_eol1 != m_eol2 && c1 == m_eol2)
                    m_lines--;
                  prev = c1;
                }
            }
        }
    }

  return c1;
}

// Read in as much of the input as coincides with the literal in the
// format string.  Return "true" if the entire literal is matched, else
// false (and set failbit).

bool
textscan::match_literal (delimited_stream& is,
                         const textscan_format_elt& fmt)
{
  // "false" -> treat EOL as normal space
  // since a delimiter at the start of a line is a mismatch, not empty field
  skip_whitespace (is, false);

  for (unsigned int i = 0; i < fmt.width; i++)
    {
      int ch = is.get_undelim ();
      if (ch != fmt.text[i])
        {
          if (ch != std::istream::traits_type::eof ())
            is.putback (ch);
          is.setstate (std::ios::failbit);
          return false;
        }
    }
  return true;
}

void
base_stream::error (const std::string& msg)
{
  m_fail = true;
  m_errmsg = msg;
}

void
base_stream::error (const std::string& who, const std::string& msg)
{
  m_fail = true;
  m_errmsg = who + ": " + msg;
}

void
base_stream::clear (void)
{
  m_fail = false;
  m_errmsg = "";
}

void
base_stream::clearerr (void)
{
  std::istream *is = input_stream ();
  std::ostream *os = preferred_output_stream ();

  if (is)
    is->clear ();

  if (os)
    os->clear ();
}

// Functions that are defined for all input streams (input streams
// are those that define is).

std::string
base_stream::do_gets (octave_idx_type max_len, bool& err,
                      bool strip_newline, const std::string& who)
{
  interpreter& interp = __get_interpreter__ ();

  if (interp.interactive () && file_number () == 0)
    ::error ("%s: unable to read from stdin while running interactively",
             who.c_str ());

  std::string retval;

  err = false;

  std::istream *isp = input_stream ();

  if (! isp)
    {
      err = true;
      invalid_operation (who, "reading");
    }
  else
    {
      std::istream& is = *isp;

      std::ostringstream buf;

      int c = 0;
      int char_count = 0;

      if (max_len != 0)
        {
          while (is && (c = is.get ()) != std::istream::traits_type::eof ())
            {
              char_count++;

              // Handle CRLF, CR, or LF as line ending.
              if (c == '\r')
                {
                  if (! strip_newline)
                    buf << static_cast<char> (c);

                  c = is.get ();

                  if (c != std::istream::traits_type::eof ())
                    {
                      if (c == '\n')
                        {
                          char_count++;

                          if (! strip_newline)
                            buf << static_cast<char> (c);
                        }
                      else
                        is.putback (c);
                    }

                  break;
                }
              else if (c == '\n')
                {
                  if (! strip_newline)
                    buf << static_cast<char> (c);

                  break;
                }
              else
                buf << static_cast<char> (c);

              if (max_len > 0 && char_count == max_len)
                break;
            }
        }

      if (! is.eof () && char_count > 0)
        {
          // GAGME.  Matlab seems to check for EOF even if the last character
          // in a file is a newline character.  This is NOT what the
          // corresponding C-library functions do.
          int disgusting_compatibility_hack = is.get ();
          if (! is.eof ())
            is.putback (disgusting_compatibility_hack);
        }

      if (is.good () || (is.eof () && char_count > 0))
        {
          retval = buf.str ();
          if (encoding ().compare ("utf-8"))
            retval = string::u8_from_encoding (who, retval, encoding ());
        }
      else
        {
          err = true;

          if (is.eof () && char_count == 0)
            error (who, "at end of file");
          else
            error (who, "read error");
        }
    }

  return retval;
}

std::string
base_stream::getl (octave_idx_type max_len, bool& err,
                   const std::string& who)
{
  return do_gets (max_len, err, true, who);
}

std::string
base_stream::gets (octave_idx_type max_len, bool& err,
                   const std::string& who)
{
  return do_gets (max_len, err, false, who);
}

off_t
base_stream::skipl (off_t num, bool& err, const std::string& who)
{
  interpreter& interp = __get_interpreter__ ();

  if (interp.interactive () && file_number () == 0)
    ::error ("%s: unable to read from stdin while running interactively",
             who.c_str ());

  off_t cnt = -1;

  err = false;

  std::istream *isp = input_stream ();

  if (! isp)
    {
      err = true;
      invalid_operation (who, "reading");
    }
  else
    {
      std::istream& is = *isp;

      int c = 0;
      int lastc = -1;
      cnt = 0;

      while (is && (c = is.get ()) != std::istream::traits_type::eof ())
        {
          // Handle CRLF, CR, or LF as line ending.
          if (c == '\r' || (c == '\n' && lastc != '\r'))
            {
              if (++cnt == num)
                break;
            }

          lastc = c;
        }

      // Maybe eat the following \n if \r was just met.
      if (c == '\r' && is.peek () == '\n')
        is.get ();

      if (is.bad ())
        {
          err = true;
          error (who, "read error");
        }

      if (err)
        cnt = -1;
    }

  return cnt;
}

template <typename T>
static std::istream&
octave_scan_1 (std::istream& is, const scanf_format_elt& fmt,
               T *valptr)
{
  T value = T ();

  switch (fmt.type)
    {
    case 'o':
      is >> std::oct >> value >> std::dec;
      break;

    case 'x':
    case 'X':
      is >> std::hex >> value >> std::dec;
      break;

    case 'i':
      {
        int c1 = std::istream::traits_type::eof ();

        while (is && (c1 = is.get ()) != std::istream::traits_type::eof ()
               && isspace (c1))
          ; // skip whitespace

        if (c1 != std::istream::traits_type::eof ())
          {
            if (c1 == '0')
              {
                int c2 = is.peek ();

                if (c2 == 'x' || c2 == 'X')
                  {
                    is.ignore ();
                    if (std::isxdigit (is.peek ()))
                      is >> std::hex >> value >> std::dec;
                    else
                      value = 0;
                  }
                else
                  {
                    if (c2 == '0' || c2 == '1' || c2 == '2'
                        || c2 == '3' || c2 == '4' || c2 == '5'
                        || c2 == '6' || c2 == '7')
                      is >> std::oct >> value >> std::dec;
                    else if (c2 == '8' || c2 == '9')
                      {
                        // FIXME: Would like to set error state on octave
                        // stream.  See bug #46493.  But only std::istream is
                        // input to fcn.
                        // error ("internal failure to match octal format");
                        value = 0;
                      }
                    else
                      value = 0;
                  }
              }
            else
              {
                is.putback (c1);

                is >> value;
              }
          }
      }
      break;

    default:
      is >> value;
      break;
    }

  // If conversion produces an integer that overflows, failbit is set but
  // value is non-zero.  We want to treat this case as success, so clear
  // failbit from the stream state to keep going.
  // FIXME: Maybe set error state on octave stream as above? Matlab does
  // *not* indicate an error message on overflow.
  if ((is.rdstate () & std::ios::failbit) && value != T ())
    is.clear (is.rdstate () & ~std::ios::failbit);

  // Only copy the converted value if the stream is in a state where we
  // want to continue reading.
  if (! (is.rdstate () & std::ios::failbit))
    *valptr = value;

  return is;
}

template <typename T>
static std::istream&
octave_scan (std::istream& is, const scanf_format_elt& fmt, T *valptr)
{
  if (fmt.width)
    {
      // Limit input to fmt.width characters by reading into a
      // temporary stringstream buffer.
      std::string strbuf;

      auto orig_pos = is.tellg ();

      is.width (fmt.width);
      is >> strbuf;

      std::istringstream ss (strbuf);

      octave_scan_1 (ss, fmt, valptr);

      if (! ss.eof ())
        {
          // If fewer characters than width were used to read a number then
          // the original istream object positioning is incorrect.
          // Rather than attempt to update istream state and positioning,
          // just redo the '>>' operation with the correct width so that
          // all flags get set correctly.

          is.clear ();  // Clear EOF, FAILBIT, BADBIT
          is.seekg (orig_pos, is.beg);

          int chars_read = ss.tellg ();
          if (chars_read > 0)
            {
              is.width (chars_read);
              is >> strbuf;
            }
        }

      // If pattern failed to match then propagate fail bit to 'is' stream.
      if (ss.fail ())
        is.setstate (std::ios::failbit);

    }
  else
    octave_scan_1 (is, fmt, valptr);

  return is;
}

template <>
std::istream&
octave_scan<> (std::istream& is, const scanf_format_elt& fmt, double *valptr)
{
  double& ref = *valptr;

  switch (fmt.type)
    {
    case 'e':
    case 'f':
    case 'g':
    case 'E':
    case 'G':
      {
        int c1 = std::istream::traits_type::eof ();

        while (is && (c1 = is.get ()) != std::istream::traits_type::eof ()
               && isspace (c1))
          ; // skip whitespace

        if (c1 != std::istream::traits_type::eof ())
          {
            is.putback (c1);

            ref = read_value<double> (is);
          }
      }
      break;

    default:
      panic_impossible ();
      break;
    }

  return is;
}

template <typename T>
static void
do_scanf_conv (std::istream& is, const scanf_format_elt& fmt,
               T valptr, Matrix& mval, double *data, octave_idx_type& idx,
               octave_idx_type& conversion_count, octave_idx_type nr,
               octave_idx_type max_size, bool discard)
{
  octave_scan (is, fmt, valptr);

  if (! is)
    return;

  if (idx == max_size && ! discard)
    {
      max_size *= 2;

      if (nr > 0)
        mval.resize (nr, max_size / nr, 0.0);
      else
        mval.resize (max_size, 1, 0.0);

      data = mval.fortran_vec ();
    }

  if (! discard)
    {
      conversion_count++;
      data[idx++] = *(valptr);
    }
}

template void
do_scanf_conv (std::istream&, const scanf_format_elt&, double *,
               Matrix&, double *, octave_idx_type&, octave_idx_type&,
               octave_idx_type, octave_idx_type, bool);

#define DO_WHITESPACE_CONVERSION()                                      \
  do                                                                    \
    {                                                                   \
      int c = std::istream::traits_type::eof ();                        \
                                                                        \
      while (is && (c = is.get ()) != std::istream::traits_type::eof () \
             && isspace (c))                                            \
        { /* skip whitespace */ }                                       \
                                                                        \
      if (c != std::istream::traits_type::eof ())                       \
        is.putback (c);                                                 \
    }                                                                   \
  while (0)

#define DO_LITERAL_CONVERSION()                                         \
  do                                                                    \
    {                                                                   \
      int c = std::istream::traits_type::eof ();                        \
                                                                        \
      int n = fmt.length ();                                            \
      int i = 0;                                                        \
                                                                        \
      while (i < n && is                                                \
             && (c = is.get ()) != std::istream::traits_type::eof ())   \
        {                                                               \
          if (c == static_cast<unsigned char> (fmt[i]))                 \
            {                                                           \
              i++;                                                      \
              continue;                                                 \
            }                                                           \
          else                                                          \
            {                                                           \
              is.putback (c);                                           \
              break;                                                    \
            }                                                           \
        }                                                               \
                                                                        \
      if (i != n)                                                       \
        is.setstate (std::ios::failbit);                                \
    }                                                                   \
  while (0)

#define DO_PCT_CONVERSION()                             \
  do                                                    \
    {                                                   \
      int c = is.get ();                                \
                                                        \
      if (c != std::istream::traits_type::eof ())       \
        {                                               \
          if (c != '%')                                 \
            {                                           \
              is.putback (c);                           \
              is.setstate (std::ios::failbit);          \
            }                                           \
        }                                               \
      else                                              \
        is.setstate (std::ios::failbit);                \
    }                                                   \
  while (0)

#define BEGIN_C_CONVERSION()                                            \
  is.unsetf (std::ios::skipws);                                         \
                                                                        \
  int width = (elt->width ? elt->width : 1);                            \
                                                                        \
  std::string tmp (width, '\0');                                        \
                                                                        \
  int c = std::istream::traits_type::eof ();                            \
  int n = 0;                                                            \
                                                                        \
  while (is && n < width                                                \
         && (c = is.get ()) != std::istream::traits_type::eof ())       \
    tmp[n++] = static_cast<char> (c);                                   \
                                                                        \
  if (n > 0 && c == std::istream::traits_type::eof ())                  \
    is.clear ();                                                        \
                                                                        \
  tmp.resize (n)

// For a '%s' format, skip initial whitespace and then read until the
// next whitespace character or until WIDTH characters have been read.
#define BEGIN_S_CONVERSION()                                            \
  int width = elt->width;                                               \
                                                                        \
  std::string tmp;                                                      \
                                                                        \
  do                                                                    \
    {                                                                   \
      if (width)                                                        \
        {                                                               \
          tmp = std::string (width, '\0');                              \
                                                                        \
          int c = std::istream::traits_type::eof ();                    \
                                                                        \
          int n = 0;                                                    \
                                                                        \
          while (is && (c = is.get ()) != std::istream::traits_type::eof ()) \
            {                                                           \
              if (! isspace (c))                                        \
                {                                                       \
                  tmp[n++] = static_cast<char> (c);                     \
                  break;                                                \
                }                                                       \
            }                                                           \
                                                                        \
          while (is && n < width                                        \
                 && (c = is.get ()) != std::istream::traits_type::eof ()) \
            {                                                           \
              if (isspace (c))                                          \
                {                                                       \
                  is.putback (c);                                       \
                  break;                                                \
                }                                                       \
              else                                                      \
                tmp[n++] = static_cast<char> (c);                       \
            }                                                           \
                                                                        \
          if (n > 0 && c == std::istream::traits_type::eof ())          \
            is.clear ();                                                \
                                                                        \
          tmp.resize (n);                                               \
        }                                                               \
      else                                                              \
        {                                                               \
          is >> std::ws >> tmp;                                         \
        }                                                               \
    }                                                                   \
  while (0)

// This format must match a nonempty sequence of characters.
#define BEGIN_CHAR_CLASS_CONVERSION()                                   \
  int width = elt->width;                                               \
                                                                        \
  std::string tmp;                                                      \
                                                                        \
  do                                                                    \
    {                                                                   \
      if (! width)                                                      \
        width = std::numeric_limits<int>::max ();                       \
                                                                        \
      std::ostringstream buf;                                           \
                                                                        \
      std::string char_class = elt->char_class;                         \
                                                                        \
      int c = std::istream::traits_type::eof ();                        \
                                                                        \
      if (elt->type == '[')                                             \
        {                                                               \
          int chars_read = 0;                                           \
          while (is && chars_read++ < width                             \
                 && (c = is.get ()) != std::istream::traits_type::eof () \
                 && char_class.find (c) != std::string::npos)           \
            buf << static_cast<char> (c);                               \
        }                                                               \
      else                                                              \
        {                                                               \
          int chars_read = 0;                                           \
          while (is && chars_read++ < width                             \
                 && (c = is.get ()) != std::istream::traits_type::eof () \
                 && char_class.find (c) == std::string::npos)           \
            buf << static_cast<char> (c);                               \
        }                                                               \
                                                                        \
      if (width == std::numeric_limits<int>::max ()                     \
          && c != std::istream::traits_type::eof ())                    \
        is.putback (c);                                                 \
                                                                        \
      tmp = buf.str ();                                                 \
                                                                        \
      if (tmp.empty ())                                                 \
        is.setstate (std::ios::failbit);                                \
      else if (c == std::istream::traits_type::eof ())                  \
        is.clear ();                                                    \
                                                                        \
    }                                                                   \
  while (0)

#define FINISH_CHARACTER_CONVERSION()                                   \
  do                                                                    \
    {                                                                   \
      if (encoding ().compare ("utf-8"))                                \
        tmp = string::u8_from_encoding (who, tmp, encoding ());         \
      width = tmp.length ();                                            \
                                                                        \
      if (is)                                                           \
        {                                                               \
          int i = 0;                                                    \
                                                                        \
          if (! discard)                                                \
            {                                                           \
              conversion_count++;                                       \
                                                                        \
              while (i < width)                                         \
                {                                                       \
                  if (data_index == max_size)                           \
                    {                                                   \
                      max_size *= 2;                                    \
                                                                        \
                      if (all_char_conv)                                \
                        {                                               \
                          if (one_elt_size_spec)                        \
                            mval.resize (1, max_size, 0.0);             \
                          else if (nr > 0)                              \
                            mval.resize (nr, max_size / nr, 0.0);       \
                          else                                          \
                            panic_impossible ();                        \
                        }                                               \
                      else if (nr > 0)                                  \
                        mval.resize (nr, max_size / nr, 0.0);           \
                      else                                              \
                        mval.resize (max_size, 1, 0.0);                 \
                                                                        \
                      data = mval.fortran_vec ();                       \
                    }                                                   \
                                                                        \
                  data[data_index++] = static_cast<unsigned char>       \
                                                  (tmp[i++]);           \
                }                                                       \
            }                                                           \
        }                                                               \
    }                                                                   \
  while (0)

octave_value
base_stream::do_scanf (scanf_format_list& fmt_list,
                       octave_idx_type nr, octave_idx_type nc,
                       bool one_elt_size_spec,
                       octave_idx_type& conversion_count,
                       const std::string& who)
{
  interpreter& interp = __get_interpreter__ ();

  if (interp.interactive () && file_number () == 0)
    ::error ("%s: unable to read from stdin while running interactively",
             who.c_str ());

  octave_value retval = Matrix ();

  conversion_count = 0;

  octave_idx_type m_nconv = fmt_list.num_conversions ();

  octave_idx_type data_index = 0;

  if (nr == 0 || nc == 0)
    {
      if (one_elt_size_spec)
        nc = 0;

      return Matrix (nr, nc, 0.0);
    }

  std::istream *isp = input_stream ();

  bool all_char_conv = fmt_list.all_character_conversions ();

  Matrix mval;
  octave_idx_type max_size = 0;
  octave_idx_type max_conv = 0;

  octave_idx_type final_nr = 0;
  octave_idx_type final_nc = 0;

  if (all_char_conv)
    {
      // Any of these could be resized later (if we have %s conversions,
      // we may read more than one element for each conversion).
      if (one_elt_size_spec)
        {
          max_size = 512;
          mval.resize (1, max_size, 0.0);

          if (nr > 0)
            max_conv = nr;
        }
      else if (nr > 0)
        {
          if (nc > 0)
            {
              mval.resize (nr, nc, 0.0);
              max_size = max_conv = nr * nc;
            }
          else
            {
              mval.resize (nr, 32, 0.0);
              max_size = nr * 32;
            }
        }
      else
        panic_impossible ();
    }
  else if (nr > 0)
    {
      if (nc > 0)
        {
          // Will not resize later.
          mval.resize (nr, nc, 0.0);
          max_size = nr * nc;
          max_conv = max_size;
        }
      else
        {
          // Maybe resize later.
          mval.resize (nr, 32, 0.0);
          max_size = nr * 32;
        }
    }
  else
    {
      // Maybe resize later.
      mval.resize (32, 1, 0.0);
      max_size = 32;
    }

  double *data = mval.fortran_vec ();

  if (isp)
    {
      std::istream& is = *isp;

      const scanf_format_elt *elt = fmt_list.first ();

      std::ios::fmtflags flags = is.flags ();

      octave_idx_type trips = 0;

      octave_idx_type num_fmt_elts = fmt_list.length ();

      for (;;)
        {
          octave_quit ();

          if (elt)
            {
              if (elt->type == scanf_format_elt::null
                  || (! (elt->type == scanf_format_elt::whitespace_conversion
                         || elt->type == scanf_format_elt::literal_conversion
                         || elt->type == '%')
                      && max_conv > 0 && conversion_count == max_conv))
                {
                  // We are done, either because we have reached the end of
                  // the format string and are not cycling through the format
                  // again or because we've converted all the values that
                  // have been requested and the next format element is a
                  // conversion.  Determine final array size and exit.
                  if (all_char_conv && one_elt_size_spec)
                    {
                      final_nr = 1;
                      final_nc = data_index;
                    }
                  else
                    {
                      final_nr = nr;
                      final_nc = (data_index - 1) / nr + 1;
                    }

                  break;
                }
              else if (data_index == max_size)
                {
                  max_size *= 2;

                  if (all_char_conv)
                    {
                      if (one_elt_size_spec)
                        mval.resize (1, max_size, 0.0);
                      else if (nr > 0)
                        mval.resize (nr, max_size / nr, 0.0);
                      else
                        panic_impossible ();
                    }
                  else if (nr > 0)
                    mval.resize (nr, max_size / nr, 0.0);
                  else
                    mval.resize (max_size, 1, 0.0);

                  data = mval.fortran_vec ();
                }

              std::string fmt = elt->text;

              bool discard = elt->discard;

              switch (elt->type)
                {
                case scanf_format_elt::whitespace_conversion:
                  DO_WHITESPACE_CONVERSION ();
                  break;

                case scanf_format_elt::literal_conversion:
                  DO_LITERAL_CONVERSION ();
                  break;

                case '%':
                  DO_PCT_CONVERSION ();
                  break;

                case 'd': case 'i':
                  {
                    switch (elt->modifier)
                      {
                      case 'h':
                        {
                          int16_t tmp;
                          do_scanf_conv (is, *elt, &tmp, mval, data,
                                         data_index, conversion_count,
                                         nr, max_size, discard);
                        }
                        break;

                      case 'l':
                        {
                          int64_t tmp;
                          do_scanf_conv (is, *elt, &tmp, mval, data,
                                         data_index, conversion_count,
                                         nr, max_size, discard);
                        }
                        break;

                      default:
                        {
                          int32_t tmp;
                          do_scanf_conv (is, *elt, &tmp, mval, data,
                                         data_index, conversion_count,
                                         nr, max_size, discard);
                        }
                        break;
                      }
                  }
                  break;

                case 'o': case 'u': case 'x': case 'X':
                  {
                    switch (elt->modifier)
                      {
                      case 'h':
                        {
                          uint16_t tmp;
                          do_scanf_conv (is, *elt, &tmp, mval, data,
                                         data_index, conversion_count,
                                         nr, max_size, discard);
                        }
                        break;

                      case 'l':
                        {
                          uint64_t tmp;
                          do_scanf_conv (is, *elt, &tmp, mval, data,
                                         data_index, conversion_count,
                                         nr, max_size, discard);
                        }
                        break;

                      default:
                        {
                          uint32_t tmp;
                          do_scanf_conv (is, *elt, &tmp, mval, data,
                                         data_index, conversion_count,
                                         nr, max_size, discard);
                        }
                        break;
                      }
                  }
                  break;

                case 'e': case 'f': case 'g':
                case 'E': case 'G':
                  {
                    double tmp;

                    do_scanf_conv (is, *elt, &tmp, mval, data,
                                   data_index, conversion_count,
                                   nr, max_size, discard);
                  }
                  break;

                case 'c':
                  {
                    BEGIN_C_CONVERSION ();

                    FINISH_CHARACTER_CONVERSION ();

                    is.setf (flags);
                  }
                  break;

                case 's':
                  {
                    BEGIN_S_CONVERSION ();

                    FINISH_CHARACTER_CONVERSION ();
                  }
                  break;

                case '[': case '^':
                  {
                    BEGIN_CHAR_CLASS_CONVERSION ();

                    FINISH_CHARACTER_CONVERSION ();
                  }
                  break;

                case 'p':
                  error (who, "unsupported format specifier");
                  break;

                default:
                  error (who, "internal format error");
                  break;
                }

              if (! ok ())
                {
                  break;
                }
              else if (is.eof () || ! is)
                {
                  if (all_char_conv)
                    {
                      if (one_elt_size_spec)
                        {
                          final_nr = 1;
                          final_nc = data_index;
                        }
                      else if (data_index > nr)
                        {
                          final_nr = nr;
                          final_nc = (data_index - 1) / nr + 1;
                        }
                      else
                        {
                          final_nr = data_index;
                          final_nc = 1;
                        }
                    }
                  else if (nr > 0)
                    {
                      if (data_index > nr)
                        {
                          final_nr = nr;
                          final_nc = (data_index - 1) / nr + 1;
                        }
                      else
                        {
                          final_nr = data_index;
                          final_nc = 1;
                        }
                    }
                  else
                    {
                      final_nr = data_index;
                      final_nc = 1;
                    }

                  // If it looks like we have a matching failure, then
                  // reset the failbit in the stream state.
                  if (! is.eof () && is.rdstate () & std::ios::failbit)
                    {
                      error (who, "format failed to match");
                      is.clear (is.rdstate () & (~std::ios::failbit));
                    }

                  // FIXME: is this the right thing to do?
                  if (interp.interactive ()
                      && ! application::forced_interactive ()
                      && name () == "stdin")
                    {
                      is.clear ();

                      // Skip to end of line.
                      bool err;
                      do_gets (-1, err, false, who);
                    }

                  break;
                }
            }
          else
            {
              error (who, "internal format error");
              break;
            }

          if (m_nconv == 0 && ++trips == num_fmt_elts)
            {
              if (all_char_conv && one_elt_size_spec)
                {
                  final_nr = 1;
                  final_nc = data_index;
                }
              else
                {
                  final_nr = nr;
                  final_nc = (data_index - 1) / nr + 1;
                }

              break;
            }
          else
            {
              // Cycle through the format list more than once if we have some
              // conversions to make and we haven't reached the limit on the
              // number of values to convert (possibly because there is no
              // specified limit).
              elt = fmt_list.next (m_nconv > 0
                                   && (max_conv == 0
                                       || conversion_count < max_conv));
            }
        }
    }

  mval.resize (final_nr, final_nc, 0.0);

  retval = mval;

  if (all_char_conv)
    retval = retval.convert_to_str (false, true);

  return retval;
}

octave_value
base_stream::scanf (const std::string& fmt, const Array<double>& size,
                    octave_idx_type& conversion_count,
                    const std::string& who)
{
  octave_value retval = Matrix ();

  conversion_count = 0;

  std::istream *isp = input_stream ();

  if (! isp)
    invalid_operation (who, "reading");
  else
    {
      scanf_format_list fmt_list (fmt);

      if (fmt_list.num_conversions () == -1)
        ::error ("%s: invalid format specified", who.c_str ());

      octave_idx_type nr = -1;
      octave_idx_type nc = -1;

      bool one_elt_size_spec;

      get_size (size, nr, nc, one_elt_size_spec, who);

      retval = do_scanf (fmt_list, nr, nc, one_elt_size_spec,
                         conversion_count, who);
    }

  return retval;
}

bool
base_stream::do_oscanf (const scanf_format_elt *elt,
                        octave_value& retval, const std::string& who)
{
  std::istream *isp = input_stream ();

  if (! isp)
    return false;

  bool quit = false;

  std::istream& is = *isp;

  std::ios::fmtflags flags = is.flags ();

  if (elt)
    {
      std::string fmt = elt->text;

      bool discard = elt->discard;

      switch (elt->type)
        {
        case scanf_format_elt::whitespace_conversion:
          DO_WHITESPACE_CONVERSION ();
          break;

        case scanf_format_elt::literal_conversion:
          DO_LITERAL_CONVERSION ();
          break;

        case '%':
          {
            DO_PCT_CONVERSION ();

            if (! is)
              quit = true;
          }
          break;

        case 'd': case 'i':
          {
            switch (elt->modifier)
              {
              case 'h':
                {
                  int16_t tmp;
                  if (octave_scan (is, *elt, &tmp))
                    {
                      if (! discard)
                        retval = tmp;
                    }
                  else
                    quit = true;
                }
                break;

              case 'l':
                {
                  int64_t tmp;
                  if (octave_scan (is, *elt, &tmp))
                    {
                      if (! discard)
                        retval = tmp;
                    }
                  else
                    quit = true;
                }
                break;

              default:
                {
                  int32_t tmp;
                  if (octave_scan (is, *elt, &tmp))
                    {
                      if (! discard)
                        retval = tmp;
                    }
                  else
                    quit = true;
                }
                break;
              }
          }
          break;

        case 'o': case 'u': case 'x': case 'X':
          {
            switch (elt->modifier)
              {
              case 'h':
                {
                  uint16_t tmp;
                  if (octave_scan (is, *elt, &tmp))
                    {
                      if (! discard)
                        retval = tmp;
                    }
                  else
                    quit = true;
                }
                break;

              case 'l':
                {
                  uint64_t tmp;
                  if (octave_scan (is, *elt, &tmp))
                    {
                      if (! discard)
                        retval = tmp;
                    }
                  else
                    quit = true;
                }
                break;

              default:
                {
                  uint32_t tmp;
                  if (octave_scan (is, *elt, &tmp))
                    {
                      if (! discard)
                        retval = tmp;
                    }
                  else
                    quit = true;
                }
                break;
              }
          }
          break;

        case 'e': case 'f': case 'g':
        case 'E': case 'G':
          {
            double tmp;

            if (octave_scan (is, *elt, &tmp))
              {
                if (! discard)
                  retval = tmp;
              }
            else
              quit = true;
          }
          break;

        case 'c':
          {
            BEGIN_C_CONVERSION ();

            if (! discard)
              retval = tmp;

            if (! is)
              quit = true;

            is.setf (flags);
          }
          break;

        case 's':
          {
            BEGIN_S_CONVERSION ();

            if (! discard)
              retval = tmp;

            if (! is)
              quit = true;
          }
          break;

        case '[':
        case '^':
          {
            BEGIN_CHAR_CLASS_CONVERSION ();

            if (! discard)
              retval = tmp;

            if (! is)
              quit = true;
          }
          break;

        case 'p':
          error (who, "unsupported format specifier");
          break;

        default:
          error (who, "internal format error");
          break;
        }
    }

  if (ok () && is.fail ())
    {
      error ("%s: read error", who.c_str ());

      // FIXME: is this the right thing to do?

      interpreter& interp = __get_interpreter__ ();

      if (interp.interactive () && ! application::forced_interactive ()
          && name () == "stdin")
        {
          // Skip to end of line.
          bool err;
          do_gets (-1, err, false, who);
        }
    }

  return quit;
}

octave_value_list
base_stream::oscanf (const std::string& fmt, const std::string& who)
{
  octave_value_list retval;

  std::istream *isp = input_stream ();

  if (! isp)
    invalid_operation (who, "reading");
  else
    {
      std::istream& is = *isp;

      scanf_format_list fmt_list (fmt);

      octave_idx_type m_nconv = fmt_list.num_conversions ();

      if (m_nconv == -1)
        ::error ("%s: invalid format specified", who.c_str ());

      is.clear ();

      octave_idx_type len = fmt_list.length ();

      retval.resize (m_nconv+2, Matrix ());

      const scanf_format_elt *elt = fmt_list.first ();

      int num_values = 0;

      bool quit = false;

      for (octave_idx_type i = 0; i < len; i++)
        {
          octave_value tmp;

          quit = do_oscanf (elt, tmp, who);

          if (quit)
            break;
          else
            {
              if (tmp.is_defined ())
                retval(num_values++) = tmp;

              if (! ok ())
                break;

              elt = fmt_list.next (m_nconv > 0);
            }
        }

      retval(m_nconv) = num_values;

      int err_num;
      retval(m_nconv+1) = error (false, err_num);

      if (! quit)
        {
          // Pick up any trailing stuff.
          if (ok () && len > m_nconv)
            {
              octave_value tmp;

              elt = fmt_list.next ();

              do_oscanf (elt, tmp, who);
            }
        }
    }

  return retval;
}

octave_value
base_stream::do_textscan (const std::string& fmt,
                          octave_idx_type ntimes,
                          const octave_value_list& options,
                          const std::string& who,
                          octave_idx_type& read_count)
{
  interpreter& interp = __get_interpreter__ ();

  if (interp.interactive () && file_number () == 0)
    ::error ("%s: unable to read from stdin while running interactively",
             who.c_str ());

  octave_value retval = Cell (dim_vector (1, 1), Matrix (0, 1));

  std::istream *isp = input_stream ();

  if (! isp)
    invalid_operation (who, "reading");
  else
    {
      textscan scanner (who, encoding ());

      retval = scanner.scan (*isp, fmt, ntimes, options, read_count);
    }

  return retval;
}

// Functions that are defined for all output streams
// (output streams are those that define os).

int
base_stream::flush (void)
{
  int retval = -1;

  std::ostream *os = preferred_output_stream ();

  if (! os)
    invalid_operation ("fflush", "writing");
  else
    {
      os->flush ();

      if (os->good ())
        retval = 0;
    }

  return retval;
}

class
printf_value_cache
{
public:

  enum state { ok, conversion_error };

  printf_value_cache (const octave_value_list& args, const std::string& who)
    : m_values (args), m_val_idx (0), m_elt_idx (0),
      m_n_vals (m_values.length ()), m_n_elts (0), m_have_data (false),
      m_curr_state (ok)
  {
    for (octave_idx_type i = 0; i < m_values.length (); i++)
      {
        octave_value val = m_values(i);

        if (val.isstruct () || val.iscell () || val.isobject ())
          err_wrong_type_arg (who, val);
      }
  }

  // No copying!

  printf_value_cache (const printf_value_cache&) = delete;

  printf_value_cache& operator = (const printf_value_cache&) = delete;

  ~printf_value_cache (void) = default;

  // Get the current value as a double and advance the internal pointer.
  octave_value get_next_value (char type = 0);

  // Get the current value as an int and advance the internal
  // pointer.  Value before conversion to int must be >= 0 and less
  // than std::numeric_limits<int>::max ().

  int int_value (void);

  operator bool () const { return (m_curr_state == ok); }

  bool exhausted (void) { return (m_val_idx >= m_n_vals); }

private:

  // Must create value cache with values!
  printf_value_cache (void);

  //--------

  const octave_value_list m_values;
  octave_idx_type m_val_idx;
  octave_idx_type m_elt_idx;
  octave_idx_type m_n_vals;
  octave_idx_type m_n_elts;
  bool m_have_data;
  octave_value m_curr_val;
  state m_curr_state;
};

octave_value
printf_value_cache::get_next_value (char type)
{
  octave_value retval;

  if (exhausted ())
    m_curr_state = conversion_error;

  while (! exhausted ())
    {
      if (! m_have_data)
        {
          m_curr_val = m_values (m_val_idx);

          m_elt_idx = 0;
          m_n_elts = m_curr_val.numel ();
          m_have_data = true;
        }

      if (m_elt_idx < m_n_elts)
        {
          if (type == 's')
            {
              if (m_curr_val.is_string ())
                {
                  dim_vector dv (1, m_curr_val.numel ());
                  octave_value tmp = m_curr_val.reshape (dv);

                  std::string sval = tmp.string_value ();

                  retval = sval.substr (m_elt_idx);

                  // We've consumed the rest of the value.
                  m_elt_idx = m_n_elts;
                }
              else
                {
                  // Convert to character string while values are
                  // integers in the range [0 : char max]
                  const NDArray val = m_curr_val.array_value ();

                  octave_idx_type idx = m_elt_idx;

                  for (; idx < m_n_elts; idx++)
                    {
                      double dval = val(idx);

                      if (math::x_nint (dval) != dval
                          || dval < 0 || dval > 255)
                        break;
                    }

                  octave_idx_type n = idx - m_elt_idx;

                  if (n > 0)
                    {
                      std::string sval (n, '\0');

                      for (octave_idx_type i = 0; i < n; i++)
                        sval[i] = val(m_elt_idx++);

                      retval = sval;
                    }
                  else
                    retval = m_curr_val.fast_elem_extract (m_elt_idx++);
                }
            }
          else
            {
              retval = m_curr_val.fast_elem_extract (m_elt_idx++);

              if (type == 'c' && ! retval.is_string ())
                {
                  double dval = retval.double_value ();

                  if (math::x_nint (dval) == dval && dval >= 0 && dval < 256)
                    retval = static_cast<char> (dval);
                }
            }

          if (m_elt_idx >= m_n_elts)
            {
              m_elt_idx = 0;
              m_val_idx++;
              m_have_data = false;
            }

          break;
        }
      else
        {
          m_val_idx++;
          m_have_data = false;

          if (m_n_elts == 0)
            {
              if (m_elt_idx == 0)
                {
                  if (type == 's' || type == 'c')
                    retval = "";
                  else
                    retval = Matrix ();

                  break;
                }

              if (exhausted ())
                m_curr_state = conversion_error;
            }
        }
    }

  return retval;
}

int
printf_value_cache::int_value (void)
{
  octave_value val = get_next_value ();

  double dval = val.double_value (true);

  if (dval < 0 || dval > std::numeric_limits<int>::max ()
      || math::x_nint (dval) != dval)
    {
      m_curr_state = conversion_error;
      return -1;
    }

  return math::nint (dval);
}

// Ugh again and again.

template <typename T>
static int
do_printf_conv (std::ostream& os, const char *fmt, int nsa, int sa_1,
                int sa_2, T arg, const std::string& who)
{
  int retval = 0;

  switch (nsa)
    {
    case 2:
      retval = format (os, fmt, sa_1, sa_2, arg);
      break;

    case 1:
      retval = format (os, fmt, sa_1, arg);
      break;

    case 0:
      retval = format (os, fmt, arg);
      break;

    default:
      ::error ("%s: internal error handling format", who.c_str ());
      break;
    }

  return retval;
}

static std::size_t
do_printf_string (std::ostream& os, const printf_format_elt *elt,
                  int nsa, int sa_1, int sa_2, const std::string& arg,
                  const std::string& who)
{
  if (nsa > 2)
    ::error ("%s: internal error handling format", who.c_str ());

  std::string flags = elt->flags;

  bool left = flags.find ('-') != std::string::npos;

  std::size_t len = arg.length ();

  std::size_t prec = (nsa > 1 ? sa_2 : (elt->prec == -1 ? len : elt->prec));

  std::string print_str = prec < arg.length () ? arg.substr (0, prec) : arg;

  std::size_t fw = (nsa > 0 ? sa_1 : (elt->fw == -1 ? len : elt->fw));

  os << std::setw (fw) << (left ? std::left : std::right) << print_str;

  return len > fw ? len : fw;
}

static bool
is_nan_or_inf (const octave_value& val)
{
  octave_value ov_isnan = val.isnan ();
  octave_value ov_isinf = val.isinf ();

  return (ov_isnan.is_true () || ov_isinf.is_true ());
}

static bool
ok_for_signed_int_conv (const octave_value& val)
{
  uint64_t limit = std::numeric_limits<int64_t>::max ();

  if (val.is_string ())
    return true;
  else if (val.isinteger ())
    {
      if (val.is_uint64_type ())
        {
          octave_uint64 ival = val.uint64_scalar_value ();

          if (ival.value () <= limit)
            return true;
        }
      else
        return true;
    }
  else
    {
      double dval = val.double_value (true);

      if (dval == math::fix (dval) && dval <= limit)
        return true;
    }

  return false;
}

static bool
ok_for_unsigned_int_conv (const octave_value& val)
{
  if (val.is_string ())
    return true;
  else if (val.isinteger ())
    {
      // Easier than dispatching here...

      octave_value ov_is_ge_zero
        = binary_op (octave_value::op_ge, val, octave_value (0.0));

      return ov_is_ge_zero.is_true ();
    }
  else
    {
      double dval = val.double_value (true);

      uint64_t limit = std::numeric_limits<uint64_t>::max ();

      if (dval == math::fix (dval) && dval >= 0 && dval <= limit)
        return true;
    }

  return false;
}

static std::string
switch_to_g_format (const printf_format_elt *elt)
{
  std::string tfmt = elt->text;

  tfmt.replace (tfmt.rfind (elt->type), 1, "g");

  return tfmt;
}

int
base_stream::do_numeric_printf_conv (std::ostream& os,
                                     const printf_format_elt *elt,
                                     int nsa, int sa_1, int sa_2,
                                     const octave_value& val,
                                     const std::string& who)
{
  int retval = 0;

  std::string tfmt = elt->text;

  if (is_nan_or_inf (val))
    {
      double dval = val.double_value ();

      std::string::size_type i1, i2;

      tfmt.replace ((i1 = tfmt.rfind (elt->type)), 1, 1, 's');

      if ((i2 = tfmt.rfind ('.')) != std::string::npos && i2 < i1)
        {
          tfmt.erase (i2, i1-i2);
          if (elt->prec == -2)
            nsa--;
        }

      const char *tval;
      if (lo_ieee_isinf (dval))
        {
          if (elt->flags.find ('+') != std::string::npos)
            tval = (dval < 0 ? "-Inf" : "+Inf");
          else
            tval = (dval < 0 ? "-Inf" : "Inf");
        }
      else
        {
          if (elt->flags.find ('+') != std::string::npos)
            tval = (lo_ieee_is_NA (dval) ? "+NA" : "+NaN");
          else
            tval = (lo_ieee_is_NA (dval) ? "NA" : "NaN");
        }

      retval += do_printf_conv (os, tfmt.c_str (), nsa, sa_1, sa_2, tval,
                                who);
    }
  else
    {
      static std::string llmod
        = (sizeof (long) == sizeof (int64_t) ? "l" : "ll");

      char type = elt->type;

      switch (type)
        {
        case 'd': case 'i': case 'c':
          if (ok_for_signed_int_conv (val))
            {
              octave_int64 tval = val.int64_scalar_value ();

              // Insert "long" modifier.
              tfmt.replace (tfmt.rfind (type), 1, llmod + type);

              retval += do_printf_conv (os, tfmt.c_str (), nsa, sa_1, sa_2,
                                        tval.value (), who);
            }
          else
            {
              tfmt = switch_to_g_format (elt);

              double dval = val.double_value (true);

              retval += do_printf_conv (os, tfmt.c_str (), nsa, sa_1, sa_2,
                                        dval, who);
            }
          break;

        case 'o': case 'x': case 'X': case 'u':
          if (ok_for_unsigned_int_conv (val))
            {
              octave_uint64 tval = val.uint64_scalar_value ();

              // Insert "long" modifier.
              tfmt.replace (tfmt.rfind (type), 1, llmod + type);

              retval += do_printf_conv (os, tfmt.c_str (), nsa, sa_1, sa_2,
                                        tval.value (), who);
            }
          else
            {
              tfmt = switch_to_g_format (elt);

              double dval = val.double_value (true);

              retval += do_printf_conv (os, tfmt.c_str (), nsa, sa_1, sa_2,
                                        dval, who);
            }
          break;

        case 'f': case 'e': case 'E':
        case 'g': case 'G':
          {
            double dval = val.double_value (true);

            retval += do_printf_conv (os, tfmt.c_str (), nsa, sa_1, sa_2,
                                      dval, who);
          }
          break;

        default:
          // Note: error is member fcn from base_stream, not ::error.
          // This error does not halt execution so "return ..." must exist.
          error (who, "invalid format specifier");
          return -1;
          break;
        }
    }

  return retval;
}

void
base_stream::field_width_error (const std::string& who) const
{
  ::error ("%s: invalid field width, must be integer >= 0 and <= INT_MAX",
           who.c_str ());
}

int
base_stream::do_printf (printf_format_list& fmt_list,
                        const octave_value_list& args,
                        const std::string& who)
{
  int retval = 0;

  octave_idx_type m_nconv = fmt_list.num_conversions ();

  std::ostream *osp = preferred_output_stream ();

  if (! osp)
    invalid_operation (who, "writing");
  else
    {
      std::ostream& os = *osp;

      preserve_stream_state stream_state (os);

      const printf_format_elt *elt = fmt_list.first ();

      printf_value_cache val_cache (args, who);

      for (;;)
        {
          octave_quit ();

          if (! elt)
            ::error ("%s: internal error handling format", who.c_str ());

          // NSA is the number of 'star' args to convert.
          int nsa = (elt->fw == -2) + (elt->prec == -2);

          int sa_1 = 0;
          int sa_2 = 0;

          if (nsa > 0)
            {
              sa_1 = val_cache.int_value ();

              if (! val_cache)
                {
                  field_width_error (who);
                  break;
                }
              else
                {
                  if (nsa > 1)
                    {
                      sa_2 = val_cache.int_value ();

                      if (! val_cache)
                        {
                          field_width_error (who);
                          break;
                        }
                    }
                }
            }

          if (elt->type == '%')
            {
              os << '%';
              retval++;
            }
          else if (elt->args == 0 && ! elt->text.empty ())
            {
              os << elt->text;
              retval += (elt->text.length ());
            }
          else if (elt->type == 's' || elt->type == 'c')
            {
              octave_value val = val_cache.get_next_value (elt->type);

              if (val_cache)
                {
                  if (val.is_string ())
                    {
                      std::string sval = val.string_value ();

                      retval += do_printf_string (os, elt, nsa, sa_1,
                                                  sa_2, sval, who);
                    }
                  else
                    retval += do_numeric_printf_conv (os, elt, nsa, sa_1,
                                                      sa_2, val, who);
                }
              else
                break;
            }
          else
            {
              octave_value val = val_cache.get_next_value ();

              if (val_cache)
                {
                  if (! val.isempty ())
                    retval += do_numeric_printf_conv (os, elt, nsa, sa_1,
                                                      sa_2, val, who);
                }
              else
                break;
            }

          if (! os)
            {
              error (who, "write error");
              break;
            }

          elt = fmt_list.next (m_nconv > 0 && ! val_cache.exhausted ());

          if (! elt || (val_cache.exhausted () && elt->args > 0))
            break;
        }
    }

  return retval;
}

int
base_stream::printf (const std::string& fmt,
                     const octave_value_list& args,
                     const std::string& who)
{
  printf_format_list fmt_list (fmt);

  if (fmt_list.num_conversions () == -1)
    ::error ("%s: invalid format specified", who.c_str ());

  return do_printf (fmt_list, args, who);
}

int
base_stream::puts (const std::string& s, const std::string& who)
{
  int retval = -1;

  std::ostream *osp = preferred_output_stream ();

  if (! osp)
    invalid_operation (who, "writing");
  else
    {
      std::ostream& os = *osp;

      os << s;

      if (! os)
        error (who, "write error");
      else
        {
          // FIXME: why does this seem to be necessary?
          // Without it, output from a loop like
          //
          //   for i = 1:100, fputs (stdout, "foo\n"); endfor
          //
          // doesn't seem to go to the pager immediately.
          os.flush ();

          if (os)
            retval = 0;
          else
            error (who, "write error");
        }
    }

  return retval;
}

// Return current error message for this stream.

std::string
base_stream::error (bool clear_err, int& err_num)
{
  err_num = (m_fail ? -1 : 0);

  std::string tmp = m_errmsg;

  if (clear_err)
    clear ();

  return tmp;
}

void
base_stream::invalid_operation (const std::string& who, const char *rw)
{
  // Note: This calls the member fcn error, not ::error from error.h.
  error (who, std::string ("stream not open for ") + rw);
}

int
stream::flush (void)
{
  int retval = -1;

  if (stream_ok ())
    retval = m_rep->flush ();

  return retval;
}

std::string
stream::getl (octave_idx_type max_len, bool& err, const std::string& who)
{
  std::string retval;

  if (stream_ok ())
    retval = m_rep->getl (max_len, err, who);

  return retval;
}

std::string
stream::getl (const octave_value& tc_max_len, bool& err,
              const std::string& who)
{
  err = false;

  int conv_err = 0;

  int max_len = -1;

  if (tc_max_len.is_defined ())
    {
      max_len = convert_to_valid_int (tc_max_len, conv_err);

      if (conv_err || max_len < 0)
        {
          err = true;
          ::error ("%s: invalid maximum length specified", who.c_str ());
        }
    }

  return getl (max_len, err, who);
}

std::string
stream::gets (octave_idx_type max_len, bool& err, const std::string& who)
{
  std::string retval;

  if (stream_ok ())
    retval = m_rep->gets (max_len, err, who);

  return retval;
}

std::string
stream::gets (const octave_value& tc_max_len, bool& err,
              const std::string& who)
{
  err = false;

  int conv_err = 0;

  int max_len = -1;

  if (tc_max_len.is_defined ())
    {
      max_len = convert_to_valid_int (tc_max_len, conv_err);

      if (conv_err || max_len < 0)
        {
          err = true;
          ::error ("%s: invalid maximum length specified", who.c_str ());
        }
    }

  return gets (max_len, err, who);
}

off_t
stream::skipl (off_t count, bool& err, const std::string& who)
{
  off_t retval = -1;

  if (stream_ok ())
    retval = m_rep->skipl (count, err, who);

  return retval;
}

off_t
stream::skipl (const octave_value& tc_count, bool& err,
               const std::string& who)
{
  err = false;

  int conv_err = 0;

  int count = 1;

  if (tc_count.is_defined ())
    {
      if (tc_count.is_scalar_type ()
          && math::isinf (tc_count.scalar_value ()))
        count = -1;
      else
        {
          count = convert_to_valid_int (tc_count, conv_err);

          if (conv_err || count < 0)
            {
              err = true;
              ::error ("%s: invalid number of lines specified",
                       who.c_str ());
            }
        }
    }

  return skipl (count, err, who);
}

int
stream::seek (off_t offset, int origin)
{
  int status = -1;

  if (stream_ok ())
    {
      clearerr ();

      // Find current position so we can return to it if needed.
      off_t orig_pos = m_rep->tell ();

      // Move to end of file.  If successful, find the offset of the end.
      status = m_rep->seek (0, SEEK_END);

      if (status == 0)
        {
          off_t eof_pos = m_rep->tell ();

          if (origin == SEEK_CUR)
            {
              // Move back to original position, otherwise we will be seeking
              // from the end of file which is probably not the original
              // location.
              m_rep->seek (orig_pos, SEEK_SET);
            }

          // Attempt to move to desired position; may be outside bounds of
          // existing file.
          status = m_rep->seek (offset, origin);

          if (status == 0)
            {
              // Where are we after moving to desired position?
              off_t desired_pos = m_rep->tell ();

              // I don't think save_pos can be less than zero,
              // but we'll check anyway...
              if (desired_pos > eof_pos || desired_pos < 0)
                {
                  // Seek outside bounds of file.
                  // Failure should leave position unchanged.
                  m_rep->seek (orig_pos, SEEK_SET);

                  status = -1;
                }
            }
          else
            {
              // Seeking to the desired position failed.
              // Move back to original position and return failure status.
              m_rep->seek (orig_pos, SEEK_SET);

              status = -1;
            }
        }
    }

  return status;
}

int
stream::seek (const octave_value& tc_offset,
              const octave_value& tc_origin)
{
  int retval = -1;

  // FIXME: should we have octave_value methods that handle off_t explicitly?
  octave_int64 val = tc_offset.xint64_scalar_value ("fseek: invalid value for offset");
  off_t xoffset = val.value ();

  int conv_err = 0;

  int origin = SEEK_SET;

  if (tc_origin.is_string ())
    {
      std::string xorigin = tc_origin.xstring_value ("fseek: invalid value for origin");

      if (xorigin == "bof")
        origin = SEEK_SET;
      else if (xorigin == "cof")
        origin = SEEK_CUR;
      else if (xorigin == "eof")
        origin = SEEK_END;
      else
        conv_err = -1;
    }
  else
    {
      int xorigin = convert_to_valid_int (tc_origin, conv_err);

      if (! conv_err)
        {
          if (xorigin == -1)
            origin = SEEK_SET;
          else if (xorigin == 0)
            origin = SEEK_CUR;
          else if (xorigin == 1)
            origin = SEEK_END;
          else
            conv_err = -1;
        }
    }

  if (conv_err)
    ::error ("fseek: invalid value for origin");

  retval = seek (xoffset, origin);

  if (retval != 0)
    // Note: error is member fcn from stream, not ::error.
    error ("fseek: failed to seek to requested position");

  return retval;
}

off_t
stream::tell (void)
{
  off_t retval = -1;

  if (stream_ok ())
    retval = m_rep->tell ();

  return retval;
}

int
stream::rewind (void)
{
  return seek (0, SEEK_SET);
}

bool
stream::is_open (void) const
{
  bool retval = false;

  if (stream_ok ())
    retval = m_rep->is_open ();

  return retval;
}

void
stream::close (void)
{
  if (stream_ok ())
    {
      m_rep->flush ();
      m_rep->close ();
    }
}

template <typename SRC_T, typename DST_T>
static octave_value
convert_and_copy (std::list<void *>& input_buf_list,
                  octave_idx_type input_buf_elts,
                  octave_idx_type elts_read,
                  octave_idx_type nr, octave_idx_type nc, bool swap,
                  bool do_float_fmt_conv, bool do_NA_conv,
                  mach_info::float_format from_flt_fmt)
{
  typedef typename DST_T::element_type dst_elt_type;

  DST_T conv (dim_vector (nr, nc));

  dst_elt_type *conv_data = conv.fortran_vec ();

  octave_idx_type j = 0;

  for (auto it = input_buf_list.cbegin (); it != input_buf_list.cend (); it++)
    {
      SRC_T *data = static_cast<SRC_T *> (*it);

      if (swap || do_float_fmt_conv)
        {
          if (do_NA_conv)
            {
              for (octave_idx_type i = 0; i < input_buf_elts && j < elts_read;
                   i++, j++)
                {
                  if (swap)
                    swap_bytes<sizeof (SRC_T)> (&data[i]);
                  else if (do_float_fmt_conv)
                    do_float_format_conversion (&data[i], sizeof (SRC_T),
                                                1, from_flt_fmt,
                                                mach_info::native_float_format ());

                  // FIXME: Potentially add conversion code for MIPS NA here
                  //        Bug #59830.
                  // dst_elt_type tmp (data[i]);
                  // if (is_MIPS_NA (tmp))
                  //  tmp = replace_MIPS_NA (tmp);
                  // conv_data[j] = tmp;

                  conv_data[j] = data[i];
                }
            }
          else
            {
              for (octave_idx_type i = 0; i < input_buf_elts && j < elts_read;
                   i++, j++)
                {
                  if (swap)
                    swap_bytes<sizeof (SRC_T)> (&data[i]);
                  else if (do_float_fmt_conv)
                    do_float_format_conversion (&data[i], sizeof (SRC_T),
                                                1, from_flt_fmt,
                                                mach_info::native_float_format ());

                  conv_data[j] = data[i];
                }
            }
        }
      else
        {
          if (do_NA_conv)
            {
              for (octave_idx_type i = 0; i < input_buf_elts && j < elts_read;
                   i++, j++)
                {
                  // FIXME: Potentially add conversion code for MIPS NA here
                  conv_data[j] = data[i];
                }
            }
          else
            {
              for (octave_idx_type i = 0; i < input_buf_elts && j < elts_read;
                   i++, j++)
                conv_data[j] = data[i];
            }
        }

      delete [] data;
    }

  input_buf_list.clear ();

  for (octave_idx_type i = elts_read; i < nr * nc; i++)
    conv_data[i] = dst_elt_type (0);

  return conv;
}

typedef octave_value (*conv_fptr)
  (std::list<void *>& input_buf_list, octave_idx_type input_buf_elts,
 octave_idx_type elts_read, octave_idx_type nr, octave_idx_type nc,
 bool swap, bool do_float_fmt_conv, bool do_NA_conv,
 mach_info::float_format from_flt_fmt);

#define TABLE_ELT(T, U, V, W)                                           \
  conv_fptr_table[oct_data_conv::T][oct_data_conv::U] = convert_and_copy<V, W>

#define FILL_TABLE_ROW(T, V)                    \
  TABLE_ELT (T, dt_int8, V, int8NDArray);       \
  TABLE_ELT (T, dt_uint8, V, uint8NDArray);     \
  TABLE_ELT (T, dt_int16, V, int16NDArray);     \
  TABLE_ELT (T, dt_uint16, V, uint16NDArray);   \
  TABLE_ELT (T, dt_int32, V, int32NDArray);     \
  TABLE_ELT (T, dt_uint32, V, uint32NDArray);   \
  TABLE_ELT (T, dt_int64, V, int64NDArray);     \
  TABLE_ELT (T, dt_uint64, V, uint64NDArray);   \
  TABLE_ELT (T, dt_single, V, FloatNDArray);    \
  TABLE_ELT (T, dt_double, V, NDArray);         \
  TABLE_ELT (T, dt_char, V, charNDArray);       \
  TABLE_ELT (T, dt_schar, V, charNDArray);      \
  TABLE_ELT (T, dt_uchar, V, charNDArray);      \
  TABLE_ELT (T, dt_logical, V, boolNDArray);

octave_value
stream::finalize_read (std::list<void *>& input_buf_list,
                       octave_idx_type input_buf_elts,
                       octave_idx_type elts_read,
                       octave_idx_type nr, octave_idx_type nc,
                       oct_data_conv::data_type input_type,
                       oct_data_conv::data_type output_type,
                       mach_info::float_format ffmt)
{
  octave_value retval;

  static bool initialized = false;

  // Table function pointers for return types x read types.

  static conv_fptr conv_fptr_table[oct_data_conv::dt_unknown][14];

  if (! initialized)
    {
      for (int i = 0; i < oct_data_conv::dt_unknown; i++)
        for (int j = 0; j < 14; j++)
          conv_fptr_table[i][j] = nullptr;

      FILL_TABLE_ROW (dt_int8, int8_t);
      FILL_TABLE_ROW (dt_uint8, uint8_t);
      FILL_TABLE_ROW (dt_int16, int16_t);
      FILL_TABLE_ROW (dt_uint16, uint16_t);
      FILL_TABLE_ROW (dt_int32, int32_t);
      FILL_TABLE_ROW (dt_uint32, uint32_t);
      FILL_TABLE_ROW (dt_int64, int64_t);
      FILL_TABLE_ROW (dt_uint64, uint64_t);
      FILL_TABLE_ROW (dt_single, float);
      FILL_TABLE_ROW (dt_double, double);
      FILL_TABLE_ROW (dt_char, char);
      FILL_TABLE_ROW (dt_schar, signed char);
      FILL_TABLE_ROW (dt_uchar, unsigned char);
      FILL_TABLE_ROW (dt_logical, bool);

      initialized = true;
    }

  bool swap = false;

  if (ffmt == mach_info::flt_fmt_unknown)
    ffmt = float_format ();

  if (mach_info::words_big_endian ())
    swap = (ffmt == mach_info::flt_fmt_ieee_little_endian);
  else
    swap = (ffmt == mach_info::flt_fmt_ieee_big_endian);

  bool do_float_fmt_conv = ((input_type == oct_data_conv::dt_double
                             || input_type == oct_data_conv::dt_single)
                            && ffmt != float_format ());

  bool do_NA_conv = (output_type == oct_data_conv::dt_double);

  switch (output_type)
    {
    case oct_data_conv::dt_int8:
    case oct_data_conv::dt_uint8:
    case oct_data_conv::dt_int16:
    case oct_data_conv::dt_uint16:
    case oct_data_conv::dt_int32:
    case oct_data_conv::dt_uint32:
    case oct_data_conv::dt_int64:
    case oct_data_conv::dt_uint64:
    case oct_data_conv::dt_single:
    case oct_data_conv::dt_double:
    case oct_data_conv::dt_char:
    case oct_data_conv::dt_schar:
    case oct_data_conv::dt_uchar:
    case oct_data_conv::dt_logical:
      {
        conv_fptr fptr = conv_fptr_table[input_type][output_type];

        retval = fptr (input_buf_list, input_buf_elts, elts_read,
                       nr, nc, swap, do_float_fmt_conv, do_NA_conv, ffmt);
      }
      break;

    default:
      ::error ("read: invalid type specification");
    }

  return retval;
}

octave_value
stream::read (const Array<double>& size, octave_idx_type block_size,
              oct_data_conv::data_type input_type,
              oct_data_conv::data_type output_type,
              octave_idx_type skip, mach_info::float_format ffmt,
              octave_idx_type& count)
{
  octave_value retval;

  if (! stream_ok ())
    return retval;

  octave_idx_type nr = -1;
  octave_idx_type nc = -1;

  bool one_elt_size_spec = false;

  // FIXME: We may eventually want to make this extensible.

  // FIXME: We need a better way to ensure that this numbering stays
  // consistent with the order of the elements in the data_type enum in the
  // oct_data_conv class.

  std::ptrdiff_t tmp_count = 0;

  try
    {
      get_size (size, nr, nc, one_elt_size_spec, "fread");
    }
  catch (const execution_exception&)
    {
      invalid_operation ("fread", "reading");

      return retval;
    }

  if (one_elt_size_spec)
    {
      // If NR == 0, Matlab returns [](0x0).

      // If NR > 0, the result will be a column vector with the given
      // number of rows.

      // If NR < 0, then we have Inf and the result will be a column
      // vector but we have to wait to see how big NR will be.

      if (nr == 0)
        nr = nc = 0;
      else
        nc = 1;
    }
  else
    {
      // Matlab returns [] even if there are two elements in the size
      // specification and one is nonzero.

      // If NC < 0 we have [NR, Inf] and we'll wait to decide how big NC
      // should be.

      if (nr == 0 || nc == 0)
        nr = nc = 0;
    }

  octave_idx_type elts_to_read = nr * nc;

  bool read_to_eof = elts_to_read < 0;

  octave_idx_type input_buf_elts = -1;

  if (skip == 0)
    {
      if (read_to_eof)
        input_buf_elts = 1024 * 1024;
      else
        input_buf_elts = elts_to_read;
    }
  else
    input_buf_elts = block_size;

  octave_idx_type input_elt_size
    = oct_data_conv::data_type_size (input_type);

  std::ptrdiff_t input_buf_size
    = static_cast<std::ptrdiff_t> (input_buf_elts) * input_elt_size;

  error_if (input_buf_size < 0);

  // Must also work and return correct type object for 0 elements to read.
  std::istream *isp = input_stream ();

  if (! isp)
    error ("fread: invalid input stream");
  else
    {
      std::istream& is = *isp;

      // Initialize eof_pos variable just once per function call
      off_t eof_pos = 0;
      off_t cur_pos = 0;
      if (skip != 0 && is && ! is.eof ())
        {
          cur_pos = is.tellg ();
          is.seekg (0, is.end);
          eof_pos = is.tellg ();
          is.seekg (cur_pos, is.beg);
        }

      std::list<void *> input_buf_list;

      while (is && ! is.eof ()
             && (read_to_eof || tmp_count < elts_to_read))
        {
          if (! read_to_eof)
            {
              octave_idx_type remaining_elts = elts_to_read - tmp_count;

              if (remaining_elts < input_buf_elts)
                input_buf_size = remaining_elts * input_elt_size;
            }

          char *input_buf = new char [input_buf_size];

          is.read (input_buf, input_buf_size);

          std::size_t gcount = is.gcount ();

          cur_pos += gcount;

          octave_idx_type nel = gcount / input_elt_size;

          tmp_count += nel;

          input_buf_list.push_back (input_buf);

          if (skip != 0 && nel == block_size && is)
            {
              // Attempt to skip.
              // If skip would move past EOF, position at EOF.
              off_t remaining = eof_pos - cur_pos;

              if (remaining < skip)
                {
                  is.seekg (0, is.end);
                  cur_pos = eof_pos;
                }
              else
                {
                  is.seekg (skip, is.cur);
                  cur_pos += skip;
                }
            }
        }

      if (read_to_eof)
        {
          if (nc < 0)
            {
              nc = tmp_count / nr;

              if (tmp_count % nr != 0)
                nc++;
            }
          else
            nr = tmp_count;
        }
      else if (tmp_count == 0)
        {
          nr = 0;
          nc = 0;
        }
      else if (tmp_count != nr * nc)
        {
          if (tmp_count % nr != 0)
            nc = tmp_count / nr + 1;
          else
            nc = tmp_count / nr;

          if (tmp_count < nr)
            nr = tmp_count;
        }

      if (tmp_count > std::numeric_limits<octave_idx_type>::max ())
        error ("fread: number of elements read exceeds max index size");
      else
        count = static_cast<octave_idx_type> (tmp_count);

      retval = finalize_read (input_buf_list, input_buf_elts, count,
                              nr, nc, input_type, output_type, ffmt);
    }

  return retval;
}

octave_idx_type
stream::write (const octave_value& data, octave_idx_type block_size,
               oct_data_conv::data_type output_type,
               octave_idx_type skip, mach_info::float_format flt_fmt)
{
  octave_idx_type retval = -1;

  if (! stream_ok ())
    invalid_operation ("fwrite", "writing");
  else
    {
      if (flt_fmt == mach_info::flt_fmt_unknown)
        flt_fmt = float_format ();

      octave_idx_type status = data.write (*this, block_size, output_type,
                                           skip, flt_fmt);

      if (status < 0)
        error ("fwrite: write error");
      else
        retval = status;
    }

  return retval;
}

template <typename T, typename V>
static void
convert_chars (const void *data, void *conv_data, octave_idx_type n_elts)
{
  const T *tt_data = static_cast<const T *> (data);

  V *vt_data = static_cast<V *> (conv_data);

  for (octave_idx_type i = 0; i < n_elts; i++)
    vt_data[i] = tt_data[i];
}

template <typename T, typename V>
static void
convert_ints (const T *data, void *conv_data, octave_idx_type n_elts,
              bool swap)
{
  typedef typename V::val_type val_type;

  val_type *vt_data = static_cast<val_type *> (conv_data);

  for (octave_idx_type i = 0; i < n_elts; i++)
    {
      // Yes, we want saturation semantics when converting to an integer type.
      V val (data[i]);

      vt_data[i] = val.value ();

      if (swap)
        swap_bytes<sizeof (val_type)> (&vt_data[i]);
    }
}

template <typename T>
class ultimate_element_type
{
public:
  typedef T type;
};

template <typename T>
class ultimate_element_type<octave_int<T>>
{
public:
  typedef T type;
};

template <typename T>
static bool
convert_data (const T *data, void *conv_data, octave_idx_type n_elts,
              oct_data_conv::data_type output_type,
              mach_info::float_format flt_fmt)
{
  bool retval = true;

  bool swap = false;

  if (mach_info::words_big_endian ())
    swap = (flt_fmt == mach_info::flt_fmt_ieee_little_endian);
  else
    swap = (flt_fmt == mach_info::flt_fmt_ieee_big_endian);

  bool do_float_conversion = flt_fmt != mach_info::float_format ();

  typedef typename ultimate_element_type<T>::type ult_elt_type;

  switch (output_type)
    {
    case oct_data_conv::dt_char:
      convert_chars<ult_elt_type, char> (data, conv_data, n_elts);
      break;

    case oct_data_conv::dt_schar:
      convert_chars<ult_elt_type, signed char> (data, conv_data, n_elts);
      break;

    case oct_data_conv::dt_uchar:
      convert_chars<ult_elt_type, unsigned char> (data, conv_data, n_elts);
      break;

    case oct_data_conv::dt_int8:
      convert_ints<T, octave_int8> (data, conv_data, n_elts, swap);
      break;

    case oct_data_conv::dt_uint8:
      convert_ints<T, octave_uint8> (data, conv_data, n_elts, swap);
      break;

    case oct_data_conv::dt_int16:
      convert_ints<T, octave_int16> (data, conv_data, n_elts, swap);
      break;

    case oct_data_conv::dt_uint16:
      convert_ints<T, octave_uint16> (data, conv_data, n_elts, swap);
      break;

    case oct_data_conv::dt_int32:
      convert_ints<T, octave_int32> (data, conv_data, n_elts, swap);
      break;

    case oct_data_conv::dt_uint32:
      convert_ints<T, octave_uint32> (data, conv_data, n_elts, swap);
      break;

    case oct_data_conv::dt_int64:
      convert_ints<T, octave_int64> (data, conv_data, n_elts, swap);
      break;

    case oct_data_conv::dt_uint64:
      convert_ints<T, octave_uint64> (data, conv_data, n_elts, swap);
      break;

    case oct_data_conv::dt_single:
      {
        float *vt_data = static_cast<float *> (conv_data);

        for (octave_idx_type i = 0; i < n_elts; i++)
          {
            vt_data[i] = data[i];

            if (do_float_conversion)
              do_float_format_conversion (&vt_data[i], 1, flt_fmt);
          }
      }
      break;

    case oct_data_conv::dt_double:
      {
        double *vt_data = static_cast<double *> (conv_data);

        for (octave_idx_type i = 0; i < n_elts; i++)
          {
            vt_data[i] = data[i];

            if (do_float_conversion)
              do_double_format_conversion (&vt_data[i], 1, flt_fmt);
          }
      }
      break;

    default:
      ::error ("write: invalid type specification");
    }

  return retval;
}

bool
stream::write_bytes (const void *data, std::size_t nbytes)
{
  bool status = false;

  std::ostream *osp = output_stream ();

  if (osp)
    {
      std::ostream& os = *osp;

      if (os)
        {
          os.write (static_cast<const char *> (data), nbytes);

          if (os)
            status = true;
        }
    }

  return status;
}

bool
stream::skip_bytes (std::size_t skip)
{
  bool status = false;

  std::ostream *osp = output_stream ();

  if (! osp)
    return false;

  std::ostream& os = *osp;

  // Seek to skip when inside bounds of existing file.
  // Otherwise, write NUL to skip.
  off_t orig_pos = tell ();

  seek (0, SEEK_END);

  off_t eof_pos = tell ();

  // Is it possible for this to fail to return us to the original position?
  seek (orig_pos, SEEK_SET);

  std::size_t remaining = eof_pos - orig_pos;

  if (remaining < skip)
    {
      seek (0, SEEK_END);

      // FIXME: probably should try to write larger blocks...
      unsigned char zero = 0;
      for (std::size_t j = 0; j < skip - remaining; j++)
        os.write (reinterpret_cast<const char *> (&zero), 1);
    }
  else
    seek (skip, SEEK_CUR);

  if (os)
    status = true;

  return status;
}

template <typename T>
octave_idx_type
stream::write (const Array<T>& data, octave_idx_type block_size,
               oct_data_conv::data_type output_type,
               octave_idx_type skip,
               mach_info::float_format flt_fmt)
{
  bool swap = false;

  if (mach_info::words_big_endian ())
    swap = (flt_fmt == mach_info::flt_fmt_ieee_little_endian);
  else
    swap = (flt_fmt == mach_info::flt_fmt_ieee_big_endian);

  bool do_data_conversion = (swap || ! is_equivalent_type<T> (output_type)
                             || flt_fmt != mach_info::float_format ());

  octave_idx_type nel = data.numel ();

  octave_idx_type chunk_size;

  if (skip != 0)
    chunk_size = block_size;
  else if (do_data_conversion)
    chunk_size = 1024 * 1024;
  else
    chunk_size = nel;

  octave_idx_type i = 0;

  const T *pdata = data.data ();

  while (i < nel)
    {
      if (skip != 0)
        {
          if (! skip_bytes (skip))
            return -1;
        }

      octave_idx_type remaining_nel = nel - i;

      if (chunk_size > remaining_nel)
        chunk_size = remaining_nel;

      bool status = false;

      if (do_data_conversion)
        {
          std::size_t output_size
            = chunk_size * oct_data_conv::data_type_size (output_type);

          OCTAVE_LOCAL_BUFFER (unsigned char, conv_data, output_size);

          status = convert_data (&pdata[i], conv_data, chunk_size,
                                 output_type, flt_fmt);

          if (status)
            status = write_bytes (conv_data, output_size);
        }
      else
        status = write_bytes (pdata, sizeof (T) * chunk_size);

      if (! status)
        return -1;

      i += chunk_size;
    }

  return nel;
}

#define INSTANTIATE_WRITE(T)                                            \
  template                                                              \
  OCTINTERP_API octave_idx_type                                         \
  stream::write (const Array<T>& data, octave_idx_type block_size,      \
                 oct_data_conv::data_type output_type,                  \
                 octave_idx_type skip,                                  \
                 mach_info::float_format flt_fmt)

INSTANTIATE_WRITE (octave_int8);
INSTANTIATE_WRITE (octave_uint8);
INSTANTIATE_WRITE (octave_int16);
INSTANTIATE_WRITE (octave_uint16);
INSTANTIATE_WRITE (octave_int32);
INSTANTIATE_WRITE (octave_uint32);
INSTANTIATE_WRITE (octave_int64);
INSTANTIATE_WRITE (octave_uint64);
INSTANTIATE_WRITE (int8_t);
INSTANTIATE_WRITE (uint8_t);
INSTANTIATE_WRITE (int16_t);
INSTANTIATE_WRITE (uint16_t);
INSTANTIATE_WRITE (int32_t);
INSTANTIATE_WRITE (uint32_t);
INSTANTIATE_WRITE (int64_t);
INSTANTIATE_WRITE (uint64_t);
INSTANTIATE_WRITE (bool);
#if defined (OCTAVE_HAVE_OVERLOAD_CHAR_INT8_TYPES)
INSTANTIATE_WRITE (char);
#endif
INSTANTIATE_WRITE (float);
INSTANTIATE_WRITE (double);

octave_value
stream::scanf (const std::string& fmt, const Array<double>& size,
               octave_idx_type& count, const std::string& who)
{
  octave_value retval;

  if (stream_ok ())
    retval = m_rep->scanf (fmt, size, count, who);

  return retval;
}

octave_value
stream::scanf (const octave_value& fmt, const Array<double>& size,
               octave_idx_type& count, const std::string& who)
{
  octave_value retval = Matrix ();

  if (fmt.is_string ())
    {
      std::string sfmt = fmt.string_value ();

      if (fmt.is_sq_string ())
        sfmt = do_string_escapes (sfmt);

      retval = scanf (sfmt, size, count, who);
    }
  else
    {
      // Note: error is member fcn from stream, not ::error.
      error (who + ": format must be a string");
    }

  return retval;
}

octave_value_list
stream::oscanf (const std::string& fmt, const std::string& who)
{
  octave_value_list retval;

  if (stream_ok ())
    retval = m_rep->oscanf (fmt, who);

  return retval;
}

octave_value_list
stream::oscanf (const octave_value& fmt, const std::string& who)
{
  octave_value_list retval;

  if (fmt.is_string ())
    {
      std::string sfmt = fmt.string_value ();

      if (fmt.is_sq_string ())
        sfmt = do_string_escapes (sfmt);

      retval = oscanf (sfmt, who);
    }
  else
    {
      // Note: error is member fcn from stream, not ::error.
      error (who + ": format must be a string");
    }

  return retval;
}

octave_value
stream::textscan (const std::string& fmt, octave_idx_type ntimes,
                  const octave_value_list& options,
                  const std::string& who, octave_idx_type& count)
{
  return (stream_ok ()
          ? m_rep->do_textscan (fmt, ntimes, options, who, count)
          : octave_value ());
}

int
stream::printf (const std::string& fmt, const octave_value_list& args,
                const std::string& who)
{
  int retval = -1;

  if (stream_ok ())
    retval = m_rep->printf (fmt, args, who);

  return retval;
}

int
stream::printf (const octave_value& fmt, const octave_value_list& args,
                const std::string& who)
{
  int retval = 0;

  if (fmt.is_string ())
    {
      std::string sfmt = fmt.string_value ();

      if (fmt.is_sq_string ())
        sfmt = do_string_escapes (sfmt);

      retval = printf (sfmt, args, who);
    }
  else
    {
      // Note: error is member fcn from stream, not ::error.
      error (who + ": format must be a string");
    }

  return retval;
}

int
stream::puts (const std::string& s, const std::string& who)
{
  int retval = -1;

  if (stream_ok ())
    retval = m_rep->puts (s, who);

  return retval;
}

// FIXME: maybe this should work for string arrays too.

int
stream::puts (const octave_value& tc_s, const std::string& who)
{
  int retval = -1;

  if (tc_s.is_string ())
    {
      std::string s = tc_s.string_value ();
      retval = puts (s, who);
    }
  else
    {
      // Note: error is member fcn from stream, not ::error.
      error (who + ": argument must be a string");
    }

  return retval;
}

bool
stream::eof (void) const
{
  int retval = -1;

  if (stream_ok ())
    retval = m_rep->eof ();

  return retval;
}

std::string
stream::error (bool clear, int& err_num)
{
  std::string retval = "invalid stream object";

  if (stream_ok (false))
    retval = m_rep->error (clear, err_num);

  return retval;
}

std::string
stream::name (void) const
{
  std::string retval;

  if (stream_ok ())
    retval = m_rep->name ();

  return retval;
}

int
stream::mode (void) const
{
  int retval = 0;

  if (stream_ok ())
    retval = m_rep->mode ();

  return retval;
}

mach_info::float_format
stream::float_format (void) const
{
  mach_info::float_format retval = mach_info::flt_fmt_unknown;

  if (stream_ok ())
    retval = m_rep->float_format ();

  return retval;
}

std::string
stream::mode_as_string (int mode)
{
  std::string retval = "???";
  std::ios::openmode in_mode = static_cast<std::ios::openmode> (mode);

  if (in_mode == std::ios::in)
    retval = "r";
  else if (in_mode == std::ios::out
           || in_mode == (std::ios::out | std::ios::trunc))
    retval = "w";
  else if (in_mode == (std::ios::out | std::ios::app))
    retval = "a";
  else if (in_mode == (std::ios::in | std::ios::out))
    retval = "r+";
  else if (in_mode == (std::ios::in | std::ios::out | std::ios::trunc))
    retval = "w+";
  else if (in_mode == (std::ios::in | std::ios::out | std::ios::ate))
    retval = "a+";
  else if (in_mode == (std::ios::in | std::ios::binary))
    retval = "rb";
  else if (in_mode == (std::ios::out | std::ios::binary)
           || in_mode == (std::ios::out | std::ios::trunc | std::ios::binary))
    retval = "wb";
  else if (in_mode == (std::ios::out | std::ios::app | std::ios::binary))
    retval = "ab";
  else if (in_mode == (std::ios::in | std::ios::out | std::ios::binary))
    retval = "r+b";
  else if (in_mode == (std::ios::in | std::ios::out | std::ios::trunc
                       | std::ios::binary))
    retval = "w+b";
  else if (in_mode == (std::ios::in | std::ios::out | std::ios::ate
                       | std::ios::binary))
    retval = "a+b";

  return retval;
}

stream_list::stream_list (interpreter& interp)
  : m_list (), m_lookup_cache (m_list.end ()), m_stdin_file (-1),
    m_stdout_file (-1), m_stderr_file (-1)
{
  stream stdin_stream = istream::create (&std::cin, "stdin");

  // This uses octave_stdout (see pager.h), not std::cout so that
  // Octave's standard output stream will pass through the pager.

  // FIXME: we should be accessing octave_stdout from the interpreter.

  output_system& output_sys = interp.get_output_system ();

  stream stdout_stream
    = ostream::create (&(output_sys.__stdout__ ()), "stdout");

  stream stderr_stream = ostream::create (&std::cerr, "stderr");

  m_stdin_file = insert (stdin_stream);
  m_stdout_file = insert (stdout_stream);
  m_stderr_file = insert (stderr_stream);
}

stream_list::~stream_list (void)
{
  clear ();
}

int stream_list::insert (stream& os)
{
  // Insert item with key corresponding to file-descriptor.

  int stream_number = os.file_number ();

  if (stream_number == -1)
    return stream_number;

  // Should we test for
  //
  //  (m_list.find (stream_number) != m_list.end ()
  //   && m_list[stream_number].is_open ())
  //
  // and respond with "error ("internal error: ...")"?  It should not
  // happen except for some bug or if the user has opened a stream with
  // an interpreted command, but closed it directly with a system call
  // in an oct-file; then the kernel knows the fd is free, but Octave
  // does not know.  If it happens, it should not do harm here to simply
  // overwrite this entry, although the wrong entry might have done harm
  // before.

  if (m_list.size () >= m_list.max_size ())
    ::error ("could not create file id");

  m_list[stream_number] = os;

  return stream_number;
}

OCTAVE_NORETURN static
void
err_invalid_file_id (int fid, const std::string& who)
{
  if (who.empty ())
    ::error ("invalid stream number = %d", fid);
  else
    ::error ("%s: invalid stream number = %d", who.c_str (), fid);
}

stream stream_list::lookup (int fid, const std::string& who) const
{
  stream retval;

  if (fid < 0)
    err_invalid_file_id (fid, who);

  if (m_lookup_cache != m_list.end () && m_lookup_cache->first == fid)
    retval = m_lookup_cache->second;
  else
    {
      ostrl_map::const_iterator iter = m_list.find (fid);

      if (iter == m_list.end ())
        err_invalid_file_id (fid, who);

      retval = iter->second;
      m_lookup_cache = iter;
    }

  return retval;
}

stream stream_list::lookup (const octave_value& fid,
                            const std::string& who) const
{
  int i = get_file_number (fid);

  return lookup (i, who);
}

int stream_list::remove (int fid, const std::string& who)
{
  // Can't remove stdin (std::cin), stdout (std::cout), or stderr (std::cerr).
  if (fid < 3)
    err_invalid_file_id (fid, who);

  auto iter = m_list.find (fid);

  if (iter == m_list.end ())
    err_invalid_file_id (fid, who);

  stream os = iter->second;
  m_list.erase (iter);
  m_lookup_cache = m_list.end ();

  // FIXME: is this check redundant?
  if (! os.is_valid ())
    err_invalid_file_id (fid, who);

  os.close ();

  return 0;
}

int stream_list::remove (const octave_value& fid, const std::string& who)
{
  int retval = -1;

  if (fid.is_string () && fid.string_value () == "all")
    {
      clear (false);

      retval = 0;
    }
  else
    {
      int i = get_file_number (fid);

      retval = remove (i, who);
    }

  return retval;
}

void stream_list::clear (bool flush)
{
  if (flush)
    {
      // Flush stdout and stderr.
      m_list[1].flush ();
      m_list[2].flush ();
    }

  for (auto iter = m_list.begin (); iter != m_list.end (); )
    {
      int fid = iter->first;
      if (fid < 3)  // Don't delete stdin, stdout, stderr
        {
          iter++;
          continue;
        }

      stream os = iter->second;

      std::string name = os.name ();
      std::transform (name.begin (), name.end (), name.begin (), tolower);

      // FIXME: This test for gnuplot is hardly foolproof.
      if (name.find ("gnuplot") != std::string::npos)
        {
          // Don't close down pipes to gnuplot
          iter++;
          continue;
        }

      // Normal file handle.  Close and delete from m_list.
      if (os.is_valid ())
        os.close ();

      m_list.erase (iter++);
    }

  m_lookup_cache = m_list.end ();
}

string_vector stream_list::get_info (int fid) const
{
  string_vector retval (4);

  if (fid < 0)
    return retval;

  stream os;
  if (m_lookup_cache != m_list.end () && m_lookup_cache->first == fid)
    os = m_lookup_cache->second;
  else
    {
      ostrl_map::const_iterator iter = m_list.find (fid);

      if (iter == m_list.end ())
        return retval;

      os = iter->second;
      m_lookup_cache = iter;
    }

  if (! os.is_valid ())
    return retval;

  retval(0) = os.name ();
  retval(1) = stream::mode_as_string (os.mode ());
  retval(2) = mach_info::float_format_as_string (os.float_format ());
  retval(3) = os.encoding ();

  return retval;
}

string_vector stream_list::get_info (const octave_value& fid) const
{
  int conv_err = 0;

  if (fid.is_single_type ())
    ::error ("file id must be a file object or integer value");

  int int_fid = convert_to_valid_int (fid, conv_err);

  if (conv_err)
    ::error ("file id must be a file object or integer value");

  return get_info (int_fid);
}

std::string stream_list::list_open_files (void) const
{
  std::ostringstream buf;

  buf << "\n"
      << "  number  mode  arch       name\n"
      << "  ------  ----  ----       ----\n";

  for (const auto& fid_strm : m_list)
    {
      stream os = fid_strm.second;

      buf << "  "
          << std::setiosflags (std::ios::right)
          << std::setw (4) << fid_strm.first << "     "
          // reset necessary in addition to setiosflags since this is one stmt.
          << std::resetiosflags (std::ios::adjustfield)
          << std::setiosflags (std::ios::left)
          << std::setw (3)
          << stream::mode_as_string (os.mode ())
          << "  "
          << std::setw (9)
          << mach_info::float_format_as_string (os.float_format ())
          << "  "
          << os.name () << "\n";
    }

  buf << "\n";

  return buf.str ();
}

octave_value stream_list::open_file_numbers (void) const
{
  Matrix retval (1, m_list.size (), 0.0);

  int num_open = 0;

  for (const auto& fid_strm : m_list)
    {
      // Skip stdin, stdout, and stderr.
      if (fid_strm.first > 2 && fid_strm.second)
        retval(0, num_open++) = fid_strm.first;
    }

  retval.resize ((num_open > 0), num_open);

  return retval;
}

int stream_list::get_file_number (const octave_value& fid) const
{
  int retval = -1;

  if (fid.is_string ())
    {
      std::string nm = fid.string_value ();

      for (const auto& fid_strm : m_list)
        {
          // stdin, stdout, and stderr are unnamed.
          if (fid_strm.first > 2)
            {
              stream os = fid_strm.second;

              if (os && os.name () == nm)
                {
                  retval = fid_strm.first;
                  break;
                }
            }
        }
    }
  else if (fid.is_single_type ())
    ::error ("file id must be a file object, std::string, or integer value");
  else
    {
      int conv_err = 0;

      int int_fid = convert_to_valid_int (fid, conv_err);

      if (conv_err)
        ::error ("file id must be a file object, std::string, or integer value");

      retval = int_fid;
    }

  return retval;
}

octave_value stream_list::stdin_file (void) const
{
  return octave_value (m_stdin_file);
}

octave_value stream_list::stdout_file (void) const
{
  return octave_value (m_stdout_file);
}

octave_value stream_list::stderr_file (void) const
{
  return octave_value (m_stderr_file);
}

OCTAVE_END_NAMESPACE(octave)
