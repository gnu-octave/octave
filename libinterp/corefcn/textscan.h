/*

Copyright (C) 2015-2016 Lachlan Andrew, Monash University

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

// @file
// Implementation of textscan, a versatile text parser.

#if ! defined (octave_textscan_h)
#define octave_textscan_h 1

#include "octave-config.h"

#include <iosfwd>
#include <list>
#include <string>

// These are only needed as arguments to private functions, so they
// are also treated as private.

class delimited_stream;
class textscan_format_elt;
class textscan_format_list;

#include "Cell.h"
#include "ov.h"
#include "ovl.h"

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

  textscan (void);

  ~textscan (void) { }

  octave_value scan (std::istream& isp, const octave_value_list& args);

  octave_value scan (std::istream& isp, const octave_value_list& args,
                     octave_idx_type& count);

private:

  friend class textscan_format_list;

  std::string buf;

  // Three cases for delim_table and delim_list
  // 1. delim_table empty, delim_list empty:  whitespace delimiters
  // 2. delim_table = look-up table of delim chars, delim_list empty.
  // 3. delim_table non-empty, delim_list = Cell array of delim strings

  std::string whitespace_table;

  // delim_table[i] == '\0' if i is not a delimiter.
  std::string delim_table;

  // String of delimiter characters.
  std::string delims;

  Cell comment_style;

  // How far ahead to look to detect an open comment.
  int comment_len;

  // First character of open comment.
  int comment_char;

  octave_idx_type buffer_size;

  std::string date_locale;

  // 'inf' and 'nan' for formatted_double.
  Cell inf_nan;

  // Array of strings of delimiters.
  Cell delim_list;

  // Longest delimiter.
  int delim_len;

  octave_value empty_value;
  std::string exp_chars;
  int header_lines;
  Cell treat_as_empty;

  // Longest string to treat as "N/A".
  int treat_as_empty_len;

  std::string whitespace;

  short eol1;
  short eol2;
  short return_on_error;

  bool collect_output;
  bool multiple_delims_as_one;
  bool default_exp;
  bool numeric_delim;

  octave_idx_type lines;

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

  int  skip_whitespace (delimited_stream& is, bool EOLstop = false);

  int  skip_delim (delimited_stream& is);

  bool is_delim (unsigned char ch) const
  {
    return ((delim_table.empty () && (isspace (ch) || ch == eol1 || ch == eol2))
            || delim_table[ch] != '\0');
  }

  bool isspace (unsigned int ch) const { return whitespace_table[ch & 0xff]; }

  // True if the only delimiter is whitespace.
  bool whitespace_delim (void) const { return delim_table.empty (); }

  // No copying!

  textscan (const textscan&);

  textscan& operator = (const textscan&);
};

#endif
