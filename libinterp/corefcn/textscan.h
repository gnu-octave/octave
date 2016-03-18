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

#include "Cell.h"
#include "ov.h"

// Delimited stream, optimised to read strings of characters separated
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

  ~delimited_stream (void);

  // Called when optimised sequence of get is finished.  Ensures that
  // there is a remaining delimiter in buf, or loads more data in.
  void field_done (void)
  {
    if (idx >= last)
      refresh_buf ();
  }

  // Load new data into buffer, and set eob, last, idx.
  // Return EOF at end of file, 0 otherwise.
  int refresh_buf (void);

  // Get a character, relying on caller to call field_done if
  // a delimiter has been reached.
  int get (void)   { return delimited ? *idx++ : get_undelim (); }

  // Get a character, checking for underrun of the buffer.
  int get_undelim (void);

  // Read character that will be got by the next get.
  int peek (void)   { return *idx; }

  // Read character that will be got by the next get.
  int peek_undelim (void);

  // Undo a 'get' or 'get_undelim'.  It is the caller's responsibility
  // to avoid overflow by calling putbacks only for a character got by
  // get() or get_undelim(), with no intervening
  // get, get_delim, field_done, refresh_buf, getline, read or seekg.
  void putback (char /*ch*/ = 0)  { --idx; }

  int getline  (std::string& dest, char delim);

  // int skipline (char delim);

  char *read (char *buffer, int size, char* &new_start);

  // Return a position suitable to "seekg", valid only within this
  // block between calls to field_done.
  char *tellg (void) { return idx; }

  void seekg (char *old_idx) { idx = old_idx; }

  bool eof (void)
  {
    return (eob == buf && i_stream.eof ()) || (flags & std::ios_base::eofbit);
  }

  operator const void* (void) { return (!eof () && !flags) ? this : 0; }

  bool fail (void) { return flags & std::ios_base::failbit; }

  std::ios_base::iostate rdstate (void) { return flags; }

  void setstate (std::ios_base::iostate m) { flags = flags | m; }

  void clear (std::ios_base::iostate m
              = (std::ios_base::eofbit & ~std::ios_base::eofbit))
  {
    flags = flags & m;
  }

  // Report if any characters have been consumed.
  // (get, read etc. not cancelled by putback or seekg)

  void progress_benchmark (void) { progress_marker = idx; }

  bool no_progress (void) { return progress_marker == idx; }

private:

  // Number of characters to read from the file at once.
  int bufsize;

  // Stream to read from.
  std::istream& i_stream;

  // Temporary storage for a "chunk" of data.
  char *buf;

  // Current read pointer.
  char *idx;

  // Location of last delimiter in the buffer at buf (undefined if
  // delimited is false).
  char *last;

  // Position after last character in buffer.
  char *eob;

  // True if there is delimiter in the bufer after idx.
  bool delimited;

  // Longest lookahead required.
  int longest;

  // Sequence of single-character delimiters.
  const std::string delims;

  // Position of start of buf in original stream.
  std::streampos buf_in_file;

  // Marker to see if a read consumes any characters.
  char *progress_marker;

  std::ios_base::iostate flags; 

  // No copying!

  delimited_stream (const delimited_stream&);

  delimited_stream& operator = (const delimited_stream&);
};

// A single conversion specifier, such as %f or %c.

class
OCTINTERP_API
textscan_format_elt
{
public:

  enum special_conversion
  {
    whitespace_conversion = 1,
    literal_conversion = 2
  };

  textscan_format_elt (const char *txt = 0, int w = 0, int p = -1,
                       int bw = 0, bool dis = false, char typ = '\0',
                       const std::string& ch_class = std::string ())
    : text (strsave (txt)), width (w), prec (p), bitwidth (bw),
      char_class (ch_class), type (typ), discard (dis),
      numeric(typ == 'd' || typ == 'u' || type == 'f' || type == 'n')
  { }

  textscan_format_elt (const textscan_format_elt& e)
    : text (strsave (e.text)), width (e.width), prec (e.prec),
      bitwidth (e.bitwidth), char_class (e.char_class), type (e.type),
      discard (e.discard), numeric (e.numeric)
  { }

  textscan_format_elt& operator = (const textscan_format_elt& e)
  {
    if (this != &e)
      {
        text = strsave (e.text);
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

  ~textscan_format_elt (void) { delete [] text; }

  // The C-style format string.
  const char *text;

  // The maximum field width.
  unsigned int width;

  // The maximum number of digits to read after the decimal in a
  // floating point conversion.
  int prec;

  // The size of the result.  For integers, bitwidth may be 8, 16, 34,
  // or 64.  For floating point values, bitwidth may be 32 or 64.
  int bitwidth;

  // The class of characters in a `[' or `^' format.
  std::string char_class;

  // Type of conversion
  //  -- `d', `u', `f', `n', `s', `q', `c', `%', `C', `D', `[' or `^'.
  char type;

  // TRUE if we are not storing the result of this conversion.
  bool discard;

  // TRUE if the type is 'd', 'u', 'f', 'n'
  bool numeric;
};

class textscan;

// The (parsed) sequence of format specifiers.

class
OCTINTERP_API
textscan_format_list
{
public:

  textscan_format_list (const std::string& fmt = std::string ());

  ~textscan_format_list (void);

  octave_idx_type num_conversions (void) const { return nconv; }

  // The length can be different than the number of conversions.
  // For example, "x %d y %d z" has 2 conversions but the length of
  // the list is 3 because of the characters that appear after the
  // last conversion.

  octave_idx_type numel (void) const { return list.numel (); }

  const textscan_format_elt *first (void)
  {
    curr_idx = 0;
    return current ();
  }

  const textscan_format_elt *current (void) const
  {
    return list.numel () > 0 ? list.elem (curr_idx) : 0;
  }

  const textscan_format_elt *next (bool cycle = true)
  {
    curr_idx++;

    if (curr_idx >= list.numel ())
      {
        if (cycle)
          curr_idx = 0;
        else
          return 0;
      }

    return current ();
  }

  void printme (void) const;

  bool ok (void) const { return (nconv >= 0); }

  operator const void* (void) const { return ok () ? this : 0; }

  // True if number of %f to be set from data file.
  bool set_from_first;

  // At least one conversion specifier is s,q,c, or [...].
  bool has_string;

  int read_first_row (delimited_stream& is, textscan& ts);

  std::list<octave_value> out_buf (void) const { return (output_container); }

private:

  // Number of conversions specified by this format string, or -1 if
  // invalid conversions have been found.
  octave_idx_type nconv;

  // Index to current element;
  octave_idx_type curr_idx;

  // FIXME -- maybe LIST should be a std::list object?
  // List of format elements.
  Array<textscan_format_elt*> list;

  // list holding column arrays of types specified by conversions
  std::list<octave_value > output_container;

  // Temporary buffer.
  std::ostringstream *buf;

  void add_elt_to_list (unsigned int width, int prec, int bitwidth,
                        octave_value val_type, bool discard,
                        char type, octave_idx_type& num_elts,
                        const std::string& char_class = std::string ());

  void process_conversion (const std::string& s, size_t& i, size_t n,
                           octave_idx_type& num_elts);

  int finish_conversion (const std::string& s, size_t& i, size_t n,
                         unsigned int& width, int& prec, int& bitwidth,
                         octave_value& val_type,
                         bool discard, char& type,
                         octave_idx_type& num_elts);
  // No copying!

  textscan_format_list (const textscan_format_list&);

  textscan_format_list& operator = (const textscan_format_list&);
};


// Main class to implement textscan.  Read data and parse it
// according to a textscan_format_list.
//
// The calling sequence is
//
//   textscan ();
//   parse_options (...);
//   scan (...);

class
textscan
{
public:

  textscan (void)
    : buf (""), delim_table (""), delims (), comment_len (0), comment_char(-2),
      buffer_size (0),
      empty_value (octave_NaN), exp_chars ("edED"), header_lines (0),
      treat_as_empty_len (0), whitespace (" \b\t"), eol1('\r'), eol2('\n'),
      return_on_error (2), collect_output (false),
      multiple_delims_as_one (false), default_exp (true),
      numeric_delim (false), lines (0)
    {
      inf_nan = Cell (dim_vector (1,2));
      inf_nan(0) = Cell (octave_value ("inf"));
      inf_nan(1) = Cell (octave_value ("nan"));
    };

  ~textscan (void) { }

  octave_value scan (std::istream* isp, textscan_format_list& fmt_list,
                     octave_idx_type ntimes);

  void parse_options (const octave_value_list& args, int first_param,
                      textscan_format_list& formats);

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

  int read_format_once (delimited_stream &isp, textscan_format_list& fmt_list,
                        std::list<octave_value> & retval,
                        Array<octave_idx_type> row, int& done_after);

  void scan_one (delimited_stream& is, const textscan_format_elt& fmt,
                 octave_value& ov, Array<octave_idx_type> row);

  // Methods to process a particular conversion specifier.
  double read_double (delimited_stream& is,
                      const textscan_format_elt& fmt) const;

  void scan_complex (delimited_stream& is, const textscan_format_elt& fmt,
                     Complex& val) const;

  int scan_bracket (delimited_stream& is, const char *pattern,
                    std::string& val) const;

  int scan_caret (delimited_stream& is, const char *, std::string& val) const;

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

  char *get_field (delimited_stream& isp, unsigned int width) const;

  bool match_literal (delimited_stream& isp, const textscan_format_elt& elem);

  int  skip_whitespace (delimited_stream& is, bool EOLstop = false);

  int  skip_delim (delimited_stream& is);

  bool is_delim (unsigned char ch) const
  {
    return ((delim_table.length () == 0
             && (isspace (ch) || ch == eol1 || ch == eol2))
            || delim_table[ch] != '\0');
  }

  bool isspace (unsigned int ch) const { return whitespace_table[ch & 0xff]; }

  // True if the only delimiter is whitespace.
  bool whitespace_delim (void) const { return delim_table.length () == 0; }

  // No copying!

  textscan (const textscan&);

  textscan& operator = (const textscan&);
};

#endif
