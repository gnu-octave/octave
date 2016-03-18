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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <deque>
#include <list>

#include "Cell.h"
#include "defun.h"
#include "oct-stream.h"
#include "ov.h"
#include "ovl.h"
#include "textscan.h"
#include "utils.h"

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

// Create a delimited stream, reading from is, with delimiters delims,
// and allowing reading of up to tellg + longest_lookeahead.  When is
// is at EOF, lookahead may be padded by ASCII nuls.

delimited_stream::delimited_stream (std::istream& is,
                                    const std::string& delimiters,
                                    int longest_lookahead,
                                    octave_idx_type bsize)
  : bufsize (bsize), i_stream (is), longest (longest_lookahead),
    delims (delimiters),
    flags (std::ios::failbit & ~std::ios::failbit) // can't cast 0
{
  buf = new char[bufsize];
  eob = buf + bufsize;
  idx = eob;                    // refresh_buf shouldn't try to copy old data
  progress_marker = idx;
  refresh_buf ();               // load the first batch of data
}

// Used to create a stream from a strstream from data read from a dstr.
// FIXME: Find a more efficient approach.  Perhaps derived dstr
delimited_stream::delimited_stream (std::istream& is,
                                    const delimited_stream& ds)
  : bufsize (ds.bufsize), i_stream (is), longest (ds.longest),
    delims (ds.delims),
    flags (std::ios::failbit & ~std::ios::failbit) // can't cast 0
{
  buf = new char[bufsize];
  eob = buf + bufsize;
  idx = eob;                    // refresh_buf shouldn't try to copy old data
  progress_marker = idx;
  refresh_buf ();               // load the first batch of data
}

delimited_stream::~delimited_stream (void)
{
  // Seek to the correct position in i_stream.
  if (!eof ())
    {
      i_stream.clear ();
      i_stream.seekg (buf_in_file);
      i_stream.read (buf, idx - buf);
    }

  delete [] buf;
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

  if (idx < eob)
    retval = *idx++;
  else
    {
      refresh_buf ();

      if (eof ())
        {
          setstate (std::ios_base::eofbit);
          retval = std::istream::traits_type::eof ();
        }
      else
        retval = *idx++;
    }

  if (idx >= last)
    delimited = false;

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
delimited_stream::refresh_buf (void)
{
  if (eof ())
    return std::istream::traits_type::eof ();

  int retval;
  int old_remaining = eob - idx;

  if (old_remaining < 0)
    {
      idx = eob;
      old_remaining = 0;
    }

  octave_quit ();                       // allow ctrl-C

  if (old_remaining > 0)
    memmove (buf, idx, old_remaining);

  progress_marker -= idx - buf;         // where original idx would have been
  idx = buf;

  int gcount;   // chars read
  if (! i_stream.eof ())
    {
      buf_in_file = i_stream.tellg ();   // record for destructor
      i_stream.read (buf + old_remaining, bufsize - old_remaining);
      gcount = i_stream.gcount ();
    }
  else
    gcount = 0;

  eob = buf + old_remaining + gcount;
  last = eob;
  if (gcount == 0)
    {
      delimited = false;

      if (eob != buf)              // no more data in file, but still some to go
        retval = 0;
      else
        // file and buffer are both done.
        retval = std::istream::traits_type::eof ();
    }
  else
    {
      delimited = true;

      for (last = eob - longest; last - buf >= 0; last--)
        {
          if (strchr (delims.c_str (), *last))
            break;
        }

      if (last - buf < 0)
        delimited = false;

      retval = 0;
    }

  // Ensure fast peek doesn't give valid char
  if (retval == std::istream::traits_type::eof ())
    *idx = '\0';      // FIXME - check that no TreatAsEmpty etc starts w. \0?

  return retval;
}

// Return a pointer to a block of data of size size, assuming that a
// sufficiently large buffer is available in buffer, if required.
// If called when delimited == true, and size is no greater than
// longest_lookahead then this will not call refresh_buf, so seekg
// still works.  Otherwise, seekg may be invalidated.

char *
delimited_stream::read (char *buffer, int size, char* &prior_tell)
{
  char *retval;

  if (eob  - idx > size)
    {
      retval = idx;
      idx += size;
      if (idx > last)
        delimited = false;
    }
  else
    {
      // If there was a tellg pointing to an earlier point than the current
      // read position, try to keep it in the active buffer.
      // In the current code, prior_tell==idx for each call,
      // so this is not necessary, just a precaution.

      if (eob - prior_tell + size < bufsize)
        {
          octave_idx_type gap = idx - prior_tell;
          idx = prior_tell;
          refresh_buf ();
          idx += gap;
        }
      else      // can't keep the tellg in range.  May skip some data.
        {
          refresh_buf ();
        }

      prior_tell = buf;

      if (eob - idx > size)
        {
          retval = idx;
          idx += size;
          if (idx > last)
            delimited = false;
        }
      else
        {
          if (size <= bufsize)          // small read, but reached EOF
            {
              retval = idx;
              memset (eob, 0, size + (idx - buf));
              idx += size;
            }
          else  // Reading more than the whole buf; return it in buffer
            {
              retval = buffer;
              // FIXME -- read bufsize at a time
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
  int len = out.length (), used = 0;
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

  size_t numel (void) const { return fmt_elts.size (); }

  const textscan_format_elt *first (void)
  {
    curr_idx = 0;
    return current ();
  }

  const textscan_format_elt *current (void) const
  {
    return numel () > 0 ? fmt_elts[curr_idx] : 0;
  }

  const textscan_format_elt *next (bool cycle = true)
  {
    curr_idx++;

    if (curr_idx >= numel ())
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
  size_t curr_idx;

  // List of format elements.
  std::deque<textscan_format_elt*> fmt_elts;

  // list holding column arrays of types specified by conversions
  std::list<octave_value > output_container;

  // Temporary buffer.
  std::ostringstream *buf;

  void add_elt_to_list (unsigned int width, int prec, int bitwidth,
                        octave_value val_type, bool discard,
                        char type,
                        const std::string& char_class = std::string ());

  void process_conversion (const std::string& s, size_t& i, size_t n);

  int finish_conversion (const std::string& s, size_t& i, size_t n,
                         unsigned int& width, int& prec, int& bitwidth,
                         octave_value& val_type,
                         bool discard, char& type);
  // No copying!

  textscan_format_list (const textscan_format_list&);

  textscan_format_list& operator = (const textscan_format_list&);
};


textscan_format_list::textscan_format_list (const std::string& s)
  : set_from_first (false), has_string (false), nconv (0), curr_idx (0),
    fmt_elts (), buf (0)
{
  size_t n = s.length ();

  size_t i = 0;

  unsigned int width = -1;              // Unspecified width = max (except %c)
  int prec = -1;
  int bitwidth = 0;
  bool discard = false;
  char type = '\0';

  bool have_more = true;

  if (s.length () == 0)
    {
      buf = new std::ostringstream ("%f");
      bitwidth = 64;
      type = 'f';
      add_elt_to_list (width, prec, bitwidth, octave_value (NDArray ()),
                       discard, type);
      have_more = false;
      set_from_first = true;
      nconv = 1;
    }
  else
    {
      set_from_first = false;

      while (i < n)
        {
          have_more = true;

          if (! buf)
            buf = new std::ostringstream ();

          if (s[i] == '%' && (i+1 == n || s[i+1] != '%'))
            {
              // Process percent-escape conversion type.

              process_conversion (s, i, n);

              have_more = (buf != 0);
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
                  *buf << s[i++];
                  width++;
                }

              add_elt_to_list (width, prec, bitwidth, octave_value (),
                               discard, type);

              have_more = false;
            }

          if (nconv < 0)
            {
              have_more = false;
              break;
            }
        }
    }

  if (have_more)
    add_elt_to_list (width, prec, bitwidth, octave_value (), discard, type);

  delete buf;
}

textscan_format_list::~textscan_format_list (void)
{
  size_t n = numel ();

  for (size_t i = 0; i < n; i++)
    {
      textscan_format_elt *elt = fmt_elts[i];
      delete elt;
    }
}

void
textscan_format_list::add_elt_to_list (unsigned int width, int prec,
                                       int bitwidth, octave_value val_type,
                                       bool discard, char type,
                                       const std::string& char_class)
{
  if (buf)
    {
      std::string text = buf->str ();

      if (! text.empty ())
        {
          textscan_format_elt *elt
            = new textscan_format_elt (text.c_str (), width, prec, bitwidth,
                                       discard, type, char_class);

          if (! discard)
            output_container.push_back (val_type);

          fmt_elts.push_back (elt);
        }

      delete buf;
      buf = 0;
    }
}

void
textscan_format_list::process_conversion (const std::string& s, size_t& i,
                                          size_t n)
{
  unsigned width = 0;
  int prec = -1;
  int bitwidth = 0;
  bool discard = false;
  octave_value val_type;
  char type = '\0';

  *buf << s[i++];

  bool have_width = false;

  while (i < n)
    {
      switch (s[i])
        {
        case '*':
          if (discard)
            nconv = -1;
          else
            {
              discard = true;
              *buf << s[i++];
            }
          break;

        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
          if (have_width)
            nconv = -1;
          else
            {
              char c = s[i++];
              width = width * 10 + c - '0';
              have_width = true;
              *buf << c;
              while (i < n && isdigit (s[i]))
                {
                  c = s[i++];
                  width = width * 10 + c - '0';
                  *buf << c;
                }

              if (i < n && s[i] == '.')
                {
                  *buf << s[i++];
                  prec = 0;
                  while (i < n && isdigit (s[i]))
                    {
                      c = s[i++];
                      prec = prec * 10 + c - '0';
                      *buf << c;
                    }
                }
            }
          break;

        case 'd': case 'u':
          {
            bool done = true;
            *buf << (type = s[i++]);
            if (i < n)
              {
                if (s[i] == '8')
                  {
                    bitwidth = 8;
                    if (type == 'd')
                      val_type = octave_value (int8NDArray ());
                    else
                      val_type = octave_value (uint8NDArray ());
                    *buf << s[i++];
                  }
                else if (s[i] == '1' && i+1 < n && s[i+1] == '6')
                  {
                    bitwidth = 16;
                    if (type == 'd')
                      val_type = octave_value (int16NDArray ());
                    else
                      val_type = octave_value (uint16NDArray ());
                    *buf << s[i++];
                    *buf << s[i++];
                  }
                else if (s[i] == '3' && i+1 < n && s[i+1] == '2')
                  {
                    done = false;       // use default size below
                    *buf << s[i++];
                    *buf << s[i++];
                  }
                else if (s[i] == '6' && i+1 < n && s[i+1] == '4')
                  {
                    bitwidth = 64;
                    if (type == 'd')
                      val_type = octave_value (int64NDArray ());
                    else
                      val_type = octave_value (uint64NDArray ());
                    *buf << s[i++];
                    *buf << s[i++];
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
          *buf << (type = s[i++]);
          bitwidth = 64;
          if (i < n)
            {
              if (s[i] == '3' && i+1 < n && s[i+1] == '2')
                {
                  bitwidth = 32;
                  val_type = octave_value (FloatNDArray ());
                  *buf << s[i++];
                  *buf << s[i++];
                }
              else if (s[i] == '6' && i+1 < n && s[i+1] == '4')
                {
                  val_type = octave_value (NDArray ());
                  *buf << s[i++];
                  *buf << s[i++];
                }
              else
                val_type = octave_value (NDArray ());
            }
          else
            val_type = octave_value (NDArray ());
          goto fini;

        case 'n':
          *buf << (type = s[i++]);
          bitwidth = 64;
          val_type = octave_value (NDArray ());
          goto fini;

        case 's': case 'q': case '[': case 'c':
          if (! discard)
            val_type = octave_value (Cell ());
          *buf << (type = s[i++]);
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
          error ("textscan: '%%%c' is not a valid format specifier", s[i]);
        }

      if (nconv < 0)
        break;
    }

  nconv = -1;
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

static std::string
textscan_char_class (const std::string& pattern)
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
            warning_with_id ("octave:textscan-pattern",
                             "textscan: [...] contains two '%c's", ch);

          if (prev == '-' && mask['-'] >= 2)
            warning_with_id ("octave:textscan-pattern",
                             "textscan: [...] contains two '-'s "
                             "outside range expressions");
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
textscan_format_list::finish_conversion (const std::string& s, size_t& i,
                                         size_t n, unsigned int& width,
                                         int& prec, int& bitwidth,
                                         octave_value& val_type, bool discard,
                                         char& type)
{
  int retval = 0;

  std::string char_class;

  size_t beg_idx = std::string::npos;
  size_t end_idx = std::string::npos;

  if (type != '%')
    {
      nconv++;
      if (type == '[')
        {
          if (i < n)
            {
              beg_idx = i;

              if (s[i] == '^')
                {
                  type = '^';
                  *buf << s[i++];

                  if (i < n)
                    {
                      beg_idx = i;

                      if (s[i] == ']')
                        *buf << s[i++];
                    }
                }
              else if (s[i] == ']')
                *buf << s[i++];
            }

          while (i < n && s[i] != ']')
            *buf << s[i++];

          if (i < n && s[i] == ']')
            {
              end_idx = i-1;
              *buf << s[i++];
            }

          if (s[i-1] != ']')
            retval = nconv = -1;
        }
    }

  if (nconv >= 0)
    {
      if (beg_idx != std::string::npos && end_idx != std::string::npos)
        char_class = textscan_char_class (s.substr (beg_idx,
                                                    end_idx - beg_idx + 1));

      add_elt_to_list (width, prec, bitwidth, val_type, discard, type,
                       char_class);
    }

  return retval;
}

void
textscan_format_list::printme (void) const
{
  size_t n = numel ();

  for (size_t i = 0; i < n; i++)
    {
      textscan_format_elt *elt = fmt_elts[i];

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
        << "char_class: `" << undo_string_escapes (elt->char_class) << "'\n"
        << "text:       `" << undo_string_escapes (elt->text) << "'\n\n";
    }
}

// If FORMAT is explicitly "", it is assumed to be "%f" repeated enough
// times to read the first row of the file.  Set it now.

int
textscan_format_list::read_first_row (delimited_stream& is, textscan& ts)
{
  // Read first line and strip end-of-line, which may be two characters
  std::string first_line (20, ' ');

  is.getline (first_line, static_cast<char> (ts.eol2));

  if (first_line.length () > 0
      && first_line[first_line.length () - 1] == ts.eol1)
    first_line.resize (first_line.length () - 1);

  std::istringstream strstr (first_line);
  delimited_stream ds (strstr, is);

  dim_vector dv (1,1);      // initial size of each output_container
  Complex val;
  octave_value val_type;
  nconv = 0;
  int max_empty = 1000;     // failsafe, if ds fails but not with eof
  int retval = 0;

  // read line, creating output_container as we go
  while (! ds.eof ())
    {
      bool already_skipped_delim = false;
      ts.skip_whitespace (ds);
      ds.progress_benchmark ();
      bool progress = false;
      ts.scan_complex (ds, *fmt_elts[0], val);
      if (ds.fail ())
        {
          ds.clear (ds.rdstate () & ~std::ios::failbit);

          if (ds.eof ())
            break;

          // If we don't continue after a conversion error, then
          // unless this was a missing value (i.e., followed by a delimiter),
          // return with an error status.
          if (ts.return_on_error < 2)
            {
              ts.skip_delim (ds);
              if (ds.no_progress ())
                {
                  retval = 4;
                  break;
                }
              already_skipped_delim = true;
            }
          else  // skip offending field
            {
              std::ios::iostate state = ds.rdstate ();
              ds.clear ();          // clear to allow read pointer to advance

              std::string dummy;
              textscan_format_elt fe ("", first_line.length ());
              ts.scan_string (ds, fe, dummy);

              progress = (dummy.length ());
              ds.setstate (state);
            }

          val = ts.empty_value.scalar_value ();

          if (! --max_empty)
            break;
        }

      if (val.imag () == 0)
        val_type = octave_value (NDArray (dv, val.real ()));
      else
        val_type = octave_value (ComplexNDArray (dv, val));

      output_container.push_back (val_type);

      if (! already_skipped_delim)
        ts.skip_delim (ds);

      if (! progress && ds.no_progress ())
        break;

      nconv++;
    }

  output_container.pop_front (); // discard empty element from constructor

  // Create fmt_list now that the size is known
  for (octave_idx_type i = 1; i < nconv; i++)
    fmt_elts.push_back (new textscan_format_elt (*fmt_elts[0]));

  return retval;             // May have returned 4 above.
}

// Perform actual textscan: read data from stream, and create cell array.

static Cell
init_inf_nan (void)
{
  Cell retval (dim_vector (1, 2));

  retval(0) = Cell (octave_value ("inf"));
  retval(1) = Cell (octave_value ("nan"));

  return retval;
}

textscan::textscan (void)
  : buf (), whitespace_table (), delim_table (), delims (),
    comment_style (), comment_len (0), comment_char (-2),
    buffer_size (0), date_locale (), inf_nan (init_inf_nan ()),
    empty_value (octave_NaN), exp_chars ("edED"),
    header_lines (0), treat_as_empty (), treat_as_empty_len (0),
    whitespace (" \b\t"), eol1 ('\r'), eol2 ('\n'),
    return_on_error (2), collect_output (false),
    multiple_delims_as_one (false), default_exp (true),
    numeric_delim (false), lines (0)
{ }

octave_value
textscan::scan (std::istream& isp, const octave_value_list& args)
{
  std::string format;
  int params = 0;

  if (args.length () == 0)
    format = "%f";      // ommited format = %f.  explicit "" = width from file
  else if (args(0).is_string ())
    {
      format = args(0).string_value ();

      if (args(0).is_sq_string ())
        format = do_string_escapes (format);

      params++;
    }
  else
    error ("textscan: FORMAT must be a string, not <%s>",
           args(0).class_name ().c_str ());

  octave_idx_type ntimes = -1;

  if (args.length () > 1)
    {
      if (args(1).is_numeric_type ())
        {
          ntimes = args(1).idx_type_value ();

          if (ntimes < args(1).double_value ())
            error ("textscan: REPEAT = %g is too large",
                   args(1).double_value ());

          params++;
        }
    }

  octave_value_list tmp_args = args.splice (0, params);

  textscan_format_list fmt_list (format);

  parse_options (tmp_args, fmt_list);

  return do_scan (isp, fmt_list, ntimes);
}

octave_value
textscan::do_scan (std::istream& isp, textscan_format_list& fmt_list,
                   octave_idx_type ntimes)
{
  octave_value retval;

  if (fmt_list.num_conversions () == -1)
    error ("textscan: invalid format specified");

  if (fmt_list.num_conversions () == 0)
    error ("textscan: no valid format conversion specifiers\n");

  // skip the first  header_lines
  std::string dummy;
  for (int i = 0; i < header_lines && isp; i++)
    getline (isp, dummy, static_cast<char> (eol2));

  // Create our own buffered stream, for fast get/putback/tell/seek.

  // First, see how far ahead it should let us look.
  int max_lookahead = std::max (std::max (comment_len, treat_as_empty_len),
                                std::max (delim_len, 3)); // 3 for NaN and Inf

  // Next, choose a buffer size to avoid reading too much, or too often.
  octave_idx_type buf_size = 4096;
  if (buffer_size)
    buf_size = buffer_size;
  else if (ntimes > 0)
    {
      // Avoid overflow of 80*ntimes...
      buf_size = std::min (buf_size, std::max (ntimes, 80 * ntimes));
      buf_size = std::max (buf_size, ntimes);
    }
  // Finally, create the stream.
  delimited_stream is (isp, whitespace + delims, max_lookahead, buf_size);

  // Grow retval dynamically.  "size" is half the initial size
  // (FIXME -- Should we start smaller if ntimes is large?)
  octave_idx_type size = ((ntimes < 8 && ntimes >= 0) ? ntimes : 1);
  Array<octave_idx_type> row_idx (dim_vector (1,2));
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
      lines = 1;

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
  if (collect_output)
    {
      int prev_type = -1;
      for (std::list<octave_value>::iterator col = out.begin ();
           col != out.end (); col++)
        {
          if (col->type_id () == prev_type
              || (fmt_list.set_from_first && prev_type != -1))
            merge_with_prev [conv++] = true;
          else
            merge_with_prev [conv++] = false;

          prev_type = col->type_id ();
        }
    }

  // This should be caught by earlier code, but this avoids a possible
  // infinite loop below.
  if (fmt_list.num_conversions () == 0)
    error ("textscan: No conversions specified");

  // Read the data.  This is the main loop.
  if (! err)
    {
      for (/* row set ~30 lines above */; row < ntimes || ntimes == -1; row++)
        {
          if (row == 0 || row >= size)
            {
              size += size+1;
              for (std::list<octave_value>::iterator col = out.begin ();
                   col != out.end (); col++)
                *col = (*col).resize (dim_vector (size, 1), 0);
            }

          row_idx(0) = row;
          err = read_format_once (is, fmt_list, out, row_idx, done_after);

          if (err > 0 || ! is || (lines >= ntimes && ntimes > -1))
            break;
        }
    }

  if ((err & 4) && ! return_on_error)
    error ("textscan: Read error in field %d of row %d",
           done_after + 1, row + 1);

  // If file does not end in EOL, do not pad columns with NaN.
  bool uneven_columns = false;
  if (isp.eof () || (err & 4))
    {
      isp.clear ();
      isp.seekg (-1, std::ios_base::end);
      int last_char = isp.get ();
      isp.setstate (isp.eofbit);
      uneven_columns = (last_char != eol1 && last_char != eol2);
    }

  // convert return value to Cell array
  Array<octave_idx_type> ra_idx (dim_vector (1,2));

  // (err & 1) means "error, and no columns read this row
  // FIXME -- This may redundant now that done_after=0 says the same
  if (err & 1)
    done_after = out.size () + 1;

  int valid_rows = (row == ntimes) ? ntimes : ((err & 1) ? row : row+1);
  dim_vector dv (valid_rows, 1);

  ra_idx(0) = 0;
  int i = 0;
  if (! collect_output)
    {
      retval = Cell (dim_vector (1, out.size ()));
      for (std::list<octave_value>::iterator col = out.begin ();
           col != out.end (); col++, i++)
        {
          // trim last columns if that was requested
          if (i == done_after && uneven_columns)
            dv = dim_vector (std::max (valid_rows - 1, 0), 1);

          ra_idx(1) = i;
          retval = do_cat_op (retval, octave_value (Cell (col->resize (dv,0))),
                              ra_idx);
        }
    }
  else  // group adjacent cells of the same type into a single cell
    {
      octave_value    cur;                // current cell, accumulating columns
      octave_idx_type group_size = 0;     // columns in this cell
      int prev_type = -1;

      conv = 0;
      retval = Cell ();
      for (std::list<octave_value>::iterator col = out.begin ();
           col != out.end (); col++)
        {
          if (! merge_with_prev [conv++])  // including first time
            {
              if (prev_type != -1)
                {
                  ra_idx(1) = i++;
                  retval = do_cat_op (retval, octave_value (Cell(cur)),
                                      ra_idx);
                }
              cur = octave_value (col->resize (dv,0));
              group_size = 1;
              prev_type = col->type_id ();
            }
          else
            {
              ra_idx(1) = group_size++;
              cur = do_cat_op (cur, octave_value (col->resize (dv,0)),
                               ra_idx);
            }
        }
      ra_idx(1) = i;
      retval = do_cat_op (retval, octave_value (Cell (cur)), ra_idx);
    }

  return retval;
}

// Calculate x^n.  Used for ...e+nn  so that, for example, 1e2 is
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

// Read a double considering the "precision" field of  fmt  and the
// exp_chars  option of  options.

double
textscan::read_double (delimited_stream& is,
                       const textscan_format_elt& fmt) const
{
  int sign = 1;
  unsigned int width_left = fmt.width;
  double retval = 0;
  bool valid = false;         // syntactically correct double?

  int ch = is.peek ();

  if (ch == '+')
    {
      is.get ();
      ch = is.peek ();
      if (width_left)
        width_left--;
    }
  else if (ch == '-')
    {
      sign = -1;
      is.get ();
      ch = is.peek ();
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

      if (width_left)
        width_left--;                  // Consider width of '.'

      if (precision == -1)
        precision = 1<<30;           // FIXME Should be MAXINT

      if (! valid)                    // if there was nothing before '.'...
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

      // skip remainder after '.', to field width, to look for exponent
      if (i == precision)
        while (width_left-- && is && (ch = is.get ()) >= '0' && ch <= '9')
          ;  // discard

      width_left++;
    }

  // look for exponent part in, e.g.,  6.023E+23
  const char *ec = exp_chars.c_str ();
  bool used_exp = false;
  if (valid && width_left > 1 && strchr (ec, ch))
    {
      int ch1 = is.peek ();
      if (ch1 == '-' || ch1 == '+' || (ch1 >= '0' && ch1 <= '9'))
        {          // if 1.0e+$ or some such, this will set failbit, as we want
          width_left--;                         // count "E"
          int exp = 0;
          int exp_sign = 1;
          if (ch1 == '+')
            {
              if (width_left)
                width_left--;
              is.get ();
            }
          else if (ch1 == '-')
            {
              exp_sign = -1;
              is.get ();
              if (width_left)
                width_left--;
            }
          valid = false;
          while (width_left-- && is && (ch = is.get ()) >= '0' && ch1 <= '9')
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
  if (! valid && width_left >= 3)
    {
      int i = lookahead (is, inf_nan, 3, false);   // false -> case insensitive
      if (i == 0)
        {
          retval = octave_Inf;
          valid = true;
        }
      else if (i == 1)
        {
          retval = octave_NaN;
          valid = true;
        }
    }

  // Check for +/- inf and NaN
  if (! valid && width_left >= 3)
    {
      int i = lookahead (is, inf_nan, 3, false);   // false -> case insensitive
      if (i == 0)
        {
          retval = octave_Inf;
          valid = true;
        }
      else if (i == 1)
        {
          retval = octave_NaN;
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
  bool as_empty = false;   // did we fail but match a "treat_as_empty" string?
  bool inf = false;

  int ch = is.peek ();
  if (ch == '+' || ch == '-')   // check for [+-][ij] with no coefficients
    {
      ch = is.get ();
      int ch2 = is.peek ();
      if (ch2 == 'i' || ch2 == 'j')
        {
          double value = 1;
          is.get ();
          // Check not -inf
          if (is.peek () == 'n')
            {
              char *pos = is.tellg ();
              std::ios::iostate state = is.rdstate ();

              is.get ();
              ch2 = is.get ();
              if (ch2 == 'f')
                {
                  inf = true;
                  re = (ch == '+') ? octave_Inf : -octave_Inf;
                  value = 0;
                }
              else
                {
                  is.clear (state);
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
      //re = octave_read_value<double> (is);
      re = read_double (is, fmt);

      // check for "treat as empty" string
      if (treat_as_empty.numel ()
          && (is.fail () || octave_is_NaN_or_NA (Complex (re))
              || re == octave_Inf))
        {

          for (int i = 0; i < treat_as_empty.numel (); i++)
            {
              if (ch == treat_as_empty (i).string_value ()[0])
                {
                  as_empty = true;   // first char matches, so read the lot
                  break;
                }
            }
          if (as_empty)              // if first char matched...
            {
              as_empty = false;      // ...look for the whole string

              is.clear (state);      // treat_as_empty "-" causes partial read
              is.seekg (pos);        // reset to position before failed read

              // treat_as_empty strings may be different sizes.
              // Read ahead longest, put it all back, then re-read the string
              // that matches.
              char *look, look_buf [treat_as_empty_len + 1];
              // prefill, in case EOF means part-filled.
              memset (look_buf, '\0', treat_as_empty_len);
              look = is.read (look_buf, treat_as_empty_len, pos);

              is.clear (state);
              is.seekg (pos);        // reset to position before look-ahead
                                     // FIXME -- is.read could invalidate pos

              for (int i = 0; i < treat_as_empty.numel (); i++)
                {
                  std::string s = treat_as_empty (i).string_value ();
                  if (! strncmp (s.c_str (), look, s.size ()))
                    {
                      as_empty = true;
                                     // read just the right amount
                      is.read (look_buf, s.size (), pos);
                      break;
                    }
                }
            }
        }

      if (! is.eof () && ! as_empty)
        {
          state = is.rdstate ();        // before tellg, since that fails at EOF
          pos = is.tellg ();
          ch = is.peek ();   // ch == EOF if read failed; no need to chk fail
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

              //im = octave_read_value<double> (is);
              im = read_double (is, fmt);
              if (is.fail ())
                im = 1;

              if (is.peek () == 'i' || is.peek () == 'j')
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
    val = empty_value.scalar_value ();
  else
    val = Complex (re, im);
}

// Return in VAL the run of characters from IS NOT contained in PATTERN.

int
textscan::scan_caret (delimited_stream& is, const char *pattern,
                      std::string& val) const
{
  int c1 = std::istream::traits_type::eof ();
  std::ostringstream obuf;              // Is this optimised for growing?

  while (is && ((c1 = (is && ! is.eof ())
                 ? is.get_undelim ()
                 : std::istream::traits_type::eof ())
                != std::istream::traits_type::eof ())
         && ! strchr (pattern, c1))
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
    {                               // find sequence ending with an ending char
      std::string next;
      scan_caret (is, ends.c_str (), next);
      retval = retval + next;   // FIXME -- could use repeated doubling of size

      int last = (! is.eof ()
                  ? is.get_undelim () : std::istream::traits_type::eof ());

      if (last != std::istream::traits_type::eof ())
        {
          retval = retval + static_cast<char> (last);
          for (int i = 0; i < delimiters.numel (); i++)
            {
              std::string delim = delimiters(i).string_value ();
              int start = retval.length () - delim.length ();
              if (start < 0)
                start = 0;
              std::string may_match = retval.substr (start);
              if (may_match == delim)
                {
                  done = true;
                  retval = retval.substr (0, start);
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
  if (delim_list.numel () == 0)
    {
      unsigned int i = 0;
      unsigned int width = fmt.width;

      for (i = 0; i < width; i++)
        {
          if (i+1 > val.length ())
            val = val + val + ' ';      // grow even if empty
          int ch = is.get ();
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
      std::string ends ("");
      for (int i = 0; i < delim_list.numel (); i++)
        {
          std::string tmp = delim_list(i).string_value ();
          ends += tmp.substr (tmp.length () - 1);
        }
      val = textscan::read_until (is, delim_list, ends);
    }
}

// Return in VAL the run of characters from IS contained in PATTERN.

int
textscan::scan_bracket (delimited_stream& is, const char *pattern,
                        std::string& val) const
{
  int c1 = std::istream::traits_type::eof ();
  std::ostringstream obuf;              // Is this optimised for growing?

  while (is && strchr (pattern, (c1 = is.get_undelim ())))
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

  if (is.peek () != '\"')
    scan_string (is, fmt, val);
  else
    {
      is.get ();
      scan_caret (is, "\"", val);       // read everything until "
      is.get ();                        // swallow "

      while (is && is.peek () == '\"')  // if double ", insert one in stream,
        {                               // and keep looking for single "
          is.get ();
          std::string val1;
          scan_caret (is, "\"", val1);
          val = val + "\"" + val1;
          is.get_undelim ();
        }
    }
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
                  if (ov.is_real_type () && v.imag () == 0)
                    ov.internal_rep ()->fast_elem_insert (row(0), v.real ());
                  else
                    {
                      if (ov.is_real_type ())  // cat does type conversion
                        ov = do_cat_op (ov, octave_value (v), row);
                      else
                        ov.internal_rep ()->fast_elem_insert (row(0), v);
                    }
                }
              else
                {
                  if (ov.is_real_type () && v.imag () == 0)
                    ov.internal_rep ()->fast_elem_insert (row(0),
                                                         float (v.real ()));
                  else
                    {
                      if (ov.is_real_type ())  // cat does type conversion
                        ov = do_cat_op (ov, octave_value (v), row);
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

      if (is.fail ())
        {
          if (! fmt.discard)
            ov = do_cat_op (ov, empty_value, row);

          // If we are continuing after errors, skip over this field
          if (return_on_error == 2)
            {
              std::ios::iostate state = is.rdstate ();
              is.clear ();          // clear to allow read pointer to advance

              std::string dummy;
              scan_string (is, fmt, dummy);

              is.setstate (state);
            }
        }

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

      // FIXME -- why does failbit get set at EOF, instead of eofbit?
      if (vv.length () != 0)
        is.clear (is.rdstate () & ~std::ios_base::failbit);
    }

  is.field_done ();
}

// Read data corresponding to the entire format string once, placing the
// values in row ROW of retval.

int
textscan::read_format_once (delimited_stream& is,
                            textscan_format_list& fmt_list,
                            std::list<octave_value> & retval,
                            Array<octave_idx_type> row, int& done_after)
{
  const textscan_format_elt *elem = fmt_list.first ();
  std::list<octave_value>::iterator out = retval.begin ();
  bool no_conversions = true;
  bool done = false;
  bool conversion_failed = false;       // Record for ReturnOnError

  octave_quit ();

  for (size_t i = 0; i < fmt_list.numel (); i++)
    {
      bool this_conversion_failed = false;

      // Clear fail of previous numeric conversions.
      is.clear ();

      switch (elem->type)
        {
        case 'C':
        case 'D':
          std::cerr << "textscan: Conversion %" << elem->type
                    << " not yet implemented\n";
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
          if (return_on_error < 2)
            this_conversion_failed = true;

          is.clear (is.rdstate () & ~std::ios::failbit);
        }

      if (! elem->discard)
        out++;

      elem = fmt_list.next ();
      char *pos = is.tellg ();

      // FIXME -- these conversions "ignore delimiters".  Should they include
      // delimiters at the start of the conversion, or can those be skipped?
      if (elem->type != textscan_format_elt::literal_conversion
          // && elem->type != '[' && elem->type != '^' && elem->type != 'c'
         )
        skip_delim (is);

      if (this_conversion_failed)
        {
          if (is.tellg () == pos && ! conversion_failed)
            {                 // done_after = first failure
              done_after = i; // note fail, but parse others to get empty_val
              conversion_failed = true;
            }
          else
            this_conversion_failed = false;
        }

      if (is.eof ())
        {
          if (! done)
            done_after = i+1;

          // note EOF, but process others to get empty_val.
          done = true;
        }
    }

  if (done)
    is.setstate (std::ios::eofbit);

  // Returning -3 means "error, and no columns read this row".
  if (is.eof ())
    return (2 + no_conversions);

  if (conversion_failed)
    return (4 + no_conversions);

  return 0;
}

void
textscan::parse_options (const octave_value_list& args,
                         textscan_format_list& fmt_list)
{
  int last = args.length ();
  int n = last;

  if (n & 1)
    error ("textscan: %d parameters given, but only %d values", n-n/2, n/2);

  delim_len = 1;
  bool have_delims = false;
  for (int i = 0; i < last; i += 2)
    {
      if (! args(i).is_string ())
        error ("textscan: Invalid paramter type <%s> for parameter %d",
               args(i).type_name ().c_str (), i/2 + 1);

      std::string param = args(i).string_value ();
      std::transform (param.begin (), param.end (),
                      param.begin (), ::tolower);
      if (param == "delimiter")
        {
          bool invalid = true;
          if (args(i+1).is_string ())
            {
              invalid = false;
              have_delims = true;
              delims = args(i+1).string_value ();
            }
          else if (args(i+1).is_cell ())
            {
              invalid = false;
              delim_list = args(i+1).cell_value ();
              delim_table = " "; // non-empty, to flag non-default delim

              // Check that all elements are strings, and find max length
              for (int j = 0; j < delim_list.numel (); j++)
                {
                  if (! delim_list(j).is_string ())
                    invalid = true;
                  else
                    {
                      octave_idx_type len = delim_list(j).string_value ()
                                                         .length ();
                      delim_len = std::max (static_cast<int>(len), delim_len);
                    }
                }
            }
          if (invalid)
            error ("textscan:  Delimiters must be either a string or "
                   "cell array of strings");
        }
      else if (param == "commentstyle")
        {
          if (args(i+1).is_string ())
            {   // check here for names like "C++", "C", "shell", ...?
              comment_style = Cell (args(i+1));
            }
          else if (args(i+1).is_cell ())
            {
              comment_style = args(i+1).cell_value ();
              int len = comment_style.numel ();
              if ((len >= 1 && ! comment_style (0).is_string ())
                  || (len >= 2 && ! comment_style (1).is_string ())
                  || (len >= 3))
                error ("textscan: CommentStyle must be either a string or "
                       "cell array of one or two strings");
            }
          else
            error ("textscan:  CommentStyle must be either a string"
                   " or cell array of one or two strings, not <%s>",
                   args(i+1).class_name ().c_str ());

          // How far ahead do we need to look to detect an open comment
          // and which character do we look for?
          if (comment_style.numel () >= 1)
            {
              comment_len  = comment_style (0).string_value ().size ();
              comment_char = comment_style (0).string_value ()[0];
            }
        }
      else if (param == "treatasempty")
        {
          bool invalid = false;
          if (args(i+1).is_string ())
            {
              treat_as_empty = Cell (args(i+1));
              treat_as_empty_len = args(i+1).string_value ().size ();
            }
          else if (args(i+1).is_cell ())
            {
              treat_as_empty = args(i+1).cell_value ();
              for (int j = 0; j < treat_as_empty.numel (); j++)
                if (! treat_as_empty (j).is_string ())
                  invalid = true;
                else
                  {
                    int k = treat_as_empty (j).string_value ().size ();
                    if (k > treat_as_empty_len)
                      treat_as_empty_len = k;
                  }
            }
          if (invalid)
            error ("textscan:  TreatAsEmpty must be either a string or "
                   "cell array of one or two strings");

          // FIXME Ensure none is a prefix of a later one. Sort by length?
        }
      else if (param == "collectoutput")
        {
          if (args(i+1).is_numeric_type ())
            collect_output = args(i+1).bool_value ();
          else
            error ("textscan:  CollectOutput must be logical or numeric");
        }
      else if (param == "emptyvalue")
        {
          if (args(i+1).is_numeric_type ())
            empty_value = args(i+1).scalar_value ();
          else
            error ("textscan: EmptyValue must be numeric, not <%s>",
                   args(i+1).class_name ().c_str ());
        }
      else if (param == "headerlines")
        {
          if (args(i+1).is_numeric_type ())
            header_lines = args(i+1).scalar_value ();
          else
            error ("textscan: HeaderLines must be numeric");
        }
      else if (param == "bufsize")
        {
          if (args(i+1).is_numeric_type ())
            buffer_size = args(i+1).scalar_value ();
          else
            error ("textscan: BufSize must be numeric");
        }
      else if (param == "multipledelimsasone")
        {
          if (args(i+1).is_numeric_type ())
            {
              if (args(i+1).bool_value ())
                multiple_delims_as_one = true;
            }
          else
            error ("textscan: MultipleDimsAsOne must be logical or numeric");
        }
      else if (param == "returnonerror")
        {
          if (args(i+1).is_numeric_type ())
            return_on_error = args(i+1).bool_value ();
          else if (args(i+1).is_string ()
                   && args(i+1).string_value () == "continue")
            return_on_error = 2;
          else
            error ("textscan: ReturnOnError must be logical or "
                   "numeric, or 'continue'");
        }
      else if (param == "whitespace")
        {
          if (args(i+1).is_string ())
            whitespace = args(i+1).string_value ();
          else
            error ("textscan: Whitespace must be a character string");
        }
      else if (param == "expchars")
        {
          if (args(i+1).is_string ())
            {
              exp_chars = args(i+1).string_value ();
              default_exp = false;
            }
          else
            error ("textscan: ExpChars must be a character string");
        }
      else if (param == "endofline")
        {
          bool valid = true;
          if (args(i+1).is_string ())
            {
              std::string s = args(i+1).string_value ();
              if (args(i+1).is_sq_string ())
                s = do_string_escapes (s);
              int l = s.length ();
              if (l == 0)
                eol1 = eol2 = -2;
              else if (l == 1)
                eol1 = eol2 = s.c_str ()[0];
              else if (l == 2)
                {
                  eol1 = s.c_str ()[0];
                  eol2 = s.c_str ()[1];
                  if (eol1 != '\r' || eol2 != '\n')    // Why limit it?
                    valid = false;
                }
              else
                valid = false;
            }
          else
            valid = false;
          if (! valid)
            error ("textscan: EndOfLine must be at most one character "
                   "or '\\r\\n'");
        }
      else
        error ("textscan: Unrecognised option '%s'", param.c_str ());
    }

  whitespace_table = std::string (256, '\0');
  for (unsigned int i = 0; i < whitespace.length (); i++)
    whitespace_table[whitespace[i]] = '1';

  // For Matlab compatibility, add 0x20 to whitespace, unless
  // whitespace is explicitly ignored.
  if (! (whitespace.length () == 0 && fmt_list.has_string))
    whitespace_table[' '] = '1';

  // Create look-up table of delimiters, based on 'delimiter'
  delim_table = std::string (256, '\0');
  if (eol1 >= 0 && eol1 < 256)
    delim_table[eol1] = '1';        // EOL is always a delimiter
  if (eol2 >= 0 && eol2 < 256)
    delim_table[eol2] = '1';        // EOL is always a delimiter
  if (! have_delims)
    for (unsigned int i = 0; i < 256; i++)
      {
        if (isspace (i))
          delim_table[i] = '1';
      }
  else
    for (unsigned int i = 0; i < delims.length (); i++)
      delim_table[delims[i]] = '1';
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
      while (is && (c1 = is.get_undelim ()) != std::istream::traits_type::eof ()
             && ( ( (c1 == eol1 || c1 == eol2) && ++lines && ! EOLstop)
                  || isspace (c1)))
        {
          if (prev == eol1 && eol1 != eol2 && c1 == eol2)
            lines--;
          prev = c1;
        }

      if (c1 == comment_char)           // see if we match an open comment
        {
          // save stream state in case we have to restore it
          char *pos   = is.tellg ();
          std::ios::iostate state = is.rdstate ();

          char *look, tmp [comment_len];
          look = is.read (tmp, comment_len-1, pos);   // already read first char
          if (is && ! strncmp (comment_style(0).string_value ().substr (1)
                               .c_str (), look, comment_len-1))
            {
              found_comment = true;

              std::string dummy;
              char eol [3] = {static_cast<char> (eol1),
                              static_cast<char> (eol2),
                              '\0'};
              if (comment_style.numel () == 1)  // skip to end of line
                {
                  scan_caret (is, eol, dummy);
                  c1 = is.get_undelim ();
                  if (c1 == eol1 && eol1 != eol2 && is.peek_undelim () == eol2)
                    is.get_undelim ();
                  lines++;
                }
              else      // matching pair
                {
                  std::string end_c = comment_style(1).string_value ();
                        // last char of end-comment sequence
                  char last[2] = {*(end_c.substr (end_c.length ()-1).c_str ()),
                                  '\0'};
                  std::string may_match ("");
                  do
                    {           // find sequence ending with last char
                      scan_caret (is, last, dummy);
                      is.get_undelim ();        // (read   last  itself)

                      may_match = may_match + dummy + *last;
                      int start = may_match.length () - end_c.length ();
                      if (start < 0)
                        start = 0;
                      may_match = may_match.substr (start);
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

  char *look, tmp [max_len + 1];

  memset (tmp, '\0', max_len); // prefill, in case EOF means part-filled.
  look = is.read (tmp, max_len, pos);

  is.clear ();
  is.seekg (pos);              // reset to position before look-ahead
                               // FIXME  pos may be corrupted by is.read

  int i;
  int (*compare)(const char *, const char *, size_t);
  compare = case_sensitive ? strncmp : strncasecmp;

  for (i = 0; i < targets.numel (); i++)
    {
      std::string s = targets (i).string_value ();
      if (! (*compare) (s.c_str (), look, s.size ()))
        {
          is.read (tmp, s.size (), pos); // read just the right amount
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
  int c1 = skip_whitespace (is, true);  // 'true': stop once EOL is read
  if (delim_list.numel () == 0)         // single character delimiter
    {
      if (is_delim (c1) || c1 == eol1 || c1 == eol2)
        {
          is.get ();
          if (c1 == eol1 && is.peek_undelim () == eol2)
            is.get_undelim ();          // if \r\n, skip the \n too.

          if (multiple_delims_as_one)
            {
              int prev = -1;
              // skip multiple delims.
              // Increment lines for each end-of-line seen; for \r\n, decrement
              while (is && ((c1 = is.get_undelim ())
                            != std::istream::traits_type::eof ())
                     && (((c1 == eol1 || c1 == eol2) && ++lines)
                         || isspace (c1) || is_delim (c1)))
                {
                  if (prev == eol1 && eol1 != eol2 && c1 == eol2)
                    lines--;
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

      if (c1 == eol1 || c1 == eol2
          || (-1 != (first_match = lookahead (is, delim_list, delim_len))))
        {
          if (c1 == eol1)
            {
              is.get_undelim ();
              if (is.peek_undelim () == eol2)
                is.get_undelim ();
            }
          else if (c1 == eol2)
            {
              is.get_undelim ();
            }

          if (multiple_delims_as_one)
            {
              int prev = -1;
              // skip multiple delims.
              // Increment lines for each end-of-line seen; for \r\n, decrement
              while (is && ((c1 = skip_whitespace (is, true))
                            != std::istream::traits_type::eof ())
                     && (((c1 == eol1 || c1 == eol2) && ++lines)
                         || -1 != lookahead (is, delim_list, delim_len)))
                {
                  if (prev == eol1 && eol1 != eol2 && c1 == eol2)
                    lines--;
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
textscan::match_literal (delimited_stream& is, const textscan_format_elt& fmt)
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

DEFUN (textscan, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {} {@var{C} =} textscan (@var{fid}, @var{format})\n\
@deftypefnx {} {@var{C} =} textscan (@var{fid}, @var{format}, @var{repeat})\n\
@deftypefnx {} {@var{C} =} textscan (@var{fid}, @var{format}, @var{param}, @var{value}, @dots{})\n\
@deftypefnx {} {@var{C} =} textscan (@var{fid}, @var{format}, @var{repeat}, @var{param}, @var{value}, @dots{})\n\
@deftypefnx {} {@var{C} =} textscan (@var{str}, @dots{})\n\
@deftypefnx {} {[@var{C}, @var{position}] =} textscan (@dots{})\n\
Read data from a text file or string.\n\
\n\
The string @var{str} or file associated with @var{fid} is read from and\n\
parsed according to @var{format}.\n\
The function is an extension of @code{strread} and @code{textread}.\n\
Differences include: the ability to read from either a file or a string,\n\
additional options, and additional format specifiers.\n\
\n\
The input is interpreted as a sequence of \"words\", delimiters\n\
(such as whitespace) and literals.\n\
The characters that form delimiters and whitespace are determined\n\
by the options.\n\
The format consists of format specifiers interspersed between literals.\n\
In the format, whitespace forms a delimiter between consecutive literals,\n\
but is otherwise ignored.\n\
\n\
The output @var{C} is a cell array whose second dimension is determined\n\
by the number of format specifiers.\n\
\n\
The first word of the input is matched to the first specifier of the\n\
format and placed in the first column of the output;\n\
the second is matched to the second specifier and placed in the second column\n\
and so forth.\n\
If there are more words than specifiers, the process is repeated until all\n\
words have been processed or the limit imposed by @var{repeat} has been met\n\
(see below).\n\
\n\
The string @var{format} describes how the words in @var{str} should be\n\
parsed.\n\
As in @var{fscanf}, any (non-whitespace) text in the format that is\n\
not one of these specifiers is considered a literal;\n\
if there is a literal between two format specifiers then that same literal\n\
must appear in the input stream between the matching words.\n\
\n\
The following specifiers are valid:\n\
\n\
@table @code\n\
@item  %f\n\
@itemx %f64\n\
@itemx %n\n\
The word is parsed as a number and converted to double.\n\
\n\
@item  %f32\n\
The word is parsed as a number and converted to single (float).\n\
\n\
@item  %d\n\
@itemx %d8\n\
@itemx %d16\n\
@itemx %d32\n\
@itemx %d64\n\
The word is parsed as a number and converted to int8, int16, int32 or int64.\n\
If not size is specified, int32 is used.\n\
\n\
@item  %u\n\
@itemx %u8\n\
@itemx %u16\n\
@itemx %u32\n\
@itemx %u64\n\
The word is parsed as a number and converted to uint8, uint16, uint32 or\n\
uint64. If not size is specified, uint32 is used.\n\
\n\
@item %s\n\
The word is parsed as a string, ending at the last character before\n\
whitespace, an end-of-line or a delimiter specified in the options.\n\
\n\
@item %q\n\
The word is parsed as a \"quoted string\".\n\
If the first character of the string is a double quote (\") then the string\n\
includes everything until a matching double quote, including whitespace,\n\
delimiters and end of line characters.\n\
If a pair of consecutive double quotes appears in the input,\n\
it is replaced in the output by a single double quote.\n\
That is, the input \"He said \"\"Hello\"\"\" would return the value\n\
'He said \"Hello\"'.\n\
\n\
@item  %c\n\
The next character of the input is read.\n\
This includes delimiters, whitespace and end of line characters.\n\
\n\
@item  %[...]\n\
@itemx %[^...]\n\
In the first form, the word consists of the longest run consisting of only\n\
characters between the brackets.\n\
Ranges of characters can be specified by a hyphen;\n\
for example, %[0-9a-zA-Z] matches all alphanumeric characters\n\
(if the underlying character set is ASCII).\n\
Since Matlab treats hyphens literally, this expansion only applies to\n\
alphanumeric characters.\n\
To include '-' in the set, it should appear first or last in the brackets;\n\
to include ']', it should be the first character.\n\
If the first character is '^' then the word consists of characters\n\
NOT listed.\n\
\n\
@item %N...\n\
For %s, %c %d, %f, %n, %u, an optional width can be specified as %Ns etc.\n\
where N is an integer > 1.\n\
For %c, this causes exactly the next N characters to be read instead of\n\
a single character.\n\
For the other specifiers, it is an upper bound on the\n\
number of characters read;\n\
normal delimiters can cause fewer characters to be read.\n\
For complex numbers, this limit applies to the real and imaginary\n\
components individually.\n\
For %f and %n, format specifiers like %N.Mf are allowed, where M is an upper\n\
bound on number of characters after the decimal point to be considered;\n\
subsequent digits are skipped.\n\
For example, the specifier %8.2f would read 12.345e6 as 1.234e7.\n\
\n\
@item %*...\n\
The word specified by the remainder of the conversion specifier is skipped.\n\
\n\
@item literals\n\
In addition the format may contain literal character strings;\n\
these will be skipped during reading.\n\
If the input string does not match this literal, the processing terminates,\n\
unless \"ReturnOnError\" is set to \"continue\".\n\
@end table\n\
\n\
Parsed words corresponding to the first specifier are returned in the first\n\
output argument and likewise for the rest of the specifiers.\n\
\n\
By default, if there is only one input argument, @var{format} is @t{\"%f\"}.\n\
This means that numbers are read from @var{str} into a single column vector.\n\
If @var{format} is explicitly empty, \"\", then textscan will return data\n\
in a number of columns matching the number of fields on the first data\n\
line of the input.\n\
Either of these is suitable only if @var{str} contains only numeric fields.\n\
\n\
For example, the string\n\
\n\
@example\n\
@group\n\
@var{str} = \"\\\n\
Bunny Bugs   5.5\\n\\\n\
Duck Daffy  -7.5e-5\\n\\\n\
Penguin Tux   6\"\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
can be read using\n\
\n\
@example\n\
@var{a} = textscan (@var{str}, \"%s %s %f\");\n\
@end example\n\
\n\
The optional numeric argument @var{repeat} can be used for limiting the\n\
number of items read:\n\
\n\
@table @asis\n\
@item -1\n\
(default) read all of the string or file until the end.\n\
\n\
@item N\n\
Read until the first of two conditions occurs: the format has been processed\n\
N times, or N lines of the input have been processed.\n\
Zero (0) is an acceptable value for @var{repeat}.\n\
Currently, end-of-line characters inside %q, %c, and %[...]$ conversions\n\
do not contribute to the line count.\n\
This is incompatible with Matlab and may change in future.\n\
@end table\n\
\n\
The behavior of @code{textscan} can be changed via property-value pairs.\n\
The following properties are recognized:\n\
\n\
@table @asis\n\
@item @qcode{\"BufSize\"}\n\
This specifies the number of bytes to use for the internal buffer.\n\
A modest speed improvement is obtained by setting this to a large value\n\
when reading a large file, especially the input contains long strings.\n\
The default is 4096, or a value dependent on @var{n} is that is specified.\n\
\n\
@item @qcode{\"CollectOutput\"}\n\
A value of 1 or true instructs textscan to concatenate consecutive columns\n\
of the same class in the output cell array.\n\
A value of 0 or false (default) leaves output in distinct columns.\n\
\n\
@item @qcode{\"CommentStyle\"}\n\
Parts of @var{str} are considered comments and will be skipped.\n\
@var{value} is the comment style and can be either\n\
(1) One string, or 1x1 cell string, to skip everything to the right of it;\n\
(2) A cell array of two strings, to skip everything between the first and\n\
second strings.\n\
Comments are only parsed where whitespace is accepted, and do not act as\n\
delimiters.\n\
\n\
@item @qcode{\"Delimiter\"}\n\
If @var{value} is a string, any character in @var{value} will be used to\n\
split @var{str} into words.\n\
If @var{value} is a cell array of strings,\n\
any string in the array will be used to split @var{str} into words.\n\
(default value = any whitespace.)\n\
\n\
@item @qcode{\"EmptyValue\"}\n\
Value to return for empty numeric values in non-whitespace delimited data.\n\
The default is NaN@.\n\
When the data type does not support NaN (int32 for example),\n\
then default is zero.\n\
\n\
@item @qcode{\"EndOfLine\"}\n\
@var{value} can be either a emtpy or one character specifying the\n\
end of line character, or the pair\n\
@qcode{\"@xbackslashchar{}r@xbackslashchar{}n\"} (CRLF).\n\
In the latter case, any of\n\
@qcode{\"@xbackslashchar{}r\"}, @qcode{\"@xbackslashchar{}n\"} or\n\
@qcode{\"@xbackslashchar{}r@xbackslashchar{}n\"} is counted as a (single)\n\
newline.\n\
If no value is given, @qcode{\"@xbackslashchar{}r@xbackslashchar{}n\"} is\n\
used.\n\
@c If set to \"\" (empty string) EOLs are ignored as delimiters and added\n\
@c to whitespace.\n\
\n\
@c When reading from a character string, optional input argument @var{n}\n\
@c specifies the number of times @var{format} should be used (i.e., to limit\n\
@c the amount of data read).\n\
@c When reading from file, @var{n} specifies the number of data lines to read;\n\
@c in this sense it differs slightly from the format repeat count in strread.\n\
\n\
@item @qcode{\"HeaderLines\"}\n\
The first @var{value} number of lines of @var{fid} are skipped.\n\
Note that this does not refer to the first non-comment lines, but the first\n\
lines of any type.\n\
\n\
@item @qcode{\"MultipleDelimsAsOne\"}\n\
If @var{value} is non-zero,\n\
treat a series of consecutive delimiters, without whitespace in between,\n\
as a single delimiter.\n\
Consecutive delimiter series need not be vertically @qcode{\"aligned\"}.\n\
Without this option, a single delimiter before the end of the line does\n\
not cause the line to be considered to end with an empty value,\n\
but a single delimiter at the start of a line causes the line\n\
to be considered to start with an empty value.\n\
\n\
@item @qcode{\"TreatAsEmpty\"}\n\
Treat single occurrences (surrounded by delimiters or whitespace) of the\n\
string(s) in @var{value} as missing values.\n\
\n\
@item @qcode{\"ReturnOnError\"}\n\
If set to numerical 1 or true, return normally as soon as an error\n\
is encountered, such as trying to read a string using @qcode{%f}.\n\
If set to 0 or false, return an error and no data.\n\
If set to \"continue\" (default), textscan attempts to continue reading\n\
beyond the location; however, this may cause the parsing to get out of sync.\n\
\n\
@item @qcode{\"Whitespace\"}\n\
Any character in @var{value} will be interpreted as whitespace and trimmed;\n\
The default value for whitespace is\n\
@c Note: the next line specifically has a newline which generates a space\n\
@c       in the output of qcode, but keeps the next line < 80 characters.\n\
@qcode{\"\n\
@xbackslashchar{}b@xbackslashchar{}r@xbackslashchar{}n@xbackslashchar{}t\"}\n\
(note the space).  Unless whitespace is set to @qcode{\"\"} (empty) AND at\n\
least one @qcode{\"%s\"} format conversion specifier is supplied, a space is\n\
always part of whitespace.\n\
\n\
@end table\n\
\n\
When the number of words in @var{str} or @var{fid} doesn't match an exact\n\
multiple of the number of format conversion specifiers,\n\
textscan's behavior depends on\n\
whether the last character of the string or file is\n\
an end-of-line as specified by the EndOfLine option:\n\
\n\
@table @asis\n\
@item last character = end-of-line\n\
Data columns are padded with empty fields, NaN or 0 (for integer fields)\n\
so that all columns have equal length\n\
\n\
@item last character is not end-of-line\n\
Data columns are not padded; textscan returns columns of unequal length\n\
@end table\n\
\n\
\n\
The second output, @var{position}, provides the position, in characters\n\
from the beginning of the file or string, at which the processing stopped.\n\
\n\
@seealso{dlmread, fscanf, load, strread, textread}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () < 1)
    print_usage ();

  octave_value_list tmp_args = args.splice (0, 1);

  textscan tscanner;

  if (args(0).is_string ())
    {
      std::istringstream is (args(0).string_value ());

      retval(0) = tscanner.scan (is, tmp_args);

      std::ios::iostate state = is.rdstate ();
      is.clear ();
      retval(1) = octave_value (static_cast<long>(is.tellg ()));
      is.setstate (state);
    }
  else
    {
      octave_stream os = octave_stream_list::lookup (args(0), "textscan");
      std::istream *isp = os.input_stream ();
      if (! isp)
        error ("internal error: textscan called with invalid istream");

      retval(0) = tscanner.scan (*isp, tmp_args);

      // FIXME -- warn if stream is not opened in binary mode?
      std::ios::iostate state = os.input_stream ()->rdstate ();
      os.input_stream ()->clear ();
      retval(1) = os.tell ();
      os.input_stream ()->setstate (state);
    }

  return retval;
}

/*
%!test
%! str = "1,  2,  3,  4\n 5,  ,  ,  8\n 9, 10, 11, 12";
%! fmtstr = "%f %d %f %s";
%! c = textscan (str, fmtstr, 2, "delimiter", ",", "emptyvalue", -Inf);
%! assert (isequal (c{1}, [1;5]));
%! assert (length (c{1}), 2);
%! assert (iscellstr (c{4}));
%! assert (isequal (c{3}, [3; -Inf]));

%!test
%! b = [10:10:100];
%! b = [b; 8*b/5];
%! str = sprintf ("%g miles/hr = %g kilometers/hr\n", b);
%! fmt = "%f miles/hr = %f kilometers/hr";
%! c = textscan (str, fmt);
%! assert (c{1}, b(1,:)', 1e-5);
%! assert (c{2}, b(2,:)', 1e-5);

%!test
%! str = "13, -, NA, str1, -25\r\n// Middle line\r\n36, na, 05, str3, 6";
%! a = textscan (str, "%d %n %f %s %n", "delimiter", ",",
%!                "treatAsEmpty", {"NA", "na", "-"},"commentStyle", "//");
%! assert (a{1}, int32 ([13; 36]));
%! assert (a{2}, [NaN; NaN]);
%! assert (a{3}, [NaN; 5]);
%! assert (a{4}, {"str1"; "str3"});
%! assert (a{5}, [-25; 6]);

%!test
%! str = "Km:10 = hhhBjjj miles16hour\r\n";
%! str = [str "Km:15 = hhhJjjj miles241hour\r\n"];
%! str = [str "Km:2 = hhhRjjj miles3hour\r\n"];
%! str = [str "Km:25 = hhhZ\r\n"];
%! fmt = "Km:%d = hhh%1sjjj miles%dhour";
%! a = textscan (str, fmt, "delimiter", " ");
%! assert (a{1}', int32 ([10 15 2 25]));
%! assert (a{2}', {'B' 'J' 'R' 'Z'});
%! assert (a{3}', int32 ([16 241 3 0]));

## Test with default endofline parameter
%!test
%! c = textscan ("L1\nL2", "%s");
%! assert (c{:}, {"L1"; "L2"});

## Test with endofline parameter set to "" (empty) - newline should be in word
%!test
%! c = textscan ("L1\nL2", "%s", "endofline", "");
%! assert (int8 ([c{:}{:}]), int8 ([ 76,  49,  10,  76,  50 ]));

###  Matlab fails this test.  A literal after a conversion is not a delimiter
#%!test
#%! ## No delimiters at all besides EOL.  Skip fields, even empty fields
#%! str = "Text1Text2Text\nTextText4Text\nText57Text";
#%! c = textscan (str, "Text%*dText%dText");
#%! assert (c{1}, int32 ([2; 4; 0]));

## CollectOutput test
%!test
%! b = [10:10:100];
%! b = [b; 8*b/5; 8*b*1000/5];
%! str = sprintf ("%g miles/hr = %g (%g) kilometers (meters)/hr\n", b);
%! fmt = "%f miles%s %s %f (%f) kilometers %*s";
%! c = textscan (str, fmt, "collectoutput", 1);
%! assert (size (c{3}), [10, 2]);
%! assert (size (c{2}), [10, 2]);

## CollectOutput test with uneven column length files
%!test
%! b = [10:10:100];
%! b = [b; 8*b/5; 8*b*1000/5];
%! str = sprintf ("%g miles/hr = %g (%g) kilometers (meters)/hr\n", b);
%! str = [str "110 miles/hr"];
%! fmt = "%f miles%s %s %f (%f) kilometers %*s";
%! c = textscan (str, fmt, "collectoutput", 1);
%! assert (size (c{1}), [11, 1]);
%! assert (size (c{3}), [11, 2]);
%! assert (size (c{2}), [11, 2]);
%! assert (c{3}(end), NaN);
%! assert (c{2}{11, 1}, "/hr");
%! assert (isempty (c{2}{11, 2}), true);

## Double quoted string
%!test
%! str = 'First    "the second called ""the middle""" third';
%! fmt = "%q";
%! c = textscan (str, fmt);
%! assert (c{1}, {"First"; 'the second called "the middle"'; "third"});

## Arbitrary character
%!test
%! c = textscan ("a first, \n second, third", "%s %c %11c", 'delimiter', ' ,');
%! assert (c{1}, {"a"; "ond"});
%! assert (c{2}, {"f"; "t"});
%! assert (c{3}, {"irst, \n sec"; "hird"});

## Field width and non-standard delimiters
%!test
%! str = "12;34;123456789;7";
%! c = textscan (str, "%4d %4d", "delimiter", ";", "collectOutput", 1);
%! assert (c, {[12 34; 1234 5678; 9 7]});

## Field width and non-standard delimiters
%!test
%! str = "12;34;123456789;7";
%! c = textscan (str, "%4f %f", "delimiter", ";", "collectOutput", 1);
%! assert (c, {[12 34; 1234 56789; 7 NaN]});

## Ignore trailing delimiter, but use leading one
%!test
%! str = "12.234e+2,34, \n12345.789-9876j,78\n,10|3";
%! c = textscan (str, "%10.2f %f", "delimiter", ",", "collectOutput", 1,
%!                "expChars", "e|");
%! assert (c, {[1223 34; 12345.79-9876j 78; NaN 10000]}, 1e-6);

%!test
%! ## Multi-character delimiter
%! str = "99end2 space88gap 4564";
%! c = textscan (str, "%d %s", "delimiter", {"end", "gap", "space"});
%! assert (c{1}, int32([99; 88]));
%! assert (c{2}, {"2 "; "4564"});

### Delimiters as part of literals, and following literals
#%!test
#%! str = "12 R&D & 7";
#%! c = textscan (str, "%f R&D %f", "delimiter", "&", "collectOutput", 1, "EmptyValue", -99);
#%! assert (c, {[12 -99; 7 -99]});
#
### Delimiters as part of literals, and before literals
#%!test
#%! str = "12 & R&D 7";
#%! c = textscan (str, "%f R&D %f", "delimiter", "&", "collectOutput", 1);
#%! assert (c, {[12 7]});

%!test
%! ## Check number of lines read, not number of passes through format string
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid, "1\n2\n3\n4\n5\n6");
%! fseek (fid, 0, "bof");
%! c = textscan (fid, "%f %f", 2);
%! E = feof (fid);
%! fclose (fid);
%! unlink (f);
%! assert (c, {1, 2});
%! assert (!E);

%!test
%! ## Check number of lines read, not number of passes through format string
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid, "1\r\n2\r3\n4\r\n5\n6");
%! fseek (fid, 0, "bof");
%! c = textscan (fid, "%f %f", 4);
%! fclose (fid);
%! unlink (f);
%! assert (c, {[1;3], [2;4]})

%!test
%! ## Check number of lines read, with multiple delimiters
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid, "1-\r\n-2\r3-\n-4\r\n5\n6");
%! fseek (fid, 0, "bof");
%! c = textscan (fid, "%f %f", 4, "delimiter", "-", "multipleDelimsAsOne", 1);
%! fclose (fid);
%! unlink (f);
%! assert (c, {[1;3], [2;4]})

%!test
%! ## Check ReturnOnError
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid, "1 2 3\n4 s 6");
%! fseek (fid, 0, "bof");
%! c = textscan (fid, "%f %f %f");
%! fseek (fid, 0, "bof");
%! d = textscan (fid, "%f %f %f", "ReturnOnError", 1);
%! fseek (fid, 0, "bof");
%! fclose (fid);
%! unlink (f);
%! assert (c, {[1;4], [2;NaN], [3;6]})
%! assert (d, {[1;4], [2], [3]})

%!test
%! ## Check ReturnOnError
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid, "1 2 3\n4 s 6\n");
%! fseek (fid, 0, "bof");
%! c = textscan (fid, "%f %f %f", "ReturnOnError", 1);
%! fseek (fid, 0, "bof");
%! fclose (fid);
%! unlink (f);
%! assert (c, {[1;4], 2, 3})

%!error <Read error in field 2 of row 2> textscan ("1 2 3\n4 s 6", "%f %f %f", "ReturnOnError", 0);

%!test
%! ## Check ReturnOnError
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid, "1 s 3\n4 5 6");
%! fseek (fid, 0, "bof");
%! c = textscan (fid, "");
%! fseek (fid, 0, "bof");
%! d = textscan (fid, "", "ReturnOnError", 1);
%! fseek (fid, 0, "bof");
%! fclose (fid);
%! unlink (f);
%! assert (c, {[1;4], [NaN;5], [3;6]})
%! assert (d, {1})

%!test
%! ## Check ReturnOnError with empty fields
%! c = textscan ("1,,3\n4,5,6", "", "Delimiter", ",", "ReturnOnError", 1);
%! assert (c, {[1;4], [NaN;5], [3;6]})

%!test
%! ## Check ReturnOnError with empty fields
%! c = textscan ("1,,3\n4,5,6", "%f %f %f", "Delimiter", ",", "ReturnOnError", 1);
%! assert (c, {[1;4], [NaN;5], [3;6]})

%!test
%! ## Check ReturnOnError in first column
%! c = textscan ("1 2 3\ns 5 6", "", "ReturnOnError", 1);
%! assert (c, {1, 2, 3})

## Test input validation
%!error textscan ()
%!error textscan (single (40))
%!error textscan ({40})
%!error <must be a string> textscan ("Hello World", 2)
#%!error <cannot provide position information> [C, pos] = textscan ("Hello World")
%!error <at most one character or> textscan ("Hello World", '%s', 'EndOfLine', 3)
%!error <'%z' is not a valid format specifier> textscan ("1.0", "%z");
%!error <no valid format conversion specifiers> textscan ("1.0", "foo");

## Test incomplete first data line
%! R = textscan (['Empty1' char(10)], 'Empty%d %f');
%! assert (R{1}, int32 (1));
%! assert (isempty (R{2}), true);

## bug #37023
%!test
%! data = textscan("   1. 1 \n 2 3\n", '%f %f');
%! assert (data{1}, [1; 2], 1e-15);
%! assert (data{2}, [1; 3], 1e-15);

## Whitespace test (bug #37333) using delimiter ";"
%!test
%! tc = [];
%! tc{1, 1} = "C:/code;";
%! tc{1, end+1} = "C:/code/meas;";
%! tc{1, end+1} = " C:/code/sim;";
%! tc{1, end+1} = "C:/code/utils;";
%! string = [tc{:}];
%! c = textscan (string, "%s", "delimiter", ";");
%! for k = 1:max (numel (c{1}), numel (tc))
%!   lh = c{1}{k};
%!   rh = tc{k};
%!   rh(rh == ";") = "";
%!   rh = strtrim (rh);
%!   assert (strcmp (lh, rh));
%! end

## Whitespace test (bug #37333), adding multipleDelimsAsOne true arg
%!test
%! tc = [];
%! tc{1, 1} = "C:/code;";
%! tc{1, end+1} = " C:/code/meas;";
%! tc{1, end+1} = "C:/code/sim;;";
%! tc{1, end+1} = "C:/code/utils;";
%! string = [tc{:}];
%! c = textscan (string, "%s", "delimiter", ";", "multipleDelimsAsOne", 1);
%! for k = 1:max (numel (c{1}), numel (tc))
%!   lh = c{1}{k};
%!   rh = tc{k};
%!   rh(rh == ";") = "";
%!   rh = strtrim (rh);
%!   assert (strcmp (lh, rh));
%! end

## Whitespace test (bug #37333), adding multipleDelimsAsOne false arg
%!test
%! tc = [];
%! tc{1, 1} = "C:/code;";
%! tc{1, end+1} = " C:/code/meas;";
%! tc{1, end+1} = "C:/code/sim;;";
%! tc{1, end+1} = "";
%! tc{1, end+1} = "C:/code/utils;";
%! string = [tc{:}];
%! c = textscan (string, "%s", "delimiter", ";", "multipleDelimsAsOne", 0);
%! for k = 1:max (numel (c{1}), numel (tc))
%!   lh = c{1}{k};
%!   rh = tc{k};
%!   rh(rh == ";") = "";
%!   rh = strtrim (rh);
%!   assert (strcmp (lh, rh));
%! end

## Whitespace test (bug #37333) whitespace "" arg
%!test
%! tc = [];
%! tc{1, 1} = "C:/code;";
%! tc{1, end+1} = " C:/code/meas;";
%! tc{1, end+1} = "C:/code/sim;";
%! tc{1, end+1} = "C:/code/utils;";
%! string = [tc{:}];
%! c = textscan (string, "%s", "delimiter", ";", "whitespace", "");
%! for k = 1:max (numel (c{1}), numel (tc))
%!   lh = c{1}{k};
%!   rh = tc{k};
%!   rh(rh == ";") = "";
%!   assert (strcmp (lh, rh));
%! end

## Whitespace test (bug #37333), whitespace " " arg
%!test
%! tc = [];
%! tc{1, 1} = "C:/code;";
%! tc{1, end+1} = " C:/code/meas;";
%! tc{1, end+1} = "C:/code/sim;";
%! tc{1, end+1} = "C:/code/utils;";
%! string = [tc{:}];
%! c = textscan (string, "%s", "delimiter", ";", "whitespace", " ");
%! for k = 1:max (numel (c{1}), numel (tc))
%!   lh = c{1}{k};
%!   rh = tc{k};
%!   rh(rh == ";") = "";
%!   rh = strtrim (rh);
%!   assert (strcmp (lh, rh));
%! end

## Tests reading with empty format, should return proper nr of columns
%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid, " 1 2 3 4\n5 6 7 8");
%! fseek (fid, 0, "bof");
%! A = textscan (fid, "");
%! E = feof (fid);
%! fclose (fid);
%! unlink (f);
%! assert (A{1}, [1 ; 5], 1e-6);
%! assert (A{2}, [2 ; 6], 1e-6);
%! assert (A{3}, [3 ; 7], 1e-6);
%! assert (A{4}, [4 ; 8], 1e-6);
%! assert (E);

## Tests reading with empty format; empty fields & incomplete lower row
%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid, " ,2,,4\n5,6");
%! fseek (fid, 0, "bof");
%! A = textscan (fid, "", "delimiter", ",", "EmptyValue", 999, "CollectOutput" , 1);
%! fclose (fid);
%! unlink (f);
%! assert (A{1}, [999, 2, 999, 4; 5, 6, 999, 999], 1e-6);

## Error message tests

%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! msg1 = "textscan: 1 parameters given, but only 0 values";
%! try
%! A = textscan (fid, "", "headerlines");
%! end_try_catch;
%! assert (!feof (fid));
%! fclose (fid);
%! unlink (f);
%! assert (msg1, lasterr);

%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! msg1 = "textscan: HeaderLines must be numeric";
%! try
%! A = textscan (fid, "", "headerlines", "hh");
%! end_try_catch;
%! fclose (fid);
%! unlink (f);
%! assert (msg1, lasterr);

%!test
%! ## Skip headerlines
%! A = textscan ("field 1  field2\n 1 2\n3 4", "", "headerlines", 1, "collectOutput", 1);
%! assert (A, {[1 2; 3 4]});

%!test
%! ## Skip headerlines with non-default EOL
%! A = textscan ("field 1  field2\r 1 2\r3 4", "", "headerlines", 2, "collectOutput", 1, "EndOfLine", '\r');
%! assert (A, {[3 4]});

%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid,"some_string");
%! fseek (fid, 0, "bof");
%! msg1 = "textscan: EndOfLine must be at most one character or '\\r\\n'";
%! try
%! A = textscan (fid, "%f", "EndOfLine", "\n\r");
%! end_try_catch;
%! fclose (fid);
%! unlink (f);
%! assert (msg1, lasterr);

%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid,"some_string");
%! fseek (fid, 0, "bof");
%! msg1 = "textscan: EndOfLine must be at most one character or '\\r\\n'";
%! try
%! A = textscan (fid, "%f", "EndOfLine", 33);
%! end_try_catch;
%! fclose (fid);
%! unlink (f);
%! assert (msg1, lasterr);

## Bug #41824
%!test
%! assert (textscan ("123", "", "whitespace", " "){:}, 123);

## Bug #42343-1, just test supplied emptyvalue
%!test
%! assert (textscan (",NaN", "", "delimiter", "," ,"emptyValue" ,Inf), {Inf, NaN});

## Bug #42343-2, test padding with supplied emptyvalue
%!test
%! a = textscan (",1,,4\nInf,  ,NaN\n", "", "delimiter", ",", "emptyvalue", -10);
%! assert (cell2mat (a), [-10, 1, -10, 4; Inf, -10, NaN, -10]);

## Bug #42528
%!test
%! assert (textscan ("1i", ""){1},  0+1i);
%! assert (cell2mat (textscan ("3, 2-4i, NaN\n -i, 1, 23.4+2.2i\n 1+1 1+1j", "", "delimiter", ",")), [3+0i, 2-4i, NaN+0i; 0-i,  1+0i, 23.4+2.2i; 1 1 1+1i]);

%!test
%! ## TreatAsEmpty
%! C = textscan ("1,2,3,NN,5,6\n", "%d%d%d%f", "delimiter", ",", "TreatAsEmpty", "NN");
%! assert (C{3}(1), int32 (3));
%! assert (C{4}(1), NaN);

## MultipleDelimsAsOne
%!test
%! str = "11, 12, 13,, 15\n21,, 23, 24, 25\n,, 33, 34, 35\n";
%! C = textscan (str, "%f %f %f %f", "delimiter", ",", "multipledelimsasone", 1, "endofline", "\n");
%! assert (C{1}', [11, 21, 33]);
%! assert (C{2}', [12, 23, 34]);
%! assert (C{3}', [13, 24, 35]);
%! assert (C{4}', [15, 25, NaN]);

## Bug #44750
%!test
%! assert (textscan ("/home/foo/", "%s", "delimiter", "/", "MultipleDelimsAsOne", 1){1}, ...
%!         {"home"; "foo"});

### Allow cuddling %sliteral but warn it is ambiguous
#%!test
#%! C = textscan ("abcxyz51\nxyz83\n##xyz101", "%s xyz %d");
#%! assert (C{1}([1 3]), {"abc"; "##"});
#%! assert (isempty (C{1}{2}), true);
#%! assert (C{2}, int32([51; 83; 101]));
### Literals are not delimiters.

## Test for false positives in check for non-supported format specifiers
%!test
%! assert (textscan ("Total: 32.5 % (of cm values)", "Total: %f %% (of cm values)"){1}, 32.5, 1e-5);

## Test various forms of string format specifiers (bug #45712)
%!test
%! str = "14 :1 z:2 z:3 z:5 z:11";
%! C = textscan (str, "%f %s %*s %3s %*3s %f", "delimiter", ":");
%! assert (C, {14, {"1 z"}, {"3 z"}, 11});

%% Bit width, fixed width conv. specifiers
%!test
%! str2 = "123456789012345 ";
%! str2 = [str2 str2 str2 str2 str2 str2 str2 str2];
%! str2 = [str2 "123456789.01234 1234567890.1234 12345.678901234 12345.678901234"];
%! pttrn = "%3u8%*s %5u16%*s %10u32%*s %15u64 %3d8%*s %5d16%*s %10d32%*s %15d64 %9f32%*s %14f64%*s %10.2f32%*s %12.2f64%*s";
%! C = textscan (str2, pttrn, "delimiter", " ");
%! assert (C{1}, uint8 (123));
%! assert (C{2}, uint16 (12345));
%! assert (C{3}, uint32 (1234567890));
%! assert (C{4}, uint64 (123456789012345));
%! assert (C{5}, int8 (123));
%! assert (C{6}, int16 (12345));
%! assert (C{7}, int32 (1234567890));
%! assert (C{8}, int64 (123456789012345));
%! assert (C{9}, single (123456789), 1e-12);
%! assert (C{10}, double (1234567890.123), 1e-15);
%! assert (C{11}, single (12345.68), 1e-5);
%! assert (C{12}, double (12345.68), 1e-11);

%% Bit width, fixed width conv. specifiers -- check the right amount is left
%!test
%! str2 = "123456789012345 ";
%! str2 = [str2 str2 "123456789.01234"];
%! pttrn = "%3u8 %5u16 %10u32 %3d8 %5d16 %10d32 %9f32 %9f";
%! C = textscan (str2, pttrn, "delimiter", " ");
%! assert (C{1}, uint8 (123));
%! assert (C{2}, uint16 (45678));
%! assert (C{3}, uint32 (9012345));
%! assert (C{4}, int8 (123));
%! assert (C{5}, int16 (45678));
%! assert (C{6}, int32 (9012345));
%! assert (C{7}, single (123456789), 1e-12);
%! assert (C{8}, double (0.01234), 1e-12);

%!test
%! C = textscan ("123.123", "%2f %3f %3f");
%! assert (C{1}, 12);
%! assert (C{2}, 3.1, 1e-11);
%! assert (C{3}, 23);

%!test
%! C = textscan ("123.123", "%3f %3f %3f");
%! assert (C{1}, 123);
%! assert (C{2}, 0.12, 1e-11);
%! assert (C{3}, 3);

%!test
%! C = textscan ("123.123", "%4f %3f");
%! assert (C{1}, 123);
%! assert (C{2}, 123);

%% field width interrupts exponent.  (Matlab incorrectly gives [12, 2e12])
%!test
%! assert (textscan ("12e12",  "%4f"), {[120;  2]});
%! assert (textscan ("12e+12", "%5f"), {[120;  2]});
%! assert (textscan ("125e-12","%6f"), {[12.5; 2]});

%% %[] tests
%% Plain [..] and *[..]
%!test
%! ar = "abcdefguvwxAny\nacegxyzTrailing\nJunk";
%! C = textscan (ar, "%[abcdefg] %*[uvwxyz] %s");
%! assert (C{1}, {"abcdefg"; "aceg"; ""});
%! assert (C{2}, {"Any"; "Trailing"; "Junk"});

%!test
%! assert (textscan ("A2 B2 C3", "%*[ABC]%d", 3), {int32([2; 2; 3])});

%% [^..] and *[^..]
%!test
%! br = "abcdefguvwx1Any\nacegxyz2Trailing\n3Junk";
%! C = textscan (br, "%[abcdefg] %*[^0123456789] %s");
%! assert (C{1}, {"abcdefg"; "aceg"; ""});
%! assert (C{2}, {"1Any"; "2Trailing"; "3Junk"});

%% [..] and [^..] containing delimiters
%!test
%! cr = "ab cd efguv wx1Any\na ce gx yz2Trailing\n   3Junk";
%! C = textscan (cr, "%[ abcdefg] %*[^0123456789] %s", "delimiter", " \n", "whitespace", "");
%! assert (C{1}, {"ab cd efg"; "a ce g"; "   "});
%! assert (C{2}, {"1Any"; "2Trailing"; "3Junk"});

%% Bug #36464
%!test
%! assert (textscan ('1 2 3 4 5 6', "%*n%n%*[^\n]"){1}, 2);

%% test %[]] and %[^]]
%!test
%! assert (textscan ('345]', "%*[123456]%[]]"){1}{1}, "]");
%! assert (textscan ('345]', "%*[^]]%s"){1}{1}, "]");

%% Test that "-i" checks the next two characters
%!test
%! C = textscan ("-i -in -inf -infinity", "%f %f%s %f %f %s");
%! assert (C, {-i, -i, {"n"}, -Inf, -Inf, {"inity"}});

%% Again for "+i", this time with custom parser
%!test
%! C = textscan ("+i +in +inf +infinity", "%f %f%s %f %f %s", "ExpChars", "eE");
%! assert (C, {i, i, {"n"}, Inf, Inf, {"inity"}});

%% Check single quoted format interprets control sequences
%!test
%! C = textscan ("1 2\t3 4", '%f %[^\t] %f %f');
%! assert (C, {1, {"2"}, 3, 4});

%% Check overflow and underflow of integer types
%!test
%! a = "-1e90 ";
%! b = "1e90 ";
%! fmt = "%d8 %d16 %d32 %d64 %u8 %u16 %u32 %u64 ";
%! C = textscan ([a a a a a a a a b b b b b b b b], fmt);
%! assert (C{1}, int8 ([-128; 127]));
%! assert (C{2}, int16([-32768; 32767]));
%! assert (C{3}, int32([-2147483648; 2147483647]));
%! assert (C{4}, int64([-9223372036854775808; 9223372036854775807]));
%! assert (C{5}, uint8 ([0; 255]));
%! assert (C{6}, uint16([0; 65535]));
%! assert (C{7}, uint32([0; 4294967295]));
%! assert (C{8}, uint64([0; 18446744073709551615]));

%% Tests from Matlab (does The MathWorks have any copyright over the input?)
%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid,"09/12/2005 Level1 12.34 45 1.23e10 inf Nan Yes 5.1+3i\n");
%! fprintf (fid,"10/12/2005 Level2 23.54 60 9e19 -inf  0.001 No 2.2-.5i\n");
%! fprintf (fid,"11/12/2005 Level3 34.90 12 2e5   10  100   No 3.1+.1i\n");
%! fseek (fid, 0, "bof");
%! C = textscan(fid,"%s %s %f32 %d8 %u %f %f %s %f");
%! %assert (C{1}, {"09/12/2005";"10/12/2005";"11/12/2005"});
%! assert (C{2}, {"Level1";"Level2";"Level3"});
%! assert (C{3}, [single(12.34);single(23.54);single(34.90)]);
%! assert (C{4}, [int8(45);int8(60);int8(12)]);
%! assert (C{5}, [uint32(4294967295);uint32(4294967295);uint32(200000)]);
%! assert (C{6}, [inf;-inf;10]);
%! assert (C{7}, [NaN;0.001;100], eps);
%! assert (C{8}, {"Yes";"No";"No"});
%! assert (C{9}, [5.1+3i;2.2-0.5i;3.1+0.1i]);
%! fseek (fid, 0, "bof");
%! C = textscan(fid,"%s Level%d %f32 %d8 %u %f %f %s %f");
%! assert (C{2}, [int32(1);int32(2);int32(3)]);
%! assert (C{3}, [single(12.34);single(23.54);single(34.90)]);
%! fseek (fid, 0, "bof");
%! C = textscan(fid,'%s %*[^\n]');
%! fclose (fid);
%! unlink (f);
%! assert (C, {{"09/12/2005";"10/12/2005";"11/12/2005"}});

%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid,"1,  2,  3,  4,   ,  6\n");
%! fprintf (fid,"7,  8,  9,   , 11, 12\n");
%! fseek (fid, 0, "bof");
%! C = textscan(fid,"%f %f %f %f %u8 %f", "Delimiter",",","EmptyValue",-Inf);
%! fclose (fid);
%! unlink (f);
%! assert (C{4}, [4; -Inf]);
%! assert (C{5}, uint8 ([0; 11]));

%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid,"abc, 2, NA, 3, 4\n");
%! fprintf (fid,"// Comment Here\n");
%! fprintf (fid,"def, na, 5, 6, 7\n");
%! fseek (fid, 0, "bof");
%! C = textscan(fid,"%s %n %n %n %n","Delimiter",",","TreatAsEmpty",{"NA","na"},"CommentStyle","//");
%! fclose (fid);
%! unlink (f);
%! assert (C{1}, {"abc";"def"});
%! assert (C{2}, [2; NaN]);
%! assert (C{3}, [NaN; 5]);
%! assert (C{4}, [3; 6]);
%! assert (C{5}, [4; 7]);

%!test
%!## Test start of comment as string
%! c = textscan ("1 / 2 // 3", "%n %s %u8", "CommentStyle", {"//"});
%! assert (c, {1, "/", 2});
*/

// These tests have end-comment sequences, so can't just be in a comment
#if 0
%!test
%!## Test unfinished comment
%! c = textscan ("1 2 /* half comment", "%n %u8", "CommentStyle", {"/*", "*/"});
%! assert (c, {1, 2});

## Test reading from a real file
%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! d = rand (1, 4);
%! fprintf (fid, "  %f %f /* comment */  %f  %f ", d);
%! fseek (fid, 0, "bof");
%! A = textscan (fid, "%f %f", "CommentStyle", {"/*", "*/"});
%! E = feof (fid);
%! fclose (fid);
%! unlink (f);
%! assert (A{1}, [d(1); d(3)], 1e-6);
%! assert (A{2}, [d(2); d(4)], 1e-6);
%! assert (E);
#endif
