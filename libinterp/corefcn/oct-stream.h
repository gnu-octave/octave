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

#if ! defined (octave_oct_stream_h)
#define octave_oct_stream_h 1

#include "octave-config.h"

#include <ios>
#include <iosfwd>
#include <list>
#include <map>
#include <memory>
#include <string>

#include "oct-string.h"

// These only appear as reference arguments or return values.

class Cell;
class octave_value;
class octave_value_list;
class string_vector;

#include "Array-fwd.h"
#include "data-conv.h"
#include "mach-info.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class interpreter;

// These are only needed as arguments to private functions, so they
// are also treated as private.

class scanf_format_elt;
class scanf_format_list;

class printf_format_elt;
class printf_format_list;

// Provide an interface for Octave streams.

class
OCTINTERP_API
base_stream
{
  friend class stream;

public:

  base_stream (std::ios::openmode arg_md = std::ios::in | std::ios::out,
               mach_info::float_format ff = mach_info::native_float_format (),
               const std::string& encoding = "utf-8")
    : m_mode (arg_md), m_flt_fmt (ff), m_encoding (encoding),
      m_conv_ostream (nullptr), m_fail (false), m_open_state (true),
      m_errmsg ()
  { }

  // No copying!

  base_stream (const base_stream&) = delete;

  base_stream& operator = (const base_stream&) = delete;

  virtual ~base_stream (void) = default;

  // The remaining functions are not specific to input or output only,
  // and must be provided by the derived classes.

  // Position a stream at OFFSET relative to ORIGIN.

  virtual int seek (off_t offset, int origin) = 0;

  // Return current stream position.

  virtual off_t tell (void) = 0;

  // Return TRUE if EOF has been reached on this stream.

  virtual bool eof (void) const = 0;

  // The name of the file.

  virtual std::string name (void) const = 0;

  // If the derived class provides this function and it returns a
  // pointer to a valid istream, scanf(), read(), getl(), and gets()
  // will automatically work for this stream.

  virtual std::istream * input_stream (void) { return nullptr; }

  // If the derived class provides this function and it returns a
  // pointer to a valid ostream, flush(), write(), and printf() will
  // automatically work for this stream.

  virtual std::ostream * output_stream (void) { return nullptr; }

  // Return either the original output stream or one wrapped with the
  // encoding facet.

  std::ostream * preferred_output_stream (void)
  {
    if (! m_encoding.compare ("utf-8"))
      return output_stream ();

    if (m_conv_ostream)
      return m_conv_ostream;

    // wrap the output stream with encoding conversion facet
    std::ostream *os = output_stream ();
    if (os && *os)
      {
        convfacet_u8 *facet = new convfacet_u8 (m_encoding);
        std::wbuffer_convert<convfacet_u8, char> *converter
          = new std::wbuffer_convert<convfacet_u8, char> (os->rdbuf (),
              facet);
        m_conv_ostream = new std::ostream (converter);
      }

    return (m_conv_ostream ? m_conv_ostream : output_stream ());
  }

  // Return TRUE if this stream is open.

  bool is_open (void) const { return m_open_state; }

  virtual void do_close (void) { }

  void close (void)
  {
    if (is_open ())
      {
        m_open_state = false;
        do_close ();
      }
  }

  virtual int file_number (void) const
  {
    // Kluge alert!

    if (name () == "stdin")
      return 0;
    else if (name () == "stdout")
      return 1;
    else if (name () == "stderr")
      return 2;
    else
      return -1;
  }

  bool ok (void) const { return ! m_fail; }

  // Return current error message for this stream.

  std::string error (bool clear, int& err_num);

protected:

  int mode (void) const { return m_mode; }

  mach_info::float_format float_format (void) const { return m_flt_fmt; }

  std::string encoding (void) const { return m_encoding; }

  // Set current error state and set fail to TRUE.

  OCTINTERP_API void error (const std::string& msg);
  OCTINTERP_API void error (const std::string& who, const std::string& msg);

  // Clear any error message and set fail to FALSE.

  OCTINTERP_API void clear (void);

  // Clear stream state.

  OCTINTERP_API void clearerr (void);

private:

  // The permission bits for the file.  Should be some combination of
  // std::ios::open_mode bits.
  int m_mode;

  // Data format.
  mach_info::float_format m_flt_fmt;

  // Code page
  std::string m_encoding;

  // encoding conversion facet
  typedef string::deletable_facet<string::codecvt_u8> convfacet_u8;

  // FIXME: Identified by compiler as unused private field.
  // Commented out 10/29/2022.
  // If there are no repercussions, delete entirely.
  // std::wbuffer_convert<convfacet_u8, char> *m_converter;

  // wrappers for encoding conversion
  // std::istream *m_conv_istream;

  std::ostream *m_conv_ostream;

  // TRUE if an error has occurred.
  bool m_fail;

  // TRUE if this stream is open.
  bool m_open_state;

  // Should contain error message if fail is TRUE.
  std::string m_errmsg;

  // Functions that are defined for all input streams (input streams
  // are those that define is).

  OCTINTERP_API std::string
  do_gets (octave_idx_type max_len, bool& err, bool strip_newline,
           const std::string& who /* = "gets" */);

  OCTINTERP_API std::string
  getl (octave_idx_type max_len, bool& err,
        const std::string& who /* = "getl" */);
  OCTINTERP_API std::string
  gets (octave_idx_type max_len, bool& err,
        const std::string& who /* = "gets" */);
  OCTINTERP_API off_t
  skipl (off_t count, bool& err, const std::string& who /* = "skipl" */);

  OCTINTERP_API octave_value
  do_scanf (scanf_format_list& fmt_list, octave_idx_type nr,
            octave_idx_type nc, bool one_elt_size_spec,
            octave_idx_type& count, const std::string& who /* = "scanf" */);

  OCTINTERP_API octave_value
  scanf (const std::string& fmt, const Array<double>& size,
         octave_idx_type& count, const std::string& who /* = "scanf" */);

  OCTINTERP_API bool
  do_oscanf (const scanf_format_elt *elt, octave_value&,
             const std::string& who /* = "scanf" */);

  OCTINTERP_API octave_value_list
  oscanf (const std::string& fmt, const std::string& who /* = "scanf" */);

  OCTINTERP_API octave_value
  do_textscan (const std::string& fmt, octave_idx_type ntimes,
               const octave_value_list& options,
               const std::string& who, octave_idx_type& count);

  // Functions that are defined for all output streams (output streams
  // are those that define os).

  OCTINTERP_API int flush (void);

  OCTINTERP_API int
  do_numeric_printf_conv (std::ostream& os, const printf_format_elt *elt,
                          int nsa, int sa_1, int sa_2,
                          const octave_value& val,
                          const std::string& who);

  OCTINTERP_API void field_width_error (const std::string& who) const;

  OCTINTERP_API int
  do_printf (printf_format_list& fmt_list, const octave_value_list& args,
             const std::string& who /* = "printf" */);

  OCTINTERP_API int
  printf (const std::string& fmt, const octave_value_list& args,
          const std::string& who /* = "printf" */);

  OCTINTERP_API int
  puts (const std::string& s, const std::string& who /* = "puts" */);

  // We can always do this in terms of seek(), so the derived class
  // only has to provide that.

  OCTINTERP_API void
  invalid_operation (const std::string& who, const char *rw);
};

class
OCTINTERP_API
stream
{
public:

  // BS must be allocated with new or nullptr.
  stream (base_stream *bs = nullptr) : m_rep (bs) { }

  stream (const stream&) = default;

  stream& operator = (const stream&) = default;

  ~stream (void) = default;

  OCTINTERP_API int flush (void);

  OCTINTERP_API std::string
  getl (octave_idx_type max_len, bool& err,
        const std::string& who /* = "getl" */);

  OCTINTERP_API std::string
  getl (const octave_value& max_len, bool& err,
        const std::string& who /* = "getl" */);

  OCTINTERP_API std::string
  gets (octave_idx_type max_len, bool& err,
        const std::string& who /* = "gets" */);

  OCTINTERP_API std::string
  gets (const octave_value& max_len, bool& err,
        const std::string& who /* = "gets" */);

  OCTINTERP_API off_t
  skipl (off_t count, bool& err, const std::string& who /* = "skipl" */);

  OCTINTERP_API off_t
  skipl (const octave_value& count, bool& err,
         const std::string& who /* = "skipl" */);

  OCTINTERP_API int seek (off_t offset, int origin);

  OCTINTERP_API int
  seek (const octave_value& offset, const octave_value& origin);

  OCTINTERP_API off_t tell (void);

  OCTINTERP_API int rewind (void);

  OCTINTERP_API bool is_open (void) const;

  OCTINTERP_API void close (void);

  OCTINTERP_API octave_value
  read (const Array<double>& size, octave_idx_type block_size,
        oct_data_conv::data_type input_type,
        oct_data_conv::data_type output_type,
        octave_idx_type skip, mach_info::float_format flt_fmt,
        octave_idx_type& count);

  OCTINTERP_API octave_idx_type
  write (const octave_value& data, octave_idx_type block_size,
         oct_data_conv::data_type output_type,
         octave_idx_type skip, mach_info::float_format flt_fmt);

  OCTINTERP_API bool write_bytes (const void *data, std::size_t n_elts);

  OCTINTERP_API bool skip_bytes (std::size_t n_elts);

  template <typename T>
  OCTINTERP_API octave_idx_type
  write (const Array<T>& data, octave_idx_type block_size,
         oct_data_conv::data_type output_type,
         octave_idx_type skip, mach_info::float_format flt_fmt);

  OCTINTERP_API octave_value
  scanf (const std::string& fmt, const Array<double>& size,
         octave_idx_type& count, const std::string& who /* = "scanf" */);

  OCTINTERP_API octave_value
  scanf (const octave_value& fmt, const Array<double>& size,
         octave_idx_type& count, const std::string& who /* = "scanf" */);

  OCTINTERP_API octave_value_list
  oscanf (const std::string& fmt, const std::string& who /* = "scanf" */);

  OCTINTERP_API octave_value_list
  oscanf (const octave_value& fmt, const std::string& who /* = "scanf" */);

  OCTINTERP_API octave_value
  textscan (const std::string& fmt, octave_idx_type ntimes,
            const octave_value_list& options,
            const std::string& who, octave_idx_type& count);

  OCTINTERP_API int
  printf (const std::string& fmt, const octave_value_list& args,
          const std::string& who /* = "printf" */);

  OCTINTERP_API int
  printf (const octave_value& fmt, const octave_value_list& args,
          const std::string& who /* = "printf" */);

  OCTINTERP_API int
  puts (const std::string& s, const std::string& who /* = "puts" */);
  OCTINTERP_API int
  puts (const octave_value& s, const std::string& who /* = "puts" */);

  OCTINTERP_API bool eof (void) const;

  OCTINTERP_API std::string error (bool clear, int& err_num);

  std::string error (bool clear = false)
  {
    int err_num;
    return error (clear, err_num);
  }

  // Set the error message and state.

  void error (const std::string& msg)
  {
    if (m_rep)
      m_rep->error (msg);
  }

  void error (const char *msg) { error (std::string (msg)); }

  int file_number (void) { return m_rep ? m_rep->file_number () : -1; }

  bool is_valid (void) const { return bool (m_rep); }

  bool ok (void) const { return m_rep && m_rep->ok (); }

  operator bool () const { return ok (); }

  OCTINTERP_API std::string name (void) const;

  OCTINTERP_API int mode (void) const;

  OCTINTERP_API mach_info::float_format float_format (void) const;

  OCTINTERP_API static std::string mode_as_string (int mode);

  std::string encoding (void)
  {
    return m_rep ? m_rep->encoding () : std::string ();
  }

  std::istream * input_stream (void)
  {
    return m_rep ? m_rep->input_stream () : nullptr;
  }

  std::ostream * output_stream (void)
  {
    return (m_rep ? m_rep->output_stream () : nullptr);
  }

  std::ostream * preferred_output_stream ()
  {
    // return output stream with encoding facet if applicable
    return (m_rep ? m_rep->preferred_output_stream () : nullptr);
  }

  void clearerr (void) { if (m_rep) m_rep->clearerr (); }

private:

  // The actual representation of this stream.
  std::shared_ptr<base_stream> m_rep;

  bool stream_ok (bool clear = true) const
  {
    bool retval = true;

    if (m_rep)
      {
        if (clear)
          m_rep->clear ();
      }
    else
      retval = false;

    return retval;
  }

  void invalid_operation (const std::string& who, const char *rw)
  {
    if (m_rep)
      m_rep->invalid_operation (who, rw);
  }

  OCTINTERP_API octave_value
  finalize_read (std::list<void *>& input_buf_list,
                 octave_idx_type input_buf_elts,
                 octave_idx_type elts_read,
                 octave_idx_type nr, octave_idx_type nc,
                 oct_data_conv::data_type input_type,
                 oct_data_conv::data_type output_type,
                 mach_info::float_format ffmt);
};

class
OCTINTERP_API
stream_list
{
public:

  OCTINTERP_API stream_list (interpreter& interp);

  stream_list (const stream_list&) = delete;
  stream_list& operator = (const stream_list&) = delete;

  OCTINTERP_API ~stream_list (void);

  OCTINTERP_API int insert (stream& os);

  OCTINTERP_API stream lookup (int fid, const std::string& who = "") const;
  OCTINTERP_API stream
  lookup (const octave_value& fid, const std::string& who = "") const;

  OCTINTERP_API int remove (int fid, const std::string& who = "");
  OCTINTERP_API int remove (const octave_value& fid,
                            const std::string& who = "");

  OCTINTERP_API void clear (bool flush = true);

  OCTINTERP_API string_vector get_info (int fid) const;
  OCTINTERP_API string_vector get_info (const octave_value& fid) const;

  OCTINTERP_API std::string list_open_files (void) const;

  OCTINTERP_API octave_value open_file_numbers (void) const;

  OCTINTERP_API int get_file_number (const octave_value& fid) const;

  OCTINTERP_API octave_value stdin_file (void) const;
  OCTINTERP_API octave_value stdout_file (void) const;
  OCTINTERP_API octave_value stderr_file (void) const;

private:

  typedef std::map<int, stream> ostrl_map;

  ostrl_map m_list;

  mutable ostrl_map::const_iterator m_lookup_cache;

  int m_stdin_file;
  int m_stdout_file;
  int m_stderr_file;
};

OCTAVE_END_NAMESPACE(octave)

#endif
