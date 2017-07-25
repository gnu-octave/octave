/*

Copyright (C) 1996-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_oct_stream_h)
#define octave_oct_stream_h 1

#include "octave-config.h"

#include <ios>
#include <iosfwd>
#include <list>
#include <map>
#include <string>

// These only appear as reference arguments or return values.

template <typename T> class Array;
class Cell;
class octave_value;
class octave_value_list;
class string_vector;

#include "data-conv.h"
#include "mach-info.h"
#include "oct-refcount.h"

namespace octave
{
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
                 mach_info::float_format ff = mach_info::native_float_format ())
      : count (0), md (arg_md), flt_fmt (ff), fail (false), open_state (true),
      errmsg ()
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

    // Return TRUE if this stream is open.

    bool is_open (void) const { return open_state; }

    virtual void do_close (void) { }

    void close (void)
    {
      if (is_open ())
        {
          open_state = false;
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

    bool ok (void) const { return ! fail; }

    // Return current error message for this stream.

    std::string error (bool clear, int& err_num);

  protected:

    int mode (void) const { return md; }

    mach_info::float_format float_format (void) const { return flt_fmt; }

    // Set current error state and set fail to TRUE.

    void error (const std::string& msg);
    void error (const std::string& who, const std::string& msg);

    // Clear any error message and set fail to FALSE.

    void clear (void);

    // Clear stream state.

    void clearerr (void);

  private:

    // A reference count.
    refcount<octave_idx_type> count;

    // The permission bits for the file.  Should be some combination of
    // std::ios::open_mode bits.
    int md;

    // Data format.
    mach_info::float_format flt_fmt;

    // TRUE if an error has occurred.
    bool fail;

    // TRUE if this stream is open.
    bool open_state;

    // Should contain error message if fail is TRUE.
    std::string errmsg;

    // Functions that are defined for all input streams (input streams
    // are those that define is).

    std::string do_gets (octave_idx_type max_len, bool& err, bool strip_newline,
                         const std::string& who /* = "gets" */);

    std::string getl (octave_idx_type max_len, bool& err,
                      const std::string& who /* = "getl" */);
    std::string gets (octave_idx_type max_len, bool& err,
                      const std::string& who /* = "gets" */);
    off_t skipl (off_t count, bool& err, const std::string& who /* = "skipl" */);

    octave_value do_scanf (scanf_format_list& fmt_list, octave_idx_type nr,
                           octave_idx_type nc,
                           bool one_elt_size_spec, octave_idx_type& count,
                           const std::string& who /* = "scanf" */);

    octave_value scanf (const std::string& fmt, const Array<double>& size,
                        octave_idx_type& count, const std::string& who /* = "scanf" */);

    bool do_oscanf (const scanf_format_elt *elt, octave_value&,
                    const std::string& who /* = "scanf" */);

    octave_value_list oscanf (const std::string& fmt,
                              const std::string& who /* = "scanf" */);

    octave_value do_textscan (const std::string& fmt, octave_idx_type ntimes,
                              const octave_value_list& options,
                              const std::string& who, octave_idx_type& count);

    // Functions that are defined for all output streams (output streams
    // are those that define os).

    int flush (void);

    int do_numeric_printf_conv (std::ostream& os, const printf_format_elt *elt,
                                int nsa, int sa_1, int sa_2,
                                const octave_value& val,
                                const std::string& who);

    int do_printf (printf_format_list& fmt_list, const octave_value_list& args,
                   const std::string& who /* = "printf" */);

    int printf (const std::string& fmt, const octave_value_list& args,
                const std::string& who /* = "printf" */);

    int puts (const std::string& s, const std::string& who /* = "puts" */);

    // We can always do this in terms of seek(), so the derived class
    // only has to provide that.

    void invalid_operation (const std::string& who, const char *rw);
  };

  class
  OCTINTERP_API
  stream
  {
  public:

    stream (base_stream *bs = nullptr);

    ~stream (void);

    stream (const stream&);

    stream& operator = (const stream&);

    int flush (void);

    std::string getl (octave_idx_type max_len, bool& err,
                      const std::string& who /* = "getl" */);
    std::string getl (const octave_value& max_len, bool& err,
                      const std::string& who /* = "getl" */);

    std::string gets (octave_idx_type max_len, bool& err,
                      const std::string& who /* = "gets" */);
    std::string gets (const octave_value& max_len, bool& err,
                      const std::string& who /* = "gets" */);

    off_t skipl (off_t count, bool& err, const std::string& who /* = "skipl" */);
    off_t skipl (const octave_value& count, bool& err,
                 const std::string& who /* = "skipl" */);

    int seek (off_t offset, int origin);
    int seek (const octave_value& offset, const octave_value& origin);

    off_t tell (void);

    int rewind (void);

    bool is_open (void) const;

    void close (void);

    octave_value read (const Array<double>& size, octave_idx_type block_size,
                       oct_data_conv::data_type input_type,
                       oct_data_conv::data_type output_type,
                       octave_idx_type skip, mach_info::float_format flt_fmt,
                       octave_idx_type& count);

    octave_idx_type write (const octave_value& data, octave_idx_type block_size,
                           oct_data_conv::data_type output_type,
                           octave_idx_type skip,
                           mach_info::float_format flt_fmt);

    bool write_bytes (const void *data, size_t n_elts);

    bool skip_bytes (size_t n_elts);

    template <typename T>
      octave_idx_type write (const Array<T>& data, octave_idx_type block_size,
                             oct_data_conv::data_type output_type,
                             octave_idx_type skip,
                             mach_info::float_format flt_fmt);

    octave_value scanf (const std::string& fmt, const Array<double>& size,
                        octave_idx_type& count, const std::string& who /* = "scanf" */);

    octave_value scanf (const octave_value& fmt, const Array<double>& size,
                        octave_idx_type& count, const std::string& who /* = "scanf" */);

    octave_value_list oscanf (const std::string& fmt,
                              const std::string& who /* = "scanf" */);

    octave_value_list oscanf (const octave_value& fmt,
                              const std::string& who /* = "scanf" */);

    octave_value textscan (const std::string& fmt, octave_idx_type ntimes,
                           const octave_value_list& options,
                           const std::string& who, octave_idx_type& count);

    int printf (const std::string& fmt, const octave_value_list& args,
                const std::string& who /* = "printf" */);

    int printf (const octave_value& fmt, const octave_value_list& args,
                const std::string& who /* = "printf" */);

    int puts (const std::string& s, const std::string& who /* = "puts" */);
    int puts (const octave_value& s, const std::string& who /* = "puts" */);

    bool eof (void) const;

    std::string error (bool clear, int& err_num);

    std::string error (bool clear = false)
    {
      int err_num;
      return error (clear, err_num);
    }

    // Set the error message and state.

    void error (const std::string& msg)
    {
      if (rep)
        rep->error (msg);
    }

    void error (const char *msg) { error (std::string (msg)); }

    int file_number (void) { return rep ? rep->file_number () : -1; }

    bool is_valid (void) const { return (rep != nullptr); }

    bool ok (void) const { return rep && rep->ok (); }

    operator bool () const { return ok (); }

    std::string name (void) const;

    int mode (void) const;

    mach_info::float_format float_format (void) const;

    static std::string mode_as_string (int mode);

    std::istream * input_stream (void)
    {
      return rep ? rep->input_stream () : nullptr;
    }

    std::ostream * output_stream (void)
    {
      return rep ? rep->output_stream () : nullptr;
    }

    void clearerr (void) { if (rep) rep->clearerr (); }

  private:

    // The actual representation of this stream.
    base_stream *rep;

    bool stream_ok (bool clear = true) const
    {
      bool retval = true;

      if (rep)
        {
          if (clear)
            rep->clear ();
        }
      else
        retval = false;

      return retval;
    }

    void invalid_operation (const std::string& who, const char *rw)
    {
      if (rep)
        rep->invalid_operation (who, rw);
    }

    octave_value
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

    stream_list (interpreter& interp);

    stream_list (const stream_list&) = delete;
    stream_list& operator = (const stream_list&) = delete;

    ~stream_list (void);

    int insert (stream& os);

    stream lookup (int fid, const std::string& who = "") const;
    stream lookup (const octave_value& fid, const std::string& who = "") const;

    int remove (int fid, const std::string& who = "");
    int remove (const octave_value& fid, const std::string& who = "");

    void clear (bool flush = true);

    string_vector get_info (int fid) const;
    string_vector get_info (const octave_value& fid) const;

    std::string list_open_files (void) const;

    octave_value open_file_numbers (void) const;

    int get_file_number (const octave_value& fid) const;

    octave_value stdin_file (void) const;
    octave_value stdout_file (void) const;
    octave_value stderr_file (void) const;

  private:

    typedef std::map<int, stream> ostrl_map;

    ostrl_map list;

    mutable ostrl_map::const_iterator lookup_cache;

    int m_stdin_file;
    int m_stdout_file;
    int m_stderr_file;
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED (4.4, "use 'octave::base_stream' instead")
typedef octave::base_stream octave_base_stream;

OCTAVE_DEPRECATED (4.4, "use 'octave::stream' instead")
typedef octave::stream octave_stream;

OCTAVE_DEPRECATED (4.4, "use 'octave::stream_list' instead")
typedef octave::stream_list octave_stream_list;

#endif

#endif
