////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2023 The Octave Project Developers
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

#include <cstring>

#include <fstream>
#include <iomanip>
#include <iostream>
#include <list>
#include <sstream>
#include <string>

#include "byte-swap.h"
#include "dMatrix.h"
#include "data-conv.h"
#include "file-ops.h"
#include "file-stat.h"
#include "glob-match.h"
#include "lo-mappers.h"
#include "mach-info.h"
#include "oct-env.h"
#include "oct-locbuf.h"
#include "oct-time.h"
#include "quit.h"
#include "str-vec.h"
#include "strftime-wrapper.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "interpreter.h"
#include "interpreter-private.h"
#include "load-path.h"
#include "load-save.h"
#include "oct-hdf5.h"
#include "ovl.h"
#include "oct-map.h"
#include "ov-cell.h"
#include "pager.h"
#include "syminfo.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include "version.h"

#include "ls-hdf5.h"
#include "ls-mat-ascii.h"
#include "ls-mat4.h"
#include "ls-mat5.h"
#include "ls-oct-text.h"
#include "ls-oct-binary.h"

// Remove gnulib definitions, if any.
#if defined (close)
#  undef close
#endif
#if defined (open)
#  undef open
#endif

#if defined (HAVE_ZLIB)
#  include "gzfstream.h"
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_NORETURN static
void
err_file_open (const std::string& fcn, const std::string& file)
{
  if (fcn == "load")
    error ("%s: unable to open input file '%s'", fcn.c_str (), file.c_str ());
  else if (fcn == "save")
    error ("%s: unable to open output file '%s'", fcn.c_str (), file.c_str ());
  else
    error ("%s: unable to open file '%s'", fcn.c_str (), file.c_str ());
}

// Return TRUE if NAME matches one of the given globbing PATTERNS.

static bool
matches_patterns (const string_vector& patterns, int pat_idx,
                  int num_pat, const std::string& name)
{
  for (int i = pat_idx; i < num_pat; i++)
    {
      glob_match pattern (patterns[i]);

      if (pattern.match (name))
        return true;
    }

  return false;
}

static int
read_binary_file_header (std::istream& is, bool& swap,
                         mach_info::float_format& flt_fmt,
                         bool quiet = false)
{
  const int magic_len = 10;
  char magic[magic_len+1];
  is.read (magic, magic_len);
  magic[magic_len] = '\0';

  if (strncmp (magic, "Octave-1-L", magic_len) == 0)
    swap = mach_info::words_big_endian ();
  else if (strncmp (magic, "Octave-1-B", magic_len) == 0)
    swap = ! mach_info::words_big_endian ();
  else
    {
      if (! quiet)
        error ("load: unable to read binary file");

      return -1;
    }

  char tmp = 0;
  is.read (&tmp, 1);

  flt_fmt = mopt_digit_to_float_format (tmp);

  if (flt_fmt == mach_info::flt_fmt_unknown)
    {
      if (! quiet)
        error ("load: unrecognized binary format!");

      return -1;
    }

  return 0;
}

#if defined (HAVE_ZLIB)
static bool
check_gzip_magic (const std::string& fname)
{
  bool retval = false;

  std::ifstream file = sys::ifstream (fname.c_str (),
                                      std::ios::in | std::ios::binary);

  unsigned char magic[2];
  if (file.read (reinterpret_cast<char *> (&magic[0]), 2)
      && magic[0] == 0x1f && magic[1] == 0x8b)
    retval = true;

  file.close ();

  return retval;
}
#endif

static std::string
find_file_to_load (const std::string& name, const std::string& orig_name)
{
  std::string fname = find_data_file_in_load_path ("load", name, true);

  std::size_t dot_pos = fname.rfind ('.');
  std::size_t sep_pos = fname.find_last_of (sys::file_ops::dir_sep_chars ());

  if (dot_pos == std::string::npos
      || (sep_pos != std::string::npos && dot_pos < sep_pos))
    {
      // Either no '.' in name or no '.' appears after last directory
      // separator.

      sys::file_stat fs (fname);

      if (! (fs.exists () && fs.is_reg ()))
        fname = find_file_to_load (fname + ".mat", orig_name);
    }
  else
    {
      sys::file_stat fs (fname);

      if (! (fs.exists () && fs.is_reg ()))
        {
          fname = "";

          error ("load: unable to find file %s", orig_name.c_str ());
        }
    }

  return fname;
}

// Return TRUE if PATTERN has any special globbing chars in it.

static bool
glob_pattern_p (const std::string& pattern)
{
  int open = 0;

  int len = pattern.length ();

  for (int i = 0; i < len; i++)
    {
      char c = pattern[i];

      switch (c)
        {
        case '?':
        case '*':
          return true;

        case '[':       // Only accept an open brace if there is a close
          open++;       // brace to match it.  Bracket expressions must be
          continue;     // complete, according to Posix.2

        case ']':
          if (open)
            return true;
          continue;

        case '\\':
          if (i == len - 1)
            return false;
          continue;

        default:
          continue;
        }
    }

  return false;
}

load_save_system::load_save_system (interpreter& interp)
  : m_interpreter (interp),
    m_crash_dumps_octave_core (true),
    m_octave_core_file_limit (-1.0),
    m_octave_core_file_name ("octave-workspace"),
    m_save_default_options ("-text"),
    m_octave_core_file_options ("-binary"),
    m_save_header_format_string (init_save_header_format ())
{
#if defined (HAVE_HDF5)
  H5dont_atexit ();
#endif
}

load_save_system::~load_save_system (void)
{
#if defined (HAVE_HDF5)
  H5close ();
#endif
}

octave_value
load_save_system::crash_dumps_octave_core (const octave_value_list& args,
    int nargout)
{
  return set_internal_variable (m_crash_dumps_octave_core, args, nargout,
                                "crash_dumps_octave_core");
}

octave_value
load_save_system::octave_core_file_limit (const octave_value_list& args,
    int nargout)
{
  return set_internal_variable (m_octave_core_file_limit, args, nargout,
                                "octave_core_file_limit");
}

octave_value
load_save_system::octave_core_file_name (const octave_value_list& args,
    int nargout)
{
  return set_internal_variable (m_octave_core_file_name, args, nargout,
                                "octave_core_file_name", false);
}

octave_value
load_save_system::save_default_options (const octave_value_list& args,
                                        int nargout)
{
  return set_internal_variable (m_save_default_options, args, nargout,
                                "save_default_options", false);
}

octave_value
load_save_system::octave_core_file_options (const octave_value_list& args,
    int nargout)
{
  return set_internal_variable (m_octave_core_file_options, args, nargout,
                                "octave_core_file_options", false);
}

octave_value
load_save_system::save_header_format_string (const octave_value_list& args,
    int nargout)
{
  return set_internal_variable (m_save_header_format_string, args, nargout,
                                "save_header_format_string");
}

load_save_format
load_save_system::get_file_format (const std::string& fname,
                                   const std::string& orig_fname,
                                   bool& use_zlib, bool quiet)
{
  load_save_format retval = UNKNOWN;

#if defined (HAVE_HDF5_UTF8)
  std::string ascii_fname = fname;
#else
  std::string ascii_fname = sys::get_ASCII_filename (fname);
#endif

#if defined (HAVE_HDF5)
  // check this before we open the file
  if (H5Fis_hdf5 (ascii_fname.c_str ()) > 0)
    return HDF5;
#endif

#if defined (HAVE_ZLIB)
  use_zlib = check_gzip_magic (fname);
#else
  use_zlib = false;
#endif

  if (! use_zlib)
    {
      std::ifstream file = sys::ifstream (fname.c_str (),
                                          std::ios::in | std::ios::binary);
      if (file)
        {
          retval = get_file_format (file, orig_fname);
          file.close ();
        }
      else if (! quiet)
        err_file_open ("load", orig_fname);
    }
#if defined (HAVE_ZLIB)
  else
    {
      gzifstream gzfile (fname.c_str (), std::ios::in | std::ios::binary);
      if (gzfile)
        {
          retval = get_file_format (gzfile, orig_fname);
          gzfile.close ();
        }
      else if (! quiet)
        err_file_open ("load", orig_fname);
    }
#endif

  return retval;
}

octave_value
load_save_system::load_vars (std::istream& stream,
                             const std::string& orig_fname,
                             const load_save_format& fmt,
                             mach_info::float_format flt_fmt,
                             bool list_only, bool swap, bool verbose,
                             const string_vector& argv, int argv_idx,
                             int argc, int nargout)
{
  octave_value retval;

  octave_scalar_map retstruct;

  std::ostringstream output_buf;
  std::list<std::string> symbol_names;

  octave_idx_type count = 0;

  for (;;)
    {
      bool global = false;
      octave_value tc;

      std::string name;
      std::string doc;

      switch (fmt.type ())
        {
        case TEXT:
          name = read_text_data (stream, orig_fname, global, tc, count);
          break;

        case BINARY:
          name = read_binary_data (stream, swap, flt_fmt, orig_fname,
                                   global, tc, doc);
          break;

        case MAT_ASCII:
          name = read_mat_ascii_data (stream, orig_fname, tc);
          break;

        case MAT_BINARY:
          name = read_mat_binary_data (stream, orig_fname, tc);
          break;

#if defined (HAVE_HDF5)
        case HDF5:
          name = read_hdf5_data (stream, orig_fname, global, tc, doc,
                                 argv, argv_idx, argc);
          break;
#endif

        case MAT5_BINARY:
        case MAT7_BINARY:
          name = read_mat5_binary_element (stream, orig_fname, swap,
                                           global, tc);
          break;

        default:
          err_unrecognized_data_fmt ("load");
          break;
        }

      if (stream.eof () || name.empty ())
        break;
      else
        {
          if (! tc.is_defined ())
            error ("load: unable to load variable '%s'", name.c_str ());

          if (fmt.type () == MAT_ASCII && argv_idx < argc)
            warning ("load: loaded ASCII file '%s' -- ignoring extra args",
                     orig_fname.c_str ());

          if (fmt.type () == MAT_ASCII
              || argv_idx == argc
              || matches_patterns (argv, argv_idx, argc, name))
            {
              count++;
              if (list_only)
                {
                  if (verbose)
                    {
                      if (count == 1)
                        output_buf
                            << "type               rows   cols   name\n"
                            << "====               ====   ====   ====\n";

                      output_buf
                          << std::setiosflags (std::ios::left)
                          << std::setw (16) << tc.type_name ().c_str ()
                          << std::setiosflags (std::ios::right)
                          << std::setw (7) << tc.rows ()
                          << std::setw (7) << tc.columns ()
                          << "   " << name << "\n";
                    }
                  else
                    symbol_names.push_back (name);
                }
              else
                {
                  if (nargout == 1)
                    {
                      if (fmt.type () == MAT_ASCII)
                        retval = tc;
                      else
                        retstruct.assign (name, tc);
                    }
                  else
                    install_loaded_variable (name, tc, global, doc);
                }
            }

          // Only attempt to read one item from a headless text file.

          if (fmt.type () == MAT_ASCII)
            break;
        }
    }

  if (list_only && count)
    {
      if (verbose)
        {
          std::string msg = output_buf.str ();

          if (nargout > 0)
            retval = msg;
          else
            octave_stdout << msg;
        }
      else
        {
          if (nargout  > 0)
            retval = Cell (string_vector (symbol_names));
          else
            {
              string_vector names (symbol_names);

              names.list_in_columns (octave_stdout);

              octave_stdout << "\n";
            }
        }
    }
  else if (retstruct.nfields () != 0)
    retval = retstruct;

  return retval;
}

string_vector
load_save_system::parse_save_options (const string_vector& argv,
                                      load_save_format& fmt, bool& append,
                                      bool& save_as_floats, bool& use_zlib)
{
#if ! defined (HAVE_ZLIB)
  octave_unused_parameter (use_zlib);
#endif

  string_vector retval;
  int argc = argv.numel ();

  bool do_double = false;
  bool do_tabs = false;

  for (int i = 0; i < argc; i++)
    {
      if (argv[i] == "-append")
        {
          append = true;
        }
      else if (argv[i] == "-ascii" || argv[i] == "-a")
        {
          fmt.set_type (MAT_ASCII);
        }
      else if (argv[i] == "-double")
        {
          do_double = true;
        }
      else if (argv[i] == "-tabs")
        {
          do_tabs = true;
        }
      else if (argv[i] == "-text" || argv[i] == "-t")
        {
          fmt.set_type (TEXT);
        }
      else if (argv[i] == "-binary" || argv[i] == "-b")
        {
          fmt.set_type (BINARY);
        }
      else if (argv[i] == "-hdf5" || argv[i] == "-h")
        {
#if defined (HAVE_HDF5)
          fmt.set_type (HDF5);
#else
          err_disabled_feature ("save", "HDF5");
#endif
        }
      else if (argv[i] == "-v7.3" || argv[i] == "-V7.3" || argv[i] == "-7.3")
        {
          error ("save: Matlab file format -v7.3 is not yet implemented");
        }
#if defined (HAVE_ZLIB)
      else if (argv[i] == "-v7" || argv[i] == "-V7" || argv[i] == "-7"
               || argv[i] == "-mat7-binary")
        {
          fmt.set_type (MAT7_BINARY);
        }
#endif
      else if (argv[i] == "-mat" || argv[i] == "-m"
               || argv[i] == "-v6" || argv[i] == "-V6" || argv[i] == "-6"
               || argv[i] == "-mat-binary")
        {
          fmt.set_type (MAT5_BINARY);
        }
      else if (argv[i] == "-v4" || argv[i] == "-V4" || argv[i] == "-4"
               || argv[i] == "-mat4-binary")
        {
          fmt.set_type (MAT_BINARY);
        }
      else if (argv[i] == "-float-binary" || argv[i] == "-f")
        {
          fmt.set_type (BINARY);
          save_as_floats = true;
        }
      else if (argv[i] == "-float-hdf5")
        {
#if defined (HAVE_HDF5)
          fmt.set_type (HDF5);
          save_as_floats = true;
#else
          err_disabled_feature ("save", "HDF5");
#endif
        }
#if defined (HAVE_ZLIB)
      else if (argv[i] == "-zip" || argv[i] == "-z")
        {
          use_zlib = true;
        }
#endif
      else if (argv[i] == "-struct")
        {
          retval.append (argv[i]);
        }
      else if (argv[i][0] == '-' && argv[i] != "-")
        {
          error ("save: Unrecognized option '%s'", argv[i].c_str ());
        }
      else
        retval.append (argv[i]);
    }

  if (do_double)
    {
      if (fmt.type () == MAT_ASCII)
        fmt.set_option (MAT_ASCII_LONG);
      else
        warning (R"(save: "-double" option only has an effect with "-ascii")");
    }

  if (do_tabs)
    {
      if (fmt.type () == MAT_ASCII)
        fmt.set_option (MAT_ASCII_TABS);
      else
        warning (R"(save: "-tabs" option only has an effect with "-ascii")");
    }

  if (append && use_zlib
      && (fmt.type () != TEXT && fmt.type () != MAT_ASCII))
    error ("save: -append and -zip options can only be used together with a text format (-text or -ascii)");

  return retval;
}

string_vector
load_save_system::parse_save_options (const std::string& arg,
                                      load_save_format& fmt,
                                      bool& append, bool& save_as_floats,
                                      bool& use_zlib)
{
  std::istringstream is (arg);
  std::string str;
  string_vector argv;

  while (! is.eof ())
    {
      is >> str;
      argv.append (str);
    }

  return parse_save_options (argv, fmt, append, save_as_floats, use_zlib);
}

void load_save_system::save_vars (const string_vector& argv, int argv_idx,
                                  int argc, std::ostream& os,
                                  const load_save_format& fmt,
                                  bool save_as_floats,
                                  bool write_header_info)
{
  if (write_header_info)
    write_header (os, fmt);

  if (argv_idx == argc)
    {
      save_vars (os, "*", fmt, save_as_floats);
    }
  else if (argv[argv_idx] == "-struct")
    {
      if (++argv_idx >= argc)
        error ("save: missing struct name");

      std::string struct_name = argv[argv_idx];

      if (! m_interpreter.is_variable (struct_name))
        error ("save: no such variable: '%s'", struct_name.c_str ());

      octave_value struct_var = m_interpreter.varval (struct_name);

      if (! struct_var.isstruct () || struct_var.numel () != 1)
        error ("save: '%s' is not a scalar structure", struct_name.c_str ());

      octave_scalar_map struct_var_map = struct_var.scalar_map_value ();

      ++argv_idx;

      if (argv_idx < argc)
        {
          for (int i = argv_idx; i < argc; i++)
            {
              if (! save_fields (os, struct_var_map, argv[i], fmt,
                                 save_as_floats))
                {
                  warning ("save: no such field '%s.%s'",
                           struct_name.c_str (), argv[i].c_str ());
                }
            }
        }
      else
        save_fields (os, struct_var_map, "*", fmt, save_as_floats);
    }
  else
    {
      for (int i = argv_idx; i < argc; i++)
        {
          if (argv[i] == "")
            continue;  // Skip empty vars for Matlab compatibility
          if (! save_vars (os, argv[i], fmt, save_as_floats))
            warning ("save: no such variable '%s'", argv[i].c_str ());
        }
    }
}

void load_save_system::dump_octave_core (void)
{
  if (m_crash_dumps_octave_core)
    {
      // FIXME: should choose better filename?

      const char *fname = m_octave_core_file_name.c_str ();

      message (nullptr, "attempting to save variables to '%s'...", fname);

      load_save_format fmt (BINARY);

      bool save_as_floats = false;

      bool append = false;

      bool use_zlib = false;

      load_save_system::parse_save_options (m_octave_core_file_options,
                                            fmt, append, save_as_floats,
                                            use_zlib);

      std::ios::openmode mode = std::ios::out;

      // Matlab v7 files are always compressed
      if (fmt.type () == MAT7_BINARY)
        use_zlib = false;

      if (fmt.type () == BINARY
#if defined (HAVE_HDF5)
          || fmt.type () == HDF5
#endif
          || fmt.type () == MAT_BINARY
          || fmt.type () == MAT5_BINARY
          || fmt.type () == MAT7_BINARY)
        mode |= std::ios::binary;

      mode |= append ? std::ios::ate : std::ios::trunc;

#if defined (HAVE_HDF5)
      if (fmt.type () == HDF5)
        {
          hdf5_ofstream file (fname, mode);

          if (file.file_id >= 0)
            {
              dump_octave_core (file, fname, fmt, save_as_floats);

              file.close ();
            }
          else
            warning ("dump_octave_core: unable to open '%s' for writing...",
                     fname);
        }
      else
#endif
        // don't insert any commands here!  The open brace below must
        // go with the else above!
        {
#if defined (HAVE_ZLIB)
          if (use_zlib)
            {
              gzofstream file (fname, mode);

              if (file)
                {
                  dump_octave_core (file, fname, fmt, save_as_floats);

                  file.close ();
                }
              else
                warning ("dump_octave_core: unable to open '%s' for writing...",
                         fname);
            }
          else
#endif
            {
              std::ofstream file = sys::ofstream (fname, mode);

              if (file)
                {
                  dump_octave_core (file, fname, fmt, save_as_floats);

                  file.close ();
                }
              else
                warning ("dump_octave_core: unable to open '%s' for writing...",
                         fname);
            }
        }
    }
}

void load_save_system::write_header (std::ostream& os,
                                     const load_save_format& fmt)
{
  switch (fmt.type ())
    {
    case BINARY:
      {
        os << (mach_info::words_big_endian ()
               ? "Octave-1-B" : "Octave-1-L");

        mach_info::float_format flt_fmt = mach_info::native_float_format ();

        char tmp = static_cast<char> (float_format_to_mopt_digit (flt_fmt));

        os.write (&tmp, 1);
      }
      break;

    case MAT5_BINARY:
    case MAT7_BINARY:
      {
        char const *versionmagic;
        char headertext[128];
        sys::gmtime now;

        // ISO 8601 format date
        const char *matlab_format = "MATLAB 5.0 MAT-file, written by Octave "
                                    OCTAVE_VERSION ", %Y-%m-%d %T UTC";
        std::string comment_string = now.strftime (matlab_format);

        std::size_t len = std::min (comment_string.length (),
                                    static_cast<std::size_t> (124));
        memset (headertext, ' ', 124);
        memcpy (headertext, comment_string.data (), len);

        // The first pair of bytes give the version of the MAT file
        // format.  The second pair of bytes form a magic number which
        // signals a MAT file.  MAT file data are always written in
        // native byte order.  The order of the bytes in the second
        // pair indicates whether the file was written by a big- or
        // little-endian machine.  However, the version number is
        // written in the *opposite* byte order from everything else!
        if (mach_info::words_big_endian ())
          versionmagic = "\x01\x00\x4d\x49"; // this machine is big endian
        else
          versionmagic = "\x00\x01\x49\x4d"; // this machine is little endian

        memcpy (headertext+124, versionmagic, 4);
        os.write (headertext, 128);
      }

      break;

#if defined (HAVE_HDF5)
    case HDF5:
#endif
    case TEXT:
      {
        sys::localtime now;

        std::string comment_string = now.strftime (m_save_header_format_string);

        if (! comment_string.empty ())
          {
#if defined (HAVE_HDF5)
            if (fmt.type () == HDF5)
              {
                hdf5_ofstream& hs = dynamic_cast<hdf5_ofstream&> (os);
                H5Gset_comment (hs.file_id, "/", comment_string.c_str ());
              }
            else
#endif
              os << comment_string << "\n";
          }
      }
      break;

    default:
      break;
    }
}

// Save variables with names matching PATTERN on stream OS in the
// format specified by FMT.

std::size_t load_save_system::save_vars (std::ostream& os,
    const std::string& pattern,
    const load_save_format& fmt,
    bool save_as_floats)
{
  tree_evaluator& tw = m_interpreter.get_evaluator ();

  symbol_info_list syminfo_list = tw.glob_symbol_info (pattern);

  std::size_t saved = 0;

  for (const auto& syminfo : syminfo_list)
    {
      do_save (os, syminfo, fmt, save_as_floats);

      saved++;
    }

  return saved;
}

void load_save_system::do_save (std::ostream& os, const octave_value& tc,
                                const std::string& name,
                                const std::string& help,
                                bool global, const load_save_format& fmt,
                                bool save_as_floats)
{
  switch (fmt.type ())
    {
    case TEXT:
      save_text_data (os, tc, name, global, 0);
      break;

    case BINARY:
      save_binary_data (os, tc, name, help, global, save_as_floats);
      break;

    case MAT_ASCII:
      if (! save_mat_ascii_data (os, tc,
                                 fmt.options () & MAT_ASCII_LONG ? 16 : 8,
                                 fmt.options () & MAT_ASCII_TABS))
        warning ("save: unable to save %s in ASCII format", name.c_str ());
      break;

    case MAT_BINARY:
      save_mat_binary_data (os, tc, name);
      break;

#if defined (HAVE_HDF5)
    case HDF5:
      save_hdf5_data (os, tc, name, help, global, save_as_floats);
      break;
#endif

    case MAT5_BINARY:
      save_mat5_binary_element (os, tc, name, global, false, save_as_floats);
      break;

    case MAT7_BINARY:
      save_mat5_binary_element (os, tc, name, global, true, save_as_floats);
      break;

    default:
      err_unrecognized_data_fmt ("save");
      break;
    }
}

// Save the info from SR on stream OS in the format specified by FMT.

void load_save_system::do_save (std::ostream& os,
                                const symbol_info& syminfo,
                                const load_save_format& fmt,
                                bool save_as_floats)
{
  octave_value val = syminfo.value ();

  if (val.is_defined ())
    {
      std::string name = syminfo.name ();
      std::string help;
      bool global = syminfo.is_global ();

      do_save (os, val, name, help, global, fmt, save_as_floats);
    }
}

// save fields of a scalar structure STR matching PATTERN on stream OS
// in the format specified by FMT.

std::size_t load_save_system::save_fields (std::ostream& os,
    const octave_scalar_map& m,
    const std::string& pattern,
    const load_save_format& fmt,
    bool save_as_floats)
{
  glob_match pat (pattern);

  std::size_t saved = 0;

  for (auto it = m.begin (); it != m.end (); it++)
    {
      std::string empty_str;

      if (pat.match (m.key (it)))
        {
          do_save (os, m.contents (it), m.key (it), empty_str,
                   0, fmt, save_as_floats);

          saved++;
        }
    }

  return saved;
}

void load_save_system::dump_octave_core (std::ostream& os,
    const char *fname,
    const load_save_format& fmt,
    bool save_as_floats)
{
  write_header (os, fmt);

  tree_evaluator& tw = m_interpreter.get_evaluator ();

  symbol_info_list syminfo_list = tw.top_scope_symbol_info ();

  double save_mem_size = 0;

  for (const auto& syminfo : syminfo_list)
    {
      octave_value val = syminfo.value ();

      std::string name = syminfo.name ();
      std::string help;
      bool global = syminfo.is_global ();

      double val_size = val.byte_size () / 1024;

      // FIXME: maybe we should try to throw out the largest first...

      if (m_octave_core_file_limit < 0
          || save_mem_size + val_size < m_octave_core_file_limit)
        {
          save_mem_size += val_size;

          do_save (os, val, name, help, global, fmt, save_as_floats);
        }
    }

  message (nullptr, "save to '%s' complete", fname);
}

// Install a variable with name NAME and the value VAL in the
// symbol table.  If GLOBAL is TRUE, make the variable global.

void load_save_system::install_loaded_variable (const std::string& name,
    const octave_value& val,
    bool global,
    const std::string& /*doc*/)
{
  m_interpreter.install_variable (name, val, global);
}

std::string load_save_system::init_save_header_format (void)
{
  return
    (std::string ("# Created by Octave " OCTAVE_VERSION
                  ", %a %b %d %H:%M:%S %Y %Z <")
     + sys::env::get_user_name ()
     + '@'
     + sys::env::get_host_name ()
     + '>');
}

load_save_format
load_save_system::get_file_format (std::istream& file,
                                   const std::string& filename)
{
  load_save_format retval = load_save_system::UNKNOWN;

  mach_info::float_format flt_fmt
    = mach_info::flt_fmt_unknown;

  bool swap = false;

  if (read_binary_file_header (file, swap, flt_fmt, true) == 0)
    retval = BINARY;
  else
    {
      file.clear ();
      file.seekg (0, std::ios::beg);

      int32_t mopt, nr, nc, imag, len;

      int err = read_mat_file_header (file, swap, mopt, nr, nc, imag, len,
                                      true);

      if (! err)
        retval = MAT_BINARY;
      else
        {
          file.clear ();
          file.seekg (0, std::ios::beg);

          err = read_mat5_binary_file_header (file, swap, true, filename);

          if (! err)
            {
              file.clear ();
              file.seekg (0, std::ios::beg);
              retval = MAT5_BINARY;
            }
          else
            {
              file.clear ();
              file.seekg (0, std::ios::beg);

              std::string name_val = extract_keyword (file, "name");
              std::string type_val = extract_keyword (file, "type");

              if (name_val.empty () != true && type_val.empty () != true)
                retval = TEXT;
              else
                {
                  file.clear ();
                  file.seekg (0, std::ios::beg);

                  // FIXME: looks_like_mat_ascii_file does not check
                  // to see whether the file contains numbers.  It
                  // just skips comments and checks for the same
                  // number of words on each line.  We may need a
                  // better check here.  The best way to do that might
                  // be just to try to read the file and see if it
                  // works.

                  if (looks_like_mat_ascii_file (file, filename))
                    retval = MAT_ASCII;
                }
            }
        }
    }

  return retval;
}

octave_value_list
load_save_system::load (const octave_value_list& args, int nargout)
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("load");

  int i = 1;
  std::string orig_fname = "";

  // Function called with Matlab-style ["filename", options] syntax
  if (argc > 1 && ! argv[1].empty () && argv[1].at (0) != '-')
    {
      orig_fname = argv[1];
      i++;
    }

  // It isn't necessary to have the default load format stored in a
  // user preference variable since we can determine the type of file
  // as we are reading.

  load_save_format format = UNKNOWN;

  bool list_only = false;
  bool verbose = false;

  for (; i < argc; i++)
    {
      if (argv[i] == "-text" || argv[i] == "-t")
        {
          format = TEXT;
        }
      else if (argv[i] == "-binary" || argv[i] == "-b")
        {
          format = BINARY;
        }
      else if (argv[i] == "-hdf5" || argv[i] == "-h")
        {
#if defined (HAVE_HDF5)
          format = HDF5;
#else
          err_disabled_feature ("load", "HDF5");
#endif
        }
      else if (argv[i] == "-ascii" || argv[i] == "-a")
        {
          format = MAT_ASCII;
        }
      else if (argv[i] == "-v7.3" || argv[i] == "-V7.3" || argv[i] == "-7.3")
        {
          error ("load: Matlab file format -v7.3 is not yet implemented");
        }
      else if (argv[i] == "-v7" || argv[i] == "-V7" || argv[i] == "-7"
               || argv[i] == "-mat7-binary")
        {
          format = MAT7_BINARY;
        }
      else if (argv[i] == "-mat" || argv[i] == "-m"
               || argv[i] == "-v6" || argv[i] == "-V6" || argv[i] == "-6"
               || argv[i] == "-mat-binary")
        {
          format = MAT5_BINARY;
        }
      else if (argv[i] == "-v4" || argv[i] == "-V4" || argv[i] == "-4"
               || argv[i] == "-mat4-binary")
        {
          format = MAT_BINARY;
        }
      else if (argv[i] == "-force" || argv[i] == "-f")
        {
          // Silently ignore this
          // warning ("load: -force ignored");
        }
      else if (argv[i] == "-import" || argv[i] == "-i")
        {
          warning ("load: -import ignored");
        }
      else if (argv[i] == "-list" || argv[i] == "-l")
        {
          list_only = true;
        }
      else if (argv[i] == "-verbose" || argv[i] == "-v")
        {
          verbose = true;
        }
      else
        break;
    }

  if (orig_fname == "")
    {
      if (i == argc)
        print_usage ();

      orig_fname = argv[i];
    }
  else
    i--;

  mach_info::float_format flt_fmt = mach_info::flt_fmt_unknown;

  bool swap = false;

  if (orig_fname == "-")
    {
      i++;

#if defined (HAVE_HDF5)
      if (format.type () == HDF5)
        error ("load: cannot read HDF5 format from stdin");
      else
#endif
        if (format.type () != UNKNOWN)
          {
            // FIXME: if we have already seen EOF on a previous call,
            // how do we fix up the state of std::cin so that we can get
            // additional input?  I'm afraid that we can't fix this
            // using std::cin only.

            retval = load_vars (std::cin, orig_fname, format, flt_fmt,
                                list_only, swap, verbose, argv, i,
                                argc, nargout);
          }
        else
          error ("load: must specify file format if reading from stdin");
    }
  else
    {
      std::string fname = sys::file_ops::tilde_expand (orig_fname);

      fname = find_file_to_load (fname, orig_fname);

      bool use_zlib = false;

      if (format.type () == UNKNOWN)
        format = get_file_format (fname, orig_fname, use_zlib);

#if defined (HAVE_HDF5)
      if (format.type () == HDF5)
        {
          i++;

          hdf5_ifstream hdf5_file (fname.c_str ());

          if (hdf5_file.file_id < 0)
            err_file_open ("load", orig_fname);

          retval = load_vars (hdf5_file, orig_fname, format, flt_fmt,
                              list_only, swap, verbose, argv, i,
                              argc, nargout);

          hdf5_file.close ();
        }
      else
#endif
        // don't insert any statements here; the "else" above has to
        // go with the "if" below!!!!!
        if (format.type () != UNKNOWN)
          {
            i++;

            // Always open in binary mode and handle various
            // line-endings explicitly.
            std::ios::openmode mode = std::ios::in | std::ios::binary;

#if defined (HAVE_ZLIB)
            if (use_zlib)
              {
                gzifstream file (fname.c_str (), mode);

                if (! file)
                  err_file_open ("load", orig_fname);

                if (format.type () == BINARY)
                  {
                    if (read_binary_file_header (file, swap, flt_fmt) < 0)
                      {
                        if (file) file.close ();
                        return retval;
                      }
                  }
                else if (format.type () == MAT5_BINARY
                         || format.type () == MAT7_BINARY)
                  {
                    if (read_mat5_binary_file_header (file, swap, false,
                                                      orig_fname) < 0)
                      {
                        if (file) file.close ();
                        return retval;
                      }
                  }

                retval = load_vars (file, orig_fname, format, flt_fmt,
                                    list_only, swap, verbose, argv, i,
                                    argc, nargout);

                file.close ();
              }
            else
#endif
              {
                std::ifstream file = sys::ifstream (fname.c_str (), mode);

                if (! file)
                  error ("load: unable to open input file '%s'",
                         orig_fname.c_str ());

                if (format.type () == BINARY)
                  {
                    if (read_binary_file_header (file, swap, flt_fmt) < 0)
                      {
                        if (file) file.close ();
                        return retval;
                      }
                  }
                else if (format.type () == MAT5_BINARY
                         || format.type () == MAT7_BINARY)
                  {
                    if (read_mat5_binary_file_header (file, swap, false,
                                                      orig_fname) < 0)
                      {
                        if (file) file.close ();
                        return retval;
                      }
                  }

                retval = load_vars (file, orig_fname, format, flt_fmt,
                                    list_only, swap, verbose, argv, i,
                                    argc, nargout);

                file.close ();
              }
          }
        else
          error ("load: unable to determine file format of '%s'",
                 orig_fname.c_str ());

    }

  return retval;
}

octave_value_list
load_save_system::save (const octave_value_list& args, int nargout)
{
  // Here is where we would get the default save format if it were
  // stored in a user preference variable.
  load_save_format format = TEXT;
  bool save_as_floats = false;
  bool append = false;
  bool use_zlib = false;


  // get default options
  parse_save_options (save_default_options (), format, append,
                      save_as_floats, use_zlib);

  // override from command line
  string_vector argv = args.make_argv ();

  argv = parse_save_options (argv, format, append, save_as_floats, use_zlib);

  int argc = argv.numel ();
  int i = 0;

  if (i == argc)
    print_usage ();

  if (save_as_floats && format.type () == TEXT)
    error ("save: cannot specify both -text and -float-binary");

  octave_value_list retval;

  if (argv[i] == "-")
    {
      i++;

#if defined (HAVE_HDF5)
      if (format.type () == HDF5)
        error ("save: cannot write HDF5 format to stdout");
      else
#endif
        // don't insert any commands here!  the brace below must go
        // with the "else" above!
        {
          if (append)
            warning ("save: ignoring -append option for output to stdout");

          if (nargout == 0)
            save_vars (argv, i, argc, octave_stdout, format,
                       save_as_floats, true);
          else
            {
              std::ostringstream output_buf;
              save_vars (argv, i, argc, output_buf, format,
                         save_as_floats, true);
              retval = octave_value (output_buf.str());
            }
        }
    }

  // Guard against things like 'save a*', which are probably mistakes...

  else if (i == argc - 1 && glob_pattern_p (argv[i]))
    print_usage ();
  else
    {
      std::string fname = sys::file_ops::tilde_expand (argv[i]);

      i++;

      // Matlab v7 files are always compressed
      if (format.type () == MAT7_BINARY)
        use_zlib = false;

      std::ios::openmode mode
        = (append ? (std::ios::app | std::ios::ate) : std::ios::out);

      // Always open in binary mode to save line endings as is.
      mode |= std::ios::binary;

#if defined (HAVE_HDF5)
      if (format.type () == HDF5)
        {
          // FIXME: It should be possible to append to HDF5 files.
          if (append)
            error ("save: appending to HDF5 files is not implemented");

#  if defined (HAVE_HDF5_UTF8)
          bool write_header_info
            = ! (append && H5Fis_hdf5 (fname.c_str ()) > 0);
#  else
          std::string ascii_fname = sys::get_ASCII_filename (fname);

          bool write_header_info
            = ! (append && H5Fis_hdf5 (ascii_fname.c_str ()) > 0);
#  endif

          hdf5_ofstream hdf5_file (fname.c_str (), mode);

          if (hdf5_file.file_id == -1)
            err_file_open ("save", fname);

          save_vars (argv, i, argc, hdf5_file, format, save_as_floats,
                     write_header_info);

          hdf5_file.close ();
        }
      else
#endif
        // don't insert any statements here!  The brace below must go
        // with the "else" above!
        {
#if defined (HAVE_ZLIB)
          if (use_zlib)
            {
              gzofstream file (fname.c_str (), mode);

              if (! file)
                err_file_open ("save", fname);

              bool write_header_info = ! file.tellp ();

              save_vars (argv, i, argc, file, format, save_as_floats,
                         write_header_info);

              file.close ();
            }
          else
#endif
            {
              std::ofstream file = sys::ofstream (fname.c_str (), mode);

              if (! file)
                err_file_open ("save", fname);

              bool write_header_info = ! file.tellp ();

              save_vars (argv, i, argc, file, format, save_as_floats,
                         write_header_info);

              file.close ();
            }
        }
    }

  return retval;
}

DEFMETHOD (load, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} load file
@deftypefnx {} {} load options file
@deftypefnx {} {} load options file v1 v2 @dots{}
@deftypefnx {} {S =} load ("options", "file", "v1", "v2", @dots{})
@deftypefnx {} {} load file options
@deftypefnx {} {} load file options v1 v2 @dots{}
@deftypefnx {} {S =} load ("file", "options", "v1", "v2", @dots{})
Load the named variables @var{v1}, @var{v2}, @dots{}, from the file
@var{file}.

If no variables are specified then all variables found in the
file will be loaded.  As with @code{save}, the list of variables to extract
can be full names or use a pattern syntax.  The format of the file is
automatically detected but may be overridden by supplying the appropriate
option.

If load is invoked using the functional form

@example
load ("-option1", @dots{}, "file", "v1", @dots{})
@end example

@noindent
then the @var{options}, @var{file}, and variable name arguments
(@var{v1}, @dots{}) must be specified as character strings.

If a variable that is not marked as global is loaded from a file when a
global symbol with the same name already exists, it is loaded in the
global symbol table.  Also, if a variable is marked as global in a file
and a local symbol exists, the local symbol is moved to the global
symbol table and given the value from the file.

If invoked with a single output argument, Octave returns data instead
of inserting variables in the symbol table.  If the data file contains
only numbers (TAB- or space-delimited columns), a matrix of values is
returned.  Otherwise, @code{load} returns a structure with members
 corresponding to the names of the variables in the file.

The @code{load} command can read data stored in Octave's text and
binary formats, and @sc{matlab}'s binary format.  If compiled with zlib
support, it can also load gzip-compressed files.  It will automatically
detect the type of file and do conversion from different floating point
formats (currently only IEEE big and little endian, though other formats
may be added in the future).

Valid options for @code{load} are listed in the following table.

@table @code
@item -force
This option is accepted for backward compatibility but is ignored.
Octave now overwrites variables currently in memory with
those of the same name found in the file.

@item -ascii
Force Octave to assume the file contains columns of numbers in text format
without any header or other information.  Data in the file will be loaded
as a single numeric matrix with the name of the variable derived from the
name of the file.

@item -binary
Force Octave to assume the file is in Octave's binary format.

@item -hdf5
Force Octave to assume the file is in @sc{hdf5} format.
(@sc{hdf5} is a free, portable binary format developed by the National
Center for Supercomputing Applications at the University of Illinois.)
Note that Octave can only read @sc{hdf5} files that were created by itself with
@code{save}.  This format is only available if Octave was built with a link to
the @sc{hdf5} libraries.

@item -import
This option is accepted for backward compatibility but is ignored.
Octave can now support multi-dimensional HDF data and automatically
modifies variable names if they are invalid Octave identifiers.

@item -text
Force Octave to assume the file is in Octave's text format.

@item  -v7.3
@itemx -V7.3
@itemx -7.3
Octave does @strong{not} yet implement @sc{matlab}'s v7.3 binary data format.

@item  -v7
@itemx -V7
@itemx -7
@itemx -mat7-binary
Force Octave to assume the file is in @sc{matlab}'s version 7 binary format.

@item  -v6
@itemx -V6
@itemx -6
@itemx -mat
@itemx -mat-binary
Force Octave to assume the file is in @sc{matlab}'s version 6 binary format.

@item  -v4
@itemx -V4
@itemx -4
@itemx -mat4-binary
Force Octave to assume the file is in @sc{matlab}'s version 4 binary format.

@end table
@seealso{save, dlmwrite, csvwrite, fwrite}
@end deftypefn */)
{
  load_save_system& load_save_sys = interp.get_load_save_system ();

  return load_save_sys.load (args, nargout);
}

DEFMETHOD (save, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} save file
@deftypefnx {} {} save options file
@deftypefnx {} {} save options file @var{v1} @var{v2} @dots{}
@deftypefnx {} {} save options file -struct @var{STRUCT}
@deftypefnx {} {} save options file -struct @var{STRUCT} @var{f1} @var{f2} @dots{}
@deftypefnx {} {} save - @var{v1} @var{v2} @dots{}
@deftypefnx {} {@var{str} =} save ("-", @qcode{"@var{v1}"}, @qcode{"@var{v2}"}, @dots{})
Save the named variables @var{v1}, @var{v2}, @dots{}, in the file @var{file}.

The special filename @samp{-} may be used to return the content of the
variables as a string.  If no variable names are listed, Octave saves all the
variables in the current scope.  Otherwise, full variable names or pattern
syntax can be used to specify the variables to save.  If the @option{-struct}
modifier is used then the fields of the @strong{scalar} struct are saved as if
they were variables with the corresponding field names.  The @option{-struct}
option can be combined with specific field names @var{f1}, @var{f2}, @dots{} to
write only certain fields to the file.

Valid options for the @code{save} command are listed in the following table.
Options that modify the output format override the format specified by
@code{save_default_options}.

If save is invoked using the functional form

@example
save ("-option1", @dots{}, "file", "v1", @dots{})
@end example

@noindent
then the @var{options}, @var{file}, and variable name arguments (@var{v1},
@dots{}) must be specified as character strings.

If called with a filename of @qcode{"-"}, write the output to stdout if nargout
is 0, otherwise return the output in a character string.

@table @code
@item -append
Append to the destination instead of overwriting.

@item -ascii
Save a matrix in a text file without a header or any other information.  The
matrix must be 2-D and only the real part of any complex value is written to
the file.  Numbers are stored in single-precision format and separated by
spaces.  Additional options for the @option{-ascii} format are

@table @code
@item -double
Store numbers in double-precision format.

@item -tabs
Separate numbers with tabs.
@end table

@item -binary
Save the data in Octave's binary data format.

@item -float-binary
Save the data in Octave's binary data format but using only single precision.
Use this format @strong{only} if you know that all the values to be saved can
be represented in single precision.

@item -hdf5
Save the data in @sc{hdf5} format.
(HDF5 is a free, portable, binary format developed by the National Center for
Supercomputing Applications at the University of Illinois.) This format is only
available if Octave was built with a link to the @sc{hdf5} libraries.

@item -float-hdf5
Save the data in @sc{hdf5} format but using only single precision.  Use this
format @strong{only} if you know that all the values to be saved can be
represented in single precision.

@item -text
Save the data in Octave's text data format.  (default)

@item  -v7.3
@itemx -V7.3
@itemx -7.3
Octave does @strong{not} yet implement @sc{matlab}'s v7.3 binary data format.

@item  -v7
@itemx -V7
@itemx -7
@itemx -mat7-binary
Save the data in @sc{matlab}'s v7 binary data format.

@item  -v6
@itemx -V6
@itemx -6
@itemx -mat
@itemx -mat-binary
Save the data in @sc{matlab}'s v6 binary data format.

@item  -v4
@itemx -V4
@itemx -4
@itemx -mat4-binary
Save the data in @sc{matlab}'s v4 binary data format.

@item  -zip
@itemx -z
Use the gzip algorithm to compress the file.  This works on files that are
compressed with gzip outside of Octave, and gzip can also be used to convert
the files for backward compatibility.  This option is only available if Octave
was built with a link to the zlib libraries.
@end table

The list of variables to save may use wildcard patterns (glob patterns)
containing the following special characters:

@table @code
@item ?
Match any single character.

@item *
Match zero or more characters.

@item [ @var{list} ]
Match the list of characters specified by @var{list}.  If the first character
is @code{!} or @code{^}, match all characters except those specified by
@var{list}.  For example, the pattern @code{[a-zA-Z]} will match all lower and
uppercase alphabetic characters.

Wildcards may also be used in the field name specifications when using the
@option{-struct} modifier (but not in the struct name itself).

@end table

Except when using the @sc{matlab} binary data file format or the @samp{-ascii}
format, saving global variables also saves the global status of the variable.
If the variable is restored at a later time using @samp{load}, it will be
restored as a global variable.

Example:

The command

@example
save -binary data a b*
@end example

@noindent
saves the variable @samp{a} and all variables beginning with @samp{b} to the
file @file{data} in Octave's binary format.
@seealso{load, save_default_options, save_header_format_string, save_precision,
dlmread, csvread, fread}
@end deftypefn */)
{
  load_save_system& load_save_sys = interp.get_load_save_system ();

  return load_save_sys.save (args, nargout);
}

/*
## Save and load strings with "-v6"
%!test
%! A = A2 = "foobar";  # normal string
%! B = B2 = "a";  # short string
%! C = C2 = ["foo"; "bar"];  # character matrix
%! D = D2 = "ab".';  # short character matrix
%! E = E2 = {"foo", "bar"};  # cell string
%! F = F2 = {"Saint Barthélemy", "Saint Kitts and Nevis"};  % non-ASCII
%! mat_file = [tempname(), ".mat"];
%! unwind_protect
%!   save (mat_file, "A", "B", "C", "D", "E", "F", "-v6");
%!   clear ("A", "B", "C", "D", "E", "F");
%!   load (mat_file);
%! unwind_protect_cleanup
%!   unlink (mat_file);
%! end_unwind_protect
%! assert (A, A2);
%! assert (B, B2);
%! assert (C, C2);
%! assert (D, D2);
%! assert (E, E2);
%! assert (F, F2);

## Save and load strings with "-v7"
%!testif HAVE_ZLIB
%! A = A2 = "foobar";  # normal string
%! B = B2 = "a";  # short string
%! C = C2 = ["foo"; "bar"];  # character matrix
%! D = D2 = "ab".';  # short character matrix
%! E = E2 = {"foo", "bar"};  # cell string
%! F = F2 = {"Saint Barthélemy", "Saint Kitts and Nevis"};  # non-ASCII
%! mat_file = [tempname(), ".mat"];
%! unwind_protect
%!   save (mat_file, "A", "B", "C", "D", "E", "F", "-v7");
%!   clear ("A", "B", "C", "D", "E", "F");
%!   load (mat_file);
%! unwind_protect_cleanup
%!   unlink (mat_file);
%! end_unwind_protect
%! assert (A, A2);
%! assert (B, B2);
%! assert (C, C2);
%! assert (D, D2);
%! assert (E, E2);
%! assert (F, F2);

## Save and load struct with "-v6"
%!test
%! struc.a = "foobar";  # normal string
%! struc.b = "a";  # short string
%! struc.c = ["foo"; "bar"];  # character matrix
%! struc.d = "ab".';  # short character matrix
%! struc.e = {"foo", "bar"};  # cell string
%! struc.f = {"Saint Barthélemy", "Saint Kitts and Nevis"};  # non-ASCII
%! struc.g = [1 2 3];  # double vector
%! struc.h = 1:5;  # range
%! struc2 = struc;
%! mat_file = [tempname(), ".mat"];
%! unwind_protect
%!   save (mat_file, "struc", "-v6");
%!   clear ("struc");
%!   load (mat_file);
%! unwind_protect_cleanup
%!   unlink (mat_file);
%! end_unwind_protect
%! assert (struc, struc2);

## Save and load struct with "-v7"
%!testif HAVE_ZLIB
%! struc.a = "foobar";  # normal string
%! struc.b = "a";  # short string
%! struc.c = ["foo"; "bar"];  # character matrix
%! struc.d = "ab".';  # short character matrix
%! struc.e = {"foo", "bar"};  # cell string
%! struc.f = {"Saint Barthélemy", "Saint Kitts and Nevis"};  # non-ASCII
%! struc.g = [1 2 3];  # double vector
%! struc.h = 1:5;  # range
%! struc2 = struc;
%! mat_file = [tempname(), ".mat"];
%! unwind_protect
%!   save (mat_file, "struc", "-v7");
%!   clear ("struc");
%!   load (mat_file);
%! unwind_protect_cleanup
%!   unlink (mat_file);
%! end_unwind_protect
%! assert (struc, struc2);

## Test input validation
%!testif HAVE_ZLIB <*59225>
%! fname = tempname ();
%! x = 1;
%! fail ('save ("-append", "-zip", "-binary", fname, "x")',
%!       "-append and -zip options .* with a text format");
*/

DEFMETHOD (crash_dumps_octave_core, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} crash_dumps_octave_core ()
@deftypefnx {} {@var{old_val} =} crash_dumps_octave_core (@var{new_val})
@deftypefnx {} {@var{old_val} =} crash_dumps_octave_core (@var{new_val}, "local")
Query or set the internal variable that controls whether Octave tries
to save all current variables to the file @file{octave-workspace} if it
crashes or receives a hangup, terminate or similar signal.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{octave_core_file_limit, octave_core_file_name,
octave_core_file_options}
@end deftypefn */)
{
  load_save_system& load_save_sys = interp.get_load_save_system ();

  return load_save_sys.crash_dumps_octave_core (args, nargout);
}

DEFMETHOD (save_default_options, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} save_default_options ()
@deftypefnx {} {@var{old_val} =} save_default_options (@var{new_val})
@deftypefnx {} {@var{old_val} =} save_default_options (@var{new_val}, "local")
Query or set the internal variable that specifies the default options
for the @code{save} command, and defines the default format.

The default value is @qcode{"-text"} (Octave's own text-based file format).
See the documentation of the @code{save} command for other choices.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{save, save_header_format_string, save_precision}
@end deftypefn */)
{
  load_save_system& load_save_sys = interp.get_load_save_system ();

  return load_save_sys.save_default_options (args, nargout);
}

DEFMETHOD (octave_core_file_limit, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} octave_core_file_limit ()
@deftypefnx {} {@var{old_val} =} octave_core_file_limit (@var{new_val})
@deftypefnx {} {@var{old_val} =} octave_core_file_limit (@var{new_val}, "local")
Query or set the internal variable that specifies the maximum amount of memory
that Octave will save when writing a crash dump file.

The limit is measured in kilobytes and is applied to the top-level workspace.
The name of the crash dump file is specified by
@var{octave_core_file_name}.

If @var{octave_core_file_options} flags specify a binary format, then
@var{octave_core_file_limit} will be approximately the maximum size of the
file.  If a text file format is used, then the file could be much larger than
the limit.  The default value is -1 (unlimited).

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{crash_dumps_octave_core, octave_core_file_name,
octave_core_file_options}
@end deftypefn */)
{
  load_save_system& load_save_sys = interp.get_load_save_system ();

  return load_save_sys.octave_core_file_limit (args, nargout);
}

DEFMETHOD (octave_core_file_name, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} octave_core_file_name ()
@deftypefnx {} {@var{old_val} =} octave_core_file_name (@var{new_val})
@deftypefnx {} {@var{old_val} =} octave_core_file_name (@var{new_val}, "local")
Query or set the internal variable that specifies the name of the file
used for saving data from the top-level workspace if Octave aborts.

The default value is @qcode{"octave-workspace"}

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{crash_dumps_octave_core, octave_core_file_name,
octave_core_file_options}
@end deftypefn */)
{
  load_save_system& load_save_sys = interp.get_load_save_system ();

  return load_save_sys.octave_core_file_name (args, nargout);
}

DEFMETHOD (octave_core_file_options, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} octave_core_file_options ()
@deftypefnx {} {@var{old_val} =} octave_core_file_options (@var{new_val})
@deftypefnx {} {@var{old_val} =} octave_core_file_options (@var{new_val}, "local")
Query or set the internal variable that specifies the options used for
saving the workspace data if Octave aborts.

The value of @code{octave_core_file_options} should follow the same format
as the options for the @code{save} function.  The default value is Octave's
binary format.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{crash_dumps_octave_core, octave_core_file_name, octave_core_file_limit}
@end deftypefn */)
{
  load_save_system& load_save_sys = interp.get_load_save_system ();

  return load_save_sys.octave_core_file_options (args, nargout);
}

DEFMETHOD (save_header_format_string, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} save_header_format_string ()
@deftypefnx {} {@var{old_val} =} save_header_format_string (@var{new_val})
@deftypefnx {} {@var{old_val} =} save_header_format_string (@var{new_val}, "local")
Query or set the internal variable that specifies the format string used for
the comment line written at the beginning of text-format data files saved by
Octave.

The format string is passed to @code{strftime} and must begin with the
character @samp{#} and contain no newline characters.  If the value of
@code{save_header_format_string} is the empty string, the header comment is
omitted from text-format data files.  The default value is
@c Set example in small font to prevent overfull line

@smallexample
"# Created by Octave VERSION, %a %b %d %H:%M:%S %Y %Z <USER@@HOST>"
@end smallexample

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{strftime, save_default_options}
@end deftypefn */)
{
  load_save_system& load_save_sys = interp.get_load_save_system ();

  return load_save_sys.save_header_format_string (args, nargout);
}

OCTAVE_END_NAMESPACE(octave)

// DEPRECATED in Octave 7

void
dump_octave_core (void)
{
  octave::load_save_system& load_save_sys = octave::__get_load_save_system__ ();

  load_save_sys.dump_octave_core ();
}
