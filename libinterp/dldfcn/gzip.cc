////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
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

//! @file gzip.cc
//! Octave interface to the compression and uncompression libraries.
//!
//! This was originally implemented as an m file which directly called
//! bzip2 and gzip applications.  This may look simpler but causes some
//! issues (see bug #43431) because we have no control over the output
//! file:
//!
//!   - created file is always in the same directory as the original file;
//!   - automatically skip files that already have gz/bz2/etc extension;
//!   - some older versions lack the --keep option.
//!
//! In addition, because system() does not have a method that allows
//! passing a list of arguments, there is the issue of having to escape
//! filenames.
//!
//! A solution is to pipe file contents into the applications instead of
//! filenames.  However, that solution causes:
//!
//!   # missing file header with original file information;
//!   # implementing ourselves the recursive transversion of directories;
//!   # do the above in a m file which will be slow;
//!   # popen2 is frail on windows.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdio>
#include <cstring>

#include <functional>
#include <list>
#include <stdexcept>
#include <string>

#include "Array.h"
#include "dir-ops.h"
#include "file-ops.h"
#include "file-stat.h"
#include "glob-match.h"
#include "lo-sysdep.h"
#include "oct-env.h"
#include "str-vec.h"

#include "Cell.h"
#include "defun-dld.h"
#include "defun-int.h"
#include "errwarn.h"
#include "ov.h"
#include "ovl.h"

#if defined (HAVE_BZLIB_H)
#  include <bzlib.h>
#endif

#if defined (HAVE_ZLIB_H)
#  include <zlib.h>
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

//! RIIA wrapper for std::FILE*.
//!
//! If error handling is available for failing to close the file, use
//! the close method which throws.
//!
//! If the file has been closed, fp is set to nullptr.  Remember that
//! behavior is undefined if the value of the pointer stream is used
//! after fclose.

class CFile
{
public:

  CFile (void) = delete;

  CFile (const std::string& path, const std::string& mode)
    : m_fp (sys::fopen (path, mode))
  {
    if (! m_fp)
      throw std::runtime_error ("unable to open file");
  }

  CFile (const CFile&) = delete;

  CFile& operator = (const CFile&) = delete;

  ~CFile (void)
  {
    if (m_fp)
      std::fclose (m_fp);
  }

  void close (void)
  {
    if (std::fclose (m_fp))
      throw std::runtime_error ("unable to close file");

    m_fp = nullptr;
  }

  std::FILE *m_fp;
};

#if defined (HAVE_BZ2)

class bz2
{
public:

  static const constexpr char *extension = ".bz2";

  static void zip (const std::string& source_path,
                   const std::string& dest_path)
  {
    bz2::zipper z (source_path, dest_path);
    z.deflate ();
    z.close ();
  }

private:

  class zipper
  {
  public:

    zipper (void) = delete;

    zipper (const std::string& source_path, const std::string& dest_path)
      : m_status (BZ_OK), m_source (source_path, "rb"),
        m_dest (dest_path, "wb"),
        m_bz (BZ2_bzWriteOpen (&m_status, m_dest.m_fp, 9, 0, 30))
    {
      if (m_status != BZ_OK)
        throw std::runtime_error ("failed to open bzip2 stream");
    }

    zipper (const zipper&) = delete;

    zipper& operator = (const zipper&) = delete;

    ~zipper (void)
    {
      if (m_bz != nullptr)
        BZ2_bzWriteClose (&m_status, m_bz, 1, nullptr, nullptr);
    }

    void deflate (void)
    {
      const std::size_t buf_len = 8192;
      char buf[buf_len];
      std::size_t n_read;
      while ((n_read = std::fread (buf, sizeof (buf[0]), buf_len, m_source.m_fp)) != 0)
        {
          if (std::ferror (m_source.m_fp))
            throw std::runtime_error ("failed to read from source file");
          BZ2_bzWrite (&m_status, m_bz, buf, n_read);
          if (m_status == BZ_IO_ERROR)
            throw std::runtime_error ("failed to write or compress");
        }
      if (std::ferror (m_source.m_fp))
        throw std::runtime_error ("failed to read from source file");
    }

    void close (void)
    {
      int abandon = (m_status == BZ_IO_ERROR) ? 1 : 0;
      BZ2_bzWriteClose (&m_status, m_bz, abandon, nullptr, nullptr);
      if (m_status != BZ_OK)
        throw std::runtime_error ("failed to close bzip2 stream");
      m_bz = nullptr;

      // We have no error handling for failing to close source, let
      // the destructor close it.
      m_dest.close ();
    }

  private:

    int m_status;
    CFile m_source;
    CFile m_dest;
    BZFILE *m_bz;
  };
};

#endif

// Note about zlib and gzip
//
// gzip is a format for compressed single files.  zlib is a format
// designed for in-memory and communication channel applications.
// gzip uses the same format internally for the compressed data but
// has different headers and trailers.
//
// zlib is also a library but gzip is not.  Very old versions of zlib do
// not include functions to create useful gzip headers and trailers:
//
//      Note that you cannot specify special gzip header contents (e.g.
//      a file name or modification date), nor will inflate tell you what
//      was in the gzip header.  If you need to customize the header or
//      see what's in it, you can use the raw deflate and inflate
//      operations and the crc32() function and roll your own gzip
//      encoding and decoding.  Read the gzip RFC 1952 for details of the
//      header and trailer format.
//                                                          zlib FAQ
//
// Recent versions (on which we are already dependent) have deflateInit2()
// to do it.  We still need to get the right metadata for the header
// ourselves though.
//
// The header is defined in RFC #1952
// GZIP file format specification version 4.3


#if defined (HAVE_Z)

class gz
{
public:

  static const constexpr char *extension = ".gz";

  static void zip (const std::string& source_path,
                   const std::string& dest_path)
  {
    gz::zipper z (source_path, dest_path);
    z.deflate ();
    z.close ();
  }

private:

  // Util class to get a non-const char*
  class uchar_array
  {
  public:

    // Bytef is a typedef for unsigned char
    unsigned char *p;

    uchar_array (void) = delete;

    uchar_array (const std::string& str)
    {
      p = new Bytef[str.length () + 1];
      std::strcpy (reinterpret_cast<char *> (p), str.c_str ());
    }

    uchar_array (const uchar_array&) = delete;

    uchar_array& operator = (const uchar_array&) = delete;

    ~uchar_array (void) { delete[] p; }
  };

  class gzip_header : public gz_header
  {
  public:

    gzip_header (void) = delete;

    gzip_header (const std::string& source_path)
      : m_basename (sys::env::base_pathname (source_path))
    {
      const sys::file_stat source_stat (source_path);
      if (! source_stat)
        throw std::runtime_error ("unable to stat source file");

      // time_t may be a signed int in which case it will be a
      // positive number so it is safe to uLong.  Or is it?  Can
      // unix_time really never be negative?
      time = uLong (source_stat.mtime ().unix_time ());

      //  If FNAME is set, an original file name is present,
      //  terminated by a zero byte.  The name must consist of ISO
      //  8859-1 (LATIN-1) characters; on operating systems using
      //  EBCDIC or any other character set for file names, the name
      //  must be translated to the ISO LATIN-1 character set.  This
      //  is the original name of the file being compressed, with any
      //  directory components removed, and, if the file being
      //  compressed is on a file system with case insensitive names,
      //  forced to lower case.
      name = m_basename.p;

      // If we don't set it to Z_NULL, then it will set FCOMMENT (4th bit)
      // on the FLG byte, and then write {0, 3} comment.
      comment = Z_NULL;

      // Seems to already be the default but we are not taking chances.
      extra = Z_NULL;

      // We do not want a CRC for the header.  That would be only 2 more
      // bytes, and maybe it would be a good thing but we want to generate
      // gz files similar to the default gzip application.
      hcrc = 0;

      // OS (Operating System):
      //      0 - FAT filesystem (MS-DOS, OS/2, NT/Win32)
      //      1 - Amiga
      //      2 - VMS (or OpenVMS)
      //      3 - Unix
      //      4 - VM/CMS
      //      5 - Atari TOS
      //      6 - HPFS filesystem (OS/2, NT)
      //      7 - Macintosh
      //      8 - Z-System
      //      9 - CP/M
      //     10 - TOPS-20
      //     11 - NTFS filesystem (NT)
      //     12 - QDOS
      //     13 - Acorn RISCOS
      //    255 - unknown
      //
      // The list is problematic because it mixes OS and filesystem.  It
      // also does not specify whether filesystem relates to source or
      // destination file.

#if defined (__WIN32__)
      // Or should it be 11?
      os = 0;
#elif defined (__APPLE__)
      os = 7;
#else
      // Unix by default?
      os = 3;
#endif
    }

    gzip_header (const gzip_header&) = delete;

    gzip_header& operator = (const gzip_header&) = delete;

    ~gzip_header (void) = default;

  private:

    // This must be kept for gz_header.name
    uchar_array m_basename;
  };

  class zipper
  {
  public:

    zipper (void) = delete;

    zipper (const std::string& source_path, const std::string& dest_path)
      : m_source (source_path, "rb"), m_dest (dest_path, "wb"),
        m_header (source_path), m_strm (new z_stream)
    {
      m_strm->zalloc = Z_NULL;
      m_strm->zfree = Z_NULL;
      m_strm->opaque = Z_NULL;
    }

    zipper (const zipper&) = delete;

    zipper& operator = (const zipper&) = delete;

    ~zipper (void)
    {
      if (m_strm)
        deflateEnd (m_strm);
      delete m_strm;
    }

    void deflate (void)
    {
      // int deflateInit2 (z_streamp m_strm,
      //                   int  level,      // compression level (default is 8)
      //                   int  method,
      //                   int  windowBits, // 15 (default) + 16 (gzip format)
      //                   int  memLevel,   // memory usage (default is 8)
      //                   int  strategy);
      int status = deflateInit2 (m_strm, 8, Z_DEFLATED, 31, 8,
                                 Z_DEFAULT_STRATEGY);
      if (status != Z_OK)
        throw std::runtime_error ("failed to open zlib stream");

      deflateSetHeader (m_strm, &m_header);

      const std::size_t buf_len = 8192;
      unsigned char buf_in[buf_len];
      unsigned char buf_out[buf_len];

      int flush;

      do
        {
          m_strm->avail_in = std::fread (buf_in, sizeof (buf_in[0]),
                                         buf_len, m_source.m_fp);

          if (std::ferror (m_source.m_fp))
            throw std::runtime_error ("failed to read source file");

          m_strm->next_in = buf_in;
          flush = (std::feof (m_source.m_fp) ? Z_FINISH : Z_NO_FLUSH);

          // If deflate returns Z_OK and with zero avail_out, it must be
          // called again after making room in the output buffer because
          // there might be more output pending.
          do
            {
              m_strm->avail_out = buf_len;
              m_strm->next_out = buf_out;
              status = ::deflate (m_strm, flush);
              if (status == Z_STREAM_ERROR)
                throw std::runtime_error ("failed to deflate");

              std::fwrite (buf_out, sizeof (buf_out[0]),
                           buf_len - m_strm->avail_out, m_dest.m_fp);
              if (std::ferror (m_dest.m_fp))
                throw std::runtime_error ("failed to write file");
            }
          while (m_strm->avail_out == 0);

          if (m_strm->avail_in != 0)
            throw std::runtime_error ("failed to write file");

        }
      while (flush != Z_FINISH);

      if (status != Z_STREAM_END)
        throw std::runtime_error ("failed to write file");
    }

    void close (void)
    {
      if (deflateEnd (m_strm) != Z_OK)
        throw std::runtime_error ("failed to close zlib stream");
      m_strm = nullptr;

      // We have no error handling for failing to close source, let
      // the destructor close it.
      m_dest.close ();
    }

  private:

    CFile m_source;
    CFile m_dest;
    gzip_header m_header;
    z_stream *m_strm;
  };
};

#endif


template<typename X>
string_vector
xzip (const Array<std::string>& source_patterns,
      const std::function<std::string(const std::string&)>& mk_dest_path)
{
  std::list<std::string> dest_paths;

  std::function<void(const std::string&)> walk;
  walk = [&walk, &mk_dest_path, &dest_paths] (const std::string& path) -> void
  {
    const sys::file_stat fs (path);
    // is_dir and is_reg will return false if failed to stat.
    if (fs.is_dir ())
      {
        string_vector dirlist;
        std::string msg;

        // Collect the whole list of filenames first, before recursion
        // to avoid issues with infinite loop if the action generates
        // files in the same directory (highly likely).
        if (sys::get_dirlist (path, dirlist, msg))
          {
            for (octave_idx_type i = 0; i < dirlist.numel (); i++)
              if (dirlist(i) != "." && dirlist(i) != "..")
                walk (sys::file_ops::concat (path, dirlist(i)));
          }
        // Note that we skip any problem with directories.
      }
    else if (fs.is_reg ())
      {
        const std::string dest_path = mk_dest_path (path);
        try
          {
            X::zip (path, dest_path);
          }
        catch (const interrupt_exception&)
          {
            throw;  // interrupts are special, just re-throw.
          }
        catch (...)
          {
            // Error "handling" is not including filename on the output list.
            // Also, remove created file which may not have been created
            // in the first place.  Note that it is possible for the file
            // to exist before the call to X::zip and that X::zip has not
            // clobber it yet, but we remove it anyway.
            sys::unlink (dest_path);
            return;
          }
        dest_paths.push_front (dest_path);
      }
    // Skip all other file types and errors.
    return;
  };

  for (octave_idx_type i = 0; i < source_patterns.numel (); i++)
    {
      const glob_match pattern (sys::file_ops::tilde_expand (source_patterns(i)));
      const string_vector filepaths = pattern.glob ();
      for (octave_idx_type j = 0; j < filepaths.numel (); j++)
        walk (filepaths(j));
    }
  return string_vector (dest_paths);
}


template<typename X>
string_vector
xzip (const Array<std::string>& source_patterns)
{
  const std::string ext = X::extension;
  const std::function<std::string(const std::string&)> mk_dest_path
  = [&ext] (const std::string& source_path) -> std::string
  {
    return source_path + ext;
  };
  return xzip<X> (source_patterns, mk_dest_path);
}

template<typename X>
string_vector
xzip (const Array<std::string>& source_patterns, const std::string& out_dir)
{
  const std::string ext = X::extension;
  const std::function<std::string(const std::string&)> mk_dest_path
  = [&out_dir, &ext] (const std::string& source_path) -> std::string
  {
    // Strip any relative path (bug #58547)
    std::size_t pos = source_path.find_last_of (sys::file_ops::dir_sep_str ());
    const std::string basename =
    (pos == std::string::npos ? source_path : source_path.substr (pos+1));
    return sys::file_ops::concat (out_dir, basename + ext);
  };

  // We don't care if mkdir fails.  Maybe it failed because it already
  // exists, or maybe it can't be created.  If the first, then there's
  // nothing to do, if the later, then it will be handled later.  Any
  // is to be handled by not listing files in the output.
  sys::mkdir (out_dir, 0777);
  return xzip<X> (source_patterns, mk_dest_path);
}

template<typename X>
static octave_value_list
xzip (const std::string& fcn_name, const octave_value_list& args)
{
  const octave_idx_type nargin = args.length ();
  if (nargin < 1 || nargin > 2)
    print_usage ();

  const Array<std::string> source_patterns
    = args(0).xcellstr_value ("%s: FILES must be a character array or cellstr",
                              fcn_name.c_str ());
  if (nargin == 1)
    return octave_value (Cell (xzip<X> (source_patterns)));
  else // nargin == 2
    {
      const std::string out_dir = args(1).string_value ();
      return octave_value (Cell (xzip<X> (source_patterns, out_dir)));
    }
}

DEFUN_DLD (gzip, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{filelist} =} gzip (@var{files})
@deftypefnx {} {@var{filelist} =} gzip (@var{files}, @var{dir})
Compress the list of files and directories specified in @var{files}.

@var{files} is a character array or cell array of strings.  Shell wildcards
in the filename such as @samp{*} or @samp{?} are accepted and expanded.
Each file is compressed separately and a new file with a @file{".gz"}
extension is created.  The original files are not modified, but existing
compressed files will be silently overwritten.  If a directory is
specified then @code{gzip} recursively compresses all files in the
directory.

If @var{dir} is defined the compressed files are placed in this directory,
rather than the original directory where the uncompressed file resides.
Note that this does not replicate a directory tree in @var{dir} which may
lead to files overwriting each other if there are multiple files with the
same name.

If @var{dir} does not exist it is created.

The optional output @var{filelist} is a list of the compressed files.
@seealso{gunzip, unpack, bzip2, zip, tar}
@end deftypefn */)
{
#if defined (HAVE_Z)

  octave_value_list retval = xzip<gz> ("gzip", args);

  return (nargout > 0 ? retval : octave_value_list ());

#else

  octave_unused_parameter (args);
  octave_unused_parameter (nargout);

  err_disabled_feature ("gzip", "gzip");

#endif
}

/*
%!error gzip ()
%!error gzip ("1", "2", "3")
%!error <FILES must be a character array or cellstr|was unavailable or disabled> gzip (1)
*/

DEFUN_DLD (bzip2, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{filelist} =} bzip2 (@var{files})
@deftypefnx {} {@var{filelist} =} bzip2 (@var{files}, @var{dir})
Compress the list of files specified in @var{files}.

@var{files} is a character array or cell array of strings.  Shell wildcards
in the filename such as @samp{*} or @samp{?} are accepted and expanded.
Each file is compressed separately and a new file with a @file{".bz2"}
extension is created.  The original files are not modified, but existing
compressed files will be silently overwritten.

If @var{dir} is defined the compressed files are placed in this directory,
rather than the original directory where the uncompressed file resides.
Note that this does not replicate a directory tree in @var{dir} which may
lead to files overwriting each other if there are multiple files with the
same name.

If @var{dir} does not exist it is created.

The optional output @var{filelist} is a list of the compressed files.
@seealso{bunzip2, unpack, gzip, zip, tar}
@end deftypefn */)
{
#if defined (HAVE_BZ2)

  octave_value_list retval = xzip<bz2> ("bzip2", args);

  return (nargout > 0 ? retval : octave_value_list ());

#else

  octave_unused_parameter (args);
  octave_unused_parameter (nargout);

  err_disabled_feature ("bzip2", "bzip2");

#endif
}

// Tests for both gzip/bzip2 and gunzip/bunzip2
/*

## Takes a single argument, a function handle for the test.  This other
## function must accept two arguments, a directory for the tests, and
## a cell array with zip function, unzip function, and file extension.

%!function run_test_function (test_function)
%!  enabled_zippers = struct ("zip", {}, "unzip", {}, "ext", {});
%!  if (__octave_config_info__ ().build_features.BZ2)
%!    enabled_zippers(end+1).zip = @bzip2;
%!    enabled_zippers(end).unzip = @bunzip2;
%!    enabled_zippers(end).ext = ".bz2";
%!  endif
%!  if (__octave_config_info__ ().build_features.Z)
%!    enabled_zippers(end+1).zip = @gzip;
%!    enabled_zippers(end).unzip = @gunzip;
%!    enabled_zippers(end).ext = ".gz";
%!  endif
%!
%!  for z = enabled_zippers
%!    test_dir = tempname ();
%!    if (! mkdir (test_dir))
%!      error ("unable to create directory for tests");
%!    endif
%!    unwind_protect
%!      test_function (test_dir, z)
%!    unwind_protect_cleanup
%!      confirm_recursive_rmdir (false, "local");
%!      sts = rmdir (test_dir, "s");
%!    end_unwind_protect
%!  endfor
%!endfunction

%!function create_file (fpath, data)
%!  fid = fopen (fpath, "wb");
%!  if (fid < 0)
%!    error ("unable to open file for writing");
%!  endif
%!  if (fwrite (fid, data, class (data)) != numel (data))
%!    error ("unable to write to file");
%!  endif
%!  if (fflush (fid) || fclose (fid))
%!    error ("unable to flush or close file");
%!  endif
%!endfunction

%!function unlink_or_error (filepath)
%!  [err, msg] = unlink (filepath);
%!  if (err)
%!    error ("unable to remove file required for the test");
%!  endif
%!endfunction

## Test with large files because of varied buffer size
%!function test_large_file (test_dir, z)
%!  test_file = tempname (test_dir);
%!  create_file (test_file, rand (500000, 1));
%!  md5 = hash ("md5", fileread (test_file));
%!
%!  z_file = [test_file z.ext];
%!  z_filelist = z.zip (test_file);
%!  assert (is_same_file (z_filelist, {z_file}))
%!
%!  unlink_or_error (test_file);
%!  uz_filelist = z.unzip (z_file);
%!  assert (is_same_file (uz_filelist, {test_file}))
%!
%!  assert (hash ("md5", fileread (test_file)), md5)
%!endfunction
%!test run_test_function (@test_large_file)

## Test that xzipped files are rexzipped (hits bug #43206, #48598)
%!function test_z_z (test_dir, z)
%!  ori_file = tempname (test_dir);
%!  create_file (ori_file, rand (100, 1));
%!  md5_ori = hash ("md5", fileread (ori_file));
%!
%!  z_file = [ori_file z.ext];
%!  z_filelist = z.zip (ori_file);
%!  assert (is_same_file (z_filelist, {z_file})) # check output
%!  assert (exist (z_file), 2) # confirm file exists
%!  assert (exist (ori_file), 2) # and did not remove original file
%!
%!  unlink_or_error (ori_file);
%!  uz_filelist = z.unzip (z_file);
%!  assert (is_same_file (uz_filelist, {ori_file})) # bug #48598
%!  assert (hash ("md5", fileread (ori_file)), md5_ori)
%!  assert (exist (z_file), 2) # bug #48597
%!
%!  ## xzip should preserve original files.
%!  z_z_file = [z_file z.ext];
%!  z_z_filelist = z.zip (z_file);
%!  assert (is_same_file (z_z_filelist, {z_z_file})) # check output
%!  assert (exist (z_z_file), 2) # confirm file exists
%!  assert (exist (z_file), 2)
%!
%!  md5_z = hash ("md5", fileread (z_file));
%!  unlink_or_error (z_file);
%!  uz_z_filelist = z.unzip (z_z_file);
%!  assert (is_same_file (uz_z_filelist, {z_file})) # bug #48598
%!  assert (exist (z_z_file), 2) # bug #43206
%!  assert (hash ("md5", fileread (z_file)), md5_z)
%!endfunction
%!test <43206> run_test_function (@test_z_z)

%!function test_xzip_dir (test_dir, z) # bug #43431
%!  fpaths = fullfile (test_dir, {"test1", "test2", "test3"});
%!  md5s = cell (1, 3);
%!  for idx = 1:numel (fpaths)
%!    create_file (fpaths{idx}, rand (100, 1));
%!    md5s(idx) = hash ("md5", fileread (fpaths{idx}));
%!  endfor
%!
%!  test_dir = [test_dir filesep()];
%!
%!  z_files = strcat (fpaths, z.ext);
%!  z_filelist = z.zip (test_dir);
%!  assert (sort (z_filelist), z_files(:))
%!  for idx = 1:numel (fpaths)
%!    assert (exist (z_files{idx}), 2)
%!    unlink_or_error (fpaths{idx});
%!  endfor
%!
%!  ## only gunzip handles directory (bunzip2 should too though)
%!  if (z.unzip == @gunzip)
%!    uz_filelist = z.unzip (test_dir);
%!  else
%!    uz_filelist = cell (1, numel (z_filelist));
%!    for idx = 1:numel (z_filelist)
%!      uz_filelist(idx) = z.unzip (z_filelist{idx});
%!    endfor
%!  endif
%!  uz_filelist = sort (uz_filelist);
%!  fpaths = sort (fpaths);
%!  assert (is_same_file (uz_filelist(:), fpaths(:))) # bug #48598
%!  for idx = 1:numel (fpaths)
%!    assert (hash ("md5", fileread (fpaths{idx})), md5s{idx})
%!  endfor
%!endfunction
%!test <48598> run_test_function (@test_xzip_dir)

%!function test_save_to_dir (test_dir, z)
%!  filename = "test-file";
%!  filepath = fullfile (test_dir, filename);
%!  create_file (filepath, rand (100, 1));
%!  md5 = hash ("md5", fileread (filepath));
%!
%!  ## test with existing and non-existing directory
%!  out_dirs = {tempname (test_dir), tempname (test_dir)};
%!  if (! mkdir (out_dirs{1}))
%!    error ("unable to create directory for test");
%!  endif
%!  unwind_protect
%!    for idx = 1:numel (out_dirs)
%!      out_dir = out_dirs{idx};
%!      uz_file = fullfile (out_dir, filename);
%!      z_file = [uz_file z.ext];
%!
%!      z_filelist = z.zip (filepath, out_dir);
%!      assert (z_filelist, {z_file})
%!      assert (exist (z_file, "file"), 2)
%!
%!      uz_filelist = z.unzip (z_file);
%!      assert (is_same_file (uz_filelist, {uz_file})) # bug #48598
%!
%!      assert (hash ("md5", fileread (uz_file)), md5)
%!    endfor
%!  unwind_protect_cleanup
%!    confirm_recursive_rmdir (false, "local");
%!    for idx = 1:numel (out_dirs)
%!      sts = rmdir (out_dirs{idx}, "s");
%!    endfor
%!  end_unwind_protect
%!endfunction
%!test run_test_function (@test_save_to_dir)
*/

OCTAVE_END_NAMESPACE(octave)
