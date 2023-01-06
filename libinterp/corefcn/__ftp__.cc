////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2006-2023 The Octave Project Developers
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

#include <string>
#include <fstream>
#include <iomanip>

#include "dir-ops.h"
#include "file-ops.h"
#include "file-stat.h"
#include "lo-sysdep.h"
#include "oct-env.h"
#include "oct-handle.h"
#include "glob-match.h"
#include "url-transfer.h"

#include "defun.h"
#include "error.h"
#include "interpreter.h"
#include "oct-map.h"
#include "oct-refcount.h"
#include "ov-cell.h"
#include "ov-classdef.h"
#include "ovl.h"
#include "pager.h"
#include "unwind-prot.h"
#include "url-handle-manager.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFMETHOD (__ftp__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{handle} =} __ftp__ (@var{host})
@deftypefnx {} {@var{handle} =} __ftp__ (@var{host}, @var{username}, @var{password})
Undocumented internal function
@end deftypefn */)
{
  int nargin = args.length ();

  std::string host = args(0).xstring_value ("__ftp__: HOST must be a string");

  std::string user = (nargin > 1)
                     ? args(1).xstring_value ("__ftp__: USER must be a string")
                     : "anonymous";

  std::string passwd = (nargin > 2)
                       ? args(2).xstring_value ("__ftp__: PASSWD must be a string")
                       : "";

  url_handle_manager& uhm = interp.get_url_handle_manager ();

  url_handle uh = uhm.make_url_handle (host, user, passwd, octave_stdout);

  return ovl (uh.value ());
}

DEFMETHOD (__ftp_pwd__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{pwd} =} __ftp_pwd__ (@var{handle})
Undocumented internal function
@end deftypefn */)
{
  url_handle_manager& uhm = interp.get_url_handle_manager ();

  url_transfer url_xfer = uhm.get_object (args(0));

  if (! url_xfer.is_valid ())
    error ("__ftp_pwd__: invalid ftp handle");

  return ovl (url_xfer.pwd ());
}

DEFMETHOD (__ftp_cwd__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __ftp_cwd__ (@var{handle}, @var{path})
Undocumented internal function
@end deftypefn */)
{
  std::string path = "";
  if (args.length () > 1)
    path = args(1).xstring_value ("__ftp_cwd__: PATH must be a string");

  url_handle_manager& uhm = interp.get_url_handle_manager ();

  url_transfer url_xfer = uhm.get_object (args(0));

  if (! url_xfer.is_valid ())
    error ("__ftp_cwd__: invalid ftp handle");

  url_xfer.cwd (path);

  return ovl ();
}

DEFMETHOD (__ftp_dir__, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} __ftp_dir__ (@var{handle})
@deftypefnx {} {@var{S} =} __ftp_dir__ (@var{handle})
Undocumented internal function
@end deftypefn */)
{
  url_handle_manager& uhm = interp.get_url_handle_manager ();

  url_transfer url_xfer = uhm.get_object (args(0));

  if (! url_xfer.is_valid ())
    error ("__ftp_dir__: invalid ftp handle");

  octave_value retval;

  if (nargout == 0)
    url_xfer.dir ();
  else
    {
      string_vector sv = url_xfer.list ();
      octave_idx_type n = sv.numel ();

      if (n == 0)
        {
          string_vector flds (5);

          flds(0) = "name";
          flds(1) = "date";
          flds(2) = "bytes";
          flds(3) = "isdir";
          flds(4) = "datenum";

          retval = octave_map (flds);
        }
      else
        {
          octave_map st;

          Cell filectime (dim_vector (n, 1));
          Cell filesize (dim_vector (n, 1));
          Cell fileisdir (dim_vector (n, 1));
          Cell filedatenum (dim_vector (n, 1));

          st.assign ("name", Cell (sv));

          for (octave_idx_type i = 0; i < n; i++)
            {
              OCTAVE_TIME_T ftime;
              bool fisdir;
              double fsize;

              url_xfer.get_fileinfo (sv(i), fsize, ftime, fisdir);

              fileisdir(i) = fisdir;
              time_t ftime_t = ftime;
              filectime(i) = ctime (&ftime_t);
              filesize(i)  = fsize;
              filedatenum(i) = double (ftime);
            }

          st.assign ("date", filectime);
          st.assign ("bytes", filesize);
          st.assign ("isdir", fileisdir);
          st.assign ("datenum", filedatenum);

          retval = st;
        }
    }

  return retval;
}

DEFMETHOD (__ftp_ascii__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __ftp_ascii__ (@var{handle})
Undocumented internal function
@end deftypefn */)
{
  url_handle_manager& uhm = interp.get_url_handle_manager ();

  url_transfer url_xfer = uhm.get_object (args(0));

  if (! url_xfer.is_valid ())
    error ("__ftp_ascii__: invalid ftp handle");

  url_xfer.ascii ();

  return ovl ();
}

DEFMETHOD (__ftp_binary__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __ftp_binary__ (@var{handle})
Undocumented internal function
@end deftypefn */)
{
  url_handle_manager& uhm = interp.get_url_handle_manager ();

  url_transfer url_xfer = uhm.get_object (args(0));

  if (! url_xfer.is_valid ())
    error ("__ftp_binary__: invalid ftp handle");

  url_xfer.binary ();

  return ovl ();
}

DEFMETHOD (__ftp_close__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __ftp_close__ (@var{handle})
Undocumented internal function
@end deftypefn */)
{
  url_handle_manager& uhm = interp.get_url_handle_manager ();

  url_handle h = uhm.lookup (args(0));

  if (! h.ok ())
    error ("__ftp_close__: invalid ftp handle");

  uhm.free (h);

  return ovl ();
}

DEFMETHOD (__ftp_mode__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{mode} =} __ftp_mode__ (@var{handle})
Undocumented internal function
@end deftypefn */)
{
  url_handle_manager& uhm = interp.get_url_handle_manager ();

  url_transfer url_xfer = uhm.get_object (args(0));

  if (! url_xfer.is_valid ())
    error ("__ftp_binary__: invalid ftp handle");

  return ovl (url_xfer.is_ascii () ? "ascii" : "binary");
}

DEFMETHOD (__ftp_delete__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __ftp_delete__ (@var{handle}, @var{path})
Undocumented internal function
@end deftypefn */)
{
  std::string file = args(1).xstring_value ("__ftp_delete__: FILE must be a string");

  url_handle_manager& uhm = interp.get_url_handle_manager ();

  url_transfer url_xfer = uhm.get_object (args(0));

  if (! url_xfer.is_valid ())
    error ("__ftp_delete__: invalid ftp handle");

  url_xfer.del (file);

  return ovl ();
}

DEFMETHOD (__ftp_rmdir__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __ftp_rmdir__ (@var{handle}, @var{path})
Undocumented internal function
@end deftypefn */)
{
  std::string dir = args(1).xstring_value ("__ftp_rmdir__: DIR must be a string");

  url_handle_manager& uhm = interp.get_url_handle_manager ();

  url_transfer url_xfer = uhm.get_object (args(0));

  if (! url_xfer.is_valid ())
    error ("__ftp_rmdir__: invalid ftp handle");

  url_xfer.rmdir (dir);

  return ovl ();
}

DEFMETHOD (__ftp_mkdir__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __ftp_mkdir__ (@var{handle}, @var{path})
Undocumented internal function
@end deftypefn */)
{
  std::string dir = args(1).xstring_value ("__ftp_mkdir__: DIR must be a string");

  url_handle_manager& uhm = interp.get_url_handle_manager ();

  url_transfer url_xfer = uhm.get_object (args(0));

  if (! url_xfer.is_valid ())
    error ("__ftp_mkdir__: invalid ftp handle");

  url_xfer.mkdir (dir);

  return ovl ();
}

DEFMETHOD (__ftp_rename__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __ftp_rename__ (@var{handle}, @var{path})
Undocumented internal function
@end deftypefn */)
{
  std::string oldname = args(1).xstring_value ("__ftp_rename__: OLDNAME must be a string");
  std::string newname = args(2).xstring_value ("__ftp_rename__: NEWNAME must be a string");

  url_handle_manager& uhm = interp.get_url_handle_manager ();

  url_transfer url_xfer = uhm.get_object (args(0));

  if (url_xfer.is_valid ())
    error ("__ftp_rename__: invalid ftp handle");

  url_xfer.rename (oldname, newname);

  return ovl ();
}

DEFMETHOD (__ftp_mput__, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} __ftp_mput__ (@var{handle}, @var{files})
@deftypefnx {} {@var{filelist} =} __ftp_mput__ (@var{handle}, @var{files})
Undocumented internal function
@end deftypefn */)
{
  std::string pat = args(1).xstring_value ("__ftp_mput__: PATTERN must be a string");

  url_handle_manager& uhm = interp.get_url_handle_manager ();

  url_transfer url_xfer = uhm.get_object (args(0));

  if (! url_xfer.is_valid ())
    error ("__ftp_mput__: invalid ftp handle");

  string_vector file_list;

  glob_match pattern (sys::file_ops::tilde_expand (pat));
  string_vector files = pattern.glob ();

  for (octave_idx_type i = 0; i < files.numel (); i++)
    {
      std::string file = files(i);

      sys::file_stat fs (file);

      if (! fs.exists ())
        error ("__ftp__mput: file does not exist");

      if (fs.is_dir ())
        {
          file_list.append (url_xfer.mput_directory ("", file));

          if (! url_xfer.good ())
            error ("__ftp_mput__: %s", url_xfer.lasterror ().c_str ());
        }
      else
        {
          // FIXME: Does ascii mode need to be flagged here?
          std::ifstream ifile =
            sys::ifstream (file.c_str (),
                           std::ios::in | std::ios::binary);

          if (! ifile.is_open ())
            error ("__ftp_mput__: unable to open file");

          url_xfer.put (file, ifile);

          ifile.close ();

          if (! url_xfer.good ())
            error ("__ftp_mput__: %s", url_xfer.lasterror ().c_str ());

          file_list.append (file);
        }
    }

  if (nargout > 0)
    return ovl (file_list);
  else
    return ovl ();
}

DEFMETHOD (__ftp_mget__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} __ftp_mget__ (@var{handle}, @var{pattern})
@deftypefnx {} {} __ftp_mget__ (@var{handle}, @var{pattern}, @var{target})
Undocumented internal function
@end deftypefn */)
{
  std::string file = args(1).xstring_value ("__ftp_mget__: PATTERN must be a string");

  std::string target;

  if (args.length () == 3 && ! args(2).isempty ())
    target = args(2).xstring_value ("__ftp_mget__: TARGET must be a string")
             + sys::file_ops::dir_sep_str ();

  url_handle_manager& uhm = interp.get_url_handle_manager ();

  url_transfer url_xfer = uhm.get_object (args(0));

  if (! url_xfer.is_valid ())
    error ("__ftp_mget__: invalid ftp handle");

  string_vector sv = url_xfer.list ();
  octave_idx_type n = 0;
  glob_match pattern (file);

  for (octave_idx_type i = 0; i < sv.numel (); i++)
    {
      if (pattern.match (sv(i)))
        {
          n++;

          OCTAVE_TIME_T ftime;
          bool fisdir;
          double fsize;

          url_xfer.get_fileinfo (sv(i), fsize, ftime, fisdir);

          if (fisdir)
            url_xfer.mget_directory (sv(i), target);
          else
            {
              std::ofstream ofile =
                sys::ofstream ((target + sv(i)).c_str (),
                               std::ios::out | std::ios::binary);

              if (! ofile.is_open ())
                error ("__ftp_mget__: unable to open file");

              int(*unlink_fptr)(const std::string&) = sys::unlink;
              unwind_action_safe delete_file (unlink_fptr, target + sv(i));

              url_xfer.get (sv(i), ofile);

              ofile.close ();

              if (url_xfer.good ())
                delete_file.discard ();
            }

          if (! url_xfer.good ())
            error ("__ftp_mget__: %s", url_xfer.lasterror ().c_str ());
        }
    }

  if (n == 0)
    error ("__ftp_mget__: file not found");

  return ovl ();
}

OCTAVE_END_NAMESPACE(octave)
