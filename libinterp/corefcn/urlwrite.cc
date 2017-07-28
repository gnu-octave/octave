/*

Copyright (C) 2006-2017 Alexander Barth
Copyright (C) 2009 David Bateman

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

// Author: Alexander Barth <abarth@marine.usf.edu>
// Adapted-By: jwe

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <string>
#include <fstream>
#include <iomanip>
#include <iostream>

#include "dir-ops.h"
#include "file-ops.h"
#include "file-stat.h"
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
#include "ovl.h"
#include "pager.h"
#include "unwind-prot.h"
#include "url-handle-manager.h"

static void
delete_file (const std::string& file)
{
  octave::sys::unlink (file);
}

DEFUN (urlwrite, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} urlwrite (@var{url}, @var{localfile})
@deftypefnx {} {@var{f} =} urlwrite (@var{url}, @var{localfile})
@deftypefnx {} {[@var{f}, @var{success}] =} urlwrite (@var{url}, @var{localfile})
@deftypefnx {} {[@var{f}, @var{success}, @var{message}] =} urlwrite (@var{url}, @var{localfile})
Download a remote file specified by its @var{url} and save it as
@var{localfile}.

For example:

@example
@group
urlwrite ("ftp://ftp.octave.org/pub/README",
          "README.txt");
@end group
@end example

The full path of the downloaded file is returned in @var{f}.

The variable @var{success} is 1 if the download was successful,
otherwise it is 0 in which case @var{message} contains an error message.

If no output argument is specified and an error occurs, then the error is
signaled through Octave's error handling mechanism.

This function uses libcurl.  Curl supports, among others, the HTTP, FTP, and
FILE protocols.  Username and password may be specified in the URL, for
example:

@example
@group
urlwrite ("http://username:password@@example.com/file.txt",
          "file.txt");
@end group
@end example

GET and POST requests can be specified by @var{method} and @var{param}.
The parameter @var{method} is either @samp{get} or @samp{post} and
@var{param} is a cell array of parameter and value pairs.
For example:

@example
@group
urlwrite ("http://www.google.com/search", "search.html",
          "get", @{"query", "octave"@});
@end group
@end example
@seealso{urlread}
@end deftypefn */)
{
  int nargin = args.length ();

  // verify arguments
  if (nargin != 2 && nargin != 4)
    print_usage ();

  std::string url = args(0).xstring_value ("urlwrite: URL must be a string");

  // name to store the file if download is successful
  std::string filename = args(1).xstring_value ("urlwrite: LOCALFILE must be a string");

  std::string method;
  Array<std::string> param;

  if (nargin == 4)
    {
      method = args(2).xstring_value ("urlwrite: METHOD must be a string");

      if (method != "get" && method != "post")
        error (R"(urlwrite: METHOD must be "get" or "post")");

      param = args(3).xcellstr_value ("urlwrite: parameters (PARAM) for get and post requests must be given as a cell array of strings");

      if (param.numel () % 2 == 1)
        error ("urlwrite: number of elements in PARAM must be even");
    }

  // The file should only be deleted if it doesn't initially exist, we
  // create it, and the download fails.  We use unwind_protect to do
  // it so that the deletion happens no matter how we exit the function.

  octave::sys::file_stat fs (filename);

  std::ofstream ofile (filename.c_str (), std::ios::out | std::ios::binary);

  if (! ofile.is_open ())
    error ("urlwrite: unable to open file");

  octave::unwind_protect_safe frame;

  frame.add_fcn (delete_file, filename);

  octave::url_transfer url_xfer (url, ofile);

  octave_value_list retval;

  if (! url_xfer.is_valid ())
    error ("support for URL transfers was disabled when Octave was built");

  url_xfer.http_action (param, method);

  ofile.close ();

  if (url_xfer.good ())
    frame.discard ();

  if (nargout > 0)
    {
      if (url_xfer.good ())
        retval = ovl (octave::sys::env::make_absolute (filename), true, "");
      else
        retval = ovl ("", false, url_xfer.lasterror ());
    }

  if (nargout < 2 && ! url_xfer.good ())
    error ("urlwrite: %s", url_xfer.lasterror ().c_str ());

  return retval;
}

DEFUN (urlread, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{s} =} urlread (@var{url})
@deftypefnx {} {[@var{s}, @var{success}] =} urlread (@var{url})
@deftypefnx {} {[@var{s}, @var{success}, @var{message}] =} urlread (@var{url})
@deftypefnx {} {[@dots{}] =} urlread (@var{url}, @var{method}, @var{param})
Download a remote file specified by its @var{url} and return its content
in string @var{s}.

For example:

@example
s = urlread ("ftp://ftp.octave.org/pub/README");
@end example

The variable @var{success} is 1 if the download was successful,
otherwise it is 0 in which case @var{message} contains an error
message.

If no output argument is specified and an error occurs, then the error is
signaled through Octave's error handling mechanism.

This function uses libcurl.  Curl supports, among others, the HTTP, FTP, and
FILE protocols.  Username and password may be specified in the URL@.  For
example:

@example
s = urlread ("http://user:password@@example.com/file.txt");
@end example

GET and POST requests can be specified by @var{method} and @var{param}.
The parameter @var{method} is either @samp{get} or @samp{post} and
@var{param} is a cell array of parameter and value pairs.
For example:

@example
@group
s = urlread ("http://www.google.com/search", "get",
            @{"query", "octave"@});
@end group
@end example
@seealso{urlwrite}
@end deftypefn */)
{
  int nargin = args.length ();

  // verify arguments
  if (nargin != 1 && nargin != 3)
    print_usage ();

  std::string url = args(0).xstring_value ("urlread: URL must be a string");

  std::string method;
  Array<std::string> param;

  if (nargin == 3)
    {
      method = args(1).xstring_value ("urlread: METHOD must be a string");

      if (method != "get" && method != "post")
        error (R"(urlread: METHOD must be "get" or "post")");

      param = args(2).xcellstr_value ("urlread: parameters (PARAM) for get and post requests must be given as a cell array of strings");

      if (param.numel () % 2 == 1)
        error ("urlread: number of elements in PARAM must be even");
    }

  std::ostringstream buf;

  octave::url_transfer url_xfer = octave::url_transfer (url, buf);

  if (! url_xfer.is_valid ())
    error ("support for URL transfers was disabled when Octave was built");

  url_xfer.http_action (param, method);

  octave_value_list retval;

  if (nargout > 0)
    {
      // Return empty string if no error occurred.
      retval = ovl (buf.str (), url_xfer.good (),
                    url_xfer.good () ? "" : url_xfer.lasterror ());
    }

  if (nargout < 2 && ! url_xfer.good ())
    error ("urlread: %s", url_xfer.lasterror ().c_str ());

  return retval;
}

DEFMETHOD (__ftp__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{handle} =} __ftp__ (@var{host})
@deftypefnx {} {@var{handle} =} __ftp__ (@var{host}, @var{username}, @var{password})
Undocumented internal function
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    print_usage ();

  std::string host = args(0).xstring_value ("__ftp__: HOST must be a string");

  std::string user = (nargin > 1)
    ? args(1).xstring_value ("__ftp__: USER must be a string")
    : std::string ("anonymous");

  std::string passwd = (nargin > 2)
    ? args(2).xstring_value ("__ftp__: PASSWD must be a string")
    : "";

  octave::url_handle_manager& uhm = interp.get_url_handle_manager ();

  octave::url_handle uh = uhm.make_url_handle (host, user, passwd,
                                               octave_stdout);

  return ovl (uh.value ());
}

DEFMETHOD (__ftp_pwd__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __ftp_pwd__ (@var{handle})
Undocumented internal function
@end deftypefn */)
{
  if (args.length () != 1)
    error ("__ftp_pwd__: incorrect number of arguments");

  octave::url_handle_manager& uhm = interp.get_url_handle_manager ();

  octave::url_transfer url_xfer = uhm.get_object (args(0));

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
  int nargin = args.length ();

  if (nargin != 1 && nargin != 2)
    error ("__ftp_cwd__: incorrect number of arguments");

  std::string path = "";
  if (nargin > 1)
    path = args(1).xstring_value ("__ftp_cwd__: PATH must be a string");

  octave::url_handle_manager& uhm = interp.get_url_handle_manager ();

  octave::url_transfer url_xfer = uhm.get_object (args(0));

  if (! url_xfer.is_valid ())
    error ("__ftp_cwd__: invalid ftp handle");

  url_xfer.cwd (path);

  return ovl ();
}

DEFMETHOD (__ftp_dir__, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __ftp_dir__ (@var{handle})
Undocumented internal function
@end deftypefn */)
{
  if (args.length () != 1)
    error ("__ftp_dir__: incorrect number of arguments");

  octave::url_handle_manager& uhm = interp.get_url_handle_manager ();

  octave::url_transfer url_xfer = uhm.get_object (args(0));

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
              time_t ftime;
              bool fisdir;
              double fsize;

              url_xfer.get_fileinfo (sv(i), fsize, ftime, fisdir);

              fileisdir (i) = fisdir;
              filectime (i) = ctime (&ftime);
              filesize (i) = fsize;
              filedatenum (i) = double (ftime);
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
  if (args.length () != 1)
    error ("__ftp_ascii__: incorrect number of arguments");

  octave::url_handle_manager& uhm = interp.get_url_handle_manager ();

  octave::url_transfer url_xfer = uhm.get_object (args(0));

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
  if (args.length () != 1)
    error ("__ftp_binary__: incorrect number of arguments");

  octave::url_handle_manager& uhm = interp.get_url_handle_manager ();

  octave::url_transfer url_xfer = uhm.get_object (args(0));

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
  if (args.length () != 1)
    error ("__ftp_close__: incorrect number of arguments");

  octave::url_handle_manager& uhm = interp.get_url_handle_manager ();

  octave::url_handle h = uhm.lookup (args(0));

  if (! h.ok ())
    error ("__ftp_close__: invalid ftp handle");

  uhm.free (h);

  return ovl ();
}

DEFMETHOD (__ftp_mode__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __ftp_mode__ (@var{handle})
Undocumented internal function
@end deftypefn */)
{
  if (args.length () != 1)
    error ("__ftp_mode__: incorrect number of arguments");

  octave::url_handle_manager& uhm = interp.get_url_handle_manager ();

  octave::url_transfer url_xfer = uhm.get_object (args(0));

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
  if (args.length () != 2)
    error ("__ftp_delete__: incorrect number of arguments");

  std::string file = args(1).xstring_value ("__ftp_delete__: FILE must be a string");

  octave::url_handle_manager& uhm = interp.get_url_handle_manager ();

  octave::url_transfer url_xfer = uhm.get_object (args(0));

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
  if (args.length () != 2)
    error ("__ftp_rmdir__: incorrect number of arguments");

  std::string dir = args(1).xstring_value ("__ftp_rmdir__: DIR must be a string");

  octave::url_handle_manager& uhm = interp.get_url_handle_manager ();

  octave::url_transfer url_xfer = uhm.get_object (args(0));

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
  if (args.length () != 2)
    error ("__ftp_mkdir__: incorrect number of arguments");

  std::string dir = args(1).xstring_value ("__ftp_mkdir__: DIR must be a string");

  octave::url_handle_manager& uhm = interp.get_url_handle_manager ();

  octave::url_transfer url_xfer = uhm.get_object (args(0));

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
  if (args.length () != 3)
    error ("__ftp_rename__: incorrect number of arguments");

  std::string oldname = args(1).xstring_value ("__ftp_rename__: OLDNAME must be a string");
  std::string newname = args(2).xstring_value ("__ftp_rename__: NEWNAME must be a string");

  octave::url_handle_manager& uhm = interp.get_url_handle_manager ();

  octave::url_transfer url_xfer = uhm.get_object (args(0));

  if (url_xfer.is_valid ())
    error ("__ftp_rename__: invalid ftp handle");

  url_xfer.rename (oldname, newname);

  return ovl ();
}

DEFMETHOD (__ftp_mput__, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __ftp_mput__ (@var{handle}, @var{files})
Undocumented internal function
@end deftypefn */)
{
  if (args.length () != 2)
    error ("__ftp_mput__: incorrect number of arguments");

  std::string pat = args(1).xstring_value ("__ftp_mput__: PATTERN must be a string");

  octave::url_handle_manager& uhm = interp.get_url_handle_manager ();

  octave::url_transfer url_xfer = uhm.get_object (args(0));

  if (! url_xfer.is_valid ())
    error ("__ftp_mput__: invalid ftp handle");

  string_vector file_list;

  glob_match pattern (octave::sys::file_ops::tilde_expand (pat));
  string_vector files = pattern.glob ();

  for (octave_idx_type i = 0; i < files.numel (); i++)
    {
      std::string file = files(i);

      octave::sys::file_stat fs (file);

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
          std::ifstream ifile (file.c_str (), std::ios::in | std::ios::binary);

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
  int nargin = args.length ();

  if (nargin != 2 && nargin != 3)
    error ("__ftp_mget__: incorrect number of arguments");

  std::string file = args(1).xstring_value ("__ftp_mget__: PATTERN must be a string");

  std::string target;

  if (nargin == 3 && ! args(2).isempty ())
    target = args(2).xstring_value ("__ftp_mget__: TARGET must be a string") + octave::sys::file_ops::dir_sep_str ();

  octave::url_handle_manager& uhm = interp.get_url_handle_manager ();

  octave::url_transfer url_xfer = uhm.get_object (args(0));

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

          time_t ftime;
          bool fisdir;
          double fsize;

          url_xfer.get_fileinfo (sv(i), fsize, ftime, fisdir);

          if (fisdir)
            url_xfer.mget_directory (sv(i), target);
          else
            {
              std::ofstream ofile ((target + sv(i)).c_str (),
                                   std::ios::out |
                                   std::ios::binary);

              if (! ofile.is_open ())
                error ("__ftp_mget__: unable to open file");

              octave::unwind_protect_safe frame;

              frame.add_fcn (delete_file, target + sv(i));

              url_xfer.get (sv(i), ofile);

              ofile.close ();

              if (url_xfer.good ())
                frame.discard ();
            }

          if (! url_xfer.good ())
            error ("__ftp_mget__: %s", url_xfer.lasterror().c_str());
        }
    }

  if (n == 0)
    error ("__ftp_mget__: file not found");

  return ovl ();
}
