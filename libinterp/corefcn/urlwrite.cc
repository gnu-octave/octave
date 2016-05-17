// urlwrite and urlread, a curl front-end for octave
/*

Copyright (C) 2006-2015 Alexander Barth
Copyright (C) 2009 David Bateman

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
#include "singleton-cleanup.h"
#include "url-transfer.h"

#include "defun.h"
#include "error.h"
#include "ovl.h"
#include "ov-cell.h"
#include "pager.h"
#include "oct-map.h"
#include "oct-refcount.h"
#include "unwind-prot.h"

static void
delete_file (const std::string& file)
{
  octave_unlink (file);
}

typedef octave_handle curl_handle;

class OCTINTERP_API ch_manager
{
protected:

  ch_manager (void)
    : handle_map (), handle_free_list (),
      next_handle (-1.0 - (rand () + 1.0) / (RAND_MAX + 2.0)) { }

public:

  static void create_instance (void);

  static bool instance_ok (void)
  {
    bool retval = true;

    if (! instance)
      create_instance ();

    if (! instance)
      error ("unable to create ch_manager!");

    return retval;
  }

  static void cleanup_instance (void) { delete instance; instance = 0; }

  static curl_handle get_handle (void)
  {
    return instance_ok () ? instance->do_get_handle () : curl_handle ();
  }

  static void free (const curl_handle& h)
  {
    if (instance_ok ())
      instance->do_free (h);
  }

  static curl_handle lookup (double val)
  {
    return instance_ok () ? instance->do_lookup (val) : curl_handle ();
  }

  static curl_handle lookup (const octave_value& val)
  {
    return val.is_real_scalar () ? lookup (val.double_value ())
                                 : curl_handle ();
  }

  static url_transfer get_object (double val)
  {
    return get_object (lookup (val));
  }

  static url_transfer get_object (const octave_value& val)
  {
    return get_object (lookup (val));
  }

  static url_transfer get_object (const curl_handle& h)
  {
    return instance_ok () ? instance->do_get_object (h) : url_transfer ();
  }

  static curl_handle make_curl_handle (const std::string& host,
                                       const std::string& user,
                                       const std::string& passwd,
                                       std::ostream& os)
  {
    return instance_ok ()
             ? instance->do_make_curl_handle (host, user, passwd, os)
             : curl_handle ();
  }

  static Matrix handle_list (void)
  {
    return instance_ok () ? instance->do_handle_list () : Matrix ();
  }

private:

  static ch_manager *instance;

  typedef std::map<curl_handle, url_transfer>::iterator iterator;
  typedef std::map<curl_handle, url_transfer>::const_iterator const_iterator;

  typedef std::set<curl_handle>::iterator free_list_iterator;
  typedef std::set<curl_handle>::const_iterator const_free_list_iterator;

  // A map of handles to curl objects.
  std::map<curl_handle, url_transfer> handle_map;

  // The available curl handles.
  std::set<curl_handle> handle_free_list;

  // The next handle available if handle_free_list is empty.
  double next_handle;

  curl_handle do_get_handle (void);

  void do_free (const curl_handle& h);

  curl_handle do_lookup (double val)
  {
    iterator p = (xisnan (val) ? handle_map.end () : handle_map.find (val));

    return (p != handle_map.end ()) ? p->first : curl_handle ();
  }

  url_transfer do_get_object (const curl_handle& h)
  {
    iterator p = (h.ok () ? handle_map.find (h) : handle_map.end ());

    return (p != handle_map.end ()) ? p->second : url_transfer ();
  }

  curl_handle do_make_curl_handle (const std::string& host,
                                   const std::string& user,
                                   const std::string& passwd,
                                   std::ostream& os)
  {
    curl_handle h = get_handle ();

    url_transfer obj (host, user, passwd, os);

    if (! obj.is_valid ())
      error ("support for URL transfers was disabled when Octave was built");

    handle_map[h] = obj;

    return h;
  }

  Matrix do_handle_list (void)
  {
    Matrix retval (1, handle_map.size ());

    octave_idx_type i = 0;
    for (const_iterator p = handle_map.begin (); p != handle_map.end (); p++)
      {
        curl_handle h = p->first;

        retval(i++) = h.value ();
      }

    return retval;
  }
};

void
ch_manager::create_instance (void)
{
  instance = new ch_manager ();

  if (instance)
    singleton_cleanup_list::add (cleanup_instance);
}

static double
make_handle_fraction (void)
{
  static double maxrand = RAND_MAX + 2.0;

  return (rand () + 1.0) / maxrand;
}

curl_handle
ch_manager::do_get_handle (void)
{
  curl_handle retval;

  // Curl handles are negative integers plus some random fractional
  // part.  To avoid running out of integers, we recycle the integer
  // part but tack on a new random part each time.

  free_list_iterator p = handle_free_list.begin ();

  if (p != handle_free_list.end ())
    {
      retval = *p;
      handle_free_list.erase (p);
    }
  else
    {
      retval = curl_handle (next_handle);

      next_handle = std::ceil (next_handle) - 1.0 - make_handle_fraction ();
    }

  return retval;
}

void
ch_manager::do_free (const curl_handle& h)
{
  if (h.ok ())
    {
      iterator p = handle_map.find (h);

      if (p == handle_map.end ())
        error ("ch_manager::free: invalid object %g", h.value ());

      // Curl handles are negative integers plus some random
      // fractional part.  To avoid running out of integers, we
      // recycle the integer part but tack on a new random part
      // each time.

      handle_map.erase (p);

      if (h.value () < 0)
        handle_free_list.insert
          (std::ceil (h.value ()) - make_handle_fraction ());
    }
}

ch_manager *ch_manager::instance = 0;

DEFUN (urlwrite, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {} {} urlwrite (@var{url}, @var{localfile})\n\
@deftypefnx {} {@var{f} =} urlwrite (@var{url}, @var{localfile})\n\
@deftypefnx {} {[@var{f}, @var{success}] =} urlwrite (@var{url}, @var{localfile})\n\
@deftypefnx {} {[@var{f}, @var{success}, @var{message}] =} urlwrite (@var{url}, @var{localfile})\n\
Download a remote file specified by its @var{url} and save it as\n\
@var{localfile}.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
urlwrite (\"ftp://ftp.octave.org/pub/README\",\n\
          \"README.txt\");\n\
@end group\n\
@end example\n\
\n\
The full path of the downloaded file is returned in @var{f}.\n\
\n\
The variable @var{success} is 1 if the download was successful,\n\
otherwise it is 0 in which case @var{message} contains an error message.\n\
\n\
If no output argument is specified and an error occurs, then the error is\n\
signaled through Octave's error handling mechanism.\n\
\n\
This function uses libcurl.  Curl supports, among others, the HTTP, FTP, and\n\
FILE protocols.  Username and password may be specified in the URL, for\n\
example:\n\
\n\
@example\n\
@group\n\
urlwrite (\"http://username:password@@example.com/file.txt\",\n\
          \"file.txt\");\n\
@end group\n\
@end example\n\
\n\
GET and POST requests can be specified by @var{method} and @var{param}.\n\
The parameter @var{method} is either @samp{get} or @samp{post} and\n\
@var{param} is a cell array of parameter and value pairs.\n\
For example:\n\
\n\
@example\n\
@group\n\
urlwrite (\"http://www.google.com/search\", \"search.html\",\n\
          \"get\", @{\"query\", \"octave\"@});\n\
@end group\n\
@end example\n\
@seealso{urlread}\n\
@end deftypefn")
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
        error ("urlwrite: METHOD must be \"get\" or \"post\"");

      param = args(3).xcellstr_value ("urlwrite: parameters (PARAM) for get and post requests must be given as a cell array of strings");

      if (param.numel () % 2 == 1)
        error ("urlwrite: number of elements in PARAM must be even");
    }

  // The file should only be deleted if it doesn't initially exist, we
  // create it, and the download fails.  We use unwind_protect to do
  // it so that the deletion happens no matter how we exit the function.

  file_stat fs (filename);

  std::ofstream ofile (filename.c_str (), std::ios::out | std::ios::binary);

  if (! ofile.is_open ())
    error ("urlwrite: unable to open file");

  unwind_protect_safe frame;

  frame.add_fcn (delete_file, filename);

  url_transfer curl = url_transfer (url, ofile);

  octave_value_list retval;

  if (! curl.is_valid ())
    error ("support for URL transfers was disabled when Octave was built");

  curl.http_action (param, method);

  ofile.close ();

  if (curl.good ())
    frame.discard ();

  if (nargout > 0)
    {
      if (curl.good ())
        retval = ovl (octave_env::make_absolute (filename), true, "");
      else
        retval = ovl ("", false, curl.lasterror ());
    }

  if (nargout < 2 && ! curl.good ())
    error ("urlwrite: %s", curl.lasterror ().c_str ());

  return retval;
}

DEFUN (urlread, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {} {@var{s} =} urlread (@var{url})\n\
@deftypefnx {} {[@var{s}, @var{success}] =} urlread (@var{url})\n\
@deftypefnx {} {[@var{s}, @var{success}, @var{message}] =} urlread (@var{url})\n\
@deftypefnx {} {[@dots{}] =} urlread (@var{url}, @var{method}, @var{param})\n\
Download a remote file specified by its @var{url} and return its content\n\
in string @var{s}.\n\
\n\
For example:\n\
\n\
@example\n\
s = urlread (\"ftp://ftp.octave.org/pub/README\");\n\
@end example\n\
\n\
The variable @var{success} is 1 if the download was successful,\n\
otherwise it is 0 in which case @var{message} contains an error\n\
message.\n\
\n\
If no output argument is specified and an error occurs, then the error is\n\
signaled through Octave's error handling mechanism.\n\
\n\
This function uses libcurl.  Curl supports, among others, the HTTP, FTP, and\n\
FILE protocols.  Username and password may be specified in the URL@.  For\n\
example:\n\
\n\
@example\n\
s = urlread (\"http://user:password@@example.com/file.txt\");\n\
@end example\n\
\n\
GET and POST requests can be specified by @var{method} and @var{param}.\n\
The parameter @var{method} is either @samp{get} or @samp{post} and\n\
@var{param} is a cell array of parameter and value pairs.\n\
For example:\n\
\n\
@example\n\
@group\n\
s = urlread (\"http://www.google.com/search\", \"get\",\n\
            @{\"query\", \"octave\"@});\n\
@end group\n\
@end example\n\
@seealso{urlwrite}\n\
@end deftypefn")
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
        error ("urlread: METHOD must be \"get\" or \"post\"");

      param = args(2).xcellstr_value ("urlread: parameters (PARAM) for get and post requests must be given as a cell array of strings");

      if (param.numel () % 2 == 1)
        error ("urlread: number of elements in PARAM must be even");
    }

  std::ostringstream buf;

  url_transfer curl = url_transfer (url, buf);

  if (! curl.is_valid ())
    error ("support for URL transfers was disabled when Octave was built");

  curl.http_action (param, method);

  octave_value_list retval;

  if (nargout > 0)
    {
      // Return empty string if no error occurred.
      retval = ovl (buf.str (), curl.good (),
                    curl.good () ? "" : curl.lasterror ());
    }

  if (nargout < 2 && ! curl.good ())
    error ("urlread: %s", curl.lasterror ().c_str ());

  return retval;
}

DEFUN (__ftp__, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {} {@var{handle} =} __ftp__ (@var{host})\n\
@deftypefnx {} {@var{handle} =} __ftp__ (@var{host}, @var{username}, @var{password})\n\
Undocumented internal function\n\
@end deftypefn")
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

  curl_handle ch
    = ch_manager::make_curl_handle (host, user, passwd, octave_stdout);

  return ovl (ch.value ());
}

DEFUN (__ftp_pwd__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} __ftp_pwd__ (@var{handle})\n\
Undocumented internal function\n\
@end deftypefn")
{
  if (args.length () != 1)
    error ("__ftp_pwd__: incorrect number of arguments");

  url_transfer curl = ch_manager::get_object (args(0));

  if (! curl.is_valid ())
    error ("__ftp_pwd__: invalid ftp handle");

  return ovl (curl.pwd ());
}

DEFUN (__ftp_cwd__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} __ftp_cwd__ (@var{handle}, @var{path})\n\
Undocumented internal function\n\
@end deftypefn")
{
  int nargin = args.length ();

  if (nargin != 1 && nargin != 2)
    error ("__ftp_cwd__: incorrect number of arguments");

  std::string path = "";
  if (nargin > 1)
    path = args(1).xstring_value ("__ftp_cwd__: PATH must be a string");

  url_transfer curl = ch_manager::get_object (args(0));

  if (! curl.is_valid ())
    error ("__ftp_cwd__: invalid ftp handle");

  curl.cwd (path);

  return ovl ();
}

DEFUN (__ftp_dir__, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn {} {} __ftp_dir__ (@var{handle})\n\
Undocumented internal function\n\
@end deftypefn")
{
  if (args.length () != 1)
    error ("__ftp_dir__: incorrect number of arguments");

  url_transfer curl = ch_manager::get_object (args(0));

  if (! curl.is_valid ())
    error ("__ftp_dir__: invalid ftp handle");

  octave_value retval;

  if (nargout == 0)
    curl.dir ();
  else
    {
      string_vector sv = curl.list ();
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

              curl.get_fileinfo (sv(i), fsize, ftime, fisdir);

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

DEFUN (__ftp_ascii__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} __ftp_ascii__ (@var{handle})\n\
Undocumented internal function\n\
@end deftypefn")
{
  if (args.length () != 1)
    error ("__ftp_ascii__: incorrect number of arguments");

  url_transfer curl = ch_manager::get_object (args(0));

  if (! curl.is_valid ())
    error ("__ftp_ascii__: invalid ftp handle");

  curl.ascii ();

  return ovl ();
}

DEFUN (__ftp_binary__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} __ftp_binary__ (@var{handle})\n\
Undocumented internal function\n\
@end deftypefn")
{
  if (args.length () != 1)
    error ("__ftp_binary__: incorrect number of arguments");

  url_transfer curl = ch_manager::get_object (args(0));

  if (! curl.is_valid ())
    error ("__ftp_binary__: invalid ftp handle");

  curl.binary ();

  return ovl ();
}

DEFUN (__ftp_close__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} __ftp_close__ (@var{handle})\n\
Undocumented internal function\n\
@end deftypefn")
{
  if (args.length () != 1)
    error ("__ftp_close__: incorrect number of arguments");

  curl_handle h = ch_manager::lookup (args(0));

  if (! h.ok ())
    error ("__ftp_close__: invalid ftp handle");

  ch_manager::free (h);

  return ovl ();
}

DEFUN (__ftp_mode__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} __ftp_mode__ (@var{handle})\n\
Undocumented internal function\n\
@end deftypefn")
{
  if (args.length () != 1)
    error ("__ftp_mode__: incorrect number of arguments");

  url_transfer curl = ch_manager::get_object (args(0));

  if (! curl.is_valid ())
    error ("__ftp_binary__: invalid ftp handle");

  return ovl (curl.is_ascii () ? "ascii" : "binary");
}

DEFUN (__ftp_delete__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} __ftp_delete__ (@var{handle}, @var{path})\n\
Undocumented internal function\n\
@end deftypefn")
{
  if (args.length () != 2)
    error ("__ftp_delete__: incorrect number of arguments");

  std::string file = args(1).xstring_value ("__ftp_delete__: FILE must be a string");

  url_transfer curl = ch_manager::get_object (args(0));

  if (! curl.is_valid ())
    error ("__ftp_delete__: invalid ftp handle");

  curl.del (file);

  return ovl ();
}

DEFUN (__ftp_rmdir__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} __ftp_rmdir__ (@var{handle}, @var{path})\n\
Undocumented internal function\n\
@end deftypefn")
{
  if (args.length () != 2)
    error ("__ftp_rmdir__: incorrect number of arguments");

  std::string dir = args(1).xstring_value ("__ftp_rmdir__: DIR must be a string");

  url_transfer curl = ch_manager::get_object (args(0));

  if (! curl.is_valid ())
    error ("__ftp_rmdir__: invalid ftp handle");

  curl.rmdir (dir);

  return ovl ();
}

DEFUN (__ftp_mkdir__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} __ftp_mkdir__ (@var{handle}, @var{path})\n\
Undocumented internal function\n\
@end deftypefn")
{
  if (args.length () != 2)
    error ("__ftp_mkdir__: incorrect number of arguments");

  std::string dir = args(1).xstring_value ("__ftp_mkdir__: DIR must be a string");

  url_transfer curl = ch_manager::get_object (args(0));

  if (! curl.is_valid ())
    error ("__ftp_mkdir__: invalid ftp handle");

  curl.mkdir (dir);

  return ovl ();
}

DEFUN (__ftp_rename__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} __ftp_rename__ (@var{handle}, @var{path})\n\
Undocumented internal function\n\
@end deftypefn")
{
  if (args.length () != 3)
    error ("__ftp_rename__: incorrect number of arguments");

  std::string oldname = args(1).xstring_value ("__ftp_rename__: OLDNAME must be a string");
  std::string newname = args(2).xstring_value ("__ftp_rename__: NEWNAME must be a string");

  url_transfer curl = ch_manager::get_object (args(0));

  if (curl.is_valid ())
    error ("__ftp_rename__: invalid ftp handle");

  curl.rename (oldname, newname);

  return ovl ();
}

DEFUN (__ftp_mput__, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn {} {} __ftp_mput__ (@var{handle}, @var{files})\n\
Undocumented internal function\n\
@end deftypefn")
{
  if (args.length () != 2)
    error ("__ftp_mput__: incorrect number of arguments");

  std::string pat = args(1).xstring_value ("__ftp_mput__: PATTERN must be a string");

  url_transfer curl = ch_manager::get_object (args(0));

  if (! curl.is_valid ())
    error ("__ftp_mput__: invalid ftp handle");

  string_vector file_list;

  glob_match pattern (file_ops::tilde_expand (pat));
  string_vector files = pattern.glob ();

  for (octave_idx_type i = 0; i < files.numel (); i++)
    {
      std::string file = files(i);

      file_stat fs (file);

      if (! fs.exists ())
        error ("__ftp__mput: file does not exist");

      if (fs.is_dir ())
        {
          file_list.append (curl.mput_directory ("", file));

          if (! curl.good ())
            error ("__ftp_mput__: %s", curl.lasterror ().c_str ());
        }
      else
        {
          // FIXME: Does ascii mode need to be flagged here?
          std::ifstream ifile (file.c_str (), std::ios::in | std::ios::binary);

          if (! ifile.is_open ())
            error ("__ftp_mput__: unable to open file");

          curl.put (file, ifile);

          ifile.close ();

          if (! curl.good ())
            error ("__ftp_mput__: %s", curl.lasterror ().c_str ());

          file_list.append (file);
        }
    }

  if (nargout > 0)
    return ovl (file_list);
  else
    return ovl ();
}

DEFUN (__ftp_mget__, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {} {} __ftp_mget__ (@var{handle}, @var{pattern})\n\
@deftypefnx {} {} __ftp_mget__ (@var{handle}, @var{pattern}, @var{target})\n\
Undocumented internal function\n\
@end deftypefn")
{
  int nargin = args.length ();

  if (nargin != 2 && nargin != 3)
    error ("__ftp_mget__: incorrect number of arguments");

  std::string file = args(1).xstring_value ("__ftp_mget__: PATTERN must be a string");

  std::string target;

  if (nargin == 3 && ! args(2).is_empty ())
    target = args(2).xstring_value ("__ftp_mget__: TARGET must be a string") + file_ops::dir_sep_str ();

  url_transfer curl = ch_manager::get_object (args(0));

  if (! curl.is_valid ())
    error ("__ftp_mget__: invalid ftp handle");

  string_vector sv = curl.list ();
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

          curl.get_fileinfo (sv(i), fsize, ftime, fisdir);

          if (fisdir)
            curl.mget_directory (sv(i), target);
          else
            {
              std::ofstream ofile ((target + sv(i)).c_str (),
                                   std::ios::out |
                                   std::ios::binary);

              if (! ofile.is_open ())
                error ("__ftp_mget__: unable to open file");

              unwind_protect_safe frame;

              frame.add_fcn (delete_file, target + sv(i));

              curl.get (sv(i), ofile);

              ofile.close ();

              if (curl.good ())
                frame.discard ();
            }

          if (! curl.good ())
            error ("__ftp_mget__: %s", curl.lasterror().c_str());
        }
    }

  if (n == 0)
    error ("__ftp_mget__: file not found");

  return ovl ();
}
