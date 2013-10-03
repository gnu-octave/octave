// urlwrite and urlread, a curl front-end for octave
/*

Copyright (C) 2006-2012 Alexander Barth
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>
#include <fstream>
#include <iomanip>
#include <iostream>

#include "dir-ops.h"
#include "file-ops.h"
#include "file-stat.h"
#include "oct-env.h"
#include "glob-match.h"

#include "defun-dld.h"
#include "error.h"
#include "oct-obj.h"
#include "ov-cell.h"
#include "pager.h"
#include "oct-map.h"
#include "oct-refcount.h"
#include "unwind-prot.h"
#include "gripes.h"

#ifdef HAVE_CURL

#include <curl/curl.h>
#include <curl/curlver.h>
#include <curl/easy.h>

static int
write_data (void *buffer, size_t size, size_t nmemb, void *streamp)
{
  std::ostream& stream = *(static_cast<std::ostream*> (streamp));
  stream.write (static_cast<const char*> (buffer), size*nmemb);
  return (stream.fail () ? 0 : size * nmemb);
}

static int
read_data (void *buffer, size_t size, size_t nmemb, void *streamp)
{
  std::istream& stream = *(static_cast<std::istream*> (streamp));
  stream.read (static_cast<char*> (buffer), size*nmemb);
  if (stream.eof ())
    return stream.gcount ();
  else
    return (stream.fail () ? 0 : size * nmemb);
}

static size_t
throw_away (void *, size_t size, size_t nmemb, void *)
{
  return static_cast<size_t>(size * nmemb);
}

class curl_object
{
private:
  class curl_object_rep
  {
  public:
    curl_object_rep (void) : count (1), valid (true), ascii (false)
      {
        curl = curl_easy_init ();
        if (!curl)
          error ("can not create curl object");
      }

    ~curl_object_rep (void)
      {
        if (curl)
          curl_easy_cleanup (curl);
      }

    bool is_valid (void) const
      {
        return valid;
      }

    bool perform (bool curlerror) const
      {
        bool retval = false;
        if (!error_state)
          {
            BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

            errnum = curl_easy_perform (curl);
            if (errnum != CURLE_OK)
              {
                if (curlerror)
                  error ("%s", curl_easy_strerror (errnum));
              }
            else
              retval = true;

            END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          }
        return retval;
      }

    CURL *object (void) const
      {
        return curl;
      }

    bool is_ascii (void) const
      {
        return ascii;
      }

    bool is_binary (void) const
      {
        return !ascii;
      }

    octave_refcount<size_t> count;
    std::string host;
    std::string url;
    std::string userpwd;
    bool valid;
    bool ascii;
    mutable CURLcode errnum;

  private:
    CURL *curl;

    // No copying!

    curl_object_rep (const curl_object_rep& ov);

    curl_object_rep& operator = (const curl_object_rep&);
  };

public:

// I'd love to rewrite this as a private method of the curl_object
// class, but you can't pass the va_list from the wrapper setopt to
// the curl_easy_setopt function.
#define setopt(option, parameter) \
  { \
    CURLcode res = curl_easy_setopt (rep->object (), option, parameter); \
    if (res != CURLE_OK) \
      error ("%s", curl_easy_strerror (res)); \
  }

  curl_object (void) : rep (new curl_object_rep ())
    {
      rep->valid = false;
    }

  curl_object (const std::string& _host, const std::string& user,
               const std::string& passwd) :
    rep (new curl_object_rep ())
    {
      rep->host = _host;
      init (user, passwd, std::cin, octave_stdout);

      rep->url = "ftp://" + _host;
      setopt (CURLOPT_URL, rep->url.c_str ());

      // Setup the link, with no transfer
      if (!error_state)
        perform ();
    }

  curl_object (const std::string& url, const std::string& method,
               const Cell& param, std::ostream& os, bool& retval) :
    rep (new curl_object_rep ())
    {
      retval = false;

      init ("", "", std::cin, os);

      setopt (CURLOPT_NOBODY, 0);

      // Restore the default HTTP request method to GET after setting
      // NOBODY to true and back to false.  This is needed for backward
      // compatibility with versions of libcurl < 7.18.2.
      setopt (CURLOPT_HTTPGET, 1);

      // Don't need to store the parameters here as we can't change
      // the URL after the object is created
      std::string query_string = form_query_string (param);

      if (method == "get")
        {
          query_string = url + "?" + query_string;
          setopt (CURLOPT_URL, query_string.c_str ());
        }
      else if (method == "post")
        {
          setopt (CURLOPT_URL, url.c_str ());
          setopt (CURLOPT_POSTFIELDS, query_string.c_str ());
        }
      else
        setopt (CURLOPT_URL, url.c_str ());

      if (!error_state)
        retval = perform (false);
    }

  curl_object (const curl_object& h) : rep (h.rep)
    {
      rep->count++;
    }

  ~curl_object (void)
    {
      if (--rep->count == 0)
        delete rep;
    }

  curl_object& operator = (const curl_object& h)
    {
      if (this != &h)
        {
          if (--rep->count == 0)
            delete rep;

          rep = h.rep;
          rep->count++;
        }
      return *this;
    }

  bool is_valid (void) const
    {
      return rep->is_valid ();
    }

  std::string lasterror (void) const
    {
      return std::string (curl_easy_strerror (rep->errnum));
    }

  void set_ostream (std::ostream& os) const
    {
      setopt (CURLOPT_WRITEDATA, static_cast<void*> (&os));
    }

  void set_istream (std::istream& is) const
    {
      setopt (CURLOPT_READDATA, static_cast<void*> (&is));
    }

  void ascii (void) const
    {
      setopt (CURLOPT_TRANSFERTEXT, 1);
      rep->ascii = true;
    }

  void binary (void) const
    {
      setopt (CURLOPT_TRANSFERTEXT, 0);
      rep->ascii = false;
    }

  bool is_ascii (void) const
    {
      return rep->is_ascii ();
    }

  bool is_binary (void) const
    {
      return rep->is_binary ();
    }

  void cwd (const std::string& path) const
    {
      struct curl_slist *slist = 0;
      std::string cmd = "cwd " + path;
      slist = curl_slist_append (slist, cmd.c_str ());
      setopt (CURLOPT_POSTQUOTE, slist);
      if (! error_state)
        perform ();
      setopt (CURLOPT_POSTQUOTE, 0);
      curl_slist_free_all (slist);
    }

  void del (const std::string& file) const
    {
      struct curl_slist *slist = 0;
      std::string cmd = "dele " + file;
      slist = curl_slist_append (slist, cmd.c_str ());
      setopt (CURLOPT_POSTQUOTE, slist);
      if (! error_state)
        perform ();
      setopt (CURLOPT_POSTQUOTE, 0);
      curl_slist_free_all (slist);
    }

  void rmdir (const std::string& path) const
    {
      struct curl_slist *slist = 0;
      std::string cmd = "rmd " + path;
      slist = curl_slist_append (slist, cmd.c_str ());
      setopt (CURLOPT_POSTQUOTE, slist);
      if (! error_state)
        perform ();
      setopt (CURLOPT_POSTQUOTE, 0);
      curl_slist_free_all (slist);
    }

  bool mkdir (const std::string& path, bool curlerror = true) const
    {
      bool retval = false;
      struct curl_slist *slist = 0;
      std::string cmd = "mkd " + path;
      slist = curl_slist_append (slist, cmd.c_str ());
      setopt (CURLOPT_POSTQUOTE, slist);
      if (! error_state)
        retval = perform (curlerror);
      setopt (CURLOPT_POSTQUOTE, 0);
      curl_slist_free_all (slist);
      return retval;
    }

  void rename (const std::string& oldname, const std::string& newname) const
    {
      struct curl_slist *slist = 0;
      std::string cmd = "rnfr " + oldname;
      slist = curl_slist_append (slist, cmd.c_str ());
      cmd = "rnto " + newname;
      slist = curl_slist_append (slist, cmd.c_str ());
      setopt (CURLOPT_POSTQUOTE, slist);
      if (! error_state)
        perform ();
      setopt (CURLOPT_POSTQUOTE, 0);
      curl_slist_free_all (slist);
    }

  void put (const std::string& file, std::istream& is) const
    {
      rep->url = "ftp://" + rep->host + "/" + file;
      setopt (CURLOPT_URL, rep->url.c_str ());
      setopt (CURLOPT_UPLOAD, 1);
      setopt (CURLOPT_NOBODY, 0);
      set_istream (is);
      if (! error_state)
        perform ();
      set_istream (std::cin);
      setopt (CURLOPT_NOBODY, 1);
      setopt (CURLOPT_UPLOAD, 0);
      rep->url = "ftp://" + rep->host;
      setopt (CURLOPT_URL, rep->url.c_str ());
    }

  void get (const std::string& file, std::ostream& os) const
    {
      rep->url = "ftp://" + rep->host + "/" + file;
      setopt (CURLOPT_URL, rep->url.c_str ());
      setopt (CURLOPT_NOBODY, 0);
      set_ostream (os);
      if (! error_state)
        perform ();
      set_ostream (octave_stdout);
      setopt (CURLOPT_NOBODY, 1);
      rep->url = "ftp://" + rep->host;
      setopt (CURLOPT_URL, rep->url.c_str ());
    }

  void dir (void) const
    {
      rep->url = "ftp://" + rep->host + "/";
      setopt (CURLOPT_URL, rep->url.c_str ());
      setopt (CURLOPT_NOBODY, 0);
      if (! error_state)
        perform ();
      setopt (CURLOPT_NOBODY, 1);
      rep->url = "ftp://" + rep->host;
      setopt (CURLOPT_URL, rep->url.c_str ());
    }

  string_vector list (void) const
    {
      std::ostringstream buf;
      rep->url = "ftp://" + rep->host + "/";
      setopt (CURLOPT_WRITEDATA, static_cast<void*> (&buf));
      setopt (CURLOPT_URL, rep->url.c_str ());
      setopt (CURLOPT_DIRLISTONLY, 1);
      setopt (CURLOPT_NOBODY, 0);
      if (! error_state)
        perform ();
      setopt (CURLOPT_NOBODY, 1);
      rep->url = "ftp://" + rep->host;
      setopt (CURLOPT_WRITEDATA, static_cast<void*> (&octave_stdout));
      setopt (CURLOPT_DIRLISTONLY, 0);
      setopt (CURLOPT_URL, rep->url.c_str ());

      // Count number of directory entries
      std::string str = buf.str ();
      octave_idx_type n = 0;
      size_t pos = 0;
      while (true)
        {
          pos = str.find_first_of ('\n', pos);
          if (pos == std::string::npos)
            break;
          pos++;
          n++;
        }
      string_vector retval (n);
      pos = 0;
      for (octave_idx_type i = 0; i < n; i++)
        {
          size_t newpos = str.find_first_of ('\n', pos);
          if (newpos == std::string::npos)
            break;

          retval(i) = str.substr(pos, newpos - pos);
          pos = newpos + 1;
        }
      return retval;
    }

  void get_fileinfo (const std::string& filename, double& filesize,
                     time_t& filetime, bool& fileisdir) const
    {
      std::string path = pwd ();

      rep->url = "ftp://" + rep->host + "/" + path + "/" + filename;
      setopt (CURLOPT_URL, rep->url.c_str ());
      setopt (CURLOPT_FILETIME, 1);
      setopt (CURLOPT_HEADERFUNCTION, throw_away);
      setopt (CURLOPT_WRITEFUNCTION, throw_away);

      // FIXME
      // The MDTM command fails for a directory on the servers I tested
      // so this is a means of testing for directories. It also means
      // I can't get the date of directories!
      if (! error_state)
        {
          if (! perform (false))
            {
              fileisdir = true;
              filetime = -1;
              filesize = 0;
            }
          else
            {
              fileisdir = false;
              time_t ft;
              curl_easy_getinfo (rep->object (), CURLINFO_FILETIME, &ft);
              filetime = ft;
              double fs;
              curl_easy_getinfo (rep->object (),
                                 CURLINFO_CONTENT_LENGTH_DOWNLOAD, &fs);
              filesize = fs;
            }
        }

      setopt (CURLOPT_WRITEFUNCTION, write_data);
      setopt (CURLOPT_HEADERFUNCTION, 0);
      setopt (CURLOPT_FILETIME, 0);
      rep->url = "ftp://" + rep->host;
      setopt (CURLOPT_URL, rep->url.c_str ());

      // The MDTM command seems to reset the path to the root with the
      // servers I tested with, so cd again into the correct path. Make
      // the path absolute so that this will work even with servers that
      // don't end up in the root after an MDTM command.
      cwd ("/" + path);
    }

  std::string pwd (void) const
    {
      struct curl_slist *slist = 0;
      std::string retval;
      std::ostringstream buf;

      slist = curl_slist_append (slist, "pwd");
      setopt (CURLOPT_POSTQUOTE, slist);
      setopt (CURLOPT_HEADERFUNCTION, write_data);
      setopt (CURLOPT_WRITEHEADER, static_cast<void *>(&buf));

      if (! error_state)
        {
          perform ();
          retval = buf.str ();

          // Can I assume that the path is alway in "" on the last line
          size_t pos2 = retval.rfind ('"');
          size_t pos1 = retval.rfind ('"', pos2 - 1);
          retval = retval.substr (pos1 + 1, pos2 - pos1 - 1);
        }
      setopt (CURLOPT_HEADERFUNCTION, 0);
      setopt (CURLOPT_WRITEHEADER, 0);
      setopt (CURLOPT_POSTQUOTE, 0);
      curl_slist_free_all (slist);

      return retval;
    }

  bool perform (bool curlerror = true) const
    {
      return rep->perform (curlerror);
    }

private:
  curl_object_rep *rep;

  std::string form_query_string (const Cell& param)
    {
      std::ostringstream query;

      for (int i = 0; i < param.numel (); i += 2)
        {
          std::string name = param(i).string_value ();
          std::string text = param(i+1).string_value ();

          // Encode strings.
          char *enc_name = curl_easy_escape (rep->object (), name.c_str (),
                                             name.length ());
          char *enc_text = curl_easy_escape (rep->object (), text.c_str (),
                                             text.length ());

          query << enc_name << "=" << enc_text;

          curl_free (enc_name);
          curl_free (enc_text);

          if (i < param.numel ()-1)
            query << "&";
        }

      query.flush ();

      return query.str ();
    }

  void init (const std::string& user, const std::string& passwd,
             std::istream& is, std::ostream& os)
    {
      // No data transfer by default
      setopt (CURLOPT_NOBODY, 1);

      // Set the username and password
      rep->userpwd = user;
      if (! passwd.empty ())
        rep->userpwd += ":" + passwd;
      if (! rep->userpwd.empty ())
        setopt (CURLOPT_USERPWD, rep->userpwd.c_str ());

      // Define our callback to get called when there's data to be written.
      setopt (CURLOPT_WRITEFUNCTION, write_data);

      // Set a pointer to our struct to pass to the callback.
      setopt (CURLOPT_WRITEDATA, static_cast<void*> (&os));

      // Define our callback to get called when there's data to be read
      setopt (CURLOPT_READFUNCTION, read_data);

      // Set a pointer to our struct to pass to the callback.
      setopt (CURLOPT_READDATA, static_cast<void*> (&is));

      // Follow redirects.
      setopt (CURLOPT_FOLLOWLOCATION, true);

      // Don't use EPSV since connecting to sites that don't support it
      // will hang for some time (3 minutes?) before moving on to try PASV
      // instead.
      setopt (CURLOPT_FTP_USE_EPSV, false);

      setopt (CURLOPT_NOPROGRESS, true);
      setopt (CURLOPT_FAILONERROR, true);

      setopt (CURLOPT_POSTQUOTE, 0);
      setopt (CURLOPT_QUOTE, 0);
    }

#undef setopt
};

class
curl_handles
{
public:

  typedef std::map<std::string, curl_object>::iterator iterator;
  typedef std::map<std::string, curl_object>::const_iterator const_iterator;

  curl_handles (void) : map ()
   {
     curl_global_init (CURL_GLOBAL_DEFAULT);
   }

  ~curl_handles (void)
    {
      // Remove the elements of the map explicitly as they should
      // be deleted before the call to curl_global_cleanup
      map.erase (begin (), end ());

      curl_global_cleanup ();
    }

  iterator begin (void) { return iterator (map.begin ()); }
  const_iterator begin (void) const { return const_iterator (map.begin ()); }

  iterator end (void) { return iterator (map.end ()); }
  const_iterator end (void) const { return const_iterator (map.end ()); }

  iterator seek (const std::string& k) { return map.find (k); }
  const_iterator seek (const std::string& k) const { return map.find (k); }

  std::string key (const_iterator p) const { return p->first; }

  curl_object& contents (const std::string& k)
    {
      return map[k];
    }

  curl_object contents (const std::string& k) const
    {
      const_iterator p = seek (k);
      return p != end () ? p->second : curl_object ();
    }

  curl_object& contents (iterator p)
    { return p->second; }

  curl_object contents (const_iterator p) const
    { return p->second; }

  void del (const std::string& k)
    {
      iterator p = map.find (k);

      if (p != map.end ())
        map.erase (p);
    }

private:
  std::map<std::string, curl_object> map;
};

static curl_handles handles;

static void
cleanup_urlwrite (std::string filename)
{
  octave_unlink (filename);
}

static void
reset_path (const curl_object& curl)
{
  curl.cwd ("..");
}

static void
delete_file (std::string file)
{
  octave_unlink (file);
}
#endif

DEFUN_DLD (urlwrite, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {} urlwrite (@var{url}, @var{localfile})\n\
@deftypefnx {Loadable Function} {@var{f} =} urlwrite (@var{url}, @var{localfile})\n\
@deftypefnx {Loadable Function} {[@var{f}, @var{success}] =} urlwrite (@var{url}, @var{localfile})\n\
@deftypefnx {Loadable Function} {[@var{f}, @var{success}, @var{message}] =} urlwrite (@var{url}, @var{localfile})\n\
Download a remote file specified by its @var{url} and save it as\n\
@var{localfile}.  For example:\n\
\n\
@example\n\
@group\n\
urlwrite (\"ftp://ftp.octave.org/pub/octave/README\",\n\
          \"README.txt\");\n\
@end group\n\
@end example\n\
\n\
The full path of the downloaded file is returned in @var{f}.  The\n\
variable @var{success} is 1 if the download was successful,\n\
otherwise it is 0 in which case @var{message} contains an error\n\
message.  If no output argument is specified and an error occurs,\n\
then the error is signaled through Octave's error handling mechanism.\n\
\n\
This function uses libcurl.  Curl supports, among others, the HTTP,\n\
FTP and FILE protocols.  Username and password may be specified in\n\
the URL, for example:\n\
\n\
@example\n\
@group\n\
urlwrite (\"http://username:password@@example.com/file.txt\",\n\
          \"file.txt\");\n\
@end group\n\
@end example\n\
\n\
GET and POST requests can be specified by @var{method} and @var{param}.\n\
The parameter @var{method} is either @samp{get} or @samp{post}\n\
and @var{param} is a cell array of parameter and value pairs.\n\
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
  octave_value_list retval;

#ifdef HAVE_CURL

  int nargin = args.length ();

  // verify arguments
  if (nargin != 2 && nargin != 4)
    {
      print_usage ();
      return retval;
    }

  std::string url = args(0).string_value ();

  if (error_state)
    {
      error ("urlwrite: URL must be a character string");
      return retval;
    }

  // name to store the file if download is succesful
  std::string filename = args(1).string_value ();

  if (error_state)
    {
      error ("urlwrite: LOCALFILE must be a character string");
      return retval;
    }

  std::string method;
  Cell param; // empty cell array

  if (nargin == 4)
    {
      method = args(2).string_value ();

      if (error_state)
        {
          error ("urlwrite: METHOD must be \"get\" or \"post\"");
          return retval;
        }

      if (method != "get" && method != "post")
        {
          error ("urlwrite: METHOD must be \"get\" or \"post\"");
          return retval;
        }

      param = args(3).cell_value ();

      if (error_state)
        {
          error ("urlwrite: parameters (PARAM) for get and post requests must be given as a cell");
          return retval;
        }


      if (param.numel () % 2 == 1 )
        {
          error ("urlwrite: number of elements in PARAM must be even");
          return retval;
        }
    }

  // The file should only be deleted if it doesn't initially exist, we
  // create it, and the download fails.  We use unwind_protect to do
  // it so that the deletion happens no matter how we exit the function.

  file_stat fs (filename);

  std::ofstream ofile (filename.c_str (), std::ios::out | std::ios::binary);

  if (! ofile.is_open ())
    {
      error ("urlwrite: unable to open file");
      return retval;
    }

  unwind_protect_safe frame;

  frame.add_fcn (cleanup_urlwrite, filename);

  bool ok;
  curl_object curl = curl_object (url, method, param, ofile, ok);

  ofile.close ();

  if (!error_state)
    frame.discard ();
  else
    frame.run ();

  if (nargout > 0)
    {
      if (ok)
        {
          retval(2) = std::string ();
          retval(1) = true;
          retval(0) = octave_env::make_absolute (filename);
        }
      else
        {
          retval(2) = curl.lasterror ();
          retval(1) = false;
          retval(0) = std::string ();
        }
    }

  if (nargout < 2 && ! ok)
    error ("urlwrite: curl: %s", curl.lasterror ().c_str ());

#else
  gripe_disabled_feature ("urlwrite", "urlwrite");
#endif

  return retval;
}

DEFUN_DLD (urlread, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{s} =} urlread (@var{url})\n\
@deftypefnx {Loadable Function} {[@var{s}, @var{success}] =} urlread (@var{url})\n\
@deftypefnx {Loadable Function} {[@var{s}, @var{success}, @var{message}] =} urlread (@var{url})\n\
@deftypefnx {Loadable Function} {[@dots{}] =} urlread (@var{url}, @var{method}, @var{param})\n\
Download a remote file specified by its @var{url} and return its content\n\
in string @var{s}.  For example:\n\
\n\
@example\n\
s = urlread (\"ftp://ftp.octave.org/pub/octave/README\");\n\
@end example\n\
\n\
The variable @var{success} is 1 if the download was successful,\n\
otherwise it is 0 in which case @var{message} contains an error\n\
message.  If no output argument is specified and an error occurs,\n\
then the error is signaled through Octave's error handling mechanism.\n\
\n\
This function uses libcurl.  Curl supports, among others, the HTTP,\n\
FTP and FILE protocols.  Username and password may be specified in the\n\
URL@.  For example:\n\
\n\
@example\n\
s = urlread (\"http://user:password@@example.com/file.txt\");\n\
@end example\n\
\n\
GET and POST requests can be specified by @var{method} and @var{param}.\n\
The parameter @var{method} is either @samp{get} or @samp{post}\n\
and @var{param} is a cell array of parameter and value pairs.\n\
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
  // Octave's return value
  octave_value_list retval;

#ifdef HAVE_CURL

  int nargin = args.length ();

  // verify arguments
  if (nargin != 1  && nargin != 3)
    {
      print_usage ();
      return retval;
    }

  std::string url = args(0).string_value ();

  if (error_state)
    {
      error ("urlread: URL must be a character string");
      return retval;
    }

  std::string method;
  Cell param; // empty cell array

  if (nargin == 3)
    {
      method = args(1).string_value ();

      if (error_state)
        {
          error ("urlread: METHOD must be \"get\" or \"post\"");
          return retval;
        }

      if (method != "get" && method != "post")
        {
          error ("urlread: METHOD must be \"get\" or \"post\"");
          return retval;
        }

      param = args(2).cell_value ();

      if (error_state)
        {
          error ("urlread: parameters (PARAM) for get and post requests must be given as a cell");
          return retval;
        }

      if (param.numel () % 2 == 1 )
        {
          error ("urlread: number of elements in PARAM must be even");
          return retval;
        }
    }

  std::ostringstream buf;

  bool ok;
  curl_object curl = curl_object (url, method, param, buf, ok);

  if (nargout > 0)
    {
      // Return empty string if no error occured.
      retval(2) = ok ? "" : curl.lasterror ();
      retval(1) = ok;
      retval(0) = buf.str ();
    }

  if (nargout < 2 && ! ok)
    error ("urlread: curl: %s", curl.lasterror().c_str());

#else
  gripe_disabled_feature ("urlread", "urlread");
#endif

  return retval;
}

DEFUN_DLD (__ftp__, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {} __ftp__ (@var{handle}, @var{host})\n\
@deftypefnx {Loadable Function} {} __ftp__ (@var{handle}, @var{host}, @var{username}, @var{password})\n\
Undocumented internal function\n\
@end deftypefn")
{
#ifdef HAVE_CURL
  int nargin = args.length ();
  std::string handle;
  std::string host;
  std::string user = "anonymous";
  std::string passwd = "";

  if (nargin < 2 || nargin > 4)
    error ("incorrect number of arguments");
  else
    {
      handle = args(0).string_value ();
      host = args(1).string_value ();

      if (nargin > 1)
        user = args(2).string_value ();

      if (nargin > 2)
        passwd = args(3).string_value ();

      if (!error_state)
        {
          handles.contents (handle) = curl_object (host, user, passwd);

          if (error_state)
            handles.del (handle);
        }
    }
#else
  gripe_disabled_feature ("__ftp__", "FTP");
#endif

  return octave_value ();
}

DEFUN_DLD (__ftp_pwd__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __ftp_pwd__ (@var{handle})\n\
Undocumented internal function\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_CURL
  int nargin = args.length ();

  if (nargin != 1)
    error ("__ftp_pwd__: incorrect number of arguments");
  else
    {
      std::string handle = args(0).string_value ();

      if (!error_state)
        {
          const curl_object curl = handles.contents (handle);

          if (curl.is_valid ())
            retval = curl.pwd ();
          else
            error ("__ftp_pwd__: invalid ftp handle");
        }
    }
#else
  gripe_disabled_feature ("__ftp_pwd__", "FTP");
#endif

  return retval;
}

DEFUN_DLD (__ftp_cwd__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __ftp_cwd__ (@var{handle}, @var{path})\n\
Undocumented internal function\n\
@end deftypefn")
{
#ifdef HAVE_CURL
  int nargin = args.length ();

  if (nargin != 1 && nargin != 2)
    error ("__ftp_cwd__: incorrect number of arguments");
  else
    {
      std::string handle = args(0).string_value ();
      std::string path = "";

      if (nargin > 1)
        path  = args(1).string_value ();

      if (!error_state)
        {
          const curl_object curl = handles.contents (handle);

          if (curl.is_valid ())
            curl.cwd (path);
          else
            error ("__ftp_cwd__: invalid ftp handle");
        }
    }
#else
  gripe_disabled_feature ("__ftp_cwd__", "FTP");
#endif

  return octave_value ();
}

DEFUN_DLD (__ftp_dir__, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __ftp_dir__ (@var{handle})\n\
Undocumented internal function\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_CURL
  int nargin = args.length ();

  if (nargin != 1)
    error ("__ftp_dir__: incorrect number of arguments");
  else
    {
      std::string handle = args(0).string_value ();

      if (!error_state)
        {
          const curl_object curl = handles.contents (handle);

          if (curl.is_valid ())
            {
              if (nargout == 0)
                curl.dir ();
              else
                {
                  string_vector sv = curl.list ();
                  octave_idx_type n = sv.length ();
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
            }
          else
            error ("__ftp_dir__: invalid ftp handle");
        }
    }
#else
  gripe_disabled_feature ("__ftp_dir__", "FTP");
#endif

  return retval;
}

DEFUN_DLD (__ftp_ascii__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __ftp_ascii__ (@var{handle})\n\
Undocumented internal function\n\
@end deftypefn")
{
#ifdef HAVE_CURL
  int nargin = args.length ();

  if (nargin != 1)
    error ("__ftp_ascii__: incorrect number of arguments");
  else
    {
      std::string handle = args(0).string_value ();

      if (!error_state)
        {
          const curl_object curl = handles.contents (handle);

          if (curl.is_valid ())
            curl.ascii ();
          else
            error ("__ftp_ascii__: invalid ftp handle");
        }
    }
#else
  gripe_disabled_feature ("__ftp_ascii__", "FTP");
#endif

  return octave_value ();
}

DEFUN_DLD (__ftp_binary__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __ftp_binary__ (@var{handle})\n\
Undocumented internal function\n\
@end deftypefn")
{
#ifdef HAVE_CURL
  int nargin = args.length ();

  if (nargin != 1)
    error ("__ftp_binary__: incorrect number of arguments");
  else
    {
      std::string handle = args(0).string_value ();

      if (!error_state)
        {
          const curl_object curl = handles.contents (handle);

          if (curl.is_valid ())
            curl.binary ();
          else
            error ("__ftp_binary__: invalid ftp handle");
        }
    }
#else
  gripe_disabled_feature ("__ftp_binary__", "FTP");
#endif

  return octave_value ();
}

DEFUN_DLD (__ftp_close__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __ftp_close__ (@var{handle})\n\
Undocumented internal function\n\
@end deftypefn")
{
#ifdef HAVE_CURL
  int nargin = args.length ();

  if (nargin != 1)
    error ("__ftp_close__: incorrect number of arguments");
  else
    {
      std::string handle = args(0).string_value ();

      if (! error_state)
        handles.del (handle);
    }
#else
  gripe_disabled_feature ("__ftp_close__", "FTP");
#endif

  return octave_value ();
}

DEFUN_DLD (__ftp_mode__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __ftp_mode__ (@var{handle})\n\
Undocumented internal function\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_CURL
  int nargin = args.length ();

  if (nargin != 1)
    error ("__ftp_mode__: incorrect number of arguments");
  else
    {
      std::string handle = args(0).string_value ();


      if (! error_state)
        {
          const curl_object curl = handles.contents (handle);

          if (curl.is_valid ())
            retval = (curl.is_ascii () ? "ascii" : "binary");
          else
            error ("__ftp_binary__: invalid ftp handle");
        }
    }
#else
  gripe_disabled_feature ("__ftp_mode__", "FTP");
#endif

  return retval;
}

DEFUN_DLD (__ftp_delete__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __ftp_delete__ (@var{handle}, @var{path})\n\
Undocumented internal function\n\
@end deftypefn")
{
#ifdef HAVE_CURL
  int nargin = args.length ();

  if (nargin != 2)
    error ("__ftp_delete__: incorrect number of arguments");
  else
    {
      std::string handle = args(0).string_value ();
      std::string file = args(1).string_value ();

      if (!error_state)
        {
          const curl_object curl = handles.contents (handle);

          if (curl.is_valid ())
            curl.del (file);
          else
            error ("__ftp_delete__: invalid ftp handle");
        }
    }
#else
  gripe_disabled_feature ("__ftp_delete__", "FTP");
#endif

  return octave_value ();
}

DEFUN_DLD (__ftp_rmdir__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __ftp_rmdir__ (@var{handle}, @var{path})\n\
Undocumented internal function\n\
@end deftypefn")
{
#ifdef HAVE_CURL
  int nargin = args.length ();

  if (nargin != 2)
    error ("__ftp_rmdir__: incorrect number of arguments");
  else
    {
      std::string handle = args(0).string_value ();
      std::string dir = args(1).string_value ();

      if (!error_state)
        {
          const curl_object curl = handles.contents (handle);

          if (curl.is_valid ())
            curl.rmdir (dir);
          else
            error ("__ftp_rmdir__: invalid ftp handle");
        }
    }
#else
  gripe_disabled_feature ("__ftp_rmdir__", "FTP");
#endif

  return octave_value ();
}

DEFUN_DLD (__ftp_mkdir__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __ftp_mkdir__ (@var{handle}, @var{path})\n\
Undocumented internal function\n\
@end deftypefn")
{
#ifdef HAVE_CURL
  int nargin = args.length ();

  if (nargin != 2)
    error ("__ftp_mkdir__: incorrect number of arguments");
  else
    {
      std::string handle = args(0).string_value ();
      std::string dir = args(1).string_value ();

      if (!error_state)
        {
          const curl_object curl = handles.contents (handle);

          if (curl.is_valid ())
            curl.mkdir (dir);
          else
            error ("__ftp_mkdir__: invalid ftp handle");
        }
    }
#else
  gripe_disabled_feature ("__ftp_mkdir__", "FTP");
#endif

  return octave_value ();
}

DEFUN_DLD (__ftp_rename__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __ftp_rename__ (@var{handle}, @var{path})\n\
Undocumented internal function\n\
@end deftypefn")
{
#ifdef HAVE_CURL
  int nargin = args.length ();

  if (nargin != 3)
    error ("__ftp_rename__: incorrect number of arguments");
  else
    {
      std::string handle = args(0).string_value ();
      std::string oldname = args(1).string_value ();
      std::string newname = args(2).string_value ();

      if (!error_state)
        {
          const curl_object curl = handles.contents (handle);

          if (curl.is_valid ())
            curl.rename (oldname, newname);
          else
            error ("__ftp_rename__: invalid ftp handle");
        }
    }
#else
  gripe_disabled_feature ("__ftp_rename__", "FTP");
#endif

  return octave_value ();
}

#ifdef HAVE_CURL
static string_vector
mput_directory (const curl_object& curl, const std::string& base,
                const std::string& dir)
{
  string_vector retval;

  if (! curl.mkdir (dir, false))
    warning ("__ftp_mput__: can not create the remote directory ""%s""",
             (base.length () == 0 ? dir : base +
              file_ops::dir_sep_str () + dir).c_str ());

  curl.cwd (dir);

  if (! error_state)
    {
      unwind_protect_safe frame;

      frame.add_fcn (reset_path, curl);

      std::string realdir = base.length () == 0 ? dir : base +
                         file_ops::dir_sep_str () + dir;

      dir_entry dirlist (realdir);

      if (dirlist)
        {
          string_vector files = dirlist.read ();

          for (octave_idx_type i = 0; i < files.length (); i++)
            {
              std::string file = files (i);

              if (file == "." || file == "..")
                continue;

              std::string realfile = realdir + file_ops::dir_sep_str () + file;
              file_stat fs (realfile);

              if (! fs.exists ())
                {
                  error ("__ftp__mput: file ""%s"" does not exist",
                         realfile.c_str ());
                  break;
                }

              if (fs.is_dir ())
                {
                  retval.append (mput_directory (curl, realdir, file));

                  if (error_state)
                    break;
                }
              else
                {
                  // FIXME Does ascii mode need to be flagged here?
                  std::ifstream ifile (realfile.c_str (), std::ios::in |
                                       std::ios::binary);

                  if (! ifile.is_open ())
                    {
                      error ("__ftp_mput__: unable to open file ""%s""",
                             realfile.c_str ());
                      break;
                    }

                  curl.put (file, ifile);

                  ifile.close ();

                  if (error_state)
                    break;

                  retval.append (realfile);
                }
            }
        }
      else
        error ("__ftp_mput__: can not read the directory ""%s""",
               realdir.c_str ());
    }

  return retval;
}
#endif

DEFUN_DLD (__ftp_mput__, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __ftp_mput__ (@var{handle}, @var{files})\n\
Undocumented internal function\n\
@end deftypefn")
{
  string_vector retval;

#ifdef HAVE_CURL
  int nargin = args.length ();

  if (nargin != 2)
    error ("__ftp_mput__: incorrect number of arguments");
  else
    {
      std::string handle = args(0).string_value ();
      std::string pat = args(1).string_value ();

      if (!error_state)
        {
          const curl_object curl = handles.contents (handle);

          if (curl.is_valid ())
            {
              glob_match pattern (file_ops::tilde_expand (pat));
              string_vector files = pattern.glob ();

              for (octave_idx_type i = 0; i < files.length (); i++)
                {
                  std::string file = files (i);

                  file_stat fs (file);

                  if (! fs.exists ())
                    {
                      error ("__ftp__mput: file does not exist");
                      break;
                    }

                  if (fs.is_dir ())
                    {
                      retval.append (mput_directory (curl, "", file));
                      if (error_state)
                        break;
                    }
                  else
                    {
                      // FIXME Does ascii mode need to be flagged here?
                      std::ifstream ifile (file.c_str (), std::ios::in |
                                           std::ios::binary);

                      if (! ifile.is_open ())
                        {
                          error ("__ftp_mput__: unable to open file");
                          break;
                        }

                      curl.put (file, ifile);

                      ifile.close ();

                      if (error_state)
                        break;

                      retval.append (file);
                    }
                }
            }
          else
            error ("__ftp_mput__: invalid ftp handle");
        }
    }
#else
  gripe_disabled_feature ("__ftp_mput__", "FTP");
#endif

  return (nargout > 0 ? octave_value (retval) : octave_value ());
}

#ifdef HAVE_CURL
static void
getallfiles (const curl_object& curl, const std::string& dir,
             const std::string& target)
{
  std::string sep = file_ops::dir_sep_str ();
  file_stat fs (dir);

  if (!fs || !fs.is_dir ())
    {
      std::string msg;
      int status = octave_mkdir (dir, 0777, msg);

      if (status < 0)
        error ("__ftp_mget__: can't create directory %s%s%s. %s",
               target.c_str (), sep.c_str (), dir.c_str (), msg.c_str ());
    }

  if (! error_state)
    {
      curl.cwd (dir);

      if (! error_state)
        {
          unwind_protect_safe frame;

          frame.add_fcn (reset_path, curl);

          string_vector sv = curl.list ();

          for (octave_idx_type i = 0; i < sv.length (); i++)
            {
              time_t ftime;
              bool fisdir;
              double fsize;

              curl.get_fileinfo (sv(i), fsize, ftime, fisdir);

              if (fisdir)
                getallfiles (curl, sv(i), target + dir + sep);
              else
                {
                  std::string realfile = target + dir + sep + sv(i);
                  std::ofstream ofile (realfile.c_str (),
                                       std::ios::out |
                                       std::ios::binary);

                  if (! ofile.is_open ())
                    {
                      error ("__ftp_mget__: unable to open file");
                      break;
                    }

                  unwind_protect_safe frame2;

                  frame2.add_fcn (delete_file, realfile);

                  curl.get (sv(i), ofile);

                  ofile.close ();

                  if (!error_state)
                    frame2.discard ();
                  else
                    frame2.run ();
                }

              if (error_state)
                break;
            }
        }
    }
}
#endif

DEFUN_DLD (__ftp_mget__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __ftp_mget__ (@var{handle}, @var{files})\n\
Undocumented internal function\n\
@end deftypefn")
{
#ifdef HAVE_CURL
  int nargin = args.length ();

  if (nargin != 2 && nargin != 3)
    error ("__ftp_mget__: incorrect number of arguments");
  else
    {
      std::string handle = args(0).string_value ();
      std::string file = args(1).string_value ();
      std::string target;

      if (nargin == 3)
        target = args(2).string_value () + file_ops::dir_sep_str ();

      if (! error_state)
        {
          const curl_object curl = handles.contents (handle);

          if (curl.is_valid ())
            {
              string_vector sv = curl.list ();
              octave_idx_type n = 0;
              glob_match pattern (file);

              for (octave_idx_type i = 0; i < sv.length (); i++)
                {
                  if (pattern.match (sv(i)))
                    {
                      n++;

                      time_t ftime;
                      bool fisdir;
                      double fsize;

                      curl.get_fileinfo (sv(i), fsize, ftime, fisdir);

                      if (fisdir)
                        getallfiles (curl, sv(i), target);
                      else
                        {
                          std::ofstream ofile ((target + sv(i)).c_str (),
                                               std::ios::out |
                                               std::ios::binary);

                          if (! ofile.is_open ())
                            {
                              error ("__ftp_mget__: unable to open file");
                              break;
                            }

                          unwind_protect_safe frame;

                          frame.add_fcn (delete_file, target + sv(i));

                          curl.get (sv(i), ofile);

                          ofile.close ();

                          if (!error_state)
                            frame.discard ();
                          else
                            frame.run ();
                        }

                      if (error_state)
                        break;
                    }
                }
              if (n == 0)
                error ("__ftp_mget__: file not found");
            }
        }
    }
#else
  gripe_disabled_feature ("__ftp_mget__", "FTP");
#endif

  return octave_value ();
}
