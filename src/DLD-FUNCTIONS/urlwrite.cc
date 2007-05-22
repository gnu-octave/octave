// urlwrite and urlread, a curl front-end for octave
/*

Copyright (C) 2006 Alexander Barth

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

// Author: Alexander Barth <abarth@marine.usf.edu>
// Adapted-By: jwe

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>
#include <fstream>
#include <iomanip>

#include "oct-env.h"

#include "defun-dld.h"
#include "error.h"
#include "oct-obj.h"
#include "ov-cell.h"
#include "pager.h"

#if defined (HAVE_CURL)

#include <curl/curl.h>
#include <curl/types.h>
#include <curl/easy.h>

// Write callback function for curl.

int
write_data (void *buffer, size_t size, size_t nmemb, void *streamp)
{
  // *stream is actually an ostream object.
  std::ostream& stream = *(static_cast<std::ostream*> (streamp));
  stream.write (static_cast<const char*> (buffer), size*nmemb);
  return (stream.fail () ? 0 : size * nmemb);
}

// Progress callback function for curl.

int
progress_func (const char *url, double dltotal, double dlnow,
	       double /*ultotal*/, double /*ulnow*/)
{
  // macro that picks up Ctrl-C signalling
  OCTAVE_QUIT;

#if !defined (URL_QUITE_DOWNLOAD)
  if (dltotal != 0)
    {
      // Is the carriage return character "\r" portable?
      octave_stdout << "\r" << url << ": "
		    << std::fixed << std::setw(5) << std::setprecision(1)
		    << dlnow*100.0/dltotal << " %";

      octave_stdout.flush ();
    }
#endif

  return 0;
}

// Form the query string based on param.

std::string
form_query_string (CURL *curl, const Cell& param)
{
  std::ostringstream query;

  for (int i = 0; i < param.numel (); i += 2)
    {
      std::string name = param(i).string_value ();
      std::string text = param(i+1).string_value ();

      // Encode strings.
      char *enc_name = curl_easy_escape (curl, name.c_str (), name.length ());
      char *enc_text = curl_easy_escape (curl, text.c_str (), text.length ());

      query << enc_name << "=" << enc_text;

      curl_free (enc_name);
      curl_free (enc_text);

      if (i < param.numel()-1)
	query << "&";
    }

  query.flush ();

  return query.str ();
}

// curl front-end

CURLcode
urlget (const std::string& url, const std::string& method,
	const Cell& param, std::ostream& stream)
{
  CURL *curl;

  curl_global_init(CURL_GLOBAL_DEFAULT);

  curl = curl_easy_init();

  if (! curl)
    return CURLE_FAILED_INIT;

  // handle paramters of GET or POST request

  std::string query_string = form_query_string (curl,param);
  //octave_stdout << "query_string " << query_string << std::endl;

  if (method == "get")
    {
      query_string = url + "?" + query_string;
      curl_easy_setopt (curl, CURLOPT_URL, query_string.c_str ());
    }
  else if (method == "post")
    {
      curl_easy_setopt (curl, CURLOPT_URL, url.c_str ());
      curl_easy_setopt (curl, CURLOPT_POSTFIELDS, query_string.c_str ());
    }
  else
    {
      curl_easy_setopt (curl, CURLOPT_URL,url.c_str());
    }

  // Define our callback to get called when there's data to be written.
  curl_easy_setopt (curl, CURLOPT_WRITEFUNCTION, write_data);

  // Set a pointer to our struct to pass to the callback.
  curl_easy_setopt (curl, CURLOPT_WRITEDATA, static_cast<void*> (&stream));

  // Follow redirects.
  curl_easy_setopt (curl, CURLOPT_FOLLOWLOCATION, 1);

  curl_easy_setopt (curl, CURLOPT_NOPROGRESS, false);
  curl_easy_setopt (curl, CURLOPT_PROGRESSFUNCTION, progress_func);
  curl_easy_setopt (curl, CURLOPT_PROGRESSDATA, url.c_str ());
  curl_easy_setopt (curl, CURLOPT_FAILONERROR, true);

  // Switch on full protocol/debug output
  // curl_easy_setopt(curl, CURLOPT_VERBOSE, true);

  CURLcode res = curl_easy_perform(curl);

#if !defined (URL_QUITE_DOWNLOAD)
  if (res == CURLE_OK)
    {
      // download is complete
      progress_func (url.c_str (), 1, 1, 0, 0);
      // new line after progress_func
      octave_stdout << std::endl;
    }
#endif

  // always cleanup
  curl_easy_cleanup (curl);

  curl_global_cleanup ();

  return res;
}

#endif

DEFUN_DLD (urlwrite, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} urlwrite (@var{URL}, @var{localfile})\n\
@deftypefnx {Loadable Function} {@var{f} =} urlwrite (@var{url}, @var{localfile})\n\
@deftypefnx {Loadable Function} {[@var{f}, @var{success}] =} urlwrite (@var{url}, @var{localfile})\n\
@deftypefnx {Loadable Function} {[@var{f}, @var{success}, @var{message}] =} urlwrite (@var{url}, @var{localfile})\n\
Download a remote file specified by its @var{URL} and save it as\n\
@var{localfile}.  For example,\n\
\n\
@example\n\
urlwrite (\"ftp://ftp.octave.org/pub/octave/README\", \"README.txt\");\n\
@end example\n\
\n\
The full path of the downloaded file is returned in @var{f}.  The\n\
variable @var{success} is 1 if the download was successful,\n\
otherwise it is 0 in which case @var{message} contains an error\n\
message.  If no output argument is specified and if an error occurs,\n\
then the error is signaled through Octave's error handling mechanism.\n\
\n\
This function uses libcurl.  Curl supports, among others, the HTTP,\n\
FTP and FILE protocols.  Username and password may be specified in\n\
the URL, for example:\n\
\n\
@example\n\
urlwrite (\"http://username:password@@example.com/file.txt\",\n\
          \"file.txt\");\n\
@end example\n\
\n\
GET and POST requests can be specified by @var{method} and @var{param}.\n\
The parameter @var{method} is either @samp{get} or @samp{post}\n\
and @var{param} is a cell array of parameter and value pairs.\n\
For example:\n\
\n\
@example\n\
urlwrite (\"http://www.google.com/search\", \"search.html\",\n\
          \"get\", @{\"query\", \"octave\"@});\n\
@end example\n\
@seealso{urlread}\n\
@end deftypefn")
{
  octave_value_list retval;

#if defined (HAVE_CURL)

  int nargin = args.length ();

  // verify arguments
  if (nargin != 2 && nargin != 4)
    {
      print_usage ();
      return retval;
    }

  std::string url = args(0).string_value();

  if (error_state)
    {
      error ("urlwrite: url must be a character string");
      return retval;
    }

  // name to store the file if download is succesful
  std::string filename = args(1).string_value();

  if (error_state)
    {
      error ("urlwrite: localfile must be a character string");
      return retval;
    }

  std::string method;
  Cell param; // empty cell array

  if (nargin == 4)
    {
      method = args(2).string_value();

      if (error_state)
        {
          error ("urlwrite: method can only be \"get\" or \"post\"");
          return retval;
        }

      if (method != "get" && method != "post")
	{
	  error ("urlwrite: method can only be \"get\" or \"post\"");
	  return retval;
	}

      param = args(3).cell_value();

      if (error_state)
	{
	  error ("urlwrite: parameters for get and post requests must be given as a cell");
	  return retval;
	}


      if (param.numel () % 2 == 1 )
	{
	  error ("urlwrite: number of elements in param must be even");
	  return retval;
	}
    }

  // create and open file stream

  std::ofstream stream (filename.c_str(), std::ios::out | std::ios::binary);

  if (! stream.is_open ())
    {
      error ("urlwrite: unable to open file");
      return retval;
    }

  CURLcode res = urlget (url, method, param, stream);

  // close the local file
  stream.close ();

  if (nargout > 0)
    {
      // FIXME: urlwrite should return full file path
      retval(0) = octave_env::make_absolute (filename, octave_env::getcwd ());
      //      retval(0) = filename;
      retval(1) = res == CURLE_OK;
      // return empty string if no error occured
      retval(2) = std::string (res == CURLE_OK ? "" : curl_easy_strerror (res));
    }

  if (nargout < 2 && res != CURLE_OK)
    error ("urlwrite: curl: %s", curl_easy_strerror (res));

#else
  retval(2) = "urlwrite: not available in this version of Octave";
  retval(1) = 0;
  retval(0) = "";
#endif

  return retval;
}

DEFUN_DLD (urlread, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{s} =} urlread (@var{url})\n\
@deftypefnx {Loadable Function} {[@var{s}, @var{success}] =} urlread (@var{url})\n\
@deftypefnx {Loadable Function} {[@var{s}, @var{success}, @var{message}] =} urlread (@var{url})\n\
@deftypefnx {Loadable Function} {[@dots{}] =} urlread (@var{url}, @var{method}, @var{param})\n\
Download a remote file specified by its @var{URL} and return its content\n\
in string @var{s}.  For example,\n\
\n\
@example\n\
s = urlread (\"ftp://ftp.octave.org/pub/octave/README\");\n\
@end example\n\
\n\
The variable @var{success} is 1 if the download was successful,\n\
otherwise it is 0 in which case @var{message} contains an error\n\
message.  If no output argument is specified and if an error occurs,\n\
then the error is signaled through Octave's error handling mechanism.\n\
\n\
This function uses libcurl.  Curl supports, among others, the HTTP,\n\
FTP and FILE protocols.  Username and password may be specified in the\n\
URL.  For example,\n\
\n\
@example\n\
s = urlread (\"http://username:password@@example.com/file.txt\");\n\
@end example\n\
\n\
GET and POST requests can be specified by @var{method} and @var{param}.\n\
The parameter @var{method} is either @samp{get} or @samp{post}\n\
and @var{param} is a cell array of parameter and value pairs.\n\
For example,\n\
\n\
@example\n\
s = urlread (\"http://www.google.com/search\", \"get\",\n\
             @{\"query\", \"octave\"@});\n\
@end example\n\
@seealso{urlwrite}\n\
@end deftypefn")
{
  // Octave's return value
  octave_value_list retval;

#if defined (HAVE_CURL)

  int nargin = args.length ();

  // verify arguments
  if (nargin != 1  && nargin != 3)
    {
      print_usage ();
      return retval;
    }

  std::string url = args(0).string_value();

  if (error_state)
    {
      error ("urlread: url must be a character string");
      return retval;
    }

  std::string method;
  Cell param; // empty cell array

  if (nargin == 3)
    {
      method = args(1).string_value();

      if (error_state)
	{
	  error ("urlread: method can only be \"get\" or \"post\"");
	  return retval;
	}

      if (method != "get" && method != "post")
	{
	  error ("urlread: method can only be \"get\" or \"post\"");
	  return retval;
	}

      param = args(2).cell_value();

      if (error_state)
	{
	  error ("urlread: parameters for get and post requests must be given as a cell");
	  return retval;
	}

      if (param.numel () % 2 == 1 )
	{
	  error ("urlread: number of elements in param must be even");
	  return retval;
	}
    }

  // string stream for output
  std::ostringstream stream;

  CURLcode res = urlget (url, method, param, stream);

  if (nargout > 0)
    {
      retval(0) = stream.str ();
      retval(1) = res == CURLE_OK;
      // return empty string if no error occured
      retval(2) = std::string (res == CURLE_OK ? "" : curl_easy_strerror (res));
    }

  if (nargout < 2 && res != CURLE_OK)
    error ("urlread: curl: %s", curl_easy_strerror (res));

#else
  retval(2) = "urlread: not available in this version of Octave";
  retval(1) = 0;
  retval(0) = "";
#endif

  return retval;
}
