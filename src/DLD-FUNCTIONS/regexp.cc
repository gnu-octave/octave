/*

Copyright (C) 2005 David Bateman
Copyright (C) 2002-2005 Paul Kienzle

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

*/

// XXX FIXME XXX
// regexprep should be written as an m-file based on regexp

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

#include "Cell.h"
#include "oct-map.h"
#include "str-vec.h"

#ifdef HAVE_PCRE
#include <pcre.h>
#else
#ifdef HAVE_REGEX
#ifdef __MINGW32__
#define __restrict
#endif
#include <regex.h>
#endif
#endif

static octave_value_list
octregexp (const octave_value_list &args, int nargout, const std::string &nm,
	   bool case_insensitive)
{
  octave_value_list retval;
#if defined (HAVE_REGEX) || defined (HAVE_PCRE) 
  int nargin = args.length();
  int nopts = nargin - 2;
  bool once = false;

  if (nargin < 2)
    {
      print_usage(nm);
      return retval;
    }

  std::string buffer = args(0).string_value ();
  if (error_state)
    {
      gripe_wrong_type_arg (nm.c_str(), args(0));
      return retval;
    }

  std::string pattern = args(1).string_value ();
  if (error_state)
    {
      gripe_wrong_type_arg (nm.c_str(), args(1));
      return retval;
    }

  for (int i = 2; i < nargin; i++)
    {
      std::string str = args(i).string_value();
      if (error_state)
	{
	  error ("%s: optional arguments must be strings", nm.c_str());
	  break;
	}
      std::transform (str.begin (), str.end (), str.begin (), tolower);
      if (str.find("once", 0) == 0)
	{
	  once = true;
	  nopts--;
	}
#ifdef HAVE_PCRE
      // XXX FIXME XXX named tokens still broken. Disable for now
#if 0
      else if (str.find("start", 0) && str.find("end", 0) &&
	       str.find("tokenextents", 0) && str.find("match", 0) &&
	       str.find("tokens", 0) && str.find("names", 0))
	error ("%s: unrecognized option", nm.c_str());
#else
      else if (str.find("names", 0) == 0)
	error ("%s: named tokens not implemented in this version", nm.c_str());
      else if (str.find("start", 0) && str.find("end", 0) &&
	       str.find("tokenextents", 0) && str.find("match", 0) &&
	       str.find("tokens", 0))
	error ("%s: unrecognized option", nm.c_str());
#endif
#else
      else if (str.find("names", 0) == 0)
	error ("%s: named tokens not implemented in this version", nm.c_str());
      else if (str.find("start", 0) && str.find("end", 0) &&
	       str.find("tokenextents", 0) && str.find("match", 0) &&
	       str.find("tokens", 0))
	error ("%s: unrecognized option", nm.c_str());
#endif
    }

  if (!error_state)
    {
      Octave_map nmap;
      Cell t, m, te;
      NDArray s, e;

      // named tokens "(?<name>...)" are only treated with PCRE not regex.
#if HAVE_PCRE
      // The syntax of named tokens in pcre is "(?P<name>...)" while we need
      // a syntax "(?<name>...)", so fix that here. Also an expression like
      // "(?<first>\w+)\s+(?<last>\w+)|(?<last>\w+),\s+(?<first>\w+)" should
      // be perfectly legal, while pcre does not allow the same named token
      // name of both sides of the alternative. Also fix that here by replacing
      // duplicate name tokens by dummy names, and dealing with the dummy names
      // later.
      



      // Compile expression
      pcre *re;
      const char *err;
      int erroffset;
      re = pcre_compile(pattern.c_str(), (case_insensitive ? PCRE_CASELESS : 0),
			&err, &erroffset, NULL);
    
      if (re == NULL) {
	error("%s: %s at position %d of expression", nm.c_str(), 
	      err, erroffset);
	return retval;
      }

      int subpatterns;
      int namecount;
      int nameentrysize;
      char *nametable;
      int idx = 0;
      int sz = 0;

      pcre_fullinfo(re, NULL, PCRE_INFO_CAPTURECOUNT,  &subpatterns);
      pcre_fullinfo(re, NULL, PCRE_INFO_NAMECOUNT, &namecount);
      pcre_fullinfo(re, NULL, PCRE_INFO_NAMEENTRYSIZE, &nameentrysize);
      pcre_fullinfo(re, NULL, PCRE_INFO_NAMETABLE, &nametable);

      OCTAVE_LOCAL_BUFFER(int, ovector, (subpatterns+1)*3);
      OCTAVE_LOCAL_BUFFER(int, nidx, namecount);
      string_vector names (namecount);

      for (int i = 0; i < namecount; i++)
	{
	  // Index of subpattern in first two bytes MSB first of name.
	  // Extract name and index.
	  nidx[i] = ((int)nametable[i*nameentrysize]) << 8 |
	    (int)nametable[i*nameentrysize+1];
	  names(i) = std::string((&(nametable[i*nameentrysize+2])));
	}

      Cell named_tokens(dim_vector(namecount,1));

      while(true)
	{
	  int matches = pcre_exec(re, NULL, buffer.c_str(), 
				  buffer.length(), idx, 
				  (idx ? PCRE_NOTBOL : 0),
				  ovector, (subpatterns+1)*3);

	  if (matches < 0 && matches != PCRE_ERROR_NOMATCH)
	    {
	      error ("%s: internal error calling pcre_exec", nm.c_str());
	      pcre_free(re);
	      return retval;
	    }
	  else if (matches == PCRE_ERROR_NOMATCH)
	    break;
	  else
	    {
	      s.resize (dim_vector(1, sz+1));
	      s(sz) = double (ovector[0]+1);
	      e.resize (dim_vector(1, sz+1));
	      e(sz) = double (ovector[1]);
	      te.resize(dim_vector(1, sz+1));
	      Matrix mat_te(matches-1,2);
	      for (int i = 1; i < matches; i++)
		{
		  mat_te(i-1,0) = double (ovector[2*i]+1);
		  mat_te(i-1,1) = double (ovector[2*i+1]);
		}
	      te(sz) = mat_te;

	      const char **listptr;
	      int status = pcre_get_substring_list(buffer.c_str(), ovector, 
						   matches, &listptr);

	      if (status == PCRE_ERROR_NOMEMORY) {
		error("%s: cannot allocate memory in pcre_get_substring_list",
		      nm.c_str());
		pcre_free(re);
		return retval;
	      }

	      m.resize (dim_vector(1, sz+1));
	      m(sz) =  std::string(*listptr);

	      t.resize (dim_vector(1, sz+1));
	      Cell cell_t (dim_vector(1,matches-1));
	      for (int i = 1; i < matches; i++)
		cell_t(i-1) = std::string(*(listptr+i));
	      t(sz) = cell_t;

	      for (int i = 0; i < namecount; i++)
		{
		  Cell tmp = named_tokens(i);
		  tmp.resize(dim_vector(1,sz+1));
		  tmp(sz) = 
		    std::string(*(listptr+nidx[i]));
		  named_tokens(i) = tmp;
		}

	      pcre_free_substring_list(listptr);

	      if (once)
		break;

	      idx = ovector[1];
	      sz++;
	    }
	}

      for (int i = 0; i < namecount; i++)
	nmap.assign (names(i), named_tokens(i));

      pcre_free(re);
#else
      regex_t compiled;
      int err=regcomp(&compiled, pattern.c_str(), REG_EXTENDED | 
		      (case_insensitive ? REG_ICASE : 0));
      if (err)
	{
	  int len = regerror(err, &compiled, NULL, 0);
	  char *errmsg = (char *)malloc(len);
	  if (errmsg)
	    {
	      regerror(err, &compiled, errmsg, len);
	      error("%s: %s in pattern (%s)", nm.c_str(), errmsg, 
		    pattern.c_str());
	      free(errmsg);
	    }
	  else
	    {
	      error("out of memory");
	    }
	  regfree(&compiled);
	  return retval;
	}

      int subexpr = 1;
      int idx = 0;
      int sz = 0;
      for (unsigned int i=0; i < pattern.length(); i++)
	  subexpr += ( pattern[i] == '(' ? 1 : 0 );
      OCTAVE_LOCAL_BUFFER (regmatch_t, match, subexpr );

      while(true)
	{
	  if (regexec(&compiled, buffer.c_str() + idx, subexpr, 
		      match, (idx ? REG_NOTBOL : 0)) == 0) 
	    {
	      // Count actual matches
	      int matches = 0;
	      while (matches < subexpr && match[matches].rm_so >= 0) 
		matches++;

	      s.resize (dim_vector(1, sz+1));
	      s(sz) = double (match[0].rm_so+1+idx);
	      e.resize (dim_vector(1, sz+1));
	      e(sz) = double (match[0].rm_eo+idx);
	      te.resize(dim_vector(1, sz+1));
	      Matrix mat_te(matches-1,2);
	      for (int i = 1; i < matches; i++)
		{
		  mat_te(i-1,0) = double (match[i].rm_so+1+idx);
		  mat_te(i-1,1) = double (match[i].rm_eo+idx);
		}
	      te(sz) = mat_te;

	      m.resize (dim_vector(1, sz+1));
	      m(sz) =  buffer.substr (match[0].rm_so+idx, 
					 match[0].rm_eo-match[0].rm_so);

	      t.resize (dim_vector(1, sz+1));
	      Cell cell_t (dim_vector(1,matches-1));
	      for (int i = 1; i < matches; i++)
		cell_t(i-1) = buffer.substr (match[i].rm_so+idx, 
					     match[i].rm_eo-match[i].rm_so);
	      t(sz) = cell_t;

	      idx += match[0].rm_eo;
	      sz++;

	      if (once)
		break;
	    }
	  else
	    break;
	}
      regfree(&compiled);
#endif

      retval(5) = nmap;
      retval(4) = t;
      retval(3) = m;
      retval(2) = te;
      retval(1) = e;
      retval(0) = s;

      // Alter the order of the output arguments
      if (nopts > 0)
	{
	  int n = 0;
	  octave_value_list new_retval;
	  new_retval.resize(nargout);

	  OCTAVE_LOCAL_BUFFER (int, arg_used, 6);
	  for (int i = 0; i < 6; i++)
	    arg_used[i] = false;
	  
	  for (int i = 2; i < nargin; i++)
	    {
	      int k = 0;
	      std::string str = args(i).string_value();
	      std::transform (str.begin (), str.end (), str.begin (), tolower);
	      if (str.find("once", 0) == 0)
		continue;
	      else if (str.find("start", 0) == 0)
		k = 0;
	      else if (str.find("end", 0) == 0)
		k = 1;
	      else if (str.find("tokenextents", 0) == 0)
		k = 2;
	      else if (str.find("match", 0) == 0)
		k = 3;
	      else if (str.find("tokens", 0) == 0)
		k = 4;
	      else if (str.find("names", 0) == 0)
		k = 5;

	      new_retval(n++) = retval(k);
	      arg_used[k] = true;

	      if (n == nargout)
		break;
	    }

	  // Fill in the rest of the arguments
	  if (n < nargout)
	    {
	      for (int i = 0; i < 6; i++)
		{
		  if (! arg_used[i])
		    new_retval(n++) = retval(i);
		}
	    }

	  retval = new_retval;
	}
    }

#else
  error ("%s: not available in this version of Octave", nm);
#endif
  return retval;
}

DEFUN_DLD (regexp, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{s}, @var{e}, @var{te}, @var{m}, @var{t}, @var{nm}] =} regexp (@var{str}, @var{pat})\n\
@deftypefnx {Loadable Function} {[@dots{}] =} regexp (@var{str}, @var{pat}, @var{opts}, @dots{})\n\
\n\
Regular expression string matching. Matches @var{pat} in @var{str} and\n\
returns the position and matching substrings or empty values if there are\n\
none.\n\
\n\
The matched pattern @var{pat} can include any of the standard regex\n\
operators, including:\n\
\n\
@table @code\n\
@item .\n\
Match any character\n\
@item * + ? @{@}\n\
Repetition operators, representing\n\
@table @code\n\
@item *\n\
Match zero or more times\n\
@item +\n\
Match one or more times\n\
@item ?\n\
Match zero or one times\n\
@item @{@}\n\
Match range operator, which is of the form @code{@{@var{n}@}} to match exactly\n\
@var{n} times, @code{@{@var{m},@}} to match @var{m} or more times,\n\
@code{@{@var{m},@var{n}@}} to match between @var{m} and @var{n} times.\n\
@end table\n\
@item [@dots{}] [^@dots{}]\n\
List operators, where for example @code{[ab]c} matches @code{ac} and @code{bc}\n\
@item ()\n\
Grouping operator\n\
@item |\n\
Alternation operator. Match one of a choice of regular expressions. The\n\
alternatives must be delimited by the grouoing operator @code{()} above\n\
@item ^ $\n\
Anchoring operator. @code{^} matches the start of the string @var{str} and\n\
@code{$} the end\n\
@end table\n\
\n\
In addition the following escaped characters have special meaning. It should\n\
be noted that it is recommended to quote @var{pat} in single quotes rather\n\
than double quotes, to avoid the escape sequences being interpreted by octave\n\
before being passed to @code{regexp}.\n\
\n\
@table @code\n\
@item \\b\n\
Match a word boundary\n\
@item \\B\n\
Match within a word\n\
@item \\w\n\
Matches any word character\n\
@item \\W\n\
Matches any non word character\n\
@item \\<\n\
Matches the beginning of a word\n\
@item \\>\n\
Matches the end of a word\n\
@item \\s\n\
Matches any whitespace character\n\
@item \\S\n\
Matches any non whitespace character\n\
@item \\d\n\
Matches any digit\n\
@item \\D\n\
Matches any non-digit\n\
@end table\n\
\n\
The outputs of @code{regexp} by default are in the order as given below\n\
\n\
@table @asis\n\
@item @var{s}\n\
The start indices of each of the matching substrings\n\
\n\
@item @var{e}\n\
The end indices of each matching substring\n\
\n\
@item @var{te}\n\
The extents of each of the matched token surrounded by @code{(@dots{})} in\n\
@var{pat}.\n\
\n\
@item @var{m}\n\
A cell array of the text of each match.\n\
\n\
@item @var{t}\n\
A cell array of the text of each token matched.\n\
\n\
@item @var{nm}\n\
A structure containing the text of each matched named token, with the name\n\
being used as the fieldname. A named token is denoted as\n\
@code{(?<name>@dots{})}\n\
@end table\n\
\n\
Particular output arguments or the order of the output arguments can be\n\
selected by additional @var{opts} arguments. These are strings and the\n\
correspondence between the output arguments and the optional argument\n\
are\n\
\n\
@multitable @columnfractions 0.2 0.3 0.3 0.2\n\
@item @tab 'start'        @tab @var{s}  @tab\n\
@item @tab 'end'          @tab @var{e}  @tab\n\
@item @tab 'tokenExtents' @tab @var{te} @tab\n\
@item @tab 'match'        @tab @var{m}  @tab\n\
@item @tab 'tokens'       @tab @var{t}  @tab\n\
@item @tab 'names'        @tab @var{nm}  @tab\n\
@end multitable\n\
\n\
A further optional argument is 'once', that limits the number of returned\n\
matches to the first match.\n\
@end deftypefn")
{
  return octregexp (args, nargout, "regexp", false);
}

/*

## seg-fault test
%!assert(regexp("abcde","."),[1,2,3,4,5])

## Check that anchoring of pattern works correctly
%!assert(regexp('abcabc','^abc'),1);
%!assert(regexp('abcabc','abc$'),4);
%!assert(regexp('abcabc','^abc$'),[]);

%!test
%! [s, e, te, m, t] = regexp(' No Match ', 'f(.*)uck');
%! assert (s,[])
%! assert (e,[])
%! assert (te,{})
%! assert (m, {})
%! assert (t, {})

%!test
%! [s, e, te, m, t] = regexp(' FiRetrUck ', 'f(.*)uck');
%! assert (s,[])
%! assert (e,[])
%! assert (te,{})
%! assert (m, {})
%! assert (t, {})

%!test
%! [s, e, te, m, t] = regexp(' firetruck ', 'f(.*)uck');
%! assert (s,2)
%! assert (e,10)
%! assert (te{1},[3,7])
%! assert (m{1}, 'firetruck')
%! assert (t{1}{1}, 'iretr')

%!test
%! [s, e, te, m, t] = regexp('short test string','\w*r\w*');
%! assert (s,[1,12])
%! assert (e,[5,17])
%! assert (size(te), [1,2])
%! assert (isempty(te{1}))
%! assert (isempty(te{2}))
%! assert (m{1},'short')
%! assert (m{2},'string')
%! assert (size(t), [1,2])
%! assert (isempty(t{1}))
%! assert (isempty(t{2}))

%!test
%! [s, e, te, m, t] = regexp('short test string','\w*r\w*','once');
%! assert (s,1)
%! assert (e,5)
%! assert (size(te), [1,1])
%! assert (isempty(te{1}))
%! assert (m{1},'short')
%! ## Matlab gives [1,0] here but that seems wrong.
%! assert (size(t), [1,1])

%!test
%! [m, te, e, s, t] = regexp('short test string','\w*r\w*','once', 'match', 'tokenExtents', 'end', 'start', 'tokens');
%! assert (s,1)
%! assert (e,5)
%! assert (size(te), [1,1])
%! assert (isempty(te{1}))
%! assert (m{1},'short')
%! ## Matlab gives [1,0] here but that seems wrong.
%! assert (size(t), [1,1])

## XXX FIXME XXX Disable test for now as PCRE version not written
%!#test
%! ## This test is expected to fail if PCRE is not installed
%! [s, e, te, m, t, nm] = regexp('short test string','(?<word1>\w*t)\s*(?<word2>\w*t)');
%! assert (s,1)
%! assert (e,10)
%! assert (size(te), [1,1])
%! assert (te{1}, [1 5; 7, 10])
%! assert (m{1},'short test')
%! assert (size(t),[1,1])
%! assert (t{1}{1},'short')
%! assert (t{1}{2},'test')
%! assert (size(nm), [1,1])
%! assert (isempty(fieldnames(nm)))
%! assert (sort(fieldnames(nm)),{'word1','word2'})
%! assert (nm.word1,'short')
%! assert (nm.word2,'test')

## XXX FIXME XXX Disable test for now as PCRE version not written
%!#test
%! ## This test is expected to fail if PCRE is not installed
%! [nm, m, te, e, s, t] = regexp('short test string','(?<word1>\w*t)\s*(?<word2>\w*t)', 'names', 'match', 'tokenExtents', 'end', 'start', 'tokens');
%! assert (s,1)
%! assert (e,10)
%! assert (size(te), [1,1])
%! assert (te{1}, [1 5; 7, 10])
%! assert (m{1},'short test')
%! assert (size(t),[1,1])
%! assert (t{1}{1},'short')
%! assert (t{1}{2},'test')
%! assert (size(nm), [1,1])
%! assert (isempty(fieldnames(nm)))
%! assert (sort(fieldnames(nm)),{'word1','word2'})
%! assert (nm.word1,'short')
%! assert (nm.word2,'test')

%!error regexp('string', 'tri', 'BadArg');
%!error regexp('string');

*/

DEFUN_DLD(regexpi, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{s}, @var{e}, @var{te}, @var{m}, @var{t}, @var{nm}] =} regexpi (@var{str}, @var{pat})\n\
@deftypefnx {Loadable Function} {[@dots{}] =} regexpi (@var{str}, @var{pat}, @var{opts}, @dots{})\n\
\n\
Case insensitive regular expression string matching. Matches @var{pat} in\n\
@var{str} and returns the position and matching substrings or empty values\n\
if there are none. See @code{regexp} for more details\n\
@end deftypefn")
{
  return octregexp (args, nargout, "regexp", true);
}

/*

## seg-fault test
%!assert(regexpi("abcde","."),[1,2,3,4,5])

## Check that anchoring of pattern works correctly
%!assert(regexpi('abcabc','^abc'),1);
%!assert(regexpi('abcabc','abc$'),4);
%!assert(regexpi('abcabc','^abc$'),[]);

%!test
%! [s, e, te, m, t] = regexpi(' No Match ', 'f(.*)uck');
%! assert (s,[])
%! assert (e,[])
%! assert (te,{})
%! assert (m, {})
%! assert (t, {})

%!test
%! [s, e, te, m, t] = regexpi(' FiRetrUck ', 'f(.*)uck');
%! assert (s,2)
%! assert (e,10)
%! assert (te{1},[3,7])
%! assert (m{1}, 'FiRetrUck')
%! assert (t{1}{1}, 'iRetr')

%!test
%! [s, e, te, m, t] = regexpi(' firetruck ', 'f(.*)uck');
%! assert (s,2)
%! assert (e,10)
%! assert (te{1},[3,7])
%! assert (m{1}, 'firetruck')
%! assert (t{1}{1}, 'iretr')

%!test
%! [s, e, te, m, t] = regexpi('ShoRt Test String','\w*r\w*');
%! assert (s,[1,12])
%! assert (e,[5,17])
%! assert (size(te), [1,2])
%! assert (isempty(te{1}))
%! assert (isempty(te{2}))
%! assert (m{1},'ShoRt')
%! assert (m{2},'String')
%! assert (size(t), [1,2])
%! assert (isempty(t{1}))
%! assert (isempty(t{2}))

%!test
%! [s, e, te, m, t] = regexpi('ShoRt Test String','\w*r\w*','once');
%! assert (s,1)
%! assert (e,5)
%! assert (size(te), [1,1])
%! assert (isempty(te{1}))
%! assert (m{1},'ShoRt')
%! ## Matlab gives [1,0] here but that seems wrong.
%! assert (size(t), [1,1])

%!test
%! [m, te, e, s, t] = regexpi('ShoRt Test String','\w*r\w*','once', 'match', 'tokenExtents', 'end', 'start', 'tokens');
%! assert (s,1)
%! assert (e,5)
%! assert (size(te), [1,1])
%! assert (isempty(te{1}))
%! assert (m{1},'ShoRt')
%! ## Matlab gives [1,0] here but that seems wrong.
%! assert (size(t), [1,1])

## XXX FIXME XXX Disable test for now as PCRE version not written
%!#test
%! ## This test is expected to fail if PCRE is not installed
%! [s, e, te, m, t, nm] = regexpi('ShoRt Test String','(?<word1>\w*t)\s*(?<word2>\w*t)');
%! assert (s,1)
%! assert (e,10)
%! assert (size(te), [1,1])
%! assert (te{1}, [1 5; 7, 10])
%! assert (m{1},'ShoRt Test')
%! assert (size(t),[1,1])
%! assert (t{1}{1},'ShoRt')
%! assert (t{1}{2},'Test')
%! assert (size(nm), [1,1])
%! assert (isempty(fieldnames(nm)))
%! assert (sort(fieldnames(nm)),{'word1','word2'})
%! assert (nm.word1,'ShoRt')
%! assert (nm.word2,'Test')

## XXX FIXME XXX Disable test for now as PCRE version not written
%!#test
%! ## This test is expected to fail if PCRE is not installed
%! [nm, m, te, e, s, t] = regexpi('ShoRt Test String','(?<word1>\w*t)\s*(?<word2>\w*t)', 'names', 'match', 'tokenExtents', 'end', 'start', 'tokens');
%! assert (s,1)
%! assert (e,10)
%! assert (size(te), [1,1])
%! assert (te{1}, [1 5; 7, 10])
%! assert (m{1},'ShoRt Test')
%! assert (size(t),[1,1])
%! assert (t{1}{1},'ShoRt')
%! assert (t{1}{2},'Test')
%! assert (size(nm), [1,1])
%! assert (isempty(fieldnames(nm)))
%! assert (sort(fieldnames(nm)),{'word1','word2'})
%! assert (nm.word1,'ShoRt')
%! assert (nm.word2,'Test')

%!error regexpi('string', 'tri', 'BadArg');
%!error regexpi('string');

*/

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
