/*

Copyright (C) 2005, 2006, 2007, 2008, 2009 David Bateman
Copyright (C) 2002, 2003, 2004, 2005 Paul Kienzle

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <algorithm>
#include <sstream>

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

#include "Cell.h"
#include "oct-map.h"
#include "str-vec.h"
#include "quit.h"
#include "parse.h"
#include "oct-locbuf.h"

#if defined (HAVE_PCRE)
#include <pcre.h>
#elif defined (HAVE_REGEX)
#if defined (__MINGW32__)
#define __restrict
#endif
#include <sys/types.h>
#include <regex.h>
#endif

// Define the maximum number of retries for a pattern that 
// possibly results in an infinite recursion.
#define PCRE_MATCHLIMIT_MAX 10

// The regexp is constructed as a linked list to avoid resizing the
// return values in arrays at each new match.

// FIXME don't bother collecting and composing return values the user
// doesn't want.

class regexp_elem
{
public:
  regexp_elem (const string_vector& _named_token, const Cell& _t, 
               const std::string& _m, const Matrix& _te, double _s, 
               double _e) :
    named_token (_named_token), t (_t), m (_m), te (_te), s (_s), e (_e) { }

  regexp_elem (const regexp_elem &a) : named_token (a.named_token), t (a.t), 
                                       m (a.m), te (a.te), s (a.s), e (a.e)
                                       { }

  string_vector named_token;
  Cell t;
  std::string m;
  Matrix te;
  double s;
  double e;
};

typedef std::list<regexp_elem>::const_iterator const_iterator;

#define MAXLOOKBEHIND 10
static bool lookbehind_warned = false;

static int
octregexp_list (const octave_value_list &args, const std::string &nm, 
                bool case_insensitive, std::list<regexp_elem> &lst, 
                string_vector &named, int &nopts, bool &once)
{
  int sz = 0;
#if defined (HAVE_REGEX) || defined (HAVE_PCRE) 
  int nargin = args.length();
  bool lineanchors = false;
  bool dotexceptnewline = false;
  bool freespacing = false;

  nopts = nargin - 2;
  once = false;

  std::string buffer = args(0).string_value ();
  size_t max_length = (buffer.length () > MAXLOOKBEHIND ? 
                       MAXLOOKBEHIND: buffer.length ());

  if (error_state)
    {
      gripe_wrong_type_arg (nm.c_str(), args(0));
      return 0;
    }

  std::string pattern = args(1).string_value ();
  if (error_state)
    {
      gripe_wrong_type_arg (nm.c_str(), args(1));
      return 0;
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
      else if (str.find("matchcase", 0) == 0)
        {
          case_insensitive = false;
          nopts--;
        }
      else if (str.find("ignorecase", 0) == 0)
        {
          case_insensitive = true;
          nopts--;
        }
      else if (str.find("dotall", 0) == 0)
        {
          dotexceptnewline = false;
          nopts--;
        }
      else if (str.find("stringanchors", 0) == 0)
        {
          lineanchors = false;
          nopts--;
        }
      else if (str.find("literalspacing", 0) == 0)
        {
          freespacing = false;
          nopts--;
        }
#if HAVE_PCRE
      // Only accept these options with pcre
      else if (str.find("dotexceptnewline", 0) == 0)
        {
          dotexceptnewline = true;
          nopts--;
        }
      else if (str.find("lineanchors", 0) == 0)
        {
          lineanchors = true;
          nopts--;
        }
      else if (str.find("freespacing", 0) == 0)
        {
          freespacing = true;
          nopts--;
        }
      else if (str.find("start", 0) && str.find("end", 0) &&
               str.find("tokenextents", 0) && str.find("match", 0) &&
               str.find("tokens", 0) && str.find("names", 0))
        error ("%s: unrecognized option", nm.c_str());
#else
      else if (str.find("names", 0) == 0 ||
               str.find("dotexceptnewline", 0) == 0 ||
               str.find("lineanchors", 0) == 0 ||
               str.find("freespacing", 0) == 0)
       error ("%s: %s not implemented in this version", str.c_str(), nm.c_str());
      else if (str.find("start", 0) && str.find("end", 0) &&
               str.find("tokenextents", 0) && str.find("match", 0) &&
               str.find("tokens", 0))
        error ("%s: unrecognized option", nm.c_str());
#endif
    }

  if (!error_state)
    {
      Cell t;
      std::string m;
      double s, e;

      // named tokens "(?<name>...)" are only treated with PCRE not regex.
#if HAVE_PCRE
      
      size_t pos = 0;
      size_t new_pos;
      int nnames = 0;
      int inames = 0;
      std::ostringstream buf;
      Array<int> named_idx;

      while ((new_pos = pattern.find ("(?",pos)) != std::string::npos)
        {
          if (pattern.at (new_pos + 2) == '<' &&  
              !(pattern.at (new_pos + 3) == '=' ||
                pattern.at (new_pos + 3) == '!'))
            {
              // The syntax of named tokens in pcre is "(?P<name>...)" while
              // we need a syntax "(?<name>...)", so fix that here. Also an 
              // expression like 
              // "(?<first>\w+)\s+(?<last>\w+)|(?<last>\w+),\s+(?<first>\w+)" 
              // should be perfectly legal, while pcre does not allow the same
              // named token name on both sides of the alternative. Also fix
              // that here by replacing name tokens by dummy names, and dealing
              // with the dummy names later.

              size_t tmp_pos = pattern.find_first_of ('>',new_pos);

              if (tmp_pos == std::string::npos)
                {
                  error ("syntax error in pattern");
                  break;
                }

              std::string tmp_name = 
                pattern.substr(new_pos+3,tmp_pos-new_pos-3);
              bool found = false;

              for (int i = 0; i < nnames; i++)
                if (named(i) == tmp_name)
                  {
                    named_idx.resize(inames+1, 1);
                    named_idx(inames) = i;
                    found = true;
                    break;
                  }
              if (! found)
                {
                  named_idx.resize(inames+1, 1);
                  named_idx(inames) = nnames;
                  named.append(tmp_name);
                  nnames++;
                }

              if (new_pos - pos > 0)
                buf << pattern.substr(pos,new_pos-pos);
              if (inames < 10)
                buf << "(?P<n00" << inames++;
              else if (inames < 100)
                buf << "(?P<n0" << inames++;
              else
                buf << "(?P<n" << inames++;
              pos = tmp_pos;
            }
          else if (pattern.at (new_pos + 2) == '<')
            {
              // Find lookbehind operators of arbitrary length (ie like 
              // "(?<=[a-z]*)") and replace with a maximum length operator 
              // as PCRE can not yet handle arbitrary length lookahead 
              // operators. Use the string length as the maximum length to 
              // avoid issues.

              int brackets = 1;
              size_t tmp_pos1 = new_pos + 2;
              size_t tmp_pos2 = tmp_pos1;
              while (tmp_pos1 <= pattern.length () && brackets > 0)
                {
                  char ch = pattern.at (tmp_pos1);
                  if (ch == '(')
                    brackets++;
                  else if (ch == ')')
                    {
                      if (brackets > 1)
                        tmp_pos2 = tmp_pos1;

                      brackets--;
                    }
                  tmp_pos1++;
                }

              if (brackets != 0)
                {
                  buf << pattern.substr (pos, new_pos - pos) << "(?";
                  pos = new_pos + 2;
                }
              else
                {
                  size_t tmp_pos3 = pattern.find_first_of ("*+", tmp_pos2);
                  if (tmp_pos3 != std::string::npos && tmp_pos3 < tmp_pos1)
                    {
                      if (!lookbehind_warned)
                        {
                          lookbehind_warned = true;
                          warning ("%s: arbitrary length lookbehind patterns are only supported up to length %d", nm.c_str(), MAXLOOKBEHIND);
                        }

                      buf << pattern.substr (pos, new_pos - pos) << "(";

                      size_t i;
                      if (pattern.at (tmp_pos3) == '*')
                        i = 0;
                      else
                        i = 1;

                      for (; i < max_length + 1; i++)
                        {
                          buf << pattern.substr(new_pos, tmp_pos3 - new_pos)
                              << "{" << i << "}";
                          buf << pattern.substr(tmp_pos3 + 1, 
                                                tmp_pos1 - tmp_pos3 - 1);
                          if (i != max_length)
                            buf << "|";
                        }
                      buf << ")";
                    }
                  else
                    buf << pattern.substr (pos, tmp_pos1 - pos);
                  pos = tmp_pos1;
                }
            }
          else
            {
              buf << pattern.substr (pos, new_pos - pos) << "(?";
              pos = new_pos + 2;
            }

        }

      buf << pattern.substr(pos);

      if (error_state)
        return 0;

      // Compile expression
      pcre *re;
      const char *err;
      int erroffset;
      std::string buf_str = buf.str ();
      re = pcre_compile (buf_str.c_str (),
                         (case_insensitive ? PCRE_CASELESS : 0) |
                         (dotexceptnewline ? 0 : PCRE_DOTALL) |
                         (lineanchors ? PCRE_MULTILINE : 0) |
                         (freespacing ? PCRE_EXTENDED : 0),
                         &err, &erroffset, 0);
    
      if (re == 0)
        {
          error("%s: %s at position %d of expression", nm.c_str(), 
                err, erroffset);
          return 0;
        }

      int subpatterns;
      int namecount;
      int nameentrysize;
      char *nametable;
      int idx = 0;

      pcre_fullinfo(re, 0, PCRE_INFO_CAPTURECOUNT,  &subpatterns);
      pcre_fullinfo(re, 0, PCRE_INFO_NAMECOUNT, &namecount);
      pcre_fullinfo(re, 0, PCRE_INFO_NAMEENTRYSIZE, &nameentrysize);
      pcre_fullinfo(re, 0, PCRE_INFO_NAMETABLE, &nametable);

      OCTAVE_LOCAL_BUFFER(int, ovector, (subpatterns+1)*3);
      OCTAVE_LOCAL_BUFFER(int, nidx, namecount);

      for (int i = 0; i < namecount; i++)
        {
          // Index of subpattern in first two bytes MSB first of name.
          // Extract index.
          nidx[i] = (static_cast<int>(nametable[i*nameentrysize])) << 8 |
            static_cast<int>(nametable[i*nameentrysize+1]);
        }

      while(true)
        {
          OCTAVE_QUIT;

          int matches = pcre_exec(re, 0, buffer.c_str(), 
                                  buffer.length(), idx, 
                                  (idx ? PCRE_NOTBOL : 0),
                                  ovector, (subpatterns+1)*3);

          if (matches == PCRE_ERROR_MATCHLIMIT)
            {
              // try harder; start with default value for MATCH_LIMIT and increase it
              warning("Your pattern caused PCRE to hit its MATCH_LIMIT.\nTrying harder now, but this will be slow.");
              pcre_extra pe;
              pcre_config(PCRE_CONFIG_MATCH_LIMIT, static_cast <void *> (&pe.match_limit));
              pe.flags = PCRE_EXTRA_MATCH_LIMIT;

              int i = 0;
              while (matches == PCRE_ERROR_MATCHLIMIT &&
                     i++ < PCRE_MATCHLIMIT_MAX)
                {
                  OCTAVE_QUIT;

                  pe.match_limit *= 10;
                  matches = pcre_exec(re, &pe, buffer.c_str(), 
                                      buffer.length(), idx, 
                                      (idx ? PCRE_NOTBOL : 0),
                                      ovector, (subpatterns+1)*3);
                }
            }

          if (matches < 0 && matches != PCRE_ERROR_NOMATCH)
            {
              error ("%s: internal error calling pcre_exec\nError code from pcre_exec is %i", nm.c_str(), matches);
              pcre_free(re);
              return 0;
            }
          else if (matches == PCRE_ERROR_NOMATCH)
            break;
          else if (ovector[1] <= ovector[0])
            {
              // FIXME: Zero sized match!! Is this the right thing to do?
              idx = ovector[0] + 1;
              continue;
            }
          else
            {
              int pos_match = 0;
              Matrix te(matches-1,2);
              for (int i = 1; i < matches; i++)
                if (ovector[2*i] >= 0 && ovector[2*i+1] > 0)
                  if (i == 1 || ovector[2*i] != ovector[2*i-2]
                      || ovector[2*i-1] != ovector[2*i+1])
                    {
                      if (ovector[2*i] >= 0 && ovector[2*i+1] > 0)
                        {
                          te(pos_match,0) = double (ovector[2*i]+1);
                          te(pos_match++,1) = double (ovector[2*i+1]);
                        }
                    }
              te.resize(pos_match,2);
              s = double (ovector[0]+1);
              e = double (ovector[1]);

              const char **listptr;
              int status = pcre_get_substring_list(buffer.c_str(), ovector, 
                                                   matches, &listptr);

              if (status == PCRE_ERROR_NOMEMORY)
                {
                  error("%s: cannot allocate memory in pcre_get_substring_list",
                        nm.c_str());
                  pcre_free(re);
                  return 0;
                }

              Cell cell_t (dim_vector(1,pos_match));
              string_vector named_tokens(nnames);
              int pos_offset = 0;
              pos_match = 0;
              for (int i = 1; i < matches; i++)
                if (ovector[2*i] >= 0 && ovector[2*i+1] > 0)
                  {
                    if (i == 1 || ovector[2*i] != ovector[2*i-2]
                        || ovector[2*i-1] != ovector[2*i+1])
                      {
                        if (namecount > 0)
                          named_tokens(named_idx(i-pos_offset-1)) = 
                            std::string(*(listptr+nidx[i-pos_offset-1]));    
                        cell_t(pos_match++) = 
                          std::string(*(listptr+i));
                      }
                    else
                      pos_offset++;
                }

              m =  std::string(*listptr);
              t = cell_t;

              pcre_free_substring_list(listptr);

              regexp_elem new_elem (named_tokens, t, m, te, s, e);
              lst.push_back (new_elem);
              idx = ovector[1];
              sz++;

              if (once || idx >= buffer.length ())
                break;

            }
        }

      pcre_free(re);
#else
      regex_t compiled;
      int err=regcomp(&compiled, pattern.c_str(), REG_EXTENDED | 
                      (case_insensitive ? REG_ICASE : 0));
      if (err)
        {
          int len = regerror(err, &compiled, 0, 0);
          OCTAVE_LOCAL_BUFFER (char, errmsg, len);
          regerror(err, &compiled, errmsg, len);
          error("%s: %s in pattern (%s)", nm.c_str(), errmsg, 
                pattern.c_str());
          regfree(&compiled);
          return 0;
        }

      int subexpr = 1;
      int idx = 0;
      for (unsigned int i=0; i < pattern.length(); i++)
          subexpr += ( pattern[i] == '(' ? 1 : 0 );
      OCTAVE_LOCAL_BUFFER (regmatch_t, match, subexpr );

      while(true)
        {
          OCTAVE_QUIT; 

          if (regexec(&compiled, buffer.c_str() + idx, subexpr, 
                      match, (idx ? REG_NOTBOL : 0)) == 0) 
            {
              // Count actual matches
              int matches = 0;
              while (matches < subexpr && match[matches].rm_so >= 0) 
                matches++;

              if (matches == 0 || match[0].rm_eo == 0)
                break;

              s = double (match[0].rm_so+1+idx);
              e = double (match[0].rm_eo+idx);
              Matrix te(matches-1,2);
              for (int i = 1; i < matches; i++)
                {
                  te(i-1,0) = double (match[i].rm_so+1+idx);
                  te(i-1,1) = double (match[i].rm_eo+idx);
                }

              m =  buffer.substr (match[0].rm_so+idx, 
                                         match[0].rm_eo-match[0].rm_so);

              Cell cell_t (dim_vector(1,matches-1));
              for (int i = 1; i < matches; i++)
                cell_t(i-1) = buffer.substr (match[i].rm_so+idx, 
                                             match[i].rm_eo-match[i].rm_so);
              t = cell_t;

              idx += match[0].rm_eo;

              string_vector sv;
              regexp_elem new_elem (sv, t, m, te, s, e);
              lst.push_back (new_elem);
              sz++;

              if (once)
                break;
            }
          else
            break;
        }
      regfree(&compiled);
#endif
    }
#else
  error ("%s: not available in this version of Octave", nm.c_str());
#endif
  return sz;
}

static octave_value_list
octregexp (const octave_value_list &args, int nargout, const std::string &nm,
           bool case_insensitive)
{
  octave_value_list retval;
  int nargin = args.length();
  std::list<regexp_elem> lst;
  string_vector named;
  int nopts;
  bool once;
  int sz = octregexp_list (args, nm, case_insensitive, lst, named, nopts, once);

  if (! error_state)
    {
      // Converted the linked list in the correct form for the return values

      octave_idx_type i = 0;
#ifdef HAVE_PCRE
      Octave_map nmap;
      if (sz == 1)
        {
          for (int j = 0; j < named.length(); j++)
            nmap.assign (named(j), lst.begin()->named_token(j));
          retval(5) = nmap;
        }
      else
        {
          for (int j = 0; j < named.length (); j++)
            {
              i = 0;
              Cell tmp(dim_vector (1, sz));
              for (const_iterator p = lst.begin(); p != lst.end(); p++)
                tmp(i++) = p->named_token(j);
              nmap.assign (named(j), octave_value (tmp));
            }
          retval(5) = nmap;
        }
#else
      retval(5) = Octave_map();
#endif

      if (once)
        retval(4) = sz ? lst.front ().t : Cell();
      else
        {
          Cell t (dim_vector(1, sz));
          i = 0;
          for (const_iterator p = lst.begin(); p != lst.end(); p++)
            t(i++) = p->t;
          retval(4) = t;
        }

      if (once)
        retval(3) = sz ? lst.front ().m : std::string();
      else
        {
          Cell m (dim_vector(1, sz));
          i = 0;
          for (const_iterator p = lst.begin(); p != lst.end(); p++)
            m(i++) = p->m;
          retval(3) = m;
        }

      if (once)
        retval(2) = sz ? lst.front ().te : Matrix();
      else
        {
          Cell te (dim_vector(1, sz));
          i = 0;
          for (const_iterator p = lst.begin(); p != lst.end(); p++)
            te(i++) = p->te;
          retval(2) = te;
        }

      if (once)
        {
          if (sz)
            retval(1) = lst.front ().e;
          else
            retval(1) = Matrix();
        }
      else
        {
          NDArray e (dim_vector(1, sz));
          i = 0;
          for (const_iterator p = lst.begin(); p != lst.end(); p++)
            e(i++) = p->e;
          retval(1) = e;
        }

      if (once)
        {
          if (sz)
            retval(0) = lst.front ().s;
          else
            retval(0) = Matrix();
        }
      else
        {
          NDArray s (dim_vector(1, sz));

          i = 0;
          for (const_iterator p = lst.begin(); p != lst.end(); p++)
            s(i++) = p->s;
          retval(0) = s;
        }

      // Alter the order of the output arguments
      if (nopts > 0)
        {
          int n = 0;
          octave_value_list new_retval;
          new_retval.resize(nargout);

          OCTAVE_LOCAL_BUFFER (int, arg_used, 6);
          for (int j = 0; j < 6; j++)
            arg_used[j] = false;
          
          for (int j = 2; j < nargin; j++)
            {
              int k = 0;
              std::string str = args(j).string_value();
              std::transform (str.begin (), str.end (), str.begin (), tolower);
              if (str.find("once", 0) == 0
                  || str.find("stringanchors", 0) == 0
                  || str.find("lineanchors", 0) == 0
                  || str.find("matchcase", 0) == 0
                  || str.find("ignorecase", 0) == 0
                  || str.find("dotall", 0) == 0
                  || str.find("dotexceptnewline", 0) == 0
                  || str.find("literalspacing", 0) == 0
                  || str.find("freespacing", 0) == 0
              )
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
              for (int j = 0; j < 6; j++)
                {
                  if (! arg_used[j])
                    new_retval(n++) = retval(j);
                }
            }

          retval = new_retval;
        }
    }

  return retval;
}

static octave_value_list
octcellregexp (const octave_value_list &args, int nargout, const std::string &nm,
               bool case_insensitive)
{
  octave_value_list retval;

  if (args(0).is_cell())
    {
      OCTAVE_LOCAL_BUFFER (Cell, newretval, nargout);
      octave_value_list new_args = args;
      Cell cellstr = args(0).cell_value();
      if (args(1).is_cell())
        {
          Cell cellpat = args(1).cell_value();

          if (cellpat.numel() == 1)
            {
              for (int j = 0; j < nargout; j++)
                newretval[j].resize(cellstr.dims());

              new_args(1) = cellpat(0);

              for (octave_idx_type i = 0; i < cellstr.numel (); i++)
                {
                  new_args(0) = cellstr(i);
                  octave_value_list tmp = octregexp (new_args, nargout, nm, 
                                                     case_insensitive);

                  if (error_state)
                    break;

                  for (int j = 0; j < nargout; j++)
                    newretval[j](i) = tmp(j);
                }
            }
          else if (cellstr.numel() == 1)
            {
              for (int j = 0; j < nargout; j++)
                newretval[j].resize(cellpat.dims());

              new_args(0) = cellstr(0);

              for (octave_idx_type i = 0; i < cellpat.numel (); i++)
                {
                  new_args(1) = cellpat(i);
                  octave_value_list tmp = octregexp (new_args, nargout, nm, 
                                                     case_insensitive);

                  if (error_state)
                    break;

                  for (int j = 0; j < nargout; j++)
                    newretval[j](i) = tmp(j);
                }
            }
          else if (cellstr.numel() == cellpat.numel())
            {

              if (cellstr.dims() != cellpat.dims())
                error ("%s: Inconsistent cell array dimensions", nm.c_str());
              else
                {
                  for (int j = 0; j < nargout; j++)
                    newretval[j].resize(cellstr.dims());

                  for (octave_idx_type i = 0; i < cellstr.numel (); i++)
                    {
                      new_args(0) = cellstr(i);
                      new_args(1) = cellpat(i);

                      octave_value_list tmp = octregexp (new_args, nargout, nm, 
                                                         case_insensitive);

                      if (error_state)
                        break;

                      for (int j = 0; j < nargout; j++)
                        newretval[j](i) = tmp(j);
                    }
                }
            }
          else
            error ("regexp: cell array arguments must be scalar or equal size");
        }
      else
        {
          for (int j = 0; j < nargout; j++)
            newretval[j].resize(cellstr.dims());

          for (octave_idx_type i = 0; i < cellstr.numel (); i++)
            {
              new_args(0) = cellstr(i);
              octave_value_list tmp = octregexp (new_args, nargout, nm, case_insensitive);

              if (error_state)
                break;

              for (int j = 0; j < nargout; j++)
                newretval[j](i) = tmp(j);
            }
        }

      if (!error_state)
        for (int j = 0; j < nargout; j++)
          retval(j) = octave_value (newretval[j]);
    }
  else if (args(1).is_cell())
    {
      OCTAVE_LOCAL_BUFFER (Cell, newretval, nargout);
      octave_value_list new_args = args;
      Cell cellpat = args(1).cell_value();

      for (int j = 0; j < nargout; j++)
        newretval[j].resize(cellpat.dims());

      for (octave_idx_type i = 0; i < cellpat.numel (); i++)
        {
          new_args(1) = cellpat(i);
          octave_value_list tmp = octregexp (new_args, nargout, nm, case_insensitive);

          if (error_state)
            break;

          for (int j = 0; j < nargout; j++)
            newretval[j](i) = tmp(j);
        }

      if (!error_state)
        for (int j = 0; j < nargout; j++)
          retval(j) = octave_value (newretval[j]);
    }
  else
    retval = octregexp (args, nargout, nm, case_insensitive);

  return retval;

}

DEFUN_DLD (regexp, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {[@var{s}, @var{e}, @var{te}, @var{m}, @var{t}, @var{nm}] =} regexp (@var{str}, @var{pat})\n\
@deftypefnx {Loadable Function} {[@dots{}] =} regexp (@var{str}, @var{pat}, \"@var{opt1}\", @dots{})\n\
Regular expression string matching.  Search for @var{pat} in @var{str} and\n\
return the positions and substrings of any matches, or empty values if there\n\
are none.  Note, some features and extended options are only available when\n\
Octave is compiled with support for Perl Compatible Regular Expressions\n\
(PCRE).\n\
\n\
The matched pattern @var{pat} can include any of the standard regex\n\
operators, including:\n\
\n\
@table @code\n\
@item .\n\
Match any character\n\
\n\
@item * + ? @{@}\n\
Repetition operators, representing\n\
@table @code\n\
@item *\n\
Match zero or more times\n\
\n\
@item +\n\
Match one or more times\n\
\n\
@item ?\n\
Match zero or one times\n\
\n\
@item @{@var{n}@}\n\
Match exactly @var{n} times\n\
\n\
@item @{@var{n},@}\n\
Match @var{n} or more times\n\
\n\
@item @{@var{m},@var{n}@}\n\
Match between @var{m} and @var{n} times\n\
@end table\n\
\n\
@item [@dots{}] [^@dots{}]\n\
\n\
List operators.  The pattern will match any character listed between \"[\"\n\
and \"]\".  If the first character is \"^\" then the pattern is inverted and\n\
any character except those listed between brackets will match\n\
\n\
@item ()\n\
Grouping operator\n\
\n\
@item |\n\
Alternation operator.  Match one of a choice of regular expressions.  The\n\
alternatives must be delimited by the grouping operator @code{()} above\n\
\n\
@item ^ $\n\
Anchoring operators.  Requires pattern to occur at the start (@code{^}) or\n\
end (@code{$}) of the string\n\
@end table\n\
\n\
In addition, the following escaped characters have special meaning.  Note,\n\
it is recommended to quote @var{pat} in single quotes, rather than double\n\
quotes, to avoid the escape sequences being interpreted by Octave before\n\
being passed to @code{regexp}.\n\
\n\
@table @code\n\
@item \\b\n\
Match a word boundary\n\
\n\
@item \\B\n\
Match within a word\n\
\n\
@item \\w\n\
Match any word character\n\
\n\
@item \\W\n\
Match any non-word character\n\
\n\
@item \\<\n\
Match the beginning of a word\n\
\n\
@item \\>\n\
Match the end of a word\n\
\n\
@item \\s\n\
Match any whitespace character\n\
\n\
@item \\S\n\
Match any non-whitespace character\n\
\n\
@item \\d\n\
Match any digit\n\
\n\
@item \\D\n\
Match any non-digit\n\
@end table\n\
\n\
The outputs of @code{regexp} default to the order given below\n\
\n\
@table @asis\n\
@item @var{s}\n\
The start indices of each matching substring\n\
\n\
@item @var{e}\n\
The end indices of each matching substring\n\
\n\
@item @var{te}\n\
The extents of each matched token surrounded by @code{(@dots{})} in\n\
@var{pat}\n\
\n\
@item @var{m}\n\
A cell array of the text of each match\n\
\n\
@item @var{t}\n\
A cell array of the text of each token matched\n\
\n\
@item @var{nm}\n\
A structure containing the text of each matched named token, with the name\n\
being used as the fieldname.  A named token is denoted by\n\
@code{(?<name>@dots{})} and is only available with PCRE support.\n\
@end table\n\
\n\
Particular output arguments, or the order of the output arguments, can be\n\
selected by additional @var{opt} arguments.  These are strings and the\n\
correspondence between the output arguments and the optional argument\n\
are\n\
\n\
@multitable @columnfractions 0.2 0.3 0.3 0.2\n\
@item @tab 'start'        @tab @var{s}  @tab\n\
@item @tab 'end'          @tab @var{e}  @tab\n\
@item @tab 'tokenExtents' @tab @var{te} @tab\n\
@item @tab 'match'        @tab @var{m}  @tab\n\
@item @tab 'tokens'       @tab @var{t}  @tab\n\
@item @tab 'names'        @tab @var{nm} @tab\n\
@end multitable\n\
\n\
Additional arguments are summarized below.\n\
\n\
@table @samp\n\
@item once\n\
Return only the first occurrence of the pattern.\n\
\n\
@item matchcase\n\
Make the matching case sensitive.  (default)\n\
\n\
Alternatively, use (?-i) in the pattern when PCRE is available.\n\
\n\
@item ignorecase\n\
Ignore case when matching the pattern to the string.\n\
\n\
Alternatively, use (?i) in the pattern when PCRE is available.\n\
\n\
@item stringanchors\n\
Match the anchor characters at the beginning and end of the string.  \n\
(default)\n\
\n\
Alternatively, use (?-m) in the pattern when PCRE is available.\n\
\n\
@item lineanchors\n\
Match the anchor characters at the beginning and end of the line.\n\
Only available when Octave is compiled with PCRE.\n\
\n\
Alternatively, use (?m) in the pattern when PCRE is available.\n\
\n\
@item dotall\n\
The pattern @code{.} matches all characters including the newline character.\n\
 (default)\n\
\n\
Alternatively, use (?s) in the pattern when PCRE is available.\n\
\n\
@item dotexceptnewline\n\
The pattern @code{.} matches all characters except the newline character.\n\
Only available when Octave is compiled with PCRE.\n\
\n\
Alternatively, use (?-s) in the pattern when PCRE is available.\n\
\n\
@item literalspacing\n\
All characters in the pattern, including whitespace, are significant and are\n\
used in pattern matching.  (default)\n\
\n\
Alternatively, use (?-x) in the pattern when PCRE is available.\n\
\n\
@item freespacing\n\
The pattern may include arbitrary whitespace and also comments beginning with\n\
the character @samp{#}.\n\
Only available when Octave is compiled with PCRE.\n\
\n\
Alternatively, use (?x) in the pattern when PCRE is available.\n\
\n\
@end table\n\
@seealso{regexpi,strfind,regexprep}\n\
@end deftypefn")
{
  octave_value_list retval;
  int nargin = args.length();

  if (nargin < 2)
    print_usage ();
  else if (args(0).is_cell() || args(1).is_cell())
    retval = octcellregexp (args, nargout, "regexp", false);
  else
    retval = octregexp (args, nargout, "regexp", false);

  return retval;
}

/*

## PCRE_ERROR_MATCHLIMIT test
%!test
%! s=sprintf('\t4\n0000\t-0.00\t-0.0000\t4\t-0.00\t-0.0000\t4\n0000\t-0.00\t-0.0000\t0\t-0.00\t-');
%! ws = warning("query");
%! unwind_protect
%!   warning("off");
%!   regexp(s, '(\s*-*\d+[.]*\d*\s*)+\n');
%! unwind_protect_cleanup
%!   warning(ws);
%! end_unwind_protect

## seg-fault test
%!assert(regexp("abcde","."),[1,2,3,4,5])

## Check that anchoring of pattern works correctly
%!assert(regexp('abcabc','^abc'),1);
%!assert(regexp('abcabc','abc$'),4);
%!assert(regexp('abcabc','^abc$'),zeros(1,0));

%!test
%! [s, e, te, m, t] = regexp(' No Match ', 'f(.*)uck');
%! assert (s,zeros(1,0))
%! assert (e,zeros(1,0))
%! assert (te,cell(1,0))
%! assert (m, cell(1,0))
%! assert (t, cell(1,0))

%!test
%! [s, e, te, m, t] = regexp(' FiRetrUck ', 'f(.*)uck');
%! assert (s,zeros(1,0))
%! assert (e,zeros(1,0))
%! assert (te,cell(1,0))
%! assert (m, cell(1,0))
%! assert (t, cell(1,0))

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
%! assert (isempty(te))
%! assert (m,'short')
%! assert (isempty(t))

%!test
%! [m, te, e, s, t] = regexp('short test string','\w*r\w*','once', 'match', 'tokenExtents', 'end', 'start', 'tokens');
%! assert (s,1)
%! assert (e,5)
%! assert (isempty(te))
%! assert (m,'short')
%! assert (isempty(t))

%!testif HAVE_PCRE
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
%! assert (!isempty(fieldnames(nm)))
%! assert (sort(fieldnames(nm)),{'word1';'word2'})
%! assert (nm.word1,'short')
%! assert (nm.word2,'test')

%!testif HAVE_PCRE
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
%! assert (!isempty(fieldnames(nm)))
%! assert (sort(fieldnames(nm)),{'word1';'word2'})
%! assert (nm.word1,'short')
%! assert (nm.word2,'test')

%!testif HAVE_PCRE
%! ## This test is expected to fail if PCRE is not installed
%! [t, nm] = regexp("John Davis\nRogers, James",'(?<first>\w+)\s+(?<last>\w+)|(?<last>\w+),\s+(?<first>\w+)','tokens','names');
%! assert (size(t), [1,2]);
%! assert (t{1}{1},'John');
%! assert (t{1}{2},'Davis');
%! assert (t{2}{1},'Rogers');
%! assert (t{2}{2},'James');
%! assert (size(nm), [1,1]);
%! assert (nm.first{1},'John');
%! assert (nm.first{2},'James');
%! assert (nm.last{1},'Davis');
%! assert (nm.last{2},'Rogers');

%!testif HAVE_PCRE
%! # Parenthesis in named token (ie (int)) causes a problem
%! assert (regexp('qwe int asd', ['(?<typestr>(int))'], 'names'), struct ('typestr', 'int'));

%!assert(regexp("abc\nabc",'.'),[1:7])
%!assert(regexp("abc\nabc",'.','dotall'),[1:7])
%!testif HAVE_PCRE
%! assert(regexp("abc\nabc",'(?s).'),[1:7])
%! assert(regexp("abc\nabc",'.','dotexceptnewline'),[1,2,3,5,6,7])
%! assert(regexp("abc\nabc",'(?-s).'),[1,2,3,5,6,7])

%!assert(regexp("caseCaSe",'case'),1)
%!assert(regexp("caseCaSe",'case',"matchcase"),1)
%!assert(regexp("caseCaSe",'case',"ignorecase"),[1,5])
%!testif HAVE_PCRE
%! assert(regexp("caseCaSe",'(?-i)case'),1)
%! assert(regexp("caseCaSe",'(?i)case'),[1,5])

%!assert (regexp("abc\nabc",'c$'),7)
%!assert (regexp("abc\nabc",'c$',"stringanchors"),7)
%!testif HAVE_PCRE
%! assert (regexp("abc\nabc",'(?-m)c$'),7)
%! assert (regexp("abc\nabc",'c$',"lineanchors"),[3,7])
%! assert (regexp("abc\nabc",'(?m)c$'),[3,7])

%!assert (regexp("this word",'s w'),4)
%!assert (regexp("this word",'s w','literalspacing'),4)
%!testif HAVE_PCRE
%! assert (regexp("this word",'(?-x)s w','literalspacing'),4)
%! assert (regexp("this word",'s w','freespacing'),zeros(1,0))
%! assert (regexp("this word",'(?x)s w'),zeros(1,0))

%!error regexp('string', 'tri', 'BadArg');
%!error regexp('string');

%!assert(regexp({'asdfg-dfd';'-dfd-dfd-';'qasfdfdaq'},'-'),{6;[1,5,9];zeros(1,0)})
%!assert(regexp({'asdfg-dfd','-dfd-dfd-','qasfdfdaq'},'-'),{6,[1,5,9],zeros(1,0)})
%!assert(regexp({'asdfg-dfd';'-dfd-dfd-';'qasfdfdaq'},{'-';'f';'q'}),{6;[3,7];[1,9]})
%!assert(regexp('Strings',{'t','s'}),{2,7})

## Test case for lookaround operators
%!testif HAVE_PCRE
%! assert(regexp('Iraq','q(?!u)'),4)
%! assert(regexp('quit','q(?!u)'), zeros(1,0))
%! assert(regexp('quit','q(?=u)','match'), {'q'})
%! assert(regexp("quit",'q(?=u+)','match'), {'q'})
%! assert(regexp("qit",'q(?=u+)','match'), cell(1,0))
%! assert(regexp("qit",'q(?=u*)','match'), {'q'})
%! assert(regexp('thingamabob','(?<=a)b'), 9)

*/

DEFUN_DLD (regexpi, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {[@var{s}, @var{e}, @var{te}, @var{m}, @var{t}, @var{nm}] =} regexpi (@var{str}, @var{pat})\n\
@deftypefnx {Loadable Function} {[@dots{}] =} regexpi (@var{str}, @var{pat}, \"@var{opt1}\", @dots{})\n\
\n\
Case insensitive regular expression string matching.  Search for @var{pat} in\n\
@var{str} and return the positions and substrings of any matches, or empty\n\
values if there are none.  @xref{doc-regexp,,regexp}, for details on the\n\
syntax of the search pattern.\n\
@seealso{regexp}\n\
@end deftypefn")
{
  octave_value_list retval;
  int nargin = args.length();

  if (nargin < 2)
    print_usage ();
  else if (args(0).is_cell() || args(1).is_cell())
    retval = octcellregexp (args, nargout, "regexpi", true);
  else
    retval = octregexp (args, nargout, "regexpi", true);

  return retval;
}

/*

## seg-fault test
%!assert(regexpi("abcde","."),[1,2,3,4,5])

## Check that anchoring of pattern works correctly
%!assert(regexpi('abcabc','^ABC'),1);
%!assert(regexpi('abcabc','ABC$'),4);
%!assert(regexpi('abcabc','^ABC$'),zeros(1,0));

%!test
%! [s, e, te, m, t] = regexpi(' No Match ', 'f(.*)uck');
%! assert (s,zeros(1,0))
%! assert (e,zeros(1,0))
%! assert (te,cell(1,0))
%! assert (m, cell(1,0))
%! assert (t, cell(1,0))

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
%! assert (isempty(te))
%! assert (m,'ShoRt')
%! assert (isempty(t))

%!test
%! [m, te, e, s, t] = regexpi('ShoRt Test String','\w*r\w*','once', 'match', 'tokenExtents', 'end', 'start', 'tokens');
%! assert (s,1)
%! assert (e,5)
%! assert (isempty(te))
%! assert (m,'ShoRt')
%! assert (isempty(t))

%!testif HAVE_PCRE
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
%! assert (!isempty(fieldnames(nm)))
%! assert (sort(fieldnames(nm)),{'word1';'word2'})
%! assert (nm.word1,'ShoRt')
%! assert (nm.word2,'Test')

%!testif HAVE_PCRE
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
%! assert (!isempty(fieldnames(nm)))
%! assert (sort(fieldnames(nm)),{'word1';'word2'})
%! assert (nm.word1,'ShoRt')
%! assert (nm.word2,'Test')

%!assert(regexpi("abc\nabc",'.'),[1:7])
%!assert(regexpi("abc\nabc",'.','dotall'),[1:7])
%!testif HAVE_PCRE
%! assert(regexpi("abc\nabc",'(?s).'),[1:7])
%! assert(regexpi("abc\nabc",'.','dotexceptnewline'),[1,2,3,5,6,7])
%! assert(regexpi("abc\nabc",'(?-s).'),[1,2,3,5,6,7])

%!assert(regexpi("caseCaSe",'case'),[1,5])
%!assert(regexpi("caseCaSe",'case',"matchcase"),1)
%!assert(regexpi("caseCaSe",'case',"ignorecase"),[1,5])
%!testif HAVE_PCRE
%! assert(regexpi("caseCaSe",'(?-i)case'),1)
%! assert(regexpi("caseCaSe",'(?i)case'),[1,5])

%!assert (regexpi("abc\nabc",'C$'),7)
%!assert (regexpi("abc\nabc",'C$',"stringanchors"),7)
%!testif HAVE_PCRE
%! assert (regexpi("abc\nabc",'(?-m)C$'),7)
%! assert (regexpi("abc\nabc",'C$',"lineanchors"),[3,7])
%! assert (regexpi("abc\nabc",'(?m)C$'),[3,7])

%!assert (regexpi("this word",'S w'),4)
%!assert (regexpi("this word",'S w','literalspacing'),4)
%!testif HAVE_PCRE
%! assert (regexpi("this word",'(?-x)S w','literalspacing'),4)
%! assert (regexpi("this word",'S w','freespacing'),zeros(1,0))
%! assert (regexpi("this word",'(?x)S w'),zeros(1,0))

%!error regexpi('string', 'tri', 'BadArg');
%!error regexpi('string');

%!assert(regexpi({'asdfg-dfd';'-dfd-dfd-';'qasfdfdaq'},'-'),{6;[1,5,9];zeros(1,0)})
%!assert(regexpi({'asdfg-dfd','-dfd-dfd-','qasfdfdaq'},'-'),{6,[1,5,9],zeros(1,0)})
%!assert(regexpi({'asdfg-dfd';'-dfd-dfd-';'qasfdfdaq'},{'-';'f';'q'}),{6;[3,7];[1,9]})
%!assert(regexpi('Strings',{'t','s'}),{2,[1,7]})

*/


static octave_value
octregexprep (const octave_value_list &args, const std::string &nm)
{
  octave_value retval;
  int nargin = args.length();

  // Make sure we have string,pattern,replacement
  const std::string buffer = args(0).string_value ();
  if (error_state) return retval;
  const std::string pattern = args(1).string_value ();
  if (error_state) return retval;
  const std::string replacement = args(2).string_value ();
  if (error_state) return retval;
  
  // Pack options excluding 'tokenize' and various output
  // reordering strings into regexp arg list
  octave_value_list regexpargs(nargin-1,octave_value());
  regexpargs(0) = args(0);
  regexpargs(1) = args(1);
  int len=2;
  for (int i = 3; i < nargin; i++) 
    {
      const std::string opt = args(i).string_value();
      if (opt != "tokenize" && opt != "start" && opt != "end"
          && opt != "tokenextents" && opt != "match" && opt != "tokens"
          && opt != "names"  && opt != "warnings") 
        {
          regexpargs(len++) = args(i);
        }
    }
  regexpargs.resize(len);
  
  // Identify replacement tokens; build a vector of group numbers in
  // the replacement string so that we can quickly calculate the size 
  // of the replacement.
  int tokens = 0;
  for (size_t i=1; i < replacement.size(); i++) 
    {
      if (replacement[i-1]=='$' && isdigit(replacement[i])) 
        {
          tokens++, i++;
        }
    }
  std::vector<int> token(tokens);
  int kk = 0;
  for (size_t i = 1; i < replacement.size(); i++) 
    {
      if (replacement[i-1]=='$' && isdigit(replacement[i])) 
        {
          token[kk++] = replacement[i]-'0';
          i++;
        }
    }

  // Perform replacement
  std::string rep;
  if (tokens > 0) 
    {
      std::list<regexp_elem> lst;
      string_vector named;
      int nopts;
      bool once;
      int sz = octregexp_list (regexpargs, nm , false, lst, named, nopts, once);

      if (error_state)
        return retval;
      if (sz == 0)
        {
          retval = args(0);
          return retval;
        }

      // Determine replacement length
      const size_t replen = replacement.size() - 2*tokens;
      int delta = 0;
      const_iterator p = lst.begin();
      for (int i = 0; i < sz; i++) 
        {
          OCTAVE_QUIT;

          const Matrix pairs(p->te);
          size_t pairlen = 0;
          for (int j = 0; j < tokens; j++) 
            {
              if (token[j] == 0) 
                pairlen += static_cast<size_t>(p->e - p->s) + 1;
              else if (token[j] <= pairs.rows()) 
                pairlen += static_cast<size_t>(pairs(token[j]-1,1) - 
                                               pairs(token[j]-1,0)) + 1;
            }
          delta += static_cast<int>(replen + pairlen) - 
            static_cast<int>(p->e - p->s + 1);
          p++;
        }
      
      // Build replacement string
      rep.reserve(buffer.size()+delta);
      size_t from = 0;
      p = lst.begin();
      for (int i=0; i < sz; i++) 
        {
          OCTAVE_QUIT;

          const Matrix pairs(p->te);
          rep.append(&buffer[from], static_cast<size_t>(p->s - 1) - from);
          from = static_cast<size_t>(p->e - 1) + 1;
          for (size_t j = 1; j < replacement.size(); j++) 
            {
              if (replacement[j-1]=='$' && isdigit(replacement[j])) 
                {
                  int k = replacement[j]-'0';
                  if (k == 0) 
                    { 
                      // replace with entire match
                      rep.append(&buffer[static_cast<size_t>(p->e - 1)],
                                 static_cast<size_t>(p->e - p->s) + 1);
                    } 
                  else if (k <= pairs.rows()) 
                    {
                      // replace with group capture
                      rep.append(&buffer[static_cast<size_t>(pairs(k-1,0)-1)],
                                 static_cast<size_t>(pairs(k-1,1) - 
                                                     pairs(k-1,0))+1);
                    }
                  else 
                    {
                      // replace with nothing
                    }
                  j++;
                } 
              else 
                {
                  rep.append(1,replacement[j-1]);
                }
              if (j+1 == replacement.size()) 
                {
                  rep.append(1,replacement[j]);
                }
            }
          p++;
        }
      rep.append(&buffer[from],buffer.size()-from);
    } 
  else 
    {
      std::list<regexp_elem> lst;
      string_vector named;
      int nopts;
      bool once;
      int sz = octregexp_list (regexpargs, nm, false, lst, named, nopts, once);

      if (error_state)
        return retval;
      if (sz == 0)
        {
          retval = args(0);
          return retval;
        }

      // Determine replacement length
      const size_t replen = replacement.size();
      int delta = 0;
      const_iterator p = lst.begin();
      for (int i = 0; i < sz; i++) 
        {
          OCTAVE_QUIT;
          delta += static_cast<int>(replen) - 
            static_cast<int>(p->e - p->s + 1);
          p++;
        }

      // Build replacement string
      rep.reserve(buffer.size()+delta);
      size_t from = 0;
      p = lst.begin();
      for (int i=0; i < sz; i++) 
        {
          OCTAVE_QUIT;
          rep.append(&buffer[from], static_cast<size_t>(p->s - 1) - from);
          from = static_cast<size_t>(p->e - 1) + 1;
          rep.append(replacement);
          p++;
        }
      rep.append(&buffer[from],buffer.size()-from);
    }
  
  retval = rep;
  return retval;
}

DEFUN_DLD (regexprep, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{outstr} =} regexprep (@var{string}, @var{pat}, @var{repstr})\n\
@deftypefnx {Loadable Function} {@var{outstr} =} regexprep (@var{string}, @var{pat}, @var{repstr}, \"@var{opt1}\", @dots{})\n\
Replace occurrences of pattern @var{pat} in @var{string} with @var{repstr}.\n\
\n\
The pattern is a regular expression as documented for @code{regexp}.\n\
@xref{doc-regexp,,regexp}.\n\
\n\
The replacement string may contain @code{$i}, which substitutes\n\
for the ith set of parentheses in the match string.  For example,\n\
\n\
@example\n\
regexprep(\"Bill Dunn\",'(\\w+) (\\w+)','$2, $1')\n\
@end example\n\
\n\
@noindent\n\
returns \"Dunn, Bill\"\n\
\n\
Options in addition to those of @code{regexp} are\n\
\n\
@table @samp\n\
\n\
@item once\n\
Replace only the first occurrence of @var{pat} in the result.\n\
\n\
@item warnings\n\
This option is present for compatibility but is ignored.\n\
\n\
@end table\n\
@seealso{regexp,regexpi,strrep}\n\
@end deftypefn")
{
  octave_value_list retval;
  int nargin = args.length();

  if (nargin < 3)
    {
      print_usage ();
      return retval;
    }

  if (args(0).is_cell() || args(1).is_cell() || args(2).is_cell())
    {
      Cell str;
      Cell pat;
      Cell rep;
      dim_vector dv0;
      dim_vector dv1(1,1);

      if (args(0).is_cell())
        str = args(0).cell_value();
      else
        str = Cell (args(0));

      if (args(1).is_cell())
        pat = args(1).cell_value();
      else
        pat = Cell (args(1));

      if (args(2).is_cell())
        rep = args(2).cell_value();
      else
        rep = Cell (args(2));

      dv0 = str.dims();
      if (pat.numel() != 1)
        {
          dv1 = pat.dims();
          if (rep.numel() != 1 && dv1 != rep.dims())
            error ("regexprep: Inconsistent cell array dimensions");
        }
      else if (rep.numel() != 1)
        dv1 = rep.dims();

      if (!error_state)
        {
          Cell ret (dv0);
          octave_value_list new_args = args;

          for (octave_idx_type i = 0; i < dv0.numel(); i++)
            {
              new_args(0) = str(i);
              if (pat.numel() == 1)
                new_args(1) = pat(0);
              if (rep.numel() == 1)
                new_args(2) = rep(0);
              for (octave_idx_type j = 0; j < dv1.numel(); j++)
                {
                  if (pat.numel() != 1)
                    new_args(1) = pat(j);
                  if (rep.numel() != 1)
                    new_args(2) = rep(j);
                  new_args(0) = octregexprep (new_args, "regexprep");

                  if (error_state)
                    break;
                }

              if (error_state)
                break;

              ret(i) = new_args(0);
            }

          if (!error_state)
            retval = octave_value (ret);
        }
    }
  else
    retval = octregexprep (args, "regexprep");

  return retval;
}

/*
%!test  # Replace with empty
%! xml = '<!-- This is some XML --> <tag v="hello">some stuff<!-- sample tag--></tag>';
%! t = regexprep(xml,'<[!?][^>]*>','');
%! assert(t,' <tag v="hello">some stuff</tag>')

%!test  # Replace with non-empty
%! xml = '<!-- This is some XML --> <tag v="hello">some stuff<!-- sample tag--></tag>';
%! t = regexprep(xml,'<[!?][^>]*>','?');
%! assert(t,'? <tag v="hello">some stuff?</tag>')

%!test  # Check that 'tokenize' is ignored
%! xml = '<!-- This is some XML --> <tag v="hello">some stuff<!-- sample tag--></tag>';
%! t = regexprep(xml,'<[!?][^>]*>','','tokenize');
%! assert(t,' <tag v="hello">some stuff</tag>')

%!testif HAVE_PCRE # Capture replacement
%! data = "Bob Smith\nDavid Hollerith\nSam Jenkins";
%! result = "Smith, Bob\nHollerith, David\nJenkins, Sam";
%! t = regexprep(data,'(?m)^(\w+)\s+(\w+)$','$2, $1');
%! assert(t,result)

# Return the original if no match
%!assert(regexprep('hello','world','earth'),'hello')

## Test a general replacement
%!assert(regexprep("a[b]c{d}e-f=g", "[^A-Za-z0-9_]", "_"), "a_b_c_d_e_f_g");

## Make sure it works at the beginning and end
%!assert(regexprep("a[b]c{d}e-f=g", "a", "_"), "_[b]c{d}e-f=g");
%!assert(regexprep("a[b]c{d}e-f=g", "g", "_"), "a[b]c{d}e-f=_");

## Options
%!assert(regexprep("a[b]c{d}e-f=g", "[^A-Za-z0-9_]", "_", "once"), "a_b]c{d}e-f=g");
%!assert(regexprep("a[b]c{d}e-f=g", "[^A-Z0-9_]", "_", "ignorecase"), "a_b_c_d_e_f_g");

## Option combinations
%!assert(regexprep("a[b]c{d}e-f=g", "[^A-Z0-9_]", "_", "once", "ignorecase"), "a_b]c{d}e-f=g");

## End conditions on replacement
%!assert(regexprep("abc","(b)",".$1"),"a.bc");
%!assert(regexprep("abc","(b)","$1"),"abc");
%!assert(regexprep("abc","(b)","$1."),"ab.c");
%!assert(regexprep("abc","(b)","$1.."),"ab..c");

## Test cell array arguments
%!assert(regexprep("abc",{"b","a"},"?"),{"??c"})
%!assert(regexprep({"abc","cba"},"b","?"),{"a?c","c?a"})
%!assert(regexprep({"abc","cba"},{"b","a"},{"?","!"}),{"!?c","c?!"})

# Nasty lookbehind expression
%!testif HAVE_PCRE
%! assert(regexprep('x^(-1)+y(-1)+z(-1)=0','(?<=[a-z]+)\(\-[1-9]*\)','_minus1'),'x^(-1)+y_minus1+z_minus1=0')

*/
