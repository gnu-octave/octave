////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2009-2023 The Octave Project Developers
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

#include <algorithm>
#include <deque>
#include <limits>
#include <string>

#include "oct-locbuf.h"

#include "Cell.h"
#include "builtin-defun-decls.h"
#include "defun.h"
#include "errwarn.h"
#include "error.h"
#include "ov.h"
#include "unwind-prot.h"
#include "utils.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// This allows safe indexing with char.
// In C++, char may be (and often is) signed!
#define ORD(ch) static_cast<unsigned char>(ch)
#define TABSIZE (std::numeric_limits<unsigned char>::max () + 1)

// This is the quick search algorithm, as described at
// http://www-igm.univ-mlv.fr/~lecroq/string/node19.html
static void
qs_preprocess (const Array<char>& needle,
               octave_idx_type *table)
{
  const char *x = needle.data ();
  octave_idx_type m = needle.numel ();

  for (octave_idx_type i = 0; i < TABSIZE; i++)
    table[i] = m + 1;
  for (octave_idx_type i = 0; i < m; i++)
    table[ORD(x[i])] = m - i;
}

static Array<octave_idx_type>
qs_search (const Array<char>& needle,
           const Array<char>& haystack,
           const octave_idx_type *table,
           bool overlaps = true)
{
  const char *x = needle.data ();
  octave_idx_type m = needle.numel ();
  const char *y = haystack.data ();
  octave_idx_type n = haystack.numel ();

  // We'll use deque because it typically has the most favorable properties for
  // the operation we need.
  std::deque<octave_idx_type> accum;
  if (m == 1)
    {
      // Looking for a single character.
      for (octave_idx_type i = 0; i < n; i++)
        {
          if (y[i] == x[0])
            accum.push_back (i);
        }
    }
  else if (m == 2)
    {
      // Two characters.
      if (overlaps)
        {
          for (octave_idx_type i = 0; i < n-1; i++)
            {
              if (y[i] == x[0] && y[i+1] == x[1])
                accum.push_back (i);
            }
        }
      else
        {
          for (octave_idx_type i = 0; i < n-1; i++)
            {
              if (y[i] == x[0] && y[i+1] == x[1])
                accum.push_back (i++);
            }
        }
    }
  else if (n >= m)
    {
      // General case.
      octave_idx_type j = 0;

      if (overlaps)
        {
          while (j < n - m)
            {
              if (std::equal (x, x + m, y + j))
                accum.push_back (j);
              j += table[ORD(y[j + m])];
            }
        }
      else
        {
          while (j < n - m)
            {
              if (std::equal (x, x + m, y + j))
                {
                  accum.push_back (j);
                  j += m;
                }
              else
                j += table[ORD(y[j + m])];
            }
        }

      if (j == n - m && std::equal (x, x + m, y + j))
        accum.push_back (j);
    }

  octave_idx_type nmatch = accum.size ();
  octave_idx_type one = 1;
  Array<octave_idx_type> result (dim_vector (std::min (one, nmatch), nmatch));
  octave_idx_type k = 0;
  for (const auto& idx : accum)
    result.xelem (k++) = idx;

  return result;
}

DEFUN (strfind, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{idx} =} strfind (@var{str}, @var{pattern})
@deftypefnx {} {@var{idx} =} strfind (@var{cellstr}, @var{pattern})
@deftypefnx {} {@var{idx} =} strfind (@dots{}, "overlaps", @var{val})
@deftypefnx {} {@var{idx} =} strfind (@dots{}, "forcecelloutput", @var{val})
Search for @var{pattern} in the string @var{str} and return the starting
index of every such occurrence in the vector @var{idx}.

If there is no such occurrence, or if @var{pattern} is longer than
@var{str}, or if @var{pattern} itself is empty, then @var{idx} is the empty
array @code{[]}.

The optional argument @qcode{"overlaps"} determines whether the pattern
can match at every position in @var{str} (true), or only for unique
occurrences of the complete pattern (false).  The default is true.

If a cell array of strings @var{cellstr} is specified then @var{idx} is a
cell array of vectors, as specified above.

The optional argument @qcode{"forcecelloutput"} forces @var{idx} to be
returned as a cell array of vectors.  The default is false.

Examples:

@example
@group
strfind ("abababa", "aba")
     @result{} [1, 3, 5]
@end group

@group
strfind ("abababa", "aba", "overlaps", false)
     @result{} [1, 5]
@end group

@group
strfind (@{"abababa", "bebebe", "ab"@}, "aba")
     @result{}
        @{
          [1,1] =

             1   3   5

          [1,2] = [](1x0)
          [1,3] = [](1x0)
        @}
@end group

@group
strfind ("abababa", "aba", "forcecelloutput", true)
     @result{}
        @{
          [1,1] =

             1   3   5
        @}
@end group
@end example
@seealso{regexp, regexpi, find}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin != 4 && nargin != 2)
    print_usage ();

  bool overlaps = true;
  bool forcecelloutput = false;
  if (nargin == 4)
    {
      if (! args(2).is_string () || ! args(3).is_scalar_type ())
        error ("strfind: invalid optional arguments");

      std::string opt = args(2).string_value ();
      std::transform (opt.begin (), opt.end (), opt.begin (), tolower);

      if (opt == "overlaps")
        overlaps = args(3).bool_value ();
      else if (opt == "forcecelloutput")
        forcecelloutput = args(3).bool_value ();
      else
        error ("strfind: unknown option: %s", opt.c_str ());
    }

  octave_value retval;

  octave_value argstr = args(0);
  octave_value argpat = args(1);

  if (argpat.is_string ())
    {
      Array<char> needle = argpat.char_array_value ();
      OCTAVE_LOCAL_BUFFER (octave_idx_type, table, TABSIZE);
      qs_preprocess (needle, table);

      if (argstr.is_string ())
        {
          if (argpat.isempty ())
            // Return a null matrix for null pattern for MW compatibility
            retval = Matrix ();
          else
            retval = octave_value (qs_search (needle,
                                              argstr.char_array_value (),
                                              table, overlaps),
                                   true, true);
          if (forcecelloutput)
            retval = Cell (retval);
        }
      else if (argstr.iscell ())
        {
          const Cell argsc = argstr.cell_value ();
          Cell retc (argsc.dims ());
          octave_idx_type ns = argsc.numel ();

          for (octave_idx_type i = 0; i < ns; i++)
            {
              octave_value argse = argsc(i);
              if (! argse.is_string ())
                error ("strfind: each element of CELLSTR must be a string");

              if (argpat.isempty ())
                retc(i) = Matrix ();
              else
                retc(i) = octave_value (qs_search (needle,
                                                   argse.char_array_value (),
                                                   table, overlaps),
                                        true, true);
            }

          retval = retc;
        }
      else
        error ("strfind: first argument must be a string or cell array of strings");
    }
  else if (argpat.iscell ())
    retval = do_simple_cellfun (Fstrfind, "strfind", args);
  else
    error ("strfind: PATTERN must be a string or cell array of strings");

  return retval;
}

/*
%!assert (strfind ("abababa", "aba"), [1, 3, 5])
%!assert (strfind ("abababa", "aba", "overlaps", false), [1, 5])
%!assert (strfind ("abababa", "aba", "forcecelloutput", false), [1, 3, 5])
%!assert (strfind ("abababa", "aba", "forcecelloutput", true), {[1, 3, 5]})
%!assert (strfind ({"abababa", "bla", "bla"}, "a"), {[1, 3, 5, 7], 3, 3})
%!assert (strfind ({"abababa", "bla", "bla"}, "a", "forcecelloutput", false),
%!        {[1, 3, 5, 7], 3, 3})
%!assert (strfind ({"abababa", "bla", "bla"}, "a", "forcecelloutput", true),
%!        {[1, 3, 5, 7], 3, 3})
%!assert (strfind ("Linux _is_ user-friendly. It just isn't ignorant-friendly or idiot-friendly.", "friendly"), [17, 50, 68])
%!assert (strfind ("abc", ""), [])
%!assert (strfind ("abc", {"", "b", ""}), {[], 2, []})
%!assert (strfind ({"abc", "def"}, ""), {[], []})

%!error strfind ()
%!error strfind ("foo", "bar", 1)
%!error <unknown option: foobar> strfind ("foo", 100, "foobar", 1)
%!error <each element of CELLSTR must be a string> strfind ({"A", 1}, "foo")
%!error <first argument must be a string> strfind (100, "foo")
%!error <PATTERN must be a string> strfind ("foo", 100)
*/

static Array<char>
qs_replace (const Array<char>& str, const Array<char>& pat,
            const Array<char>& rep,
            const octave_idx_type *table,
            bool overlaps = true)
{
  Array<char> ret = str;

  octave_idx_type siz = str.numel ();
  octave_idx_type psiz = pat.numel ();
  octave_idx_type rsiz = rep.numel ();

  if (psiz != 0)
    {
      // Look up matches, without overlaps.
      const Array<octave_idx_type> idx = qs_search (pat, str, table, overlaps);
      octave_idx_type nidx = idx.numel ();

      if (nidx)
        {
          // Compute result size.
          octave_idx_type retsiz;
          if (overlaps)
            {
              retsiz = 0;
              // OMG.  Is this the "right answer" MW always looks for, or
              // someone was just lazy?
              octave_idx_type k = 0;
              for (octave_idx_type i = 0; i < nidx; i++)
                {
                  octave_idx_type j = idx(i);
                  if (j >= k)
                    retsiz += j - k;
                  retsiz += rsiz;
                  k = j + psiz;
                }

              retsiz += siz - k;
            }
          else
            retsiz = siz + nidx * (rsiz - psiz);

          if (retsiz == 0)
            ret.clear (dim_vector (0, 0));
          else
            {
              ret.clear (dim_vector (1, retsiz));
              const char *src = str.data ();
              const char *reps = rep.data ();
              char *dest = ret.fortran_vec ();

              octave_idx_type k = 0;
              for (octave_idx_type i = 0; i < nidx; i++)
                {
                  octave_idx_type j = idx(i);
                  if (j >= k)
                    dest = std::copy (src + k, src + j, dest);
                  dest = std::copy (reps, reps + rsiz, dest);
                  k = j + psiz;
                }

              std::copy (src + k, src + siz, dest);
            }
        }
    }

  return ret;
}

DEFUN (strrep, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{newstr} =} strrep (@var{str}, @var{ptn}, @var{rep})
@deftypefnx {} {@var{newstr} =} strrep (@var{cellstr}, @var{ptn}, @var{rep})
@deftypefnx {} {@var{newstr} =} strrep (@dots{}, "overlaps", @var{val})
Replace all occurrences of the pattern @var{ptn} in the string @var{str}
with the string @var{rep} and return the result.

The optional argument @qcode{"overlaps"} determines whether the pattern
can match at every position in @var{str} (true), or only for unique
occurrences of the complete pattern (false).  The default is true.

@var{s} may also be a cell array of strings, in which case the replacement
is done for each element and a cell array is returned.

Example:

@example
@group
strrep ("This is a test string", "is", "&%$")
    @result{}  "Th&%$ &%$ a test string"
@end group
@end example

@seealso{regexprep, strfind}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin != 3 && nargin != 5)
    print_usage ();

  bool overlaps = true;

  if (nargin == 5)
    {
      if (! args(3).is_string () || ! args(4).is_scalar_type ())
        error ("strrep: invalid optional argument");

      std::string opt = args(3).string_value ();
      if (opt != "overlaps")
        error ("strrep: unknown option: %s", opt.c_str ());

      overlaps = args(4).bool_value ();
    }

  octave_value retval;

  // Aliasing for better code readability
  octave_value argstr = args(0);
  octave_value argpat = args(1);
  octave_value argrep = args(2);

  if (argpat.is_string () && argrep.is_string ())
    {
      const Array<char> pat = argpat.char_array_value ();
      const Array<char> rep = argrep.char_array_value ();

      OCTAVE_LOCAL_BUFFER (octave_idx_type, table, TABSIZE);
      qs_preprocess (pat, table);

      if (argstr.is_string ())
        if (argstr.rows () == 1)  // most common case of a single string
          retval = qs_replace (argstr.char_array_value (), pat, rep,
                               table, overlaps);
        else
          {
            const charMatrix argchm = argstr.char_matrix_value ();
            octave_idx_type nel = argchm.rows ();
            octave_idx_type nc = argchm.columns ();
            charMatrix retchm (nel, 0);

            for (octave_idx_type i = 0; i < nel; i++)
              {
                charMatrix rowchm;
                rowchm = qs_replace (argchm.extract (i, 0, i, nc-1),
                                     pat, rep, table, overlaps);
                retchm.insert (rowchm, i, 0);
              }

            retval = retchm;
          }
      else if (argstr.iscell ())
        {
          const Cell argcell = argstr.cell_value ();
          if (! argcell.iscellstr ())
            error ("strrep: each element of S must be a string");

          Cell retcell (argcell.dims ());
          octave_idx_type nel = argcell.numel ();

          for (octave_idx_type i = 0; i < nel; i++)
            {
              retcell(i) = qs_replace (argcell(i).char_array_value (),
                                       pat, rep, table, overlaps);
            }

          retval = retcell;
        }
      else
        error ("strrep: S must be a string or cell array of strings");
    }
  else if (argpat.iscell () || argrep.iscell ())
    retval = do_simple_cellfun (Fstrrep, "strrep", args);
  else
    error ("strrep: PTN and REP arguments must be strings or cell arrays of strings");

  return retval;
}

/*
%!assert (strrep ("This is a test string", "is", "&%$"),
%!                "Th&%$ &%$ a test string")
%!assert (strrep ("abababc", "abab", "xyz"), "xyzxyzc")
%!assert (strrep ("abababc", "abab", "xyz", "overlaps", false), "xyzabc")
%!assert (strrep ({"Hello World"; "Goodbye World"}, "World", "Jane"),
%!                {"Hello Jane"; "Goodbye Jane"})
%!assert (strrep (char ("Hello World", "Goodbye World"), "World", "Jane"),
%!                char ("Hello Jane", "Goodbye Jane"))

%!assert (size (strrep ("a", "a", "")), [0 0])

%!error strrep ()
%!error strrep ("A")
%!error strrep ("A", "B")
%!error strrep ("A", "B", "C", "D")
%!error strrep ("A", "B", "C", "D", "E", "F")
%!error <invalid optional argument> strrep ("A", "B", "C", 3, true)
%!error <invalid optional argument> strrep ("A", "B", "C", "str", ones (2,2))
%!error <unknown option: foobar> strrep ("A", "B", "C", "foobar", true)
%!error <each element of S must be a string> strrep ({"A", 1.0}, "B", "C")
%!error <S must be a string or cell array of strings> strrep (1.0, "B", "C")
%!error <PTN and REP arguments must be strings> strrep ("A", 1.0, "C")
%!error <PTN and REP arguments must be strings> strrep ("A", "B", 1.0)
*/

OCTAVE_END_NAMESPACE(octave)
