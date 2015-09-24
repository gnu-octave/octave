/*

Copyright (C) 2014 Jacopo Corno
Copyright (C) 2014 Carlo de Falco
Copyright (C) 2013 Roberto Porcu
Copyright (C) 2013 Mattia Penati

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun.h"

#define MIN2(A,B) ((A)<(B)?(A):(B))
#define MIN3(A,B,C) (MIN2(MIN2((A),(B)),(C)))

DEFUN (levenshtein, args, nargout,
          "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{d} =} levenshtein (@var{str1}, @var{str2}, [@var{u_bound}])\n \
@deftypefnx {Loadable Function} {[@var{d}, @var{mat}] =} levenshtein (@var{str1}, @var{str2}, [@var{u_bound}])\n \
This function file computes the Levenshtein distance between two strings. More details at @url{http://en.wikipedia.org/wiki/Levenshtein_distance}.\n \
This function can be called with one or two output arguments: @var{d} is the distance between the two strings and @var{mat} is the matrix computed by Levenshtein algorithm.\n \
The first and the second input arguments, @var{str1} and @var{str2}, are the two strings to be compared. This comparison is case-sensitive.\n \
The third argument @var{u_bound} is optional and fixes an upper bound for the distance. If the distance is greater than this limit then the function ends and returns a value equal to Inf.\n \
@seealso{odeset}\n\
@end deftypefn")
{
  octave_value_list retval;
  int nargin = args.length ();
  octave_idx_type ub;

  if (nargin < 2
      || nargin > 3
      || nargout > 2)
    {
      print_usage ();
      return retval;
    }

  if (nargin == 3)
    ub = args(2).idx_type_value ();
  else
    ub = std::numeric_limits<int32_t>::max ();

  Array<char> s1 (args(0).char_array_value ());
  char *s1_p = s1.fortran_vec ();

  Array<char> s2 (args(1).char_array_value ());
  char *s2_p = s2.fortran_vec ();

  octave_idx_type
    s1len = s1.length (),
    s2len = s2.length (),
    ii, jj;

  if (! error_state)
    {
      Array<octave_idx_type>
        dist (dim_vector (s1len + 1, s2len + 1), 0);

      for (ii = 1; ii <= s1len; ii++)
        dist.xelem (ii, 0) = ii;

      for (jj = 1; jj <= s2len; jj++)
        dist.xelem (0, jj) = jj;

      for (jj = 1; jj <= s2len; jj++)
        {
          for (ii = 1; ii <= s1len; ii++)
            if (s1_p[ii-1] == s2_p[jj-1])
              dist.xelem (ii, jj) = dist.xelem (ii-1, jj-1);
            else
              dist.xelem (ii, jj) =
                MIN3(dist.xelem (ii-1, jj) + 1,
                     dist.xelem (ii, jj-1) + 1,
                     dist.xelem (ii-1, jj-1) + 1);

          if (dist(MIN2(jj, s1len), jj) > ub)
            {
              retval(0) = std::numeric_limits<int32_t>::max ();
              if (nargout == 2)
                retval(1) = Matrix ();
              return retval;
            }
        }
      retval(0) = dist.xelem (s1len, s2len);
      if (nargout == 2)
        retval(1) = dist;
    }
  return retval;
}
