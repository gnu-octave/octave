/*

Copyright (C) 2000-2011 Kai Habel

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

/*
20. Augiust 2000 - Kai Habel: first release
*/

/*
2003-12-14 Rafael Laboissiere <rafael@laboissiere.net>
Added optional second argument to pass options to the underlying
qhull command
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdio>

#include "lo-ieee.h"

#include "Cell.h"
#include "defun-dld.h"
#include "error.h"
#include "oct-obj.h"

#ifdef HAVE_QHULL
extern "C" {
#include <qhull/qhull_a.h>
}

#ifdef NEED_QHULL_VERSION
char qh_version[] = "__voronoi__.oct 2007-07-24";
#endif
#endif

DEFUN_DLD (__voronoi__, args, ,
        "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{tri} =} __voronoi__ (@var{point})\n\
@deftypefnx {Loadable Function} {@var{tri} =} __voronoi__ (@var{point}, @var{options})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;

#ifdef HAVE_QHULL

  retval(0) = 0.0;

  int nargin = args.length ();
  if (nargin < 1 || nargin > 2)
    {
      print_usage ();
      return retval;
    }

  std::string options = "qhull v Fv T0";

  if (nargin == 2)
    {
      if (args(1).is_cellstr ())
        {
          Array<std::string> tmp = args(1).cellstr_value ();

          for (octave_idx_type i = 0; i < tmp.numel (); i++)
            options += " " + tmp(i);
        }
      else if (args(1).is_string ())
        options += " " + args(1).string_value ();
      else
        {
          error ("__voronoi__: OPTIONS argument must be a string or cellstr");
          return retval;
        }
    }

  Matrix p (args(0).matrix_value ());

  const octave_idx_type dim = p.columns ();
  const octave_idx_type np = p.rows ();
  p = p.transpose ();

  double *pt_array = p.fortran_vec ();

  //double  pt_array[dim * np];
  //for (int i = 0; i < np; i++)
  //  {
  //    for (int j = 0; j < dim; j++)
  //      {
  //        pt_array[j+i*dim] = p(i,j);
  //      }
  //  }

  boolT ismalloc = false;

  // If you want some debugging information replace the 0 pointer
  // with stdout or some other file open for writing.

  FILE *outfile = 0;
  FILE *errfile = stderr;

  // Qhull flags argument is not const char*...
  OCTAVE_LOCAL_BUFFER (char, flags, options.length () + 1);

  strcpy (flags, options.c_str ());

  if (! qh_new_qhull (dim, np, pt_array, ismalloc, flags, outfile, errfile))
    {
      facetT *facet;
      vertexT *vertex;

      octave_idx_type i = 0, n = 1, k = 0, m = 0, fidx = 0, j = 0, r = 0;
      OCTAVE_LOCAL_BUFFER (octave_idx_type, ni, np);

      for (i = 0; i < np; i++)
        ni[i] = 0;
      qh_setvoronoi_all ();
      bool infinity_seen = false;
      facetT *neighbor, **neighborp;
      coordT *voronoi_vertex;

      FORALLfacets
        {
          facet->seen = false;
        }

      FORALLvertices
        {
          if (qh hull_dim == 3)
            qh_order_vertexneighbors (vertex);
          infinity_seen = false;

          FOREACHneighbor_ (vertex)
            {
              if (! neighbor->upperdelaunay)
                {
                  if (! neighbor->seen)
                    {
                      n++;
                      neighbor->seen = true;
                    }
                  ni[k]++;
                }
              else if (! infinity_seen)
                {
                  infinity_seen = true;
                  ni[k]++;
                }
            }
          k++;
        }

      Matrix v (n, dim);
      for (octave_idx_type d = 0; d < dim; d++)
        v(0,d) = octave_Inf;

      boolMatrix AtInf (np, 1);
      for (i = 0; i < np; i++)
        AtInf(i) = false;
      octave_value_list F (np, octave_value ());
      k = 0;
      i = 0;

      FORALLfacets
        {
          facet->seen = false;
        }

      FORALLvertices
        {
          if (qh hull_dim == 3)
            qh_order_vertexneighbors(vertex);
          infinity_seen = false;
          RowVector facet_list (ni[k++]);
          m = 0;

          FOREACHneighbor_(vertex)
            {
              if (neighbor->upperdelaunay)
                {
                  if (! infinity_seen)
                    {
                      infinity_seen = true;
                      facet_list(m++) = 1;
                      AtInf(j) = true;
                    }
                }
              else
                {
                  if (! neighbor->seen)
                    {
                      voronoi_vertex = neighbor->center;
                      fidx = neighbor->id;
                      i++;
                      for (octave_idx_type d = 0; d < dim; d++)
                        {
                          v(i,d) = *(voronoi_vertex++);
                        }
                      neighbor->seen = true;
                      neighbor->visitid = i;
                    }
                  facet_list(m++) = neighbor->visitid + 1;
                }
            }
          F(r++) = facet_list;
          j++;
        }

      Cell C(r, 1);
      for (i = 0; i < r; i++)
        C.elem (i) = F(i);

      retval(0) = v;
      retval(1) = C;
      AtInf.resize (r, 1);
      retval(2) = AtInf;

      // free long memory
      qh_freeqhull (! qh_ALL);

      // free short memory and memory allocator
      int curlong, totlong;
      qh_memfreeshort (&curlong, &totlong);

      if (curlong || totlong)
        warning ("__voronoi__: did not free %d bytes of long memory (%d pieces)", totlong, curlong);
    }
  else
    error ("__voronoi__: qhull failed");

#else
  error ("__voronoi__: not available in this version of Octave");
#endif

  return retval;
}

/*

## No test needed for internal helper function.
%!assert (1)

*/
