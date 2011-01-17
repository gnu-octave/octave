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
  16. July 2000 - Kai Habel: first release

  25. September 2002 - Changes by Rafael Laboissiere <rafael@laboissiere.net>

  * Added Qbb option to normalize the input and avoid crashes in Octave.
  * delaunayn accepts now a second (optional) argument that must be a string
  containing extra options to the qhull command.
  * Fixed doc string.  The dimension of the result matrix is [m, dim+1], and
  not [n, dim-1].

  6. June 2006: Changes by Alexander Barth <abarth@marine.usf.edu>

  * triangulate non-simplicial facets 
  * allow options to be specified as cell array of strings
  * change the default options (for compatibility with matlab)
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <string>

#include "Cell.h"
#include "defun-dld.h"
#include "error.h"
#include "oct-obj.h"

#ifdef HAVE_QHULL
extern "C" {
#include <qhull/qhull_a.h>
}

#ifdef NEED_QHULL_VERSION
char qh_version[] = "__delaunayn__.oct 2007-08-21";
#endif
#endif

DEFUN_DLD (__delaunayn__, args, ,
           "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{T} =} __delaunayn__ (@var{P})\n\
@deftypefnx {Loadable Function} {@var{T} =} __delaunayn__ (@var{P}, @var{opt})\n\
Undocumented internal function.\n\
@end deftypefn")

{
  octave_value_list retval;

#ifdef HAVE_QHULL

  retval(0) = 0.0;
  std::string options = "";

  int nargin = args.length ();
  if (nargin < 1 || nargin > 2)
    {
      print_usage ();
      return retval;
    }

  Matrix p (args(0).matrix_value ());
  const octave_idx_type dim = p.columns ();
  const octave_idx_type n = p.rows ();

  // default options
  if (dim <= 3)
    options = "Qt Qbb Qc";
  else
    options = "Qt Qbb Qc Qx";


  if (nargin == 2)
    {
    if (args(1).is_empty ())
      {
        // keep default options
      }
    else if (args(1).is_string ()) 
      {
        // option string is directly provided
        options = args(1).string_value ();
      }
    else if (args(1).is_cell ()) 
      {
        options = "";

        Cell c = args(1).cell_value ();
        for (octave_idx_type i = 0; i < c.numel (); i++)
          {

            if (! c.elem(i).is_string ()) 
              {
                error ("__delaunayn__: all options must be strings");
                return retval;
              }

            options = options + c.elem(i).string_value () + " ";
          }
      }
    else 
      {
        error ("__delaunayn__: OPT argument must be a string, cell array of strings, or empty");
        return retval;
      }
    } 

  //octave_stdout << "options " << options << std::endl;

  if (n > dim + 1) 
    {
      p = p.transpose ();
      double *pt_array = p.fortran_vec ();
      boolT ismalloc = false;

      OCTAVE_LOCAL_BUFFER (char, flags, 250);

      sprintf (flags, "qhull d %s", options.c_str ());

      // If you want some debugging information replace the 0 pointer
      // with stdout or some other file open for writing.

      FILE *outfile = 0;
      FILE *errfile = stderr;

      if (! qh_new_qhull (dim, n, pt_array, ismalloc, flags, outfile, errfile))
        {
          // triangulate non-simplicial facets
          qh_triangulate (); 

          facetT *facet;
          vertexT *vertex, **vertexp;
          octave_idx_type nf = 0, i = 0;

          FORALLfacets
            {
              if (! facet->upperdelaunay)
                nf++;

              // Double check
              if (! facet->simplicial) 
                {
                  error ("__delaunayn__: Qhull returned non-simplicial facets -- try delaunayn with different options");
                  break;
                }
            }

          Matrix simpl (nf, dim+1);

          FORALLfacets
            {
              if (! facet->upperdelaunay) 
                {
                  octave_idx_type j = 0;

                  FOREACHvertex_ (facet->vertices)
                    {
                      // if delaunayn crashes, enable this check
#if 0
                      if (j > dim)
                        {
                          error ("__delaunayn__: internal error. Qhull returned non-simplicial facets");
                          return retval;
                        }
#endif

                      simpl(i, j++) = 1 + qh_pointid(vertex->point);
                    }
                  i++;
                }
            }

          retval(0) = simpl;

          // free long memory
          qh_freeqhull (! qh_ALL);

          // free short memory and memory allocator
          int curlong, totlong;
          qh_memfreeshort (&curlong, &totlong);

          if (curlong || totlong)
            warning ("__delaunay__: did not free %d bytes of long memory (%d pieces)",
                     totlong, curlong);
        }
      else
        error ("__delaunayn__: qhull failed.");
    }
  else if (n == dim + 1) 
    {
      // one should check if nx points span a simplex
      // I will look at this later.
      RowVector vec (n);
      for (octave_idx_type i = 0; i < n; i++) 
        vec(i) = i + 1.0;

      retval(0) = vec;
    }

#else
  error ("__delaunayn__: not available in this version of Octave");
#endif

  return retval;
}
