/*

Copyright (C) 2000  Kai Habel

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

/*
29. July 2000 - Kai Habel: first release
2002-04-22 Paul Kienzle
* Use warning(...) function rather than writing to cerr
2006-05-01 Tom Holroyd
* add support for consistent winding in all dimensions; output is
* guaranteed to be simplicial.
*/

#include <sstream>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "oct.h"
#include "Cell.h"

#ifdef HAVE_QHULL
#if defined(HAVE__SNPRINTF) && !defined(HAVE_SNPRINTF)
#define snprintf _snprintf
#endif

extern "C" {
#include <qhull/qhull_a.h>
}

#ifdef NEED_QHULL_VERSION
char qh_version[] = "convhulln.oct 2007-07-24";
#endif
#endif

DEFUN_DLD (convhulln, args, ,
	"-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{H} =} convhulln (@var{p})\n\
@deftypefnx {Loadable Function} {@var{H} =} convhulln (@var{p}, @var{opt})\n\
Returns an index vector to the points of the enclosing convex hull.\n\
The input matrix of size [n, dim] contains n points of dimension dim.\n\n\
If a second optional argument is given, it must be a string or cell array\n\
of strings containing options for the underlying qhull command.  (See\n\
the Qhull documentation for the available options.)  The default options\n\
are \"s Qci Tcv\".\n\n\
@seealso{convhull, delaunayn}\n\
@end deftypefn")
{
  octave_value_list retval;
#ifdef HAVE_QHULL
  std::string options;

  int nargin = args.length();
  if (nargin < 1 || nargin > 2) 
    {
      print_usage ();
      return retval;
    }

  if (nargin == 2) 
    {
      if (args (1).is_string () ) 
	options = args (1).string_value();
      else if (args (1).is_cell ())
	{
	  Cell c = args (1). cell_value ();
	  options = "";
	  for (octave_idx_type i = 0; i < c.numel(); i++)
	    {
	      if (!c.elem(i).is_string()) 
		{
		  error ("convhulln: second argument must be a string or cell array of strings");
		  return retval;
	      }

	      options = options + c.elem(i).string_value() + " ";
	    }
	}
      else
	{
	  error ("convhulln: second argument must be a string or cell array of strings");
	  return retval;
	}
    }
  else
    // turn on some consistency checks
    options = "s Qci Tcv";

  Matrix p(args(0).matrix_value());

  const octave_idx_type dim = p.columns();
  const octave_idx_type n = p.rows();
  p = p.transpose();

  double *pt_array = p.fortran_vec();

  boolT ismalloc = False;

  OCTAVE_LOCAL_BUFFER (char, flags, 250);

  // hmm, lots of options for qhull here
  // QJ guarantees that the output will be triangles
  snprintf(flags, sizeof(flags), "qhull QJ %s", options.c_str());

  if (!qh_new_qhull (dim, n, pt_array, ismalloc, flags, NULL, stderr)) 
    {
      // If you want some debugging information replace the NULL
      // pointer with stdout

      vertexT *vertex,**vertexp;
      facetT *facet;
      setT *vertices;
      unsigned int nf = qh num_facets;

      Matrix idx(nf, dim);

      octave_idx_type j, i = 0;
      FORALLfacets 
	{
	  j = 0;
	  if (!facet->simplicial)
	    // should never happen with QJ
	    error("convhulln: non-simplicial facet");

	  if (dim == 3) 
	    {
	      vertices = qh_facet3vertex (facet);
	      FOREACHvertex_(vertices)
		idx(i, j++) = 1 + qh_pointid(vertex->point);
	      qh_settempfree(&vertices);
	    } 
	  else 
	    {
	      if (facet->toporient ^ qh_ORIENTclock) 
		{
		  FOREACHvertex_(facet->vertices)
		    idx(i, j++) = 1 + qh_pointid(vertex->point);
		} 
	      else 
		{
		  FOREACHvertexreverse12_(facet->vertices)
		    idx(i, j++) = 1 + qh_pointid(vertex->point);
		}
	    }
	  if (j < dim)
	    // likewise but less fatal
	    warning("facet %d only has %d vertices",i,j);
	  i++;
	}
      retval(0) = octave_value(idx);
    }
  qh_freeqhull (!qh_ALL);
  //free long memory
  int curlong, totlong;
  qh_memfreeshort (&curlong, &totlong);
  //free short memory and memory allocator

  if (curlong || totlong) 
    {
      warning("convhulln: did not free %d bytes of long memory (%d pieces)",
	      totlong, curlong);
    }
#else
  error ("convhulln: not available in this version of Octave");
#endif
  return retval;
}
