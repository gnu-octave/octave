////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2000-2023 The Octave Project Developers
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

/*
29. July 2000 - Kai Habel: first release
2002-04-22 Paul Kienzle
* Use warning(...) function rather than writing to cerr
2006-05-01 Tom Holroyd
* add support for consistent winding in all dimensions; output is
* guaranteed to be simplicial.
*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <limits>
#include <string>

#include "Array.h"
#include "dMatrix.h"
#include "oct-locbuf.h"
#include "unwind-prot.h"

#include "defun-dld.h"
#include "error.h"
#include "errwarn.h"
#include "ov.h"

#if defined (HAVE_QHULL)

#  include "oct-qhull.h"

#  if defined (NEED_QHULL_R_VERSION)
char qh_version[] = "convhulln.oct 2007-07-24";
#  endif

static void
free_qhull_memory (qhT *qh)
{
  qh_freeqhull (qh, ! qh_ALL);

  int curlong, totlong;
  qh_memfreeshort (qh, &curlong, &totlong);

  if (curlong || totlong)
    warning ("convhulln: did not free %d bytes of long memory (%d pieces)",
             totlong, curlong);
}

static bool
octave_qhull_dims_ok (octave_idx_type dim, octave_idx_type n, const char *who)
{
  if (sizeof (octave_idx_type) > sizeof (int))
    {
      int maxval = std::numeric_limits<int>::max ();

      if (dim > maxval || n > maxval)
        error ("%s: dimension too large for Qhull", who);
    }

  return true;
}

#endif

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN_DLD (convhulln, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{h} =} convhulln (@var{pts})
@deftypefnx {} {@var{h} =} convhulln (@var{pts}, @var{options})
@deftypefnx {} {[@var{h}, @var{v}] =} convhulln (@dots{})
Compute the convex hull of the set of points @var{pts}.

@var{pts} is a matrix of size [n, dim] containing n points in a space of
dimension dim.

The hull @var{h} is an index vector into the set of points and specifies
which points form the enclosing hull.

An optional second argument, which must be a string or cell array of
strings, contains options passed to the underlying qhull command.  See the
documentation for the Qhull library for details
@url{http://www.qhull.org/html/qh-quick.htm#options}.
The default options depend on the dimension of the input:

@itemize
@item 2D, 3D, 4D: @var{options} = @code{@{"Qt"@}}

@item 5D and higher: @var{options} = @code{@{"Qt", "Qx"@}}
@end itemize

If @var{options} is not present or @code{[]} then the default arguments are
used.  Otherwise, @var{options} replaces the default argument list.
To append user options to the defaults it is necessary to repeat the
default arguments in @var{options}.  Use a null string to pass no arguments.

If the second output @var{v} is requested the volume of the enclosing
convex hull is calculated.
@seealso{convhull, delaunayn, voronoin}
@end deftypefn */)
{
#if defined (HAVE_QHULL)

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_value_list retval;

  Matrix points (args(0).matrix_value ());
  const octave_idx_type dim = points.columns ();
  const octave_idx_type num_points = points.rows ();

  if (! octave_qhull_dims_ok (dim, num_points, "convhulln"))
    return retval;

  points = points.transpose ();

  std::string options;

  if (dim <= 4)
    options = " Qt";
  else
    options = " Qt Qx";

  if (nargin == 2)
    {
      if (args(1).is_string ())
        options = ' ' + args(1).string_value ();
      else if (args(1).isempty ())
        ; // Use default options.
      else if (args(1).iscellstr ())
        {
          options = "";

          Array<std::string> tmp = args(1).cellstr_value ();

          for (octave_idx_type i = 0; i < tmp.numel (); i++)
            options += ' ' + tmp(i);
        }
      else
        error ("convhulln: OPTIONS must be a string, cell array of strings, or empty");
    }

  boolT ismalloc = false;

  // Set the outfile pointer to stdout for status information.
  FILE *outfile = nullptr;
  FILE *errfile = stderr;

  qhT context = { };
  qhT *qh = &context;

  std::string cmd = "qhull" + options;

  int exitcode = qh_new_qhull (qh, dim, num_points, points.fortran_vec (),
                               ismalloc, &cmd[0], outfile, errfile);

  unwind_action free_memory ([qh] () { free_qhull_memory (qh); });

  if (exitcode)
    error ("convhulln: qhull failed");

  bool nonsimp_seen = false;

  octave_idx_type nf = qh->num_facets;

  Matrix idx (nf, dim + 1);

  facetT *facet;

  octave_idx_type i = 0;

  FORALLfacets
  {
    octave_idx_type j = 0;

    if (! (nonsimp_seen || facet->simplicial || qh->hull_dim == 2))
      {
        nonsimp_seen = true;

        if (cmd.find ("QJ") != std::string::npos)
          // Should never happen with QJ.
          error ("convhulln: qhull failed: option 'QJ' returned non-simplicial facet");
      }

    if (dim == 3)
      {
        setT *vertices = qh_facet3vertex (qh, facet);

        vertexT *vertex, **vertexp;

        FOREACHvertex_ (vertices)
        idx(i, j++) = 1 + qh_pointid(qh, vertex->point);

        qh_settempfree (qh, &vertices);
      }
    else
      {
        if (facet->toporient ^ qh_ORIENTclock)
          {
            vertexT *vertex, **vertexp;

            FOREACHvertex_ (facet->vertices)
            idx(i, j++) = 1 + qh_pointid(qh, vertex->point);
          }
        else
          {
            vertexT *vertex, **vertexp;

            FOREACHvertexreverse12_ (facet->vertices)
            idx(i, j++) = 1 + qh_pointid(qh, vertex->point);
          }
      }
    if (j < dim)
      warning ("convhulln: facet %" OCTAVE_IDX_TYPE_FORMAT
               " only has %" OCTAVE_IDX_TYPE_FORMAT
               " vertices", i, j);

    i++;
  }

  // Remove extra dimension if all facets were simplicial.

  if (! nonsimp_seen)
    idx.resize (nf, dim, 0.0);

  if (nargout == 2)
    {
      // Calculate volume of convex hull, taken from qhull src/geom2.c.

      realT area;
      realT dist;

      FORALLfacets
      {
        if (! facet->normal)
          continue;

        if (facet->upperdelaunay && qh->ATinfinity)
          continue;

        facet->f.area = area = qh_facetarea (qh, facet);
        facet->isarea = True;

        if (qh->DELAUNAY)
          {
            if (facet->upperdelaunay == qh->UPPERdelaunay)
              qh->totarea += area;
          }
        else
          {
            qh->totarea += area;
            qh_distplane (qh, qh->interior_point, facet, &dist);
            qh->totvol += -dist * area / qh->hull_dim;
          }
      }

      retval(1) = octave_value (qh->totvol);
    }

  retval(0) = idx;

  return retval;

#else

  octave_unused_parameter (args);
  octave_unused_parameter (nargout);

  err_disabled_feature ("convhulln", "Qhull");

#endif
}

/*
%!testif HAVE_QHULL
%! cube = [0 0 0;1 0 0;1 1 0;0 1 0;0 0 1;1 0 1;1 1 1;0 1 1];
%! [h, v] = convhulln (cube, "Qt");
%! assert (size (h), [12 3]);
%! h = sortrows (sort (h, 2), [1:3]);
%! assert (h,
%!         [1 2 4; 1 2 6; 1 4 8; 1 5 6; 1 5 8; 2 3 4; 2 3 7; 2 6 7; 3 4 7; 4 7 8; 5 6 7; 5 7 8]);
%! assert (v, 1, 10*eps);
%! [h2, v2] = convhulln (cube);  # Test default option = "Qt"
%! assert (size (h2), size (h));
%! h2 = sortrows (sort (h2, 2), [1:3]);
%! assert (h2, h);
%! assert (v2, v, 10*eps);

%!testif HAVE_QHULL
%! cube = [0 0 0;1 0 0;1 1 0;0 1 0;0 0 1;1 0 1;1 1 1;0 1 1];
%! [h, v] = convhulln (cube, "QJ");
%! assert (size (h), [12 3]);
%! assert (sortrows (sort (h, 2), [1:3]),
%!         [1 2 4; 1 2 5; 1 4 5; 2 3 4; 2 3 6; 2 5 6; 3 4 8; 3 6 7; 3 7 8; 4 5 8; 5 6 8; 6 7 8]);
%! assert (v, 1.0, 1e6*eps);

%!testif HAVE_QHULL
%! tetrahedron = [1 1 1;-1 -1 1;-1 1 -1;1 -1 -1];
%! [h, v] = convhulln (tetrahedron);
%! h = sortrows (sort (h, 2), [1 2 3]);
%! assert (h, [1 2 3;1 2 4; 1 3 4; 2 3 4]);
%! assert (v, 8/3, 10*eps);

%!testif HAVE_QHULL
%! triangle = [0 0; 1 1; 1 0; 1 2];
%! h = convhulln (triangle);
%! assert (size (h), [3 2]);
*/

OCTAVE_END_NAMESPACE(octave)
