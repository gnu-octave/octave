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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdio>

#include <limits>
#include <string>

#include "Array.h"
#include "dMatrix.h"
#include "dRowVector.h"
#include "oct-locbuf.h"
#include "unwind-prot.h"

#include "defun-dld.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"

#if defined (HAVE_QHULL)
#  include "oct-qhull.h"
#  if defined (NEED_QHULL_R_VERSION)
char qh_version[] = "__delaunayn__.oct 2007-08-21";
#  endif
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

#if defined (HAVE_QHULL)

static void
free_qhull_memory (qhT *qh)
{
  qh_freeqhull (qh, ! qh_ALL);

  int curlong, totlong;
  qh_memfreeshort (qh, &curlong, &totlong);

  if (curlong || totlong)
    warning ("__delaunayn__: did not free %d bytes of long memory (%d pieces)",
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

DEFUN_DLD (__delaunayn__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{T} =} __delaunayn__ (@var{pts})
@deftypefnx {} {@var{T} =} __delaunayn__ (@var{pts}, @var{options})
Undocumented internal function.
@end deftypefn */)

{
#if defined (HAVE_QHULL)

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_value_list retval;

  retval(0) = 0.0;

  Matrix p (args(0).matrix_value ());
  const octave_idx_type dim = p.columns ();
  const octave_idx_type n = p.rows ();

  if (! octave_qhull_dims_ok (dim, n, "__delaynayn__"))
    return retval;

  // Default options
  std::string options;
  if (dim <= 3)
    options = "Qt Qbb Qc";
  else
    options = "Qt Qbb Qc Qx";

  if (nargin == 2)
    {
      if (args(1).is_string ())
        options = args(1).string_value ();
      else if (args(1).isempty ())
        ;  // Use default options
      else if (args(1).iscellstr ())
        {
          options = "";
          Array<std::string> tmp = args(1).cellstr_value ();

          for (octave_idx_type i = 0; i < tmp.numel (); i++)
            options += tmp(i) + ' ';
        }
      else
        error ("__delaunayn__: OPTIONS argument must be a string, cell array of strings, or empty");
    }

  if (n > dim + 1)
    {
      p = p.transpose ();
      double *pt_array = p.fortran_vec ();
      boolT ismalloc = false;

      std::string cmd = "qhull d " + options;

      // Set the outfile pointer to stdout for status information.
      FILE *outfile = nullptr;

      // Set the errfile pointer to stderr for errors and summary information.
      // Note: pointer cannot be NULL to disable reporting, unlike outfile.
#if defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM) && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM)
      FILE *errfile = std::fopen ("NUL", "w");
#else
      FILE *errfile = std::fopen ("/dev/null", "w");
#endif

      if (! errfile)
        error ("__delaunayn__: unable to redirect Qhull errors to /dev/null");

      unwind_action close_errfile ([=] () { std::fclose (errfile); });

      qhT context = { };
      qhT *qh = &context;

      int exitcode = qh_new_qhull (qh, dim, n, pt_array, ismalloc, &cmd[0],
                                   outfile, errfile);

      unwind_action free_memory ([qh] () { free_qhull_memory (qh); });

      if (exitcode)
        error ("__delaunayn__: qhull failed");

      // triangulate non-simplicial facets
      qh_triangulate (qh);

      facetT *facet;
      vertexT *vertex, **vertexp;
      octave_idx_type nf = 0;
      octave_idx_type i = 0;

      FORALLfacets
      {
        if (! facet->upperdelaunay)
          nf++;

        // Double check.  Non-simplicial facets will cause segfault below
        if (! facet->simplicial)
          error ("__delaunayn__: Qhull returned non-simplicial facets -- try delaunayn with different options");
      }

      Matrix simpl (nf, dim+1);

      FORALLfacets
      {
        if (! facet->upperdelaunay)
          {
            octave_idx_type j = 0;

            FOREACHvertex_ (facet->vertices)
            {
              simpl(i, j++) = 1 + qh_pointid(qh, vertex->point);
            }
            i++;
          }
      }

      retval(0) = simpl;
    }
  else if (n == dim + 1)
    {
      // FIXME: One should check if nx points span a simplex.
      //        I will look at this later.
      RowVector vec (n);
      for (octave_idx_type i = 0; i < n; i++)
        vec(i) = i + 1.0;

      retval(0) = vec;
    }

  return retval;

#else

  octave_unused_parameter (args);

  err_disabled_feature ("__delaunayn__", "Qhull");

#endif
}

/*
## No test needed for internal helper function.
%!assert (1)
*/

OCTAVE_END_NAMESPACE(octave)
