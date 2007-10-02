/* Contour lines for function evaluated on a grid.

Copyright (C) 2004 Shai Ayal

Adapted to an oct file from the stand alone contourl by Victro Munoz
Copyright (C) 2004 Victor Munoz

Based on contour plot routine (plcont.c) in PLPlot package
http://plplot.org/

Copyright (C) 1995, 2000, 2001 Maurice LeBrun
Copyright (C) 2000, 2002 Joao Cardoso
Copyright (C) 2000, 2001, 2002, 2004  Alan W. Irwin
Copyright (C) 2004  Andrew Ross

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "quit.h"

#include "defun-dld.h"
#include "error.h"
#include "oct-obj.h"

static Matrix this_contour;
static Matrix contourc;
static int elem;

// this is the quanta in which we increase this_contour
#define CONTOUR_QUANT 50

// cl_add_point(x,y);
//
// Add a coordinate point (x,y) to this_contour 

static void
cl_add_point (double x, double y)
{
  if (elem % CONTOUR_QUANT == 0)
    this_contour = this_contour.append (Matrix (2, CONTOUR_QUANT, 0));

  this_contour (0, elem) = x;
  this_contour (1, elem) = y;
  elem++;
}

// cl_end_contour();
//
// Adds contents of current contour to contourc.
// this_contour.cols () - 1;

static void
cl_end_contour (void)
{
  if (elem > 2)
    {
      this_contour (1, 0) = elem - 1;
      contourc = contourc.append (this_contour.extract_n (0, 0, 2, elem));
    }
  this_contour = Matrix ();
  elem = 0;
}

// cl_start_contour(flev,x,y);
//
// Start a new contour, and adds contents of current one to contourc

static void
cl_start_contour (double flev, double x, double y)
{
  cl_end_contour ();
  this_contour.resize (2, 0);
  cl_add_point (flev, flev);
  cl_add_point (x, y);
}

static void
cl_drawcn (RowVector & X, RowVector & Y, Matrix & Z, double flev,
	   int krow, int kcol, double lastx, double lasty, int startedge,
	   Matrix & ipts)
{

  int kx = 0, lx = Z.cols () - 1, ky = 0, ly = Z.rows () - 1;

  double f[4];
  double px[4], py[4], locx[4], locy[4];
  int iedge[4];
  int num, first, inext, kcolnext, krownext;

  px[0] = X (krow + 1);
  px[1] = X (krow);
  px[2] = X (krow);
  px[3] = X (krow + 1);
  py[0] = Y (kcol);
  py[1] = Y (kcol);
  py[2] = Y (kcol + 1);
  py[3] = Y (kcol + 1);

  f[0] = Z (krow + 1, kcol) - flev;
  f[1] = Z (krow, kcol) - flev;
  f[2] = Z (krow, kcol + 1) - flev;
  f[3] = Z (krow + 1, kcol + 1) - flev;

  for (int i = 0, j = 1; i < 4; i++, j = (j + 1) % 4)
    {
      iedge[i] = (f[i] * f[j] > 0.0) ? -1 : ((f[i] * f[j] < 0.0) ? 1 : 0);
    }

  // Mark this square as done
  ipts(krow,kcol) = 1;

  // Check if no contour has been crossed i.e. iedge[i] = -1
  if (iedge[0] == -1 && iedge[1] == -1 && iedge[2] == -1 && iedge[3] == -1)
    return;

  // Check if this is a completely flat square - in which case ignore it
  if (f[0] == 0.0 && f[1] == 0.0 && f[2] == 0.0 && f[3] == 0.0)
    return;

  // Calculate intersection points
  num = 0;
  if (startedge < 0)
    {
      first = 1;
    }
  else
    {
      locx[num] = lastx;
      locy[num] = lasty;
      num++;
      first = 0;
    }

  for (int k = 0, i = (startedge < 0 ? 0 : startedge); k < 4;
       k++, i = (i + 1) % 4)
    {
      if (i == startedge)
	continue;

      // If the contour is an edge check it hasn't already been done
      if (f[i] == 0.0 && f[(i + 1) % 4] == 0.0)
	{
	  kcolnext = kcol;
	  krownext = krow;
	  if (i == 0)
	    kcolnext--;
	  if (i == 1)
	    krownext--;
	  if (i == 2)
	    kcolnext++;
	  if (i == 3)
	    krownext++;
	  if (kcolnext < kx || kcolnext >= lx || krownext < ky
	      || krownext >= ly || ipts(krownext,kcolnext) == 1)
	    continue;
	}
      if (iedge[i] == 1 || f[i] == 0.0)
	{
	  int j = (i + 1) % 4;
	  if (f[i] != 0.0)
	    {
	      locx[num] =
		(px[i] * fabs (f[j]) + px[j] * fabs (f[i])) / fabs (f[j] -
								    f[i]);
	      locy[num] =
		(py[i] * fabs (f[j]) + py[j] * fabs (f[i])) / fabs (f[j] -
								    f[i]);
	    }
	  else
	    {
	      locx[num] = px[i];
	      locy[num] = py[i];
	    }
	  // If this is the start of the contour then move to the point
	  if (first == 1)
	    {
	      cl_start_contour (flev, locx[num], locy[num]);
	      first = 0;
	    }
	  else
	    {
	      // Link to the next point on the contour
	      cl_add_point (locx[num], locy[num]);
	      // Need to follow contour into next grid box
	      // Easy case where contour does not pass through corner
	      if (f[i] != 0.0)
		{
		  kcolnext = kcol;
		  krownext = krow;
		  inext = (i + 2) % 4;
		  if (i == 0)
		    kcolnext--;
		  if (i == 1)
		    krownext--;
		  if (i == 2)
		    kcolnext++;
		  if (i == 3)
		    krownext++;
		  if (kcolnext >= kx && kcolnext < lx
		      && krownext >= ky && krownext < ly
		      && ipts(krownext,kcolnext) == 0)
		    {
		      cl_drawcn (X, Y, Z, flev, krownext, kcolnext,
				 locx[num], locy[num], inext, ipts);
		    }
		}
	      // Hard case where contour passes through corner.  This
	      // is still not perfect - it may lose the contour  which
	      // won't upset the contour itself (we can find it again
	      // later) but might upset the labelling (which is only
	      // relevant for the PLPlot implementation, since we
	      // don't worry about labels---for now!)
	      else
		{
		  kcolnext = kcol;
		  krownext = krow;
		  inext = (i + 2) % 4;
		  if (i == 0)
		    {
		      kcolnext--;
		      krownext++;
		    }
		  if (i == 1)
		    {
		      krownext--;
		      kcolnext--;
		    }
		  if (i == 2)
		    {
		      kcolnext++;
		      krownext--;
		    }
		  if (i == 3)
		    {
		      krownext++;
		      kcolnext++;
		    }
		  if (kcolnext >= kx && kcolnext < lx
		      && krownext >= ky && krownext < ly
		      && ipts(krownext,kcolnext) == 0)
		    {
		      cl_drawcn (X, Y, Z, flev, krownext, kcolnext,
				 locx[num], locy[num], inext, ipts);
		    }

		}
	      if (first == 1)
		{
		  // Move back to first point
		  cl_start_contour (flev, locx[num], locy[num]);
		  first = 0;
		}
	      else
		{
		  first = 1;
		}
	      num++;
	    }
	}
    }
}

static void
cl_cntr (RowVector & X, RowVector & Y, Matrix & Z, double flev)
{
  Matrix ipts (Z.rows (), Z.cols (), 0);

  for (int krow = 0; krow < Z.rows () - 1; krow++)
    {
      for (int kcol = 0; kcol < Z.cols () - 1; kcol++)
	{
	  if (ipts(krow,kcol) == 0)
	    {
	      cl_drawcn (X, Y, Z, flev, krow, kcol, 0.0, 0.0, -2, ipts);
	    }
	}
    }
}

DEFUN_DLD (__contourc__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __contourc__ (@var{x}, @var{y}, @var{z}, @var{levels})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 4)
    {
      RowVector X = args (0).row_vector_value ();
      RowVector Y = args (1).row_vector_value ();
      Matrix Z = args (2).matrix_value ().transpose ();
      RowVector L = args (3).row_vector_value ();

      if (! error_state)
	{
	  contourc.resize (2, 0);

	  for (int i = 0; i < L.length (); i++)
	    cl_cntr (X, Y, Z, L (i));

	  cl_end_contour ();

	  retval = contourc;
	}
      else
	error ("__contourc__: invalid argument values");
    }
  else
    print_usage ();

  return retval;
}
