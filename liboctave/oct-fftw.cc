/*

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_FFTW

#include "oct-fftw.h"
#include "lo-error.h"


// Helper class to create and cache fftw plans for both 1d and 2d. This
// implementation uses FFTW_ESTIMATE to create the plans, which in theory
// is suboptimal, but provides quite reasonable performance. Future
// enhancement will be to add a dynamically loadable interface ("fftw")
// to manipulate fftw wisdom so that users may choose the appropriate
// planner.

class
octave_fftw_planner
{
public:
  octave_fftw_planner ();

  fftw_plan create_plan (fftw_direction, size_t);
  fftwnd_plan create_plan2d (fftw_direction, size_t, size_t);

private:
  int plan_flags;

  fftw_plan plan[2];
  fftwnd_plan plan2d[2];

  size_t n[2];
  size_t n2d[2][2];
};

octave_fftw_planner::octave_fftw_planner ()
{
  plan_flags = FFTW_ESTIMATE;

  plan[0] = plan[1] = 0;
  plan2d[0] = plan2d[1] = 0;
  
  n[0] = n[1] = 0;
  n2d[0][0] = n2d[0][1] = n2d[1][0] = n2d[1][1] = 0;
}

fftw_plan
octave_fftw_planner::create_plan (fftw_direction dir, size_t npts)
{
  size_t which = (dir == FFTW_FORWARD) ? 0 : 1;
  fftw_plan *cur_plan_p = &plan[which];
  bool create_new_plan = false;

  if (plan[which] == 0 || n[which] != npts)
    {
      create_new_plan = true;
      n[which] = npts;
    }

  if (create_new_plan)
    {
      if (*cur_plan_p)
	fftw_destroy_plan (*cur_plan_p);

      *cur_plan_p = fftw_create_plan (npts, dir, plan_flags);

      if (*cur_plan_p == 0)
	(*current_liboctave_error_handler) ("Error creating fftw plan");
    }

  return *cur_plan_p;
}
 
fftwnd_plan
octave_fftw_planner::create_plan2d (fftw_direction dir, 
                                   size_t nrows, size_t ncols)
{
  size_t which = (dir == FFTW_FORWARD) ? 0 : 1;
  fftwnd_plan *cur_plan_p = &plan2d[which];
  bool create_new_plan = false;

  if (plan2d[which] == 0 || n2d[which][0] != nrows || n2d[which][1] != ncols)
    {
      create_new_plan = true;

      n2d[which][0] = nrows;
      n2d[which][1] = ncols;
    }

  if (create_new_plan)
    {
      if (*cur_plan_p)
	fftwnd_destroy_plan (*cur_plan_p);

      *cur_plan_p = fftw2d_create_plan (nrows, ncols, dir, 
					plan_flags | FFTW_IN_PLACE);

      if (*cur_plan_p == 0)
	(*current_liboctave_error_handler) ("Error creating 2d fftw plan");
    }

  return *cur_plan_p;
}

static octave_fftw_planner fftw_planner;

int
octave_fftw::fft (const Complex *in, Complex *out, size_t npts)
{
  fftw_one (fftw_planner.create_plan (FFTW_FORWARD, npts),
            reinterpret_cast<fftw_complex *> (const_cast<Complex *> (in)),
            reinterpret_cast<fftw_complex *> (out));

  return 0;
}

int
octave_fftw::ifft (const Complex *in, Complex *out, size_t npts)
{
  fftw_one (fftw_planner.create_plan (FFTW_BACKWARD, npts),
            reinterpret_cast<fftw_complex *> (const_cast<Complex *> (in)),
            reinterpret_cast<fftw_complex *> (out));

  const Complex scale = npts;
  for (size_t i = 0; i < npts; i++)
    out[i] /= scale;

  return 0;
}

int
octave_fftw::fft2d (Complex *inout, size_t nr, size_t nc)
{
  fftwnd_one (fftw_planner.create_plan2d (FFTW_FORWARD, nr, nc),
              reinterpret_cast<fftw_complex *> (inout),
              NULL);

  return 0;
}

int
octave_fftw::ifft2d (Complex *inout, size_t nr, size_t nc)
{
  fftwnd_one (fftw_planner.create_plan2d (FFTW_BACKWARD, nr, nc),
              reinterpret_cast<fftw_complex *> (inout),
              NULL);

  const size_t npts = nr * nc;
  const Complex scale = npts;
  for (size_t i = 0; i < npts; i++)
    inout[i] /= scale;

  return 0;
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

