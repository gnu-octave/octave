/*

Copyright (C) 2015 Andreas Weber <andy.weber.aw@gmail.com>

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

This code is based on Brian Pauls' src/osdemos/osdemo.c
from git://anongit.freedesktop.org/mesa/demos

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun-dld.h"
#include "gl-render.h"
#include "gl2ps-renderer.h"
#include "graphics.h"

#include "gripes.h"

#ifdef HAVE_OSMESA
#include "GL/osmesa.h"
#endif

DEFUN_DLD(__osmesa_print__, args, ,
          "-*- texinfo -*-\n\
@deftypefn {Loadable Function}  __osmesa_print__ (@var{h}, @var{file}, @var{term})\n\
@deftypefnx {Loadable Function} {@var{img}  =} __osmesa_print__ (@var{h})\n\
Print figure @var{h} using OSMesa and gl2ps for vector formats.\n\
\n\
This is a private internal function.\n\
The first method calls gl2ps with the appropriate @var{term} and writes\n\
the output of gl2ps to @var{file}. If the first character of @var{file}\n\
is @qcode{|}, then a process is started and the output of gl2ps is piped to it.\n\
\n\
Valid options for @var{term}, which can be concatenated in one string, are:\n\
@table @asis\n\
@item @qcode{eps}, @qcode{pdf}, @qcode{ps}, @qcode{svg}, @qcode{pgf}, @qcode{tex}\n\
Select output format.\n\
@item @qcode{is2D}\n\
Use GL2PS_SIMPLE_SORT instead of GL2PS_BSP_SORT as Z-depth sorting algorithm.\n\
@item @qcode{notext}\n\
Don't render text.\n\
@end table\n\
\n\
The second method doesn't use gl2ps and returns a RGB image in @var{img} instead.\n\
\n\
@end deftypefn")
{
  octave_value_list retval;

#ifndef HAVE_OSMESA
  gripe_disabled_feature ("__osmesa_print__", "Offscreen rendering");
#else

  int nargin = args.length ();

  if (! (nargin == 1 || nargin == 3))
    {
      print_usage ();
      return retval;
    }

  if ((nargin == 3))
    {
      if(! (args(1).is_string () && args(2).is_string ()))
        {
          error ("__osmesa_print__: FILE and TERM has to be strings");
          return retval;
        }

      #ifndef HAVE_GL2PS_H
        error ("__osmesa_print__: Octave has been compiled without gl2ps");
        return retval;
      #endif
    }

  int h = args(0).double_value ();
  graphics_object fobj = gh_manager::get_object (h);
  if (! (fobj &&  fobj.isa ("figure")))
    {
      error ("__osmesa_print__: H has to be a valid figure handle");
      return retval;
    }

  figure::properties& fp =
    dynamic_cast<figure::properties&> (fobj.get_properties ());

  bool internal = true;
  Matrix bb = fp.get_boundingbox (internal);

  int Width = bb(2);
  int Height = bb(3);

  OSMesaContext ctx;
  void *buffer;

  // Create an RGBA-mode context, specify Z=16, stencil=0, accum=0 sizes
  ctx = OSMesaCreateContextExt (OSMESA_RGBA, 16, 0, 0, NULL);
  if (! ctx)
    {
      error ("__osmesa_print__: OSMesaCreateContext failed!\n");
      return retval;
    }

  // Allocate the image buffer
  buffer = malloc (Width * Height * 4 * sizeof (GLubyte));
  if (! buffer)
    {
      error ("__osmesa_print__: Alloc image buffer failed!\n");
      return retval;
    }

  // Bind the buffer to the context and make it current
  if (! OSMesaMakeCurrent (ctx, buffer, GL_UNSIGNED_BYTE, Width, Height))
    {
      error ("__osmesa_print__: OSMesaMakeCurrent failed!\n");
      return retval;
    }

  // Test for a bug in OSMesa with version < 9.0
  // Unfortunately the macros OSMESA_MAJOR_VERSION and OSMESA_MINOR_VERSION weren't
  // updated between many releases and can't be used for detection therefore.
  // (Version 8.0 until 9.1.4 all return MAJOR 6, MINOR 5)
  int z, s;
  glGetIntegerv (GL_DEPTH_BITS, &z);
  glGetIntegerv (GL_STENCIL_BITS, &s);
  if ((z != 16) || (s != 0))
    error ("__osmesa_print__: Depth and stencil doesn't match,"
           " are you sure you are using OSMesa >= 9.0?");

  // check if the figure is visible
  bool v = fp.is_visible ();
  if (v)
    fp.set_visible ("off");

  if (nargin == 3)
    {
      // use gl2ps
      std::string file = args(1).string_value ();
      std::string term = args(2).string_value ();

      if (! error_state)
        {
          size_t pos = file.find_first_not_of ("|");
          if (pos > 0)
            {
              // create process and pipe gl2ps output to it
              std::string cmd = file.substr (pos);
              gl2ps_print (fobj, cmd, term);
            }
          else
            {
              // write gl2ps output directly to file
              FILE *filep;
              filep = fopen (file.c_str (), "w");
              if (filep)
                {
                  glps_renderer rend (filep, term);
                  rend.draw (fobj, "");

                  // Make sure buffered commands are finished!!!
                  glFinish ();
                  fclose (filep);
                }
              else
                error ("__osmesa_print__: Couldn't create file \"%s\"", file.c_str ());
            }
        }
    }
  else
    {
      // return RGB image
      opengl_renderer rend;
      rend.draw (fobj);

      // Make sure buffered commands are finished!!!
      glFinish ();

      dim_vector dv (4, Width, Height);

      // FIXME: We expect that GLubyte is 1 Byte long.
      // Adapt code if this isn't always true
      assert (sizeof (GLubyte) == 1);
      uint8NDArray img (dv);
      unsigned char *p = reinterpret_cast<unsigned char*>(img.fortran_vec ());
      memcpy (p, buffer, (4 * Width * Height));

      Array<octave_idx_type> perm (dim_vector (3, 1));
      perm(0) = 2;
      perm(1) = 1;
      perm(2) = 0;

      Array<idx_vector> idx (dim_vector (3, 1));

      // Flip Y
      idx(0) = idx_vector::make_range (Height - 1, -1, Height);
      idx(1) = idx_vector::colon;

      // Remove alpha channel
      idx(2) = idx_vector (0, 3);
      retval = octave_value (img.permute (perm). index(idx));
    }

  // restore visibility if necessary
  if (v)
    fp.set_visible ("on");

  free (buffer);
  OSMesaDestroyContext (ctx);

#endif
  return retval;
}

/*
%!testif HAVE_OSMESA
%! h = figure ("visible", "off");
%! fn = tempname ();
%! sombrero ();
%! __osmesa_print__ (h, fn, "svg");
%! assert (stat (fn).size, 2692270, -0.1);
%! unlink (fn);
%! img = __osmesa_print__ (h);
%! assert (size (img), [get(h, "position")([4, 3]), 3])
%! ## Use pixel sum per RGB channel as fingerprint
%! img_fp = squeeze (sum (sum (img), 2));
%! assert (img_fp, [52942515; 54167797; 56158178], -0.05);

%!testif HAVE_OSMESA
%! h = figure ("visible", "off");
%! fn = tempname ();
%! plot (sin (0:0.1:2*pi));
%! __osmesa_print__ (h, fn, "svgis2d");
%! assert (stat (fn).size, 7438, -0.05);
%! unlink (fn);
%! img = __osmesa_print__ (h);
%! assert (size (img), [get(h, "position")([4, 3]), 3])
%! ## Use pixel sum per RGB channel as fingerprint
%! img_fp = squeeze (sum (sum (img), 2));
%! assert (img_fp, [59281711; 59281711; 59482179], -0.05);
*/
