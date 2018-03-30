/*

Copyright (C) 2016-2018 Andreas Weber <andy.weber.aw@gmail.com>

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

This code is based on Brian Pauls' src/osdemos/osdemo.c
from git://anongit.freedesktop.org/mesa/demos

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#if defined (HAVE_OSMESA_H)
#  include <osmesa.h>
#elif defined (HAVE_GL_OSMESA_H)
#  include <GL/osmesa.h>
#endif

#include <string>

#include "Array.h"
#include "dMatrix.h"
#include "oct-locbuf.h"
#include "uint8NDArray.h"
#include "unwind-prot.h"

#include "defun-dld.h"
#include "error.h"
#include "errwarn.h"
#include "gl-render.h"
#include "gl2ps-print.h"
#include "graphics.h"
#include "oct-opengl.h"
#include "ov.h"
#include "ovl.h"

#if defined (HAVE_OSMESA)

static void
reset_visibility (figure::properties *fp)
{
  fp->set_visible ("on");
}

#endif

DEFUN_DLD(__osmesa_print__, args, ,
          doc: /* -*- texinfo -*-
@deftypefn  {} {} __osmesa_print__ (@var{h}, @var{file}, @var{term})
@deftypefnx {} {@var{img} =} __osmesa_print__ (@var{h})
Print figure @var{h} using OSMesa and gl2ps for vector formats.

This is a private internal function.

The first method calls gl2ps with the appropriate @var{term} and writes
the output of gl2ps to @var{file}.  If the first character of @var{file}
is @code{|}, then a process is started and the output of gl2ps is piped
to it.

Valid options for @var{term}, which can be concatenated in one string, are:

@table @asis
@item @qcode{eps}, @qcode{pdf}, @qcode{ps}, @qcode{svg}, @qcode{pgf}, @qcode{tex}
Select output format.

@item @code{is2D}
Use GL2PS_SIMPLE_SORT instead of GL2PS_BSP_SORT as Z-depth sorting
algorithm.

@item @code{notext}
Don't render text.
@end table

The second method doesn't use gl2ps and returns a RGB image in @var{img}
instead.

@end deftypefn */)
{
#if defined (HAVE_OSMESA)

  int nargin = args.length ();

  if (nargin != 1 && nargin != 3)
    print_usage ();

  if (nargin == 3)
    {
      if (! (args(1).is_string () && args(2).is_string ()))
        error ("__osmesa_print__: FILE and TERM must be strings");
    }

  octave_value_list retval;

  int h = args(0).double_value ();
  graphics_object fobj = gh_manager::get_object (h);
  if (! (fobj && fobj.isa ("figure")))
    error ("__osmesa_print__: H must be a valid figure handle");

  figure::properties& fp =
    dynamic_cast<figure::properties&> (fobj.get_properties ());

  bool internal = true;
  Matrix bb = fp.get_boundingbox (internal);

  GLsizei Width = static_cast<GLsizei> (bb(2));
  GLsizei Height = static_cast<GLsizei> (bb(3));

  // Create an RGBA-mode context, specify Z=16, stencil=0, accum=0 sizes
  OSMesaContext ctx = OSMesaCreateContextExt (OSMESA_RGBA, 16, 0, 0, nullptr);
  if (! ctx)
    error ("__osmesa_print__: OSMesaCreateContext failed!\n");

  // Allocate the image buffer
  OCTAVE_LOCAL_BUFFER (GLubyte, buffer, 4 * Width * Height);

  // Bind the buffer to the context and make it current
  if (! OSMesaMakeCurrent (ctx, buffer, GL_UNSIGNED_BYTE, Width, Height))
    error ("__osmesa_print__: OSMesaMakeCurrent failed!\n");

  // Test for a bug in OSMesa with version < 9.0
  //
  // Unfortunately the macros OSMESA_MAJOR_VERSION and OSMESA_MINOR_VERSION
  // weren't updated between many releases and can't be used for detection.
  // (Version 8.0 until 9.1.4 all return MAJOR 6, MINOR 5)
  GLint z, s;
  glGetIntegerv (GL_DEPTH_BITS, &z);
  glGetIntegerv (GL_STENCIL_BITS, &s);
  if (z != 16 || s != 0)
    error ("__osmesa_print__: Depth and stencil doesn't match,"
           " are you sure you are using OSMesa >= 9.0?");

  octave::unwind_protect outer_frame;

  bool v = fp.is_visible ();

  if (v)
    {
      outer_frame.add_fcn (reset_visibility, &fp);

      fp.set_visible ("off");
    }

  if (nargin == 3)
    {
      std::string file = args(1).string_value ();
      std::string term = args(2).string_value ();

      octave::gl2ps_print (fobj, file, term);
    }
  else
    {
      // return RGB image
      octave::opengl_renderer rend;

      // Draw and finish () or there may primitives missing in the
      // output.
      rend.draw (fobj);
      rend.finish ();

      dim_vector dv (4, Width, Height);

      // FIXME: We expect that GLubyte is 1 Byte long.
      // Adapt code if this isn't always true
      assert (sizeof (GLubyte) == 1);
      uint8NDArray img (dv);
      unsigned char *p = reinterpret_cast<unsigned char *>(img.fortran_vec ());
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
      retval(0) = octave_value (img.permute (perm).index(idx));
    }

  OSMesaDestroyContext (ctx);

  return retval;

#else

  octave_unused_parameter (args);

  err_disabled_feature ("__osmesa_print__", "offscreen rendering with OSMesa");

#endif
}

/*
## FIXME: osmesa does not work correctly on Windows platforms.
##        This is not critical, since this facility will mostly be used in
##        the future for generating the images in Octave's own documentation.
##        For the moment, disable these tests on PC's and Macs.
%!testif HAVE_OPENGL, HAVE_OSMESA, HAVE_GL2PS_H
%! if (isunix ())
%!   hf = figure ("visible", "off");
%!   fn = tempname ();
%!   unwind_protect
%!     sombrero ();
%!     __osmesa_print__ (hf, fn, "svg");
%!     assert (stat (fn).size > 2e6);
%!     img = __osmesa_print__ (hf);
%!     assert (size (img), [get(hf, "position")([4, 3]), 3]);
%!     ## Use pixel sum per RGB channel as fingerprint
%!     img_fp = squeeze (sum (sum (img), 2));
%!     assert (img_fp, [52942515; 54167797; 56158178], -0.05);
%!   unwind_protect_cleanup
%!     close (hf);
%!     unlink (fn);
%!   end_unwind_protect
%! endif

%!testif HAVE_OPENGL, HAVE_OSMESA, HAVE_GL2PS_H
%! if (isunix ())
%!   hf = figure ("visible", "off");
%!   fn = tempname ();
%!   unwind_protect
%!     plot (sin (0:0.1:2*pi));
%!     __osmesa_print__ (hf, fn, "svgis2d");
%!     assert (stat (fn).size, 9022, -0.20);
%!     img = __osmesa_print__ (hf);
%!     assert (size (img), [get(hf, "position")([4, 3]), 3]);
%!     ## Use pixel sum per RGB channel as fingerprint
%!     img_fp = squeeze (sum (sum (img), 2));
%!     assert (img_fp, [59281711; 59281711; 59482179], -0.05);
%!   unwind_protect_cleanup
%!     close (hf);
%!     unlink (fn);
%!   end_unwind_protect
%! endif
*/
