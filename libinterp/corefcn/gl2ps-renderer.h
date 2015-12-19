/*

Copyright (C) 2009-2015 Shai Ayal

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

#if ! defined (octave_gl2ps_renderer_h)
#define octave_gl2ps_renderer_h 1

#include "graphics.h"

#ifdef HAVE_GL2PS_H

#include "gl-render.h"
#include <gl2ps.h>

class
OCTINTERP_API
glps_renderer : public opengl_renderer
{
public:
  glps_renderer (FILE *_fp, const std::string& _term)
    : opengl_renderer () , fp (_fp), term (_term), fontsize (),
    fontname () { }

  ~glps_renderer (void) { }

  void draw (const graphics_object& go, const std::string& print_cmd);

protected:

  Matrix render_text (const std::string& txt,
                      double x, double y, double z,
                      int halign, int valign, double rotation = 0.0);

  void set_font (const base_properties& props);

  void draw_axes (const axes::properties& props)
  {
    // Initialize a sorting tree (viewport) in gl2ps for each axes
    GLint vp[4];
    glGetIntegerv (GL_VIEWPORT, vp);
    gl2psBeginViewport (vp);

    // Draw and  glFinish () or there may primitives missing in the
    // gl2ps output.
    opengl_renderer::draw_axes (props);
    glFinish ();

    // Finalize viewport
    GLint state = gl2psEndViewport ();
    if (state == GL2PS_NO_FEEDBACK)
      warning ("gl2ps-renderer::draw: empty feedback buffer and/or nothing else to print");
  }

  void draw_text (const text::properties& props);
  void draw_pixels (GLsizei w, GLsizei h, GLenum format,
                    GLenum type, const GLvoid *data);

  void set_linestyle (const std::string& s, bool use_stipple = false)
  {
    opengl_renderer::set_linestyle (s, use_stipple);

    if (s == "-" && ! use_stipple)
      gl2psDisable (GL2PS_LINE_STIPPLE);
    else
      gl2psEnable (GL2PS_LINE_STIPPLE);
  }

  void set_polygon_offset (bool on, float offset = 0.0f)
  {
    if (on)
      {
        opengl_renderer::set_polygon_offset (on, offset);
        gl2psEnable (GL2PS_POLYGON_OFFSET_FILL);
      }
    else
      {
        gl2psDisable (GL2PS_POLYGON_OFFSET_FILL);
        opengl_renderer::set_polygon_offset (on, offset);
      }
  }

  void set_linewidth (float w)
  {
    gl2psLineWidth (w);
  }

private:
  // Use xform to compute the coordinates of the ft_string list
  // that have been parsed by freetype
  void fix_strlist_position (double x, double y, double z,
                             Matrix box, double rotation,
                             std::list<ft_render::ft_string>& lst);

private:
  int alignment_to_mode (int ha, int va) const;
  FILE *fp;
  caseless_str term;
  double fontsize;
  std::string fontname;
};

#endif

extern OCTINTERP_API void
gl2ps_print (const graphics_object& fig, const std::string& cmd,
             const std::string& term);

#endif
