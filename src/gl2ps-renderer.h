/*

Copyright (C) 2009-2011 Shai Ayal

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

#if !defined (gl2ps_renderer_h)
#define gl2ps_renderer_h 1

#include "gl-render.h"
#include "gl2ps.h"

class
OCTINTERP_API
glps_renderer : public opengl_renderer
{
public:
  glps_renderer (const int _fid, const std::string& _term)
    : opengl_renderer () , fid (_fid), term (_term),
    fontsize (), fontname () { }

  ~glps_renderer (void) { }

  virtual void draw (const graphics_object& go);

protected:

  virtual Matrix render_text (const std::string& txt,
                              double x, double y, double z,
                              int halign, int valign, double rotation = 0.0);


  virtual void set_font (const base_properties& props);

  virtual void draw_text (const text::properties& props);
  virtual void draw_pixels (GLsizei w, GLsizei h, GLenum format,
                            GLenum type, const GLvoid *data);

  virtual void set_linestyle (const std::string& s, bool use_stipple)
  {
    opengl_renderer::set_linestyle (s, use_stipple);
    if (use_stipple)
      gl2psEnable (GL2PS_LINE_STIPPLE);
    else
      gl2psDisable (GL2PS_LINE_STIPPLE);
  }

  virtual void set_polygon_offset (bool on, double offset = 0.0)
  {
    opengl_renderer::set_polygon_offset (on, offset);
    if (on)
      gl2psEnable (GL2PS_POLYGON_OFFSET_FILL);
    else
      gl2psDisable (GL2PS_POLYGON_OFFSET_FILL);
  }

  virtual void set_linewidth (float w)
  {
    gl2psLineWidth (w);
  }

private:
  int alignment_to_mode (int ha, int va) const;
  int fid;
  caseless_str term;
  double fontsize;
  std::string fontname;
};

#endif
