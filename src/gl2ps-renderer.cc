/*

Copyright (C) 2009 Shai Ayal

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if defined (HAVE_OPENGL)

#include <cstdio>

#include "lo-mappers.h"
#include "oct-locbuf.h"

#include "gl2ps-renderer.h"
#include "gl2ps.h"

void 
glps_renderer::draw (const graphics_object& go) 
{
  static bool in_draw = false;
  
  if (!in_draw) 
    {
      in_draw = true;

      FILE *fp = fdopen (fid, "wb");
      GLint buffsize = 0, state = GL2PS_OVERFLOW;
      GLint viewport[4];

      glGetIntegerv (GL_VIEWPORT, viewport);

      GLint gl2ps_term;
      if (term.find ("eps") != std::string::npos) gl2ps_term = GL2PS_EPS;
      else if (term.find ("pdf") != std::string::npos) gl2ps_term = GL2PS_PDF;
      else if (term.find ("svg") != std::string::npos) gl2ps_term = GL2PS_SVG;
      else if (term.find ("ps") != std::string::npos) gl2ps_term = GL2PS_PS;
      else if (term.find ("pgf") != std::string::npos) gl2ps_term = GL2PS_PGF;
      else if (term.find ("tex") != std::string::npos) gl2ps_term = GL2PS_TEX;
      else 
        {
          error ("gl2ps-renderer:: Unknown terminal");
          return;
        }

      GLint gl2ps_text = 0;
      if (term.find ("notxt") != std::string::npos) gl2ps_text = GL2PS_NO_TEXT;

      while (state == GL2PS_OVERFLOW)
        { 
          buffsize += 1024*1024;
          gl2psBeginPage ("glps_renderer figure", "Octave", viewport,
                          gl2ps_term, GL2PS_BSP_SORT,
                          (GL2PS_SILENT | GL2PS_SIMPLE_LINE_OFFSET
                           | GL2PS_NO_BLENDING | GL2PS_OCCLUSION_CULL
                           | GL2PS_BEST_ROOT | gl2ps_text), 
                          GL_RGBA, 0, NULL, 0, 0, 0,
                          buffsize, fp, "" );

          opengl_renderer::draw (go);
          state = gl2psEndPage ();
        }

      gnulib::fclose (fp);

      in_draw = 0;
    }
  else
    opengl_renderer::draw (go); 
}

int
glps_renderer::alignment_to_mode (int ha, int va) const
{
  int gl2psa=GL2PS_TEXT_BL;
  if (ha == 0)
    {
      if (va == 0 || va == 3)
        gl2psa=GL2PS_TEXT_BL;
      else if (va == 2)
        gl2psa=GL2PS_TEXT_TL;
      else if (va == 1)
        gl2psa=GL2PS_TEXT_CL;
    }
  else if (ha == 2)
    {
      if (va == 0 || va == 3)
        gl2psa=GL2PS_TEXT_BR;
      else if (va == 2)
        gl2psa=GL2PS_TEXT_TR;
      else if (va == 1)
        gl2psa=GL2PS_TEXT_CR;
    }
  else if (ha == 1)
    {
      if (va == 0 || va == 3)
        gl2psa=GL2PS_TEXT_B;
      else if (va == 2)
        gl2psa=GL2PS_TEXT_T;
      else if (va == 1)
        gl2psa=GL2PS_TEXT_C;
    }
  return gl2psa;
}

Matrix 
glps_renderer::render_text (const std::string& txt,
                            double x, double y, double z,
                            int ha, int va, double rotation)
{
  if (txt.empty ())
    return Matrix (1, 4, 0.0);

  glRasterPos3d (x, y, z);
  gl2psTextOpt (txt.c_str (), fontname.c_str (), fontsize,
                alignment_to_mode (ha, va), rotation);

  // FIXME? -- we have no way of getting a bounding box from gl2ps, so
  // we use freetype
  Matrix bbox;
  uint8NDArray pixels;
  text_to_pixels (txt, pixels, bbox, 0, 0, rotation);
  return bbox;
}

void
glps_renderer::set_font (const base_properties& props)
{
  opengl_renderer::set_font (props);

  fontsize = props.get ("fontsize").double_value ();

  caseless_str fn = props.get ("fontname").string_value ();
  fontname = "";
  if (fn == "times" || fn == "times-roman")
    fontname = "Times-Roman";
  else if (fn == "courier")
    fontname = "Courier";
  else if (fn == "symbol")
    fontname = "Symbol";
  else if (fn == "zapfdingbats")
    fontname = "ZapfDingbats";
  else 
    fontname = "Helvetica";

  // FIXME -- add support for bold and italic
}

template <typename T>
static void
draw_pixels (GLsizei w, GLsizei h, GLenum format, const T *data)
{
  OCTAVE_LOCAL_BUFFER (GLfloat, a, 3*w*h);

  for (int i = 0; i < 3*w*h; i++)
    a[i] = data[i];
    
  gl2psDrawPixels (w, h, 0, 0, format, GL_FLOAT, a);
}

void 
glps_renderer::draw_pixels (GLsizei w, GLsizei h, GLenum format,
                            GLenum type, const GLvoid *data)
{
  if (type == GL_UNSIGNED_SHORT) 
    ::draw_pixels (w, h, format, static_cast<const GLushort *> (data));
  else if (type == GL_UNSIGNED_BYTE) 
    ::draw_pixels (w, h, format, static_cast<const GLubyte *> (data));
  else
    gl2psDrawPixels (w, h, 0, 0, format, type, data);
}

void
glps_renderer::draw_text (const text::properties& props)
{
  if (props.get_string ().empty ())
    return;

  set_font (props);
  set_color (props.get_color_rgb ());

  const Matrix pos = get_transform ().scale (props.get_data_position ());
  int halign = 0, valign = 0;

  if (props.horizontalalignment_is ("center"))
    halign = 1;
  else if (props.horizontalalignment_is ("right"))
    halign = 2;
  
  if (props.verticalalignment_is ("top"))
    valign = 2;
  else if (props.verticalalignment_is ("baseline"))
    valign = 3;
  else if (props.verticalalignment_is ("middle"))
    valign = 1;

  // FIXME: handle margin and surrounding box

  glRasterPos3d (pos(0), pos(1), pos(2));
  gl2psTextOpt (props.get_string ().c_str (), fontname.c_str (), fontsize,
                alignment_to_mode (halign, valign), props.get_rotation ());

}

#endif
