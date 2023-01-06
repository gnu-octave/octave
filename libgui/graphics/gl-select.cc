////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "gl-select.h"

OCTAVE_BEGIN_NAMESPACE(octave)

void
opengl_selector::apply_pick_matrix (void)
{
  GLdouble p_matrix[16];

  m_glfcns.glGetDoublev (GL_PROJECTION_MATRIX, p_matrix);
  m_glfcns.glMatrixMode (GL_PROJECTION);
  m_glfcns.glLoadIdentity ();

  // The following block is equivalent to gluPickMatrix, but we avoid
  // using glu functions so that we can call OpenGL functions through
  // the QOpenGLFunctions class so that the OpenGL implementation may
  // be selected dynamically.

  Matrix viewport = get_viewport_scaled ();

  if (size > 0)
    {
      m_glfcns.glTranslatef ((viewport(2) - 2 * (xp - viewport(0))) / size,
                             (viewport(3) - 2 * (yp - viewport(1))) / size, 0);

      m_glfcns.glScalef (viewport(2) / size, viewport(3) / size, 1.0);
    }

  m_glfcns.glMultMatrixd (p_matrix);
  m_glfcns.glMatrixMode (GL_MODELVIEW);
}

void
opengl_selector::setup_opengl_transformation (const axes::properties& props)
{
  opengl_renderer::setup_opengl_transformation (props);
  apply_pick_matrix ();
}

void
opengl_selector::init_marker (const std::string& m, double sz, float width)
{
  opengl_renderer::init_marker (m, sz, width);
  apply_pick_matrix ();
}

# define BUFFER_SIZE 128

graphics_object
opengl_selector::select (const graphics_object& ax, int x, int y, int flags)
{
  m_glfcns.glEnable (GL_DEPTH_TEST);
  m_glfcns.glDepthFunc (GL_LEQUAL);

  xp = x;
  yp = y;

  GLuint select_buffer[BUFFER_SIZE];

  m_glfcns.glSelectBuffer (BUFFER_SIZE, select_buffer);
  m_glfcns.glRenderMode (GL_SELECT);
  m_glfcns.glInitNames ();

  object_map.clear ();

  draw (ax);

  int hits = m_glfcns.glRenderMode (GL_RENDER);
  graphics_object obj;

  if (hits > 0)
    {
      GLuint current_minZ = 0xffffffff;
      GLuint current_name = 0xffffffff;

      for (int i = 0, j = 0; i < hits && j < BUFFER_SIZE-3; i++)
        {
          GLuint n = select_buffer[j++],
            minZ = select_buffer[j++];

          j++; // skip maxZ
          if (((flags & select_last) == 0 && (minZ <= current_minZ))
              || ((flags & select_last) != 0 && (minZ >= current_minZ)))
            {
              bool candidate = true;
              GLuint name =
                select_buffer[std::min (j + n, GLuint (BUFFER_SIZE)) - 1];

              if ((flags & select_ignore_hittest) == 0)
                {
                  graphics_object go = object_map[name];

                  if (! go.get_properties ().is_hittest ())
                    candidate = false;
                }

              if (candidate)
                {
                  current_minZ = minZ;
                  current_name = name;
                }

              j += n;
            }
          else
            j += n;
        }

      if (current_name != 0xffffffff)
        obj = object_map[current_name];
    }
  else if (hits < 0)
    warning ("opengl_selector::select: selection buffer overflow");

  object_map.clear ();

  return obj;
}

void
opengl_selector::draw (const graphics_object& go, bool toplevel)
{
  GLuint name = object_map.size ();

  object_map[name] = go;
  m_glfcns.glPushName (name);
  set_selecting (true);
  opengl_renderer::draw (go, toplevel);
  set_selecting (false);
  m_glfcns.glPopName ();
}

void
opengl_selector::fake_text (double x, double y, double z, const Matrix& bbox,
                            bool use_scale)
{
  ColumnVector xpos, xp1, xp2;

  xpos = get_transform ().transform (x, y, z, use_scale);

  xp1 = xp2 = xpos;
  xp1(0) += bbox(0);
  xp1(1) -= bbox(1);
  xp2(0) += (bbox(0) + bbox(2));
  xp2(1) -= (bbox(1) + bbox(3));

  ColumnVector p1, p2, p3, p4;

  p1 = get_transform ().untransform (xp1(0), xp1(1), xp1(2), false);
  p2 = get_transform ().untransform (xp2(0), xp1(1), xp1(2), false);
  p3 = get_transform ().untransform (xp2(0), xp2(1), xp1(2), false);
  p4 = get_transform ().untransform (xp1(0), xp2(1), xp1(2), false);

  m_glfcns.glBegin (GL_QUADS);
  m_glfcns.glVertex3dv (p1.data ());
  m_glfcns.glVertex3dv (p2.data ());
  m_glfcns.glVertex3dv (p3.data ());
  m_glfcns.glVertex3dv (p4.data ());
  m_glfcns.glEnd ();
}

void
opengl_selector::draw_text (const text::properties& props)
{
  if (props.get_string ().isempty ())
    return;

  Matrix pos = props.get_data_position ();
  const Matrix bbox = props.get_extent_matrix ();

  fake_text (pos(0), pos(1), pos.numel () > 2 ? pos(2) : 0.0, bbox);
}

Matrix
opengl_selector::render_text (const std::string& txt,
                              double x, double y, double z,
                              int halign, int valign, double rotation)
{
  uint8NDArray pixels;
  Matrix bbox (1, 4, 0.0);

  // FIXME: probably more efficient to only compute bbox instead
  //        of doing full text rendering...
  text_to_pixels (txt, pixels, bbox, halign, valign, rotation);
  fake_text (x, y, z, bbox, false);

  return bbox;
}

void
opengl_selector::draw_image (const image::properties& props)
{
  Matrix xd = props.get_xdata ().matrix_value ();
  octave_idx_type nc = props.get_cdata ().columns ();
  double x_pix_size = (nc == 1 ? 1 : (xd(1) - xd(0)) / (nc - 1));

  Matrix yd = props.get_ydata ().matrix_value ();
  octave_idx_type nr = props.get_cdata ().rows ();
  double y_pix_size = (nr == 1 ? 1 : (yd(1) - yd(0)) / (nr - 1));

  ColumnVector p1(3, 0.0), p2(3, 0.0), p3(3, 0.0), p4(3, 0.0);
  p1(0) = xd(0) - x_pix_size/2;
  p1(1) = yd(0) - y_pix_size/2;

  p2(0) = xd(1) + x_pix_size/2;
  p2(1) = yd(0) - y_pix_size/2;

  p3(0) = xd(1) + x_pix_size/2;
  p3(1) = yd(1) + y_pix_size/2;

  p4(0) = xd(0) - x_pix_size/2;
  p4(1) = yd(1) + y_pix_size/2;

  m_glfcns.glBegin (GL_QUADS);
  m_glfcns.glVertex3dv (p1.data ());
  m_glfcns.glVertex3dv (p2.data ());
  m_glfcns.glVertex3dv (p3.data ());
  m_glfcns.glVertex3dv (p4.data ());
  m_glfcns.glEnd ();
}

OCTAVE_END_NAMESPACE(octave)
