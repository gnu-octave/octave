/*

Copyright (C) 2008, 2009 Michael Goffioul

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

#if !defined (gl_render_h)
#define gl_render_h 1

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif

#ifdef HAVE_GL_GL_H
#include <GL/gl.h>
#elif defined HAVE_OPENGL_GL_H || defined HAVE_FRAMEWORK_OPENGL
#include <OpenGL/gl.h>
#endif

#ifdef HAVE_GL_GLU_H
#include <GL/glu.h>
#elif defined HAVE_OPENGL_GLU_H || defined HAVE_FRAMEWORK_OPENGL
#include <OpenGL/glu.h>
#endif

#include "graphics.h"

class
OCTINTERP_API
opengl_renderer
{
public:
  opengl_renderer (void) { }

  virtual ~opengl_renderer (void) { }

  virtual void draw (const graphics_handle& h)
    { draw (gh_manager::get_object (h)); }

  virtual void draw (const graphics_object& go);

  virtual void draw (const Matrix& hlist)
    {
      int len = hlist.length ();

      for (int i = 0; i < len; i++)
	{
	  graphics_handle h = gh_manager::lookup (hlist(i));

	  if (h.ok ())
	    draw (h);
	}
    }

  virtual void set_viewport (int w, int h);

protected:
  virtual void draw (const figure::properties& props);
  virtual void draw (const axes::properties& props);
  virtual void draw (const line::properties& props);
  virtual void draw (const surface::properties& props);
  virtual void draw (const patch::properties& props);
  virtual void draw (const hggroup::properties& props);

  virtual void set_color (const Matrix& c);
  virtual void set_polygon_offset (bool on, double offset = 0.0);
  virtual void set_linewidth (float w);
  virtual void set_linestyle (const std::string& s, bool stipple = false);
  virtual void set_clipbox (double x1, double x2, double y1, double y2,
			    double z1, double z2);
  virtual void set_clipping (bool on);

  virtual void init_marker (const std::string& m, double size, float width);
  virtual void end_marker (void);
  virtual void draw_marker (double x, double y, double z,
			    const Matrix& lc, const Matrix& fc);

private:
  opengl_renderer (const opengl_renderer&) { }

  opengl_renderer& operator = (const opengl_renderer&)
    { return *this; }

  bool is_nan_or_inf (double x, double y, double z) const
    {
      return (xisnan (x) || xisnan (y) || xisnan (z)
	      || xisinf (x) || xisinf (y) || xisinf (z));
    }

  octave_uint8 clip_code (double x, double y, double z) const
    {
      return ((x < xmin ? 1 : 0)
	      | (x > xmax ? 1 : 0) << 1
	      | (y < ymin ? 1 : 0) << 2
	      | (y > ymax ? 1 : 0) << 3
	      | (z < zmin ? 1 : 0) << 4
	      | (z > zmax ? 1 : 0) << 5
	      | (is_nan_or_inf (x, y, z) ? 0 : 1) << 6);
    }

  unsigned int make_marker_list (const std::string& m, double size,
				 bool filled) const;

private:
  // the backend associated with the figure being rendered
  graphics_backend backend;

  // axes transformation data
  graphics_xform xform;

  // axis limits in model scaled coordinate
  double xmin, xmax;
  double ymin, ymax;
  double zmin, zmax;

  // Z projection limits in windows coordinate
  double xZ1, xZ2;

  // call lists identifiers for markers
  unsigned int marker_id, filled_marker_id;

  // camera information for primitive sorting
  ColumnVector camera_pos, camera_dir;

private:
  class patch_tesselator;
};

#endif
