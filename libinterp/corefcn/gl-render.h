/*

Copyright (C) 2008-2016 Michael Goffioul

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_gl_render_h)
#define octave_gl_render_h 1

#include "octave-config.h"

#include "graphics.h"
#include "text-renderer.h"

namespace octave
{
  class
  OCTINTERP_API
  opengl_renderer
  {
  public:

    opengl_renderer (void);

    // No copying!

    opengl_renderer (const opengl_renderer&) = delete;

    opengl_renderer& operator = (const opengl_renderer&) = delete;

    virtual ~opengl_renderer (void) = default;

    virtual void draw (const graphics_object& go, bool toplevel = true);

    virtual void draw (const Matrix& hlist, bool toplevel = false)
    {
      int len = hlist.numel ();

      for (int i = len-1; i >= 0; i--)
        {
          graphics_object obj = gh_manager::get_object (hlist(i));

          if (obj)
            draw (obj, toplevel);
        }
    }

    virtual void set_viewport (int w, int h);
    virtual graphics_xform get_transform (void) const { return xform; }

    virtual void finish (void);

  protected:
    virtual void draw_figure (const figure::properties& props);
    virtual void draw_axes (const axes::properties& props);
    virtual void draw_line (const line::properties& props);
    virtual void draw_surface (const surface::properties& props);
    virtual void draw_patch (const patch::properties& props);
    virtual void draw_light (const light::properties& props);
    virtual void draw_hggroup (const hggroup::properties& props);
    virtual void draw_text (const text::properties& props);
    virtual void draw_image (const image::properties& props);
    virtual void draw_uipanel (const uipanel::properties& props,
                               const graphics_object& go);
    virtual void draw_uibuttongroup (const uibuttongroup::properties& props,
                                     const graphics_object& go);
    virtual void init_gl_context (bool enhanced, const Matrix& backgroundColor);
    virtual void setup_opengl_transformation (const axes::properties& props);

    virtual void set_color (const Matrix& c);
    virtual void set_polygon_offset (bool on, float offset = 0.0f);
    virtual void set_linewidth (float w);
    virtual void set_linestyle (const std::string& s, bool stipple = false,
                                double linewidth = 0.5);
    virtual void set_linecap (const std::string&) { };
    virtual void set_linejoin (const std::string&) { };
    virtual void set_clipbox (double x1, double x2, double y1, double y2,
                              double z1, double z2);
    virtual void set_clipping (bool on);
    virtual void set_font (const base_properties& props);
    virtual void set_interpreter (const caseless_str& interp)
    {
      interpreter = interp;
    }

    virtual void init_marker (const std::string& m, double size, float width);
    virtual void end_marker (void);
    virtual void draw_marker (double x, double y, double z,
                              const Matrix& lc, const Matrix& fc);

    virtual void text_to_pixels (const std::string& txt,
                                 uint8NDArray& pixels,
                                 Matrix& bbox,
                                 int halign = 0, int valign = 0,
                                 double rotation = 0.0);

    virtual void text_to_strlist (const std::string& txt,
                                  std::list<text_renderer::string>& lst,
                                  Matrix& bbox,
                                  int halign = 0, int valign = 0,
                                  double rotation = 0.0);

    virtual Matrix render_text (const std::string& txt,
                                double x, double y, double z,
                                int halign, int valign, double rotation = 0.0);

    virtual void draw_pixels (int w, int h, const float *data);
    virtual void draw_pixels (int w, int h, const uint8_t *data);
    virtual void draw_pixels (int w, int h, const uint16_t *data);

    virtual void render_grid (const double linewidth,
                              const std::string& gridstyle,
                              const Matrix& gridcolor, const double gridalpha,
                              const Matrix& ticks, double lim1, double lim2,
                              double p1, double p1N, double p2, double p2N,
                              int xyz, bool is_3D);

    virtual void render_tickmarks (const Matrix& ticks, double lim1, double lim2,
                                   double p1, double p1N, double p2, double p2N,
                                   double dx, double dy, double dz,
                                   int xyz, bool doubleside);

    virtual void render_ticktexts (const Matrix& ticks,
                                   const string_vector& ticklabels,
                                   double lim1, double lim2,
                                   double p1, double p2,
                                   int xyz, int ha, int va,
                                   int& wmax, int& hmax);

  private:

    bool is_nan_or_inf (double x, double y, double z) const
    {
      return (octave::math::isnan (x) || octave::math::isnan (y)
              || octave::math::isnan (z)
              || octave::math::isinf (x) || octave::math::isinf (y)
              || octave::math::isinf (z));
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

    void set_normal (int bfl_mode, const NDArray& n, int j, int i);

    unsigned int make_marker_list (const std::string& m, double size,
                                   bool filled) const;

    void draw_axes_planes (const axes::properties& props);
    void draw_axes_boxes (const axes::properties& props);

    void draw_axes_x_grid (const axes::properties& props);
    void draw_axes_y_grid (const axes::properties& props);
    void draw_axes_z_grid (const axes::properties& props);

    void draw_axes_children (const axes::properties& props);

    void draw_all_lights (const base_properties& props,
                          std::list<graphics_object>& obj_list);

  private:
    // The graphics toolkit associated with the figure being rendered.
    graphics_toolkit toolkit;

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

    // camera information for primitive sorting and lighting
    ColumnVector camera_pos, camera_dir, view_vector;

    // interpreter to be used by text_to_pixels
    caseless_str interpreter;

    text_renderer txt_renderer;

    // light object present and visible
    int num_lights;
    unsigned int current_light;
    int max_lights;

  private:
    class patch_tesselator;
  };
}

#endif
