////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2023 The Octave Project Developers
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

#if ! defined (octave_gl_render_h)
#define octave_gl_render_h 1

#include "octave-config.h"

#include "graphics.h"
#include "text-renderer.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class opengl_functions;

class
OCTINTERP_API
opengl_renderer
{
public:

  opengl_renderer (opengl_functions& glfcns);

  // No copying!

  opengl_renderer (const opengl_renderer&) = delete;

  opengl_renderer& operator = (const opengl_renderer&) = delete;

  virtual ~opengl_renderer (void) = default;

  opengl_functions& get_opengl_functions (void) const { return m_glfcns; }

  virtual void draw (const graphics_object& go, bool toplevel = true);

  // The following version of the draw method is not declared virtual
  // because no derived class overrides it.

  void draw (const Matrix& hlist, bool toplevel = false);

  virtual void set_viewport (int w, int h);
  virtual void set_device_pixel_ratio (double dpr) { m_devpixratio = dpr; }
  virtual Matrix get_viewport_scaled (void) const;
  virtual graphics_xform get_transform (void) const { return m_xform; }
  virtual uint8NDArray get_pixels (int width, int height);

  virtual void draw_zoom_box (int width, int height,
                              int x1, int y1, int x2, int y2,
                              const Matrix& overlaycolor,
                              double overlayalpha,
                              const Matrix& bordercolor,
                              double borderalpha, double borderwidth);

  virtual void finish (void);

protected:

  virtual void draw_figure (const figure::properties& props);
  virtual void draw_axes (const axes::properties& props);
  virtual void draw_line (const line::properties& props);
  virtual void draw_surface (const surface::properties& props);
  virtual void draw_patch (const patch::properties& props);
  virtual void draw_scatter (const scatter::properties& props);
  virtual void draw_light (const light::properties& props);
  virtual void draw_hggroup (const hggroup::properties& props);
  virtual void draw_text (const text::properties& props);
  virtual void draw_text_background (const text::properties& props,
                                     bool do_rotate = false);
  virtual void draw_image (const image::properties& props);
  virtual void draw_uipanel (const uipanel::properties& props,
                             const graphics_object& go);
  virtual void draw_uibuttongroup (const uibuttongroup::properties& props,
                                   const graphics_object& go);
  virtual void init_gl_context (bool enhanced, const Matrix& backgroundColor);
  virtual void setup_opengl_transformation (const axes::properties& props);

  virtual void set_clipbox (double x1, double x2, double y1, double y2,
                            double z1, double z2);
  virtual void set_clipping (bool on);
  virtual void set_font (const base_properties& props);
  virtual void set_color (const Matrix& c);
  virtual void set_interpreter (const caseless_str& interp)
  {
    m_interpreter = interp;
  }
  virtual void set_linewidth (float w);
  virtual void set_linestyle (const std::string& s, bool stipple = false,
                              double linewidth = 0.5);
  virtual void set_linecap (const std::string&) { };
  virtual void set_linejoin (const std::string&) { };
  virtual void set_polygon_offset (bool on, float offset = 0.0f);
  virtual void set_selecting (bool on)
  {
    m_selecting = on;
  }

  virtual void init_marker (const std::string& m, double size, float width);
  virtual void change_marker (const std::string& m, double size);
  virtual void end_marker (void);
  virtual void draw_marker (double x, double y, double z,
                            const Matrix& lc, const Matrix& fc,
                            const double la = 1.0, const double fa = 1.0);

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

  virtual void render_grid (const double linewidth,
                            const std::string& gridstyle,
                            const Matrix& gridcolor, const double gridalpha,
                            const Matrix& ticks, double lim1, double lim2,
                            double p1, double p1N, double p2, double p2N,
                            int xyz, bool is_3D);

  virtual void render_tickmarks (const Matrix& ticks,
                                 double lim1, double lim2,
                                 double p1, double p1N, double p2, double p2N,
                                 double dx, double dy, double dz,
                                 int xyz, bool doubleside);

  virtual void render_ticktexts (const Matrix& ticks,
                                 const string_vector& ticklabels,
                                 double lim1, double lim2,
                                 double p1, double p2,
                                 int xyz, int ha, int va,
                                 int& wmax, int& hmax);

  virtual void draw_zoom_rect (int x1, int y1, int x2, int y2);

  //--------

  opengl_functions& m_glfcns;

  // axis limits in model scaled coordinate
  double m_xmin, m_xmax;
  double m_ymin, m_ymax;
  double m_zmin, m_zmax;

  // Factor used for translating Octave pixels to actual device pixels
  double m_devpixratio;

  // axes transformation data
  graphics_xform m_xform;

private:

  class patch_tessellator;

  void init_maxlights (void);

  std::string get_string (unsigned int id) const;

  bool is_nan_or_inf (double x, double y, double z) const
  {
    return (math::isnan (x) || math::isnan (y)
            || math::isnan (z)
            || math::isinf (x) || math::isinf (y)
            || math::isinf (z));
  }

  uint8_t clip_code (double x, double y, double z) const
  {
    return ((x < m_xmin ? 1 : 0)
            | (x > m_xmax ? 1 : 0) << 1
            | (y < m_ymin ? 1 : 0) << 2
            | (y > m_ymax ? 1 : 0) << 3
            | (z < m_zmin ? 1 : 0) << 4
            | (z > m_zmax ? 1 : 0) << 5
            | (is_nan_or_inf (x, y, z) ? 0 : 1) << 6);
  }

  void render_text (uint8NDArray pixels, Matrix bbox,
                    double x, double y, double z, double rotation);

  void set_normal (int bfl_mode, const NDArray& n, int j, int i);

  void set_ortho_coordinates (void);

  void restore_previous_coordinates (void);

  double points_to_pixels (const double val) const;

  unsigned int make_marker_list (const std::string& m, double size,
                                 bool filled) const;

  void draw_axes_planes (const axes::properties& props);
  void draw_axes_boxes (const axes::properties& props);

  void draw_axes_grids (const axes::properties& props);
  void draw_axes_x_grid (const axes::properties& props);
  void draw_axes_y_grid (const axes::properties& props);
  void draw_axes_z_grid (const axes::properties& props);

  void draw_axes_children (const axes::properties& props);

  void draw_all_lights (const base_properties& props,
                        std::list<graphics_object>& obj_list);

  void draw_texture_image (const octave_value cdata,
                           Matrix x, Matrix y, bool ortho = false);

  //--------

  // The graphics m_toolkit associated with the figure being rendered.
  graphics_toolkit m_toolkit;

  // Z projection limits in windows coordinate
  double m_xZ1, m_xZ2;

  // call lists identifiers for markers
  unsigned int m_marker_id, m_filled_marker_id;

  // camera information for primitive sorting and lighting
  ColumnVector m_camera_pos, m_camera_dir, m_view_vector;

  // interpreter to be used by text_to_pixels
  caseless_str m_interpreter;

  text_renderer m_txt_renderer;

  // light object present and visible
  unsigned int m_current_light;
  unsigned int m_max_lights;

  // Indicate we are drawing for selection purpose
  bool m_selecting;

  // Indicate we are drawing for printing purpose
  bool m_printing;
};

OCTAVE_END_NAMESPACE(octave)

#endif
