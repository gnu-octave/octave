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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <limits>
#include <memory>
#include <sstream>

#if defined (HAVE_WINDOWS_H)
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>
#endif

#include "lo-mappers.h"
#include "oct-locbuf.h"

#include "errwarn.h"
#include "gl-render.h"
#include "interpreter-private.h"
#include "oct-opengl.h"
#include "text-renderer.h"

OCTAVE_BEGIN_NAMESPACE(octave)

#if defined (HAVE_OPENGL)

static int
next_power_of_2 (int n)
{
  int m = 1;

  while (m < n && m < std::numeric_limits<int>::max ())
    m <<= 1;

  return m;
}

#define LIGHT_MODE GL_FRONT_AND_BACK

// Use symbolic names for axes
enum
{
  X_AXIS,
  Y_AXIS,
  Z_AXIS
};

// Use symbolic names for color mode
enum
{
  UNIFORM,
  FLAT,
  INTERP,
  TEXTURE
};

// Use symbolic names for lighting
enum
{
  NONE,
  //FLAT,  // Already declared in anonymous enum for color mode
  GOURAUD = 2
};

// Win32 API requires the CALLBACK attributes for
// GLU callback functions.  Define it to empty on
// other platforms.
#if ! defined (CALLBACK)
#  define CALLBACK
#endif

class opengl_texture
{
private:

  class texture_rep
  {
  public:

    texture_rep (opengl_functions& glfcns)
      : m_glfcns (glfcns), m_id (), m_w (), m_h (), m_tw (), m_th (),
        m_tx (), m_ty (), m_valid (false)
    { }

    texture_rep (opengl_functions& glfcns, GLuint id, int w, int h,
                 int tw, int th)
      : m_glfcns (glfcns), m_id (id), m_w (w), m_h (h), m_tw (tw), m_th (th),
        m_tx (double(m_w)/m_tw), m_ty (double(m_h)/m_th), m_valid (true)
    { }

    ~texture_rep (void)
    {
      if (m_valid)
        m_glfcns.glDeleteTextures (1, &m_id);
    }

    void bind (int mode) const
    {
      if (m_valid)
        m_glfcns.glBindTexture (mode, m_id);
    }

    void tex_coord (double q, double r) const
    {
      if (m_valid)
        m_glfcns.glTexCoord2d (q*m_tx, r*m_ty);
    }

    opengl_functions& m_glfcns;
    GLuint m_id;
    int m_w, m_h;
    int m_tw, m_th;
    double m_tx, m_ty;
    bool m_valid;
  };

public:

  opengl_texture (opengl_functions& glfcns)
    : m_rep (new texture_rep (glfcns))
  { }

  opengl_texture (opengl_functions& glfcns, GLuint id, int w, int h,
                  int tw, int th)
    : m_rep (new texture_rep (glfcns, id, w, h, tw, th))
  { }

  opengl_texture (const opengl_texture&) = default;

  ~opengl_texture (void) = default;

  opengl_texture& operator = (const opengl_texture&) = default;

  static opengl_texture create (opengl_functions& glfcns,
                                const octave_value& data);

  void bind (int mode = GL_TEXTURE_2D) const { m_rep->bind (mode); }

  void tex_coord (double q, double r) const { m_rep->tex_coord (q, r); }

  bool is_valid (void) const { return m_rep->m_valid; }

private:

  opengl_texture (const std::shared_ptr<texture_rep>& new_rep)
    : m_rep (new_rep)
  { }

  std::shared_ptr<texture_rep> m_rep;
};

opengl_texture
opengl_texture::create (opengl_functions& glfcns, const octave_value& data)
{
  opengl_texture retval (glfcns);

  dim_vector dv (data.dims ());

  // Expect RGB data
  if (dv.ndims () == 3 && (dv(2) == 3 || dv(2) == 4))
    {
      // FIXME: dim_vectors hold octave_idx_type values.
      //        Should we check for dimensions larger than intmax?
      int h, w, tw, th;
      h = dv(0), w = dv(1);

      // Return early if the image data are larger than the texture
      // can hold
      int max_size;
      glGetIntegerv (GL_MAX_TEXTURE_SIZE, &max_size);
      static bool warned = false;
      if (h > max_size || w > max_size)
        {
          if (! warned)
            {
              warning ("opengl_texture::create: the opengl library in use "
                       "doesn't support images with either dimension larger "
                       "than %d. Not rendering.", max_size);
              warned = true;
            }

          return opengl_texture (glfcns);
        }

      GLuint id;
      bool ok = true;

      tw = next_power_of_2 (w);
      th = next_power_of_2 (h);

      glfcns.glGenTextures (1, &id);
      glfcns.glBindTexture (GL_TEXTURE_2D, id);

      if (data.is_double_type ())
        {
          const NDArray xdata = data.array_value ();

          OCTAVE_LOCAL_BUFFER (GLfloat, a, (3*tw*th));

          for (int i = 0; i < h; i++)
            {
              for (int j = 0, idx = i*tw*3; j < w; j++, idx += 3)
                {
                  a[idx]   = xdata(i, j, 0);
                  a[idx+1] = xdata(i, j, 1);
                  a[idx+2] = xdata(i, j, 2);
                }
            }

          glfcns.glTexImage2D (GL_TEXTURE_2D, 0, 3, tw, th, 0, GL_RGB,
                               GL_FLOAT, a);
        }

      else if (data.is_single_type ())
        {
          const FloatNDArray xdata = data.float_array_value ();

          OCTAVE_LOCAL_BUFFER (GLfloat, a, (3*tw*th));

          for (int i = 0; i < h; i++)
            {
              for (int j = 0, idx = i*tw*3; j < w; j++, idx += 3)
                {
                  a[idx]   = xdata(i, j, 0);
                  a[idx+1] = xdata(i, j, 1);
                  a[idx+2] = xdata(i, j, 2);
                }
            }

          glfcns.glTexImage2D (GL_TEXTURE_2D, 0, 3, tw, th, 0, GL_RGB,
                               GL_FLOAT, a);
        }
      else if (data.is_uint16_type ())
        {
          const uint16NDArray xdata = data.uint16_array_value ();

          OCTAVE_LOCAL_BUFFER (GLushort, a, (3*tw*th));

          for (int i = 0; i < h; i++)
            {
              for (int j = 0, idx = i*tw*3; j < w; j++, idx += 3)
                {
                  a[idx]   = xdata(i, j, 0);
                  a[idx+1] = xdata(i, j, 1);
                  a[idx+2] = xdata(i, j, 2);
                }
            }

          glfcns.glTexImage2D (GL_TEXTURE_2D, 0, 3, tw, th, 0,
                               GL_RGB, GL_UNSIGNED_SHORT, a);
        }
      else if (data.is_uint8_type () && dv(2) == 3)
        {
          const uint8NDArray xdata = data.uint8_array_value ();

          OCTAVE_LOCAL_BUFFER (GLubyte, a, (3*tw*th));

          for (int i = 0; i < h; i++)
            {
              for (int j = 0, idx = i*tw*3; j < w; j++, idx += 3)
                {
                  a[idx]   = xdata(i, j, 0);
                  a[idx+1] = xdata(i, j, 1);
                  a[idx+2] = xdata(i, j, 2);
                }
            }

          glfcns.glTexImage2D (GL_TEXTURE_2D, 0, 3, tw, th, 0,
                               GL_RGB, GL_UNSIGNED_BYTE, a);
        }
      else if (data.is_uint8_type () && dv(2) == 4)
        {
          const uint8NDArray xdata = data.uint8_array_value ();

          OCTAVE_LOCAL_BUFFER (GLubyte, a, (4*tw*th));

          for (int i = 0; i < h; i++)
            {
              for (int j = 0, idx = i*tw*4; j < w; j++, idx += 4)
                {
                  a[idx]   = xdata(i, j, 0);
                  a[idx+1] = xdata(i, j, 1);
                  a[idx+2] = xdata(i, j, 2);
                  a[idx+3] = xdata(i, j, 3);
                }
            }

          glfcns.glTexImage2D (GL_TEXTURE_2D, 0, GL_RGBA, tw, th, 0,
                               GL_RGBA, GL_UNSIGNED_BYTE, a);
        }
      else
        {
          ok = false;
          warning ("opengl_texture::create: invalid image data type, expected double, single, uint8, or uint16");
        }

      if (ok)
        {
          glfcns.glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
                                  GL_NEAREST);
          glfcns.glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,
                                  GL_NEAREST);

          if (glfcns.glGetError () != GL_NO_ERROR)
            warning ("opengl_texture::create: OpenGL error while generating texture data");
          else
            retval = opengl_texture (glfcns, id, w, h, tw, th);
        }
    }
  else
    warning ("opengl_texture::create: invalid texture data size");

  return retval;
}

class
opengl_tessellator
{
public:
#if defined (HAVE_FRAMEWORK_OPENGL) && defined (HAVE_GLUTESSCALLBACK_THREEDOTS)
  typedef GLvoid (CALLBACK *fcn) (...);
#else
  typedef void (CALLBACK *fcn) (void);
#endif

public:

  opengl_tessellator (void) : m_glu_tess (nullptr), m_fill () { init (); }

  // No copying!

  opengl_tessellator (const opengl_tessellator&) = delete;

  opengl_tessellator operator = (const opengl_tessellator&) = delete;

  virtual ~opengl_tessellator (void)
  { if (m_glu_tess) gluDeleteTess (m_glu_tess); }

  void begin_polygon (bool filled = true)
  {
    gluTessProperty (m_glu_tess, GLU_TESS_BOUNDARY_ONLY,
                     (filled ? GL_FALSE : GL_TRUE));
    m_fill = filled;
    gluTessBeginPolygon (m_glu_tess, this);
  }

  void end_polygon (void) const
  { gluTessEndPolygon (m_glu_tess); }

  void begin_contour (void) const
  { gluTessBeginContour (m_glu_tess); }

  void end_contour (void) const
  { gluTessEndContour (m_glu_tess); }

  void add_vertex (double *loc, void *data) const
  { gluTessVertex (m_glu_tess, loc, data); }

protected:
  virtual void begin (GLenum /*type*/) { }

  virtual void end (void) { }

  virtual void vertex (void * /*data*/) { }

  virtual void combine (GLdouble [3] /*c*/, void *[4] /*data*/,
                        GLfloat  [4] /*w*/, void ** /*out_data*/) { }

  virtual void edge_flag (GLboolean /*flag*/) { }

  virtual void error (GLenum err)
  { ::error ("OpenGL tessellation error (%d)", err); }

  virtual void init (void)
  {
    m_glu_tess = gluNewTess ();

    gluTessCallback (m_glu_tess, GLU_TESS_BEGIN_DATA,
                     reinterpret_cast<fcn> (tess_begin));
    gluTessCallback (m_glu_tess, GLU_TESS_END_DATA,
                     reinterpret_cast<fcn> (tess_end));
    gluTessCallback (m_glu_tess, GLU_TESS_VERTEX_DATA,
                     reinterpret_cast<fcn> (tess_vertex));
    gluTessCallback (m_glu_tess, GLU_TESS_COMBINE_DATA,
                     reinterpret_cast<fcn> (tess_combine));
    gluTessCallback (m_glu_tess, GLU_TESS_EDGE_FLAG_DATA,
                     reinterpret_cast<fcn> (tess_edge_flag));
    gluTessCallback (m_glu_tess, GLU_TESS_ERROR_DATA,
                     reinterpret_cast<fcn> (tess_error));
  }

  bool is_filled (void) const { return m_fill; }

private:
  static void CALLBACK tess_begin (GLenum type, void *t)
  { reinterpret_cast<opengl_tessellator *> (t)->begin (type); }

  static void CALLBACK tess_end (void *t)
  { reinterpret_cast<opengl_tessellator *> (t)->end (); }

  static void CALLBACK tess_vertex (void *v, void *t)
  { reinterpret_cast<opengl_tessellator *> (t)->vertex (v); }

  static void CALLBACK tess_combine (GLdouble c[3], void *v[4], GLfloat w[4],
                                     void **out,  void *t)
  { reinterpret_cast<opengl_tessellator *> (t)->combine (c, v, w, out); }

  static void CALLBACK tess_edge_flag (GLboolean flag, void *t)
  { reinterpret_cast<opengl_tessellator *> (t)->edge_flag (flag); }

  static void CALLBACK tess_error (GLenum err, void *t)
  { reinterpret_cast<opengl_tessellator *> (t)->error (err); }

  //--------

  GLUtesselator *m_glu_tess;
  bool m_fill;
};

class vertex_data
{
public:

  class vertex_data_rep
  {
  public:

    vertex_data_rep (void)
      : m_coords (), m_color (), m_vertex_normal (), m_face_normal (),
        m_alpha (), m_ambient (), m_diffuse (), m_specular (),
        m_specular_exp (), m_specular_color_refl ()
    { }

    vertex_data_rep (const Matrix& c, const Matrix& col, const Matrix& vn,
                     const Matrix& fn, double a, float as, float ds, float ss,
                     float se, float scr)
      : m_coords (c), m_color (col), m_vertex_normal (vn),
        m_face_normal (fn), m_alpha (a), m_ambient (as), m_diffuse (ds),
        m_specular (ss), m_specular_exp (se), m_specular_color_refl (scr)
    { }

    Matrix m_coords;
    Matrix m_color;
    Matrix m_vertex_normal;
    Matrix m_face_normal;
    double m_alpha;
    float m_ambient;
    float m_diffuse;
    float m_specular;
    float m_specular_exp;
    float m_specular_color_refl;
  };

public:

  // Required to instantiate std::list<vertex_data> objects.
  vertex_data (void) : m_rep (nil_rep ()) { }

  vertex_data (const Matrix& c, const Matrix& col, const Matrix& vn,
               const Matrix& fn, double a, float as, float ds, float ss,
               float se, float scr)
    : m_rep (new vertex_data_rep (c, col, vn, fn, a, as, ds, ss, se, scr))
  { }

  vertex_data (const vertex_data&) = default;

  ~vertex_data (void) = default;

  vertex_data& operator = (const vertex_data&) = default;

  vertex_data_rep * get_rep (void) const { return m_rep.get (); }

private:

  static std::shared_ptr<vertex_data_rep> nil_rep (void)
  {
    static std::shared_ptr<vertex_data_rep> nr (new vertex_data_rep ());

    return nr;
  }

  std::shared_ptr<vertex_data_rep> m_rep;
};

class
opengl_renderer::patch_tessellator : public opengl_tessellator
{
public:
  patch_tessellator (opengl_renderer *r, int cmode, int lmode, bool fl,
                     float idx = 0.0)
    : opengl_tessellator (), m_renderer (r),
      m_color_mode (cmode), m_light_mode (lmode), m_face_lighting (fl),
      m_index (idx), m_first (true), m_tmp_vdata ()
  { }

protected:
  void begin (GLenum type)
  {
    opengl_functions& glfcns = m_renderer->get_opengl_functions ();

    //printf ("patch_tessellator::begin (%d)\n", type);
    m_first = true;

    if (m_color_mode == INTERP || m_light_mode == GOURAUD)
      glfcns.glShadeModel (GL_SMOOTH);
    else
      glfcns.glShadeModel (GL_FLAT);

    if (is_filled ())
      m_renderer->set_polygon_offset (true, m_index);

    glfcns.glBegin (type);
  }

  void end (void)
  {
    opengl_functions& glfcns = m_renderer->get_opengl_functions ();

    //printf ("patch_tessellator::end\n");
    glfcns.glEnd ();
    m_renderer->set_polygon_offset (false);
  }

  void vertex (void *data)
  {
    opengl_functions& glfcns = m_renderer->get_opengl_functions ();

    vertex_data::vertex_data_rep *v
      = reinterpret_cast<vertex_data::vertex_data_rep *> (data);
    //printf ("patch_tessellator::vertex (%g, %g, %g)\n", v->m_coords(0), v->m_coords(1), v->m_coords(2));

    // NOTE: OpenGL can re-order vertices.  For "flat" coloring of FaceColor
    // the first vertex must be identified in the draw_patch routine.

    if (m_color_mode == INTERP || (m_color_mode == FLAT && ! is_filled ()))
      {
        Matrix col = v->m_color;

        if (col.numel () == 3)
          {
            glfcns.glColor4d (col(0), col(1), col(2), v->m_alpha);
            if (m_light_mode > 0)
              {
                // edge lighting only uses ambient light
                float buf[4] = { 0.0f, 0.0f, 0.0f, 1.0f };

                if (m_face_lighting)
                  for (int k = 0; k < 3; k++)
                    buf[k] = (v->m_specular
                              * (v->m_specular_color_refl +
                                 (1 - v->m_specular_color_refl) * col(k)));
                glfcns.glMaterialfv (LIGHT_MODE, GL_SPECULAR, buf);

                if (m_face_lighting)
                  for (int k = 0; k < 3; k++)
                    buf[k] = (v->m_diffuse * col(k));
                glfcns.glMaterialfv (LIGHT_MODE, GL_DIFFUSE, buf);

                for (int k = 0; k < 3; k++)
                  buf[k] = (v->m_ambient * col(k));
                glfcns.glMaterialfv (LIGHT_MODE, GL_AMBIENT, buf);
              }
          }
      }

    if (m_light_mode == FLAT && m_first)
      glfcns.glNormal3dv (v->m_face_normal.data ());
    else if (m_light_mode == GOURAUD)
      glfcns.glNormal3dv (v->m_vertex_normal.data ());

    glfcns.glVertex3dv (v->m_coords.data ());

    m_first = false;
  }

  void combine (GLdouble xyz[3], void *data[4], GLfloat w[4], void **out_data)
  {
    vertex_data::vertex_data_rep *v[4];
    int vmax = 4;

    for (int i = 0; i < 4; i++)
      {
        v[i] = reinterpret_cast<vertex_data::vertex_data_rep *> (data[i]);

        if (vmax == 4 && ! v[i])
          vmax = i;
      }

    Matrix vv (1, 3, 0.0);
    Matrix cc;
    Matrix vnn (1, 3, 0.0);
    Matrix fnn (1, 3, 0.0);
    double aa = 0.0;

    vv(0) = xyz[0];
    vv(1) = xyz[1];
    vv(2) = xyz[2];

    if (v[0]->m_color.numel ())
      {
        cc.resize (1, 3, 0.0);
        for (int ic = 0; ic < 3; ic++)
          for (int iv = 0; iv < vmax; iv++)
            cc(ic) += (w[iv] * v[iv]->m_color (ic));
      }

    if (v[0]->m_vertex_normal.numel () > 0)
      {
        for (int in = 0; in < 3; in++)
          for (int iv = 0; iv < vmax; iv++)
            vnn(in) += (w[iv] * v[iv]->m_vertex_normal (in));
      }

    if (v[0]->m_face_normal.numel () > 0)
      {
        for (int in = 0; in < 3; in++)
          for (int iv = 0; iv < vmax; iv++)
            fnn(in) += (w[iv] * v[iv]->m_face_normal (in));
      }

    for (int iv = 0; iv < vmax; iv++)
      aa += (w[iv] * v[iv]->m_alpha);

    vertex_data new_v (vv, cc, vnn, fnn, aa, v[0]->m_ambient, v[0]->m_diffuse,
                       v[0]->m_specular, v[0]->m_specular_exp,
                       v[0]->m_specular_color_refl);
    m_tmp_vdata.push_back (new_v);

    *out_data = new_v.get_rep ();
  }

private:

  // No copying!

  patch_tessellator (const patch_tessellator&) = delete;

  patch_tessellator& operator = (const patch_tessellator&) = delete;

  opengl_renderer *m_renderer;
  int m_color_mode;
  int m_light_mode;
  bool m_face_lighting;
  int m_index;
  bool m_first;
  std::list<vertex_data> m_tmp_vdata;
};

#else

class
opengl_renderer::patch_tessellator
{
  // Dummy class.
};

#endif

opengl_renderer::opengl_renderer (opengl_functions& glfcns)
  : m_glfcns (glfcns), m_xmin (), m_xmax (), m_ymin (), m_ymax (),
    m_zmin (), m_zmax (), m_devpixratio (1.0), m_xform (), m_toolkit (),
    m_xZ1 (), m_xZ2 (), m_marker_id (), m_filled_marker_id (),
    m_camera_pos (), m_camera_dir (), m_view_vector (),
    m_interpreter ("none"), m_txt_renderer (), m_current_light (0),
    m_max_lights (0), m_selecting (false), m_printing (false)
{
  // This constructor will fail if we don't have OpenGL or if the data
  // types we assumed in our public interface aren't compatible with the
  // OpenGL types.

#if defined (HAVE_OPENGL)

  // Ensure that we can't request an image larger than OpenGL can handle.
  // FIXME: should we check signed vs. unsigned?

  static bool ok = (sizeof (int) <= sizeof (GLsizei));

  if (! ok)
    error ("the size of GLsizei is smaller than the size of int");

#else

  err_disabled_feature ("opengl_renderer", "OpenGL");

#endif
}

void
opengl_renderer::draw (const graphics_object& go, bool toplevel)
{
  if (! go.valid_object ())
    return;

  const base_properties& props = go.get_properties ();

  if (! m_toolkit)
    m_toolkit = props.get_toolkit ();

  if (go.isa ("figure"))
    draw_figure (dynamic_cast<const figure::properties&> (props));
  else if (go.isa ("axes"))
    draw_axes (dynamic_cast<const axes::properties&> (props));
  else if (go.isa ("line"))
    draw_line (dynamic_cast<const line::properties&> (props));
  else if (go.isa ("surface"))
    draw_surface (dynamic_cast<const surface::properties&> (props));
  else if (go.isa ("patch"))
    draw_patch (dynamic_cast<const patch::properties&> (props));
  else if (go.isa ("scatter"))
    draw_scatter (dynamic_cast<const scatter::properties&> (props));
  else if (go.isa ("light"))
    draw_light (dynamic_cast<const light::properties&> (props));
  else if (go.isa ("hggroup"))
    draw_hggroup (dynamic_cast<const hggroup::properties&> (props));
  else if (go.isa ("text"))
    draw_text (dynamic_cast<const text::properties&> (props));
  else if (go.isa ("image"))
    draw_image (dynamic_cast<const image::properties&> (props));
  else if (go.isa ("uimenu") || go.isa ("uicontrol")
           || go.isa ("uicontextmenu") || go.isa ("uitoolbar")
           || go.isa ("uipushtool") || go.isa ("uitoggletool")
           || go.isa ("uitable"))
    ; // SKIP
  else if (go.isa ("uipanel"))
    {
      if (toplevel)
        draw_uipanel (dynamic_cast<const uipanel::properties&> (props), go);
    }
  else if (go.isa ("uibuttongroup"))
    {
      if (toplevel)
        draw_uibuttongroup (dynamic_cast<const uibuttongroup::properties&> (props), go);
    }
  else
    {
      warning ("opengl_renderer: cannot render object of type '%s'",
               props.graphics_object_name ().c_str ());
    }

#if defined (HAVE_OPENGL)

  GLenum gl_error = m_glfcns.glGetError ();
  if (gl_error)
    warning ("opengl_renderer: Error '%s' (%d) occurred drawing '%s' object",
             gluErrorString (gl_error), gl_error,
             props.graphics_object_name ().c_str ());

#endif
}

void
opengl_renderer::draw_figure (const figure::properties& props)
{
  m_printing = props.is___printing__ ();

  // Initialize OpenGL context
  init_gl_context (props.is_graphicssmoothing (), props.get_color_rgb ());

#if defined (HAVE_OPENGL)

  props.set___gl_extensions__ (get_string (GL_EXTENSIONS));
  props.set___gl_renderer__ (get_string (GL_RENDERER));
  props.set___gl_vendor__ (get_string (GL_VENDOR));
  props.set___gl_version__ (get_string (GL_VERSION));

#endif

  // Draw children

  draw (props.get_all_children (), false);
}

void
opengl_renderer::draw_uipanel (const uipanel::properties& props,
                               const graphics_object& go)
{
  graphics_object fig = go.get_ancestor ("figure");
  const figure::properties& figProps
    = dynamic_cast<const figure::properties&> (fig.get_properties ());

  // Initialize OpenGL context

  init_gl_context (figProps.is_graphicssmoothing (),
                   props.get_backgroundcolor_rgb ());

  // Draw children

  draw (props.get_all_children (), false);
}

void
opengl_renderer::draw_uibuttongroup (const uibuttongroup::properties& props,
                                     const graphics_object& go)
{
  graphics_object fig = go.get_ancestor ("figure");
  const figure::properties& figProps
    = dynamic_cast<const figure::properties&> (fig.get_properties ());

  // Initialize OpenGL context

  init_gl_context (figProps.is_graphicssmoothing (),
                   props.get_backgroundcolor_rgb ());

  // Draw children

  draw (props.get_all_children (), false);
}

void
opengl_renderer::init_gl_context (bool enhanced, const Matrix& c)
{
#if defined (HAVE_OPENGL)

  // Initialize OpenGL context

  m_glfcns.glEnable (GL_DEPTH_TEST);
  m_glfcns.glDepthFunc (GL_LEQUAL);
  m_glfcns.glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  m_glfcns.glAlphaFunc (GL_GREATER, 0.0f);
  m_glfcns.glEnable (GL_NORMALIZE);
  m_glfcns.glEnable (GL_BLEND);

  if (enhanced)
    {
      m_glfcns.glEnable (GL_MULTISAMPLE);
      bool has_multisample = false;
      if (! m_glfcns.glGetError ())
        {
          GLint iMultiSample, iNumSamples;
          m_glfcns.glGetIntegerv (GL_SAMPLE_BUFFERS, &iMultiSample);
          m_glfcns.glGetIntegerv (GL_SAMPLES, &iNumSamples);
          if (iMultiSample == GL_TRUE && iNumSamples > 0)
            has_multisample = true;
        }

      if (! has_multisample)
        {
          // MultiSample not implemented.  Use old-style anti-aliasing
          m_glfcns.glDisable (GL_MULTISAMPLE);
          // Disabling GL_MULTISAMPLE will raise a gl error if it is not
          // implemented.  Thus, call glGetError to reset the error state.
          m_glfcns.glGetError ();

          m_glfcns.glEnable (GL_LINE_SMOOTH);
          m_glfcns.glHint (GL_LINE_SMOOTH_HINT, GL_NICEST);
        }
    }
  else
    {
      m_glfcns.glDisable (GL_LINE_SMOOTH);
    }

  // Clear background

  if (c.numel () >= 3)
    {
      m_glfcns.glClearColor (c(0), c(1), c(2), 1);
      m_glfcns.glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    }

  GLenum gl_error = m_glfcns.glGetError ();
  if (gl_error)
    warning ("opengl_renderer: Error '%s' (%d) occurred in init_gl_context",
             gluErrorString (gl_error), gl_error);

#else

  octave_unused_parameter (enhanced);
  octave_unused_parameter (c);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::render_grid (const double linewidth,
                              const std::string& gridstyle,
                              const Matrix& gridcolor, const double gridalpha,
                              const Matrix& ticks, double lim1, double lim2,
                              double p1, double p1N, double p2, double p2N,
                              int xyz, bool is_3D)
{
#if defined (HAVE_OPENGL)

  m_glfcns.glColor4d (gridcolor(0), gridcolor(1), gridcolor(2), gridalpha);
  set_linestyle (gridstyle, true, linewidth);
  m_glfcns.glBegin (GL_LINES);
  for (int i = 0; i < ticks.numel (); i++)
    {
      double val = ticks(i);
      if (lim1 <= val && val <= lim2)
        {
          if (xyz == X_AXIS)
            {
              m_glfcns.glVertex3d (val, p1N, p2);
              m_glfcns.glVertex3d (val, p1, p2);
              if (is_3D)
                {
                  m_glfcns.glVertex3d (val, p1, p2N);
                  m_glfcns.glVertex3d (val, p1, p2);
                }
            }
          else if (xyz == Y_AXIS)
            {
              m_glfcns.glVertex3d (p1N, val, p2);
              m_glfcns.glVertex3d (p1, val, p2);
              if (is_3D)
                {
                  m_glfcns.glVertex3d (p1, val, p2N);
                  m_glfcns.glVertex3d (p1, val, p2);
                }
            }
          else if (xyz == Z_AXIS)
            {
              m_glfcns.glVertex3d (p1N, p2, val);
              m_glfcns.glVertex3d (p1, p2, val);
              m_glfcns.glVertex3d (p1, p2N, val);
              m_glfcns.glVertex3d (p1, p2, val);
            }
        }
    }
  m_glfcns.glEnd ();
  set_linestyle ("-");  // Disable LineStipple
  double black[3] = {0, 0, 0};
  m_glfcns.glColor3dv (black);

#else

  octave_unused_parameter (linewidth);
  octave_unused_parameter (gridstyle);
  octave_unused_parameter (gridcolor);
  octave_unused_parameter (gridalpha);
  octave_unused_parameter (ticks);
  octave_unused_parameter (lim1);
  octave_unused_parameter (lim2);
  octave_unused_parameter (p1);
  octave_unused_parameter (p1N);
  octave_unused_parameter (p2);
  octave_unused_parameter (p2N);
  octave_unused_parameter (xyz);
  octave_unused_parameter (is_3D);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::render_tickmarks (const Matrix& ticks,
                                   double lim1, double lim2,
                                   double p1, double p1N,
                                   double p2, double p2N,
                                   double dx, double dy, double dz,
                                   int xyz, bool mirror)
{
#if defined (HAVE_OPENGL)

  m_glfcns.glBegin (GL_LINES);

  for (int i = 0; i < ticks.numel (); i++)
    {
      double val = ticks(i);

      if (lim1 <= val && val <= lim2)
        {
          if (xyz == X_AXIS)
            {
              m_glfcns.glVertex3d (val, p1, p2);
              m_glfcns.glVertex3d (val, p1+dy, p2+dz);
              if (mirror)
                {
                  m_glfcns.glVertex3d (val, p1N, p2N);
                  m_glfcns.glVertex3d (val, p1N-dy, p2N-dz);
                }
            }
          else if (xyz == Y_AXIS)
            {
              m_glfcns.glVertex3d (p1, val, p2);
              m_glfcns.glVertex3d (p1+dx, val, p2+dz);
              if (mirror)
                {
                  m_glfcns.glVertex3d (p1N, val, p2N);
                  m_glfcns.glVertex3d (p1N-dx, val, p2N-dz);
                }
            }
          else if (xyz == Z_AXIS)
            {
              m_glfcns.glVertex3d (p1, p2, val);
              m_glfcns.glVertex3d (p1+dx, p2+dy, val);
              if (mirror)
                {
                  m_glfcns.glVertex3d (p1N, p2N, val);
                  m_glfcns.glVertex3d (p1N-dx, p2N-dy, val);
                }
            }
        }
    }

  m_glfcns.glEnd ();

#else

  octave_unused_parameter (ticks);
  octave_unused_parameter (lim1);
  octave_unused_parameter (lim2);
  octave_unused_parameter (p1);
  octave_unused_parameter (p1N);
  octave_unused_parameter (p2);
  octave_unused_parameter (p2N);
  octave_unused_parameter (dx);
  octave_unused_parameter (dy);
  octave_unused_parameter (dz);
  octave_unused_parameter (xyz);
  octave_unused_parameter (mirror);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::render_ticktexts (const Matrix& ticks,
                                   const string_vector& ticklabels,
                                   double lim1, double lim2,
                                   double p1, double p2,
                                   int xyz, int ha, int va,
                                   int& wmax, int& hmax)
{
#if defined (HAVE_OPENGL)

  int nticks  = ticks.numel ();
  int nlabels = ticklabels.numel ();

  if (nlabels == 0)
    return;

  for (int i = 0; i < nticks; i++)
    {
      double val = ticks(i);

      if (lim1 <= val && val <= lim2)
        {
          Matrix b;

          std::string label (ticklabels(i % nlabels));
          label.erase (0, label.find_first_not_of (' '));
          label = label.substr (0, label.find_last_not_of (' ')+1);

          // FIXME: As tick text is transparent, shouldn't it be
          //        drawn after axes object, for correct rendering?
          if (xyz == X_AXIS)
            {
              b = render_text (label, val, p1, p2, ha, va);
            }
          else if (xyz == Y_AXIS)
            {
              b = render_text (label, p1, val, p2, ha, va);
            }
          else if (xyz == Z_AXIS)
            {
              b = render_text (label, p1, p2, val, ha, va);
            }

          wmax = std::max (wmax, static_cast<int> (b(2)));
          hmax = std::max (hmax, static_cast<int> (b(3)));
        }
    }

#else

  octave_unused_parameter (ticks);
  octave_unused_parameter (ticklabels);
  octave_unused_parameter (lim1);
  octave_unused_parameter (lim2);
  octave_unused_parameter (p1);
  octave_unused_parameter (p2);
  octave_unused_parameter (xyz);
  octave_unused_parameter (ha);
  octave_unused_parameter (va);
  octave_unused_parameter (wmax);
  octave_unused_parameter (hmax);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_zoom_rect (int x1, int y1, int x2, int y2)
{
#if defined (HAVE_OPENGL)

  m_glfcns.glVertex2d (x1, y1);
  m_glfcns.glVertex2d (x2, y1);
  m_glfcns.glVertex2d (x2, y2);
  m_glfcns.glVertex2d (x1, y2);
  m_glfcns.glVertex2d (x1, y1);

#else

  octave_unused_parameter (x1);
  octave_unused_parameter (x2);
  octave_unused_parameter (y1);
  octave_unused_parameter (y2);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_zoom_box (int width, int height,
                                int x1, int y1, int x2, int y2,
                                const Matrix& overlaycolor,
                                double overlayalpha,
                                const Matrix& bordercolor,
                                double borderalpha, double borderwidth)
{
#if defined (HAVE_OPENGL)

  m_glfcns.glMatrixMode (GL_MODELVIEW);
  m_glfcns.glPushMatrix ();
  m_glfcns.glLoadIdentity ();

  m_glfcns.glMatrixMode (GL_PROJECTION);
  m_glfcns.glPushMatrix ();
  m_glfcns.glLoadIdentity ();
  m_glfcns.glOrtho (0, width, height, 0, 1, -1);

  m_glfcns.glPushAttrib (GL_DEPTH_BUFFER_BIT | GL_CURRENT_BIT);
  m_glfcns.glDisable (GL_DEPTH_TEST);

  m_glfcns.glBegin (GL_POLYGON);
  m_glfcns.glColor4f (overlaycolor(0), overlaycolor(1), overlaycolor(2),
                      overlayalpha);
  draw_zoom_rect (x1, y1, x2, y2);
  m_glfcns.glEnd ();

  m_glfcns.glLineWidth (borderwidth);
  m_glfcns.glBegin (GL_LINE_STRIP);
  m_glfcns.glColor4f (bordercolor(0), bordercolor(1), bordercolor(2),
                      borderalpha);
  draw_zoom_rect (x1, y1, x2, y2);
  m_glfcns.glEnd ();

  m_glfcns.glPopAttrib ();

  m_glfcns.glMatrixMode (GL_MODELVIEW);
  m_glfcns.glPopMatrix ();

  m_glfcns.glMatrixMode (GL_PROJECTION);
  m_glfcns.glPopMatrix ();

#else

  octave_unused_parameter (width);
  octave_unused_parameter (height);
  octave_unused_parameter (x1);
  octave_unused_parameter (x2);
  octave_unused_parameter (y1);
  octave_unused_parameter (y2);
  octave_unused_parameter (overlaycolor);
  octave_unused_parameter (overlayalpha);
  octave_unused_parameter (bordercolor);
  octave_unused_parameter (borderalpha);
  octave_unused_parameter (borderwidth);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

uint8NDArray
opengl_renderer::get_pixels (int width, int height)
{
#if defined (HAVE_OPENGL)

  m_glfcns.glPixelStorei (GL_PACK_ALIGNMENT, 1);
  uint8NDArray pix(dim_vector (3, width, height), 0);

  m_glfcns.glReadPixels(0, 0, width, height, GL_RGB, GL_UNSIGNED_BYTE,
                        pix.fortran_vec ());

  // Permute and flip data
  Array<octave_idx_type> perm (dim_vector (3, 1));
  perm(0) = 2;
  perm(1) = 1;
  perm(2) = 0;

  Array<idx_vector> idx (dim_vector (3, 1));
  idx(0) = idx_vector::make_range (height - 1, -1, height);
  idx(1) = idx_vector::colon;
  idx(2) = idx_vector::colon;

  return pix.permute (perm).index (idx);

#else

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  octave_unused_parameter (width);
  octave_unused_parameter (height);

  panic_impossible ();

#endif
}

void
opengl_renderer::finish (void)
{
#if defined (HAVE_OPENGL)

  m_glfcns.glFinish ();

#else

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::setup_opengl_transformation (const axes::properties& props)
{
#if defined (HAVE_OPENGL)

  // setup OpenGL transformation

  Matrix x_zlim = props.get_transform_zlim ();

  // Expand the distance between the clipping planes symmetrically by
  // an arbitrary factor (see bug #54551).
  const double expansion_fac = 100.0;
  // Also make sure that the distance between the clipping planes
  // differs in single precision (see bug #58956).  This factor is also
  // arbitrary.  Different values (>2) might also work.
  const double single_prec_fac = 10.0;

  double avgZ = x_zlim(0) / 2.0 + x_zlim(1) / 2.0;
  double span
    = std::max (expansion_fac * (x_zlim(1)-x_zlim(0)),
                single_prec_fac * std::abs (avgZ)
                * std::numeric_limits<float>::epsilon ());
  m_xZ1 = avgZ - span;
  m_xZ2 = avgZ + span;

  Matrix x_mat1 = props.get_opengl_matrix_1 ();
  Matrix x_mat2 = props.get_opengl_matrix_2 ();

  m_glfcns.glMatrixMode (GL_MODELVIEW);
  m_glfcns.glLoadIdentity ();
  m_glfcns.glScaled (1, 1, -1);
  m_glfcns.glMultMatrixd (x_mat1.data ());
  m_glfcns.glMatrixMode (GL_PROJECTION);
  m_glfcns.glLoadIdentity ();

  Matrix vp = get_viewport_scaled ();
  m_glfcns.glOrtho (0, vp(2), vp(3), 0, m_xZ1, m_xZ2);
  m_glfcns.glMultMatrixd (x_mat2.data ());
  m_glfcns.glMatrixMode (GL_MODELVIEW);

  m_glfcns.glClear (GL_DEPTH_BUFFER_BIT);

  // store axes transformation data

  m_xform = props.get_transform ();

#else

  octave_unused_parameter (props);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_axes_planes (const axes::properties& props)
{
#if defined (HAVE_OPENGL)

  Matrix axe_color = props.get_color_rgb ();
  if (axe_color.isempty () || ! props.is_visible ())
    return;

  double xPlane = props.get_xPlane ();
  double yPlane = props.get_yPlane ();
  double zPlane = props.get_zPlane ();
  double xPlaneN = props.get_xPlaneN ();
  double yPlaneN = props.get_yPlaneN ();
  double zPlaneN = props.get_zPlaneN ();
  bool is2D = props.get_is2D ();

  // Axes planes
  set_color (axe_color);
  set_polygon_offset (true, 9.0);

  m_glfcns.glBegin (GL_QUADS);

  if (! is2D)
    {
      // X plane
      m_glfcns.glVertex3d (xPlane, yPlaneN, zPlaneN);
      m_glfcns.glVertex3d (xPlane, yPlane, zPlaneN);
      m_glfcns.glVertex3d (xPlane, yPlane, zPlane);
      m_glfcns.glVertex3d (xPlane, yPlaneN, zPlane);

      // Y plane
      m_glfcns.glVertex3d (xPlaneN, yPlane, zPlaneN);
      m_glfcns.glVertex3d (xPlane, yPlane, zPlaneN);
      m_glfcns.glVertex3d (xPlane, yPlane, zPlane);
      m_glfcns.glVertex3d (xPlaneN, yPlane, zPlane);
    }

  // Z plane
  m_glfcns.glVertex3d (xPlaneN, yPlaneN, zPlane);
  m_glfcns.glVertex3d (xPlane, yPlaneN, zPlane);
  m_glfcns.glVertex3d (xPlane, yPlane, zPlane);
  m_glfcns.glVertex3d (xPlaneN, yPlane, zPlane);

  m_glfcns.glEnd ();

  set_polygon_offset (false);

#else

  octave_unused_parameter (props);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_axes_boxes (const axes::properties& props)
{
#if defined (HAVE_OPENGL)

  if (! props.is_visible ())
    return;

  bool xySym = props.get_xySym ();
  bool layer2Dtop = props.get_layer2Dtop ();
  bool is2D = props.get_is2D ();
  bool isXOrigin = props.xaxislocation_is ("origin")
                   && ! props.yscale_is ("log");
  bool isYOrigin = props.yaxislocation_is ("origin")
                   && ! props.xscale_is ("log");
  bool boxFull = (props.get_boxstyle () == "full");
  double linewidth = props.get_linewidth ();
  double xPlane = props.get_xPlane ();
  double yPlane = props.get_yPlane ();
  double zPlane = props.get_zPlane ();
  double xPlaneN = props.get_xPlaneN ();
  double yPlaneN = props.get_yPlaneN ();
  double zPlaneN = props.get_zPlaneN ();
  double xpTick = props.get_xpTick ();
  double ypTick = props.get_ypTick ();
  double zpTick = props.get_zpTick ();
  double xpTickN = props.get_xpTickN ();
  double ypTickN = props.get_ypTickN ();
  double zpTickN = props.get_zpTickN ();

  bool plotyy = (props.has_property ("__plotyy_axes__"));

  // Axes box

  set_linecap ("square");
  set_linestyle ("-", true, linewidth);

  m_glfcns.glBegin (GL_LINES);

  if (layer2Dtop)
    std::swap (zpTick, zpTickN);

  // X box
  Matrix color = props.get_xcolor_rgb ();

  if (! color.isempty ())
    {
      set_color (color);

      if (! isXOrigin || props.is_box() || ! is2D)
        {
          m_glfcns.glVertex3d (xPlaneN, ypTick, zpTick);
          m_glfcns.glVertex3d (xPlane, ypTick, zpTick);
        }

      if (props.is_box ())
        {
          m_glfcns.glVertex3d (xPlaneN, ypTickN, zpTick);
          m_glfcns.glVertex3d (xPlane, ypTickN, zpTick);
          if (! is2D)
            {
              m_glfcns.glVertex3d (xPlaneN, ypTickN, zpTickN);
              m_glfcns.glVertex3d (xPlane, ypTickN, zpTickN);
              if (boxFull)
                {
                  m_glfcns.glVertex3d (xPlaneN, ypTick, zpTickN);
                  m_glfcns.glVertex3d (xPlane, ypTick, zpTickN);
                }
            }
        }
    }

  // Y box
  color = props.get_ycolor_rgb ();

  if (! color.isempty ())
    {
      set_color (color);
      if (! isYOrigin || props.is_box() || ! is2D)
        {
          m_glfcns.glVertex3d (xpTick, yPlaneN, zpTick);
          m_glfcns.glVertex3d (xpTick, yPlane, zpTick);
        }

      if (props.is_box () && ! plotyy)
        {
          m_glfcns.glVertex3d (xpTickN, yPlaneN, zpTick);
          m_glfcns.glVertex3d (xpTickN, yPlane, zpTick);

          if (! is2D)
            {
              m_glfcns.glVertex3d (xpTickN, yPlaneN, zpTickN);
              m_glfcns.glVertex3d (xpTickN, yPlane, zpTickN);
              if (boxFull)
                {
                  m_glfcns.glVertex3d (xpTick, yPlaneN, zpTickN);
                  m_glfcns.glVertex3d (xpTick, yPlane, zpTickN);
                }
            }
        }
    }

  // Z box
  color = props.get_zcolor_rgb ();

  if (! color.isempty () && ! is2D)
    {
      set_color (color);

      if (xySym)
        {
          m_glfcns.glVertex3d (xPlaneN, yPlane, zPlaneN);
          m_glfcns.glVertex3d (xPlaneN, yPlane, zPlane);
        }
      else
        {
          m_glfcns.glVertex3d (xPlane, yPlaneN, zPlaneN);
          m_glfcns.glVertex3d (xPlane, yPlaneN, zPlane);
        }

      if (props.is_box ())
        {
          m_glfcns.glVertex3d (xPlane, yPlane, zPlaneN);
          m_glfcns.glVertex3d (xPlane, yPlane, zPlane);

          if (xySym)
            {
              m_glfcns.glVertex3d (xPlane, yPlaneN, zPlaneN);
              m_glfcns.glVertex3d (xPlane, yPlaneN, zPlane);
            }
          else
            {
              m_glfcns.glVertex3d (xPlaneN, yPlane, zPlaneN);
              m_glfcns.glVertex3d (xPlaneN, yPlane, zPlane);
            }

          if (boxFull)
            {
              m_glfcns.glVertex3d (xPlaneN, yPlaneN, zPlaneN);
              m_glfcns.glVertex3d (xPlaneN, yPlaneN, zPlane);
            }
        }
    }

  m_glfcns.glEnd ();

  set_linestyle ("-");  // Disable LineStipple

#else

  octave_unused_parameter (props);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_axes_x_grid (const axes::properties& props)
{
#if defined (HAVE_OPENGL)

  gh_manager& gh_mgr = __get_gh_manager__ ();

  int xstate = props.get_xstate ();

  if (xstate != AXE_DEPTH_DIR
      && (props.is_visible ()
          || (m_selecting && props.pickableparts_is ("all"))))
    {
      int zstate = props.get_zstate ();
      bool x2Dtop = props.get_x2Dtop ();
      bool layer2Dtop = props.get_layer2Dtop ();
      bool xyzSym = props.get_xyzSym ();
      bool nearhoriz = props.get_nearhoriz ();
      double xticklen = props.get_xticklen ();
      double xtickoffset = props.get_xtickoffset ();
      double fy = props.get_fy ();
      double fz = props.get_fz ();
      double x_min = props.get_x_min ();
      double x_max = props.get_x_max ();
      double y_min = props.get_y_min ();
      double y_max = props.get_y_max ();
      double yPlane = props.get_yPlane ();
      double yPlaneN = props.get_yPlaneN ();
      double ypTick = props.get_ypTick ();
      double ypTickN = props.get_ypTickN ();
      double zPlane = props.get_zPlane ();
      double zPlaneN = props.get_zPlaneN ();
      double zpTick = props.get_zpTick ();
      double zpTickN = props.get_zpTickN ();

      // X ticks and grid properties
      Matrix xticks = m_xform.xscale (props.get_xtick ().matrix_value ());
      Matrix xmticks = m_xform.xscale (props.get_xminortickvalues ().matrix_value ());
      bool do_xminortick = props.is_xminortick () && ! xticks.isempty ();
      string_vector xticklabels = props.get_xticklabel ().string_vector_value ();
      int wmax = 0;
      int hmax = 0;
      bool tick_along_z = nearhoriz || math::isinf (fy);
      double linewidth = props.get_linewidth ();
      std::string gridstyle = props.get_gridlinestyle ();
      std::string minorgridstyle = props.get_minorgridlinestyle ();
      Matrix gridcolor = props.get_gridcolor_rgb ();
      Matrix minorgridcolor = props.get_minorgridcolor_rgb ();
      double gridalpha = props.get_gridalpha ();
      double minorgridalpha = props.get_minorgridalpha ();
      bool do_xgrid = (props.is_xgrid () && (gridstyle != "none"));
      bool do_xminorgrid = (props.is_xminorgrid ()
                            && (minorgridstyle != "none")
                            && ! xticks.isempty ());
      bool is_origin = props.xaxislocation_is ("origin") && props.get_is2D ()
                       && ! props.yscale_is ("log");
      bool is_origin_low = is_origin && (y_min + y_max) < 0;
      bool mirror = props.is_box () && xstate != AXE_ANY_DIR;

      // X grid

      // possibly use axis color for gridcolor & minorgridcolor
      if (props.gridcolormode_is ("auto"))
        if (props.xcolormode_is ("manual") && ! props.xcolor_is ("none"))
          gridcolor = props.get_xcolor_rgb ();

      if (props.minorgridcolormode_is ("auto"))
        if (props.xcolormode_is ("manual") && ! props.xcolor_is ("none"))
          minorgridcolor = props.get_xcolor_rgb ();

      if (gridcolor.isempty ())
        do_xgrid = false;

      if (minorgridcolor.isempty ())
        do_xminorgrid = false;

      // set styles when drawing only minor grid
      if (do_xminorgrid && ! do_xgrid)
        {
          gridstyle = minorgridstyle;
          gridcolor = minorgridcolor;
          gridalpha = minorgridalpha;
          do_xgrid = true;
        }

      // minor grid lines
      if (do_xminorgrid)
        render_grid (linewidth,
                     minorgridstyle, minorgridcolor, minorgridalpha,
                     xmticks, x_min, x_max,
                     yPlane, yPlaneN, layer2Dtop ? zPlaneN : zPlane, zPlaneN,
                     0, (zstate != AXE_DEPTH_DIR));

      // grid lines
      if (do_xgrid)
        render_grid (linewidth,
                     gridstyle, gridcolor, gridalpha,
                     xticks, x_min, x_max,
                     yPlane, yPlaneN, layer2Dtop ? zPlaneN : zPlane, zPlaneN,
                     0, (zstate != AXE_DEPTH_DIR));

      // Skip drawing axis, ticks, and ticklabels when color is "none"
      if (props.xcolor_is ("none"))
        return;

      set_color (props.get_xcolor_rgb ());

      // axis line
      double y_axis_pos = 0.;
      if (is_origin)
        {
          y_axis_pos = math::max (math::min (0., y_max), y_min);
          m_glfcns.glBegin (GL_LINES);
          set_color (props.get_xcolor_rgb ());
          m_glfcns.glVertex3d (x_min, y_axis_pos, zpTick);
          m_glfcns.glVertex3d (x_max, y_axis_pos, zpTick);
          m_glfcns.glEnd ();
        }

      // minor tick marks
      if (do_xminortick)
        {
          if (tick_along_z)
            render_tickmarks (xmticks, x_min, x_max,
                              is_origin ? y_axis_pos : ypTick, ypTick,
                              zpTick, zpTickN, 0., 0.,
                              (is_origin_low ? -1. : 1.) *
                              math::signum (zpTick-zpTickN)*fz*xticklen/2,
                              0, ! is_origin && mirror);
          else
            render_tickmarks (xmticks, x_min, x_max,
                              is_origin ? y_axis_pos : ypTick, ypTickN,
                              zpTick, zpTick, 0.,
                              (is_origin_low ? -1. : 1.) *
                              math::signum (ypTick-ypTickN)*fy*xticklen/2,
                              0., 0, ! is_origin && mirror);
        }

      // tick marks
      if (tick_along_z)
        render_tickmarks (xticks, x_min, x_max,
                          is_origin ? y_axis_pos : ypTick, ypTick,
                          zpTick, zpTickN, 0., 0.,
                          (is_origin_low ? -1. : 1.) *
                          math::signum (zpTick-zpTickN)*fz*xticklen,
                          0, ! is_origin && mirror);
      else
        render_tickmarks (xticks, x_min, x_max,
                          is_origin ? y_axis_pos : ypTick, ypTickN,
                          zpTick, zpTick, 0.,
                          (is_origin_low ? -1. : 1.) *
                          math::signum (ypTick-ypTickN)*fy*xticklen,
                          0., 0, ! is_origin && mirror);

      // tick texts
      if (xticklabels.numel () > 0)
        {
          int halign = (xstate == AXE_HORZ_DIR
                        ? 1
                        : (xyzSym || is_origin_low ? 0 : 2));
          int valign = (xstate == AXE_VERT_DIR
                        ? 1
                        : (x2Dtop || is_origin_low ? 0 : 2));

          if (tick_along_z)
            render_ticktexts (xticks, xticklabels, x_min, x_max,
                              is_origin ? y_axis_pos : ypTick,
                              zpTick +
                              (is_origin_low ? -1. : 1.) *
                              math::signum (zpTick-zpTickN)*fz*xtickoffset,
                              0, halign, valign, wmax, hmax);
          else
            render_ticktexts (xticks, xticklabels, x_min, x_max,
                              (is_origin ? y_axis_pos : ypTick) +
                              (is_origin_low ?  -1. : 1.) *
                              math::signum (ypTick-ypTickN)*fy*xtickoffset,
                              zpTick, 0, halign, valign, wmax, hmax);
        }

      gh_mgr.get_object (props.get_xlabel ()).set ("visible", "on");
    }
  else
    gh_mgr.get_object (props.get_xlabel ()).set ("visible", "off");

#else

  octave_unused_parameter (props);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_axes_y_grid (const axes::properties& props)
{
#if defined (HAVE_OPENGL)

  gh_manager& gh_mgr = __get_gh_manager__ ();

  int ystate = props.get_ystate ();

  if (ystate != AXE_DEPTH_DIR && props.is_visible ()
      && (props.is_visible ()
          || (m_selecting && props.pickableparts_is ("all"))))
    {
      int zstate = props.get_zstate ();
      bool y2Dright = props.get_y2Dright ();
      bool layer2Dtop = props.get_layer2Dtop ();
      bool xyzSym = props.get_xyzSym ();
      bool nearhoriz = props.get_nearhoriz ();
      double yticklen = props.get_yticklen ();
      double ytickoffset = props.get_ytickoffset ();
      double fx = props.get_fx ();
      double fz = props.get_fz ();
      double xPlane = props.get_xPlane ();
      double xPlaneN = props.get_xPlaneN ();
      double xpTick = props.get_xpTick ();
      double xpTickN = props.get_xpTickN ();
      double y_min = props.get_y_min ();
      double y_max = props.get_y_max ();
      double x_min = props.get_x_min ();
      double x_max = props.get_x_max ();
      double zPlane = props.get_zPlane ();
      double zPlaneN = props.get_zPlaneN ();
      double zpTick = props.get_zpTick ();
      double zpTickN = props.get_zpTickN ();

      // Y ticks and grid properties
      Matrix yticks = m_xform.yscale (props.get_ytick ().matrix_value ());
      Matrix ymticks = m_xform.yscale (props.get_yminortickvalues ().matrix_value ());
      bool do_yminortick = props.is_yminortick () && ! yticks.isempty ();
      string_vector yticklabels = props.get_yticklabel ().string_vector_value ();
      int wmax = 0;
      int hmax = 0;
      bool tick_along_z = nearhoriz || math::isinf (fx);
      double linewidth = props.get_linewidth ();
      std::string gridstyle = props.get_gridlinestyle ();
      std::string minorgridstyle = props.get_minorgridlinestyle ();
      Matrix gridcolor = props.get_gridcolor_rgb ();
      Matrix minorgridcolor = props.get_minorgridcolor_rgb ();
      double gridalpha = props.get_gridalpha ();
      double minorgridalpha = props.get_minorgridalpha ();
      bool do_ygrid = (props.is_ygrid () && (gridstyle != "none"));
      bool do_yminorgrid = (props.is_yminorgrid ()
                            && (minorgridstyle != "none")
                            && ! yticks.isempty ());
      bool is_origin = props.yaxislocation_is ("origin") && props.get_is2D ()
                       && ! props.xscale_is ("log");
      bool is_origin_low = is_origin && (x_min + x_max) < 0;
      bool mirror = props.is_box () && ystate != AXE_ANY_DIR
                    && (! props.has_property ("__plotyy_axes__"));

      // Y grid

      // possibly use axis color for gridcolor & minorgridcolor
      if (props.gridcolormode_is ("auto"))
        if (props.ycolormode_is ("manual") && ! props.ycolor_is ("none"))
          gridcolor = props.get_ycolor_rgb ();

      if (props.minorgridcolormode_is ("auto"))
        if (props.ycolormode_is ("manual") && ! props.ycolor_is ("none"))
          minorgridcolor = props.get_ycolor_rgb ();

      if (gridcolor.isempty ())
        do_ygrid = false;

      if (minorgridcolor.isempty ())
        do_yminorgrid = false;

      // set styles when drawing only minor grid
      if (do_yminorgrid && ! do_ygrid)
        {
          gridstyle = minorgridstyle;
          gridcolor = minorgridcolor;
          gridalpha = minorgridalpha;
          do_ygrid = true;
        }

      // minor grid lines
      if (do_yminorgrid)
        render_grid (linewidth,
                     minorgridstyle, minorgridcolor, minorgridalpha,
                     ymticks, y_min, y_max,
                     xPlane, xPlaneN, layer2Dtop ? zPlaneN : zPlane, zPlaneN,
                     1, (zstate != AXE_DEPTH_DIR));

      // grid lines
      if (do_ygrid)
        render_grid (linewidth,
                     gridstyle, gridcolor, gridalpha,
                     yticks, y_min, y_max,
                     xPlane, xPlaneN, layer2Dtop ? zPlaneN : zPlane, zPlaneN,
                     1, (zstate != AXE_DEPTH_DIR));

      // Skip drawing axis, ticks, and ticklabels when color is "none"
      if (props.ycolor_is ("none"))
        return;

      set_color (props.get_ycolor_rgb ());

      // axis line
      double x_axis_pos = 0.;
      if (is_origin)
        {
          x_axis_pos = math::max (math::min (0., x_max), x_min);
          m_glfcns.glBegin (GL_LINES);
          set_color (props.get_ycolor_rgb ());
          m_glfcns.glVertex3d (x_axis_pos, y_min, zpTick);
          m_glfcns.glVertex3d (x_axis_pos, y_max, zpTick);
          m_glfcns.glEnd ();
        }

      // minor tick marks
      if (do_yminortick)
        {
          if (tick_along_z)
            render_tickmarks (ymticks, y_min, y_max,
                              is_origin ? x_axis_pos : xpTick, xpTick,
                              zpTick, zpTickN, 0., 0.,
                              (is_origin_low ? -1. : 1.) *
                              math::signum (zpTick-zpTickN)*fz*yticklen/2,
                              1, ! is_origin && mirror);
          else
            render_tickmarks (ymticks, y_min, y_max,
                              is_origin ? x_axis_pos : xpTick, xpTickN,
                              zpTick, zpTick,
                              (is_origin_low ? -1. : 1.) *
                              math::signum (xpTick-xpTickN)*fx*yticklen/2,
                              0., 0., 1, ! is_origin && mirror);
        }

      // tick marks
      if (tick_along_z)
        render_tickmarks (yticks, y_min, y_max,
                          is_origin ? x_axis_pos : xpTick, xpTick,
                          zpTick, zpTickN, 0., 0.,
                          (is_origin_low ? -1. : 1.) *
                          math::signum (zpTick-zpTickN)*fz*yticklen,
                          1, ! is_origin && mirror);
      else
        render_tickmarks (yticks, y_min, y_max,
                          is_origin ? x_axis_pos : xpTick, xpTickN,
                          zpTick, zpTick,
                          (is_origin_low ? -1. : 1.) *
                          math::signum (xPlaneN-xPlane)*fx*yticklen,
                          0., 0., 1, ! is_origin && mirror);

      // tick texts
      if (yticklabels.numel () > 0)
        {
          int halign = (ystate == AXE_HORZ_DIR
                        ? 1
                        : (! xyzSym || y2Dright || is_origin_low ? 0 : 2));
          int valign = (ystate == AXE_VERT_DIR
                        ? 1
                        : (is_origin_low ? 0 : 2));

          if (tick_along_z)
            render_ticktexts (yticks, yticklabels, y_min, y_max,
                              is_origin ? x_axis_pos : xpTick,
                              zpTick +
                              (is_origin_low ? -1. : 1.) *
                              math::signum (zpTick-zpTickN)*fz*ytickoffset,
                              1, halign, valign, wmax, hmax);
          else
            render_ticktexts (yticks, yticklabels, y_min, y_max,
                              (is_origin ? x_axis_pos : xpTick) +
                              (is_origin_low ?  -1. : 1.) *
                              math::signum (xpTick-xpTickN)*fx*ytickoffset,
                              zpTick, 1, halign, valign, wmax, hmax);
        }

      gh_mgr.get_object (props.get_ylabel ()).set ("visible", "on");
    }
  else
    gh_mgr.get_object (props.get_ylabel ()).set ("visible", "off");

#else

  octave_unused_parameter (props);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_axes_z_grid (const axes::properties& props)
{
  gh_manager& gh_mgr = __get_gh_manager__ ();

  int zstate = props.get_zstate ();

  if (zstate != AXE_DEPTH_DIR && props.is_visible ()
      && (props.is_visible ()
          || (m_selecting && props.pickableparts_is ("all"))))
    {
      bool xySym = props.get_xySym ();
      bool zSign = props.get_zSign ();
      double zticklen = props.get_zticklen ();
      double ztickoffset = props.get_ztickoffset ();
      double fx = props.get_fx ();
      double fy = props.get_fy ();
      double xPlane = props.get_xPlane ();
      double xPlaneN = props.get_xPlaneN ();
      double yPlane = props.get_yPlane ();
      double yPlaneN = props.get_yPlaneN ();
      double z_min = props.get_z_min ();
      double z_max = props.get_z_max ();

      // Z ticks and grid properties
      Matrix zticks = m_xform.zscale (props.get_ztick ().matrix_value ());
      Matrix zmticks = m_xform.zscale (props.get_zminortickvalues ().matrix_value ());
      bool do_zminortick = props.is_zminortick () && ! zticks.isempty ();
      string_vector zticklabels = props.get_zticklabel ().string_vector_value ();
      int wmax = 0;
      int hmax = 0;
      double linewidth = props.get_linewidth ();
      std::string gridstyle = props.get_gridlinestyle ();
      std::string minorgridstyle = props.get_minorgridlinestyle ();
      Matrix gridcolor = props.get_gridcolor_rgb ();
      Matrix minorgridcolor = props.get_minorgridcolor_rgb ();
      double gridalpha = props.get_gridalpha ();
      double minorgridalpha = props.get_minorgridalpha ();
      bool do_zgrid = (props.is_zgrid () && (gridstyle != "none"));
      bool do_zminorgrid = (props.is_zminorgrid ()
                            && (minorgridstyle != "none")
                            && ! zticks.isempty ());
      bool mirror = props.is_box () && zstate != AXE_ANY_DIR;

      // Z grid

      // possibly use axis color for gridcolor & minorgridcolor
      if (props.gridcolormode_is ("auto"))
        if (props.zcolormode_is ("manual") && ! props.zcolor_is ("none"))
          gridcolor = props.get_zcolor_rgb ();

      if (props.minorgridcolormode_is ("auto"))
        if (props.zcolormode_is ("manual") && ! props.zcolor_is ("none"))
          minorgridcolor = props.get_zcolor_rgb ();

      if (gridcolor.isempty ())
        do_zgrid = false;

      if (minorgridcolor.isempty ())
        do_zminorgrid = false;

      // set styles when drawing only minor grid
      if (do_zminorgrid && ! do_zgrid)
        {
          gridstyle = minorgridstyle;
          gridcolor = minorgridcolor;
          gridalpha = minorgridalpha;
          do_zgrid = true;
        }

      // minor grid lines
      if (do_zminorgrid)
        render_grid (linewidth,
                     minorgridstyle, minorgridcolor, minorgridalpha,
                     zmticks, z_min, z_max,
                     xPlane, xPlaneN, yPlane, yPlaneN, 2, true);

      // grid lines
      if (do_zgrid)
        render_grid (linewidth,
                     gridstyle, gridcolor, gridalpha,
                     zticks, z_min, z_max,
                     xPlane, xPlaneN, yPlane, yPlaneN, 2, true);

      // Skip drawing axis, ticks, and ticklabels when color is "none"
      if (props.zcolor_is ("none"))
        return;

      set_color (props.get_zcolor_rgb ());

      // minor tick marks
      if (do_zminortick)
        {
          if (xySym)
            {
              if (math::isinf (fy))
                render_tickmarks (zmticks, z_min, z_max, xPlaneN, xPlane,
                                  yPlane, yPlane,
                                  math::signum (xPlaneN-xPlane)*fx*zticklen/2,
                                  0., 0., 2, mirror);
              else
                render_tickmarks (zmticks, z_min, z_max, xPlaneN, xPlaneN,
                                  yPlane, yPlane, 0.,
                                  math::signum (yPlane-yPlaneN)*fy*zticklen/2,
                                  0., 2, false);
            }
          else
            {
              if (math::isinf (fx))
                render_tickmarks (zmticks, z_min, z_max, xPlane, xPlane,
                                  yPlaneN, yPlane, 0.,
                                  math::signum (yPlaneN-yPlane)*fy*zticklen/2,
                                  0., 2, mirror);
              else
                render_tickmarks (zmticks, z_min, z_max, xPlane, xPlane,
                                  yPlaneN, yPlaneN,
                                  math::signum (xPlane-xPlaneN)*fx*zticklen/2,
                                  0., 0., 2, false);
            }
        }

      // tick marks
      if (xySym)
        {
          if (math::isinf (fy))
            render_tickmarks (zticks, z_min, z_max, xPlaneN, xPlane,
                              yPlane, yPlane,
                              math::signum (xPlaneN-xPlane)*fx*zticklen,
                              0., 0., 2, mirror);
          else
            render_tickmarks (zticks, z_min, z_max, xPlaneN, xPlaneN,
                              yPlane, yPlane, 0.,
                              math::signum (yPlane-yPlaneN)*fy*zticklen,
                              0., 2, false);
        }
      else
        {
          if (math::isinf (fx))
            render_tickmarks (zticks, z_min, z_max, xPlaneN, xPlane,
                              yPlaneN, yPlane, 0.,
                              math::signum (yPlaneN-yPlane)*fy*zticklen,
                              0., 2, mirror);
          else
            render_tickmarks (zticks, z_min, z_max, xPlane, xPlane,
                              yPlaneN, yPlane,
                              math::signum (xPlane-xPlaneN)*fx*zticklen,
                              0., 0., 2, false);
        }

      // tick texts
      if (zticklabels.numel () > 0)
        {
          int halign = 2;
          int valign = (zstate == AXE_VERT_DIR ? 1 : (zSign ? 3 : 2));

          if (xySym)
            {
              if (math::isinf (fy))
                render_ticktexts (zticks, zticklabels, z_min, z_max,
                                  xPlaneN + math::signum (xPlaneN-xPlane)*fx*ztickoffset,
                                  yPlane, 2, halign, valign, wmax, hmax);
              else
                render_ticktexts (zticks, zticklabels, z_min, z_max, xPlaneN,
                                  yPlane + math::signum (yPlane-yPlaneN)*fy*ztickoffset,
                                  2, halign, valign, wmax, hmax);
            }
          else
            {
              if (math::isinf (fx))
                render_ticktexts (zticks, zticklabels, z_min, z_max, xPlane,
                                  yPlaneN + math::signum (yPlaneN-yPlane)*fy*ztickoffset,
                                  2, halign, valign, wmax, hmax);
              else
                render_ticktexts (zticks, zticklabels, z_min, z_max,
                                  xPlane + math::signum (xPlane-xPlaneN)*fx*ztickoffset,
                                  yPlaneN, 2, halign, valign, wmax, hmax);
            }
        }

      gh_mgr.get_object (props.get_zlabel ()).set ("visible", "on");
    }
  else
    gh_mgr.get_object (props.get_zlabel ()).set ("visible", "off");
}

void
opengl_renderer::draw_axes_grids (const axes::properties& props)
{
#if defined (HAVE_OPENGL)
  // Disable line smoothing for axes
  GLboolean antialias;

  m_glfcns.glGetBooleanv (GL_LINE_SMOOTH, &antialias);

  if (antialias == GL_TRUE)
    m_glfcns.glDisable (GL_LINE_SMOOTH);

  set_linecap ("butt");
  set_linewidth (props.get_linewidth ());
  set_font (props);
  set_interpreter (props.get_ticklabelinterpreter ());

  draw_axes_x_grid (props);
  draw_axes_y_grid (props);
  draw_axes_z_grid (props);

  if (antialias == GL_TRUE)
    m_glfcns.glEnable (GL_LINE_SMOOTH);
#else

  octave_unused_parameter (props);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_all_lights (const base_properties& props,
                                  std::list<graphics_object>& obj_list)
{
#if defined (HAVE_OPENGL)
  gh_manager& gh_mgr = __get_gh_manager__ ();

  Matrix children = props.get_all_children ();

  for (octave_idx_type i = children.numel () - 1; i >= 0; i--)
    {
      graphics_object go = gh_mgr.get_object (children(i));

      base_properties p = go.get_properties ();

      if (p.is_visible ()
          || (m_selecting && p.pickableparts_is ("all")))
        {
          if (go.isa ("light") && ! m_selecting)
            {
              if (m_current_light-GL_LIGHT0 < m_max_lights)
                {
                  set_clipping (p.is_clipping ());
                  draw (go);
                  m_current_light++;
                }
            }
          else if (go.isa ("hggroup")
                   && ! (m_selecting && p.pickableparts_is ("none")))
            draw_all_lights (go.get_properties (), obj_list);
          else if (! (m_selecting && p.pickableparts_is ("none")))
            obj_list.push_back (go);
        }
    }
#else

  octave_unused_parameter (props);
  octave_unused_parameter (obj_list);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_axes_children (const axes::properties& props)
{
#if defined (HAVE_OPENGL)
  // list for non-light child objects
  std::list<graphics_object> obj_list;
  std::list<graphics_object>::iterator it;

  // 1st pass: draw light objects

  // FIXME: max_lights only needs to be set once.
  // It would be better if this could be in the constructor for gl_renderer
  // but this seems to lead to calls of OpenGL functions before the context
  // is actually initialized.  See bug #48669.
  // Check actual maximum number of lights possible
  init_maxlights ();

  // Start with the last element of the array of child objects to
  // display them in the order they were added to the array.

  if (props.get_num_lights () > m_max_lights)
    warning_with_id ("Octave:max-lights-exceeded",
                     "light: Maximum number of lights (%d) in these axes is "
                     "exceeded.", m_max_lights);

  m_current_light = GL_LIGHT0;
  draw_all_lights (props, obj_list);

  // disable other OpenGL lights
  for (unsigned int i = props.get_num_lights (); i < m_max_lights; i++)
    m_glfcns.glDisable (GL_LIGHT0 + i);

  // save camera position and set ambient light color before drawing
  // other objects
  m_view_vector = props.get_cameraposition ().matrix_value ();

  float cb[4] = { 1.0, 1.0, 1.0, 1.0 };
  ColumnVector ambient_color = props.get_ambientlightcolor_rgb ();
  for (int i = 0; i < 3; i++)
    cb[i] = ambient_color(i);
  m_glfcns.glLightfv (GL_LIGHT0, GL_AMBIENT, cb);

  // 2nd pass: draw other objects (with units set to "data")

  it = obj_list.begin ();
  while (it != obj_list.end ())
    {
      graphics_object go = (*it);

      // FIXME: check whether object has "units" property and it is set
      // to "data"
      if (! go.isa ("text") || go.get ("units").string_value () == "data")
        {
          set_clipping (go.get_properties ().is_clipping ());
          draw (go);

          it = obj_list.erase (it);
        }
      else
        it++;
    }

  // 3rd pass: draw remaining objects

  m_glfcns.glDisable (GL_DEPTH_TEST);

  for (it = obj_list.begin (); it != obj_list.end (); it++)
    {
      graphics_object go = (*it);

      set_clipping (go.get_properties ().is_clipping ());
      draw (go);
    }

  set_clipping (false);

  // FIXME: finalize rendering (transparency processing)
  // FIXME: draw zoom box, if needed

#else

  octave_unused_parameter (props);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_axes (const axes::properties& props)
{
#if defined (HAVE_OPENGL)

  // Legends are not drawn when "visible" is "off".
  if (! props.is_visible () && props.get_tag () == "legend")
    return;

  // Don't draw the axes and its children if we are in selection and
  // pickable parts is "none".
  if (m_selecting && props.pickableparts_is ("none"))
    return;

  static double floatmax = std::numeric_limits<float>::max ();

  double x_min = props.get_x_min ();
  double x_max = props.get_x_max ();
  double y_min = props.get_y_min ();
  double y_max = props.get_y_max ();
  double z_min = props.get_z_min ();
  double z_max = props.get_z_max ();

  if (x_max > floatmax || y_max > floatmax || z_max > floatmax
      || x_min < -floatmax || y_min < -floatmax || z_min < -floatmax)
    {
      warning ("opengl_renderer: data values greater than float capacity.  (1) Scale data, or (2) Use gnuplot");
      return;
    }

  setup_opengl_transformation (props);

  // For 2D axes with only 2D primitives, draw from back to front without
  // depth sorting
  bool is2D = props.get_is2D (true);
  if (is2D)
    m_glfcns.glDisable (GL_DEPTH_TEST);
  else
    m_glfcns.glEnable (GL_DEPTH_TEST);

  draw_axes_planes (props);

  if (! is2D || props.layer_is ("bottom"))
    {
      draw_axes_grids (props);
      if (props.get_tag () != "legend" || props.get_box () != "off")
        draw_axes_boxes (props);
    }

  set_clipbox (x_min, x_max, y_min, y_max, z_min, z_max);

  draw_axes_children (props);

  if (is2D && props.layer_is ("top"))
    {
      draw_axes_grids (props);
      if (props.get_tag () != "legend" || props.get_box () != "off")
        draw_axes_boxes (props);
    }

#else

  octave_unused_parameter (props);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_line (const line::properties& props)
{
#if defined (HAVE_OPENGL)

  bool draw_all = m_selecting && props.pickableparts_is ("all");

  Matrix x = m_xform.xscale (props.get_xdata ().matrix_value ());
  Matrix y = m_xform.yscale (props.get_ydata ().matrix_value ());
  Matrix z = m_xform.zscale (props.get_zdata ().matrix_value ());

  bool has_z = (z.numel () > 0);
  int n = static_cast<int> (std::min (std::min (x.numel (), y.numel ()),
                                      (has_z ? z.numel ()
                                       : std::numeric_limits<int>::max ())));
  uint8_t clip_mask = (props.is_clipping () ? 0x7F : 0x40);
  uint8_t clip_ok = 0x40;

  std::vector<uint8_t> clip (n);

  if (has_z)
    for (int i = 0; i < n; i++)
      clip[i] = (clip_code (x(i), y(i), z(i)) & clip_mask);
  else
    {
      double z_mid = (m_zmin+m_zmax)/2;

      for (int i = 0; i < n; i++)
        clip[i] = (clip_code (x(i), y(i), z_mid) & clip_mask);
    }

  if (! props.linestyle_is ("none") && ! props.color_is ("none"))
    {
      set_color (props.get_color_rgb ());
      set_linestyle (props.get_linestyle (), false, props.get_linewidth ());
      set_linewidth (props.get_linewidth ());
      set_linecap ("butt");
      set_linejoin (props.get_linejoin ());

      if (has_z)
        {
          bool flag = false;

          for (int i = 1; i < n; i++)
            {
              if ((clip[i-1] & clip[i]) == clip_ok)
                {
                  if (! flag)
                    {
                      flag = true;
                      m_glfcns.glBegin (GL_LINE_STRIP);
                      m_glfcns.glVertex3d (x(i-1), y(i-1), z(i-1));
                    }
                  m_glfcns.glVertex3d (x(i), y(i), z(i));
                }
              else if (flag)
                {
                  flag = false;
                  m_glfcns.glEnd ();
                }
            }

          if (flag)
            m_glfcns.glEnd ();
        }
      else
        {
          bool flag = false;

          for (int i = 1; i < n; i++)
            {
              if ((clip[i-1] & clip[i]) == clip_ok)
                {
                  if (! flag)
                    {
                      flag = true;
                      m_glfcns.glBegin (GL_LINE_STRIP);
                      m_glfcns.glVertex2d (x(i-1), y(i-1));
                    }
                  m_glfcns.glVertex2d (x(i), y(i));
                }
              else if (flag)
                {
                  flag = false;
                  m_glfcns.glEnd ();
                }
            }

          if (flag)
            m_glfcns.glEnd ();
        }

      set_linewidth (0.5f);
      set_linestyle ("-");
    }

  set_clipping (false);

  if (! props.marker_is ("none")
      && ! (props.markeredgecolor_is ("none")
            && props.markerfacecolor_is ("none")))
    {
      Matrix lc, fc;

      if (draw_all)
        lc = Matrix (1, 3, 0.0);
      else if (props.markeredgecolor_is ("auto"))
        lc = props.get_color_rgb ();
      else if (! props.markeredgecolor_is ("none"))
        lc = props.get_markeredgecolor_rgb ();

      if (draw_all)
        fc = Matrix (1, 3, 0.0);
      if (props.markerfacecolor_is ("auto"))
        fc = props.get_color_rgb ();
      else if (! props.markerfacecolor_is ("none"))
        fc = props.get_markerfacecolor_rgb ();

      init_marker (props.get_marker (), props.get_markersize (),
                   props.get_linewidth ());

      for (int i = 0; i < n; i++)
        {
          if (clip[i] == clip_ok)
            draw_marker (x(i), y(i),
                         has_z ? z(i) : 0.0,
                         lc, fc);
        }

      end_marker ();
    }

  set_clipping (props.is_clipping ());

#else

  octave_unused_parameter (props);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_surface (const surface::properties& props)
{
#if defined (HAVE_OPENGL)

  bool draw_all = m_selecting && props.pickableparts_is ("all");

  const Matrix x = m_xform.xscale (props.get_xdata ().matrix_value ());
  const Matrix y = m_xform.yscale (props.get_ydata ().matrix_value ());
  const Matrix z = m_xform.zscale (props.get_zdata ().matrix_value ());

  int zr = z.rows ();
  int zc = z.columns ();

  NDArray c;
  const NDArray vn = props.get_vertexnormals ().array_value ();
  dim_vector vn_dims = vn.dims ();
  bool has_vertex_normals = (vn_dims(0) == zr && vn_dims(1) == zc
                             && vn_dims(2) == 3);
  const NDArray fn = props.get_facenormals ().array_value ();
  dim_vector fn_dims = fn.dims ();
  bool has_face_normals = (fn_dims(0) == zr - 1 && fn_dims(1) == zc - 1
                           && fn_dims(2) == 3);

  // FIXME: handle transparency
  Matrix a;

  int fc_mode = (props.facecolor_is_rgb () ? 0 :
                 (props.facecolor_is ("flat") ? 1 :
                  (props.facecolor_is ("interp") ? 2 :
                   (props.facecolor_is ("texturemap") ? 3 : -1))));
  int fl_mode = (props.facelighting_is ("none") ? 0 :
                 (props.facelighting_is ("flat") ?
                  (has_face_normals ? 1 : 0) :
                  (has_vertex_normals ? 2 : 0)));
  int fa_mode = (props.facealpha_is_double () ? 0 :
                 (props.facealpha_is ("flat") ? 1 : 2));
  int ec_mode = (props.edgecolor_is_rgb () ? 0 :
                 (props.edgecolor_is ("flat") ? 1 :
                  (props.edgecolor_is ("interp") ? 2 : -1)));
  int el_mode = (props.edgelighting_is ("none") ? 0 :
                 (props.edgelighting_is ("flat") ?
                  (has_face_normals ? 1 : 0) :
                  (has_vertex_normals ? 2 : 0)));
  int ea_mode = (props.edgealpha_is_double () ? 0 :
                 (props.edgealpha_is ("flat") ? 1 : 2));
  int bfl_mode = (props.backfacelighting_is ("lit") ? 0 :
                  (props.backfacelighting_is ("reverselit") ? 1 : 2));
  bool do_lighting = props.get_do_lighting ();

  Matrix fcolor = (fc_mode == TEXTURE ? Matrix (1, 3, 1.0)
                   : props.get_facecolor_rgb ());
  Matrix ecolor = props.get_edgecolor_rgb ();
  double fa = 1.0;

  float as = props.get_ambientstrength ();
  float ds = props.get_diffusestrength ();
  float ss = props.get_specularstrength ();
  float se = props.get_specularexponent () * 5; // to fit Matlab
  float scr = props.get_specularcolorreflectance ();
  float cb[4] = { 0.0, 0.0, 0.0, 1.0 };

  opengl_texture tex (m_glfcns);

  int i1, i2, j1, j2;
  bool x_mat = (x.rows () == z.rows ());
  bool y_mat = (y.columns () == z.columns ());

  i1 = i2 = j1 = j2 = 0;

  if ((fc_mode > 0 && fc_mode < 3) || ec_mode > 0)
    c = props.get_color_data ().array_value ();

  boolMatrix clip (z.dims (), false);

  for (int i = 0; i < zr; i++)
    {
      if (x_mat)
        i1 = i;

      for (int j = 0; j < zc; j++)
        {
          if (y_mat)
            j1 = j;

          clip(i, j) = is_nan_or_inf (x(i1, j), y(i, j1), z(i, j));
        }
    }

  if (fa_mode > 0 || ea_mode > 0)
    {
      // FIXME: implement alphadata conversion
      //a = props.get_alpha_data ();
    }

  if (fl_mode > 0 || el_mode > 0)
    m_glfcns.glMaterialf (LIGHT_MODE, GL_SHININESS, se);

  // FIXME: good candidate for caching,
  //        transferring pixel data to OpenGL is time consuming.
  if (fc_mode == TEXTURE)
    tex = opengl_texture::create (m_glfcns, props.get_color_data ());

  if (draw_all || ! props.facecolor_is ("none"))
    {
      if (fa_mode == 0)
        {
          fa = props.get_facealpha_double ();
          cb[3] = fa;
          if (fc_mode == UNIFORM || fc_mode == TEXTURE)
            {
              m_glfcns.glColor4d (fcolor(0), fcolor(1), fcolor(2), fa);
              if (fl_mode > 0)
                {
                  for (int i = 0; i < 3; i++)
                    cb[i] = as * fcolor(i);
                  m_glfcns.glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                  for (int i = 0; i < 3; i++)
                    cb[i] = ds * fcolor(i);
                  m_glfcns.glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);

                  for (int i = 0; i < 3; i++)
                    cb[i] = ss * (scr + (1-scr) * fcolor(i));
                  m_glfcns.glMaterialfv (LIGHT_MODE, GL_SPECULAR, cb);
                }
            }

          if ((fl_mode > 0) && do_lighting)
            m_glfcns.glEnable (GL_LIGHTING);
          m_glfcns.glShadeModel ((fc_mode == INTERP || fl_mode == GOURAUD)
                                 ? GL_SMOOTH : GL_FLAT);
          set_polygon_offset (true, 1.0);
          if (fc_mode == TEXTURE)
            m_glfcns.glEnable (GL_TEXTURE_2D);

          for (int i = 1; i < zc; i++)
            {
              if (y_mat)
                {
                  i1 = i-1;
                  i2 = i;
                }

              for (int j = 1; j < zr; j++)
                {

                  if (clip(j-1, i-1) || clip(j, i-1)
                      || clip(j-1, i) || clip(j, i))
                    continue;

                  if (fc_mode == FLAT)
                    {
                      // "flat" only needs color at lower-left vertex
                      if (! math::isfinite (c(j-1, i-1)))
                        continue;
                    }
                  else if (fc_mode == INTERP)
                    {
                      // "interp" needs valid color at all 4 vertices
                      if (! (math::isfinite (c(j-1, i-1))
                             && math::isfinite (c(j, i-1))
                             && math::isfinite (c(j-1, i))
                             && math::isfinite (c(j, i))))
                        continue;
                    }

                  if (x_mat)
                    {
                      j1 = j-1;
                      j2 = j;
                    }

                  m_glfcns.glBegin (GL_QUADS);

                  // Vertex 1
                  if (fc_mode == TEXTURE)
                    tex.tex_coord (double (i-1) / (zc-1),
                                   double (j-1) / (zr-1));
                  else if (fc_mode > 0)
                    {
                      // FIXME: is there a smarter way to do this?
                      for (int k = 0; k < 3; k++)
                        cb[k] = c(j-1, i-1, k);
                      m_glfcns.glColor4fv (cb);

                      if (fl_mode > 0)
                        {
                          for (int k = 0; k < 3; k++)
                            cb[k] *= as;
                          m_glfcns.glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                          for (int k = 0; k < 3; k++)
                            cb[k] = ds * c(j-1, i-1, k);
                          m_glfcns.glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);

                          for (int k = 0; k < 3; k++)
                            cb[k] = ss * (scr + (1-scr) * c(j-1, i-1, k));
                          m_glfcns.glMaterialfv (LIGHT_MODE, GL_SPECULAR, cb);
                        }
                    }
                  if (fl_mode > 0)
                    set_normal (bfl_mode, (fl_mode == GOURAUD ? vn : fn),
                                j-1, i-1);

                  m_glfcns.glVertex3d (x(j1, i-1), y(j-1, i1), z(j-1, i-1));

                  // Vertex 2
                  if (fc_mode == TEXTURE)
                    tex.tex_coord (double (i) / (zc-1),
                                   double (j-1) / (zr-1));
                  else if (fc_mode == INTERP)
                    {
                      for (int k = 0; k < 3; k++)
                        cb[k] = c(j-1, i, k);
                      m_glfcns.glColor4fv (cb);

                      if (fl_mode > 0)
                        {
                          for (int k = 0; k < 3; k++)
                            cb[k] *= as;
                          m_glfcns.glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                          for (int k = 0; k < 3; k++)
                            cb[k] = ds * c(j-1, i, k);
                          m_glfcns.glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);

                          for (int k = 0; k < 3; k++)
                            cb[k] = ss * (scr + (1-scr) * c(j-1, i, k));
                          m_glfcns.glMaterialfv (LIGHT_MODE, GL_SPECULAR, cb);
                        }
                    }

                  if (fl_mode == GOURAUD)
                    set_normal (bfl_mode, vn, j-1, i);

                  m_glfcns.glVertex3d (x(j1, i), y(j-1, i2), z(j-1, i));

                  // Vertex 3
                  if (fc_mode == TEXTURE)
                    tex.tex_coord (double (i) / (zc-1), double (j) / (zr-1));
                  else if (fc_mode == INTERP)
                    {
                      for (int k = 0; k < 3; k++)
                        cb[k] = c(j, i, k);
                      m_glfcns.glColor4fv (cb);

                      if (fl_mode > 0)
                        {
                          for (int k = 0; k < 3; k++)
                            cb[k] *= as;
                          m_glfcns.glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                          for (int k = 0; k < 3; k++)
                            cb[k] = ds * c(j, i, k);
                          m_glfcns.glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);

                          for (int k = 0; k < 3; k++)
                            cb[k] = ss * (scr + (1-scr) * c(j, i, k));
                          m_glfcns.glMaterialfv (LIGHT_MODE, GL_SPECULAR, cb);
                        }
                    }
                  if (fl_mode == GOURAUD)
                    set_normal (bfl_mode, vn, j, i);

                  m_glfcns.glVertex3d (x(j2, i), y(j, i2), z(j, i));

                  // Vertex 4
                  if (fc_mode == TEXTURE)
                    tex.tex_coord (double (i-1) / (zc-1),
                                   double (j) / (zr-1));
                  else if (fc_mode == INTERP)
                    {
                      for (int k = 0; k < 3; k++)
                        cb[k] = c(j, i-1, k);
                      m_glfcns.glColor4fv (cb);

                      if (fl_mode > 0)
                        {
                          for (int k = 0; k < 3; k++)
                            cb[k] *= as;
                          m_glfcns.glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                          for (int k = 0; k < 3; k++)
                            cb[k] = ds * c(j, i-1, k);
                          m_glfcns.glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);

                          for (int k = 0; k < 3; k++)
                            cb[k] = ss * (scr + (1-scr) * c(j, i-1, k));
                          m_glfcns.glMaterialfv (LIGHT_MODE, GL_SPECULAR, cb);
                        }
                    }
                  if (fl_mode == GOURAUD)
                    set_normal (bfl_mode, vn, j, i-1);

                  m_glfcns.glVertex3d (x(j2, i-1), y(j, i1), z(j, i-1));

                  m_glfcns.glEnd ();
                }
            }

          set_polygon_offset (false);
          if (fc_mode == TEXTURE)
            m_glfcns.glDisable (GL_TEXTURE_2D);

          if ((fl_mode > 0) && do_lighting)
            m_glfcns.glDisable (GL_LIGHTING);
        }
      else
        {
          // FIXME: implement flat, interp and texturemap transparency
        }
    }

  if (! props.edgecolor_is ("none") && ! props.linestyle_is ("none"))
    {
      if (props.get_edgealpha_double () == 1)
        {
          cb[3] = 1.0; // edgealpha isn't implemented yet
          if (ec_mode == UNIFORM)
            {
              m_glfcns.glColor3dv (ecolor.data ());
              if (el_mode > 0)
                {
                  for (int i = 0; i < 3; i++)
                    cb[i] = as * ecolor(i);
                  m_glfcns.glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                  for (int i = 0; i < 3; i++)
                    cb[i] = ds * ecolor(i);
                  m_glfcns.glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);

                  for (int i = 0; i < 3; i++)
                    cb[i] = ss * (scr + (1-scr) * ecolor(i));
                  m_glfcns.glMaterialfv (LIGHT_MODE, GL_SPECULAR, cb);
                }
            }

          if ((el_mode > 0) && do_lighting)
            m_glfcns.glEnable (GL_LIGHTING);
          m_glfcns.glShadeModel ((ec_mode == INTERP || el_mode == GOURAUD)
                                 ? GL_SMOOTH : GL_FLAT);

          set_linestyle (props.get_linestyle (), false,
                         props.get_linewidth ());
          set_linewidth (props.get_linewidth ());
          set_linecap ("butt");
          set_linejoin ("miter");

          // Mesh along Y-axis

          if (props.meshstyle_is ("both") || props.meshstyle_is ("column"))
            {
              for (int i = 0; i < zc; i++)
                {
                  if (y_mat)
                    {
                      i1 = i-1;
                      i2 = i;
                    }

                  for (int j = 1; j < zr; j++)
                    {
                      if (clip(j-1, i) || clip(j, i))
                        continue;

                      if (ec_mode == FLAT)
                        {
                          // "flat" only needs color at lower-left vertex
                          if (! math::isfinite (c(j-1, i)))
                            continue;
                        }
                      else if (ec_mode == INTERP)
                        {
                          // "interp" needs valid color at both vertices
                          if (! (math::isfinite (c(j-1, i))
                                 && math::isfinite (c(j, i))))
                            continue;
                        }

                      if (x_mat)
                        {
                          j1 = j-1;
                          j2 = j;
                        }

                      m_glfcns.glBegin (GL_LINES);

                      // Vertex 1
                      if (ec_mode > 0)
                        {
                          for (int k = 0; k < 3; k++)
                            cb[k] = c(j-1, i, k);
                          m_glfcns.glColor3fv (cb);

                          if (el_mode > 0)
                            {
                              for (int k = 0; k < 3; k++)
                                cb[k] *= as;
                              m_glfcns.glMaterialfv (LIGHT_MODE, GL_AMBIENT,
                                                     cb);

                              for (int k = 0; k < 3; k++)
                                cb[k] = ds * c(j-1, i, k);
                              m_glfcns.glMaterialfv (LIGHT_MODE, GL_DIFFUSE,
                                                     cb);

                              for (int k = 0; k < 3; k++)
                                cb[k] = ss * (scr + (1-scr) * c(j-1, i, k));
                              m_glfcns.glMaterialfv (LIGHT_MODE, GL_SPECULAR,
                                                     cb);
                            }
                        }
                      if (el_mode > 0)
                        {
                          if (el_mode == GOURAUD)
                            set_normal (bfl_mode, vn, j-1, i);
                          else
                            set_normal (bfl_mode, fn, j-1, std::min (i, zc-2));
                        }

                      m_glfcns.glVertex3d (x(j1, i), y(j-1, i2), z(j-1, i));

                      // Vertex 2
                      if (ec_mode == INTERP)
                        {
                          for (int k = 0; k < 3; k++)
                            cb[k] = c(j, i, k);
                          m_glfcns.glColor3fv (cb);

                          if (el_mode > 0)
                            {
                              for (int k = 0; k < 3; k++)
                                cb[k] *= as;
                              m_glfcns.glMaterialfv (LIGHT_MODE, GL_AMBIENT,
                                                     cb);

                              for (int k = 0; k < 3; k++)
                                cb[k] = ds * c(j, i, k);
                              m_glfcns.glMaterialfv (LIGHT_MODE, GL_DIFFUSE,
                                                     cb);

                              for (int k = 0; k < 3; k++)
                                cb[k] = ss * (scr + (1-scr) * c(j, i, k));
                              m_glfcns.glMaterialfv (LIGHT_MODE, GL_SPECULAR,
                                                     cb);
                            }
                        }
                      if (el_mode == GOURAUD)
                        set_normal (bfl_mode, vn, j, i);

                      m_glfcns.glVertex3d (x(j2, i), y(j, i2), z(j, i));

                      m_glfcns.glEnd ();
                    }
                }
            }

          // Mesh along X-axis

          if (props.meshstyle_is ("both") || props.meshstyle_is ("row"))
            {
              for (int j = 0; j < zr; j++)
                {
                  if (x_mat)
                    {
                      j1 = j-1;
                      j2 = j;
                    }

                  for (int i = 1; i < zc; i++)
                    {
                      if (clip(j, i-1) || clip(j, i))
                        continue;

                      if (ec_mode == FLAT)
                        {
                          // "flat" only needs color at lower-left vertex
                          if (! math::isfinite (c(j, i-1)))
                            continue;
                        }
                      else if (ec_mode == INTERP)
                        {
                          // "interp" needs valid color at both vertices
                          if (! (math::isfinite (c(j, i-1))
                                 && math::isfinite (c(j, i))))
                            continue;
                        }

                      if (y_mat)
                        {
                          i1 = i-1;
                          i2 = i;
                        }

                      m_glfcns.glBegin (GL_LINES);

                      // Vertex 1
                      if (ec_mode > 0)
                        {
                          for (int k = 0; k < 3; k++)
                            cb[k] = c(j, i-1, k);
                          m_glfcns.glColor3fv (cb);

                          if (el_mode > 0)
                            {
                              for (int k = 0; k < 3; k++)
                                cb[k] *= as;
                              m_glfcns.glMaterialfv (LIGHT_MODE, GL_AMBIENT,
                                                     cb);

                              for (int k = 0; k < 3; k++)
                                cb[k] = ds * c(j, i-1, k);
                              m_glfcns.glMaterialfv (LIGHT_MODE, GL_DIFFUSE,
                                                     cb);

                              for (int k = 0; k < 3; k++)
                                cb[k] = ss * (scr + (1-scr) * c(j, i-1, k));
                              m_glfcns.glMaterialfv (LIGHT_MODE, GL_SPECULAR,
                                                     cb);
                            }
                        }
                      if (el_mode > 0)
                        {
                          if (el_mode == GOURAUD)
                            set_normal (bfl_mode, vn, j, i-1);
                          else
                            set_normal (bfl_mode, fn, std::min (j, zr-2), i-1);
                        }

                      m_glfcns.glVertex3d (x(j2, i-1), y(j, i1), z(j, i-1));

                      // Vertex 2
                      if (ec_mode == INTERP)
                        {
                          for (int k = 0; k < 3; k++)
                            cb[k] = c(j, i, k);
                          m_glfcns.glColor3fv (cb);

                          if (el_mode > 0)
                            {
                              for (int k = 0; k < 3; k++)
                                cb[k] *= as;
                              m_glfcns.glMaterialfv (LIGHT_MODE, GL_AMBIENT,
                                                     cb);

                              for (int k = 0; k < 3; k++)
                                cb[k] = ds * c(j, i, k);
                              m_glfcns.glMaterialfv (LIGHT_MODE, GL_DIFFUSE,
                                                     cb);

                              for (int k = 0; k < 3; k++)
                                cb[k] = ss * (scr + (1-scr) * c(j, i, k));
                              m_glfcns.glMaterialfv (LIGHT_MODE, GL_SPECULAR,
                                                     cb);
                            }
                        }
                      if (el_mode == GOURAUD)
                        set_normal (bfl_mode, vn, j, i);

                      m_glfcns.glVertex3d (x(j2, i), y(j, i2), z(j, i));

                      m_glfcns.glEnd ();
                    }
                }
            }

          set_linestyle ("-");  // Disable LineStipple
          set_linewidth (0.5f);

          if ((el_mode > 0) && do_lighting)
            m_glfcns.glDisable (GL_LIGHTING);
        }
      else
        {
          // FIXME: implement transparency
        }
    }

  if (! props.marker_is ("none")
      && ! (props.markeredgecolor_is ("none")
            && props.markerfacecolor_is ("none")))
    {
      // FIXME: check how transparency should be handled in markers
      // FIXME: check what to do with marker facecolor set to auto
      //        and facecolor set to none.

      bool do_edge = draw_all || ! props.markeredgecolor_is ("none");
      bool do_face = draw_all || ! props.markerfacecolor_is ("none");

      Matrix mecolor = (draw_all ? Matrix (1, 3, 0.0) :
                        props.get_markeredgecolor_rgb ());
      Matrix mfcolor = (draw_all ? Matrix (1, 3, 0.0) :
                        props.get_markerfacecolor_rgb ());
      Matrix cc (1, 3, 0.0);

      if (mecolor.isempty () && props.markeredgecolor_is ("auto"))
        {
          mecolor = props.get_edgecolor_rgb ();
          do_edge = ! props.edgecolor_is ("none");
        }

      if (mfcolor.isempty () && props.markerfacecolor_is ("auto"))
        {
          mfcolor = props.get_facecolor_rgb ();
          do_face = ! props.facecolor_is ("none");
        }

      if ((mecolor.isempty () || mfcolor.isempty ()) && c.isempty ())
        c = props.get_color_data ().array_value ();

      init_marker (props.get_marker (), props.get_markersize (),
                   props.get_linewidth ());

      uint8_t clip_mask = (props.is_clipping () ? 0x7F : 0x40);
      uint8_t clip_ok = 0x40;

      for (int i = 0; i < zc; i++)
        {
          if (y_mat)
            i1 = i;

          for (int j = 0; j < zr; j++)
            {
              if (x_mat)
                j1 = j;

              if ((clip_code (x(j1, i), y(j, i1), z(j, i)) & clip_mask)
                  != clip_ok)
                continue;

              if ((do_edge && mecolor.isempty ())
                  || (do_face && mfcolor.isempty ()))
                {
                  if (! math::isfinite (c(j, i)))
                    continue;  // Skip NaNs in color data

                  for (int k = 0; k < 3; k++)
                    cc(k) = c(j, i, k);
                }

              Matrix lc = (do_edge ? (mecolor.isempty () ? cc : mecolor)
                           : Matrix ());
              Matrix fc = (do_face ? (mfcolor.isempty () ? cc : mfcolor)
                           : Matrix ());

              draw_marker (x(j1, i), y(j, i1), z(j, i), lc, fc);
            }
        }

      end_marker ();
    }

#else

  octave_unused_parameter (props);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

// FIXME: global optimization (rendering, data structures...),
// there is probably a smarter/faster/less-memory-consuming way to do this.
void
opengl_renderer::draw_patch (const patch::properties& props)
{
#if defined (HAVE_OPENGL)

  // Do not render if the patch has incoherent data
  std::string msg;
  if (props.has_bad_data (msg))
    {
      warning ("opengl_renderer: %s.  Not rendering.", msg.c_str ());
      return;
    }

  bool draw_all = m_selecting && props.pickableparts_is ("all");
  const Matrix f = props.get_faces ().matrix_value ();
  const Matrix v = m_xform.scale (props.get_vertices ().matrix_value ());
  Matrix c;
  Matrix a;
  double fa = 1.0;

  int nv = v.rows ();
  int nf = f.rows ();
  int fcmax = f.columns ();

  bool has_z = (v.columns () > 2);
  bool has_facecolor = false;
  bool has_facealpha = false;

  int fc_mode = ((props.facecolor_is ("none")
                  || props.facecolor_is_rgb () || draw_all) ? 0 :
                 (props.facecolor_is ("flat") ? 1 : 2));
  int fl_mode = (props.facelighting_is ("none") ? 0 :
                 (props.facelighting_is ("flat") ? 1 : 2));
  int fa_mode = (props.facealpha_is_double () ? 0 :
                 (props.facealpha_is ("flat") ? 1 : 2));
  int ec_mode = ((props.edgecolor_is ("none")
                  || props.edgecolor_is_rgb ()) ? 0 :
                 (props.edgecolor_is ("flat") ? 1 : 2));
  int el_mode = (props.edgelighting_is ("none") ? 0 :
                 (props.edgelighting_is ("flat") ? 1 : 2));
  int ea_mode = (props.edgealpha_is_double () ? 0 :
                 (props.edgealpha_is ("flat") ? 1 : 2));
  int bfl_mode = (props.backfacelighting_is ("lit") ? 0 :
                  (props.backfacelighting_is ("reverselit") ? 1 : 2));
  bool do_lighting = props.get_do_lighting ();

  Matrix fcolor = props.get_facecolor_rgb ();
  Matrix ecolor = props.get_edgecolor_rgb ();

  float as = props.get_ambientstrength ();
  float ds = props.get_diffusestrength ();
  float ss = props.get_specularstrength ();
  float se = props.get_specularexponent () * 5; // to fit Matlab
  float scr = props.get_specularcolorreflectance ();

  const Matrix vn = props.get_vertexnormals ().matrix_value ();
  bool has_vertex_normals = (vn.rows () == nv);
  const Matrix fn = props.get_facenormals ().matrix_value ();
  bool has_face_normals = (fn.rows () == nf);

  boolMatrix clip (1, nv, false);

  if (has_z)
    for (int i = 0; i < nv; i++)
      clip(i) = is_nan_or_inf (v(i, 0), v(i, 1), v(i, 2));
  else
    for (int i = 0; i < nv; i++)
      clip(i) = is_nan_or_inf (v(i, 0), v(i, 1), 0);

  boolMatrix clip_f (1, nf, false);
  Array<int> count_f (dim_vector (nf, 1), 0);

  for (int i = 0; i < nf; i++)
    {
      bool fclip = false;
      int count = 0;

      for (int j = 0; j < fcmax && ! math::isnan (f(i, j)); j++, count++)
        fclip = (fclip || clip(int (f(i, j) - 1)));

      clip_f(i) = fclip;
      count_f(i) = count;
    }

  if (draw_all || fc_mode > 0 || ec_mode > 0)
    {
      if (draw_all)
        c = Matrix (1, 3, 0.0);
      else
        c = props.get_color_data ().matrix_value ();

      if (c.rows () == 1)
        {
          // Single color specifications, we can simplify a little bit

          if (draw_all || fc_mode > 0)
            {
              fcolor = c;
              fc_mode = UNIFORM;
            }

          if (draw_all || ec_mode > 0)
            {
              ecolor = c;
              ec_mode = UNIFORM;
            }

          c = Matrix ();
        }
      else
        has_facecolor = ((c.numel () > 0) && (c.rows () == f.rows ()));
    }

  if (fa_mode > 0 || ea_mode > 0)
    {
      // FIXME: retrieve alpha data from patch object
      //a = props.get_alpha_data ();
      has_facealpha = ((a.numel () > 0) && (a.rows () == f.rows ()));
    }

  if (fa_mode == 0)
    fa = props.get_facealpha_double ();

  octave_idx_type fr = f.rows ();
  std::vector<vertex_data> vdata (f.numel ());

  for (int i = 0; i < nf; i++)
    for (int j = 0; j < count_f(i); j++)
      {
        int idx = int (f(i, j) - 1);

        Matrix vv (1, 3, 0.0);
        Matrix cc;
        Matrix vnn (1, 3, 0.0);
        Matrix fnn (1, 3, 0.0);
        double aa = 1.0;

        vv(0) = v(idx, 0); vv(1) = v(idx, 1);
        if (has_z)
          vv(2) = v(idx, 2);
        if (((fl_mode == FLAT) || (el_mode == FLAT)) && has_face_normals)
          {
            double dir = 1.0;
            if (bfl_mode > 0)
              dir = ((fn(i, 0) * m_view_vector(0)
                      + fn(i, 1) * m_view_vector(1)
                      + fn(i, 2) * m_view_vector(2) < 0)
                     ? ((bfl_mode > 1) ? 0.0 : -1.0) : 1.0);
            fnn(0) = dir * fn(i, 0);
            fnn(1) = dir * fn(i, 1);
            fnn(2) = dir * fn(i, 2);
          }
        if ((fl_mode == GOURAUD || el_mode == GOURAUD) && has_vertex_normals)
          {
            double dir = 1.0;
            if (bfl_mode > 0)
              dir = ((vn(idx, 0) * m_view_vector(0)
                      + vn(idx, 1) * m_view_vector(1)
                      + vn(idx, 2) * m_view_vector(2) < 0)
                     ? ((bfl_mode > 1) ? 0.0 : -1.0) : 1.0);
            vnn(0) = dir * vn(idx, 0);
            vnn(1) = dir * vn(idx, 1);
            vnn(2) = dir * vn(idx, 2);
          }
        if (c.numel () > 0)
          {
            cc.resize (1, 3);
            if (has_facecolor)
              cc(0) = c(i, 0), cc(1) = c(i, 1), cc(2) = c(i, 2);
            else
              cc(0) = c(idx, 0), cc(1) = c(idx, 1), cc(2) = c(idx, 2);
          }
        if (fa_mode == 0)
          aa = fa;
        else if (a.numel () > 0)
          {
            if (has_facealpha)
              aa = a(i);
            else
              aa = a(idx);
          }

        vdata[i+j*fr]
          = vertex_data (vv, cc, vnn, fnn, aa, as, ds, ss, se, scr);
      }

  if (fl_mode > 0 || el_mode > 0)
    m_glfcns.glMaterialf (LIGHT_MODE, GL_SHININESS, se);

  if (draw_all || ! props.facecolor_is ("none"))
    {
      // FIXME: adapt to double-radio property
      if (fa_mode == 0)
        {
          if (fc_mode == UNIFORM)
            {
              m_glfcns.glColor4d (fcolor(0), fcolor(1), fcolor(2), fa);
              if (fl_mode > 0)
                {
                  float cb[4] = { 0.0f, 0.0f, 0.0f, 1.0f };

                  for (int i = 0; i < 3; i++)
                    cb[i] = as * fcolor(i);
                  m_glfcns.glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                  for (int i = 0; i < 3; i++)
                    cb[i] = ds * fcolor(i);
                  m_glfcns.glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);

                  for (int i = 0; i < 3; i++)
                    cb[i] = ss * (scr + (1-scr) * fcolor(i));
                  m_glfcns.glMaterialfv (LIGHT_MODE, GL_SPECULAR, cb);
                }
            }

          if ((fl_mode > 0) && do_lighting)
            m_glfcns.glEnable (GL_LIGHTING);

          // NOTE: Push filled part of patch backwards to avoid Z-fighting
          // with tessellator outline.  A value of 1.0 seems to work fine.
          // Value can't be too large or the patch will be pushed below the
          // axes planes at +2.5.
          patch_tessellator tess (this, fc_mode, fl_mode, true, 1.0);

          std::vector<octave_idx_type>::const_iterator it;
          octave_idx_type i_start, i_end;

          for (int i = 0; i < nf; i++)
            {
              if (clip_f(i))
                continue;

              bool is_non_planar = false;
              if (props.m_coplanar_last_idx.size () > 0
                  && props.m_coplanar_last_idx[i].size () > 1)
                {
                  is_non_planar = true;
                  it = props.m_coplanar_last_idx[i].end ();
                  it--;
                }

              // loop over planar subsets of face
              do
                {
                  if (is_non_planar)
                    {
                      i_end = *it;
                      if (it == props.m_coplanar_last_idx[i].begin ())
                        i_start = 0;
                      else
                        {
                          it--;
                          i_start = *it - 1;
                        }
                    }
                  else
                    {
                      i_end = count_f(i) - 1;
                      i_start = 0;
                    }

                  tess.begin_polygon (true);
                  tess.begin_contour ();

                  // Add vertices in reverse order for Matlab compatibility
                  for (int j = i_end; j > i_start; j--)
                    {
                      vertex_data::vertex_data_rep *vv
                        = vdata[i+j*fr].get_rep ();

                      tess.add_vertex (vv->m_coords.fortran_vec (), vv);
                    }

                  if (count_f(i) > 0)
                    {
                      vertex_data::vertex_data_rep *vv = vdata[i].get_rep ();

                      if (fc_mode == FLAT)
                        {
                          // For "flat" shading, use color of 1st vertex.
                          Matrix col = vv->m_color;

                          if (col.numel () == 3)
                            {
                              m_glfcns.glColor4d (col(0), col(1), col(2), fa);
                              if (fl_mode > 0)
                                {
                                  float cb[4] = { 0.0f, 0.0f, 0.0f, 1.0f };

                                  for (int k = 0; k < 3; k++)
                                    cb[k] = (vv->m_ambient * col(k));
                                  m_glfcns.glMaterialfv (LIGHT_MODE,
                                                         GL_AMBIENT, cb);

                                  for (int k = 0; k < 3; k++)
                                    cb[k] = (vv->m_diffuse * col(k));
                                  m_glfcns.glMaterialfv (LIGHT_MODE,
                                                         GL_DIFFUSE, cb);

                                  for (int k = 0; k < 3; k++)
                                    cb[k] = vv->m_specular *
                                            (vv->m_specular_color_refl
                                             + (1-vv->m_specular_color_refl) *
                                             col(k));
                                  m_glfcns.glMaterialfv (LIGHT_MODE,
                                                         GL_SPECULAR, cb);
                                }
                            }
                        }

                      tess.add_vertex (vv->m_coords.fortran_vec (), vv);
                    }

                  tess.end_contour ();
                  tess.end_polygon ();
                }
              while (i_start > 0);
            }

          if ((fl_mode > 0) && do_lighting)
            m_glfcns.glDisable (GL_LIGHTING);
        }
      else
        {
          // FIXME: implement flat and interp transparency
        }
    }

  if (draw_all
      || (! props.edgecolor_is ("none") && ! props.linestyle_is ("none")))
    {
      // FIXME: adapt to double-radio property
      if (props.get_edgealpha_double () == 1)
        {
          if (ec_mode == UNIFORM)
            {
              m_glfcns.glColor3dv (ecolor.data ());
              if (el_mode > 0)
                {
                  // edge lighting only uses ambient light
                  float cb[4] = { 0.0f, 0.0f, 0.0f, 1.0f };
                  m_glfcns.glMaterialfv (LIGHT_MODE, GL_SPECULAR, cb);
                  m_glfcns.glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);

                  for (int i = 0; i < 3; i++)
                    cb[i] = (as * ecolor(i));
                  m_glfcns.glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);
                }
            }

          if ((el_mode > 0) && do_lighting)
            m_glfcns.glEnable (GL_LIGHTING);

          double linewidth = props.get_linewidth ();
          set_linestyle (props.get_linestyle (), false, linewidth);
          set_linewidth (linewidth);
          set_linecap ("butt");
          set_linejoin ("miter");

          // NOTE: patch contour cannot be offset.  Offset must occur with
          // the filled portion of the patch above.  The tessellator uses
          // GLU_TESS_BOUNDARY_ONLY to get the outline of the patch and OpenGL
          // automatically sets the glType to GL_LINE_LOOP.  This primitive is
          // not supported by glPolygonOffset which is used to do Z offsets.
          patch_tessellator tess (this, ec_mode, el_mode, false);

          for (int i = 0; i < nf; i++)
            {
              bool is_non_planar = false;
              if (props.m_coplanar_last_idx.size () > 0
                  && props.m_coplanar_last_idx[i].size () > 1)
                is_non_planar = true;
              if (clip_f(i) || is_non_planar)
                {
                  // This is an unclosed contour or a non-planar face.
                  // Draw it as a line.
                  bool flag = false;

                  m_glfcns.glShadeModel ((ec_mode == INTERP
                                          || el_mode == GOURAUD)
                                         ? GL_SMOOTH : GL_FLAT);

                  // Add vertices in reverse order for Matlab compatibility
                  for (int j = count_f(i)-1; j >= 0; j--)
                    {
                      if (! clip(int (f(i, j) - 1)))
                        {
                          vertex_data::vertex_data_rep *vv
                            = vdata[i+j*fr].get_rep ();
                          const Matrix m = vv->m_coords;
                          if (! flag)
                            {
                              flag = true;
                              m_glfcns.glBegin (GL_LINE_STRIP);
                            }
                          if (ec_mode != UNIFORM)
                            {
                              Matrix col = vv->m_color;

                              if (col.numel () == 3)
                                m_glfcns.glColor3dv (col.data ());
                            }
                          m_glfcns.glVertex3d (m(0), m(1), m(2));
                        }
                      else if (flag)
                        {
                          flag = false;
                          m_glfcns.glEnd ();
                        }
                    }
                  // Do loop body with vertex N to "close" GL_LINE_STRIP
                  // from vertex 0 to vertex N.
                  int j = count_f(i)-1;
                  if (flag && ! clip(int (f(i, j) - 1)))
                    {
                      vertex_data::vertex_data_rep *vv
                        = vdata[i+j*fr].get_rep ();
                      const Matrix m = vv->m_coords;
                      if (ec_mode != UNIFORM)
                        {
                          Matrix col = vv->m_color;

                          if (col.numel () == 3)
                            m_glfcns.glColor3dv (col.data ());
                        }
                      m_glfcns.glVertex3d (m(0), m(1), m(2));
                    }

                  if (flag)
                    m_glfcns.glEnd ();
                }
              else  // Normal edge contour drawn with tessellator
                {
                  tess.begin_polygon (false);
                  tess.begin_contour ();

                  for (int j = count_f(i)-1; j >= 0; j--)
                    {
                      vertex_data::vertex_data_rep *vv
                        = vdata[i+j*fr].get_rep ();
                      tess.add_vertex (vv->m_coords.fortran_vec (), vv);
                    }

                  tess.end_contour ();
                  tess.end_polygon ();
                }
            }

          set_linestyle ("-");  // Disable LineStipple
          set_linewidth (0.5f);

          if ((el_mode > 0) && do_lighting)
            m_glfcns.glDisable (GL_LIGHTING);
        }
      else
        {
          // FIXME: implement transparency
        }
    }

  if (! props.marker_is ("none")
      && ! (props.markeredgecolor_is ("none")
            && props.markerfacecolor_is ("none")))
    {
      bool do_edge = draw_all || ! props.markeredgecolor_is ("none");
      bool do_face = draw_all || ! props.markerfacecolor_is ("none");

      Matrix mecolor = (draw_all ? Matrix (1, 3, 0.0) :
                        props.get_markeredgecolor_rgb ());
      Matrix mfcolor = (draw_all ? Matrix (1, 3, 0.0) :
                        props.get_markerfacecolor_rgb ());

      bool has_markerfacecolor = draw_all || false;

      if ((mecolor.isempty () && ! props.markeredgecolor_is ("none"))
          || (mfcolor.isempty () && ! props.markerfacecolor_is ("none")))
        {
          Matrix mc = props.get_color_data ().matrix_value ();

          if (mc.rows () == 1)
            {
              // Single color specifications, we can simplify a little bit
              if (mfcolor.isempty () && ! props.markerfacecolor_is ("none"))
                mfcolor = mc;

              if (mecolor.isempty () && ! props.markeredgecolor_is ("none"))
                mecolor = mc;
            }
          else
            {
              if (c.isempty ())
                c = props.get_color_data ().matrix_value ();
              has_markerfacecolor = ((c.numel () > 0)
                                     && (c.rows () == f.rows ()));
            }
        }

      init_marker (props.get_marker (), props.get_markersize (),
                   props.get_linewidth ());

      uint8_t clip_mask = (props.is_clipping () ? 0x7F : 0x40);
      uint8_t clip_ok = 0x40;

      for (int i = 0; i < nf; i++)
        for (int j = 0; j < count_f(i); j++)
          {
            int idx = int (f(i, j) - 1);

            if ((clip_code (v(idx, 0), v(idx, 1), (has_z ? v(idx, 2) : 0))
                 & clip_mask) != clip_ok)
              continue;

            Matrix cc;
            if (c.numel () > 0)
              {
                cc.resize (1, 3);
                if (has_markerfacecolor)
                  cc(0) = c(i, 0), cc(1) = c(i, 1), cc(2) = c(i, 2);
                else
                  cc(0) = c(idx, 0), cc(1) = c(idx, 1), cc(2) = c(idx, 2);
              }

            Matrix lc = (do_edge ? (mecolor.isempty () ? cc : mecolor)
                         : Matrix ());
            Matrix fc = (do_face ? (mfcolor.isempty () ? cc : mfcolor)
                         : Matrix ());

            draw_marker (v(idx, 0), v(idx, 1), (has_z ? v(idx, 2) : 0), lc, fc);
          }

      end_marker ();
    }

#else

  octave_unused_parameter (props);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_scatter (const scatter::properties& props)
{
#if defined (HAVE_OPENGL)

  // Do not render if the scatter object has incoherent data
  std::string msg;
  if (props.has_bad_data (msg))
    {
      warning ("opengl_renderer: %s.  Not rendering.", msg.c_str ());
      return;
    }

  bool draw_all = m_selecting;

  if (draw_all || (! props.marker_is ("none")
                   && ! (props.markeredgecolor_is ("none")
                         && props.markerfacecolor_is ("none"))))
    {
      bool do_edge = draw_all || ! props.markeredgecolor_is ("none");
      bool do_face = draw_all || ! props.markerfacecolor_is ("none");

      const Matrix x = props.get_xdata ().matrix_value ();
      const Matrix y = props.get_ydata ().matrix_value ();
      const Matrix z = props.get_zdata ().matrix_value ();
      const Matrix c = props.get_color_data ().matrix_value ();
      const Matrix s = props.get_sizedata ().matrix_value ();

      int np = x.rows ();
      bool has_z = ! z.isempty ();

      // If markeredgecolor is "flat", mecolor is empty
      Matrix mecolor = (draw_all ? Matrix (1, 3, 0.0) :
                        props.get_markeredgecolor_rgb ());
      Matrix mfcolor = (draw_all ? Matrix (1, 3, 0.0) :
                        props.get_markerfacecolor_rgb ());
      const double mea = props.get_markeredgealpha ();
      const double mfa = props.get_markerfacealpha ();

      if (props.markerfacecolor_is ("auto"))
        {
          gh_manager& gh_mgr = __get_gh_manager__ ();
          graphics_object go = gh_mgr.get_object (props.get___myhandle__ ());
          graphics_object ax = go.get_ancestor ("axes");
          const axes::properties& ax_props
            = dynamic_cast<const axes::properties&> (ax.get_properties ());

          mfcolor = ax_props.get_color ().matrix_value ();
        }

      init_marker (props.get_marker (), std::sqrt (s(0)),
                   props.get_linewidth ());

      uint8_t clip_mask = (props.is_clipping () ? 0x7F : 0x40);
      uint8_t clip_ok = 0x40;

      Matrix cc;
      if (! c.isempty ())
        {
          if (c.rows () == 1)
            cc = c;
          else
            {
              cc.resize (1, 3);
              cc(0) = c(0, 0);
              cc(1) = c(0, 1);
              cc(2) = c(0, 2);
            }
        }

      for (int i = 0; i < np; i++)
        {
          if ((clip_code (x(i), y(i), (has_z ? z(i) : 0.0)) & clip_mask)
              != clip_ok)
            continue;

          if (c.rows () > 1)
            {
              cc(0) = c(i, 0);
              cc(1) = c(i, 1);
              cc(2) = c(i, 2);
            }

          Matrix lc = (do_edge ? (mecolor.isempty () ? cc : mecolor)
                       : Matrix ());
          Matrix fc = (do_face ? (mfcolor.isempty () ? cc : mfcolor)
                       : Matrix ());

          if (s.numel () > 1)
            change_marker (props.get_marker (), std::sqrt (s(i)));

          draw_marker (x(i), y(i), (has_z ? z(i) : 0.0), lc, fc, mea, mfa);
        }

      end_marker ();
    }

#else

  octave_unused_parameter (props);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_light (const light::properties& props)
{
#if defined (HAVE_OPENGL)

  // enable light source
  m_glfcns.glEnable (m_current_light);

  // light position
  float pos[4] = { 0, 0, 0, 0 }; // X,Y,Z,infinite/local
  Matrix lpos = props.get_position ().matrix_value ();
  for (int i = 0; i < 3; i++)
    pos[i] = lpos(i);
  if (props.style_is ("local"))
    pos[3] = 1;
  m_glfcns.glLightfv (m_current_light, GL_POSITION, pos);

  // light color
  float col[4] = { 1, 1, 1, 1 }; // R,G,B,ALPHA (the latter has no meaning)
  Matrix lcolor = props.get_color ().matrix_value ();
  for (int i = 0; i < 3; i++)
    col[i] = lcolor(i);
  m_glfcns.glLightfv (m_current_light, GL_DIFFUSE,  col);
  m_glfcns.glLightfv (m_current_light, GL_SPECULAR, col);

#else

  octave_unused_parameter (props);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_hggroup (const hggroup::properties& props)
{
  draw (props.get_children ());
}

void
opengl_renderer::set_ortho_coordinates (void)
{
#if defined (HAVE_OPENGL)

  m_glfcns.glMatrixMode (GL_PROJECTION);
  m_glfcns.glPushMatrix ();
  m_glfcns.glLoadIdentity ();

  Matrix vp = get_viewport_scaled ();
  m_glfcns.glOrtho (0, vp(2), vp(3), 0, m_xZ1, m_xZ2);
  m_glfcns.glMatrixMode (GL_MODELVIEW);
  m_glfcns.glPushMatrix ();
  m_glfcns.glLoadIdentity ();

#else

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::restore_previous_coordinates (void)
{
#if defined (HAVE_OPENGL)

  // Restore previous coordinate system
  m_glfcns.glMatrixMode (GL_MODELVIEW);
  m_glfcns.glPopMatrix();
  m_glfcns.glMatrixMode (GL_PROJECTION);
  m_glfcns.glPopMatrix();

#else

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_text (const text::properties& props)
{
#if defined (HAVE_OPENGL)

  if (props.get_string ().isempty () || props.color_is ("none"))
    return;

  Matrix pos = m_xform.scale (props.get_data_position ());

  // Handle clipping manually when drawing text in ortho coordinates
  if (! props.is_clipping ()
      || (clip_code (pos(0), pos(1), pos.numel () > 2 ? pos(2) : 0.0) == 0x40))
    {
      set_clipping (false);

      draw_text_background (props);

      set_font (props);

      render_text (props.get_pixels (), props.get_extent_matrix (),
                   pos(0), pos(1), pos(2), props.get_rotation ());

      set_clipping (props.is_clipping ());
    }

#else

  octave_unused_parameter (props);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_text_background (const text::properties& props,
                                       bool /*do_rotate*/)
{
#if defined (HAVE_OPENGL)

  Matrix bgcol = props.get_backgroundcolor_rgb ();
  Matrix ecol = props.get_edgecolor_rgb ();

  if (bgcol.isempty () && ecol.isempty ())
    return;

  Matrix pos = props.get_data_position ();
  ColumnVector pixpos = get_transform ().transform (pos(0), pos(1),
                        pos(2), true);

  // Save current transform matrices and set orthogonal window coordinates
  set_ortho_coordinates ();

  // Translate coordinates so that the text anchor is (0,0)
  m_glfcns.glTranslated (pixpos(0), pixpos(1), -pixpos(2));

  // FIXME: Only multiples of 90° are handled by the text renderer.
  //        Handle others here.
  double rotation = props.get_rotation ();

  m_glfcns.glRotated (-rotation, 0.0, 0.0, 1.0);

  double m = points_to_pixels (props.get_margin ());
  const Matrix bbox = props.get_extent_matrix ();
  double x0 = bbox (0) / m_devpixratio - m;
  double x1 = x0 + bbox(2) / m_devpixratio + 2 * m;
  double y0 = -(bbox (1) / m_devpixratio - m);
  double y1 = y0 - (bbox(3) / m_devpixratio + 2 * m);

  if (! bgcol.isempty ())
    {
      m_glfcns.glColor3f (bgcol(0), bgcol(1), bgcol(2));

      bool depth_test = m_glfcns.glIsEnabled (GL_DEPTH_TEST);
      if (depth_test)
        set_polygon_offset (true, 4.0);

      m_glfcns.glBegin (GL_QUADS);
      m_glfcns.glVertex2d (x0, y0);
      m_glfcns.glVertex2d (x1, y0);
      m_glfcns.glVertex2d (x1, y1);
      m_glfcns.glVertex2d (x0, y1);
      m_glfcns.glEnd ();

      if (depth_test)
        set_polygon_offset (false);
    }

  if (! ecol.isempty ())
    {
      m_glfcns.glColor3f (ecol(0), ecol(1), ecol(2));

      set_linestyle (props.get_linestyle (), false, props.get_linewidth ());
      set_linewidth (props.get_linewidth ());

      m_glfcns.glBegin (GL_LINE_STRIP);
      m_glfcns.glVertex2d (x0, y0);
      m_glfcns.glVertex2d (x1, y0);
      m_glfcns.glVertex2d (x1, y1);
      m_glfcns.glVertex2d (x0, y1);
      m_glfcns.glVertex2d (x0, y0);
      m_glfcns.glEnd ();

      set_linestyle ("-");
    }

  restore_previous_coordinates ();

#else

  octave_unused_parameter (props);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_image (const image::properties& props)
{
#if defined (HAVE_OPENGL)

  octave_value cdata = props.get_color_data ();
  Matrix x = props.get_xdata ().matrix_value ();
  Matrix y = props.get_ydata ().matrix_value ();

  draw_texture_image (cdata, x, y);

#else

  octave_unused_parameter (props);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_texture_image (const octave_value cdata, Matrix x,
                                     Matrix y, bool ortho)
{
#if defined (HAVE_OPENGL)

  dim_vector dv (cdata.dims ());
  int h = dv(0);
  int w = dv(1);
  double x0, x1, y0, y1;

  double dx = 1.0;
  if (w > 1)
    dx = (x(1) - x(0)) / (w - 1);

  x0 = x(0)-dx/2;
  x1 = x(1)+dx/2;

  double dy = 1.0;
  if (h > 1)
    dy = (y(1) - y(0)) / (h - 1);

  y0 = y(0)-dy/2;
  y1 = y(1)+dy/2;

  // Expect RGB data
  if (dv.ndims () == 3 && (dv(2) == 3 || dv(2) == 4))
    {
      opengl_texture tex  = opengl_texture::create (m_glfcns, cdata);
      if (tex.is_valid ())
        {
          m_glfcns.glColor4d (1.0, 1.0, 1.0, 1.0);

          m_glfcns.glEnable (GL_TEXTURE_2D);

          m_glfcns.glBegin (GL_QUADS);

          tex.tex_coord (0.0, 0.0);
          if (ortho)
            m_glfcns.glVertex2d (x0, y0);
          else
            m_glfcns.glVertex3d (x0, y0, 0.0);

          tex.tex_coord (1.0, 0.0);
          if (ortho)
            m_glfcns.glVertex2d (x1, y0);
          else
            m_glfcns.glVertex3d (x1, y0, 0.0);

          tex.tex_coord (1.0, 1.0);
          if (ortho)
            m_glfcns.glVertex2d (x1, y1);
          else
            m_glfcns.glVertex3d (x1, y1, 0.0);

          tex.tex_coord (0.0, 1.0);
          if (ortho)
            m_glfcns.glVertex2d (x0, y1);
          else
            m_glfcns.glVertex3d (x0, y1, 0.0);

          m_glfcns.glEnd ();
          m_glfcns.glDisable (GL_TEXTURE_2D);
        }
    }
  else
    warning ("opengl_renderer: invalid image size (expected MxNx3 or MxN)");

#else

  octave_unused_parameter (cdata);
  octave_unused_parameter (x);
  octave_unused_parameter (y);
  octave_unused_parameter (ortho);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void opengl_renderer::draw (const Matrix& hlist, bool toplevel)
{
  int len = hlist.numel ();

  gh_manager& gh_mgr = __get_gh_manager__ ();

  for (int i = len-1; i >= 0; i--)
    {
      graphics_object obj = gh_mgr.get_object (hlist(i));

      if (obj)
        draw (obj, toplevel);
    }
}

void
opengl_renderer::set_viewport (int w, int h)
{
#if defined (HAVE_OPENGL)

  m_glfcns.glViewport (0, 0, w, h);

#else

  octave_unused_parameter (w);
  octave_unused_parameter (h);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

Matrix
opengl_renderer::get_viewport_scaled (void) const
{
  Matrix retval (1, 4, 0.0);

#if defined (HAVE_OPENGL)
#if defined (HAVE_FRAMEWORK_OPENGL)
  GLint vp[4];
#else
  int vp[4];
#endif

  m_glfcns.glGetIntegerv (GL_VIEWPORT, vp);

  for (int i = 0; i < 4; i++)
    retval(i) = static_cast<double> (vp[i]) / m_devpixratio;

#else

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif

  return retval;
}

void
opengl_renderer::set_color (const Matrix& c)
{
#if defined (HAVE_OPENGL)

  m_glfcns.glColor3dv (c.data ());

  if (! c.isempty ())
    m_txt_renderer.set_color (c);

#else

  octave_unused_parameter (c);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::set_font (const base_properties& props)
{
  bool do_anti_alias = props.get ("fontsmoothing").string_value () == "on";
  m_txt_renderer.set_anti_aliasing (do_anti_alias);
  m_txt_renderer.set_font (props.get ("fontname").string_value (),
                           props.get ("fontweight").string_value (),
                           props.get ("fontangle").string_value (),
                           props.get ("__fontsize_points__").double_value ()
                           * m_devpixratio);
}

void
opengl_renderer::set_polygon_offset (bool on, float offset)
{
#if defined (HAVE_OPENGL)

  if (on)
    {
      m_glfcns.glEnable (GL_POLYGON_OFFSET_FILL);
      m_glfcns.glEnable (GL_POLYGON_OFFSET_LINE);
      m_glfcns.glPolygonOffset (offset, offset);
    }
  else
    {
      m_glfcns.glDisable (GL_POLYGON_OFFSET_FILL);
      m_glfcns.glDisable (GL_POLYGON_OFFSET_LINE);
    }

#else

  octave_unused_parameter (on);
  octave_unused_parameter (offset);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::set_linewidth (float w)
{
#if defined (HAVE_OPENGL)
  // Measure LineWidth in points.  See bug #53056.
  m_glfcns.glLineWidth (points_to_pixels (w) * m_devpixratio);

#else

  octave_unused_parameter (w);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::set_linestyle (const std::string& s, bool use_stipple,
                                double linewidth)
{
#if defined (HAVE_OPENGL)
  // Measure LineWidth in points.  See bug #53056.
  int factor = math::round (points_to_pixels (linewidth) * m_devpixratio);
  if (factor < 1)
    factor = 1;

  uint16_t pattern = 0xFFFF;

  bool solid = false;

  if (s == "-")
    solid = true;
  else if (s == ":")
    {
      if (factor > 1)
        pattern = 0x5555;
      else
        pattern = 0x1111;
    }
  else if (s == "--")
    {
      if (factor > 1)
        pattern = 0x0F0F;
      else
        pattern = 0x01FF;
    }
  else if (s == "-.")
    {
      if (factor > 1)
        pattern = 0x6F6F;
      else
        pattern = 0x18FF;
    }
  else
    pattern = 0x0000;

  m_glfcns.glLineStipple (factor, pattern);

  if (solid && ! use_stipple)
    m_glfcns.glDisable (GL_LINE_STIPPLE);
  else
    m_glfcns.glEnable (GL_LINE_STIPPLE);

#else

  octave_unused_parameter (s);
  octave_unused_parameter (use_stipple);
  octave_unused_parameter (linewidth);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::set_clipbox (double x1, double x2, double y1, double y2,
                              double z1, double z2)
{
#if defined (HAVE_OPENGL)

  double dx = (x2-x1);
  double dy = (y2-y1);
  double dz = (z2-z1);

  x1 -= 0.001*dx; x2 += 0.001*dx;
  y1 -= 0.001*dy; y2 += 0.001*dy;
  z1 -= 0.001*dz; z2 += 0.001*dz;

  ColumnVector p (4, 0.0);

  p(0) = -1; p(3) = x2;
  m_glfcns.glClipPlane (GL_CLIP_PLANE0, p.data ());
  p(0) = 1; p(3) = -x1;
  m_glfcns.glClipPlane (GL_CLIP_PLANE1, p.data ());
  p(0) = 0; p(1) = -1; p(3) = y2;
  m_glfcns.glClipPlane (GL_CLIP_PLANE2, p.data ());
  p(1) = 1; p(3) = -y1;
  m_glfcns.glClipPlane (GL_CLIP_PLANE3, p.data ());
  p(1) = 0; p(2) = -1; p(3) = z2;
  m_glfcns.glClipPlane (GL_CLIP_PLANE4, p.data ());
  p(2) = 1; p(3) = -z1;
  m_glfcns.glClipPlane (GL_CLIP_PLANE5, p.data ());

  m_xmin = x1; m_xmax = x2;
  m_ymin = y1; m_ymax = y2;
  m_zmin = z1; m_zmax = z2;

#else

  octave_unused_parameter (x1);
  octave_unused_parameter (x2);
  octave_unused_parameter (y1);
  octave_unused_parameter (y2);
  octave_unused_parameter (z1);
  octave_unused_parameter (z2);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::set_clipping (bool enable)
{
#if defined (HAVE_OPENGL)

  bool has_clipping = (m_glfcns.glIsEnabled (GL_CLIP_PLANE0) == GL_TRUE);

  if (enable != has_clipping)
    {
      if (enable)
        for (int i = 0; i < 6; i++)
          m_glfcns.glEnable (GL_CLIP_PLANE0+i);
      else
        for (int i = 0; i < 6; i++)
          m_glfcns.glDisable (GL_CLIP_PLANE0+i);
    }

#else

  octave_unused_parameter (enable);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::init_marker (const std::string& m, double size, float width)
{
#if defined (HAVE_OPENGL)
  m_glfcns.glMatrixMode (GL_PROJECTION);
  m_glfcns.glPushMatrix ();
  m_glfcns.glLoadIdentity ();

  Matrix vp = get_viewport_scaled ();
  m_glfcns.glOrtho (0, vp(2), vp(3), 0, m_xZ1, m_xZ2);
  m_glfcns.glMatrixMode (GL_MODELVIEW);
  m_glfcns.glPushMatrix ();

  set_clipping (false);
  set_linewidth (width);

  m_marker_id = make_marker_list (m, size, false);
  m_filled_marker_id = make_marker_list (m, size, true);

#else

  octave_unused_parameter (m);
  octave_unused_parameter (size);
  octave_unused_parameter (width);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::change_marker (const std::string& m, double size)
{
#if defined (HAVE_OPENGL)

  m_marker_id = make_marker_list (m, size, false);
  m_filled_marker_id = make_marker_list (m, size, true);

#else

  octave_unused_parameter (m);
  octave_unused_parameter (size);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::end_marker (void)
{
#if defined (HAVE_OPENGL)

  m_glfcns.glDeleteLists (m_marker_id, 1);
  m_glfcns.glDeleteLists (m_filled_marker_id, 1);

  m_glfcns.glMatrixMode (GL_MODELVIEW);
  m_glfcns.glPopMatrix ();
  m_glfcns.glMatrixMode (GL_PROJECTION);
  m_glfcns.glPopMatrix ();
  set_linewidth (0.5f);

#else

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::draw_marker (double x, double y, double z,
                              const Matrix& lc, const Matrix& fc,
                              const double la, const double fa)
{
#if defined (HAVE_OPENGL)

  ColumnVector tmp = m_xform.transform (x, y, z, false);

  m_glfcns.glLoadIdentity ();
  m_glfcns.glTranslated (tmp(0), tmp(1), -tmp(2));

  if (m_filled_marker_id > 0 && fc.numel () > 0)
    {
      m_glfcns.glColor4d (fc(0), fc(1), fc(2), fa);
      set_polygon_offset (true, -1.0);
      m_glfcns.glCallList (m_filled_marker_id);
      if (lc.numel () > 0)
        {
          m_glfcns.glColor4d (lc(0), lc(1), lc(2), la);
          m_glfcns.glPolygonMode (GL_FRONT_AND_BACK, GL_LINE);
          m_glfcns.glEdgeFlag (GL_TRUE);
          set_polygon_offset (true, -2.0);
          m_glfcns.glCallList (m_filled_marker_id);
          m_glfcns.glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
        }
      set_polygon_offset (false);
    }
  else if (m_marker_id > 0 && lc.numel () > 0)
    {
      m_glfcns.glColor4d (lc(0), lc(1), lc(2), la);
      m_glfcns.glCallList (m_marker_id);
    }

#else

  octave_unused_parameter (x);
  octave_unused_parameter (y);
  octave_unused_parameter (z);
  octave_unused_parameter (lc);
  octave_unused_parameter (fc);
  octave_unused_parameter (la);
  octave_unused_parameter (fa);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::init_maxlights (void)
{
#if defined (HAVE_OPENGL)

  // Check actual maximum number of lights possible
  if (m_max_lights == 0)
    {
      GLint max_lights;
      m_glfcns.glGetIntegerv (GL_MAX_LIGHTS, &max_lights);
      m_max_lights = max_lights;
    }

#else

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

std::string
opengl_renderer::get_string (unsigned int id) const
{
#if defined (HAVE_OPENGL)

  // This is kind of ugly, but glGetString returns a pointer to GLubyte
  // and there is no std::string constructor that matches.  Is there a
  // better way?

  std::ostringstream buf;

  buf << m_glfcns.glGetString (static_cast<GLenum> (id));

  return std::string (buf.str ());

#else

  octave_unused_parameter (id);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();
  return std::string ();

#endif
}

void
opengl_renderer::set_normal (int bfl_mode, const NDArray& n, int j, int i)
{
#if defined (HAVE_OPENGL)

  double x = n(j, i, 0);
  double y = n(j, i, 1);
  double z = n(j, i, 2);

  double d = sqrt (x*x + y*y + z*z);

  double dir = 1.0;

  if (bfl_mode > 0)
    dir = ((x*m_view_vector(0) + y*m_view_vector(1) + z*m_view_vector(2) < 0)
           ? ((bfl_mode > 1) ? 0.0 : -1.0) : 1.0);

  m_glfcns.glNormal3d (dir*x/d, dir*y/d, dir*z/d);

#else

  octave_unused_parameter (bfl_mode);
  octave_unused_parameter (n);
  octave_unused_parameter (j);
  octave_unused_parameter (i);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

double
opengl_renderer::points_to_pixels (const double val) const
{
  gh_manager& gh_mgr = __get_gh_manager__ ();

  // FIXME: Does making this static cause problems if figure is moved to a
  //        2nd monitor with a different value for "screenpixelsperinch"?
  static const double pix_per_pts =
    gh_mgr.get_object (0).get ("screenpixelsperinch").double_value () / 72.0;

  double retval = val;

  if (! m_printing)
    retval *= pix_per_pts;

  return retval;
}

unsigned int
opengl_renderer::make_marker_list (const std::string& marker, double size,
                                   bool filled) const
{
#if defined (HAVE_OPENGL)

  char c = marker[0];

  if (filled && (c == '+' || c == 'x' || c == '*' || c == '.'
                 || c == '|' || c == '_'))
    return 0;

  unsigned int ID = m_glfcns.glGenLists (1);

  // FIXME: See bug #53056 (measure LineWidth in points).
  double sz = points_to_pixels (size);

  // constants for the * marker
  const double sqrt2d4 = 0.35355339059327;
  double tt = sz*sqrt2d4;

  m_glfcns.glNewList (ID, GL_COMPILE);

  switch (marker[0])
    {
    case '+':
      m_glfcns.glBegin (GL_LINES);
      m_glfcns.glVertex2d (-sz/2, 0);
      m_glfcns.glVertex2d (sz/2, 0);
      m_glfcns.glVertex2d (0, -sz/2);
      m_glfcns.glVertex2d (0, sz/2);
      m_glfcns.glEnd ();
      break;
    case '|':
      m_glfcns.glBegin (GL_LINES);
      m_glfcns.glVertex2d (0, -sz/2);
      m_glfcns.glVertex2d (0, sz/2);
      m_glfcns.glEnd ();
      break;
    case '_':
      m_glfcns.glBegin (GL_LINES);
      m_glfcns.glVertex2d (-sz/2, 0);
      m_glfcns.glVertex2d (sz/2, 0);
      m_glfcns.glEnd ();
      break;
    case 'x':
      m_glfcns.glBegin (GL_LINES);
      m_glfcns.glVertex2d (-sz/2, -sz/2);
      m_glfcns.glVertex2d (sz/2, sz/2);
      m_glfcns.glVertex2d (-sz/2, sz/2);
      m_glfcns.glVertex2d (sz/2, -sz/2);
      m_glfcns.glEnd ();
      break;
    case '*':
      m_glfcns.glBegin (GL_LINES);
      m_glfcns.glVertex2d (-sz/2, 0);
      m_glfcns.glVertex2d (sz/2, 0);
      m_glfcns.glVertex2d (0, -sz/2);
      m_glfcns.glVertex2d (0, sz/2);
      m_glfcns.glVertex2d (-tt, -tt);
      m_glfcns.glVertex2d (+tt, +tt);
      m_glfcns.glVertex2d (-tt, +tt);
      m_glfcns.glVertex2d (+tt, -tt);
      m_glfcns.glEnd ();
      break;
    case '.':
      {
        // The dot marker is special and is drawn at 1/3rd the specified size

        // Ensure that something is drawn even at very small markersizes
        if (sz > 0 && sz < 3)
          sz = 3;

        int div = static_cast<int> (M_PI * sz / 12);
        if (! (div % 2))
          div += 1;               // ensure odd number for left/right symmetry
        div = std::max (div, 3);  // ensure at least a few vertices are drawn
        double ang_step = M_PI / div;

        m_glfcns.glBegin (GL_POLYGON);
        for (double ang = 0; ang < 2*M_PI; ang += ang_step)
          m_glfcns.glVertex2d (sz/6* cos (ang), sz/6*sin (ang));
        m_glfcns.glEnd ();
      }
      break;
    case 's':
      m_glfcns.glBegin (filled ? GL_POLYGON : GL_LINE_LOOP);
      m_glfcns.glVertex2d (-sz/2, -sz/2);
      m_glfcns.glVertex2d (-sz/2, sz/2);
      m_glfcns.glVertex2d (sz/2, sz/2);
      m_glfcns.glVertex2d (sz/2, -sz/2);
      m_glfcns.glEnd ();
      break;
    case 'o':
      {
        int div = static_cast<int> (M_PI * sz / 4);
        if (! (div % 2))
          div += 1;               // ensure odd number for left/right symmetry
        div = std::max (div, 5);  // ensure at least a few vertices are drawn
        double ang_step = M_PI / div;

        m_glfcns.glBegin (filled ? GL_POLYGON : GL_LINE_LOOP);
        for (double ang = 0; ang < 2*M_PI; ang += ang_step)
          m_glfcns.glVertex2d (sz/2* cos (ang), sz/2*sin (ang));
        m_glfcns.glEnd ();
      }
      break;
    case 'd':
      m_glfcns.glBegin (filled ? GL_POLYGON : GL_LINE_LOOP);
      m_glfcns.glVertex2d (0, -sz/2);
      m_glfcns.glVertex2d (sz/2, 0);
      m_glfcns.glVertex2d (0, sz/2);
      m_glfcns.glVertex2d (-sz/2, 0);
      m_glfcns.glEnd ();
      break;
    case 'v':
      m_glfcns.glBegin (filled ? GL_POLYGON : GL_LINE_LOOP);
      m_glfcns.glVertex2d (0, sz/2);
      m_glfcns.glVertex2d (sz/2, -sz/2);
      m_glfcns.glVertex2d (-sz/2, -sz/2);
      m_glfcns.glEnd ();
      break;
    case '^':
      m_glfcns.glBegin (filled ? GL_POLYGON : GL_LINE_LOOP);
      m_glfcns.glVertex2d (0, -sz/2);
      m_glfcns.glVertex2d (-sz/2, sz/2);
      m_glfcns.glVertex2d (sz/2, sz/2);
      m_glfcns.glEnd ();
      break;
    case '>':
      m_glfcns.glBegin (filled ? GL_POLYGON : GL_LINE_LOOP);
      m_glfcns.glVertex2d (sz/2, 0);
      m_glfcns.glVertex2d (-sz/2, sz/2);
      m_glfcns.glVertex2d (-sz/2, -sz/2);
      m_glfcns.glEnd ();
      break;
    case '<':
      m_glfcns.glBegin (filled ? GL_POLYGON : GL_LINE_LOOP);
      m_glfcns.glVertex2d (-sz/2, 0);
      m_glfcns.glVertex2d (sz/2, -sz/2);
      m_glfcns.glVertex2d (sz/2, sz/2);
      m_glfcns.glEnd ();
      break;
    case 'p':
      {
        double ang, r, dr;
        dr = 1.0 - sin (M_PI/10)/sin (3*M_PI/10)*1.02;

        m_glfcns.glBegin (filled ? GL_POLYGON : GL_LINE_LOOP);
        for (int i = 0; i < 2*5; i++)
          {
            ang = (-0.5 + double (i+1) / 5) * M_PI;
            r = 1.0 - (dr * fmod (double (i+1), 2.0));
            m_glfcns.glVertex2d (sz/2*r* cos (ang), sz/2*r*sin (ang));
          }
        m_glfcns.glEnd ();
      }
      break;
    case 'h':
      {
        double ang, r, dr;
        dr = 1.0 - 0.5/sin (M_PI/3)*1.02;

        m_glfcns.glBegin (filled ? GL_POLYGON : GL_LINE_LOOP);
        for (int i = 0; i < 2*6; i++)
          {
            ang = (0.5 + double (i+1) / 6.0) * M_PI;
            r = 1.0 - (dr * fmod (double (i+1), 2.0));
            m_glfcns.glVertex2d (sz/2*r* cos (ang), sz/2*r*sin (ang));
          }
        m_glfcns.glEnd ();
      }
      break;
    default:
      warning ("opengl_renderer: unsupported marker '%s'", marker.c_str ());
      break;
    }

  m_glfcns.glEndList ();

  return ID;

#else

  octave_unused_parameter (marker);
  octave_unused_parameter (size);
  octave_unused_parameter (filled);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::text_to_pixels (const std::string& txt,
                                 uint8NDArray& pixels,
                                 Matrix& bbox,
                                 int halign, int valign, double rotation)
{
  m_txt_renderer.text_to_pixels (txt, pixels, bbox, halign, valign,
                                 rotation, m_interpreter);
}

void
opengl_renderer::text_to_strlist (const std::string& txt,
                                  std::list<text_renderer::string>& lst,
                                  Matrix& bbox,
                                  int halign, int valign, double rotation)
{
  m_txt_renderer.text_to_strlist (txt, lst, bbox, halign, valign,
                                  rotation, m_interpreter);
}

Matrix
opengl_renderer::render_text (const std::string& txt,
                              double x, double y, double z,
                              int halign, int valign, double rotation)
{
#if defined (HAVE_OPENGL)

  Matrix bbox (1, 4, 0.0);

  if (txt.empty ())
    return bbox;

  if (m_txt_renderer.ok ())
    {
      uint8NDArray pixels;
      text_to_pixels (txt, pixels, bbox, halign, valign, rotation);

      render_text (pixels, bbox, x, y, z, rotation);
    }

  return bbox;

#else

  octave_unused_parameter (txt);
  octave_unused_parameter (x);
  octave_unused_parameter (y);
  octave_unused_parameter (z);
  octave_unused_parameter (halign);
  octave_unused_parameter (valign);
  octave_unused_parameter (rotation);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

void
opengl_renderer::render_text (uint8NDArray pixels, Matrix bbox,
                              double x, double y, double z, double rotation)
{
#if defined (HAVE_OPENGL)

  // Transform data coordinates to screen pixel ortho coordinates
  ColumnVector pixpos = get_transform ().transform (x, y, z, false);
  Matrix xdata(1, 2, bbox(0) / m_devpixratio);
  xdata(1) += (bbox(2) - 1) / m_devpixratio;
  Matrix ydata(1, 2, -bbox(1) / m_devpixratio);
  ydata(1) -= (bbox(3) - 1) / m_devpixratio;

  bool blend = m_glfcns.glIsEnabled (GL_BLEND);
  m_glfcns.glEnable (GL_BLEND);
  m_glfcns.glEnable (GL_ALPHA_TEST);

  set_ortho_coordinates ();

  // Translate coordinates so that the text anchor is (0,0)
  m_glfcns.glTranslated (pixpos(0), pixpos(1), -pixpos(2));

  m_glfcns.glRotated (-rotation, 0.0, 0.0, 1.0);

  // Permute pixels returned by freetype
  Array<octave_idx_type> perm (dim_vector (3, 1));
  perm(0) = 2;
  perm(1) = 1;
  perm(2) = 0;
  draw_texture_image (pixels.permute (perm),
                      xdata, ydata, true);

  restore_previous_coordinates ();

  m_glfcns.glDisable (GL_ALPHA_TEST);

  if (! blend)
    m_glfcns.glDisable (GL_BLEND);

#else

  octave_unused_parameter (pixels);
  octave_unused_parameter (bbox);
  octave_unused_parameter (x);
  octave_unused_parameter (y);
  octave_unused_parameter (z);
  octave_unused_parameter (rotation);

  // This shouldn't happen because construction of opengl_renderer
  // objects is supposed to be impossible if OpenGL is not available.

  panic_impossible ();

#endif
}

OCTAVE_END_NAMESPACE(octave)
