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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if defined (HAVE_OPENGL)

#include <iostream>

#include <lo-mappers.h>
#include "oct-locbuf.h"
#include "gl-render.h"
#include "txt-eng.h"
#include "txt-eng-ft.h"

#define LIGHT_MODE GL_FRONT_AND_BACK

// Win32 API requires the CALLBACK attributes for
// GLU callback functions. Define it to empty on
// other platforms.
#ifndef CALLBACK
#define CALLBACK
#endif

enum {
  AXE_ANY_DIR   = 0,
  AXE_DEPTH_DIR = 1,
  AXE_HORZ_DIR  = 2,
  AXE_VERT_DIR  = 3
};

static octave_idx_type
xmin (octave_idx_type x, octave_idx_type y)
{
  return x < y ? x : y;
}

class
opengl_texture
{
protected:
  class texture_rep
  {
  public:
    texture_rep (void) : valid (false), count (1) { }

    texture_rep (GLuint id_arg, int w_arg, int h_arg, int tw_arg, int th_arg)
	: id (id_arg), w (w_arg), h (h_arg), tw (tw_arg), th (th_arg),
	  tx (double(w)/tw), ty (double(h)/th), valid (true),
	  count (1) { }

    ~texture_rep (void)
      {
	if (valid)
	  glDeleteTextures (1, &id);
      }

    void bind (int mode) const
      { if (valid) glBindTexture (mode, id); }

    void tex_coord (double q, double r) const
      { if (valid) glTexCoord2d (q*tx, r*ty); }

    GLuint id;
    int w, h;
    int tw, th;
    double tx, ty;
    bool valid;
    int count;
  };

  texture_rep *rep;

private:
  opengl_texture (texture_rep *_rep) : rep (_rep) { }

public:
  opengl_texture (void) : rep (new texture_rep ()) { }

  opengl_texture (const opengl_texture& tx)
      : rep (tx.rep)
    {
      rep->count++;
    }

  ~opengl_texture (void)
    {
      if (--rep->count == 0)
	delete rep;
    }

  opengl_texture& operator = (const opengl_texture& tx)
    {
      if (--rep->count == 0)
	delete rep;

      rep = tx.rep;
      rep->count++;

      return *this;
    }

  static opengl_texture create (const octave_value& data);

  void bind (int mode = GL_TEXTURE_2D) const
    { rep->bind (mode); }

  void tex_coord (double q, double r) const
    { rep->tex_coord (q, r); }
  
  bool is_valid (void) const
    { return rep->valid; }
};

static int
next_power_of_2 (int n)
{
  int m = 1;

  while (m < n && m < INT_MAX)
    m <<= 1;

  return m;
}

opengl_texture
opengl_texture::create (const octave_value& data)
{
  opengl_texture retval;

  dim_vector dv (data.dims ());

  // Expect RGB data
  if (dv.length () == 3 && dv(2) == 3)
    {
      // FIXME -- dim_vectors hold octave_idx_type values.  Should we
      // check for dimensions larger than intmax?
      int h = dv(0), w = dv(1), tw, th;
      GLuint id;
      bool ok = true;

      tw = next_power_of_2 (w);
      th = next_power_of_2 (w);

      glGenTextures (1, &id);
      glBindTexture (GL_TEXTURE_2D, id);

      if (data.is_double_type ())
	{
	  const NDArray xdata = data.array_value ();

	  OCTAVE_LOCAL_BUFFER (float, a, (3*tw*th));

	  for (int i = 0; i < h; i++)
	    {
	      for (int j = 0, idx = i*tw*3; j < w; j++, idx += 3)
		{
		  a[idx]   = xdata(i,j,0);
		  a[idx+1] = xdata(i,j,1);
		  a[idx+2] = xdata(i,j,2);
		}
	    }

	  glTexImage2D (GL_TEXTURE_2D, 0, 3, tw, th, 0,
			GL_RGB, GL_FLOAT, a);
	}
      else if (data.is_uint8_type ())
	{
	  const uint8NDArray xdata = data.uint8_array_value ();

	  OCTAVE_LOCAL_BUFFER (octave_uint8, a, (3*tw*th));

	  for (int i = 0; i < h; i++)
	    {
	      for (int j = 0, idx = i*tw*3; j < w; j++, idx += 3)
		{
		  a[idx]   = xdata(i,j,0);
		  a[idx+1] = xdata(i,j,1);
		  a[idx+2] = xdata(i,j,2);
		}
	    }

	  glTexImage2D (GL_TEXTURE_2D, 0, 3, tw, th, 0,
			GL_RGB, GL_UNSIGNED_BYTE, a);
	}
      else
	{
	  ok = false;
	  warning ("opengl_texture::create: invalid texture data type (expected double or uint8)");
	}

      if (ok)
	{
	  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

	  if (glGetError () != GL_NO_ERROR)
	    warning ("opengl_texture::create: OpenGL error while generating texture data");
	  else
	    retval = opengl_texture (new texture_rep (id, w, h, tw, th));
	}
    }
  else
    warning ("opengl_texture::create: invalid texture data size");

  return retval;
}

class
opengl_tesselator
{
public:
#if defined (HAVE_FRAMEWORK_OPENGL) && defined (HAVE_GLUTESSCALLBACK_THREEDOTS)
  typedef GLvoid (CALLBACK *fcn) (...);
#else
  typedef void (CALLBACK *fcn) (void);
#endif

public:

  opengl_tesselator (void) : glu_tess (0) { init (); }

  virtual ~opengl_tesselator (void)
    { if (glu_tess) gluDeleteTess (glu_tess); }

  void begin_polygon (bool filled = true)
    {
      gluTessProperty (glu_tess, GLU_TESS_BOUNDARY_ONLY,
		       (filled ? GL_FALSE : GL_TRUE));
      fill = filled;
      gluTessBeginPolygon (glu_tess, this);
    }

  void end_polygon (void) const
    { gluTessEndPolygon (glu_tess); }

  void begin_contour (void) const
    { gluTessBeginContour (glu_tess); }

  void end_contour (void) const
    { gluTessEndContour (glu_tess); }

  void add_vertex (double *loc, void *data) const
    { gluTessVertex (glu_tess, loc, data); }

protected:
  virtual void begin (GLenum /*type*/) { }

  virtual void end (void) { }

  virtual void vertex (void */*data*/) { }

  virtual void combine (GLdouble /*c*/[3], void */*data*/[4],
			GLfloat /*w*/[4], void **/*out_data*/) { }

  virtual void edge_flag (GLboolean /*flag*/) { }

  virtual void error (GLenum err)
    { ::error ("OpenGL tesselation error (%d)", err); }

  virtual void init (void)
    {
      glu_tess = gluNewTess ();

      gluTessCallback (glu_tess, GLU_TESS_BEGIN_DATA,
		       reinterpret_cast<fcn> (tess_begin));
      gluTessCallback (glu_tess, GLU_TESS_END_DATA,
		       reinterpret_cast<fcn> (tess_end));
      gluTessCallback (glu_tess, GLU_TESS_VERTEX_DATA,
		       reinterpret_cast<fcn> (tess_vertex));
      gluTessCallback (glu_tess, GLU_TESS_COMBINE_DATA,
		       reinterpret_cast<fcn> (tess_combine));
      gluTessCallback (glu_tess, GLU_TESS_EDGE_FLAG_DATA,
		       reinterpret_cast<fcn> (tess_edge_flag));
      gluTessCallback (glu_tess, GLU_TESS_ERROR_DATA,
		       reinterpret_cast<fcn> (tess_error));
    }

  bool is_filled (void) const { return fill; }

private:
  static void CALLBACK tess_begin (GLenum type, void *t)
    { reinterpret_cast<opengl_tesselator *> (t)->begin (type); }
  
  static void CALLBACK tess_end (void *t)
    { reinterpret_cast<opengl_tesselator *> (t)->end (); }
  
  static void CALLBACK tess_vertex (void *v, void *t)
    { reinterpret_cast<opengl_tesselator *> (t)->vertex (v); }
  
  static void CALLBACK tess_combine (GLdouble c[3], void *v[4], GLfloat w[4],
				     void **out,  void *t)
    { reinterpret_cast<opengl_tesselator *> (t)->combine (c, v, w, out); }
  
  static void CALLBACK tess_edge_flag (GLboolean flag, void *t)
    { reinterpret_cast<opengl_tesselator *> (t)->edge_flag (flag); }
  
  static void CALLBACK tess_error (GLenum err, void *t)
    { reinterpret_cast<opengl_tesselator *> (t)->error (err); }

private:
  GLUtesselator *glu_tess;
  bool fill;
};

class
vertex_data
{
public:
  class vertex_data_rep
  {
  public:
    Matrix coords;
    Matrix color;
    Matrix normal;
    double alpha;
    float ambient;
    float diffuse;
    float specular;
    float specular_exp;

    // reference counter
    int count;

    vertex_data_rep (void) : count (1) { }

    vertex_data_rep (const Matrix& c, const Matrix& col, const Matrix& n,
		     double a, float as, float ds, float ss, float se)
	: coords (c), color (col), normal (n), alpha (a),
	  ambient (as), diffuse (ds), specular (ss), specular_exp (se),
	  count (1) { }
  };

private:
  vertex_data_rep *rep;

  vertex_data_rep *nil_rep (void) const
    {
      static vertex_data_rep *nr = new vertex_data_rep ();

      return nr;
    }

public:
  vertex_data (void) : rep (nil_rep ())
    { rep->count++; }

  vertex_data (const vertex_data& v) : rep (v.rep)
    { rep->count++; }

  vertex_data (const Matrix& c, const Matrix& col, const Matrix& n,
	       double a, float as, float ds, float ss, float se)
      : rep (new vertex_data_rep (c, col, n, a, as, ds, ss, se))
    { }

  vertex_data (vertex_data_rep *new_rep)
      : rep (new_rep) { }

  ~vertex_data (void)
    {
      if (--rep->count == 0)
	delete rep;
    }

  vertex_data& operator = (const vertex_data& v)
    {
      if (--rep->count == 0)
	delete rep;

      rep = v.rep;
      rep->count++;

      return *this;
    }

  vertex_data_rep *get_rep (void) const { return rep; }
};

class
opengl_renderer::patch_tesselator : public opengl_tesselator
{
public:
  patch_tesselator (opengl_renderer *r, int cmode, int lmode, int idx = 0)
      : opengl_tesselator (), renderer (r),
        color_mode (cmode), light_mode (lmode), index (idx),
        first (true) { }

protected:
  void begin (GLenum type)
    {
      //printf("patch_tesselator::begin (%d)\n", type);
      first = true;

      if (color_mode == 2 || light_mode == 2)
	glShadeModel (GL_SMOOTH);
      else
	glShadeModel (GL_FLAT);

      if (is_filled ())
	renderer->set_polygon_offset (true, 1+index);

      glBegin (type);
    }

  void end (void)
    {
      //printf("patch_tesselator::end\n");
      glEnd ();
      renderer->set_polygon_offset (false);
    }

  void vertex (void *data)
    {
      vertex_data::vertex_data_rep *v
	  = reinterpret_cast<vertex_data::vertex_data_rep *> (data);
      //printf("patch_tesselator::vertex (%g, %g, %g)\n", v->coords(0), v->coords(1), v->coords(2));

      // FIXME: why did I need to keep the first vertex of the face
      // in JHandles? I think it's related to the fact that the 
      // tessellation process might re-order the vertices, such that
      // the first one you get here might not be the first one of the face;
      // but I can't figure out the actual reason.
      if (color_mode > 0 && (first || color_mode == 2))
	{
	  Matrix col = v->color;

	  if (col.numel () == 3)
	    {
	      glColor3dv (col.data ());
	      if (light_mode > 0)
		{
		  float buf[4] = { 0, 0, 0, 1 };

		  for (int k = 0; k < 3; k++)
		    buf[k] = (v->ambient * col(k));
		  glMaterialfv (LIGHT_MODE, GL_AMBIENT, buf);

		  for (int k = 0; k < 3; k++)
		    buf[k] = (v->diffuse * col(k));
		  glMaterialfv (LIGHT_MODE, GL_AMBIENT, buf);
		}
	    }
	}

      if (light_mode > 0 && (first || light_mode == 2))
	glNormal3dv (v->normal.data ());

      glVertex3dv (v->coords.data ());

      first = false;
    }

  void combine (GLdouble xyz[3], void *data[4], GLfloat w[4],
		void **out_data)
    {
      //printf("patch_tesselator::combine\n");

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
      Matrix nn (1, 3, 0.0);
      double aa = 0.0;

      vv(0) = xyz[0];
      vv(1) = xyz[1];
      vv(2) = xyz[2];

      if (v[0]->color.numel ())
	{
	  cc.resize (1, 3, 0.0);
	  for (int ic = 0; ic < 3; ic++)
	    for (int iv = 0; iv < vmax; iv++)
	      cc(ic) += (w[iv] * v[iv]->color(ic));
	}

      if (v[0]->normal.numel () > 0)
	{
	  for (int in = 0; in < 3; in++)
	    for (int iv = 0; iv < vmax; iv++)
	      nn(in) += (w[iv] * v[iv]->normal(in));
	}

      for (int iv = 0; iv < vmax; iv++)
	aa += (w[iv] * v[iv]->alpha);

      vertex_data new_v (vv, cc, nn, aa, v[0]->ambient, v[0]->diffuse,
			 v[0]->specular, v[0]->specular_exp);
      tmp_vdata.push_back (new_v);

      *out_data = new_v.get_rep ();
    }

private:
  opengl_renderer *renderer;
  int color_mode;	// 0: uni,  1: flat, 2: interp
  int light_mode;	// 0: none, 1: flat, 2: gouraud
  int index;
  bool first;
  std::list<vertex_data> tmp_vdata;
};

void
opengl_renderer::draw (const graphics_object& go)
{
  if (! go.valid_object ())
    return;
  
  const base_properties& props = go.get_properties ();

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
  else if (go.isa ("hggroup"))
    draw_hggroup (dynamic_cast<const hggroup::properties&> (props));
  else if (go.isa ("text"))
    draw_text (dynamic_cast<const text::properties&> (props));
  else if (go.isa ("image"))
    draw_image (dynamic_cast<const image::properties&> (props));
  else
    warning ("opengl_renderer: cannot render object of type `%s'",
	     props.graphics_object_name ().c_str ());
}

void
opengl_renderer::draw_figure (const figure::properties& props)
{
  backend = props.get_backend ();

  // Initialize OpenGL context

  glEnable (GL_DEPTH_TEST);
  glDepthFunc (GL_LEQUAL);
  glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glAlphaFunc (GL_GREATER, 0.0f);
  glEnable (GL_NORMALIZE);

  if (props.is___enhanced__ ())
    {
      glEnable (GL_BLEND);
      glEnable (GL_LINE_SMOOTH);
    }
  else
    {
      glDisable (GL_BLEND);
      glDisable (GL_LINE_SMOOTH);
    }

  // Clear background

  Matrix c = props.get_color_rgb ();

  if (c.length() >= 3)
    {
      glClearColor (c(0), c(1), c(2), 1);
      glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    }

  // Draw children

  draw (props.get_all_children ());
}

void
opengl_renderer::draw_axes (const axes::properties& props)
{
  // setup OpenGL transformation

  Matrix x_zlim = props.get_transform_zlim ();
  Matrix x_mat1 = props.get_opengl_matrix_1 ();
  Matrix x_mat2 = props.get_opengl_matrix_2 ();
  
  xZ1 = x_zlim(0)-(x_zlim(1)-x_zlim(0))/2;
  xZ2 = x_zlim(1)+(x_zlim(1)-x_zlim(0))/2;

#if defined (HAVE_FRAMEWORK_OPENGL)
  GLint vw[4];
#else
  int vw[4];
#endif

  glGetIntegerv (GL_VIEWPORT, vw);

  glMatrixMode (GL_MODELVIEW);
  glLoadIdentity ();
  glScaled(1, 1, -1);
  glMultMatrixd (x_mat1.data ());
  glMatrixMode (GL_PROJECTION);
  glLoadIdentity ();
  glOrtho (0, vw[2], vw[3], 0, xZ1, xZ2);
  glMultMatrixd (x_mat2.data ());
  glMatrixMode (GL_MODELVIEW);

  glClear (GL_DEPTH_BUFFER_BIT);

  // store axes transformation data

  xform = props.get_transform ();
  
  // draw axes object

  GLboolean antialias;
  glGetBooleanv (GL_LINE_SMOOTH, &antialias);
  glDisable (GL_LINE_SMOOTH);
  
  Matrix xlim = xform.xscale (props.get_xlim ().matrix_value ());
  Matrix ylim = xform.yscale (props.get_ylim ().matrix_value ());
  Matrix zlim = xform.zscale (props.get_zlim ().matrix_value ());
  double x_min = xlim(0), x_max = xlim(1);
  double y_min = ylim(0), y_max = ylim(1);
  double z_min = zlim(0), z_max = zlim(1);
  
  double xd = (props.xdir_is ("normal") ? 1 : -1);
  double yd = (props.ydir_is ("normal") ? 1 : -1);
  double zd = (props.zdir_is ("normal") ? 1 : -1);

  ColumnVector p1, p2, xv (3), yv (3), zv (3);
  int xstate, ystate, zstate;

  xstate = ystate = zstate = AXE_ANY_DIR;

  p1 = xform.transform (x_min, (y_min+y_max)/2, (z_min+z_max)/2, false);
  p2 = xform.transform (x_max, (y_min+y_max)/2, (z_min+z_max)/2, false);
  xv(0) = xround (p2(0)-p1(0));
  xv(1) = xround (p2(1)-p1(1));
  xv(2) = (p2(2)-p1(2));
  if (xv(0) == 0 && xv(1) == 0)
    xstate = AXE_DEPTH_DIR;
  else if (xv(2) == 0)
    {
      if (xv(0) == 0)
        xstate = AXE_VERT_DIR;
      else if (xv(1) == 0)
        xstate = AXE_HORZ_DIR;
    }
  double xPlane;
  if (xv(2) == 0)
    if (xv(1) == 0)
      xPlane = (xv(0) > 0 ? x_max : x_min);
    else
      xPlane = (xv(1) < 0 ? x_max : x_min);
  else
    xPlane = (xv(2) < 0 ? x_min : x_max);
  double xPlaneN = (xPlane == x_min ? x_max : x_min);
  double fx = (x_max-x_min)/sqrt(xv(0)*xv(0)+xv(1)*xv(1));

  p1 = xform.transform ((x_min+x_max)/2, y_min, (z_min+z_max)/2, false);
  p2 = xform.transform ((x_min+x_max)/2, y_max, (z_min+z_max)/2, false);
  yv(0) = xround (p2(0)-p1(0));
  yv(1) = xround (p2(1)-p1(1));
  yv(2) = (p2(2)-p1(2));
  if (yv(0) == 0 && yv(1) == 0)
    ystate = AXE_DEPTH_DIR;
  else if (yv(2) == 0)
    {
      if (yv(0) == 0)
        ystate = AXE_VERT_DIR;
      else if (yv(1) == 0)
        ystate = AXE_HORZ_DIR;
    }
  double yPlane;
  if (yv(2) == 0)
    if (yv(1) == 0)
      yPlane = (yv(0) > 0 ? y_max : y_min);
    else
      yPlane = (yv(1) < 0 ? y_max : y_min);
  else
    yPlane = (yv(2) < 0 ? y_min : y_max);
  double yPlaneN = (yPlane == y_min ? y_max : y_min);
  double fy = (y_max-y_min)/sqrt(yv(0)*yv(0)+yv(1)*yv(1));

  p1 = xform.transform((x_min+x_max)/2, (y_min+y_max)/2, z_min, false);
  p2 = xform.transform((x_min+x_max)/2, (y_min+y_max)/2, z_max, false);
  zv(0) = xround(p2(0)-p1(0));
  zv(1) = xround (p2(1)-p1(1));
  zv(2) = (p2(2)-p1(2));
  if (zv(0) == 0 && zv(1) == 0)
    zstate = AXE_DEPTH_DIR;
  else if (zv(2) == 0)
  {
    if (zv(0) == 0)
      zstate = AXE_VERT_DIR;
    else if (zv(1) == 0)
      zstate = AXE_HORZ_DIR;
  }
  double zPlane;
  if (zv(2) == 0)
    if (zv(1) == 0)
      zPlane = (zv(0) > 0 ? z_min : z_max);
    else
      zPlane = (zv(1) < 0 ? z_min : z_max);
  else
    zPlane = (zv(2) < 0 ? z_min : z_max);
  double zPlaneN = (zPlane == z_min ? z_max : z_min);
  double fz = (z_max-z_min)/sqrt(zv(0)*zv(0)+zv(1)*zv(1));

  bool mode2d = (((xstate > AXE_DEPTH_DIR ? 1 : 0) +
        (ystate > AXE_DEPTH_DIR ? 1 : 0) +
        (zstate > AXE_DEPTH_DIR ? 1 : 0)) == 2);
  if (props.tickdirmode_is ("auto"))
  {
    // FIXME: tickdir should be updated (code below comes
    //        from JHandles)
    //autoMode++;
    //TickDir.set(mode2d ? "in" : "out", true);
    //autoMode--;
  }

  // FIXME: use ticklength property
  double xticklen = 7, yticklen = 7, zticklen = 7;

  //double tickdir = (props.tickdir_is ("in") ? -1 : 1);
  double tickdir = (props.tickdirmode_is ("auto") ?
		    (mode2d ? -1 : 1) :
		    (props.tickdir_is ("in") ? -1 : 1));
  double xtickoffset = (mode2d && tickdir < 0 ? 0 : xticklen) + 5;
  double ytickoffset = (mode2d && tickdir < 0 ? 0 : yticklen) + 5;
  double ztickoffset = (mode2d && tickdir < 0 ? 0 : zticklen) + 5;

  bool xySym = (xd*yd*(xPlane-xPlaneN)*(yPlane-yPlaneN) > 0);
  bool x2Dtop = false;
  bool y2Dright = false;
  double zpTick = zPlane;

  /* 2D mode */
  if (xstate == AXE_HORZ_DIR && ystate == AXE_VERT_DIR)
  {
    if (props.xaxislocation_is ("top"))
    {
      double tmp = yPlane;
      yPlane = yPlaneN;
      yPlaneN = tmp;
      x2Dtop = true;
    }
    if (props.yaxislocation_is ("right"))
    {
      double tmp = xPlane;
      xPlane = xPlaneN;
      xPlaneN = tmp;
      y2Dright = true;
    }
    if (props.layer_is ("top"))
      zpTick = zPlaneN;
  }

  Matrix axe_color = props.get_color_rgb ();
  bool visible = props.is_visible ();
  bool box = props.is_box ();

  // Axes planes

  if (axe_color.numel () > 0 && visible)
    {
      set_color (axe_color);
      set_polygon_offset (true, 2.5);

      glBegin (GL_QUADS);

      // X plane
      glVertex3d (xPlane, y_min, z_min);
      glVertex3d (xPlane, y_max, z_min);
      glVertex3d (xPlane, y_max, z_max);
      glVertex3d (xPlane, y_min, z_max);

      // Y plane
      glVertex3d (x_min, yPlane, z_min);
      glVertex3d (x_max, yPlane, z_min);
      glVertex3d (x_max, yPlane, z_max);
      glVertex3d (x_min, yPlane, z_max);

      // Z plane
      glVertex3d (x_min, y_min, zPlane);
      glVertex3d (x_max, y_min, zPlane);
      glVertex3d (x_max, y_max, zPlane);
      glVertex3d (x_min, y_max, zPlane);

      glEnd ();

      set_polygon_offset (false);
    }

  // Axes box

  set_linestyle ("-", true);
  set_linewidth (props.get_linewidth ());

  if (visible)
    {
      glBegin (GL_LINES);

      // X box
      set_color (props.get_xcolor_rgb ());
      glVertex3d (xPlaneN, yPlaneN, zPlane);
      glVertex3d (xPlane, yPlaneN, zPlane);
      if (box)
        {
          glVertex3d (xPlaneN, yPlane, zPlane);
          glVertex3d (xPlane, yPlane, zPlane);
          glVertex3d (xPlaneN, yPlane, zPlaneN);
          glVertex3d (xPlane, yPlane, zPlaneN);
          glVertex3d (xPlaneN, yPlaneN, zPlaneN);
          glVertex3d (xPlane, yPlaneN, zPlaneN);
        }

      // Y box
      set_color (props.get_ycolor_rgb ());
      glVertex3d (xPlaneN, yPlaneN, zPlane);
      glVertex3d (xPlaneN, yPlane, zPlane);
      if (box)
        {
          glVertex3d (xPlane, yPlaneN, zPlane);
          glVertex3d (xPlane, yPlane, zPlane);
          glVertex3d (xPlane, yPlaneN, zPlaneN);
          glVertex3d (xPlane, yPlane, zPlaneN);
          glVertex3d (xPlaneN, yPlaneN, zPlaneN);
          glVertex3d (xPlaneN, yPlane, zPlaneN);
        }

      // Z box
      set_color (props.get_zcolor_rgb ());
      if (xySym)
        {
          glVertex3d (xPlaneN, yPlane, zPlaneN);
          glVertex3d (xPlaneN, yPlane, zPlane);
        }
      else
        {
          glVertex3d (xPlane, yPlaneN, zPlaneN);
          glVertex3d (xPlane, yPlaneN, zPlane);
        }
      if (box)
        {
          glVertex3d (xPlane, yPlane, zPlaneN);
          glVertex3d (xPlane, yPlane, zPlane);
          if (xySym)
            {
              glVertex3d (xPlane, yPlaneN, zPlaneN);
              glVertex3d (xPlane, yPlaneN, zPlane);
            }
          else
            {
              glVertex3d (xPlaneN, yPlane, zPlaneN);
              glVertex3d (xPlaneN, yPlane, zPlane);
            }
          glVertex3d (xPlaneN, yPlaneN, zPlaneN);
          glVertex3d (xPlaneN, yPlaneN, zPlane);
        }

      glEnd ();
    }

  std::string gridstyle = props.get_gridlinestyle ();
  std::string minorgridstyle = props.get_minorgridlinestyle ();

  set_font (props);

  // X grid

  if (visible && xstate != AXE_DEPTH_DIR)
    {
      bool do_xgrid = (props.is_xgrid () && (gridstyle != "none"));
      bool do_xminorgrid = (props.is_xminorgrid () && (minorgridstyle != "none"));
      bool do_xminortick = props.is_xminortick ();
      Matrix xticks = xform.xscale (props.get_xtick ().matrix_value ());
      // FIXME: use pre-computed minor ticks
      Matrix xmticks;
      string_vector xticklabels = props.get_xticklabel ().all_strings ();
      int wmax = 0, hmax = 0;
      bool tick_along_z = xisinf (fy);
      Matrix tickpos (xticks.numel (), 3);

      set_color (props.get_xcolor_rgb ());

      // grid lines
      if (do_xgrid)
        {
          set_linestyle (gridstyle, true);
          glBegin (GL_LINES);
          for (int i = 0; i < xticks.numel (); i++)
            {
              double xval = xticks(i);

              glVertex3d (xval, yPlaneN, zpTick);
              glVertex3d (xval, yPlane, zpTick);
              if (zstate != AXE_DEPTH_DIR)
                {
                  glVertex3d (xval, yPlane, zPlaneN);
                  glVertex3d (xval, yPlane, zPlane);
                }
            }
          glEnd ();
          set_linestyle ("-", true);
        }

      // tick marks
      if (tick_along_z)
        {
          glBegin (GL_LINES);
          for (int i = 0; i < xticks.numel (); i++)
            {
              double xval = xticks(i);

              glVertex3d (xval, yPlaneN, zPlane);
              glVertex3d (xval, yPlaneN, zPlane+signum(zPlane-zPlaneN)*fz*xticklen*tickdir);
              if (box && xstate != AXE_ANY_DIR)
                {
                  glVertex3d (xval, yPlaneN, zPlaneN);
                  glVertex3d (xval, yPlaneN,
                        zPlaneN+signum(zPlaneN-zPlane)*fz*xticklen*tickdir);
                }
              tickpos(i,0) = xval;
              tickpos(i,1) = yPlaneN;
              tickpos(i,2) = zPlane+signum(zPlane-zPlaneN)*fz*xtickoffset;
            }
          glEnd ();
        }
      else
        {
          glBegin (GL_LINES);
          for (int i = 0; i < xticks.numel (); i++)
            {
              double xval = xticks(i);

              glVertex3d (xval, yPlaneN, zpTick);
              glVertex3d (xval, yPlaneN+signum(yPlaneN-yPlane)*fy*xticklen*tickdir, zpTick);
              if (box && xstate != AXE_ANY_DIR)
                {
                  glVertex3d (xval, yPlane, zpTick);
                  glVertex3d (xval,
                        yPlane+signum(yPlane-yPlaneN)*fy*xticklen*tickdir, zpTick);
                }
              tickpos(i,0) = xval;
              tickpos(i,1) = yPlaneN+signum(yPlaneN-yPlane)*fy*xtickoffset;
              tickpos(i,2) = zPlane;
            }
          glEnd ();
        }

      // tick texts
      if (xticklabels.numel () > 0)
	{
	  int n = std::min (xticklabels.numel (), xticks.numel ());
	  int halign = (xstate == AXE_HORZ_DIR ? 1 : (xySym ? 0 : 2));
	  int valign = (xstate == AXE_VERT_DIR
			? 1
		       : (zd*zv(2) <= 0 && !x2Dtop ? 2 : 0));

	  for (int i = 0; i < n; i++)
	    {
	      // FIXME: as tick text is transparent, shouldn't be
	      //        drawn after axes object, for correct rendering?
	      Matrix b = render_text (xticklabels(i),
				    tickpos(i,0), tickpos(i,1), tickpos(i,2),
				    halign, valign); 

	      wmax = std::max (wmax, static_cast<int> (b(2)));
	      hmax = std::max (hmax, static_cast<int> (b(3)));
	    }
	}

      // minor grid lines
      if (do_xminorgrid)
        {
          set_linestyle (minorgridstyle, true);
          glBegin (GL_LINES);
          for (int i = 0; i < xmticks.numel (); i++)
            {
              double xval = xmticks(i);

              glVertex3d (xval, yPlaneN, zpTick);
              glVertex3d (xval, yPlane, zpTick);
              if (zstate != AXE_DEPTH_DIR)
                {
                  glVertex3d (xval, yPlane, zPlaneN);
                  glVertex3d (xval, yPlane, zPlane);
                }
            }
          glEnd ();
          set_linestyle ("-", true);
        }
			
      // minor tick marks
      if (do_xminortick)
        {
          if (tick_along_z)
            {
              glBegin (GL_LINES);
              for (int i = 0; i < xmticks.numel (); i++)
                {
                  double xval = xmticks(i);

                  glVertex3d (xval, yPlaneN, zPlane);
                  glVertex3d (xval, yPlaneN,
                      zPlane+signum(zPlane-zPlaneN)*fz*xticklen/2*tickdir);
                  if (box && xstate != AXE_ANY_DIR)
                    {
                      glVertex3d (xval, yPlaneN, zPlaneN);
                      glVertex3d (xval, yPlaneN,
                          zPlaneN+signum(zPlaneN-zPlane)*fz*xticklen/2*tickdir);
                    }
                }
              glEnd ();
            }
          else
            {
              glBegin (GL_LINES);
              for (int i = 0; i < xmticks.numel (); i++)
                {
                  double xval = xmticks(i);

                  glVertex3d (xval, yPlaneN, zpTick);
                  glVertex3d (xval,
                        yPlaneN+signum(yPlaneN-yPlane)*fy*xticklen/2*tickdir, zpTick);
                  if (box && xstate != AXE_ANY_DIR)
                    {
                      glVertex3d (xval, yPlane, zpTick);
                      glVertex3d (xval,
                            yPlane+signum(yPlane-yPlaneN)*fy*xticklen/2*tickdir, zpTick);
                    }
                }
              glEnd ();
            }
        }

      text::properties& xlabel_props =
        reinterpret_cast<text::properties&> (gh_manager::get_object (props.get_xlabel ()).get_properties ());

      xlabel_props.set_visible ("on");

      // FIXME: auto-positioning should be disabled if the 
      //        label has been positioned manually
      if (! xlabel_props.get_string ().empty ())
        {
          xlabel_props.set_horizontalalignment (xstate > AXE_DEPTH_DIR ? "center" : (xySym ? "left" : "right"));
	  xlabel_props.set_verticalalignment (xstate == AXE_VERT_DIR ? "bottom" : (zd*zv(2) <= 0 ? "top" : "bottom"));

          double angle = 0;
          ColumnVector p = graphics_xform::xform_vector ((x_min+x_max)/2, yPlaneN, zPlane);

          if (tick_along_z)
            p(2) += (signum(zPlane-zPlaneN)*fz*xtickoffset);
          else
            p(1) += (signum(yPlaneN-yPlane)*fy*xtickoffset);
          p = xform.transform (p(0), p(1), p(2), false);
          switch (xstate)
            {
              case AXE_ANY_DIR:
                p(0) += (xySym ? wmax : -wmax);
                p(1) += (zd*zv(2) <= 0 ? hmax : -hmax);
                break;
              case AXE_VERT_DIR:
                p(0) -= wmax;
                angle = 90;
                break;
              case AXE_HORZ_DIR:
                p(1) += hmax;
                break;
            }
          p = xform.untransform (p(0), p(1), p(2), true);
          xlabel_props.set_position (p.extract_n (0, 3).transpose ());
          xlabel_props.set_rotation (angle);
        }
    }
  else
    {
      gh_manager::get_object (props.get_xlabel ()).set ("visible", "off");
    }

  // Y grid
		
  if (ystate != AXE_DEPTH_DIR && visible)
    {
      bool do_ygrid = (props.is_ygrid () && (gridstyle != "none"));
      bool do_yminorgrid = (props.is_yminorgrid () && (minorgridstyle != "none"));
      bool do_yminortick = props.is_yminortick ();
      Matrix yticks = xform.yscale (props.get_ytick ().matrix_value ());
      // FIXME: use pre-computed minor ticks
      Matrix ymticks;
      string_vector yticklabels = props.get_yticklabel ().all_strings ();
      int wmax = 0, hmax = 0;
      bool tick_along_z = xisinf (fx);
      Matrix tickpos (yticks.numel (), 3);

      set_color (props.get_ycolor_rgb ());

      // grid lines
      if (do_ygrid)
        {
          set_linestyle (gridstyle, true);
          glBegin (GL_LINES);
          for (int i = 0; i < yticks.numel (); i++)
            {
              double yval = yticks(i);

              glVertex3d (xPlaneN, yval, zpTick);
              glVertex3d (xPlane, yval, zpTick);
              if (zstate != AXE_DEPTH_DIR)
                {
                  glVertex3d (xPlane, yval, zPlaneN);
                  glVertex3d (xPlane, yval, zPlane);
                }
            }
          glEnd ();
          set_linestyle ("-", true);
        }

      // tick marks
      if (tick_along_z)
        {
          glBegin (GL_LINES);
          for (int i = 0; i < yticks.numel (); i++)
            {
              double yval = yticks(i);

              glVertex3d (xPlaneN, yval, zPlane);
              glVertex3d (xPlaneN, yval, zPlane+signum(zPlane-zPlaneN)*fz*yticklen*tickdir);
              if (box && ystate != AXE_ANY_DIR)
                {
                  glVertex3d (xPlaneN, yval, zPlaneN);
                  glVertex3d (xPlaneN, yval,
                        zPlaneN+signum(zPlaneN-zPlane)*fz*yticklen*tickdir);
                }
              tickpos(i,0) = xPlaneN;
              tickpos(i,1) = yval;
              tickpos(i,2) = zPlane+signum(zPlane-zPlaneN)*fz*ytickoffset;
            }
          glEnd ();
        }
      else
        {
          glBegin (GL_LINES);
          for (int i = 0; i < yticks.numel (); i++)
            {
              double yval = yticks(i);

              glVertex3d (xPlaneN, yval, zpTick);
              glVertex3d (xPlaneN+signum(xPlaneN-xPlane)*fx*yticklen*tickdir, yval, zpTick);
              if (box && ystate != AXE_ANY_DIR)
                {
                  glVertex3d (xPlane, yval, zpTick);
                  glVertex3d (xPlane+signum(xPlane-xPlaneN)*fx*yticklen*tickdir,
                        yval, zpTick);
                }
              tickpos(i,0) = xPlaneN+signum(xPlaneN-xPlane)*fx*ytickoffset;
              tickpos(i,1) = yval;
              tickpos(i,2) = zPlane;
            }
          glEnd ();
        }

      // tick texts
      if (yticklabels.numel () > 0)
	{
	  int n = std::min (yticklabels.numel (), yticks.numel ());
	  int halign = (ystate == AXE_HORZ_DIR ? 1 : (!xySym || y2Dright ? 0 : 2));
	  int valign = (ystate == AXE_VERT_DIR ? 1 : (zd*zv(2) <= 0 ? 2 : 0));

	  for (int i = 0; i < n; i++)
	    {
	      // FIXME: as tick text is transparent, shouldn't be
	      //        drawn after axes object, for correct rendering?
	      Matrix b = render_text (yticklabels(i),
				    tickpos(i,0), tickpos(i,1), tickpos(i,2),
				    halign, valign); 

	      wmax = std::max (wmax, static_cast<int> (b(2)));
	      hmax = std::max (hmax, static_cast<int> (b(3)));
	    }
	}

      // minor grid lines
      if (do_yminorgrid)
        {
          set_linestyle (minorgridstyle, true);
          glBegin (GL_LINES);
          for (int i = 0; i < ymticks.numel (); i++)
            {
              double yval = ymticks(i);

              glVertex3d (xPlaneN, yval, zpTick);
              glVertex3d (xPlane, yval, zpTick);
              if (zstate != AXE_DEPTH_DIR)
                {
                  glVertex3d (xPlane, yval, zPlaneN);
                  glVertex3d (xPlane, yval, zPlane);
                }
            }
          glEnd ();
          set_linestyle ("-", true);
        }

      // minor tick marks
      if (do_yminortick)
        {
          if (tick_along_z)
            {
              glBegin (GL_LINES);
              for (int i = 0; i < ymticks.numel (); i++)
                {
                  double yval = ymticks(i);

                  glVertex3d (xPlaneN, yval, zPlane);
                  glVertex3d (xPlaneN, yval,
                        zPlane+signum(zPlane-zPlaneN)*fz*yticklen/2*tickdir);
                  if (box && ystate != AXE_ANY_DIR)
                    {
                      glVertex3d (xPlaneN, yval, zPlaneN);
                      glVertex3d (xPlaneN, yval,
                            zPlaneN+signum(zPlaneN-zPlane)*fz*yticklen/2*tickdir);
                    }
                }
              glEnd ();
            }
          else
            {
              glBegin (GL_LINES);
              for (int i = 0; i < ymticks.numel (); i++)
                {
                  double yval = ymticks(i);

                  glVertex3d (xPlaneN, yval, zpTick);
                  glVertex3d (xPlaneN+signum(xPlaneN-xPlane)*fx*yticklen/2*tickdir,
                        yval, zpTick);
                  if (box && ystate != AXE_ANY_DIR)
                    {
                      glVertex3d (xPlane, yval, zpTick);
                      glVertex3d (xPlane+signum(xPlane-xPlaneN)*fx*yticklen/2*tickdir,
                            yval, zpTick);
                    }
                }
              glEnd ();
            }
        }

      text::properties& ylabel_props =
        reinterpret_cast<text::properties&> (gh_manager::get_object (props.get_ylabel ()).get_properties ());

      ylabel_props.set_visible ("on");

      // FIXME: auto-positioning should be disabled if the 
      //        label has been positioned manually
      if (! ylabel_props.get_string ().empty ())
        {
          ylabel_props.set_horizontalalignment (ystate > AXE_DEPTH_DIR ? "center" : (!xySym ? "left" : "right"));
	  ylabel_props.set_verticalalignment (ystate == AXE_VERT_DIR ? "bottom" : (zd*zv(2) <= 0 ? "top" : "bottom"));

          double angle = 0;
          ColumnVector p = graphics_xform::xform_vector (xPlaneN, (y_min+y_max)/2, zPlane);

          if (tick_along_z)
            p(2) += (signum(zPlane-zPlaneN)*fz*ytickoffset);
          else
            p(0) += (signum(xPlaneN-xPlane)*fx*ytickoffset);
          p = xform.transform (p(0), p(1), p(2), false);
          switch (ystate)
            {
              case AXE_ANY_DIR:
                p(0) += (!xySym ? wmax : -wmax);
                p(1) += (zd*zv(2) <= 0 ? hmax : -hmax);
                break;
              case AXE_VERT_DIR:
                p(0) -= wmax;
                angle = 90;
                break;
              case AXE_HORZ_DIR:
                p(1) += hmax;
                break;
            }
          p = xform.untransform(p(0), p(1), p(2), true);
          ylabel_props.set_position (p.extract_n (0, 3).transpose ());
          ylabel_props.set_rotation (angle);
        }
    }
  else
    {
      gh_manager::get_object (props.get_ylabel ()).set ("visible", "off");
    }
		
  // Z Grid

  if (zstate != AXE_DEPTH_DIR && visible)
    {
      bool do_zgrid = (props.is_zgrid () && (gridstyle != "none"));
      bool do_zminorgrid = (props.is_zminorgrid () && (minorgridstyle != "none"));
      bool do_zminortick = props.is_zminortick ();
      Matrix zticks = xform.zscale (props.get_ztick ().matrix_value ());
      // FIXME: use pre-computed minor ticks
      Matrix zmticks;
      string_vector zticklabels = props.get_zticklabel ().all_strings ();
      int wmax = 0, hmax = 0;
      Matrix tickpos (zticks.numel (), 3);

      set_color (props.get_zcolor_rgb ());

      // grid lines
      if (do_zgrid)
        {
          set_linestyle (gridstyle, true);
          glBegin (GL_LINES);
          for (int i = 0; i < zticks.numel (); i++)
            {
              double zval = zticks(i);

              glVertex3d (xPlaneN, yPlane, zval);
              glVertex3d (xPlane, yPlane, zval);
              glVertex3d (xPlane, yPlaneN, zval);
              glVertex3d (xPlane, yPlane, zval);
            }
          glEnd ();
          set_linestyle ("-", true);
        }

      // tick marks
      if (xySym)
        {
          if (xisinf (fy))
            {
              glBegin (GL_LINES);
              for (int i = 0; i < zticks.numel (); i++)
                {
                  double zval = zticks(i);

                  glVertex3d (xPlaneN, yPlane, zval);
                  glVertex3d (xPlaneN+signum(xPlaneN-xPlane)*fx*zticklen*tickdir,
                        yPlane, zval);
                  if (box && zstate != AXE_ANY_DIR)
                    {
                      glVertex3d (xPlane, yPlane, zval);
                      glVertex3d (xPlane+signum(xPlane-xPlaneN)*fx*zticklen*tickdir,
                            yPlane, zval);
                    }
                  tickpos(i,0) = xPlaneN+signum(xPlaneN-xPlane)*fx*ztickoffset;
                  tickpos(i,1) = yPlane;
                  tickpos(i,2) = zval;
                }
              glEnd ();
            }
          else
            {
              glBegin (GL_LINES);
              for (int i = 0; i < zticks.numel (); i++)
                {
                  double zval = zticks(i);

                  glVertex3d (xPlaneN, yPlane, zval);
                  glVertex3d (xPlaneN, yPlane+signum(yPlane-yPlaneN)*fy*zticklen*tickdir, zval);
                  tickpos(i,0) = xPlaneN;
                  tickpos(i,1) = yPlane+signum(yPlane-yPlaneN)*fy*ztickoffset;
                  tickpos(i,2) = zval;
                }
              glEnd ();
            }
        }
      else
        {
          if (xisinf (fx))
            {
              glBegin (GL_LINES);
              for (int i = 0; i < zticks.numel (); i++)
                {
                  double zval = zticks(i);

                  glVertex3d (xPlane, yPlaneN, zval);
                  glVertex3d (xPlane, yPlaneN+signum(yPlaneN-yPlane)*fy*zticklen*tickdir, zval);
                  if (box && zstate != AXE_ANY_DIR)
                    {
                      glVertex3d (xPlane, yPlane, zval);
                      glVertex3d (xPlane, yPlane+signum(yPlane-yPlaneN)*fy*zticklen*tickdir, zval);
                    }
                  tickpos(i,0) = xPlane;
                  tickpos(i,1) = yPlaneN+signum(yPlaneN-yPlane)*fy*ztickoffset;
                  tickpos(i,2) = zval;
                }
              glEnd ();
            }
          else
          {
            glBegin (GL_LINES);
            for (int i = 0; i < zticks.numel (); i++)
              {
                double zval = zticks(i);

                glVertex3d (xPlane, yPlaneN, zval);
                glVertex3d (xPlane+signum(xPlane-xPlaneN)*fx*zticklen*tickdir, yPlaneN, zval);
                tickpos(i,0) = xPlane+signum(xPlane-xPlaneN)*fx*ztickoffset;
                tickpos(i,1) = yPlaneN;
                tickpos(i,2) = zval;
              }
            glEnd ();
          }
        }

      // FIXME: tick texts
      if (zticklabels.numel () > 0)
	{
	  int n = std::min (zticklabels.numel (), zticks.numel ());
	  int halign = 2;
	  int valign = (zstate == AXE_VERT_DIR ? 1 : (zd*zv(2) < 0 ? 3 : 2));

	  for (int i = 0; i < n; i++)
	    {
	      // FIXME: as tick text is transparent, shouldn't be
	      //        drawn after axes object, for correct rendering?
	      Matrix b = render_text (zticklabels(i),
				    tickpos(i,0), tickpos(i,1), tickpos(i,2),
				    halign, valign); 

	      wmax = std::max (wmax, static_cast<int> (b(2)));
	      hmax = std::max (hmax, static_cast<int> (b(3)));
	    }
	}

      // minor grid lines
      if (do_zminorgrid)
        {
          set_linestyle (minorgridstyle, true);
          glBegin (GL_LINES);
          for (int i = 0; i < zmticks.numel (); i++)
            {
              double zval = zmticks(i);

              glVertex3d (xPlaneN, yPlane, zval);
              glVertex3d (xPlane, yPlane, zval);
              glVertex3d (xPlane, yPlaneN, zval);
              glVertex3d (xPlane, yPlane, zval);
            }
          glEnd ();
          set_linestyle ("-", true);
        }

      // minor tick marks
      if (do_zminortick)
        {
          if (xySym)
            {
              if (xisinf (fy))
                {
                  glBegin (GL_LINES);
                  for (int i = 0; i < zmticks.numel (); i++)
                    {
                      double zval = zmticks(i);

                      glVertex3d (xPlaneN, yPlane, zval);
                      glVertex3d (xPlaneN+signum(xPlaneN-xPlane)*fx*zticklen/2*tickdir,
                            yPlane, zval);
                      if (box && zstate != AXE_ANY_DIR)
                        {
                          glVertex3d (xPlane, yPlane, zval);
                          glVertex3d (xPlane+signum(xPlane-xPlaneN)*fx*zticklen/2*tickdir,
                                yPlane, zval);
                        }
                    }
                  glEnd ();
                }
              else
                {
                  glBegin (GL_LINES);
                  for (int i = 0; i < zmticks.numel (); i++)
                    {
                      double zval = zmticks(i);

                      glVertex3d (xPlaneN, yPlane, zval);
                      glVertex3d (xPlaneN, yPlane+signum(yPlane-yPlaneN)*fy*zticklen/2*tickdir, zval);
                    }
                  glEnd ();
                }
            }
          else
            {
              if (xisinf (fx))
                {
                  glBegin (GL_LINES);
                  for (int i = 0; i < zmticks.numel (); i++)
                    {
                      double zval = zmticks(i);

                      glVertex3d (xPlane, yPlaneN, zval);
                      glVertex3d (xPlane, yPlaneN+signum(yPlaneN-yPlane)*fy*zticklen/2*tickdir, zval);
                      if (box && zstate != AXE_ANY_DIR)
                        {
                          glVertex3d (xPlane, yPlane, zval);
                          glVertex3d (xPlane, yPlane+signum(yPlane-yPlaneN)*fy*zticklen/2*tickdir, zval);
                        }
                    }
                  glEnd ();
                }
              else
                {
                  glBegin (GL_LINES);
                  for (int i = 0; i < zmticks.numel (); i++)
                    {
                      double zval = zmticks(i);

                      glVertex3d (xPlane, yPlaneN, zval);
                      glVertex3d (xPlane+signum(xPlane-xPlaneN)*fx*zticklen/2*tickdir, yPlaneN, zval);
                    }
                  glEnd ();
                }
            }
        }

      text::properties& zlabel_props =
        reinterpret_cast<text::properties&> (gh_manager::get_object (props.get_zlabel ()).get_properties ());

      zlabel_props.set_visible ("on");

      // FIXME: auto-positioning should be disabled if the 
      //        label has been positioned manually
      if (! zlabel_props.get_string ().empty ())
        {
          bool camAuto = props.cameraupvectormode_is ("auto");

          zlabel_props.set_horizontalalignment ((zstate > AXE_DEPTH_DIR || camAuto) ? "center" : "right");
	  zlabel_props.set_verticalalignment(zstate == AXE_VERT_DIR ? "bottom" : ((zd*zv(2) < 0 || camAuto) ? "bottom" : "top"));

          double angle = 0;
          ColumnVector p;

          if (xySym)
            {
              p = graphics_xform::xform_vector (xPlaneN, yPlane, (z_min+z_max)/2);
              if (xisinf (fy))
                p(0) += (signum(xPlaneN-xPlane)*fx*ztickoffset);
              else
                p(1) += (signum(yPlane-yPlaneN)*fy*ztickoffset);
            }
          else
            {
              p = graphics_xform::xform_vector (xPlane, yPlaneN, (z_min+z_max)/2);
              if (xisinf (fx))
                p(1) += (signum(yPlaneN-yPlane)*fy*ztickoffset);
              else
                p(0) += (signum(xPlane-xPlaneN)*fx*ztickoffset);
            }
          p = xform.transform (p(0), p(1), p(2), false);
          switch (zstate)
            {
              case AXE_ANY_DIR:
                if (camAuto)
                  {
                    p(0) -= wmax;
                    angle = 90;
                  }
                /* FIXME: what's the correct offset?
                   p[0] += (!xySym ? wmax : -wmax);
                   p[1] += (zd*zv[2] <= 0 ? hmax : -hmax);
                   */
                break;
              case AXE_VERT_DIR:
                p(0) -= wmax;
                angle = 90;
                break;
              case AXE_HORZ_DIR:
                p(1) += hmax;
                break;
            }
          p = xform.untransform (p(0), p(1), p(2), true);
          zlabel_props.set_position (p.extract_n (0, 3).transpose ());
          zlabel_props.set_rotation (angle);
        }
    }
  else
    {
      gh_manager::get_object (props.get_zlabel ()).set ("visible", "off");
    }

  set_linestyle ("-");

  // Title

  text::properties& title_props =
    reinterpret_cast<text::properties&> (gh_manager::get_object (props.get_title ()).get_properties ());
      
  // FIXME: auto-positioning should be disabled if the 
  //        title has been positioned manually
  if (! title_props.get_string ().empty ())
    {
      Matrix bb = props.get_boundingbox (true);
      ColumnVector p = xform.untransform (bb(0)+bb(2)/2, (bb(1)-10),
          (x_zlim(0)+x_zlim(1))/2, true);
      title_props.set_position (p.extract_n(0, 3).transpose ());
    }

  set_clipbox (x_min, x_max, y_min, y_max, z_min, z_max);

  // Children

  if (antialias == GL_TRUE)
    glEnable (GL_LINE_SMOOTH);

  Matrix children = props.get_all_children ();
  std::list<graphics_object> obj_list;
  std::list<graphics_object>::iterator it;

  // 1st pass: draw light objects

  for (int i = 0; i < children.numel (); i++)
    {
      graphics_object go = gh_manager::get_object (children (i));

      if (go.get_properties ().is_visible ())
        {
          if (go.isa ("light"))
	    draw (go);
          else
            obj_list.push_back (go);
        }
    }

  // 2nd pass: draw other objects (with units set to "data")

  it = obj_list.begin ();
  while (it != obj_list.end ())
    {
      graphics_object go = (*it);

      // FIXME: check whether object has "units" property and it is set to "data"
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

  for (it = obj_list.begin (); it != obj_list.end (); it++)
    {
      graphics_object go = (*it);

      set_clipping (go.get_properties ().is_clipping ());
      draw (go);
    }

  set_clipping (false);
  // FIXME: finalize rendering (transparency processing)
  // FIXME: draw zoom box, if needed
}

void
opengl_renderer::draw_line (const line::properties& props)
{
  Matrix x = xform.xscale (props.get_xdata ().matrix_value ());
  Matrix y = xform.yscale (props.get_ydata ().matrix_value ());
  Matrix z = xform.zscale (props.get_zdata ().matrix_value ());

  bool has_z = (z.numel () > 0);
  int n = static_cast<int> (::xmin (::xmin (x.numel (), y.numel ()), (has_z ? z.numel () : INT_MAX)));
  octave_uint8 clip_mask = (props.is_clipping () ? 0x7F : 0x40), clip_ok (0x40);

  std::vector<octave_uint8> clip (n);

  if (has_z)
    for (int i = 0; i < n; i++)
      clip[i] = (clip_code (x(i), y(i), z(i)) & clip_mask);
  else
    {
      double z_mid = (zmin+zmax)/2;

      for (int i = 0; i < n; i++)
	clip[i] = (clip_code (x(i), y(i), z_mid) & clip_mask);
    }

  if (! props.linestyle_is ("none"))
    {
      set_color (props.get_color_rgb ());
      set_linestyle (props.get_linestyle (), false);
      set_linewidth (props.get_linewidth ());

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
		      glBegin (GL_LINE_STRIP);
		      glVertex3d (x(i-1), y(i-1), z(i-1));
		    }
		  glVertex3d (x(i), y(i), z(i));
		}
	      else if (flag)
		{
		  flag = false;
		  glEnd ();
		}
	    }

	  if (flag)
	    glEnd ();
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
		      glBegin (GL_LINE_STRIP);
		      glVertex2d (x(i-1), y(i-1));
		    }
		  glVertex2d (x(i), y(i));
		}
	      else if (flag)
		{
		  flag = false;
		  glEnd ();
		}
	    }

	  if (flag)
	    glEnd ();
	}
	  
      set_linewidth (0.5);
      set_linestyle ("-");
    }

  set_clipping (false);

  if (! props.marker_is ("none") &&
      ! (props.markeredgecolor_is ("none")
	 && props.markerfacecolor_is ("none")))
    {
      Matrix lc, fc;

      if (props.markeredgecolor_is ("auto"))
	lc = props.get_color_rgb ();
      else if (! props.markeredgecolor_is ("none"))
	lc = props.get_markeredgecolor_rgb ();

      if (props.markerfacecolor_is ("auto"))
	fc = props.get_color_rgb ();
      else if (! props.markerfacecolor_is ("none"))
	fc = props.get_markerfacecolor_rgb ();

      init_marker (props.get_marker (), props.get_markersize (),
		   props.get_linewidth ());

      for (int i = 0; i < n; i++)
        {
          if (clip[i] == clip_ok)
            draw_marker (x(i), y(i), (has_z ? z(i) : 0), lc, fc);
        }

      end_marker ();
    }
  
  set_clipping (props.is_clipping ());
}

void
opengl_renderer::draw_surface (const surface::properties& props)
{
  const Matrix x = xform.xscale (props.get_xdata ().matrix_value ());
  const Matrix y = xform.yscale (props.get_ydata ().matrix_value ());
  const Matrix z = xform.zscale (props.get_zdata ().matrix_value ());

  int zr = z.rows (), zc = z.columns ();

  NDArray c;
  const NDArray n = props.get_vertexnormals ().array_value ();

  // FIXME: handle transparency
  Matrix a;

  if (props.facelighting_is ("phong") || props.edgelighting_is ("phong"))
    warning ("opengl_renderer::draw: phong light model not supported");

  int fc_mode = (props.facecolor_is_rgb () ? 0 :
		 (props.facecolor_is ("flat") ? 1 :
		  (props.facecolor_is ("interp") ? 2 :
		   (props.facecolor_is ("texturemap") ? 3 : -1))));
  int fl_mode = (props.facelighting_is ("none") ? 0 :
		 (props.facelighting_is ("flat") ? 1 : 2));
  int fa_mode = (props.facealpha_is_double () ? 0 :
		 (props.facealpha_is ("flat") ? 1 : 2));
  int ec_mode = (props.edgecolor_is_rgb () ? 0 :
		 (props.edgecolor_is ("flat") ? 1 :
		  (props.edgecolor_is ("interp") ? 2 : -1)));
  int el_mode = (props.edgelighting_is ("none") ? 0 :
		 (props.edgelighting_is ("flat") ? 1 : 2));
  int ea_mode = (props.edgealpha_is_double () ? 0 :
		 (props.edgealpha_is ("flat") ? 1 : 2));

  Matrix fcolor = (fc_mode == 3 ? Matrix (1, 3, 1.0) : props.get_facecolor_rgb ());
  Matrix ecolor = props.get_edgecolor_rgb ();

  float as = props.get_ambientstrength ();
  float ds = props.get_diffusestrength ();
  float ss = props.get_specularstrength ();
  float se = props.get_specularexponent ();
  float cb[4] = { 0.0, 0.0, 0.0, 1.0 };
  double d = 1.0;

  opengl_texture tex;

  int i1, i2, j1, j2;
  bool x_mat = (x.rows () == z.rows ());
  bool y_mat = (y.columns () == z.columns ());

  i1 = i2 = j1 = j2 = 0;

  boolMatrix clip (z.dims (), false);

  for (int i = 0; i < zr; i++)
    {
      if (x_mat)
	i1 = i;

      for (int j = 0; j < zc; j++)
	{
	  if (y_mat)
	    j1 = j;

	  clip(i,j) = is_nan_or_inf (x(i1,j), y(i,j1), z(i,j));
	}
    }

  if ((fc_mode > 0 && fc_mode < 3) || ec_mode > 0)
    c = props.get_color_data ().array_value ();

  if (fa_mode > 0 || ea_mode > 0)
    {
      // FIXME: implement alphadata conversion
      //a = props.get_alpha_data ();
    }

  if (fl_mode > 0 || el_mode > 0)
    {
      float buf[4] = { ss, ss, ss, 1 };

      glMaterialfv (LIGHT_MODE, GL_SPECULAR, buf);
      glMaterialf (LIGHT_MODE, GL_SHININESS, se);
    }

  // FIXME: good candidate for caching, transfering pixel
  // data to OpenGL is time consuming.
  if (fc_mode == 3)
    tex = opengl_texture::create (props.get_color_data ());

  if (! props.facecolor_is ("none"))
    {
      if (props.get_facealpha_double () == 1)
	{
	  if (fc_mode == 0 || fc_mode == 3)
	    {
	      glColor3dv (fcolor.data ());
	      if (fl_mode > 0)
		{
		  for (int i = 0; i < 3; i++)
		    cb[i] = as * fcolor(i);
		  glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

		  for (int i = 0; i < 3; i++)
		    cb[i] = ds * fcolor(i);
		  glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
		}
	    }

	  if (fl_mode > 0)
	    glEnable (GL_LIGHTING);
	  glShadeModel ((fc_mode == 2 || fl_mode == 2) ? GL_SMOOTH : GL_FLAT);
	  set_polygon_offset (true, 1);
	  if (fc_mode == 3)
	    glEnable (GL_TEXTURE_2D);

	  for (int i = 1; i < zc; i++)
	    {
	      if (y_mat)
		{
		  i1 = i-1;
		  i2 = i;
		}

	      for (int j = 1; j < zr; j++)
		{
		  if (clip(j-1, i-1) || clip (j, i-1)
		      || clip (j-1, i) || clip (j, i))
		    continue;

		  if (x_mat)
		    {
		      j1 = j-1;
		      j2 = j;
		    }

		  glBegin (GL_QUADS);

		  // Vertex 1
		  if (fc_mode == 3)
		    tex.tex_coord (double (i-1) / (zc-1), double (j-1) / (zr-1));
		  else if (fc_mode > 0)
		    {
		      // FIXME: is there a smarter way to do this?
		      for (int k = 0; k < 3; k++)
			cb[k] = c(j-1, i-1, k);
		      glColor3fv (cb);

		      if (fl_mode > 0)
			{
			  for (int k = 0; k < 3; k++)
			    cb[k] *= as;
			  glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);
			  
			  for (int k = 0; k < 3; k++)
			    cb[k] = ds * c(j-1, i-1, k);
			  glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
			}
		    }
                  if (fl_mode > 0)
		    {
		      d = sqrt (n(j-1,i-1,0) * n(j-1,i-1,0)
				+ n(j-1,i-1,1) * n(j-1,i-1,1)
				+ n(j-1,i-1,2) * n(j-1,i-1,2));
		      glNormal3d (n(j-1,i-1,0)/d, n(j-1,i-1,1)/d, n(j-1,i-1,2)/d);
		    }
		  glVertex3d (x(j1,i-1), y(j-1,i1), z(j-1,i-1));

		  // Vertex 2
		  if (fc_mode == 3)
		    tex.tex_coord (double (i) / (zc-1), double (j-1) / (zr-1));
		  else if (fc_mode == 2)
		    {
		      for (int k = 0; k < 3; k++)
			cb[k] = c(j-1, i, k);
		      glColor3fv (cb);

		      if (fl_mode > 0)
			{
			  for (int k = 0; k < 3; k++)
			    cb[k] *= as;
			  glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);
			  
			  for (int k = 0; k < 3; k++)
			    cb[k] = ds * c(j-1, i, k);
			  glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
			}
		    }

                  if (fl_mode == 2)
		    {
		      d = sqrt (n(j-1,i,0) * n(j-1,i,0)
				+ n(j-1,i,1) * n(j-1,i,1)
				+ n(j-1,i,2) * n(j-1,i,2));
		      glNormal3d (n(j-1,i,0)/d, n(j-1,i,1)/d, n(j-1,i,2)/d);
		    }

		  glVertex3d (x(j1,i), y(j-1,i2), z(j-1,i));
		  
		  // Vertex 3
		  if (fc_mode == 3)
		    tex.tex_coord (double (i) / (zc-1), double (j) / (zr-1));
		  else if (fc_mode == 2)
		    {
		      for (int k = 0; k < 3; k++)
			cb[k] = c(j, i, k);
		      glColor3fv (cb);

		      if (fl_mode > 0)
			{
			  for (int k = 0; k < 3; k++)
			    cb[k] *= as;
			  glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);
			  
			  for (int k = 0; k < 3; k++)
			    cb[k] = ds * c(j, i, k);
			  glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
			}
		    }
		  if (fl_mode == 2)
		    {
		      d = sqrt (n(j,i,0) * n(j,i,0)
				+ n(j,i,1) * n(j,i,1)
				+ n(j,i,2) * n(j,i,2));
		      glNormal3d (n(j,i,0)/d, n(j,i,1)/d, n(j,i,2)/d);
		    }
		  glVertex3d (x(j2,i), y(j,i2), z(j,i));

		  // Vertex 4
		  if (fc_mode == 3)
		    tex.tex_coord (double (i-1) / (zc-1), double (j) / (zr-1));
		  else if (fc_mode == 2)
		    {
		      for (int k = 0; k < 3; k++)
			cb[k] = c(j, i-1, k);
		      glColor3fv (cb);

		      if (fl_mode > 0)
			{
			  for (int k = 0; k < 3; k++)
			    cb[k] *= as;
			  glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);
			  
			  for (int k = 0; k < 3; k++)
			    cb[k] = ds * c(j, i-1, k);
			  glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
			}
		    }
                  if (fl_mode == 2)
		    {
		      d = sqrt (n(j,i-1,0) * n(j,i-1,0)
				+ n(j,i-1,1) * n(j,i-1,1)
				+ n(j,i-1,2) * n(j,i-1,2));
		      glNormal3d (n(j,i-1,0)/d, n(j,i-1,1)/d, n(j,i-1,2)/d);
		    }
		  glVertex3d (x(j2,i-1), y(j,i1), z(j,i-1));

		  glEnd ();
		}
	    }

	  set_polygon_offset (false);
	  if (fc_mode == 3)
	    glDisable (GL_TEXTURE_2D);

	  if (fl_mode > 0)
	    glDisable (GL_LIGHTING);
	}
      else
	{
	  // FIXME: implement transparency
	}
    }

  if (! props.edgecolor_is ("none"))
    {
      if (props.get_edgealpha_double () == 1)
	{
	  if (ec_mode == 0)
	    {
	      glColor3dv (ecolor.data ());
	      if (fl_mode > 0)
		{
		  for (int i = 0; i < 3; i++)
		    cb[i] = as * ecolor(i);
		  glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

		  for (int i = 0; i < 3; i++)
		    cb[i] = ds * ecolor(i);
		  glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
		}
	    }

	  if (el_mode > 0)
	    glEnable (GL_LIGHTING);
	  glShadeModel ((ec_mode == 2 || el_mode == 2) ? GL_SMOOTH : GL_FLAT);

	  set_linestyle (props.get_linestyle (), false);
	  set_linewidth (props.get_linewidth ());

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
		      if (clip(j-1,i) || clip(j,i))
			continue;

		      if (x_mat)
			{
			  j1 = j-1;
			  j2 = j;
			}

		      glBegin (GL_LINES);

		      // Vertex 1
		      if (ec_mode > 0)
			{
			  for (int k = 0; k < 3; k++)
			    cb[k] = c(j-1, i, k);
			  glColor3fv (cb);

			  if (fl_mode > 0)
			    {
			      for (int k = 0; k < 3; k++)
				cb[k] *= as;
			      glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

			      for (int k = 0; k < 3; k++)
				cb[k] = ds * c(j-1, i, k);
			      glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
			    }
			}
		      if (el_mode > 0)
			{
			  d = sqrt (n(j-1,i,0) * n(j-1,i,0)
				    + n(j-1,i,1) * n(j-1,i,1)
				    + n(j-1,i,2) * n(j-1,i,2));
			  glNormal3d (n(j-1,i,0)/d, n(j-1,i,1)/d, n(j-1,i,2)/d);
			}
		      glVertex3d (x(j1,i), y(j-1,i2), z(j-1,i));

		      // Vertex 2
		      if (ec_mode == 2)
			{
			  for (int k = 0; k < 3; k++)
			    cb[k] = c(j, i, k);
			  glColor3fv (cb);

			  if (fl_mode > 0)
			    {
			      for (int k = 0; k < 3; k++)
				cb[k] *= as;
			      glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

			      for (int k = 0; k < 3; k++)
				cb[k] = ds * c(j, i, k);
			      glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
			    }
			}
		      if (el_mode == 2)
		        {
			  d = sqrt (n(j,i,0) * n(j,i,0)
				    + n(j,i,1) * n(j,i,1)
				    + n(j,i,2) * n(j,i,2));
			  glNormal3d (n(j,i,0)/d, n(j,i,1)/d, n(j,i,2)/d);
			}
		      glVertex3d (x(j2,i), y(j,i2), z(j,i));

		      glEnd ();
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
		      if (clip(j,i-1) || clip(j,i))
			continue;

		      if (y_mat)
			{
			  i1 = i-1;
			  i2 = i;
			}

		      glBegin (GL_LINES);

		      // Vertex 1
		      if (ec_mode > 0)
			{
			  for (int k = 0; k < 3; k++)
			    cb[k] = c(j, i-1, k);
			  glColor3fv (cb);

			  if (fl_mode > 0)
			    {
			      for (int k = 0; k < 3; k++)
				cb[k] *= as;
			      glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

			      for (int k = 0; k < 3; k++)
				cb[k] = ds * c(j, i-1, k);
			      glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
			    }
			}
		      if (el_mode > 0)
                        {
			  d = sqrt (n(j,i-1,0) * n(j,i-1,0)
				    + n(j,i-1,1) * n(j,i-1,1)
				    + n(j,i-1,2) * n(j,i-1,2));
			  glNormal3d (n(j,i-1,0)/d, n(j,i-1,1)/d, n(j,i-1,2)/d);
			}
		      glVertex3d (x(j2,i-1), y(j,i1), z(j,i-1));
		      
		      // Vertex 2
		      if (ec_mode == 2)
			{
			  for (int k = 0; k < 3; k++)
			    cb[k] = c(j, i, k);
			  glColor3fv (cb);

			  if (fl_mode > 0)
			    {
			      for (int k = 0; k < 3; k++)
				cb[k] *= as;
			      glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

			      for (int k = 0; k < 3; k++)
				cb[k] = ds * c(j, i, k);
			      glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
			    }
			}
		      if (el_mode == 2)
		        {
			  d = sqrt (n(j,i,0) * n(j,i,0)
				    + n(j,i,1) * n(j,i,1)
				    + n(j,i,2) * n(j,i,2));
			  glNormal3d (n(j,i,0)/d, n(j,i,1)/d, n(j,i,2)/d);
			}
		      glVertex3d (x(j2,i), y(j,i2), z(j,i));
		      
		      glEnd ();
		    }
		}
	    }

	  set_linestyle ("-");
	  set_linewidth (0.5);

	  if (el_mode > 0)
	    glDisable (GL_LIGHTING);
	}
      else
	{
	  // FIXME: implement transparency
	}
    }

  if (! props.marker_is ("none") &&
      ! (props.markeredgecolor_is ("none")
	 && props.markerfacecolor_is ("none")))
    {
      // FIXME: check how transparency should be handled in markers
      // FIXME: check what to do with marker facecolor set to auto
      //        and facecolor set to none.

      bool do_edge = ! props.markeredgecolor_is ("none");
      bool do_face = ! props.markerfacecolor_is ("none");

      Matrix mecolor = props.get_markeredgecolor_rgb ();
      Matrix mfcolor = props.get_markerfacecolor_rgb ();
      Matrix cc (1, 3, 0.0);

      if (mecolor.numel () == 0 && props.markeredgecolor_is ("auto"))
	{
	  mecolor = props.get_edgecolor_rgb ();
	  do_edge = ! props.edgecolor_is ("none");
	}

      if (mfcolor.numel () == 0 && props.markerfacecolor_is ("auto"))
	{
	  mfcolor = props.get_facecolor_rgb ();
	  do_face = ! props.facecolor_is ("none");
	}

      if ((mecolor.numel () == 0 || mfcolor.numel () == 0)
	  && c.numel () == 0)
	c = props.get_color_data ().array_value ();

      init_marker (props.get_marker (), props.get_markersize (),
		   props.get_linewidth ());

      for (int i = 0; i < zc; i++)
	{
	  if (y_mat)
	    i1 = i;
	  
	  for (int j = 0; j < zr; j++)
	    {
	      if (clip(j,i))
		continue;

	      if (x_mat)
		j1 = j;

	      if ((do_edge && mecolor.numel () == 0)
		  || (do_face && mfcolor.numel () == 0))
		{
		  for (int k = 0; k < 3; k++)
		    cc(k) = c(j,i,k);
		}

	      Matrix lc = (do_edge ? (mecolor.numel () == 0 ? cc : mecolor) : Matrix ());
	      Matrix fc = (do_face ? (mfcolor.numel () == 0 ? cc : mfcolor) : Matrix ());

	      draw_marker (x(j1,i), y(j,i1), z(j,i), lc, fc);
	    }
	}

      end_marker ();
    }
}

// FIXME: global optimization (rendering, data structures...), there
// is probably a smarter/faster/less-memory-consuming way to do this.
void
opengl_renderer::draw_patch (const patch::properties &props)
{
  const Matrix f = props.get_faces ().matrix_value ();
  const Matrix v = xform.scale (props.get_vertices ().matrix_value ());
  Matrix c;
  const Matrix n = props.get_vertexnormals ().matrix_value ();
  Matrix a;

  int nv = v.rows ();
  // int vmax = v.columns ();
  int nf = f.rows ();
  int fcmax = f.columns ();

  bool has_z = (v.columns () > 2);
  bool has_facecolor = false;
  bool has_facealpha = false;

  int fc_mode = (props.facecolor_is_rgb () ? 0 :
		 (props.facecolor_is("flat") ? 1 : 2));
  int fl_mode = (props.facelighting_is ("none") ? 0 :
		 (props.facelighting_is ("flat") ? 1 : 2));
  int fa_mode = (props.facealpha_is_double () ? 0 :
		 (props.facealpha_is ("flat") ? 1 : 2));
  int ec_mode = (props.edgecolor_is_rgb () ? 0 :
		 (props.edgecolor_is("flat") ? 1 : 2));
  int el_mode = (props.edgelighting_is ("none") ? 0 :
		 (props.edgelighting_is ("flat") ? 1 : 2));
  int ea_mode = (props.edgealpha_is_double () ? 0 :
		 (props.edgealpha_is ("flat") ? 1 : 2));

  Matrix fcolor = props.get_facecolor_rgb ();
  Matrix ecolor = props.get_edgecolor_rgb ();
  
  float as = props.get_ambientstrength ();
  float ds = props.get_diffusestrength ();
  float ss = props.get_specularstrength ();
  float se = props.get_specularexponent ();

  boolMatrix clip (1, nv, false);

  if (has_z)
    for (int i = 0; i < nv; i++)
      clip(i) = is_nan_or_inf (v(i,0), v(i,1), v(i,2));
  else
    for (int i = 0; i < nv; i++)
      clip(i) = is_nan_or_inf (v(i,0), v(i,1), 0);

  boolMatrix clip_f (1, nf, false);
  Array<int> count_f (nf, 0);

  for (int i = 0; i < nf; i++)
    {
      bool fclip = false;
      int count = 0;

      for (int j = 0; j < fcmax && ! xisnan (f(i,j)); j++, count++)
	fclip = (fclip || clip(int (f(i,j) - 1)));

      clip_f(i) = fclip;
      count_f(i) = count;
    }

  if (fc_mode > 0 || ec_mode > 0)
    {
      c = props.get_color_data ().matrix_value ();

      if (c.rows () == 1)
	{
	  // Single color specifications, we can simplify a little bit
	  
	  if (fc_mode > 0)
	    {
	      fcolor = c;
	      fc_mode = 0;
	    }

	  if (ec_mode > 0)
	    {
	      ecolor = c;
	      ec_mode = 0;
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

  octave_idx_type fr = f.rows ();
  std::vector<vertex_data> vdata (f.numel ());

  for (int i = 0; i < nf; i++)
    for (int j = 0; j < count_f(i); j++)
      {
	int idx = int (f(i,j) - 1);

	Matrix vv (1, 3, 0.0);
	Matrix cc;
	Matrix nn(1, 3, 0.0);
	double aa = 1.0;

	vv(0) = v(idx,0); vv(1) = v(idx,1);
	if (has_z)
	  vv(2) = v(idx,2);
	// FIXME: uncomment when patch object has normal computation
	//nn(0) = n(idx,0); nn(1) = n(idx,1); nn(2) = n(idx,2);
	if (c.numel () > 0)
	  {
	    cc.resize (1, 3);
	    if (has_facecolor)
	      cc(0) = c(i,0), cc(1) = c(i,1), cc(2) = c(i,2);
	    else
	      cc(0) = c(idx,0), cc(1) = c(idx,1), cc(2) = c(idx,2);
	  }
	if (a.numel () > 0)
	  {
	    if (has_facealpha)
	      aa = a(i);
	    else
	      aa = a(idx);
	  }

	vdata[i+j*fr] =
	    vertex_data (vv, cc, nn, aa, as, ds, ss, se);
      }

  if (fl_mode > 0 || el_mode > 0)
    {
      float buf[4] = { ss, ss, ss, 1 };

      glMaterialfv (LIGHT_MODE, GL_SPECULAR, buf);
      glMaterialf (LIGHT_MODE, GL_SHININESS, se);
    }

  if (! props.facecolor_is ("none"))
    {
      // FIXME: adapt to double-radio property
      if (props.get_facealpha_double () == 1)
	{
	  if (fc_mode == 0)
	    {
	      glColor3dv (fcolor.data ());
	      if (fl_mode > 0)
		{
		  float cb[4] = { 0, 0, 0, 1 };

		  for (int i = 0; i < 3; i++)
		    cb[i] = (as * fcolor(i));
		  glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

		  for (int i = 0; i < 3; i++)
		    cb[i] = ds * fcolor(i);
		  glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
		}
	    }

	  if (fl_mode > 0)
	    glEnable (GL_LIGHTING);

	  // FIXME: use __index__ property from patch object
	  patch_tesselator tess (this, fc_mode, fl_mode, 0);

	  for (int i = 0; i < nf; i++)
	    {
	      if (clip_f(i))
		continue;

	      tess.begin_polygon (true);
	      tess.begin_contour ();

	      for (int j = 0; j < count_f(i); j++)
		{
		  vertex_data::vertex_data_rep *vv = vdata[i+j*fr].get_rep ();
	
		  tess.add_vertex (vv->coords.fortran_vec (), vv);
		}

	      tess.end_contour ();
	      tess.end_polygon ();
	    }

	  if (fl_mode > 0)
	    glDisable (GL_LIGHTING);
	}
      else
	{
	  // FIXME: implement transparency
	}
    }

  if (! props.edgecolor_is ("none"))
    {
      // FIXME: adapt to double-radio property
      if (props.get_edgealpha_double () == 1)
	{
	  if (ec_mode == 0)
	    {
	      glColor3dv (ecolor.data ());
	      if (el_mode > 0)
		{
		  float cb[4] = { 0, 0, 0, 1 };

		  for (int i = 0; i < 3; i++)
		    cb[i] = (as * ecolor(i));
		  glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

		  for (int i = 0; i < 3; i++)
		    cb[i] = ds * ecolor(i);
		  glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
		}
	    }

	  if (el_mode > 0)
	    glEnable (GL_LIGHTING);

	  set_linestyle (props.get_linestyle (), false);
	  set_linewidth (props.get_linewidth ());

	  // FIXME: use __index__ property from patch object; should we
	  // offset patch contour as well?
	  patch_tesselator tess (this, ec_mode, el_mode);

	  for (int i = 0; i < nf; i++)
	    {
	      if (clip_f(i))
		continue;

	      tess.begin_polygon (false);
	      tess.begin_contour ();

	      for (int j = 0; j < count_f(i); j++)
		{
		  vertex_data::vertex_data_rep *vv = vdata[i+j*fr].get_rep ();
	
		  tess.add_vertex (vv->coords.fortran_vec (), vv);
		}

	      tess.end_contour ();
	      tess.end_polygon ();
	    }

	  set_linestyle ("-");
	  set_linewidth (0.5);

	  if (el_mode > 0)
	    glDisable (GL_LIGHTING);
	}
      else
	{
	  // FIXME: implement transparency
	}
    }

  if (! props.marker_is ("none") &&
      ! (props.markeredgecolor_is ("none") && props.markerfacecolor_is ("none")))
    {
      bool do_edge = ! props.markeredgecolor_is ("none");
      bool do_face = ! props.markerfacecolor_is ("none");

      Matrix mecolor = props.get_markeredgecolor_rgb ();
      Matrix mfcolor = props.get_markerfacecolor_rgb ();
      Matrix cc (1, 3, 0.0);

      if (mecolor.numel () == 0 && props.markeredgecolor_is ("auto"))
	{
	  mecolor = props.get_edgecolor_rgb ();
	  do_edge = ! props.edgecolor_is ("none");
	}

      if (mfcolor.numel () == 0 && props.markerfacecolor_is ("auto"))
	{
	  mfcolor = props.get_facecolor_rgb ();
	  do_face = ! props.facecolor_is ("none");
	}

      init_marker (props.get_marker (), props.get_markersize (),
		   props.get_linewidth ());

      for (int i = 0; i < nf; i++)
	for (int j = 0; j < count_f(i); j++)
	  {
	    int idx = int (f(i,j) - 1);

	    if (clip(idx))
	      continue;

	    Matrix lc = (do_edge ? (mecolor.numel () == 0 ?
				    vdata[i+j*fr].get_rep ()->color : mecolor)
			 : Matrix ());
	    Matrix fc = (do_face ? (mfcolor.numel () == 0 ?
				    vdata[i+j*fr].get_rep ()->color : mfcolor)
			 : Matrix ());

	    draw_marker (v(idx,0), v(idx,1), (has_z ? v(idx,2) : 0), lc, fc);
	  }

      end_marker ();
    }
}

void
opengl_renderer::draw_hggroup (const hggroup::properties &props)
{
  draw (props.get_children ());
}

void
opengl_renderer::draw_text (const text::properties& props)
{
  if (props.get_string ().empty ())
    return;

  set_font (props);
  set_color (props.get_color_rgb ());

  // FIXME: take "units" into account
  const Matrix pos = props.get_position ().matrix_value ();
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

  render_text (props.get_string (),
	     pos(0), pos(1), pos(2),
	     halign, valign, props.get_rotation ());
}

void
opengl_renderer::draw_image (const image::properties& props)
{
  octave_value cdata = props.get_color_data ();
  dim_vector dv (cdata.dims ());
  int h = dv(0), w = dv(1);
  bool ok = true;
  
  const Matrix x = props.get_xdata ().matrix_value ();
  const Matrix y = props.get_ydata ().matrix_value ();
  const ColumnVector p0 = xform.transform (x(0), y(0), 0);
  const ColumnVector p1 = xform.transform (x(1), y(1), 0);

  // image pixel size in screen pixel units
  float pix_dx, pix_dy;
  // image pixel size in normalized units
  float nor_dx, nor_dy;

  if (w > 1) 
    {
      pix_dx = (p1(0) - p0(0))/(w-1);
      nor_dx = (x(1) - x(0))/(w-1);
    }
  else
    {
      const ColumnVector p1 = xform.transform (x(1) + 1, y(1), 0);
      pix_dx = p1(0) - p0(0);
      nor_dx = 1;
    }
  if (h > 1)
    {
      pix_dy = (p1(1) - p0(1))/(h-1);
      nor_dy = (y(1) - y(0))/(h-1);
    }
  else
    {
      const ColumnVector p1 = xform.transform (x(1), y(1) + 1, 0);
      pix_dy = p1(1) - p0(1);
      nor_dy = 1;
    }


  // OpenGL won't draw the image if it's origin is outside the
  // viewport/clipping plane so we must do the clipping
  // ourselfes - only draw part of the image

  int j0 = 0, j1 = w;
  int i0 = 0, i1 = h;

  float im_xmin = x(0) - nor_dx/2;
  float im_xmax = x(1) + nor_dx/2;
  float im_ymin = y(0) - nor_dy/2;
  float im_ymax = y(1) + nor_dy/2;
  if (props.is_clipping ()) // clip to axes
    {
      if (im_xmin < xmin)
	j0 += (xmin - im_xmin)/nor_dx + 1;
      if (im_xmax > xmax)
	j1 -= (im_xmax - xmax)/nor_dx ;

      if (im_ymin < ymin)
	i0 += (ymin - im_ymin)/nor_dy + 1;
      if (im_ymax > ymax)
	i1 -= (im_ymax - ymax)/nor_dy;
    }
  else // clip to viewport
    {
      GLfloat vp[4];
      glGetFloatv(GL_VIEWPORT, vp);
      // FIXME -- actually add the code to do it!
      
    }

  if (i0 >= i1 || j0 >= j1) 
    return;

  glPixelZoom (pix_dx, -pix_dy);
  glRasterPos3d (im_xmin + nor_dx*j0, im_ymin + nor_dy*i0, 0);

  // by default this is 4
  glPixelStorei (GL_UNPACK_ALIGNMENT,1);

  // Expect RGB data
  if (dv.length () == 3 && dv(2) == 3)
    {
      if (cdata.is_double_type ())
	{
	  const NDArray xcdata = cdata.array_value ();

	  OCTAVE_LOCAL_BUFFER (GLfloat, a, 3*(j1-j0)*(i1-i0));

	  for (int i = i0; i < i1; i++)
	    {
	      for (int j = j0, idx = (i-i0)*(j1-j0)*3; j < j1; j++, idx += 3)
		{
		  a[idx]   = xcdata(i,j,0);
		  a[idx+1] = xcdata(i,j,1);
		  a[idx+2] = xcdata(i,j,2);
		}
	    }

	  draw_pixels (j1-j0, i1-i0, GL_RGB, GL_FLOAT, a);

	}
      else if (cdata.is_uint16_type ())
	{
	  const uint16NDArray xcdata = cdata.uint16_array_value ();

	  OCTAVE_LOCAL_BUFFER (GLushort, a, 3*(j1-j0)*(i1-i0));

	  for (int i = i0; i < i1; i++)
	    {
	      for (int j = j0, idx = (i-i0)*(j1-j0)*3; j < j1; j++, idx += 3)
		{
		  a[idx]   = xcdata(i,j,0);
		  a[idx+1] = xcdata(i,j,1);
		  a[idx+2] = xcdata(i,j,2);
		}
	    }

	  draw_pixels (j1-j0, i1-i0, GL_RGB, GL_UNSIGNED_SHORT, a);

	}
      else if (cdata.is_uint8_type ())
	{
	  const uint8NDArray xcdata = cdata.uint8_array_value ();

	  OCTAVE_LOCAL_BUFFER (GLubyte, a, 3*(j1-j0)*(i1-i0));

	  for (int i = i0; i < i1; i++)
	    {
	      for (int j = j0, idx = (i-i0)*(j1-j0)*3; j < j1; j++, idx += 3)
		{
		  a[idx]   = xcdata(i,j,0);
		  a[idx+1] = xcdata(i,j,1);
		  a[idx+2] = xcdata(i,j,2);
		}
	    }

	  draw_pixels (j1-j0, i1-i0, GL_RGB, GL_UNSIGNED_BYTE, a);
	}
      else
	{
	  ok = false;
	  warning ("opengl_texture::draw: invalid image data type (expected double, uint16, or uint8)");
	}
    }
  else 
    {
      ok = false;
      warning ("opengl_texture::draw: invalid image size (expected n*m*3 or n*m)");
    }
  glPixelZoom (1, 1);
}

void
opengl_renderer::set_viewport (int w, int h)
{
  glViewport (0, 0, w, h);
}

void
opengl_renderer::draw_pixels (GLsizei width, GLsizei height, GLenum format,
                              GLenum type, const GLvoid *data)
{
  glDrawPixels (width, height, format, type, data);
}

void
opengl_renderer::set_color (const Matrix& c)
{
  glColor3dv (c.data ());
#if HAVE_FREETYPE
  text_renderer.set_color (c);
#endif
}

void
opengl_renderer::set_font (const base_properties& props)
{
#if HAVE_FREETYPE
  text_renderer.set_font (props);
#endif
}

void
opengl_renderer::set_polygon_offset (bool on, double offset)
{
  if (on)
    {
      glPolygonOffset (offset, offset);
      glEnable (GL_POLYGON_OFFSET_FILL);
      glEnable (GL_POLYGON_OFFSET_LINE);
    }
  else
    {
      glDisable (GL_POLYGON_OFFSET_FILL);
      glDisable (GL_POLYGON_OFFSET_LINE);
    }
}

void
opengl_renderer::set_linewidth (float w)
{
  glLineWidth (w);
}

void
opengl_renderer::set_linestyle (const std::string& s, bool use_stipple)
{
  bool solid = false;

  if (s == "-")
    {
      glLineStipple (1, static_cast<unsigned short> (0xFFFF));
      solid = true;
    }
  else if (s == ":")
    glLineStipple (1, static_cast<unsigned short> (0x8888));
  else if (s == "--")
    glLineStipple (1, static_cast<unsigned short> (0x0FFF));
  else if (s == "-.")
    glLineStipple (1, static_cast<unsigned short> (0x020F));
  else
    glLineStipple (1, static_cast<unsigned short> (0x0000));

  if (solid && ! use_stipple)
    glDisable (GL_LINE_STIPPLE);
  else
    glEnable (GL_LINE_STIPPLE);
}

void
opengl_renderer::set_clipbox (double x1, double x2, double y1, double y2,
			      double z1, double z2)
{
  double dx = (x2-x1);
  double dy = (y2-y1);
  double dz = (z2-z1);

  x1 -= 0.001*dx; x2 += 0.001*dx;
  y1 -= 0.001*dy; y2 += 0.001*dy;
  z1 -= 0.001*dz; z2 += 0.001*dz;

  ColumnVector p (4, 0.0);

  p(0) = -1; p(3) = x2;
  glClipPlane (GL_CLIP_PLANE0, p.data ());
  p(0) = 1; p(3) = -x1;
  glClipPlane (GL_CLIP_PLANE1, p.data ());
  p(0) = 0; p(1) = -1; p(3) = y2;
  glClipPlane (GL_CLIP_PLANE2, p.data ());
  p(1) = 1; p(3) = -y1;
  glClipPlane (GL_CLIP_PLANE3, p.data ());
  p(1) = 0; p(2) = -1; p(3) = z2;
  glClipPlane (GL_CLIP_PLANE4, p.data ());
  p(2) = 1; p(3) = -z1;
  glClipPlane (GL_CLIP_PLANE5, p.data ());

  xmin = x1; xmax = x2;
  ymin = y1; ymax = y2;
  zmin = z1; zmax = z2;
}

void
opengl_renderer::set_clipping (bool enable)
{
  bool has_clipping = (glIsEnabled (GL_CLIP_PLANE0) == GL_TRUE);

  if (enable != has_clipping)
    {
      if (enable)
        for (int i = 0; i < 6; i++)
          glEnable (GL_CLIP_PLANE0+i);
      else
        for (int i = 0; i < 6; i++)
          glDisable (GL_CLIP_PLANE0+i);
    }
}

void
opengl_renderer::init_marker (const std::string& m, double size, float width)
{
#if defined (HAVE_FRAMEWORK_OPENGL)
  GLint vw[4];
#else
  int vw[4];
#endif

  glGetIntegerv (GL_VIEWPORT, vw);

  glMatrixMode (GL_PROJECTION);
  glPushMatrix ();
  glLoadIdentity ();
  glOrtho (0, vw[2], vw[3], 0, xZ1, xZ2);
  glMatrixMode (GL_MODELVIEW);
  glPushMatrix ();

  set_clipping (false);
  set_linewidth (width);

  marker_id = make_marker_list (m, size, false);
  filled_marker_id = make_marker_list (m, size, true);
}

void
opengl_renderer::end_marker (void)
{
  glDeleteLists (marker_id, 1);
  glDeleteLists (filled_marker_id, 1);

  glMatrixMode (GL_MODELVIEW);
  glPopMatrix ();
  glMatrixMode (GL_PROJECTION);
  glPopMatrix ();
  set_linewidth (0.5f);
}

void
opengl_renderer::draw_marker (double x, double y, double z,
			      const Matrix& lc, const Matrix& fc)
{
  ColumnVector tmp = xform.transform (x, y, z, false);
  
  glLoadIdentity ();
  glTranslated (tmp(0), tmp(1), -tmp(2));

  if (filled_marker_id > 0 && fc.numel () > 0)
    {
      glColor3dv (fc.data ());
      set_polygon_offset (true, -1.0);
      glCallList (filled_marker_id);
      if (lc.numel () > 0)
	{
	  glColor3dv (lc.data ());
	  glPolygonMode (GL_FRONT_AND_BACK, GL_LINE);
	  glEdgeFlag (GL_TRUE);
	  set_polygon_offset (true, -2.0);
	  glCallList (filled_marker_id);
	  glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
	}
      set_polygon_offset (false);
    }
  else if (marker_id > 0 && lc.numel () > 0)
    {
      glColor3dv (lc.data ());
      glCallList (marker_id);
    }
}

unsigned int
opengl_renderer::make_marker_list (const std::string& marker, double size,
				   bool filled) const
{
  char c = marker[0];

  if (filled && (c == '+' || c == 'x' || c == '*' || c == '.'))
    return 0;

  unsigned int ID = glGenLists (1);
  double sz = size * backend.get_screen_resolution () / 72.0;

  // constants for the * marker
  const double sqrt2d4 = 0.35355339059327;
  double tt = sz*sqrt2d4;

  glNewList (ID, GL_COMPILE);

  switch (marker[0])
    {
    case '+':
      glBegin (GL_LINES);
      glVertex2f (-sz/2, 0);
      glVertex2f (sz/2, 0);
      glVertex2f (0, -sz/2);
      glVertex2f (0, sz/2);
      glEnd ();
      break;
    case 'x':
      glBegin(GL_LINES);
      glVertex2f (-sz/2, -sz/2);
      glVertex2f (sz/2, sz/2);
      glVertex2f (-sz/2, sz/2);
      glVertex2f (sz/2, -sz/2);
      glEnd ();
      break;
    case '*':
      glBegin (GL_LINES);
      glVertex2f (-sz/2, 0);
      glVertex2f (sz/2, 0);
      glVertex2f (0, -sz/2);
      glVertex2f (0, sz/2);
      glVertex2f (-tt, -tt);
      glVertex2f (+tt, +tt);
      glVertex2f (-tt, +tt);
      glVertex2f (+tt, -tt);
      glEnd ();
      break;
    case '.':
      glBegin (GL_POLYGON);
      glVertex2f (-sz/10, -sz/10);
      glVertex2f (-sz/10, sz/10);
      glVertex2f (sz/10, sz/10);
      glVertex2f (sz/10, -sz/10);
      glEnd ();
      break;
    case 's':
      glBegin ((filled ? GL_POLYGON : GL_LINE_LOOP));
      glVertex2d (-sz/2, -sz/2);
      glVertex2d (-sz/2, sz/2);
      glVertex2d (sz/2, sz/2);
      glVertex2d (sz/2, -sz/2);
      glEnd();
      break;
    case 'o':
      {
	double ang_step = M_PI / 5;

	glBegin ((filled ? GL_POLYGON : GL_LINE_LOOP));
	for (double ang = 0; ang < (2*M_PI); ang += ang_step)
	  glVertex2d (sz*cos(ang)/2, sz*sin(ang)/2);
	glEnd ();
      }
      break;
    case 'd':
      glBegin ((filled ? GL_POLYGON : GL_LINE_LOOP));
      glVertex2d (0, -sz/2);
      glVertex2d (sz/2, 0);
      glVertex2d (0, sz/2);
      glVertex2d (-sz/2, 0);
      glEnd();
      break;
    case '^':
      glBegin ((filled ? GL_POLYGON : GL_LINE_LOOP));
      glVertex2f (0, sz/2);
      glVertex2f (sz/2, -sz/2);
      glVertex2f (-sz/2, -sz/2);
      glEnd ();
      break;
    case 'v':
      glBegin ((filled ? GL_POLYGON : GL_LINE_LOOP));
      glVertex2f (0, -sz/2);
      glVertex2f (-sz/2, sz/2);
      glVertex2f (sz/2, sz/2);
      glEnd ();
      break;
    case '>':
      glBegin ((filled ? GL_POLYGON : GL_LINE_LOOP));
      glVertex2f (sz/2, 0);
      glVertex2f (-sz/2, sz/2);
      glVertex2f (-sz/2, -sz/2);
      glEnd ();
      break;
    case '<':
      glBegin ((filled ? GL_POLYGON : GL_LINE_LOOP));
      glVertex2f (-sz/2, 0);
      glVertex2f (sz/2, -sz/2);
      glVertex2f (sz/2, sz/2);
      glEnd ();
      break;
    default:
      warning ("opengl_renderer: unsupported marker `%s'",
	       marker.c_str ());
      break;
    }

  glEndList ();

  return ID;
}

void
opengl_renderer::text_to_pixels (const std::string& txt,
				 double rotation,
				 uint8NDArray& pixels,
				 Matrix& bbox,
				 int& rot_mode)
{
  // FIXME: clip "rotation" between 0 and 360

  rot_mode = ft_render::ROTATION_0;

  if (rotation == 90.0)
    rot_mode = ft_render::ROTATION_90;
  else if (rotation == 180.0)
    rot_mode = ft_render::ROTATION_180;
  else if (rotation == 270.0)
    rot_mode = ft_render::ROTATION_270;

  text_element *elt = text_parser_none ().parse (txt);
  pixels = text_renderer.render (elt, bbox, rot_mode);
  delete elt;
}

Matrix
opengl_renderer::render_text (const std::string& txt,
			    double x, double y, double z,
			    int halign, int valign, double rotation)
{
#if HAVE_FREETYPE
  if (txt.empty ())
    return Matrix (1, 4, 0.0);

  Matrix bbox;
  uint8NDArray pixels;
  int rot_mode;
  text_to_pixels (txt, rotation, pixels, bbox, rot_mode);

  int x0 = 0, y0 = 0;
  int w = bbox(2), h = bbox(3);

  if (pixels.numel () == 0)
    {
      // nothing to render
      return bbox;
    }

  switch (halign)
    {
    default: break;
    case 1: x0 = -bbox(2)/2; break;
    case 2: x0 = -bbox(2); break;
    }
  switch (valign)
    {
    default: break;
    case 1: y0 = -bbox(3)/2; break;
    case 2: y0 = -bbox(3); break;
    case 3: y0 = bbox(1); break;
    }

  switch (rot_mode)
    {
    case ft_render::ROTATION_90:
      std::swap (x0, y0);
      std::swap (w, h);
      x0 = -x0-bbox(3);
      break;
    case ft_render::ROTATION_180:
      x0 = -x0-bbox(2);
      y0 = -y0-bbox(3);
      break;
    case ft_render::ROTATION_270:
      std::swap (x0, y0);
      std::swap (w, h);
      y0 = -y0-bbox(2);
      break;
    }

  bool blend = glIsEnabled (GL_BLEND);

  glEnable (GL_BLEND);
  glEnable (GL_ALPHA_TEST);
  glRasterPos3d (x, y, z);
  glBitmap(0, 0, 0, 0, x0, y0, 0);
  glDrawPixels (w, h,
		GL_RGBA, GL_UNSIGNED_BYTE, pixels.data ());
  glDisable (GL_ALPHA_TEST);
  if (! blend)
    glDisable (GL_BLEND);

  return bbox;
#else
  ::warning ("render_text: cannot render text, Freetype library not available");
  return Matrix (1, 4, 0.0);
#endif
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
