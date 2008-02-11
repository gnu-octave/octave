/*

Copyright (C) 2008 Michael Goffioul

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

#include <lo-mappers.h>
#include "gl-render.h"

#include <GL/gl.h>
#include <GL/glu.h>

enum {
  AXE_ANY_DIR   = 0,
  AXE_DEPTH_DIR = 1,
  AXE_HORZ_DIR  = 2,
  AXE_VERT_DIR  = 3
};
void
opengl_renderer::draw (const graphics_object& go)
{
  if (! go.valid_object ())
    return;

  const base_properties& props = go.get_properties ();

  if (go.isa ("figure"))
    draw (dynamic_cast<const figure::properties&> (props));
  else if (go.isa ("axes"))
    draw (dynamic_cast<const axes::properties&> (props));
  else if (go.isa ("line"))
    draw (dynamic_cast<const line::properties&> (props));
  else
    warning ("opengl_renderer: cannot render object of type `%s'",
	     props.graphics_object_name ().c_str ());
}

void
opengl_renderer::draw (const figure::properties& props)
{
  backend = props.get_backend ();

  Matrix c = props.get_color_rgb ();

  if (c.length() >= 3)
    {
      glClearColor (c(0), c(1), c(2), 1);
      glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    }

  draw (props.get_children ());
}

void
opengl_renderer::draw (const axes::properties& props)
{
  // setup OpenGL transformation

  Matrix x_zlim = props.get_transform_zlim ();
  Matrix x_mat1 = props.get_opengl_matrix_1 ();
  Matrix x_mat2 = props.get_opengl_matrix_2 ();
  
  xZ1 = x_zlim(0)-(x_zlim(1)-x_zlim(0))/2;
  xZ2 = x_zlim(1)+(x_zlim(1)-x_zlim(0))/2;

  int vw[4];
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
  
  Matrix xlim = xform.xscale (props.get_xlim ().matrix_value ());
  Matrix ylim = xform.yscale (props.get_ylim ().matrix_value ());
  Matrix zlim = xform.zscale (props.get_zlim ().matrix_value ());
  double xmin = xlim(0), xmax = xlim(1);
  double ymin = ylim(0), ymax = ylim(1);
  double zmin = zlim(0), zmax = zlim(1);
  
  double xd = (props.xdir_is ("normal") ? 1 : -1);
  double yd = (props.ydir_is ("normal") ? 1 : -1);
  double zd = (props.zdir_is ("normal") ? 1 : -1);

  ColumnVector p1, p2, xv (3), yv (3), zv (3);
  int xstate, ystate, zstate;

  xstate = ystate = zstate = AXE_ANY_DIR;

  p1 = xform.transform (xmin, (ymin+ymax)/2, (zmin+zmax)/2, false);
  p2 = xform.transform (xmax, (ymin+ymax)/2, (zmin+zmax)/2, false);
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
      xPlane = (xv(0) > 0 ? xmax : xmin);
    else
      xPlane = (xv(1) < 0 ? xmax : xmin);
  else
    xPlane = (xv(2) < 0 ? xmin : xmax);
  double xPlaneN = (xPlane == xmin ? xmax : xmin);
  double fx = (xmax-xmin)/sqrt(xv(0)*xv(0)+xv(1)*xv(1));

  p1 = xform.transform ((xmin+xmax)/2, ymin, (zmin+zmax)/2, false);
  p2 = xform.transform ((xmin+xmax)/2, ymax, (zmin+zmax)/2, false);
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
      yPlane = (yv(0) > 0 ? ymax : ymin);
    else
      yPlane = (yv(1) < 0 ? ymax : ymin);
  else
    yPlane = (yv(2) < 0 ? ymin : ymax);
  double yPlaneN = (yPlane == ymin ? ymax : ymin);
  double fy = (ymax-ymin)/sqrt(yv(0)*yv(0)+yv(1)*yv(1));

  p1 = xform.transform((xmin+xmax)/2, (ymin+ymax)/2, zmin, false);
  p2 = xform.transform((xmin+xmax)/2, (ymin+ymax)/2, zmax, false);
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
      zPlane = (zv(0) > 0 ? zmin : zmax);
    else
      zPlane = (zv(1) < 0 ? zmin : zmax);
  else
    zPlane = (zv(2) < 0 ? zmin : zmax);
  double zPlaneN = (zPlane == zmin ? zmax : zmin);
  double fz = (zmax-zmin)/sqrt(zv(0)*zv(0)+zv(1)*zv(1));

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
      glVertex3d (xPlane, ymin, zmin);
      glVertex3d (xPlane, ymax, zmin);
      glVertex3d (xPlane, ymax, zmax);
      glVertex3d (xPlane, ymin, zmax);

      // Y plane
      glVertex3d (xmin, yPlane, zmin);
      glVertex3d (xmax, yPlane, zmin);
      glVertex3d (xmax, yPlane, zmax);
      glVertex3d (xmin, yPlane, zmax);

      // Z plane
      glVertex3d (xmin, ymin, zPlane);
      glVertex3d (xmax, ymin, zPlane);
      glVertex3d (xmax, ymax, zPlane);
      glVertex3d (xmin, ymax, zPlane);

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

  // X grid

  if (visible && xstate != AXE_DEPTH_DIR)
    {
      bool do_xgrid = (props.is_xgrid () && (gridstyle != "none"));
      bool do_xminorgrid = (props.is_xminorgrid () && (minorgridstyle != "none"));
      bool do_xminortick = props.is_xminortick ();
      Matrix xticks = xform.xscale (props.get_xtick ().matrix_value ());
      // FIXME: use pre-computed minor ticks
      Matrix xmticks;
      // FIXME: use xticklabels property
      string_vector xticklabels;
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

      // FIXME: tick texts

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

      // FIXME: auto-positioning should be disabled if the 
      //        label has been positioned manually
      if (! xlabel_props.get_string ().empty ())
        {
          xlabel_props.set_horizontalalignment (xstate > AXE_DEPTH_DIR ? "center" : (xySym ? "left" : "right"));
	  xlabel_props.set_verticalalignment (xstate == AXE_VERT_DIR ? "bottom" : (zd*zv(2) <= 0 ? "top" : "bottom"));

          double angle = 0;
          ColumnVector p = graphics_xform::xform_vector ((xmin+xmax)/2, yPlaneN, zPlane);

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

  // Y grid
		
  if (ystate != AXE_DEPTH_DIR && visible)
    {
      bool do_ygrid = (props.is_ygrid () && (gridstyle != "none"));
      bool do_yminorgrid = (props.is_yminorgrid () && (minorgridstyle != "none"));
      bool do_yminortick = props.is_yminortick ();
      Matrix yticks = xform.yscale (props.get_ytick ().matrix_value ());
      // FIXME: use pre-computed minor ticks
      Matrix ymticks;
      // FIXME: use yticklabels property
      string_vector yticklabels;
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

      // FIXME: tick texts

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

      // FIXME: auto-positioning should be disabled if the 
      //        label has been positioned manually
      if (! ylabel_props.get_string ().empty ())
        {
          ylabel_props.set_horizontalalignment (ystate > AXE_DEPTH_DIR ? "center" : (!xySym ? "left" : "right"));
	  ylabel_props.set_verticalalignment (ystate == AXE_VERT_DIR ? "bottom" : (zd*zv(2) <= 0 ? "top" : "bottom"));

          double angle = 0;
          ColumnVector p = graphics_xform::xform_vector (xPlaneN, (ymin+ymax)/2, zPlane);

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
		
  // Z Grid

  if (zstate != AXE_DEPTH_DIR && visible)
    {
      bool do_zgrid = (props.is_zgrid () && (gridstyle != "none"));
      bool do_zminorgrid = (props.is_zminorgrid () && (minorgridstyle != "none"));
      bool do_zminortick = props.is_zminortick ();
      Matrix zticks = xform.zscale (props.get_ztick ().matrix_value ());
      // FIXME: use pre-computed minor ticks
      Matrix zmticks;
      // FIXME: use zticklabels property
      string_vector zticklabels;
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
              p = graphics_xform::xform_vector (xPlaneN, yPlane, (zmin+zmax)/2);
              if (xisinf (fy))
                p(0) += (signum(xPlaneN-xPlane)*fx*ztickoffset);
              else
                p(1) += (signum(yPlane-yPlaneN)*fy*ztickoffset);
            }
          else
            {
              p = graphics_xform::xform_vector (xPlane, yPlaneN, (zmin+zmax)/2);
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

  set_clipbox (xmin, xmax, ymin, ymax, zmin, zmax);

  // Children

  Matrix children = props.get_children ();
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
opengl_renderer::draw (const line::properties& props)
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
opengl_renderer::set_viewport (int w, int h)
{
  glViewport (0, 0, w, h);
}

void
opengl_renderer::set_color (const Matrix& c)
{
  glColor3dv (c.data ());
}

void
opengl_renderer::set_polygon_offset (bool on, double offset)
{
  if (on)
    {
      glPolygonOffset (offset, offset);
      glEnable (GL_POLYGON_OFFSET_FILL);
    }
  else
    glDisable (GL_POLYGON_OFFSET_FILL);
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
  int vw[4];

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

  if (fc.numel () > 0)
    {
      glColor3dv (fc.data ());
      set_polygon_offset (true, 1.0);
      glCallList (filled_marker_id);
      set_polygon_offset (false);
    }

  if (lc.numel () > 0)
    {
      glColor3dv (lc.data ());
      glCallList (marker_id);
    }
}

unsigned int
opengl_renderer::make_marker_list (const std::string& marker, double size,
				   bool filled) const
{
  unsigned int ID = glGenLists (1);
  double sz = size * backend.get_screen_resolution () / 72.0;

  glNewList (ID, GL_COMPILE);

  switch (marker[0])
    {
      case 's':
        glBegin ((filled ? GL_POLYGON : GL_LINE_LOOP));
        glVertex2d (-sz/2, -sz/2);
        glVertex2d (-sz/2,  sz/2);
        glVertex2d ( sz/2,  sz/2);
        glVertex2d ( sz/2, -sz/2);
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
        glVertex2d (    0, -sz/2);
        glVertex2d ( sz/2,     0);
        glVertex2d (    0,  sz/2);
        glVertex2d (-sz/2,     0);
        glEnd();
        break;
      default:
	warning ("opengl_renderer: unsupported marker `%s'",
		 marker.c_str ());
	break;
    }

  glEndList ();

  return ID;
}
