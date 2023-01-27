////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007-2023 The Octave Project Developers
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

#include <cctype>
#include <cmath>
#include <cstdint>
#include <cstdlib>

#include <algorithm>
#include <iostream>
#include <limits>
#include <list>
#include <map>
#include <set>
#include <string>
#include <sstream>

#include "cmd-edit.h"
#include "file-ops.h"
#include "file-stat.h"
#include "oct-locbuf.h"
#include "oct-time.h"

#include "builtin-defun-decls.h"
#include "defun.h"
#include "display.h"
#include "error.h"
#include "graphics.h"
#include "input.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "ov.h"
#include "ovl.h"
#include "oct-map.h"
#include "ov-fcn-handle.h"
#include "pager.h"
#include "parse.h"
#include "text-engine.h"
#include "text-renderer.h"
#include "unwind-prot.h"
#include "utils.h"
#include "octave-default-image.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// forward declarations
static octave_value xget (const graphics_handle& h, const caseless_str& name);

OCTAVE_NORETURN static
void
err_set_invalid (const std::string& pname)
{
  error ("set: invalid value for %s property", pname.c_str ());
}

// Check to see that PNAME matches just one of PNAMES uniquely.
// Return the full name of the match, or an empty caseless_str object
// if there is no match, or the match is ambiguous.

static caseless_str
validate_property_name (const std::string& who, const std::string& what,
                        const std::set<std::string>& pnames,
                        const caseless_str& pname)
{
  std::size_t len = pname.length ();
  std::set<std::string> matches;

  // Find exact or partial matches to property name
  for (const auto& propnm : pnames)
    {
      if (pname.compare (propnm, len))
        {
          if (len == propnm.length ())
            return pname;  // Exact match.

          matches.insert (propnm);
        }
    }

  std::size_t num_matches = matches.size ();

  if (num_matches == 0)
    error ("%s: unknown %s property %s",
           who.c_str (), what.c_str (), pname.c_str ());
  else if (num_matches > 1)
    {
      string_vector sv (matches);

      std::ostringstream os;

      sv.list_in_columns (os);

      std::string match_list = os.str ();

      error ("%s: ambiguous %s property name %s; possible matches:\n\n%s",
             who.c_str (), what.c_str (), pname.c_str (), match_list.c_str ());
    }
  else  // num_matches == 1
    {
      // Exact match was handled above.
      std::string possible_match = *(matches.begin ());

      warning_with_id ("Octave:abbreviated-property-match",
                       "%s: allowing %s to match %s property %s",
                       who.c_str (), pname.c_str (), what.c_str (),
                       possible_match.c_str ());

      return possible_match;
    }

  return caseless_str ();
}

static Matrix
viridis_colormap (void)
{
  // The values below have been produced by viridis (64)(:)
  // It would be nice to be able to feval the
  // viridis function but since there is a static property object that includes
  // a colormap_property object, we need to initialize this before main is
  // even called, so calling an interpreted function is not possible.

  const double cmapv[] =
  {
    2.67004010000000e-01, 2.72651720952381e-01, 2.77106307619048e-01,
    2.80356151428571e-01, 2.82390045238095e-01, 2.83204606666667e-01,
    2.82809341428571e-01, 2.81230763333333e-01, 2.78516153333333e-01,
    2.74735528571429e-01, 2.69981791904762e-01, 2.64368580952381e-01,
    2.58026184285714e-01, 2.51098684761905e-01, 2.43732853333333e-01,
    2.36073294285714e-01, 2.28263191428571e-01, 2.20424955714286e-01,
    2.12666598571429e-01, 2.05079113809524e-01, 1.97721880952381e-01,
    1.90631350000000e-01, 1.83819438571429e-01, 1.77272360952381e-01,
    1.70957518571429e-01, 1.64832915714286e-01, 1.58845368095238e-01,
    1.52951235714286e-01, 1.47131626666667e-01, 1.41402210952381e-01,
    1.35832975714286e-01, 1.30582113809524e-01, 1.25898377619048e-01,
    1.22163105714286e-01, 1.19872409523810e-01, 1.19626570000000e-01,
    1.22045948571429e-01, 1.27667691904762e-01, 1.36834947142857e-01,
    1.49643331428571e-01, 1.65967274285714e-01, 1.85538397142857e-01,
    2.08030450000000e-01, 2.33127309523809e-01, 2.60531475238095e-01,
    2.90000730000000e-01, 3.21329971428571e-01, 3.54355250000000e-01,
    3.88930322857143e-01, 4.24933143333333e-01, 4.62246770476190e-01,
    5.00753620000000e-01, 5.40336957142857e-01, 5.80861172380952e-01,
    6.22170772857143e-01, 6.64087320476191e-01, 7.06403823333333e-01,
    7.48885251428571e-01, 7.91273132857143e-01, 8.33302102380952e-01,
    8.74717527142857e-01, 9.15296319047619e-01, 9.54839555238095e-01,
    9.93247890000000e-01, 4.87433000000000e-03, 2.58456800000000e-02,
    5.09139004761905e-02, 7.42014957142857e-02, 9.59536042857143e-02,
    1.16893314761905e-01, 1.37350195714286e-01, 1.57479940000000e-01,
    1.77347967619048e-01, 1.96969168571429e-01, 2.16330337619048e-01,
    2.35404660952381e-01, 2.54161735714286e-01, 2.72573219047619e-01,
    2.90619516666667e-01, 3.08291041428571e-01, 3.25586450952381e-01,
    3.42517215238095e-01, 3.59102207142857e-01, 3.75366067142857e-01,
    3.91340913333333e-01, 4.07061480000000e-01, 4.22563764285714e-01,
    4.37885543809524e-01, 4.53062984285714e-01, 4.68129543809524e-01,
    4.83117059523810e-01, 4.98052961428571e-01, 5.12959473333333e-01,
    5.27854311428571e-01, 5.42750087142857e-01, 5.57652481904762e-01,
    5.72563073333333e-01, 5.87476284285714e-01, 6.02382410952381e-01,
    6.17265840000000e-01, 6.32106955714286e-01, 6.46881817142857e-01,
    6.61562926190476e-01, 6.76119717142857e-01, 6.90518987142857e-01,
    7.04725181904762e-01, 7.18700950000000e-01, 7.32406441904762e-01,
    7.45802021904762e-01, 7.58846480000000e-01, 7.71497934761905e-01,
    7.83714033809524e-01, 7.95453081428571e-01, 8.06673890000000e-01,
    8.17337565714286e-01, 8.27409135714286e-01, 8.36858167619048e-01,
    8.45663399523809e-01, 8.53815582857143e-01, 8.61321019047619e-01,
    8.68206316666667e-01, 8.74522215714286e-01, 8.80346158571429e-01,
    8.85780083333333e-01, 8.90945338571429e-01, 8.95973498571429e-01,
    9.01005800000000e-01, 9.06156570000000e-01, 3.29415190000000e-01,
    3.53367293333333e-01, 3.76236064761905e-01, 3.97901482857143e-01,
    4.18250757142857e-01, 4.37178920000000e-01, 4.54595888571429e-01,
    4.70433883333333e-01, 4.84653865714286e-01, 4.97250492857143e-01,
    5.08254501428571e-01, 5.17731949047619e-01, 5.25780221428571e-01,
    5.32522206190476e-01, 5.38097133333333e-01, 5.42651800000000e-01,
    5.46335411904762e-01, 5.49287148571429e-01, 5.51635008571429e-01,
    5.53493173333333e-01, 5.54953478571429e-01, 5.56089070000000e-01,
    5.56952166666667e-01, 5.57576145714286e-01, 5.57974025714286e-01,
    5.58142745238095e-01, 5.58058673809524e-01, 5.57684744285714e-01,
    5.56973310000000e-01, 5.55864478571429e-01, 5.54288677142857e-01,
    5.52175699047619e-01, 5.49445382857143e-01, 5.46023368571429e-01,
    5.41830633809524e-01, 5.36795616666667e-01, 5.30847985714286e-01,
    5.23924198571429e-01, 5.15966779523810e-01, 5.06924262857143e-01,
    4.96751861428571e-01, 4.85412122857143e-01, 4.72873300000000e-01,
    4.59105875238095e-01, 4.44095883333333e-01, 4.27825852857143e-01,
    4.10292713809524e-01, 3.91487632857143e-01, 3.71420688571429e-01,
    3.50098750000000e-01, 3.27544678571429e-01, 3.03798967142857e-01,
    2.78916748571429e-01, 2.53000856190476e-01, 2.26223670000000e-01,
    1.98879439523810e-01, 1.71494930000000e-01, 1.45037631428572e-01,
    1.21291048571429e-01, 1.03326155238095e-01, 9.53507900000000e-02,
    1.00469958095238e-01, 1.17876387142857e-01, 1.43936200000000e-01
  };

  // It would be nice if Matrix had a ctor allowing to do the
  // following without a copy
  Matrix cmap (64, 3, 0.0);
  std::copy (cmapv, cmapv + (64*3), cmap.fortran_vec ());
  return cmap;
}

static double
default_screendepth (void)
{
  octave::display_info& dpy_info = octave::__get_display_info__ ();

  return dpy_info.depth ();
}

static Matrix
default_screensize (void)
{
  Matrix retval (1, 4);

  octave::display_info& dpy_info = octave::__get_display_info__ ();

  retval(0) = 1.0;
  retval(1) = 1.0;
  retval(2) = dpy_info.width ();
  retval(3) = dpy_info.height ();

  return retval;
}

static double
default_screenpixelsperinch (void)
{
  octave::display_info& dpy_info = octave::__get_display_info__ ();

  return (dpy_info.x_dpi () + dpy_info.y_dpi ()) / 2;
}

static Matrix
default_colororder (void)
{
  Matrix retval (7, 3, 0.0);

  retval(0, 1) = 0.447;
  retval(0, 2) = 0.741;

  retval(1, 0) = 0.850;
  retval(1, 1) = 0.325;
  retval(1, 2) = 0.098;

  retval(2, 0) = 0.929;
  retval(2, 1) = 0.694;
  retval(2, 2) = 0.125;

  retval(3, 0) = 0.494;
  retval(3, 1) = 0.184;
  retval(3, 2) = 0.556;

  retval(4, 0) = 0.466;
  retval(4, 1) = 0.674;
  retval(4, 2) = 0.188;

  retval(5, 0) = 0.301;
  retval(5, 1) = 0.745;
  retval(5, 2) = 0.933;

  retval(6, 0) = 0.635;
  retval(6, 1) = 0.078;
  retval(6, 2) = 0.184;

  return retval;
}

static Matrix
default_lim (bool logscale = false)
{
  Matrix m (1, 2);

  if (logscale)
    {
      m(0) = 0.1;
      m(1) = 1.0;
    }
  else
    {
      m(0) = 0.0;
      m(1) = 1.0;
    }

  return m;
}

static Matrix
default_data (void)
{
  Matrix retval (1, 2);

  retval(0) = 0;
  retval(1) = 1;

  return retval;
}

static Matrix
default_data_lim (void)
{
  Matrix retval (1, 4);

  retval(0) = 0;
  retval(1) = 1;
  retval(2) = 1;  // minimum positive
  retval(3) = -octave::numeric_limits<double>::Inf (); // maximum negative

  return retval;
}

static Matrix
default_image_cdata (void)
{
  Matrix m (64, 64);

  int i = 0;
  for (int col = 0; col < 64; col++)
    for (int row = 0; row < 64; row++)
      {
        m(col, row) = static_cast<double> (default_im_data[i]);
        i++;
      }

  return m;
}

static Matrix
default_surface_xdata (void)
{
  Matrix m (3, 3);

  for (int col = 0; col < 3; col++)
    for (int row = 0; row < 3; row++)
      m(row, col) = col+1;

  return m;
}

static Matrix
default_surface_ydata (void)
{
  Matrix m (3, 3);

  for (int row = 0; row < 3; row++)
    for (int col = 0; col < 3; col++)
      m(row, col) = row+1;

  return m;
}

static Matrix
default_surface_zdata (void)
{
  Matrix m (3, 3, 0.0);

  for (int row = 0; row < 3; row++)
    m(row, row) = 1.0;

  return m;
}

static Matrix
default_surface_cdata (void)
{
  return default_surface_zdata ();
}

static Matrix
default_patch_faces (void)
{
  Matrix m (1, 3);

  m(0) = 1.0;
  m(1) = 2.0;
  m(2) = 3.0;

  return m;
}

static Matrix
default_patch_vertices (void)
{
  Matrix m (3, 2, 0.0);

  m(1) = 1.0;
  m(3) = 1.0;
  m(4) = 1.0;

  return m;
}

static Matrix
default_patch_xdata (void)
{
  Matrix m (3, 1, 0.0);

  m(1) = 1.0;

  return m;
}

static Matrix
default_patch_ydata (void)
{
  Matrix m (3, 1, 1.0);

  m(2) = 0.0;

  return m;
}

static Matrix
default_axes_position (void)
{
  Matrix m (1, 4);

  m(0) = 0.13;
  m(1) = 0.11;
  m(2) = 0.775;
  m(3) = 0.815;

  return m;
}

static Matrix
default_axes_outerposition (void)
{
  Matrix m (1, 4);

  m(0) = 0.0;
  m(1) = 0.0;
  m(2) = 1.0;
  m(3) = 1.0;

  return m;
}

static Matrix
default_axes_view (void)
{
  Matrix m (1, 2);

  m(0) = 0.0;
  m(1) = 90.0;

  return m;
}

static Matrix
default_axes_tick (void)
{
  Matrix m (1, 6);

  m(0) = 0.0;
  m(1) = 0.2;
  m(2) = 0.4;
  m(3) = 0.6;
  m(4) = 0.8;
  m(5) = 1.0;

  return m;
}

static Matrix
default_axes_ticklength (void)
{
  Matrix m (1, 2);

  m(0) = 0.01;
  m(1) = 0.025;

  return m;
}

static Matrix
default_figure_position (void)
{
  Matrix m (1, 4);

  m(0) = 300;
  m(1) = 200;
  m(2) = 560;
  m(3) = 420;

  return m;
}

static Matrix
default_figure_papersize (void)
{
  Matrix m (1, 2);

  m(0) = 8.5;
  m(1) = 11.0;

  return m;
}

static Matrix
default_figure_paperposition (void)
{
  Matrix m (1, 4);

  // Update if default_figure_position or default_figure_papersize change
  m(0) = 1.3421852580027660;
  m(1) = 3.3191389435020748;
  m(2) = 5.8156294839944680;
  m(3) = 4.3617221129958503;

  return m;
}

static std::string
default_graphics_toolkit (void)
{
  octave::gtk_manager& gtk_mgr = octave::__get_gtk_manager__ ();

  return gtk_mgr.default_toolkit ();
}

static Matrix
default_control_position (void)
{
  Matrix retval (1, 4);

  retval(0) = 0;
  retval(1) = 0;
  retval(2) = 80;
  retval(3) = 30;

  return retval;
}

static Matrix
default_control_sliderstep (void)
{
  Matrix retval (1, 2);

  retval(0) = 0.01;
  retval(1) = 0.1;

  return retval;
}

static Matrix
default_panel_position (void)
{
  Matrix retval (1, 4);

  retval(0) = 0;
  retval(1) = 0;
  retval(2) = 1;
  retval(3) = 1;

  return retval;
}

static Matrix
default_light_position (void)
{
  Matrix m (1, 3);

  m(0) = 1.0;
  m(1) = 0.0;
  m(2) = 1.0;

  return m;
}

static Matrix
default_table_position (void)
{
  Matrix retval (1, 4);

  retval(0) = 20;
  retval(1) = 20;
  retval(2) = 300;
  retval(3) = 300;

  return retval;
}

static Matrix
default_table_backgroundcolor (void)
{
  Matrix retval (2, 3);
  retval(0, 0) = 1;
  retval(0, 1) = 1;
  retval(0, 2) = 1;
  retval(1, 0) = 0.94;
  retval(1, 1) = 0.94;
  retval(1, 2) = 0.94;
  return retval;
}

static graphics_handle
make_graphics_handle (const std::string& go_name,
                      const graphics_handle& parent,
                      bool integer_figure_handle = false,
                      bool call_createfcn = true,
                      bool notify_toolkit = true)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  return gh_mgr.make_graphics_handle (go_name, parent, integer_figure_handle,
                                      call_createfcn, notify_toolkit);
}

static double
convert_font_size (double font_size, const caseless_str& from_units,
                   const caseless_str& to_units, double parent_height = 0)
{
  // Simple case where from_units == to_units

  if (from_units.compare (to_units))
    return font_size;

  // Converts the given fontsize using the following transformation:
  // <old_font_size> => points => <new_font_size>

  double points_size = 0;
  double res = 0;

  if (from_units.compare ("points"))
    points_size = font_size;
  else
    {
      res = xget (0, "screenpixelsperinch").double_value ();

      if (from_units.compare ("pixels"))
        points_size = font_size * 72.0 / res;
      else if (from_units.compare ("inches"))
        points_size = font_size * 72.0;
      else if (from_units.compare ("centimeters"))
        points_size = font_size * 72.0 / 2.54;
      else if (from_units.compare ("normalized"))
        points_size = font_size * parent_height * 72.0 / res;
    }

  double new_font_size = 0;

  if (to_units.compare ("points"))
    new_font_size = points_size;
  else
    {
      if (res <= 0)
        res = xget (0, "screenpixelsperinch").double_value ();

      if (to_units.compare ("pixels"))
        new_font_size = points_size * res / 72.0;
      else if (to_units.compare ("inches"))
        new_font_size = points_size / 72.0;
      else if (to_units.compare ("centimeters"))
        new_font_size = points_size * 2.54 / 72.0;
      else if (to_units.compare ("normalized"))
        {
          // Avoid setting font size to (0/0) = NaN

          if (parent_height > 0)
            new_font_size = points_size * res / (parent_height * 72.0);
        }
    }

  return new_font_size;
}

static Matrix
convert_position (const Matrix& pos, const caseless_str& from_units,
                  const caseless_str& to_units, const Matrix& parent_dim)
{
  Matrix retval (1, pos.numel (), 0.0);
  double res = 0;
  bool is_rectangle = (pos.numel () == 4);
  bool is_2D = (pos.numel () == 2);

  if (from_units.compare ("pixels"))
    retval = pos;
  else if (from_units.compare ("normalized"))
    {
      retval(0) = pos(0) * parent_dim(0) + 1;
      retval(1) = pos(1) * parent_dim(1) + 1;
      if (is_rectangle)
        {
          retval(2) = pos(2) * parent_dim(0);
          retval(3) = pos(3) * parent_dim(1);
        }
      else if (! is_2D)
        retval(2) = 0;
    }
  else if (from_units.compare ("characters"))
    {
      if (res <= 0)
        res = xget (0, "screenpixelsperinch").double_value ();

      double f = 0.0;

      // FIXME: this assumes the system font is Helvetica 10pt
      //        (for which "x" requires 6x12 pixels at 74.951 pixels/inch)
      f = 12.0 * res / 74.951;

      if (f > 0)
        {
          retval(0) = 0.5 * pos(0) * f;
          retval(1) = pos(1) * f;
          if (is_rectangle)
            {
              retval(2) = 0.5 * pos(2) * f;
              retval(3) = pos(3) * f;
            }
          else if (! is_2D)
            retval(2) = 0;
        }
    }
  else
    {
      if (res <= 0)
        res = xget (0, "screenpixelsperinch").double_value ();

      double f = 0.0;

      if (from_units.compare ("points"))
        f = res / 72.0;
      else if (from_units.compare ("inches"))
        f = res;
      else if (from_units.compare ("centimeters"))
        f = res / 2.54;

      if (f > 0)
        {
          retval(0) = pos(0) * f + 1;
          retval(1) = pos(1) * f + 1;
          if (is_rectangle)
            {
              retval(2) = pos(2) * f;
              retval(3) = pos(3) * f;
            }
          else if (! is_2D)
            retval(2) = 0;
        }
    }

  if (! to_units.compare ("pixels"))
    {
      if (to_units.compare ("normalized"))
        {
          retval(0) = (retval(0) - 1) / parent_dim(0);
          retval(1) = (retval(1) - 1) / parent_dim(1);
          if (is_rectangle)
            {
              retval(2) /= parent_dim(0);
              retval(3) /= parent_dim(1);
            }
          else if (! is_2D)
            retval(2) = 0;
        }
      else if (to_units.compare ("characters"))
        {
          if (res <= 0)
            res = xget (0, "screenpixelsperinch").double_value ();

          double f = 0.0;

          f = 12.0 * res / 74.951;

          if (f > 0)
            {
              retval(0) = 2 * retval(0) / f;
              retval(1) = retval(1) / f;
              if (is_rectangle)
                {
                  retval(2) = 2 * retval(2) / f;
                  retval(3) = retval(3) / f;
                }
              else if (! is_2D)
                retval(2) = 0;
            }
        }
      else
        {
          if (res <= 0)
            res = xget (0, "screenpixelsperinch").double_value ();

          double f = 0.0;

          if (to_units.compare ("points"))
            f = res / 72.0;
          else if (to_units.compare ("inches"))
            f = res;
          else if (to_units.compare ("centimeters"))
            f = res / 2.54;

          if (f > 0)
            {
              retval(0) = (retval(0) - 1) / f;
              retval(1) = (retval(1) - 1) / f;
              if (is_rectangle)
                {
                  retval(2) /= f;
                  retval(3) /= f;
                }
              else if (! is_2D)
                retval(2) = 0;
            }
        }
    }

  return retval;
}

static Matrix
convert_text_position (const Matrix& pos, const text::properties& props,
                       const caseless_str& from_units,
                       const caseless_str& to_units)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (props.get___myhandle__ ());

  graphics_object ax = go.get_ancestor ("axes");

  Matrix retval;

  if (ax.valid_object ())
    {
      const axes::properties& ax_props
        = dynamic_cast<const axes::properties&> (ax.get_properties ());
      graphics_xform ax_xform = ax_props.get_transform ();
      bool is_rectangle = (pos.numel () == 4);
      Matrix ax_bbox = ax_props.get_boundingbox (true),
             ax_size = ax_bbox.extract_n (0, 2, 1, 2);

      if (from_units.compare ("data"))
        {
          if (is_rectangle)
            {
              ColumnVector v1 = ax_xform.transform (pos(0), pos(1), 0),
                           v2 = ax_xform.transform (pos(0) + pos(2),
                                                    pos(1) + pos(3), 0);

              retval.resize (1, 4);

              retval(0) = v1(0) - ax_bbox(0) + 1;
              retval(1) = ax_bbox(1) + ax_bbox(3) - v1(1) + 1;
              retval(2) = v2(0) - v1(0);
              retval(3) = v1(1) - v2(1);
            }
          else
            {
              ColumnVector v = ax_xform.transform (pos(0), pos(1), pos(2));

              retval.resize (1, 3);

              retval(0) = v(0) - ax_bbox(0) + 1;
              retval(1) = ax_bbox(1) + ax_bbox(3) - v(1) + 1;
              retval(2) = 0;
            }
        }
      else
        retval = convert_position (pos, from_units, "pixels", ax_size);

      if (! to_units.compare ("pixels"))
        {
          if (to_units.compare ("data"))
            {
              if (is_rectangle)
                {
                  ColumnVector v1, v2;
                  v1 = ax_xform.untransform (retval(0)  + ax_bbox(0) - 1,
                                             ax_bbox(1) + ax_bbox(3) - retval(1) + 1);
                  v2 = ax_xform.untransform (retval(0)  + retval(2)  + ax_bbox(0) - 1,
                                             ax_bbox(1) + ax_bbox(3) - (retval(1) + retval(3)) + 1);

                  retval.resize (1, 4);

                  retval(0) = v1(0);
                  retval(1) = v1(1);
                  retval(2) = v2(0) - v1(0);
                  retval(3) = v2(1) - v1(1);
                }
              else
                {
                  ColumnVector v;
                  v = ax_xform.untransform (retval(0)  + ax_bbox(0) - 1,
                                            ax_bbox(1) + ax_bbox(3) - retval(1) + 1);

                  retval.resize (1, 3);

                  retval(0) = v(0);
                  retval(1) = v(1);
                  retval(2) = v(2);
                }
            }
          else
            retval = convert_position (retval, "pixels", to_units, ax_size);
        }
    }

  return retval;
}

// This function always returns the screensize in pixels
static Matrix
screen_size_pixels (void)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object obj = gh_mgr.get_object (0);

  Matrix sz = obj.get ("screensize").matrix_value ();

  return convert_position (sz, obj.get ("units").string_value (), "pixels",
                           sz.extract_n (0, 2, 1, 2)).extract_n (0, 2, 1, 2);
}

static double
device_pixel_ratio (graphics_handle h)
{
  double retval = 1.0;

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object fig = gh_mgr.get_object (h).get_ancestor ("figure");

  if (fig.valid_object ())
    retval = fig.get ("__device_pixel_ratio__").double_value ();

  return retval;
}

static void
convert_cdata_2 (bool is_scaled, bool is_real, double clim_0, double clim_1,
                 const double *cmapv, double x, octave_idx_type lda,
                 octave_idx_type nc, octave_idx_type i, double *av)
{
  if (is_scaled)
    x = octave::math::fix (nc * (x - clim_0) / (clim_1 - clim_0));
  else if (is_real)
    x = octave::math::fix (x - 1);

  if (octave::math::isnan (x))
    {
      av[i]       = x;
      av[i+lda]   = x;
      av[i+2*lda] = x;
    }
  else
    {
      if (x < 0)
        x = 0;
      else if (x >= nc)
        x = (nc - 1);

      octave_idx_type idx = static_cast<octave_idx_type> (x);

      av[i]       = cmapv[idx];
      av[i+lda]   = cmapv[idx+nc];
      av[i+2*lda] = cmapv[idx+2*nc];
    }
}

template <typename T>
void
convert_cdata_1 (bool is_scaled, bool is_real, double clim_0, double clim_1,
                 const double *cmapv, const T *cv, octave_idx_type lda,
                 octave_idx_type nc, double *av)
{
  for (octave_idx_type i = 0; i < lda; i++)
    convert_cdata_2 (is_scaled, is_real,
                     clim_0, clim_1, cmapv, cv[i], lda, nc, i, av);
}

static octave_value
convert_cdata (const base_properties& props, const octave_value& cdata,
               bool is_scaled, int cdim)
{
  dim_vector dv (cdata.dims ());

  // TrueColor data doesn't require conversion
  if (dv.ndims () == cdim && dv(cdim-1) == 3)
    return cdata;

  Matrix cmap (1, 3, 0.0);
  Matrix clim (1, 2, 0.0);

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (props.get___myhandle__ ());
  graphics_object ax = go.get_ancestor ("axes");

  if (ax.valid_object ())
    {
      Matrix _cmap = ax.get (caseless_str ("colormap")).matrix_value ();

      cmap = _cmap;

      if (is_scaled)
        {
          Matrix _clim = ax.get (caseless_str ("clim")).matrix_value ();

          clim = _clim;
        }
    }

  dv.resize (cdim);
  dv(cdim-1) = 3;

  NDArray a (dv);

  octave_idx_type lda = a.numel () / static_cast<octave_idx_type> (3);
  octave_idx_type nc = cmap.rows ();

  double *av = a.fortran_vec ();
  const double *cmapv = cmap.data ();

  double clim_0 = clim(0);
  double clim_1 = clim(1);

  // FIXME: There is a lot of processing time spent just on data conversion
  //        both here in graphics.cc and again in gl-render.cc.  There must
  //        be room for improvement!  Here a macro expands to a templated
  //        function which in turn calls another function (covert_cdata_2).
  //        And in gl-render.cc (opengl_renderer::draw_image), only GLfloat
  //        is supported anyways so there is another double for loop across
  //        height and width to convert all of the input data to GLfloat.

#define CONVERT_CDATA_1(ARRAY_T, VAL_FN, IS_REAL)                       \
  do                                                                    \
    {                                                                   \
      ARRAY_T tmp = cdata. VAL_FN ## array_value ();                    \
                                                                        \
      convert_cdata_1 (is_scaled, IS_REAL, clim_0, clim_1, cmapv,       \
                       tmp.data (), lda, nc, av);                       \
    }                                                                   \
  while (0)

  if (cdata.is_int8_type ())
    CONVERT_CDATA_1 (int8NDArray, int8_, false);
  else if (cdata.is_int16_type ())
    CONVERT_CDATA_1 (int16NDArray, int16_, false);
  else if (cdata.is_int32_type ())
    CONVERT_CDATA_1 (int32NDArray, int32_, false);
  else if (cdata.is_int64_type ())
    CONVERT_CDATA_1 (int64NDArray, int64_, false);
  else if (cdata.is_uint8_type ())
    CONVERT_CDATA_1 (uint8NDArray, uint8_, false);
  else if (cdata.is_uint16_type ())
    CONVERT_CDATA_1 (uint16NDArray, uint16_, false);
  else if (cdata.is_uint32_type ())
    CONVERT_CDATA_1 (uint32NDArray, uint32_, false);
  else if (cdata.is_uint64_type ())
    CONVERT_CDATA_1 (uint64NDArray, uint64_, false);
  else if (cdata.is_double_type ())
    CONVERT_CDATA_1 (NDArray,, true);
  else if (cdata.is_single_type ())
    CONVERT_CDATA_1 (FloatNDArray, float_, true);
  else if (cdata.islogical ())
    CONVERT_CDATA_1 (boolNDArray, bool_, false);
  else
    {
      // Don't throw an error; leads to an incomplete FLTK object (bug #46933).
      warning ("unsupported type for cdata (= %s).  "
               "Valid types are int8, int16, int32, int64, uint8, uint16, "
               "uint32, uint64, double, single, and bool.",
               cdata.type_name ().c_str ());
      a = NDArray (dv, 0);  // return 0 instead
    }

#undef CONVERT_CDATA_1

  return octave_value (a);
}

template <typename T>
static void
get_array_limits (const Array<T>& m, double& emin, double& emax,
                  double& eminp, double& emaxp)
{
  const T *data = m.data ();
  octave_idx_type n = m.numel ();

  for (octave_idx_type i = 0; i < n; i++)
    {
      double e = double (data[i]);

      // Don't need to test for NaN here as NaN>x and NaN<x is always false
      if (! octave::math::isinf (e))
        {
          if (e < emin)
            emin = e;

          if (e > emax)
            emax = e;

          if (e > 0 && e < eminp)
            eminp = e;

          if (e < 0 && e > emaxp)
            emaxp = e;
        }
    }
}

static bool
lookup_object_name (const caseless_str& name, caseless_str& go_name,
                    caseless_str& rest)
{
  int len = name.length ();
  int offset = 0;
  bool result = false;

  if (len >= 4)
    {
      caseless_str pfx = name.substr (0, 4);

      if (pfx.compare ("axes") || pfx.compare ("line")
          || pfx.compare ("text"))
        offset = 4;
      else if (len >= 5)
        {
          pfx = name.substr (0, 5);

          if (pfx.compare ("image") || pfx.compare ("patch"))
            offset = 5;
          else if (len >= 6)
            {
              pfx = name.substr (0, 6);

              if (pfx.compare ("figure") || pfx.compare ("uimenu"))
                offset = 6;
              else if (len >= 7)
                {
                  pfx = name.substr (0, 7);

                  if (pfx.compare ("surface") || pfx.compare ("scatter")
                      || pfx.compare ("hggroup") || pfx.compare ("uipanel")
                      || pfx.compare ("uitable"))
                    offset = 7;
                  else if (len >= 9)
                    {
                      pfx = name.substr (0, 9);

                      if (pfx.compare ("uicontrol")
                          || pfx.compare ("uitoolbar"))
                        offset = 9;
                      else if (len >= 10)
                        {
                          pfx = name.substr (0, 10);

                          if (pfx.compare ("uipushtool"))
                            offset = 10;
                          else if (len >= 12)
                            {
                              pfx = name.substr (0, 12);

                              if (pfx.compare ("uitoggletool"))
                                offset = 12;
                              else if (len >= 13)
                                {
                                  pfx = name.substr (0, 13);

                                  if (pfx.compare ("uicontextmenu")
                                      || pfx.compare ("uibuttongroup"))
                                    offset = 13;
                                }
                            }
                        }
                    }
                }
            }
        }

      if (offset > 0)
        {
          go_name = pfx;
          rest = name.substr (offset);
          result = true;
        }
    }

  return result;
}

static base_graphics_object *
make_graphics_object_from_type (const caseless_str& type,
                                const graphics_handle& h = graphics_handle (),
                                const graphics_handle& p = graphics_handle ())
{
  base_graphics_object *go = nullptr;

  if (type.compare ("figure"))
    go = new figure (h, p);
  else if (type.compare ("axes"))
    go = new axes (h, p);
  else if (type.compare ("line"))
    go = new line (h, p);
  else if (type.compare ("text"))
    go = new text (h, p);
  else if (type.compare ("image"))
    go = new image (h, p);
  else if (type.compare ("light"))
    go = new light (h, p);
  else if (type.compare ("patch"))
    go = new patch (h, p);
  else if (type.compare ("scatter"))
    go = new scatter (h, p);
  else if (type.compare ("surface"))
    go = new surface (h, p);
  else if (type.compare ("hggroup"))
    go = new hggroup (h, p);
  else if (type.compare ("uimenu"))
    go = new uimenu (h, p);
  else if (type.compare ("uicontrol"))
    go = new uicontrol (h, p);
  else if (type.compare ("uipanel"))
    go = new uipanel (h, p);
  else if (type.compare ("uibuttongroup"))
    go = new uibuttongroup (h, p);
  else if (type.compare ("uicontextmenu"))
    go = new uicontextmenu (h, p);
  else if (type.compare ("uitable"))
    go = new uitable (h, p);
  else if (type.compare ("uitoolbar"))
    go = new uitoolbar (h, p);
  else if (type.compare ("uipushtool"))
    go = new uipushtool (h, p);
  else if (type.compare ("uitoggletool"))
    go = new uitoggletool (h, p);
  return go;
}

// ---------------------------------------------------------------------

bool
base_property::set (const octave_value& v, bool do_run, bool do_notify_toolkit)
{
  if (do_set (v))
    {
      // Notify graphics toolkit.
      if (m_id >= 0 && do_notify_toolkit)
        {
          gh_manager& gh_mgr = octave::__get_gh_manager__ ();

          graphics_object go = gh_mgr.get_object (m_parent);
          if (go)
            go.update (m_id);
        }

      // run listeners
      if (do_run)
        run_listeners (GCB_POSTSET);

      return true;
    }

  return false;
}

void
base_property::run_listeners (listener_mode mode)
{
  const octave_value_list& l = m_listeners[mode];

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  for (int i = 0; i < l.length (); i++)
    gh_mgr.execute_listener (m_parent, l(i));
}

radio_values::radio_values (const std::string& opt_string)
  : m_default_val (), m_possible_vals ()
{
  std::size_t beg = 0;
  std::size_t len = opt_string.length ();
  bool done = len == 0;

  while (! done)
    {
      std::size_t end = opt_string.find ('|', beg);

      if (end == std::string::npos)
        {
          end = len;
          done = true;
        }

      std::string t = opt_string.substr (beg, end-beg);

      // Special case for '|' symbol itself
      if (t.empty () && opt_string[beg] == '|')
        {
          t = '|';
          end++;
        }

      // Might want more error checking on parsing default value...
      if (t[0] == '{')
        {
          t = t.substr (1, t.length () - 2);
          m_default_val = t;
        }
      else if (beg == 0) // ensure default value
        m_default_val = t;

      m_possible_vals.insert (t);

      beg = end + 1;
    }
}

std::string
radio_values::values_as_string (void) const
{
  std::string retval;

  for (const auto& val : m_possible_vals)
    {
      if (retval.empty ())
        {
          if (val == default_value ())
            retval = '{' + val + '}';
          else
            retval = val;
        }
      else
        {
          if (val == default_value ())
            retval += " | {" + val + '}';
          else
            retval += " | " + val;
        }
    }

  if (! retval.empty ())
    retval = "[ " + retval + " ]";

  return retval;
}

Cell
radio_values::values_as_cell (void) const
{
  octave_idx_type i = 0;
  Cell retval (nelem (), 1);

  for (const auto& val : m_possible_vals)
    retval(i++) = std::string (val);

  return retval;
}

bool
color_values::str2rgb (const std::string& str_arg)
{
  bool retval = true;

  double tmp_rgb[3] = {0, 0, 0};

  std::string str = str_arg;
  unsigned int len = str.length ();

  std::transform (str.begin (), str.end (), str.begin (), tolower);

  // "blue" must precede black for Matlab compatibility
  if (str.compare (0, len, "blue", 0, len) == 0)
    tmp_rgb[2] = 1;
  else if (str.compare (0, len, "black", 0, len) == 0
           || str.compare (0, len, "k", 0, len) == 0)
    tmp_rgb[0] = tmp_rgb[1] = tmp_rgb[2] = 0;
  else if (str.compare (0, len, "red", 0, len) == 0)
    tmp_rgb[0] = 1;
  else if (str.compare (0, len, "green", 0, len) == 0)
    tmp_rgb[1] = 1;
  else if (str.compare (0, len, "yellow", 0, len) == 0)
    tmp_rgb[0] = tmp_rgb[1] = 1;
  else if (str.compare (0, len, "magenta", 0, len) == 0)
    tmp_rgb[0] = tmp_rgb[2] = 1;
  else if (str.compare (0, len, "cyan", 0, len) == 0)
    tmp_rgb[1] = tmp_rgb[2] = 1;
  else if (str.compare (0, len, "white", 0, len) == 0
           || str.compare (0, len, "w", 0, len) == 0)
    tmp_rgb[0] = tmp_rgb[1] = tmp_rgb[2] = 1;
  else if (str[0] == '#' && len == 7)
    {
      try
        {
          tmp_rgb[0] = static_cast<double> (stoi (str.substr (1, 2), nullptr, 16))
                       / 255.0;
          tmp_rgb[1] = static_cast<double> (stoi (str.substr (3, 2), nullptr, 16))
                       / 255.0;
          tmp_rgb[2] = static_cast<double> (stoi (str.substr (5, 2), nullptr, 16))
                       / 255.0;
        }
      catch (const octave::execution_exception&)
        {
          retval = false;
        }
    }
  else if (str[0] == '#' && len == 4)
    {
      try
        {
          tmp_rgb[0] = static_cast<double> (stoi (str.substr (1, 1), nullptr, 16))
                       / 15.0;
          tmp_rgb[1] = static_cast<double> (stoi (str.substr (2, 1), nullptr, 16))
                       / 15.0;
          tmp_rgb[2] = static_cast<double> (stoi (str.substr (3, 1), nullptr, 16))
                       / 15.0;
        }
      catch (const octave::execution_exception&)
        {
          retval = false;
        }
    }
  else
    retval = false;

  if (retval)
    {
      for (int i = 0; i < 3; i++)
        m_rgb(i) = tmp_rgb[i];
    }

  return retval;
}

bool
color_property::do_set (const octave_value& val)
{
  if (val.is_string ())
    {
      std::string s = val.string_value ();

      if (s.empty ())
        error (R"(invalid value for color property "%s")",
               get_name ().c_str ());

      std::string match;

      if (m_radio_val.contains (s, match))
        {
          if (m_current_type != radio_t || match != m_current_val)
            {
              if (s.length () != match.length ())
                warning_with_id ("Octave:abbreviated-property-match",
                                 "%s: allowing %s to match %s value %s",
                                 "set", s.c_str (), get_name ().c_str (),
                                 match.c_str ());
              m_current_val = match;
              m_current_type = radio_t;
              return true;
            }
        }
      else
        {
          try
            {
              color_values col (s);

              if (m_current_type != color_t || col != m_color_val)
                {
                  m_color_val = col;
                  m_current_type = color_t;
                  return true;
                }
            }
          catch (octave::execution_exception& ee)
            {
              error (ee, R"(invalid value for color property "%s" (value = %s))",
                     get_name ().c_str (), s.c_str ());
            }
        }
    }
  else if (val.isnumeric ())
    {
      Matrix m = val.matrix_value ();

      if (m.numel () != 3)
        error (R"(invalid value for color property "%s")",
               get_name ().c_str ());

      color_values col (m(0), m(1), m(2));

      if (m_current_type != color_t || col != m_color_val)
        {
          m_color_val = col;
          m_current_type = color_t;
          return true;
        }
    }
  else
    error (R"(invalid value for color property "%s")",
           get_name ().c_str ());

  return false;
}

bool
double_radio_property::do_set (const octave_value& val)
{
  if (val.is_string ())
    {
      std::string s = val.string_value ();
      std::string match;

      if (s.empty () || ! m_radio_val.contains (s, match))
        error (R"(invalid value for double_radio property "%s")",
               get_name ().c_str ());

      if (m_current_type != radio_t || match != m_current_val)
        {
          if (s.length () != match.length ())
            warning_with_id ("Octave:abbreviated-property-match",
                             "%s: allowing %s to match %s value %s",
                             "set", s.c_str (), get_name ().c_str (),
                             match.c_str ());
          m_current_val = match;
          m_current_type = radio_t;
          return true;
        }
    }
  else if (val.is_scalar_type () && val.isreal ())
    {
      double new_dval = val.double_value ();

      if (m_current_type != double_t || new_dval != m_dval)
        {
          m_dval = new_dval;
          m_current_type = double_t;
          return true;
        }
    }
  else
    error (R"(invalid value for double_radio property "%s")",
           get_name ().c_str ());

  return false;
}

bool
array_property::validate (const octave_value& v)
{
  bool xok = false;

  // check value type
  if (m_type_constraints.size () > 0)
    {
      if (m_type_constraints.find (v.class_name ()) != m_type_constraints.end ())
        xok = true;

      // check if complex is allowed (it's also of class "double", so
      // checking that alone is not enough to ensure real type)
      if (m_type_constraints.find ("real") != m_type_constraints.end ()
          && v.iscomplex ())
        xok = false;
    }
  else
    xok = v.isnumeric () || v.is_bool_scalar ();

  if (xok && m_size_constraints.size () > 0)
    {
      dim_vector vdims = v.dims ();
      int vlen = vdims.ndims ();

      xok = false;

      // check dimensional size constraints until a match is found
      for (auto it = m_size_constraints.cbegin ();
           ! xok && it != m_size_constraints.cend ();
           ++it)
        {
          dim_vector itdims = (*it);

          if (itdims.ndims () == vlen)
            {
              xok = true;

              for (int i = 0; xok && i < vlen; i++)
                {
                  if (itdims(i) > 0)
                    {
                      if (itdims(i) != vdims(i))
                        xok = false;
                    }
                  else if (itdims(i) == 0)
                    {
                      if (! v.isempty ())
                        xok = false;
                      break;
                    }
                }
            }
        }
    }

  if (xok)
    {
      NDArray v_mat = v.array_value ();
      // Check min and max
      if (! octave::math::isnan (m_minval.first))
        {
          for (octave_idx_type i = 0; i < v_mat.numel (); i++)
            if (m_minval.second && m_minval.first > v_mat(i))
              error (R"(set: "%s" must be greater than or equal to %g)",
                     get_name ().c_str (), m_minval.first);
            else if (! m_minval.second && m_minval.first >= v_mat(i))
              error (R"(set: "%s" must be greater than %g)",
                     get_name ().c_str (), m_minval.first);
        }

      if (! octave::math::isnan (m_maxval.first))
        {
          for (octave_idx_type i = 0; i < v_mat.numel (); i++)
            if (m_maxval.second && m_maxval.first < v_mat(i))
              error (R"(set: "%s" must be less than or equal to %g)",
                     get_name ().c_str (), m_maxval.first);
            else if (! m_maxval.second && m_maxval.first <= v_mat(i))
              error (R"(set: "%s" must be less than %g)",
                     get_name ().c_str (), m_maxval.first);
        }

      if (m_finite_constraint == NO_CHECK) { /* do nothing */ }
      else if (m_finite_constraint == FINITE)
        {
          for (octave_idx_type i = 0; i < v_mat.numel (); i++)
            if (! octave::math::isfinite (v_mat(i)))
              error (R"(set: "%s" must be finite)", get_name ().c_str ());
        }
      else if (m_finite_constraint == NOT_NAN)
        {
          for (octave_idx_type i = 0; i < v_mat.numel (); i++)
            if (octave::math::isnan (v_mat(i)))
              error (R"(set: "%s" must not be nan)", get_name ().c_str ());
        }
      else if (m_finite_constraint == NOT_INF)
        {
          for (octave_idx_type i = 0; i < v_mat.numel (); i++)
            if (octave::math::isinf (v_mat(i)))
              error (R"(set: "%s" must not be infinite)", get_name ().c_str ());
        }

    }

  return xok;
}

bool
array_property::is_equal (const octave_value& v) const
{
  if (m_data.type_name () == v.type_name ())
    {
      if (m_data.dims () == v.dims ())
        {

#define CHECK_ARRAY_EQUAL(T, F, A)                                      \
          {                                                             \
            if (m_data.numel () == 1)                                     \
              return m_data.F ## scalar_value () ==                       \
                v.F ## scalar_value ();                                 \
            else                                                        \
              {                                                         \
                /* Keep copy of array_value to allow */                 \
                /* sparse/bool arrays that are converted, to */         \
                /* not be deallocated early */                          \
                const A m1 = m_data.F ## array_value ();                  \
                const T *d1 = m1.data ();                               \
                const A m2 = v.F ## array_value ();                     \
                const T *d2 = m2.data ();                               \
                                                                        \
                bool flag = true;                                       \
                                                                        \
                for (int i = 0; flag && i < m_data.numel (); i++)         \
                  if (d1[i] != d2[i])                                   \
                    flag = false;                                       \
                                                                        \
                return flag;                                            \
              }                                                         \
          }

          if (m_data.is_double_type () || m_data.islogical ())
            CHECK_ARRAY_EQUAL (double,, NDArray)
            else if (m_data.is_single_type ())
              CHECK_ARRAY_EQUAL (float, float_, FloatNDArray)
              else if (m_data.is_int8_type ())
                CHECK_ARRAY_EQUAL (octave_int8, int8_, int8NDArray)
                else if (m_data.is_int16_type ())
                  CHECK_ARRAY_EQUAL (octave_int16, int16_, int16NDArray)
                  else if (m_data.is_int32_type ())
                    CHECK_ARRAY_EQUAL (octave_int32, int32_, int32NDArray)
                    else if (m_data.is_int64_type ())
                      CHECK_ARRAY_EQUAL (octave_int64, int64_, int64NDArray)
                      else if (m_data.is_uint8_type ())
                        CHECK_ARRAY_EQUAL (octave_uint8, uint8_, uint8NDArray)
                        else if (m_data.is_uint16_type ())
                          CHECK_ARRAY_EQUAL (octave_uint16, uint16_, uint16NDArray)
                          else if (m_data.is_uint32_type ())
                            CHECK_ARRAY_EQUAL (octave_uint32, uint32_, uint32NDArray)
                            else if (m_data.is_uint64_type ())
                              CHECK_ARRAY_EQUAL (octave_uint64, uint64_, uint64NDArray)
                            }
    }

  return false;
}

void
array_property::get_data_limits (void)
{
  m_min_val = m_min_pos = octave::numeric_limits<double>::Inf ();
  m_max_val = m_max_neg = -octave::numeric_limits<double>::Inf ();

  if (! m_data.isempty ())
    {
      if (m_data.isinteger ())
        {
          if (m_data.is_int8_type ())
            get_array_limits (m_data.int8_array_value (),
                              m_min_val, m_max_val, m_min_pos, m_max_neg);
          else if (m_data.is_uint8_type ())
            get_array_limits (m_data.uint8_array_value (),
                              m_min_val, m_max_val, m_min_pos, m_max_neg);
          else if (m_data.is_int16_type ())
            get_array_limits (m_data.int16_array_value (),
                              m_min_val, m_max_val, m_min_pos, m_max_neg);
          else if (m_data.is_uint16_type ())
            get_array_limits (m_data.uint16_array_value (),
                              m_min_val, m_max_val, m_min_pos, m_max_neg);
          else if (m_data.is_int32_type ())
            get_array_limits (m_data.int32_array_value (),
                              m_min_val, m_max_val, m_min_pos, m_max_neg);
          else if (m_data.is_uint32_type ())
            get_array_limits (m_data.uint32_array_value (),
                              m_min_val, m_max_val, m_min_pos, m_max_neg);
          else if (m_data.is_int64_type ())
            get_array_limits (m_data.int64_array_value (),
                              m_min_val, m_max_val, m_min_pos, m_max_neg);
          else if (m_data.is_uint64_type ())
            get_array_limits (m_data.uint64_array_value (),
                              m_min_val, m_max_val, m_min_pos, m_max_neg);
        }
      else
        get_array_limits (m_data.array_value (),
                          m_min_val, m_max_val, m_min_pos, m_max_neg);
    }
}

bool
handle_property::do_set (const octave_value& v)
{
  // Users may want to use empty matrix to reset a handle property
  if (v.isempty ())
    {
      if (! get ().isempty ())
        {
          m_current_val = graphics_handle ();
          return true;
        }
      else
        return false;
    }

  double dv = v.xdouble_value (R"(set: invalid graphics handle for property "%s")",
                               get_name ().c_str ());

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_handle gh = gh_mgr.lookup (dv);

  // Check the object type if necessary
  bool type_ok = true;
  if (gh.ok () && ! m_type_constraints.empty ())
    {
      type_ok = false;
      graphics_object obj = gh_mgr.get_object (gh);

      for (const auto& type : m_type_constraints)
        if (obj.isa (type))
          {
            type_ok = true;
            break;
          }
    }

  if (! octave::math::isnan (gh.value ()) && ! (gh.ok () && type_ok))
    {
      if (type_ok)
        error (R"(set: invalid graphics handle (= %g) for property "%s")",
               dv, get_name ().c_str ());
      else
        error (R"(set: invalid graphics object type for property "%s")",
               get_name ().c_str ());
    }

  if (m_current_val != gh)
    {
      m_current_val = gh;
      return true;
    }

  return false;
}

/*
## Test validation of contextmenu property
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hpa = patch ("parent", hax);
%!   try
%!     set (hax, "contextmenu", hpa);
%!   catch
%!     err = lasterr ();
%!   end_try_catch
%!   assert (err, 'set: invalid graphics object type for property "contextmenu"');
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect
*/

Matrix
children_property::do_get_children (bool return_hidden) const
{
  Matrix retval (m_children_list.size (), 1);
  octave_idx_type k = 0;

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (0);

  root_figure::properties& props
    = dynamic_cast<root_figure::properties&> (go.get_properties ());

  if (! props.is_showhiddenhandles ())
    {
      for (const auto& hchild : m_children_list)
        {
          graphics_handle kid = hchild;

          if (gh_mgr.is_handle_visible (kid))
            {
              if (! return_hidden)
                retval(k++) = hchild;
            }
          else if (return_hidden)
            retval(k++) = hchild;
        }

      retval.resize (k, 1);
    }
  else
    {
      for (const auto& hchild : m_children_list)
        retval(k++) = hchild;
    }

  return retval;
}

void
children_property::do_delete_children (bool clear, bool from_root)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  if (from_root)
    {
      for (graphics_handle hchild : m_children_list)
        {
          graphics_object go = gh_mgr.get_object (hchild);
          if (go.valid_object ()
              && ! go.get_properties ().is_beingdeleted ())
            gh_mgr.free (hchild, from_root);
        }
      m_children_list.clear ();
    }
  else
    while (! m_children_list.empty ())
      {
        // gh_mgr.free removes hchild from children_list
        graphics_handle hchild = m_children_list.front ();
        graphics_object go = gh_mgr.get_object (hchild);
        if (go.valid_object ()
            && ! go.get_properties ().is_beingdeleted ())
          gh_mgr.free (hchild, from_root);
      }

  // FIXME: children_list should be clear anyway at this point.
  if (clear)
    m_children_list.clear ();
}

bool
callback_property::validate (const octave_value& v) const
{
  // case 1: empty matrix
  // case 2: function handle
  // case 3: string corresponding to known function name
  // case 4: string that can be eval()'ed
  // case 5: cell array with first element being a function handle

  if (v.isempty ())
    return true;
  else if (v.is_function_handle ())
    return true;
  else if (v.is_string ())
    // complete validation will be done at execution-time
    return true;
  else if (v.iscell () && (v.rows () == 1 || v.columns () == 1)
           && v.cell_value ()(0).is_function_handle ())
    return true;

  return false;
}

class callback_props
{
public:

  callback_props (void) : m_set () { }

  callback_props (const callback_props&) = delete;

  callback_props& operator = (const callback_props&) = delete;

  ~callback_props (void) = default;

  bool empty (void) const { return m_set.empty (); }

  void insert (const callback_property *ptr)
  {
    m_set.insert (reinterpret_cast<intptr_t> (ptr));
  }

  void erase (const callback_property *ptr)
  {
    m_set.erase (reinterpret_cast<intptr_t> (ptr));
  }

  bool contains (const callback_property *ptr) const
  {
    return m_set.find (reinterpret_cast<intptr_t> (ptr)) != m_set.end ();
  }

private:

  std::set<intptr_t> m_set;
};

// Elements of this set are pointers to currently executing
// callback_property objects.  Used to determine handle visibility
// inside callback functions.

static callback_props executing_callbacks;

void
callback_property::execute (const octave_value& data) const
{
  // We are executing a callback function, so allow handles that have
  // their handlevisibility property set to "callback" to be visible.

  octave::unwind_action executing_callbacks_cleanup
  ([=] () { executing_callbacks.erase (this); });

  if (! executing_callbacks.contains (this))
    {
      executing_callbacks.insert (this);

      if (m_callback.is_defined () && ! m_callback.isempty ())
        {
          gh_manager& gh_mgr = octave::__get_gh_manager__ ();

          gh_mgr.execute_callback (get_parent (), m_callback, data);
        }
    }
}

// Used to cache dummy graphics objects from which dynamic properties can be
// cloned.
static std::map<caseless_str, graphics_object> dprop_obj_map;

property
property::create (const std::string& name, const graphics_handle& h,
                  const caseless_str& type, const octave_value_list& args)
{
  property retval;

  if (type.compare ("string"))
    {
      std::string sv = (args.length () > 0 ? args(0).string_value () : "");

      retval = property (new string_property (name, h, sv));
    }
  else if (type.compare ("any"))
    {
      octave_value ov = (args.length () > 0 ? args(0)
                         : octave_value (Matrix ()));

      retval = property (new any_property (name, h, ov));
    }
  else if (type.compare ("radio"))
    {
      if (args.length () < 1)
        error ("addproperty: missing possible values for radio property");

      std::string sv = args(
                         0).xstring_value ("addproperty: argument for radio property must be a string");

      retval = property (new radio_property (name, h, sv));

      if (args.length () > 1)
        retval.set (args(1));
    }
  else if (type.compare ("double"))
    {
      double dv = (args.length () > 0 ? args(0).double_value () : 0.0);

      retval = property (new double_property (name, h, dv));
    }
  else if (type.compare ("handle"))
    {
      double hv = (args.length () > 0 ? args(0).double_value ()
                   : octave::numeric_limits<double>::NaN ());

      graphics_handle gh (hv);

      retval = property (new handle_property (name, h, gh));
    }
  else if (type.compare ("boolean"))
    {
      retval = property (new bool_property (name, h, false));

      if (args.length () > 0)
        retval.set (args(0));
    }
  else if (type.compare ("data"))
    {
      retval = property (new array_property (name, h, Matrix ()));

      if (args.length () > 0)
        {
          retval.set (args(0));
          // FIXME: additional argument could define constraints,
          //        but is this really useful?
        }
    }
  else if (type.compare ("color"))
    {
      color_values cv (0, 0, 0);
      radio_values rv;

      if (args.length () > 1)
        rv = radio_values (args(1).string_value ());

      retval = property (new color_property (name, h, cv, rv));

      if (args.length () > 0 && ! args(0).isempty ())
        retval.set (args(0));
      else
        retval.set (rv.default_value ());
    }
  else
    {
      caseless_str go_name, go_rest;

      if (! lookup_object_name (type, go_name, go_rest))
        error ("addproperty: unsupported type for dynamic property (= %s)",
               type.c_str ());

      graphics_object go;

      std::map<caseless_str, graphics_object>::const_iterator it
        = dprop_obj_map.find (go_name);

      if (it == dprop_obj_map.end ())
        {
          base_graphics_object *bgo = make_graphics_object_from_type (go_name);

          if (bgo)
            {
              go = graphics_object (bgo);

              dprop_obj_map[go_name] = go;
            }
        }
      else
        go = it->second;

      if (! go.valid_object ())
        error ("addproperty: invalid object type (= %s)",
               go_name.c_str ());

      property prop = go.get_properties ().get_property (go_rest);

      retval = prop.clone ();

      retval.set_parent (h);
      retval.set_name (name);

      if (args.length () > 0)
        retval.set (args(0));
    }

  return retval;
}

static void
finalize_r (const graphics_handle& h)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (h);

  if (go)
    {
      Matrix children = go.get_properties ().get_all_children ();

      for (int k = 0; k < children.numel (); k++)
        finalize_r (children(k));

      go.finalize ();
    }
}

static void
initialize_r (const graphics_handle& h)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (h);

  if (go)
    {
      Matrix children = go.get_properties ().get_all_children ();

      go.initialize ();

      for (int k = 0; k < children.numel (); k++)
        initialize_r (children(k));
    }
}

void
figure::properties::set_toolkit (const octave::graphics_toolkit& b)
{
  if (m_toolkit)
    finalize_r (get___myhandle__ ());

  m_toolkit = b;
  m___graphics_toolkit__ = b.get_name ();
  m___plot_stream__ = Matrix ();

  if (m_toolkit)
    initialize_r (get___myhandle__ ());

  mark_modified ();
}

void
figure::properties::set___mouse_mode__ (const octave_value& val_arg)
{
  std::string direction = "in";

  octave_value val = val_arg;

  if (val.is_string ())
    {
      std::string modestr = val.string_value ();

      if (modestr == "zoom in")
        {
          val = modestr = "zoom";
          direction = "in";
        }
      else if (modestr == "zoom out")
        {
          val = modestr = "zoom";
          direction = "out";
        }

      if (m___mouse_mode__.set (val, true))
        {
          std::string mode = m___mouse_mode__.current_value ();

          octave_scalar_map pm = get___pan_mode__ ().scalar_map_value ();
          pm.setfield ("Enable", mode == "pan" ? "on" : "off");
          set___pan_mode__ (pm);

          octave_scalar_map rm = get___rotate_mode__ ().scalar_map_value ();
          rm.setfield ("Enable", mode == "rotate" ? "on" : "off");
          set___rotate_mode__ (rm);

          octave_scalar_map zm = get___zoom_mode__ ().scalar_map_value ();
          zm.setfield ("Enable", mode == "zoom" ? "on" : "off");
          zm.setfield ("Direction", direction);
          set___zoom_mode__ (zm);

          mark_modified ();
        }
      else if (modestr == "zoom")
        {
          octave_scalar_map zm = get___zoom_mode__ ().scalar_map_value ();
          std::string curr_direction
            = zm.getfield ("Direction").string_value ();

          if (direction != curr_direction)
            {
              zm.setfield ("Direction", direction);
              set___zoom_mode__ (zm);

              mark_modified ();
            }
        }
    }
}

void
figure::properties::update_handlevisibility (void)
{
  if (! is_handle_visible ())
    {
      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      octave_value cf = gh_mgr.get_object (0).get ("currentfigure");

      if (! cf.isempty () && cf.double_value () == m___myhandle__)
        {
          octave::autolock guard (gh_mgr.graphics_lock ());

          octave_value kids =  gh_mgr.get_object (0).get ("children");

          if (kids.isempty ())
            gh_mgr.get_object (0).set ("currentfigure", Matrix ());
          else
            {
              NDArray kidsarray = kids.array_value ();
              gh_mgr.get_object (0).set ("currentfigure", kidsarray(0));
            }
        }
    }

  base_properties::update_handlevisibility ();
}

static void
update_text_pos (graphics_handle h)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (h);

  if (go.isa ("text"))
    {
      text::properties& tp
        = dynamic_cast<text::properties&> (go.get_properties ());
      tp.update_font ();
      tp.update_text_extent ();
    }
  else if (go.isa ("figure") || go.isa ("uipanel") || go.isa ("axes")
           || go.isa ("hggroup"))
    {
      Matrix ch = go.get_properties ().get_all_children ();
      for (octave_idx_type ii = 0; ii < ch.numel (); ii++)
        update_text_pos (graphics_handle (ch(ii)));

      if (go.isa ("axes"))
        {
          axes::properties& ap
            = dynamic_cast<axes::properties&> (go.get_properties ());
          ap.update_font ();
          ap.sync_positions ();
        }
    }
}

void
figure::properties::update___device_pixel_ratio__ (void)
{
  update_text_pos (get___myhandle__ ());
}

// ---------------------------------------------------------------------

void
property_list::set (const caseless_str& name, const octave_value& val)
{
  std::size_t offset = 0;

  std::size_t len = name.length ();

  if (len > 4)
    {
      caseless_str pfx = name.substr (0, 4);

      if (pfx.compare ("axes") || pfx.compare ("line")
          || pfx.compare ("text"))
        offset = 4;
      else if (len > 5)
        {
          pfx = name.substr (0, 5);

          if (pfx.compare ("image") || pfx.compare ("patch"))
            offset = 5;
          else if (len > 6)
            {
              pfx = name.substr (0, 6);

              if (pfx.compare ("figure") || pfx.compare ("uimenu"))
                offset = 6;
              else if (len > 7)
                {
                  pfx = name.substr (0, 7);

                  if (pfx.compare ("surface") || pfx.compare ("scatter")
                      || pfx.compare ("hggroup")|| pfx.compare ("uipanel")
                      || pfx.compare ("uitable"))
                    offset = 7;
                  else if (len > 9)
                    {
                      pfx = name.substr (0, 9);

                      if (pfx.compare ("uicontrol")
                          || pfx.compare ("uitoolbar"))
                        offset = 9;
                      else if (len > 10)
                        {
                          pfx = name.substr (0, 10);

                          if (pfx.compare ("uipushtool"))
                            offset = 10;
                          else if (len > 12)
                            {
                              pfx = name.substr (0, 12);

                              if (pfx.compare ("uitoogletool"))
                                offset = 12;
                              else if (len > 13)
                                {
                                  pfx = name.substr (0, 13);

                                  if (pfx.compare ("uicontextmenu")
                                      || pfx.compare ("uibuttongroup"))
                                    offset = 13;
                                }
                            }
                        }
                    }
                }
            }
        }

      if (offset > 0)
        {
          // FIXME: should we validate property names and values here?

          std::string pname = name.substr (offset);

          std::transform (pfx.begin (), pfx.end (), pfx.begin (), tolower);
          std::transform (pname.begin (), pname.end (), pname.begin (),
                          tolower);

          bool has_property = false;
          if (pfx == "axes")
            has_property = axes::properties::has_core_property (pname);
          else if (pfx == "figure")
            has_property = figure::properties::has_core_property (pname);
          else if (pfx == "line")
            has_property = line::properties::has_core_property (pname);
          else if (pfx == "text")
            has_property = text::properties::has_core_property (pname);
          else if (pfx == "image")
            has_property = image::properties::has_core_property (pname);
          else if (pfx == "patch")
            has_property = patch::properties::has_core_property (pname);
          else if (pfx == "scatter")
            has_property = scatter::properties::has_core_property (pname);
          else if (pfx == "surface")
            has_property = surface::properties::has_core_property (pname);
          else if (pfx == "hggroup")
            has_property = hggroup::properties::has_core_property (pname);
          else if (pfx == "uimenu")
            has_property = uimenu::properties::has_core_property (pname);
          else if (pfx == "uicontrol")
            has_property = uicontrol::properties::has_core_property (pname);
          else if (pfx == "uibuttongroup")
            has_property = uibuttongroup::properties::has_core_property (pname);
          else if (pfx == "uipanel")
            has_property = uipanel::properties::has_core_property (pname);
          else if (pfx == "uicontextmenu")
            has_property = uicontextmenu::properties::has_core_property (pname);
          else if (pfx == "uitable")
            has_property = uitable::properties::has_core_property (pname);
          else if (pfx == "uitoolbar")
            has_property = uitoolbar::properties::has_core_property (pname);
          else if (pfx == "uipushtool")
            has_property = uipushtool::properties::has_core_property (pname);

          if (! has_property)
            error ("invalid %s property '%s'", pfx.c_str (), pname.c_str ());

          bool remove = false;
          if (val.is_string ())
            {
              std::string sval = val.string_value ();

              remove = (sval == "remove");
            }

          pval_map_type& pval_map = m_plist_map[pfx];

          if (remove)
            {
              auto p = pval_map.find (pname);

              if (p != pval_map.end ())
                pval_map.erase (p);
            }
          else
            pval_map[pname] = val;
        }
    }

  if (offset == 0)
    error ("invalid default property specification");
}

octave_value
property_list::lookup (const caseless_str& name) const
{
  octave_value retval;

  std::size_t offset = 0;

  std::size_t len = name.length ();

  if (len > 4)
    {
      caseless_str pfx = name.substr (0, 4);

      if (pfx.compare ("axes") || pfx.compare ("line")
          || pfx.compare ("text"))
        offset = 4;
      else if (len > 5)
        {
          pfx = name.substr (0, 5);

          if (pfx.compare ("image") || pfx.compare ("patch"))
            offset = 5;
          else if (len > 6)
            {
              pfx = name.substr (0, 6);

              if (pfx.compare ("figure") || pfx.compare ("uimenu"))
                offset = 6;
              else if (len > 7)
                {
                  pfx = name.substr (0, 7);

                  if (pfx.compare ("surface") || pfx.compare ("scatter")
                      || pfx.compare ("hggroup") || pfx.compare ("uipanel")
                      || pfx.compare ("uitable"))
                    offset = 7;
                  else if (len > 9)
                    {
                      pfx = name.substr (0, 9);

                      if (pfx.compare ("uicontrol")
                          || pfx.compare ("uitoolbar"))
                        offset = 9;
                      else if (len > 10)
                        {
                          pfx = name.substr (0, 10);

                          if (pfx.compare ("uipushtool"))
                            offset = 10;
                          else if (len > 12)
                            {
                              pfx = name.substr (0, 12);

                              if (pfx.compare ("uitoggletool"))
                                offset = 12;
                              else if (len > 13)
                                {
                                  pfx = name.substr (0, 13);

                                  if (pfx.compare ("uicontextmenu")
                                      || pfx.compare ("uibuttongroup"))
                                    offset = 13;
                                }
                            }
                        }
                    }
                }
            }
        }

      if (offset > 0)
        {
          std::string pname = name.substr (offset);

          std::transform (pfx.begin (), pfx.end (), pfx.begin (), tolower);
          std::transform (pname.begin (), pname.end (), pname.begin (),
                          tolower);

          plist_map_const_iterator p = find (pfx);

          if (p != end ())
            {
              const pval_map_type& pval_map = p->second;

              pval_map_const_iterator q = pval_map.find (pname);

              if (q != pval_map.end ())
                retval = q->second;
            }
        }
    }

  return retval;
}

octave_scalar_map
property_list::as_struct (const std::string& prefix_arg) const
{
  octave_scalar_map m;

  for (auto p = begin (); p != end (); p++)
    {
      std::string prefix = prefix_arg + p->first;

      for (const auto& prop_val : p->second)
        m.assign (prefix + prop_val.first, prop_val.second);
    }

  return m;
}

// Set properties given as a cs-list of name, value pairs.

void
graphics_object::set (const octave_value_list& args)
{
  int nargin = args.length ();

  if (nargin == 0)
    error ("graphics_object::set: Nothing to set");

  for (int i = 0; i < nargin; )
    {
      if (args(i).isstruct () )
        {
          set (args(i).map_value ());
          i++;
        }
      else if (i < nargin - 1)
        {
          caseless_str pname = args(i).xstring_value ("set: argument %d must be a property name", i);
          octave_value val = args(i+1);
          set_value_or_default (pname, val);
          i += 2;
        }
      else
        error ("set: invalid number of arguments");
    }
}

/*
## test set with name, value pairs
%!test
%! hf = figure ("visible", "off");
%! h = plot (1:10, 10:-1:1);
%! set (h, "linewidth", 10, "marker", "x");
%! lw = get (h, "linewidth");
%! mk = get (h, "marker");
%! close (hf);
%! assert (lw, 10);
%! assert (mk, "x");
*/

// Set properties given in two cell arrays containing names and values.
void
graphics_object::set (const Array<std::string>& pnames,
                      const Cell& values, octave_idx_type row)
{
  if (pnames.numel () != values.columns ())
    error ("set: number of names must match number of value columns "
           "(%" OCTAVE_IDX_TYPE_FORMAT " != %" OCTAVE_IDX_TYPE_FORMAT ")",
           pnames.numel (), values.columns ());

  octave_idx_type k = pnames.columns ();

  for (octave_idx_type column = 0; column < k; column++)
    {
      caseless_str pname = pnames(column);
      octave_value val = values(row, column);

      set_value_or_default (pname, val);
    }
}

/*
## test set with cell array arguments
%!test
%! hf = figure ("visible", "off");
%! h = plot (1:10, 10:-1:1);
%! set (h, {"linewidth", "marker"}, {10, "x"});
%! lw = get (h, "linewidth");
%! mk = get (h, "marker");
%! close (hf);
%! assert (lw, 10);
%! assert (mk, "x");

## test set with multiple handles and cell array arguments
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (1:10, 10:-1:1, 1:10, 1:10);
%!   set (h, {"linewidth", "marker"}, {10, "x"; 5, "o"});
%!   assert (get (h, "linewidth"), {10; 5});
%!   assert (get (h, "marker"), {"x"; "o"});
%!   set (h, {"linewidth", "marker"}, {10, "x"});
%!   assert (get (h, "linewidth"), {10; 10});
%!   assert (get (h, "marker"), {"x"; "x"});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!error <set: number of graphics handles must match number of value rows>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (1:10, 10:-1:1, 1:10, 1:10);
%!   set (h, {"linewidth", "marker"}, {10, "x"; 5, "o"; 7, "."});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!error <set: number of names must match number of value columns>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (1:10, 10:-1:1, 1:10, 1:10);
%!   set (h, {"linewidth"}, {10, "x"; 5, "o"});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
*/

// Set properties given in a struct array
void
graphics_object::set (const octave_map& m)
{
  for (octave_idx_type p = 0; p < m.nfields (); p++)
    {
      // FIXME: Would it be better to extract all the keys at once rather than
      //        repeatedly call keys() inside a for loop?
      caseless_str pname = m.keys ()[p];

      octave_value val = octave_value (m.contents (pname).elem (m.numel () - 1));

      set_value_or_default (pname, val);
    }
}

/*
## test set ticklabels for compatibility
%!test
%! hf = figure ("visible", "off");
%! set (gca (), "xticklabel", [0, 0.2, 0.4, 0.6, 0.8, 1]);
%! xticklabel = get (gca (), "xticklabel");
%! close (hf);
%! assert (class (xticklabel), "char");
%! assert (size (xticklabel), [6, 3]);

%!test
%! hf = figure ("visible", "off");
%! set (gca (), "xticklabel", "0|0.2|0.4|0.6|0.8|1");
%! xticklabel = get (gca (), "xticklabel");
%! close (hf);
%! assert (class (xticklabel), "char");
%! assert (size (xticklabel), [6, 3]);

%!test
%! hf = figure ("visible", "off");
%! set (gca (), "xticklabel", ["0 "; "0.2"; "0.4"; "0.6"; "0.8"; "1 "]);
%! xticklabel = get (gca (), "xticklabel");
%! close (hf);
%! assert (class (xticklabel), "char");
%! assert (size (xticklabel), [6, 3]);

%!test
%! hf = figure ("visible", "off");
%! set (gca (), "xticklabel", {"0", "0.2", "0.4", "0.6", "0.8", "1"});
%! xticklabel = get (gca (), "xticklabel");
%! close (hf);
%! assert (class (xticklabel), "cell");
%! assert (size (xticklabel), [6, 1]);
*/

/*
## test set with struct arguments
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (1:10, 10:-1:1);
%!   set (h, struct ("linewidth", 10, "marker", "x"));
%!   assert (get (h, "linewidth"), 10);
%!   assert (get (h, "marker"), "x");
%!   h = plot (1:10, 10:-1:1, 1:10, 1:10);
%!   set (h, struct ("linewidth", {5, 10}));
%!   assert (get (h, "linewidth"), {10; 10});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## test ordering
%!test
%! markchanged = @(h, foobar, name) set (h, "userdata", [get(h,"userdata"); {name}]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = line ();
%!   set (h, "userdata", {});
%!   addlistener (h, "color", {markchanged, "color"});
%!   addlistener (h, "linewidth", {markchanged, "linewidth"});
%!   ## "linewidth" first
%!   props.linewidth = 2;
%!   props.color = "r";
%!   set (h, props);
%!   assert (get (h, "userdata"), fieldnames (props));
%!   clear props;
%!   clf ();
%!   h = line ();
%!   set (h, "userdata", {});
%!   addlistener (h, "color", {markchanged, "color"});
%!   addlistener (h, "linewidth", {markchanged, "linewidth"});
%!   ## "color" first
%!   props.color = "r";
%!   props.linewidth = 2;
%!   set (h, props);
%!   assert (get (h, "userdata"), fieldnames (props));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
*/

// Set a property to a value or to its (factory) default value.

void
graphics_object::set_value_or_default (const caseless_str& pname,
                                       const octave_value& val)
{
  if (val.is_string () && val.rows () == 1)
    {
      std::string sval = val.string_value ();

      octave_value default_val;

      if (sval == "default")
        {
          default_val = get_default (pname);

          m_rep->set (pname, default_val);
        }
      else if (sval == "factory")
        {
          default_val = get_factory_default (pname);

          m_rep->set (pname, default_val);
        }
      else
        {
          // Matlab specifically uses "\default" to escape string setting
          if (sval == R"(\default)")
            m_rep->set (pname, "default");
          else if (sval == R"(\factory)")
            m_rep->set (pname, "factory");
          else
            m_rep->set (pname, val);
        }
    }
  else
    m_rep->set (pname, val);
}

/*
## test setting of default values
%!test
%! old_lw = get (0, "defaultlinelinewidth");
%! unwind_protect
%!   hf = figure ("visible", "off");
%!   h = plot (1:10, 10:-1:1);
%!   set (0, "defaultlinelinewidth", 20);
%!   set (h, "linewidth", "default");
%!   assert (get (h, "linewidth"), 20);
%!   set (h, "linewidth", "factory");
%!   assert (get (h, "linewidth"), 0.5);
%! unwind_protect_cleanup
%!   close (hf);
%!   set (0, "defaultlinelinewidth", old_lw);
%! end_unwind_protect
*/

static double
make_handle_fraction (void)
{
  static double maxrand = RAND_MAX + 2.0;

  return (rand () + 1.0) / maxrand;
}

graphics_handle
gh_manager::get_handle (bool integer_figure_handle)
{
  graphics_handle retval;

  if (integer_figure_handle)
    {
      // Figure handles are positive integers corresponding
      // to the figure number.

      // We always want the lowest unused figure number.

      retval = 1;

      while (m_handle_map.find (retval) != m_handle_map.end ())
        retval++;
    }
  else
    {
      // Other graphics handles are negative integers plus some random
      // fractional part.  To avoid running out of integers, we recycle the
      // integer part but tack on a new random part each time.

      auto p = m_handle_free_list.begin ();

      if (p != m_handle_free_list.end ())
        {
          retval = *p;
          m_handle_free_list.erase (p);
        }
      else
        {
          retval = graphics_handle (m_next_handle);

          m_next_handle = std::ceil (m_next_handle) - 1.0 - make_handle_fraction ();
        }
    }

  return retval;
}

static bool
isfigure (double val)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (val);

  return go && go.isa ("figure");
}

void
gh_manager::free (const graphics_handle& h, bool from_root)
{
  if (h.ok ())
    {
      if (h.value () == 0)
        error ("graphics_handle::free: can't delete root object");

      auto p = m_handle_map.find (h);

      if (p == m_handle_map.end ())
        error ("graphics_handle::free: invalid object %g", h.value ());

      base_properties& bp = p->second.get_properties ();

      if (! p->second.valid_object () || bp.is_beingdeleted ())
        return;

      graphics_handle parent_h = p->second.get_parent ();
      graphics_object parent_go = nullptr;
      if (! from_root || isfigure (h.value ()))
        parent_go = get_object (parent_h);

      bp.set_beingdeleted (true);

      // delete listeners before invalidating object
      p->second.remove_all_listeners ();

      bp.delete_children (true, from_root);

      // NOTE: Call the delete function while the object's state is still valid.
      octave_value val = bp.get_deletefcn ();

      bp.execute_deletefcn ();

      // Notify graphics toolkit.
      p->second.finalize ();


      // NOTE: Call remove_child before erasing the go from the map if not
      // removing from groot.
      // A callback function might have already deleted the parent
      if ((! from_root || isfigure (h.value ())) && parent_go.valid_object ()
          && h.ok ())
        parent_go.remove_child (h);

      // Note: this will be valid only for first explicitly deleted
      // object.  All its children will then have an
      // unknown graphics toolkit.

      // Graphics handles for non-figure objects are negative
      // integers plus some random fractional part.  To avoid
      // running out of integers, we recycle the integer part
      // but tack on a new random part each time.

      m_handle_map.erase (p);

      if (h.value () < 0)
        m_handle_free_list.insert
        (std::ceil (h.value ()) - make_handle_fraction ());
    }
}

void
gh_manager::renumber_figure (const graphics_handle& old_gh,
                             const graphics_handle& new_gh)
{
  auto p = m_handle_map.find (old_gh);

  if (p == m_handle_map.end ())
    error ("graphics_handle::free: invalid object %g", old_gh.value ());

  graphics_object go = p->second;

  m_handle_map.erase (p);

  m_handle_map[new_gh] = go;

  if (old_gh.value () < 0)
    m_handle_free_list.insert (std::ceil (old_gh.value ())
                               - make_handle_fraction ());

  for (auto& hfig : m_figure_list)
    {
      if (hfig == old_gh)
        {
          hfig = new_gh;
          break;
        }
    }
}

static void
xset (const graphics_handle& h, const caseless_str& pname,
      const octave_value& val)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (h);

  go.set (pname, val);
}

static void
xset (const graphics_handle& h, const octave_value_list& args)
{
  if (args.length () > 0)
    {
      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      graphics_object go = gh_mgr.get_object (h);

      go.set (args);
    }
}

static octave_value
xget (const graphics_handle& h, const caseless_str& pname)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (h);

  return go.get (pname);
}

static graphics_handle
reparent (const octave_value& ov, const std::string& who,
          const std::string& pname, const graphics_handle& new_parent,
          bool adopt = true)
{
  double hv = ov.xdouble_value ("%s: %s must be a graphics handle",
                                who.c_str (), pname.c_str ());

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_handle h = gh_mgr.lookup (hv);

  if (! h.ok ())
    error ("%s: invalid graphics handle (= %g) for %s",
           who.c_str (), hv, pname.c_str ());

  graphics_object go = gh_mgr.get_object (h);

  graphics_handle parent_h = go.get_parent ();

  graphics_object parent_go = gh_mgr.get_object (parent_h);

  parent_go.remove_child (h);

  if (adopt)
    go.set ("parent", new_parent.value ());
  else
    go.reparent (new_parent);

  return h;
}

// This function is NOT equivalent to the scripting language function gcf.
graphics_handle
gcf (void)
{
  octave_value val = xget (0, "currentfigure");

  return val.isempty () ? octave::numeric_limits<double>::NaN ()
         : val.double_value ();
}

// This function is NOT equivalent to the scripting language function gca.
graphics_handle
gca (void)
{
  octave_value val = xget (gcf (), "currentaxes");

  return val.isempty () ? octave::numeric_limits<double>::NaN ()
         : val.double_value ();
}

static void
delete_graphics_object (const graphics_handle& h, bool from_root = false)
{
  if (h.ok ())
    {
      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      graphics_object go = gh_mgr.get_object (h);

      // Don't do recursive deleting, due to callbacks
      if (! go.get_properties ().is_beingdeleted ())
        {
          // NOTE: Freeing the handle also calls any deletefcn.  It also calls
          //       the parent's delete_child function.

          gh_mgr.free (h, from_root || go.isa ("figure"));

          Vdrawnow_requested = true;
        }
    }
}

static void
delete_graphics_object (double val, bool from_root = false)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  delete_graphics_object (gh_mgr.lookup (val), from_root || isfigure (val));
}

// Flag to stop redraws due to callbacks while deletion is in progress.
static bool delete_executing = false;

static void
delete_graphics_objects (const NDArray vals, bool from_root = false)
{
  // Prevent redraw of partially deleted objects.
  octave::unwind_protect_var<bool> restore_var (delete_executing, true);

  for (octave_idx_type i = 0; i < vals.numel (); i++)
    delete_graphics_object (vals.elem (i), from_root);
}

static void
close_figure (const graphics_handle& h)
{
  octave_value closerequestfcn = xget (h, "closerequestfcn");

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  gh_mgr.execute_callback (h, closerequestfcn);
}

static void
force_close_figure (const graphics_handle& h)
{
  // Remove the deletefcn and closerequestfcn callbacks
  // and delete the object directly.

  xset (h, "deletefcn", Matrix ());
  xset (h, "closerequestfcn", Matrix ());

  delete_graphics_object (h, true);
}

void
gh_manager::close_all_figures (void)
{
  // FIXME: should we process or discard pending events?

  m_event_queue.clear ();

  // Don't use m_figure_list_iterator because we'll be removing elements
  // from the list elsewhere.

  Matrix hlist = figure_handle_list (true);

  for (octave_idx_type i = 0; i < hlist.numel (); i++)
    {
      graphics_handle h = lookup (hlist(i));

      if (h.ok ())
        close_figure (h);
    }

  // They should all be closed now.  If not, force them to close.

  hlist = figure_handle_list (true);

  for (octave_idx_type i = 0; i < hlist.numel (); i++)
    {
      graphics_handle h = lookup (hlist(i));

      if (h.ok ())
        force_close_figure (h);
    }

  // None left now, right?

  hlist = figure_handle_list (true);

  if (hlist.numel () != 0)
    warning ("gh_manager::close_all_figures: some graphics elements failed to close");

  // Clear all callback objects from our list.

  m_callback_objects.clear ();
}

static void
adopt (const graphics_handle& parent_h, const graphics_handle& h)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object parent_go = gh_mgr.get_object (parent_h);

  parent_go.adopt (h);
}

static bool
ishghandle (const graphics_handle& h)
{
  return h.ok ();
}

static bool
ishghandle (double val)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_handle h = gh_mgr.lookup (val);

  return h.ok ();
}

static octave_value
ishghandle (const octave_value& val)
{
  octave_value retval = false;

  if (val.is_real_scalar () && ishghandle (val.double_value ()))
    retval = true;
  else if (val.isnumeric () && val.isreal ())
    {
      const NDArray handles = val.array_value ();

      boolNDArray result (handles.dims ());

      for (octave_idx_type i = 0; i < handles.numel (); i++)
        result.xelem (i) = ishghandle (handles(i));

      retval = result;
    }

  return retval;
}

static void
xcreatefcn (const graphics_handle& h)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (h);

  go.get_properties ().execute_createfcn  ();
}

static void
xinitialize (const graphics_handle& h)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (h);

  if (go)
    go.initialize ();
}

// ---------------------------------------------------------------------

static int
toggle_warn (std::string id, bool on, int state = -1)
{
  if (! on)
    {
      state = octave::warning_enabled (id);
      octave::disable_warning (id);
    }
  else
    {
      if (state == 1)
        octave::set_warning_state (id, "on");
      else if (state == 2)
        octave::set_warning_state (id, "error");
    }
  return state;
}

static void
xreset_default_properties (graphics_handle h,
                           property_list::pval_map_type factory_pval)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (h);

  // Replace factory defaults by user defined ones
  std::string go_name = go.get_properties ().graphics_object_name ();
  property_list::pval_map_type pval;
  go.build_user_defaults_map (pval, go_name);

  for (const auto& p : pval)
    factory_pval[p.first] = p.second;

  // Save warning state of "Octave:deprecated-property"
  int state = toggle_warn ("Octave:deprecated-property", false);

  // Reset defaults
  for (const auto& p : factory_pval)
    {
      std::string pname = p.first;

      // Don't reset internal properties and handle_properties
      if (! go.has_readonly_property (pname)
          && pname.find ("__") != 0 && pname.find ("current") != 0
          && pname != "uicontextmenu" && pname != "parent")
        {
          // Store *mode prop/val in order to set them last
          if (pname.find ("mode") == (pname.length () - 4))
            pval[pname] = p.second;
          else
            go.set (pname, p.second);
        }
    }

  // set *mode properties
  for (const auto& p : pval)
    go.set (p.first, p.second);

  toggle_warn ("Octave:deprecated-property", true, state);
}

// ---------------------------------------------------------------------

void
base_properties::set_from_list (base_graphics_object& bgo,
                                property_list& defaults)
{
  std::string go_name = graphics_object_name ();

  property_list::plist_map_const_iterator plist = defaults.find (go_name);

  if (plist != defaults.end ())
    {
      const property_list::pval_map_type pval_map = plist->second;

      for (const auto& prop_val : pval_map)
        {
          std::string pname = prop_val.first;

          try
            {
              bgo.set (pname, prop_val.second);
            }
          catch (octave::execution_exception& ee)
            {
              error (ee, "error setting default property %s", pname.c_str ());
            }
        }
    }
}

/*
## test defaults are set in the order they were stored
%!test
%! set (0, "defaultfigureunits", "normalized");
%! set(0, "defaultfigureposition", [0.7 0 0.3 0.3]);
%! hf = figure ("visible", "off");
%! tol = 20 * eps;
%! unwind_protect
%!   assert (get (hf, "position"), [0.7 0 0.3 0.3], tol);
%! unwind_protect_cleanup
%!   close (hf);
%!   set (0, "defaultfigureunits", "remove");
%!   set (0, "defaultfigureposition", "remove");
%! end_unwind_protect
*/

octave_value
base_properties::get_dynamic (const caseless_str& pname) const
{
  std::map<caseless_str, property, cmp_caseless_str>::const_iterator it
    = m_all_props.find (pname);

  if (it == m_all_props.end ())
    error (R"(get: unknown property "%s")", pname.c_str ());

  return it->second.get ();
}

octave_value
base_properties::get_dynamic (bool all) const
{
  octave_scalar_map m;

  for (std::map<caseless_str, property, cmp_caseless_str>::const_iterator
       it = m_all_props.begin (); it != m_all_props.end (); ++it)
    if (all || ! it->second.is_hidden ())
      m.assign (it->second.get_name (), it->second.get ());

  return m;
}

std::set<std::string>
base_properties::dynamic_property_names (void) const
{
  return m_dynamic_properties;
}

bool
base_properties::has_dynamic_property (const std::string& pname) const
{
  const std::set<std::string>& dynprops = dynamic_property_names ();

  if (dynprops.find (pname) != dynprops.end ())
    return true;
  else
    return m_all_props.find (pname) != m_all_props.end ();
}

void
base_properties::set_dynamic (const caseless_str& pname,
                              const octave_value& val)
{
  auto it = m_all_props.find (pname);

  if (it == m_all_props.end ())
    error (R"(set: unknown property "%s")", pname.c_str ());

  it->second.set (val);

  m_dynamic_properties.insert (pname);

  mark_modified ();
}

property
base_properties::get_property_dynamic (const caseless_str& pname) const
{
  std::map<caseless_str, property, cmp_caseless_str>::const_iterator it
    = m_all_props.find (pname);

  if (it == m_all_props.end ())
    error (R"(get_property: unknown property "%s")", pname.c_str ());

  return it->second;
}

void
base_properties::set_parent (const octave_value& val)
{
  double hp = val.xdouble_value ("set: parent must be a graphics handle");
  if (hp == m___myhandle__)
    error ("set: can not set object parent to be object itself");

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_handle new_parent = gh_mgr.lookup (hp);
  if (! new_parent.ok ())
    error ("set: invalid graphics handle (= %g) for parent", hp);

  // Remove child from current parent
  graphics_object old_parent_go;
  old_parent_go = gh_mgr.get_object (get_parent ());

  if (old_parent_go.get_handle () != hp)
    old_parent_go.remove_child (m___myhandle__);
  else
    return;  // Do nothing more

  // Check new parent's parent is not this child to avoid recursion
  graphics_object new_parent_go;
  new_parent_go = gh_mgr.get_object (new_parent);
  if (new_parent_go.get_parent () == m___myhandle__)
    {
      // new parent's parent gets child's original parent
      new_parent_go.get_properties ().set_parent (get_parent ().as_octave_value ());
    }

  // Set parent property to new_parent and do adoption
  m_parent = new_parent.as_octave_value ();
  octave::adopt (m_parent.handle_value (), m___myhandle__);
}

/*
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = gca ();
%!   set (hax, "parent", gcf ());
%!   assert (gca (), hax);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
*/

void
base_properties::mark_modified (void)
{
  // Mark existing object as modified
  m___modified__ = "on";

  // Attempt to mark parent object as modified if it exists

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object parent_go = gh_mgr.get_object (get_parent ());

  if (parent_go)
    parent_go.mark_modified ();
}

void
base_properties::override_defaults (base_graphics_object& obj)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object parent_go = gh_mgr.get_object (get_parent ());

  if (parent_go)
    parent_go.override_defaults (obj);
}

void
base_properties::update_axis_limits (const std::string& axis_type) const
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (m___myhandle__);

  if (go)
    go.update_axis_limits (axis_type);
}

void
base_properties::update_axis_limits (const std::string& axis_type,
                                     const graphics_handle& h) const
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (m___myhandle__);

  if (go)
    go.update_axis_limits (axis_type, h);
}

void
base_properties::update_contextmenu (void) const
{
  if (m_contextmenu.get ().isempty ())
    return;

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (m_contextmenu.get ());

  if (go && go.isa ("uicontextmenu"))
    {
      uicontextmenu::properties& props
        = reinterpret_cast<uicontextmenu::properties&> (go.get_properties ());
      props.add_dependent_obj (m___myhandle__);
    }
}

bool
base_properties::is_handle_visible (void) const
{
  return (m_handlevisibility.is ("on")
          || (! executing_callbacks.empty () && ! m_handlevisibility.is ("off")));
}

octave::graphics_toolkit
base_properties::get_toolkit (void) const
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (get_parent ());

  if (go)
    return go.get_toolkit ();
  else
    return octave::graphics_toolkit ();
}

void
base_properties::update_boundingbox (void)
{
  Matrix kids = get_children ();

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  for (int i = 0; i < kids.numel (); i++)
    {
      graphics_object go = gh_mgr.get_object (kids(i));

      if (go.valid_object ())
        go.get_properties ().update_boundingbox ();
    }
}

void
base_properties::update_autopos (const std::string& elem_type)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object parent_go = gh_mgr.get_object (get_parent ());

  if (parent_go.valid_object ())
    parent_go.get_properties ().update_autopos (elem_type);
}

void
base_properties::update_handlevisibility (void)
{
  if (is_handle_visible ())
    return;

  // This object should not be the figure "currentobject"

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go (gh_mgr.get_object (get___myhandle__ ()));

  graphics_object fig (go.get_ancestor ("figure"));

  if (fig.valid_object ())
    {
      octave_value co = fig.get ("currentobject");
      if (! co.isempty () && co.double_value () == m___myhandle__)
        {
          octave::autolock guard (gh_mgr.graphics_lock ());

          auto& fig_props = dynamic_cast<figure::properties&> (fig.get_properties ());
          fig_props.set_currentobject (Matrix ());
        }
    }
}

/*
## test current figure and current axes have visible handles
%!test
%! hf1 = figure ("visible", "off");
%! hf2 = figure ("visible", "off");
%! hax1 = axes ();
%! hax2 = axes ();
%! unwind_protect
%!   assert (get (0, "currentfigure"), hf2);
%!   assert (get (hf2, "currentaxes"), hax2);
%!   set (hf2, "handlevisibility", "off");
%!   assert (get (0, "currentfigure"), hf1);
%!   set (hax2, "handlevisibility", "off");
%!   assert (get (hf2, "currentaxes"), hax1);
%!   assert (get (hf2, "currentobject"), []);
%! unwind_protect_cleanup
%!   close ([hf1, hf2]);
%! end_unwind_protect
*/

/*
## test current callback object have visible handle
%!test
%! hf = figure ("handlevisibility", "off", "visible", "off");
%! hax = axes ("parent", hf, "handlevisibility", "off");
%! unwind_protect
%!   fcn = @(h, ~) setappdata (h, "testdata", gcbo ());
%!   addlistener (hf, "color", fcn);
%!   addlistener (hax, "color", fcn);
%!   set (hf, "color", "b");
%!   set (hax, "color", "b");
%!   assert (getappdata (hf, "testdata"), hf)
%!   assert (getappdata (hax, "testdata"), hax)
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
*/

void
base_properties::add_listener (const caseless_str& pname,
                               const octave_value& val,
                               listener_mode mode)
{
  property p = get_property (pname);

  if (p.ok ())
    p.add_listener (val, mode);
}

void
base_properties::delete_listener (const caseless_str& pname,
                                  const octave_value& val,
                                  listener_mode mode)
{
  property p = get_property (pname);

  if (p.ok ())
    p.delete_listener (val, mode);
}

void
base_properties::get_children_of_type (const caseless_str& chtype,
                                       bool get_invisible,
                                       bool traverse,
                                       std::list<graphics_object>& children_list) const
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  Matrix ch = get_children ();

  for (octave_idx_type i = 0; i < ch.numel (); i++)
    {
      graphics_handle hkid = gh_mgr.lookup (ch(i));

      if (hkid.ok ())
        {
          graphics_object go = gh_mgr.get_object (hkid);
          if ( get_invisible || go.get_properties ().is_visible () )
            {
              if (go.isa (chtype))
                children_list.push_back (go);
              else if (traverse && go.isa ("hggroup"))
                go.get_properties ().get_children_of_type (chtype,
                    get_invisible,
                    traverse,
                    children_list);
            }
        }
    }
}

// ---------------------------------------------------------------------

void
base_graphics_object::update_axis_limits (const std::string& axis_type)
{
  if (! valid_object ())
    error ("base_graphics_object::update_axis_limits: invalid graphics object");

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object parent_go = gh_mgr.get_object (get_parent ());

  if (parent_go)
    parent_go.update_axis_limits (axis_type);
}

void
base_graphics_object::update_axis_limits (const std::string& axis_type,
    const graphics_handle& h)
{
  if (! valid_object ())
    error ("base_graphics_object::update_axis_limits: invalid graphics object");

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object parent_go = gh_mgr.get_object (get_parent ());

  if (parent_go)
    parent_go.update_axis_limits (axis_type, h);
}

void
base_graphics_object::remove_all_listeners (void)
{
  int state = toggle_warn ("Octave:deprecated-property", false);
  octave_map m = get (true).map_value ();
  toggle_warn ("Octave:deprecated-property", true, state);

  for (const auto& pm : m)
    {
      // FIXME: there has to be a better way.  I think we want to
      // ask whether it is OK to delete the listener for the given
      // property.  How can we know in advance that it will be OK?

      octave::unwind_protect frame;

      octave::interpreter_try (frame);

      try
        {
          property p = get_properties ().get_property (pm.first);

          if (p.ok ())
            p.delete_listener ();
        }
      catch (const octave::execution_exception&)
        {
          octave::interpreter& interp = octave::__get_interpreter__ ();

          interp.recover_from_exception ();
        }
    }
}

void
base_graphics_object::build_user_defaults_map (property_list::pval_map_type& def,
    const std::string go_name) const
{
  property_list local_defaults = get_defaults_list ();
  const auto it = local_defaults.find (go_name);

  if (it != local_defaults.end ())
    {
      property_list::pval_map_type pval_lst = it->second;
      for (const auto& prop_val : pval_lst)
        {
          std::string pname = prop_val.first;
          if (def.find (pname) == def.end ())
            def[pname] = prop_val.second;
        }
    }

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object parent_go = gh_mgr.get_object (get_parent ());

  if (parent_go)
    parent_go.build_user_defaults_map (def, go_name);
}

void
base_graphics_object::reset_default_properties (void)
{
  if (valid_object ())
    {
      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      property_list::pval_map_type factory_pval
        = gh_mgr.get_object (0).get_factory_defaults_list ().find (type ())->second;

      remove_all_listeners ();
      xreset_default_properties (get_handle (), factory_pval);
    }
}

std::string
base_graphics_object::values_as_string (void)
{
  if (! valid_object ())
    error ("base_graphics_object::values_as_string: invalid graphics object");

  std::string retval;
  octave_map m = get ().map_value ();

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (get_handle ());

  for (const auto& pm : m)
    {
      const auto& pname = pm.first;
      if (pname != "children" && ! go.has_readonly_property (pname))
        {
          property p = get_properties ().get_property (pname);

          if (p.ok () && ! p.is_hidden ())
            {
              retval += "\n\t" + std::string (pname) + ":  ";
              if (p.is_radio ())
                retval += p.values_as_string ();
            }
        }
    }

  if (! retval.empty ())
    retval += '\n';

  return retval;
}

std::string
base_graphics_object::value_as_string (const std::string& prop)
{
  std::string retval;

  if (! valid_object ())
    error ("base_graphics_object::value_as_string: invalid graphics object");

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (get_handle ());

  if (prop != "children" && ! go.has_readonly_property (prop))
    {
      property p = get_properties ().get_property (prop);

      if (p.ok () && ! p.is_hidden ())
        {
          if (p.is_radio ())
            retval += p.values_as_string ();
        }
    }

  if (! retval.empty ())
    retval += '\n';

  return retval;
}

octave_scalar_map
base_graphics_object::values_as_struct (void)
{
  octave_scalar_map retval;

  if (! valid_object ())
    error ("base_graphics_object::values_as_struct: invalid graphics object");

  octave_scalar_map m = get ().scalar_map_value ();

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (get_handle ());

  for (const auto& pm : m)
    {
      const auto& pname = pm.first;
      if (pname != "children" && ! go.has_readonly_property (pname))
        {
          property p = get_properties ().get_property (pname);

          if (p.ok () && ! p.is_hidden ())
            {
              if (p.is_radio ())
                retval.assign (p.get_name (), p.values_as_cell ());
              else
                retval.assign (p.get_name (), Cell ());
            }
        }
    }

  return retval;
}

/*
%!test
%! hfig = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ();
%!   ret = set (hax, "tightinset");
%!   assert (isempty (ret));
%!   ret = set (hax, "type");
%!   assert (isempty (ret));
%!   ret = set (hfig, "tag");
%!   assert (isempty (ret));
%!   ret = set (0, "commandwindowsize");
%!   assert (isempty (ret));
%!   ret = set (0);
%!   assert (! isfield (ret, "commandwindowsize"));
%! unwind_protect_cleanup
%!   close (hfig);
%! end_unwind_protect
*/

graphics_object
graphics_object::get_ancestor (const std::string& obj_type) const
{
  if (valid_object ())
    {
      if (isa (obj_type))
        return *this;
      else
        {
          gh_manager& gh_mgr = octave::__get_gh_manager__ ();

          return gh_mgr.get_object (get_parent ()).get_ancestor (obj_type);
        }
    }
  else
    return graphics_object ();
}

// ---------------------------------------------------------------------

#include "graphics-props.cc"

// ---------------------------------------------------------------------

void
root_figure::properties::set_callbackobject (const octave_value& v)
{
  graphics_handle val (v);

  if (octave::math::isnan (val.value ()))
    m_callbackobject = graphics_handle ();
  else if (ishghandle (val))
    m_callbackobject = val;
  else
    err_set_invalid ("callbackobject");
}

void
root_figure::properties::set_currentfigure (const octave_value& v)
{
  graphics_handle val (v);

  if (octave::math::isnan (val.value ()) || ishghandle (val))
    {
      m_currentfigure = val;

      if (val.ok ())
        {
          gh_manager& gh_mgr = octave::__get_gh_manager__ ();

          gh_mgr.push_figure (val);
        }
    }
  else
    err_set_invalid ("currentfigure");
}

void
figure::properties::set_integerhandle (const octave_value& val)
{
  if (m_integerhandle.set (val, true))
    {
      bool int_fig_handle = m_integerhandle.is_on ();

      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      graphics_object this_go = gh_mgr.get_object (m___myhandle__);

      graphics_handle old_myhandle = m___myhandle__;

      m___myhandle__ = gh_mgr.get_handle (int_fig_handle);

      gh_mgr.renumber_figure (old_myhandle, m___myhandle__);

      graphics_object parent_go = gh_mgr.get_object (get_parent ());

      base_properties& props = parent_go.get_properties ();

      props.renumber_child (old_myhandle, m___myhandle__);

      Matrix kids = get_children ();

      for (octave_idx_type i = 0; i < kids.numel (); i++)
        {
          graphics_object kid = gh_mgr.get_object (kids(i));

          kid.get_properties ().renumber_parent (m___myhandle__);
        }

      graphics_handle cf = gh_mgr.current_figure ();

      if (m___myhandle__ == cf)
        xset (0, "currentfigure", m___myhandle__.value ());

      this_go.update (m_integerhandle.get_id ());

      mark_modified ();
    }
}

// FIXME: This should update monitorpositions and pointerlocation, but as these
// properties aren't yet used, it doesn't matter that they aren't set either.
void
root_figure::properties::update_units (void)
{
  std::string xunits = get_units ();

  Matrix scrn_sz = default_screensize ();

  double dpi = get_screenpixelsperinch ();

  if (xunits == "pixels")
    {
      // Most common case (default).
      // Don't need to convert anything, but short-circuit if/else tree.
    }
  else if (xunits == "normalized")
    {
      scrn_sz = Matrix (1, 4, 1.0);
      scrn_sz(0) = 0;
      scrn_sz(1) = 0;
    }
  else if (xunits == "inches")
    {
      scrn_sz(0) = 0;
      scrn_sz(1) = 0;
      scrn_sz(2) /= dpi;
      scrn_sz(3) /= dpi;
    }
  else if (xunits == "centimeters")
    {
      scrn_sz(0) = 0;
      scrn_sz(1) = 0;
      scrn_sz(2) *= 2.54 / dpi;
      scrn_sz(3) *= 2.54 / dpi;
    }
  else if (xunits == "points")
    {
      scrn_sz(0) = 0;
      scrn_sz(1) = 0;
      scrn_sz(2) *= 72 / dpi;
      scrn_sz(3) *= 72 / dpi;
    }
  else if (xunits == "characters")
    {
      scrn_sz(0) = 0;
      scrn_sz(1) = 0;
      // FIXME: this assumes the system font is Helvetica 10pt
      //        (for which "x" requires 6x12 pixels at 74.951 pixels/inch)
      scrn_sz(2) *= 74.951 / 12.0 / dpi;
      scrn_sz(3) *= 74.951 / 12.0 / dpi;
    }

  set_screensize (scrn_sz);
}

Matrix
root_figure::properties::get_boundingbox (bool, const Matrix&) const
{
  Matrix screen_size = screen_size_pixels ();
  Matrix pos = Matrix (1, 4, 0.0);

  pos(2) = screen_size(0);
  pos(3) = screen_size(1);

  return pos;
}

/*
%!test
%! old_units = get (0, "units");
%! unwind_protect
%!   set (0, "units", "pixels");
%!   sz = get (0, "screensize") - [1, 1, 0, 0];
%!   dpi = get (0, "screenpixelsperinch");
%!   set (0, "units", "inches");
%!   assert (get (0, "screensize"), sz / dpi, 0.5 / dpi);
%!   set (0, "units", "centimeters");
%!   assert (get (0, "screensize"), sz / dpi * 2.54, 0.5 / dpi * 2.54);
%!   set (0, "units", "points");
%!   assert (get (0, "screensize"), sz / dpi * 72, 0.5 / dpi * 72);
%!   set (0, "units", "normalized");
%!   assert (get (0, "screensize"), [0.0, 0.0, 1.0, 1.0]);
%!   set (0, "units", "pixels");
%!   assert (get (0, "screensize"), sz + [1, 1, 0, 0]);
%!   set (0, "units", "characters");
%!   assert (get (0, "screensize"),
%!           sz / dpi * (74.951 / 12.0), 0.5 / dpi * (74.951 / 12.0));
%! unwind_protect_cleanup
%!   set (0, "units", old_units);
%! end_unwind_protect
*/

void
root_figure::properties::remove_child (const graphics_handle& h, bool)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  gh_mgr.pop_figure (h);

  graphics_handle cf = gh_mgr.current_figure ();

  xset (0, "currentfigure", cf.value ());

  base_properties::remove_child (h, true);
}

void
root_figure::reset_default_properties (void)
{
  // empty list of local defaults
  m_default_properties = property_list ();

  remove_all_listeners ();
  xreset_default_properties (get_handle (),
                             m_properties.factory_defaults ());
}

// ---------------------------------------------------------------------

void
figure::properties::set_currentaxes (const octave_value& val)
{
  graphics_handle hax (val);

  if (octave::math::isnan (hax.value ()) || ishghandle (hax))
    m_currentaxes = hax;
  else
    err_set_invalid ("currentaxes");
}

void
figure::properties::remove_child (const graphics_handle& h, bool from_root)
{
  base_properties::remove_child (h, from_root);

  if (h == m_currentaxes.handle_value ())
    {
      graphics_handle new_currentaxes;

      Matrix kids = get_children ();

      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      for (octave_idx_type i = 0; i < kids.numel (); i++)
        {
          graphics_handle kid = kids(i);

          graphics_object go = gh_mgr.get_object (kid);

          if (go.isa ("axes"))
            {
              new_currentaxes = kid;
              break;
            }
        }

      m_currentaxes = new_currentaxes;
    }
}

octave_value
figure::properties::get_number (void) const
{
  if (m_integerhandle.is_on ())
    return m___myhandle__.value ();
  else
    return Matrix ();
}

octave::graphics_toolkit
figure::properties::get_toolkit (void) const
{
  return m_toolkit;
}

void
figure::properties::set___graphics_toolkit__ (const octave_value& val)
{
  if (! val.is_string ())
    error ("set___graphics_toolkit__: toolkit must be a string");

  std::string nm = val.string_value ();

  octave::gtk_manager& gtk_mgr = octave::__get_gtk_manager__ ();

  octave::graphics_toolkit b = gtk_mgr.find_toolkit (nm);

  if (b.get_name () != nm)
    error ("set___graphics_toolkit__: invalid graphics toolkit");

  if (nm != get___graphics_toolkit__ ())
    {
      set_toolkit (b);
      mark_modified ();
    }
}

void
figure::properties::adopt (const graphics_handle& h)
{
  base_properties::adopt (h);

  if (! get_currentaxes ().ok ())
    {
      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      graphics_object go = gh_mgr.get_object (h);

      if (go.type () == "axes")
        set_currentaxes (h.as_octave_value ());
    }
}

/*
%!test
%! hf1 = figure ("visible", "off");
%! ax1 = subplot (1,2,1);
%! ax2 = subplot (1,2,2);
%! hf2 = figure ("visible", "off");
%! unwind_protect
%!   set (ax2, "parent", hf2);
%!   assert (get (hf2, "currentaxes"), ax2);
%!   assert (get (hf1, "currentaxes"), ax1);
%!   set (ax1, "parent", hf2);
%!   assert (get (hf2, "currentaxes"), ax2);
%! unwind_protect_cleanup
%!   close (hf1);
%!   close (hf2);
%! end_unwind_protect
*/

void
figure::properties::set_visible (const octave_value& val)
{
  std::string sval = val.string_value ();

  if (sval == "on")
    xset (0, "currentfigure", m___myhandle__.value ());

  m_visible = val;
}

Matrix
figure::properties::get_boundingbox (bool internal, const Matrix&) const
{
  Matrix screen_size = screen_size_pixels ();
  Matrix pos = (internal ?
                get_position ().matrix_value () :
                get_outerposition ().matrix_value ());

  pos = convert_position (pos, get_units (), "pixels", screen_size);

  pos(0)--;
  pos(1)--;
  pos(1) = screen_size(1) - pos(1) - pos(3);

  return pos;
}

Matrix
figure::properties::bbox2position (const Matrix& bb) const
{
  Matrix screen_size = screen_size_pixels ();
  Matrix pos = bb;

  pos(1) = screen_size(1) - pos(1) - pos(3);
  pos(1)++;
  pos(0)++;
  pos = convert_position (pos, "pixels", get_units (), screen_size);
  return pos;
}

void
figure::properties::set_boundingbox (const Matrix& bb, bool internal,
                                     bool do_notify_toolkit)
{
  Matrix screen_size = screen_size_pixels ();
  Matrix pos = bbox2position (bb);

  if (internal)
    set_position (pos, do_notify_toolkit);
  else
    set_outerposition (pos, do_notify_toolkit);
}

Matrix
figure::properties::map_from_boundingbox (double x, double y) const
{
  Matrix bb = get_boundingbox (true);
  Matrix pos (1, 2, 0.0);

  pos(0) = x;
  pos(1) = y;

  pos(1) = bb(3) - pos(1);
  pos(0)++;
  pos = convert_position (pos, "pixels", get_units (),
                          bb.extract_n (0, 2, 1, 2));

  return pos;
}

Matrix
figure::properties::map_to_boundingbox (double x, double y) const
{
  Matrix bb = get_boundingbox (true);
  Matrix pos (1, 2, 0.0);

  pos(0) = x;
  pos(1) = y;

  pos = convert_position (pos, get_units (), "pixels",
                          bb.extract_n (0, 2, 1, 2));
  pos(0)--;
  pos(1) = bb(3) - pos(1);

  return pos;
}

void
figure::properties::set_position (const octave_value& v,
                                  bool do_notify_toolkit)
{
  Matrix old_bb, new_bb;
  bool modified = false;

  old_bb = get_boundingbox (true);
  modified = m_position.set (v, false, do_notify_toolkit);
  new_bb = get_boundingbox (true);

  if (old_bb != new_bb)
    {
      if (old_bb(2) != new_bb(2) || old_bb(3) != new_bb(3))
        {
          gh_manager& gh_mgr = octave::__get_gh_manager__ ();

          if (! get_resizefcn ().isempty ())
            gh_mgr.post_callback (m___myhandle__, "resizefcn");

          if (! get_sizechangedfcn ().isempty ())
            gh_mgr.post_callback (m___myhandle__, "sizechangedfcn");

          update_boundingbox ();
        }
    }

  if (modified)
    {
      m_position.run_listeners (GCB_POSTSET);
      mark_modified ();
    }

  if (m_paperpositionmode.is ("auto"))
    m_paperposition.set (get_auto_paperposition ());
}

void
figure::properties::set_outerposition (const octave_value& v,
                                       bool do_notify_toolkit)
{
  if (m_outerposition.set (v, true, do_notify_toolkit))
    mark_modified ();
}

void
figure::properties::set_paperunits (const octave_value& val)
{
  caseless_str punits = val.string_value ();
  caseless_str ptype = get_papertype ();

  if (punits.compare ("normalized") && ptype.compare ("<custom>"))
    error ("set: can't set paperunits to normalized when papertype is custom");

  caseless_str old_paperunits = get_paperunits ();
  if (m_paperunits.set (val, true))
    {
      update_paperunits (old_paperunits);
      mark_modified ();
    }
}

void
figure::properties::set_papertype (const octave_value& val)
{
  caseless_str ptype = val.string_value ();
  caseless_str punits = get_paperunits ();

  if (punits.compare ("normalized") && ptype.compare ("<custom>"))
    error ("set: can't set paperunits to normalized when papertype is custom");

  if (m_papertype.set (val, true))
    {
      update_papertype ();
      mark_modified ();
    }
}

static Matrix
papersize_from_type (const caseless_str punits, const caseless_str ptype)
{
  Matrix retval (1, 2, 1.0);

  if (! punits.compare ("normalized"))
    {
      double in2units;
      double mm2units;

      if (punits.compare ("inches"))
        {
          in2units = 1.0;
          mm2units = 1 / 25.4;
        }
      else if (punits.compare ("centimeters"))
        {
          in2units = 2.54;
          mm2units = 1 / 10.0;
        }
      else // points
        {
          in2units = 72.0;
          mm2units = 72.0 / 25.4;
        }

      if (ptype.compare ("usletter"))
        {
          retval(0) = 8.5 * in2units;
          retval(1) = 11.0 * in2units;
        }
      else if (ptype.compare ("uslegal"))
        {
          retval(0) = 8.5 * in2units;
          retval(1) = 14.0 * in2units;
        }
      else if (ptype.compare ("tabloid"))
        {
          retval(0) = 11.0 * in2units;
          retval(1) = 17.0 * in2units;
        }
      else if (ptype.compare ("a0"))
        {
          retval(0) = 841.0 * mm2units;
          retval(1) = 1189.0 * mm2units;
        }
      else if (ptype.compare ("a1"))
        {
          retval(0) = 594.0 * mm2units;
          retval(1) = 841.0 * mm2units;
        }
      else if (ptype.compare ("a2"))
        {
          retval(0) = 420.0 * mm2units;
          retval(1) = 594.0 * mm2units;
        }
      else if (ptype.compare ("a3"))
        {
          retval(0) = 297.0 * mm2units;
          retval(1) = 420.0 * mm2units;
        }
      else if (ptype.compare ("a4"))
        {
          retval(0) = 210.0 * mm2units;
          retval(1) = 297.0 * mm2units;
        }
      else if (ptype.compare ("a5"))
        {
          retval(0) = 148.0 * mm2units;
          retval(1) = 210.0 * mm2units;
        }
      else if (ptype.compare ("b0"))
        {
          retval(0) = 1029.0 * mm2units;
          retval(1) = 1456.0 * mm2units;
        }
      else if (ptype.compare ("b1"))
        {
          retval(0) = 728.0 * mm2units;
          retval(1) = 1028.0 * mm2units;
        }
      else if (ptype.compare ("b2"))
        {
          retval(0) = 514.0 * mm2units;
          retval(1) = 728.0 * mm2units;
        }
      else if (ptype.compare ("b3"))
        {
          retval(0) = 364.0 * mm2units;
          retval(1) = 514.0 * mm2units;
        }
      else if (ptype.compare ("b4"))
        {
          retval(0) = 257.0 * mm2units;
          retval(1) = 364.0 * mm2units;
        }
      else if (ptype.compare ("b5"))
        {
          retval(0) = 182.0 * mm2units;
          retval(1) = 257.0 * mm2units;
        }
      else if (ptype.compare ("arch-a"))
        {
          retval(0) = 9.0 * in2units;
          retval(1) = 12.0 * in2units;
        }
      else if (ptype.compare ("arch-b"))
        {
          retval(0) = 12.0 * in2units;
          retval(1) = 18.0 * in2units;
        }
      else if (ptype.compare ("arch-c"))
        {
          retval(0) = 18.0 * in2units;
          retval(1) = 24.0 * in2units;
        }
      else if (ptype.compare ("arch-d"))
        {
          retval(0) = 24.0 * in2units;
          retval(1) = 36.0 * in2units;
        }
      else if (ptype.compare ("arch-e"))
        {
          retval(0) = 36.0 * in2units;
          retval(1) = 48.0 * in2units;
        }
      else if (ptype.compare ("a"))
        {
          retval(0) = 8.5 * in2units;
          retval(1) = 11.0 * in2units;
        }
      else if (ptype.compare ("b"))
        {
          retval(0) = 11.0 * in2units;
          retval(1) = 17.0 * in2units;
        }
      else if (ptype.compare ("c"))
        {
          retval(0) = 17.0 * in2units;
          retval(1) = 22.0 * in2units;
        }
      else if (ptype.compare ("d"))
        {
          retval(0) = 22.0 * in2units;
          retval(1) = 34.0 * in2units;
        }
      else if (ptype.compare ("e"))
        {
          retval(0) = 34.0 * in2units;
          retval(1) = 43.0 * in2units;
        }
    }

  return retval;
}

Matrix
figure::properties::get_auto_paperposition (void)
{
  Matrix pos = get_position ().matrix_value ();
  Matrix sz;

  caseless_str funits = get_units ();
  caseless_str punits = get_paperunits ();

  // Convert position from figure units to paperunits
  if (funits == "normalized" || punits == "normalized")
    {
      sz = screen_size_pixels ();
      pos = convert_position (pos, funits, "inches", sz);

      if (punits == "normalized")
        sz = papersize_from_type ("points", get_papertype ());

      pos = convert_position (pos, "inches", punits, sz);
    }
  else
    pos = convert_position (pos, funits, punits, sz);

  // Center the figure on the page
  sz = get_papersize ().matrix_value ();

  pos(0) = sz(0)/2 - pos(2)/2;
  pos(1) = sz(1)/2 - pos(3)/2;

  return pos;
}

/*
%!test
%! hf = figure ("visible", "off", "paperpositionmode", "auto");
%! in_pos = [0 0 4 5];
%! tol = 20 * eps ();
%! unwind_protect
%!   ## paperpositionmode "auto" converts figure size to paper units
%!   set (hf, "units", "inches");
%!   set (hf, "position", in_pos);
%!   set (hf, "paperunits", "centimeters");
%!   psz = get (hf, "papersize");
%!   fsz = in_pos(3:4) * 2.54;
%!   pos = [(psz/2 - fsz/2) fsz];
%!   set (hf, "paperpositionmode", "auto");
%!   assert (get (hf, "paperposition"), pos, tol);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off", "paperpositionmode", "auto");
%! in_pos = [0 0 4 5];
%! tol = 20 * eps ();
%! unwind_protect
%!   ## likewise with normalized units
%!   set (hf, "units", "inches");
%!   set (hf, "position", in_pos);
%!   psz = get (hf, "papersize");
%!   set (hf, "paperunits", "normalized");
%!   fsz = in_pos(3:4) ./ psz;
%!   pos = [([0.5 0.5] - fsz/2) fsz];
%!   assert (get (hf, "paperposition"), pos, tol);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off", "paperpositionmode", "auto");
%! in_pos = [0 0 4 5];
%! tol = 20 * eps ();
%! unwind_protect
%!   ## changing papertype updates paperposition
%!   set (hf, "units", "inches");
%!   set (hf, "position", in_pos);
%!   set  (hf, "papertype", "a4");
%!   psz = get (hf, "papersize");
%!   fsz = in_pos(3:4);
%!   pos = [(psz/2 - fsz/2) fsz];
%!   assert (get (hf, "paperposition"), pos, tol);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off", "paperpositionmode", "auto");
%! in_pos = [0 0 4 5];
%! tol = 20 * eps ();
%! unwind_protect
%!   ## lanscape updates paperposition
%!   set (hf, "units", "inches");
%!   set (hf, "position", in_pos);
%!   set (hf, "paperorientation", "landscape");
%!   psz = get (hf, "papersize");
%!   fsz = in_pos(3:4);
%!   pos = [(psz/2 - fsz/2) fsz];
%!   assert (get (hf, "paperposition"), pos, tol);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off", "paperpositionmode", "auto");
%! in_pos = [0 0 4 5];
%! unwind_protect
%!   ## back to manual mode
%!   set (hf, "paperposition", in_pos * 1.1);
%!   assert (get (hf, "paperpositionmode"), "manual");
%!   assert (get (hf, "paperposition"), in_pos * 1.1);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
*/

void
figure::properties::update_paperunits (const caseless_str& old_paperunits)
{
  Matrix pos = get_paperposition ().matrix_value ();
  Matrix sz = get_papersize ().matrix_value ();

  pos(0) /= sz(0);
  pos(1) /= sz(1);
  pos(2) /= sz(0);
  pos(3) /= sz(1);

  std::string porient = get_paperorientation ();
  caseless_str punits = get_paperunits ();
  caseless_str ptype = get_papertype ();

  if (ptype.compare ("<custom>"))
    {
      if (old_paperunits.compare ("centimeters"))
        {
          sz(0) /= 2.54;
          sz(1) /= 2.54;
        }
      else if (old_paperunits.compare ("points"))
        {
          sz(0) /= 72.0;
          sz(1) /= 72.0;
        }

      if (punits.compare ("centimeters"))
        {
          sz(0) *= 2.54;
          sz(1) *= 2.54;
        }
      else if (punits.compare ("points"))
        {
          sz(0) *= 72.0;
          sz(1) *= 72.0;
        }
    }
  else
    {
      sz = papersize_from_type (punits, ptype);
      if (porient == "landscape")
        std::swap (sz(0), sz(1));
    }

  pos(0) *= sz(0);
  pos(1) *= sz(1);
  pos(2) *= sz(0);
  pos(3) *= sz(1);

  m_papersize.set (octave_value (sz));
  m_paperposition.set (octave_value (pos));
}

void
figure::properties::update_papertype (void)
{
  std::string typ = get_papertype ();
  if (typ != "<custom>")
    {
      Matrix sz = papersize_from_type (get_paperunits (), typ);
      if (get_paperorientation () == "landscape")
        std::swap (sz(0), sz(1));
      // Call papersize.set rather than set_papersize to avoid loops
      // between update_papersize and update_papertype.
      m_papersize.set (octave_value (sz));
    }

  if (m_paperpositionmode.is ("auto"))
    m_paperposition.set (get_auto_paperposition ());
}

void
figure::properties::update_papersize (void)
{
  Matrix sz = get_papersize ().matrix_value ();
  if (sz(0) > sz(1))
    {
      std::swap (sz(0), sz(1));
      m_papersize.set (octave_value (sz));
      m_paperorientation.set (octave_value ("landscape"));
    }
  else
    {
      m_paperorientation.set ("portrait");
    }

  std::string punits = get_paperunits ();
  if (punits == "centimeters")
    {
      sz(0) /= 2.54;
      sz(1) /= 2.54;
    }
  else if (punits == "points")
    {
      sz(0) /= 72.0;
      sz(1) /= 72.0;
    }
  if (punits == "normalized")
    {
      if (get_papertype () == "<custom>")
        error ("set: can't set the papertype to <custom> when the paperunits is normalized");
    }
  else
    {
      // FIXME: The papersizes info is also in papersize_from_type().
      //        Both should be rewritten to avoid the duplication.
      //        Don't Repeat Yourself (DRY) principle.
      std::string ptype = "<custom>";
      const double mm2in = 1.0 / 25.4;
      const double tol = 0.01;

      if (std::abs (sz(0) - 8.5) + std::abs (sz(1) - 11.0) < tol)
        ptype = "usletter";
      else if (std::abs (sz(0) - 8.5) + std::abs (sz(1) - 14.0) < tol)
        ptype = "uslegal";
      else if (std::abs (sz(0) - 11.0) + std::abs (sz(1) - 17.0) < tol)
        ptype = "tabloid";
      else if (std::abs (sz(0) - 841.0 * mm2in)
               + std::abs (sz(1) - 1198.0 * mm2in) < tol)
        ptype = "a0";
      else if (std::abs (sz(0) - 594.0 * mm2in)
               + std::abs (sz(1) - 841.0 * mm2in) < tol)
        ptype = "a1";
      else if (std::abs (sz(0) - 420.0 * mm2in)
               + std::abs (sz(1) - 594.0 * mm2in) < tol)
        ptype = "a2";
      else if (std::abs (sz(0) - 297.0 * mm2in)
               + std::abs (sz(1) - 420.0 * mm2in) < tol)
        ptype = "a3";
      else if (std::abs (sz(0) - 210.0 * mm2in)
               + std::abs (sz(1) - 297.0 * mm2in) < tol)
        ptype = "a4";
      else if (std::abs (sz(0) - 148.0 * mm2in)
               + std::abs (sz(1) - 210.0 * mm2in) < tol)
        ptype = "a5";
      else if (std::abs (sz(0) - 1029.0 * mm2in)
               + std::abs (sz(1) - 1456.0 * mm2in) < tol)
        ptype = "b0";
      else if (std::abs (sz(0) - 728.0 * mm2in)
               + std::abs (sz(1) - 1028.0 * mm2in) < tol)
        ptype = "b1";
      else if (std::abs (sz(0) - 514.0 * mm2in)
               + std::abs (sz(1) - 728.0 * mm2in) < tol)
        ptype = "b2";
      else if (std::abs (sz(0) - 364.0 * mm2in)
               + std::abs (sz(1) - 514.0 * mm2in) < tol)
        ptype = "b3";
      else if (std::abs (sz(0) - 257.0 * mm2in)
               + std::abs (sz(1) - 364.0 * mm2in) < tol)
        ptype = "b4";
      else if (std::abs (sz(0) - 182.0 * mm2in)
               + std::abs (sz(1) - 257.0 * mm2in) < tol)
        ptype = "b5";
      else if (std::abs (sz(0) - 9.0)
               + std::abs (sz(1) - 12.0) < tol)
        ptype = "arch-a";
      else if (std::abs (sz(0) - 12.0)
               + std::abs (sz(1) - 18.0) < tol)
        ptype = "arch-b";
      else if (std::abs (sz(0) - 18.0)
               + std::abs (sz(1) - 24.0) < tol)
        ptype = "arch-c";
      else if (std::abs (sz(0) - 24.0)
               + std::abs (sz(1) - 36.0) < tol)
        ptype = "arch-d";
      else if (std::abs (sz(0) - 36.0)
               + std::abs (sz(1) - 48.0) < tol)
        ptype = "arch-e";
      else if (std::abs (sz(0) - 8.5)
               + std::abs (sz(1) - 11.0) < tol)
        ptype = "a";
      else if (std::abs (sz(0) - 11.0)
               + std::abs (sz(1) - 17.0) < tol)
        ptype = "b";
      else if (std::abs (sz(0) - 17.0)
               + std::abs (sz(1) - 22.0) < tol)
        ptype = "c";
      else if (std::abs (sz(0) - 22.0)
               + std::abs (sz(1) - 34.0) < tol)
        ptype = "d";
      else if (std::abs (sz(0) - 34.0)
               + std::abs (sz(1) - 43.0) < tol)
        ptype = "e";
      // Call papertype.set rather than set_papertype to avoid loops between
      // update_papersize and update_papertype
      m_papertype.set (ptype);
    }
  if (punits == "centimeters")
    {
      sz(0) *= 2.54;
      sz(1) *= 2.54;
    }
  else if (punits == "points")
    {
      sz(0) *= 72.0;
      sz(1) *= 72.0;
    }
  if (get_paperorientation () == "landscape")
    {
      std::swap (sz(0), sz(1));
      m_papersize.set (octave_value (sz));
    }

  if (m_paperpositionmode.is ("auto"))
    m_paperposition.set (get_auto_paperposition ());
}

/*
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   set (hf, "paperunits", "inches");
%!   set (hf, "papersize", [5, 4]);
%!   set (hf, "paperunits", "points");
%!   assert (get (hf, "papersize"), [5, 4] * 72, 1);
%!   papersize = get (hf, "papersize");
%!   set (hf, "papersize", papersize + 1);
%!   set (hf, "papersize", papersize);
%!   assert (get (hf, "papersize"), [5, 4] * 72, 1);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   set (hf, "paperunits", "inches");
%!   set (hf, "papersize", [5, 4]);
%!   set (hf, "paperunits", "centimeters");
%!   assert (get (hf, "papersize"), [5, 4] * 2.54, 2.54/72);
%!   papersize = get (hf, "papersize");
%!   set (hf, "papersize", papersize + 1);
%!   set (hf, "papersize", papersize);
%!   assert (get (hf, "papersize"), [5, 4] * 2.54, 2.54/72);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
*/

void
figure::properties::update_paperorientation (void)
{
  std::string porient = get_paperorientation ();
  Matrix sz = get_papersize ().matrix_value ();
  if ((sz(0) > sz(1) && porient == "portrait")
      || (sz(0) < sz(1) && porient == "landscape"))
    {
      std::swap (sz(0), sz(1));
      // Call papertype.set rather than set_papertype to avoid loops
      // between update_papersize and update_papertype
      m_papersize.set (octave_value (sz));
    }

  if (m_paperpositionmode.is ("auto"))
    m_paperposition.set (get_auto_paperposition ());
}

/*
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   tol = 100 * eps ();
%!   ## UPPER case and MiXed case is part of test and should not be changed.
%!   set (hf, "paperorientation", "PORTRAIT");
%!   set (hf, "paperunits", "inches");
%!   set (hf, "papertype", "USletter");
%!   assert (get (hf, "papersize"), [8.5, 11.0], tol);
%!   set (hf, "paperorientation", "Landscape");
%!   assert (get (hf, "papersize"), [11.0, 8.5], tol);
%!   set (hf, "paperunits", "centimeters");
%!   assert (get (hf, "papersize"), [11.0, 8.5] * 2.54, tol);
%!   set (hf, "papertype", "a4");
%!   assert (get (hf, "papersize"), [29.7, 21.0], tol);
%!   set (hf, "paperunits", "inches", "papersize", [8.5, 11.0]);
%!   assert (get (hf, "papertype"), "usletter");
%!   assert (get (hf, "paperorientation"), "portrait");
%!   set (hf, "papersize", [11.0, 8.5]);
%!   assert (get (hf, "papertype"), "usletter");
%!   assert (get (hf, "paperorientation"), "landscape");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
*/

void
figure::properties::set_units (const octave_value& val)
{
  caseless_str old_units = get_units ();

  if (m_units.set (val, true))
    {
      update_units (old_units);
      mark_modified ();
    }
}

void
figure::properties::update_units (const caseless_str& old_units)
{
  m_position.set (convert_position (get_position ().matrix_value (),
                                    old_units, get_units (),
                                    screen_size_pixels ()), false);
}

/*
%!test
%! hf = figure ("visible", "off");
%! old_units = get (0, "units");
%! unwind_protect
%!   set (0, "units", "pixels");
%!   rsz = get (0, "screensize");
%!   set (gcf (), "units", "pixels");
%!   fsz = get (gcf (), "position");
%!   set (gcf (), "units", "normalized");
%!   pos = get (gcf (), "position");
%!   assert (pos, (fsz - [1, 1, 0, 0]) ./ rsz([3, 4, 3, 4]));
%! unwind_protect_cleanup
%!   close (hf);
%!   set (0, "units", old_units);
%! end_unwind_protect
*/

std::string
figure::properties::get_title (void) const
{
  std::string title;
  if (! get_number ().isempty () && is_numbertitle ())
    {
      std::ostringstream os;
      std::string nm = get_name ();

      os << "Figure " << m___myhandle__.value ();
      if (! nm.empty ())
        os << ": " << get_name ();

      title = os.str ();
    }
  else
    title = get_name ();

  // Qt will use QCoreApplication name (set in main-window.cc)
  // if the name is empty, so force blank.
  if (title.empty ())
    title = " ";

  return title;
}

octave_value
figure::get_default (const caseless_str& name) const
{
  octave_value retval = m_default_properties.lookup (name);

  if (retval.is_undefined ())
    {
      graphics_handle parent_h = get_parent ();

      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      graphics_object parent_go = gh_mgr.get_object (parent_h);

      retval = parent_go.get_default (name);
    }

  return retval;
}

void
figure::reset_default_properties (void)
{
  // empty list of local defaults
  m_default_properties = property_list ();
  property_list::pval_map_type plist = m_properties.factory_defaults ();

  plist.erase ("units");
  plist.erase ("position");
  plist.erase ("outerposition");
  plist.erase ("paperunits");
  plist.erase ("paperposition");
  plist.erase ("windowstyle");

  remove_all_listeners ();
  xreset_default_properties (get_handle (), plist);
}

// ---------------------------------------------------------------------

void
axes::properties::init (void)
{
  m_position.add_constraint (dim_vector (1, 4));
  m_outerposition.add_constraint (dim_vector (1, 4));
  m_tightinset.add_constraint (dim_vector (1, 4));
  m_looseinset.add_constraint (dim_vector (1, 4));
  m_colororder.add_constraint (dim_vector (-1, 3));
  m_dataaspectratio.add_constraint (3);
  m_dataaspectratio.add_constraint ("min", 0, false);
  m_dataaspectratio.add_constraint (FINITE);
  m_plotboxaspectratio.add_constraint (3);
  m_plotboxaspectratio.add_constraint ("min", 0, false);
  m_plotboxaspectratio.add_constraint (FINITE);
  // FIXME: Should these use dimension vectors?  Currently can set 'xlim' to
  // any matrix size, but only first two elements are used.
  m_alim.add_constraint (2);
  m_alim.add_constraint (NOT_NAN);
  m_clim.add_constraint (2);
  m_clim.add_constraint (NOT_NAN);
  m_xlim.add_constraint (2);
  m_xlim.add_constraint (NOT_NAN);
  m_ylim.add_constraint (2);
  m_ylim.add_constraint (NOT_NAN);
  m_zlim.add_constraint (2);
  m_zlim.add_constraint (NOT_NAN);
  m_xtick.add_constraint (dim_vector (1, -1));
  m_xtick.add_constraint (FINITE);
  m_ytick.add_constraint (dim_vector (1, -1));
  m_ytick.add_constraint (FINITE);
  m_ztick.add_constraint (dim_vector (1, -1));
  m_ztick.add_constraint (FINITE);
  m_ticklength.add_constraint (dim_vector (1, 2));
  Matrix vw (1, 2, 0);
  vw(1) = 90;
  m_view = vw;
  m_view.add_constraint (dim_vector (1, 2));
  m_cameraposition.add_constraint (3);
  m_cameraposition.add_constraint (FINITE);
  m_cameratarget.add_constraint (3);
  m_cameratarget.add_constraint (FINITE);
  Matrix upv (1, 3, 0.0);
  upv(2) = 1.0;
  m_cameraupvector = upv;
  m_cameraupvector.add_constraint (3);
  m_cameraupvector.add_constraint (FINITE);
  m_cameraviewangle.add_constraint (FINITE);
  m_currentpoint.add_constraint (dim_vector (2, 3));

  // Range constraints for double properties
  m_fontsize.add_constraint ("min", 0.0, false);
  m_gridalpha.add_constraint ("min", 0.0, true);
  m_gridalpha.add_constraint ("max", 1.0, true);
  m_labelfontsizemultiplier.add_constraint ("min", 0.0, false);
  m_linewidth.add_constraint ("min", 0.0, false);
  m_minorgridalpha.add_constraint ("min", 0.0, true);
  m_minorgridalpha.add_constraint ("max", 1.0, true);
  m_titlefontsizemultiplier.add_constraint ("min", 0.0, false);

  // No constraints for hidden transform properties
  update_font ();

  m_x_zlim.resize (1, 2);

  m_sx = "linear";
  m_sy = "linear";
  m_sz = "linear";

  calc_ticklabels (m_xtick, m_xticklabel, m_xscale.is ("log"),
                   xaxislocation_is ("origin"),
                   m_yscale.is ("log") ? 2 : (yaxislocation_is ("origin") ? 0 :
                       (yaxislocation_is ("left") ? -1 : 1)), m_xlim);
  calc_ticklabels (m_ytick, m_yticklabel, m_yscale.is ("log"),
                   yaxislocation_is ("origin"),
                   m_xscale.is ("log") ? 2 : (xaxislocation_is ("origin") ? 0 :
                       (xaxislocation_is ("bottom") ? -1 : 1)), m_ylim);
  calc_ticklabels (m_ztick, m_zticklabel, m_zscale.is ("log"),
                   false, 2, m_zlim);

  xset (m_xlabel.handle_value (), "handlevisibility", "off");
  xset (m_ylabel.handle_value (), "handlevisibility", "off");
  xset (m_zlabel.handle_value (), "handlevisibility", "off");
  xset (m_title.handle_value (), "handlevisibility", "off");

  xset (m_xlabel.handle_value (), "horizontalalignment", "center");
  xset (m_xlabel.handle_value (), "horizontalalignmentmode", "auto");
  xset (m_ylabel.handle_value (), "horizontalalignment", "center");
  xset (m_ylabel.handle_value (), "horizontalalignmentmode", "auto");
  xset (m_zlabel.handle_value (), "horizontalalignment", "right");
  xset (m_zlabel.handle_value (), "horizontalalignmentmode", "auto");
  xset (m_title.handle_value (), "horizontalalignment", "center");
  xset (m_title.handle_value (), "horizontalalignmentmode", "auto");

  xset (m_xlabel.handle_value (), "verticalalignment", "top");
  xset (m_xlabel.handle_value (), "verticalalignmentmode", "auto");
  xset (m_ylabel.handle_value (), "verticalalignment", "bottom");
  xset (m_ylabel.handle_value (), "verticalalignmentmode", "auto");
  xset (m_title.handle_value (), "verticalalignment", "bottom");
  xset (m_title.handle_value (), "verticalalignmentmode", "auto");

  xset (m_ylabel.handle_value (), "rotation", 90.0);
  xset (m_ylabel.handle_value (), "rotationmode", "auto");

  xset (m_zlabel.handle_value (), "visible", "off");

  xset (m_xlabel.handle_value (), "clipping", "off");
  xset (m_ylabel.handle_value (), "clipping", "off");
  xset (m_zlabel.handle_value (), "clipping", "off");
  xset (m_title.handle_value (), "clipping", "off");

  xset (m_xlabel.handle_value (), "__autopos_tag__", "xlabel");
  xset (m_ylabel.handle_value (), "__autopos_tag__", "ylabel");
  xset (m_zlabel.handle_value (), "__autopos_tag__", "zlabel");
  xset (m_title.handle_value (), "__autopos_tag__", "title");

  double fs = m_labelfontsizemultiplier.double_value () *
              m_fontsize.double_value ();
  xset (m_xlabel.handle_value (), "fontsize", octave_value (fs));
  xset (m_ylabel.handle_value (), "fontsize", octave_value (fs));
  xset (m_zlabel.handle_value (), "fontsize", octave_value (fs));
  fs = m_titlefontsizemultiplier.double_value () * m_fontsize.double_value ();
  xset (m_title.handle_value (), "fontsize", octave_value (fs));
  xset (m_title.handle_value (), "fontweight", m_titlefontweight.get ());

  adopt (m_xlabel.handle_value ());
  adopt (m_ylabel.handle_value ());
  adopt (m_zlabel.handle_value ());
  adopt (m_title.handle_value ());

  Matrix tlooseinset = default_axes_position ();
  tlooseinset(2) = 1-tlooseinset(0)-tlooseinset(2);
  tlooseinset(3) = 1-tlooseinset(1)-tlooseinset(3);
  m_looseinset = tlooseinset;
}

/*
## Test validation of axes double properties range
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   try
%!     set (hax, "linewidth", -1);
%!   catch
%!     err = lasterr ();
%!   end_try_catch
%!   assert (err, 'set: "linewidth" must be greater than 0');
%!   try
%!     set (hax, "minorgridalpha", 1.5);
%!   catch
%!     err = lasterr ();
%!   end_try_catch
%!   assert (err, 'set: "minorgridalpha" must be less than or equal to 1');
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect
*/

Matrix
axes::properties::calc_tightbox (const Matrix& init_pos)
{
  Matrix pos = init_pos;

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (get_parent ());

  Matrix parent_bb = go.get_properties ().get_boundingbox (true);

  // FIXME: The layout should be clean at this stage and we should not have to
  //        update ticks and labels positions here again.  See bug #48718.
  update_ticklength ();

  Matrix ext = get_extent (true, true);
  ext(1) = parent_bb(3) - ext(1) - ext(3);
  ext(0)++;
  ext(1)++;
  ext = convert_position (ext, "pixels", get_units (),
                          parent_bb.extract_n (0, 2, 1, 2));
  if (ext(0) < pos(0))
    {
      pos(2) += pos(0)-ext(0);
      pos(0) = ext(0);
    }
  if (ext(0)+ext(2) > pos(0)+pos(2))
    pos(2) = ext(0)+ext(2)-pos(0);

  if (ext(1) < pos(1))
    {
      pos(3) += pos(1)-ext(1);
      pos(1) = ext(1);
    }
  if (ext(1)+ext(3) > pos(1)+pos(3))
    pos(3) = ext(1)+ext(3)-pos(1);

  return pos;
}

void
axes::properties::sync_positions (void)
{
  // First part is equivalent to 'update_tightinset ()'
  if (m_positionconstraint.is ("innerposition"))
    update_position ();
  else
    update_outerposition ();
  caseless_str old_units = get_units ();
  set_units ("normalized");
  Matrix pos = m_position.get ().matrix_value ();
  Matrix outpos = m_outerposition.get ().matrix_value ();
  Matrix tightpos = calc_tightbox (pos);
  Matrix tinset (1, 4, 1.0);
  tinset(0) = pos(0)-tightpos(0);
  tinset(1) = pos(1)-tightpos(1);
  tinset(2) = tightpos(0)+tightpos(2)-pos(0)-pos(2);
  tinset(3) = tightpos(1)+tightpos(3)-pos(1)-pos(3);
  m_tightinset = tinset;
  set_units (old_units);
  update_transform ();
  if (m_positionconstraint.is ("innerposition"))
    update_position ();
  else
    update_outerposition ();
}

/*
%!testif HAVE_OPENGL, HAVE_QT; have_window_system () && any (strcmp ("qt", available_graphics_toolkits ()))
%! hf = figure ("visible", "off");
%! graphics_toolkit (hf, "qt");
%! unwind_protect
%!   subplot (2,1,1); plot (rand (10,1)); subplot (2,1,2); plot (rand (10,1));
%!   hax = findall (gcf (), "type", "axes");
%!   positions = cell2mat (get (hax, "position"));
%!   outerpositions = cell2mat (get (hax, "outerposition"));
%!   looseinsets = cell2mat (get (hax, "looseinset"));
%!   tightinsets = cell2mat (get (hax, "tightinset"));
%!   subplot (2,1,1); plot (rand (10,1)); subplot (2,1,2); plot (rand (10,1));
%!   hax = findall (gcf (), "type", "axes");
%!   assert (cell2mat (get (hax, "position")), positions, 1e-4);
%!   assert (cell2mat (get (hax, "outerposition")), outerpositions, 1e-4);
%!   assert (cell2mat (get (hax, "looseinset")), looseinsets, 1e-4);
%!   assert (cell2mat (get (hax, "tightinset")), tightinsets, 1e-4);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!testif HAVE_OPENGL, HAVE_QT; have_window_system () && any (strcmp ("qt", available_graphics_toolkits ()))
%! hf = figure ("visible", "off");
%! graphics_toolkit (hf, "qt");
%! fpos = get (hf, "position");
%! unwind_protect
%!   plot (rand (3));
%!   position = get (gca, "position");
%!   outerposition = get (gca, "outerposition");
%!   looseinset = get (gca, "looseinset");
%!   tightinset = get (gca, "tightinset");
%!   set (hf, "position", [fpos(1:2), 2*fpos(3:4)]);
%!   set (hf, "position", fpos);
%!   assert (get (gca, "outerposition"), outerposition, 0.001);
%!   assert (get (gca, "position"), position, 0.001);
%!   assert (get (gca, "looseinset"), looseinset, 0.001);
%!   assert (get (gca, "tightinset"), tightinset, 0.001);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!testif HAVE_OPENGL, HAVE_QT; have_window_system () && any (strcmp ("qt", available_graphics_toolkits ()))
%! hf = figure ("visible", "off");
%! graphics_toolkit (hf, "qt");
%! fpos = get (hf, "position");
%! set (gca, "positionconstraint", "innerposition");
%! unwind_protect
%!   plot (rand (3));
%!   position = get (gca, "position");
%!   outerposition = get (gca, "outerposition");
%!   looseinset = get (gca, "looseinset");
%!   tightinset = get (gca, "tightinset");
%!   set (hf, "position", [fpos(1:2), 2*fpos(3:4)]);
%!   set (hf, "position", fpos);
%!   assert (get (gca, "position"), position, 0.001);
%!   assert (get (gca, "outerposition"), outerposition, 0.001);
%!   assert (get (gca, "looseinset"), looseinset, 0.001);
%!   assert (get (gca, "tightinset"), tightinset, 0.001);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
*/

void
axes::properties::set_text_child (handle_property& hp,
                                  const std::string& who,
                                  const octave_value& v)
{
  if (v.is_string ())
    {
      xset (hp.handle_value (), "string", v);
      return;
    }

  graphics_handle val;

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (gh_mgr.lookup (v));

  if (go.isa ("text"))
    val = octave::reparent (v, "set", who, m___myhandle__, false);
  else
    {
      std::string cname = v.class_name ();

      error ("set: expecting text graphics object or character string for %s property, found %s",
             who.c_str (), cname.c_str ());
    }

  xset (val, "handlevisibility", "off");

  gh_mgr.free (hp.handle_value ());

  hp = val;

  adopt (hp.handle_value ());
}

void
axes::properties::set_xlabel (const octave_value& v)
{
  set_text_child (m_xlabel, "xlabel", v);
  xset (m_xlabel.handle_value (), "positionmode", "auto");
  xset (m_xlabel.handle_value (), "rotationmode", "auto");
  xset (m_xlabel.handle_value (), "horizontalalignmentmode", "auto");
  xset (m_xlabel.handle_value (), "verticalalignmentmode", "auto");
  xset (m_xlabel.handle_value (), "clipping", "off");
  xset (m_xlabel.handle_value (), "color", get_xcolor ());
  xset (m_xlabel.handle_value (), "__autopos_tag__", "xlabel");
  update_xlabel_position ();
}

void
axes::properties::set_ylabel (const octave_value& v)
{
  set_text_child (m_ylabel, "ylabel", v);
  xset (m_ylabel.handle_value (), "positionmode", "auto");
  xset (m_ylabel.handle_value (), "rotationmode", "auto");
  xset (m_ylabel.handle_value (), "horizontalalignmentmode", "auto");
  xset (m_ylabel.handle_value (), "verticalalignmentmode", "auto");
  xset (m_ylabel.handle_value (), "clipping", "off");
  xset (m_ylabel.handle_value (), "color", get_ycolor ());
  xset (m_ylabel.handle_value (), "__autopos_tag__", "ylabel");
  update_ylabel_position ();
}

void
axes::properties::set_zlabel (const octave_value& v)
{
  set_text_child (m_zlabel, "zlabel", v);
  xset (m_zlabel.handle_value (), "positionmode", "auto");
  xset (m_zlabel.handle_value (), "rotationmode", "auto");
  xset (m_zlabel.handle_value (), "horizontalalignmentmode", "auto");
  xset (m_zlabel.handle_value (), "verticalalignmentmode", "auto");
  xset (m_zlabel.handle_value (), "clipping", "off");
  xset (m_zlabel.handle_value (), "color", get_zcolor ());
  xset (m_zlabel.handle_value (), "__autopos_tag__", "zlabel");
  update_zlabel_position ();
}

void
axes::properties::set_title (const octave_value& v)
{
  set_text_child (m_title, "title", v);
  xset (m_title.handle_value (), "positionmode", "auto");
  xset (m_title.handle_value (), "horizontalalignment", "center");
  xset (m_title.handle_value (), "horizontalalignmentmode", "auto");
  xset (m_title.handle_value (), "verticalalignment", "bottom");
  xset (m_title.handle_value (), "verticalalignmentmode", "auto");
  xset (m_title.handle_value (), "clipping", "off");
  xset (m_title.handle_value (), "__autopos_tag__", "title");
  update_title_position ();
}

void
axes::properties::set_defaults (base_graphics_object& bgo,
                                const std::string& mode)
{
  // FIXME: Should this have all properties in it?
  // Including ones we do don't implement?

  // FIXME: This function is probably never called without mode == "reset".
  //        Verify this is the case with error_unless() (1/6/2017).
  //        If there are reports of problems then figure out what code is
  //        calling it with the mode set to something else.
  error_unless (mode == "reset");

  Matrix tlim (1, 2, 0.0);
  tlim(1) = 1;
  m_alim = tlim;
  m_xlim = tlim;
  m_ylim = tlim;
  m_zlim = tlim;

  m_alimmode = "auto";
  m_climmode = "auto";
  m_xlimmode = "auto";
  m_ylimmode = "auto";
  m_zlimmode = "auto";

  m_ambientlightcolor = Matrix (1, 3, 1.0);

  m_box = "off";
  m_boxstyle = "back";

  // Note: camera properties (not mode) will be set in update_transform
  m_camerapositionmode = "auto";
  m_cameratargetmode = "auto";
  m_cameraupvectormode = "auto";
  m_cameraviewanglemode = "auto";

  Matrix cl (1, 2, 0.0);
  cl(1) = 1;
  m_clim = cl;

  m_clippingstyle = "3dbox";

  m_color = color_values ("white");
  m_colororder = default_colororder ();
  m_colororderindex = 1.0;

  // Note: dataspectratio (not mode) will be set through update_aspectratios
  m_dataaspectratiomode = "auto";

  m_fontangle = "normal";
  m_fontname = OCTAVE_DEFAULT_FONTNAME;
  m_fontsize = 10;
  m_fontunits = "points";
  m_fontsmoothing = "on";
  m_fontweight = "normal";

  m_gridalpha = 0.15;
  m_gridalphamode = "auto";
  m_gridcolor = color_values (0.15, 0.15, 0.15);
  m_gridcolormode = "auto";
  m_gridlinestyle = "-";

  m_labelfontsizemultiplier = 1.1;

  m_layer = "bottom";

  m_linestyleorder = "-";
  m_linestyleorderindex = 1.0;

  m_linewidth = 0.5;

  m_minorgridalpha = 0.25;
  m_minorgridalphamode = "auto";
  m_minorgridcolor = color_values (0.1, 0.1, 0.1);
  m_minorgridcolormode = "auto";
  m_minorgridlinestyle = ":";

  m_nextplot = "replace";

  // Note: plotboxaspectratio will be set through update_aspectratios
  m_plotboxaspectratiomode = "auto";
  m_projection = "orthographic";

  m_sortmethod = "depth";

  m_tickdir = "in";
  m_tickdirmode = "auto";
  m_ticklabelinterpreter = "tex";
  m_ticklength = default_axes_ticklength ();

  m_tightinset = Matrix (1, 4, 0.0);

  m_titlefontsizemultiplier = 1.1;
  m_titlefontweight = "bold";

  Matrix tview (1, 2, 0.0);
  tview(1) = 90;
  m_view = tview;

  m_xaxislocation = "bottom";

  m_xcolor = color_values (0.15, 0.15, 0.15);
  m_xcolormode = "auto";
  m_xdir = "normal";
  m_xgrid = "off";
  m_xminorgrid = "off";
  m_xminortick = "off";
  m_xscale = "linear";
  m_xtick = Matrix ();
  m_xticklabel = "";
  m_xticklabelmode = "auto";
  m_xticklabelrotation = 0.0;
  m_xtickmode = "auto";

  m_yaxislocation = "left";

  m_ycolor = color_values (0.15, 0.15, 0.15);
  m_ycolormode = "auto";
  m_ydir = "normal";
  m_ygrid = "off";
  m_yminorgrid = "off";
  m_yminortick = "off";
  m_yscale = "linear";
  m_ytick = Matrix ();
  m_yticklabel = "";
  m_yticklabelmode = "auto";
  m_yticklabelrotation = 0.0;
  m_ytickmode = "auto";

  m_zcolor = color_values (0.15, 0.15, 0.15);
  m_zcolormode = "auto";
  m_zdir = "normal";
  m_zgrid = "off";
  m_zminorgrid = "off";
  m_zminortick = "off";
  m_zscale = "linear";
  m_ztick = Matrix ();
  m_zticklabel = "";
  m_zticklabelmode = "auto";
  m_zticklabelrotation = 0.0;
  m_ztickmode = "auto";

  m_sx = "linear";
  m_sy = "linear";
  m_sz = "linear";

  m_visible = "on";

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (m_xlabel.handle_value ());
  go.reset_default_properties ();
  go = gh_mgr.get_object (m_ylabel.handle_value ());
  go.reset_default_properties ();
  go = gh_mgr.get_object (m_zlabel.handle_value ());
  go.reset_default_properties ();
  go = gh_mgr.get_object (m_title.handle_value ());
  go.reset_default_properties ();

  xset (m_xlabel.handle_value (), "handlevisibility", "off");
  xset (m_ylabel.handle_value (), "handlevisibility", "off");
  xset (m_zlabel.handle_value (), "handlevisibility", "off");
  xset (m_title.handle_value (), "handlevisibility", "off");

  xset (m_xlabel.handle_value (), "horizontalalignment", "center");
  xset (m_xlabel.handle_value (), "horizontalalignmentmode", "auto");
  xset (m_ylabel.handle_value (), "horizontalalignment", "center");
  xset (m_ylabel.handle_value (), "horizontalalignmentmode", "auto");
  xset (m_zlabel.handle_value (), "horizontalalignment", "right");
  xset (m_zlabel.handle_value (), "horizontalalignmentmode", "auto");
  xset (m_title.handle_value (), "horizontalalignment", "center");
  xset (m_title.handle_value (), "horizontalalignmentmode", "auto");

  xset (m_xlabel.handle_value (), "verticalalignment", "top");
  xset (m_xlabel.handle_value (), "verticalalignmentmode", "auto");
  xset (m_ylabel.handle_value (), "verticalalignment", "bottom");
  xset (m_ylabel.handle_value (), "verticalalignmentmode", "auto");
  xset (m_title.handle_value (), "verticalalignment", "bottom");
  xset (m_title.handle_value (), "verticalalignmentmode", "auto");

  xset (m_ylabel.handle_value (), "rotation", 90.0);
  xset (m_ylabel.handle_value (), "rotationmode", "auto");

  xset (m_zlabel.handle_value (), "visible", "off");

  xset (m_xlabel.handle_value (), "clipping", "off");
  xset (m_ylabel.handle_value (), "clipping", "off");
  xset (m_zlabel.handle_value (), "clipping", "off");
  xset (m_title.handle_value (), "clipping", "off");

  xset (m_xlabel.handle_value (), "__autopos_tag__", "xlabel");
  xset (m_ylabel.handle_value (), "__autopos_tag__", "ylabel");
  xset (m_zlabel.handle_value (), "__autopos_tag__", "zlabel");
  xset (m_title.handle_value (), "__autopos_tag__", "title");

  double fs;
  fs = m_labelfontsizemultiplier.double_value () * m_fontsize.double_value ();
  xset (m_xlabel.handle_value (), "fontsize", octave_value (fs));
  xset (m_ylabel.handle_value (), "fontsize", octave_value (fs));
  xset (m_zlabel.handle_value (), "fontsize", octave_value (fs));
  fs = m_titlefontsizemultiplier.double_value () * m_fontsize.double_value ();
  xset (m_title.handle_value (), "fontsize", octave_value (fs));
  xset (m_title.handle_value (), "fontweight", m_titlefontweight.get ());

  update_transform ();
  sync_positions ();
  override_defaults (bgo);
}

octave_value
axes::properties::get_colormap (void) const
{
  if (m___colormap__.get ().isempty ())
    {
      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      graphics_object go (gh_mgr.get_object (get___myhandle__ ()));
      graphics_object go_f (go.get_ancestor ("figure"));
      figure::properties& figure_props
        = reinterpret_cast<figure::properties&> (go_f.get_properties ());
      return figure_props.get_colormap ();
    }

  return get___colormap__ ();
}

void
axes::properties::delete_text_child (handle_property& hp, bool from_root)
{
  graphics_handle h = hp.handle_value ();

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  if (h.ok ())
    {
      graphics_object go = gh_mgr.get_object (h);

      if (go.valid_object ())
        gh_mgr.free (h, from_root);
    }

  // FIXME: is it necessary to check whether the axes object is
  // being deleted now?  I think this function is only called when an
  // individual child object is delete and not when the parent axes
  // object is deleted.

  if (! is_beingdeleted ())
    {
      hp = gh_mgr.make_graphics_handle ("text", m___myhandle__, false, false);

      xset (hp.handle_value (), "handlevisibility", "off");

      adopt (hp.handle_value ());
    }
}

void
axes::properties::remove_child (const graphics_handle& h, bool from_root)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (h);

  if (m_xlabel.handle_value ().ok () && h == m_xlabel.handle_value ())
    {
      delete_text_child (m_xlabel, from_root);
      update_xlabel_position ();
    }
  else if (m_ylabel.handle_value ().ok () && h == m_ylabel.handle_value ())
    {
      delete_text_child (m_ylabel, from_root);
      update_ylabel_position ();
    }
  else if (m_zlabel.handle_value ().ok () && h == m_zlabel.handle_value ())
    {
      delete_text_child (m_zlabel, from_root);
      update_zlabel_position ();
    }
  else if (m_title.handle_value ().ok () && h == m_title.handle_value ())
    {
      delete_text_child (m_title, from_root);
      update_title_position ();
    }
  else if (get_num_lights () > 0 && go.isa ("light")
           && go.get_properties ().is_visible ())
    decrease_num_lights ();

  if (go.valid_object ())
    base_properties::remove_child (h, from_root);

}

void
axes::properties::adopt (const graphics_handle& h)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go (gh_mgr.get_object (h));

  if (go.isa ("light") && go.get_properties ().is_visible ())
    increase_num_lights ();

  base_properties::adopt (h);

  // FIXME: For performance reasons, we would like to call
  //          update_axis_limits ("xlim", h);
  //        which updates the limits based ONLY on the new data from h.
  //        But this isn't working properly at the moment, so we
  //        call the other form which invokes a full tree traversal of all
  //        of the axes children.
  if (xlimmode_is ("auto"))
    update_axis_limits ("xlim");

  if (ylimmode_is ("auto"))
    update_axis_limits ("ylim");

  if (zlimmode_is ("auto"))
    update_axis_limits ("zlim");

  if (climmode_is ("auto"))
    update_axis_limits ("clim");

  if (climmode_is ("auto"))
    update_axis_limits ("alim");
}

inline Matrix
xform_matrix (void)
{
  Matrix m (4, 4, 0.0);

  for (int i = 0; i < 4; i++)
    m(i, i) = 1;

  return m;
}

inline ColumnVector
xform_vector (void)
{
  ColumnVector v (4, 0.0);

  v(3) = 1;

  return v;
}

inline ColumnVector
xform_vector (double x, double y, double z)
{
  ColumnVector v (4, 1.0);

  v(0) = x;
  v(1) = y;
  v(2) = z;

  return v;
}

inline ColumnVector
transform (const Matrix& m, double x, double y, double z)
{
  return (m * xform_vector (x, y, z));
}

inline Matrix
xform_scale (double x, double y, double z)
{
  Matrix m (4, 4, 0.0);

  m(0, 0) = x;
  m(1, 1) = y;
  m(2, 2) = z;
  m(3, 3) = 1;

  return m;
}

inline Matrix
xform_translate (double x, double y, double z)
{
  Matrix m = xform_matrix ();

  m(0, 3) = x;
  m(1, 3) = y;
  m(2, 3) = z;
  m(3, 3) = 1;

  return m;
}

inline void
scale (Matrix& m, double x, double y, double z)
{
  m = m * xform_scale (x, y, z);
}

inline void
translate (Matrix& m, double x, double y, double z)
{
  m = m * xform_translate (x, y, z);
}

inline void
xform (ColumnVector& v, const Matrix& m)
{
  v = m * v;
}

inline void
scale (ColumnVector& v, double x, double y, double z)
{
  v(0) *= x;
  v(1) *= y;
  v(2) *= z;
}

inline void
translate (ColumnVector& v, double x, double y, double z)
{
  v(0) += x;
  v(1) += y;
  v(2) += z;
}

inline void
normalize (ColumnVector& v)
{
  double fact = 1.0 / sqrt (v(0)*v(0)+v(1)*v(1)+v(2)*v(2));
  scale (v, fact, fact, fact);
}

inline double
dot (const ColumnVector& v1, const ColumnVector& v2)
{
  return (v1(0)*v2(0)+v1(1)*v2(1)+v1(2)*v2(2));
}

inline double
norm (const ColumnVector& v)
{
  return sqrt (dot (v, v));
}

inline ColumnVector
cross (const ColumnVector& v1, const ColumnVector& v2)
{
  ColumnVector r = xform_vector ();

  r(0) = v1(1)*v2(2) - v1(2)*v2(1);
  r(1) = v1(2)*v2(0) - v1(0)*v2(2);
  r(2) = v1(0)*v2(1) - v1(1)*v2(0);

  return r;
}

inline Matrix
unit_cube (void)
{
  static double data[32] =
  {
    0, 0, 0, 1,
    1, 0, 0, 1,
    0, 1, 0, 1,
    0, 0, 1, 1,
    1, 1, 0, 1,
    1, 0, 1, 1,
    0, 1, 1, 1,
    1, 1, 1, 1
  };
  Matrix m (4, 8);

  memcpy (m.fortran_vec (), data, sizeof (double)*32);

  return m;
}

inline ColumnVector
cam2xform (const Array<double>& m)
{
  ColumnVector retval (4, 1.0);

  memcpy (retval.fortran_vec (), m.data (), sizeof (double)*3);

  return retval;
}

inline RowVector
xform2cam (const ColumnVector& v)
{
  return v.extract_n (0, 3).transpose ();
}

void
axes::properties::update_camera (void)
{
  double xd = (xdir_is ("normal") ? 1 : -1);
  double yd = (ydir_is ("normal") ? 1 : -1);
  double zd = (zdir_is ("normal") ? 1 : -1);

  Matrix xlimits = m_sx.scale (get_xlim ().matrix_value ());
  Matrix ylimits = m_sy.scale (get_ylim ().matrix_value ());
  Matrix zlimits = m_sz.scale (get_zlim ().matrix_value ());

  double xo = xlimits(xd > 0 ? 0 : 1);
  double yo = ylimits(yd > 0 ? 0 : 1);
  double zo = zlimits(zd > 0 ? 0 : 1);

  Matrix pb = get_plotboxaspectratio ().matrix_value ();

  bool autocam = (camerapositionmode_is ("auto")
                  && cameratargetmode_is ("auto")
                  && cameraupvectormode_is ("auto")
                  && cameraviewanglemode_is ("auto"));
  bool dowarp = (autocam && dataaspectratiomode_is ("auto")
                 && plotboxaspectratiomode_is ("auto"));

  ColumnVector c_eye (xform_vector ());
  ColumnVector c_center (xform_vector ());
  ColumnVector c_upv (xform_vector ());

  if (cameratargetmode_is ("auto"))
    {
      c_center(0) = (xlimits(0) + xlimits(1)) / 2;
      c_center(1) = (ylimits(0) + ylimits(1)) / 2;
      c_center(2) = (zlimits(0) + zlimits(1)) / 2;

      m_cameratarget = xform2cam (c_center);
    }
  else
    c_center = cam2xform (get_cameratarget ().matrix_value ());

  if (camerapositionmode_is ("auto"))
    {
      Matrix tview = get_view ().matrix_value ();
      double az = tview(0);
      double el = tview(1);
      double d = 5 * sqrt (pb(0)*pb(0) + pb(1)*pb(1) + pb(2)*pb(2));

      if (el == 90 || el == -90)
        c_eye(2) = d*octave::math::signum (el);
      else
        {
          az *= M_PI/180.0;
          el *= M_PI/180.0;
          c_eye(0) = d * cos (el) * sin (az);
          c_eye(1) = -d* cos (el) * cos (az);
          c_eye(2) = d * sin (el);
        }
      c_eye(0) = c_eye(0)*(xlimits(1)-xlimits(0))/(xd*pb(0))+c_center(0);
      c_eye(1) = c_eye(1)*(ylimits(1)-ylimits(0))/(yd*pb(1))+c_center(1);
      c_eye(2) = c_eye(2)*(zlimits(1)-zlimits(0))/(zd*pb(2))+c_center(2);

      m_cameraposition = xform2cam (c_eye);
    }
  else
    c_eye = cam2xform (get_cameraposition ().matrix_value ());

  if (cameraupvectormode_is ("auto"))
    {
      Matrix tview = get_view ().matrix_value ();
      double az = tview(0);
      double el = tview(1);

      if (el == 90 || el == -90)
        {
          c_upv(0) = -octave::math::signum (el)
                     * sin (az*M_PI/180.0)*(xlimits(1)-xlimits(0))/pb(0);
          c_upv(1) = octave::math::signum (el)
                     * cos (az*M_PI/180.0)*(ylimits(1)-ylimits(0))/pb(1);
        }
      else
        c_upv(2) = 1;

      m_cameraupvector = xform2cam (c_upv);
    }
  else
    c_upv = cam2xform (get_cameraupvector ().matrix_value ());

  Matrix x_view = xform_matrix ();
  Matrix x_projection = xform_matrix ();
  Matrix x_viewport = xform_matrix ();
  Matrix x_normrender;
  Matrix x_pre = xform_matrix ();

  m_x_render = xform_matrix ();
  m_x_render_inv = xform_matrix ();

  scale (x_pre, pb(0), pb(1), pb(2));
  translate (x_pre, -0.5, -0.5, -0.5);
  scale (x_pre, xd/(xlimits(1)-xlimits(0)), yd/(ylimits(1)-ylimits(0)),
         zd/(zlimits(1)-zlimits(0)));
  translate (x_pre, -xo, -yo, -zo);

  xform (c_eye, x_pre);
  xform (c_center, x_pre);
  scale (c_upv, pb(0)/(xlimits(1)-xlimits(0)), pb(1)/(ylimits(1)-ylimits(0)),
         pb(2)/(zlimits(1)-zlimits(0)));
  translate (c_center, -c_eye(0), -c_eye(1), -c_eye(2));

  ColumnVector F (c_center), f (F), UP (c_upv);
  normalize (f);
  normalize (UP);

  if (std::abs (dot (f, UP)) > 1e-15)
    {
      double fa = 1 / sqrt (1 - f(2)*f(2));
      scale (UP, fa, fa, fa);
    }

  ColumnVector s = cross (f, UP);
  ColumnVector u = cross (s, f);

  scale (x_view, 1, 1, -1);
  Matrix l = xform_matrix ();
  l(0, 0) = s(0); l(0, 1) = s(1); l(0, 2) = s(2);
  l(1, 0) = u(0); l(1, 1) = u(1); l(1, 2) = u(2);
  l(2, 0) = -f(0); l(2, 1) = -f(1); l(2, 2) = -f(2);
  x_view = x_view * l;
  translate (x_view, -c_eye(0), -c_eye(1), -c_eye(2));
  scale (x_view, pb(0), pb(1), pb(2));
  translate (x_view, -0.5, -0.5, -0.5);

  Matrix x_cube = x_view * unit_cube ();
  ColumnVector cmin = x_cube.row_min ();
  ColumnVector cmax = x_cube.row_max ();
  double xM = cmax(0) - cmin(0);
  double yM = cmax(1) - cmin(1);

  Matrix bb = get_boundingbox (true);

  double v_angle;

  if (cameraviewanglemode_is ("auto"))
    {
      double af;

      // FIXME: was this really needed?  When compared to Matlab, it
      // does not seem to be required.  Need investigation with concrete
      // graphics toolkit to see results visually.
      if (false && dowarp)
        af = (1.0 / (xM > yM ? xM : yM));
      else
        {
          if ((bb(2)/bb(3)) > (xM/yM))
            af = 1.0 / yM;
          else
            af = 1.0 / xM;
        }
      v_angle = 2 * (180.0 / M_PI) * atan (1 / (2 * af * norm (F)));

      m_cameraviewangle = v_angle;
    }
  else
    v_angle = get_cameraviewangle ();

  double pf = 1 / (2 * tan ((v_angle / 2) * M_PI / 180.0) * norm (F));
  scale (x_projection, pf, pf, 1);

  if (dowarp)
    {
      xM *= pf;
      yM *= pf;
      translate (x_viewport, bb(0)+bb(2)/2, bb(1)+bb(3)/2, 0);
      scale (x_viewport, bb(2)/xM, -bb(3)/yM, 1);
    }
  else
    {
      double pix = 1;
      if (autocam)
        {
          if ((bb(2)/bb(3)) > (xM/yM))
            pix = bb(3);
          else
            pix = bb(2);
        }
      else
        pix = (bb(2) < bb(3) ? bb(2) : bb(3));
      translate (x_viewport, bb(0)+bb(2)/2, bb(1)+bb(3)/2, 0);
      scale (x_viewport, pix, -pix, 1);
    }

  x_normrender = x_viewport * x_projection * x_view;

  x_cube = x_normrender * unit_cube ();
  cmin = x_cube.row_min ();
  cmax = x_cube.row_max ();
  m_x_zlim.resize (1, 2);
  m_x_zlim(0) = cmin(2);
  m_x_zlim(1) = cmax(2);

  m_x_render = x_normrender;
  scale (m_x_render, xd/(xlimits(1)-xlimits(0)), yd/(ylimits(1)-ylimits(0)),
         zd/(zlimits(1)-zlimits(0)));
  translate (m_x_render, -xo, -yo, -zo);

  m_x_render_inv = m_x_render.inverse ();

  // Note: these matrices are a slight modified version of the regular matrices,
  // more suited for OpenGL rendering (m_x_gl_mat1 => light => m_x_gl_mat2)
  m_x_gl_mat1 = x_view;
  scale (m_x_gl_mat1, xd/(xlimits(1)-xlimits(0)), yd/(ylimits(1)-ylimits(0)),
         zd/(zlimits(1)-zlimits(0)));
  translate (m_x_gl_mat1, -xo, -yo, -zo);
  m_x_gl_mat2 = x_viewport * x_projection;
}

static bool updating_axes_layout = false;

void
axes::properties::update_axes_layout (void)
{
  if (updating_axes_layout)
    return;

  graphics_xform xform = get_transform ();

  double xd = (xdir_is ("normal") ? 1 : -1);
  double yd = (ydir_is ("normal") ? 1 : -1);
  double zd = (zdir_is ("normal") ? 1 : -1);

  const Matrix xlims = xform.xscale (get_xlim ().matrix_value ());
  const Matrix ylims = xform.yscale (get_ylim ().matrix_value ());
  const Matrix zlims = xform.zscale (get_zlim ().matrix_value ());

  double x_min, x_max, y_min, y_max, z_min, z_max;
  x_min = xlims(0), x_max = xlims(1);
  y_min = ylims(0), y_max = ylims(1);
  z_min = zlims(0), z_max = zlims(1);

  ColumnVector p1, p2, dir (3);

  m_xstate = m_ystate = m_zstate = AXE_ANY_DIR;

  p1 = xform.transform (x_min, (y_min+y_max)/2, (z_min+z_max)/2, false);
  p2 = xform.transform (x_max, (y_min+y_max)/2, (z_min+z_max)/2, false);
  dir(0) = octave::math::round (p2(0) - p1(0));
  dir(1) = octave::math::round (p2(1) - p1(1));
  dir(2) = (p2(2) - p1(2));
  if (dir(0) == 0 && dir(1) == 0)
    m_xstate = AXE_DEPTH_DIR;
  else if (dir(2) == 0)
    {
      if (dir(0) == 0)
        m_xstate = AXE_VERT_DIR;
      else if (dir(1) == 0)
        m_xstate = AXE_HORZ_DIR;
    }

  if (dir(2) == 0)
    {
      if (dir(1) == 0)
        m_xPlane = (dir(0) > 0 ? x_max : x_min);
      else
        m_xPlane = (dir(1) < 0 ? x_max : x_min);
    }
  else
    m_xPlane = (dir(2) < 0 ? x_min : x_max);

  m_xPlaneN = (m_xPlane == x_min ? x_max : x_min);
  m_fx = (x_max - x_min) / sqrt (dir(0)*dir(0) + dir(1)*dir(1));

  p1 = xform.transform ((x_min + x_max)/2, y_min, (z_min + z_max)/2, false);
  p2 = xform.transform ((x_min + x_max)/2, y_max, (z_min + z_max)/2, false);
  dir(0) = octave::math::round (p2(0) - p1(0));
  dir(1) = octave::math::round (p2(1) - p1(1));
  dir(2) = (p2(2) - p1(2));
  if (dir(0) == 0 && dir(1) == 0)
    m_ystate = AXE_DEPTH_DIR;
  else if (dir(2) == 0)
    {
      if (dir(0) == 0)
        m_ystate = AXE_VERT_DIR;
      else if (dir(1) == 0)
        m_ystate = AXE_HORZ_DIR;
    }

  if (dir(2) == 0)
    {
      if (dir(1) == 0)
        m_yPlane = (dir(0) > 0 ? y_max : y_min);
      else
        m_yPlane = (dir(1) < 0 ? y_max : y_min);
    }
  else
    m_yPlane = (dir(2) < 0 ? y_min : y_max);

  m_yPlaneN = (m_yPlane == y_min ? y_max : y_min);
  m_fy = (y_max - y_min) / sqrt (dir(0)*dir(0) + dir(1)*dir(1));

  p1 = xform.transform ((x_min + x_max)/2, (y_min + y_max)/2, z_min, false);
  p2 = xform.transform ((x_min + x_max)/2, (y_min + y_max)/2, z_max, false);
  dir(0) = octave::math::round (p2(0) - p1(0));
  dir(1) = octave::math::round (p2(1) - p1(1));
  dir(2) = (p2(2) - p1(2));
  if (dir(0) == 0 && dir(1) == 0)
    m_zstate = AXE_DEPTH_DIR;
  else if (dir(2) == 0)
    {
      if (dir(0) == 0)
        m_zstate = AXE_VERT_DIR;
      else if (dir(1) == 0)
        m_zstate = AXE_HORZ_DIR;
    }

  if (dir(2) == 0)
    {
      if (dir(1) == 0)
        m_zPlane = (dir(0) > 0 ? z_min : z_max);
      else
        m_zPlane = (dir(1) < 0 ? z_min : z_max);
    }
  else
    m_zPlane = (dir(2) < 0 ? z_min : z_max);

  m_zPlaneN = (m_zPlane == z_min ? z_max : z_min);
  m_fz = (z_max - z_min) / sqrt (dir(0)*dir(0) + dir(1)*dir(1));

  octave::unwind_protect_var<bool> restore_var (updating_axes_layout, true);

  m_xySym = (xd*yd*(m_xPlane-m_xPlaneN)*(m_yPlane-m_yPlaneN) > 0);
  m_zSign = (zd*(m_zPlane-m_zPlaneN) <= 0);
  m_xyzSym = (m_zSign ? m_xySym : ! m_xySym);
  m_xpTick = (m_zSign ? m_xPlaneN : m_xPlane);
  m_ypTick = (m_zSign ? m_yPlaneN : m_yPlane);
  m_zpTick = (m_zSign ? m_zPlane : m_zPlaneN);
  m_xpTickN = (m_zSign ? m_xPlane : m_xPlaneN);
  m_ypTickN = (m_zSign ? m_yPlane : m_yPlaneN);
  m_zpTickN = (m_zSign ? m_zPlaneN : m_zPlane);

  // 2D mode
  m_x2Dtop = false;
  m_y2Dright = false;
  m_layer2Dtop = false;
  if (m_xstate == AXE_HORZ_DIR && m_ystate == AXE_VERT_DIR)
    {
      Matrix ylimits = get_ylim ().matrix_value ();
      if (xaxislocation_is ("top")
          || (yscale_is ("log") && xaxislocation_is ("origin")
              && (ylimits(1) < 0.)))
        {
          std::swap (m_yPlane, m_yPlaneN);
          m_x2Dtop = true;
        }
      m_ypTick = m_yPlaneN;
      m_ypTickN = m_yPlane;
      Matrix xlimits = get_xlim ().matrix_value ();
      if (yaxislocation_is ("right")
          || (xscale_is ("log") && yaxislocation_is ("origin")
              && (xlimits(1) < 0.)))
        {
          std::swap (m_xPlane, m_xPlaneN);
          m_y2Dright = true;
        }
      m_xpTick = m_xPlaneN;
      m_xpTickN = m_xPlane;
      if (layer_is ("top"))
        {
          m_zpTick = m_zPlaneN;
          m_layer2Dtop = true;
        }
      else
        m_zpTick = m_zPlane;
    }

  Matrix viewmat = get_view ().matrix_value ();
  m_nearhoriz = std::abs (viewmat(1)) <= 5;
  m_is2D = viewmat(1) == 90;

  update_ticklength ();
}

void
axes::properties::update_ticklength (void)
{
  bool mode2D = (((m_xstate > AXE_DEPTH_DIR ? 1 : 0) +
                  (m_ystate > AXE_DEPTH_DIR ? 1 : 0) +
                  (m_zstate > AXE_DEPTH_DIR ? 1 : 0)) == 2);

  if (tickdirmode_is ("auto"))
    m_tickdir.set (mode2D ? "in" : "out", true);

  double ticksign = (tickdir_is ("in") ? -1 : 1);

  Matrix bbox = get_boundingbox (true);
  Matrix ticklen = get_ticklength ().matrix_value ();
  ticklen(0) *= std::max (bbox(2), bbox(3));
  // FIXME: This algorithm is not Matlab-compatible.  See bug #55483.
  //        Scale the results of Octave's algorithm for better visuals.
  ticklen(1) *= (0.76 * std::max (bbox(2), bbox(3)));

  m_xticklen = ticksign * (mode2D ? ticklen(0) : ticklen(1));
  m_yticklen = ticksign * (mode2D ? ticklen(0) : ticklen(1));
  m_zticklen = ticksign * (mode2D ? ticklen(0) : ticklen(1));

  double offset = get___fontsize_points__ () / 2;

  m_xtickoffset = (mode2D ? std::max (0., m_xticklen) : std::abs (m_xticklen)) +
                  (m_xstate == AXE_HORZ_DIR ? offset*1.5 : offset);
  m_ytickoffset = (mode2D ? std::max (0., m_yticklen) : std::abs (m_yticklen)) +
                  (m_ystate == AXE_HORZ_DIR ? offset*1.5 : offset);
  m_ztickoffset = (mode2D ? std::max (0., m_zticklen) : std::abs (m_zticklen)) +
                  (m_zstate == AXE_HORZ_DIR ? offset*1.5 : offset);

  update_xlabel_position ();
  update_ylabel_position ();
  update_zlabel_position ();
  update_title_position ();
}

/*
## FIXME: A demo can't be called in a C++ file.  This should be made a test
## or moved to a .m file where it can be called.
%!demo
%! clf;
%! subplot (2,1,1);
%!  plot (rand (3));
%!  xlabel xlabel;
%!  ylabel ylabel;
%!  title title;
%! subplot (2,1,2);
%!  plot (rand (3));
%!  set (gca, "ticklength", get (gca, "ticklength") * 2, "tickdir", "out");
%!  xlabel xlabel;
%!  ylabel ylabel;
%!  title title;
*/

static ColumnVector
convert_label_position (const ColumnVector& p,
                        const text::properties& props,
                        const graphics_xform& xform,
                        const Matrix& bbox)
{
  ColumnVector retval;

  std::string to_units = props.get_units ();

  if (to_units != "data")
    {
      ColumnVector v = xform.transform (p(0), p(1), p(2));

      retval.resize (3);

      retval(0) = v(0) - bbox(0) + 1;
      retval(1) = bbox(1) + bbox(3) - v(1) + 1;
      retval(2) = 0;

      retval = convert_position (retval, "pixels", to_units,
                                 bbox.extract_n (0, 2, 1, 2));
    }
  else
    retval = p;

  return retval;
}

static bool updating_xlabel_position = false;

void
axes::properties::update_xlabel_position (void)
{
  if (updating_xlabel_position)
    return;

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (get_xlabel ());

  if (! go.valid_object ())
    return;

  text::properties& xlabel_props
    = reinterpret_cast<text::properties&> (go.get_properties ());

  bool isempty = xlabel_props.get_string ().isempty ();

  octave::unwind_protect_var<bool>
  restore_var (updating_xlabel_position, true);

  if (! isempty)
    {
      if (xlabel_props.horizontalalignmentmode_is ("auto"))
        {
          xlabel_props.set_horizontalalignment
          (m_xstate > AXE_DEPTH_DIR ? "center"
           : (m_xyzSym ? "left" : "right"));

          xlabel_props.set_horizontalalignmentmode ("auto");
        }

      if (xlabel_props.verticalalignmentmode_is ("auto"))
        {
          xlabel_props.set_verticalalignment
          (m_xstate == AXE_VERT_DIR || m_x2Dtop ? "bottom" : "top");

          xlabel_props.set_verticalalignmentmode ("auto");
        }
    }

  if (xlabel_props.positionmode_is ("auto")
      || xlabel_props.rotationmode_is ("auto"))
    {
      graphics_xform xform = get_transform ();

      Matrix ext (1, 2, 0.0);
      ext = get_ticklabel_extents (get_xtick ().matrix_value (),
                                   get_xticklabel ().string_vector_value (),
                                   get_xlim ().matrix_value ());

      double margin = 5;
      double wmax = ext(0) + margin;
      double hmax = ext(1) + margin;
      double angle = 0.0;
      ColumnVector p
        = graphics_xform::xform_vector ((m_xpTickN + m_xpTick)/2, m_ypTick, m_zpTick);

      bool tick_along_z = m_nearhoriz || octave::math::isinf (m_fy);
      if (tick_along_z)
        p(2) += (octave::math::signum (m_zpTick - m_zpTickN) * m_fz * m_xtickoffset);
      else
        p(1) += (octave::math::signum (m_ypTick - m_ypTickN) * m_fy * m_xtickoffset);

      p = xform.transform (p(0), p(1), p(2), false);

      switch (m_xstate)
        {
        case AXE_ANY_DIR:
          p(0) += (m_xyzSym ? wmax : -wmax);
          p(1) += hmax;
          break;

        case AXE_VERT_DIR:
          p(0) -= wmax;
          angle = 90;
          break;

        case AXE_HORZ_DIR:
          p(1) += (m_x2Dtop ? -hmax : hmax);
          break;
        }

      if (xlabel_props.positionmode_is ("auto"))
        {
          p = xform.untransform (p(0), p(1), p(2), true);

          p = convert_label_position (p, xlabel_props, xform,
                                      get_extent (false));

          xlabel_props.set_position (p.extract_n (0, 3).transpose ());
          xlabel_props.set_positionmode ("auto");
        }

      if (! isempty && xlabel_props.rotationmode_is ("auto"))
        {
          xlabel_props.set_rotation (angle);
          xlabel_props.set_rotationmode ("auto");
        }
    }
}

static bool updating_ylabel_position = false;

void
axes::properties::update_ylabel_position (void)
{
  if (updating_ylabel_position)
    return;

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (get_ylabel ());

  if (! go.valid_object ())
    return;

  text::properties& ylabel_props
    = reinterpret_cast<text::properties&> (go.get_properties ());

  bool isempty = ylabel_props.get_string ().isempty ();

  octave::unwind_protect_var<bool>
  restore_var (updating_ylabel_position, true);

  if (! isempty)
    {
      if (ylabel_props.horizontalalignmentmode_is ("auto"))
        {
          ylabel_props.set_horizontalalignment
          (m_ystate > AXE_DEPTH_DIR ? "center"
           : (! m_xyzSym ? "left" : "right"));

          ylabel_props.set_horizontalalignmentmode ("auto");
        }

      if (ylabel_props.verticalalignmentmode_is ("auto"))
        {
          ylabel_props.set_verticalalignment
          (m_ystate == AXE_VERT_DIR && ! m_y2Dright ? "bottom" : "top");

          ylabel_props.set_verticalalignmentmode ("auto");
        }
    }

  if (ylabel_props.positionmode_is ("auto")
      || ylabel_props.rotationmode_is ("auto"))
    {
      graphics_xform xform = get_transform ();

      Matrix ext (1, 2, 0.0);

      ext = get_ticklabel_extents (get_ytick ().matrix_value (),
                                   get_yticklabel ().string_vector_value (),
                                   get_ylim ().matrix_value ());
      double margin = 5;
      double wmax = ext(0) + margin;
      double hmax = ext(1) + margin;
      double angle = 0.0;
      ColumnVector p
        = graphics_xform::xform_vector (m_xpTick, (m_ypTickN + m_ypTick)/2, m_zpTick);

      bool tick_along_z = m_nearhoriz || octave::math::isinf (m_fx);
      if (tick_along_z)
        p(2) += (octave::math::signum (m_zpTick - m_zpTickN) * m_fz * m_ytickoffset);
      else
        p(0) += (octave::math::signum (m_xpTick - m_xpTickN) * m_fx * m_ytickoffset);

      p = xform.transform (p(0), p(1), p(2), false);

      switch (m_ystate)
        {
        case AXE_ANY_DIR:
          p(0) += (! m_xyzSym ? wmax : -wmax);
          p(1) += hmax;
          break;

        case AXE_VERT_DIR:
          p(0) += (m_y2Dright ? wmax : -wmax);
          angle = 90;
          break;

        case AXE_HORZ_DIR:
          p(1) += hmax;
          break;
        }

      if (ylabel_props.positionmode_is ("auto"))
        {
          p = xform.untransform (p(0), p(1), p(2), true);

          p = convert_label_position (p, ylabel_props, xform,
                                      get_extent (false));

          ylabel_props.set_position (p.extract_n (0, 3).transpose ());
          ylabel_props.set_positionmode ("auto");
        }

      if (! isempty && ylabel_props.rotationmode_is ("auto"))
        {
          ylabel_props.set_rotation (angle);
          ylabel_props.set_rotationmode ("auto");
        }
    }
}

static bool updating_zlabel_position = false;

void
axes::properties::update_zlabel_position (void)
{
  if (updating_zlabel_position)
    return;

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (get_zlabel ());

  if (! go.valid_object ())
    return;

  text::properties& zlabel_props
    = reinterpret_cast<text::properties&> (go.get_properties ());

  bool camAuto = cameraupvectormode_is ("auto");
  bool isempty = zlabel_props.get_string ().isempty ();

  octave::unwind_protect_var<bool>
  restore_updating_zlabel_position (updating_zlabel_position, true);

  if (! isempty)
    {
      if (zlabel_props.horizontalalignmentmode_is ("auto"))
        {
          zlabel_props.set_horizontalalignment
          ((m_zstate > AXE_DEPTH_DIR || camAuto) ? "center" : "right");

          zlabel_props.set_horizontalalignmentmode ("auto");
        }

      if (zlabel_props.verticalalignmentmode_is ("auto"))
        {
          zlabel_props.set_verticalalignment
          (m_zstate == AXE_VERT_DIR
           ? "bottom" : ((m_zSign || camAuto) ? "bottom" : "top"));

          zlabel_props.set_verticalalignmentmode ("auto");
        }
    }

  if (zlabel_props.positionmode_is ("auto")
      || zlabel_props.rotationmode_is ("auto"))
    {
      graphics_xform xform = get_transform ();

      Matrix ext (1, 2, 0.0);
      ext = get_ticklabel_extents (get_ztick ().matrix_value (),
                                   get_zticklabel ().string_vector_value (),
                                   get_zlim ().matrix_value ());

      double margin = 5;
      double wmax = ext(0) + margin;
      double hmax = ext(1) + margin;
      double angle = 0.0;
      ColumnVector p;

      if (m_xySym)
        {
          p = graphics_xform::xform_vector (m_xPlaneN, m_yPlane,
                                            (m_zpTickN + m_zpTick)/2);
          if (octave::math::isinf (m_fy))
            p(0) += octave::math::signum (m_xPlaneN - m_xPlane) * m_fx * m_ztickoffset;
          else
            p(1) += octave::math::signum (m_yPlane - m_yPlaneN) * m_fy * m_ztickoffset;
        }
      else
        {
          p = graphics_xform::xform_vector (m_xPlane, m_yPlaneN,
                                            (m_zpTickN + m_zpTick)/2);
          if (octave::math::isinf (m_fx))
            p(1) += octave::math::signum (m_yPlaneN - m_yPlane) * m_fy * m_ztickoffset;
          else
            p(0) += octave::math::signum (m_xPlane - m_xPlaneN) * m_fx * m_ztickoffset;
        }

      p = xform.transform (p(0), p(1), p(2), false);

      switch (m_zstate)
        {
        case AXE_ANY_DIR:
          if (camAuto)
            {
              p(0) -= wmax;
              angle = 90;
            }

          // FIXME: what's the correct offset?
          //
          //   p[0] += (! m_xySym ? wmax : -wmax);
          //   p[1] += (m_zSign ? hmax : -hmax);

          break;

        case AXE_VERT_DIR:
          p(0) -= wmax;
          angle = 90;
          break;

        case AXE_HORZ_DIR:
          p(1) += hmax;
          break;
        }

      if (zlabel_props.positionmode_is ("auto"))
        {
          p = xform.untransform (p(0), p(1), p(2), true);

          p = convert_label_position (p, zlabel_props, xform,
                                      get_extent (false));

          zlabel_props.set_position (p.extract_n (0, 3).transpose ());
          zlabel_props.set_positionmode ("auto");
        }

      if (! isempty && zlabel_props.rotationmode_is ("auto"))
        {
          zlabel_props.set_rotation (angle);
          zlabel_props.set_rotationmode ("auto");
        }
    }
}

static bool updating_title_position = false;

void
axes::properties::update_title_position (void)
{
  if (updating_title_position)
    return;

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (get_title ());

  if (! go.valid_object ())
    return;

  text::properties& title_props
    = reinterpret_cast<text::properties&> (go.get_properties ());

  octave::unwind_protect_var<bool> restore_var (updating_title_position, true);

  if (title_props.positionmode_is ("auto"))
    {
      graphics_xform xform = get_transform ();

      // FIXME: bbox should be stored in axes::properties
      Matrix bbox = get_extent (false);

      ColumnVector p
        = graphics_xform::xform_vector (bbox(0) + bbox(2)/2, bbox(1) - 10,
                                        (m_x_zlim(0) + m_x_zlim(1))/2);

      if (m_x2Dtop)
        {
          Matrix ext (1, 2, 0.0);
          ext = get_ticklabel_extents (get_xtick ().matrix_value (),
                                       get_xticklabel ().string_vector_value (),
                                       get_xlim ().matrix_value ());
          p(1) -= ext(1);
        }

      p = xform.untransform (p(0), p(1), p(2), true);

      p = convert_label_position (p, title_props, xform, bbox);

      title_props.set_position (p.extract_n (0, 3).transpose ());
      title_props.set_positionmode ("auto");
    }
}

void
axes::properties::update_autopos (const std::string& elem_type)
{
  if (elem_type == "xlabel")
    update_xlabel_position ();
  else if (elem_type == "ylabel")
    update_ylabel_position ();
  else if (elem_type == "zlabel")
    update_zlabel_position ();
  else if (elem_type == "title")
    update_title_position ();
  else if (elem_type == "sync")
    sync_positions ();
}

static void
normalized_aspectratios (Matrix& aspectratios, const Matrix& scalefactors,
                         double xlength, double ylength, double zlength)
{
  double xval = xlength / scalefactors(0);
  double yval = ylength / scalefactors(1);
  double zval = zlength / scalefactors(2);

  double minval = octave::math::min (octave::math::min (xval, yval), zval);

  aspectratios(0) = xval / minval;
  aspectratios(1) = yval / minval;
  aspectratios(2) = zval / minval;
}

static void
max_axes_scale (double& s, Matrix& limits, const Matrix& kids,
                double pbfactor, double dafactor, char limit_type, bool tight)
{
  if (tight)
    {
      double minval = octave::numeric_limits<double>::Inf ();
      double maxval = -octave::numeric_limits<double>::Inf ();
      double min_pos = octave::numeric_limits<double>::Inf ();
      double max_neg = -octave::numeric_limits<double>::Inf ();
      get_children_limits (minval, maxval, min_pos, max_neg, kids, limit_type);
      if (octave::math::isfinite (minval) && octave::math::isfinite (maxval))
        {
          limits(0) = minval;
          limits(1) = maxval;
          s = octave::math::max (s, (maxval - minval) / (pbfactor * dafactor));
        }
    }
  else
    s = octave::math::max (s, (limits(1) - limits(0)) / (pbfactor * dafactor));
}

static std::set<double> updating_aspectratios;

void
axes::properties::update_aspectratios (void)
{
  if (updating_aspectratios.find (get___myhandle__ ().value ())
      != updating_aspectratios.end ())
    return;

  Matrix xlimits = get_xlim ().matrix_value ();
  Matrix ylimits = get_ylim ().matrix_value ();
  Matrix zlimits = get_zlim ().matrix_value ();

  double dx = (xlimits(1) - xlimits(0));
  double dy = (ylimits(1) - ylimits(0));
  double dz = (zlimits(1) - zlimits(0));

  Matrix da = get_dataaspectratio ().matrix_value ();
  Matrix pba = get_plotboxaspectratio ().matrix_value ();

  if (dataaspectratiomode_is ("auto"))
    {
      if (plotboxaspectratiomode_is ("auto"))
        {
          pba = Matrix (1, 3, 1.0);
          m_plotboxaspectratio.set (pba, false);
        }

      normalized_aspectratios (da, pba, dx, dy, dz);
      m_dataaspectratio.set (da, false);
    }
  else if (plotboxaspectratiomode_is ("auto"))
    {
      normalized_aspectratios (pba, da, dx, dy, dz);
      m_plotboxaspectratio.set (pba, false);
    }
  else
    {
      double s = -octave::numeric_limits<double>::Inf ();
      bool modified_limits = false;
      Matrix kids;

      if (xlimmode_is ("auto") && ylimmode_is ("auto") && zlimmode_is ("auto"))
        {
          modified_limits = true;
          kids = get_children ();
          max_axes_scale (s, xlimits, kids, pba(0), da(0), 'x', true);
          max_axes_scale (s, ylimits, kids, pba(1), da(1), 'y', true);
          max_axes_scale (s, zlimits, kids, pba(2), da(2), 'z', true);
        }
      else if (xlimmode_is ("auto") && ylimmode_is ("auto"))
        {
          modified_limits = true;
          max_axes_scale (s, zlimits, kids, pba(2), da(2), 'z', false);
        }
      else if (ylimmode_is ("auto") && zlimmode_is ("auto"))
        {
          modified_limits = true;
          max_axes_scale (s, xlimits, kids, pba(0), da(0), 'x', false);
        }
      else if (zlimmode_is ("auto") && xlimmode_is ("auto"))
        {
          modified_limits = true;
          max_axes_scale (s, ylimits, kids, pba(1), da(1), 'y', false);
        }

      if (modified_limits)
        {
          octave::unwind_protect_var<std::set<double>>
              restore_var (updating_aspectratios);

          updating_aspectratios.insert (get___myhandle__ ().value ());

          dx = pba(0) * da(0);
          dy = pba(1) * da(1);
          dz = pba(2) * da(2);
          if (octave::math::isinf (s))
            s = 1 / octave::math::min (octave::math::min (dx, dy), dz);

          if (xlimmode_is ("auto"))
            {
              dx = s * dx;
              xlimits(0) = 0.5 * (xlimits(0) + xlimits(1) - dx);
              xlimits(1) = xlimits(0) + dx;
              set_xlim (xlimits);
              set_xlimmode ("auto");
            }

          if (ylimmode_is ("auto"))
            {
              dy = s * dy;
              ylimits(0) = 0.5 * (ylimits(0) + ylimits(1) - dy);
              ylimits(1) = ylimits(0) + dy;
              set_ylim (ylimits);
              set_ylimmode ("auto");
            }

          if (zlimmode_is ("auto"))
            {
              dz = s * dz;
              zlimits(0) = 0.5 * (zlimits(0) + zlimits(1) - dz);
              zlimits(1) = zlimits(0) + dz;
              set_zlim (zlimits);
              set_zlimmode ("auto");
            }
        }
      else
        {
          normalized_aspectratios (pba, da, dx, dy, dz);
          m_plotboxaspectratio.set (pba, false);
        }
    }
}

void
axes::properties::update_label_color (handle_property label,
                                      color_property col)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  gh_mgr.get_object (label.handle_value ()).set ("color", col.get ());
}

void
axes::properties::update_font (std::string prop)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  if (! prop.empty ())
    {
      octave_value val = get (prop);
      octave_value tval = val;
      if (prop == "fontsize")
        {
          tval = octave_value (val.double_value () *
                               get_titlefontsizemultiplier ());
          val  = octave_value (val.double_value () *
                               get_labelfontsizemultiplier ());
        }
      else if (prop == "fontweight")
        tval = get ("titlefontweight");

      gh_mgr.get_object (get_xlabel ()).set (prop, val);
      gh_mgr.get_object (get_ylabel ()).set (prop, val);
      gh_mgr.get_object (get_zlabel ()).set (prop, val);
      gh_mgr.get_object (get_title ()).set (prop, tval);
    }

  double dpr = device_pixel_ratio (get___myhandle__ ());

  octave::autolock guard (gh_mgr.graphics_lock ());

  m_txt_renderer.set_font (get ("fontname").string_value (),
                           get ("fontweight").string_value (),
                           get ("fontangle").string_value (),
                           get ("__fontsize_points__").double_value () * dpr);
}

// The INTERNAL flag defines whether position or outerposition is used.

Matrix
axes::properties::get_boundingbox (bool internal,
                                   const Matrix& parent_pix_size) const
{
  Matrix pos = (internal ? get_position ().matrix_value ()
                : get_outerposition ().matrix_value ());
  Matrix parent_size (parent_pix_size);

  if (parent_size.isempty ())
    {
      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      graphics_object go = gh_mgr.get_object (get_parent ());

      if (go.valid_object ())
        parent_size
          = go.get_properties ().get_boundingbox (true).extract_n (0, 2, 1, 2);
      else
        parent_size = default_figure_position ();
    }

  pos = convert_position (pos, get_units (), "pixels", parent_size);

  pos(0)--;
  pos(1)--;
  pos(1) = parent_size(1) - pos(1) - pos(3);

  return pos;
}

Matrix
axes::properties::get_extent (bool with_text, bool only_text_height) const
{
  graphics_xform xform = get_transform ();

  Matrix ext (1, 4, 0.0);
  ext(0) = ext(1) = octave::numeric_limits<double>::Inf ();
  ext(2) = ext(3) = -octave::numeric_limits<double>::Inf ();
  for (int i = 0; i <= 1; i++)
    for (int j = 0; j <= 1; j++)
      for (int k = 0; k <= 1; k++)
        {
          ColumnVector p = xform.transform (i ? m_xPlaneN : m_xPlane,
                                            j ? m_yPlaneN : m_yPlane,
                                            k ? m_zPlaneN : m_zPlane, false);
          ext(0) = std::min (ext(0), p(0));
          ext(1) = std::min (ext(1), p(1));
          ext(2) = std::max (ext(2), p(0));
          ext(3) = std::max (ext(3), p(1));
        }

  if (with_text)
    {
      for (int i = 0; i < 4; i++)
        {
          graphics_handle htext;
          if (i == 0)
            htext = get_title ();
          else if (i == 1)
            htext = get_xlabel ();
          else if (i == 2)
            htext = get_ylabel ();
          else if (i == 3)
            htext = get_zlabel ();

          gh_manager& gh_mgr = octave::__get_gh_manager__ ();

          text::properties& text_props
            = reinterpret_cast<text::properties&>
              (gh_mgr.get_object (htext).get_properties ());

          Matrix text_pos = text_props.get_data_position ();
          text_pos = xform.transform (text_pos(0), text_pos(1), text_pos(2));
          if (text_props.get_string ().isempty ())
            {
              ext(0) = std::min (ext(0), text_pos(0));
              ext(1) = std::min (ext(1), text_pos(1));
              ext(2) = std::max (ext(2), text_pos(0));
              ext(3) = std::max (ext(3), text_pos(1));
            }
          else
            {
              Matrix text_ext = text_props.get_extent_matrix (true);

              // The text extent is returned in device pixels.  Unscale and
              // work with logical pixels
              double dpr = device_pixel_ratio (get___myhandle__ ());
              if (dpr != 1.0)
                for (int j = 0; j < 4; j++)
                  text_ext(j) /= dpr;

              bool ignore_horizontal = false;
              bool ignore_vertical = false;
              if (only_text_height)
                {
                  double text_rotation = text_props.get_rotation ();
                  if (text_rotation == 0. || text_rotation == 180.)
                    ignore_horizontal = true;
                  else if (text_rotation == 90. || text_rotation == 270.)
                    ignore_vertical = true;
                }

              if (! ignore_horizontal)
                {
                  ext(0) = std::min (ext(0), text_pos(0)+text_ext(0));
                  ext(2) = std::max (ext(2),
                                     text_pos(0)+text_ext(0)+text_ext(2));
                }

              if (! ignore_vertical)
                {
                  ext(1) = std::min (ext(1),
                                     text_pos(1)-text_ext(1)-text_ext(3));
                  ext(3) = std::max (ext(3), text_pos(1)-text_ext(1));
                }
            }
        }
    }

  ext(2) = ext(2) - ext(0);
  ext(3) = ext(3) - ext(1);

  return ext;
}

static octave_value
convert_ticklabel_string (const octave_value& val)
{
  octave_value retval = val;

  if (val.iscellstr ())
    {
      // Always return a column vector for Matlab compatibility
      if (val.columns () > 1)
        retval = val.reshape (dim_vector (val.numel (), 1));
    }
  else
    {
      string_vector sv;
      if (val.isnumeric ())
        {
          NDArray data = val.array_value ();
          std::ostringstream oss;
          oss.precision (5);
          for (octave_idx_type i = 0; i < val.numel (); i++)
            {
              oss.str ("");
              oss << data(i);
              sv.append (oss.str ());
            }
        }
      else if (val.is_string () && val.rows () == 1)
        {
          std::string valstr = val.string_value ();
          std::istringstream iss (valstr);
          std::string tmpstr;

          // Split string with delimiter '|'
          while (std::getline (iss, tmpstr, '|'))
            sv.append (tmpstr);

          // If string ends with '|' Matlab appends a null string
          if (*valstr.rbegin () == '|')
            sv.append (std::string (""));
        }
      else
        return retval;

      charMatrix chmat (sv, ' ');

      retval = octave_value (chmat);
    }

  return retval;
}

void
axes::properties::set_xticklabel (const octave_value& val)
{
  if (m_xticklabel.set (convert_ticklabel_string (val), false))
    {
      set_xticklabelmode ("manual");
      m_xticklabel.run_listeners (GCB_POSTSET);
      mark_modified ();
    }
  else
    set_xticklabelmode ("manual");

  sync_positions ();
}

void
axes::properties::set_yticklabel (const octave_value& val)
{
  if (m_yticklabel.set (convert_ticklabel_string (val), false))
    {
      set_yticklabelmode ("manual");
      m_yticklabel.run_listeners (GCB_POSTSET);
      mark_modified ();
    }
  else
    set_yticklabelmode ("manual");

  sync_positions ();
}

void
axes::properties::set_zticklabel (const octave_value& val)
{
  if (m_zticklabel.set (convert_ticklabel_string (val), false))
    {
      set_zticklabelmode ("manual");
      m_zticklabel.run_listeners (GCB_POSTSET);
      mark_modified ();
    }
  else
    set_zticklabelmode ("manual");

  sync_positions ();
}

// Almost identical to convert_ticklabel_string but it only accepts
// cellstr or string, not numeric input.
static octave_value
convert_linestyleorder_string (const octave_value& val)
{
  octave_value retval = val;

  if (val.iscellstr ())
    {
      // Always return a column vector for Matlab Compatibility
      if (val.columns () > 1)
        retval = val.reshape (dim_vector (val.numel (), 1));
    }
  else
    {
      string_vector sv;
      if (val.is_string () && val.rows () == 1)
        {
          std::string valstr = val.string_value ();
          std::istringstream iss (valstr);
          std::string tmpstr;

          // Split string with delimiter '|'
          while (std::getline (iss, tmpstr, '|'))
            sv.append (tmpstr);

          // If string ends with '|' Matlab appends a null string
          if (*valstr.rbegin () == '|')
            sv.append (std::string (""));
        }
      else
        return retval;

      charMatrix chmat (sv, ' ');

      retval = octave_value (chmat);
    }

  return retval;
}

void
axes::properties::set_linestyleorder (const octave_value& val)
{
  m_linestyleorder.set (convert_linestyleorder_string (val), false);
}

void
axes::properties::set_units (const octave_value& val)
{
  caseless_str old_units = get_units ();

  if (m_units.set (val, true))
    {
      update_units (old_units);
      mark_modified ();
    }
}

void
axes::properties::update_units (const caseless_str& old_units)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object parent_go = gh_mgr.get_object (get_parent ());

  Matrix parent_bb
    = parent_go.get_properties ().get_boundingbox (true).extract_n (0, 2, 1, 2);

  caseless_str new_units = get_units ();
  m_position.set (octave_value
                  (convert_position (get_position ().matrix_value (),
                                     old_units, new_units, parent_bb)),
                  false);
  m_outerposition.set (octave_value
                       (convert_position (get_outerposition ().matrix_value (),
                                          old_units, new_units, parent_bb)),
                       false);
  m_tightinset.set (octave_value
                    (convert_position (get_tightinset ().matrix_value (),
                                       old_units, new_units, parent_bb)),
                    false);
  m_looseinset.set (octave_value
                    (convert_position (get_looseinset ().matrix_value (),
                                       old_units, new_units, parent_bb)),
                    false);
}

void
axes::properties::set_fontunits (const octave_value& val)
{
  caseless_str old_fontunits = get_fontunits ();

  if (m_fontunits.set (val, true))
    {
      update_fontunits (old_fontunits);
      mark_modified ();
    }
}

void
axes::properties::update_fontunits (const caseless_str& old_units)
{
  caseless_str new_units = get_fontunits ();
  double parent_height = get_boundingbox (true).elem (3);
  double fontsz = get_fontsize ();

  fontsz = convert_font_size (fontsz, old_units, new_units, parent_height);

  set_fontsize (octave_value (fontsz));
}

double
axes::properties::get___fontsize_points__ (double box_pix_height) const
{
  double fontsz = get_fontsize ();
  double parent_height = box_pix_height;

  if (fontunits_is ("normalized") && parent_height <= 0)
    parent_height = get_boundingbox (true).elem (3);

  return convert_font_size (fontsz, get_fontunits (), "points", parent_height);
}

ColumnVector
graphics_xform::xform_vector (double x, double y, double z)
{
  return octave::xform_vector (x, y, z);
}

Matrix
graphics_xform::xform_eye (void)
{
  return octave::xform_matrix ();
}

ColumnVector
graphics_xform::transform (double x, double y, double z, bool use_scale) const
{
  if (use_scale)
    {
      x = m_sx.scale (x);
      y = m_sy.scale (y);
      z = m_sz.scale (z);
    }

  return octave::transform (m_xform, x, y, z);
}

ColumnVector
graphics_xform::untransform (double x, double y, double z,
                             bool use_scale) const
{
  ColumnVector v = octave::transform (m_xform_inv, x, y, z);

  if (use_scale)
    {
      v(0) = m_sx.unscale (v(0));
      v(1) = m_sy.unscale (v(1));
      v(2) = m_sz.unscale (v(2));
    }

  return v;
}

octave_value
axes::get_default (const caseless_str& pname) const
{
  octave_value retval = m_default_properties.lookup (pname);

  if (retval.is_undefined ())
    {
      graphics_handle parent_h = get_parent ();

      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      graphics_object parent_go = gh_mgr.get_object (parent_h);

      retval = parent_go.get_default (pname);
    }

  return retval;
}

// FIXME: remove.
// FIXME: maybe this should go into array_property class?
/*
static void
check_limit_vals (double& min_val, double& max_val,
                  double& min_pos, double& max_neg,
                  const array_property& data)
{
  double val = data.min_val ();
  if (octave::math::isfinite (val) && val < min_val)
    min_val = val;
  val = data.max_val ();
  if (octave::math::isfinite (val) && val > max_val)
    max_val = val;
  val = data.min_pos ();
  if (octave::math::isfinite (val) && val > 0 && val < min_pos)
    min_pos = val;
  val = data.max_neg ();
  if (octave::math::isfinite (val) && val < 0 && val > max_neg)
    max_neg = val;
}
*/

static void
check_limit_vals (double& min_val, double& max_val,
                  double& min_pos, double& max_neg,
                  const octave_value& data)
{
  Matrix m;

  if (data.is_matrix_type ())
    m = data.matrix_value ();

  if (m.numel () != 4)
    {
      m = Matrix (1, 4, 0.0);
      m(2) = octave::numeric_limits<double>::Inf ();
      m(3) = -octave::numeric_limits<double>::Inf ();
    }

  double val;

  val = m(0);
  if (octave::math::isfinite (val) && val < min_val)
    min_val = val;

  val = m(1);
  if (octave::math::isfinite (val) && val > max_val)
    max_val = val;

  val = m(2);
  if (octave::math::isfinite (val) && val > 0 && val < min_pos)
    min_pos = val;

  val = m(3);
  if (octave::math::isfinite (val) && val < 0 && val > max_neg)
    max_neg = val;
}

// magform(x) Returns (a, b),
// where x = a * 10^b, abs (a) >= 1., and b is integer.

static void
magform (double x, double& a, int& b)
{
  if (x == 0)
    {
      a = 0;
      b = 0;
    }
  else
    {
      b = static_cast<int> (std::floor (std::log10 (std::abs (x))));
      a = x / std::pow (10.0, b);
    }
}

void
axes::properties::update_outerposition (void)
{
  set_positionconstraint ("outerposition");
  caseless_str old_units = get_units ();
  set_units ("normalized");

  Matrix outerbox = m_outerposition.get ().matrix_value ();

  double outer_left = outerbox(0);
  double outer_bottom = outerbox(1);
  double outer_width = outerbox(2);
  double outer_height = outerbox(3);

  double outer_right = outer_width + outer_left;
  double outer_top = outer_height + outer_bottom;

  Matrix linset = m_looseinset.get ().matrix_value ();
  Matrix tinset = m_tightinset.get ().matrix_value ();

  double left_margin = std::max (linset(0), tinset(0));
  double bottom_margin = std::max (linset(1), tinset(1));
  double right_margin = std::max (linset(2), tinset(2));
  double top_margin = std::max (linset(3), tinset(3));

  double inner_left = outer_left;
  double inner_right = outer_right;

  if ((left_margin + right_margin) < outer_width)
    {
      inner_left += left_margin;
      inner_right -= right_margin;
    }

  double inner_bottom = outer_bottom;
  double inner_top = outer_top;

  if ((bottom_margin + top_margin) < outer_height)
    {
      inner_bottom += bottom_margin;
      inner_top -= top_margin;
    }

  double inner_width = inner_right - inner_left;
  double inner_height = inner_top - inner_bottom;

  Matrix innerbox (1, 4);

  innerbox(0) = inner_left;
  innerbox(1) = inner_bottom;
  innerbox(2) = inner_width;
  innerbox(3) = inner_height;

  m_position = innerbox;

  set_units (old_units);
  update_transform ();
}

void
axes::properties::update_position (void)
{
  set_positionconstraint ("innerposition");
  caseless_str old_units = get_units ();
  set_units ("normalized");

  Matrix innerbox = m_position.get ().matrix_value ();

  double inner_left = innerbox(0);
  double inner_bottom = innerbox(1);
  double inner_width = innerbox(2);
  double inner_height = innerbox(3);

  double inner_right = inner_width + inner_left;
  double inner_top = inner_height + inner_bottom;

  Matrix linset = m_looseinset.get ().matrix_value ();
  Matrix tinset = m_tightinset.get ().matrix_value ();

  double left_margin = std::max (linset(0), tinset(0));
  double bottom_margin = std::max (linset(1), tinset(1));
  double right_margin = std::max (linset(2), tinset(2));
  double top_margin = std::max (linset(3), tinset(3));

  // FIXME: do we need to place limits on any of these?

  double outer_left = inner_left - left_margin;
  double outer_bottom = inner_bottom - bottom_margin;
  double outer_right = inner_right + right_margin;
  double outer_top = inner_top + top_margin;

  double outer_width = outer_right - outer_left;
  double outer_height = outer_top - outer_bottom;

  Matrix outerbox (1, 4);

  outerbox(0) = outer_left;
  outerbox(1) = outer_bottom;
  outerbox(2) = outer_width;
  outerbox(3) = outer_height;

  m_outerposition = outerbox;

  set_units (old_units);
  update_transform ();
}

void
axes::properties::update_looseinset (void)
{
  caseless_str old_units = get_units ();
  set_units ("normalized");

  Matrix linset = m_looseinset.get ().matrix_value ();
  Matrix tinset = m_tightinset.get ().matrix_value ();

  double left_margin = std::max (linset(0), tinset(0));
  double bottom_margin = std::max (linset(1), tinset(1));
  double right_margin = std::max (linset(2), tinset(2));
  double top_margin = std::max (linset(3), tinset(3));

  if (m_positionconstraint.is ("innerposition"))
    {
      Matrix innerbox = m_position.get ().matrix_value ();

      double inner_left = innerbox(0);
      double inner_bottom = innerbox(1);
      double inner_width = innerbox(2);
      double inner_height = innerbox(3);

      double inner_right = inner_width + inner_left;
      double inner_top = inner_height + inner_bottom;

      // FIXME: do we need to place limits on any of these?

      double outer_left = inner_left - left_margin;
      double outer_bottom = inner_bottom - bottom_margin;
      double outer_right = inner_right + right_margin;
      double outer_top = inner_top + top_margin;

      double outer_width = outer_right - outer_left;
      double outer_height = outer_top - outer_bottom;

      Matrix outerbox (1, 4);

      outerbox(0) = outer_left;
      outerbox(1) = outer_bottom;
      outerbox(2) = outer_width;
      outerbox(3) = outer_height;

      m_outerposition = outerbox;
    }
  else
    {
      Matrix outerbox = m_outerposition.get ().matrix_value ();

      double outer_left = outerbox(0);
      double outer_bottom = outerbox(1);
      double outer_width = outerbox(2);
      double outer_height = outerbox(3);

      double outer_right = outer_width + outer_left;
      double outer_top = outer_height + outer_bottom;

      double inner_left = outer_left;
      double inner_right = outer_right;

      if ((left_margin + right_margin) < outer_width)
        {
          inner_left += left_margin;
          inner_right -= right_margin;
        }

      double inner_bottom = outer_bottom;
      double inner_top = outer_top;

      if ((bottom_margin + top_margin) < outer_height)
        {
          inner_bottom += bottom_margin;
          inner_top -= top_margin;
        }

      double inner_width = inner_right - inner_left;
      double inner_height = inner_top - inner_bottom;

      Matrix innerbox (1, 4);

      innerbox(0) = inner_left;
      innerbox(1) = inner_bottom;
      innerbox(2) = inner_width;
      innerbox(3) = inner_height;

      m_position = innerbox;
    }

  set_units (old_units);
  update_transform ();
}

// A translation from Tom Holoryd's python code at
// http://kurage.nimh.nih.gov/tomh/tics.py
// FIXME: add log ticks

double
axes::properties::calc_tick_sep (double lo, double hi)
{
  int ticint = 5;

  // Reference: Lewart, C. R., "Algorithms SCALE1, SCALE2, and SCALE3 for
  // Determination of Scales on Computer Generated Plots", Communications of
  // the ACM, 10 (1973), 639-640.
  // Also cited as ACM Algorithm 463.

  double a;
  int b, x;

  magform ((hi - lo) / ticint, a, b);

  static const double sqrt_2 = sqrt (2.0);
  static const double sqrt_10 = sqrt (10.0);
  static const double sqrt_50 = sqrt (50.0);

  if (a < sqrt_2)
    x = 1;
  else if (a < sqrt_10)
    x = 2;
  else if (a < sqrt_50)
    x = 5;
  else
    x = 10;

  return x * std::pow (10., b);
}

// Attempt to make "nice" limits from the actual max and min of the data.
// For log plots, we will also use the smallest strictly positive value.

Matrix
axes::properties::get_axis_limits (double xmin, double xmax,
                                   double min_pos, double max_neg,
                                   const bool logscale,
                                   const std::string& method)
{
  Matrix retval;

  double min_val = xmin;
  double max_val = xmax;

  if (octave::math::isinf (min_val) && min_val > 0
      && octave::math::isinf (max_val) && max_val < 0)
    {
      retval = default_lim (logscale);
      return retval;
    }
  else if (! (octave::math::isinf (min_val) || octave::math::isinf (max_val)))
    {
      if (logscale)
        {
          if (octave::math::isinf (min_pos) && octave::math::isinf (max_neg))
            {
              // FIXME: max_neg is needed for "loglog ([0 -Inf])"
              //        This is the *only* place where max_neg is needed.
              //        Is there another way?
              retval = default_lim (logscale);
              return retval;
            }
          if (min_val <= 0)
            {
              if (max_val > 0)
                {
                  warning_with_id ("Octave:negative-data-log-axis",
                                   "axis: omitting non-positive data in log plot");
                  min_val = min_pos;
                }
              else if (max_val == 0)
                max_val = max_neg;
            }
          // FIXME: maybe this test should also be relative?
          if (std::abs (min_val - max_val)
              < sqrt (std::numeric_limits<double>::epsilon ()))
            {
              // Widen range when too small
              if (min_val >= 0)
                {
                  min_val *= 0.9;
                  max_val *= 1.1;
                }
              else
                {
                  min_val *= 1.1;
                  max_val *= 0.9;
                }
            }

          if (method == "tickaligned")
            {
              if (min_val > 0)
                {
                  // Log plots with all positive data
                  min_val = std::pow (10, std::floor (log10 (min_val)));
                  max_val = std::pow (10, std::ceil (log10 (max_val)));
                }
              else
                {
                  // Log plots with all negative data
                  min_val = -std::pow (10, std::ceil (log10 (-min_val)));
                  max_val = -std::pow (10, std::floor (log10 (-max_val)));
                }
            }
          else if (method == "padded")
            {
              if (min_val > 0)
                {
                  // Log plots with all positive data
                  double pad = (log10 (max_val) - log10 (min_val)) * 0.07;
                  min_val = std::pow (10, log10 (min_val) - pad);
                  max_val = std::pow (10, log10 (max_val) + pad);
                }
              else
                {
                  // Log plots with all negative data
                  double pad = (log10 (-min_val) - log10 (-max_val)) * 0.07;
                  min_val = -std::pow (10, log10 (-min_val) + pad);
                  max_val = -std::pow (10, log10 (-max_val) - pad);
                }
            }
        }
      else
        {
          if (min_val == 0 && max_val == 0)
            {
              min_val = -1;
              max_val = 1;
            }
          // FIXME: maybe this test should also be relative?
          else if (std::abs (min_val - max_val)
                   < sqrt (std::numeric_limits<double>::epsilon ()))
            {
              min_val -= 0.1 * std::abs (min_val);
              max_val += 0.1 * std::abs (max_val);
            }

          if (method == "tickaligned")
            {
              double tick_sep = calc_tick_sep (min_val, max_val);
              double min_tick = std::floor (min_val / tick_sep);
              double max_tick = std::ceil (max_val / tick_sep);
              // Prevent round-off from cropping ticks
              min_val = std::min (min_val, tick_sep * min_tick);
              max_val = std::max (max_val, tick_sep * max_tick);
            }
          else if (method == "padded")
            {
              double pad = 0.07 * (max_val - min_val);
              min_val -= pad;
              max_val += pad;
            }
        }
    }

  retval.resize (1, 2);

  retval(0) = min_val;
  retval(1) = max_val;

  return retval;
}

void
axes::properties::check_axis_limits (Matrix& limits, const Matrix kids,
                                     const bool logscale, char& update_type)
{
  double min_val = octave::numeric_limits<double>::Inf ();
  double max_val = -octave::numeric_limits<double>::Inf ();
  double min_pos = octave::numeric_limits<double>::Inf ();
  double max_neg = -octave::numeric_limits<double>::Inf ();
  double eps = std::numeric_limits<double>::epsilon ();
  bool do_update = false;
  bool have_children_limits = false;

  // check whether we need to get children limits
  if (! octave::math::isfinite (limits(0))
      || ! octave::math::isfinite (limits(1)))
    {
      get_children_limits (min_val, max_val, min_pos, max_neg, kids,
                           update_type);
      have_children_limits = true;
    }
  if (! octave::math::isfinite (limits(0)))
    {
      limits(0) = min_val;
      do_update = true;
    }
  if (! octave::math::isfinite (limits(1)))
    {
      limits(1) = max_val;
      do_update = true;
    }
  if (limits(0) == 0 && limits(1) == 0)
    {
      limits = default_lim (logscale);
      do_update = true;
    }
  // FIXME: maybe this test should also be relative?
  else if (! logscale && (std::abs (limits(0) - limits(1)) < sqrt (eps)))
    {
      limits(0) -= 0.1 * std::abs (limits(0));
      limits(1) += 0.1 * std::abs (limits(1));
      do_update = true;
    }
  else if (logscale
           && (std::abs (std::log10 (limits(0) / limits(1))) < sqrt (eps)))
    {
      limits(0) = (limits(0) < 0 ? 10.0 * limits(0) : 0.1 * limits(0));
      limits(1) = (limits(1) < 0 ? 0.1 * limits(1) : 10.0 * limits(1));
      do_update = true;
    }

  if (logscale && limits(0)*limits(1) <= 0)
    {
      if (! have_children_limits)
        get_children_limits (min_val, max_val, min_pos, max_neg, kids,
                             update_type);

      if (limits(1) > 0)
        {
          warning_with_id ("Octave:axis-non-positive-log-limits",
                           "Non-positive limit for logarithmic axis ignored\n");
          if (octave::math::isfinite (min_pos))
            limits(0) = min_pos;
          else
            limits(0) = 0.1 * limits(1);
        }
      else
        {
          warning_with_id ("Octave:axis-non-negative-log-limits",
                           "Non-negative limit for logarithmic axis ignored\n");
          if (octave::math::isfinite (max_neg))
            limits(1) = max_neg;
          else
            limits(1) = 0.1 * limits(0);
        }
      // FIXME: maybe this test should also be relative?
      if (std::abs (limits(0) - limits(1)) < sqrt (eps))
        {
          // Widen range when too small
          if (limits(0) > 0)
            {
              limits(0) *= 0.9;
              limits(1) *= 1.1;
            }
          else
            {
              limits(0) *= 1.1;
              limits(1) *= 0.9;
            }
        }
      do_update = true;
    }

  if (! do_update)
    update_type = 0;

}

/*
## Test validation of auto and manual axis limits
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   plot (0, pi);
%!   assert (get (hax, "xlim"), [-1, 1]);
%!   assert (get (hax, "xlimmode"), "auto");
%!   assert (get (hax, "ylim"), [2.8, 3.5], 2*eps);
%!   assert (get (hax, "ylimmode"), "auto");
%!   set (hax, "xlim", [1, 1], "ylim", [0, 0]);
%!   assert (get (hax, "xlim"), [0.9, 1.1]);
%!   assert (get (hax, "xlimmode"), "manual");
%!   assert (get (hax, "ylim"), [0, 1]);
%!   assert (get (hax, "ylimmode"), "manual");
%!   set (hax, "xlim", [-Inf, Inf], "ylim", [-Inf, Inf]);
%!   ## Matlab does not update the properties
%!   assert (get (hax, "xlim"), [0, 1]);
%!   assert (get (hax, "ylim"), [0.9, 1.1]*pi, 2*eps);
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   plot ([0, exp(1)], [-pi, 3]);
%!   assert (get (hax, "xlim"), [0, 3]);
%!   assert (get (hax, "xlimmode"), "auto");
%!   assert (get (hax, "ylim"), [-4, 3]);
%!   assert (get (hax, "ylimmode"), "auto");
%!   set (hax, "xlim", [-Inf, Inf], "ylim", [-Inf, Inf]);
%!   ## Matlab does not update the properties but uses tight limits on screen
%!   assert (get (hax, "xlim"), [0, exp(1)]);
%!   assert (get (hax, "xlimmode"), "manual");
%!   assert (get (hax, "ylim"), [-pi, 3]);
%!   assert (get (hax, "ylimmode"), "manual");
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   loglog (0, pi);
%!   assert (get (hax, "xlim"), [0.1, 1.0]);
%!   assert (get (hax, "xlimmode"), "auto");
%!   assert (get (hax, "ylim"), [1, 10]);
%!   assert (get (hax, "ylimmode"), "auto");
%!   set (hax, "xlim", [1, 1], "ylim", [0, 0]);
%!   assert (get (hax, "xlim"), [0.1, 10]);
%!   assert (get (hax, "xlimmode"), "manual");
%!   assert (get (hax, "ylim"), [0.1, 1.0]);
%!   assert (get (hax, "ylimmode"), "manual");
%!   set (hax, "xlim", [-Inf, Inf], "ylim", [-Inf, Inf]);
%!   ## Matlab does not update the properties
%!   assert (get (hax, "xlim"), [0.1, 1.0]);
%!   assert (get (hax, "ylim"), [0.1, 10]*pi);
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   loglog ([0 -1], [0 1]);
%!   assert (get (hax, "xlim"), [-10, -0.1]);
%!   assert (get (hax, "xlimmode"), "auto");
%!   assert (get (hax, "ylim"), [0.1, 10]);
%!   assert (get (hax, "ylimmode"), "auto");
%!   set (hax, "xlim", [-Inf, Inf], "ylim", [-Inf, Inf]);
%!   ## Matlab does not update the properties
%!   assert (get (hax, "xlim"), [-1.1, -0.9]);
%!   assert (get (hax, "ylim"), [0.9, 1.1]);
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   loglog ([1 -1], [1 pi]);
%!   assert (get (hax, "xlim"), [0.1, 10]);
%!   assert (get (hax, "xlimmode"), "auto");
%!   assert (get (hax, "ylim"), [1, 10]);
%!   assert (get (hax, "ylimmode"), "auto");
%!   set (hax, "xlim", [-Inf, Inf], "ylim", [-Inf, Inf]);
%!   ## Matlab does not update the properties but uses tight limits on screen
%!   assert (get (hax, "xlim"), [0.9, 1.1]);
%!   assert (get (hax, "ylim"), [1, pi]);
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect

## Check that graphics objects with hidden handle visibility are included in
## axis limit calculation.
%!test <*63095>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   plot (hax, [0, 1]);
%!   assert (get (hax, "ylim"), [0, 1]);
%!   hold (hax, "on");
%!   plot (hax, [2, 0], "handlevisibility", "off");
%!   assert (get (hax, "ylim"), [0, 2]);
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect

## Check automatically determined axes ticks with "tickaligned" (default) and
## "tight" xlimitmethod.
%!test <*63624>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   plot (hax, [1, 201], [0, 1]);
%!   assert (get (hax, "xtick"), 0:50:250);
%!   axis (hax, "tight");
%!   assert (get (hax, "xtick"), 50:50:200);
%! unwind_protect_cleanup
%!   delete (hf);
%! end_unwind_protect
*/

void
axes::properties::calc_ticks_and_lims (array_property& lims,
                                       array_property& ticks,
                                       array_property& mticks,
                                       bool limmode_is_auto,
                                       bool tickmode_is_auto,
                                       bool is_logscale,
                                       bool method_is_padded,
                                       bool method_is_tight)
{
  if (lims.get ().isempty ())
    return;

  double lo = (lims.get ().matrix_value ())(0);
  double hi = (lims.get ().matrix_value ())(1);

  double lo_lim = lo;
  double hi_lim = hi;
  bool is_negative = lo < 0 && hi < 0;

  // FIXME: should this be checked for somewhere else? (i.e., set{x,y,z}lim)
  if (hi < lo)
    std::swap (hi, lo);

  if (is_logscale)
    {
      if (is_negative)
        {
          double tmp = hi;
          hi = std::log10 (-lo);
          lo = std::log10 (-tmp);
        }
      else
        {
          hi = std::log10 (hi);
          lo = std::log10 (lo);
        }
    }

  Matrix tmp_ticks;
  if (tickmode_is_auto)
    {
      double tick_sep;

      if (is_logscale)
        {
          if (! (octave::math::isinf (hi) || octave::math::isinf (lo)))
            tick_sep = 1;  // Tick is every order of magnitude (bug #39449)
          else
            tick_sep = 0;
        }
      else
        tick_sep = calc_tick_sep (lo, hi);

      double i1 = std::floor (lo / tick_sep);
      double i2 = std::ceil (hi / tick_sep);

      if (limmode_is_auto)
        {
          Matrix tmp_lims (1, 2);

          if (! method_is_padded && ! method_is_tight)
            {
              // Adjust limits to include min and max ticks
              tmp_lims(0) = std::min (tick_sep * i1, lo);
              tmp_lims(1) = std::max (tick_sep * i2, hi);
            }
          else
            {
              tmp_lims(0) = lo;
              tmp_lims(1) = hi;

              // adjust min and max ticks to be within limits
              if (i1*tick_sep < lo)
                i1++;
              if (i2*tick_sep > hi && i2 > i1)
                i2--;
            }

          if (is_logscale)
            {
              tmp_lims(0) = std::pow (10., tmp_lims(0));
              tmp_lims(1) = std::pow (10., tmp_lims(1));

              if (tmp_lims(0) <= 0)
                tmp_lims(0) = std::pow (10., lo);

              if (is_negative)
                {
                  double tmp = tmp_lims(0);
                  tmp_lims(0) = -tmp_lims(1);
                  tmp_lims(1) = -tmp;
                }
            }

          lims = tmp_lims;
        }
      else
        {
          // adjust min and max ticks to be within limits
          if (i1*tick_sep < lo)
            i1++;
          if (i2*tick_sep > hi && i2 > i1)
            i2--;
        }

      tmp_ticks = Matrix (1, i2-i1+1);
      for (int i = 0; i <= static_cast<int> (i2-i1); i++)
        {
          tmp_ticks(i) = tick_sep * (i+i1);
          if (is_logscale)
            tmp_ticks(i) = std::pow (10., tmp_ticks(i));
        }
      if (is_logscale && is_negative)
        {
          Matrix rev_ticks (1, i2-i1+1);
          rev_ticks = -tmp_ticks;
          for (int i = 0; i <= static_cast<int> (i2-i1); i++)
            tmp_ticks(i) = rev_ticks(i2-i1-i);
        }

      ticks = tmp_ticks;
    }
  else
    tmp_ticks = ticks.get ().matrix_value ();

  octave_idx_type n_ticks = tmp_ticks.numel ();
  if (n_ticks < 2)
    return;

  // minor ticks between, above, and below min and max ticks
  int n = (is_logscale ? 8 : 4);
  double mult_below = (is_logscale ? tmp_ticks(1) / tmp_ticks(0) : 1);
  double mult_above = (is_logscale ? tmp_ticks(n_ticks-1) / tmp_ticks(n_ticks-2)
                       : 1);

  double d_below = (tmp_ticks(1) - tmp_ticks(0)) / mult_below / (n+1);
  int n_below = static_cast<int> (std::floor ((tmp_ticks(0)-lo_lim) / d_below));
  if (n_below < 0)
    n_below = 0;
  int n_between = n * (n_ticks - 1);
  double d_above = (tmp_ticks(n_ticks-1) - tmp_ticks(n_ticks-2)) * mult_above
                   / (n+1);
  int n_above = static_cast<int> (std::floor ((hi_lim-tmp_ticks(n_ticks-1))
                                  / d_above));
  if (n_above < 0)
    n_above = 0;

  Matrix tmp_mticks (1, n_below + n_between + n_above);
  for (int i = 0; i < n_below; i++)
    tmp_mticks(i) = tmp_ticks(0) - (n_below-i) * d_below;
  for (int i = 0; i < n_ticks-1; i++)
    {
      double d = (tmp_ticks(i+1) - tmp_ticks(i)) / (n + 1);
      for (int j = 0; j < n; j++)
        tmp_mticks(n_below+n*i+j) = tmp_ticks(i) + d * (j+1);
    }
  for (int i = 0; i < n_above; i++)
    tmp_mticks(n_below+n_between+i) = tmp_ticks(n_ticks-1) + (i + 1) * d_above;

  mticks = tmp_mticks;
}

/*
%!test <*45356>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot (1:10);
%!   xlim ([4.75, 8.5]);
%!   tics = get (gca, "xtick");
%!   assert (tics, [5 6 7 8]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
*/

void
axes::properties::calc_ticklabels (const array_property& ticks,
                                   any_property& labels, bool logscale,
                                   const bool is_origin,
                                   const int other_axislocation,
                                   const array_property& axis_lims)
{
  Matrix values = ticks.get ().matrix_value ();
  Matrix lims = axis_lims.get ().matrix_value ();
  Cell c (values.dims ());
  std::ostringstream os;

  // omit tick labels depending on location of other axis
  ColumnVector omit_ticks (3, octave::numeric_limits<double>::NaN ());
  if (get_is2D () && is_origin)
    {
      if (other_axislocation == 0)
        {
          omit_ticks(0) = octave::math::max (octave::math::min (0., lims(1)),
                                             lims(0));
        }
      else if (other_axislocation == 1)
        omit_ticks(0) = lims(1);
      else if (other_axislocation == -1)
        omit_ticks(0) = lims(0);
      if (is_box ())
        {
          omit_ticks(1) = lims(0);
          omit_ticks(2) = lims(1);
        }
    }

  if (logscale)
    {
      double significand;
      double exponent;
      double exp_max = 0.0;
      double exp_min = 0.0;

      for (int i = 0; i < values.numel (); i++)
        {
          double exp = std::log10 (values(i));
          exp_min = std::min (exp_min, exp);
          exp_max = std::max (exp_max, exp);
        }

      for (int i = 0; i < values.numel (); i++)
        {
          bool omit_tick = false;
          for (int i_omit = 0; i_omit < omit_ticks.numel (); i_omit++)
            if (values(i) == omit_ticks(i_omit))
              omit_tick = true;
          if (omit_tick)
            {
              c(i) = "";
              continue;
            }

          if (values(i) < 0.0)
            exponent = std::floor (std::log10 (-values(i)));
          else
            exponent = std::floor (std::log10 (values(i)));
          significand = values(i) * std::pow (10., -exponent);

          os.str ("");
          if ((std::abs (significand) - 1) >
              10*std::numeric_limits<double>::epsilon())
            os << significand << 'x';
          else if (significand < 0)
            os << '-';

          os << "10^{";

          if (exponent < 0.0)
            {
              os << '-';
              exponent = -exponent;
            }
          if (exponent < 10. && (exp_max > 9 || exp_min < -9))
            os << '0';
          os << exponent << '}';

          if (m_ticklabelinterpreter.is ("latex"))
            c(i) = "$" + os.str () + "$";
          else
            c(i) = os.str ();
        }
    }
  else
    {
      for (int i = 0; i < values.numel (); i++)
        {
          bool omit_tick = false;
          for (int i_omit = 0; i_omit < omit_ticks.numel (); i_omit++)
            if (values(i) == omit_ticks(i_omit))
              omit_tick = true;
          if (omit_tick)
            c(i) = "";
          else
            {
              os.str ("");
              os << values(i);
              c(i) = os.str ();
            }
        }
    }

  labels = c;
}

Matrix
axes::properties::get_ticklabel_extents (const Matrix& ticks,
    const string_vector& ticklabels,
    const Matrix& limits)
{
  Matrix ext (1, 2, 0.0);
  double wmax, hmax;
  double dpr = device_pixel_ratio (get___myhandle__ ());
  wmax = hmax = 0.0;
  int n = std::min (ticklabels.numel (), ticks.numel ());
  for (int i = 0; i < n; i++)
    {
      double val = ticks(i);
      if (limits(0) <= val && val <= limits(1))
        {
          std::string label (ticklabels(i));
          label.erase (0, label.find_first_not_of (' '));
          label = label.substr (0, label.find_last_not_of (' ')+1);

          if (m_txt_renderer.ok ())
            {
              gh_manager& gh_mgr = octave::__get_gh_manager__ ();

              octave::autolock guard (gh_mgr.graphics_lock ());

              ext = m_txt_renderer.get_extent (label, 0.0,
                                               get_ticklabelinterpreter ());

              wmax = std::max (wmax, ext(0) / dpr);
              hmax = std::max (hmax, ext(1) / dpr);
            }
          else
            {
              // FIXME: find a better approximation
              double fsize = get ("fontsize").double_value ();
              int len = label.length ();

              wmax = std::max (wmax, 0.5*fsize*len);
              hmax = fsize;
            }
        }
    }

  ext(0) = wmax;
  ext(1) = hmax;
  return ext;
}

void
get_children_limits (double& min_val, double& max_val,
                     double& min_pos, double& max_neg,
                     const Matrix& kids, char limit_type)
{
  octave_idx_type n = kids.numel ();

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  switch (limit_type)
    {
    case 'x':
      for (octave_idx_type i = 0; i < n; i++)
        {
          graphics_object go = gh_mgr.get_object (kids(i));

          if (go.is_xliminclude ())
            {
              octave_value lim = go.get_xlim ();

              check_limit_vals (min_val, max_val, min_pos, max_neg, lim);
            }
        }
      break;

    case 'y':
      for (octave_idx_type i = 0; i < n; i++)
        {
          graphics_object go = gh_mgr.get_object (kids(i));

          if (go.is_yliminclude ())
            {
              octave_value lim = go.get_ylim ();

              check_limit_vals (min_val, max_val, min_pos, max_neg, lim);
            }
        }
      break;

    case 'z':
      for (octave_idx_type i = 0; i < n; i++)
        {
          graphics_object go = gh_mgr.get_object (kids(i));

          if (go.is_zliminclude ())
            {
              octave_value lim = go.get_zlim ();

              check_limit_vals (min_val, max_val, min_pos, max_neg, lim);
            }
        }
      break;

    case 'c':
      for (octave_idx_type i = 0; i < n; i++)
        {
          graphics_object go = gh_mgr.get_object (kids(i));

          if (go.is_climinclude ())
            {
              octave_value lim = go.get_clim ();

              check_limit_vals (min_val, max_val, min_pos, max_neg, lim);
            }
        }
      break;

    case 'a':
      for (octave_idx_type i = 0; i < n; i++)
        {
          graphics_object go = gh_mgr.get_object (kids(i));

          if (go.is_aliminclude ())
            {
              octave_value lim = go.get_alim ();

              check_limit_vals (min_val, max_val, min_pos, max_neg, lim);
            }
        }
      break;

    default:
      break;
    }
}

static std::set<double> updating_axis_limits;

void
axes::update_axis_limits (const std::string& axis_type,
                          const graphics_handle& h)
{
  if (updating_axis_limits.find (get_handle ().value ())
      != updating_axis_limits.end ())
    return;

  Matrix kids = Matrix (1, 1, h.value ());

  double min_val = octave::numeric_limits<double>::Inf ();
  double max_val = -octave::numeric_limits<double>::Inf ();
  double min_pos = octave::numeric_limits<double>::Inf ();
  double max_neg = -octave::numeric_limits<double>::Inf ();

  char update_type = 0;

  Matrix limits (1, 2);
  double val;

#define FIX_LIMITS                          \
  val = limits(0);                          \
  if (octave::math::isfinite (val))         \
    min_val = val;                          \
  val = limits(1);                          \
  if (octave::math::isfinite (val))         \
    max_val = val;

  if (axis_type == "xdata" || axis_type == "xscale"
      || axis_type == "xlimmode" || axis_type == "xliminclude"
      || axis_type == "xlim")
    {
      limits = m_properties.get_xlim ().matrix_value ();
      FIX_LIMITS;

      update_type = 'x';
      if (m_properties.xlimmode_is ("auto"))
        {
          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'x');

          std::string method = m_properties.get_xlimitmethod ();
          limits = m_properties.get_axis_limits (min_val, max_val,
                                                 min_pos, max_neg,
                                                 m_properties.xscale_is ("log"),
                                                 method);
        }
      else
        m_properties.check_axis_limits (limits, kids,
                                        m_properties.xscale_is ("log"),
                                        update_type);
    }
  else if (axis_type == "ydata" || axis_type == "yscale"
           || axis_type == "ylimmode" || axis_type == "yliminclude"
           || axis_type == "ylim")
    {
      limits = m_properties.get_ylim ().matrix_value ();
      FIX_LIMITS;

      update_type = 'y';
      if (m_properties.ylimmode_is ("auto"))
        {
          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'y');

          std::string method = m_properties.get_ylimitmethod ();
          limits = m_properties.get_axis_limits (min_val, max_val,
                                                 min_pos, max_neg,
                                                 m_properties.yscale_is ("log"),
                                                 method);
        }
      else
        m_properties.check_axis_limits (limits, kids,
                                        m_properties.yscale_is ("log"),
                                        update_type);
    }
  else if (axis_type == "zdata" || axis_type == "zscale"
           || axis_type == "zlimmode" || axis_type == "zliminclude"
           || axis_type == "zlim")
    {
      limits = m_properties.get_zlim ().matrix_value ();
      FIX_LIMITS;

      update_type = 'z';
      if (m_properties.zlimmode_is ("auto"))
        {
          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'z');

          m_properties.set_has3Dkids ((max_val - min_val) >
                                      std::numeric_limits<double>::epsilon ());

          std::string method = m_properties.get_zlimitmethod ();
          limits = m_properties.get_axis_limits (min_val, max_val,
                                                 min_pos, max_neg,
                                                 m_properties.zscale_is ("log"),
                                                 method);
        }
      else
        {
          // FIXME: get_children_limits is only needed here in order to know
          // if there are 3D children.  Is there a way to avoid this call?
          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'z');

          m_properties.set_has3Dkids ((max_val - min_val) >
                                      std::numeric_limits<double>::epsilon ());

          m_properties.check_axis_limits (limits, kids,
                                          m_properties.zscale_is ("log"),
                                          update_type);
        }
    }
  else if (axis_type == "cdata" || axis_type == "climmode"
           || axis_type == "cdatamapping" || axis_type == "climinclude"
           || axis_type == "clim")
    {
      if (m_properties.climmode_is ("auto"))
        {
          limits = m_properties.get_clim ().matrix_value ();
          FIX_LIMITS;

          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'c');

          if (min_val > max_val)
            {
              min_val = min_pos = 0;
              max_val = 1;
            }
          else if (min_val == max_val)
            {
              max_val = min_val + 1;
              min_val -= 1;
            }

          limits(0) = min_val;
          limits(1) = max_val;

          update_type = 'c';
        }
    }
  else if (axis_type == "alphadata" || axis_type == "alimmode"
           || axis_type == "alphadatamapping" || axis_type == "aliminclude"
           || axis_type == "alim")
    {
      if (m_properties.alimmode_is ("auto"))
        {
          limits = m_properties.get_alim ().matrix_value ();
          FIX_LIMITS;

          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'a');

          if (min_val > max_val)
            {
              min_val = min_pos = 0;
              max_val = 1;
            }
          else if (min_val == max_val)
            max_val = min_val + 1;

          limits(0) = min_val;
          limits(1) = max_val;

          update_type = 'a';
        }
    }

#undef FIX_LIMITS

  octave::unwind_protect_var<std::set<double>>
      restore_var (updating_axis_limits);

  updating_axis_limits.insert (get_handle ().value ());
  bool is_auto;

  switch (update_type)
    {
    case 'x':
      is_auto = m_properties.xlimmode_is ("auto");
      m_properties.set_xlim (limits);
      if (is_auto)
        m_properties.set_xlimmode ("auto");
      m_properties.update_xlim ();
      break;

    case 'y':
      is_auto = m_properties.ylimmode_is ("auto");
      m_properties.set_ylim (limits);
      if (is_auto)
        m_properties.set_ylimmode ("auto");
      m_properties.update_ylim ();
      break;

    case 'z':
      is_auto = m_properties.zlimmode_is ("auto");
      m_properties.set_zlim (limits);
      if (is_auto)
        m_properties.set_zlimmode ("auto");
      m_properties.update_zlim ();
      break;

    case 'c':
      m_properties.set_clim (limits);
      m_properties.set_climmode ("auto");
      break;

    case 'a':
      m_properties.set_alim (limits);
      m_properties.set_alimmode ("auto");
      break;

    default:
      break;
    }

  m_properties.update_transform ();
}

// FIXME: This function is called repeatedly while the axes are being set up.
// There is probably some way to make this more efficient.

void
axes::update_axis_limits (const std::string& axis_type)
{
  if ((updating_axis_limits.find (get_handle ().value ())
       != updating_axis_limits.end ())
      || (updating_aspectratios.find (get_handle ().value ())
          != updating_aspectratios.end ()))
    return;

  Matrix kids = m_properties.get_all_children ();

  double min_val = octave::numeric_limits<double>::Inf ();
  double max_val = -octave::numeric_limits<double>::Inf ();
  double min_pos = octave::numeric_limits<double>::Inf ();
  double max_neg = -octave::numeric_limits<double>::Inf ();

  char update_type = 0;

  Matrix limits;

  if (axis_type == "xdata" || axis_type == "xscale"
      || axis_type == "xlimmode" || axis_type == "xliminclude"
      || axis_type == "xlim")
    {
      update_type = 'x';
      if (m_properties.xlimmode_is ("auto"))
        {
          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'x');

          std::string method = m_properties.get_xlimitmethod ();
          limits = m_properties.get_axis_limits (min_val, max_val,
                                                 min_pos, max_neg,
                                                 m_properties.xscale_is ("log"),
                                                 method);
        }
      else
        {
          limits = m_properties.get_xlim ().matrix_value ();
          m_properties.check_axis_limits (limits, kids,
                                          m_properties.xscale_is ("log"),
                                          update_type);
          if (axis_type == "xscale")
            update_type = 'x';
        }
    }
  else if (axis_type == "ydata" || axis_type == "yscale"
           || axis_type == "ylimmode" || axis_type == "yliminclude"
           || axis_type == "ylim")
    {
      update_type = 'y';
      if (m_properties.ylimmode_is ("auto"))
        {
          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'y');

          std::string method = m_properties.get_ylimitmethod ();
          limits = m_properties.get_axis_limits (min_val, max_val,
                                                 min_pos, max_neg,
                                                 m_properties.yscale_is ("log"),
                                                 method);
        }
      else
        {
          limits = m_properties.get_ylim ().matrix_value ();
          m_properties.check_axis_limits (limits, kids,
                                          m_properties.yscale_is ("log"),
                                          update_type);
          if (axis_type == "yscale")
            update_type = 'y';
        }
    }
  else if (axis_type == "zdata" || axis_type == "zscale"
           || axis_type == "zlimmode" || axis_type == "zliminclude"
           || axis_type == "zlim")
    {
      update_type = 'z';
      if (m_properties.zlimmode_is ("auto"))
        {
          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'z');

          m_properties.set_has3Dkids ((max_val - min_val) >
                                      std::numeric_limits<double>::epsilon ());

          // FIXME: How to correctly handle (positive or negative) log scale?
          if ((! octave::math::isfinite (min_val)
               || ! octave::math::isfinite (max_val))
              && ! m_properties.zscale_is ("log"))
            min_val = max_val = 0.;

          std::string method = m_properties.get_zlimitmethod ();
          limits = m_properties.get_axis_limits (min_val, max_val,
                                                 min_pos, max_neg,
                                                 m_properties.zscale_is ("log"),
                                                 method);
        }
      else
        {
          // FIXME: get_children_limits is only needed here in order to know
          // if there are 3D children.  Is there a way to avoid this call?
          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'z');

          m_properties.set_has3Dkids ((max_val - min_val) >
                                      std::numeric_limits<double>::epsilon ());

          limits = m_properties.get_zlim ().matrix_value ();
          m_properties.check_axis_limits (limits, kids,
                                          m_properties.zscale_is ("log"),
                                          update_type);
          if (axis_type == "zscale")
            update_type = 'z';
        }
    }
  else if (axis_type == "cdata" || axis_type == "climmode"
           || axis_type == "cdatamapping" || axis_type == "climinclude"
           || axis_type == "clim")
    {
      if (m_properties.climmode_is ("auto"))
        {
          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'c');

          if (min_val > max_val)
            {
              min_val = min_pos = 0;
              max_val = 1;
            }
          else if (min_val == max_val)
            {
              max_val = min_val + 1;
              min_val -= 1;
            }

          limits.resize (1, 2);

          limits(0) = min_val;
          limits(1) = max_val;

          update_type = 'c';
        }

    }
  else if (axis_type == "alphadata" || axis_type == "alimmode"
           || axis_type == "alphadatamapping" || axis_type == "aliminclude"
           || axis_type == "alim")
    {
      if (m_properties.alimmode_is ("auto"))
        {
          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'a');

          if (min_val > max_val)
            {
              min_val = min_pos = 0;
              max_val = 1;
            }
          else if (min_val == max_val)
            max_val = min_val + 1;

          limits.resize (1, 2);

          limits(0) = min_val;
          limits(1) = max_val;

          update_type = 'a';
        }

    }

  octave::unwind_protect_var<std::set<double>>
      restore_var (updating_axis_limits);

  updating_axis_limits.insert (get_handle ().value ());
  bool is_auto;

  switch (update_type)
    {
    case 'x':
      is_auto = m_properties.xlimmode_is ("auto");
      m_properties.set_xlim (limits);
      if (is_auto)
        m_properties.set_xlimmode ("auto");
      m_properties.update_xlim ();
      break;

    case 'y':
      is_auto = m_properties.ylimmode_is ("auto");
      m_properties.set_ylim (limits);
      if (is_auto)
        m_properties.set_ylimmode ("auto");
      m_properties.update_ylim ();
      break;

    case 'z':
      is_auto = m_properties.zlimmode_is ("auto");
      m_properties.set_zlim (limits);
      if (is_auto)
        m_properties.set_zlimmode ("auto");
      m_properties.update_zlim ();
      break;

    case 'c':
      m_properties.set_clim (limits);
      m_properties.set_climmode ("auto");
      break;

    case 'a':
      m_properties.set_alim (limits);
      m_properties.set_alimmode ("auto");
      break;

    default:
      break;
    }

  m_properties.update_transform ();
}

inline
double force_in_range (double x, double lower, double upper)
{
  if (x < lower)
    return lower;
  else if (x > upper)
    return upper;
  else
    return x;
}

static Matrix
do_zoom (double val, double factor, const Matrix& lims, bool is_logscale)
{
  Matrix new_lims = lims;

  double lo = lims(0);
  double hi = lims(1);

  bool is_negative = lo < 0 && hi < 0;

  if (is_logscale)
    {
      if (is_negative)
        {
          double tmp = hi;
          hi = std::log10 (-lo);
          lo = std::log10 (-tmp);
          val = std::log10 (-val);
        }
      else
        {
          hi = std::log10 (hi);
          lo = std::log10 (lo);
          val = std::log10 (val);
        }
    }

  // Perform the zooming
  lo = val + (lo - val) / factor;
  hi = val + (hi - val) / factor;

  if (is_logscale)
    {
      if (is_negative)
        {
          double tmp = -std::pow (10.0, hi);
          hi = -std::pow (10.0, lo);
          lo = tmp;
        }
      else
        {
          lo = std::pow (10.0, lo);
          hi = std::pow (10.0, hi);
        }
    }

  new_lims(0) = lo;
  new_lims(1) = hi;

  return new_lims;
}

void
axes::properties::zoom_about_point (const std::string& mode,
                                    double x, double y, double factor,
                                    bool push_to_zoom_stack)
{
  // FIXME: Do we need error checking here?
  Matrix xlims = get_xlim ().matrix_value ();
  Matrix ylims = get_ylim ().matrix_value ();

  // Get children axes limits
  Matrix kids = get_children ();
  double minx = octave::numeric_limits<double>::Inf ();
  double maxx = -octave::numeric_limits<double>::Inf ();
  double min_pos_x = octave::numeric_limits<double>::Inf ();
  double max_neg_x = -octave::numeric_limits<double>::Inf ();
  get_children_limits (minx, maxx, min_pos_x, max_neg_x, kids, 'x');

  double miny = octave::numeric_limits<double>::Inf ();
  double maxy = -octave::numeric_limits<double>::Inf ();
  double min_pos_y = octave::numeric_limits<double>::Inf ();
  double max_neg_y = -octave::numeric_limits<double>::Inf ();
  get_children_limits (miny, maxy, min_pos_y, max_neg_y, kids, 'y');

  xlims = do_zoom (x, factor, xlims, xscale_is ("log"));
  ylims = do_zoom (y, factor, ylims, yscale_is ("log"));

  zoom (mode, xlims, ylims, push_to_zoom_stack);
}

void
axes::properties::zoom (const std::string& mode, double factor,
                        bool push_to_zoom_stack)
{
  // FIXME: Do we need error checking here?
  Matrix xlims = get_xlim ().matrix_value ();
  Matrix ylims = get_ylim ().matrix_value ();

  double x = (xlims(0) + xlims(1)) / 2;
  double y = (ylims(0) + ylims(1)) / 2;

  zoom_about_point (mode, x, y, factor, push_to_zoom_stack);
}

void
axes::properties::push_zoom_stack (void)
{
  if (m_zoom_stack.empty ())
    {
      m_zoom_stack.push_front (m_xlimmode.get ());
      m_zoom_stack.push_front (m_xlim.get ());
      m_zoom_stack.push_front (m_ylimmode.get ());
      m_zoom_stack.push_front (m_ylim.get ());
      m_zoom_stack.push_front (m_zlimmode.get ());
      m_zoom_stack.push_front (m_zlim.get ());
      m_zoom_stack.push_front (m_view.get ());
    }
}

void
axes::properties::zoom (const std::string& mode,
                        const Matrix& xl, const Matrix& yl,
                        bool push_to_zoom_stack)
{
  if (xl(0) == xl(1) || yl(0) == yl(1))
    {
      warning ("invalid zoom region");
      return;
    }

  if (push_to_zoom_stack)
    push_zoom_stack ();

  if (mode == "horizontal" || mode == "both")
    {
      m_xlim = xl;
      m_xlimmode = "manual";
    }

  if (mode == "vertical" || mode == "both")
    {
      m_ylim = yl;
      m_ylimmode = "manual";
    }

  update_transform ();

  if (mode == "horizontal" || mode == "both")
    update_xlim ();

  if (mode == "vertical" || mode == "both")
    update_ylim ();
}

static Matrix
do_translate (double x0, double x1, const Matrix& lims, bool is_logscale)
{
  Matrix new_lims = lims;

  double lo = lims(0);
  double hi = lims(1);

  bool is_negative = lo < 0 && hi < 0;

  double delta;

  if (is_logscale)
    {
      if (is_negative)
        {
          double tmp = hi;
          hi = std::log10 (-lo);
          lo = std::log10 (-tmp);
          x0 = -x0;
          x1 = -x1;
        }
      else
        {
          hi = std::log10 (hi);
          lo = std::log10 (lo);
        }

      delta = std::log10 (x0) - std::log10 (x1);
    }
  else
    {
      delta = x0 - x1;
    }

  // Perform the translation
  lo += delta;
  hi += delta;

  if (is_logscale)
    {
      if (is_negative)
        {
          double tmp = -std::pow (10.0, hi);
          hi = -std::pow (10.0, lo);
          lo = tmp;
        }
      else
        {
          lo = std::pow (10.0, lo);
          hi = std::pow (10.0, hi);
        }
    }

  new_lims(0) = lo;
  new_lims(1) = hi;

  return new_lims;
}

void
axes::properties::translate_view (const std::string& mode,
                                  double x0, double x1, double y0, double y1,
                                  bool push_to_zoom_stack)
{
  // FIXME: Do we need error checking here?
  Matrix xlims = get_xlim ().matrix_value ();
  Matrix ylims = get_ylim ().matrix_value ();

  // Get children axes limits
  Matrix kids = get_children ();
  double minx = octave::numeric_limits<double>::Inf ();
  double maxx = -octave::numeric_limits<double>::Inf ();
  double min_pos_x = octave::numeric_limits<double>::Inf ();
  double max_neg_x = -octave::numeric_limits<double>::Inf ();
  get_children_limits (minx, maxx, min_pos_x, max_neg_x, kids, 'x');

  double miny = octave::numeric_limits<double>::Inf ();
  double maxy = -octave::numeric_limits<double>::Inf ();
  double min_pos_y = octave::numeric_limits<double>::Inf ();
  double max_neg_y = -octave::numeric_limits<double>::Inf ();
  get_children_limits (miny, maxy, min_pos_y, max_neg_y, kids, 'y');

  xlims = do_translate (x0, x1, xlims, xscale_is ("log"));
  ylims = do_translate (y0, y1, ylims, yscale_is ("log"));

  zoom (mode, xlims, ylims, push_to_zoom_stack);
}

void
axes::properties::pan (const std::string& mode, double factor,
                       bool push_to_zoom_stack)
{
  // FIXME: Do we need error checking here?
  Matrix xlims = get_xlim ().matrix_value ();
  Matrix ylims = get_ylim ().matrix_value ();

  double x0 = (xlims(0) + xlims(1)) / 2;
  double y0 = (ylims(0) + ylims(1)) / 2;

  double x1 = x0 + (xlims(1) - xlims(0)) * factor;
  double y1 = y0 + (ylims(1) - ylims(0)) * factor;

  translate_view (mode, x0, x1, y0, y1, push_to_zoom_stack);
}

void
axes::properties::rotate3d (double x0, double x1, double y0, double y1,
                            bool push_to_zoom_stack)
{
  if (push_to_zoom_stack)
    push_zoom_stack ();

  Matrix bb = get_boundingbox (true);
  Matrix new_view = get_view ().matrix_value ();

  // Compute new view angles
  new_view(0) += ((x0 - x1) * (180.0 / bb(2)));
  new_view(1) += ((y1 - y0) * (180.0 / bb(3)));

  // Clipping
  new_view(1) = std::min (new_view(1), 90.0);
  new_view(1) = std::max (new_view(1), -90.0);
  if (new_view(0) > 180.0)
    new_view(0) -= 360.0;
  else if (new_view(0) < -180.0)
    new_view(0) += 360.0;

  // Snapping
  double snapmargin = 1.0;
  for (int a = -90; a <= 90; a += 90)
    {
      if ((a - snapmargin) < new_view(1) && new_view(1) < (a + snapmargin))
        {
          new_view(1) = a;
          break;
        }
    }

  for (int a = -180; a <= 180; a += 180)
    if ((a - snapmargin) < new_view(0) && new_view(0) < (a + snapmargin))
      {
        if (a == 180)
          new_view(0) = -180;
        else
          new_view(0) = a;
        break;
      }

  // Update axes properties
  set_view (new_view);
}

void
axes::properties::rotate_view (double delta_el, double delta_az,
                               bool push_to_zoom_stack)
{
  if (push_to_zoom_stack)
    push_zoom_stack ();

  Matrix v = get_view ().matrix_value ();

  v(1) += delta_el;

  if (v(1) > 90)
    v(1) = 90;
  if (v(1) < -90)
    v(1) = -90;

  v(0) = fmod (v(0) - delta_az + 720, 360);

  set_view (v);

  update_transform ();
}

void
axes::properties::unzoom (void)
{
  if (m_zoom_stack.size () >= 7)
    {
      m_view = m_zoom_stack.front ();
      m_zoom_stack.pop_front ();

      m_zlim = m_zoom_stack.front ();
      m_zoom_stack.pop_front ();

      m_zlimmode = m_zoom_stack.front ();
      m_zoom_stack.pop_front ();

      m_ylim = m_zoom_stack.front ();
      m_zoom_stack.pop_front ();

      m_ylimmode = m_zoom_stack.front ();
      m_zoom_stack.pop_front ();

      m_xlim = m_zoom_stack.front ();
      m_zoom_stack.pop_front ();

      m_xlimmode = m_zoom_stack.front ();
      m_zoom_stack.pop_front ();

      update_transform ();

      update_xlim ();
      update_ylim ();
      update_zlim ();

      update_view ();
    }
}

void
axes::properties::update_handlevisibility (void)
{
  if (! is_handle_visible ())
    {
      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      graphics_object go (gh_mgr.get_object (get___myhandle__ ()));

      graphics_object fig (go.get_ancestor ("figure"));
      octave_value ca = fig.get ("currentaxes");
      if (! ca.isempty () && ca.double_value () == m___myhandle__)
        {
          octave::autolock guard (gh_mgr.graphics_lock ());

          octave_value kids =  fig.get ("children");
          if (kids.isempty ())
            fig.set ("currentaxes", Matrix ());
          else
            {
              NDArray kidsarray = kids.array_value ();
              fig.set ("currentaxes", kidsarray(0));
            }
        }
    }

  base_properties::update_handlevisibility ();
}

void
figure::properties::init_toolkit (void)
{
  octave::gtk_manager& gtk_mgr = octave::__get_gtk_manager__ ();

  m_toolkit = gtk_mgr.get_toolkit ();
}

void
axes::properties::clear_zoom_stack (bool do_unzoom)
{
  std::size_t items_to_leave_on_stack = (do_unzoom ? 7 : 0);

  while (m_zoom_stack.size () > items_to_leave_on_stack)
    m_zoom_stack.pop_front ();

  if (do_unzoom)
    unzoom ();
}

void
axes::properties::trigger_normals_calc (void)
{
  // Find all patch (and surface) objects within axes
  std::list<graphics_object> children_list;
  std::list<graphics_object>::iterator children_list_iter;
  get_children_of_type ("patch", false, true, children_list);
  get_children_of_type ("surface", false, true, children_list);

  // trigger normals calculation for these objects
  for (children_list_iter = children_list.begin ();
       children_list_iter != children_list.end (); children_list_iter++)
    {
      graphics_object kid = *children_list_iter;
      if (kid.isa ("patch"))
        {
          patch::properties& patch_props
            = dynamic_cast<patch::properties&> (kid.get_properties ());
          patch_props.update_normals (false);
        }
      else
        {
          surface::properties& surface_props
            = dynamic_cast<surface::properties&> (kid.get_properties ());
          surface_props.update_normals (false);
        }
    }
}

void
axes::reset_default_properties (void)
{
  // empty list of local defaults
  m_default_properties = property_list ();

  // Save warning state of "Octave:deprecated-property"
  int state = toggle_warn ("Octave:deprecated-property", false);

  // reset factory defaults
  remove_all_listeners ();
  set_defaults ("reset");

  toggle_warn ("Octave:deprecated-property", true, state);
}

void
axes::initialize (const graphics_object& go)
{
  base_graphics_object::initialize (go);

  xinitialize (m_properties.get_title ());
  xinitialize (m_properties.get_xlabel ());
  xinitialize (m_properties.get_ylabel ());
  xinitialize (m_properties.get_zlabel ());

  m_properties.sync_positions ();
}

// ---------------------------------------------------------------------

Matrix
line::properties::compute_xlim (void) const
{
  Matrix m (1, 4);

  m(0) = m_xdata.min_val ();
  m(1) = m_xdata.max_val ();
  m(2) = m_xdata.min_pos ();
  m(3) = m_xdata.max_neg ();

  return m;
}

Matrix
line::properties::compute_ylim (void) const
{
  Matrix m (1, 4);

  m(0) = m_ydata.min_val ();
  m(1) = m_ydata.max_val ();
  m(2) = m_ydata.min_pos ();
  m(3) = m_ydata.max_neg ();

  return m;
}

// ---------------------------------------------------------------------

Matrix
text::properties::get_data_position (void) const
{
  Matrix pos = get_position ().matrix_value ();

  if (! units_is ("data"))
    pos = convert_text_position (pos, *this, get_units (), "data");

  return pos;
}

Matrix
text::properties::get_extent_matrix (bool rotated) const
{
  // FIXME: Should this function also add the (x,y) base position?
  Matrix ext = m_extent.get ().matrix_value ();

  if (rotated && get_rotation () != 0)
    {
      double rot = get_rotation () * 4.0 * atan (1.0) / 180;
      double x0 = ext(0) * cos (rot) - ext(1) * sin (rot);
      double x1 = x0;
      double y0 = ext(0) * sin (rot) + ext(1) * cos (rot);
      double y1 = y0;

      double tmp = (ext(0)+ext(2)) * cos (rot) - ext(1) * sin (rot);
      x0 = std::min (x0, tmp);
      x1 = std::max (x1, tmp);
      tmp = (ext(0)+ext(2)) * sin (rot) + ext(1) * cos (rot);
      y0 = std::min (y0, tmp);
      y1 = std::max (y1, tmp);

      tmp = (ext(0)+ext(2)) * cos (rot) - (ext(1)+ext(3)) * sin (rot);
      x0 = std::min (x0, tmp);
      x1 = std::max (x1, tmp);
      tmp = (ext(0)+ext(2)) * sin (rot) + (ext(1)+ext(3)) * cos (rot);
      y0 = std::min (y0, tmp);
      y1 = std::max (y1, tmp);

      tmp = ext(0) * cos (rot) - (ext(1)+ext(3)) * sin (rot);
      x0 = std::min (x0, tmp);
      x1 = std::max (x1, tmp);
      tmp = ext(0) * sin (rot) + (ext(1)+ext(3)) * cos (rot);
      y0 = std::min (y0, tmp);
      y1 = std::max (y1, tmp);

      ext(0) = x0;
      ext(1) = y0;
      ext(2) = x1 - x0;
      ext(3) = y1 - y0;
    }

  return ext;
}

octave_value
text::properties::get_extent (void) const
{
  // FIXME: This doesn't work right for 3D plots.
  // (It doesn't in Matlab either, at least not in version 6.5.)
  Matrix m = get_extent_matrix (true);
  Matrix pos = get_position ().matrix_value ();
  Matrix p = convert_text_position (pos, *this, get_units (), "pixels");

  m(0) += p(0);
  m(1) += p(1);

  Matrix bbox = convert_text_position (m, *this, "pixels", get_units ());

  double dpr = device_pixel_ratio (get___myhandle__ ());

  for (octave_idx_type ii = 0; ii < bbox.numel (); ii++)
    bbox(ii) = bbox(ii) / dpr;

  return bbox;
}

void
text::properties::set_fontunits (const octave_value& val)
{
  caseless_str old_fontunits = get_fontunits ();

  if (m_fontunits.set (val, true))
    {
      update_fontunits (old_fontunits);
      mark_modified ();
    }
}

void
text::properties::update_fontunits (const caseless_str& old_units)
{
  caseless_str new_units = get_fontunits ();
  double parent_height = 0;
  double fontsz = get_fontsize ();

  if (new_units == "normalized" || old_units == "normalized")
    {
      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      graphics_object go (gh_mgr.get_object (get___myhandle__ ()));

      graphics_object ax (go.get_ancestor ("axes"));

      parent_height = ax.get_properties ().get_boundingbox (true).elem (3);
    }

  fontsz = convert_font_size (fontsz, old_units, new_units, parent_height);

  set_fontsize (octave_value (fontsz));
}

void
text::properties::update_font (void)
{
  double dpr = device_pixel_ratio (get___myhandle__ ());

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  m_txt_renderer.set_font (get ("fontname").string_value (),
                           get ("fontweight").string_value (),
                           get ("fontangle").string_value (),
                           get ("__fontsize_points__").double_value () * dpr);

  m_txt_renderer.set_anti_aliasing (is_fontsmoothing ());

  Matrix c = get_color_rgb ();
  if (! c.isempty ())
    m_txt_renderer.set_color (c);

}

void
text::properties::update_text_extent (void)
{
  int halign = 0;
  int valign = 0;

  if (horizontalalignment_is ("center"))
    halign = 1;
  else if (horizontalalignment_is ("right"))
    halign = 2;

  if (verticalalignment_is ("middle"))
    valign = 1;
  else if (verticalalignment_is ("top"))
    valign = 2;
  else if (verticalalignment_is ("baseline"))
    valign = 3;
  else if (verticalalignment_is ("cap"))
    valign = 4;

  Matrix bbox;

  // FIXME: string should be parsed only when modified, for efficiency

  octave_value string_prop = get_string ();

  string_vector sv = string_prop.string_vector_value ();

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  m_txt_renderer.text_to_pixels (sv.join ("\n"), m_pixels, bbox,
                                 halign, valign, 0.0, get_interpreter ());
  // The bbox is relative to the text's position.  We'll leave it that
  // way, because get_position does not return valid results when the
  // text is first constructed.  Conversion to proper coordinates is
  // performed in get_extent.
  set_extent (bbox);

  if (__autopos_tag___is ("xlabel") || __autopos_tag___is ("ylabel")
      || __autopos_tag___is ("zlabel") || __autopos_tag___is ("title"))
    update_autopos ("sync");
}

void
text::properties::request_autopos (void)
{
  if (__autopos_tag___is ("xlabel") || __autopos_tag___is ("ylabel")
      || __autopos_tag___is ("zlabel") || __autopos_tag___is ("title"))
    update_autopos (get___autopos_tag__ ());
}

void
text::properties::update_units (void)
{
  if (! units_is ("data"))
    {
      set_xliminclude ("off");
      set_yliminclude ("off");
      set_zliminclude ("off");
    }

  Matrix pos = get_position ().matrix_value ();

  pos = convert_text_position (pos, *this, m_cached_units, get_units ());

  // FIXME: if the current axes view is 2D, then one should probably drop
  // the z-component of "pos" and leave "zliminclude" to "off".

  bool autopos = positionmode_is ("auto");

  set_position (pos);

  if (autopos)
    set_positionmode ("auto");

  if (units_is ("data"))
    {
      set_xliminclude ("on");
      set_yliminclude ("on");
      // FIXME: see above
      set_zliminclude ("off");
    }

  m_cached_units = get_units ();
}

double
text::properties::get___fontsize_points__ (double box_pix_height) const
{
  double fontsz = get_fontsize ();
  double parent_height = box_pix_height;

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go (gh_mgr.get_object (get___myhandle__ ()));

  if (fontunits_is ("normalized") && parent_height <= 0)
    {
      graphics_object ax (go.get_ancestor ("axes"));

      parent_height = ax.get_properties ().get_boundingbox (true).elem (3);
    }

  return convert_font_size (fontsz, get_fontunits (), "points", parent_height);
}

// ---------------------------------------------------------------------

octave_value
image::properties::get_color_data (void) const
{
  return convert_cdata (*this, get_cdata (), cdatamapping_is ("scaled"), 3);
}

// ---------------------------------------------------------------------

void
light::initialize (const graphics_object& go)
{
  base_graphics_object::initialize (go);

  // trigger normals calculation for the respective children of this axes object
  axes::properties& parent_axes_prop
    = dynamic_cast<axes::properties&> (go.get_ancestor ("axes").get_properties ());
  parent_axes_prop.trigger_normals_calc ();
}

void
light::properties::update_visible (void)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (get___myhandle__ ());

  axes::properties& ax_props = dynamic_cast<axes::properties&>
                               (go.get_ancestor ("axes").get_properties ());
  if (is_visible ())
    ax_props.increase_num_lights ();
  else
    ax_props.decrease_num_lights ();
}

// ---------------------------------------------------------------------

bool
patch::properties::get_do_lighting (void) const
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (get___myhandle__ ());

  axes::properties& ax_props = dynamic_cast<axes::properties&>
                               (go.get_ancestor ("axes").get_properties ());

  return (ax_props.get_num_lights () > 0);
}

octave_value
patch::properties::get_color_data (void) const
{
  octave_value fvc = get_facevertexcdata ();
  if (fvc.is_undefined () || fvc.isempty ())
    return Matrix ();
  else
    return convert_cdata (*this, fvc, cdatamapping_is ("scaled"), 2);
}

static bool updating_patch_data = false;

void
patch::properties::update_fvc (void)
{
  if (updating_patch_data)
    return;

  Matrix xd = get_xdata ().matrix_value ();
  Matrix yd = get_ydata ().matrix_value ();
  Matrix zd = get_zdata ().matrix_value ();
  NDArray cd = get_cdata ().array_value ();

  m_bad_data_msg = "";
  if (xd.dims () != yd.dims ()
      || (xd.dims () != zd.dims () && ! zd.isempty ()))
    {
      m_bad_data_msg = "x/y/zdata must have the same dimensions";
      return;
    }

  // Faces and Vertices
  dim_vector dv;
  bool is3D = false;
  octave_idx_type nr = xd.rows ();
  octave_idx_type nc = xd.columns ();
  if (nr == 1 && nc > 1)
    {
      nr = nc;
      nc = 1;
      xd = xd.as_column ();
      yd = yd.as_column ();
      zd = zd.as_column ();
    }

  dv(0) = nr * nc;
  if (zd.isempty ())
    dv(1) = 2;
  else
    {
      dv(1) = 3;
      is3D = true;
    }

  Matrix vert (dv);
  Matrix idx (nc, nr);

  octave_idx_type kk = 0;
  for (octave_idx_type jj = 0; jj < nc; jj++)
    {
      for (octave_idx_type ii = 0; ii < nr; ii++)
        {
          vert(kk, 0) = xd(ii, jj);
          vert(kk, 1) = yd(ii, jj);
          if (is3D)
            vert(kk, 2) = zd(ii, jj);

          idx(jj, ii) = static_cast<double> (kk+1);

          kk++;
        }
    }

  // facevertexcdata
  Matrix fvc;
  if (cd.ndims () == 3)
    {
      dv(0) = cd.rows () * cd.columns ();
      dv(1) = cd.dims ()(2);
      fvc = cd.reshape (dv);
    }
  else
    fvc = cd.as_column ();

  // FIXME: shouldn't we update facevertexalphadata here ?

  octave::unwind_protect_var<bool> restore_var (updating_patch_data, true);

  m_faces.set (idx);
  m_vertices.set (vert);
  m_facevertexcdata.set (fvc);
}

// core coplanar tester
bool is_coplanar (const Matrix& cov)
{
  // Accuracy note: this test will also accept single precision input (although
  // stored in double precision).  This is because the error threshold is
  // sqrt(tol) = 1.5e-7.
  double tol = 100 * std::numeric_limits<double>::epsilon ();
  EIG eig (cov, false, false, true);
  ColumnVector ev = real (eig.eigenvalues ());
  return ev.min () <= tol * ev.max ();
}

std::vector<octave_idx_type>
coplanar_partition (const Matrix& vert, const Matrix& idx,
                    octave_idx_type nc, octave_idx_type jj)
{
  std::vector<octave_idx_type> coplanar_ends;

  Matrix plane_pivot = Matrix (1, 3, 0.0);
  for (octave_idx_type i = 0; i < 3; i++)
    plane_pivot(0, i) = vert(idx(0, jj)-1, i);

  Matrix fc = Matrix (0, 3, 0.0);  // face corner vertex coordinates
  Matrix fa = Matrix (1, 3, 0.0);  // for append face corner
  Matrix coor_cov = Matrix (3, 3, 0.0);

  if (nc >= 5)
    {
      // Coplanar test that involves all points.
      // For nc == 4, this initial test is not beneficial at all.
      // If the probability of coplanar input is more than half, for
      // the best average performance, we should use nc >= 5.
      // Higher threshold is meaningful only when input is known to be
      // non-coplanar and nc is small.

      fc.resize (nc - 1, 3);
      for (octave_idx_type j = 1; j < nc; j++)
        for (octave_idx_type i = 0; i < 3; i++)
          fc(j-1, i) = vert(idx(j, jj)-1, i) - plane_pivot(i);

      coor_cov = fc.transpose () * fc;
      if (is_coplanar (coor_cov))
        {
          coplanar_ends.push_back (nc - 1);
          return coplanar_ends;
        }
    }

  fc.resize (3, 3);
  octave_idx_type i_start = 1;
  octave_idx_type i_end = 2;

  // Split the polygon into coplanar segments.
  // The first point is common corner of all planes.
  while (i_start < nc - 1)
    {
      i_end = i_start + 2;
      if (i_end > nc - 1)
        {
          coplanar_ends.push_back (nc - 1);
          break;
        }

      // Algorithm: Start from 3 points, keep adding points until the point set
      // is no more in a plane.  Record the coplanar point set, then advance
      // i_start.

      // Prepare 1+3 points for coplanar test.
      // The first point is implicitly included.
      for (octave_idx_type j = 0; j < 3; j++)
        for (octave_idx_type i = 0; i < 3; i++)
          fc(j, i) = vert(idx(j+i_start, jj)-1, i) - plane_pivot(i);

      // covariance matrix between coordinates of vertices
      coor_cov = fc.transpose () * fc;

      while (true)
        {
          // coplanar test
          if (! is_coplanar (coor_cov))
            break;

          i_end++;
          if (i_end > nc - 1)
            break;

          // add a point to plane
          for (octave_idx_type i = 0; i < 3; i++)
            fa(0, i) = vert(idx(i_end, jj)-1, i) - plane_pivot(i);
          coor_cov += fa.transpose () * fa;
        }

      i_start = i_end - 1;
      coplanar_ends.push_back (i_start);
    }
  return coplanar_ends;
}

void
patch::properties::update_data (void)
{
  if (updating_patch_data)
    return;

  Matrix idx = get_faces ().matrix_value ().transpose ();
  Matrix vert = get_vertices ().matrix_value ();
  NDArray fvc = get_facevertexcdata ().array_value ();

  octave_idx_type nfaces = idx.columns ();
  octave_idx_type nvert = vert.rows ();

  // Check all vertices in faces are defined
  m_bad_data_msg = "";
  if (static_cast<double> (nvert) < idx.row_max ().max ())
    {
      m_bad_data_msg = R"(some vertices in "faces" property are undefined)";
      return;
    }

  // Replace NaNs
  if (idx.any_element_is_inf_or_nan ())
    {
      for (octave_idx_type jj = 0; jj < idx.columns (); jj++)
        {
          double valid_vert = idx(0, jj);
          bool turn_valid = false;
          for (octave_idx_type ii = 0; ii < idx.rows (); ii++)
            {
              if (octave::math::isnan (idx(ii, jj)) || turn_valid)
                {
                  idx(ii, jj) = valid_vert;
                  turn_valid = true;
                }
              else
                valid_vert = idx(ii, jj);
            }
        }
    }

  // check coplanarity for 3D-faces with more than 3 corners
  int fcmax = idx.rows ();
  if (fcmax > 3 && vert.columns () > 2
      && ! (facecolor_is ("none") && edgecolor_is ("none")))
    {
      m_coplanar_last_idx.resize (idx.columns ());
      for (octave_idx_type jj = 0; jj < idx.columns (); jj++)
        {
          if (octave::math::isnan (idx(3, jj)))
            continue;

          // find first element that is NaN to get number of corners
          octave_idx_type nc = 3;
          while (nc < fcmax && ! octave::math::isnan (idx(nc, jj)))
            nc++;

          // If any of the corners is NaN or Inf, skip coplanar test.
          // FIXME: Add support for non-coplanar faces with unclosed contour.
          bool is_unclosed = false;
          for (octave_idx_type j = 0; j < nc; j++)
            {
              const octave_idx_type k = idx(j, jj) - 1;
              if (! (octave::math::isfinite (vert(k, 0))
                     && octave::math::isfinite (vert(k, 1))
                     && octave::math::isfinite (vert(k, 2))))
                {
                  is_unclosed = true;
                  break;
                }
            }
          if (is_unclosed)
            continue;

          m_coplanar_last_idx[jj] = coplanar_partition (vert, idx, nc, jj);
        }
    }
  else
    m_coplanar_last_idx.resize (0);

  // Build cdata
  dim_vector dv = dim_vector::alloc (3);
  NDArray cd;
  bool pervertex = false;

  if (fvc.rows () == nfaces || fvc.rows () == 1)
    {
      dv(0) = 1;
      dv(1) = fvc.rows ();
      dv(2) = fvc.columns ();
      cd = fvc.reshape (dv);
    }
  else
    {
      if (! fvc.isempty ())
        {
          dv(0) = idx.rows ();
          dv(1) = nfaces;
          dv(2) = fvc.columns ();
          cd.resize (dv);
          pervertex = true;
        }
    }

  // Build x,y,zdata and eventually per vertex cdata
  Matrix xd (idx.dims ());
  Matrix yd (idx.dims ());
  Matrix zd;
  bool has_zd = false;
  if (vert.columns () > 2)
    {
      zd = Matrix (idx.dims ());
      has_zd = true;
    }

  for (octave_idx_type jj = 0; jj < nfaces; jj++)
    {
      for (octave_idx_type ii = 0; ii < idx.rows (); ii++)
        {
          octave_idx_type row = static_cast<octave_idx_type> (idx(ii, jj)-1);
          xd(ii, jj) = vert(row, 0);
          yd(ii, jj) = vert(row, 1);

          if (has_zd)
            zd(ii, jj) = vert(row, 2);

          if (pervertex)
            for (int kk = 0; kk < fvc.columns (); kk++)
              cd(ii, jj, kk) = fvc(row, kk);
        }
    }

  // Update normals
  update_normals (true);

  octave::unwind_protect_var<bool> restore_var (updating_patch_data, true);

  set_xdata (xd);
  set_ydata (yd);
  set_zdata (zd);
  set_cdata (cd);
}

inline void
cross_product (double x1, double y1, double z1,
               double x2, double y2, double z2,
               double& x, double& y, double& z)
{
  x += (y1 * z2 - z1 * y2);
  y += (z1 * x2 - x1 * z2);
  z += (x1 * y2 - y1 * x2);
}

void
patch::properties::calc_face_normals (Matrix& fn)
{
  Matrix v = get_vertices ().matrix_value ();
  Matrix f = get_faces ().matrix_value ();

  bool is_3D = (v.columns () == 3);   // 2D or 3D patches
  octave_idx_type num_f = f.rows ();  // number of faces
  octave_idx_type max_nc = f.columns ();  // max. number of polygon corners

  // In which cases can we skip updating the normals?
  if (max_nc < 3)
    {
      fn = Matrix ();
      return;
    }

  // Calculate normals for all faces
  octave_idx_type i1, i2, i3;
  octave_idx_type j1, j2;
  for (octave_idx_type i = 0; i < num_f; i++)
    {
      bool is_coplanar = true;
      if (m_coplanar_last_idx.size () > 0 && m_coplanar_last_idx[i].size () > 1)
        is_coplanar = false;

      // get number of corners
      octave_idx_type nc = 3;
      if (max_nc > 3)
        {
          while (nc < max_nc && ! octave::math::isnan (f(i, nc)))
            nc++;
        }

      RowVector fnc (3, 0.0);
      double& nx = fnc(0);
      double& ny = fnc(1);
      double& nz = fnc(2);

      if (is_coplanar)
        {
          // fast way for coplanar polygons
          i1 = f(i, 0) - 1; i2 = f(i, 1) - 1; i3 = f(i, nc-1) - 1;

          if (is_3D)
            cross_product
            (v(i3, 0) - v(i1, 0), v(i3, 1) - v(i1, 1), v(i3, 2) - v(i1, 2),
             v(i2, 0) - v(i1, 0), v(i2, 1) - v(i1, 1), v(i2, 2) - v(i1, 2),
             nx, ny, nz);
          else
            {
              nz = (v(i2, 0) - v(i1, 0)) * (v(i3, 1) - v(i1, 1)) -
                   (v(i2, 1) - v(i1, 1)) * (v(i3, 0) - v(i1, 0));
              // 2-d vertices always point towards +z
              nz = (nz < 0) ? -nz : nz;
            }
        }
      else
        {
          // more general for non-planar polygons

          // calculate face normal with Newell's method
          // https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal#Newell.27s_Method

          j1 = nc - 1; j2 = 0;
          i1 = f(i, j1) - 1; i2 = f(i, j2) - 1;

          nx = (v(i2, 1) - v(i1, 1)) * (v(i1, 2) + v(i2, 2));
          ny = (v(i2, 2) - v(i1, 2)) * (v(i1, 0) + v(i2, 0));
          nz = (v(i2, 0) - v(i1, 0)) * (v(i1, 1) + v(i2, 1));

          for (octave_idx_type j = 1; j < nc; j++)
            {
              j1 = j-1; j2 = j;
              i1 = f(i, j1) - 1; i2 = f(i, j2) - 1;

              nx += (v(i2, 1) - v(i1, 1)) * (v(i1, 2) + v(i2, 2));
              ny += (v(i2, 2) - v(i1, 2)) * (v(i1, 0) + v(i2, 0));
              nz += (v(i2, 0) - v(i1, 0)) * (v(i1, 1) + v(i2, 1));
            }
        }

      // normalize normal vector
      double n_len = sqrt (nx*nx+ny*ny+nz*nz);

      // assign normal to current face
      if ( n_len < std::numeric_limits<double>::epsilon () )
        for (octave_idx_type j = 0; j < 3; j++)
          fn(i, j) = 0.0;
      else
        for (octave_idx_type j = 0; j < 3; j++)
          fn(i, j) = fnc(j) / n_len;
    }
}

void
patch::properties::update_face_normals (bool reset, bool force)
{
  if (updating_patch_data || ! facenormalsmode_is ("auto"))
    return;

  if (force || ((facelighting_is ("flat") || edgelighting_is ("flat"))
                && get_do_lighting ()))
    {
      Matrix f = get_faces ().matrix_value ();

      octave_idx_type num_f = f.rows ();  // number of faces
      Matrix fn (num_f, 3, 0.0);

      calc_face_normals (fn);
      m_facenormals = fn;
    }
  else if (reset)
    m_facenormals = Matrix ();
}

void
patch::properties::update_vertex_normals (bool reset, bool force)
{
  if (updating_patch_data || ! vertexnormalsmode_is ("auto"))
    return;

  if (force || ((facelighting_is ("gouraud") || facelighting_is ("phong")
                 || edgelighting_is ("gouraud") || edgelighting_is ("phong"))
                && get_do_lighting ()))
    {
      Matrix v = get_vertices ().matrix_value ();
      Matrix f = get_faces ().matrix_value ();

      octave_idx_type num_v = v.rows ();      // number of vertices
      octave_idx_type num_f = f.rows ();      // number of faces
      octave_idx_type max_nc = f.columns ();  // max. number of polygon corners

      // In which cases can we skip updating the normals?
      if (max_nc < 3)
        return;

      // First step: Calculate the normals for all faces
      Matrix fn = get_facenormals ().matrix_value ();
      if ( fn.isempty () )
        {
          // calculate facenormals here
          fn = Matrix (num_f, 3, 0.0);
          calc_face_normals (fn);
        }

      // Second step: assign normals to the respective vertices

      // The following code collects the face normals for all faces adjacent to
      // each vertex.  For this, a std::vector of length NUM_V (which might be
      // very large) is used so that memory is allocated from the heap rather
      // than the stack.  Each element of this vector corresponds to one vertex
      // of the patch.  The element itself is a variable length std::vector.
      // This second vector contains the face normals (of type RowVector) of
      // the adjacent faces.
      std::vector<std::vector<RowVector>> vec_vn (num_v);
      for (octave_idx_type i = 0; i < num_f; i++)
        {
          // get number of corners
          octave_idx_type nc = 3;
          if (max_nc > 3)
            {
              while (nc < max_nc && ! octave::math::isnan (f(i, nc)))
                nc++;
            }

          for (octave_idx_type j = 0; j < nc; j++)
            vec_vn[static_cast<octave_idx_type> (f(i, j) - 1)].push_back (fn.row (i));
        }

      // Third step: Calculate the normal for the vertices taking the average
      // of the normals determined from all adjacent faces
      Matrix vn (num_v, 3, 0.0);
      for (octave_idx_type i = 0; i < num_v; i++)
        {
          std::vector<RowVector>::iterator it = vec_vn[i].begin ();

          // The normal of unused vertices is NaN.
          RowVector vn0 (3, octave_NaN);

          if (it != vec_vn[i].end ())
            {
              // FIXME: Currently, the first vector also determines the
              // direction of the normal.  How to determine the inner and outer
              // faces of all parts of the patch and point the normals outwards?
              // (Necessary for correct lighting with "backfacelighting" set to
              // "lit" or "unlit".)  Matlab does not seem to do it correctly
              // either.  So should we bother?

              vn0 = *it;

              for (++it; it != vec_vn[i].end (); ++it)
                {
                  RowVector vn1 = *it;
                  // Use sign of dot product to point vectors in a similar
                  // direction before taking the average.
                  double dir = (vn0(0)*vn1(0) + vn0(1)*vn1(1) + vn0(2)*vn1(2) < 0) ? -1 : 1;
                  for (octave_idx_type j = 0; j < 3; j++)
                    vn0(j) += dir * vn1(j);
                }

              // normalize normal vector
              double n_len = sqrt (vn0(0)*vn0(0)+vn0(1)*vn0(1)+vn0(2)*vn0(2));

              // save normal in matrix
              for (octave_idx_type j = 0; j < 3; j++)
                vn(i, j) = vn0(j)/n_len;
            }
        }

      m_vertexnormals = vn;
    }
  else if (reset)
    m_vertexnormals = Matrix ();
}

void
patch::initialize (const graphics_object& go)
{
  base_graphics_object::initialize (go);

  // calculate normals for default data
  // This is done because the normals for the default data do not match
  // get(0, "DefaultPatchVertexNormals") in Matlab.
  m_properties.update_normals (true);
}


void
patch::reset_default_properties (void)
{
  // empty list of local defaults
  m_default_properties = property_list ();
  xreset_default_properties (get_handle (), m_properties.factory_defaults ());

  // calculate normals for default data
  // This is done because the normals for the default data do not match
  // get(0, "DefaultPatchVertexNormals") in Matlab.
  m_properties.update_normals (true);
}

// ---------------------------------------------------------------------

octave_value
scatter::properties::get_color_data (void) const
{
  octave_value c = get_cdata ();
  if (c.is_undefined () || c.isempty ())
    return Matrix ();
  else
    return convert_cdata (*this, c, c.columns () == 1, 2);
}

void
scatter::properties::update_data (void)
{
  Matrix xd = get_xdata ().matrix_value ();
  Matrix yd = get_ydata ().matrix_value ();
  Matrix zd = get_zdata ().matrix_value ();
  Matrix cd = get_cdata ().matrix_value ();
  Matrix sd = get_sizedata ().matrix_value ();

  m_bad_data_msg = "";
  if (xd.dims () != yd.dims ()
      || (xd.dims () != zd.dims () && ! zd.isempty ()))
    {
      m_bad_data_msg = "x/y/zdata must have the same dimensions";
      return;
    }

  octave_idx_type x_rows = xd.rows ();
  octave_idx_type c_cols = cd.columns ();
  octave_idx_type c_rows = cd.rows ();

  if (! cd.isempty () && (c_rows != 1 || c_cols != 3)
      && (c_rows != x_rows || (c_cols != 1 && c_cols != 3)))
    {
      m_bad_data_msg = "cdata must be an rgb triplet or have the same number "
                       "of rows as X and one or three columns";
      return;
    }

  octave_idx_type s_rows = sd.rows ();
  if (s_rows != 1 && s_rows != x_rows)
    {
      m_bad_data_msg = "sizedata must be a scalar or a vector with the same "
                       "dimensions as X";
      return;
    }
}

static bool updating_scatter_cdata = false;

void
scatter::properties::update_color (void)
{
  if (updating_scatter_cdata)
    return;

  Matrix series_idx = get_seriesindex ().matrix_value ();
  if (series_idx.isempty ())
    return;

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (get___myhandle__ ());

  axes::properties& parent_axes_prop
    = dynamic_cast<axes::properties&>
      (go.get_ancestor ("axes").get_properties ());

  Matrix color_order = parent_axes_prop.get_colororder ().matrix_value ();
  octave_idx_type s = (static_cast<octave_idx_type> (series_idx(0)) - 1)
                      % color_order.rows ();

  Matrix color = Matrix (1, 3, 0.);
  color(0) = color_order(s, 0);
  color(1) = color_order(s, 1);
  color(2) = color_order(s, 2);

  octave::unwind_protect_var<bool> restore_var (updating_scatter_cdata, true);

  set_cdata (color);
  set_cdatamode ("auto");
}

void
scatter::initialize (const graphics_object& go)
{
  base_graphics_object::initialize (go);

  Matrix series_idx = m_properties.get_seriesindex ().matrix_value ();
  if (series_idx.isempty ())
    {
      // Increment series index counter in parent axes
      axes::properties& parent_axes_prop
        = dynamic_cast<axes::properties&>
          (go.get_ancestor ("axes").get_properties ());

      if (! parent_axes_prop.nextplot_is ("add"))
        parent_axes_prop.set_nextseriesindex (1);

      series_idx.resize (1, 1);
      series_idx(0) = parent_axes_prop.get_nextseriesindex ();
      m_properties.set_seriesindex (series_idx);

      parent_axes_prop.set_nextseriesindex
      (parent_axes_prop.get_nextseriesindex () + 1);
    }

  if (m_properties.cdatamode_is ("auto"))
    m_properties.update_color ();
}

// ---------------------------------------------------------------------

octave_value
surface::properties::get_color_data (void) const
{
  return convert_cdata (*this, get_cdata (), cdatamapping_is ("scaled"), 3);
}

bool
surface::properties::get_do_lighting (void) const
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (get___myhandle__ ());

  axes::properties& ax_prop = dynamic_cast<axes::properties&>
                              (go.get_ancestor ("axes").get_properties ());

  return (ax_prop.get_num_lights () > 0);
}

void
surface::properties::update_face_normals (bool reset, bool force)
{
  if (! facenormalsmode_is ("auto"))
    return;

  if (force || ((facelighting_is ("flat") || edgelighting_is ("flat"))
                && get_do_lighting ()))
    {
      Matrix x = get_xdata ().matrix_value ();
      Matrix y = get_ydata ().matrix_value ();
      Matrix z = get_zdata ().matrix_value ();

      int p = z.columns ();
      int q = z.rows ();

      // FIXME: There might be a cleaner way to do this.  When data is changed
      // the update_xdata, update_ydata, update_zdata routines are called in a
      // serial fashion.  Until the final call to update_zdata the matrices
      // will be of mismatched dimensions which can cause an out-of-bound
      // indexing in the code below.  This one-liner prevents calculating
      // normals until dimensions match.
      if (x.columns () != p || y.rows () != q)
        return;

      bool x_mat = (x.rows () == q);
      bool y_mat = (y.columns () == p);

      NDArray n (dim_vector (q-1, p-1, 3), 1);

      int i1, i2, j1, j2;
      i1 = i2 = 0;
      j1 = j2 = 0;
      double x0, x1, x2, x3, y0, y1, y2, y3, z0, z1, z2, z3;
      double x1m0, x2m1, x3m2, x0m3, y1m0, y2m1, y3m2, y0m3;
      double x1p0, x2p1, x3p2, x0p3, y1p0, y2p1, y3p2, y0p3;
      x3m2 = y0m3 = -1;
      x2m1 = x0m3 = y1m0 = y3m2 = 0;
      x1m0 = y2m1 = 1;
      x0p3 = y1p0 = 0;
      x1p0 = x3p2 = y2p1 = y0p3 = 1;
      x2p1 = y3p2 = 2;

      for (int i = 0; i < p-1; i++)
        {
          i1 = i;
          i2 = i + 1;

          for (int j = 0; j < q-1; j++)
            {
              j1 = j;
              j2 = j + 1;

              if (x_mat || y_mat)
                {
                  x0 = x(x_mat?j1:0, y_mat?i1:0);
                  x1 = x(x_mat?j1:0, y_mat?i2:0);
                  x2 = x(x_mat?j2:0, y_mat?i2:0);
                  x3 = x(x_mat?j2:0, y_mat?i1:0);
                  x1m0 = x1 - x0;
                  x2m1 = x2 - x1;
                  x3m2 = x3 - x2;
                  x0m3 = x0 - x3;
                  x1p0 = x1 + x0;
                  x2p1 = x2 + x1;
                  x3p2 = x3 + x2;
                  x0p3 = x0 + x3;
                  y0 = y(x_mat?j1:0, y_mat?i1:0);
                  y1 = y(x_mat?j1:0, y_mat?i2:0);
                  y2 = y(x_mat?j2:0, y_mat?i2:0);
                  y3 = y(x_mat?j2:0, y_mat?i1:0);
                  y1m0 = y1 - y0;
                  y2m1 = y2 - y1;
                  y3m2 = y3 - y2;
                  y0m3 = y0 - y3;
                  y1p0 = y1 + y0;
                  y2p1 = y2 + y1;
                  y3p2 = y3 + y2;
                  y0p3 = y0 + y3;
                }

              double& nx = n(j, i, 0);
              double& ny = n(j, i, 1);
              double& nz = n(j, i, 2);

              z0 = z(j1, i1);
              z1 = z(j1, i2);
              z2 = z(j2, i2);
              z3 = z(j2, i1);

              // calculate face normal with Newell's method
              // https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal#Newell.27s_Method

              nx = y1m0 * (z1 + z0) + y2m1 * (z2 + z1)
                   + y3m2 * (z3 + z2) + y0m3 * (z0 + z3);
              ny = (z1 - z0) * x1p0 + (z2 - z1) * x2p1
                   + (z3 - z2) * x3p2 + (z0 - z3) * x0p3;
              nz = x1m0 * y1p0 + x2m1 * y2p1 + x3m2 * y3p2 + x0m3 * y0p3;

              double d = std::max (std::max (fabs (nx), fabs (ny)), fabs (nz));

              nx /= d;
              ny /= d;
              nz /= d;
            }
        }
      m_facenormals = n;
    }
  else if (reset)
    m_facenormals = Matrix ();
}

void
surface::properties::update_vertex_normals (bool reset, bool force)
{
  if (! vertexnormalsmode_is ("auto"))
    return;

  if (force || ((facelighting_is ("gouraud") || facelighting_is ("phong")
                 || edgelighting_is ("gouraud") || edgelighting_is ("phong"))
                && get_do_lighting ()))
    {
      Matrix x = get_xdata ().matrix_value ();
      Matrix y = get_ydata ().matrix_value ();
      Matrix z = get_zdata ().matrix_value ();

      int p = z.columns ();
      int q = z.rows ();

      // FIXME: There might be a cleaner way to do this.  When data is changed
      // the update_xdata, update_ydata, update_zdata routines are called in a
      // serial fashion.  Until the final call to update_zdata the matrices
      // will be of mismatched dimensions which can cause an out-of-bound
      // indexing in the code below.  This one-liner prevents calculating
      // normals until dimensions match.
      if (x.columns () != p || y.rows () != q)
        return;

      NDArray n (dim_vector (q, p, 3), 0.0);

      bool x_mat = (x.rows () == q);
      bool y_mat = (y.columns () == p);

      int i1, i2, i3, j1, j2, j3;
      i1 = i2 = i3 = 0;
      j1 = j2 = j3 = 0;

      for (int i = 0; i < p; i++)
        {
          if (y_mat)
            {
              i1 = i - 1;
              i2 = i;
              i3 = i + 1;
            }

          for (int j = 0; j < q; j++)
            {
              if (x_mat)
                {
                  j1 = j - 1;
                  j2 = j;
                  j3 = j + 1;
                }

              double& nx = n(j, i, 0);
              double& ny = n(j, i, 1);
              double& nz = n(j, i, 2);

              if ((j > 0) && (i > 0))
                // upper left quadrangle
                cross_product
                (x(j1, i-1)-x(j2, i), y(j-1, i1)-y(j, i2), z(j-1, i-1)-z(j, i),
                 x(j2, i-1)-x(j1, i), y(j, i1)-y(j-1, i2), z(j, i-1)-z(j-1, i),
                 nx, ny, nz);

              if ((j > 0) && (i < (p -1)))
                // upper right quadrangle
                cross_product
                (x(j1, i+1)-x(j2, i), y(j-1, i3)-y(j, i2), z(j-1, i+1)-z(j, i),
                 x(j1, i)-x(j2, i+1), y(j-1, i2)-y(j, i3), z(j-1, i)-z(j, i+1),
                 nx, ny, nz);

              if ((j < (q - 1)) && (i > 0))
                // lower left quadrangle
                cross_product
                (x(j2, i-1)-x(j3, i), y(j, i1)-y(j+1, i2), z(j, i-1)-z(j+1, i),
                 x(j3, i-1)-x(j2, i), y(j+1, i1)-y(j, i2), z(j+1, i-1)-z(j, i),
                 nx, ny, nz);

              if ((j < (q - 1)) && (i < (p -1)))
                // lower right quadrangle
                cross_product
                (x(j3, i)-x(j2, i+1), y(j+1, i2)-y(j, i3), z(j+1, i)-z(j, i+1),
                 x(j3, i+1)-x(j2, i), y(j+1, i3)-y(j, i2), z(j+1, i+1)-z(j, i),
                 nx, ny, nz);

              double d = -std::max (std::max (fabs (nx), fabs (ny)), fabs (nz));

              nx /= d;
              ny /= d;
              nz /= d;
            }
        }
      m_vertexnormals = n;
    }
  else if (reset)
    m_vertexnormals = Matrix ();
}

DEFMETHOD (__update_normals__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __update_normals__ (@var{h})
Update FaceNormals and VertexNormals of the patch or surface referred to by
@var{h}.

@end deftypefn */)
{
  gh_manager& gh_mgr = interp.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  if (args.length () != 1)
    print_usage ();

  octave_value val = args(0);

  graphics_object go = gh_mgr.get_object (val);

  if (go.isa ("surface"))
    {
      surface::properties& props
        = dynamic_cast <surface::properties&> (go.get_properties ());
      props.update_normals (false, true);
    }
  else if (go.isa ("patch"))
    {
      patch::properties& props
        = dynamic_cast <patch::properties&> (go.get_properties ());
      props.update_normals (false, true);
    }
  else
    error ("__update_normals__: "
           "H must be a handle to a valid surface or patch object.");

  return ovl ();
}

/*
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   Z = peaks ();
%!   hs = surf (Z, "facelighting", "none");
%!   assert (isempty (get (hs, "vertexnormals")));
%!   assert (isempty (get (hs, "facenormals")));
%!   __update_normals__ (hs);
%!   assert (! isempty (get (hs, "vertexnormals")));
%!   assert (! isempty (get (hs, "facenormals")));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hp = patch ("facelighting", "none");
%!   assert (isempty (get (hp, "vertexnormals")));
%!   assert (isempty (get (hp, "facenormals")));
%!   __update_normals__ (hp);
%!   assert (! isempty (get (hp, "vertexnormals")));
%!   assert (! isempty (get (hp, "facenormals")));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
*/

// ---------------------------------------------------------------------

void
hggroup::properties::remove_child (const graphics_handle& h, bool from_root)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (h);

  if (! from_root && go.isa ("light") && go.get_properties ().is_visible ())
    {
      axes::properties& ax_props
        = dynamic_cast<axes::properties&> (go.get_ancestor ("axes").get_properties ());
      ax_props.decrease_num_lights ();
    }
  base_properties::remove_child (h, from_root);
  update_limits ();
}

void
hggroup::properties::adopt (const graphics_handle& h)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (h);

  if (go.isa ("light") && go.get_properties ().is_visible ())
    {
      axes::properties& ax_props
        = dynamic_cast<axes::properties&> (go.get_ancestor ("axes").get_properties ());
      ax_props.increase_num_lights ();
    }
  base_properties::adopt (h);
  update_limits (h);
}

void
hggroup::properties::update_limits (void) const
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (m___myhandle__);

  if (go)
    {
      go.update_axis_limits ("xlim");
      go.update_axis_limits ("ylim");
      go.update_axis_limits ("zlim");
      go.update_axis_limits ("clim");
      go.update_axis_limits ("alim");
    }
}

void
hggroup::properties::update_limits (const graphics_handle& h) const
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (m___myhandle__);

  if (go)
    {
      go.update_axis_limits ("xlim", h);
      go.update_axis_limits ("ylim", h);
      go.update_axis_limits ("zlim", h);
      go.update_axis_limits ("clim", h);
      go.update_axis_limits ("alim", h);
    }
}

static bool updating_hggroup_limits = false;

void
hggroup::update_axis_limits (const std::string& axis_type,
                             const graphics_handle& h)
{
  if (updating_hggroup_limits)
    return;

  Matrix kids = Matrix (1, 1, h.value ());

  double min_val = octave::numeric_limits<double>::Inf ();
  double max_val = -octave::numeric_limits<double>::Inf ();
  double min_pos = octave::numeric_limits<double>::Inf ();
  double max_neg = -octave::numeric_limits<double>::Inf ();

  Matrix limits;
  double val;

  char update_type = 0;

  if (axis_type == "xlim" || axis_type == "xliminclude")
    {
      limits = m_properties.get_xlim ().matrix_value ();
      update_type = 'x';
    }
  else if (axis_type == "ylim" || axis_type == "yliminclude")
    {
      limits = m_properties.get_ylim ().matrix_value ();
      update_type = 'y';
    }
  else if (axis_type == "zlim" || axis_type == "zliminclude")
    {
      limits = m_properties.get_zlim ().matrix_value ();
      update_type = 'z';
    }
  else if (axis_type == "clim" || axis_type == "climinclude")
    {
      limits = m_properties.get_clim ().matrix_value ();
      update_type = 'c';
    }
  else if (axis_type == "alim" || axis_type == "aliminclude")
    {
      limits = m_properties.get_alim ().matrix_value ();
      update_type = 'a';
    }

  if (limits.numel () == 4)
    {
      val = limits(0);
      if (octave::math::isfinite (val))
        min_val = val;
      val = limits(1);
      if (octave::math::isfinite (val))
        max_val = val;
      val = limits(2);
      if (octave::math::isfinite (val))
        min_pos = val;
      val = limits(3);
      if (octave::math::isfinite (val))
        max_neg = val;
    }
  else
    {
      limits.resize (1, 4);
      limits(0) = min_val;
      limits(1) = max_val;
      limits(2) = min_pos;
      limits(3) = max_neg;
    }

  get_children_limits (min_val, max_val, min_pos, max_neg, kids, update_type);

  octave::unwind_protect_var<bool> restore_var (updating_hggroup_limits, true);

  if (limits(0) != min_val || limits(1) != max_val
      || limits(2) != min_pos || limits(3) != max_neg)
    {
      limits(0) = min_val;
      limits(1) = max_val;
      limits(2) = min_pos;
      limits(3) = max_neg;

      switch (update_type)
        {
        case 'x':
          m_properties.set_xlim (limits);
          break;

        case 'y':
          m_properties.set_ylim (limits);
          break;

        case 'z':
          m_properties.set_zlim (limits);
          break;

        case 'c':
          m_properties.set_clim (limits);
          break;

        case 'a':
          m_properties.set_alim (limits);
          break;

        default:
          break;
        }

      graphics_handle hg = m_properties.get___myhandle__ ();
      base_graphics_object::update_axis_limits (axis_type, hg);
    }
}

void
hggroup::update_axis_limits (const std::string& axis_type)
{
  if (updating_hggroup_limits)
    return;

  Matrix kids = m_properties.get_children ();

  double min_val = octave::numeric_limits<double>::Inf ();
  double max_val = -octave::numeric_limits<double>::Inf ();
  double min_pos = octave::numeric_limits<double>::Inf ();
  double max_neg = -octave::numeric_limits<double>::Inf ();

  char update_type = 0;

  if (axis_type == "xlim" || axis_type == "xliminclude")
    {
      get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'x');

      update_type = 'x';
    }
  else if (axis_type == "ylim" || axis_type == "yliminclude")
    {
      get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'y');

      update_type = 'y';
    }
  else if (axis_type == "zlim" || axis_type == "zliminclude")
    {
      get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'z');

      update_type = 'z';
    }
  else if (axis_type == "clim" || axis_type == "climinclude")
    {
      get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'c');

      update_type = 'c';
    }
  else if (axis_type == "alim" || axis_type == "aliminclude")
    {
      get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'a');

      update_type = 'a';
    }

  octave::unwind_protect_var<bool> restore_var (updating_hggroup_limits, true);

  Matrix limits (1, 4);

  limits(0) = min_val;
  limits(1) = max_val;
  limits(2) = min_pos;
  limits(3) = max_neg;

  switch (update_type)
    {
    case 'x':
      m_properties.set_xlim (limits);
      break;

    case 'y':
      m_properties.set_ylim (limits);
      break;

    case 'z':
      m_properties.set_zlim (limits);
      break;

    case 'c':
      m_properties.set_clim (limits);
      break;

    case 'a':
      m_properties.set_alim (limits);
      break;

    default:
      break;
    }

  base_graphics_object::update_axis_limits (axis_type);
}

// ---------------------------------------------------------------------

void
uicontextmenu::properties::update_beingdeleted (void)
{
  // Clear the uicontextmenu property of dependent objects
  if (m_beingdeleted.is ("on"))
    {
      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      std::list<graphics_handle> lst = get_dependent_obj_list ();

      for (auto& hobj : lst)
        {
          graphics_object go = gh_mgr.get_object (hobj);

          if (go.valid_object ()
              && go.get ("contextmenu") == get___myhandle__ ())
            go.set ("contextmenu", Matrix ());
        }
    }
}

/*
## Test deletion/reset of uicontextmenu
%!test
%! hf = figure ("visible", "off");
%! hax = axes ("parent", hf);
%! unwind_protect
%!   hctx1 = uicontextmenu ("parent", hf);
%!   hctx2 = uicontextmenu ("parent", hf);
%!   set (hf, "uicontextmenu", hctx2);
%!   set (hax, "uicontextmenu", hctx2);
%!   assert (get (hf, "uicontextmenu"), hctx2);
%!   assert (get (hax, "uicontextmenu"), hctx2);
%!   assert (get (hf, "children"), [hctx2; hctx1; hax]);
%!   delete (hctx2);
%!   assert (get (hf, "uicontextmenu"), []);
%!   assert (get (hax, "uicontextmenu"), []);
%!   assert (get (hf, "children"), [hctx1; hax]);
%!   set (hf, "uicontextmenu", hctx1);
%!   assert (get (hf, "uicontextmenu"), hctx1);
%!   set (hf, "uicontextmenu", []);
%!   assert (get (hf, "uicontextmenu"), []);
%!   assert (get (hf, "children"), [hctx1; hax]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
*/

// ---------------------------------------------------------------------

octave_value
uicontrol::properties::get_extent (void) const
{
  Matrix m = m_extent.get ().matrix_value ();

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object parent_go = gh_mgr.get_object (get_parent ());

  Matrix parent_bbox = parent_go.get_properties ().get_boundingbox (true);
  Matrix parent_size = parent_bbox.extract_n (0, 2, 1, 2);

  return convert_position (m, "pixels", get_units (), parent_size);
}

void
uicontrol::properties::update_text_extent (void)
{
  // FIXME: support multiline text

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (get___myhandle__ ());

  set_extent (go.get_toolkit ().get_text_extent (go));
}

void
uicontrol::properties::update_units (void)
{
  Matrix pos = get_position ().matrix_value ();

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object parent_go = gh_mgr.get_object (get_parent ());

  Matrix parent_bbox = parent_go.get_properties ().get_boundingbox (true);
  Matrix parent_size = parent_bbox.extract_n (0, 2, 1, 2);

  pos = convert_position (pos, m_cached_units, get_units (), parent_size);
  set_position (pos);

  m_cached_units = get_units ();
}

void
uicontrol::properties::set_style (const octave_value& st)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go_parent = gh_mgr.get_object (get_parent ());
  if (go_parent.valid_object () && go_parent.isa ("uibuttongroup"))
    {
      bool was_button = style_is ("radiobutton") || style_is ("togglebutton");
      m_style = st;
      bool now_button = style_is ("radiobutton") || style_is ("togglebutton");
      uibuttongroup::properties& props =
        dynamic_cast<uibuttongroup::properties&> (go_parent.get_properties ());
      // update selectedobject
      if (! was_button && now_button && ! props.get_selectedobject ().ok ())
        {
          props.set_selectedobject (get___myhandle__ ().value ());
          m_value.set (octave_value (1));
        }
      else if (was_button && ! now_button
               && (props.get_selectedobject ().value ()
                   == get___myhandle__ ().value ()))
        props.set_selectedobject (Matrix ());
    }

  // Don't notify the style change until the "value" property is fixed
  bool modified = m_style.set (st, true, false);

  // Override "value" property for listbox and popupmenu.
  if (modified)
    {
      if (style_is ("listbox") || style_is ("popupmenu"))
        {
          Matrix v = m_value.get ().matrix_value ();
          if (v.numel () == 1 && v(0) == 0)
            m_value.set (octave_value (1), true, false);
        }

      // Notify toolkit

      graphics_object go = gh_mgr.get_object (get___myhandle__ ());

      if (go)
        go.update (m_style.get_id ());
    }
}

Matrix
uicontrol::properties::get_boundingbox (bool,
                                        const Matrix& parent_pix_size) const
{
  Matrix pos = get_position ().matrix_value ();
  Matrix parent_size (parent_pix_size);

  if (parent_size.isempty ())
    {
      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      graphics_object go = gh_mgr.get_object (get_parent ());

      if (go.valid_object ())
        parent_size = go.get_properties ().get_boundingbox (true).extract_n (0, 2, 1, 2);
      else
        parent_size = default_figure_position ();
    }

  pos = convert_position (pos, get_units (), "pixels", parent_size);

  pos(0)--;
  pos(1)--;
  pos(1) = parent_size(1) - pos(1) - pos(3);

  return pos;
}

void
uicontrol::properties::set_fontunits (const octave_value& val)
{
  caseless_str old_fontunits = get_fontunits ();

  if (m_fontunits.set (val, true))
    {
      update_fontunits (old_fontunits);
      mark_modified ();
    }
}

void
uicontrol::properties::update_fontunits (const caseless_str& old_units)
{
  caseless_str new_units = get_fontunits ();
  double parent_height = get_boundingbox (false).elem (3);
  double fontsz = get_fontsize ();

  fontsz = convert_font_size (fontsz, old_units, new_units, parent_height);

  m_fontsize.set (octave_value (fontsz), true);
}

double
uicontrol::properties::get___fontsize_points__ (double box_pix_height) const
{
  double fontsz = get_fontsize ();
  double parent_height = box_pix_height;

  if (fontunits_is ("normalized") && parent_height <= 0)
    parent_height = get_boundingbox (false).elem (3);

  return convert_font_size (fontsz, get_fontunits (), "points", parent_height);
}

// ---------------------------------------------------------------------

Matrix
uibuttongroup::properties::get_boundingbox (bool internal,
    const Matrix& parent_pix_size) const
{
  Matrix pos = get_position ().matrix_value ();
  Matrix parent_size (parent_pix_size);

  if (parent_size.isempty ())
    {
      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      graphics_object go = gh_mgr.get_object (get_parent ());

      parent_size = go.get_properties ().get_boundingbox (true).extract_n (0, 2, 1, 2);
    }

  pos = convert_position (pos, get_units (), "pixels", parent_size);

  pos(0)--;
  pos(1)--;
  pos(1) = parent_size(1) - pos(1) - pos(3);

  if (internal)
    {
      double outer_height = pos(3);

      pos(0) = pos(1) = 0;

      if (! bordertype_is ("none"))
        {
          double bw = get_borderwidth ();
          double mul = 1.0;

          if (bordertype_is ("etchedin") || bordertype_is ("etchedout"))
            mul = 2.0;

          pos(0) += mul * bw;
          pos(1) += mul * bw;
          pos(2) -= 2 * mul * bw;
          pos(3) -= 2 * mul * bw;
        }

      if (! get_title ().empty ())
        {
          double fontsz = get_fontsize ();

          if (! fontunits_is ("pixels"))
            {
              double res = xget (0, "screenpixelsperinch").double_value ();

              if (fontunits_is ("points"))
                fontsz *= (res / 72.0);
              else if (fontunits_is ("inches"))
                fontsz *= res;
              else if (fontunits_is ("centimeters"))
                fontsz *= (res / 2.54);
              else if (fontunits_is ("normalized"))
                fontsz *= outer_height;
            }

          if (titleposition_is ("lefttop") || titleposition_is ("centertop")
              || titleposition_is ("righttop"))
            pos(1) += (fontsz / 2);
          pos(3) -= (fontsz / 2);
        }
    }

  return pos;
}

void
uibuttongroup::properties::set_position (const octave_value& v)
{
  Matrix old_bb, new_bb;
  bool modified = false;

  old_bb = get_boundingbox (true);
  modified = m_position.set (v, false);
  new_bb = get_boundingbox (true);

  if (old_bb != new_bb)
    {
      if (old_bb(2) != new_bb(2) || old_bb(3) != new_bb(3))
        {
          gh_manager& gh_mgr = octave::__get_gh_manager__ ();

          if (! get_resizefcn ().isempty ())
            gh_mgr.post_callback (m___myhandle__, "resizefcn");

          if (! get_sizechangedfcn ().isempty ())
            gh_mgr.post_callback (m___myhandle__, "sizechangedfcn");

          update_boundingbox ();
        }
    }

  if (modified)
    {
      m_position.run_listeners (GCB_POSTSET);
      mark_modified ();
    }
}

void
uibuttongroup::properties::set_units (const octave_value& val)
{
  caseless_str old_units = get_units ();

  if (m_units.set (val, true))
    {
      update_units (old_units);
      mark_modified ();
    }
}

void
uibuttongroup::properties::update_units (const caseless_str& old_units)
{
  Matrix pos = get_position ().matrix_value ();

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object parent_go = gh_mgr.get_object (get_parent ());

  Matrix parent_bbox = parent_go.get_properties ().get_boundingbox (true);
  Matrix parent_size = parent_bbox.extract_n (0, 2, 1, 2);

  pos = convert_position (pos, old_units, get_units (), parent_size);
  set_position (pos);
}

void
uibuttongroup::properties::set_fontunits (const octave_value& val)
{
  caseless_str old_fontunits = get_fontunits ();

  if (m_fontunits.set (val, true))
    {
      update_fontunits (old_fontunits);
      mark_modified ();
    }
}

void
uibuttongroup::properties::update_fontunits (const caseless_str& old_units)
{
  caseless_str new_units = get_fontunits ();
  double parent_height = get_boundingbox (false).elem (3);
  double fontsz = get_fontsize ();

  fontsz = convert_font_size (fontsz, old_units, new_units, parent_height);

  set_fontsize (octave_value (fontsz));
}

double
uibuttongroup::properties::get___fontsize_points__ (double box_pix_height) const
{
  double fontsz = get_fontsize ();
  double parent_height = box_pix_height;

  if (fontunits_is ("normalized") && parent_height <= 0)
    parent_height = get_boundingbox (false).elem (3);

  return convert_font_size (fontsz, get_fontunits (), "points", parent_height);
}

void
uibuttongroup::properties::set_selectedobject (const octave_value& v)
{
  graphics_handle current_selectedobject = get_selectedobject();
  m_selectedobject = current_selectedobject;
  if (v.isempty ())
    {
      if (current_selectedobject.ok ())
        {
          m_selectedobject = graphics_handle ();
          mark_modified ();
        }
      return;
    }

  graphics_handle val (v);
  if (val.ok ())
    {
      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      graphics_object go (gh_mgr.get_object (val));

      base_properties& gop = go.get_properties ();

      if (go.valid_object ()
          && gop.get_parent () == get___myhandle__ ()
          && go.isa ("uicontrol"))
        {
          uicontrol::properties& cop
            = dynamic_cast<uicontrol::properties&> (go.get_properties ());
          const caseless_str& style = cop.get_style ();
          if (style.compare ("radiobutton") || style.compare ("togglebutton"))
            {
              m_selectedobject = val;
              mark_modified ();
              return;
            }
        }
    }
  err_set_invalid ("selectedobject");
}

void
uibuttongroup::properties::remove_child (const graphics_handle& h,
    bool from_root)
{
  graphics_handle current_selected = get_selectedobject ();
  if (h.value () == current_selected.value ())
    set_selectedobject (Matrix ());

  base_properties::remove_child (h, from_root);
}

void
uibuttongroup::properties::adopt (const graphics_handle& h)
{
  base_properties::adopt (h);

  graphics_handle current_selected = get_selectedobject ();
  bool has_selected = current_selected.ok ();

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (h);

  if (! has_selected && go.valid_object () && go.isa ("uicontrol"))
    {
      const uicontrol::properties& props =
        dynamic_cast<const uicontrol::properties&> (go.get_properties ());
      if (props.style_is ("radiobutton") || props.style_is ("togglebutton"))
        set_selectedobject (h.value ());
    }
}

// ---------------------------------------------------------------------

Matrix
uipanel::properties::get_boundingbox (bool internal,
                                      const Matrix& parent_pix_size) const
{
  Matrix pos = get_position ().matrix_value ();
  Matrix parent_size (parent_pix_size);

  if (parent_size.isempty ())
    {
      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      graphics_object go = gh_mgr.get_object (get_parent ());

      parent_size = go.get_properties ().get_boundingbox (true).extract_n (0, 2, 1, 2);
    }

  pos = convert_position (pos, get_units (), "pixels", parent_size);

  pos(0)--;
  pos(1)--;
  pos(1) = parent_size(1) - pos(1) - pos(3);

  if (internal)
    {
      double outer_height = pos(3);

      pos(0) = pos(1) = 0;

      if (! bordertype_is ("none"))
        {
          double bw = get_borderwidth ();
          double mul = 1.0;

          if (bordertype_is ("etchedin") || bordertype_is ("etchedout"))
            mul = 2.0;

          pos(0) += mul * bw;
          pos(1) += mul * bw;
          pos(2) -= 2 * mul * bw;
          pos(3) -= 2 * mul * bw;
        }

      if (! get_title ().empty ())
        {
          double fontsz = get_fontsize ();

          if (! fontunits_is ("pixels"))
            {
              double res = xget (0, "screenpixelsperinch").double_value ();

              if (fontunits_is ("points"))
                fontsz *= (res / 72.0);
              else if (fontunits_is ("inches"))
                fontsz *= res;
              else if (fontunits_is ("centimeters"))
                fontsz *= (res / 2.54);
              else if (fontunits_is ("normalized"))
                fontsz *= outer_height;
            }

          if (titleposition_is ("lefttop") || titleposition_is ("centertop")
              || titleposition_is ("righttop"))
            pos(1) += (fontsz / 2);
          pos(3) -= (fontsz / 2);
        }
    }

  return pos;
}

void
uipanel::properties::set_position (const octave_value& v)
{
  Matrix old_bb, new_bb;
  bool modified = false;

  old_bb = get_boundingbox (true);
  modified = m_position.set (v, false);
  new_bb = get_boundingbox (true);

  if (old_bb != new_bb)
    {
      if (old_bb(2) != new_bb(2) || old_bb(3) != new_bb(3))
        {
          gh_manager& gh_mgr = octave::__get_gh_manager__ ();

          if (! get_resizefcn ().isempty ())
            gh_mgr.post_callback (m___myhandle__, "resizefcn");

          if (! get_sizechangedfcn ().isempty ())
            gh_mgr.post_callback (m___myhandle__, "sizechangedfcn");

          update_boundingbox ();
        }
    }

  if (modified)
    {
      m_position.run_listeners (GCB_POSTSET);
      mark_modified ();
    }
}


void
uipanel::properties::set_units (const octave_value& val)
{
  caseless_str old_units = get_units ();

  if (m_units.set (val, true))
    {
      update_units (old_units);
      mark_modified ();
    }
}

void
uipanel::properties::update_units (const caseless_str& old_units)
{
  Matrix pos = get_position ().matrix_value ();

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object parent_go = gh_mgr.get_object (get_parent ());

  Matrix parent_bbox = parent_go.get_properties ().get_boundingbox (true);
  Matrix parent_size = parent_bbox.extract_n (0, 2, 1, 2);

  pos = convert_position (pos, old_units, get_units (), parent_size);
  set_position (pos);
}

void
uipanel::properties::set_fontunits (const octave_value& val)
{
  caseless_str old_fontunits = get_fontunits ();

  if (m_fontunits.set (val, true))
    {
      update_fontunits (old_fontunits);
      mark_modified ();
    }
}

void
uipanel::properties::update_fontunits (const caseless_str& old_units)
{
  caseless_str new_units = get_fontunits ();
  double parent_height = get_boundingbox (false).elem (3);
  double fontsz = get_fontsize ();

  fontsz = convert_font_size (fontsz, old_units, new_units, parent_height);

  set_fontsize (octave_value (fontsz));
}

double
uipanel::properties::get___fontsize_points__ (double box_pix_height) const
{
  double fontsz = get_fontsize ();
  double parent_height = box_pix_height;

  if (fontunits_is ("normalized") && parent_height <= 0)
    parent_height = get_boundingbox (false).elem (3);

  return convert_font_size (fontsz, get_fontunits (), "points", parent_height);
}

// ---------------------------------------------------------------------

Matrix
uitable::properties::get_boundingbox (bool,
                                      const Matrix& parent_pix_size) const
{
  Matrix pos = get_position ().matrix_value ();
  Matrix parent_size (parent_pix_size);

  if (parent_size.isempty ())
    {
      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      graphics_object go = gh_mgr.get_object (get_parent ());

      parent_size = go.get_properties ().get_boundingbox (true).extract_n (0, 2, 1, 2);
    }

  pos = convert_position (pos, get_units (), "pixels", parent_size);

  pos(0)--;
  pos(1)--;
  pos(1) = parent_size(1) - pos(1) - pos(3);

  return pos;
}

void
uitable::properties::set_columnformat (const octave_value& val)
{
  /* Matlab only allows certain values for ColumnFormat. Here we only check the
   * structure of the argument.  Values will be checked in Table.cc */

  if (val.iscellstr ())
    {
      if (m_columnformat.set (val, true))
        mark_modified ();
    }
  else if (val.iscell ())
    {
      Cell cell_value = val.cell_value ();

      for (int i = 0; i < cell_value.numel (); i++)
        {
          octave_value v = cell_value(i);
          if (v.iscell ())
            {
              /* We are in a pop-up menu selection.
               * Matlab only allows non-empty strings here. */
              Cell popup = v.cell_value ();
              for (int j = 0; j < popup.numel (); j++)
                {
                  octave_value p = popup(j);
                  if (! p.is_string () || p.isempty ())
                    error ("set: pop-up menu definitions must be non-empty strings");
                }
            }
          else if (! (v.is_string () || v.isempty ()))
            {
              error ("set: columnformat definintions must be a cellstr of "
                     "either 'char', 'short [e|g|eng]?', 'long [e|g|eng]?', "
                     "'numeric', 'bank', '+', 'rat', 'logical', "
                     "or a cellstr of non-empty pop-up menu definitions.");
            }
        }

      if (m_columnformat.set (val, true))
        mark_modified ();
    }
  else if (val.isempty ())
    {
      if (m_columnformat.set (Cell (), true))
        mark_modified ();
    }
  else
    {
      error ("set: expecting cell of strings");
    }
}

void
uitable::properties::set_columnwidth (const octave_value& val)
{
  bool error_exists = false;

  if (val.is_string () && val.string_value (false) == "auto")
    error_exists = false;
  else if (val.iscell ())
    {
      Cell cell_value = val.cell_value ();
      for (int i = 0; i < cell_value.numel (); i++)
        {
          octave_value v = cell_value(i);
          if (v.is_string ())
            {
              if (v.string_value (false) != "auto")
                error_exists = true;
            }
          else if (v.iscell ())
            {
              error_exists = true;
            }
          else if (! v.is_scalar_type ())
            {
              error_exists = true;
            }
        }
    }
  else
    error_exists = true;

  if (error_exists)
    error ("set: expecting either 'auto' or a cell of pixel values or auto");
  else
    {
      if (m_columnwidth.set (val, true))
        mark_modified ();
    }
}

void
uitable::properties::set_units (const octave_value& val)
{
  caseless_str old_units = get_units ();

  if (m_units.set (val, true))
    {
      update_units (old_units);
      mark_modified ();
    }
}

void
uitable::properties::update_units (const caseless_str& old_units)
{
  Matrix pos = get_position ().matrix_value ();

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object parent_go = gh_mgr.get_object (get_parent ());

  Matrix parent_bbox = parent_go.get_properties ().get_boundingbox (true);
  Matrix parent_size = parent_bbox.extract_n (0, 2, 1, 2);

  pos = convert_position (pos, old_units, get_units (), parent_size);
  set_position (pos);
}

void
uitable::properties::set_fontunits (const octave_value& val)
{
  caseless_str old_fontunits = get_fontunits ();

  if (m_fontunits.set (val, true))
    {
      update_fontunits (old_fontunits);
      mark_modified ();
    }
}

void
uitable::properties::update_fontunits (const caseless_str& old_units)
{
  caseless_str new_units = get_fontunits ();
  double parent_height = get_boundingbox (false).elem (3);
  double fontsz = get_fontsize ();

  fontsz = convert_font_size (fontsz, old_units, new_units, parent_height);

  set_fontsize (octave_value (fontsz));
}

double
uitable::properties::get___fontsize_points__ (double box_pix_height) const
{
  double fontsz = get_fontsize ();
  double parent_height = box_pix_height;

  if (fontunits_is ("normalized") && parent_height <= 0)
    parent_height = get_boundingbox (false).elem (3);

  return convert_font_size (fontsz, get_fontunits (), "points", parent_height);
}

double
uitable::properties::get_fontsize_pixels (double box_pix_height) const
{
  double fontsz = get_fontsize ();
  double parent_height = box_pix_height;

  if (fontunits_is ("normalized") && parent_height <= 0)
    parent_height = get_boundingbox (false).elem (3);

  return convert_font_size (fontsz, get_fontunits (), "pixels", parent_height);
}

Matrix
uitable::properties::get_backgroundcolor_rgb (void)
{
  Matrix bg = m_backgroundcolor.get ().matrix_value ();
  return bg.row (0);
}

Matrix
uitable::properties::get_alternatebackgroundcolor_rgb (void)
{
  int i = 0;
  Matrix bg = m_backgroundcolor.get ().matrix_value ();
  if (bg.rows () > 1)
    i = 1;

  return bg.row (i);
}

Matrix
uitable::properties::get_extent_matrix (void) const
{
  return m_extent.get ().matrix_value ();
}

octave_value
uitable::properties::get_extent (void) const
{
  // FIXME: Is it really acceptable to just let the toolkit update the extent?
  Matrix m = m_extent.get ().matrix_value ();

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object parent_go = gh_mgr.get_object (get_parent ());

  if (parent_go)
    {
      Matrix parent_bbox = parent_go.get_properties ().get_boundingbox (true);
      Matrix parent_size = parent_bbox.extract_n (0, 2, 1, 2);

      return convert_position (m, "pixels", get_units (), parent_size);
    }

  return m;
}

// ---------------------------------------------------------------------

octave_value
uitoolbar::get_default (const caseless_str& pname) const
{
  octave_value retval = m_default_properties.lookup (pname);

  if (retval.is_undefined ())
    {
      graphics_handle parent_h = get_parent ();

      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      graphics_object parent_go = gh_mgr.get_object (parent_h);

      retval = parent_go.get_default (pname);
    }

  return retval;
}

void
uitoolbar::reset_default_properties (void)
{
  // empty list of local defaults
  m_default_properties = property_list ();

  remove_all_listeners ();
  xreset_default_properties (get_handle (), m_properties.factory_defaults ());
}

// ---------------------------------------------------------------------

octave_value
base_graphics_object::get_default (const caseless_str& pname) const
{
  graphics_handle parent_h = get_parent ();

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object parent_go = gh_mgr.get_object (parent_h);

  return parent_go.get_default (type () + pname);
}

octave_value
base_graphics_object::get_factory_default (const caseless_str& name) const
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object parent_go = gh_mgr.get_object (0);

  return parent_go.get_factory_default (type () + name);
}

// We use a random value for the handle to avoid issues with plots and
// scalar values for the first argument.
gh_manager::gh_manager (octave::interpreter& interp)
  : m_interpreter (interp), m_handle_map (), m_handle_free_list (),
    m_next_handle (-1.0 - (rand () + 1.0) / (RAND_MAX + 2.0)),
    m_figure_list (), m_graphics_lock (),  m_event_queue (),
    m_callback_objects (), m_event_processing (0)
{
  m_handle_map[0] = graphics_object (new root_figure ());

  octave::gtk_manager& gtk_mgr = octave::__get_gtk_manager__ ();

  // Make sure the default graphics toolkit is registered.
  gtk_mgr.default_toolkit ();
}

graphics_handle
gh_manager::make_graphics_handle (const std::string& go_name,
                                  const graphics_handle& p,
                                  bool integer_figure_handle,
                                  bool call_createfcn, bool notify_toolkit)
{
  graphics_handle h = get_handle (integer_figure_handle);

  base_graphics_object *bgo = make_graphics_object_from_type (go_name, h, p);

  if (! bgo)
    error ("gh_manager::make_graphics_handle: invalid object type '%s'",
           go_name.c_str ());

  graphics_object go (bgo);

  m_handle_map[h] = go;

  if (go_name == "axes")
    {
      // Handle defaults for labels since overriding defaults for
      // them can't work before the axes object is fully
      // constructed.

      axes::properties& props
        = dynamic_cast<axes::properties&> (go.get_properties ());

      graphics_object tgo;

      tgo = get_object (props.get_xlabel ());
      tgo.override_defaults ();

      tgo = get_object (props.get_ylabel ());
      tgo.override_defaults ();

      tgo = get_object (props.get_zlabel ());
      tgo.override_defaults ();

      tgo = get_object (props.get_title ());
      tgo.override_defaults ();
    }

  // Overriding defaults will work now because the handle is valid
  // and we can find parent objects (not just handles).
  go.override_defaults ();

  if (call_createfcn)
    bgo->get_properties ().execute_createfcn ();

  // Notify graphics toolkit.
  if (notify_toolkit)
    go.initialize ();

  return h;
}

graphics_handle
gh_manager::make_figure_handle (double val, bool notify_toolkit)
{
  graphics_handle h = val;

  base_graphics_object *bgo = new figure (h, 0);
  graphics_object go (bgo);

  m_handle_map[h] = go;

  // Notify graphics toolkit.
  if (notify_toolkit)
    go.initialize ();

  go.override_defaults ();

  return h;
}

void
gh_manager::push_figure (const graphics_handle& h)
{
  pop_figure (h);

  m_figure_list.push_front (h);
}

void
gh_manager::pop_figure (const graphics_handle& h)
{
  for (auto it = m_figure_list.begin (); it != m_figure_list.end (); it++)
    {
      if (*it == h)
        {
          m_figure_list.erase (it);
          break;
        }
    }
}

class
callback_event : public base_graphics_event
{
public:
  callback_event (const graphics_handle& h, const std::string& name,
                  const octave_value& data = Matrix (),
                  int busyaction = base_graphics_event::QUEUE)
    : base_graphics_event (busyaction), m_handle (h), m_callback_name (name),
      m_callback (), m_callback_data (data) { }

  callback_event (const graphics_handle& h, const octave_value& cb,
                  const octave_value& data = Matrix (),
                  int busyaction = base_graphics_event::QUEUE)
    : base_graphics_event (busyaction), m_handle (h), m_callback_name (),
      m_callback (cb), m_callback_data (data) { }

  void execute (void)
  {
    gh_manager& gh_mgr = octave::__get_gh_manager__ ();

    if (m_callback.is_defined ())
      gh_mgr.execute_callback (m_handle, m_callback, m_callback_data);
    else
      gh_mgr.execute_callback (m_handle, m_callback_name, m_callback_data);
  }

private:
  callback_event (void)
    : base_graphics_event (), m_handle (), m_callback_name (),
      m_callback_data ()
  { }

private:
  graphics_handle m_handle;
  std::string m_callback_name;
  octave_value m_callback;
  octave_value m_callback_data;
};

class
mcode_event : public base_graphics_event
{
public:
  mcode_event (const graphics_handle& h, const std::string& cmd,
               int busyaction = base_graphics_event::QUEUE)
    : base_graphics_event (busyaction), m_handle (h), m_mcode (cmd)
  { }

  void execute (void)
  {
    if (! m_mcode.empty ())
      {
        gh_manager& gh_mgr = octave::__get_gh_manager__ ();

        graphics_object go = gh_mgr.get_object (m_handle);

        if (go.valid_object ())
          {
            octave_value cb (m_mcode);
            gh_mgr.execute_callback (m_handle, cb);
          }
      }
  }

private:
  mcode_event (void)
    : base_graphics_event (), m_handle (), m_mcode ()
  { }

private:
  graphics_handle m_handle;
  std::string m_mcode;
};

class
function_event : public base_graphics_event
{
public:

  // function_event objects must be created with at least a function.

  function_event (void) = delete;

  function_event (graphics_event::event_fcn fcn, void *data = nullptr)
    : base_graphics_event (), m_function (fcn), m_function_data (data)
  { }

  // No copying!

  function_event (const function_event&) = delete;

  function_event& operator = (const function_event&) = delete;

  void execute (void)
  {
    m_function (m_function_data);
  }

private:

  graphics_event::event_fcn m_function;

  void *m_function_data;
};

class
set_event : public base_graphics_event
{
public:
  set_event (const graphics_handle& h, const std::string& name,
             const octave_value& value, bool do_notify_toolkit = true,
             bool redraw_figure = false)
    : base_graphics_event (), m_handle (h), m_property_name (name),
      m_property_value (value), m_notify_toolkit (do_notify_toolkit),
      m_redraw_figure (redraw_figure)
  { }

  void execute (void)
  {
    gh_manager& gh_mgr = octave::__get_gh_manager__ ();

    octave::autolock guard (gh_mgr.graphics_lock ());

    graphics_object go = gh_mgr.get_object (m_handle);

    if (go)
      {
        property p = go.get_properties ().get_property (m_property_name);

        if (p.ok ())
          {
            // FIXME: figure position and outerposition properties set_xxx have
            // a signature that allows passing the notify_toolkit argument.
            // Should we change all set_xxx signatures and allow
            // base_properties::set to accept this also? This would allow for
            // the use of high level set_xxx instead of directly changing the
            // property value.
            if (go.isa ("figure") && m_property_name == "position")
              {
                figure::properties& fprops
                  = dynamic_cast<figure::properties&> (go.get_properties ());
                fprops.set_position (m_property_value, m_notify_toolkit);
              }
            else if (go.isa ("figure") && m_property_name == "outerposition")
              {
                figure::properties& fprops
                  = dynamic_cast<figure::properties&> (go.get_properties ());
                fprops.set_outerposition (m_property_value, m_notify_toolkit);
              }
            else
              p.set (m_property_value, true, m_notify_toolkit);

            if (m_redraw_figure)
              {
                if (! go.isa ("figure"))
                  go = go.get_ancestor ("figure");

                if (go.valid_object ())
                  {
                    figure::properties& fprops
                      = dynamic_cast<figure::properties&> (go.get_properties ());
                    fprops.get_toolkit ().redraw_figure (go);
                  }
              }
          }
      }
  }

private:
  set_event (void)
    : base_graphics_event (), m_handle (), m_property_name (), m_property_value ()
  { }

private:
  graphics_handle m_handle;
  std::string m_property_name;
  octave_value m_property_value;
  bool m_notify_toolkit;
  bool m_redraw_figure;
};

graphics_event
graphics_event::create_callback_event (const graphics_handle& h,
                                       const std::string& name,
                                       const octave_value& data,
                                       int busyaction)
{
  return graphics_event (new callback_event (h, name, data, busyaction));
}

graphics_event
graphics_event::create_callback_event (const graphics_handle& h,
                                       const octave_value& cb,
                                       const octave_value& data,
                                       int busyaction)
{
  return graphics_event (new callback_event (h, cb, data, busyaction));
}

graphics_event
graphics_event::create_mcode_event (const graphics_handle& h,
                                    const std::string& cmd,
                                    int busyaction)
{
  return graphics_event (new mcode_event (h, cmd, busyaction));
}

graphics_event
graphics_event::create_function_event (graphics_event::event_fcn fcn,
                                       void *data)
{
  return graphics_event (new function_event (fcn, data));
}

graphics_event
graphics_event::create_set_event (const graphics_handle& h,
                                  const std::string& name,
                                  const octave_value& data,
                                  bool notify_toolkit, bool redraw_figure)
{
  return graphics_event (new set_event (h, name, data, notify_toolkit,
                                        redraw_figure));
}

static void
xset_gcbo (const graphics_handle& h)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (0);

  root_figure::properties& props
    = dynamic_cast<root_figure::properties&> (go.get_properties ());

  props.set_callbackobject (h.as_octave_value ());
}

void
gh_manager::restore_gcbo (void)
{
  octave::autolock guard (m_graphics_lock);

  m_callback_objects.pop_front ();

  xset_gcbo (m_callback_objects.empty ()
             ? graphics_handle () : m_callback_objects.front ().get_handle ());
}

void
gh_manager::execute_listener (const graphics_handle& h, const octave_value& l)
{
  if (octave::thread::is_thread ())
    execute_callback (h, l, octave_value ());
  else
    {
      octave::autolock guard (m_graphics_lock);

      post_event (graphics_event::create_callback_event (h, l));
    }
}

void
gh_manager::execute_callback (const graphics_handle& h,
                              const octave_value& cb_arg,
                              const octave_value& data)
{
  if (cb_arg.is_defined () && ! cb_arg.isempty ())
    {
      octave_value_list args;
      octave_value ov_fcn;
      octave_function *fcn = nullptr;

      args(0) = h.as_octave_value ();
      if (data.is_defined ())
        args(1) = data;
      else
        args(1) = Matrix ();

      octave::unwind_action_safe restore_gcbo_action
      (&gh_manager::restore_gcbo, this);

      graphics_object go (get_object (h));
      if (go)
        {
          // FIXME: Is the lock necessary when we're only calling a
          //        const "get" method?
          octave::autolock guard (m_graphics_lock);
          m_callback_objects.push_front (go);
          xset_gcbo (h);
        }

      // Copy CB because "function_value" method is non-const.
      octave_value cb = cb_arg;

      if (cb.is_function ())
        fcn = cb.function_value ();
      else if (cb.is_function_handle ())
        ov_fcn = cb;
      else if (cb.is_string ())
        {
          int status;
          std::string s = cb.string_value ();

          try
            {
              m_interpreter.eval_string (s, false, status, 0);
            }
          catch (const octave::execution_exception& ee)
            {
              m_interpreter.handle_exception (ee);
            }
        }
      else if (cb.iscell () && cb.length () > 0
               && (cb.rows () == 1 || cb.columns () == 1)
               && (cb.cell_value ()(0).is_function ()
                   || cb.cell_value ()(0).is_function_handle ()))
        {
          Cell c = cb.cell_value ();

          ov_fcn = c(0);

          for (int i = 1; i < c.numel () ; i++)
            args(1+i) = c(i);
        }
      else
        {
          std::string nm = cb.class_name ();
          error ("trying to execute non-executable object (class = %s)",
                 nm.c_str ());
        }

      if (fcn || ov_fcn.is_defined ())
        try
          {
            if (ov_fcn.is_defined ())
              octave::feval (ov_fcn, args);
            else
              octave::feval (fcn, args);
          }
        catch (const octave::execution_exception& ee)
          {
            m_interpreter.handle_exception (ee);
          }

      // Redraw after interacting with a user-interface (ui*) object.
      if (Vdrawnow_requested)
        {
          if (go)
            {
              std::string go_name
                = go.get_properties ().graphics_object_name ();

              if (go_name.length () > 1
                  && go_name[0] == 'u' && go_name[1] == 'i')
                {
                  Fdrawnow (m_interpreter);
                  Vdrawnow_requested = false;
                }
            }
        }
    }
}

static int
process_graphics_events (void)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  return gh_mgr.process_events ();
}

void
gh_manager::post_event (const graphics_event& e)
{
  m_event_queue.push_back (e);

  octave::command_editor::add_event_hook (process_graphics_events);
}

void
gh_manager::post_callback (const graphics_handle& h, const std::string& name,
                           const octave_value& data)
{
  octave::autolock guard (m_graphics_lock);

  graphics_object go = get_object (h);

  if (go.valid_object ())
    {
      caseless_str cname (name);
      int busyaction = base_graphics_event::QUEUE;

      if (cname == "deletefcn" || cname == "createfcn"
          || cname == "closerequestfcn"
          || ((go.isa ("figure") || go.isa ("uipanel")
               || go.isa ("uibuttongroup"))
              && (cname == "resizefcn" || cname == "sizechangedfcn")))
        busyaction = base_graphics_event::INTERRUPT;
      else if (go.get_properties ().get_busyaction () == "cancel")
        busyaction = base_graphics_event::CANCEL;

      // The "closerequestfcn" callback must be executed once the figure has
      // been made current.  Let "close" do the job.
      if (cname == "closerequestfcn")
        {
          std::string cmd ("close (gcbf ());");
          post_event (graphics_event::create_mcode_event (h, cmd, busyaction));
        }
      else
        post_event (graphics_event::create_callback_event (h, name, data,
                    busyaction));
    }
}

void
gh_manager::post_function (graphics_event::event_fcn fcn, void *fcn_data)
{
  octave::autolock guard (m_graphics_lock);

  post_event (graphics_event::create_function_event (fcn, fcn_data));
}

void
gh_manager::post_set (const graphics_handle& h, const std::string& name,
                      const octave_value& value, bool notify_toolkit,
                      bool redraw_figure)
{
  octave::autolock guard (m_graphics_lock);

  post_event (graphics_event::create_set_event (h, name, value, notify_toolkit,
              redraw_figure));
}

int
gh_manager::process_events (bool force)
{
  graphics_event e;
  bool old_Vdrawnow_requested = Vdrawnow_requested;
  bool events_executed = false;

  do
    {
      e = graphics_event ();

      {
        octave::autolock guard (m_graphics_lock);

        if (! m_event_queue.empty ())
          {
            if (m_callback_objects.empty () || force)
              {
                e = m_event_queue.front ();

                m_event_queue.pop_front ();
              }
            else
              {
                const graphics_object& go = m_callback_objects.front ();

                if (go.get_properties ().is_interruptible ())
                  {
                    e = m_event_queue.front ();

                    m_event_queue.pop_front ();
                  }
                else
                  {
                    std::list<graphics_event>::iterator p = m_event_queue.begin ();

                    while (p != m_event_queue.end ())
                      if (p->get_busyaction () == base_graphics_event::CANCEL)
                        {
                          p = m_event_queue.erase (p);
                        }
                      else if (p->get_busyaction ()
                               == base_graphics_event::INTERRUPT)
                        {
                          e = (*p);
                          m_event_queue.erase (p);
                          break;
                        }
                      else
                        p++;
                  }
              }
          }
      }

      if (e.ok ())
        {
          e.execute ();
          events_executed = true;
        }
    }
  while (e.ok ());

  {
    octave::autolock guard (m_graphics_lock);

    if (m_event_queue.empty () && m_event_processing == 0)
      octave::command_editor::remove_event_hook (process_graphics_events);
  }

  if (events_executed)
    octave::flush_stdout ();

  if (Vdrawnow_requested && ! old_Vdrawnow_requested)
    {
      Fdrawnow (m_interpreter);

      Vdrawnow_requested = false;
    }

  return 0;
}


/*
## Test interruptible/busyaction properties
%!function cb (h, ~)
%! setappdata (gcbf (), "cb_exec", [getappdata(gcbf (), "cb_exec") h]);
%! drawnow ();
%! setappdata (gcbf (), "cb_exec", [getappdata(gcbf (), "cb_exec") h]);
%!endfunction
%!
%!testif HAVE_OPENGL, HAVE_QT; have_window_system () && any (strcmp ("qt", available_graphics_toolkits ()))
%! hf = figure ("visible", "off", "resizefcn", @cb);
%! graphics_toolkit (hf, "qt");
%! unwind_protect
%!   ## Default
%!   hui1 = uicontrol ("parent", hf, "interruptible", "on", "callback", @cb);
%!   hui2 = uicontrol ("parent", hf, "busyaction", "queue", "callback", @cb);
%!   hui3 = uicontrol ("parent", hf, "busyaction", "queue", "callback", @cb);
%!   __go_post_callback__ (hui1, "callback");
%!   __go_post_callback__ (hui2, "callback");
%!   __go_post_callback__ (hui3, "callback");
%!
%!   assert (getappdata (hf, "cb_exec"), []);
%!   drawnow ();
%!   assert (getappdata (hf, "cb_exec"), [hui1 hui2 hui3 hui3 hui2 hui1]);
%!
%!   ## Interruptible off
%!   setappdata (hf, "cb_exec", []);
%!   set (hui1, "interruptible", "off");
%!   __go_post_callback__ (hui1, "callback");
%!   __go_post_callback__ (hui2, "callback");
%!   __go_post_callback__ (hui3, "callback");
%!   drawnow ();
%!   assert (getappdata (hf, "cb_exec"), [hui1 hui1 hui2 hui3 hui3 hui2]);
%!
%!   ## "resizefcn" callback interrupts regardless of interruptible property
%!   setappdata (hf, "cb_exec", []);
%!   __go_post_callback__ (hui1, "callback");
%!   __go_post_callback__ (hf, "resizefcn");
%!   drawnow ();
%!   assert (getappdata (hf, "cb_exec"), [hui1 hf hf hui1]);
%!
%!   ## test "busyaction" "cancel"
%!   setappdata (hf, "cb_exec", []);
%!   set (hui2, "busyaction", "cancel");
%!   __go_post_callback__ (hui1, "callback");
%!   __go_post_callback__ (hui2, "callback");
%!   __go_post_callback__ (hui3, "callback");
%!   __go_post_callback__ (hf, "resizefcn");
%!   drawnow ();
%!   assert (getappdata (hf, "cb_exec"), [hui1 hf hui3 hui3 hf hui1]);
%! unwind_protect_cleanup
%!   close (hf)
%! end_unwind_protect
*/

void
gh_manager::enable_event_processing (bool enable)
{
  octave::autolock guard (m_graphics_lock);

  if (enable)
    {
      m_event_processing++;

      octave::command_editor::add_event_hook (process_graphics_events);
    }
  else
    {
      m_event_processing--;

      if (m_event_queue.empty () && m_event_processing == 0)
        octave::command_editor::remove_event_hook (process_graphics_events);
    }
}

property_list::plist_map_type
root_figure::init_factory_properties (void)
{
  property_list::plist_map_type plist_map;

  plist_map["figure"] = figure::properties::factory_defaults ();
  plist_map["axes"] = axes::properties::factory_defaults ();
  plist_map["line"] = line::properties::factory_defaults ();
  plist_map["text"] = text::properties::factory_defaults ();
  plist_map["image"] = image::properties::factory_defaults ();
  plist_map["patch"] = patch::properties::factory_defaults ();
  plist_map["scatter"] = scatter::properties::factory_defaults ();
  plist_map["surface"] = surface::properties::factory_defaults ();
  plist_map["light"] = light::properties::factory_defaults ();
  plist_map["hggroup"] = hggroup::properties::factory_defaults ();
  plist_map["uimenu"] = uimenu::properties::factory_defaults ();
  plist_map["uicontrol"] = uicontrol::properties::factory_defaults ();
  plist_map["uibuttongroup"] = uibuttongroup::properties::factory_defaults ();
  plist_map["uipanel"] = uipanel::properties::factory_defaults ();
  plist_map["uicontextmenu"] = uicontextmenu::properties::factory_defaults ();
  plist_map["uitoolbar"] = uitoolbar::properties::factory_defaults ();
  plist_map["uipushtool"] = uipushtool::properties::factory_defaults ();
  plist_map["uitoggletool"] = uitoggletool::properties::factory_defaults ();

  return plist_map;
}

// ---------------------------------------------------------------------

DEFMETHOD (ishghandle, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} ishghandle (@var{h})
Return true if @var{h} is a graphics handle and false otherwise.

@var{h} may also be a matrix of handles in which case a logical array is
returned that is true where the elements of @var{h} are graphics handles and
false where they are not.
@seealso{isgraphics, isaxes, isfigure, ishandle}
@end deftypefn */)
{
  gh_manager& gh_mgr = interp.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  if (args.length () != 1)
    print_usage ();

  return ovl (ishghandle (args(0)));
}

/*
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   assert (ishghandle (hf));
%!   assert (! ishghandle (-hf));
%!   ax = gca ();
%!   l = line ();
%!   assert (ishghandle (ax));
%!   assert (! ishghandle (-ax));
%!   assert (ishghandle ([l, -1, ax, hf]), logical ([1, 0, 1, 1]));
%!   assert (ishghandle ([l, -1, ax, hf]'), logical ([1, 0, 1, 1]'));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!assert (ishghandle ([-1 0]), [false true])
*/

static bool
is_handle_visible (const graphics_handle& h)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  return h.ok () && gh_mgr.is_handle_visible (h);
}

static bool
is_handle_visible (double val)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  return is_handle_visible (gh_mgr.lookup (val));
}

static octave_value
is_handle_visible (const octave_value& val)
{
  octave_value retval = false;

  if (val.is_real_scalar () && is_handle_visible (val.double_value ()))
    retval = true;
  else if (val.isnumeric () && val.isreal ())
    {
      const NDArray handles = val.array_value ();

      boolNDArray result (handles.dims ());

      for (octave_idx_type i = 0; i < handles.numel (); i++)
        result.xelem (i) = is_handle_visible (handles(i));

      retval = result;
    }

  return retval;
}

DEFUN (__is_handle_visible__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} __is_handle_visible__ (@var{h})
Undocumented internal function.
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (is_handle_visible (args(0)));
}

DEFMETHOD (reset, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} reset (@var{h})
Reset the properties of the graphic object @var{h} to their default values.

For figures, the properties @qcode{"position"}, @qcode{"units"},
@qcode{"windowstyle"}, and @qcode{"paperunits"} are not affected.
For axes, the properties @qcode{"position"} and @qcode{"units"} are
not affected.

The input @var{h} may also be a vector of graphic handles in which case
each individual object will be reset.
@seealso{cla, clf, newplot}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  // get vector of graphics handles
  ColumnVector hcv = args(0).xvector_value ("reset: H must be a graphics handle");

  gh_manager& gh_mgr = interp.get_gh_manager ();

  // loop over graphics objects
  for (octave_idx_type n = 0; n < hcv.numel (); n++)
    gh_mgr.get_object (hcv(n)).reset_default_properties ();

  Vdrawnow_requested = true;

  return ovl ();
}

/*

%!test  # line object
%! hf = figure ("visible", "off");
%! unwind_protect
%!   tol = 20 * eps;
%!   hax = axes ("defaultlinelinewidth", 3);
%!
%!   hli = line (1:10, 1:10, 1:10, "marker", "o",
%!               "markerfacecolor", "b", "linestyle", ":");
%!
%!   reset (hli);
%!   assert (get (hli, "marker"), get (0, "defaultlinemarker"));
%!   assert (get (hli, "markerfacecolor"),
%!           get (0, "defaultlinemarkerfacecolor"));
%!   assert (get (hli, "linestyle"), get (0, "defaultlinelinestyle"));
%!   assert (get (hli, "linewidth"), 3, tol);  # parent axes defaults
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test  # patch object
%! hf = figure ("visible", "off");
%! unwind_protect
%!   tol = 20 * eps;
%!   t1 = (1/16:1/8:1)' * 2*pi;
%!   t2 = ((1/16:1/16:1)' + 1/32) * 2*pi;
%!   x1 = sin (t1) - 0.8;
%!   y1 = cos (t1);
%!   x2 = sin (t2) + 0.8;
%!   y2 = cos (t2);
%!   vert = [x1, y1; x2, y2];
%!   fac = [1:8,NaN(1,8);9:24];
%!   hpa = patch ("Faces",fac, "Vertices",vert, "FaceColor","r");
%!
%!   reset (hpa);
%!   assert (get (hpa, "faces"), get (0, "defaultpatchfaces"), tol);
%!   assert (get (hpa, "vertices"), get (0, "defaultpatchvertices"), tol);
%!   assert (get (hpa, "facevertexcdata"),
%!           get (0, "defaultpatchfacevertexcdata"), tol);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test  # surface object
%! hf = figure ("visible", "off");
%! unwind_protect
%!   tol = 20 * eps;
%!   hsu = surface (peaks, "edgecolor", "none");
%!
%!   reset (hsu);
%!   assert (get (hsu, "xdata"), get (0, "defaultsurfacexdata"), tol);
%!   assert (get (hsu, "ydata"), get (0, "defaultsurfaceydata"), tol);
%!   assert (get (hsu, "zdata"), get (0, "defaultsurfacezdata"), tol);
%!   assert (get (hsu, "edgecolor"), get (0, "defaultsurfaceedgecolor"), tol);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test  # image object
%! hf = figure ("visible", "off");
%! unwind_protect
%!   tol = 20 * eps;
%!   him = image (rand (10,10), "cdatamapping", "scaled");
%!
%!   reset (him);
%!   assert (get (him, "cdata"), get (0, "defaultimagecdata"), tol);
%!   assert (get (him, "cdatamapping"),
%!           get (0, "defaultimagecdatamapping"), tol);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test  # text object
%! hf = figure ("visible", "off");
%! unwind_protect
%!   tol = 20 * eps;
%!   hte = text (5, 5, "Hi!", "fontsize", 20 ,"color", "r");
%!
%!   reset (hte);
%!   assert (get (hte, "position"), get (0, "defaulttextposition"), tol);
%!   assert (get (hte, "fontsize"), get (0, "defaulttextfontsize"), tol);
%!   assert (get (hte, "color"), get (0, "defaulttextcolor"), tol);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test  # axes object
%! hf = figure ("visible", "off");
%! unwind_protect
%!   tol = 20 * eps;
%!   pos = get (0, "defaultaxesposition") * .5;
%!   hax = axes ("linewidth", 2, "position", pos);
%!   title ("Reset me, please!");
%!
%!   reset (hax);
%!   assert (get (hax, "linewidth"), get (0, "defaultaxeslinewidth"), tol);
%!   assert (get (hax, "position"), pos, tol);  # axes position is unchanged
%!   assert (get (hax, "default"), struct ());  # no more axes' defaults
%!   assert (get (get (hax, "title"), "string"), "");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test  # root object
%! set (0, "defaultfigurevisible", "off");
%! hf = figure ("visible", "off", "paperunits", "centimeters",
%!              "papertype", "a4");
%! unwind_protect
%!   reset (hf);
%!   assert (get (hf, "papertype"), get (0, "defaultfigurepapertype"));
%!   assert (get (hf, "paperunits"), "centimeters");  # paperunits is unchanged
%!   assert (get (hf, "visible"), get (0, "defaultfigurevisible"));
%! unwind_protect_cleanup
%!   close (hf);
%!   set (0, "defaultfigurevisible", "remove");
%! end_unwind_protect

*/

DEFMETHOD (set, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} set (@var{h}, @var{property}, @var{value}, @dots{})
@deftypefnx {} {} set (@var{h}, @{@var{properties}@}, @{@var{values}@})
@deftypefnx {} {} set (@var{h}, @var{pv})
@deftypefnx {} {@var{value_list} =} set (@var{h}, @var{property})
@deftypefnx {} {@var{all_value_list} =} set (@var{h})
Set named property values for the graphics handle (or vector of graphics
handles) @var{h}.

There are three ways to give the property names and values:

@itemize
@item as a comma-separated list of @var{property}, @var{value} pairs

Each @var{property} is a string containing the property name, each @var{value}
is a value of the appropriate type for the property.  When there are multiple
handles in @var{h}, each one is assigned the same @var{value}.  For example:

@example
@group
h = plot ([0, 1]);
set (h, 'color', 'green');
@end group
@end example

@item as a cell array of strings @var{properties} containing property names
and a cell array @var{values} containing property values.

In this case, the number of columns of @var{values} must match the number of
elements in @var{properties}.  The first column of @var{values} contains
values for the first entry in @var{properties}, etc.  The number of rows of
@var{values} must be 1 or match the number of elements of @var{h}.  In the
first case, each handle in @var{h} will be assigned the same values.  In the
second case, the first handle in @var{h} will be assigned the values from
the first row of @var{values} and so on.  For example:

@example
@group
h = plot ([0, 1; 1, 0]);
set (h, @{'color'@}, @{'green'; 'red'@});
@end group
@end example

@item as a structure @var{pv}

This is the same as the first case where the field names of @var{pv} represent
the property names, and the field values give the property values.  As with
the first case, it is only possible to set one value for a property which will
be applied to all handles in @var{h}.  For example:

@example
@group
h = plot ([0, 1]);
props.color = 'green';
set (h, props);
@end group
@end example
@end itemize

@code{set} is also used to query the list of values a named property will
take.  @code{@var{clist} = set (@var{h}, "property")} will return the list
of possible values for @qcode{"property"} in the cell list @var{clist}.
If no output variable is used then the list is formatted and printed to the
screen.

If no property is specified (@code{@var{slist} = set (@var{h})}) then a
structure @var{slist} is returned where the fieldnames are the properties of
the object @var{h} and the fields are the list of possible values for each
property.  If no output variable is used then the list is formatted and
printed to the screen.

For example,

@example
@group
hf = figure ();
set (hf, "paperorientation")
@result{}  [ landscape | @{portrait@} ]
@end group
@end example

@noindent
shows the paperorientation property can take two values with the default
being @qcode{"portrait"}.
@seealso{get}
@end deftypefn */)
{
  gh_manager& gh_mgr = interp.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  int nargin = args.length ();

  if (nargin == 0)
    print_usage ();

  octave_value retval;

  // get vector of graphics handles
  ColumnVector hcv = args(0).xvector_value ("set: H must be a graphics handle");

  bool request_drawnow = false;

  // loop over graphics objects
  for (octave_idx_type n = 0; n < hcv.numel (); n++)
    {
      graphics_object go = gh_mgr.get_object (hcv(n));

      if (! go)
        error ("set: invalid handle (= %g)", hcv(n));

      if (nargin == 3 && args(1).iscellstr () && args(2).iscell ())
        {
          if (args(2).cell_value ().rows () == 1)
            go.set (args(1).cellstr_value (), args(2).cell_value (), 0);
          else if (hcv.numel () == args(2).cell_value ().rows ())
            go.set (args(1).cellstr_value (), args(2).cell_value (), n);
          else
            error ("set: number of graphics handles must match number of "
                   "value rows (%" OCTAVE_IDX_TYPE_FORMAT " != "
                   "%" OCTAVE_IDX_TYPE_FORMAT ")",
                   hcv.numel (), args(2).cell_value ().rows ());
        }
      else if (nargin == 2 && args(1).isstruct ())
        go.set (args(1).map_value ());
      else if (nargin == 2 && args(1).is_string ())
        {
          std::string property = args(1).string_value ();
          std::transform (property.begin (), property.end (),
                          property.begin (), tolower);

          octave_map pmap = go.values_as_struct ();

          if (go.has_readonly_property (property))
            if (nargout != 0)
              retval = Matrix ();
            else
              octave_stdout << "set: " << property
                            <<" is read-only" << std::endl;
          else if (pmap.isfield (property))
            {
              if (nargout != 0)
                retval = pmap.getfield (property)(0);
              else
                {
                  std::string s = go.value_as_string (property);

                  octave_stdout << s;
                }
            }
          else
            error (R"(set: unknown property "%s")", property.c_str ());
        }
      else if (nargin == 1)
        {
          if (nargout != 0)
            retval = go.values_as_struct ();
          else
            {
              std::string s = go.values_as_string ();

              octave_stdout << s;
            }
        }
      else
        {
          go.set (args.splice (0, 1));
        }

      request_drawnow = true;
    }

  if (request_drawnow)
    Vdrawnow_requested = true;

  return retval;
}

static std::string
get_graphics_object_type (double val)
{
  std::string retval;

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (val);

  if (! go)
    error ("get: invalid handle (= %g)", val);

  return go.type ();
}

DEFMETHOD (get, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} get (@var{h})
@deftypefnx {} {@var{val} =} get (@var{h}, @var{p})
Return the value of the named property @var{p} from the graphics handle
@var{h}.

If @var{p} is omitted, return the complete property list for @var{h}.

If @var{h} is a vector, return a cell array including the property values or
lists respectively.
@seealso{set}
@end deftypefn */)
{
  gh_manager& gh_mgr = interp.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  if (args(0).isempty ())
    return ovl (Matrix ());

  ColumnVector hcv = args(0).xvector_value ("get: H must be a graphics handle");

  octave_idx_type hcv_len = hcv.numel ();

  if (nargin == 1 && hcv_len > 1)
    {
      std::string typ0 = get_graphics_object_type (hcv(0));

      for (octave_idx_type n = 1; n < hcv_len; n++)
        {
          std::string typ = get_graphics_object_type (hcv(n));

          if (typ != typ0)
            error ("get: vector of handles must all have the same type");
        }
    }

  octave_value retval;
  Cell vals;
  bool use_cell_format = false;

  if (nargin > 1 && args(1).iscellstr ())
    {
      Array<std::string> plist = args(1).cellstr_value ();

      octave_idx_type plen = plist.numel ();

      use_cell_format = true;

      vals.resize (dim_vector (hcv_len, plen));

      for (octave_idx_type n = 0; n < hcv_len; n++)
        {
          graphics_object go = gh_mgr.get_object (hcv(n));

          if (! go)
            error ("get: invalid handle (= %g)", hcv(n));

          for (octave_idx_type m = 0; m < plen; m++)
            {
              caseless_str property = plist(m);

              vals(n, m) = go.get (property);
            }
        }
    }
  else
    {
      caseless_str property;

      if (nargin > 1)
        property = args(
                     1).xstring_value ("get: second argument must be property name or cell array of property names");

      vals.resize (dim_vector (hcv_len, 1));

      for (octave_idx_type n = 0; n < hcv_len; n++)
        {
          graphics_object go = gh_mgr.get_object (hcv(n));

          if (! go)
            error ("get: invalid handle (= %g)", hcv(n));

          if (nargin == 1)
            vals(n) = go.get ();
          else
            vals(n) = go.get (property);
        }
    }

  if (use_cell_format)
    retval = vals;
  else
    {
      octave_idx_type vals_len = vals.numel ();

      if (vals_len == 0)
        retval = Matrix ();
      else if (vals_len == 1)
        retval = vals(0);
      else if (vals_len > 1 && nargin == 1)
        {
          OCTAVE_LOCAL_BUFFER (octave_scalar_map, tmp, vals_len);

          for (octave_idx_type n = 0; n < vals_len; n++)
            tmp[n] = vals(n).scalar_map_value ();

          retval = octave_map::cat (0, vals_len, tmp);
        }
      else
        retval = vals;
    }

  return retval;
}

/*
%!assert (get (findobj (0, "Tag", "nonexistenttag"), "nonexistentproperty"), [])
*/

// Return all properties from the graphics handle @var{h}.
// If @var{h} is a vector, return a cell array including the
// property values or lists respectively.

DEFMETHOD (__get__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{props} =} __get__ (@var{h})
Undocumented internal function.
@end deftypefn */)
{
  gh_manager& gh_mgr = interp.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  if (args.length () != 1)
    print_usage ();

  ColumnVector hcv = args(0).xvector_value ("get: H must be a graphics handle");

  octave_idx_type hcv_len = hcv.numel ();

  Cell vals (dim_vector (hcv_len, 1));

//  vals.resize (dim_vector (hcv_len, 1));

  for (octave_idx_type n = 0; n < hcv_len; n++)
    {
      graphics_object go = gh_mgr.get_object (hcv(n));

      if (! go)
        error ("get: invalid handle (= %g)", hcv(n));

      // Disable "Octave:deprecated-property" warnings
      int state = toggle_warn ("Octave:deprecated-property", false);

      vals(n) = go.get (true);

      toggle_warn ("Octave:deprecated-property", true, state);
    }

  octave_idx_type vals_len = vals.numel ();

  if (vals_len > 1)
    return ovl (vals);
  else if (vals_len == 1)
    return ovl (vals(0));
  else
    return ovl ();
}

static octave_value
make_graphics_object (const std::string& go_name,
                      bool integer_figure_handle,
                      const octave_value_list& args)
{
  octave_value retval;

  double val = octave::numeric_limits<double>::NaN ();

  octave_value_list xargs = args.splice (0, 1);

  caseless_str p ("parent");

  // Remove all "parent" property overrides of the first argument to function
  // and accept only the last one (bug #55322).
  for (int i = 0; i < xargs.length (); i += 2)
    {
      if (xargs(i).is_string () && p.compare (xargs(i).string_value ()))
        {
          if (i >= (xargs.length () - 1))
            error ("__go_%s__: missing value for parent property",
                   go_name.c_str ());

          val = xargs(i+1).double_value ();

          xargs = xargs.splice (i, 2);
          i -= 2;
        }
    }

  if (octave::math::isnan (val))
    val = args(0).xdouble_value ("__go_%s__: invalid parent", go_name.c_str ());

  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  graphics_handle parent = gh_mgr.lookup (val);

  if (! parent.ok ())
    error ("__go_%s__: invalid parent", go_name.c_str ());

  graphics_handle h;

  try
    {
      h = gh_mgr.make_graphics_handle (go_name, parent,
                                       integer_figure_handle, false, false);
    }
  catch (octave::execution_exception& ee)
    {
      error (ee, "__go_%s__: %s, unable to create graphics handle",
             go_name.c_str (), ee.message ().c_str ());
    }

  try
    {
      xset (h, xargs);
    }
  catch (octave::execution_exception& ee)
    {
      delete_graphics_object (h);
      error (ee, "__go_%s__: %s, unable to create graphics handle",
             go_name.c_str (), ee.message ().c_str ());
    }

  adopt (parent, h);

  xcreatefcn (h);
  xinitialize (h);

  retval = h.value ();

  Vdrawnow_requested = true;

  return retval;
}

DEFMETHOD (__go_figure__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hfig} =} __go_figure__ (@var{fignum})
Undocumented internal function.
@end deftypefn */)
{
  gh_manager& gh_mgr = interp.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  if (args.length () == 0)
    print_usage ();

  double val = args(0).xdouble_value ("__go_figure__: figure number must be a double value");

  octave_value retval;

  if (isfigure (val))
    {
      graphics_handle h = gh_mgr.lookup (val);

      xset (h, args.splice (0, 1));

      retval = h.value ();
    }
  else
    {
      bool int_fig_handle = true;

      octave_value_list xargs = args.splice (0, 1);

      graphics_handle h = octave::numeric_limits<double>::NaN ();

      if (octave::math::isnan (val))
        {
          caseless_str pname ("integerhandle");

          for (int i = 0; i < xargs.length (); i++)
            {
              if (xargs(i).is_string ()
                  && pname.compare (xargs(i).string_value ()))
                {
                  if (i < (xargs.length () - 1))
                    {
                      std::string pval = xargs(i+1).string_value ();

                      caseless_str on ("on");
                      int_fig_handle = on.compare (pval);
                      xargs = xargs.splice (i, 2);

                      break;
                    }
                }
            }

          h = gh_mgr.make_graphics_handle ("figure", 0, int_fig_handle,
                                           false, false);

          if (! int_fig_handle)
            {
              // We need to initialize the integerhandle property
              // without calling the set_integerhandle method,
              // because doing that will generate a new handle value...
              graphics_object go = gh_mgr.get_object (h);
              go.get_properties ().init_integerhandle ("off");
            }
        }
      else if (val > 0 && octave::math::x_nint (val) == val)
        h = gh_mgr.make_figure_handle (val, false);

      if (! h.ok ())
        error ("__go_figure__: failed to create figure handle");

      try
        {
          xset (h, xargs);
        }
      catch (octave::execution_exception& ee)
        {
          delete_graphics_object (h);
          error (ee, "__go_figure__: unable to create figure handle");
        }

      adopt (0, h);

      gh_mgr.push_figure (h);

      xcreatefcn (h);
      xinitialize (h);

      retval = h.value ();
    }

  return retval;
}

#define GO_BODY(TYPE)                                                   \
  gh_manager& gh_mgr = interp.get_gh_manager ();                        \
                                                                        \
  octave::autolock guard (gh_mgr.graphics_lock ());                     \
                                                                        \
  if (args.length () == 0)                                              \
    print_usage ();                                                     \
                                                                        \
  return octave_value (make_graphics_object (#TYPE, false, args));      \

int
calc_dimensions (const graphics_object& go)
{
  int nd = 2;

  if (go.isa ("surface"))
    nd = 3;
  else if ((go.isa ("line") || go.isa ("patch") || go.isa ("scatter"))
           && ! go.get ("zdata").isempty ())
    nd = 3;
  else
    {
      Matrix kids = go.get_properties ().get_children ();

      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      for (octave_idx_type i = 0; i < kids.numel (); i++)
        {
          graphics_handle hkid = gh_mgr.lookup (kids(i));

          if (hkid.ok ())
            {
              const graphics_object& kid = gh_mgr.get_object (hkid);

              if (kid.valid_object ())
                nd = calc_dimensions (kid);

              if (nd == 3)
                break;
            }
        }
    }

  return nd;
}

DEFMETHOD (__calc_dimensions__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{ndims} =} __calc_dimensions__ (@var{axes})
Internal function.

Determine the number of dimensions in a graphics object, either 2 or 3.
@end deftypefn */)
{
  gh_manager& gh_mgr = interp.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  if (args.length () != 1)
    print_usage ();

  double h = args(0).xdouble_value ("__calc_dimensions__: first argument must be a graphics handle");

  return ovl (calc_dimensions (gh_mgr.get_object (h)));
}

DEFMETHOD (__go_axes__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hax} =} __go_axes__ (@var{parent})
Undocumented internal function.
@end deftypefn */)
{
  GO_BODY (axes);
}

DEFMETHOD (__go_line__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hl} =} __go_line__ (@var{parent})
Undocumented internal function.
@end deftypefn */)
{
  GO_BODY (line);
}

DEFMETHOD (__go_text__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{ht} =} __go_text__ (@var{parent})
Undocumented internal function.
@end deftypefn */)
{
  GO_BODY (text);
}

DEFMETHOD (__go_image__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hi} =} __go_image__ (@var{parent})
Undocumented internal function.
@end deftypefn */)
{
  GO_BODY (image);
}

DEFMETHOD (__go_surface__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hs} =} __go_surface__ (@var{parent})
Undocumented internal function.
@end deftypefn */)
{
  GO_BODY (surface);
}

DEFMETHOD (__go_patch__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hp} =} __go_patch__ (@var{parent})
Undocumented internal function.
@end deftypefn */)
{
  GO_BODY (patch);
}

DEFMETHOD (__go_scatter__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hs} =} __go_scatter__ (@var{parent})
Undocumented internal function.
@end deftypefn */)
{
  GO_BODY (scatter);
}

DEFMETHOD (__go_light__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hl} =} __go_light__ (@var{parent})
Undocumented internal function.
@end deftypefn */)
{
  GO_BODY (light);
}

DEFMETHOD (__go_hggroup__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hgg} =} __go_hggroup__ (@var{parent})
Undocumented internal function.
@end deftypefn */)
{
  GO_BODY (hggroup);
}

DEFMETHOD (__go_uimenu__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hui} =} __go_uimenu__ (@var{parent})
Undocumented internal function.
@end deftypefn */)
{
  GO_BODY (uimenu);
}

DEFMETHOD (__go_uicontrol__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hui} =} __go_uicontrol__ (@var{parent})
Undocumented internal function.
@end deftypefn */)
{
  GO_BODY (uicontrol);
}

DEFMETHOD (__go_uibuttongroup__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hui} =} __go_uibuttongroup__ (@var{parent})
Undocumented internal function.
@end deftypefn */)
{
  GO_BODY (uibuttongroup);
}

DEFMETHOD (__go_uipanel__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hui} =} __go_uipanel__ (@var{parent})
Undocumented internal function.
@end deftypefn */)
{
  GO_BODY (uipanel);
}

DEFMETHOD (__go_uicontextmenu__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hui} =} __go_uicontextmenu__ (@var{parent})
Undocumented internal function.
@end deftypefn */)
{
  GO_BODY (uicontextmenu);
}

DEFMETHOD (__go_uitable__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hui} =} __go_uitable__ (@var{parent})
Undocumented internal function.
@end deftypefn */)
{
  GO_BODY (uitable);
}

DEFMETHOD (__go_uitoolbar__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hui} =} __go_uitoolbar__ (@var{parent})
Undocumented internal function.
@end deftypefn */)
{
  GO_BODY (uitoolbar);
}

DEFMETHOD (__go_uipushtool__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hui} =} __go_uipushtool__ (@var{parent})
Undocumented internal function.
@end deftypefn */)
{
  GO_BODY (uipushtool);
}

DEFMETHOD (__go_uitoggletool__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hui} =} __go_uitoggletool__ (@var{parent})
Undocumented internal function.
@end deftypefn */)
{
  GO_BODY (uitoggletool);
}

DEFMETHOD (__go_delete__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __go_delete__ (@var{h})
Undocumented internal function.
@end deftypefn */)
{
  gh_manager& gh_mgr = interp.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  if (args.length () != 1)
    print_usage ();

  graphics_handle h = octave::numeric_limits<double>::NaN ();

  const NDArray vals = args(0).xarray_value ("delete: invalid graphics object");

  // Check all the handles to delete are valid first,
  // as callbacks might delete one of the handles we later want to delete.
  for (octave_idx_type i = 0; i < vals.numel (); i++)
    {
      h = gh_mgr.lookup (vals(i));

      if (! h.ok ())
        error ("delete: invalid graphics object (= %g)", vals(i));
    }

  delete_graphics_objects (vals);

  return ovl ();
}

DEFMETHOD (__go_handles__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hg_list} =} __go_handles__ (@var{show_hidden})
Undocumented internal function.
@end deftypefn */)
{
  gh_manager& gh_mgr = interp.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  bool show_hidden = false;

  if (args.length () > 0)
    show_hidden = args(0).bool_value ();

  return ovl (gh_mgr.handle_list (show_hidden));
}

DEFMETHOD (__go_figure_handles__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{hfig_list} =} __go_figure_handles__ (@var{show_hidden})
Undocumented internal function.
@end deftypefn */)
{
  gh_manager& gh_mgr = interp.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  bool show_hidden = false;

  if (args.length () > 0)
    show_hidden = args(0).bool_value ();

  return ovl (gh_mgr.figure_handle_list (show_hidden));
}

DEFMETHOD (__go_execute_callback__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} __go_execute_callback__ (@var{h}, @var{name})
@deftypefnx {} {} __go_execute_callback__ (@var{h}, @var{name}, @var{param})
Undocumented internal function.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  const NDArray vals = args(0).xarray_value ("__go_execute_callback__: invalid graphics object");

  std::string name = args(1).xstring_value ("__go_execute_callback__: invalid callback name");

  gh_manager& gh_mgr = interp.get_gh_manager ();

  for (octave_idx_type i = 0; i < vals.numel (); i++)
    {
      double val = vals(i);

      graphics_handle h = gh_mgr.lookup (val);

      if (! h.ok ())
        error ("__go_execute_callback__: invalid graphics object (= %g)", val);

      if (nargin == 2)
        gh_mgr.execute_callback (h, name);
      else
        gh_mgr.execute_callback (h, name, args(2));
    }

  return ovl ();
}

DEFMETHOD (__go_post_callback__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} __go_post_callback__ (@var{h}, @var{name})
@deftypefnx {} {} __go_post_callback__ (@var{h}, @var{name}, @var{param})
Undocumented internal function.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  const NDArray vals = args(0).xarray_value ("__go_post_callback__: invalid graphics object");

  std::string name = args(1).xstring_value ("__go_post_callback__: invalid callback name");

  gh_manager& gh_mgr = interp.get_gh_manager ();

  for (octave_idx_type i = 0; i < vals.numel (); i++)
    {
      double val = vals(i);

      graphics_handle h = gh_mgr.lookup (val);

      if (! h.ok ())
        error ("__go_execute_callback__: invalid graphics object (= %g)", val);

      if (nargin == 2)
        gh_mgr.post_callback (h, name);
      else
        gh_mgr.post_callback (h, name, args(2));
    }

  return ovl ();
}

DEFMETHOD (__image_pixel_size__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{sz} =} __image_pixel_size__ (@var{h})
Internal function: returns the pixel size of the image in normalized units.
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  gh_manager& gh_mgr = interp.get_gh_manager ();

  double h = args(0).xdouble_value ("__image_pixel_size__: argument is not a handle");

  graphics_object go = gh_mgr.get_object (h);

  if (! go || ! go.isa ("image"))
    error ("__image_pixel_size__: object is not an image");

  image::properties& ip
    = dynamic_cast<image::properties&> (go.get_properties ());

  Matrix dp = Matrix (1, 2);
  dp(0) = ip.pixel_xsize ();
  dp(1) = ip.pixel_ysize ();
  return ovl (dp);
}

DEFMETHOD (available_graphics_toolkits, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{toolkits} =} available_graphics_toolkits ()
Return a cell array of registered graphics toolkits.
@seealso{graphics_toolkit, register_graphics_toolkit}
@end deftypefn */)
{
  gh_manager& gh_mgr = interp.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  octave::gtk_manager& gtk_mgr = interp.get_gtk_manager ();

  return ovl (gtk_mgr.available_toolkits_list ());
}

DEFMETHOD (register_graphics_toolkit, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} register_graphics_toolkit ("@var{toolkit}")
List @var{toolkit} as an available graphics toolkit.

Programming Note: No input validation is done on the input string; it is simply
added to the list of possible graphics toolkits.
@seealso{available_graphics_toolkits}
@end deftypefn */)
{
  gh_manager& gh_mgr = interp.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  if (args.length () != 1)
    print_usage ();

  std::string name = args(0).xstring_value ("register_graphics_toolkit: TOOLKIT must be a string");

  octave::gtk_manager& gtk_mgr = interp.get_gtk_manager ();

  gtk_mgr.register_toolkit (name);

  return ovl ();
}

DEFMETHOD (loaded_graphics_toolkits, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{toolkits} =} loaded_graphics_toolkits ()
Return a cell array of the currently loaded graphics toolkits.
@seealso{available_graphics_toolkits}
@end deftypefn */)
{
  gh_manager& gh_mgr = interp.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  octave::gtk_manager& gtk_mgr = interp.get_gtk_manager ();

  return ovl (gtk_mgr.loaded_toolkits_list ());
}

DEFMETHOD (__show_figure__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __show_figure__ (@var{n})
Undocumented internal function.
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  gh_manager& gh_mgr = interp.get_gh_manager ();

  double h = args(0).xdouble_value ("__show_figure__: invalid handle H");

  graphics_handle gh = gh_mgr.lookup (h);

  if (! gh.ok ())
    error ("__show_figure__: invalid graphics object (= %g)", h);

  graphics_object go = gh_mgr.get_object (gh);

  figure::properties& fprops
    = dynamic_cast<figure::properties&> (go.get_properties ());

  fprops.get_toolkit ().show_figure (go);

  return ovl ();
}

DEFMETHOD (drawnow, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} drawnow ()
@deftypefnx {} {} drawnow ("expose")
@deftypefnx {} {} drawnow (@var{term}, @var{file}, @var{debug_file})
Update figure windows and their children.

The event queue is flushed and any callbacks generated are executed.

With the optional argument @qcode{"expose"}, only graphic objects are
updated and no other events or callbacks are processed.

The third calling form of @code{drawnow} is for debugging and is
undocumented.
@seealso{refresh}
@end deftypefn */)
{
  if (args.length () > 3)
    print_usage ();

  octave::unwind_protect_var<bool> restore_var (Vdrawnow_requested, false);

  // Redraw unless we are in the middle of a deletion.

  if (! delete_executing)
    {
      gh_manager& gh_mgr = interp.get_gh_manager ();

      octave::autolock guard (gh_mgr.graphics_lock ());

      if (args.length () <= 1)
        {
          // First process events so that the redraw happens when all
          // objects are in their definite state.
          bool do_events = true;

          if (args.length () == 1)
            {
              caseless_str val (args(0).xstring_value ("drawnow: first argument must be a string"));

              if (val.compare ("expose"))
                do_events = false;
              else
                error ("drawnow: invalid argument, 'expose' is only valid option");
            }

          if (do_events)
            {
              gh_mgr.unlock ();

              gh_mgr.process_events ();

              gh_mgr.lock ();
            }

          Matrix hlist = gh_mgr.figure_handle_list (true);

          // Redraw modified figures
          for (int i = 0; i < hlist.numel (); i++)
            {
              graphics_handle h = gh_mgr.lookup (hlist(i));

              if (h.ok () && h != 0)
                {
                  graphics_object go = gh_mgr.get_object (h);
                  figure::properties& fprops
                    = dynamic_cast<figure::properties&> (go.get_properties ());

                  if (fprops.is_modified ())
                    {
                      if (fprops.is_visible ())
                        {
                          gh_mgr.unlock ();

                          fprops.get_toolkit ().redraw_figure (go);

                          gh_mgr.lock ();
                        }

                      fprops.set_modified (false);
                    }
                }

            }
        }
      else if (args.length () >= 2 && args.length () <= 3)
        {
          std::string term, file, debug_file;

          term = args(0).xstring_value ("drawnow: TERM must be a string");

          file = args(1).xstring_value ("drawnow: FILE must be a string");

          if (file.empty ())
            error ("drawnow: empty output ''");
          else if (file.length () == 1 && file[0] == '|')
            error ("drawnow: empty pipe '|'");
          else if (file[0] != '|')
            {
              std::size_t pos = file.find_last_of (octave::sys::file_ops::dir_sep_chars ());

              if (pos != std::string::npos)
                {
                  std::string dirname = file.substr (0, pos+1);

                  octave::sys::file_stat fs (dirname);

                  if (! fs || ! fs.is_dir ())
                    error ("drawnow: nonexistent directory '%s'",
                           dirname.c_str ());

                }
            }

          debug_file = (args.length () > 2 ? args(2).xstring_value ("drawnow: DEBUG_FILE must be a string") :
                        "");

          graphics_handle h = gcf ();

          if (! h.ok ())
            error ("drawnow: nothing to draw");

          graphics_object go = gh_mgr.get_object (h);

          gh_mgr.unlock ();

          go.get_toolkit ().print_figure (go, term, file, debug_file);

          gh_mgr.lock ();
        }
    }

  return ovl ();
}

DEFMETHOD (addlistener, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} addlistener (@var{h}, @var{prop}, @var{fcn})
Register @var{fcn} as listener for the property @var{prop} of the graphics
object @var{h}.

Property listeners are executed (in order of registration) when the property
is set.  The new value is already available when the listeners are executed.

@var{prop} must be a string naming a valid property in @var{h}.

@var{fcn} can be a function handle, a string or a cell array whose first
element is a function handle.  If @var{fcn} is a function handle, the
corresponding function should accept at least 2 arguments, that will be
set to the object handle and the empty matrix respectively.  If @var{fcn}
is a string, it must be any valid octave expression.  If @var{fcn} is a cell
array, the first element must be a function handle with the same signature
as described above.  The next elements of the cell array are passed
as additional arguments to the function.

Example:

@example
@group
function my_listener (h, dummy, p1)
  fprintf ("my_listener called with p1=%s\n", p1);
endfunction

addlistener (gcf, "position", @{@@my_listener, "my string"@})
@end group
@end example

@seealso{dellistener, addproperty, hggroup}
@end deftypefn */)
{
  gh_manager& gh_mgr = interp.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  int nargin = args.length ();

  if (nargin < 3 || nargin > 4)
    print_usage ();

  double h = args(0).xdouble_value ("addlistener: invalid handle H");

  std::string pname = args(1).xstring_value ("addlistener: PROP must be a string");

  graphics_handle gh = gh_mgr.lookup (h);

  if (! gh.ok ())
    error ("addlistener: invalid graphics object (= %g)", h);

  graphics_object go = gh_mgr.get_object (gh);

  go.add_property_listener (pname, args(2), GCB_POSTSET);

  if (args.length () == 4)
    {
      caseless_str persistent = args(3).string_value ();
      if (persistent.compare ("persistent"))
        go.add_property_listener (pname, args(2), GCB_PERSISTENT);
    }

  return ovl ();
}

DEFMETHOD (dellistener, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} dellistener (@var{h}, @var{prop}, @var{fcn})
Remove the registration of @var{fcn} as a listener for the property
@var{prop} of the graphics object @var{h}.

The function @var{fcn} must be the same variable (not just the same value),
as was passed to the original call to @code{addlistener}.

If @var{fcn} is not defined then all listener functions of @var{prop}
are removed.

Example:

@example
@group
function my_listener (h, dummy, p1)
  fprintf ("my_listener called with p1=%s\n", p1);
endfunction

c = @{@@my_listener, "my string"@};
addlistener (gcf, "position", c);
dellistener (gcf, "position", c);
@end group
@end example

@seealso{addlistener}
@end deftypefn */)
{
  gh_manager& gh_mgr = interp.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  if (args.length () < 2 || args.length () > 3)
    print_usage ();

  double h = args(0).xdouble_value ("dellistener: invalid handle");

  std::string pname = args(1).xstring_value ("dellistener: PROP must be a string");

  graphics_handle gh = gh_mgr.lookup (h);

  if (! gh.ok ())
    error ("dellistener: invalid graphics object (= %g)", h);

  graphics_object go = gh_mgr.get_object (gh);

  if (args.length () == 2)
    go.delete_property_listener (pname, octave_value (), GCB_POSTSET);
  else
    {
      if (args(2).is_string ()
          && args(2).string_value () == "persistent")
        {
          go.delete_property_listener (pname, octave_value (),
                                       GCB_PERSISTENT);
          go.delete_property_listener (pname, octave_value (),
                                       GCB_POSTSET);
        }
      else
        go.delete_property_listener (pname, args(2), GCB_POSTSET);
    }

  return ovl ();
}

DEFMETHOD (addproperty, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} addproperty (@var{name}, @var{h}, @var{type})
@deftypefnx {} {} addproperty (@var{name}, @var{h}, @var{type}, @var{arg}, @dots{})
Create a new property named @var{name} in graphics object @var{h}.

@var{type} determines the type of the property to create.  @var{args}
usually contains the default value of the property, but additional
arguments might be given, depending on the type of the property.

The supported property types are:

@table @code
@item string
A string property.  @var{arg} contains the default string value.

@item any
An @nospell{un-typed} property.  This kind of property can hold any octave
value.  @var{args} contains the default value.

@item radio
A string property with a limited set of accepted values.  The first
argument must be a string with all accepted values separated by
a vertical bar ('|').  The default value can be marked by enclosing
it with a '@{' '@}' pair.  The default value may also be given as
an optional second string argument.

@item boolean
A boolean property.  This property type is equivalent to a radio
property with "on|off" as accepted values.  @var{arg} contains
the default property value.

@item double
A scalar double property.  @var{arg} contains the default value.

@item handle
A handle property.  This kind of property holds the handle of a
graphics object.  @var{arg} contains the default handle value.
When no default value is given, the property is initialized to
the empty matrix.

@item data
A data (matrix) property.  @var{arg} contains the default data
value.  When no default value is given, the data is initialized to
the empty matrix.

@item color
A color property.  @var{arg} contains the default color value.
When no default color is given, the property is set to black.
An optional second string argument may be given to specify an
additional set of accepted string values (like a radio property).
@end table

@var{type} may also be the concatenation of a core object type and
a valid property name for that object type.  The property created
then has the same characteristics as the referenced property (type,
possible values, hidden state@dots{}).  This allows one to clone an
existing property into the graphics object @var{h}.

Examples:

@example
@group
addproperty ("my_property", gcf, "string", "a string value");
addproperty ("my_radio", gcf, "radio", "val_1|val_2|@{val_3@}");
addproperty ("my_style", gcf, "linelinestyle", "--");
@end group
@end example

@seealso{addlistener, hggroup}
@end deftypefn */)
{
  gh_manager& gh_mgr = interp.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  if (args.length () < 3)
    print_usage ();

  std::string name = args(0).xstring_value ("addproperty: NAME must be a string");

  double h = args(1).xdouble_value ("addproperty: invalid handle H");

  graphics_handle gh = gh_mgr.lookup (h);

  if (! gh.ok ())
    error ("addproperty: invalid graphics object (= %g)", h);

  graphics_object go = gh_mgr.get_object (gh);

  std::string type = args(2).xstring_value ("addproperty: TYPE must be a string");

  if (go.get_properties ().has_property (name))
    error ("addproperty: a '%s' property already exists in the graphics object",
           name.c_str ());

  property p = property::create (name, gh, type, args.splice (0, 3));

  go.get_properties ().insert_property (name, p);

  return ovl ();
}

octave_value
get_property_from_handle (double handle, const std::string& property,
                          const std::string& fcn)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  graphics_object go = gh_mgr.get_object (handle);

  if (! go)
    error ("%s: invalid handle (= %g)", fcn.c_str (), handle);

  return go.get (caseless_str (property));
}

bool
set_property_in_handle (double handle, const std::string& property,
                        const octave_value& arg, const std::string& fcn)
{
  gh_manager& gh_mgr = octave::__get_gh_manager__ ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  graphics_object go = gh_mgr.get_object (handle);

  if (! go)
    error ("%s: invalid handle (= %g)", fcn.c_str (), handle);

  go.set (caseless_str (property), arg);

  return true;
}

static bool
compare_property_values (const octave_value& ov1, const octave_value& ov2)
{
  octave_value_list args(2);

  args(0) = ov1;
  args(1) = ov2;

  octave_value_list result = octave::feval ("isequal", args, 1);

  if (result.length () > 0)
    return result(0).bool_value ();

  return false;
}

static std::map<uint32_t, bool> waitfor_results;

static void
cleanup_waitfor_id (uint32_t id)
{
  waitfor_results.erase (id);
}

static void
do_cleanup_waitfor_listener (const octave_value& listener,
                             listener_mode mode = GCB_POSTSET)
{
  Cell c = listener.cell_value ();

  if (c.numel () >= 4)
    {
      double h = c(2).double_value ();

      caseless_str pname = c(3).string_value ();

      gh_manager& gh_mgr = octave::__get_gh_manager__ ();

      octave::autolock guard (gh_mgr.graphics_lock ());

      graphics_handle gh = gh_mgr.lookup (h);

      if (gh.ok ())
        {
          graphics_object go = gh_mgr.get_object (gh);

          if (go.get_properties ().has_property (pname))
            {
              go.get_properties ().delete_listener (pname, listener, mode);

              if (mode == GCB_POSTSET)
                go.get_properties ().delete_listener (pname, listener,
                                                      GCB_PERSISTENT);
            }
        }
    }
}

static void
cleanup_waitfor_postset_listener (const octave_value& listener)
{ do_cleanup_waitfor_listener (listener, GCB_POSTSET); }

static void
cleanup_waitfor_predelete_listener (const octave_value& listener)
{ do_cleanup_waitfor_listener (listener, GCB_PREDELETE); }

DECLARE_STATIC_FUNX (waitfor_listener, args, )
{
  if (args.length () > 3)
    {
      uint32_t id = args(2).uint32_scalar_value ().value ();

      if (args.length () > 5)
        {
          double h = args(0).double_value ();

          caseless_str pname = args(4).string_value ();

          gh_manager& gh_mgr = octave::__get_gh_manager__ ();

          octave::autolock guard (gh_mgr.graphics_lock ());

          graphics_handle gh = gh_mgr.lookup (h);

          if (gh.ok ())
            {
              graphics_object go = gh_mgr.get_object (gh);
              octave_value pvalue = go.get (pname);

              if (compare_property_values (pvalue, args(5)))
                waitfor_results[id] = true;
            }
        }
      else
        waitfor_results[id] = true;
    }

  return ovl ();
}

DECLARE_STATIC_FUNX (waitfor_del_listener, args, )
{
  if (args.length () > 2)
    {
      uint32_t id = args(2).uint32_scalar_value ().value ();

      waitfor_results[id] = true;
    }

  return ovl ();
}

DEFMETHOD (waitfor, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} waitfor (@var{h})
@deftypefnx {} {} waitfor (@var{h}, @var{prop})
@deftypefnx {} {} waitfor (@var{h}, @var{prop}, @var{value})
@deftypefnx {} {} waitfor (@dots{}, "timeout", @var{timeout})
Suspend the execution of the current program until a condition is
satisfied on the graphics handle @var{h}.

While the program is suspended graphics events are still processed normally,
allowing callbacks to modify the state of graphics objects.  This function
is reentrant and can be called from a callback, while another @code{waitfor}
call is pending at the top-level.

In the first form, program execution is suspended until the graphics object
@var{h} is destroyed.  If the graphics handle is invalid or if @var{h} is
the root graphics handle and no property @var{prop} was provided, the function
returns immediately.

In the second form, execution is suspended until the graphics object is
destroyed or the property named @var{prop} is modified.  If the graphics
handle is invalid or the property does not exist, the function returns
immediately.

In the third form, execution is suspended until the graphics object is
destroyed or the property named @var{prop} is set to @var{value}.  The
function @code{isequal} is used to compare property values.  If the graphics
handle is invalid, the property does not exist or the property is already
set to @var{value}, the function returns immediately.

An optional timeout can be specified using the property @qcode{"timeout"}.
This timeout value is the number of seconds to wait for the condition to be
true.  @var{timeout} must be at least 1.  If a smaller value is specified, a
warning is issued and a value of 1 is used instead.  If the timeout value is
not an integer, it is truncated towards 0.

To define a condition on a property named @qcode{"timeout"}, use the string
@qcode{'@backslashchar{}timeout'} instead.

In all cases, typing CTRL-C stops program execution immediately.
@seealso{waitforbuttonpress, isequal}
@end deftypefn */)
{
  if (args.length () == 0)
    print_usage ();

  // return immediately if the graphics handle is invalid
  if (args(0).isempty ())
    return ovl ();

  double h = args(0).xdouble_value ("waitfor: invalid handle value");

  if (! ishghandle (h) || (h == 0 && args.length () == 1))
    return ovl ();

  caseless_str pname;

  octave::unwind_action cleanup_waitfor_id_action;
  octave::unwind_action cleanup_waitfor_postset_listener_action;
  octave::unwind_action cleanup_waitfor_predelete_listener_action;

  static uint32_t id_counter = 0;
  uint32_t id = 0;

  int max_arg_index = 0;
  int timeout_index = -1;

  double timeout = 0;

  gh_manager& gh_mgr = interp.get_gh_manager ();

  if (args.length () > 1)
    {
      pname = args(1).xstring_value ("waitfor: PROP must be a string");

      if (pname.empty ())
        error ("waitfor: PROP must be a non-empty string");

      if (pname != "timeout")
        {
          if (pname.compare (R"(\timeout)"))
            pname = "timeout";

          static octave_value wf_listener;

          if (! wf_listener.is_defined ())
            wf_listener
              = octave_value (new octave_builtin (waitfor_listener,
                                                  "waitfor_listener"));

          max_arg_index++;
          if (args.length () > 2)
            {
              if (args(2).is_string ())
                {
                  caseless_str s = args(2).string_value ();

                  if (s.compare ("timeout"))
                    timeout_index = 2;
                  else
                    max_arg_index++;
                }
              else
                max_arg_index++;
            }

          Cell listener (1, max_arg_index >= 2 ? 5 : 4);

          id = id_counter++;
          cleanup_waitfor_id_action.set (cleanup_waitfor_id, id);
          waitfor_results[id] = false;

          listener(0) = wf_listener;
          listener(1) = octave_uint32 (id);
          listener(2) = h;
          listener(3) = pname;

          if (max_arg_index >= 2)
            listener(4) = args(2);

          octave_value ov_listener (listener);

          octave::autolock guard (gh_mgr.graphics_lock ());

          graphics_handle gh = gh_mgr.lookup (h);

          if (gh.ok ())
            {
              graphics_object go = gh_mgr.get_object (gh);

              if (max_arg_index >= 2
                  && compare_property_values (go.get (pname), args(2)))
                waitfor_results[id] = true;
              else
                {
                  cleanup_waitfor_postset_listener_action.set
                  (cleanup_waitfor_postset_listener, ov_listener);

                  go.add_property_listener (pname, ov_listener, GCB_POSTSET);
                  go.add_property_listener (pname, ov_listener, GCB_PERSISTENT);

                  if (go.get_properties ().has_dynamic_property (pname))
                    {
                      static octave_value wf_del_listener;

                      if (! wf_del_listener.is_defined ())
                        wf_del_listener
                          = octave_value (new octave_builtin
                                          (waitfor_del_listener,
                                           "waitfor_del_listener"));

                      Cell del_listener (1, 4);

                      del_listener(0) = wf_del_listener;
                      del_listener(1) = octave_uint32 (id);
                      del_listener(2) = h;
                      del_listener(3) = pname;

                      octave_value ov_del_listener (del_listener);

                      cleanup_waitfor_predelete_listener_action.set
                      (cleanup_waitfor_predelete_listener, ov_del_listener);

                      go.add_property_listener (pname, ov_del_listener,
                                                GCB_PREDELETE);
                    }
                }
            }
        }
    }

  if (timeout_index < 0 && args.length () > (max_arg_index + 1))
    {
      caseless_str s = args(max_arg_index +
                            1).xstring_value ("waitfor: invalid parameter, expected 'timeout'");

      if (! s.compare ("timeout"))
        error ("waitfor: invalid parameter '%s'", s.c_str ());

      timeout_index = max_arg_index + 1;
    }

  if (timeout_index >= 0)
    {
      if (args.length () <= (timeout_index + 1))
        error ("waitfor: missing TIMEOUT value");

      timeout = args(timeout_index + 1).xscalar_value ("waitfor: TIMEOUT must be a scalar >= 1");

      if (timeout < 1)
        {
          warning ("waitfor: TIMEOUT value must be >= 1, using 1 instead");
          timeout = 1;
        }
    }

  // FIXME: There is still a "hole" in the following loop.  The code
  //        assumes that an object handle is unique, which is a fair
  //        assumption, except for figures.  If a figure is destroyed
  //        then recreated with the same figure ID, within the same
  //        run of event hooks, then the figure destruction won't be
  //        caught and the loop will not stop.  This is an unlikely
  //        possibility in practice, though.
  //
  //        Using deletefcn callback is also unreliable as it could be
  //        modified during a callback execution and the waitfor loop
  //        would not stop.
  //
  //        The only "good" implementation would require object
  //        listeners, similar to property listeners.

  octave::sys::time start;

  if (timeout > 0)
    start.stamp ();

  while (true)
    {
      if (true)
        {
          octave::autolock guard (gh_mgr.graphics_lock ());

          graphics_handle gh = gh_mgr.lookup (h);

          if (gh.ok ())
            {
              if (! pname.empty () && waitfor_results[id])
                break;
            }
          else
            break;
        }

      octave::sleep (0.1);  // FIXME: really needed?

      octave_quit ();

      octave::command_editor::run_event_hooks ();

      if (timeout > 0)
        {
          octave::sys::time now;

          if (start + timeout < now)
            break;
        }
    }

  return ovl ();
}

DEFMETHOD (__zoom__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} __zoom__ (@var{axes}, @var{mode}, @var{factor})
@deftypefnx {} {} __zoom__ (@var{axes}, "out")
@deftypefnx {} {} __zoom__ (@var{axes}, "reset")
Undocumented internal function.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin != 2 && nargin != 3)
    print_usage ();

  double h = args(0).double_value ();

  gh_manager& gh_mgr = interp.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  graphics_handle handle = gh_mgr.lookup (h);

  if (! handle.ok ())
    error ("__zoom__: invalid handle");

  graphics_object ax = gh_mgr.get_object (handle);

  axes::properties& ax_props
    = dynamic_cast<axes::properties&> (ax.get_properties ());

  if (nargin == 2)
    {
      std::string opt = args(1).string_value ();

      if (opt == "out" || opt == "reset")
        {
          if (opt == "out")
            {
              ax_props.clear_zoom_stack ();
              Vdrawnow_requested = true;
            }
          else
            ax_props.clear_zoom_stack (false);
        }
    }
  else
    {
      std::string mode = args(1).string_value ();
      double factor = args(2).scalar_value ();

      ax_props.zoom (mode, factor);
      Vdrawnow_requested = true;
    }

  return ovl ();
}

DEFMETHOD (__get_frame__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{cdata} =} __get_frame__ (@var{hfig})
Internal function.

Return the pixel cdata of figure hfig in the form of a height-by-width-by-3
uint8 array.
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  double h = args(0).xdouble_value ("__get_frame__: HFIG is not a handle");

  gh_manager& gh_mgr = interp.get_gh_manager ();

  graphics_object go = gh_mgr.get_object (h);

  if (! go || ! go.isa ("figure"))
    error ("__get_frame__: HFIG is not a figure");

  // For Matlab compatibility, getframe must flush the event queue.
  gh_mgr.process_events ();

  return ovl (go.get_toolkit ().get_pixels (go));
}

DEFMETHOD (__get_position__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{pos} =} __get_position__ (@var{h}, @var{units})
Internal function.

Return the position of the graphics object @var{h} in the specified
@var{units}.
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  double h
    = args(0).xdouble_value ("__get_position__: H must be a graphics handle");

  std::string units
    = args(1).xstring_value ("__get_position__: UNITS must be a string");

  gh_manager& gh_mgr = interp.get_gh_manager ();

  graphics_object go = gh_mgr.get_object (h);

  if (h == 0 || ! go)
    error ("__get_position__: H must be a handle to a valid graphics object");

  graphics_object parent_go = gh_mgr.get_object (go.get_parent ());
  Matrix bbox = parent_go.get_properties ().get_boundingbox (true)
                .extract_n (0, 2, 1, 2);

  Matrix pos = convert_position (go.get ("position").matrix_value (),
                                 go.get ("units").string_value (),
                                 units, bbox);

  return ovl (pos);
}

DEFUN (__get_system_fonts__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{font_struct} =} __get_system_fonts__ ()
Internal function.
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  octave::text_renderer txt_renderer;

  return ovl (txt_renderer.get_system_fonts ());
}

OCTAVE_END_NAMESPACE(octave)
