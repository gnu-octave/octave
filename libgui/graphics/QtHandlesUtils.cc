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

#include <list>

#include <QApplication>
#include <QKeyEvent>
#include <QMouseEvent>

#include "Container.h"
#include "KeyMap.h"
#include "Object.h"
#include "QtHandlesUtils.h"
#include "qt-graphics-toolkit.h"

#include "oct-string.h"

#include "graphics.h"
#include "ov.h"

OCTAVE_BEGIN_NAMESPACE(octave)

namespace Utils
{

  QString
  fromStdString (const std::string& s)
  {
    return QString::fromUtf8 (s.c_str ());
  }

  std::string
  toStdString (const QString& s)
  {
    return std::string (s.toUtf8 ().data ());
  }

  QStringList
  fromStringVector (const string_vector& v)
  {
    QStringList l;
    octave_idx_type n = v.numel ();

    for (octave_idx_type i = 0; i < n; i++)
      l << fromStdString (v[i]);

    return l;
  }

  string_vector
  toStringVector (const QStringList& l)
  {
    string_vector v (l.length ());
    int i = 0;

    for (const auto& s : l)
      v[i++] = toStdString (s);

    return v;
  }

  Cell toCellString (const QStringList& l)
  {
    QStringList tmp = l;

    // don't get any empty lines from end of the list
    while ((tmp.length () > 0) && tmp.last ().isEmpty ())
      {
        tmp.removeLast ();
      }
    // no strings converts to a 1x1 cell with empty string
    if (tmp.isEmpty ())
      tmp += "";

    Cell v (toStringVector (tmp));
    return v;
  }

  template <typename T>
  QFont
  computeFont (const typename T::properties& props, int height)
  {
    QFont f (fromStdString (props.get_fontname ()));

    static std::map<std::string, QFont::Weight> weightMap;
    static std::map<std::string, QFont::Style> angleMap;
    static bool mapsInitialized = false;

    if (! mapsInitialized)
      {
        weightMap["normal"] = QFont::Normal;
        weightMap["bold"] = QFont::Bold;

        angleMap["normal"] = QFont::StyleNormal;
        angleMap["italic"] = QFont::StyleItalic;
        angleMap["oblique"] = QFont::StyleOblique;

        mapsInitialized = true;
      }

    f.setPointSizeF (props.get___fontsize_points__ (height));
    f.setWeight (weightMap[props.get_fontweight ()]);
    f.setStyle (angleMap[props.get_fontangle ()]);

    return f;
  }

  template QFont computeFont<uicontrol> (const uicontrol::properties& props,
                                         int height);

  template QFont computeFont<uipanel> (const uipanel::properties& props,
                                       int height);

  template QFont computeFont<uibuttongroup> (const uibuttongroup::properties&
                                             props,
                                             int height);

  template QFont computeFont<uitable> (const uitable::properties& props,
                                       int height);

  QColor
  fromRgb (const Matrix& rgb)
  {
    QColor c;

    if (rgb.numel () == 3)
      c.setRgbF (rgb(0), rgb(1), rgb(2));

    return c;
  }

  Matrix
  toRgb (const QColor& c)
  {
    Matrix rgb (1, 3);
    double *rgbData = rgb.fortran_vec ();

    // qreal is a typedef for double except for ARM CPU architectures
    // where it is a typedef for float (Bug #44970).
    qreal tmp[3];
    c.getRgbF (tmp, tmp+1, tmp+2);
    rgbData[0] = tmp[0]; rgbData[1] = tmp[1]; rgbData[2] = tmp[2];

    return rgb;
  }

  std::string
  figureSelectionType (QMouseEvent *event, bool isDoubleClick)
  {
    if (isDoubleClick)
      return "open";
    else
      {
        Qt::MouseButtons buttons = event->buttons ();
        Qt::KeyboardModifiers mods = event->modifiers ();

        if (mods == Qt::NoModifier)
          {
            if (buttons == Qt::LeftButton)
              return "normal";
            else if (buttons == Qt::RightButton)
              return "alt";
            else if (buttons == Qt::MiddleButton
                     || buttons == (Qt::LeftButton | Qt::RightButton))
              return "extend";
          }
        else if (buttons == Qt::LeftButton)
          {
            if (mods == Qt::ShiftModifier)
              return "extend";
            else if (mods == Qt::ControlModifier)
              return "alt";
          }
      }

    return "normal";
  }

  /*
    Two figureCurrentPoint() routines are required:
    1) Used for QMouseEvents where cursor position data is in callback from Qt.
    2) Used for QKeyEvents where cursor position must be determined.
  */
  Matrix
  figureCurrentPoint (const graphics_object& fig, QMouseEvent *event)
  {
    Object *tkFig = qt_graphics_toolkit::toolkitObject (fig);

    if (tkFig)
      {
        Container *c = tkFig->innerContainer ();

        if (c)
          {
            QPoint qp = c->mapFromGlobal (event->globalPos ());

            return tkFig->properties<figure> ().map_from_boundingbox (qp.x (),
                                                                      qp.y ());
          }
      }

    return Matrix (1, 2, 0.0);
  }

  Matrix
  figureCurrentPoint (const graphics_object& fig)
  {
    Object *tkFig = qt_graphics_toolkit::toolkitObject (fig);

    if (tkFig)
      {
        Container *c = tkFig->innerContainer ();

        if (c)
          {
            // FIXME: QCursor::pos() may give inaccurate results with
            //        asynchronous window systems like X11 over ssh.
            QPoint qp = c->mapFromGlobal (QCursor::pos ());

            return tkFig->properties<figure> ().map_from_boundingbox (qp.x (),
                                                                      qp.y ());
          }
      }

    return Matrix (1, 2, 0.0);
  }

  Qt::Alignment
  fromHVAlign (const std::string& halign, const std::string& valign)
  {
    Qt::Alignment flags;

    if (octave::string::strcmpi (halign, "left"))
      flags |= Qt::AlignLeft;
    else if (octave::string::strcmpi (halign, "center"))
      flags |= Qt::AlignHCenter;
    else if (octave::string::strcmpi (halign, "right"))
      flags |= Qt::AlignRight;
    else
      flags |= Qt::AlignLeft;

    if (octave::string::strcmpi (valign, "middle"))
      flags |= Qt::AlignVCenter;
    else if (octave::string::strcmpi (valign, "top"))
      flags |= Qt::AlignTop;
    else if (octave::string::strcmpi (valign, "bottom"))
      flags |= Qt::AlignBottom;
    else
      flags |= Qt::AlignVCenter;

    return flags;
  }

  QImage
  makeImageFromCData (const octave_value& v, int width, int height)
  {
    dim_vector dv (v.dims ());

    if (dv.ndims () == 3 && dv(2) == 3)
      {
        int w = qMin (dv(1), static_cast<octave_idx_type> (width));
        int h = qMin (dv(0), static_cast<octave_idx_type> (height));

        // If size mismatch, take data from center of CDATA and
        // place in in center of QImage.
        int x_img_off = (w < width ? (width - w) / 2 : 0);
        int y_img_off = (h < height ? (height - h) / 2 : 0);
        int x_cdat_off = (dv(1) > w ? (dv(1) - w) / 2 : 0);
        int y_cdat_off = (dv(0) > h ? (dv(0) - h) / 2 : 0);

        QImage img (width, height, QImage::Format_ARGB32);
        img.fill (qRgba (0, 0, 0, 0));

        if (v.is_uint8_type ())
          {
            uint8NDArray d = v.uint8_array_value ();

            for (int i = x_cdat_off; i < w + x_cdat_off; i++)
              for (int j = y_cdat_off; j < h + y_cdat_off; j++)
                {
                  int r = d(j, i, 0);
                  int g = d(j, i, 1);
                  int b = d(j, i, 2);
                  int a = 255;

                  img.setPixel (x_img_off + i - x_cdat_off,
                                y_img_off + j - y_cdat_off,
                                qRgba (r, g, b, a));
                }
          }
        else if (v.is_single_type ())
          {
            FloatNDArray f = v.float_array_value ();

            for (int i = x_cdat_off; i < w + x_cdat_off; i++)
              for (int j = y_cdat_off; j < h + y_cdat_off; j++)
                {
                  float r = f(j, i, 0);
                  float g = f(j, i, 1);
                  float b = f(j, i, 2);
                  int a = (octave::math::isnan (r) || octave::math::isnan (g)
                           || octave::math::isnan (b) ? 0 : 255);

                  img.setPixel (x_img_off + i - x_cdat_off,
                                y_img_off + j - y_cdat_off,
                                qRgba (octave::math::round (r * 255),
                                       octave::math::round (g * 255),
                                       octave::math::round (b * 255),
                                       a));
                }
          }
        else if (v.isreal ())
          {
            NDArray d = v.array_value ();

            for (int i = x_cdat_off; i < w + x_cdat_off; i++)
              for (int j = y_cdat_off; j < h + y_cdat_off; j++)
                {
                  double r = d(j, i, 0);
                  double g = d(j, i, 1);
                  double b = d(j, i, 2);
                  int a = (octave::math::isnan (r) || octave::math::isnan (g)
                           || octave::math::isnan (b) ? 0 : 255);

                  img.setPixel (x_img_off + i - x_cdat_off,
                                y_img_off + j - y_cdat_off,
                                qRgba (octave::math::round (r * 255),
                                       octave::math::round (g * 255),
                                       octave::math::round (b * 255),
                                       a));
                }
          }

        return img;
      }

    return QImage ();
  }

  octave_scalar_map
  makeKeyEventStruct (QKeyEvent *event)
  {
    octave_scalar_map retval;

    retval.setfield ("Key", KeyMap::qKeyToKeyString (event->key ()));
    retval.setfield ("Character", toStdString (event->text ()));

    std::list<std::string> modList;
    Qt::KeyboardModifiers mods = event->modifiers ();

    if (mods & Qt::ShiftModifier)
      modList.push_back ("shift");
    if (mods & Qt::ControlModifier)
#if defined (Q_OS_MAC)
      modList.push_back ("command");
#else
    modList.push_back ("control");
#endif
    if (mods & Qt::AltModifier)
      modList.push_back ("alt");
#if defined (Q_OS_MAC)
    if (mods & Qt::MetaModifier)
      modList.push_back ("control");
#endif

    retval.setfield ("Modifier", Cell (modList));

    return retval;
  }

  octave_scalar_map
  makeScrollEventStruct (QWheelEvent *event)
  {
    octave_scalar_map retval;

    // We assume a standard mouse with 15 degree steps and Qt returns
    // 1/8 of a degree.
#if defined (HAVE_QWHEELEVENT_ANGLEDELTA)
    int ydelta = -(event->angleDelta().y ());
#else
    int ydelta = -(event->delta ());
#endif
    retval.setfield ("VerticalScrollCount", octave_value (ydelta / 120));

    // FIXME: Is there any way to access the number of lines a scroll step
    // should correspond to?
    retval.setfield ("VerticalScrollAmount", octave_value (3));
    retval.setfield ("EventName", octave_value ("WindowScrollWheel"));

    return retval;
  }

}

OCTAVE_END_NAMESPACE(octave)
