////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2020 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <http://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#include <iostream>

#include <QtCore>
#include <QtXml>

#include <QApplication>
#include <QFontDatabase>
#include <QImage>
#include <QPainter>
#include <QPrinter>
#include <QRegExp>

class pdfpainter : public QPainter
{
public:
  pdfpainter (QString fname, QRectF sizepix, double dpi)
    : m_fname (fname), m_sizef (sizepix), m_dpi (dpi), m_printer ()
  {
    double scl = get_scale ();
    m_sizef.setWidth (m_sizef.width () * scl);
    m_sizef.setHeight (m_sizef.height () * scl);

    // Printer settings
    m_printer.setOutputFormat (QPrinter::PdfFormat);
    m_printer.setFontEmbeddingEnabled (true);
    m_printer.setOutputFileName (get_fname ());
    m_printer.setFullPage (true);
    m_printer.setPaperSize (get_rectf ().size (), QPrinter::DevicePixel);

    // Painter settings
    begin (&m_printer);
    setViewport (get_rect ());
    scale (get_scale (), get_scale ());
  }

  ~pdfpainter (void) { }

  QString get_fname (void) const { return m_fname; }

  QRectF get_rectf (void) const { return m_sizef; }

  QRect get_rect (void) const { return m_sizef.toRect (); }

  double get_scale (void) const { return m_dpi / 72.0; }

  void finish (void) { end (); }

private:
  QString m_fname;
  QRectF m_sizef;
  double m_dpi;
  QPrinter m_printer;
};

// String conversion functions
QVector<double> qstr2vectorf (QString str)
{
  QVector<double> pts;
  QStringList coords = str.split (",");
  for (QStringList::iterator p = coords.begin (); p != coords.end (); p += 1)
    {
      double pt = (*p).toDouble ();
      pts.append (pt);
    }
  return pts;
}

QVector<double> qstr2vectord (QString str)
{
  QVector<double> pts;
  QStringList coords = str.split (",");
  for (QStringList::iterator p = coords.begin (); p != coords.end (); p += 1)
    {
      double pt = (*p).toDouble ();
      pts.append (pt);
    }

  return pts;
}

QVector<QPointF> qstr2ptsvector (QString str)
{
  QVector<QPointF> pts;
  str = str.trimmed ();
  str.replace (" ", ",");
  QStringList coords = str.split (",");
  for (QStringList::iterator p = coords.begin (); p != coords.end (); p += 2)
    {
      QPointF pt ((*p).toDouble (), (*(p+1)).toDouble ());
      pts.append (pt);
    }
  return pts;
}

QVector<QPoint> qstr2ptsvectord (QString str)
{
  QVector<QPoint> pts;
  str = str.trimmed ();
  str.replace (" ", ",");
  QStringList coords = str.split (",");
  for (QStringList::iterator p = coords.begin (); p != coords.end (); p += 2)
    {
      QPoint pt ((*p).toDouble (), (*(p+1)).toDouble ());
      pts.append (pt);
    }
  return pts;
}

// Extract field arguments in a style-like string, e.g. "bla field(1,34,56) bla"
QString get_field (QString str, QString field)
{
  QString retval;
  QRegExp rx (field + "\\(([^\\)]*)\\)");
  int pos = 0;
  pos = rx.indexIn (str, pos);
  if (pos > -1)
    retval = rx.cap (1);

  return retval;
}

// Polygon reconstruction class
class octave_polygon
{
public:
  octave_polygon (void)
  { }

  octave_polygon (QPolygonF p)
  { m_polygons.push_back (p); }

  ~octave_polygon (void) { }

  int count (void) const
  { return m_polygons.count (); }

  void reset (void)
  { m_polygons.clear (); }

  QList<QPolygonF> reconstruct (void)
  {
    if (m_polygons.isEmpty ())
      return QList<QPolygonF> ();

    // Once a polygon has been merged to another, it is marked unsuded
    QVector<bool> unused;
    for (auto it = m_polygons.begin (); it != m_polygons.end (); it++)
      unused.push_back (false);

    bool tryagain = (m_polygons.count () > 1);

    while (tryagain)
      {
        tryagain = false;
        for (auto ii = 0; ii < m_polygons.count (); ii++)
          {
            if (! unused[ii])
              {
                QPolygonF polygon = m_polygons[ii];
                for (auto jj = ii+1; jj < m_polygons.count (); jj++)
                  {
                    if (! unused[jj])
                      {
                        QPolygonF newpoly = mergepoly (polygon, m_polygons[jj]);
                        if (newpoly.count ())
                          {
                            polygon = newpoly;
                            m_polygons[ii] = newpoly;
                            unused[jj] = true;
                            tryagain = true;
                          }
                      }
                  }
              }
          }
      }

    // Try to remove cracks in polygons
    for (auto ii = 0; ii < m_polygons.count (); ii++)
      {
        QPolygonF polygon = m_polygons[ii];
        tryagain = ! unused[ii];

        while (tryagain && polygon.count () > 4)
          {
            tryagain = false;
            QVector<int> del;

            for (auto jj = 1; jj < (polygon.count () - 1); jj++)
              if (polygon[jj-1] == polygon[jj+1])
                {
                  if (! del.contains (jj))
                    del.push_front (jj);

                  del.push_front (jj+1);
                }

            for (auto idx : del)
              polygon.remove (idx);

            if (del.count ())
              tryagain = true;
          }
        m_polygons[ii] = polygon;
      }

    // FIXME: There may still be residual cracks, we should do something like
    //   resetloop = 2;
    //   while (resetloop)
    //     currface = shift (currface, 1);
    //     if (currface(1) == currface(3))
    //       currface([2 3]) = [];
    //       resetloop = 2;
    //     else
    //       resetloop--;
    //     endif
    //   endwhile

    QList<QPolygonF> retval;
    for (int ii = 0; ii < m_polygons.count (); ii++)
      {
        QPolygonF polygon = m_polygons[ii];
        if (! unused[ii] && polygon.count () > 2)
          retval.push_back (polygon);
      }

    return retval;
  }

  static inline
  bool eq (QPointF p1, QPointF p2)
  {
    return ((qAbs (p1.x () - p2.x ())
             <= 0.00001 * qMin (qAbs (p1.x ()), qAbs (p2.x ())))
            && (qAbs (p1.y () - p2.y ())
                <= 0.00001 * qMin (qAbs (p1.y ()), qAbs (p2.y ()))));
  }

  static
  QPolygonF mergepoly (QPolygonF poly1, QPolygonF poly2)
  {
    // Close polygon contour
    poly1.push_back (poly1[0]);
    poly2.push_back (poly2[0]);

    for (int ii = 0; ii < (poly1.size () - 1); ii++)
      {
        for (int jj = 0; jj < (poly2.size () - 1); jj++)
          {
            bool forward = (eq (poly1[ii], poly2[jj])
                            && eq (poly1[ii+1], poly2[jj+1]));
            bool backward = ! forward && (eq (poly1[ii], poly2[jj+1])
                                          && eq (poly1[ii+1], poly2[jj]));

            if (forward || backward)
              {
                // Unclose contour
                poly1.pop_back ();
                poly2.pop_back ();

                QPolygonF merged;
                for (int kk = 0; kk < (ii+1); kk++)
                  merged.push_back (poly1[kk]);

                // Shift vertices and eliminate the common edge
                std::rotate (poly2.begin (), poly2.begin () + jj, poly2.end ());
                poly2.erase (poly2.begin ());
                poly2.erase (poly2.begin ());

                if (forward)
                  for (int kk = poly2.size (); kk > 0; kk--)
                    merged.push_back (poly2[kk-1]);
                else
                  for (int kk = 0; kk < poly2.size (); kk++)
                    merged.push_back (poly2[kk]);

                for (int kk = ii+1; kk < poly1.size (); kk++)
                  merged.push_back (poly1[kk]);

                // Return row vector
                QPolygonF out (merged.size ());
                for (int kk = 0; kk < merged.size (); kk++)
                  out[kk] = merged[kk];

                return out;
              }
          }
      }
    return QPolygonF ();
  }

  void add (QPolygonF p)
  {
    if (m_polygons.count () == 0)
      m_polygons.push_back (p);
    else
      {
        QPolygonF tmp = mergepoly (m_polygons.back (), p);
        if (tmp.count ())
          m_polygons.back () = tmp;
        else
          m_polygons.push_back (p);
      }
  }

private:
  QList<QPolygonF> m_polygons;
};

void draw (QDomElement& parent_elt, pdfpainter& painter)
{
  QDomNodeList nodes = parent_elt.childNodes ();

  static QString clippath_id;
  static QMap< QString, QVector<QPoint> > clippath;

  // tspan elements must have access to the font and position extracted from
  // their parent text element
  static QFont font;
  static double dx = 0, dy = 0;

  for (int i = 0; i < nodes.count (); i++)
    {
      QDomNode node = nodes.at (i);
      if (! node.isElement ())
        continue;

      QDomElement elt = node.toElement ();

      if (elt.tagName () == "clipPath")
        {
          clippath_id = "#" + elt.attribute ("id");
          draw (elt, painter);
          clippath_id = QString ();
        }
      else if (elt.tagName () == "g")
        {
          bool current_clipstate = painter.hasClipping ();
          QRegion current_clippath = painter.clipRegion ();

          QString str = elt.attribute ("clip-path");
          if (! str.isEmpty ())
            {
              QVector<QPoint> pts = clippath[get_field (str, "url")];
              if (! pts.isEmpty ())
                {
                  painter.setClipRegion (QRegion (QPolygon (pts)));
                  painter.setClipping (true);
                }
            }

          draw (elt, painter);

          // Restore previous clipping settings
          painter.setClipRegion (current_clippath);
          painter.setClipping (current_clipstate);
        }
      else if (elt.tagName () == "text")
        {
          // Font
          font = QFont ();
          QString str = elt.attribute ("font-family");
          if (! str.isEmpty ())
            font.setFamily (elt.attribute ("font-family"));

          str = elt.attribute ("font-weight");
          if (! str.isEmpty () && str != "normal")
            font.setWeight (QFont::Bold);

          str = elt.attribute ("font-style");
          if (! str.isEmpty () && str != "normal")
            font.setStyle (QFont::StyleItalic);

          int sz = elt.attribute ("font-size").toInt ();
          if (sz > 0)
            font.setPixelSize (sz);

          painter.setFont (font);

          // Translation and rotation
          painter.save ();
          str = get_field (elt.attribute ("transform"), "translate");
          if (! str.isEmpty ())
            {
              QStringList trans = str.split (",");
              dx = trans[0].toDouble ();
              dy = trans[1].toDouble ();

              str = get_field (elt.attribute ("transform"), "rotate");
              if (! str.isEmpty ())
                {
                  QStringList rot = str.split (",");
                  painter.translate (dx+rot[1].toDouble (),
                                     dy+rot[2].toDouble ());
                  painter.rotate (rot[0].toDouble ());
                  dx = rot[1].toDouble ();
                  dy = rot[2].toDouble ();
                }
              else
                {
                  painter.translate (dx, dy);
                  dx = 0;
                  dy = 0;
                }
            }

          draw (elt, painter);
          painter.restore ();
        }
      else if (elt.tagName () == "tspan")
        {
          // Font
          QFont saved_font (font);

          QString str = elt.attribute ("font-family");
          if (! str.isEmpty ())
            font.setFamily (elt.attribute ("font-family"));

          str = elt.attribute ("font-weight");
          if (! str.isEmpty ())
            {
              if (str != "normal")
                font.setWeight (QFont::Bold);
              else
                font.setWeight (QFont::Normal);
            }

          str = elt.attribute ("font-style");
          if (! str.isEmpty ())
            {
              if (str != "normal")
                font.setStyle (QFont::StyleItalic);
              else
                font.setStyle (QFont::StyleNormal);
            }

          int sz = elt.attribute ("font-size").toInt ();
          if (sz > 0)
            font.setPixelSize (sz);

          painter.setFont (font);

          // Color is specified in rgb
          str = get_field (elt.attribute ("fill"), "rgb");
          if (! str.isEmpty ())
            {
              QStringList clist = str.split (",");
              painter.setPen (QColor (clist[0].toInt (), clist[1].toInt (),
                                      clist[2].toInt ()));
            }

          QStringList xx = elt.attribute ("x").split (" ");
          int y = elt.attribute ("y").toInt ();
          str = elt.text ();
          if (! str.isEmpty ())
            {
              int ii = 0;
              foreach (QString s,  xx)
                if (ii < str.size ())
                  painter.drawText (s.toInt ()-dx, y-dy, str.at (ii++));
            }

          draw (elt, painter);
          font = saved_font;
        }
      else if (elt.tagName () == "polyline")
        {
          // Color
          QColor c (elt.attribute ("stroke"));
          QString str = elt.attribute ("stroke-opacity");
          if (! str.isEmpty () && str.toDouble () != 1.0
              && str.toDouble () >= 0.0)
            c.setAlphaF (str.toDouble ());

          QPen pen;
          pen.setColor (c);

          // Line properties
          str = elt.attribute ("stroke-width");
          if (! str.isEmpty ())
            {
              double w = str.toDouble () * painter.get_scale ();
              if (w > 0)
                pen.setWidthF (w / painter.get_scale ());
            }

          str = elt.attribute ("stroke-linecap");
          pen.setCapStyle (Qt::SquareCap);
          if (str == "round")
            pen.setCapStyle (Qt::RoundCap);
          else if (str == "butt")
            pen.setCapStyle (Qt::FlatCap);

          str = elt.attribute ("stroke-linejoin");
          pen.setJoinStyle (Qt::MiterJoin);
          if (str == "round")
            pen.setJoinStyle (Qt::RoundJoin);
          else if (str == "bevel")
            pen.setJoinStyle (Qt::BevelJoin);

          str = elt.attribute ("stroke-dasharray");
          pen.setStyle (Qt::SolidLine);
          if (! str.isEmpty ())
            {
              QVector<double> pat = qstr2vectord (str);
              if (pat.count () != 2 || pat[1] != 0)
                {
                  // Express pattern in linewidth units
                  for (auto& p : pat)
                    p /= pen.widthF ();

                  pen.setDashPattern (pat);
                }
            }

          painter.setPen (pen);
          painter.drawPolyline (qstr2ptsvector (elt.attribute ("points")));
        }
      else if (elt.tagName () == "image")
        {
          // Images are represented as a base64 stream of png formatted data
          QString href_att = elt.attribute ("xlink:href");
          QString prefix ("data:image/png;base64,");
          QByteArray data
            = QByteArray::fromBase64 (href_att.mid (prefix.length ()).toLatin1 ());
          QImage img;
          if (img.loadFromData (data, "PNG"))
            {
              QRect pos(elt.attribute ("x").toInt (),
                        elt.attribute ("y").toInt (),
                        elt.attribute ("width").toInt (),
                        elt.attribute ("height").toInt ());

              // Translate
              painter.save ();
              QString str = get_field (elt.attribute ("transform"), "matrix");
              if (! str.isEmpty ())
                {
                  QVector<double> m = qstr2vectorf (str);
                  double scl = painter.get_scale ();
                  QTransform tform(m[0]*scl, m[1]*scl, m[2]*scl,
                                   m[3]*scl, m[4]*scl, m[5]*scl);
                  painter.setTransform (tform);
                }

              painter.setRenderHint (QPainter::Antialiasing, false);
              painter.drawImage (pos, img);
              painter.setRenderHint (QPainter::Antialiasing, true);
              painter.restore  ();
            }
        }
      else if (elt.tagName () == "polygon")
        {
          if (! clippath_id.isEmpty ())
            clippath[clippath_id] = qstr2ptsvectord (elt.attribute ("points"));
          else
            {
              QString str = elt.attribute ("fill");
              if (! str.isEmpty ())
                {
                  QColor color (str);

                  str = elt.attribute ("fill-opacity");
                  if (! str.isEmpty () && str.toDouble () != 1.0
                      && str.toDouble () >= 0.0)
                    color.setAlphaF (str.toDouble ());

                  QPolygonF p (qstr2ptsvector (elt.attribute ("points")));

                  if (p.count () > 2)
                    {
                      painter.setBrush (color);
                      painter.setPen (Qt::NoPen);

                      painter.setRenderHint (QPainter::Antialiasing, false);
                      painter.drawPolygon (p);
                      painter.setRenderHint (QPainter::Antialiasing, true);
                    }
                }
            }
        }
    }
}

// Append a list of reconstructed child polygons to a QDomElement and remove
// the original nodes

void replace_polygons (QDomElement& parent_elt, QList<QDomNode> orig,
                       QList<QPolygonF> polygons)
{
  if (! orig.count () || (orig.count () == polygons.count ()))
    return;

  QDomNode last = orig.last ();
  for (int ii = 0; ii < polygons.count (); ii++)
    {
      QPolygonF polygon = polygons[ii];

      QDomNode node = last.cloneNode ();

      QString pts;

      for (int jj = 0; jj < polygon.count (); jj++)
        {
          pts += QString ("%1,%2 ").arg (polygon[jj].x ())
                 .arg (polygon[jj].y ());
        }

      node.toElement ().setAttribute ("points", pts.trimmed ());

      if (! last.isNull ())
        last = parent_elt.insertAfter (node, last);
    }

  for (int ii = 0; ii < orig.count (); ii++)
    parent_elt.removeChild (orig.at (ii));
}

void reconstruct_polygons (QDomElement& parent_elt)
{
  QDomNodeList nodes = parent_elt.childNodes ();
  QColor current_color;
  QList<QDomNode> replaced_nodes;
  octave_polygon current_polygon;

  // Collection of child nodes to be removed and polygons to be added
  QList< QPair<QList<QDomNode>,QList<QPolygonF> > > collection;

  for (int ii = 0; ii < nodes.count (); ii++)
    {
      QDomNode node = nodes.at (ii);
      if (! node.isElement ())
        continue;

      QDomElement elt = node.toElement ();

      if (elt.tagName () == "polygon")
        {
          QString str = elt.attribute ("fill");
          if (! str.isEmpty ())
            {
              QColor color (str);
              str = elt.attribute ("fill-opacity");
              if (! str.isEmpty ())
                {
                  double alpha = str.toDouble ();
                  if (alpha != 1.0 && str.toDouble () >= 0.0)
                    color.setAlphaF (alpha);
                }

              if (! current_polygon.count ())
                current_color = color;

              if (color != current_color)
                {
                  // Reconstruct the previous series of triangle
                  QList<QPolygonF> polygons = current_polygon.reconstruct ();
                  collection.push_back (QPair<QList<QDomNode>,QList<QPolygonF> >
                                        (replaced_nodes, polygons));

                  replaced_nodes.clear ();
                  current_polygon.reset ();

                  current_color = color;
                }

              QPolygonF p (qstr2ptsvector (elt.attribute ("points")));
              current_polygon.add (p);
              replaced_nodes.push_back (node);
            }
        }
      else
        {
          if (current_polygon.count ())
            {
              QList<QPolygonF> polygons = current_polygon.reconstruct ();
              collection.push_back (QPair<QList<QDomNode>,QList<QPolygonF> >
                                    (replaced_nodes, polygons));
              replaced_nodes.clear ();
              current_polygon.reset ();
            }
          reconstruct_polygons (elt);
        }
    }

  // Finish
  collection.push_back (QPair<QList<QDomNode>,QList<QPolygonF> >
                        (replaced_nodes, current_polygon.reconstruct ()));

  for (int ii = 0; ii < collection.count (); ii++)
    replace_polygons (parent_elt, collection[ii].first, collection[ii].second);
}

int main(int argc, char *argv[])
{
  const char *doc = "See \"octave-svgconvert -h\"";
  const char *help = "Usage:\n\
octave-svgconvert infile fmt dpi font reconstruct outfile\n\n\
Convert svg file to pdf, or svg. All arguments are mandatory:\n\
* infile: input svg file or \"-\" to indicate that the input svg file should be \
read from stdin\n\
* fmt: format of the output file. May be one of pdf or svg\n\
* dpi: device dependent resolution in screen pixel per inch\n\
* font: specify a file name for the default FreeSans font\n\
* reconstruct: specify whether to reconstruct triangle to polygons (0 or 1)\n\
* outfile: output file name\n";

  if (strcmp (argv[1], "-h") == 0)
    {
      std::cout << help;
      return 0;
    }
  else if (argc != 7)
    {
      std::cerr << help;
      return -1;
    }

  // Open svg file
  QFile file;
  if (strcmp (argv[1], "-") != 0)
    {
      // Read from file
      file.setFileName (argv[1]);
      if (! file.open (QIODevice::ReadOnly | QIODevice::Text))
        {
          std::cerr << "Unable to open file " << argv[1] << "\n";
          std::cerr << help;
          return -1;
        }
    }
  else
    {
      // Read from stdin
      if (! file.open (stdin, QIODevice::ReadOnly | QIODevice::Text))
        {
          std::cerr << "Unable read from stdin\n";
          std::cerr << doc;
          return -1;
        }
    }

  // Create a DOM document and load the svg file
  QDomDocument document;
  QString msg;
  if (! document.setContent (&file, false, &msg))
    {
      std::cerr << "Failed to parse XML contents" << std::endl
                << msg.toStdString ();
      std::cerr << doc;
      file.close();
      return -1;
    }

  file.close ();

  // Format
  if (strcmp (argv[2], "pdf") != 0 && strcmp (argv[2], "svg") != 0)
    {
      std::cerr << "Unhandled output file format " << argv[2] << "\n";
      std::cerr << doc;
      return -1;
    }

  // Resolution
  double dpi = QString (argv[3]).toDouble ();
  if (dpi <= 0.0)
    {
      std::cerr << "DPI must be positive\n";
      return -1;
    }


  // Get the viewport from the root element
  QDomElement root = document.firstChildElement();
  double x0, y0, dx, dy;
  QString s = root.attribute ("viewBox");
  QTextStream (&s) >> x0 >> y0 >> dx >> dy;
  QRectF vp (x0, y0, dx, dy);

  // Setup application and add default FreeSans font if needed
  QApplication a (argc, argv);

  // When printing to PDF we may need the default FreeSans font
  if (! strcmp (argv[2], "pdf"))
    {
      QFont font ("FreeSans");
      if (! font.exactMatch ())
        {
          QString fontpath (argv[4]);
          if (! fontpath.isEmpty ())
            {
              int id = QFontDatabase::addApplicationFont (fontpath);
              if (id < 0)
                std::cerr << "warning: print: "
                             "Unable to add default font to database\n";
            }
          else
            std::cerr << "warning: print: FreeSans font not found\n";
        }
    }

  // First render in a temporary file
  QTemporaryFile fout;
  if (! fout.open ())
    {
      std::cerr << "Could not open temporary file\n";
      return -1;
    }

  // Do basic polygons reconstruction
  if (QString (argv[5]).toInt ())
    reconstruct_polygons (root);

  // Draw
  if (! strcmp (argv[2], "pdf"))
    {
      // PDF painter
      pdfpainter painter (fout.fileName (), vp, dpi);

      draw (root, painter);
      painter.finish ();
    }
  else
    {
      // Return modified svg document
      QTextStream out (&fout);
      out.setCodec ("UTF-8");
      out << document.toByteArray ();
    }

  // Delete output file before writing with new data
  if (QFile::exists (argv[6]))
    if (! QFile::remove (argv[6]))
      {
        std::cerr << "Unable to replace existing file " << argv[6] << "\n";
        return -1;
      }

  fout.copy (argv[6]);

  return 0;
}
