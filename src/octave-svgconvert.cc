////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2021 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <iostream>

#include <QtCore>
#include <QtXml>

#include <QApplication>
#include <QFontDatabase>
#include <QImage>
#include <QPainter>
#include <QPrinter>
#include <QRegExp>
#include <QSvgRenderer>

// Render to pdf
class pdfpainter : public QPainter
{
public:
  pdfpainter (QString fname, QRectF sz, double dpi)
    : m_printer ()
  {
    double scl = dpi / 72.0;
    sz.setWidth (sz.width () * scl);
    sz.setHeight (sz.height () * scl);

    // Printer settings
    m_printer.setOutputFormat (QPrinter::PdfFormat);
    m_printer.setFontEmbeddingEnabled (true);
    m_printer.setOutputFileName (fname);
    m_printer.setFullPage (true);
#if defined (HAVE_QPRINTER_SETPAGESIZE)
    m_printer.setPageSize (QPageSize (sz.size (), QPageSize::Point));
#else
    m_printer.setPaperSize (sz.size (), QPrinter::DevicePixel);
#endif
  }

  ~pdfpainter (void) { }

  void render (const QByteArray svg_content)
  {
    QSvgRenderer renderer (svg_content);
    begin (&m_printer);
    renderer.render (this);
    end ();
  }

private:
  QPrinter m_printer;
};

// String conversion functions

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
                  if (alpha != 1.0 && alpha >= 0.0)
                    color.setAlphaF (alpha);
                }

              if (! current_polygon.count ())
                current_color = color;

              if (color != current_color)
                {
                  // Reconstruct the previous series of triangles
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

// Split "text" elements into single characters elements
void split_strings (QDomElement& parent_elt)
{
  QDomNodeList nodes = parent_elt.childNodes ();

  for (int ii = 0; ii < nodes.count (); ii++)
    {
      QDomNode node = nodes.at (ii);

      if (! node.isElement ())
        continue;

      QDomElement elt = node.toElement ();

      if (elt.tagName () == "text")
        {
          QString str = elt.attribute ("x");
          if (! str.isEmpty ())
            {
              QStringList xx = elt.attribute ("x").split (" ");
              str = elt.text ();

              if (! str.isEmpty () && xx.count () > 1)
                {
                  QDomNode last_node = node;

                  for (int jj = 0; jj < (xx.count ()) && (jj < str.size ());
                       jj++)
                    {
                      if (! last_node.isNull ())
                        {
                          QDomNode new_node = node.cloneNode ();
                          new_node.toElement ().setAttribute ("x", xx.at (jj));

                          QDomNodeList subnodes = new_node.childNodes ();

                          // Change the text node of this element
                          for (int kk = 0; kk < subnodes.count (); kk++)
                            if (subnodes.at (kk).isText ())
                              subnodes.at (kk).toText ().setData (str.at (jj));

                          parent_elt.insertAfter (new_node, last_node);
                          last_node = new_node;
                        }
                    }

                  parent_elt.removeChild (node);
                }
            }
        }
      else
        split_strings (elt);
    }
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
                << msg.toStdString () << std::endl;
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
      // Remove clippath which is not supported in svg-tiny
      QDomNodeList lst = root.elementsByTagName ("clipPath");
      for (int ii = lst.count (); ii > 0; ii--)
        lst.at (ii-1).parentNode ().removeChild (lst.at (ii-1));

      // Split text strings into single characters with single x coordinates
      split_strings (root);

      // PDF painter
      pdfpainter painter (fout.fileName (), vp, dpi);

      painter.render (document.toByteArray ());
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
