/*

Copyright (C) 2012-2018 Jacob Dawid
Copyright (C) 2009 P. L. Lucas

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

// Author: P. L. Lucas
// Author: Jacob Dawid <jacob.dawid@cybercatalyst.com>

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "texinfo-parser.h"
#include "procstream.h"
#include <QFileInfo>
#include <QDir>
#include <QFile>
#include <QUrl>
#include <QRegExp>
#include <QBuffer>

texinfo_parser::texinfo_parser (QObject *p)
  : QObject(p)
{
  _compressors_map.insert ( "bz2",  R"(bzip2 -dc "%1")" );
  _compressors_map.insert ( "gz",   R"(gzip -dc "%1")"  );
  _compressors_map.insert ( "lzma", R"(lzma -dc "%1")"  );
  _compressors_map.insert ( "xz",   R"(xz -dc "%1")"    );
  _compressors_map.insert ( "Z",    R"(gunzip -c "%1")" );
}

bool
texinfo_parser::set_info_path (const QString& infoPath)
{
  this->_info_path = infoPath;

  _info_files.clear ();

  QFileInfo info (infoPath);

  bool info_file_exists = info.exists ();
  QHash<QString, QString>::iterator it;
  for (it = _compressors_map.begin (); it != _compressors_map.end (); it++)
    {
      if (info_file_exists)
        break;
      info_file_exists = QFileInfo (info.absoluteFilePath () + '.' +
                                    it.key ()).exists ();
    }

  if (info_file_exists)
    {
      QString path = info.absolutePath ();
      QString fileName = info.fileName ();

      QDir infoDir (path);
      QStringList filter;
      filter.append (fileName + '*');

      _info_files = infoDir.entryInfoList (filter, QDir::Files);

      parse_info_map ();

      return true;
    }
  else
    return false;
}

QString
texinfo_parser::get_info_path ()
{
  return _info_path;
}

QIODevice *
texinfo_parser::open_file (QFileInfo & file_info)
{
  QIODevice *iodevice = nullptr;
  if (_compressors_map.contains (file_info.suffix ()))
    {
      QString command = _compressors_map.value (file_info.suffix ()).arg (
                          file_info.absoluteFilePath ());
      iprocstream ips (command.toStdString ());

      if (ips.bad ())
        return nullptr;

      QByteArray result;
      char buffer[1024];

      while (! ips.eof ())
        {
          ips.read (buffer, sizeof (buffer));
          result.append (buffer, ips.gcount ());
        }

      QBuffer *io = new QBuffer (this);
      io->setData (result);

      if (! io->open (QIODevice::ReadOnly | QIODevice::Text))
        return nullptr;

      iodevice = io;
    }
  else
    {
      QFile *io = new QFile (file_info.absoluteFilePath ());
      if (! io->open (QIODevice::ReadOnly | QIODevice::Text))
        return nullptr;
      iodevice = io;
    }

  return iodevice;
}

QString
texinfo_parser::find_reference (const QString& ref_name)
{
  QString xref_name = "XREF" + ref_name;
  xref_name.remove (' ');  // Delete spaces as XREF uses no whitespace

  if (_ref_map.contains (xref_name))
    return xref_name;
  else if (_node_map.contains ("The " + ref_name + " Statement"))
    {
      // See, for example "The while Statement" which has no XREF.
      return "The " + ref_name + " Statement";
    }
  else
    return "Top";
}

bool
texinfo_parser::is_reference (const QString& ref)
{
  return _ref_map.contains (ref);
}

QString
texinfo_parser::search_node (const QString& node_arg)
{
  QString node = node_arg;
  QString text = "";

  // If node_arg was a reference, translate to node.
  if (_ref_map.contains (node_arg))
    node = _ref_map [node_arg]._node_name;

  if (_node_map.contains (node))
    {
      QFileInfo file_info;
      int real_pos;
      real_position (_node_map [node].pos, file_info, real_pos);

      QIODevice* io = open_file (file_info);
      if (! io)
        return text;

      seek (io, real_pos);

      text = get_next_node (io);

      io->close ();
      delete io;
    }

  return text;
}

void
texinfo_parser::append_line (QString *text, const char *line)
{
  QString line_converted = QString::fromLatin1 (line);
  int len = line_converted.length ();
  line_converted = QString::fromUtf8 (line);
  for (int i = len - line_converted.length (); i > 0; i--)
    line_converted.insert (line_converted.size () - 1, QByteArray (" "));
  text->append (line_converted);
}

QString
texinfo_parser::get_next_node (QIODevice *io)
{
  QString text;
  QByteArray line, line_buffer;
  char c;
  int i;

  while (! io->atEnd ())
    {
      io->getChar (&c);
      if (c)
        {
          // first char is not equal 0
          io->ungetChar (c);
          line = io->readLine ();
        }
      else
        {
          // 0 was read -> image -> handle text replacement (length changes)
          line_buffer = io->readLine ();  // start of image tag -> drop it
          int len = line_buffer.size ();
          line = io->readLine ();         // get first line of its text
          line_buffer = line;             // and store it
          append_line (&text, line);
          line = io->readLine ();         // get next line of text
          append_line (&text, line);
          line = io->readLine ();         // drop last line (unneeded chars)
          line = line_buffer;             // and take the first instead
          // now correct the size of the dropped line and 5 additional chars
          for (i = 1; i < len + 6; i++)
            line.insert (line.size ()-1, QByteArray (" "));  // adding blanks
        }

      if (line.at(0) == 31)
        break;
      else
        append_line (&text, line);
    }
  return text;
}

static QString
get_first_line (const QString& text)
{
  int n = text.indexOf ("\n");

  if (n < 0)
    return QString ();

  QString first_line = text.left (n);
  return first_line;
}

static QString
texinfo_parser_node (const QString& text, const QString& node_name)
{
  QString firstLine = get_first_line (text);
  QStringList nodes = firstLine.split (",");
  for (int i = 0; i < nodes.size (); i++)
    {
      QString node = nodes.at (i).trimmed ();

      if (node.startsWith (node_name))
        return node.remove (0, node_name.size ()).trimmed ();
    }
  return QString ();
}

QString
texinfo_parser::get_node_name (const QString& text)
{
  return texinfo_parser_node (text, "Node:");
}

QString
texinfo_parser::get_node_up (const QString& text)
{
  return texinfo_parser_node (text, "Up:");
}

QString
texinfo_parser::get_node_next (const QString& text)
{
  return texinfo_parser_node (text, "Next:");
}

QString
texinfo_parser::get_node_prev (const QString& text)
{
  return texinfo_parser_node (text, "Prev:");
}

static void
replace_links (QString& text)
{
  QRegExp re ("(\\*[N|n]ote|\n\\*)([ |\n]+)([^:]+):([^:\\.,]*)([:,\\.]+)");
  int i = 0, f;

  while ((i = re.indexIn (text, i)) != -1)
    {
      QString type     = re.cap (1);
      QString note     = re.cap (3);
      QString url_link = re.cap (4);
      QString term     = re.cap (5);

      if (url_link.isEmpty ())
        url_link = note;

      term.replace (":", "");
      note.replace (":", "");
      note.replace (QRegExp ("`([^']+)'"),"\\1");  // no extra format in links

      QRegExp re_break ("(\n[ ]*)");

      if (note == "fig" || note == "tab")
        url_link.prepend ("#");

      QString href;
      if (type == "\n*")
        href = "\n";

      if (re_break.indexIn (url_link) != -1)
        term += re_break.cap (1);
      else if (re_break.indexIn (re.cap (2)) != -1)
        href = re_break.cap (1) + ' ';
      else if (re_break.indexIn (note) != -1)
        term += re_break.cap (1);
      note.replace (re_break,"&nbsp;");

      url_link = url_link.trimmed ();
      url_link.replace ("\n", " ");
      url_link.replace (QRegExp ("  +"), " ");
      url_link.replace ("<b>", "");
      url_link.replace ("</b>", "");

      href += R"(<font style="color:DarkGray; font-weight:bold;">&raquo;</font>)";
      href += "&nbsp;<a href='" + url_link + "'>" + note + "</a>" + term;
      f = re.matchedLength ();
      text.replace (i, f, href);
      i += href.size ();
    }
}

static void
replace_colons (QString& text)
{
  QRegExp re ("`([^']+)'");
  int i = 0, f;
  while ((i = re.indexIn (text, i)) != -1)
    {
      QString t = re.cap (1);
      QString bold = R"(<font style="color:SteelBlue;font-weight:bold">)" + t +
                     "</font>";

      f = re.matchedLength ();
      text.replace (i, f, bold);
      i += bold.size ();
    }
}

static void
info_to_html (QString& text)
{
  text.replace ("&", "&amp;");
  text.replace ("<", "&lt;");
  text.replace (">", "&gt;");

  text.replace ("\n* Menu:",
                "\n<font style=\"color:DarkRed;font-weight:bold\">Menu:</font>");
  text.replace ("See also:",
                R"(<font style="color:DarkRed;font-style:italic;font-weight:bold">See also:</font>)");
  replace_links (text);
  replace_colons (text);
}

QString
texinfo_parser::node_as_html (const QString& node, const QString& anchor)
{
  QString text = search_node (node);

  QString nodeName = get_node_name (text);
  QString nodeUp   = get_node_up (text);
  QString nodeNext = get_node_next (text);
  QString nodePrev = get_node_prev (text);

  // Insert anchor, if node is a XREF-reference
  if (is_reference (node))
    {
      node_position ref = _ref_map[node];
      int anchor_pos = ref.pos - _node_map[ref._node_name].pos;

      QString text1 = text.left (anchor_pos);
      QString text2 = text.mid (anchor_pos);

      text1.remove (0, text1.indexOf ("\n"));

      info_to_html (text1);
      info_to_html (text2);

      text = text1 + "<a name='" + anchor
             + R"('/><font style="color:DarkBlue; font: bold monospace large;">&diams;</font><br>&nbsp;)"
             + text2;
    }
  else
    {
      text.remove (0, text.indexOf ("\n"));
      info_to_html (text);
    }

  QString navigationLinks = QString (
        R"(<b>Section:</b> <font style="color:DarkRed">%1</font><br>)"
        "<b>Previous Section:</b> <a href='%2'>%3</a><br>"
        "<b>Next Section:</b> <a href='%4'>%5</a><br>"
        "<b>Up:</b> <a href='%6'>%7</a><br>\n"
        )
    .arg (nodeName, nodePrev, nodePrev, nodeNext, nodeNext, nodeUp, nodeUp);

  text.prepend ("<hr>\n<pre style=\"font-family:monospace\">");
  text.append ("</pre>\n<hr><hr>\n");
  text.prepend (navigationLinks);
  text.append (navigationLinks);
  text.prepend ("<html><body>\n");
  text.append ("</body></html>\n");

  return text;
}

void
texinfo_parser::parse_info_map ()
{
  QRegExp re ("(Node|Ref): ([^\\0177]+)\\0177(\\d+)\n");
  QRegExp re_files ("([^:]+): (\\d+)\n");
  int foundCount = 0;

  for (int i = 0; i < _info_files.size (); i++)
    {
      QFileInfo fileInfo = _info_files.at (i);

      QIODevice *io = open_file (fileInfo);
      if (! io)
        continue;

      QString nodeText;
      while (! (nodeText = get_next_node (io)).isEmpty () && foundCount < 2)
        {
          QString first_line = get_first_line (nodeText);
          if (first_line.startsWith ("Tag"))
            {
              foundCount++;
              int pos = 0;
              QString last_node;

              while ((pos = re.indexIn (nodeText, pos)) != -1)
                {
                  QString type = re.cap (1);
                  QString node = re.cap (2);
                  int index = re.cap (3).toInt ();

                  if (type == "Node")
                    {
                      node_map_item item;
                      item.pos = index;
                      _node_map [node] = item;
                      last_node = node;
                    }
                  else if (type == "Ref")
                    {
                      node_position item;
                      item._node_name = last_node;
                      item.pos = index;
                      _ref_map [node] = item;
                    }
                  pos += re.matchedLength ();
                }
              break;
            }
          else if (first_line.startsWith ("Indirect:"))
            {
              foundCount++;
              int pos = 0;

              while ((pos = re_files.indexIn (nodeText, pos)) != -1)
                {
                  QString fileCap = re_files.cap (1).trimmed ();
                  int index = re_files.cap (2).toInt ();

                  info_file_item item;
                  for (int j = 0; j < _info_files.size (); j++)
                    {
                      QFileInfo info = _info_files.at (j);
                      if (info.fileName ().startsWith (fileCap))
                        {
                          item.file_info = info;
                          break;
                        }
                    }
                  item.real_size = index;
                  _info_file_real_size_list.append (item);
                  pos += re_files.matchedLength ();
                }
            }
        }
      io->close ();
      delete io;
    }
}

void
texinfo_parser::real_position (int pos, QFileInfo& file_info, int& real_pos)
{
  int header = -1;
  int sum = 0;
  for (int i = 0; i < _info_file_real_size_list.size (); i++)
    {
      info_file_item item = _info_file_real_size_list.at (i);
      if (header == -1)
        {
          file_info = item.file_info;
          header = item.real_size;
        }

      if (pos < item.real_size)
        {
          break;
        }

      file_info = item.file_info;
      sum = item.real_size;
    }
  real_pos = pos - sum + header + 2;
}

void
texinfo_parser::seek (QIODevice *io, int pos)
{
  char ch;
  while (! io->atEnd () && pos > 0)
    {
      io->getChar (&ch);
      pos--;
    }
}

QString
texinfo_parser::global_search (const QString& text, int max_results)
{
  QString results;
  QStringList words = text.split (" ", QString::SkipEmptyParts);

  QString re_program ('(' + QRegExp::escape (words.at (0)));
  for (int i = 1; i < words.size (); i++)
    re_program += '|' + QRegExp::escape (words.at (i));
  re_program += ')';

  QRegExp re (re_program, Qt::CaseInsensitive);

  results.append ("<html><body>\n<h1>Search results</h1>\n<b>Results for:</b> ");
  results.append (text);
  results.append ("<br>\n");

  for (int i = 0; i < _info_files.size (); i++)
    {
      QFileInfo file_info = _info_files.at (i);
      QIODevice *io = open_file (file_info);
      if (! io)
        continue;

      QString node_text;
      while (! (node_text = get_next_node (io)).isEmpty ())
        {
          QString firstLine = get_first_line (node_text);
          QString node = get_node_name (node_text);
          if (node.isEmpty ())
            continue;

          node_text.remove (0, node_text.indexOf ("\n"));

          int pos = 0;
          int founds = 0;

          for (; founds < words.size ()
                 && node_text.indexOf (words.at (founds), 0,Qt::CaseInsensitive)
                    >= 0;
               founds++)
            { }

          if (founds < words.size ())
            continue;

          founds = 0;

          while ((pos = re.indexIn (node_text, pos)) != -1
                 && founds < max_results)
            {
              if (founds == 0)
                {
                  results.append(
                    "<br>\n<font style=\"color:DarkGray; font-weight:bold;\">&raquo;</font> <a href='"
                    + node +
                    "'>");
                  results.append (node);
                  results.append ("</a><br>\n");
                }

              // Replace text found with BOLD TEXT in search results
              int line_start, line_end;
              line_start = node_text.lastIndexOf ("\n", pos);
              line_end = node_text.indexOf ("\n", pos);
              QString line = node_text.mid (line_start,
                                            line_end - line_start);

              int pos2 = pos - line_start;
              int len = re.matchedLength ();

              QString ptn = line.mid (pos2, len);
              QString repl ("<b>%1</b>");
              repl = repl.arg (ptn);
              line.remove (pos2, len);
              line.insert (pos2, repl);
              line = line.trimmed ();
              results.append (line);
              results.append ("<br>\n");

              pos += len;
              founds++;
            }
        }
      io->close ();
      delete io;
    }

  results.append ("</body></html>");
  return results;
}
