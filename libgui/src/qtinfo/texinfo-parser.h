/*

Copyright (C) 2009 P.L. Lucas
Copyright (C) 2012-2018 Jacob Dawid

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

#if ! defined (octave_texinfo_parser_h)
#define octave_texinfo_parser_h 1

#include <QStringList>
#include <QIODevice>
#include <QFileInfoList>
#include <QHash>

//! This class processes Texinfo `*.info`-files and their contained nodes
//! for searching and translation to HTML.
//!
//! Texinfo files are structured by nodes and contain a map with the
//! `position` of each node.  The nodes themselves hold the actual
//! documentation.
//!
//! If you make a queue with info files, `position` will be the number of
//! bytes from begining to a node position.
//!
//! But is not so easy. There is headers, and qtinfo must not take these
//! headers into account.

class texinfo_parser
  : public QObject
{
  Q_OBJECT

public:

  //! Ctor.

  texinfo_parser (QObject *parent = nullptr);

  //! Sets the path of the Texinfo files to @p info_path.
  //!
  //! @returns true, if successful, otherwise false.

  bool set_info_path (const QString& info_path);

  //! Returns the path of the Texinfo files.

  QString get_info_path ();

  //! Search for the text of @p node.

  QString search_node (const QString& node);

  //! Search for string @p text with @p max_results search results.

  QString global_search (const QString& text, int max_results);

  //! Find reference @p ref.
  //!
  //! @returns A valid XREF-reference, if @p ref exists.
  //!          Otherwise node "Top" is returned.

  QString find_reference (const QString& ref);

  //! Checks if @p ref is a XREF-reference.

  bool is_reference (const QString& ref);

  //! Get a HTML representation of @p node.
  //!
  //! If @p node is a XREF-reference, an HTML anchor `<a name="anchor">` is
  //! inserted at the position of the XREF-reference to navigate to it.
  //!
  //! @param anchor Name of the anchor.
  //!
  //! @returns HTML string.

  QString node_as_html (const QString& node,
                        const QString& anchor = QString ());

private:

  struct node_position
  {
    QString _node_name;
    int pos;
  };

  struct node_map_item
  {
    int pos;
  };

  struct info_file_item
  {
    QFileInfo file_info;
    int real_size;
  };

  QString get_next_node (QIODevice* io);
  QString get_node_name (const QString& text);
  QString get_node_up (const QString& text);
  QString get_node_next (const QString& text);
  QString get_node_prev (const QString& text);

  //! Append @p line to @p text.

  void append_line (QString *text, const char *line);

  //! Parse `*.info` file and generate map of nodes and their positions.

  void parse_info_map ();

  //! Open compressed `*.info` file @p fileInfo.

  QIODevice* open_file (QFileInfo& fileInfo);

  //! Calculates real position of nodes.
  //!
  //! @param pos Position from info file.
  //! @param file_info Returns file that contains @p pos.
  //! @param real_pos Returns real position inside @p file_info.

  void real_position (int pos, QFileInfo& file_info, int& real_pos);

  //! Seeks in @p io to position @p pos.

  void seek (QIODevice *io, int pos);

  QString                       _info_path;
  QFileInfoList                 _info_files;
  QHash<QString, node_map_item> _node_map;
  QHash<QString, node_position> _ref_map;
  QList<info_file_item>         _info_file_real_size_list;
  QHash<QString, QString>       _compressors_map;
};

#endif
