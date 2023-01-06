////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2018-2023 The Octave Project Developers
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
// WiTHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_documentation_bookmarks_h)
#define octave_documentation_bookmarks_h 1

#include <QCheckBox>
#include <QComboBox>
#include <QTreeWidget>
#include <QXmlStreamWriter>

#include "documentation.h"
#include "octave-qobject.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;
class documentation;

class documentation_bookmarks : public QWidget
{
  Q_OBJECT

public:

  documentation_bookmarks (
                           documentation *doc, documentation_browser *browser,
                           base_qobject& oct_qobj, QWidget *p = nullptr);

  ~documentation_bookmarks (void) = default;

public slots:

  void add_bookmark (void);
  void add_folder (bool);
  void save_settings (gui_settings *settings);

private slots:

  void filter_bookmarks (const QString& pattern);
  void filter_activate (bool state);
  void update_filter_history (void);
  void handle_double_click (QTreeWidgetItem *item, int col = 0);
  void ctx_menu (const QPoint& xpos);
  void open (bool);
  void edit (bool);
  void remove (bool);
  void show_filter (bool);

private:

  enum item_role
    {
      url_role = Qt::UserRole,
      tag_role = Qt::UserRole + 1
    };
  enum item_tag
    {
      bookmark_tag,
      folder_tag
    };

  void add_bookmark (const QString& title, const QString& url,
                     QTreeWidgetItem *item = nullptr);
  QTreeWidgetItem* add_folder (const QString& folder,
                               QTreeWidgetItem *item = nullptr,
                               bool expanded = true);

  /*!
    Writing to and reading bookmarks from an xbel-file as
    proposed in the qt example
    [QXmlStream Bookmarks Example](https://doc.qt.io/qt-5/qtxml-streambookmarks-example.html)
  */
  void write_bookmarks (void);
  void write_tree_item (QXmlStreamWriter *xml_writer,
                        const QTreeWidgetItem *item);
  QString read_bookmarks (void);
  void read_next_item (QXmlStreamReader *xml_writer, item_tag tag,
                       QTreeWidgetItem *item = nullptr);

  documentation *m_doc;
  documentation_browser *m_browser;
  base_qobject& m_octave_qobj;

  QComboBox *m_filter;
  QTreeWidget *m_tree;

  QTreeWidgetItem *m_ctx_menu_item;

  QIcon icon_folder;
  QIcon icon_bookmark;

  QWidget *m_filter_widget;
  QCheckBox *m_filter_checkbox;
  bool m_filter_shown;

  QFile m_xbel_file;
};

OCTAVE_END_NAMESPACE(octave)

#endif
