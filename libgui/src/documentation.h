////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2018-2024 The Octave Project Developers
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

#if ! defined (octave_documentation_h)
#define octave_documentation_h 1

#include <QComboBox>
#include <QMenu>
#include <QShortcut>
#include <QSplitter>
#include <QTextBrowser>
#include <QToolBar>
#include <QListWidget>
#include <QToolButton>
#include <QWidget>
#include <QtHelp/QHelpEngine>

#include "find-widget.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class documentation;
class documentation_bookmarks;

//! Documentation browser derived from Textbrowser

class documentation_browser : public QTextBrowser
{
  Q_OBJECT

public:

  documentation_browser (QHelpEngine *help_engine, QWidget *parent = nullptr);
  ~documentation_browser () = default;

  virtual QVariant loadResource (int type, const QUrl& url);

public Q_SLOTS:

  void handle_index_clicked (const QUrl& url,
                             const QString& keyword = QString ());
  void notice_settings ();
  void save_settings ();

  //! Zooming in and out while taking care of the zoom level
  //!@{
  void zoom_in ();
  void zoom_out ();
  void zoom_original ();
  //!@}

protected:

  void wheelEvent (QWheelEvent *we);

private:

  QHelpEngine *m_help_engine;

  //! Store the current zoom level
  int m_zoom_level;

  //! Minimal and maximal zoom level avoiding calling
  //! zoom_in and zoom_out without actually zooming but
  //! with changing the stored zoom level
  enum
  {
    min_zoom_level = -5,
    max_zoom_level = 10
  };
};

//! The documentation main class derived from QSplitter

class documentation : public QSplitter
{
  Q_OBJECT

public:

  documentation (QWidget *parent);
  ~documentation ();

  /*!
      Generate a string with page name @p title and current anchor
      from @p url for using in prev/next or bookmarks menu:

        @param title current title of the page as QString
        @param url   current url  as QUrl

        @return QString "title: anchor"
  */
  QString title_and_anchor (const QString& title, const QUrl& url);

Q_SIGNALS:

  void show_single_result (const QUrl&);

public Q_SLOTS:

  void notice_settings ();
  void save_settings ();

  void copyClipboard ();
  void pasteClipboard ();
  void selectAll ();

  void load_index ();
  void load_ref (const QString& name = QString ());
  void registerDoc (const QString& name);
  void unregisterDoc (const QString& name);

private Q_SLOTS:

  void global_search ();
  void global_search_started ();
  void global_search_finished (int hits);
  void filter_update (const QString& expression);
  void filter_update_history ();
  void find (const QString&, bool);
  void find_forward_from_anchor (const QString& text);
  void record_anchor_position ();
  void handle_cursor_position_change ();
  void handle_search_result_clicked (const QUrl& url);

  void update_history_menus ();
  void open_hist_url (QAction *a);

private:

  void construct_tool_bar ();
  QAction * add_action (const QIcon& icon, const QString& text,
                        const char *member, QWidget *receiver = nullptr,
                        QToolBar *tool_bar = nullptr);
  void update_history (int new_count, QAction **actions);

  //! Select all occurrences of a string in the doc browser
  void select_all_occurrences (const QString& text);

  QHelpEngine *m_help_engine;
  QString m_internal_search;
  documentation_browser *m_doc_browser;
  documentation_bookmarks *m_bookmarks;
  int m_search_anchor_position;
  QComboBox *m_filter;
  QString m_collection;

  QWidget *m_doc_widget;
  find_widget *m_find_widget;
  QToolBar *m_tool_bar;
  QString m_query_string;

  bool m_indexed;
  QString m_current_ref_name;

  QAction *m_action_go_home;
  QAction *m_action_go_prev;
  QAction *m_action_go_next;
  QMenu *m_prev_pages_menu;
  QMenu *m_next_pages_menu;
  int m_prev_pages_count;
  int m_next_pages_count;

  enum { max_history_entries = 10 };
  QAction *m_prev_pages_actions[max_history_entries];
  QAction *m_next_pages_actions[max_history_entries];

  QAction *m_action_bookmark;
  QAction *m_action_find;

  QAction *m_action_zoom_in;
  QAction *m_action_zoom_out;
  QAction *m_action_zoom_original;
};

OCTAVE_END_NAMESPACE(octave)

#endif
