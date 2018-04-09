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
// Author: 2012 Jacob Dawid <jacob.dawid@cybercatalyst.com>

#if ! defined (octave_webinfo_h)
#define octave_webinfo_h 1

#include <QTextBrowser>
#include "texinfo-parser.h"
#include <QStackedWidget>
#include <QTabBar>
#include <QPushButton>
#include <QLineEdit>
#include <QCheckBox>
#include <QToolButton>
#include <QMenu>
#include <QAction>
#include <QMouseEvent>
#include <QSettings>

#include "tab-bar.h"

namespace octave
{
  // The webinfo class
  class webinfo : public QWidget
  {
    Q_OBJECT

  public:

    webinfo (QWidget *parent = nullptr);

    bool set_info_path (const QString& info_path);
    void load_node (const QString& node_name);
    void load_ref (const QString& ref_name);
    void notice_settings (const QSettings *settings);

    void load_info_file (const QString& info_file);

  public slots:

    void link_clicked (const QUrl& link);
    void current_tab_changed (int index);
    void close_tab (int index);
    void search (void);
    void zoom_in (void);
    void zoom_out (void);

    void copyClipboard (void);
    void pasteClipboard (void);
    void selectAll (void);

    void request_close_tab (bool);
    void request_close_other_tabs (bool);

  private:

    QAction *_close_action;
    QAction *_close_others_action;
    QAction *m_switch_left_tab_action;
    QAction *m_switch_right_tab_action;
    QAction *m_move_tab_left_action;
    QAction *m_move_tab_right_action;

    QTextBrowser *_text_browser;
    tab_bar *_tab_bar;
    QStackedWidget *_stacked_widget;
    QLineEdit *_search_line_edit;
    QCheckBox *_search_check_box;
    QToolButton *_zoom_in_button;
    QToolButton *_zoom_out_button;

    texinfo_parser _parser;
    QFont _font_web;

    QTextBrowser * addNewTab (const QString& name);

    QAction * add_action (QMenu *menu, const QIcon& icon, const QString& text,
                          QWidget *receiver, const char *member);

    void tab_state_changed (void);

  };
}

#endif
