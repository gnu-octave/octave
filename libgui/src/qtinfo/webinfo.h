/* Copyright (C) 2009 P.L. Lucas
 * Copyright (C) 2012 Jacob Dawid <jacob.dawid@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <QTextBrowser>
#include "parser.h"
#include <QStackedWidget>
#include <QTabBar>
#include <QPushButton>
#include <QLineEdit>
#include <QCheckBox>
#include <QToolButton>

class webinfo : public QWidget
{
  Q_OBJECT
public:
  webinfo (QWidget *parent = 0);
  void set_info_path (const QString& info_path);
  void load_node (const QString& node_name);

public slots:
  void link_clicked (const QUrl& link);
  void current_tab_changed (int index);
  void close_tab (int index);
  void search ();
  void zoom_in ();
  void zoom_out ();

private:
  QTextBrowser        *_text_browser;
  QTabBar             *_tab_bar;
  QStackedWidget      *_stacked_widget;
  QLineEdit           *_search_line_edit;
  QCheckBox           *_search_check_box;
  QToolButton         *_zoom_in_button;
  QToolButton         *_zoom_out_button;

  parser              _parser;
  QFont               _font_web;

  QTextBrowser *addNewTab (const QString& name);
};
