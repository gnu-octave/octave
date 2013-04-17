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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "webinfo.h"
#include <QVBoxLayout>
#include <QHBoxLayout>

#include "file-ops.h"
#include "help.h"
#include "defaults.h"


webinfo::webinfo (QWidget *p)
  : QWidget (p)
{
  _font_web = font ();

  QVBoxLayout *vbox_layout = new QVBoxLayout ();
  vbox_layout->setMargin (0);
  setLayout (vbox_layout);

  QHBoxLayout *hbox_layout = new QHBoxLayout ();
  hbox_layout->setMargin (2);
  vbox_layout->addLayout (hbox_layout);

  _tab_bar = new QTabBar (this);
  _tab_bar->setSizePolicy (QSizePolicy::Preferred,QSizePolicy::Preferred);
  _tab_bar->setExpanding (false);
  _tab_bar->setTabsClosable (true);
  _tab_bar->setMovable (true);
  hbox_layout->addWidget (_tab_bar);

  _zoom_in_button = new QToolButton (this);
  _zoom_in_button->setSizePolicy (QSizePolicy::Fixed,QSizePolicy::Preferred);
  _zoom_in_button->setIcon (QIcon (":/actions/icons/zoom-in.png"));
  hbox_layout->addWidget (_zoom_in_button);

  _zoom_out_button = new QToolButton (this);
  _zoom_out_button->setSizePolicy (QSizePolicy::Fixed,QSizePolicy::Preferred);
  _zoom_out_button->setIcon (QIcon (":/actions/icons/zoom-out.png"));
  hbox_layout->addWidget (_zoom_out_button);

  _stacked_widget = new QStackedWidget (this);
  vbox_layout->addWidget (_stacked_widget);

  hbox_layout = new QHBoxLayout ();
  vbox_layout->addLayout (hbox_layout);

  _search_line_edit = new QLineEdit(this);
#ifdef HAVE_SETPLACEHOLDERTEXT
  _search_line_edit->setPlaceholderText (tr ("Type here and press \'Return\' to search"));
#endif
  hbox_layout->addWidget (_search_line_edit);

  _search_check_box = new QCheckBox (tr ("Global search"));
  hbox_layout->addWidget (_search_check_box);

  connect (_tab_bar, SIGNAL (tabCloseRequested (int)), this, SLOT (close_tab (int)));
  connect (_tab_bar, SIGNAL (currentChanged (int)), this, SLOT (current_tab_changed (int)));
  connect (_zoom_in_button, SIGNAL (clicked ()), this, SLOT (zoom_in ()));
  connect (_zoom_out_button, SIGNAL (clicked ()), this, SLOT (zoom_out ()));
  connect (_search_line_edit, SIGNAL (returnPressed ()), this, SLOT (search ()));

  resize (500, 300);

  set_info_path (QString::fromStdString (Vinfo_file));
}

void
webinfo::set_info_path (const QString& info_path)
{
  _parser.set_info_path (info_path);
  load_node ("Top");
}

void
webinfo::load_node (const QString& node_name)
{
  //Check if node has been already opened.
  for (int i = 0;i < _tab_bar->count (); i++)
    {
      if (node_name == _tab_bar->tabText (i))
        {
          _tab_bar->setCurrentIndex (i);
          return;
        }
    }

  QString text = _parser.search_node (node_name);
  int i = _parser.is_ref (node_name);
  _text_browser = addNewTab (node_name);
  _text_browser->setHtml (_parser.node_text_to_html (text, i - 1, "anchor"));

  if (i != -1)
    {
      _text_browser->scrollToAnchor ("anchor");
    }
}

void
webinfo::link_clicked (const QUrl & link)
{
  QString node = link.toString ();
  load_node (node);
}

void
webinfo::current_tab_changed (int index)
{
  QVariant tab_data = _tab_bar->tabData (index);
  _text_browser = static_cast<QTextBrowser*> (tab_data.value<void*> ());

  _stacked_widget->setCurrentIndex (_stacked_widget->indexOf (_text_browser));

  if (_text_browser->font () != _font_web)
    {
      _text_browser->setFont (_font_web);
    }
}

QTextBrowser *
webinfo::addNewTab (const QString& name)
{
  _text_browser = new QTextBrowser (this);
  _text_browser->setOpenLinks (false);
  _text_browser->show ();

  connect (_text_browser, SIGNAL (anchorClicked (const QUrl &)), this, SLOT (link_clicked (const QUrl &)) );
  disconnect(_tab_bar, SIGNAL (currentChanged(int)), this, SLOT (current_tab_changed (int)));

  int ns = _stacked_widget->addWidget (_text_browser);
  _stacked_widget->setCurrentIndex (ns);

  int nt = _tab_bar->addTab (name);
  _tab_bar->setCurrentIndex (nt);
  QVariant tab_data;
  tab_data.setValue (static_cast<void*> (_text_browser));
  _tab_bar->setTabData (nt, tab_data);

  connect (_tab_bar, SIGNAL (currentChanged (int)), this, SLOT (current_tab_changed (int)));

  if (_text_browser->font () != _font_web)
    {
      _text_browser->setFont (_font_web);
    }
  return _text_browser;
}

void
webinfo::close_tab (int index)
{
  if (_tab_bar->count () > 1)
    {
      QVariant tab_data = _tab_bar->tabData (index);
      QWidget *w = static_cast<QWidget*> (tab_data.value<void*> ());
      _stacked_widget->removeWidget (w);
      delete w;

      _tab_bar->removeTab (index);
    }
}

void
webinfo::search ()
{
  if (_search_check_box->isChecked ())
    {
      // Global search
      QString results = _parser.global_search (_search_line_edit->text (), 5);
      _text_browser=addNewTab ("Results for: " + _search_line_edit->text ());
      _text_browser->setHtml (results);
    }
  else
    {
      // Local search
      _text_browser->find (_search_line_edit->text ());
    }
}

void
webinfo::zoom_in ()
{
  _font_web.setPointSize (_font_web.pointSize() + 1);
  _text_browser->setFont (_font_web);
}

void
webinfo::zoom_out ()
{
  _font_web.setPointSize (_font_web.pointSize() - 1);
  _text_browser->setFont (_font_web);
}

