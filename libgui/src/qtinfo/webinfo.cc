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

#include "webinfo.h"
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QApplication>
#include <QClipboard>

#include "file-ops.h"
#include "help.h"
#include "defaults.h"
#include "resource-manager.h"
#include "shortcut-manager.h"

namespace octave
{
  webinfo::webinfo (QWidget *p)
    : QWidget (p)
  {
    _font_web = font ();

    QVBoxLayout *vbox_layout = new QVBoxLayout ();
    vbox_layout->setMargin (0);
    setLayout (vbox_layout);

    QHBoxLayout *hbox_layout = new QHBoxLayout ();
    hbox_layout->setMargin (0);
    hbox_layout->setSpacing (0);
    vbox_layout->addLayout (hbox_layout);

    _tab_bar = new octave::tab_bar (this);
    _tab_bar->setSizePolicy (QSizePolicy::Preferred,QSizePolicy::Preferred);
    _tab_bar->setExpanding (false);
    _tab_bar->setTabsClosable (true);
#if defined (HAVE_QTABWIDGET_SETMOVABLE)
    _tab_bar->setMovable (true);
#endif
    hbox_layout->addWidget (_tab_bar);

    _zoom_in_button = new QToolButton (this);
    _zoom_in_button->setIcon (resource_manager::icon ("zoom-in"));
    hbox_layout->addWidget (_zoom_in_button);

    _zoom_out_button = new QToolButton (this);
    _zoom_out_button->setIcon (resource_manager::icon ("zoom-out"));
    hbox_layout->addWidget (_zoom_out_button);

    _stacked_widget = new QStackedWidget (this);
    vbox_layout->addWidget (_stacked_widget);

    hbox_layout = new QHBoxLayout ();
    vbox_layout->addLayout (hbox_layout);

    _search_line_edit = new QLineEdit (this);
#if defined (HAVE_QLINEEDIT_SETPLACEHOLDERTEXT)
    _search_line_edit->setPlaceholderText (
      tr ("Type here and press \'Return\' to search"));
#endif
    hbox_layout->addWidget (_search_line_edit);

    _search_check_box = new QCheckBox (tr ("Global search"));
    hbox_layout->addWidget (_search_check_box);

    _close_action = add_action (_tab_bar->get_context_menu (),
          resource_manager::icon ("window-close",false), tr ("&Close"),
          this, SLOT (request_close_tab (bool)));
    _close_others_action = add_action (_tab_bar->get_context_menu (),
          resource_manager::icon ("window-close",false), tr ("Close &Other Tabs"),
          this, SLOT (request_close_other_tabs (bool)));
    _close_action->setEnabled (false);
    _close_others_action->setEnabled (false);

    connect (_tab_bar, SIGNAL (close_current_tab_signal (bool)),
             this, SLOT (request_close_tab (bool)));
    connect (_tab_bar, SIGNAL (tabCloseRequested (int)),
             this, SLOT (close_tab (int)));
    connect (_tab_bar, SIGNAL (currentChanged (int)), this,
             SLOT (current_tab_changed (int)));
    connect (_zoom_in_button, SIGNAL (clicked ()), this, SLOT (zoom_in ()));
    connect (_zoom_out_button, SIGNAL (clicked ()), this, SLOT (zoom_out ()));
    connect (_search_line_edit, SIGNAL (returnPressed ()), this, SLOT (search ()));

    // Actions for tab navigation
    m_switch_left_tab_action
      = add_action (nullptr, QIcon (), "",
                    _tab_bar, SLOT (switch_left_tab (void)));
    m_switch_right_tab_action
      = add_action (nullptr, QIcon (), "",
                    _tab_bar, SLOT (switch_right_tab (void)));
    m_move_tab_left_action
      = add_action (nullptr, QIcon (), "",
                    _tab_bar, SLOT (move_tab_left (void)));
    m_move_tab_right_action
      = add_action (nullptr, QIcon (), "",
                    _tab_bar, SLOT (move_tab_right (void)));

    resize (500, 300);
  }

  // Add an action to a menu or the widget itself
  QAction* webinfo::add_action (QMenu *menu, const QIcon& icon,
                                const QString& text,
                                QWidget* receiver, const char *member)
  {
    QAction *a;

    if (menu)
      a = menu->addAction (icon, text, receiver, member);
    else
      {
        a = new QAction (this);
        connect (a, SIGNAL (triggered ()), receiver, member);
      }

    addAction (a);  // important for shortcut context
    a->setShortcutContext (Qt::WidgetWithChildrenShortcut);

    return a;
  }

  // Slot for the close tab action
  void webinfo::request_close_tab (bool)
  {
    close_tab (_tab_bar->currentIndex ());
  }

  // Slot for the close other tabs action
  void webinfo::request_close_other_tabs (bool)
  {
    int current = _tab_bar->currentIndex ();

    for (int index = _tab_bar->count ()-1; index >= 0; index--)
      {
        if (current != index)
          close_tab (index);
      }
  }

  void webinfo::load_info_file (const QString& info_file)
  {
    if (! set_info_path (info_file))
      {
        // Info file does not exist
        _search_check_box->setEnabled (false);
        _search_line_edit->setEnabled (false);

        QTextBrowser *msg = addNewTab (tr ("Error"));
        QString msg_text = QString (
                                    "<html><body><br><br><center><b>%1</b></center></body></html>").
          arg (tr ("The info file<p>%1<p>or compressed versions do not exist").
               arg (info_file));
        msg->setHtml (msg_text);
      }
  }

  bool webinfo::set_info_path (const QString& info_path)
  {
    if (_parser.set_info_path (info_path))
      {
        load_node ("Top");
        return true;
      }
    else
      return false;
  }

  void webinfo::load_node (const QString& node_name)
  {
    // no XREF in the tabs
    QString tab_text = node_name;
    tab_text.replace ("XREF", "");

    //Check if node has been already opened.
    for (int i = 0; i < _tab_bar->count (); i++)
      {
        if (tab_text == _tab_bar->tabText (i))
          {
            _tab_bar->setCurrentIndex (i);
            return;
          }
      }

    _text_browser = addNewTab (tab_text);
    _text_browser->setHtml (_parser.node_as_html (node_name, "anchor"));

    if (_parser.is_reference (node_name))
      _text_browser->scrollToAnchor ("anchor");
  }

  void webinfo::link_clicked (const QUrl& link)
  {
    QString node = link.toString ();
    if (node.at (0) != '#')
      load_node (node);
    else
      _text_browser->scrollToAnchor (node);
  }

  void webinfo::tab_state_changed (void)
  {
    _close_action->setEnabled (_tab_bar->count () > 1);
    _close_others_action->setEnabled (_tab_bar->count () > 1);
    setFocusProxy (_stacked_widget->currentWidget ());
  }

  void webinfo::current_tab_changed (int index)
  {
    QVariant tab_data = _tab_bar->tabData (index);
    _text_browser = static_cast<QTextBrowser *> (tab_data.value<void*> ());

    _stacked_widget->setCurrentIndex (_stacked_widget->indexOf (_text_browser));

    if (_text_browser->font () != _font_web)
      _text_browser->setFont (_font_web);

    tab_state_changed ();
  }

  QTextBrowser * webinfo::addNewTab (const QString& name)
  {
    _text_browser = new QTextBrowser (this);
    _text_browser->setOpenLinks (false);
    _text_browser->show ();

    connect (_text_browser, SIGNAL (anchorClicked (const QUrl&)), this,
             SLOT (link_clicked (const QUrl&)));
    disconnect (_tab_bar, SIGNAL (currentChanged (int)), this,
                SLOT (current_tab_changed (int)));

    int ns = _stacked_widget->addWidget (_text_browser);
    _stacked_widget->setCurrentIndex (ns);

    int nt = _tab_bar->addTab (name);
    _tab_bar->setCurrentIndex (nt);
    QVariant tab_data;
    tab_data.setValue (static_cast<void *> (_text_browser));
    _tab_bar->setTabData (nt, tab_data);

    connect (_tab_bar, SIGNAL (currentChanged (int)), this,
             SLOT (current_tab_changed (int)));

    tab_state_changed ();

    if (_text_browser->font () != _font_web)
      _text_browser->setFont (_font_web);

    return _text_browser;
  }

  void webinfo::close_tab (int index)
  {
    if (_tab_bar->count () > 1)
      {
        QVariant tab_data = _tab_bar->tabData (index);
        QWidget *w = static_cast<QWidget *> (tab_data.value<void*> ());
        _stacked_widget->removeWidget (w);
        delete w;

        _tab_bar->removeTab (index);
      }

    tab_state_changed ();

    setFocus ();
  }

  void webinfo::load_ref (const QString& ref_name)
  {
    // Will load "Top", if ref_name was not found.
    load_node (_parser.find_reference (ref_name));

    if (_text_browser)
      _text_browser->setFocus ();
  }

  void webinfo::search (void)
  {
    if (_search_line_edit->text ().trimmed ().isEmpty ())
      return;   // do nothing if search field is empty or only has whitespaces

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

  void webinfo::zoom_in (void)
  {
    _font_web.setPointSize (_font_web.pointSize () + 1);
    _text_browser->setFont (_font_web);
  }

  void webinfo::zoom_out (void)
  {
    _font_web.setPointSize (_font_web.pointSize () - 1);
    _text_browser->setFont (_font_web);
  }

  void webinfo::copyClipboard (void)
  {
    if (_search_line_edit->hasFocus () && _search_line_edit->hasSelectedText ())
      {
        QClipboard *clipboard = QApplication::clipboard ();

        clipboard->setText (_search_line_edit->selectedText ());
      }

    if (_text_browser->hasFocus ())
      _text_browser->copy ();
  }

  void webinfo::selectAll (void)
  {
    if (_search_line_edit->hasFocus ())
      _search_line_edit->selectAll ();

    if (_text_browser->hasFocus ())
      _text_browser->selectAll ();
  }

  void webinfo::pasteClipboard (void)
  {
    if (_search_line_edit->hasFocus ())
      {
        QClipboard *clipboard = QApplication::clipboard ();
        QString str = clipboard->text ();
        if (str.length () > 0)
          _search_line_edit->insert (str);
      }
  }

  void webinfo::notice_settings (const QSettings *)
  {
    shortcut_manager::set_shortcut (_close_action, "editor_file:close");
    shortcut_manager::set_shortcut (_close_others_action, "editor_file:close_other");
    shortcut_manager::set_shortcut (m_switch_left_tab_action, "editor_tabs:switch_left_tab");
    shortcut_manager::set_shortcut (m_switch_right_tab_action, "editor_tabs:switch_right_tab");
    shortcut_manager::set_shortcut (m_move_tab_left_action, "editor_tabs:move_tab_left");
    shortcut_manager::set_shortcut (m_move_tab_right_action, "editor_tabs:move_tab_right");
  }
}

