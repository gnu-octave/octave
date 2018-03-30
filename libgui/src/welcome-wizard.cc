/*

Copyright (C) 2013-2018 John W. Eaton
Copyright (C) 2011-2018 Jacob Dawid

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QApplication>
#include <QPushButton>
#include <QHBoxLayout>
#include <QVBoxLayout>

#if defined (OCTAVE_USE_WINDOWS_API)
  #define WIN32_LEAN_AND_MEAN
  #include <windows.h>
#endif

#include "welcome-wizard.h"
#include "resource-manager.h"

static QLabel *
make_octave_logo (QWidget *p = nullptr, int height = 100)
{
  QLabel *logo = new QLabel (p);
  QPixmap logo_pixmap (":/actions/icons/logo.png");
  logo->setPixmap (logo_pixmap.scaledToHeight (height));
  return logo;
};

namespace octave
{
  welcome_wizard::welcome_wizard (QWidget *p)
    : QDialog (p), m_page_ctor_list (), m_page_list_iterator (),
      m_current_page (initial_page::create (this)),
      m_allow_web_connect_state (false)
  {
    m_page_ctor_list.push_back (initial_page::create);
    m_page_ctor_list.push_back (setup_community_news::create);
    m_page_ctor_list.push_back (final_page::create);

    m_page_list_iterator = m_page_ctor_list.begin ();

    setWindowTitle (tr ("Welcome to GNU Octave"));

    setEnabled (true);
    resize (600, 480);
    setMinimumSize (QSize (600, 480));

    show_page ();

#if defined (OCTAVE_USE_WINDOWS_API)
    // HACK to forceshow of dialog if started minimized
    ShowWindow (reinterpret_cast<HWND> (winId ()), SW_SHOWNORMAL);
#endif
  }

  void welcome_wizard::handle_web_connect_option (int state)
  {
    m_allow_web_connect_state = state == Qt::Checked;
  }

  void welcome_wizard::show_page (void)
  {
    delete m_current_page;
    delete layout ();

    m_current_page = (*m_page_list_iterator) (this);

    QVBoxLayout *new_layout = new QVBoxLayout ();
    setLayout (new_layout);

    new_layout->addWidget (m_current_page);
  }

  void welcome_wizard::previous_page (void)
  {
    --m_page_list_iterator;

    show_page ();
  }

  void welcome_wizard::next_page (void)
  {
    ++m_page_list_iterator;

    show_page ();
  }

  void welcome_wizard::accept (void)
  {
    // Create default settings file.

    resource_manager::reload_settings ();

    QSettings *settings = resource_manager::get_settings ();

    if (settings)
      {
        settings->setValue ("news/allow_web_connection",
                            m_allow_web_connect_state);

        settings->sync ();
      }

    QDialog::accept ();
  }

  initial_page::initial_page (welcome_wizard *wizard)
    : QWidget (wizard),
      m_title (new QLabel (tr ("Welcome to Octave!"), this)),
      m_message (new QLabel (this)),
      m_logo (make_octave_logo (this)),
      m_next (new QPushButton (tr ("Next"), this)),
      m_cancel (new QPushButton (tr ("Cancel"), this))
  {
    QFont ft;
    ft.setPointSize (20);
    m_title->setFont (ft);

    m_message->setText
      (tr ("<html><body>\n"
           "<p>You seem to be using the Octave graphical interface for the first time on this computer.\n"
           "Click 'Next' to create a configuration file and launch Octave.</p>\n"
           "<p>The configuration file is stored in<br>%1.</p>\n"
           "</body></html>").
       arg (resource_manager::get_settings_file ()));
    m_message->setWordWrap (true);
    m_message->setMinimumWidth (400);

    QVBoxLayout *message_layout = new QVBoxLayout;

    message_layout->addWidget (m_title);
    message_layout->addWidget (m_message);

    QHBoxLayout *message_and_logo = new QHBoxLayout;

    message_and_logo->addLayout (message_layout);
    message_and_logo->addStretch (10);
    message_and_logo->addWidget (m_logo, 0, Qt::AlignTop);

    QHBoxLayout *button_bar = new QHBoxLayout;

    button_bar->addStretch (10);
    button_bar->addWidget (m_next);
    button_bar->addWidget (m_cancel);

    QVBoxLayout *page_layout = new QVBoxLayout (this);
    setLayout (page_layout);

    page_layout->addLayout (message_and_logo);
    page_layout->addStretch (10);
    page_layout->addLayout (button_bar);

    m_next->setDefault (true);
    m_next->setFocus ();

    connect (m_next, SIGNAL (clicked ()), wizard, SLOT (next_page ()));
    connect (m_cancel, SIGNAL (clicked ()), wizard, SLOT (reject ()));
  }

  setup_community_news::setup_community_news (welcome_wizard *wizard)
    : QWidget (wizard),
      m_title (new QLabel (tr ("Community News"), this)),
      m_message (new QLabel (this)),
      m_checkbox (new QCheckBox (this)),
      m_checkbox_message (new QLabel (this)),
      m_logo (make_octave_logo (this)),
      m_previous (new QPushButton (tr ("Previous"), this)),
      m_next (new QPushButton (tr ("Next"), this)),
      m_cancel (new QPushButton (tr ("Cancel"), this))
  {
    QFont ft;
    ft.setPointSize (20);
    m_title->setFont (ft);

    m_message->setText
      (tr ("<html><body>\n"
           "<p>When Octave starts, it will optionally check the Octave web site for current news and information about the Octave community.\n"
           "The check will happen at most once each day and news will only be displayed if there is something new since the last time you viewed the news.</p>\n"
           "<p>You may also view the news by selecting the \"Community News\" item in the \"Help\" menu, or by visiting\n"
           "<a href=\"https://octave.org/community-news.html\">https://octave.org/community-news.html</a>.</p>\n"
           "</body></html>"));
    m_message->setWordWrap (true);
    m_message->setMinimumWidth (400);
    m_message->setOpenExternalLinks (true);

    QVBoxLayout *message_layout = new QVBoxLayout;

    message_layout->addWidget (m_title);
    message_layout->addWidget (m_message);

    QHBoxLayout *message_and_logo = new QHBoxLayout;

    message_and_logo->addLayout (message_layout);
    message_and_logo->addStretch (10);
    message_and_logo->addWidget (m_logo, 0, Qt::AlignTop);

    QHBoxLayout *checkbox_layout = new QHBoxLayout;

    // FIXME: Synchronize the initial state of this checkbox with the default
    // value of "news/allow_web_connection" stored elsewhere.
    m_checkbox->setCheckState (Qt::Unchecked);

    m_checkbox_message->setText
      (tr ("<html><head>\n"
           "<style>\n"
           "a:link { text-decoration: underline; color: #0000ff; }\n"
           "</style>\n"
           "<head/><body>\n"
           "<p>Allow Octave to connect to the Octave web site when it starts to display current news and information about the Octave community.</p>\n"
           "</body></html>"));
    m_checkbox_message->setWordWrap (true);
    m_checkbox_message->setOpenExternalLinks (true);
    m_checkbox_message->setMinimumWidth (500);

    checkbox_layout->addWidget (m_checkbox, 0, Qt::AlignTop);
    checkbox_layout->addSpacing (20);
    checkbox_layout->addWidget (m_checkbox_message, 0, Qt::AlignTop);
    checkbox_layout->addStretch (10);

    QVBoxLayout *message_logo_and_checkbox = new QVBoxLayout;

    message_logo_and_checkbox->addLayout (message_and_logo);
    message_logo_and_checkbox->addSpacing (20);
    message_logo_and_checkbox->addLayout (checkbox_layout);

    QHBoxLayout *button_bar = new QHBoxLayout;

    button_bar->addStretch (10);
    button_bar->addWidget (m_previous);
    button_bar->addWidget (m_next);
    button_bar->addWidget (m_cancel);

    QVBoxLayout *page_layout = new QVBoxLayout (this);
    setLayout (page_layout);

    page_layout->addLayout (message_logo_and_checkbox);
    page_layout->addStretch (10);
    page_layout->addLayout (button_bar);

    m_next->setDefault (true);
    m_next->setFocus ();

    connect (m_checkbox, SIGNAL (stateChanged (int)),
             wizard, SLOT (handle_web_connect_option (int)));

    connect (m_previous, SIGNAL (clicked ()), wizard, SLOT (previous_page ()));
    connect (m_next, SIGNAL (clicked ()), wizard, SLOT (next_page ()));
    connect (m_cancel, SIGNAL (clicked ()), wizard, SLOT (reject ()));
  }

  final_page::final_page (welcome_wizard *wizard)
    : QWidget (wizard),
      m_title (new QLabel (tr ("Enjoy!"), this)),
      m_message (new QLabel (this)),
      m_logo (make_octave_logo (this)),
      m_links (new QLabel (this)),
      m_previous (new QPushButton (tr ("Previous"), this)),
      m_finish (new QPushButton (tr ("Finish"), this)),
      m_cancel (new QPushButton (tr ("Cancel"), this))
  {
    QFont ft;
    ft.setPointSize (20);
    m_title->setFont (ft);

    m_message->setText
      (tr ("<html><body>\n"
           "<p>We hope you find Octave to be a useful tool.</p>\n"
           "<p>If you encounter problems, there are a number of ways to get help, including commercial support options, a mailing list, a wiki, and other community-based support channels.\n"
           "You can find more information about each of these by visiting <a href=\"https://octave.org/support.html\">https://octave.org/support.html</a> (opens in external browser).</p>\n"
           "</body></html>"));
    m_message->setWordWrap (true);
    m_message->setMinimumWidth (400);
    m_message->setOpenExternalLinks (true);

    QVBoxLayout *message_layout = new QVBoxLayout;

    message_layout->addWidget (m_title);
    message_layout->addWidget (m_message);

    QHBoxLayout *message_and_logo = new QHBoxLayout;

    message_and_logo->addLayout (message_layout);
    message_and_logo->addStretch (10);
    message_and_logo->addWidget (m_logo, 0, Qt::AlignTop);

    m_links->setText
      (tr ("<html><head>\n"
           "<style>\n"
           "a:link { text-decoration: underline; color: #0000ff; }\n"
           "</style>\n"
           "<head/><body>\n"
           "<p>For more information about Octave:</p>\n"
           "<ul>\n"
           "<li>Visit <a href=\"https://octave.org\">https://octave.org</a> (opens in external browser)</li>\n"
           "<li>Get the documentation online as <a href=\"https://www.gnu.org/software/octave/doc/interpreter/index.html\">html</a>- or <a href=\"https://www.gnu.org/software/octave/octave.pdf\">pdf</span></a>-document (opens in external browser)</li>\n"
           "<li>Open the documentation browser of the Octave GUI with the help menu</li>\n"
           "</ul>\n"
           "</body></html>"));
    m_links->setWordWrap (true);
    m_links->setOpenExternalLinks (true);

    QHBoxLayout *button_bar = new QHBoxLayout;

    button_bar->addStretch (10);
    button_bar->addWidget (m_previous);
    button_bar->addWidget (m_finish);
    button_bar->addWidget (m_cancel);

    QVBoxLayout *page_layout = new QVBoxLayout (this);
    setLayout (page_layout);

    page_layout->addLayout (message_and_logo);
    page_layout->addSpacing (20);
    page_layout->addWidget (m_links);
    page_layout->addStretch (10);
    page_layout->addLayout (button_bar);

    m_finish->setDefault (true);
    m_finish->setFocus ();

    connect (m_previous, SIGNAL (clicked ()), wizard, SLOT (previous_page ()));
    connect (m_finish, SIGNAL (clicked ()), wizard, SLOT (accept ()));
    connect (m_cancel, SIGNAL (clicked ()), wizard, SLOT (reject ()));
  }
}
