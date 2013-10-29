/*

Copyright (C) 2013 John W. Eaton
Copyright (C) 2011-2013 Jacob Dawid

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <QApplication>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QLabel>
#include <QPushButton>

#include "welcome-wizard.h"
#include "resource-manager.h"

welcome_wizard::welcome_wizard (QWidget *p)
  : QDialog (p)
{
  setWindowTitle (tr ("Welcome to GNU Octave"));

  setEnabled (true);
  resize (600, 480);
  setMinimumSize (QSize (600, 480));


  QVBoxLayout *page_layout = new QVBoxLayout (this);
  setLayout (page_layout);

  QHBoxLayout *message_and_logo = new QHBoxLayout;

  QVBoxLayout *message = new QVBoxLayout;

  QLabel *title = new QLabel (tr ("Welcome to Octave!"));
  QFont ft;
  ft.setPointSize (20);
  title->setFont (ft);

  QLabel *msg_1 = new QLabel (
    tr ("You seem to be using the Octave graphical interface for the first  time on this computer.  Click 'Finish' to write a configuration file  and launch Octave GUI."));
  msg_1->setWordWrap (true);

  QString msg_2_text = QString (tr ("The configuration file is stored in %1. "
                                    "If that file exists, you will not see this "
                                    "dialog when Octave starts again.").
                                arg (resource_manager::get_settings_file ()));
  QLabel *msg_2 = new QLabel (msg_2_text);
  msg_2->setWordWrap (true);

  message->addWidget (title);
  message->addWidget (msg_1);
  message->addWidget (msg_2);

  QSpacerItem *logo_filler = new QSpacerItem (40, 20, QSizePolicy::Expanding,
                                              QSizePolicy::Minimum);

  QLabel *logo = new QLabel;
  QPixmap logo_pixmap (":/actions/icons/logo.png");
  logo->setPixmap (logo_pixmap.scaledToHeight (150));

  message_and_logo->addLayout (message);
  message_and_logo->addItem (logo_filler);
  message_and_logo->addWidget (logo);

  QLabel *links = new QLabel
    (tr ("<html><head>\n"
         "<style>\n"
         "a:link { text-decoration: underline; color: #0000ff; }\n"
         "</style>\n"
         "<head/><body>\n"
         "<p>For more information about Octave:</p>\n"
         "<ul>\n"
         "<li>Visit <a href=\"http://octave.org\">http://octave.org</a></li>\n"
         "<li>Get the documentation online as <a href=\"http://www.gnu.org/software/octave/doc/interpreter/index.html\">html</a>- or <a href=\"http://www.gnu.org/software/octave/octave.pdf\">pdf</span></a>-document</li>\n"
         "<li>Open the documentation browser of Octave GUI with the help menu</li>\n"
         "</ul>\n"
         "</body></html>"),
     this);
  links->setWordWrap (true);
  links->setOpenExternalLinks (true);

  QSpacerItem *hfill = new QSpacerItem (40, 20, QSizePolicy::Expanding,
                                        QSizePolicy::Minimum);

  QPushButton *finish_button = new QPushButton;
  finish_button->setText (tr ("Finish"));

  QSpacerItem *vspace = new QSpacerItem (20, 40, QSizePolicy::Minimum);

  QHBoxLayout *button_bar = new QHBoxLayout;

  button_bar->addItem (hfill);
  button_bar->addWidget (finish_button);

  QSpacerItem *vfill = new QSpacerItem (20, 40, QSizePolicy::Minimum,
                                        QSizePolicy::Expanding);

  page_layout->addLayout (message_and_logo);
  page_layout->addWidget (links);
  page_layout->addItem (vspace);
  page_layout->addLayout (button_bar);
  page_layout->addItem (vfill);

  connect (finish_button, SIGNAL (clicked ()), this, SLOT (accept ()));
}
