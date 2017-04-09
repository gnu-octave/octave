/*

Copyright (C) 2017 Torsten

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QSettings>
#include <QMessageBox>
#include <QProcess>

#include "external-editor-interface.h"
#include "resource-manager.h"

external_editor_interface::external_editor_interface (QWidget *main_win)
    : QWidget (main_win)
{
  // Connect the signal for displaying a specific preference dialog
  connect (this, SIGNAL (request_settings_dialog (const QString&)),
           main_win, SLOT (process_settings_dialog_request (const QString&)));
};

// Get and verify the settings of the external editor program
QString
external_editor_interface::external_editor ()
{
  QSettings *settings = resource_manager::get_settings ();
  QString editor = settings->value ("customFileEditor").toString ();

  // check the settings (avoid an empty string)
  if (editor.trimmed ().isEmpty ())
    {
      QMessageBox *msgBox = new QMessageBox (QMessageBox::Warning,
                              tr ("Octave Editor"),
                              tr ("There is no custom editor configured yet.\n"
                                  "Do you want to open the preferences?"),
                              QMessageBox::No | QMessageBox::Yes);
      msgBox->setDefaultButton (QMessageBox::Yes);
      msgBox->setAttribute (Qt::WA_DeleteOnClose);
      int button = msgBox->exec ();

      if (button == QMessageBox::Yes)
        emit request_settings_dialog ("editor");
    }

  return editor;
}

// Calling the external editor
bool
external_editor_interface::call_custom_editor (const QString& file, int line)
{
  if (line > -1)  // check for a specific line (debugging)
    return true;  // yes: do not open a file in external editor

  QString editor = external_editor ();
  if (editor.isEmpty ())
    return true;

  // replace macros
  editor.replace ("%f", file);
  editor.replace ("%l", QString::number (line));

  // start the process and check for success
  bool started_ok = QProcess::startDetached (editor);

  if (started_ok != true)
    {
      QMessageBox *msgBox = new QMessageBox (QMessageBox::Critical,
                               tr ("Octave Editor"),
                               tr ("Could not start custom file editor\n%1").
                               arg (editor),
                               QMessageBox::Ok);

      msgBox->setWindowModality (Qt::NonModal);
      msgBox->setAttribute (Qt::WA_DeleteOnClose);
      msgBox->show ();
    }

  return started_ok;
}


// Slots for the several signals for invoking the editor

void
external_editor_interface::request_new_file (const QString&)
{
  call_custom_editor ();
}

void
external_editor_interface::request_open_file (const QString& file_name,
                const QString&, int line, bool, bool, bool, const QString&)
{
  call_custom_editor (file_name, line);
}

void
external_editor_interface::handle_edit_file_request (const QString& file)
{
  call_custom_editor (file);
}

