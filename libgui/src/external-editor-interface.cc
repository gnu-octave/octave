////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QMessageBox>
#include <QProcess>

#include "external-editor-interface.h"
#include "gui-settings.h"
#include "gui-preferences-global.h"
#include "octave-qobject.h"

OCTAVE_BEGIN_NAMESPACE(octave)

external_editor_interface::external_editor_interface (QWidget *p,
                                                      base_qobject& oct_qobj)
: QWidget (p), m_octave_qobj (oct_qobj)
{ }

// Calling the external editor
bool
external_editor_interface::call_custom_editor (const QString& file, int line)
{
  QString editor = external_editor ();
  if (editor.isEmpty ())
    return true;

  if (line < 0)
    line = 0;

  // replace macros
  editor.replace ("%f", file);
  editor.replace ("%l", QString::number (line));

  QStringList arguments = editor.split (QRegExp("\\s+"));
  editor = arguments.takeFirst ();

  // start the process and check for success
  bool started_ok = QProcess::startDetached (editor, arguments);

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

void external_editor_interface::request_new_file (const QString&)
{
  call_custom_editor ();
}

void external_editor_interface::request_open_file (const QString& file_name,
                                                   const QString&, int line,
                                                   bool, bool, bool,
                                                   const QString&)
{
  call_custom_editor (file_name, line);
}

void external_editor_interface::handle_edit_file_request (const QString& file)
{
  call_custom_editor (file);
}

// Get and verify the settings of the external editor program
QString external_editor_interface::external_editor (void)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  QString editor = settings->value (global_custom_editor.key,
                                    global_custom_editor.def).toString ();

  // check the settings (avoid an empty string)
  if (editor.trimmed ().isEmpty ())
    {
      QMessageBox *msgBox
        = new QMessageBox (QMessageBox::Warning,
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

OCTAVE_END_NAMESPACE(octave)
