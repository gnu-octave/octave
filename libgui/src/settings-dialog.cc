/*

Copyright (C) 2011-2012 Jacob Dawid

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

#include "resource-manager.h"
#include "settings-dialog.h"
#include "ui-settings-dialog.h"
#include <QSettings>
#include <QDir>
#include <QFileInfo>

settings_dialog::settings_dialog (QWidget *p):
  QDialog (p), ui (new Ui::settings_dialog)
{
  ui->setupUi (this);

  QSettings *settings = resource_manager::get_settings ();
  // FIXME -- what should happen if settings is 0?

  // look for available language files and the actual settings
  QString qm_dir_name = resource_manager::get_gui_translation_dir ();
  QDir qm_dir (qm_dir_name);
  QFileInfoList qm_files = qm_dir.entryInfoList (QStringList ("*.qm"),
                                                 QDir::Files | QDir::Readable,
                                                 QDir::Name);
  for (int i = 0; i < qm_files.length (); i++)    // insert available languages
    ui->comboBox_language->addItem (qm_files.at (i).baseName ());
  ui->comboBox_language->insertItem (0,tr("System setting")); // System at beginning
  ui->comboBox_language->insertSeparator (1);         // separator after System
  QString language = settings->value ("language","SYSTEM").toString ();
  if (language == "SYSTEM")
    language = tr("System setting");
  int selected = ui->comboBox_language->findText (language);
  if (selected >= 0)
    ui->comboBox_language->setCurrentIndex (selected);
  else
    ui->comboBox_language->setCurrentIndex (0);  // System is default

  // which icon has to be selected
  QString widget_icon_set =
      settings->value ("DockWidgets/widget_icon_set","NONE").toString ();
  ui->general_icon_octave-> setChecked (true);  // the default (if invalid set)
  ui->general_icon_octave-> setChecked (widget_icon_set == "NONE");
  ui->general_icon_graphic-> setChecked (widget_icon_set == "GRAPHIC");
  ui->general_icon_letter-> setChecked (widget_icon_set == "LETTER");

  ui->useCustomFileEditor->setChecked (settings->value ("useCustomFileEditor").toBool ());
  ui->customFileEditor->setText (settings->value ("customFileEditor").toString ());
  ui->editor_showLineNumbers->setChecked (settings->value ("editor/showLineNumbers",true).toBool () );
  ui->editor_highlightCurrentLine->setChecked (settings->value ("editor/highlightCurrentLine",true).toBool () );
  ui->editor_codeCompletion->setChecked (settings->value ("editor/codeCompletion",true).toBool () );
  ui->editor_fontName->setCurrentFont (QFont (settings->value ("editor/fontName","Courier").toString()) );
  ui->editor_fontSize->setValue (settings->value ("editor/fontSize",10).toInt ());
  ui->editor_longWindowTitle->setChecked (settings->value ("editor/longWindowTitle",false).toBool ());
  ui->editor_restoreSession->setChecked (settings->value ("editor/restoreSession",true).toBool ());
  ui->terminal_fontName->setCurrentFont (QFont (settings->value ("terminal/fontName","Courier").toString()) );
  ui->terminal_fontSize->setValue (settings->value ("terminal/fontSize",10).toInt ());
  ui->showFilenames->setChecked (settings->value ("showFilenames").toBool());
  ui->showFileSize->setChecked (settings->value ("showFileSize").toBool());
  ui->showFileType->setChecked (settings->value ("showFileType").toBool());
  ui->showLastModified->setChecked (settings->value ("showLastModified").toBool());
  ui->showHiddenFiles->setChecked (settings->value ("showHiddenFiles").toBool());
  ui->useAlternatingRowColors->setChecked (settings->value ("useAlternatingRowColors").toBool());
  ui->useProxyServer->setChecked (settings->value ("useProxyServer").toBool ());
  ui->proxyHostName->setText (settings->value ("proxyHostName").toString ());
  ui->terminal_cursorBlinking->setChecked (settings->value ("terminal/cursorBlinking").toBool ());

  QString cursorType = settings->value ("terminal/cursorType").toString ();

  QStringList items;
  items << QString("0") << QString("1") << QString("2");
  ui->terminal_cursorType->addItems(items);
  ui->terminal_cursorType->setItemText (0, "IBeam Cursor");
  ui->terminal_cursorType->setItemText (1, "Block Cursor");
  ui->terminal_cursorType->setItemText (2, "Underline Cursor");

  if (cursorType == "ibeam")
    ui->terminal_cursorType->setCurrentIndex (0);
  else if (cursorType == "block")
    ui->terminal_cursorType->setCurrentIndex (1);
  else if (cursorType == "underline")
    ui->terminal_cursorType->setCurrentIndex (2);

  int currentIndex = 0;
  QString proxyTypeString = settings->value ("proxyType").toString ();
  while ( (currentIndex < ui->proxyType->count ()) && (ui->proxyType->currentText () != proxyTypeString))
    {
      currentIndex++;
      ui->proxyType->setCurrentIndex (currentIndex);
    }

  ui->proxyPort->setText (settings->value ("proxyPort").toString ());
  ui->proxyUserName->setText (settings->value ("proxyUserName").toString ());
  ui->proxyPassword->setText (settings->value ("proxyPassword").toString ());
}

settings_dialog::~settings_dialog ()
{
  delete ui;
}

void
settings_dialog::write_changed_settings ()
{
  QSettings *settings = resource_manager::get_settings ();
  // FIXME -- what should happen if settings is 0?

  // the icon set
  QString widget_icon_set = "NONE";
  if (ui->general_icon_letter->isChecked ())
    widget_icon_set = "LETTER";
  else if (ui->general_icon_graphic->isChecked ())
    widget_icon_set = "GRAPHIC";
  settings->setValue ("DockWidgets/widget_icon_set",widget_icon_set);

  // language
  QString language = ui->comboBox_language->currentText ();
  if (language == tr("System setting"))
    language = "SYSTEM";
  settings->setValue ("language", language);

  // other settings
  settings->setValue ("useCustomFileEditor", ui->useCustomFileEditor->isChecked ());
  settings->setValue ("customFileEditor", ui->customFileEditor->text ());
  settings->setValue ("editor/showLineNumbers", ui->editor_showLineNumbers->isChecked ());
  settings->setValue ("editor/highlightCurrentLine", ui->editor_highlightCurrentLine->isChecked ());
  settings->setValue ("editor/codeCompletion", ui->editor_codeCompletion->isChecked ());
  settings->setValue ("editor/fontName", ui->editor_fontName->currentFont().family());
  settings->setValue ("editor/fontSize", ui->editor_fontSize->value());
  settings->setValue ("editor/longWindowTitle", ui->editor_longWindowTitle->isChecked());
  settings->setValue ("editor/restoreSession", ui->editor_restoreSession->isChecked ());
  settings->setValue ("terminal/fontSize", ui->terminal_fontSize->value());
  settings->setValue ("terminal/fontName", ui->terminal_fontName->currentFont().family());
  settings->setValue ("showFilenames", ui->showFilenames->isChecked ());
  settings->setValue ("showFileSize", ui->showFileSize->isChecked ());
  settings->setValue ("showFileType", ui->showFileType->isChecked ());
  settings->setValue ("showLastModified", ui->showLastModified->isChecked ());
  settings->setValue ("showHiddenFiles", ui->showHiddenFiles->isChecked ());
  settings->setValue ("useAlternatingRowColors", ui->useAlternatingRowColors->isChecked ());
  settings->setValue ("useProxyServer", ui->useProxyServer->isChecked ());
  settings->setValue ("proxyType", ui->proxyType->currentText ());
  settings->setValue ("proxyHostName", ui->proxyHostName->text ());
  settings->setValue ("proxyPort", ui->proxyPort->text ());
  settings->setValue ("proxyUserName", ui->proxyUserName->text ());
  settings->setValue ("proxyPassword", ui->proxyPassword->text ());
  settings->setValue ("terminal/cursorBlinking", ui->terminal_cursorBlinking->isChecked ());

  // the cursor
  QString cursorType;
  switch (ui->terminal_cursorType->currentIndex ())
    {
    case 0: cursorType = "ibeam"; break;
    case 1: cursorType = "block"; break;
    case 2: cursorType = "underline";  break;
    }
  settings->setValue ("terminal/cursorType", cursorType);
  settings->sync ();
}
