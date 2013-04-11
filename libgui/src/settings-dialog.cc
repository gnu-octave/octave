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

#ifdef HAVE_QSCINTILLA
#include <QScrollArea>
#include "color-picker.h"
#include <Qsci/qscilexercpp.h>
#include <Qsci/qscilexerbash.h>
#include <Qsci/qscilexerperl.h>
#include <Qsci/qscilexerbatch.h>
#include <Qsci/qscilexerdiff.h>
#endif

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

  ui->useCustomFileEditor->setChecked (settings->value ("useCustomFileEditor",false).toBool ());
  ui->customFileEditor->setText (settings->value ("customFileEditor").toString ());
  ui->editor_showLineNumbers->setChecked (settings->value ("editor/showLineNumbers",true).toBool () );
  ui->editor_highlightCurrentLine->setChecked (settings->value ("editor/highlightCurrentLine",true).toBool () );
  ui->editor_codeCompletion->setChecked (settings->value ("editor/codeCompletion",true).toBool () );
  ui->editor_longWindowTitle->setChecked (settings->value ("editor/longWindowTitle",false).toBool ());
  ui->editor_restoreSession->setChecked (settings->value ("editor/restoreSession",true).toBool ());
  ui->terminal_fontName->setCurrentFont (QFont (settings->value ("terminal/fontName","Courier New").toString()) );
  ui->terminal_fontSize->setValue (settings->value ("terminal/fontSize",10).toInt ());
  ui->showFileSize->setChecked (settings->value ("filesdockwidget/showFileSize",false).toBool());
  ui->showFileType->setChecked (settings->value ("filesdockwidget/showFileType",false).toBool());
  ui->showLastModified->setChecked (settings->value ("filesdockwidget/showLastModified",false).toBool());
  ui->showHiddenFiles->setChecked (settings->value ("filesdockwidget/showHiddenFiles",false).toBool());
  ui->useAlternatingRowColors->setChecked (settings->value ("filesdockwidget/useAlternatingRowColors",true).toBool());
  ui->sync_octave_directory->setChecked (settings->value ("filesdockwidget/sync_octave_directory",true).toBool());
  ui->useProxyServer->setChecked (settings->value ("useProxyServer",false).toBool ());
  ui->proxyHostName->setText (settings->value ("proxyHostName").toString ());
  ui->terminal_cursorBlinking->setChecked (settings->value ("terminal/cursorBlinking",true).toBool ());

  QString cursorType = settings->value ("terminal/cursorType","ibeam").toString ();

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

#ifdef HAVE_QSCINTILLA
  // editor styles: create lexer, read settings, and create dialog elements
  QsciLexer *lexer;
  lexer = new lexer_octave_gui ();
  read_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new QsciLexerCPP ();
  read_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new QsciLexerPerl ();
  read_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new QsciLexerBatch ();
  read_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new QsciLexerDiff ();
  read_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new QsciLexerBash ();
  read_lexer_settings (lexer,settings);
  delete lexer;
#endif    
}

settings_dialog::~settings_dialog ()
{
  delete ui;
}


#ifdef HAVE_QSCINTILLA
void
settings_dialog::read_lexer_settings (QsciLexer *lexer, QSettings *settings)
{
  lexer->readSettings (*settings);
  int styles = 0;
  while (lexer->description(styles) != "")
    styles++;
  QGridLayout *style_grid = new QGridLayout ();
  QLabel *description[styles];
  QFontComboBox *select_font[styles];
  QSpinBox *font_size[styles];
  QCheckBox *attrib_font[3][styles];
  color_picker *color[styles];
  int default_size = 10;
  QFont default_font = QFont ();
  for (int i = 0; i < styles; i++)  // create dialog elements for all styles
    {
      QString actual_name = lexer->description (i);
      QFont   actual_font = lexer->font (i);
      description[i] = new QLabel (actual_name);
      select_font[i] = new QFontComboBox ();
      select_font[i]->setObjectName (actual_name+"_font");
      font_size[i] = new QSpinBox ();
      font_size[i]->setObjectName (actual_name+"_size");
      if (i == 0) // the default
        {
          select_font[i]->setCurrentFont (actual_font);
          default_font = actual_font;
          font_size[i]->setRange (6,24);
          default_size = actual_font.pointSize ();
          font_size[i]->setValue (default_size);
        }
      else   // other styles
        {
          select_font[i]->setCurrentFont (actual_font);
          if (actual_font.family () == default_font.family ())
            select_font[i]->setEditText (lexer->description (0));
          font_size[i]->setRange (-4,4);
          font_size[i]->setValue (actual_font.pointSize ()-default_size);
          font_size[i]->setToolTip ("Difference to the defalt size");
        }
      attrib_font[0][i] = new QCheckBox (tr("b"));
      attrib_font[1][i] = new QCheckBox (tr("i"));
      attrib_font[2][i] = new QCheckBox (tr("u"));
      attrib_font[0][i]->setChecked(Qt::Checked && actual_font.bold ());
      attrib_font[0][i]->setObjectName (actual_name+"_bold");
      attrib_font[1][i]->setChecked(Qt::Checked && actual_font.italic ());
      attrib_font[1][i]->setObjectName (actual_name+"_italic");
      attrib_font[2][i]->setChecked(Qt::Checked && actual_font.underline ());
      attrib_font[2][i]->setObjectName (actual_name+"_underline");
      color[i] = new color_picker (lexer->color (i));
      color[i]->setObjectName (actual_name+"_color");
      int column = 1;
      style_grid->addWidget (description[i],   i,column++);
      style_grid->addWidget (select_font[i],   i,column++);
      style_grid->addWidget (font_size[i],     i,column++);
      style_grid->addWidget (attrib_font[0][i],i,column++);
      style_grid->addWidget (attrib_font[1][i],i,column++);
      style_grid->addWidget (attrib_font[2][i],i,column++);
      style_grid->addWidget (color[i],         i,column++);
    }
  // place grid with elements into the tab
  QScrollArea *scroll_area = new QScrollArea ();
  QWidget *scroll_area_contents = new QWidget ();
  scroll_area_contents->setObjectName (QString (lexer->language ())+"_styles");
  scroll_area_contents->setLayout (style_grid);
  scroll_area->setWidget (scroll_area_contents);
  ui->tabs_editor_styles->addTab (scroll_area,lexer->language ());
}
#endif  


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
  settings->setValue ("editor/longWindowTitle", ui->editor_longWindowTitle->isChecked());
  settings->setValue ("editor/restoreSession", ui->editor_restoreSession->isChecked ());
  settings->setValue ("terminal/fontSize", ui->terminal_fontSize->value());
  settings->setValue ("terminal/fontName", ui->terminal_fontName->currentFont().family());
  settings->setValue ("filesdockwidget/showFileSize", ui->showFileSize->isChecked ());
  settings->setValue ("filesdockwidget/showFileType", ui->showFileType->isChecked ());
  settings->setValue ("filesdockwidget/showLastModified", ui->showLastModified->isChecked ());
  settings->setValue ("filesdockwidget/showHiddenFiles", ui->showHiddenFiles->isChecked ());
  settings->setValue ("filesdockwidget/useAlternatingRowColors", ui->useAlternatingRowColors->isChecked ());
  settings->setValue ("filesdockwidget/sync_octave_directory", ui->sync_octave_directory->isChecked ());
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

#ifdef HAVE_QSCINTILLA
  // editor styles: create lexer, get dialog contents, and write settings
  QsciLexer *lexer;
  lexer = new lexer_octave_gui ();
  write_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new QsciLexerCPP ();
  write_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new QsciLexerPerl ();
  write_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new QsciLexerBatch ();
  write_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new QsciLexerDiff ();
  write_lexer_settings (lexer,settings);
  delete lexer;
  lexer = new QsciLexerBash ();
  write_lexer_settings (lexer,settings);
  delete lexer;
#endif
}

#ifdef HAVE_QSCINTILLA
void
settings_dialog::write_lexer_settings (QsciLexer *lexer, QSettings *settings)
{
  QWidget *tab = ui->tabs_editor_styles->
            findChild <QWidget *>(QString (lexer->language ())+"_styles");
  int styles = 0;
  while (lexer->description(styles) != "")
    styles++;
  QFontComboBox *select_font;
  QSpinBox *font_size;
  QCheckBox *attrib_font[3];
  color_picker *color;
  int default_size = 10;
  QFont default_font = QFont ("Courier New",10,-1,0);
  for (int i = 0; i < styles; i++)  // get dialog elements and their contents
    {
      QString actual_name = lexer->description (i);
      select_font    = tab->findChild <QFontComboBox *>(actual_name+"_font");
      font_size      = tab->findChild <QSpinBox *>(actual_name+"_size");
      attrib_font[0] = tab->findChild <QCheckBox *>(actual_name+"_bold");
      attrib_font[1] = tab->findChild <QCheckBox *>(actual_name+"_italic");
      attrib_font[2] = tab->findChild <QCheckBox *>(actual_name+"_underline");
      color          = tab->findChild <color_picker *>(actual_name+"_color");
      QFont new_font = default_font;
      if (select_font)
        {
          new_font = select_font->currentFont ();
          if (i == 0)
            default_font = new_font;
          else
            if (select_font->currentText () == lexer->description (0))
              new_font = default_font;
        }
      if (font_size)
        {
          if (i == 0)
            {
              default_size = font_size->value ();
              new_font.setPointSize (font_size->value ());
            }
          else
            new_font.setPointSize (font_size->value ()+default_size);
        }
      if (attrib_font[0])
        new_font.setBold (attrib_font[0]->isChecked ());
      if (attrib_font[1])
        new_font.setItalic (attrib_font[1]->isChecked ());
      if (attrib_font[2])
        new_font.setUnderline (attrib_font[2]->isChecked ());
      lexer->setFont (new_font,i);
      if (i == 0)
        lexer->setDefaultFont (new_font);
      if (color)
        lexer->setColor (color->color (),i);
    }
  lexer->writeSettings (*settings);
}
#endif
