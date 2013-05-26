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
#include "workspace-model.h"
#include "settings-dialog.h"
#include "ui-settings-dialog.h"
#include <QDir>
#include <QFileInfo>
#include <QVector>

#ifdef HAVE_QSCINTILLA
#include <QScrollArea>

#if defined (HAVE_QSCI_QSCILEXEROCTAVE_H)
#define HAVE_LEXER_OCTAVE
#include <Qsci/qscilexeroctave.h>
#elif defined (HAVE_QSCI_QSCILEXERMATLAB_H)
#define HAVE_LEXER_MATLAB
#include <Qsci/qscilexermatlab.h>
#endif

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

  ui->toolbar_icon_size->setValue (settings->value ("toolbar_icon_size",24).toInt ());

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

  QVariant default_var = QColor (240, 240, 240);
  QColor setting_color = settings->value ("editor/highlight_current_line_color",
                                          default_var).value<QColor> ();
  _editor_current_line_color = new color_picker (setting_color);
  ui->editor_grid_current_line->addWidget (_editor_current_line_color,0,3);
  _editor_current_line_color->setMinimumSize (50,10);
  _editor_current_line_color->setEnabled (false);
  connect (ui->editor_highlightCurrentLine, SIGNAL (toggled (bool)),
           _editor_current_line_color, SLOT (setEnabled (bool)));
  ui->editor_highlightCurrentLine->setChecked (settings->value ("editor/highlightCurrentLine",true).toBool () );

  ui->editor_codeCompletion->setChecked (settings->value ("editor/codeCompletion",true).toBool () );
  ui->editor_spinbox_ac_threshold->setValue (settings->value ("editor/codeCompletion_threshold",2).toInt ());
  ui->editor_checkbox_ac_keywords->setChecked (settings->value ("editor/codeCompletion_keywords",true).toBool ());
  ui->editor_checkbox_ac_document->setChecked (settings->value ("editor/codeCompletion_document",false).toBool ());
  ui->editor_checkbox_ac_case->setChecked (settings->value ("editor/codeCompletion_case",true).toBool ());
  ui->editor_checkbox_ac_replace->setChecked (settings->value ("editor/codeCompletion_replace",false).toBool ());
  ui->editor_ws_checkbox->setChecked (settings->value ("editor/show_white_space",false).toBool ());
  ui->editor_ws_indent_checkbox->setChecked (settings->value ("editor/show_white_space_indent",false).toBool ());
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
  ui->terminal_cursorUseForegroundColor->setChecked (settings->value ("terminal/cursorUseForegroundColor",true).toBool ());

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

  // qorkspace colors
  read_workspace_colors (settings);

  // terminal colors
  read_terminal_colors (settings);

#ifdef HAVE_QSCINTILLA
  // editor styles: create lexer, read settings, and create dialog elements
  QsciLexer *lexer;
#if defined (HAVE_LEXER_OCTAVE)
  lexer = new QsciLexerOctave ();
  read_lexer_settings (lexer,settings);
  delete lexer;
#elif defined (HAVE_LEXER_MATLAB)
  lexer = new QsciLexerMatlab ();
  read_lexer_settings (lexer,settings);
  delete lexer;
#endif
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

  ui->tabWidget->setCurrentIndex (settings->value("settings/last_tab",0).toInt ());
}

settings_dialog::~settings_dialog ()
{
  delete ui;
}


#ifdef HAVE_QSCINTILLA
int
settings_dialog::get_valid_lexer_styles (QsciLexer *lexer, int styles[])
{
  int max_style = 0;
  int actual_style = 0;
  while (actual_style < MaxStyleNumber && max_style < MaxLexerStyles)
    {
      if ((lexer->description(actual_style)) != "")  // valid style
        styles[max_style++] = actual_style;
      actual_style++;
    }
  return max_style;
}

void
settings_dialog::read_lexer_settings (QsciLexer *lexer, QSettings *settings)
{
  lexer->readSettings (*settings);
  int styles[MaxLexerStyles];  // array for saving valid styles (enum is not continuous)
  int max_style = get_valid_lexer_styles (lexer, styles);
  QGridLayout *style_grid = new QGridLayout ();
  QVector<QLabel*> description (max_style);
  QVector<QFontComboBox*> select_font (max_style);
  QVector<QSpinBox*> font_size (max_style);
  QVector<QCheckBox*> attrib_font (3 * max_style);
  QVector<color_picker*> color (max_style);
  QVector<color_picker*> bg_color (max_style);
  int default_size = 10;
  QFont default_font = QFont ();
  QColor default_color = QColor ();
  QColor dummy_color = QColor (255,0,255);

  for (int i = 0; i < max_style; i++)  // create dialog elements for all styles
    {
      QString actual_name = lexer->description (styles[i]);
      QFont   actual_font = lexer->font (styles[i]);
      description[i] = new QLabel (actual_name);
      description[i]->setWordWrap (true);
      description[i]->setMaximumSize (160,QWIDGETSIZE_MAX);
      description[i]->setMinimumSize (160,1);
      select_font[i] = new QFontComboBox ();
      select_font[i]->setObjectName (actual_name+"_font");
      select_font[i]->setMaximumSize (180,QWIDGETSIZE_MAX);
      select_font[i]->setMinimumSize (180,1);
      font_size[i] = new QSpinBox ();
      font_size[i]->setObjectName (actual_name+"_size");
      if (styles[i] == 0) // the default
        {
          select_font[i]->setCurrentFont (actual_font);
          default_font = actual_font;
          font_size[i]->setRange (6,24);
          default_size = actual_font.pointSize ();
          font_size[i]->setValue (default_size);
          default_color = lexer->defaultPaper ();
          bg_color[i] = new color_picker (default_color);
        }
      else   // other styles
        {
          select_font[i]->setCurrentFont (actual_font);
          if (actual_font.family () == default_font.family ())
            select_font[i]->setEditText (lexer->description (0));
          font_size[i]->setRange (-4,4);
          font_size[i]->setValue (actual_font.pointSize ()-default_size);
          font_size[i]->setToolTip (tr ("Difference to the defalt size"));
          if (lexer->paper (styles[i]) == default_color)
            bg_color[i] = new color_picker (dummy_color);
          else
            bg_color[i] = new color_picker (lexer->paper (styles[i]));
            bg_color[i]->setToolTip
                  (tr ("Background color, pink (255,0,255) means default"));
        }
      attrib_font[0+3*i] = new QCheckBox (tr("b"));
      attrib_font[1+3*i] = new QCheckBox (tr("i"));
      attrib_font[2+3*i] = new QCheckBox (tr("u"));
      attrib_font[0+3*i]->setChecked(Qt::Checked && actual_font.bold ());
      attrib_font[0+3*i]->setObjectName (actual_name+"_bold");
      attrib_font[1+3*i]->setChecked(Qt::Checked && actual_font.italic ());
      attrib_font[1+3*i]->setObjectName (actual_name+"_italic");
      attrib_font[2+3*i]->setChecked(Qt::Checked && actual_font.underline ());
      attrib_font[2+3*i]->setObjectName (actual_name+"_underline");
      color[i] = new color_picker (lexer->color (styles[i]));
      color[i]->setObjectName (actual_name+"_color");
      bg_color[i]->setObjectName (actual_name+"_bg_color");
      int column = 1;
      style_grid->addWidget (description[i],     i, column++);
      style_grid->addWidget (select_font[i],     i, column++);
      style_grid->addWidget (font_size[i],       i, column++);
      style_grid->addWidget (attrib_font[0+3*i], i, column++);
      style_grid->addWidget (attrib_font[1+3*i], i, column++);
      style_grid->addWidget (attrib_font[2+3*i], i, column++);
      style_grid->addWidget (color[i],           i, column++);
      style_grid->addWidget (bg_color[i],        i, column++);
    }
  // place grid with elements into the tab
  QScrollArea *scroll_area = new QScrollArea ();
  QWidget *scroll_area_contents = new QWidget ();
  scroll_area_contents->setObjectName (QString (lexer->language ())+"_styles");
  scroll_area_contents->setLayout (style_grid);
  scroll_area->setWidget (scroll_area_contents);
  ui->tabs_editor_styles->addTab (scroll_area,lexer->language ());

  ui->tabs_editor_styles->setCurrentIndex (
          settings->value("settings/last_editor_styles_tab",0).toInt ());
}
#endif  

void
settings_dialog::read_workspace_colors (QSettings *settings)
{

  QList<QColor> default_colors = resource_manager::storage_class_default_colors ();
  QStringList class_names = resource_manager::storage_class_names ();
  QString class_chars = resource_manager::storage_class_chars ();
  int nr_of_classes = class_chars.length ();

  QGridLayout *style_grid = new QGridLayout ();
  QVector<QLabel*> description (nr_of_classes);
  QVector<color_picker*> color (nr_of_classes);

  int column = 0;
  int row = 0;
  for (int i = 0; i < nr_of_classes; i++)
    {
      description[i] = new QLabel (class_names.at (i));
      description[i]->setAlignment (Qt::AlignRight);
      QVariant default_var = default_colors.at (i);
      QColor setting_color = settings->value ("workspaceview/color_"+class_chars.mid (i,1),
                                              default_var).value<QColor> ();
      color[i] = new color_picker (setting_color);
      color[i]->setObjectName ("color_"+class_chars.mid (i,1));
      color[i]->setMinimumSize (30,10);
      style_grid->addWidget (description[i], row,3*column);
      style_grid->addWidget (color[i],       row,3*column+1);
      if (++column == 3)
        {
          row++;
          column = 0;
        }
    }

  // place grid with elements into the tab
  ui->workspace_colors_box->setLayout (style_grid);
}

void
settings_dialog::read_terminal_colors (QSettings *settings)
{

  QList<QColor> default_colors = resource_manager::terminal_default_colors ();
  QStringList class_names = resource_manager::terminal_color_names ();
  QString class_chars = resource_manager::terminal_color_chars ();
  int nr_of_classes = class_chars.length ();

  QGridLayout *style_grid = new QGridLayout ();
  QVector<QLabel*> description (nr_of_classes);
  QVector<color_picker*> color (nr_of_classes);

  int column = 0;
  int row = 0;
  for (int i = 0; i < nr_of_classes; i++)
    {
      description[i] = new QLabel (class_names.at (i));
      description[i]->setAlignment (Qt::AlignRight);
      QVariant default_var = default_colors.at (i);
      QColor setting_color = settings->value ("terminal/color_"+class_chars.mid (i,1),
                                              default_var).value<QColor> ();
      color[i] = new color_picker (setting_color);
      color[i]->setObjectName ("terminal_color_"+class_chars.mid (i,1));
      color[i]->setMinimumSize (30,10);
      style_grid->addWidget (description[i], row,2*column);
      style_grid->addWidget (color[i],       row,2*column+1);
      if (++column == 2)
        {
          row++;
          column = 0;
        }
    }

  // place grid with elements into the tab
  ui->terminal_colors_box->setLayout (style_grid);
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
  settings->setValue ("toolbar_icon_size", ui->toolbar_icon_size->value ());
  settings->setValue ("useCustomFileEditor", ui->useCustomFileEditor->isChecked ());
  settings->setValue ("customFileEditor", ui->customFileEditor->text ());
  settings->setValue ("editor/showLineNumbers", ui->editor_showLineNumbers->isChecked ());
  settings->setValue ("editor/highlightCurrentLine", ui->editor_highlightCurrentLine->isChecked ());
  settings->setValue ("editor/highlight_current_line_color",_editor_current_line_color->color ());
  settings->setValue ("editor/codeCompletion", ui->editor_codeCompletion->isChecked ());
  settings->setValue ("editor/codeCompletion_threshold", ui->editor_spinbox_ac_threshold->value ());
  settings->setValue ("editor/codeCompletion_keywords", ui->editor_checkbox_ac_keywords->isChecked ());
  settings->setValue ("editor/codeCompletion_document", ui->editor_checkbox_ac_document->isChecked ());
  settings->setValue ("editor/codeCompletion_case", ui->editor_checkbox_ac_case->isChecked ());
  settings->setValue ("editor/codeCompletion_replace", ui->editor_checkbox_ac_replace->isChecked ());
  settings->setValue ("editor/show_white_space", ui->editor_ws_checkbox->isChecked ());
  settings->setValue ("editor/show_white_space_indent", ui->editor_ws_indent_checkbox->isChecked ());
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
  settings->setValue ("terminal/cursorUseForegroundColor", ui->terminal_cursorUseForegroundColor->isChecked ());

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
#if defined (HAVE_LEXER_OCTAVE)
  lexer = new QsciLexerOctave ();
  write_lexer_settings (lexer,settings);
  delete lexer;
#elif defined (HAVE_LEXER_MATLAB)
  lexer = new QsciLexerMatlab ();
  write_lexer_settings (lexer,settings);
  delete lexer;
#endif
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

  write_workspace_colors (settings);

  write_terminal_colors (settings);

  settings->setValue("settings/last_tab",ui->tabWidget->currentIndex ());
}

#ifdef HAVE_QSCINTILLA
void
settings_dialog::write_lexer_settings (QsciLexer *lexer, QSettings *settings)
{
  QWidget *tab = ui->tabs_editor_styles->
            findChild <QWidget *>(QString (lexer->language ())+"_styles");
  int styles[MaxLexerStyles];  // array for saving valid styles (enum is not continuous)
  int max_style = get_valid_lexer_styles (lexer, styles);
  QFontComboBox *select_font;
  QSpinBox *font_size;
  QCheckBox *attrib_font[3];
  color_picker *color;
  color_picker *bg_color;
  int default_size = 10;
  QFont default_font = QFont ("Courier New",10,-1,0);
  QColor default_color = QColor ();
  QColor dummy_color = QColor (255,0,255);

  for (int i = 0; i < max_style; i++)  // get dialog elements and their contents
    {
      QString actual_name = lexer->description (styles[i]);
      select_font    = tab->findChild <QFontComboBox *>(actual_name+"_font");
      font_size      = tab->findChild <QSpinBox *>(actual_name+"_size");
      attrib_font[0] = tab->findChild <QCheckBox *>(actual_name+"_bold");
      attrib_font[1] = tab->findChild <QCheckBox *>(actual_name+"_italic");
      attrib_font[2] = tab->findChild <QCheckBox *>(actual_name+"_underline");
      color          = tab->findChild <color_picker *>(actual_name+"_color");
      bg_color       = tab->findChild <color_picker *>(actual_name+"_bg_color");
      QFont new_font = default_font;
      if (select_font)
        {
          new_font = select_font->currentFont ();
          if (styles[i] == 0)
            default_font = new_font;
          else
            if (select_font->currentText () == lexer->description (0))
              new_font = default_font;
        }
      if (font_size)
        {
          if (styles[i] == 0)
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
      lexer->setFont (new_font,styles[i]);
      if (styles[i] == 0)
        lexer->setDefaultFont (new_font);
      if (color)
        lexer->setColor (color->color (),styles[i]);
      if (bg_color)
        {
          if (styles[i] == 0)
            {
              default_color = bg_color->color ();
              lexer->setPaper (default_color,styles[i]);
              lexer->setDefaultPaper (default_color);
            }
          else
            {
              if (bg_color->color () == dummy_color)
                lexer->setPaper (default_color,styles[i]);
              else
                lexer->setPaper (bg_color->color (),styles[i]);
            }
        }
    }

  lexer->writeSettings (*settings);

  settings->setValue (
    "settings/last_editor_styles_tab",ui->tabs_editor_styles->currentIndex ());
}
#endif

void
settings_dialog::write_workspace_colors (QSettings *settings)
{

  QString class_chars = resource_manager::storage_class_chars ();
  color_picker *color;

  for (int i = 0; i < class_chars.length (); i++)
    {
      color = ui->workspace_colors_box->findChild <color_picker *>(
                            "color_"+class_chars.mid (i,1));
      if (color)
        settings->setValue ("workspaceview/color_"+class_chars.mid (i,1),
                            color->color ());
    }
  settings->sync ();
}

void
settings_dialog::write_terminal_colors (QSettings *settings)
{
  QString class_chars = resource_manager::terminal_color_chars ();
  color_picker *color;

  for (int i = 0; i < class_chars.length (); i++)
    {
      color = ui->terminal_colors_box->findChild <color_picker *>(
                            "terminal_color_"+class_chars.mid (i,1));
      if (color)
        settings->setValue ("terminal/color_"+class_chars.mid (i,1),
                            color->color ());
    }
  settings->sync ();
}
