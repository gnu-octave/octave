////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2018-2023 The Octave Project Developers
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

#include <QCompleter>
#include <QMenu>
#include <QShortcut>
#include <QVBoxLayout>
#include <QWidget>

#include "documentation.h"
#include "documentation-bookmarks.h"

#include "gui-settings.h"
#include "gui-preferences-global.h"
#include "gui-preferences-dc.h"
#include "gui-preferences-sc.h"
#include "octave-qtutils.h"
#include "shortcut-manager.h"

#include "defaults.h"
#include "file-ops.h"
#include "oct-env.h"

OCTAVE_BEGIN_NAMESPACE(octave)

documentation_bookmarks::documentation_bookmarks (
                                                  documentation *doc, documentation_browser *browser,
                                                  base_qobject& oct_qobj, QWidget *p)
: QWidget (p),
  m_doc (doc), m_browser (browser), m_octave_qobj (oct_qobj),
  m_ctx_menu_item (nullptr)
{
  setObjectName ("documentation_tab_bookmarks");

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  // Setup the tree view with the bookmarks
  m_tree = new QTreeWidget (p);

  m_tree->setContextMenuPolicy (Qt::CustomContextMenu);
  m_tree->setSelectionMode (QAbstractItemView::ExtendedSelection);
  m_tree->setSortingEnabled (false);
  m_tree->setDragEnabled(true);
  m_tree->viewport()->setAcceptDrops(true);
  m_tree->setDropIndicatorShown(true);
  m_tree->setDragDropMode(QAbstractItemView::InternalMove);
  m_tree->setColumnCount (1);
  m_tree->setHeaderHidden (true);
  m_tree->setEditTriggers (QAbstractItemView::EditKeyPressed
                           | QAbstractItemView::SelectedClicked);

  connect (m_tree, &QTreeWidget::customContextMenuRequested,
           this, &documentation_bookmarks::ctx_menu);
  connect (m_tree, &QTreeWidget::itemDoubleClicked,
           this, &documentation_bookmarks::handle_double_click);

  // Define the icons for the tree view
  icon_folder.addPixmap (style ()->standardPixmap(QStyle::SP_DirClosedIcon),
                         QIcon::Normal, QIcon::Off);
  icon_folder.addPixmap (style ()->standardPixmap(QStyle::SP_DirOpenIcon),
                         QIcon::Normal, QIcon::On);
  icon_bookmark.addPixmap (style ()->standardPixmap(QStyle::SP_FileIcon));

  // Setup and read the bookmarkfile
  QFileInfo f (settings->fileName ());
  QString f_path = f.absolutePath ();
  f.setFile (QDir (f_path), dc_bookmark_file);
  m_xbel_file.setFileName (f.absoluteFilePath ());

  if (m_xbel_file.exists ())
    {
      QString err = read_bookmarks ();
      if ( !err.isEmpty ())
        {
          err.append (tr ("\nNo documentation bookmarks loaded!"));
          QMessageBox::warning (this,
                                tr ("Octave: Loading Documentation Bookmarks"),
                                err);
          m_xbel_file.close ();
        }
    }

  // Setup the filter widget
  m_filter_widget = new QWidget (p);
  m_filter = new QComboBox (m_filter_widget);

  m_filter->setToolTip (tr ("Enter text to search the bookmarks"));
  m_filter->setEditable (true);
  m_filter->setInsertPolicy (QComboBox::NoInsert);
  m_filter->setMaxCount (10);
  m_filter->setMaxVisibleItems (10);
  m_filter->setSizeAdjustPolicy (QComboBox::AdjustToMinimumContentsLengthWithIcon);
  QSizePolicy size_pol (QSizePolicy::Expanding, QSizePolicy::Preferred);
  m_filter->setSizePolicy (size_pol);
  m_filter->completer ()->setCaseSensitivity (Qt::CaseSensitive);

  m_filter->addItems (settings->value (dc_bookmark_filter_mru).toStringList ());

  connect (m_filter, &QComboBox::editTextChanged,
           this, &documentation_bookmarks::filter_bookmarks);
  connect (m_filter->lineEdit (), &QLineEdit::editingFinished,
           this, &documentation_bookmarks::update_filter_history);

  m_filter_checkbox = new QCheckBox (m_filter_widget);
  bool filter_state = settings->value (dc_bookmark_filter_active).toBool ();
  m_filter_checkbox->setChecked (filter_state);
  filter_activate (filter_state);

  connect (m_filter_checkbox, &QCheckBox::toggled,
           this, &documentation_bookmarks::filter_activate);

  QLabel *filter_label = new QLabel (tr ("Filter"), m_filter_widget);
  QHBoxLayout *h_box_bm = new QHBoxLayout (m_filter_widget);
  h_box_bm->addWidget (filter_label);
  h_box_bm->addWidget (m_filter_checkbox);
  h_box_bm->addWidget (m_filter);
  h_box_bm->setMargin (2);
  m_filter_widget->setLayout (h_box_bm);

  m_filter_shown = settings->value (dc_bookmark_filter_shown).toBool ();
  m_filter_widget->setVisible (m_filter_shown);

  // Resulting Layout of this widget
  QVBoxLayout *v_box_bm = new QVBoxLayout (this);
  v_box_bm->addWidget (m_filter_widget);
  v_box_bm->addWidget (m_tree);
  setLayout (v_box_bm);
}

// Slot for adding the current page as a bookmark
void documentation_bookmarks::add_bookmark (void)
{
  QUrl url = m_browser->historyUrl (0);

  // Check if bookmark already exists and select if yes
  QTreeWidgetItemIterator it (m_tree);
  while (*it)
    {
      QUrl url_i = (*it)->data (0, url_role).toUrl ();
      if (url == url_i)
        {
          m_tree->setCurrentItem (*it);
          (*it)->setExpanded (true);
          return;
        }
      it++;
    }

  // Add the anchor name to the title of the page and add the bookmark
  // as top-level-item
  QString title = m_doc->title_and_anchor (m_browser->historyTitle (0), url);
  add_bookmark (title, url.toString ());
}

// Function for actually adding a bookmark to the tree
void documentation_bookmarks::add_bookmark (const QString& title,
                                            const QString& url,
                                            QTreeWidgetItem* item)
{
  // Create new bookmark
  QTreeWidgetItem *new_item = new QTreeWidgetItem (QStringList (title));
  new_item->setData (0, tag_role, QVariant (bookmark_tag));
  new_item->setData (0, url_role, QVariant (url));
  new_item->setFlags ((new_item->flags () & (~Qt::ItemIsDropEnabled))
                      | Qt::ItemIsEditable
                      | Qt::ItemIsDragEnabled);
  new_item->setIcon (0, icon_bookmark);

  // Insert as top level or child item
  // TODO: Open dialog allowing to select a target folder if this
  //       bookmark is added manually and not by reading a bookmark file
  if (item)
    item->addChild (new_item);
  else
    m_tree->addTopLevelItem (new_item);
}

// Slot for adding a folder from the context menu
void documentation_bookmarks::add_folder (bool)
{
  QTreeWidgetItem *parent_item = nullptr;

  if (m_ctx_menu_item)
    {
      if (m_ctx_menu_item->data (0, tag_role).toInt () == folder_tag)
        parent_item = m_ctx_menu_item;
      else
        {
          QTreeWidgetItem *p = m_ctx_menu_item->parent ();
          if (p)
            parent_item = p;
        }
    }

  QTreeWidgetItem *new_folder = add_folder (tr ("New Folder"), parent_item);

  m_tree->setCurrentItem (new_folder);
  m_tree->editItem (new_folder);
}

// Function for actually adding a folder to the tree
QTreeWidgetItem* documentation_bookmarks::add_folder (const QString& folder,
                                                      QTreeWidgetItem *item, bool expanded)
{
  QTreeWidgetItem *new_folder = new QTreeWidgetItem (QStringList (folder));
  new_folder->setData (0, tag_role, QVariant (folder_tag));
  new_folder->setFlags (new_folder->flags() | Qt::ItemIsEditable
                        | Qt::ItemIsDragEnabled
                        | Qt::ItemIsDropEnabled);
  new_folder->setChildIndicatorPolicy (QTreeWidgetItem::DontShowIndicatorWhenChildless);
  new_folder->setIcon (0, icon_folder);
  new_folder->setExpanded (expanded);

  // Insert as top level or child item
  if (item)
    item->addChild (new_folder);
  else
    m_tree->addTopLevelItem (new_folder);

  return new_folder;
}

void documentation_bookmarks::filter_bookmarks (const QString& pattern)
{
  QTreeWidgetItemIterator it (m_tree);

  while (*it)
    {
      if ((*it)->text (0).contains (pattern, Qt::CaseInsensitive))
        {
          (*it)->setHidden (false);
          (*it)->setExpanded (true);
          QTreeWidgetItem *p = (*it)->parent ();
          while (p)
            {
              p->setHidden (false);
              p->setExpanded (true);
              p = p->parent ();
            }
        }
      else
        (*it)->setHidden (true);

      it++;
    }
}

void documentation_bookmarks::filter_activate (bool state)
{
  m_filter->setEnabled (state);

  QString pattern;
  if (state)
    pattern = m_filter->currentText ();

  filter_bookmarks (pattern);
}

void documentation_bookmarks::update_filter_history (void)
{
  QString text = m_filter->currentText ();   // get current text
  int index = m_filter->findText (text);     // and its actual index

  if (index > -1)
    m_filter->removeItem (index);    // remove if already existing

  m_filter->insertItem (0, text);    // (re)insert at beginning
  m_filter->setCurrentIndex (0);
}

void documentation_bookmarks::handle_double_click (QTreeWidgetItem *item, int)
{
  int tag = item->data (0, tag_role).toInt ();

  if (tag == folder_tag)
    {
      item->setExpanded (! item->isExpanded ());
      return;
    }

  if (tag == bookmark_tag)
    {
      QUrl url = item->data (0, url_role).toUrl ();
      if (! url.isEmpty ())
        m_browser->setSource (url);
      return;
    }
}

void documentation_bookmarks::ctx_menu (const QPoint& xpos)
{
  QMenu menu (this);

  m_ctx_menu_item = m_tree->itemAt (xpos);

  if (m_ctx_menu_item)
    {
      resource_manager& rmgr = m_octave_qobj.get_resource_manager ();

      menu.addAction (tr ("&Open"), this, &documentation_bookmarks::open);
      menu.addAction (tr ("&Rename"), this, &documentation_bookmarks::edit);
      menu.addAction (rmgr.icon ("window-close"), tr ("Remo&ve"),
                      this, &documentation_bookmarks::remove);
      menu.addSeparator ();
    }

  menu.addAction (tr ("&Add Folder"), this,
                  QOverload<bool>::of (&documentation_bookmarks::add_folder));

  menu.addSeparator ();

  if (m_filter_shown)
    menu.addAction (tr ("Hide &Filter"),
                    this, &documentation_bookmarks::show_filter);
  else
    menu.addAction (tr ("Show &Filter"),
                    this, &documentation_bookmarks::show_filter);

  menu.exec (m_tree->mapToGlobal (xpos));
}

void documentation_bookmarks::open (bool)
{
  QList<QTreeWidgetItem *> items = m_tree->selectedItems ();

  if (items.size () > 0)
    handle_double_click (items.at (0));
}

void documentation_bookmarks::edit (bool)
{
  QList<QTreeWidgetItem *> items = m_tree->selectedItems ();

  if (items.size () > 0)
    m_tree->editItem (items.at (0));
}

void documentation_bookmarks::remove (bool)
{
  QList<QTreeWidgetItem *> items = m_tree->selectedItems ();

  for (auto it = items.begin () ; it != items.end (); it++)
    {
      if (*it)
        m_tree->takeTopLevelItem (
                                  m_tree->indexOfTopLevelItem (*it));
    }
}

void documentation_bookmarks::show_filter (bool)
{
  m_filter_shown = ! m_filter_shown;
  m_filter_widget->setVisible (m_filter_shown);
}

void documentation_bookmarks::save_settings (gui_settings *settings)
{
  // Write the bookmarks to the xbel-file
  write_bookmarks ();

  // Store settings
  settings->setValue (dc_bookmark_filter_active.key, m_filter_checkbox->isChecked ());
  settings->setValue (dc_bookmark_filter_shown.key, m_filter_shown);

  QStringList mru;
  for (int i = 0; i < m_filter->count (); i++)
    mru.append (m_filter->itemText (i));
  settings->setValue (dc_bookmark_filter_mru.key, mru);

  settings->sync ();
}

void documentation_bookmarks::write_bookmarks (void)
{
  if (! m_xbel_file.open (QFile::WriteOnly | QFile::Text))
    {
      QMessageBox::warning (this, tr("Octave: Saving Documentation Bookmarks"),
                            tr("Unable to write file %1:\n%2.\n\n"
                               "Documentation bookmarks are not saved!\n")
                            .arg (m_xbel_file.fileName ())
                            .arg (m_xbel_file.errorString()));
      return;
    }

  QXmlStreamWriter xml_writer (&m_xbel_file);
  xml_writer.setAutoFormatting (true);

  xml_writer.writeStartDocument ();
  xml_writer.writeDTD (dc_xbel_doctype);
  xml_writer.writeStartElement (dc_xbel_name_format);
  xml_writer.writeAttribute (dc_xbel_attr_version, dc_xbel_value_version);

  for (int i = 0; i < m_tree->topLevelItemCount(); i++)
    write_tree_item (&xml_writer, m_tree->topLevelItem (i));

  xml_writer.writeEndDocument();

  m_xbel_file.flush ();
  m_xbel_file.close ();
}

void documentation_bookmarks::write_tree_item (QXmlStreamWriter* xml_writer,
                                               const QTreeWidgetItem *item)
{
  switch (item->data (0, tag_role).toInt ())
    {
    case folder_tag:
      xml_writer->writeStartElement (dc_xbel_name_folder);
      xml_writer->writeAttribute (dc_xbel_attr_folded,
                                  item->isExpanded () ? dc_xbel_value_no : dc_xbel_value_yes);
      xml_writer->writeTextElement (dc_xbel_name_title, item->text(0));
      for (int i = 0; i < item->childCount (); i++)
        write_tree_item (xml_writer, item->child (i));
      xml_writer->writeEndElement ();
      break;

    case bookmark_tag:
      xml_writer->writeStartElement (dc_xbel_name_bookmark);
      xml_writer->writeAttribute (dc_xbel_attr_href, item->data (0, url_role).toString ());
      xml_writer->writeTextElement (dc_xbel_name_title, item->text (0));
      xml_writer->writeEndElement ();
      break;
    }
}

QString documentation_bookmarks::read_bookmarks (void)
{
  QString error_message;

  // Check the file
  if (! m_xbel_file.open (QFile::ReadOnly | QFile::Text))
    {
      error_message = tr ("Unable to read file %1:\n%2.")
        .arg (m_xbel_file.fileName ())
        .arg (m_xbel_file.errorString());
      return error_message;
    }

  QXmlStreamReader xml_reader (&m_xbel_file);

  if (! xml_reader.readNextStartElement ())
    {
      error_message = tr ("No start element found in %1.\n"
                          "Invalid bookmark file?")
        .arg (m_xbel_file.fileName ());
      return error_message;
    }

  if (xml_reader.name() != dc_xbel_name_format
      || xml_reader.attributes ().value (dc_xbel_attr_version) != dc_xbel_value_version)
    {
      error_message = tr ("The file\n"
                          "%1\n"
                          "is not a valid XBEL file version 1.0.")
        .arg (m_xbel_file.fileName ());
      return error_message;
    }

  // Read the elements from the file
  while (xml_reader.readNextStartElement ())
    {
      if (xml_reader.name () == dc_xbel_name_folder)
        read_next_item (&xml_reader, folder_tag);
      else if (xml_reader.name () == dc_xbel_name_bookmark)
        read_next_item (&xml_reader, bookmark_tag);
      else
        xml_reader.skipCurrentElement ();
    }

  m_xbel_file.close ();

  return error_message;
}

void documentation_bookmarks::read_next_item (QXmlStreamReader *xml_reader,
                                              item_tag tag, QTreeWidgetItem *item)
{
  QString title (tr ("Unknown title"));
  if (tag == folder_tag)
    {
      // Next item is a folder, which might also have children
      bool expanded = (xml_reader->attributes().value (dc_xbel_attr_folded) == dc_xbel_value_no);

      QTreeWidgetItem *new_folder = add_folder (title, item, expanded);

      // Check elements of this folder for title and for recursively
      // adding sub-items
      while (xml_reader->readNextStartElement ())
        {
          if (xml_reader->name () == dc_xbel_name_title)
            {
              title = xml_reader->readElementText();
              new_folder->setText (0, title);
            }
          else if (xml_reader->name () == dc_xbel_name_folder)
            read_next_item (xml_reader, folder_tag, new_folder);
          else if (xml_reader->name () == dc_xbel_name_bookmark)
            read_next_item (xml_reader, bookmark_tag, new_folder);
          else
            xml_reader->skipCurrentElement ();
        }
    }
  else if (tag == bookmark_tag)
    {
      // Next item is a bookmark, without children
      QString url = xml_reader->attributes().value (dc_xbel_attr_href).toString ();
      while (xml_reader->readNextStartElement ())
        {
          if (xml_reader->name() == dc_xbel_name_title)
            title = xml_reader->readElementText();
          else
            xml_reader->skipCurrentElement ();
        }
      add_bookmark (title, url, item);
    }
}

OCTAVE_END_NAMESPACE(octave)
