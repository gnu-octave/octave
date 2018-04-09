/*

Copyright (C) 2013-2018 John W. Eaton
Copyright (C) 2015 Michael Barnes
Copyright (C) 2013 Rüdiger Sonderfeld

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <limits>

#include <QApplication>
#include <QClipboard>
#include <QFileDialog>
#include <QHeaderView>
#include <QLabel>
#include <QMdiArea>
#include <QMenu>
#include <QPalette>
#include <QScreen>
#include <QScrollBar>
#include <QSignalMapper>
#include <QStackedWidget>
#include <QTableView>
#include <QTextEdit>
#include <QTabWidget>
#include <QToolBar>
#include <QToolButton>
#include <QVBoxLayout>

#include "resource-manager.h"
#include "shortcut-manager.h"
#include "variable-editor.h"
#include "variable-editor-model.h"

// Code reuse functions

static QString
idx_to_expr (int32_t from, int32_t to)
{
  return (from == to
          ? QString ("%1").arg (from)
          : QString ("%1:%2").arg (from).arg (to));
}

static QSignalMapper *
make_plot_mapper (QMenu *menu)
{
  QList<QString> list;
  list << "plot" << "bar" << "stem" << "stairs" << "area" << "pie" << "hist";

  QSignalMapper *plot_mapper = new QSignalMapper (menu);

  for (int i = 0; i < list.size(); ++i)
    {
      plot_mapper->setMapping
        (menu->addAction (list.at (i), plot_mapper, SLOT (map ())),
         "figure (); " + list.at (i) + " (%1); title (\"%1\");");
    }

  return plot_mapper;
}

namespace octave
{
  // Variable dock widget

  variable_dock_widget::variable_dock_widget (QWidget *p)
    : label_dock_widget (p)
  {
    setFocusPolicy (Qt::StrongFocus);
    setAttribute (Qt::WA_DeleteOnClose);

    connect (m_dock_action, SIGNAL (triggered (bool)),
             this, SLOT (change_floating (bool)));
    connect (m_close_action, SIGNAL (triggered (bool)),
             this, SLOT (change_existence (bool)));
    connect (this, SIGNAL (topLevelChanged(bool)),
             this, SLOT (toplevel_change (bool)));
    connect (p, SIGNAL (visibilityChanged (bool)),
             this, SLOT (setVisible (bool)));

#if defined (HAVE_QGUIAPPLICATION)
#define DOCKED_FULLSCREEN_BUTTON_TOOLTIP "Fullscreen undock"
#define UNDOCKED_FULLSCREEN_BUTTON_TOOLTIP "Fullscreen"
    // Add a fullscreen button

    m_fullscreen_action = nullptr;
    m_full_screen = false;
    m_prev_floating = false;
    m_prev_geom = QRect (0, 0, 0, 0);

    QHBoxLayout *h_layout = findChild<QHBoxLayout *> ();
    if (h_layout != nullptr && titleBarWidget () != nullptr)
      {
        m_fullscreen_action = new QAction
          (resource_manager::icon ("view-fullscreen", false), "", this);
        m_fullscreen_action->setToolTip (tr (DOCKED_FULLSCREEN_BUTTON_TOOLTIP));
        QToolButton *fullscreen_button = new QToolButton (titleBarWidget ());
        fullscreen_button->setDefaultAction (m_fullscreen_action);
        fullscreen_button->setFocusPolicy (Qt::NoFocus);
        fullscreen_button->setIconSize (QSize (m_icon_size,m_icon_size));
        QString css_button = QString ("QToolButton {background: transparent; border: 0px;}");
        fullscreen_button->setStyleSheet (css_button);

        connect (m_fullscreen_action, SIGNAL (triggered ()),
                 this, SLOT (change_fullscreen ()));

        int index = -1;
        QToolButton *first = titleBarWidget ()->findChild<QToolButton *> ();
        if (first != nullptr)
          index = h_layout->indexOf (first);
        h_layout->insertWidget (index, fullscreen_button);
      }
#endif

    // Custom title bars cause loss of decorations, add a frame
    m_frame = new QFrame (this);
    m_frame->setFrameStyle (QFrame::Box | QFrame::Sunken);
    m_frame->setAttribute (Qt::WA_TransparentForMouseEvents);
  }

  // slot for (un)dock action
  void
  variable_dock_widget::change_floating (bool)
  {
#if defined (HAVE_QGUIAPPLICATION)
    if (isFloating ())
      {
        if (m_full_screen)
          {
            setGeometry (m_prev_geom);
            m_fullscreen_action->setIcon (resource_manager::icon ("view-fullscreen", false));
            m_full_screen = false;
          }
        m_fullscreen_action->setToolTip (tr (DOCKED_FULLSCREEN_BUTTON_TOOLTIP));
      }
    else
      m_fullscreen_action->setToolTip (tr (UNDOCKED_FULLSCREEN_BUTTON_TOOLTIP));
#endif

    setFloating (! isFloating ());
  }

  // slot for hiding the widget
  void
  variable_dock_widget::change_existence (bool)
  {
    close ();
  }

  void
  variable_dock_widget::toplevel_change (bool toplevel)
  {
    if (toplevel)
      {
        m_dock_action->setIcon (QIcon (":/actions/icons/widget-dock.png"));
        m_dock_action->setToolTip (tr ("Dock widget"));

        activateWindow();
        setFocus (Qt::OtherFocusReason);
      }
    else
      {
        m_dock_action->setIcon (QIcon (":/actions/icons/widget-undock.png"));
        m_dock_action->setToolTip (tr ("Undock widget"));

        setFocus (Qt::OtherFocusReason);
      }
  }

  void
  variable_dock_widget::change_fullscreen (void)
  {
#if defined (HAVE_QGUIAPPLICATION)
    if (! m_full_screen)
      {
        m_prev_floating = isFloating ();
        m_prev_geom = geometry ();

        m_fullscreen_action->setIcon (resource_manager::icon ("view-restore", false));
        if (m_prev_floating)
          m_fullscreen_action->setToolTip (tr ("Restore geometry"));
        else
          {
            m_fullscreen_action->setToolTip (tr ("Redock"));
            setFloating (true);
          }

        // showFullscreen() and setWindowState() only work for QWindow objects.
        QScreen *pscreen = QGuiApplication::primaryScreen ();
        QRect rect (0, 0, 0, 0);
        rect = pscreen->availableGeometry ();
        setGeometry (rect);

        m_full_screen = true;
      }
    else
      {
        m_fullscreen_action->setIcon (resource_manager::icon ("view-fullscreen", false));
        setGeometry (m_prev_geom);
        if (m_prev_floating)
          m_fullscreen_action->setToolTip (tr (UNDOCKED_FULLSCREEN_BUTTON_TOOLTIP));
        else
          {
            setFloating (false);
            m_fullscreen_action->setToolTip (tr (DOCKED_FULLSCREEN_BUTTON_TOOLTIP));
          }

        m_full_screen = false;
      }
#undef DOCKED_FULLSCREEN_BUTTON_TOOLTIP
#undef UNDOCKED_FULLSCREEN_BUTTON_TOOLTIP
#endif
  }

  void
  variable_dock_widget::closeEvent (QCloseEvent *e)
  {
    QDockWidget::closeEvent (e);
  }

  void
  variable_dock_widget::handle_focus_change (QWidget *old, QWidget *now)
  {
    octave_unused_parameter (now);

    // The is a proxied test
    if (hasFocus ())
      {
        QLabel *label = titleBarWidget ()->findChild<QLabel *> ();
        if (label != nullptr)
          {
            label->setBackgroundRole (QPalette::Highlight);
            label->setAutoFillBackground (true);
          }

        emit variable_focused_signal (objectName ());
      }
    else if (old == focusWidget())
      {
        QLabel *label = titleBarWidget ()->findChild<QLabel *> ();
        if (label != NULL)
          label->setBackgroundRole (QPalette::NoRole);
      }
  }

  void variable_dock_widget::resizeEvent (QResizeEvent *)
  {
    if (m_frame)
      m_frame->resize (size ());
  }


  // Variable editor stack

  variable_editor_stack::variable_editor_stack (QWidget *p)
    : QStackedWidget (p), m_edit_view (new variable_editor_view (this))
  {
    setFocusPolicy (Qt::StrongFocus);

    m_disp_view = make_disp_view (this);

    addWidget (m_edit_view);
    addWidget (m_disp_view);
  }

  QTextEdit *
  variable_editor_stack::make_disp_view (QWidget *parent)
  {
    QTextEdit *viewer = new QTextEdit (parent);

    viewer->setLineWrapMode (QTextEdit::NoWrap);
    viewer->setReadOnly (true);

    return viewer;
  }

  void
  variable_editor_stack::set_editable (bool editable)
  {
    // The QTableView is for editable data models
    // and the QTextEdit is for non-editable models.

    if (editable)
      {
        if (m_edit_view != nullptr)
          {
            setCurrentWidget (m_edit_view);
            setFocusProxy (m_edit_view);
            m_edit_view->setFocusPolicy (Qt::StrongFocus);
          }

        if (m_disp_view != nullptr)
          m_disp_view->setFocusPolicy (Qt::NoFocus);
      }
    else
      {
        if (m_disp_view != nullptr)
          {
            setCurrentWidget (m_disp_view);
            setFocusProxy (m_disp_view);

            QAbstractTableModel *model = findChild<QAbstractTableModel *> ();
            if (model != nullptr)
              m_disp_view->setPlainText (model->data (QModelIndex ()).toString ());
            else
              m_disp_view->setPlainText ("");
          }

        if (m_edit_view != nullptr)
          m_edit_view->setFocusPolicy (Qt::NoFocus);
      }
  }

  void
  variable_editor_stack::levelUp (void)
  {
    if (! hasFocus ())
      return;

    QString name = objectName ();

    // FIXME: Is there a better way?

    if (name.endsWith (')') || name.endsWith ('}'))
      {
        name.remove (QRegExp ("(\\(|\\{)[^({]*(\\)|\\})$"));
        emit edit_variable_signal (name, octave_value ());
      }
  }

  void
  variable_editor_stack::save (void)
  {
    if (! hasFocus ())
      return;

    QString name = objectName ();
    QString file
      = QFileDialog::getSaveFileName (this,
                                      tr ("Save Variable %1 As").arg (name),
    // FIXME: Should determine extension from save_default_options
                                      QString ("./%1.txt").arg (name),
                                      0, 0,
                                      QFileDialog::DontUseNativeDialog);

    // FIXME: Type? binary, float-binary, ascii, text, hdf5, matlab format?
    // FIXME: Call octave_value::save_* directly?

    if (! file.isEmpty ())
      emit command_signal (QString ("save (\"%1\", \"%2\");")
                           .arg (file)
                           .arg (name));
  }


  // Custom editable variable table view

  variable_editor_view::variable_editor_view (QWidget *p)
    : QTableView (p), m_var_model (nullptr)
  {
    setWordWrap (false);
    setContextMenuPolicy (Qt::CustomContextMenu);
    setSelectionMode (QAbstractItemView::ContiguousSelection);

    horizontalHeader ()->setContextMenuPolicy (Qt::CustomContextMenu);
    verticalHeader ()->setContextMenuPolicy (Qt::CustomContextMenu);

    setHorizontalScrollMode (QAbstractItemView::ScrollPerPixel);
    setVerticalScrollMode (QAbstractItemView::ScrollPerPixel);

#if defined (HAVE_QHEADERVIEW_SETSECTIONRESIZEMODE)
    verticalHeader ()->setSectionResizeMode (QHeaderView::Interactive);
#else
    verticalHeader ()->setResizeMode (QHeaderView::Interactive);
#endif
  }

  void
  variable_editor_view::setModel (QAbstractItemModel *model)
  {
    QTableView::setModel (model);

#if defined (HAVE_QHEADERVIEW_SETSECTIONRESIZEMODE)
    horizontalHeader ()->setSectionResizeMode (QHeaderView::Interactive);
#else
    horizontalHeader ()->setResizeMode (QHeaderView::Interactive);
#endif

    m_var_model = parent ()->findChild<variable_editor_model *> ();

    if (m_var_model != nullptr && m_var_model->column_width () > 0)
      {
        // col_width is in characters.  The font should be a fixed-width
        // font, so any character will do.  If not, you lose!

        QFontMetrics fm (font ());
        int w = m_var_model->column_width () * fm.width ('0');
        horizontalHeader ()->setDefaultSectionSize (w);
      }
  }

  QList<int>
  variable_editor_view::range_selected (void)
  {
    QItemSelectionModel *sel = selectionModel ();

    // Return early if nothing selected.
    if (! sel->hasSelection ())
      return QList<int> ();

    QList<QModelIndex> indices = sel->selectedIndexes ();

    // FIXME: Shouldn't this be keyed to octave_idx_type?

    int32_t from_row = std::numeric_limits<int32_t>::max ();
    int32_t to_row = 0;
    int32_t from_col = std::numeric_limits<int32_t>::max ();
    int32_t to_col = 0;

    for (const auto& idx : indices)
      {
        from_row = std::min (from_row, idx.row ());
        to_row = std::max (to_row, idx.row ());
        from_col = std::min (from_col, idx.column ());
        to_col = std::max (to_col, idx.column ());
      }

    QVector<int> vect;
    vect << from_row + 1 << to_row + 1 << from_col + 1 << to_col + 1;
    QList<int> range = QList<int>::fromVector(vect);

    return range;
  }

  QString
  variable_editor_view::selected_to_octave (void)
  {
    QList<int> range = range_selected ();
    if (range.isEmpty ())
      return objectName ();

    QString rows = idx_to_expr (range.at (0), range.at (1));
    QString cols = idx_to_expr (range.at (2), range.at (3));

    // FIXME: Does cell need separate handling?  Maybe use '{.,.}'?

    return QString ("%1(%2, %3)").arg (objectName ()).arg (rows).arg (cols);
  }

  void
  variable_editor_view::selected_command_requested (const QString& cmd)
  {
    if (! hasFocus ())
      return;

    QString selarg = selected_to_octave ();
    if (! selarg.isEmpty ())
      emit command_signal (cmd.arg (selarg));
  }

  void
  variable_editor_view::add_edit_actions (QMenu *menu,
                                          const QString& qualifier_string)
  {
    menu->addAction (resource_manager::icon ("edit-cut"),
                     tr ("Cut") + qualifier_string,
                     this, SLOT (cutClipboard ()));

    menu->addAction (resource_manager::icon ("edit-copy"),
                     tr ("Copy") + qualifier_string,
                     this, SLOT (copyClipboard ()));

    menu->addAction (resource_manager::icon ("edit-paste"),
                     tr ("Paste"),
                     this, SLOT (pasteClipboard ()));

    // FIXME: Different icon for Paste Table?

    menu->addAction (resource_manager::icon ("edit-paste"),
                     tr ("Paste Table"),
                     this, SLOT (pasteTableClipboard ()));

    menu->addSeparator ();

    menu->addAction (resource_manager::icon ("edit-delete"),
                     tr ("Clear") + qualifier_string,
                     this, SLOT (clearContent ()));

    menu->addAction (resource_manager::icon ("edit-delete"),
                     tr ("Delete") + qualifier_string,
                     this, SLOT (delete_selected ()));

    menu->addAction (resource_manager::icon ("document-new"),
                     tr ("Variable from Selection"),
                     this, SLOT (createVariable ()));
  }

  void
  variable_editor_view::createContextMenu (const QPoint& qpos)
  {
    QModelIndex index = indexAt (qpos);

    if (index.isValid ())
      {
        QMenu *menu = new QMenu (this);

        add_edit_actions (menu, tr (""));

        // FIXME: addAction for sort?
        // FIXME: Add icon for transpose.

        menu->addAction (tr ("Transpose"), this, SLOT (transposeContent ()));

        QItemSelectionModel *sel = selectionModel ();

        QList<QModelIndex> indices = sel->selectedIndexes ();

        if (! indices.isEmpty ())
          {
            menu->addSeparator ();

            QSignalMapper *plot_mapper = make_plot_mapper (menu);

            connect (plot_mapper, SIGNAL (mapped (const QString&)),
                     this, SLOT (selected_command_requested (const QString&)));
          }

        menu->exec (mapToGlobal (qpos));
      }
  }

  void
  variable_editor_view::createColumnMenu (const QPoint& pt)
  {
    int index = horizontalHeader ()->logicalIndexAt (pt);

    if (index < 0 || index > model ()->columnCount ())
      return;

    QList<int> coords = range_selected ();

    bool nothingSelected = coords.isEmpty ();

    bool whole_columns_selected
      =  (nothingSelected
          ? false
          : (coords[0] == 1 && coords[1] == model ()->rowCount ()));

    bool current_column_selected
      = nothingSelected ? false : (coords[2] <= index+1 && coords[3] > index);

    int column_selection_count
      = nothingSelected ? 0 : (coords[3] - coords[2] + 1);

    if (! whole_columns_selected || ! current_column_selected)
      {
        selectColumn (index);
        column_selection_count = 1;
        current_column_selected = true;
        whole_columns_selected = true;
      }

    QString column_string
      = tr (column_selection_count > 1 ? " columns" : " column");

    QMenu *menu = new QMenu (this);

    add_edit_actions (menu, column_string);

    menu->addSeparator ();

    QSignalMapper *plot_mapper = make_plot_mapper (menu);

    connect (plot_mapper, SIGNAL (mapped (const QString&)),
             this, SLOT (selected_command_requested (const QString&)));

    QPoint menupos = pt;
    menupos.setY (horizontalHeader ()->height ());

    menu->exec (mapToGlobal (menupos));
  }

  void
  variable_editor_view::createRowMenu (const QPoint& pt)
  {
    int index = verticalHeader ()->logicalIndexAt (pt);

    if (index < 0 || index > model ()->columnCount ())
      return;

    QList<int> coords = range_selected ();

    bool nothingSelected = coords.isEmpty ();

    bool whole_rows_selected
      = (nothingSelected
         ? false
         : (coords[2] == 1 && coords[3] == model ()->columnCount ()));

    bool current_row_selected
      = (nothingSelected ? false : (coords[0] <= index+1 && coords[1] > index));

    int rowselection_count = nothingSelected ? 0 : (coords[3] - coords[2] + 1);

    if (! whole_rows_selected || ! current_row_selected)
      {
        selectRow (index);
        rowselection_count = 1;
        current_row_selected = true;
        whole_rows_selected = true;
      }

    QString row_string = tr (rowselection_count > 1 ? " rows" : " row");

    QMenu *menu = new QMenu (this);

    add_edit_actions (menu, row_string);

    menu->addSeparator ();

    QSignalMapper *plot_mapper = make_plot_mapper (menu);

    connect (plot_mapper, SIGNAL (mapped (const QString&)),
             this, SLOT (selected_command_requested (const QString&)));

    QPoint menupos = pt;
    menupos.setX (verticalHeader ()->width ());

    // FIXME: What was the intent here?
    // setY (verticalHeader ()->sectionPosition (index+1) +
    //       verticalHeader ()->sectionSize (index));

    menu->exec (mapToGlobal (menupos));
  }

  void
  variable_editor_view::createVariable (void)
  {
    // FIXME: Create unnamed1..n if exist ('unnamed', 'var') is true.

    selected_command_requested ("unnamed = %1");
  }

  void
  variable_editor_view::transposeContent (void)
  {
    if (! hasFocus ())
      return;

    emit command_signal (QString ("%1 = %1';").arg (objectName ()));
  }

  void
  variable_editor_view::delete_selected (void)
  {
    if (! hasFocus ())
      return;

    QAbstractItemModel *mod = model ();
    QList<int> coords = range_selected ();

    if (coords.isEmpty ())
      return;

    bool whole_columns_selected
      = coords[0] == 1 && coords[1] == mod->rowCount ();

    bool whole_rows_selected
      = coords[2] == 1 && coords[3] == mod->columnCount ();

    // Must be deleting whole columns or whole rows, and not the whole thing.

    if (whole_columns_selected == whole_rows_selected)
      return;

    if (whole_rows_selected)
      mod->removeRows (coords[0], coords[1] - coords[0]);

    if (whole_columns_selected)
      mod->removeColumns (coords[2], coords[3] - coords[2]);
  }

  void
  variable_editor_view::clearContent (void)
  {
    if (! hasFocus ())
      return;

    if (m_var_model == nullptr)
      return;

    QItemSelectionModel *sel = selectionModel ();
    QList<QModelIndex> indices = sel->selectedIndexes ();

    // FIXME: Use [] for empty cells?

    for (const auto& idx : indices)
      m_var_model->clear_content (idx);
  }

  void
  variable_editor_view::cutClipboard (void)
  {
    copyClipboard ();

    clearContent ();
  }

  void
  variable_editor_view::copyClipboard (void)
  {
    if (! hasFocus ())
      return;

    QItemSelectionModel *sel = selectionModel ();
    QList<QModelIndex> indices = sel->selectedIndexes ();
    qSort (indices);

    if (indices.isEmpty ())
      return;

    // Convert selected items into TSV format and copy that.
    // Spreadsheet tools should understand that.

    QAbstractItemModel *mod = model ();
    QModelIndex previous = indices.first ();
    QString copy = mod->data (previous).toString ();
    indices.removeFirst ();
    foreach (QModelIndex idx, indices)
      {
        copy.push_back (previous.row () != idx.row () ? '\n' : '\t');
        copy.append (mod->data (idx).toString ());
        previous = idx;
      }

    QClipboard *clipboard = QApplication::clipboard ();
    clipboard->setText (copy);
  }

  void
  variable_editor_view::pasteClipboard (void)
  {
    if (! hasFocus ())
      return;

    QAbstractItemModel *mod = model ();
    QItemSelectionModel *sel = selectionModel ();
    QList<QModelIndex> indices = sel->selectedIndexes ();

    QClipboard *clipboard = QApplication::clipboard ();
    QString text = clipboard->text ();

    if (indices.isEmpty ())
      {
        if (size () == QSize (1,1))
          mod->setData (mod->index (0,0), text.toDouble ());
        else if (size () == QSize (0,0))
          {
            mod->insertColumn (0);
            mod->insertRow (0);
            mod->setData (mod->index (0,0), text.toDouble ());
          }
      }
    else
      {
        QStringList cells = text.split(QRegExp("\n|\r\n|\r"));
        int clen = cells.size ();
        for (int i = 0; i < indices.size (); i++)
          mod->setData (indices[i], cells.at (i % clen).toDouble ());
      }
  }

  void variable_editor_view::pasteTableClipboard (void)
  {
    if (! hasFocus ())
      return;

    QAbstractItemModel *mod = model ();
    QItemSelectionModel *sel = selectionModel ();
    QList<QModelIndex> indices = sel->selectedIndexes ();

    QClipboard *clipboard = QApplication::clipboard ();
    QString text = clipboard->text ();

    QPoint start, end;

    QPoint tabsize = QPoint (mod->rowCount (), mod->columnCount ());

    if (indices.isEmpty ())
      {
        start = QPoint (0,0);
        end = tabsize;
      }
    else if (indices.size () == 1)
      {
        start = QPoint (indices[0].row (), indices[0].column ());
        end = tabsize;
      }
    else
      {
        end = QPoint (0,0);
        start = tabsize;

        for (int i = 0; i < indices.size (); i++)
          {
            if (indices[i].column () < start.y ())
              start.setY (indices[i].column ());

            if (indices[i].column () > end.y ())
              end.setY (indices[i].column ());

            if (indices[i].row () < start.x ())
              start.setX (indices[i].column ());

            if (indices[i].row () > end.x ())
              end.setX (indices[i].column ());
          }
      }

    int rownum = 0;
    int colnum = 0;

    QStringList rows = text.split ('\n');
    for (const auto& row : rows)
      {
        if (rownum > end.x () - start.x ())
          continue;

        QStringList cols = row.split ('\t');
        if (cols.isEmpty ())
          continue;

        for (const auto& col : cols)
          {
            if (col.isEmpty ())
              continue;
            if (colnum > end.y () - start.y () )
              continue;

            mod->setData (mod->index (rownum + start.x (),
                                      colnum + start.y ()),
                          QVariant (col));

            colnum++;
          }

        colnum = 0;
        rownum++;
      }
  }

  void
  variable_editor_view::handle_horizontal_scroll_action (int action)
  {
    if (action == QAbstractSlider::SliderSingleStepAdd
        || action == QAbstractSlider::SliderPageStepAdd
        || action == QAbstractSlider::SliderToMaximum
        || action == QAbstractSlider::SliderMove)
      {
        if (m_var_model != nullptr)
          {
            QScrollBar *sb = horizontalScrollBar ();

            if (sb && sb->value () == sb->maximum ())
              {
                int new_cols = m_var_model->display_columns () + 16;

                m_var_model->maybe_resize_columns (new_cols);
              }
          }
      }
  }

  void
  variable_editor_view::handle_vertical_scroll_action (int action)
  {
    if (action == QAbstractSlider::SliderSingleStepAdd
        || action == QAbstractSlider::SliderPageStepAdd
        || action == QAbstractSlider::SliderToMaximum
        || action == QAbstractSlider::SliderMove)
      {
        if (m_var_model != nullptr)
          {
            QScrollBar *sb = verticalScrollBar ();

            if (sb && sb->value () == sb->maximum ())
              {
                int new_rows = m_var_model->display_rows () + 16;

                m_var_model->maybe_resize_rows (new_rows);
              }
          }
      }
  }


  // Gadgets for focus restoration

  HoverToolButton::HoverToolButton (QWidget *parent)
    : QToolButton (parent)
  {
    installEventFilter (this);
  }

  bool HoverToolButton::eventFilter (QObject *obj, QEvent *ev)
  {
    if (ev->type () == QEvent::HoverEnter)
      emit hovered_signal ();
    else if (ev->type () == QEvent::MouseButtonPress ||
             ev->type () == QEvent::MouseButtonPress)
      emit popup_shown_signal ();

    return QToolButton::eventFilter (obj, ev);
  }

  ReturnFocusToolButton::ReturnFocusToolButton (QWidget *parent)
    : HoverToolButton (parent)
  {
    installEventFilter (this);
  }

  bool ReturnFocusToolButton::eventFilter (QObject *obj, QEvent *ev)
  {

    if (ev->type () == QEvent::MouseButtonRelease && isDown ())
      {
        emit about_to_activate ();

        setDown (false);
        QAction *action = defaultAction ();
        if (action != nullptr)
          action->activate (QAction::Trigger);

        return true;
      }

    return HoverToolButton::eventFilter (obj, ev);
  }

  ReturnFocusMenu::ReturnFocusMenu (QWidget *parent)
    : QMenu (parent)
  {
    installEventFilter (this);
  }

  bool ReturnFocusMenu::eventFilter (QObject *obj, QEvent *ev)
  {
    if (ev->type () == QEvent::MouseButtonRelease && underMouse ())
      {
        emit about_to_activate ();
      }

    return QMenu::eventFilter (obj, ev);
  }

  // Variable editor.

  variable_editor::variable_editor (QWidget *p)
    : octave_dock_widget (p),
      m_main (new QMainWindow ()),
      m_tool_bar (new QToolBar (m_main)),
      m_default_width (30),
      m_default_height (100),
      m_add_font_height (0),
      m_use_terminal_font (true),
      m_alternate_rows (true),
      m_stylesheet (""),
      m_font (),
      m_sel_font (),
      m_table_colors (),
      m_current_focus_vname (""),
      m_hovered_focus_vname ("")
  {
    setObjectName ("VariableEditor");
    set_title (tr ("Variable Editor"));
    setStatusTip (tr ("Edit variables."));
    setWindowIcon (QIcon (":/actions/icons/logo.png"));
    setAttribute (Qt::WA_AlwaysShowToolTips);

    m_main->setParent (this);
// See Octave bug #53409 and https://bugreports.qt.io/browse/QTBUG-55357
#if (QT_VERSION < 0x050601) || (QT_VERSION >= 0x050701)
    m_main->setDockOptions (QMainWindow::AnimatedDocks |
                            QMainWindow::AllowNestedDocks |
                            QMainWindow::VerticalTabs);
#else
    m_main->setDockNestingEnabled (true);
#endif

    // Tool Bar.

    construct_tool_bar ();
    m_main->addToolBar (m_tool_bar);

    // Colors.

    for (int i = 0; i < resource_manager::varedit_color_chars ().length (); i++)
      m_table_colors.append (QColor (Qt::white));

    // Use an MDI area that is shrunk to nothing as the central widget.
    // Future feature might be to switch to MDI mode in which the dock
    // area is shrunk to nothing and the widgets live in the MDI window.

    QMdiArea *central_mdiarea = new QMdiArea (m_main);
    central_mdiarea->setMinimumSize (QSize (0, 0));
    central_mdiarea->setMaximumSize (QSize (0, 0));
    central_mdiarea->resize (QSize (0, 0));
    m_main->setCentralWidget (central_mdiarea);

    setWidget (m_main);

    connect (this, SIGNAL (command_signal (const QString&)),
             p, SLOT (execute_command_in_terminal (const QString&)));
  }

  // Add an action to a menu or the widget itself.

  QAction*
  variable_editor::add_action (QMenu *menu, const QIcon& icon,
                               const QString& text,
                               const char *member)
  {
    QAction *a;

    if (menu)
      a = menu->addAction (icon, text, this, member);
    else
      {
        a = new QAction (this);
        connect (a, SIGNAL (triggered ()), this, member);
      }

    addAction (a);  // important for shortcut context
    a->setShortcutContext (Qt::WidgetWithChildrenShortcut);

    return a;
  }

  void
  variable_editor::edit_variable (const QString& name, const octave_value& val)
  {
    if (m_stylesheet.isEmpty ())
      {
        QSettings *settings = resource_manager::get_settings ();
        notice_settings (settings);
      }

    QDockWidget *existing_qdw = m_main->findChild<QDockWidget *> (name);
    if (existing_qdw != NULL)
      {
        // Already open.

        // Put current focused variable out of focus
        if (m_main->focusWidget () != nullptr)
          {
            QFocusEvent event (QEvent::FocusOut, Qt::OtherFocusReason);
            QApplication::sendEvent (m_main->focusWidget (), &event);
          }

        // Put existing variable in focus and raise
        m_main->parentWidget ()->show ();
        existing_qdw->show ();
        existing_qdw->raise ();
        existing_qdw->activateWindow ();
        tab_to_front ();
        existing_qdw->setFocus ();

        return;
      }

    variable_dock_widget *page = new variable_dock_widget (this);
    page->setObjectName (name);
    m_main->addDockWidget (Qt::LeftDockWidgetArea, page);

    connect (QApplication::instance(), SIGNAL (focusChanged (QWidget *, QWidget *)),
             page, SLOT (handle_focus_change (QWidget *, QWidget *)));
    connect (page, SIGNAL (destroyed (QObject *)),
             this, SLOT (variable_destroyed (QObject *)));
    connect (page, SIGNAL (variable_focused_signal (const QString&)),
             this, SLOT (variable_focused (const QString&)));

    variable_editor_stack *stack = new variable_editor_stack (page);
    stack->setObjectName (name);
    page->setWidget (stack);
    page->setFocusProxy (stack);

    connect (stack, SIGNAL (command_signal (const QString&)),
             this, SIGNAL (command_signal (const QString&)));
    connect (stack, SIGNAL (edit_variable_signal (const QString&, const octave_value&)),
             this, SLOT (edit_variable (const QString&, const octave_value&)));
    connect (this, SIGNAL (level_up_signal ()),
             stack, SLOT (levelUp ()));
    connect (this, SIGNAL (save_signal ()),
             stack, SLOT (save ()));

    variable_editor_view *edit_view = stack->edit_view ();

    edit_view->setObjectName (name);
    edit_view->setFont (m_font);
    edit_view->setStyleSheet (m_stylesheet);
    edit_view->setAlternatingRowColors (m_alternate_rows);
    edit_view->verticalHeader ()->setDefaultSectionSize (m_default_height
                                                         + m_add_font_height);

    connect (edit_view, SIGNAL (command_signal (const QString&)),
             this, SIGNAL (command_signal (const QString&)));
    connect (this, SIGNAL (delete_selected_signal ()),
             edit_view, SLOT (delete_selected ()));
    connect (this, SIGNAL (clear_content_signal ()),
             edit_view, SLOT (clearContent ()));
    connect (this, SIGNAL (copy_clipboard_signal ()),
             edit_view, SLOT (copyClipboard ()));
    connect (this, SIGNAL (paste_clipboard_signal ()),
             edit_view, SLOT (pasteClipboard ()));
    connect (this, SIGNAL (paste_table_clipboard_signal ()),
             edit_view, SLOT (pasteTableClipboard ()));
    connect (this, SIGNAL (selected_command_signal (const QString&)),
             edit_view, SLOT (selected_command_requested (const QString&)));
    connect (edit_view->horizontalHeader (),
             SIGNAL (customContextMenuRequested (const QPoint&)),
             edit_view, SLOT (createColumnMenu (const QPoint&)));
    connect (edit_view->verticalHeader (),
             SIGNAL (customContextMenuRequested (const QPoint&)),
             edit_view, SLOT (createRowMenu (const QPoint&)));
    connect (edit_view, SIGNAL (customContextMenuRequested (const QPoint&)),
             edit_view, SLOT (createContextMenu (const QPoint&)));
    connect (edit_view->horizontalScrollBar (), SIGNAL (actionTriggered (int)),
             edit_view, SLOT (handle_horizontal_scroll_action (int)));
    connect (edit_view->verticalScrollBar (), SIGNAL (actionTriggered (int)),
             edit_view, SLOT (handle_vertical_scroll_action (int)));

    variable_editor_model *model =
      new variable_editor_model (name, val, stack);

    connect (model, SIGNAL (edit_variable_signal (const QString&, const octave_value&)),
             this, SLOT (edit_variable (const QString&, const octave_value&)));
    connect (model, SIGNAL (dataChanged (const QModelIndex&, const QModelIndex&)),
             this, SLOT (callUpdate (const QModelIndex&, const QModelIndex&)));
    connect (this, SIGNAL (refresh_signal ()),
             model, SLOT (update_data_cache ()));
    connect (model, SIGNAL (set_editable_signal (bool)),
             stack, SLOT (set_editable (bool)));

    edit_view->setModel (model);
    connect (edit_view, SIGNAL (doubleClicked (const QModelIndex&)),
             model, SLOT (double_click (const QModelIndex&)));

    // Must supply a title for a QLabel to be created.  Calling set_title()
    // more than once will add more QLabels.  Could change octave_dock_widget
    // to always supply a QLabl (initially empty) and then simply update its
    // contents.
    page->set_title (name);
    QLabel *existing_ql = page->titleBarWidget ()->findChild<QLabel *> ();
    connect (model, SIGNAL (update_label_signal (const QString&)),
             existing_ql, SLOT (setText (const QString&)));
    existing_ql->setMargin (2);

    model->update_data (val);

    QList<QTableView *> viewlist = findChildren<QTableView *> ();
    if (viewlist.size () == 1)
      m_tool_bar->setEnabled (true);

    m_main->parentWidget ()->show ();
    page->show ();
    page->raise ();
    page->activateWindow ();
    tab_to_front ();
    page->setFocus ();
  }

  void
  variable_editor::tab_to_front (void)
  {
    if (parent () != nullptr)
      {
        QList<QTabBar *> barlist = main_win ()->findChildren<QTabBar *> ();

        foreach (QTabBar *tbar, barlist)
          {
            for (int i=0; i < tbar->count (); i++)
              {
                  if ((QWidget *) tbar->tabData (i).toULongLong () == this)
                  {
                    tbar->setCurrentIndex (i);
                    return;
                  }
              }
          }
      }
  }

  void
  variable_editor::refresh (void)
  {
    emit refresh_signal ();
  }

  QList<QColor>
  variable_editor::default_colors (void)
  {
    QList<QColor> colorlist;

    colorlist << qApp->palette ().color (QPalette::WindowText);
    colorlist << qApp->palette ().color (QPalette::Base);
    colorlist << qApp->palette ().color (QPalette::HighlightedText);
    colorlist << qApp->palette ().color (QPalette::Highlight);
    colorlist << qApp->palette ().color (QPalette::AlternateBase);

    return colorlist;
  }

  QStringList
  variable_editor::color_names (void)
  {
    QStringList output;

    output << "Foreground";
    output << "Background";
    output << "Selected Foreground";
    output << "Selected Background";
    output << "Alternate Background";

    return output;
  }

  void
  variable_editor::callUpdate (const QModelIndex&, const QModelIndex&)
  {
    emit updated ();
  }

  void
  variable_editor::notice_settings (const QSettings *settings)
  {
    // FIXME: Why use object->tostring->toint?  Why not just 100?

    m_default_width = settings->value ("variable_editor/column_width",
                                       100).toInt ();

    m_default_height = settings->value ("variable_editor/row_height",
                                        10).toInt ();

    m_alternate_rows = settings->value ("variable_editor/alternate_rows",
                                        false).toBool ();

    QList<QColor> default_colors = resource_manager::varedit_default_colors ();

    QString class_chars = resource_manager::varedit_color_chars ();

    m_use_terminal_font = settings->value ("variable_editor/use_terminal_font",
                                           true).toBool ();

    QString font_name;
    int font_size;

    if (m_use_terminal_font)
      {
        font_name = settings->value ("terminal/fontName", "Courier New").toString ();
        font_size = settings->value ("terminal/fontSize", 10).toInt ();
      }
    else
      {
        font_name = settings->value ("variable_editor/font_name",
                                     settings->value ("terminal/fontName",
                                                      "Courier New")).toString ();

        font_size = settings->value ("variable_editor/font_size", 10).toInt ();
      }

    m_font = QFont (font_name, font_size);

    QFontMetrics fm (m_font);

    m_add_font_height = fm.height ();

    for (int i = 0; i < class_chars.length (); i++)
      {
        QVariant default_var;
        if (i < default_colors.length ())
          default_var = default_colors.at (i);
        else
          default_var = QColor ();

        QColor setting_color = settings->value ("variable_editor/color_"
                                                + class_chars.mid (i, 1),
                                                default_var).value<QColor> ();

        if (i < m_table_colors.length ())
          m_table_colors.replace (i, setting_color);
      }

    update_colors ();

    // Icon size in the toolbar.

    int icon_size_settings = settings->value ("toolbar_icon_size", 0).toInt ();
    QStyle *st = style ();
    int icon_size = st->pixelMetric (QStyle::PM_ToolBarIconSize);

    // FIXME: Magic numbers.  Use enum?

    if (icon_size_settings == 1)
      icon_size = st->pixelMetric (QStyle::PM_LargeIconSize);
    else if (icon_size_settings == -1)
      icon_size = st->pixelMetric (QStyle::PM_SmallIconSize);

    m_tool_bar->setIconSize (QSize (icon_size, icon_size));
  }

  void
  variable_editor::closeEvent (QCloseEvent *e)
  {
    emit finished ();

    octave_dock_widget::closeEvent (e);
  }

  void
  variable_editor::variable_destroyed (QObject *)
  {
    QList<variable_dock_widget *> vdwlist = findChildren<variable_dock_widget *> ();
    if (vdwlist.isEmpty ())
      m_tool_bar->setEnabled (false);
  }

  void
  variable_editor::variable_focused (const QString &name)
  {
    m_current_focus_vname = name;
  }

  void
  variable_editor::record_hovered_focus_variable (void)
  {
    m_hovered_focus_vname = m_current_focus_vname;
  }

  void
  variable_editor::restore_hovered_focus_variable (void)
  {
    variable_dock_widget *tofocus = findChild<variable_dock_widget *> (m_hovered_focus_vname);
    if (tofocus != nullptr)
      {
        // Note that this may be platform and window system dependent.
        // On a particular Linux system, activateWindow() alone didn't
        // immediately set the active window and there was a race
        // between the window focus and action signal.  Setting the
        // active window via the QApplication route did work.
        QApplication::setActiveWindow(tofocus->window());
        tofocus->activateWindow ();
        tofocus->setFocus (Qt::OtherFocusReason);
      }
  }

  void
  variable_editor::save (void)
  {
    emit save_signal ();
  }

  void
  variable_editor::cutClipboard (void)
  {
    copyClipboard ();

    emit clear_content_signal ();
  }

  void
  variable_editor::copyClipboard (void)
  {
    emit copy_clipboard_signal ();
  }

  void
  variable_editor::pasteClipboard (void)
  {
    emit paste_clipboard_signal ();

    emit updated ();
  }

  void
  variable_editor::pasteTableClipboard (void)
  {
    emit paste_table_clipboard_signal ();

    emit updated ();
  }

  void
  variable_editor::levelUp (void)
  {
    emit level_up_signal ();
  }

  void
  variable_editor::relay_selected_command (const QString& cmd)
  {
    emit selected_command_signal (cmd);
  }

  // Also updates the font.

  void variable_editor::update_colors (void)
  {
    m_stylesheet = "";

    if (m_table_colors.length () > 0)
      m_stylesheet += "QTableView::item{ foreground-color: "
                      + m_table_colors[0].name () +" }";

    if (m_table_colors.length () > 1)
      m_stylesheet += "QTableView::item{ background-color: "
                      + m_table_colors[1].name () +" }";

    if (m_table_colors.length () > 2)
      m_stylesheet += "QTableView::item{ selection-color: "
                      + m_table_colors[2].name () +" }";

    if (m_table_colors.length () > 3)
      m_stylesheet += "QTableView::item:selected{ background-color: "
                      + m_table_colors[3].name () +" }";

    if (m_table_colors.length () > 4 && m_alternate_rows)
      {
        m_stylesheet
          += "QTableView::item:alternate{ background-color: "
          + m_table_colors[4].name () +" }";

        m_stylesheet
          += "QTableView::item:alternate:selected{ background-color: "
          + m_table_colors[3].name () +" }";
      }

    QList<QTableView *> viewlist = findChildren<QTableView *> ();
    for (int i = 0; i < viewlist.size (); i++)
      {
        QTableView *view = viewlist.at (i);

        if (! view)
          continue;

        view->setAlternatingRowColors (m_alternate_rows);
        view->setStyleSheet (m_stylesheet);
        view->setFont (m_font);
      }

  }

  QAction *
  variable_editor::add_tool_bar_button (const QIcon &icon,
                                        const QString &text,
                                        const QObject *receiver,
                                        const char *member)
  {
    QAction *action = new QAction (icon, text, this);
    connect(action, SIGNAL (triggered ()), receiver, member);
    QToolButton *button = new ReturnFocusToolButton (m_tool_bar);
    button->setDefaultAction (action);
    button->setText (text);
    button->setToolTip (text);
    button->setIcon (icon);
    m_tool_bar->addWidget (button);

    return action;
  }

  void
  variable_editor::construct_tool_bar (void)
  {
    m_tool_bar->setAllowedAreas (Qt::TopToolBarArea);

    m_tool_bar->setObjectName ("VariableEditorToolBar");

    m_tool_bar->setWindowTitle (tr ("Variable Editor Toolbar"));

    QAction *action;
    action = add_tool_bar_button (resource_manager::icon ("document-save"),
                                  tr ("Save"), this, SLOT (save ()));
    action->setShortcuts (QKeySequence::Save);
    action->setStatusTip(tr("Save variable to a file"));

    m_tool_bar->addSeparator ();

    action = add_tool_bar_button (resource_manager::icon ("edit-cut"),
                                  tr ("Cut"), this, SLOT (cutClipboard ()));
    action->setStatusTip(tr("Cut data to clipboard"));

    action = add_tool_bar_button (resource_manager::icon ("edit-copy"),
                                  tr ("Copy"), this, SLOT (copyClipboard ()));
    action->setStatusTip(tr("Copy data to clipboard"));

    action = add_tool_bar_button (resource_manager::icon ("edit-paste"),
                                  tr ("Paste"), this, SLOT (pasteClipboard ()));
    action->setStatusTip(tr("Paste clipboard into variable data"));

    // FIXME: Different icon for Paste Table?

    action = add_tool_bar_button (resource_manager::icon ("edit-paste"),
                                  tr ("Paste Table"),
                                  this, SLOT (pasteTableClipboard ()));
    action->setStatusTip(tr("Another paste clipboard into variable data"));

    m_tool_bar->addSeparator ();

    // FIXME: Add a print item?
    // QAction *print_action; /icons/fileprint.png
    // m_tool_bar->addSeparator ();

    action = new QAction (resource_manager::icon ("plot-xy-curve"),
                          tr ("Plot"), m_tool_bar);
    action->setToolTip (tr ("Plot Selected Data"));
    QToolButton *plot_tool_button = new HoverToolButton (m_tool_bar);
    plot_tool_button->setDefaultAction (action);

    plot_tool_button->setText (tr ("Plot"));
    plot_tool_button->setToolTip (tr ("Plot selected data"));
    plot_tool_button->setIcon (resource_manager::icon ("plot-xy-curve"));

    plot_tool_button->setPopupMode (QToolButton::InstantPopup);

    QMenu *plot_menu = new ReturnFocusMenu (plot_tool_button);
    plot_menu->setTitle (tr ("Plot"));
    plot_menu->setSeparatorsCollapsible (false);

    QSignalMapper *plot_mapper = make_plot_mapper (plot_menu);

    connect (plot_mapper, SIGNAL (mapped (const QString&)),
             this, SLOT (relay_selected_command (const QString&)));

    plot_tool_button->setMenu (plot_menu);

    m_tool_bar->addWidget (plot_tool_button);

    m_tool_bar->addSeparator ();

    action = add_tool_bar_button (resource_manager::icon ("go-up"),
                                  tr ("Up"), this, SLOT (levelUp ()));
    action->setStatusTip(tr("Go one level up in variable hierarchy"));

    // The QToolButton mouse-clicks change active window, so connect all
    // HoverToolButton and RuternFocusToolButton objects to the mechanism
    // that restores active window and focus before acting.
    QList<HoverToolButton *> hbuttonlist
      = m_tool_bar->findChildren<HoverToolButton *> (""
#if defined (QOBJECT_FINDCHILDREN_ACCEPTS_FINDCHILDOPTIONS)
                                                     , Qt::FindDirectChildrenOnly
#endif
                                                     );
    for (int i = 0; i < hbuttonlist.size (); i++)
      {
        connect (hbuttonlist.at (i), SIGNAL (hovered_signal ()),
                 this, SLOT (record_hovered_focus_variable ()));
        connect (hbuttonlist.at (i), SIGNAL (popup_shown_signal ()),
                 this, SLOT (restore_hovered_focus_variable ()));
      }

    QList<ReturnFocusToolButton *> rfbuttonlist
      = m_tool_bar->findChildren<ReturnFocusToolButton *> (""
#if defined (QOBJECT_FINDCHILDREN_ACCEPTS_FINDCHILDOPTIONS)
                                                           , Qt::FindDirectChildrenOnly
#endif
                                                           );
    for (int i = 0; i < rfbuttonlist.size (); i++)
      {
        connect (rfbuttonlist.at (i), SIGNAL (about_to_activate ()),
                 this, SLOT (restore_hovered_focus_variable ()));
      }

    // Same for QMenu
    QList<ReturnFocusMenu *> menulist
      = m_tool_bar->findChildren<ReturnFocusMenu *> ();
    for (int i = 0; i < menulist.size (); i++)
      {
        connect (menulist.at (i), SIGNAL (about_to_activate ()),
                 this, SLOT (restore_hovered_focus_variable ()));
      }

    m_tool_bar->setAttribute(Qt::WA_ShowWithoutActivating);
    m_tool_bar->setFocusPolicy (Qt::NoFocus);

    // Disabled when no tab is present.

    m_tool_bar->setEnabled (false);
  }
}
