/* Copyright (C) 2007 Alejandro Álvarez
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef CODEEDIT_H
#define CODEEDIT_H

#include <QTextEdit>
#include <QMenu>
#include <QTextCursor>
#include "SyntaxHighlighter.h"
#include <QCompleter>
#include <QStringListModel>
#include <QTimer>
#include "config.h"
#include <QUndoStack>

struct UndoRedoItem
{
	int size, pos;
	QString text;
};

/**TextEdit that supports highlited syntax and autocompletion.*/
class CodeEdit: public QTextEdit {
    Q_OBJECT
public:
    CodeEdit(QWidget *parent = 0);
    ~CodeEdit();

    /**List of y top left positions of bounding rects of each visible block of text.
    * @param list List of top left positions.
    * @param first_line First visible block in TextEdit.
    */
    void publicBlockBoundingRectList(QVector<qreal>  &list, int &first_line);

public slots:
    void undo();
    void redo();
    void deleteSelection();
    void toggleBreakpoint();
    void buildAutoCompletionList(int pos, int charsRemoved, int charsAdded );
    void buildAutoCompletionList();
    void doCompletion(const QModelIndex &index);
    void octaveCommandCompletion();
    void textModified_cb(bool ok);

signals:
    void toggleBreakpoint(int lineno);

    /** Dynamic help required. */
    void dynamicHelpRequired(const QString &text);

    /** Text modified. */
    void textModified(bool ok);

protected:
    SyntaxHighlighter *m_syntaxHighlighter;
    QMenu contextMenu;
    void contextMenuEvent(QContextMenuEvent *e);
    bool event( QEvent * e );

private:
    bool auto_indent;
    QCompleter completion;
    QStringListModel *completion_model;
    QTimer braketsTimer, octaveCommandTimer, completionTimer;
    QStringList completion_list;
    int completionPosition;

    /** Builds auto completion list from block blockInit to blockEnd. */
    void buildAutoCompletionListSlide(QStringList &list, QTextBlock blockInit, QTextBlock blockEnd, QString word_to_complete, QString actual_word);

    bool text_modified_stop_ok; //Stops emit of text_modified signal
    bool context_changed_ok;

    /** Automatic indention for while, if, for, switch, do and try statements. */
    bool automatic_indention_statement_ok;
    /** Auto completion. */
    bool autocompletion_ok;
};

#endif // CODEEDIT_H
