/* Copyright (C) 2010 P.L. Lucas
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

#ifndef SIMPLEEDITOR_H
#define SIMPLEEDITOR_H

#include <QPlainTextEdit>
#include <QCompleter>
#include <QStringListModel>
#include "SyntaxHighlighter.h"

class SimpleEditor : public QPlainTextEdit {
    Q_OBJECT
public:
    SimpleEditor(QWidget * parent = 0);
    bool load(QString file);
    bool save();
    QString getFileName();
    void setFile(QString file);
    void setCharFormat(QTextCharFormat m_charFormat);
    void publicBlockBoundingRectList(QVector<qreal>  &list, int &firstLine);
    void loadSyntaxXMLDescription();

public slots:
    void activated(const QString& text);
    void cursorPositionChangedCallBack();
    void autoComplete(int size = 3);
    void autoComplete(int position, int charsRemoved, int charsAdded);

protected:
    virtual void keyPressEvent(QKeyEvent * e);

private:
    bool m_firstTimeUse;
    QString m_currentFileName;
    QTextCharFormat m_charFormat;
    QCompleter *m_completer;
    QStringListModel *m_completerModel;
    SyntaxHighlighter *m_syntaxHighlighter;
    QStringList m_commandsCompletionList;
    bool m_autoIndentation;
    bool m_automaticIndentationStatement;
};

#endif // SIMPLEEDITOR_H

