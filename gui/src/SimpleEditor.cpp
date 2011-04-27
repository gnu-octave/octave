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

#include "SimpleEditor.h"
#include <QFile>
#include <QTextStream>
#include <QTextBlock>
#include <QFileInfo>
#include <QDir>

SimpleEditor::SimpleEditor(QWidget *parent)
    : QPlainTextEdit(parent),
      m_syntaxHighlighter(0),
      m_firstTimeUse(true) {

    m_completerModel = new QStringListModel ();
    m_completer = new QCompleter(m_completerModel, this);
    m_completer->setCompletionMode(QCompleter::PopupCompletion);
    m_completer->setWidget(this);
    m_autoIndentation = true;
    m_automaticIndentationStatement = true;

    QFont font;
    font.setFamily("Courier");
    font.setPointSize(10);
    setFont(font);

    connect(m_completer, SIGNAL(activated(const QString &)), this, SLOT(activated(const QString &)));
    connect(this, SIGNAL(cursorPositionChanged()), this, SLOT(cursorPositionChangedCallBack()));
    connect(document(), SIGNAL(contentsChange(int, int, int)), this, SLOT(autoComplete(int, int, int)));
}

void SimpleEditor::loadSyntaxXMLDescription() {
    QString installPath = QString("../syntax_files")
        + QDir::separator();

    QFileInfo file(m_currentFileName);
    QString suffix = file.suffix();

    if(m_commandsCompletionList.isEmpty()) {
        QString home = QDir::home().path()
            + QDir::separator()
            + ".qtoctave"
            + QDir::separator()
            + "commands.txt";

        QFile file(home);

        if(file.open(QFile::ReadOnly)) {
            char buf[1024];
            while(file.readLine(buf, sizeof(buf)) >= 0) {
                m_commandsCompletionList.append(QString(buf).trimmed());
            }
            file.close();
        }
    }

    QFileInfo xml(installPath + suffix + ".xml");
    if(xml.exists()) {
        m_syntaxHighlighter = new SyntaxHighlighter(document());
        m_syntaxHighlighter->load(xml.absoluteFilePath());
        m_syntaxHighlighter->setDocument(document());
    }
}

bool SimpleEditor::load(QString file) {
    if(file.isEmpty()) {
        setPlainText("");
        m_currentFileName = file;
        return true;
    }

    FILE *input = fopen(file.toLocal8Bit().data(),"r");
    if(!input)
        return false;
    fclose(input);
    QFile in(file);
    if(!in.open(QIODevice::ReadOnly | QIODevice::Text)) {
        return false;
    }
    QByteArray data = in.readAll();
    setPlainText(QString::fromLocal8Bit(data));
    m_currentFileName = file;
    m_firstTimeUse = false;

    loadSyntaxXMLDescription();

    return true;
}

bool SimpleEditor::save() {
    QFile::remove(m_currentFileName + "~");
    QFile::copy(m_currentFileName, m_currentFileName + "~");
    FILE *out=fopen(m_currentFileName.toLocal8Bit().data(),"w");
    if(!out)
        return false;
    fprintf(out, "%s", toPlainText().toLocal8Bit().data());
    fclose(out);
    document()->setModified(false);
    return true;
}

void SimpleEditor::keyPressEvent(QKeyEvent * keyEvent) {
    //In all cases completer popup must been hided.
    if(keyEvent->key() != Qt::Key_Return && keyEvent->key() != Qt::Key_Enter) {
        QAbstractItemView *view = m_completer->popup();
        if(view->isVisible()) view->hide();
    }

    if(keyEvent->key() == Qt::Key_Return || keyEvent->key() == Qt::Key_Enter) {
        QAbstractItemView *view = m_completer->popup();
        if(view->isVisible()) {
            QString word = view->currentIndex().data().toString();
            if(word.isEmpty()) {
                word = m_completer->currentCompletion();
            }
            activated(word);
            return;
        } else if(m_autoIndentation) {
            QTextCursor cursor = textCursor();
            QString line = cursor.block().text();
            QString line2 = line;
            for(int i=0;i<line.length();i++) {
                if(line[i] != ' ' && line[i] != '\t') {
                    line.resize(i);
                    break;
                }
            }

            cursor.insertText("\n" + line);
            if(m_automaticIndentationStatement) {
                    QRegExp re("^while .*|^if .*|^for .*|^switch .*|^do$|^try|^function .*|^else$|^elseif .*");
                    if(re.exactMatch(line2.trimmed())) {
                            cursor.insertText("\t");
                    }
            }
            setTextCursor(cursor);
        } else {
            QPlainTextEdit::keyPressEvent(keyEvent);
        }
    } else if(keyEvent->key() == Qt::Key_Tab) {
            QTextCursor cursor=textCursor();
            int start=cursor.selectionStart();
            int end=cursor.selectionEnd();
            if(start == end) {
                QPlainTextEdit::keyPressEvent(keyEvent);
                return;
            }
            cursor.beginEditBlock();
            cursor.setPosition(end);
            end=cursor.blockNumber();
            cursor.setPosition(start);
            cursor.movePosition(QTextCursor::StartOfBlock);
            while(true) {
                cursor.insertText("\t");
                if(cursor.blockNumber()>=end) {
                    break;
                }
                cursor.movePosition(QTextCursor::NextBlock);
            }
            cursor.endEditBlock();
    } else if(keyEvent->key()==Qt::Key_Backtab) {
        QTextCursor cursor=textCursor();
        int start=cursor.selectionStart();
        int end=cursor.selectionEnd();
        if(start==end) {
            QPlainTextEdit::keyPressEvent(keyEvent);
            return;
        }
        cursor.beginEditBlock();
        cursor.setPosition(end);
        end=cursor.blockNumber();
        cursor.setPosition(start);
        cursor.movePosition(QTextCursor::StartOfBlock);
        while(true) {
            QString line=cursor.block().text();
            if(line.length()>0 && (line[0]==' ' || line[0] =='\t')) {
                cursor.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor);
                cursor.removeSelectedText();
            }
            if(cursor.blockNumber()>=end) break;
            cursor.movePosition(QTextCursor::NextBlock);
            cursor.movePosition(QTextCursor::StartOfBlock);
        }
        cursor.endEditBlock();
    } else {
        if(keyEvent->key()==(Qt::Key_B) && Qt::ControlModifier==keyEvent->modifiers()) {
            autoComplete(0);
            return;
        }
        QPlainTextEdit::keyPressEvent(keyEvent);
    }
}

void SimpleEditor::setCharFormat(QTextCharFormat charFormat) {
    this->m_charFormat=charFormat;
    QTextCursor cursor=textCursor();
    cursor.movePosition(QTextCursor::Start);
    cursor.setCharFormat(charFormat);
    cursor.movePosition(QTextCursor::End, QTextCursor::KeepAnchor);
    setFont(charFormat.font());

    QFontMetrics fm(charFormat.font());
    int textWidthInPixels = fm.width("        ");
    setTabStopWidth(textWidthInPixels);
}

void SimpleEditor::activated(const QString& text) {
    QAbstractItemView *view=m_completer->popup();
    QTextCursor cursor=textCursor();
    cursor.movePosition(QTextCursor::PreviousWord, QTextCursor::KeepAnchor);
    cursor.insertText(text);
    view->hide();
}

void SimpleEditor::autoComplete(int position, int charsRemoved, int charsAdded) {
    if(charsAdded==1)
            autoComplete();
}

void SimpleEditor::autoComplete(int size) {
    QTextCursor cursor = textCursor();
    cursor.movePosition(QTextCursor::PreviousWord, QTextCursor::KeepAnchor);
    if(cursor.selectedText().endsWith(" ")
            || cursor.selectedText().trimmed().length() < size) {
        return;
    }

    QStringList list=toPlainText().split(QRegExp("\\W+"));
    list.removeDuplicates();
    list.removeOne(cursor.selectedText());
    list.sort();
    list.append(m_commandsCompletionList);

    m_completerModel->setStringList(list);
    m_completer->setCompletionPrefix(cursor.selectedText());

    if(m_completer->completionCount() > 0) {
            QRect r=cursorRect(cursor);
            r.setWidth(200);
            m_completer->complete(r);
    }
}

QString SimpleEditor::getFileName() {
    return m_currentFileName;
}

void SimpleEditor::setFile(QString file) {
    m_currentFileName = file;
    loadSyntaxXMLDescription();
}

void SimpleEditor::cursorPositionChangedCallBack() {
    if(m_syntaxHighlighter)
            m_syntaxHighlighter->setFormatPairBrackets(this);
}

void SimpleEditor::publicBlockBoundingRectList(QVector<qreal> &list, int &firstLine) {
    qreal pageBottom = height();
    QPointF offset = contentOffset();
    QTextBlock block = firstVisibleBlock();
    firstLine = block.blockNumber() + 1;
    qreal first_position = blockBoundingGeometry(block).topLeft().y();
    for(; block.isValid(); block = block.next()) {
        QRectF position = blockBoundingGeometry(block);
        qreal y = position.topLeft().y() + offset.y() - first_position;
        if(y > pageBottom)
            break;
        list.append(y);
    }
}

