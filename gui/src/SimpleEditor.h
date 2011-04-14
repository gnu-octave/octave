#ifndef __SIMPLEEDITOR_H__
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


#define __SIMPLEEDITOR_H__

#include <QPlainTextEdit>
#include <QCompleter>
#include <QStringListModel>
#include "SyntaxHighlighter.h"

class SimpleEditor : public QPlainTextEdit
{
	Q_OBJECT
	QString fileName;
	QTextCharFormat charFormat;
	QCompleter *completer;
	QStringListModel *completerModel;
        SyntaxHighlighter *syntaxHighlighter;
	QStringList comands_completion_list;
	
	bool auto_indent, automatic_indention_statement_ok;
	
	void loadSyntaxXMLDescription();
	
	public:
	bool firtsTimeUsedOk;
	
        SimpleEditor(QWidget * parent = 0);
	
	bool load(QString file);
	bool save();
	QString getFileName();
	void setFile(QString file);
	
	void setCharFormat(QTextCharFormat charFormat);
	
	
	
	/**List of y top left positions of bounding rects of each visible block of text.
	 * @param list List of top left positions.
	 * @param first_line First visible block in TextEdit.
	 */
	void publicBlockBoundingRectList(QVector<qreal>  &list, int &first_line);
	
	public slots:
	
	void activated( const QString & text );
	void cursorPositionChangedCallBack();
	void autoComplete(int size=3);
	void autoComplete(int position, int charsRemoved, int charsAdded);
	
	protected:
	virtual void keyPressEvent(QKeyEvent * e);
	//virtual void focusInEvent(QFocusEvent  * event);
	

	signals:
	/**Dinamic help required.*/
	void dynamic_help_required(const QString &text);
	
	///**Text modified.*/
	//void text_modified(bool ok);
	
	//void toggleBreakpoint(int lineno);
	
	//void focusChanged(QWidget *);
}
;
#endif

