/****************************************************************************
**
** Copyright (C) 2006-2009 fullmetalcoder <fullmetalcoder@hotmail.fr>
**
** This file is part of the Edyuk project <http://edyuk.org>
** 
** This file may be used under the terms of the GNU General Public License
** version 3 as published by the Free Software Foundation and appearing in the
** file GPL.txt included in the packaging of this file.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
****************************************************************************/

#ifndef _QSNIPPET_EDIT_H_
#define _QSNIPPET_EDIT_H_

#include "qce-config.h"

/*!
	\file qsnippetedit.h
	\brief Definition of the QSnippetEdit widget
*/

#include <QWidget>
#include "ui_snippetedit.h"

class QSnippet;
class QSnippetManager;

class QCE_EXPORT QSnippetEdit : public QWidget, private Ui::SnippetEdit
{
	Q_OBJECT
	
	public:
		QSnippetEdit(QWidget *p = 0);
		QSnippetEdit(QSnippetManager *mgr, QWidget *p = 0);
		
		QSnippetManager* snippetManager() const;
		
	public slots:
		void setSnippetManager(QSnippetManager *mgr);
		
		bool maybeSave();
		
		void retranslate();
		
	private slots:
		void on_lwSnippets_currentRowChanged(int idx);
		
		void on_leSnippetName_editingFinished();
		void on_leSnippetScope_editingFinished();
		
		void on_tbCreateSnippet_clicked();
		void on_tbDeleteSnippet_clicked();
		void on_bMoreSnippets_clicked();
		
		void snippetRemoved(int i);
		void snippetAdded(QSnippet *s);
		
	private:
		int m_editedSnippet;
		QSnippetManager *m_manager;
};

#endif
