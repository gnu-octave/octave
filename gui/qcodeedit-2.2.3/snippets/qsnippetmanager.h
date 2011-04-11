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

#ifndef _QSNIPPET_MANAGER_H_
#define _QSNIPPET_MANAGER_H_

#include "qce-config.h"

/*!
	\file qsnippetmanager.h
	\brief Definition of the QSnippetManager class
*/

#include <QStringList>

class QSnippet;
class QSnippetPatternLoader;

class QCE_EXPORT QSnippetManager : public QObject
{
	Q_OBJECT
	
	public:
		QSnippetManager(QObject *p = 0);
		virtual ~QSnippetManager();
		
		int snippetCount() const;
		QSnippet* snippet(int i) const;
		void removeSnippet(int i, bool cleanup = true);
		
		bool loadSnippetFromFile(const QString& file, const QString& type = QString());
		bool loadSnippetFromString(const QString& name, const QString& pattern, const QString& type = QString());
		
		void saveSnippetsToDirectory(const QString& path);
		void loadSnippetsFromDirectory(const QString& path);
		
	public slots:
		void addSnippet(QSnippet *s);
		void removeSnippet(QSnippet *s);
		
		void addPatternLoader(QSnippetPatternLoader *pl);
		void removePatternLoader(QSnippetPatternLoader *pl);
		
	signals:
		void snippetAdded(QSnippet *s);
		
		void snippetRemoved(int i);
		void snippetRemoved(QSnippet *s);
		
	private:
		QString typeGuess(const QString& pattern) const;
		QSnippetPatternLoader* patternLoader(const QString& type) const;
		
		QList<QSnippet*> m_snippets;
		QList<QSnippetPatternLoader*> m_patternLoaders;
};

#endif // !_SNIPPETS_H_
