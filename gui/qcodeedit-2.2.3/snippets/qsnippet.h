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

#ifndef _QSNIPPET_H_
#define _QSNIPPET_H_

#include "qce-config.h"

/*!
	\file qsnippet.h
	\brief Definition of the QSnippet class
*/

#include <QStringList>

class QEditor;
class QSnippetManager;

#include "qsnippetpatternloader.h"

class QCE_EXPORT QSnippet
{
	friend class QSnippetManager;
	
	public:
		inline QSnippet(const QSnippetPatternLoader *pl) : m_patternLoader(pl) {}
		virtual ~QSnippet() {}
		
		inline QString name() const { return m_name; }
		inline void setName(const QString& n) { m_name = n; }
		
		inline QStringList contexts() const { return m_contexts; }
		inline void setContexts(const QStringList& l) { m_contexts = l; }
		
		inline QString pattern() const
		{ return m_pattern; }
		
		inline void setPattern(const QString& p)
		{ m_pattern = p; m_patternLoader->reloadSnippet(this, p); }
		
		virtual void insert(QEditor *e) const = 0;
		
	protected:
		QString m_name;
		QString m_pattern;
		QStringList m_contexts;
		const QSnippetPatternLoader *m_patternLoader;
};

#endif
