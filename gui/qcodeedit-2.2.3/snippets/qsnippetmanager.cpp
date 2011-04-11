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

/*!
	\file qsnippetmanager.cpp
	\brief Implementation of the QSnippetManager class
*/

#include "qsnippetmanager.h"

#include "qsnippet_p.h"

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QTextStream>

/*
class StubAffector : public QEditor::PlaceHolder::Affector
{
	public:
		virtual void affect(const QStringList& base, int ph, const QKeyEvent *e, int mirror, QString& after) const
		{
			after = after.toUpper();
		}
};
*/

/*!
	\class QSnippetManager
	\brief A class managing code snippets
	
	
*/

QSnippetManager::QSnippetManager(QObject *p)
 : QObject(p)
{
	addPatternLoader(new QCE::Snippets::PlainText::Loader);
	addPatternLoader(new QCE::Snippets::Simple::Loader);
}

QSnippetManager::~QSnippetManager()
{
	qDeleteAll(m_snippets);
	qDeleteAll(m_patternLoaders);
}

int QSnippetManager::snippetCount() const
{
	return m_snippets.count();
}

QSnippet* QSnippetManager::snippet(int i) const
{
	return i >= 0 && i < m_snippets.count() ? m_snippets.at(i) : 0;
}

void QSnippetManager::removeSnippet(int i, bool cleanup)
{
	if ( i < 0 || i >= m_snippets.count() )
		return;
	
	QSnippet *snip = m_snippets.takeAt(i);
	
	emit snippetRemoved(i);
	emit snippetRemoved(snip);
	
	if ( cleanup )
		delete snip;
}

void QSnippetManager::removeSnippet(QSnippet *snip)
{
	int idx = m_snippets.indexOf(snip);
	
	if ( idx == -1 )
		return;
	
	m_snippets.removeAt(idx);
	
	emit snippetRemoved(idx);
	emit snippetRemoved(snip);
}

void QSnippetManager::addSnippet(QSnippet *snip)
{
	if ( !snip )
		return;
	
	m_snippets << snip;
	emit snippetAdded(snip);
}

bool QSnippetManager::loadSnippetFromString(const QString& name, const QString& s, const QString& type)
{
	QSnippetPatternLoader *pl = patternLoader(type);
	
	QSnippet *snip = pl->loadSnippet(s);
	
	if ( snip )
		snip->setName(name);
	
	addSnippet(snip);
	
	return snip;
}

bool QSnippetManager::loadSnippetFromFile(const QString& file, const QString& type)
{
	QFile f(file);
	
	if ( !f.open(QFile::ReadOnly | QFile::Text) )
	{
		qWarning("Unable to load snippet from %s", qPrintable(file));
		return false;
	}
	
	//qDebug("loading from : %s", qPrintable(file));
	
	QString s = QString::fromLocal8Bit(f.readAll());
	
	static const QRegExp meta("# name:(\\S+) context:(\\S*)[^\n]*\n");
	
	int idx = meta.indexIn(s);
	bool metaMatch = idx != -1;
	
	if ( metaMatch )
	{
		//qDebug("meta! : %i => %s", idx, qPrintable(meta.cap(0)));
		s.remove(idx, meta.matchedLength());
	}
	
	if ( s.endsWith('\n') )
		s.chop(1);
	
	bool ok = loadSnippetFromString(metaMatch ? meta.cap(1) : QFileInfo(file).baseName(), s, type);
	
	if ( ok )
	{
		QSnippet *snip = m_snippets.last();
		snip->setContexts(metaMatch ? meta.cap(2).split(",") : QStringList("all"));
	}
	
	return ok;
}

QSnippetPatternLoader* QSnippetManager::patternLoader(const QString& type) const
{
	foreach ( QSnippetPatternLoader *pl, m_patternLoaders )
	{
		if ( pl->type() == type )
			return pl;
	}
	
	return 0;
}

void QSnippetManager::saveSnippetsToDirectory(const QString& path)
{
	QDir d(path);
	
	foreach ( QSnippet *snip, m_snippets )
	{
		QFile f(d.filePath(snip->name() + ".qcs"));
		
		if ( !f.open(QFile::WriteOnly | QFile::Text) )
			continue;
		
		QTextStream s(&f);
		s << "# name:" << snip->name() << " context:" << snip->contexts().join(",") << endl;
		s << snip->pattern();
	}
}

void QSnippetManager::loadSnippetsFromDirectory(const QString& path)
{
	QDir d(path);
	
	QFileInfoList l = d.entryInfoList(QDir::Files | QDir::Readable);
	
	foreach ( const QFileInfo& info, l )
	{
		if ( info.suffix() != "qcs" )
			continue;
		
		// TODO : pattern selection?
		loadSnippetFromFile(info.absoluteFilePath(), "Simple");
	}
}

void QSnippetManager::addPatternLoader(QSnippetPatternLoader *pl)
{
	m_patternLoaders << pl;
}

void QSnippetManager::removePatternLoader(QSnippetPatternLoader *pl)
{
	m_patternLoaders.removeAll(pl);
}

QString QSnippetManager::typeGuess(const QString& pattern) const
{
	Q_UNUSED(pattern)
	
	return "Simple";
}
