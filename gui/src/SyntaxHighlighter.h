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

#ifndef __SYNTAX_H__
#define __SYNTAX_H__

#include <QSyntaxHighlighter>
#include <QTextBlockUserData>
#include <QVector>
#include <QPlainTextEdit>

class BlockData:public QTextBlockUserData
{
	public:
	BlockData();
	
	struct Bracket
	{
		int type;	//Type of bracket
		int pos;	//Position of bracket
		int length;	//Number of chars of bracket
		bool startBracketOk;	//Is it a start or end bracket?
	};
	
	QVector <Bracket> brackets;
};

class SyntaxHighlighter:public QSyntaxHighlighter
{
	Q_OBJECT
	
	struct HighlightingRule
	{
		QRegExp pattern;
		QTextCharFormat format;
		int ruleOrder;
		int lastFound;
	};
	
	QVector<HighlightingRule> highlightingRules;
	
	struct HighlightingBlockRule
	{
		QRegExp startPattern, endPattern;
		QTextCharFormat format;
		int ruleOrder;
	};
	
	QVector<HighlightingBlockRule> highlightingBlockRules;
	QVector<HighlightingBlockRule> highlightingBracketsRules;
	
	struct Rule1st
	{
		int rule;
		int startIndex;
		int length;
		int ruleOrder;
	};
	
	/**1st rule to apply from startIndex.
	 */
	Rule1st highlight1stRule(const QString & text, int startIndex);
	
	/**1st block rule to apply from startIndex.
	 */
	Rule1st highlight1stBlockRule(const QString & text, int startIndex);
	
	/** Set format using rule.
	 */
	int ruleSetFormat(Rule1st rule);
	
	/** Set format using block rule.
	 */
	int blockRuleSetFormat(const QString & text, Rule1st rule1st);
	
	/** Finds brackets and put them in BlockData.
	 */
	void findBrackets(const QString & text, int start, int end, BlockData *blockData);
	
	public:
	
        SyntaxHighlighter(QObject * parent = 0);
	bool load(QString file);
	
	/**Formats pair of brackets
	 */
	void setFormatPairBrackets(QPlainTextEdit *textEdit);
	
	protected:
	void highlightBlock ( const QString & text );
};
#endif
