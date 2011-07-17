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


#include "SyntaxHighlighter.h"
#include <QXmlStreamReader>
#include <QStack>
#include <QFile>
#include <stdio.h>

SyntaxHighlighter::SyntaxHighlighter(QObject * parent):QSyntaxHighlighter(parent)
{
}


bool SyntaxHighlighter::load(QString file)
{
	QXmlStreamReader xml;
	QStack <QString> stack;
	QFile fileDevice(file);
	if (!fileDevice.open(QFile::ReadOnly | QFile::Text)) {
	         return false;
	}
	
	xml.setDevice(&fileDevice);
	
	QMap <QString,QString> values;
	
	QVector<QString> xmlMainItems;
	xmlMainItems << "item" << "block" << "bracket";
	
	int ruleOrder=0;
	
	while (!xml.atEnd())
	{
		QXmlStreamReader::TokenType tokenType=xml.readNext();
		switch(tokenType)
		{
			case QXmlStreamReader::StartElement:
				if(xml.name()!="syntax")
				{
					if( xmlMainItems.contains(xml.name().toString()) )
						stack.push(xml.name().toString());
					else
						values[xml.name().toString()]=xml.readElementText().trimmed();
				}
			break;
			case QXmlStreamReader::EndElement:
				if(stack.isEmpty()) break;
				QString name=stack.top();
				if(name==xml.name()) stack.pop();
				if(stack.isEmpty())
				{
					QTextCharFormat format;
					if(values.contains("bold") && values["bold"]=="true") format.setFontWeight(QFont::Bold);
					if(values.contains("underline") && values["underline"]=="true") format.setFontUnderline(true);
					if(values.contains("italic") && values["italic"]=="true") format.setFontItalic(true);
					if(values.contains("foreground")) format.setForeground(QBrush(QColor(values["foreground"])));
					if(values.contains("background")) format.setBackground(QBrush(QColor(values["background"])));
					if(name=="item")
					{
						HighlightingRule rule;
						rule.format=format;
						rule.pattern=QRegExp(values["pattern"]);
						rule.ruleOrder=ruleOrder++;
						highlightingRules.append(rule);
						values.clear();
					}
					else if(name=="block" || name=="bracket")
					{
						HighlightingBlockRule rule;
						rule.format=format;
						rule.startPattern=QRegExp(values["startPattern"]);
						rule.endPattern=QRegExp(values["endPattern"]);
						rule.ruleOrder=ruleOrder++;
						if(name=="block") highlightingBlockRules.append(rule);
						else highlightingBracketsRules.append(rule); //Bracket rule
						values.clear();
					}
				}
			break;
		}
	}
	if (xml.hasError())
	{
		// do error handling
		printf("Error %s: %ld:%ld %s\n", file.toLocal8Bit().data(), xml.lineNumber(), xml.columnNumber(), xml.errorString().toLocal8Bit().data() );
		return false;
	}
	
	return true;
}

SyntaxHighlighter::Rule1st SyntaxHighlighter::highlight1stRule(const QString & text, int startIndex)
{
	Rule1st rule1st;
	rule1st.startIndex=text.length();
	rule1st.rule=-1;
	
	for(int i=0; i<highlightingRules.size(); i++)
	{
		HighlightingRule *rule=&(highlightingRules[i]);
		
		QRegExp *expression = &(rule->pattern);
		int index = rule->lastFound;
		if(index>-1 && index<startIndex)
		{
			rule->lastFound = index = expression->indexIn(text, startIndex);
		}
		if ( index>-1 && index<rule1st.startIndex )
		{
			rule1st.startIndex=index;
			rule1st.length=expression->matchedLength();
			rule1st.rule=i;
			rule1st.ruleOrder=rule->ruleOrder;
		}
		
		if(index==startIndex) break;
	}
	
	if(rule1st.rule==-1) rule1st.startIndex=-1;
	
	return rule1st;
}

SyntaxHighlighter::Rule1st SyntaxHighlighter::highlight1stBlockRule(const QString & text, int startIndex)
{
	Rule1st rule1st;
	rule1st.startIndex=text.length();
	rule1st.rule=-1;
	
	for(int i=0; i<highlightingBlockRules.size(); i++)
	{
		HighlightingBlockRule rule=highlightingBlockRules[i];
		
		int index = rule.startPattern.indexIn(text, startIndex);
		
		if ( index>-1 && index<rule1st.startIndex )
		{
			rule1st.startIndex=index;
			rule1st.rule=i;
			rule1st.ruleOrder=rule.ruleOrder;
		}
		
		if(index==startIndex) break;
	}
	
	if(rule1st.rule==-1) rule1st.startIndex=-1;
	
	return rule1st;
}

/**Inserts brackets in position order in blockData->brackets
 */
static void insertInOrder(BlockData *blockData, BlockData::Bracket &bracket)
{
	if(blockData->brackets.isEmpty()) blockData->brackets.append(bracket);
	else
	{
		int j=0;
		
		for(;j<blockData->brackets.size();j++)
		{
			if(blockData->brackets[j].pos>bracket.pos)
			{
				blockData->brackets.insert(j,bracket);
				break;
			}
		}
		if(j>=blockData->brackets.size())
		{
			blockData->brackets.append(bracket);
		}
	}
}


void SyntaxHighlighter::findBrackets(const QString & text, int start, int end, BlockData *blockData)
{
	//blockData->brackets.clear();
	
	if( end<0 || end>text.length() ) end=text.length();
	
	if(start>end) return;
	
	for(int i=0; i<highlightingBracketsRules.size(); i++)
	{
		HighlightingBlockRule rule=highlightingBracketsRules[i];
		
		int startIndex=start;
		
		int index = rule.startPattern.indexIn(text, startIndex);
		
		while( index>-1 && index<end )
		{
			BlockData::Bracket bracket;
			bracket.pos=index;
			bracket.type=i;
			bracket.length=rule.startPattern.matchedLength();
			bracket.startBracketOk=true;
			
			startIndex=index+bracket.length;
			
			insertInOrder(blockData, bracket);
			index = rule.startPattern.indexIn(text, startIndex);
		}
		
		startIndex=start;
		
		index = rule.endPattern.indexIn(text, startIndex);
		
		
		
		while( index>-1 && index<end )
		{
			BlockData::Bracket bracket;
			bracket.pos=index;
			bracket.type=i;
			bracket.length=rule.endPattern.matchedLength();
			bracket.startBracketOk=false;
			insertInOrder(blockData, bracket);
			startIndex=index+bracket.length;
			index = rule.endPattern.indexIn(text, startIndex);
		}
	}
}


int SyntaxHighlighter::ruleSetFormat(Rule1st rule1st)
{
	HighlightingRule rule=highlightingRules[rule1st.rule];
	
	setFormat(rule1st.startIndex, rule1st.length, rule.format);
	
	return rule1st.startIndex + rule1st.length;
}


int SyntaxHighlighter::blockRuleSetFormat(const QString & text, Rule1st rule1st)
{
	HighlightingBlockRule rule=highlightingBlockRules[rule1st.rule];
		
	int endIndex = rule.endPattern.indexIn(text, rule1st.startIndex);
	int commentLength;
	if (endIndex == -1)
	{
	    setCurrentBlockState(rule1st.rule);
	    commentLength = text.length() - rule1st.startIndex;
	    setFormat(rule1st.startIndex, commentLength, rule.format);
	    return text.length();
	}
	else
	{
	    commentLength = endIndex - rule1st.startIndex
	                    + rule.endPattern.matchedLength();
	    setFormat(rule1st.startIndex, commentLength, rule.format);
	    
	    return endIndex+1;
	}
}


void SyntaxHighlighter::highlightBlock ( const QString & text )
{

	setCurrentBlockState(-1);
	
	int startIndex = 0;
	
	//Checks previous block state
	if (previousBlockState() >= 0)
	{
		Rule1st rule1st;
		rule1st.rule=previousBlockState();
		rule1st.startIndex=0;
		
		startIndex=blockRuleSetFormat(text,rule1st);
		
		//TODO: Posible fallo al establecer el estado del bloque
		
		if(startIndex==text.length()) return;
	}
	
	//Gets BlockData
	BlockData *blockData=new BlockData();
	
	//Finds first rule to apply. 

	Rule1st rule1st, blockRule1st;
	
	//Find initial matches
	for(int i=0; i<highlightingRules.size(); i++)
	{
		HighlightingRule *rule= &(highlightingRules[i]);
		QRegExp *expression = &(rule->pattern);
		int index = expression->indexIn(text, startIndex);
                rule->lastFound = index;
	}

	rule1st=highlight1stRule( text, startIndex);
	blockRule1st=highlight1stBlockRule( text, startIndex);

	while(rule1st.rule>=0 || blockRule1st.rule>=0)
	{
		if(rule1st.rule>=0 && blockRule1st.rule>=0)
		{
			if
				( 
					rule1st.startIndex<blockRule1st.startIndex
					|| 
					(
						rule1st.startIndex==blockRule1st.startIndex
						&&
						rule1st.ruleOrder<blockRule1st.ruleOrder
					)
				)
			{
				findBrackets(text, startIndex, rule1st.startIndex, blockData);
				startIndex=ruleSetFormat(rule1st);
				rule1st=highlight1stRule( text, startIndex);
			}
			else
			{
				findBrackets(text, startIndex, blockRule1st.startIndex, blockData);
				startIndex=blockRuleSetFormat(text,blockRule1st);
				blockRule1st=highlight1stBlockRule( text, startIndex);
			}
		}
		else if(rule1st.rule>=0)
		{
			findBrackets(text, startIndex, rule1st.startIndex, blockData);
			startIndex=ruleSetFormat(rule1st);
			rule1st=highlight1stRule( text, startIndex);
		}
		else
		{
			findBrackets(text, startIndex, blockRule1st.startIndex, blockData);
			startIndex=blockRuleSetFormat(text,blockRule1st);
			blockRule1st=highlight1stBlockRule( text, startIndex);
		}
	}
	
	findBrackets(text,startIndex, -1, blockData);
	
	setCurrentBlockUserData(blockData);
}

/**Search brackets in one QTextBlock.*/
static BlockData::Bracket *searchBracket(int i, int increment, int &bracketsCount, BlockData *blockData, BlockData::Bracket *bracket1)
{
	if(blockData==NULL) return NULL;
	
	if(i==-1) i=blockData->brackets.size()-1;
	
	for(; i>=0 && i<blockData->brackets.size(); i+=increment)
	{
		BlockData::Bracket *bracket=&(blockData->brackets[i]);
		if(bracket->type==bracket1->type)
		{
			if(bracket->startBracketOk!=bracket1->startBracketOk)
				bracketsCount--;
			else
				bracketsCount++;
			
			if(bracketsCount==0)
				return bracket;
		}
	}
	
	//printf("[searchBracket] bracketsCount=%d\n", bracketsCount);
	
	return NULL;
}

void SyntaxHighlighter::setFormatPairBrackets(QPlainTextEdit *textEdit)
{
	QList<QTextEdit::ExtraSelection> selections;
	
	textEdit->setExtraSelections(selections);
	
	QTextCursor cursor=textEdit->textCursor();
	QTextBlock block=cursor.block();
	BlockData *blockData=(BlockData *)block.userData();
	if(blockData==NULL) return;
	
	int pos=cursor.position()-block.position();
	
	BlockData::Bracket *bracket1;
	QTextBlock block_bracket1=block;
	
	int i=blockData->brackets.size()-1;
	for(; i>=0; i--)
	{
		BlockData::Bracket *bracket=&(blockData->brackets[i]);
		if(bracket->pos==pos)
		{
			bracket1=bracket;
			break;
		}
	}
	
	if(i<0) return;
	
	int increment=(bracket1->startBracketOk) ? +1:-1;
	int bracketsCount=0;
	//Looks in this block the other bracket
	BlockData::Bracket *bracket2=NULL;
	QTextBlock block_bracket2=block;
	
	bracket2=searchBracket( i, increment, bracketsCount, blockData, bracket1);
	
	{ //Search brackets in other blocks
		while( bracket2==NULL )
		{
			if(increment>0)
			{
				block_bracket2=block_bracket2.next();
				i=0;
			}
			else
			{
				block_bracket2=block_bracket2.previous();
				i=-1;
			}
			
			if(!block_bracket2.isValid()) break;
			
			blockData=(BlockData *)block_bracket2.userData();
			/*
			printf("[Syntax::setFormatPairBrackets] Interno brackets.size=%d\n", blockData->brackets.size());
			for(int x=0;x<blockData->brackets.size();x++)
			{
				BlockData::Bracket *bracket=&(blockData->brackets[x]);
				printf("[Syntax::setFormatPairBrackets] bracket.pos=%d bracket.type=%d bracket.len=%d bracket.start=%d\n", bracket->pos, bracket->type, bracket->length, (bracket->startBracketOk) );
			}
			*/
			
			bracket2=searchBracket( i, increment, bracketsCount, blockData, bracket1);
		}
		
		if(bracket2==NULL) return;
	}
	
	pos=cursor.position();
	
	QTextEdit::ExtraSelection selection1;
	
	cursor.setPosition(pos+bracket1->length, QTextCursor::KeepAnchor);
	selection1.cursor=cursor;
	selection1.format=highlightingBracketsRules[bracket1->type].format;
	
	pos=bracket2->pos+block_bracket2.position();
	QTextEdit::ExtraSelection selection2;
	cursor.setPosition(pos);
	cursor.setPosition(pos+bracket2->length, QTextCursor::KeepAnchor);
	selection2.cursor=cursor;
	selection2.format=highlightingBracketsRules[bracket2->type].format;
	
	selections.append(selection1); selections.append(selection2);
	
	textEdit->setExtraSelections(selections);
	
}



BlockData::BlockData():QTextBlockUserData()
{
}


