/* Copyright (C) 2006 P.L. Lucas
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
#include <QPlainTextEdit>
#include <QSyntaxHighlighter>
#include <QVector>
#include <QTextCharFormat>
#include <QMap>
#include "config.h"

/**SyntaxHighlighter for Octave code.*/
class SyntaxHighlighter: public QSyntaxHighlighter
{
 Q_OBJECT
 public:
  SyntaxHighlighter(QTextDocument *parent);
  ~SyntaxHighlighter();
  void highlightBlock(const QString &str);
  void load(const QString &file);

  //void setItem(const QString &item, const QString &type);
  void setItem(const QString &item, const QString &type, const QString parent=QString() );
  void setComment(const QString &start, const QString &end, const QString &type);
  void setType(const QString &type, const QTextCharFormat &format);
  
  /**Stops syntax highlight*/
  void setActive(bool active);
  
  static QStringList octave_comands;
  
 public slots:
 /**Return true or false if brackets are been macthed*/
 inline bool getIsActiveBraketsMacth() {return braketsMacth_ok;}
 
  
 private:
  struct Rule
  {
    QRegExp pattern;
    QString type;
    QTextCharFormat format;
    QList<Rule*> rules;
  };
  
  int backward_search(QTextBlock & block, int pos, char bracket_start, char bracket_end);
  int forward_search(QTextBlock & block, int pos, char bracket_start, char bracket_end);

  //static QMap<QString, QList<Rule> > instances;
  
  QMap<QString, Rule *> rules_map;

  static QList<Rule*> rules;
  QMap<QString, QTextCharFormat> _format;
  
  //Next two properties are used inside highlightBlock method
  QVector<int> __i_aux; //Auxiliar positions
  QVector<QRegExp> __re; //Regular expresions
  
  bool active_ok;
  bool braketsMacth_ok;
};

#endif
