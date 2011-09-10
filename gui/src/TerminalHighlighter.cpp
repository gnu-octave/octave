/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid
 * jacob.dawid@googlemail.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "ResourceManager.h"
#include "TerminalHighlighter.h"

TerminalHighlighter::TerminalHighlighter(QTextDocument *parent)
  : QSyntaxHighlighter(parent)
{
  HighlightingRule rule;

  keywordFormat.setForeground(Qt::darkBlue);
  QStringList keywordPatterns
      = QString(ResourceManager::instance ()->octaveKeywords ()).split(" ", QString::SkipEmptyParts);
  keywordPatterns << "GNU" << "Octave" << "OctaveGUI";

  for (int i = 0; i < keywordPatterns.size (); i++)
    keywordPatterns.replace(i, QString("\\b%1\\b").arg(keywordPatterns.at (i)));

  foreach (const QString &pattern, keywordPatterns)
    {
      rule.pattern = QRegExp(pattern);
      rule.format = keywordFormat;
      highlightingRules.append(rule);
    }

  numberFormat.setForeground(Qt::darkRed);
  rule.pattern = QRegExp("\\b[0-9\\.\\+\\-\\^]+\\b");
  rule.format = numberFormat;
  highlightingRules.append(rule);

  doubleQouteFormat.setForeground(Qt::darkGreen);
  rule.pattern = QRegExp("\"[^\"]*\"");
  rule.format = doubleQouteFormat;
  highlightingRules.append(rule);

  functionFormat.setFontItalic(true);
  functionFormat.setForeground(Qt::blue);
  rule.pattern = QRegExp("\\b[A-Za-z0-9_]+\\s*(?=\\()");
  rule.format = functionFormat;
  highlightingRules.append(rule);

  urlFormat.setForeground(Qt::darkYellow);
  rule.pattern = QRegExp("((?:https?|ftp)://\\S+)");
  rule.format = urlFormat;
  highlightingRules.append(rule);

  subCaptionFormat.setForeground (Qt::black);
  subCaptionFormat.setFontItalic (true);
  rule.pattern = QRegExp("^\\s+\\*.+$");
  rule.format = subCaptionFormat;
  highlightingRules.append(rule);

  captionFormat.setForeground(Qt::black);
  captionFormat.setFontWeight(QFont::Bold);
  rule.pattern = QRegExp("^\\s+\\*\\*.+$");
  rule.format = captionFormat;
  highlightingRules.append(rule);

}

void TerminalHighlighter::highlightBlock(const QString &text)
{
  foreach (const HighlightingRule &rule, highlightingRules)
    {
      QRegExp expression(rule.pattern);
      int index = expression.indexIn(text);
      while (index >= 0)
        {
          int length = expression.matchedLength();
          setFormat(index, length, rule.format);
          index = expression.indexIn(text, index + length);
        }
    }
}
