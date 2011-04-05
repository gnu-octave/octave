#include "terminalhighlighter.h"
#include <QtGui>

TerminalHighlighter::TerminalHighlighter(QTextDocument *parent)
    : QSyntaxHighlighter(parent)
{
    HighlightingRule rule;

    keywordFormat.setForeground(Qt::darkRed);
    keywordFormat.setFontWeight(QFont::Bold);
    QStringList keywordPatterns;
    keywordPatterns << "\\bOctave\\b" << "\\bGNU\\b";

    foreach (const QString &pattern, keywordPatterns) {
        rule.pattern = QRegExp(pattern);
        rule.format = keywordFormat;
        highlightingRules.append(rule);
    }

    numberFormat.setForeground(Qt::darkGreen);
    numberFormat.setFontWeight(QFont::Bold);
    rule.pattern = QRegExp("\\b[0-9\\.]+\\b");
    rule.format = numberFormat;
    highlightingRules.append(rule);

    urlFormat.setForeground(Qt::darkBlue);
    rule.pattern = QRegExp("\\bhttp://[^\\s]+\\b");
    rule.format = urlFormat;
    highlightingRules.append(rule);

    quotationFormat.setForeground(Qt::darkGreen);
    rule.pattern = QRegExp("\".*\"");
    rule.format = quotationFormat;
    highlightingRules.append(rule);
}

void TerminalHighlighter::highlightBlock(const QString &text)
{
    foreach (const HighlightingRule &rule, highlightingRules) {
        QRegExp expression(rule.pattern);
        int index = expression.indexIn(text);
        while (index >= 0) {
            int length = expression.matchedLength();
            setFormat(index, length, rule.format);
            index = expression.indexIn(text, index + length);
        }
    }
}
