#ifndef TERMINALHIGHLIGHTER_H
#define TERMINALHIGHLIGHTER_H

#include <QSyntaxHighlighter>
class QTextDocument;
class TerminalHighlighter : public QSyntaxHighlighter {
    Q_OBJECT

public:
    TerminalHighlighter(QTextDocument *parent = 0);

protected:
    void highlightBlock(const QString &text);

private:
    struct HighlightingRule {
        QRegExp pattern;
        QTextCharFormat format;
    };
    QVector<HighlightingRule> highlightingRules;

    QTextCharFormat keywordFormat;
    QTextCharFormat quotationFormat;
    QTextCharFormat numberFormat;
    QTextCharFormat urlFormat;
};

#endif // TERMINALHIGHLIGHTER_H
