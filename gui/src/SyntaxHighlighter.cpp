/* Copyright (C) 2006-2008 P.L. Lucas
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
#include <iostream>
#include <QtXml/QXmlSimpleReader>
#include <QtXml/QXmlDefaultHandler>
#include <QTextBlockUserData>
#include <QHash>
#include <QDir>

QList<SyntaxHighlighter::Rule*> SyntaxHighlighter::rules;
QStringList SyntaxHighlighter::octave_comands;

/*** Xml Handler ***/
class SyntaxXmlHandler:public QXmlDefaultHandler {
private:
    SyntaxHighlighter *syntax;
    QString type_name, text;
    struct Tag
    {
        QString tag, type;
        QStringList items;
        QList<Tag> childs;
    };
    QList<Tag> stack;
    QStringList *octave_comands;

public:
    // Constructor
    SyntaxXmlHandler(SyntaxHighlighter *s, QStringList *octave_comands)
        : QXmlDefaultHandler(), syntax(s) {
            this->octave_comands=octave_comands;
    }

    bool startElement(const QString &/*namespaceURI*/, const QString &/*localName*/,
                    const QString &qname, const QXmlAttributes &atts) {
        Tag tag;
        tag.tag=qname;

        if(qname == "list")
        {// List block. Get the type name.
            tag.type = atts.value("name").trimmed();
            if(tag.type=="functions")
            {
                    tag.items << (*octave_comands);
            }
        }
        //else if(qname == "item")
        //{// Item. Next string is an item.
        //}
        else if(qname == "comment")
        {// Comments.
            syntax->setComment(atts.value("start"), "$", atts.value("name"));
        }

        stack.append(tag);
        return true;
    }


    bool characters(const QString &ch) {
        text+=ch;
        return true;
    }

    bool endElement(const QString & /*namespaceURI*/,
                    const QString & /*localName*/,
                    const QString & qname ) {
        Tag tag;

        if(stack.isEmpty()) return true;

        tag=stack.last();
        stack.removeLast();

        if(tag.tag!=qname) {
            printf("Error reading XML syntax\n");
            return false;
        }

        if(qname == "list") {// List block. Get the type name.
            if(stack.last().tag=="list") {
                stack.last().childs.append(tag);
            } else {
                syntax->setItem(tag.items.join("|"), tag.type);
                for(int i=0;i<tag.childs.size();i++) {
                    syntax->setItem(tag.childs[i].items.join("|"), tag.childs[i].type,tag.type);
                }
            }
        } else if(qname == "item") {
            // Item. Next string is an item.
            if(! text.trimmed().isEmpty() )
                    stack.last().items << text.trimmed();
        }

        text="";
        return true;
    }
};

/*** Block data ***/
class BlockData:public QTextBlockUserData {
public:
    BlockData() {braket_start_pos=braket_end_pos=-1;}
    ~BlockData (){}
    int braket_start_pos, braket_end_pos;
    QHash<int,QString> bracket;
};

/*** Syntax ***/
SyntaxHighlighter::SyntaxHighlighter(QTextDocument *parent)
    : QSyntaxHighlighter(parent) {
    QTextCharFormat f;

    QFont text_edit_font;
    QString font_name="Monospace";//get_config("textEditFont");
    QString font_size="10";//get_config("textEditFontSize");
    if(font_name.isEmpty()) {
        font_name=text_edit_font.family();
    }

    if(font_size.isEmpty()) {
        font_size=QString::number(text_edit_font.pointSize());
    }

    text_edit_font.setFamily(font_name);
    text_edit_font.setPointSize(font_size.toInt());

    f.setFont(text_edit_font);
    f.setFontWeight(QFont::Bold);
    _format["keywords"] = f;
    _format["commands"] = f;
    f.setFontWeight(QFont::Normal);
    f.setForeground(Qt::darkGreen);
    _format["builtin"] = f;
    f.setForeground(Qt::blue);
    _format["functions"] = f;
    // operators
    f.setForeground(Qt::black);
    _format["variables"] = f;
    f.setForeground(Qt::darkMagenta);
    _format["numbers"] = f;
    f.setForeground(Qt::red);
    _format["strings"] = f;
    // delimiters
    f.setForeground(Qt::darkGray);
    _format["singleLine"] = f;
    //Brackets matched
    f.setForeground(Qt::black);
    //f.setFontWeight(QFont::Bold);
    f.setBackground(Qt::yellow);
    _format["bracket match"]=f;

    active_ok=true;

    braketsMacth_ok=false;

    //printf("Syntax Builded\n");

    //Add rules to vectors to help to highlightBlock method
    __re.clear();
    __i_aux.clear();
    for(int n=0;n<rules.size();n++) {
        __re.append(rules[n]->pattern);
        __i_aux.append(-1);
        //printf("%s %d %d\n", rules.at(n)->type.toLocal8Bit().data(), __re.size(), __i_aux[n]);
    }
}

SyntaxHighlighter::~SyntaxHighlighter() {
	//foreach(Rule *value, rules_map)
	//{
	//	delete value;
	//}

	//This line is added because sometimes Qt try rehighlight text at destroy
	setDocument(NULL);
}

void SyntaxHighlighter::load(const QString &path) {
    if(octave_comands.isEmpty()) {
        QString home=QDir::home().path()+"/.qtoctave/commands.txt";

        QFile file(home);

        if(file.open(QFile::ReadOnly)) {
            char buf[1024];

            while(file.readLine(buf, sizeof(buf))>=0) {
                octave_comands.append(QString(buf).trimmed());
            }
            file.close();
        }
    }

    //rules = &(instances[path]);
    if(rules.isEmpty()) {
        // Load from file
        FILE *fl;

        fl = fopen(path.toLocal8Bit().constData(), "rt");
        if(!fl) {
            std::cerr << "[Syntax::load] Can not load the syntax file" << std::endl;
            return;
        }

        QFile file(path);
        QXmlSimpleReader parser;
        QXmlInputSource source(&file);
        SyntaxXmlHandler handler(this, &octave_comands);

        file.open(fl, QIODevice::ReadOnly);

        parser.setContentHandler(&handler);
        parser.setErrorHandler(&handler);

        parser.parse(&source);

        file.close();

        fclose(fl);

        std::cout << "[Sytax::load] "
                << path.toLocal8Bit().constData()
                << " loaded"
                << std::endl;
    }
}

void SyntaxHighlighter::setItem(const QString &item, const QString &type, const QString parent) {
    Rule *r;
    if(!item.isEmpty()) {
        r=new Rule;
        r->pattern = QRegExp(item);
        r->type = type;
        r->format = _format[type];
        rules_map[type]=r;
        if(parent.isEmpty() || !rules_map.contains(parent))
            rules.push_back(r);
        else
            rules_map[parent]->rules.push_back(r);
    }
}

void SyntaxHighlighter::setComment(const QString &start, const QString &end, const QString &type) {
    Rule *r;
    if(!type.isEmpty()) {
        r=new Rule;
        r->pattern = QRegExp(/*QString("^") +*/ start + ".*" + end);
        r->type = type;
        r->format = _format[type];
        rules_map[type]=r;
        rules.push_back(r);
    }
}

void SyntaxHighlighter::setType(const QString &type, const QTextCharFormat &f) {
      _format[type] = f;
}

void SyntaxHighlighter::highlightBlock(const QString &str) {
    //Para aumentar el rendimiento se hace una tabla i_aux con la posición de lo
    //que ha encontrado cada expresión regular rules.at(n)->pattern.
    //Se aplicará el formato debido a la Rule que tenga la i_aux más pequeña
    if(!str.isEmpty() && !rules.isEmpty() && active_ok) {

    int i=0, len=0; //Actual position
    int n_min; //Minimal position

    BlockData *dat=(BlockData *)currentBlockUserData();
    if(dat!=NULL) {
            dat->bracket.clear();
    }

    for(int n=0;n<__re.size();n++) {
        //re[n]=rules.at(n)->pattern;
        __i_aux[n] = __re[n].indexIn( str, i);
        //printf("%s %d %d\n", rules.at(n)->type.toLocal8Bit().data(), n, __i_aux[n]);
    }

    while(i >= 0) {
            n_min=-1;
            for(int n=0;n<__re.size();n++)
            {
                    if(__i_aux[n]<0) continue;
                    if(__i_aux[n]<i ) __i_aux[n] = __re[n].indexIn( str, i);
                    //printf("%s n=%d i_aux=%d n_min=%d i=%d\n", rules.at(n)->type.toLocal8Bit().data(), n, i_aux[n], n_min, i);
                    if( n_min<0 || __i_aux[n_min]<0 || (__i_aux[n]>=0 && __i_aux[n]<__i_aux[n_min]) )
                    {
                            n_min=n;
                            if(__i_aux[n]==i) break;
                    }
            }
            //printf("n_min=%d elegido\n", n_min);
            if(n_min>=0) i=__i_aux[n_min];
            else break;
            if( i<0 ) break;
            len = __re[n_min].matchedLength();

            //QStringList list=re[n_min].capturedTexts ();
            //printf("\n");
            //for(int n=0;n<list.size();n++)
            //{
            //	printf("%d >%s<\n", n, list.at(n).toLocal8Bit().data() );
            //}
            //printf("Aplicando %s i=%d len=%d\n", rules.at(n_min)->type.toLocal8Bit().data(), i, len);
            if(len<1) break;
            //QTextCharFormat i_format=format(i);
            //if( !(i_format==strings) )

            if(rules.at(n_min)->rules.isEmpty())
            {
                    setFormat(i, len, rules.at(n_min)->format);

                    if( rules.at(n_min)->type=="delimiters" )
                    {
                            QString bracket_found=__re[n_min].cap();

                            if(dat==NULL)
                            {
                                    dat=new BlockData();
                                    setCurrentBlockUserData(dat);
                            }
                            dat->bracket[i]=bracket_found;

                            //Do brackets macth
                            if( braketsMacth_ok && dat != NULL )
                            {
                                    if(dat->braket_start_pos>=0)
                                            setFormat(dat->braket_start_pos, 1, _format["bracket match"]);
                                    if(dat->braket_end_pos>=0)
                                            setFormat(dat->braket_end_pos, 1, _format["bracket match"]);
                            }
                    }

            }
            else
            {
                    //Rules can contains another rules
                    QString text=str.mid(i,len);
                    //printf("text=%s\n", text.toLocal8Bit().data() );
                    bool format_ok=true;
                    for(int n=0;n<rules.at(n_min)->rules.size(); n++)
                    {
                            if(rules.at(n_min)->rules.at(n)->pattern.exactMatch(text))
                            {
                                    setFormat(i, len, rules.at(n_min)->rules.at(n)->format);
                                    format_ok=false;
                                    break;
                            }
                    }
                    if(format_ok) setFormat(i, len, rules.at(n_min)->format);
            }
            i+=len;

    }
}
}


int SyntaxHighlighter::forwardSearch(QTextBlock &textBlock, int position, char bracketStart, char bracketEnd) {
    int i=position,  open=0;

    while(textBlock.isValid()) {
        BlockData *dat=(BlockData *)textBlock.userData();
        if(dat!=NULL) {
            QList<int> positions=dat->bracket.keys();
            qSort(positions);
            for(int k=0;k<positions.size();k++) {
                int b_pos=positions[k];
                if(b_pos<i) continue;

                QChar ch=dat->bracket[b_pos].at(0);

                if(ch==bracketEnd) {
                        open--;
                        if(open==0) return b_pos;
                }
                else if(ch==bracketStart) open++;
            }
        }
        textBlock=textBlock.next();
        i=0;
    }
    return -1;
}

int SyntaxHighlighter::backwardSearch(QTextBlock & textBlock, int position, char bracketStart, char bracketEnd) {
    int i=position,  open=0;
    while(textBlock.isValid()) {
        BlockData *dat=(BlockData *)textBlock.userData();
        if(dat!=NULL) {
            QList<int> positions=dat->bracket.keys();
            qSort(positions);
            for(int k=positions.size()-1;k>=0;k--) {
                int b_pos=positions[k];
                if(b_pos>i) continue;

                QChar ch=dat->bracket[b_pos].at(0);

                if(ch==bracketStart) {
                    open--;
                    if(open==0) return b_pos;
                }
                else if(ch==bracketEnd) open++;
            }
        }

        textBlock=textBlock.previous();
        if(textBlock.isValid() && !textBlock.text().isEmpty()) i=textBlock.length()-1;
    }
    return -1;
}

static void set_block_data(QTextBlock & block0, QTextBlock & block1, int start, int end) {
    BlockData *udat=(BlockData *)block0.userData();
    if(udat==NULL) {
        udat=new BlockData();
        block0.setUserData(udat);
    }
    udat->braket_start_pos=start;

    if(block0==block1) {
        udat->braket_end_pos=end;
    } else {
        BlockData *udat=(BlockData *)block1.userData();
        if(udat==NULL) {
            udat=new BlockData();
            block1.setUserData(udat);
        }
        udat->braket_end_pos=end;
    }
}

static void clear_block_data(QTextDocument *doc, bool rehigh) {
    QTextBlock block=doc->findBlock(0);
    while(block.isValid()) {
        BlockData *udat=(BlockData *)block.userData();
        if(udat!=NULL && (udat->braket_end_pos!=-1 || udat->braket_start_pos!=-1)) {
            udat->braket_end_pos=-1; udat->braket_start_pos=-1;
            if(rehigh) {
                //QTextCursor cursor(doc);
                //cursor.setPosition(block.position());
                //cursor.setBlockFormat(block.blockFormat());
            }
        }
        block=block.next();
    }
}

void SyntaxHighlighter::setActive(bool active) {
    active_ok=active;
}
