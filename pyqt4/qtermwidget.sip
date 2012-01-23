%Module QTermWidget 0

%Import QtCore/QtCoremod.sip
%Import QtGui/QtGuimod.sip


class QTermWidget : QWidget {

%TypeHeaderCode
#include <../lib/qtermwidget.h>
%End

public:
	QTermWidget(int startnow = 1, QWidget *parent = 0);
	~QTermWidget();
	enum ScrollBarPosition
    {
        NoScrollBar=0,
        ScrollBarLeft=1,
        ScrollBarRight=2
    };
	void setTerminalFont(QFont &font);
	void setArgs(QStringList &args);
	void setTextCodec(QTextCodec *codec);
	void setColorScheme(int scheme);
	void setSize(int h, int v);
	void setHistorySize(int lines);
	void setScrollBarPosition(ScrollBarPosition);
	void sendText(QString &text);
protected:
	void resizeEvent(QResizeEvent *e);
private:
	void *createTermWidget(int startnow, void *parent); 
	
};
