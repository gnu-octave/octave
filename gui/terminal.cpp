#include "terminal.h"

Terminal::Terminal()
    : QMdiSubWindow() {
    setWidget(new QTermWidget(1, this));
}
