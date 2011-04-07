/****************************************************************************
** Meta object code from reading C++ file 'TerminalDisplay.h'
**
** Created: Thu Apr 7 10:08:45 2011
**      by: The Qt Meta Object Compiler version 62 (Qt 4.7.2)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../lib/TerminalDisplay.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'TerminalDisplay.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.7.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Konsole__TerminalDisplay[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
      24,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       8,       // signalCount

 // signals: signature, parameters, type, tag, flags
      28,   26,   25,   25, 0x05,
      65,   57,   25,   25, 0x05,
     122,   93,   25,   25, 0x05,
     164,  151,   25,   25, 0x05,
     197,  151,   25,   25, 0x05,
     247,  231,   25,   25, 0x05,
     293,   25,   25,   25, 0x05,
     315,   25,   25,   25, 0x05,

 // slots: signature, parameters, type, tag, flags
     344,   25,   25,   25, 0x0a,
     358,   25,   25,   25, 0x0a,
     381,   25,   25,   25, 0x0a,
     397,   25,   25,   25, 0x0a,
     414,   25,   25,   25, 0x0a,
     439,  431,   25,   25, 0x0a,
     484,  474,   25,   25, 0x0a,
     516,  506,   25,   25, 0x0a,
     540,   25,  535,   25, 0x0a,
     560,  552,   25,   25, 0x0a,
     580,  574,   25,   25, 0x09,
     610,   25,   25,   25, 0x09,
     623,   25,   25,   25, 0x09,
     642,   25,   25,   25, 0x09,
     655,   25,   25,   25, 0x08,
     672,   25,   25,   25, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_Konsole__TerminalDisplay[] = {
    "Konsole::TerminalDisplay\0\0e\0"
    "keyPressedSignal(QKeyEvent*)\0suspend\0"
    "flowControlKeyPressed(bool)\0"
    "button,column,line,eventType\0"
    "mouseSignal(int,int,int,int)\0height,width\0"
    "changedFontMetricSignal(int,int)\0"
    "changedContentSizeSignal(int,int)\0"
    ",state,position\0"
    "configureRequest(TerminalDisplay*,int,QPoint)\0"
    "isBusySelecting(bool)\0"
    "sendStringToEmu(const char*)\0updateImage()\0"
    "updateLineProperties()\0copyClipboard()\0"
    "pasteClipboard()\0pasteSelection()\0"
    "enabled\0setFlowControlWarningEnabled(bool)\0"
    "suspended\0outputSuspended(bool)\0"
    "usesMouse\0setUsesMouse(bool)\0bool\0"
    "usesMouse()\0message\0bell(QString)\0"
    "value\0scrollBarPositionChanged(int)\0"
    "blinkEvent()\0blinkCursorEvent()\0"
    "enableBell()\0swapColorTable()\0"
    "tripleClickTimeout()\0"
};

const QMetaObject Konsole::TerminalDisplay::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_Konsole__TerminalDisplay,
      qt_meta_data_Konsole__TerminalDisplay, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &Konsole::TerminalDisplay::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *Konsole::TerminalDisplay::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *Konsole::TerminalDisplay::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Konsole__TerminalDisplay))
        return static_cast<void*>(const_cast< TerminalDisplay*>(this));
    return QWidget::qt_metacast(_clname);
}

int Konsole::TerminalDisplay::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: keyPressedSignal((*reinterpret_cast< QKeyEvent*(*)>(_a[1]))); break;
        case 1: flowControlKeyPressed((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 2: mouseSignal((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2])),(*reinterpret_cast< int(*)>(_a[3])),(*reinterpret_cast< int(*)>(_a[4]))); break;
        case 3: changedFontMetricSignal((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 4: changedContentSizeSignal((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 5: configureRequest((*reinterpret_cast< TerminalDisplay*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2])),(*reinterpret_cast< const QPoint(*)>(_a[3]))); break;
        case 6: isBusySelecting((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 7: sendStringToEmu((*reinterpret_cast< const char*(*)>(_a[1]))); break;
        case 8: updateImage(); break;
        case 9: updateLineProperties(); break;
        case 10: copyClipboard(); break;
        case 11: pasteClipboard(); break;
        case 12: pasteSelection(); break;
        case 13: setFlowControlWarningEnabled((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 14: outputSuspended((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 15: setUsesMouse((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 16: { bool _r = usesMouse();
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 17: bell((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 18: scrollBarPositionChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 19: blinkEvent(); break;
        case 20: blinkCursorEvent(); break;
        case 21: enableBell(); break;
        case 22: swapColorTable(); break;
        case 23: tripleClickTimeout(); break;
        default: ;
        }
        _id -= 24;
    }
    return _id;
}

// SIGNAL 0
void Konsole::TerminalDisplay::keyPressedSignal(QKeyEvent * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void Konsole::TerminalDisplay::flowControlKeyPressed(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void Konsole::TerminalDisplay::mouseSignal(int _t1, int _t2, int _t3, int _t4)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)), const_cast<void*>(reinterpret_cast<const void*>(&_t4)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void Konsole::TerminalDisplay::changedFontMetricSignal(int _t1, int _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}

// SIGNAL 4
void Konsole::TerminalDisplay::changedContentSizeSignal(int _t1, int _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 4, _a);
}

// SIGNAL 5
void Konsole::TerminalDisplay::configureRequest(TerminalDisplay * _t1, int _t2, const QPoint & _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 5, _a);
}

// SIGNAL 6
void Konsole::TerminalDisplay::isBusySelecting(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 6, _a);
}

// SIGNAL 7
void Konsole::TerminalDisplay::sendStringToEmu(const char * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 7, _a);
}
QT_END_MOC_NAMESPACE
