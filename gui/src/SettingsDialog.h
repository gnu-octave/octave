#ifndef SETTINGSDIALOG_H
#define SETTINGSDIALOG_H

#include <QDialog>

namespace Ui {
    class SettingsDialog;
}

class SettingsDialog : public QDialog
{
    Q_OBJECT

public:
    explicit SettingsDialog(QWidget *parent, QString settingsFile);
    ~SettingsDialog();

private:
    Ui::SettingsDialog *ui;
    QString m_settingsFile;
};

#endif // SETTINGSDIALOG_H
