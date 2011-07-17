#include "SettingsDialog.h"
#include "ui_SettingsDialog.h"
#include <QSettings>

SettingsDialog::SettingsDialog (QWidget * parent, QString settingsFile):
QDialog (parent), ui (new Ui::SettingsDialog)
{
  m_settingsFile = settingsFile;
  ui->setupUi (this);

  QSettings settings (m_settingsFile, QSettings::IniFormat);
  ui->connectOnStartup->setChecked (settings.value ("connectOnStartup").
				    toBool ());
  ui->showMessageOfTheDay->setChecked (settings.value ("showMessageOfTheDay").
				       toBool ());
  ui->showTopic->setChecked (settings.value ("showTopic").toBool ());
  ui->autoIdentification->setChecked (settings.value ("autoIdentification").
				      toBool ());
  ui->nickServPassword->setText (settings.value ("nickServPassword").
				 toString ());
}

SettingsDialog::~SettingsDialog ()
{
  QSettings settings (m_settingsFile, QSettings::IniFormat);
  settings.setValue ("connectOnStartup", ui->connectOnStartup->isChecked ());
  settings.setValue ("showMessageOfTheDay",
		     ui->showMessageOfTheDay->isChecked ());
  settings.setValue ("showTopic", ui->showTopic->isChecked ());
  settings.setValue ("autoIdentification",
		     ui->autoIdentification->isChecked ());
  settings.setValue ("nickServPassword", ui->nickServPassword->text ());
  delete ui;
}
