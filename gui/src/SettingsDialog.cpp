#include "ResourceManager.h"
#include "SettingsDialog.h"
#include "ui_SettingsDialog.h"
#include <QSettings>

SettingsDialog::SettingsDialog (QWidget * parent):
QDialog (parent), ui (new Ui::SettingsDialog)
{
  ui->setupUi (this);

  QSettings *settings = ResourceManager::instance ()->settings ();
  ui->connectOnStartup->setChecked (settings->value ("connectOnStartup").toBool ());
  ui->showMessageOfTheDay->setChecked (settings->value ("showMessageOfTheDay").toBool ());
  ui->showTopic->setChecked (settings->value ("showTopic").toBool ());
  ui->autoIdentification->setChecked (settings->value ("autoIdentification").toBool ());
  ui->nickServPassword->setText (settings->value ("nickServPassword").toString ());
  ui->useCustomFileEditor->setChecked (settings->value ("useCustomFileEditor").toBool ());
  ui->customFileEditor->setText (settings->value ("customFileEditor").toString ());
  ui->showFilenames->setChecked (settings->value ("showFilenames").toBool());
  ui->showFileSize->setChecked (settings->value ("showFileSize").toBool());
  ui->showFileType->setChecked (settings->value ("showFileType").toBool());
  ui->showLastModified->setChecked (settings->value ("showLastModified").toBool());
  ui->showHiddenFiles->setChecked (settings->value ("showHiddenFiles").toBool());
  ui->useAlternatingRowColors->setChecked (settings->value ("useAlternatingRowColors").toBool());
}

SettingsDialog::~SettingsDialog ()
{
  QSettings *settings = ResourceManager::instance ()->settings ();
  settings->setValue ("connectOnStartup", ui->connectOnStartup->isChecked ());
  settings->setValue ("showMessageOfTheDay", ui->showMessageOfTheDay->isChecked ());
  settings->setValue ("showTopic", ui->showTopic->isChecked ());
  settings->setValue ("autoIdentification", ui->autoIdentification->isChecked ());
  settings->setValue ("nickServPassword", ui->nickServPassword->text ());
  settings->setValue ("useCustomFileEditor", ui->useCustomFileEditor->isChecked ());
  settings->setValue ("customFileEditor", ui->customFileEditor->text ());
  settings->setValue ("showFilenames", ui->showFilenames->isChecked ());
  settings->setValue ("showFileSize", ui->showFileSize->isChecked ());
  settings->setValue ("showFileType", ui->showFileType->isChecked ());
  settings->setValue ("showLastModified", ui->showLastModified->isChecked ());
  settings->setValue ("showHiddenFiles", ui->showHiddenFiles->isChecked ());
  settings->setValue ("useAlternatingRowColors", ui->useAlternatingRowColors->isChecked ());
  delete ui;
}
