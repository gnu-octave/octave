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
  ui->useProxyServer->setChecked (settings->value ("useProxyServer").toBool ());
  ui->proxyHostName->setText (settings->value ("proxyHostName").toString ());

  int currentIndex = 0;
  QString proxyTypeString = settings->value ("proxyType").toString ();
  while ( (currentIndex < ui->proxyType->count ()) && (ui->proxyType->currentText () != proxyTypeString))
    {
      currentIndex++;
      ui->proxyType->setCurrentIndex (currentIndex);
    }

  ui->proxyPort->setText (settings->value ("proxyPort").toString ());
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
  settings->setValue ("useProxyServer", ui->useProxyServer->isChecked ());
  settings->setValue ("proxyType", ui->proxyType->currentText ());
  settings->setValue ("proxyHostName", ui->proxyHostName->text ());
  settings->setValue ("proxyPort", ui->proxyPort->text ());
  delete ui;
}
