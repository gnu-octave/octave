#include "WelcomeWizard.h"
#include "ui_WelcomeWizard.h"

WelcomeWizard::WelcomeWizard (QWidget *parent) :
  QDialog (parent),
  ui (new Ui::WelcomeWizard)
{
  ui->setupUi (this);
  connect (ui->nextButton1, SIGNAL (clicked ()), this, SLOT (next ()));
  connect (ui->nextButton2, SIGNAL (clicked ()), this, SLOT (next ()));
  connect (ui->nextButton3, SIGNAL (clicked ()), this, SLOT (next ()));
  connect (ui->nextButton4, SIGNAL (clicked ()), this, SLOT (next ()));

  connect (ui->previousButton2, SIGNAL (clicked ()), this, SLOT (previous ()));
  connect (ui->previousButton3, SIGNAL (clicked ()), this, SLOT (previous ()));
  connect (ui->previousButton4, SIGNAL (clicked ()), this, SLOT (previous ()));
  connect (ui->previousButton5, SIGNAL (clicked ()), this, SLOT (previous ()));
}

WelcomeWizard::~WelcomeWizard()
{
  delete ui;
}

void
WelcomeWizard::next ()
{
  ui->stackedWidget->setCurrentIndex (ui->stackedWidget->currentIndex () + 1);
}

void
WelcomeWizard::previous ()
{
  ui->stackedWidget->setCurrentIndex (ui->stackedWidget->currentIndex () - 1);
}

