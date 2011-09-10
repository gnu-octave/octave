#ifndef WELCOMEWIZARD_H
#define WELCOMEWIZARD_H

#include <QDialog>

namespace Ui {
class WelcomeWizard;
}

class WelcomeWizard : public QDialog
{
  Q_OBJECT

public:
  explicit WelcomeWizard(QWidget *parent = 0);
  ~WelcomeWizard();

public slots:
  void next ();
  void previous ();

private:
  Ui::WelcomeWizard *ui;
};

#endif // WELCOMEWIZARD_H
