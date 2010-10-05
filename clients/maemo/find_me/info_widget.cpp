#include "info_widget.h"
#include <QHBoxLayout>

namespace FindMe
{
   InfoWidget::InfoWidget(QWidget * parent) :
      QFrame(parent),
      m_btn_ok("OK"),
      m_btn_cancel("Cancel"),
      m_lbl_text("Request new ID?")
   {
      this->setObjectName("Panel");
      m_btn_ok.setObjectName("OK");
      m_btn_cancel.setObjectName("Cancel");

      QHBoxLayout * layout = new QHBoxLayout;
      layout->addWidget(&m_lbl_text, 8);
      layout->addWidget(&m_btn_ok, 1);
      layout->addWidget(&m_btn_cancel, 1);
      setLayout(layout);

      connect(&m_btn_ok, SIGNAL(clicked()), this, SIGNAL(okClicked()));
      connect(&m_btn_cancel, SIGNAL(clicked()), this, SIGNAL(cancelClicked()));
   }
}
