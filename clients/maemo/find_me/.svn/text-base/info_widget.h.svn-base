#ifndef __FIND_ME_INFO_WIDGET_H
#define __FIND_ME_INFO_WIDGET_H

#include <QFrame>
#include <QPushButton>
#include <QLabel>

namespace FindMe
{
   class InfoWidget : public QFrame
   {
      Q_OBJECT
      Q_DISABLE_COPY(InfoWidget)

      public:
         InfoWidget(QWidget * parent = 0);

      signals:
         void okClicked();
         void cancelClicked();

      private:
         QPushButton m_btn_ok;
         QPushButton m_btn_cancel;
         QLabel m_lbl_text;
   };
}

#endif // __FIND_ME_INFO_WIDGET_H
