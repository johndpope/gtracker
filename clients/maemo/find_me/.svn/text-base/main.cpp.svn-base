#include <QApplication>
#include <QFile>
#include "window.h"
#include "log.h"

namespace
{
   const QString Version            = "1.1.10";
   const QString ApplicationName    = "Find Me";
   const QString OrganizationName   = "GTracker";
}

int main(int argc, char * argv[])
{
   INFO("%s %s executed", ApplicationName.toAscii().data(), Version.toAscii().data());

   QApplication app(argc, argv);
   app.setApplicationVersion(Version);
   app.setApplicationName(ApplicationName);
   app.setOrganizationName(OrganizationName);

   QFile style(":style.qss");
   if (style.open(QIODevice::ReadOnly | QIODevice::Text))
      app.setStyleSheet(style.readAll());

   FindMe::Window window;
   window.show();

   return app.exec();
}
