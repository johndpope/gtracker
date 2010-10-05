#include "client.h"
#include "../log.h"

namespace GTracker
{
   namespace
   {
      const qint64 max_size = 1024 * 1024; // 1 MB max size
      const int heartbeat_interval = 60000; // every minute
   }

   Client::Client(QString userId, QObject * parent) :
      QObject(parent),
      m_user_id(userId),
      m_state(Client::Disconnected)
   {
      connect(&m_socket, SIGNAL(connected()), this, SLOT(authenticate()));
      connect(&m_socket, SIGNAL(readyRead()), this, SLOT(socketReadyRead()));
      connect(&m_socket, SIGNAL(stateChanged(QAbstractSocket::SocketState)),
            this, SLOT(socketStateChanged(QAbstractSocket::SocketState)));
      connect(&m_socket, SIGNAL(error(QAbstractSocket::SocketError)),
            this, SLOT(socketError(QAbstractSocket::SocketError)));
   }

   Client::~Client()
   {
   }

   QString Client::userId() const
   {
      return m_user_id;
   }

   Client::State Client::state() const
   {
      return m_state;
   }

   void Client::connectToHost(QString hostName, quint16 port)
   {
      m_socket.connectToHost(hostName, port);
   }

   void Client::disconnectFromHost()
   {
      m_socket.disconnectFromHost();
   }

   void Client::updatePosition(double lat, double lon, QDateTime timestamp)
   {
      DEBUG("Client| updatePosition = { lat: %f, lon: %f, timestamp: %s }", lat, lon,
            timestamp.toString().toAscii().data());

      static const double exp = 1000000.0;

      double _lat = qAbs(lat);
      double _lon = qAbs(lon);

      CoordinateMessage msg;
      msg.type       = Coordinate;
      msg.lat        = int16_t(lat);
      msg.lat_e      = uint32_t((_lat - int(_lat)) * exp);
      msg.lon        = int16_t(lon);
      msg.lon_e      = uint32_t((_lon - int(_lon)) * exp);
      msg.timestamp  = uint32_t(timestamp.toTime_t());

      sendMessage(sizeof(msg), &msg);
   }

   void Client::resetId()
   {
      DEBUG("Client| resetId");
      m_user_id.clear();
      emit userIdChanged(m_user_id);
   }

   void Client::socketStateChanged(QAbstractSocket::SocketState state)
   {
      DEBUG("Client| socketStateChanged = { state: %d }", state);

      switch (state)
      {
         case QAbstractSocket::UnconnectedState:
            setState(Client::Disconnected);
            break;

         case QAbstractSocket::HostLookupState:
         case QAbstractSocket::ConnectingState:
            setState(Client::Connecting);
            break;

         case QAbstractSocket::ConnectedState:
            setState(Client::Authentication);
            break;

         default:
            break;
      }
   }

   void Client::authenticate()
   {
      DEBUG("Client| authenticate");

      AuthenticateMessage msg;
      msg.type = Authenticate;
      msg.version = ProtocolVersion;
      if (m_user_id.size() != int(IDSize))
      {
         sendMessage(sizeof(msg) - IDSize, &msg);
      }
      else
      {
         memcpy(msg.id, m_user_id.toLocal8Bit().data(), IDSize);
         sendMessage(sizeof(msg), &msg);
      }
   }

   void Client::socketReadyRead()
   {
      static const int LengthSize = sizeof(Length);
      m_buffer.append(m_socket.readAll());

      while (m_buffer.size() > LengthSize)
      {
         const Length & length = *reinterpret_cast<Length *>(m_buffer.data());
         if (m_buffer.size() < int(length + LengthSize))
            break;

         const Type & type = *reinterpret_cast<Type *>(m_buffer.data() + LengthSize);

         switch (type)
         {
            case AuthenticateResponse:
               authenticateResponseMessage(*reinterpret_cast<AuthenticateResponseMessage *>(m_buffer.data() +
                        LengthSize));
               break;

            case Error:
               errorMessage(*reinterpret_cast<ErrorMessage *>(m_buffer.data() + LengthSize));
               break;

            default:
               // unknown or incorrect message
               break;
         }

         m_buffer.remove(0, length + sizeof(Length));
      }
   }

   void Client::socketError(QAbstractSocket::SocketError error)
   {
      WARNING("Client| socketError = { error: %d }, errorString = { %s }", error, m_socket.errorString().toAscii().data());
   }

   void Client::setState(Client::State state)
   {
      if (m_state != state)
      {
         DEBUG("Client| stateChanged = { state: %d }, previus state = { %d }", state, m_state);
         m_state = state;
         emit stateChanged(m_state);
      }
   }

   void Client::sendMessage(Length length, const void * data)
   {
      DEBUG("Client| sendMessage = { length = %d }", length);
      if (m_socket.state() == QAbstractSocket::ConnectedState)
      {
         DEBUG("Client|    to Socket");

         m_socket.write(reinterpret_cast<const char *>(&length), sizeof(length));
         m_socket.write(reinterpret_cast<const char *>(data), length);
      }
      else
      {
         DEBUG("Client|    to Temporary File");

         if (!m_tmp.isOpen())
            m_tmp.open();

         if (m_tmp.size() >= max_size)
            m_tmp.resize(0); // clear buffer if size exceeded max_size

         m_tmp.write(reinterpret_cast<const char *>(&length), sizeof(length));
         m_tmp.write(reinterpret_cast<const char *>(data), length);
      }
   }

   void Client::authenticateResponseMessage(const AuthenticateResponseMessage & msg)
   {
      m_user_id = QString::fromLocal8Bit((const char *) msg.id, IDSize);

      DEBUG("Client| authenticateResponseMessage = { id: %s }", m_user_id.toAscii().data());

      setState(Client::Connected);
      emit userIdChanged(m_user_id);

      // write all data from m_tmp
      if (m_tmp.size() > 0)
      {
         DEBUG("Client|    sending stored data = { size: %d }", int(m_tmp.size()));

         m_tmp.seek(0);
         m_socket.write(m_tmp.readAll());
         m_tmp.resize(0);
      }
   }

   void Client::errorMessage(const ErrorMessage & msg)
   {
      DEBUG("Client| errorMessage = { id: %d }, text = { %s }", msg.code, errorString(msg.code));
      emit error(msg.code);
   }

   const char * Client::errorString(int code)
   {
      switch (code)
      {
         case ERROR_FUCK_OFF:             return "You are banned";
         case ERROR_WRONG_MSG:            return "Invalid message";
         case ERROR_NOT_AUTH:             return "Not authenticated";
         case ERROR_SERVER_UNAVAILABLE:   return "Server not available";
         case ERROR_WRONG_CID:            return "Invalid ID";
         case ERROR_ALREADY_AUTH:         return "Already authenticated";

         default:
            break;
      }

      return "Unknown error";
   }
}
