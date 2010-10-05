/*
 * Copyright (C) 04/20/2010 Dmitry S. Melnikov (dmitryme@gmail.com)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package com.gtracker;

import java.util.Vector;

public class GtClient implements Runnable, IClient
{

   public GtClient(Settings s, IStateCallback c) throws Exception
   {
      settings_ = s;
      state_ = c;
      conn_ = new Connection(this);
   }

   private void createLocator(Settings s) throws Exception
   {
      if (locator_ != null && started_)
      {
         locator_.stop();
         locator_ = null;
      }
      if (s.getGpsSource() == Settings.GPSSource.Internal)
      {
         try
         {
            locator_ = new InternalLocator(this);
         }
         catch(ClassNotFoundException ex)
         {
            state_.onState(IStateCallback.StateEnum.GpsDisconnected, ex.getMessage());
         }
         catch(Exception ex)
         {
            state_.onState(IStateCallback.StateEnum.GpsDisconnected, "AAAAA" /*ex.toString()*/);
         }
      }
      else
      {
         locator_ = new BluetoothLocator(this, s.getGpsDeviceName());
      }
      if (started_ && locator_ != null)
      {
         locator_.start();
      }
   }

   public void reconfigure(Settings s) throws Exception
   {
      if (started_ && 0 != settings_.getServerName().compareTo(s.getServerName())
              || (settings_.getServerPort() != s.getServerPort()))
      {
         conn_.close();
         conn_.open(
                 s.getServerName(), s.getServerPort(),
                 s.getBkpServerName(), s.getBkpServerPort(),
                 s.getDeviceID());
      }
      if (started_  && 0 != settings_.getDeviceID().compareTo(s.getDeviceID())
              && !s.getDeviceID().equals(""))
      {
         conn_.send(new AuthMessage(s.getDeviceID()));
      }
      settings_ = s;
      if (settings_.getDynamicSendPeriod())
      {
         sendPeriod_ = 0;
      }
      else
      {
         sendPeriod_ = settings_.getSendPeriod();
      }
   }

   public void start()
   {
      if (started_)
      {
         return;
      }
      try
      {
         started_ = true;
         working_thread_ = new Thread(this);
         working_thread_.start();
         createLocator(settings_);
         conn_.open(
                 settings_.getServerName(), settings_.getServerPort(),
                 settings_.getBkpServerName(), settings_.getBkpServerPort(),
                 settings_.getDeviceID());
      } catch (Exception ex)
      {
         stop();
         state_.onState(IStateCallback.StateEnum.ServerDisconnected, ex.toString());
      }
   }

   public void stop()
   {
      if (!started_)
      { // already in stopping process
         return;
      }
      try
      {
         conn_.close();
      }
      catch(Exception ex) {}
      try
      {
         if (locator_ != null)
         {
            locator_.stop();
         }
      }
      catch(Exception ex) {}
      synchronized(pendingMsgs_)
      {
         started_ = false;
         pendingMsgs_.notify();
      }
      state_.onState(IStateCallback.StateEnum.ServerDisconnected, "");
   }

   public void run()
   {
      long last_hb = System.currentTimeMillis();
      while (started_)
      {
         try
         {
            long now = System.currentTimeMillis();
            if (last_hb + 60000 <= now && readyToSend_)
            {
               conn_.send(new HeartBeatMessage());
               last_hb = now;
            }
            IMessage in = null;
            synchronized (pendingMsgs_)
            {
               if (pendingMsgs_.size() > 0)
               {
                  in = (IMessage) pendingMsgs_.firstElement();
                  pendingMsgs_.removeElement(pendingMsgs_.firstElement());
               }
               else
               {
                  pendingMsgs_.wait(60000);
               }
            }
            if (in != null)
            {
               in.process(this);
            }
         } catch (Exception ex)
         {
            started_ = false;
            state_.onState(IStateCallback.StateEnum.ServerDisconnected, ex.toString());
         }
      }
   }

   public void process(SpeedMessage msg)
   {
      if (settings_.getDynamicSendPeriod())
      {
         if (msg.getValue() >= 10) // > 36 km/h
         {
            sendPeriod_ = 1;
         }
         else if (msg.getValue() >= 5 && msg.getValue() < 10) // [18,26) km/h
         {
            sendPeriod_ = 3;
         }
         else if (msg.getValue() >= 1 && msg.getValue() < 5) // [3.6, 18) km/h
         {
            sendPeriod_ = 5;
         }
         else
         {
            sendPeriod_ = 60;
         }
      }
   }

   public void process(CoordinateMessage msg)
   {
      if (sendPeriod_ > 0 ||
              msg.getTimestamp() >= lastSendMsgTS_ + sendPeriod_ * 1000)
      {
         conn_.send(msg);
         lastSendMsgTS_ = msg.getTimestamp();
      }
   }

   public void process(ServerStateMessage msg)
   {
      if (msg.getState() == IStateCallback.StateEnum.ServerAuthOk)
      {
         try
         {
            settings_.setDeviceID(msg.getReason());
            settings_.saveSettings();
            state_.onState(IStateCallback.StateEnum.ServerAuthOk, "");
            readyToSend_ = true;
         }
         catch(Exception ex) {}
      }
      else
      {
         state_.onState(msg.getState(), msg.getReason());
      }
   }

   public void process(BytesInOut msg)
   {
      state_.onBytesInOut(msg.getIn(), msg.getOut());
   }

   public void process(ErrorMessage msg) throws Exception
   {
      switch (msg.getErrCode())
      {
         case ErrorMessage.ErrorCode.WrongDID:
         {
            state_.onState(IStateCallback.StateEnum.DeviceWrongDID, "");
            readyToSend_ = false;
            break;
         }
         case ErrorMessage.ErrorCode.AlreadyAuth:
         {
            state_.onState(IStateCallback.StateEnum.DeviceAlreadyInUse, "");
            readyToSend_ = false;
            break;
         }
         case ErrorMessage.ErrorCode.FuckOff:
         {
            state_.onState(IStateCallback.StateEnum.ServerDisconnected, "Fuck off");
            stop();
            break;
         }
         case ErrorMessage.ErrorCode.ServerUnavailable:
         {
            state_.onState(IStateCallback.StateEnum.ServerDisconnected, "Server is unavailable");
            stop();
            break;
         }
         case ErrorMessage.ErrorCode.WrongMsg:
         {
            state_.onState(IStateCallback.StateEnum.ServerDisconnected,
                    "Server got unrecognized message");
            stop();
            break;
         }
         case ErrorMessage.ErrorCode.NotAuth:
         {
            state_.onState(IStateCallback.StateEnum.ServerDisconnected, "Not authorized");
            stop();
            break;
         }
      }
   }

   public void onMessage(IMessage msg)
   {
      synchronized (pendingMsgs_)
      {
         try
         {
            pendingMsgs_.addElement(msg);
            pendingMsgs_.notify();
         }
         catch(Exception ex)
         {
         }
      }
   }

   private boolean         started_ = false;
   private Thread          working_thread_ = null;
   private IStateCallback  state_ = null;
   private Settings        settings_;
   private Connection      conn_ = null;
   private boolean         readyToSend_ = false;
   private final Vector    pendingMsgs_ = new Vector();
   private ILocator        locator_ = null;
   private long            lastSendMsgTS_ = 0;
   private long            sendPeriod_ = 0;
}
