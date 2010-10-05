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

import javax.bluetooth.*;
import javax.microedition.io.Connector;
import javax.microedition.io.StreamConnection;
import com.sergioestevao.midp.StringTokenizer;
import java.io.DataInputStream;
import java.io.IOException;
import java.util.Calendar;

public class BluetoothLocator implements ILocator, Runnable, DiscoveryListener
{
   public BluetoothLocator(GtClient client, String btName) throws Exception
   {
      client_ = client;
      state_ = new State(client, IStateCallback.StateEnum.GpsDisconnected);
      btName_ = btName;
      thread_ = new Thread(this);
   }

   public void start() throws Exception
   {
      thread_.start();
   }
   
   public void stop() throws Exception
   {
      quit_ = true;
      close();
      synchronized(this)
      {
         notify();
      }
      thread_.join();
   }

   public void run()
   {
      byte[] buff = new byte[512];
      int offset = 0;
      int len = buff.length;
      int begin = -1;
      int end = -1;
      while(!quit_)
      {
         try
         {
            connect();
            if (state_.getState() != IStateCallback.StateEnum.GpsConnected)
            {
               Thread.sleep(1000);
               Thread.yield();
               continue;
            }
            int res = in_.read(buff, offset, len);
            if (res <= 0)
            {
               state_.setState(IStateCallback.StateEnum.GpsDisconnected);
               continue;
            }
            for(int i = offset; i < offset + res; ++i)
            {
               if(buff[i] == '$')
               { // begin of NMEA message
                  begin = i;
                  end = -1;
               } else if (begin >= 0 && buff[i] == '\n')
               {
                  end = i;
                  parse(new String(buff, begin, end - begin));
                  begin = -1;
               }
            }
            offset += res;
            len = len - res;
            if (begin > buff.length - 80)
            {
               if (end < 0)
               {
                  offset = offset - begin;
                  System.arraycopy(buff, begin, buff, 0, offset);
                  len = buff.length - offset;
                  begin = 0;
                  end = -1;
               }
            }
            if (len == 0)
            {
               offset = 0;
               len = buff.length;
               begin = -1;
               end = -1;
            }
         }
         catch(IOException ex)
         {
            if (quit_)
            {
               break;
            }
            state_.setState(IStateCallback.StateEnum.GpsDisconnected, ex.getMessage());
         }
         catch(Exception ex)
         {
            if (quit_)
            {
               break;
            }
            state_.setText(ex.toString());
         }
      }
      close();
      state_.setState(IStateCallback.StateEnum.GpsDisconnected);
   }

   private float lat2float(String lat)
   {
      String lat_deg = lat.substring(0, 2);
      String lat_min1 = lat.substring(2, 4);
      String lat_min2 = lat.substring(5);
      String lat_min3 = "0." + lat_min1 + lat_min2;
      float lat_dec = Float.parseFloat(lat_min3)/.6f;
      return Float.parseFloat(lat_deg) + lat_dec;
   }

   private float lon2float(String lon)
   {
      String lon_deg = lon.substring(0, 3);
      String lon_min1 = lon.substring(3, 5);
      String lon_min2 = lon.substring(6);
      String lon_min3 = "0." + lon_min1 + lon_min2;
      float lon_dec = Float.parseFloat(lon_min3)/.6f;
      return Float.parseFloat(lon_deg) + lon_dec;
   }

   private void parse(String s)
   {
      //state_.setText(s);
      StringTokenizer tok = new StringTokenizer(s, ",");
      String header = tok.nextToken();
      float lat = 0;
      String lat_dir = "";
      float lon = 0;
      String lon_dir = "";
      float speed = 0;
      if (0 == header.compareTo("$GPGGA"))
      {
         str2Time(tok.nextToken());
         String tmp = tok.nextToken();
         if (tmp.length() > 0)
         {
            lat = lat2float(tmp);
         }
         lat_dir = tok.nextToken();
         tmp = tok.nextToken();
         if (tmp.length() > 0)
         {
            lon = lon2float(tmp);
         }
         lon_dir = tok.nextToken();
      }
      else if (0 == header.compareTo("$GPRMC"))
      {
         tok.nextToken(); // skip time
         if (0 == tok.nextToken().compareTo("A"))
         {
            tok.nextToken(); // skip Lat
            tok.nextToken(); // skip North/South
            tok.nextToken(); // skip Lon
            tok.nextToken(); // skip East/West
            String tmp = tok.nextToken();
            if (tmp.length() > 0)
            {
               speed = Float.parseFloat(tmp); // speed
            }
            tok.nextToken(); // skip Course
            str2Date(tok.nextToken());
         }
/*       str2Time(tok.nextToken());
         if (0 == tok.nextToken().compareTo("A"))
         {
            String tmp = tok.nextToken();
            if (tmp.length() > 0)
            {
               lat = lat2float(tmp);
            }
            lat_dir = tok.nextToken();
            tmp = tok.nextToken();
            if (tmp.length() > 0)
            {
               lon = lon2float(tmp);
            }
            lon_dir = tok.nextToken();
            tok.nextToken(); // skip
            tok.nextToken(); // skip
            str2Date(tok.nextToken());
         }  */
      }

      if (lat != .0 && lon != .0)
      {
         if (lat_dir.equals("S"))
         {
            lat = lat * (-1);
         }
         if (lon_dir.equals("W"))
         {
            lon = lon * (-1);
         }
         if (cal_.get(Calendar.YEAR) > 0)
         {
            client_.onMessage(
                    new CoordinateMessage(lat, lon, cal_.getTime().getTime()));
         }
      }
      client_.onMessage(new SpeedMessage(speed));
   }

   private void str2Date(String str)
   {
      if (str.length() < 6)
      {
         return;
      }
      int day = Integer.parseInt(str.substring(0, 2));
      int month = Integer.parseInt(str.substring(2, 4)) - 1;
      int year = Integer.parseInt(str.substring(4, 6));

      cal_.set(Calendar.DAY_OF_MONTH, day);
      cal_.set(Calendar.MONTH, month);
      cal_.set(Calendar.YEAR, 2000 + year);
   }

   private void str2Time(String str)
   {
      if (str.length() < 6)
      {
         return;
      }
      int hour = Integer.parseInt(str.substring(0, 2));
      int min = Integer.parseInt(str.substring(2, 4));
      int sec = Integer.parseInt(str.substring(4, 6));

      cal_.set(Calendar.HOUR, hour);
      cal_.set(Calendar.MINUTE, min);
      cal_.set(Calendar.SECOND, sec);
   }

   private void close()
   {
      try
      {
         if (in_ != null)
         {
            in_.close();
            in_ = null;
         }
         if (conn_ != null)
         {
            conn_.close();
            conn_ = null;
         }
      }
      catch(Exception ex)
      {
         state_.setState(IStateCallback.StateEnum.GpsDisconnected, "BLA!");
      }
   }

   private String getDeviceName(RemoteDevice rd)
   {
      String name = "";
      try
      {
         name = rd.getFriendlyName(false);
      }
      catch(IOException ex) {}
      if (name.length() == 0)
      {
         state_.setText("noname" + String.valueOf(++counter_) + " found.");
      }
      else
      {
         state_.setText(name + " found.");
      }
      return name;
   }

   private void connect()
   {
      try
      {
         if (quit_ || state_.getState() == IStateCallback.StateEnum.GpsConnected)
         {
            return;
         }
         counter_ = 0;
         state_.setState(IStateCallback.StateEnum.GpsConnecting);
         ld_ = LocalDevice.getLocalDevice();
         agent_ = ld_.getDiscoveryAgent();
         int options[] = {DiscoveryAgent.CACHED, DiscoveryAgent.PREKNOWN};
         for(int i = 0; i < options.length; ++i)
         {
            RemoteDevice devices[] =  agent_.retrieveDevices(i);
            if (devices != null)
            {
               for(int j = 0; j < devices.length; ++j)
               {
                  if (getDeviceName(devices[i]).compareTo(btName_) == 0)
                  {
                     rd_ = devices[i];
                     break;
                  }
               }
            }
         }

         if (rd_ == null)
         {
            agent_.startInquiry(DiscoveryAgent.GIAC, this);
            synchronized(this)
            {
               wait();
            }
         }

         if (rd_ == null)
         {
            state_.setState(IStateCallback.StateEnum.GpsNotFound);
            return;
         }
         UUID[] uuids = new UUID[]{new UUID(0x1101)};
         agent_.searchServices(null, uuids, rd_, this);
         synchronized(this)
         {
            wait();
         }
      }
      catch(Exception ex)
      {
         state_.setState(IStateCallback.StateEnum.GpsDisconnected, ex.toString());
      }
   }

   public void deviceDiscovered(RemoteDevice btDevice, DeviceClass cod)
   {
      String name = getDeviceName(btDevice);
      if (0 == name.compareTo(btName_))
      {
         rd_ = btDevice;
         agent_.cancelInquiry(this);
      }
   }

   public void inquiryCompleted(int discType)
   {
      synchronized(this)
      {
         notify();
      }
   }

   public void servicesDiscovered(int transID, ServiceRecord[] servRecord)
   {
      if (servRecord.length > 0)
      {
         try
         {
            String url = servRecord[0].getConnectionURL(ServiceRecord.AUTHENTICATE_NOENCRYPT, false);
            conn_ = (StreamConnection)Connector.open(url, Connector.READ_WRITE);
            in_ = conn_.openDataInputStream();
            state_.setState(IStateCallback.StateEnum.GpsConnected);
         }
         catch(Exception ex)
         {
            state_.setText(ex.toString());
         }
         synchronized(this)
         {
            notify();
         }
      }
   }

   public void serviceSearchCompleted(int transID, int respCode)
   {
      synchronized(this)
      {
         notify();
      }
   }

   private IClient            client_ = null;
   private LocalDevice        ld_ = null;
   private RemoteDevice       rd_ = null;
   private Thread             thread_ = null;
   private String             btName_;
   private StreamConnection   conn_ = null;
   private DataInputStream    in_ = null;
   private State              state_ = null;
   private boolean            quit_ = false;
   private DiscoveryAgent     agent_ = null;
   private int                counter_ = 0;
   private Calendar           cal_ = Calendar.getInstance();
}
