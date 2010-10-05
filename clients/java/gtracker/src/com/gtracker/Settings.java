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

import javax.microedition.rms.*;

public class Settings
{
   private class SettingFields
   {

      public static final int DeviceID = 1;
      public static final int ServerName = 2;
      public static final int BkpServerName = 3;
      public static final int ServerPort = 4;
      public static final int BkpServerPort = 5;
      public static final int SendPeriod = 6;
      public static final int GpsSource = 7;
      public static final int GpsDeviceName = 8;
      public static final int DynamicSendPeriod = 9;
   }

   public class GPSSource
   {

      static final int Internal = 0;
      static final int External = 1;
   };

   public Settings(String store) throws Exception
   {
      storeName_ = store;
      try
      {
         loadSettings();
      } catch (RecordStoreNotFoundException ex)
      {
         initStorage();
         saveSettings();
      }
   }

   public Settings()
   {
   }

   public String getServerName()
   {
      return serverName_;
   }

   public String getBkpServerName()
   {
      return bkpServerName_;
   }

   public int getServerPort()
   {
      return serverPort_;
   }

   public int getBkpServerPort()
   {
      return bkpServerPort_;
   }

   public int getSendPeriod()
   {
      return sendPeriod_;
   }

   public String getDeviceID()
   {
      return did_;
   }

   public int getGpsSource()
   {
      return gpsSource_;
   }

   public String getGpsDeviceName()
   {
      return gpsDeviceName_;
   }

   public boolean getDynamicSendPeriod()
   {
      return dynamicSendPeriod_;
   }

   public void setServerName(String newVal)
   {
      serverName_ = newVal;
   }

   public void setBkpServerName(String newVal)
   {
      bkpServerName_ = newVal;
   }

   public void setServerPort(int newVal)
   {
      serverPort_ = newVal;
   }

   public void setBkpServerPort(int newVal)
   {
      bkpServerPort_ = newVal;
   }

   public void setDeviceID(String newVal)
   {
      did_ = newVal;
   }

   public void setSendPeriod(int newVal)
   {
      sendPeriod_ = newVal;
   }

   public void setGpsSource(int newVal)
   {
      gpsSource_ = newVal;
   }

   public void setGpsDeviceName(String newVal)
   {
      gpsDeviceName_ = newVal;
   }

   public void setDynamicSendPeriod(boolean newVal)
   {
      dynamicSendPeriod_ = newVal;
   }

   public void saveSettings() throws Exception
   {
      if (storeName_ == null)
      {
         return;
      }
      RecordStore rs = RecordStore.openRecordStore(storeName_, true);
      saveVal(rs, SettingFields.DeviceID, getDeviceID());
      saveVal(rs, SettingFields.ServerName, getServerName());
      saveVal(rs, SettingFields.BkpServerName, getBkpServerName());
      saveVal(rs, SettingFields.ServerPort, String.valueOf(getServerPort()));
      saveVal(rs, SettingFields.BkpServerPort, String.valueOf(getBkpServerPort()));
      saveVal(rs, SettingFields.SendPeriod, String.valueOf(getSendPeriod()));
      saveVal(rs, SettingFields.GpsSource, String.valueOf(getGpsSource()));
      saveVal(rs, SettingFields.GpsDeviceName, getGpsDeviceName());
      saveVal(rs, SettingFields.DynamicSendPeriod,
              getDynamicSendPeriod() ? "1" : "0");
      rs.closeRecordStore();
   }

   public void reload() throws Exception
   {
      loadSettings();
   }

   private void loadSettings() throws Exception
   {
      if (storeName_ == null)
      {
         return;
      }
      RecordStore rs = RecordStore.openRecordStore(storeName_, false);
      setDeviceID(getVal(rs, SettingFields.DeviceID));
      setServerName(getVal(rs, SettingFields.ServerName));
      setBkpServerName(getVal(rs, SettingFields.BkpServerName));
      setServerPort(Integer.parseInt(getVal(rs, SettingFields.ServerPort)));
      setBkpServerPort(Integer.parseInt(getVal(rs, SettingFields.BkpServerPort)));
      setSendPeriod(Integer.parseInt(getVal(rs, SettingFields.SendPeriod)));
      setGpsSource(Integer.parseInt(getVal(rs, SettingFields.GpsSource)));
      setGpsDeviceName(getVal(rs, SettingFields.GpsDeviceName));
      setDynamicSendPeriod(
              Integer.parseInt(getVal(rs, SettingFields.DynamicSendPeriod)) == 0 ? false : true);
      rs.closeRecordStore();
   }

   private void initStorage() throws Exception
   {
      RecordStore rs = RecordStore.openRecordStore(storeName_, true);
      byte buf[] = {' '};
      rs.addRecord(buf, 0, buf.length);
      rs.addRecord(buf, 0, buf.length);
      rs.addRecord(buf, 0, buf.length);
      rs.addRecord(buf, 0, buf.length);
      rs.addRecord(buf, 0, buf.length);
      rs.addRecord(buf, 0, buf.length);
      rs.addRecord(buf, 0, buf.length);
      rs.addRecord(buf, 0, buf.length);
      rs.addRecord(buf, 0, buf.length);
      rs.closeRecordStore();
   }

   private String getVal(RecordStore rs, int index) throws Exception
   {
      byte[] buf = rs.getRecord(index);
      if (buf != null)
      {
         return new String(buf);
      }
      return "";
   }

   private void saveVal(RecordStore rs, int index, String value) throws Exception
   {
      byte buf[] = value.getBytes();
      rs.setRecord(index, buf, 0, buf.length);
   }
   private String storeName_;
   private String did_ = "";
   private String serverName_ = "gtracker.ru";
   private String bkpServerName_ = "bakup.gtracker.ru";
   private int serverPort_ = 7777;
   private int bkpServerPort_ = 7777;
   private int sendPeriod_ = 1;
   private int gpsSource_ = GPSSource.Internal;
   private String gpsDeviceName_ = "";
   private boolean dynamicSendPeriod_ = false;
};
