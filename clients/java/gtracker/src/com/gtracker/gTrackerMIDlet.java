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

import javax.microedition.midlet.*;
import javax.microedition.lcdui.*;

/**
 * @author dmitryme
 */
public class gTrackerMIDlet extends MIDlet implements CommandListener, IStateCallback, ItemStateListener,
        ItemCommandListener
{
   private final String SETTINGS_STORAGE = "gtracker.settings";
   private boolean midletPaused = false;
   private Settings settings_;
   private GtClient client_;
   long bytesIn_ = 0;
   long bytesOut_ = 0;
   //<editor-fold defaultstate="collapsed" desc=" Generated Fields ">//GEN-BEGIN:|fields|0|
   private Form mainWindow;
   private StringItem strDeviceID;
   private StringItem strConnStatus;
   private StringItem strBytesInOut;
   private StringItem strGPSStatus;
   private Alert notImplBox;
   private Form settingsForm;
   private TextField txtServerPort;
   private TextField txtServerName;
   private ChoiceGroup choiceGPS;
   private TextField txtSendPeriod;
   private TextField txtGpsDeviceName;
   private TextField txtDeviceID;
   private TextField txtBkpServerPort;
   private TextField txtBkpServerName;
   private ChoiceGroup dynamicSendPeriod;
   private Alert errorBox;
   private Alert AboutBox;
   private Command exitCommand;
   private Command settingsCommand;
   private Command aboutCommand;
   private Command helpCommand;
   private Command startCommand;
   private Command cancelCommand;
   private Command saveCommand;
   private Image image;
   //</editor-fold>//GEN-END:|fields|0|

   /**
    * The gTrackerMIDlet constructor.
    */
   public gTrackerMIDlet() throws Exception
   {
      try
      {
         settings_ = new Settings(SETTINGS_STORAGE);
         client_ = new GtClient(settings_, this);
      } catch (Exception ex)
      {
         exitMIDlet();
      }
   }

   //<editor-fold defaultstate="collapsed" desc=" Generated Methods ">//GEN-BEGIN:|methods|0|
   //</editor-fold>//GEN-END:|methods|0|
   //<editor-fold defaultstate="collapsed" desc=" Generated Method: initialize ">//GEN-BEGIN:|0-initialize|0|0-preInitialize
   /**
    * Initilizes the application.
    * It is called only once when the MIDlet is started. The method is called before the <code>startMIDlet</code> method.
    */
   private void initialize() {//GEN-END:|0-initialize|0|0-preInitialize
       // write pre-initialize user code here
      settingsForm = new Form("Settings", new Item[] { getTxtDeviceID(), getTxtServerName(), getTxtServerPort(), getTxtBkpServerName(), getTxtBkpServerPort(), getDynamicSendPeriod(), getTxtSendPeriod(), getChoiceGPS(), getTxtGpsDeviceName() });//GEN-BEGIN:|0-initialize|1|0-postInitialize
      settingsForm.addCommand(getSaveCommand());
      settingsForm.addCommand(getCancelCommand());
      settingsForm.setCommandListener(this);//GEN-END:|0-initialize|1|0-postInitialize
       // write post-initialize user code here
      settingsForm.setItemStateListener(this);
   }//GEN-BEGIN:|0-initialize|2|
   //</editor-fold>//GEN-END:|0-initialize|2|

   //<editor-fold defaultstate="collapsed" desc=" Generated Method: startMIDlet ">//GEN-BEGIN:|3-startMIDlet|0|3-preAction
   /**
    * Performs an action assigned to the Mobile Device - MIDlet Started point.
    */
   public void startMIDlet() {//GEN-END:|3-startMIDlet|0|3-preAction
       // write pre-action user code here
      switchDisplayable(null, getMainWindow());//GEN-LINE:|3-startMIDlet|1|3-postAction
       // write post-action user code here
   }//GEN-BEGIN:|3-startMIDlet|2|
   //</editor-fold>//GEN-END:|3-startMIDlet|2|

   //<editor-fold defaultstate="collapsed" desc=" Generated Method: resumeMIDlet ">//GEN-BEGIN:|4-resumeMIDlet|0|4-preAction
   /**
    * Performs an action assigned to the Mobile Device - MIDlet Resumed point.
    */
   public void resumeMIDlet() {//GEN-END:|4-resumeMIDlet|0|4-preAction
       // write pre-action user code here
//GEN-LINE:|4-resumeMIDlet|1|4-postAction
       // write post-action user code here
   }//GEN-BEGIN:|4-resumeMIDlet|2|
   //</editor-fold>//GEN-END:|4-resumeMIDlet|2|

   //<editor-fold defaultstate="collapsed" desc=" Generated Method: switchDisplayable ">//GEN-BEGIN:|5-switchDisplayable|0|5-preSwitch
   /**
    * Switches a current displayable in a display. The <code>display</code> instance is taken from <code>getDisplay</code> method. This method is used by all actions in the design for switching displayable.
    * @param alert the Alert which is temporarily set to the display; if <code>null</code>, then <code>nextDisplayable</code> is set immediately
    * @param nextDisplayable the Displayable to be set
    */
   public void switchDisplayable(Alert alert, Displayable nextDisplayable) {//GEN-END:|5-switchDisplayable|0|5-preSwitch
       // write pre-switch user code here
      Display display = getDisplay();//GEN-BEGIN:|5-switchDisplayable|1|5-postSwitch
      if (alert == null) {
         display.setCurrent(nextDisplayable);
      } else {
         display.setCurrent(alert, nextDisplayable);
      }//GEN-END:|5-switchDisplayable|1|5-postSwitch
       // write post-switch user code here
   }//GEN-BEGIN:|5-switchDisplayable|2|
   //</editor-fold>//GEN-END:|5-switchDisplayable|2|

   //<editor-fold defaultstate="collapsed" desc=" Generated Method: commandAction for Displayables ">//GEN-BEGIN:|7-commandAction|0|7-preCommandAction
   /**
    * Called by a system to indicated that a command has been invoked on a particular displayable.
    * @param command the Command that was invoked
    * @param displayable the Displayable where the command was invoked
    */
   public void commandAction(Command command, Displayable displayable) {//GEN-END:|7-commandAction|0|7-preCommandAction
       // write pre-action user code here
      if (displayable == mainWindow) {//GEN-BEGIN:|7-commandAction|1|24-preAction
         if (command == aboutCommand) {//GEN-END:|7-commandAction|1|24-preAction
               // write pre-action user code here
            switchDisplayable(getAboutBox(), getMainWindow());//GEN-LINE:|7-commandAction|2|24-postAction
               // write post-action user code here
         } else if (command == exitCommand) {//GEN-LINE:|7-commandAction|3|20-preAction
            client_.stop();
            exitMIDlet();//GEN-LINE:|7-commandAction|4|20-postAction
               // write post-action user code here
         } else if (command == helpCommand) {//GEN-LINE:|7-commandAction|5|26-preAction
               // write pre-action user code here
            switchDisplayable(getNotImplBox(), getMainWindow());//GEN-LINE:|7-commandAction|6|26-postAction
               // write post-action user code here
         } else if (command == settingsCommand) {//GEN-LINE:|7-commandAction|7|22-preAction
              // write pre-action user code here
            reloadSettings();
            switchDisplayable(null, settingsForm);//GEN-LINE:|7-commandAction|8|22-postAction
               // write post-action user code here
         } else if (command == startCommand) {//GEN-LINE:|7-commandAction|9|31-preAction
               // write pre-action user code here
               client_.start();
//GEN-LINE:|7-commandAction|10|31-postAction
               // write post-action user code here
         }//GEN-BEGIN:|7-commandAction|11|48-preAction
      } else if (displayable == settingsForm) {
         if (command == cancelCommand) {//GEN-END:|7-commandAction|11|48-preAction
               // write pre-action user code here
            switchDisplayable(null, getMainWindow());//GEN-LINE:|7-commandAction|12|48-postAction
               // write post-action user code here
         } else if (command == saveCommand) {//GEN-LINE:|7-commandAction|13|45-preAction
               // write pre-action user code here
               try
               {
                  Settings s = new Settings(SETTINGS_STORAGE);
                  s.setServerName(txtServerName.getString());
                  s.setServerPort(Integer.parseInt(txtServerPort.getString()));
                  s.setSendPeriod(Integer.parseInt(txtSendPeriod.getString()));
                  s.setDeviceID(txtDeviceID.getString());
                  s.setGpsSource(choiceGPS.getSelectedIndex());
                  s.setGpsDeviceName(txtGpsDeviceName.getString());
                  s.setDynamicSendPeriod(dynamicSendPeriod.isSelected(0));
                  s.saveSettings();
                  client_.reconfigure(s);
                  settings_.reload();

                  switchDisplayable(null, getMainWindow());//GEN-LINE:|7-commandAction|14|45-postAction
               } catch (Exception ex)
               {
                  onError(ex.toString());
               }
         }//GEN-BEGIN:|7-commandAction|15|7-postCommandAction
      }//GEN-END:|7-commandAction|15|7-postCommandAction
       // write post-action user code here
   }//GEN-BEGIN:|7-commandAction|16|
   //</editor-fold>//GEN-END:|7-commandAction|16|

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: mainWindow ">//GEN-BEGIN:|14-getter|0|14-preInit
   /**
    * Returns an initiliazed instance of mainWindow component.
    * @return the initialized component instance
    */
   public Form getMainWindow() {
      if (mainWindow == null) {//GEN-END:|14-getter|0|14-preInit
           // write pre-init user code here
         mainWindow = new Form("gTracker", new Item[] { getStrConnStatus(), getStrDeviceID(), getStrGPSStatus(), getStrBytesInOut() });//GEN-BEGIN:|14-getter|1|14-postInit
         mainWindow.addCommand(getStartCommand());
         mainWindow.addCommand(getSettingsCommand());
         mainWindow.addCommand(getAboutCommand());
         mainWindow.addCommand(getHelpCommand());
         mainWindow.addCommand(getExitCommand());
         mainWindow.setCommandListener(this);//GEN-END:|14-getter|1|14-postInit
           // write post-init user code here
      }//GEN-BEGIN:|14-getter|2|
      return mainWindow;
   }
   //</editor-fold>//GEN-END:|14-getter|2|
   //</editor-fold>

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: notImplBox ">//GEN-BEGIN:|27-getter|0|27-preInit
   /**
    * Returns an initiliazed instance of notImplBox component.
    * @return the initialized component instance
    */
   public Alert getNotImplBox() {
      if (notImplBox == null) {//GEN-END:|27-getter|0|27-preInit
           // write pre-init user code here
         notImplBox = new Alert("", "Not implemented yet", null, AlertType.WARNING);//GEN-BEGIN:|27-getter|1|27-postInit
         notImplBox.setTimeout(Alert.FOREVER);//GEN-END:|27-getter|1|27-postInit
           // write post-init user code here
      }//GEN-BEGIN:|27-getter|2|
      return notImplBox;
   }
   //</editor-fold>//GEN-END:|27-getter|2|

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: exitCommand ">//GEN-BEGIN:|19-getter|0|19-preInit
   /**
    * Returns an initiliazed instance of exitCommand component.
    * @return the initialized component instance
    */
   public Command getExitCommand() {
      if (exitCommand == null) {//GEN-END:|19-getter|0|19-preInit
           // write pre-init user code here
         exitCommand = new Command("Exit", Command.ITEM, 0);//GEN-LINE:|19-getter|1|19-postInit
           // write post-init user code here
      }//GEN-BEGIN:|19-getter|2|
      return exitCommand;
   }
   //</editor-fold>//GEN-END:|19-getter|2|

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: settingsCommand ">//GEN-BEGIN:|21-getter|0|21-preInit
   /**
    * Returns an initiliazed instance of settingsCommand component.
    * @return the initialized component instance
    */
   public Command getSettingsCommand() {
      if (settingsCommand == null) {//GEN-END:|21-getter|0|21-preInit
           // write pre-init user code here
         settingsCommand = new Command("Settings", Command.ITEM, 0);//GEN-LINE:|21-getter|1|21-postInit
           // write post-init user code here
      }//GEN-BEGIN:|21-getter|2|
      return settingsCommand;
   }
   //</editor-fold>//GEN-END:|21-getter|2|

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: aboutCommand ">//GEN-BEGIN:|23-getter|0|23-preInit
   /**
    * Returns an initiliazed instance of aboutCommand component.
    * @return the initialized component instance
    */
   public Command getAboutCommand() {
      if (aboutCommand == null) {//GEN-END:|23-getter|0|23-preInit
           // write pre-init user code here
         aboutCommand = new Command("About", Command.ITEM, 0);//GEN-LINE:|23-getter|1|23-postInit
           // write post-init user code here
      }//GEN-BEGIN:|23-getter|2|
      return aboutCommand;
   }
   //</editor-fold>//GEN-END:|23-getter|2|

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: helpCommand ">//GEN-BEGIN:|25-getter|0|25-preInit
   /**
    * Returns an initiliazed instance of helpCommand component.
    * @return the initialized component instance
    */
   public Command getHelpCommand() {
      if (helpCommand == null) {//GEN-END:|25-getter|0|25-preInit
           // write pre-init user code here
         helpCommand = new Command("Help", Command.ITEM, 0);//GEN-LINE:|25-getter|1|25-postInit
           // write post-init user code here
      }//GEN-BEGIN:|25-getter|2|
      return helpCommand;
   }
   //</editor-fold>//GEN-END:|25-getter|2|
   //</editor-fold>

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: startCommand ">//GEN-BEGIN:|30-getter|0|30-preInit
   /**
    * Returns an initiliazed instance of startCommand component.
    * @return the initialized component instance
    */
   public Command getStartCommand() {
      if (startCommand == null) {//GEN-END:|30-getter|0|30-preInit
           // write pre-init user code here
         startCommand = new Command("Start", Command.ITEM, 0);//GEN-LINE:|30-getter|1|30-postInit
           // write post-init user code here
      }//GEN-BEGIN:|30-getter|2|
      return startCommand;
   }
   //</editor-fold>//GEN-END:|30-getter|2|
   //</editor-fold>
   //</editor-fold>

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: strDeviceID ">//GEN-BEGIN:|38-getter|0|38-preInit
   /**
    * Returns an initiliazed instance of strDeviceID component.
    * @return the initialized component instance
    */
   public StringItem getStrDeviceID() {
      if (strDeviceID == null) {//GEN-END:|38-getter|0|38-preInit
           // write pre-init user code here
         strDeviceID = new StringItem("Device ID (DID)", "not set");//GEN-LINE:|38-getter|1|38-postInit
           // write post-init user code here
      }//GEN-BEGIN:|38-getter|2|
      return strDeviceID;
   }
   //</editor-fold>//GEN-END:|38-getter|2|
   //</editor-fold>
   //</editor-fold>

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: strConnStatus ">//GEN-BEGIN:|39-getter|0|39-preInit
   /**
    * Returns an initiliazed instance of strConnStatus component.
    * @return the initialized component instance
    */
   public StringItem getStrConnStatus() {
      if (strConnStatus == null) {//GEN-END:|39-getter|0|39-preInit
           // write pre-init user code here
         strConnStatus = new StringItem("Connection status", "disconnected", Item.PLAIN);//GEN-LINE:|39-getter|1|39-postInit
           // write post-init user code here
      }//GEN-BEGIN:|39-getter|2|
      return strConnStatus;
   }
   //</editor-fold>//GEN-END:|39-getter|2|
   //</editor-fold>
   //</editor-fold>

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: strBytesInOut ">//GEN-BEGIN:|40-getter|0|40-preInit
   /**
    * Returns an initiliazed instance of strBytesInOut component.
    * @return the initialized component instance
    */
   public StringItem getStrBytesInOut() {
      if (strBytesInOut == null) {//GEN-END:|40-getter|0|40-preInit
           // write pre-init user code here
         strBytesInOut = new StringItem("Bytes In/Out", "0/0");//GEN-LINE:|40-getter|1|40-postInit
           // write post-init user code here
      }//GEN-BEGIN:|40-getter|2|
      return strBytesInOut;
   }
   //</editor-fold>//GEN-END:|40-getter|2|
   //</editor-fold>
   //</editor-fold>

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: strGPSStatus ">//GEN-BEGIN:|41-getter|0|41-preInit
   /**
    * Returns an initiliazed instance of strGPSStatus component.
    * @return the initialized component instance
    */
   public StringItem getStrGPSStatus() {
      if (strGPSStatus == null) {//GEN-END:|41-getter|0|41-preInit
           // write pre-init user code here
         strGPSStatus = new StringItem("GPS status", "disconnected");//GEN-BEGIN:|41-getter|1|41-postInit
         strGPSStatus.setPreferredSize(-1, -1);//GEN-END:|41-getter|1|41-postInit
           // write post-init user code here
      }//GEN-BEGIN:|41-getter|2|
      return strGPSStatus;
   }
   //</editor-fold>//GEN-END:|41-getter|2|
   //</editor-fold>


   //</editor-fold>

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: txtServerName ">//GEN-BEGIN:|50-getter|0|50-preInit
   /**
    * Returns an initiliazed instance of txtServerName component.
    * @return the initialized component instance
    */
   public TextField getTxtServerName() {
      if (txtServerName == null) {//GEN-END:|50-getter|0|50-preInit
           // write pre-init user code here
         txtServerName = new TextField("Server name or IP", "", 32, TextField.ANY);//GEN-LINE:|50-getter|1|50-postInit
           // write post-init user code here
           txtServerName.setString(settings_.getServerName());
      }//GEN-BEGIN:|50-getter|2|
      return txtServerName;
   }
   //</editor-fold>//GEN-END:|50-getter|2|
   //</editor-fold>

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: txtServerPort ">//GEN-BEGIN:|51-getter|0|51-preInit
   /**
    * Returns an initiliazed instance of txtServerPort component.
    * @return the initialized component instance
    */
   public TextField getTxtServerPort() {
      if (txtServerPort == null) {//GEN-END:|51-getter|0|51-preInit
           // write pre-init user code here
         txtServerPort = new TextField("Server port", "", 32, TextField.NUMERIC);//GEN-LINE:|51-getter|1|51-postInit
           // write post-init user code here
           txtServerPort.setString(String.valueOf(settings_.getServerPort()));
      }//GEN-BEGIN:|51-getter|2|
      return txtServerPort;
   }
   //</editor-fold>//GEN-END:|51-getter|2|
   //</editor-fold>

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: txtSendPeriod ">//GEN-BEGIN:|52-getter|0|52-preInit
   /**
    * Returns an initiliazed instance of txtSendPeriod component.
    * @return the initialized component instance
    */
   public TextField getTxtSendPeriod() {
      if (txtSendPeriod == null) {//GEN-END:|52-getter|0|52-preInit
           // write pre-init user code here
         txtSendPeriod = new TextField("Send period (in sec)", "", 32, TextField.NUMERIC);//GEN-LINE:|52-getter|1|52-postInit
           // write post-init user code here
           txtSendPeriod.setString(String.valueOf(settings_.getSendPeriod()));
            if (settings_.getDynamicSendPeriod())
            {
               txtSendPeriod.setConstraints(TextField.NUMERIC | TextField.UNEDITABLE);
            }
            else
            {
               txtSendPeriod.setConstraints(TextField.NUMERIC);
            }

      }//GEN-BEGIN:|52-getter|2|
      return txtSendPeriod;
   }
   //</editor-fold>//GEN-END:|52-getter|2|
   //</editor-fold>
   //</editor-fold>

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: choiceGPS ">//GEN-BEGIN:|53-getter|0|53-preInit
   /**
    * Returns an initiliazed instance of choiceGPS component.
    * @return the initialized component instance
    */
   public ChoiceGroup getChoiceGPS() {
      if (choiceGPS == null) {//GEN-END:|53-getter|0|53-preInit
           // write pre-init user code here
         choiceGPS = new ChoiceGroup("GPS", Choice.EXCLUSIVE);//GEN-BEGIN:|53-getter|1|53-postInit
         choiceGPS.append("internal", null);
         choiceGPS.append("external", null);
         choiceGPS.setItemCommandListener(this);
         choiceGPS.setSelectedFlags(new boolean[] { true, false });//GEN-END:|53-getter|1|53-postInit
           // write post-init user code here
           choiceGPS.setSelectedIndex(settings_.getGpsSource(), true);
      }//GEN-BEGIN:|53-getter|2|
      return choiceGPS;
   }
   //</editor-fold>//GEN-END:|53-getter|2|
   //</editor-fold>


   //</editor-fold>

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: saveCommand ">//GEN-BEGIN:|44-getter|0|44-preInit
   /**
    * Returns an initiliazed instance of saveCommand component.
    * @return the initialized component instance
    */
   public Command getSaveCommand() {
      if (saveCommand == null) {//GEN-END:|44-getter|0|44-preInit
           // write pre-init user code here
         saveCommand = new Command("Save", Command.OK, 0);//GEN-LINE:|44-getter|1|44-postInit
           // write post-init user code here
      }//GEN-BEGIN:|44-getter|2|
      return saveCommand;
   }
   //</editor-fold>//GEN-END:|44-getter|2|

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: cancelCommand ">//GEN-BEGIN:|47-getter|0|47-preInit
   /**
    * Returns an initiliazed instance of cancelCommand component.
    * @return the initialized component instance
    */
   public Command getCancelCommand() {
      if (cancelCommand == null) {//GEN-END:|47-getter|0|47-preInit
           // write pre-init user code here
         cancelCommand = new Command("Cancel", Command.CANCEL, 0);//GEN-LINE:|47-getter|1|47-postInit
           // write post-init user code here
      }//GEN-BEGIN:|47-getter|2|
      return cancelCommand;
   }
   //</editor-fold>//GEN-END:|47-getter|2|

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: txtDeviceID ">//GEN-BEGIN:|57-getter|0|57-preInit
   /**
    * Returns an initiliazed instance of txtDeviceID component.
    * @return the initialized component instance
    */
   public TextField getTxtDeviceID() {
      if (txtDeviceID == null) {//GEN-END:|57-getter|0|57-preInit
           // write pre-init user code here
         txtDeviceID = new TextField("Device ID (DID)", "", 32, TextField.ANY);//GEN-LINE:|57-getter|1|57-postInit
           // write post-init user code here
           txtDeviceID.setString(settings_.getDeviceID());
      }//GEN-BEGIN:|57-getter|2|
      return txtDeviceID;
   }
   //</editor-fold>//GEN-END:|57-getter|2|
   //</editor-fold>

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: errorBox ">//GEN-BEGIN:|58-getter|0|58-preInit
   /**
    * Returns an initiliazed instance of errorBox component.
    * @return the initialized component instance
    */
   public Alert getErrorBox() {
      if (errorBox == null) {//GEN-END:|58-getter|0|58-preInit
           // write pre-init user code here
         errorBox = new Alert("Error", null, null, AlertType.ERROR);//GEN-BEGIN:|58-getter|1|58-postInit
         errorBox.setTimeout(Alert.FOREVER);//GEN-END:|58-getter|1|58-postInit
           // write post-init user code here
      }//GEN-BEGIN:|58-getter|2|
      return errorBox;
   }
   //</editor-fold>//GEN-END:|58-getter|2|

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: txtGpsDeviceName ">//GEN-BEGIN:|59-getter|0|59-preInit
   /**
    * Returns an initiliazed instance of txtGpsDeviceName component.
    * @return the initialized component instance
    */
   public TextField getTxtGpsDeviceName() {
      if (txtGpsDeviceName == null) {//GEN-END:|59-getter|0|59-preInit
          // write pre-init user code here
         txtGpsDeviceName = new TextField("Bluetooth device name", "", 32, TextField.ANY);//GEN-LINE:|59-getter|1|59-postInit
         if (choiceGPS.getSelectedIndex() == Settings.GPSSource.Internal)
         {
            txtGpsDeviceName.setConstraints(TextField.ANY | TextField.UNEDITABLE);
         }
         else
         {
            txtGpsDeviceName.setConstraints(TextField.ANY);
         }
         txtGpsDeviceName.setString(settings_.getGpsDeviceName());
      }//GEN-BEGIN:|59-getter|2|
      return txtGpsDeviceName;
   }
   //</editor-fold>//GEN-END:|59-getter|2|
   //</editor-fold>

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: txtBkpServerName ">//GEN-BEGIN:|60-getter|0|60-preInit
   /**
    * Returns an initiliazed instance of txtBkpServerName component.
    * @return the initialized component instance
    */
   public TextField getTxtBkpServerName() {
      if (txtBkpServerName == null) {//GEN-END:|60-getter|0|60-preInit
         // write pre-init user code here
         txtBkpServerName = new TextField("Backup server name or IP", "", 32, TextField.ANY);//GEN-LINE:|60-getter|1|60-postInit
         // write post-init user code here
         txtBkpServerName.setString(settings_.getBkpServerName());
      }//GEN-BEGIN:|60-getter|2|
      return txtBkpServerName;
   }
   //</editor-fold>//GEN-END:|60-getter|2|
   //</editor-fold>

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: txtBkpServerPort ">//GEN-BEGIN:|61-getter|0|61-preInit
   /**
    * Returns an initiliazed instance of txtBkpServerPort component.
    * @return the initialized component instance
    */
   public TextField getTxtBkpServerPort() {
      if (txtBkpServerPort == null) {//GEN-END:|61-getter|0|61-preInit
         // write pre-init user code here
         txtBkpServerPort = new TextField("Backup server port", "", 32, TextField.NUMERIC);//GEN-LINE:|61-getter|1|61-postInit
         // write post-init user code here
         txtBkpServerPort.setString(String.valueOf(settings_.getBkpServerPort()));

      }//GEN-BEGIN:|61-getter|2|
      return txtBkpServerPort;
   }
   //</editor-fold>//GEN-END:|61-getter|2|
   //</editor-fold>
   //</editor-fold>







   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: image ">//GEN-BEGIN:|65-getter|0|65-preInit
   /**
    * Returns an initiliazed instance of image component.
    * @return the initialized component instance
    */
   public Image getImage() {
      if (image == null) {//GEN-END:|65-getter|0|65-preInit
         // write pre-init user code here
         try {//GEN-BEGIN:|65-getter|1|65-@java.io.IOException
            image = Image.createImage("/gtracker16x16.png");
         } catch (java.io.IOException e) {//GEN-END:|65-getter|1|65-@java.io.IOException
            e.printStackTrace();
         }//GEN-LINE:|65-getter|2|65-postInit
         // write post-init user code here
      }//GEN-BEGIN:|65-getter|3|
      return image;
   }
   //</editor-fold>//GEN-END:|65-getter|3|

   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: AboutBox ">//GEN-BEGIN:|68-getter|0|68-preInit
   /**
    * Returns an initiliazed instance of AboutBox component.
    * @return the initialized component instance
    */
   public Alert getAboutBox() {
      if (AboutBox == null) {//GEN-END:|68-getter|0|68-preInit
         // write pre-init user code here
         AboutBox = new Alert("About", null, null, AlertType.INFO);//GEN-BEGIN:|68-getter|1|68-postInit
         AboutBox.setTimeout(Alert.FOREVER);//GEN-END:|68-getter|1|68-postInit
         // write post-init user code here
         AboutBox.setString(getAppProperty("MIDlet-Name") + " " +
                 getAppProperty("MIDlet-Version"));
      }//GEN-BEGIN:|68-getter|2|
      return AboutBox;
   }
   //</editor-fold>//GEN-END:|68-getter|2|
   //</editor-fold>
   //</editor-fold>
   //</editor-fold>



   //<editor-fold defaultstate="collapsed" desc=" Generated Getter: dynamicSendPeriod ">//GEN-BEGIN:|70-getter|0|70-preInit
   /**
    * Returns an initiliazed instance of dynamicSendPeriod component.
    * @return the initialized component instance
    */
   public ChoiceGroup getDynamicSendPeriod() {
      if (dynamicSendPeriod == null) {//GEN-END:|70-getter|0|70-preInit
         // write pre-init user code here
         dynamicSendPeriod = new ChoiceGroup("", Choice.MULTIPLE);//GEN-BEGIN:|70-getter|1|70-postInit
         dynamicSendPeriod.append("dynamic send period", null);
         dynamicSendPeriod.setSelectedFlags(new boolean[] { false });//GEN-END:|70-getter|1|70-postInit
         // write post-init user code here
         dynamicSendPeriod.setSelectedIndex(0, settings_.getDynamicSendPeriod());
      }//GEN-BEGIN:|70-getter|2|
      return dynamicSendPeriod;
   }
   //</editor-fold>//GEN-END:|70-getter|2|

  
   /**
    * Returns a display instance.
    * @return the display instance.
    */
   public Display getDisplay()
   {
      return Display.getDisplay(this);
   }

   /**
    * Exits MIDlet.
    */
   public void exitMIDlet()
   {
      switchDisplayable(null, null);
      destroyApp(true);
      notifyDestroyed();
   }

   /**
    * Called when MIDlet is started.
    * Checks whether the MIDlet have been already started and initialize/starts or resumes the MIDlet.
    */
   public void startApp()
   {
      if (midletPaused)
      {
         resumeMIDlet();
      } else
      {
         initialize();
         startMIDlet();
      }
      midletPaused = false;
   }

   /**
    * Called when MIDlet is paused.
    */
   public void pauseApp()
   {
      midletPaused = true;
   }

   /**
    * Called to signal the MIDlet to terminate.
    * @param unconditional if true, then the MIDlet has to be unconditionally terminated and all resources has to be released.
    */
   public void destroyApp(boolean unconditional)
   {
   }

   public void onState(int state, String txt)
   {
      try
      {
         switch (state)
         {
            case StateEnum.ServerConnecting:
               strConnStatus.setText("connecting... " + txt);
               break;
            case StateEnum.ServerConnected:
               strConnStatus.setText("connected to " + txt);
               break;
            case StateEnum.ServerDisconnected:
               strConnStatus.setText("disconnected. " + txt);
               break;
            case StateEnum.ServerAuthCheck:
               strDeviceID.setText("checking... " + txt);
               break;
            case StateEnum.ServerAuthOk:
               settings_.reload();
               strDeviceID.setText(settings_.getDeviceID() + " " + txt);
               break;
            case StateEnum.DeviceWrongDID:
               strDeviceID.setText("wrong DID. " + txt);
               break;
            case StateEnum.DeviceAlreadyInUse:
               strDeviceID.setText("already in use. " + txt);
               break;
            case StateEnum.GpsConnecting:
               strGPSStatus.setText("searching... " + txt);
               break;
            case StateEnum.GpsConnected:
               strGPSStatus.setText("connected. " + txt);
               break;
            case StateEnum.GpsNotFound:
               strGPSStatus.setText("not found. " + txt);
               break;
            case StateEnum.GpsDisconnected:
               strGPSStatus.setText("disconnected. " + txt);
               break;
         }
      } catch (Exception ex)
      {
         onError(ex.toString());
      }
   }

   public void onError(String err)
   {
      Alert a = getErrorBox();
      if (err != null)
      {
         a.setString("ERR: " + err);
      }
      else
      {
         a.setString("ERR: <none>");
      }
      switchDisplayable(a, getMainWindow());
   }

   public void onBytesInOut(int in, int out)
   {
      bytesIn_ += in;
      bytesOut_ += out;
      strBytesInOut.setText(String.valueOf(bytesIn_) + '/' + String.valueOf(bytesOut_));
   }

   public void itemStateChanged(Item item)
   {
      if (item == choiceGPS)
      {
         if (((Choice)item).getSelectedIndex() == Settings.GPSSource.Internal)
         {
            txtGpsDeviceName.setConstraints(TextField.ANY | TextField.UNEDITABLE);
         }
         else
         {
            txtGpsDeviceName.setConstraints(TextField.ANY);
         }
      }
      if (item == dynamicSendPeriod)
      {
         if (((Choice)item).isSelected(0))
         {
            txtSendPeriod.setConstraints(TextField.NUMERIC | TextField.UNEDITABLE);
         }
         else
         {
            txtSendPeriod.setConstraints(TextField.NUMERIC);
         }
      }
   }
   public void commandAction(Command c, Item i)
   {
      
   }

   private void reloadSettings()
   {
      try
      {
         settings_.reload();
         txtDeviceID.setString(settings_.getDeviceID());
         txtGpsDeviceName.setString(settings_.getGpsDeviceName());
         txtSendPeriod.setString(String.valueOf(settings_.getSendPeriod()));
         txtServerName.setString(settings_.getServerName());
         txtServerPort.setString(String.valueOf(settings_.getServerPort()));
      }
      catch(Exception ex)
      {
         onError(ex.toString());
      }
   }
}
