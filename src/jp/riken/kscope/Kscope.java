/*
 * K-scope
 * Copyright 2012-2015 RIKEN, Japan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package jp.riken.kscope;

import java.awt.EventQueue;
import java.awt.Toolkit;
import java.io.IOException;
import java.io.InputStream;
import javax.swing.JOptionPane;
import javax.swing.UIManager;
import javax.swing.plaf.metal.MetalLookAndFeel;
import jp.riken.kscope.gui.MainFrame;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.utils.Logger;
import jp.riken.kscope.utils.ResourceUtils;

/**
 * Main class
 *
 * @author RIKEN
 */
public class Kscope {

  /**
   * Main method
   *
   * @param args Start argument
   */
  public static void main(String args[]) {
    // Initial setting
    // Set system properties for Java runtime environment on MacOSX.
    boolean debug = (System.getenv("DEBUG") != null);
    String version = KscopeProperties.APPLICATION_VERSION;
    if (debug) {
      System.out.println("Java: " + System.getProperty("java.version"));
    }
    System.out.println(KscopeProperties.APPLICATION_NAME + " v" + version);
    if (debug) {
      System.out.println(
          "For debugging use DEBUG env var: "
              + "\"high\" - much debug info, "
              + "\"extreme\" - too much debug info,"
              + " any other value, but not empy - less debug info.\nCurrently DEBUG is set to \""
              + System.getenv("DEBUG")
              + "\"");
    }
    if (KscopeProperties.isMac()) {
      // Instead of adding a menu to JFrame, make it a screen menu at the top of the screen like a
      // general OSX application.
      System.setProperty("apple.laf.useScreenMenuBar", "true");
      System.setProperty("com.apple.macos.smallTabs", "true");
      try {
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
      } catch (Exception ex) {
        ex.printStackTrace();
        return;
      }
      // Set the application name displayed on the left end of the screen menu (If nothing is set,
      // it will be the class name)
      String title = Message.getString("application.name");
      System.setProperty("com.apple.mrj.application.apple.menu.about.name", title);
    } else if (KscopeProperties.isWindows()) {
      try {
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        MetalLookAndFeel.setCurrentTheme(new ThemeWindows());
        UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
      } catch (Exception ex) {
        ex.printStackTrace();
        return;
      }
    }
    // Not set for Linux.
    else {
      /*
      try {
                 String LandF;
                 LandF = UIManager.getSystemLookAndFeelClassName();
                 // Can set GTK Look and Feel, but it is very slow with X11
          // LandF = "com.sun.java.swing.plaf.gtk.GTKLookAndFeel";
                 UIManager.setLookAndFeel(LandF);
      }
      catch (UnsupportedLookAndFeelException ex) {
      	ex.printStackTrace();
             return;
      }
      catch (ClassNotFoundException ex) {
      	ex.printStackTrace();
             return;
      }
      catch (InstantiationException ex) {
      	ex.printStackTrace();
             return;
      }
      catch (IllegalAccessException ex) {
       	ex.printStackTrace();
             return;
      } */
    }

    Kscope app = new Kscope();
    app.initApp();

    // Mainframe display
    EventQueue.invokeLater(
        new Runnable() {
          @Override
          public void run() {
            try {
              showMainFrame();
            } catch (Exception e) {
              e.printStackTrace();
            }
          }
        });
  }

  /** Display the mainframe. */
  private static void showMainFrame() {

    try {
      // Create mainframe
      MainFrame frame = new MainFrame();

      // Create controller
      AppController ctrl = new AppController();

      // Mainframe initialization
      frame.initialize(ctrl);

      // Controller initialization
      ctrl.initialize(frame);

      String status = Message.getString("go.status.start");
      Application.status.setMessageMain(status);

      // Mainframe display
      frame.setVisible(true);

    } catch (Exception ex) {
      ex.printStackTrace();

      // Error message
      String title = Message.getString("go.message.start.error.title");
      String msg = ex.getMessage();
      if (msg == null) {
        msg = ex.toString();
      }
      JOptionPane.showMessageDialog(null, msg, title, JOptionPane.ERROR_MESSAGE);

      // end
      System.exit(-1);

      return;
    }
  }

  /** Initialize the application. */
  private void initApp() {
    // Initialize the log.
    // true = Debug mode log output (also outputs CONFIG level)
    inititalizeLogging(true);

    KscopeProperties.PROPERTIES_FILE = KscopeProperties.PROPERTIES_FILE_DEFAULT;

    try {
      ResourceUtils.setRootAppClass(this.getClass());
      ResourceUtils.addFile("resources/icons");
    } catch (IOException e) {
      JOptionPane.showMessageDialog(null, e, "Error", JOptionPane.ERROR_MESSAGE);
      e.printStackTrace();
    }

    // Read property file
    KscopeProperties.loadXml();
  }

  /**
   * Output log settings
   *
   * @param debugMode Output debug mode flag
   */
  private void inititalizeLogging(boolean debugMode) {
    // Log settings
    InputStream logProp = getClass().getResourceAsStream("logging.properties");

    String appName = this.getClass().getName();
    Logger.configure(logProp, appName, debugMode);

    // System property value output
    String[] props = {
      "java.version",
      "java.vm.version",
      "java.runtime.version",
      "java.vendor",
      "java.compiler",
      "os.name",
      "os.version",
      "os.arch",
      "user.home",
      "java.home",
      "java.class.path",
    };

    try {
      for (int i = 0; i < props.length; i++) {
        Logger.info(props[i] + "=" + System.getProperty(props[i]));
      }
    } catch (Exception e) {
      e.printStackTrace();
      Logger.logOff();
    }

    String desktopProps = "awt.multiClickInterval";
    Logger.info(desktopProps + "=" + Toolkit.getDefaultToolkit().getDesktopProperty(desktopProps));
  }
}
