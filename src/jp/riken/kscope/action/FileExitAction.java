/*
 * K-scope
 * Copyright 2012-2013 RIKEN, Japan
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
package jp.riken.kscope.action;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import javax.swing.JOptionPane;
import jp.riken.kscope.Message;
import jp.riken.kscope.service.AppController;

/**
 * End action
 *
 * @author RIKEN
 */
public class FileExitAction extends ActionBase implements WindowListener {

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public FileExitAction(AppController controller) {
    super(controller);
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    // Get the parent Frame.
    Frame frame = getWindowAncestor(event);

    // Display a confirmation message and exit the application.
    exitApplication(frame);
  }

  /**
   * Display a confirmation message and exit the application.
   *
   * @param frame Parent frame
   */
  public void exitApplication(Frame frame) {

    // Display a confirmation message.
    int option =
        JOptionPane.showConfirmDialog(
            frame,
            Message.getString("fileexitaction.exit.dialog.message"), // Do you want to quit K-scope?
            Message.getString("fileexitaction.exit.dialog.title"), // End of K-scope
            JOptionPane.OK_CANCEL_OPTION,
            JOptionPane.WARNING_MESSAGE);
    if (option != JOptionPane.OK_OPTION) {
      return;
    }

    // Exit the application
    System.exit(0);
  }

  /**
   * Pre-window event
   *
   * @param event Event information
   */
  @Override
  public void windowClosing(WindowEvent event) {

    Frame frame = (Frame) event.getWindow();

    // Display a confirmation message and exit the application.
    exitApplication(frame);
  }

  /**
   * Window open event
   *
   * @param event Event information
   */
  @Override
  public void windowOpened(WindowEvent event) {}

  /**
   * Window exit event
   *
   * @param event Event information
   */
  @Override
  public void windowClosed(WindowEvent event) {}

  /**
   * Window iconization event
   *
   * @param event Event information
   */
  @Override
  public void windowIconified(WindowEvent event) {}

  /**
   * Window iconization event
   *
   * @param event Event information
   */
  @Override
  public void windowDeiconified(WindowEvent event) {}

  /**
   * Window active event
   *
   * @param event Event information
   */
  @Override
  public void windowActivated(WindowEvent event) {}

  /**
   * Window inactivity event
   *
   * @param event Event information
   */
  @Override
  public void windowDeactivated(WindowEvent event) {}
}
