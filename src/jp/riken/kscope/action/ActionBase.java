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

import java.awt.Component;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;
import jp.riken.kscope.service.AppController;

/**
 * Base action class
 *
 * @author RIKEN
 */
public abstract class ActionBase implements ActionListener {

  /** Application controller */
  protected AppController controller;

  /** Constructor */
  public ActionBase() {
    super();
  }

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public ActionBase(AppController controller) {
    super();
    this.controller = controller;
  }

  /**
   * Get the top JFrame.
   *
   * @param event Event information
   * @return mainframe
   */
  public Frame getWindowAncestor(ActionEvent event) {

    Component source = (Component) event.getSource();
    while (source instanceof JMenuItem) {
      //        if ( source instanceof JMenuItem ) {
      JMenuItem mi = (JMenuItem) source;
      if (mi.getParent() instanceof JPopupMenu) {
        JPopupMenu pm = (JPopupMenu) mi.getParent();
        source = pm.getInvoker();
      } else {
        break;
      }
    }
    JFrame frame = (JFrame) SwingUtilities.getWindowAncestor(source);
    return frame;
  }

  /**
   * Check if the action is executable. <br>
   * Check before executing the action and switch the menu enable. <br>
   *
   * @return true = Action can be executed
   */
  public boolean validateAction() {
    return true;
  }
}
