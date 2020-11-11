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

import java.awt.event.ActionEvent;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.service.AppController;

/**
 * View-Action to close file
 *
 * @author RIKEN
 */
public class ViewCloseAction extends ActionBase {

  /** Flag to close all file tabs */
  private boolean closeAll = false;

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public ViewCloseAction(AppController controller) {
    super(controller);
  }

  /**
   * Constructor
   *
   * @param controller Application controller
   * @param all true = Close all source files tab
   */
  public ViewCloseAction(AppController controller, boolean all) {
    super(controller);
    this.closeAll = all;
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    // Status bar display message
    String message = null;
    // Close the source file tab
    if (this.closeAll) {
      // Close all source files tab
      this.controller.getMainframe().getPanelSourceView().closeAllTabs();
      message = Message.getString("mainmenu.view.close-all-file"); // close all
    } else {
      // Close the active source file tab
      this.controller.getMainframe().getPanelSourceView().closeTabComponent();
      message = Message.getString("mainmenu.view.closefile"); // close the file
    }

    Application.status.setMessageMain(message);
  }
}
