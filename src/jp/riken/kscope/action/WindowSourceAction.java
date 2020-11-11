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
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.service.AppController;

/**
 * Display the source panel.
 *
 * @author RIKEN
 */
public class WindowSourceAction extends ActionBase {

  /** Source file to display */
  private SourceFile source;

  /**
   * Constructor
   *
   * @param controller Application controller
   * @param source Display source file
   */
  public WindowSourceAction(AppController controller, SourceFile source) {
    super(controller);
    this.source = source;
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    try {
      // Display the specified source file.
      this.controller.getMainframe().getPanelSourceView().viewSource(this.source);
    } catch (Exception ex) {
      ex.printStackTrace();
    }
  }
}
