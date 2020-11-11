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
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.service.AppController;

/**
 * Clipboard copy action class
 *
 * @author RIKEN
 */
public class EditClipboardCopyAction extends ActionBase {

  /** Clipboard copy destination view */
  private FRAME_VIEW view;

  /**
   * Constructor
   *
   * @param controller Application controller
   * @param view Clipboard copy view
   */
  public EditClipboardCopyAction(AppController controller, FRAME_VIEW view) {
    super(controller);
    this.view = view;
  }

  /**
   * Clipboard copy event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    if (view == FRAME_VIEW.SOURCE_VIEW) {
      // Copy from Source View to Clipboard
      this.controller.getMainframe().getPanelSourceView().copyClipboard();
    } else if (view == FRAME_VIEW.ANALYSIS_VIEW) {
      // Copy from analysis view to clipboard
      this.controller.getMainframe().getPanelAnalysisView().copyClipboard();
    }
    return;
  }
}
