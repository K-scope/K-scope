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
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.common.TRACE_DIR;
import jp.riken.kscope.gui.SearchResultPanel;
import jp.riken.kscope.model.SearchResultModel;
import jp.riken.kscope.service.AppController;

/**
 * Search action (move previous, move next, clear) action
 *
 * @author RIKEN
 */
public class SearchResultAction extends ActionBase {

  // Search direction
  private TRACE_DIR searchDir;

  /**
   * Constructor
   *
   * @param controller Application controller
   * @param dir Search direction
   */
  public SearchResultAction(AppController controller, TRACE_DIR dir) {
    super(controller);
    this.searchDir = dir;
  }

  /**
   * Check if the action is executable. <br>
   * Check before executing the action and switch the menu enable. <br>
   *
   * @return true = Action can be executed
   */
  @Override
  public boolean validateAction() {
    SearchResultModel model = this.controller.getSearchResultModel();
    String text = model.getSearchText();
    return (text != null && !text.isEmpty());
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    // Status bar
    final String message = Application.status.getMessageMain().split(":")[0];
    String actionMsg = null;

    if (!validateAction()) return;

    // Search result model
    SearchResultPanel panel =
        this.controller.getMainframe().getPanelAnalysisView().getPanelSearchResult();
    if (this.searchDir == TRACE_DIR.UP) {
      panel.moveUp();
      actionMsg = Message.getString("searchresultaction.backward.status"); // Forward
    } else if (this.searchDir == TRACE_DIR.DOWN) {
      panel.moveDown();
      actionMsg = Message.getString("searchresultaction.forward.status"); // next
    } else if (this.searchDir == TRACE_DIR.END) {
      panel.clearModel();

      // Reset the search string highlight in Source view
      this.controller.getMainframe().getPanelSourceView().clearSearchWords(KEYWORD_TYPE.SEARCH);
      actionMsg = Message.getString("searchresultaction.clear.status"); // clear
    } else if (this.searchDir == TRACE_DIR.REFRESH) {
      // Reset the search string highlight in Source view
      this.controller.setSearchKeywords();
      actionMsg = Message.getString("searchresultaction.refresh.status"); // update
    }

    // Activate the search results tab
    this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.SEARCHRESULT);
    Application.status.setMessageMain(message + ":" + actionMsg);
  }
}
