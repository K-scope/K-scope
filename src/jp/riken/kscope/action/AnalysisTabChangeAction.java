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
// import java.io.File;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
// import javax.swing.tree.DefaultMutableTreeNode;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.gui.IAnalisysComponent;
import jp.riken.kscope.service.AppController;

/**
 * Analysis tab change action class
 *
 * @author RIKEN
 */
public class AnalysisTabChangeAction extends ActionBase implements ChangeListener {

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public AnalysisTabChangeAction(AppController controller) {
    super(controller);
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {
    // Display the keywords of the selected analysis tab in the source view
    changeAnalisysTab();
  }

  /**
   * Tab selection change event
   *
   * @param event Event information
   */
  @Override
  public void stateChanged(ChangeEvent event) {
    // Display information for the selected tree node
    changeAnalisysTab();
  }

  /**
   * Selective analysis view change event.
   *
   * <p>Display the keywords of the selected analysis tab in the source view. <br>
   * Set profile bar graph to source view. <br>
   * Display additional information. </ p>
   */
  public void changeAnalisysTab() {
    // Display the keywords of the selected analysis tab in the source view
    viewKeywardSource();
    // Set profile bar graph to source view
    setProfilerBargraph();
    // Display additional information.
    setInformation();
  }

  /** Set keywords on the Analysis tab to Source View */
  public void viewKeywardSource() {
    IAnalisysComponent panel =
        this.controller.getMainframe().getPanelAnalysisView().getSelectedPanel();
    if (panel == null) {
      return;
    }
    if (panel.getEnumPanel() == ANALYSIS_PANEL.SEARCHRESULT) {
      // Set search keywords
      this.controller.setSearchKeywords();
    } else if (panel.getEnumPanel() == ANALYSIS_PANEL.TRACE) {
      // Set the trace keyword
      this.controller.setTraceKeywords();
    }
  }

  /** Set profile bar graph to source view */
  public void setProfilerBargraph() {
    this.controller.setProfilerBargraph();

    return;
  }

  /** Display additional information. */
  public void setInformation() {

    IAnalisysComponent panel =
        this.controller.getMainframe().getPanelAnalysisView().getSelectedPanel();
    if (panel == null) {
      return;
    }
    if (panel.getEnumPanel() != ANALYSIS_PANEL.INFORMATION) {
      return;
    }

    // Explorer tree change action class
    ExploreTreeChangeAction action = new ExploreTreeChangeAction(this.controller);
    action.setInformation();
  }
}
