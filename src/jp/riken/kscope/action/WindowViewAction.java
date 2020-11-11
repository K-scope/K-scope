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
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.TraceResultModel;
import jp.riken.kscope.service.AppController;

/**
 * Show tab panel.
 *
 * @author RIKEN
 */
public class WindowViewAction extends ActionBase {

  /** Explorer panel to display */
  private EXPLORE_PANEL panelExplore;

  /** Analysis panel for display */
  private ANALYSIS_PANEL panelAnalysis;

  /** Structural panel model for display */
  private LanguageTreeModel modelLanguage;

  /** Trace panel model to display */
  private TraceResultModel modelTrace;

  /**
   * Constructor
   *
   * @param controller Application controller
   * @param panel Display analysis panel
   */
  public WindowViewAction(AppController controller, ANALYSIS_PANEL panel) {
    super(controller);
    this.panelAnalysis = panel;
    this.panelExplore = null;
  }

  /**
   * Constructor
   *
   * @param controller Application controller
   * @param panel Display explorer panel
   */
  public WindowViewAction(AppController controller, EXPLORE_PANEL panel) {
    super(controller);
    this.panelAnalysis = null;
    this.panelExplore = panel;
  }

  /**
   * Constructor
   *
   * @param controller Application controller
   * @param model Structural panel model to display
   */
  public WindowViewAction(AppController controller, LanguageTreeModel model) {
    super(controller);
    this.panelAnalysis = null;
    this.modelLanguage = model;
  }

  /**
   * Constructor
   *
   * @param controller Application controller
   * @param model Trace panel model to display
   */
  public WindowViewAction(AppController controller, TraceResultModel model) {
    super(controller);
    this.panelAnalysis = null;
    this.modelTrace = model;
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    // Display the structure panel
    if (this.modelLanguage != null) {
      this.controller.getMainframe().getPanelExplorerView().viewLanguageTree(this.modelLanguage);
    }
    // Show the trace panel
    else if (this.modelTrace != null) {
      this.controller.getMainframe().getPanelAnalysisView().viewAnalysisTrace(this.modelTrace);
    }
    // Display the explorer panel
    else if (this.panelExplore != null) {
      this.controller.getMainframe().getPanelExplorerView().setSelectedPanel(this.panelExplore);
    }
    // Display the analysis panel
    else if (this.panelAnalysis != null) {
      this.controller.getMainframe().getPanelAnalysisView().setSelectedPanel(this.panelAnalysis);
    }
  }
}
