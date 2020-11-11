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
package jp.riken.kscope.gui;

import jp.riken.kscope.common.ANALYSIS_PANEL;

/**
 * Analytical Information Panel Basis Class
 *
 * @author RIKEN
 */
public abstract class AnalisysPanelBase extends javax.swing.JPanel implements ITabComponent {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Analysis information panel identifier */
  private ANALYSIS_PANEL enumPanel;

  /** Parent component */
  private ITabComponent parentCompornent = null;

  /**
   * Constructor
   *
   * @param panel Analysis information panel identifier
   */
  public AnalisysPanelBase(ANALYSIS_PANEL panel) {
    this.enumPanel = panel;
  }

  /** Constructor */
  public AnalisysPanelBase() {
    super();
  }

  /**
   * Set the analysis information panel identifier
   *
   * @return enumPanel Analysis Information Panel Identifier
   */
  public ANALYSIS_PANEL getEnumPanel() {
    return this.enumPanel;
  }

  /**
   * Get the parent component.
   *
   * @return Parent component
   */
  @Override
  public ITabComponent getParentComponent() {
    return this.parentCompornent;
  }

  /**
   * Set the parent component.
   *
   * @param component Parent component
   */
  @Override
  public void setParentComponent(ITabComponent component) {
    this.parentCompornent = component;
  }

  /** Close tab */
  @Override
  public void closeTabComponent() {
    // Close the tab with the parent tab pine.
    if (this.parentCompornent != null) {
      this.parentCompornent.closeTabComponent();
    }
  }

  /** Activate your own analytics panel. */
  public void setSelectedPanel() {
    if (this.parentCompornent instanceof AnalysisView) {
      AnalysisView view = (AnalysisView) this.parentCompornent;
      view.setSelectedPanel(this.enumPanel);
    }
  }
}
