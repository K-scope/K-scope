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
import java.io.File;
// import java.util.List;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.EXPLORE_PANEL;
// import jp.riken.kscope.information.InformationBlock;
// import jp.riken.kscope.information.TextInfo;
import jp.riken.kscope.language.IInformation;
// import jp.riken.kscope.language.Program;
import jp.riken.kscope.language.utils.InformationEntry;
import jp.riken.kscope.language.utils.LanguageVisitor;
// import jp.riken.kscope.language.utils.VariableMemoryEntry;
import jp.riken.kscope.model.InformationModel;
import jp.riken.kscope.service.AppController;

/**
 * Explorer tree change action class
 *
 * @author RIKEN
 */
public class ExploreTreeChangeAction extends ActionBase
    implements TreeSelectionListener, ChangeListener {

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public ExploreTreeChangeAction(AppController controller) {
    super(controller);
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {
    // Display information for the selected tree node
    viewSelectedNodeInfo();
  }

  /**
   * Tree selection change event
   *
   * @param event Event information
   */
  @Override
  public void valueChanged(TreeSelectionEvent event) {
    if (event.getNewLeadSelectionPath() == null) return;

    // Display information for the selected tree node
    viewSelectedNodeInfo();
  }

  /**
   * Tab selection change event
   *
   * @param event Event information
   */
  @Override
  public void stateChanged(ChangeEvent event) {
    // Display information for the selected tree node
    viewSelectedNodeInfo();
  }

  /** Display information about the selected tree node */
  public void viewSelectedNodeInfo() {
    EXPLORE_PANEL panel =
        this.controller.getMainframe().getPanelExplorerView().getSelectedEnumPanel();

    // Structure tree or module tree
    if (panel == EXPLORE_PANEL.LANGUAGE || panel == EXPLORE_PANEL.MODULE) {
      // Structure tree, module tree property setting actions
      LanguagePropertiesAction action = new LanguagePropertiesAction(this.controller);
      // Set the property.
      action.setProperties();

      if (this.controller.getMainframe().getPanelAnalysisView().getSelectedPanel() != null
          && this.controller.getMainframe().getPanelAnalysisView().getSelectedPanel().getEnumPanel()
              == ANALYSIS_PANEL.INFORMATION) {
        // Display additional information
        setInformation();
      }
    } else if (panel == EXPLORE_PANEL.SOURCE || panel == EXPLORE_PANEL.XML) {
      // File property setting action
      FilePropertiesAction action = new FilePropertiesAction(this.controller);
      // Set the property.
      action.setProperties();
    }
  }

  /** Display additional information. */
  public void setInformation() {

    // Project folder
    File projectFolder = this.controller.getProjectModel().getProjectFolder();

    // Selected structure tree node
    DefaultMutableTreeNode[] nodes =
        this.controller.getMainframe().getPanelExplorerView().getSelectedNodes();
    if (nodes == null) return;
    // If it is a root node, make it a child node
    if (nodes.length == 1 && nodes[0].isRoot()) {
      Object obj = nodes[0].getUserObject();
      if (obj != null && !(obj instanceof IInformation)) {
        DefaultMutableTreeNode[] newnodes = new DefaultMutableTreeNode[nodes[0].getChildCount()];
        for (int i = 0; i < nodes[0].getChildCount(); i++) {
          newnodes[i] = (DefaultMutableTreeNode) nodes[0].getChildAt(i);
        }
        nodes = newnodes;
      }
    }

    // Additional information panel model settings
    InformationModel infoModel =
        this.controller.getMainframe().getPanelAnalysisView().getPanelInformation().getModel();
    // Project folder settings
    infoModel.setProjectFolder(projectFolder);

    // Clear the model
    infoModel.clearInformation();

    // Add additional information
    for (DefaultMutableTreeNode node : nodes) {
      Object obj = node.getUserObject();
      if (obj == null) continue;
      if (!(obj instanceof IInformation)) continue;

      // Search for additional information
      InformationEntry entry = new InformationEntry(this.controller.getFortranLanguage());
      LanguageVisitor visitor = new LanguageVisitor(entry);
      visitor.entryInformation((IInformation) obj);
      IInformation[] infos = entry.getListInformation();
      if (infos != null) {
        for (IInformation info : infos) {
          // Additional information settings
          infoModel.setTitle(info.toString());
          infoModel.addInformation(info);
        }
      }
    }
  }
}
