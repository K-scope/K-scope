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
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.LanguageService;

/**
 * Action class to open a new structure tree
 *
 * @author RIKEN
 */
public class ViewOpenLanguageTreeAction extends ActionBase {

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public ViewOpenLanguageTreeAction(AppController controller) {
    super(controller);
  }

  /**
   * Check if the action is executable. <br>
   * Check before executing the action and switch the menu enable. <br>
   *
   * @return true = Action can be executed
   */
  @Override
  public boolean validateAction() {

    // Get the currently selected node
    IBlock[] blocks = this.controller.getMainframe().getPanelExplorerView().getSelectedBlocks();
    if (blocks == null) return false;
    IBlock selectedProcedure = null;
    for (IBlock block : blocks) {
      if (block instanceof Procedure) {
        selectedProcedure = block;
        break;
      }
    }
    if (selectedProcedure == null) return false;

    return true;
  }

  /**
   * Open the structure tree
   *
   * @param block New root block
   */
  public void openLanguageTree(IBlock block) {
    // Only Procedure can be the root block
    if (!(block instanceof Procedure)) {
      return;
    }

    // Fortran database
    Fortran fortran = this.controller.getFortranLanguage();
    // Error information model
    ErrorInfoModel errorModel = this.controller.getErrorInfoModel();
    // Generate a new structure tree model
    LanguageTreeModel languageModel = new LanguageTreeModel();

    // Structural analysis service
    LanguageService service = new LanguageService(fortran);
    // Set the structure tree model
    service.setLanguageTreeModel(languageModel);
    // Set the error information model.
    service.setErrorInfoModel(errorModel);

    // Generate a structure tree
    if (!service.writeTree(block)) {
      return;
    }

    // Create a new tree tab with the structure tree model that has already been generated.
    this.controller.getMainframe().getPanelExplorerView().viewLanguageTree(languageModel);

    // Apply the structure tree filter
    this.controller.applyLanguageTreeFilter();

    return;
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {
    // Status bar
    Application.status.setMessageMain(
        Message.getString("mainmenu.view.newtree")); // New structure tree

    // Get the currently selected node
    IBlock[] blocks = this.controller.getMainframe().getPanelExplorerView().getSelectedBlocks();
    if (blocks == null) return;
    IBlock selectedProcedure = null;
    for (IBlock block : blocks) {
      if (block instanceof Procedure) {
        selectedProcedure = block;
        break;
      }
    }
    if (selectedProcedure == null) return;

    // open the structure tree
    openLanguageTree(selectedProcedure);
  }
}
