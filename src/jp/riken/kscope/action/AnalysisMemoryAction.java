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

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.data.BlockList;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.dialog.RequiredBFDialog;
import jp.riken.kscope.dialog.VariableAccessDialog;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.utils.LanguageUtils;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.RequiredBFModel;
import jp.riken.kscope.properties.RequiredBFProperties;
import jp.riken.kscope.properties.VariableMemoryProperties;
import jp.riken.kscope.service.AnalysisMemoryService;
import jp.riken.kscope.service.AppController;

/**
 * Request Byte / FLOP action class
 *
 * @author RIKEN
 */
public class AnalysisMemoryAction extends ActionBase {
  /** Action mode */
  public enum ACTION_MODE {
    /** Variable access destination memory setting */
    ACCESS_SETTING,
    /** Request Byte / FLOP calculation */
    MEMORY_CALCULATE
  };

  /** Request Byte / FLOP action mode */
  private ACTION_MODE mode;
  /** Access destination variable acquisition destination view */
  private FRAME_VIEW view;

  /**
   * Constructor
   *
   * @param controller Application controller
   * @param mode Action mode
   * @param view Access destination variable acquisition destination view
   */
  public AnalysisMemoryAction(AppController controller, ACTION_MODE mode, FRAME_VIEW view) {
    super(controller);
    this.mode = mode;
    this.view = view;
  }

  /**
   * Check if the action is executable. <br>
   * Check before executing the action and switch the menu enable. <br>
   *
   * @return true = Action can be executed
   */
  @Override
  public boolean validateAction() {
    // Get the selected block
    IBlock[] blocks = getSelectedBlocks();
    if (blocks == null) return false;
    return true;
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    // Execution check
    if (!validateAction()) return;

    // Get the selected block
    IBlock[] blocks = getSelectedBlocks();
    if (blocks == null) return;

    // Clear the source code selection and change to the selected background color
    if (this.view == FRAME_VIEW.SOURCE_VIEW) {
      setSelectedBlockNoCaret();
    }
    // Status message
    String message = "";
    if (this.mode == ACTION_MODE.ACCESS_SETTING) {
      message = Message.getString("mainmenu.analysis.access");
    } else if (this.mode == ACTION_MODE.MEMORY_CALCULATE) {
      message = Message.getString("mainmenu.analysis.calculate");
    }
    Application.status.setMessageMain(message);

    // Get the parent Frame.
    Frame frame = getWindowAncestor(event);
    // Request Byte / FLOP configuration property
    RequiredBFProperties properitiesMemory = this.controller.getPropertiesMemory();
    // Variable access destination memory setting
    VariableMemoryProperties properitiesVariable = this.controller.getPropertiesVariable();
    // Memory access destination setting dialog
    VariableAccessDialog dialogVariable =
        new VariableAccessDialog(frame, true); // Memory access performance dialog
    RequiredBFDialog dialogPerformance = new RequiredBFDialog(frame, true);
    // Property settings
    dialogVariable.setPropertiesVariable(properitiesVariable);
    dialogVariable.setPropertiesMemoryband(properitiesMemory);
    dialogPerformance.setPropertiesMemoryband(properitiesMemory);
    dialogPerformance.setPropertiesVariable(properitiesVariable);
    // Select block
    dialogVariable.setSelectedblocks(blocks);
    dialogPerformance.setSelectedblocks(blocks);
    // Display dialog
    dialogVariable.setMemoryPerformanceDialog(dialogPerformance);
    dialogPerformance.setVariableAccessDialog(dialogVariable);

    // Request Byte / FLOP calculation service
    RequiredBFModel modelRequired = this.controller.getRequiredByteFlopModel();
    LanguageTreeModel modelLanguage = this.controller.getLanguageTreeModel();
    modelRequired.setModelLanguageTree(modelLanguage);
    AnalysisMemoryService serviceMemory = new AnalysisMemoryService();
    serviceMemory.setBlocks(blocks);
    serviceMemory.setProperitiesRequiredBF(properitiesMemory);
    serviceMemory.setPropertiesOperand(this.controller.getPropertiesOperation());
    serviceMemory.setModelRequiredBF(modelRequired);
    serviceMemory.setPropertiesVariableMemory(properitiesVariable);
    dialogPerformance.setServiceMemory(serviceMemory);

    if (this.mode == ACTION_MODE.ACCESS_SETTING) {
      // Display the variable access destination setting dialog.
      int result = dialogVariable.showDialog();
      if (result == Constant.CANCEL_DIALOG) {
        return;
      }
    } else if (this.mode == ACTION_MODE.MEMORY_CALCULATE) {
      // Display the memory access performance dialog class.
      int result = dialogPerformance.showDialog();
      if (result == Constant.CANCEL_DIALOG) {
        return;
      }
    }

    return;
  }

  /**
   * Get the selected block
   *
   * @return selection block
   */
  private IBlock[] getSelectedBlocks() {
    IBlock[] blocks = null;
    if (this.view == FRAME_VIEW.SOURCE_VIEW) {
      // Source view
      IBlock[] codeblocks = getSelectedCodeLines();
      if (codeblocks == null || codeblocks.length <= 0) return null;
      // From the source view, create a block list as the parent block.
      if (codeblocks.length > 1) {
        BlockList parent = new BlockList(codeblocks);
        blocks = new IBlock[codeblocks.length + 1];
        blocks[0] = parent;
        for (int i = 0; i < codeblocks.length; i++) {
          blocks[i + 1] = codeblocks[i];
        }
      } else {
        blocks = codeblocks;
      }
    } else {
      // Get the selected block from the structure explorer view
      blocks = this.controller.getMainframe().getPanelExplorerView().getSelectedBlocks();
    }
    if (blocks == null) return null;

    // Check IBlock
    List<IBlock> list = new ArrayList<IBlock>();
    for (IBlock block : blocks) {
      // Add something other than the declaration statement
      if (block.getBlockType() != BlockType.VARIABLEDEFINITION) {
        list.add(block);
      }
    }

    if (list.size() <= 0) return null;
    return list.toArray(new IBlock[0]);
  }

  /**
   * Get the selected block from the source view
   *
   * @return selection block
   */
  private IBlock[] getSelectedCodeLines() {

    // Get the source code selection
    CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedArea();
    LanguageUtils utils = new LanguageUtils(this.controller.getFortranLanguage());
    IBlock[] blocks = utils.getCodeLineBlocks(line);
    return blocks;
  }

  /** Clear the selection of the source code and change it to the selected background color. */
  private void setSelectedBlockNoCaret() {
    // Get the source code selection
    CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedArea();
    this.controller.getMainframe().getPanelSourceView().setSelectedBlockNoCaret(line);
  }
}
