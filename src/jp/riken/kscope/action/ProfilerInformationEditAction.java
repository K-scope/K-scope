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
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.gui.IAnalisysComponent;
import jp.riken.kscope.gui.ProfilerTablePanel;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.Program;
import jp.riken.kscope.language.utils.LanguageUtils;
import jp.riken.kscope.profiler.ProfilerBaseData;
import jp.riken.kscope.service.AppController;

/**
 * Additional information addition action class for profiler information
 *
 * @author RIKEN
 */
public class ProfilerInformationEditAction extends ActionBase {

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public ProfilerInformationEditAction(AppController controller) {
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
    return isSelectedInformation();
  }

  /**
   * Additional information editing event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    // Execution check
    if (!validateAction()) return;

    // Status message
    final String message = Message.getString("mainmenu.edit.info"); // "Edit additional information"
    Application.status.setMessageMain(message);

    // Get additional selection information
    IAnalisysComponent panel =
        this.controller.getMainframe().getPanelAnalysisView().getSelectedPanel();
    if (!(panel instanceof ProfilerTablePanel)) return;
    ProfilerBaseData value = ((ProfilerTablePanel) panel).getModel().getSelectedInfo();
    if (value == null) return;
    // Get additional profiler information
    String addinfo = ((ProfilerTablePanel) panel).getModel().getSelectedText();

    // Edit additional information
    if (!editInformations(value, addinfo)) {
      // Cancel editing of additional information
      Application.status.setMessageMain(message + Message.getString("action.common.cancel.status"));
      return;
    }

    // Activate the additional information panel
    this.controller
        .getMainframe()
        .getPanelAnalysisView()
        .setSelectedPanel(ANALYSIS_PANEL.INFORMATION);
    Application.status.setMessageMain(message + Message.getString("action.common.done.status"));
    return;
  }

  /**
   * Get additional selection information
   *
   * @return Selectable additional information
   */
  private boolean isSelectedInformation() {
    IAnalisysComponent panel =
        this.controller.getMainframe().getPanelAnalysisView().getSelectedPanel();
    if (!(panel instanceof ProfilerTablePanel)) return false;
    ProfilerBaseData value = ((ProfilerTablePanel) panel).getModel().getSelectedInfo();
    if (value == null) return false;

    IInformation info = getInformationBlock(value);
    if (info == null) return false;
    return true;
  }

  /**
   * Edit additional information
   *
   * @param infoNodes Additional information range
   * @param addText Additional additional information
   * @return Whether to edit additional information
   */
  public boolean editInformations(IInformation[] infoNodes, String addText) {
    if (infoNodes == null) return false;
    // Get the additional information class from multiple ranges of additional information.
    IInformation info = getProgramInformation(infoNodes);

    // Display additional information dialog
    EditInformationEditAction action =
        new EditInformationEditAction(this.controller, FRAME_VIEW.ANALYSIS_VIEW);
    return action.editInformation(info, addText);
  }

  /**
   * Get the additional information class from multiple ranges of additional information. Generate
   * and return InformationBlock of additional information range in Program class. <br>
   * If there is only one additional information range, the first additional information is
   * returned.
   *
   * @param infos Additional information range
   * @return Additional information
   */
  private IInformation getProgramInformation(IInformation[] infos) {
    if (infos == null) return null;
    Program fortran = this.controller.getFortranLanguage();
    if (fortran == null) return null;
    if (infos.length == 0) return null;
    // single
    if (infos.length == 1) {
      return infos[0];
    }
    // Multiple ranges
    IInformation start = infos[0];
    IInformation end = infos[infos.length - 1];
    if (start != null && end != null) {
      IInformation info = fortran.getInformation(start, end);
      return info;
    }

    return start;
  }

  /**
   * Edit additional information
   *
   * @param value Profiler data
   * @param text Additional additional information
   * @return Whether to edit additional information
   */
  public boolean editInformations(ProfilerBaseData value, String text) {
    IInformation info = getInformationBlock(value);
    if (info == null) return false;

    // Display additional information dialog
    EditInformationEditAction action =
        new EditInformationEditAction(this.controller, FRAME_VIEW.ANALYSIS_VIEW);
    return action.editInformation(info, text);
  }

  /**
   * Get additional information block from profiler data.
   *
   * @param value Profiler data
   * @return Additional information block
   */
  private IInformation getInformationBlock(ProfilerBaseData value) {
    if (value == null) return null;
    // Code line information
    CodeLine line = value.getCodeLine();
    IBlock block = value.getBlock();
    if (block == null) {
      LanguageUtils utils = new LanguageUtils(this.controller.getFortranLanguage());
      IBlock[] blocks = utils.getCodeLineBlocks(line);
      if (blocks == null || blocks.length <= 0) return null;
      if (!(blocks[0] instanceof IInformation)) return null;
      block = blocks[0];
      value.setBlock(block);
    }
    if (block == null) return null;
    IInformation info = (IInformation) block;

    return info;
  }
}
