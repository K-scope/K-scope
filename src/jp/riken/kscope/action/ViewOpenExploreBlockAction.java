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
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
// import java.io.File;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JOptionPane;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.service.AppController;

/**
 * Action class to open a file
 *
 * @author RIKEN
 */
public class ViewOpenExploreBlockAction extends ActionBase implements MouseListener {

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public ViewOpenExploreBlockAction(AppController controller) {
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
    // Get selected source code line information
    CodeLine[] line = this.controller.getMainframe().getPanelExplorerView().getSelectedCodeLines();
    if (line == null) return false;

    return true;
  }

  /**
   * Open selected file Changed from private to public for AllAnalysisMemoryAction class (2014/4/8
   * ohichi)
   */
  public void openFile() {

    // Get selected source code line information
    CodeLine[] line = this.controller.getMainframe().getPanelExplorerView().getSelectedCodeLines();
    if (line == null) return;

    // Open the file from the selected source code line information
    for (int i = 0; i < line.length; i++) {
      if (line[i].getSourceFile() == null || line[i].getSourceFile().getFile() == null) {
        Frame frame = this.controller.getMainframe();
        JOptionPane.showMessageDialog(
            frame,
            Message.getString(
                "viewopenexploreblockaction.errdialog.notsetsource.message",
                line[i].getStatement()), // The source file for is not set.
            Message.getString("dialog.common.error"), // error
            JOptionPane.ERROR_MESSAGE);
        continue;
      }

      try {
        this.controller.openSourceFile(line[i]);
      } catch (Exception ex) {
        ex.printStackTrace();
        this.controller.getErrorInfoModel().addErrorInfo(line[i], ex.toString());
      }
    }

    // Select the selection code line
    // Create a list of source files
    List<SourceFile> files = new ArrayList<SourceFile>();
    for (int i = 0; i < line.length; i++) {
      if (line[i].getSourceFile() == null) continue;
      if (line[i].getSourceFile().getFile() == null) continue;

      if (!files.contains(line[i].getSourceFile())) {
        files.add(line[i].getSourceFile());
      }
    }

    // Create a selection code line list
    for (SourceFile file : files) {
      List<CodeLine> lines = new ArrayList<CodeLine>();
      for (int i = 0; i < line.length; i++) {
        if (line[i].getSourceFile() == null) continue;
        if (file.equals(line[i].getSourceFile())) {
          lines.add(line[i]);
        }
      }
      if (lines.size() > 0) {
        // Select the selection code line
        this.controller.setSelectedBlock(lines.toArray(new CodeLine[0]));
      }
    }

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
    Application.status.setMessageMain(Message.getString("mainmenu.view.openfile")); // open the file
    // open the selected file
    openFile();
  }

  /**
   * Mouse click event
   *
   * @param event Event information
   */
  @Override
  public void mouseClicked(MouseEvent event) {
    // Double click check
    if (SwingUtilities.isLeftMouseButton(event) && event.getClickCount() == 2) {
      if (event.getSource() instanceof JTree) {
        // open the selected file
        openFile();
      }
    }
  }

  /**
   * Mouse button down event
   *
   * @param e Mouse event information
   */
  @Override
  public void mousePressed(MouseEvent e) {}

  /**
   * Mouse button up event
   *
   * @param e Mouse event information
   */
  @Override
  public void mouseReleased(MouseEvent e) {}

  /**
   * Mouseover event
   *
   * @param e Mouse event information
   */
  @Override
  public void mouseEntered(MouseEvent e) {}

  /**
   * Mouse out event
   *
   * @param e Mouse event information
   */
  @Override
  public void mouseExited(MouseEvent e) {}
}
