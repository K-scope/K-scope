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

import java.awt.Component;
import java.awt.Frame;
import java.awt.HeadlessException;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.service.AppController;

/**
 * Action to open the error location
 *
 * @author RIKEN
 */
public class ErrorOpenFileAction extends ActionBase implements MouseListener {

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public ErrorOpenFileAction(AppController controller) {
    super(controller);
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    // Get the parent Frame.
    Frame frame = getWindowAncestor(event);

    // Open the error part
    openErrorFile(frame);
  }

  /**
   * Open the error location
   *
   * @param parent parent component
   */
  private void openErrorFile(Component parent) {

    CodeLine line = null;
    try {
      // Get the code information of the error part
      line = this.controller.getMainframe().getPanelAnalysisView().getSelectedCodeLine();
      // Error message
      String errorMessage =
          this.controller
              .getMainframe()
              .getPanelAnalysisView()
              .getPanelError()
              .getSelectedErrorMessage();
      if (line == null && errorMessage == null) return;

      if (line == null || line.getSourceFile() == null || line.getSourceFile().getFile() == null) {
        // Error message
        JOptionPane.showMessageDialog(
            parent,
            errorMessage,
            Message.getString("erroropenfileaction.errorinfo.dialog.title"), // Error information
            JOptionPane.INFORMATION_MESSAGE);
        return;
      }

      // Set the information of the error part and open it
      this.controller.openSourceFile(line);

      // Select the selection code line
      this.controller.getMainframe().getPanelSourceView().setSelectedLine(line);

    } catch (HeadlessException ex) {
      ex.printStackTrace();
    } catch (Exception ex) {
      ex.printStackTrace();
    }
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
      if (event.getSource() instanceof JTable) {
        // Open the error part
        openErrorFile(this.controller.getMainframe());
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
