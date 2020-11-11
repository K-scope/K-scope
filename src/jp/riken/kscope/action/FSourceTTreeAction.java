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
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeModel;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.UseState;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.service.AppController;

/**
 * Reverse lookup function to Filtered-AST
 *
 * @author ohichi
 */
public class FSourceTTreeAction extends ActionBase implements MouseListener {

  /** File open action * */
  ViewOpenExploreBlockAction action;

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public FSourceTTreeAction(AppController controller) {
    super(controller);
    action = new ViewOpenExploreBlockAction(this.controller);
  }

  /**
   * Check if the action is executable. <br>
   * Check before executing the action and switch the menu enable. <br>
   *
   * @return true = Action can be executed
   */
  @Override
  public boolean validateAction() {

    // Get the selection tree model
    TreeModel modelTree = this.controller.getMainframe().getPanelExplorerView().getTreeModel();
    if (modelTree == null) {
      return false;
    }
    return true;
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {
    // Status message
    final String message = Message.getString("mainmenu.edit.click");
    Application.status.setMessageMain(message);

    getSelectedLine();
  }

  /** Get the selected row and search for the corresponding node */
  private void getSelectedLine() {
    // Execution check
    if (!validateAction()) return;
    CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedCodeLine();
    EXPLORE_PANEL view =
        this.controller.getMainframe().getPanelExplorerView().getSelectedPanel().getEnumPanel();

    if (view == EXPLORE_PANEL.LANGUAGE) {
      // Get the selection tree model
      TreeModel modelTree = this.controller.getMainframe().getPanelExplorerView().getTreeModel();
      // Get the root node
      DefaultMutableTreeNode root = (DefaultMutableTreeNode) modelTree.getRoot();
      if (root != null) {
        searchLanguageNode(line, root);
      }
    } else if (view == EXPLORE_PANEL.MODULE) {
      // Get the selection tree model
      TreeModel modelTree = this.controller.getMainframe().getPanelExplorerView().getTreeModel();
      // Get the root node
      DefaultMutableTreeNode root = (DefaultMutableTreeNode) modelTree.getRoot();
      if (root != null) {
        searchModuleNode(line, root);
      }
    }
  }

  /**
   * Node search on the Program Structure tab
   *
   * @param select selection code line information
   * @param node Search node
   * @return true = Finding a corresponding node
   */
  private boolean searchLanguageNode(CodeLine select, DefaultMutableTreeNode node) {
    int i, n;
    int start_l = select.getStartLine();
    String f_name = select.getSourceFile().getFile().getName();

    if (node.getUserObject() instanceof Block) {
      // Get code information for search node
      CodeLine line = ((Block) node.getUserObject()).get_start().getLineInfo();
      // File name comparison
      if (line.getSourceFile().getFile().getName().compareTo(f_name) == 0) {
        // If the line numbers are the same
        if (line.getStartLine() <= start_l && start_l <= line.getEndLine()) {
          n = node.getChildCount();
          for (i = 0; i < n; i++)
            if (searchLanguageNode(select, (DefaultMutableTreeNode) node.getChildAt(i))) {
              return true;
            }
          viewSelectedBlock(node);
          return true;
        }
      }
    }

    n = node.getChildCount();
    for (i = 0; i < n; i++)
      if (searchLanguageNode(select, (DefaultMutableTreeNode) node.getChildAt(i))) return true;
    return false;
  }

  /**
   * Node search on the module tab
   *
   * @param select selection code line information
   * @param node Search node
   * @return true = Finding a corresponding node
   */
  private boolean searchModuleNode(CodeLine select, DefaultMutableTreeNode node) {
    int i, n;
    int start_l = select.getStartLine();
    String f_name = select.getSourceFile().getFile().getName();

    if (node.getUserObject() instanceof UseState) {
      // Get code information for search node
      CodeLine line = ((UseState) node.getUserObject()).get_start().getLineInfo();
      // File name comparison
      if (line.getSourceFile() != null)
        if (line.getSourceFile().getFile().getName().compareTo(f_name) == 0) {
          // If the line numbers are the same
          if (line.getStartLine() == start_l) {
            viewSelectedBlock(node);
            return true;
          }
        }
    } else if (node.getUserObject() instanceof ProgramUnit) {
      CodeLine line = ((ProgramUnit) node.getUserObject()).getStartCodeLine();
      if (line != null && line.getSourceFile() != null) { // Excluding NO_Module
        if (line.getSourceFile().getFile().getName().compareTo(f_name) == 0) {
          if (line.getStartLine() == start_l || line.getEndLine() == start_l) {
            viewSelectedBlock(node);
            return true;
          }
        }
      }
    } else if (node.getUserObject() instanceof VariableDefinition) {
      CodeLine line = ((VariableDefinition) node.getUserObject()).getStartCodeLine();
      if (line.getSourceFile() != null)
        if (line.getSourceFile().getFile().getName().compareTo(f_name) == 0)
          if (line.getStartLine() == start_l) {
            selectDef(line, (DefaultMutableTreeNode) node.getParent(), start_l);
            return true;
          }
    }

    n = node.getChildCount();
    for (i = 0; i < n; i++)
      if (searchModuleNode(select, (DefaultMutableTreeNode) node.getChildAt(i))) return true;
    return false;
  }

  /**
   * Select the target node
   *
   * @param node Selected node
   */
  private void viewSelectedBlock(DefaultMutableTreeNode node) {
    DefaultMutableTreeNode b_node =
        this.controller.getMainframe().getPanelExplorerView().getSelectedNode();

    if (node.getUserObject() instanceof ProcedureUsage) {
      // If you select the same node twice in a row
      if (b_node == node && node.getChildCount() == 1) {
        node = ((DefaultMutableTreeNode) node.getChildAt(0));
      }
    }

    this.controller.getMainframe().getPanelExplorerView().setSelectedNode(node);
    action.openFile();
  }

  /**
   * Select the target node (in the case of variable declaration)
   *
   * @param select_l Codeline information for the first variable
   * @param node Selected node
   * @param line_l Selected line number
   */
  private void selectDef(CodeLine select_l, DefaultMutableTreeNode parent, int line_n) {
    int n = parent.getChildCount();
    DefaultMutableTreeNode node;
    ArrayList<IBlock> list_b = new ArrayList<IBlock>();

    CodeLine[] line_c = {select_l};
    this.controller.setSelectedBlock(line_c);

    // Add the corresponding node to the list
    for (int i = 0; i < n; i++) {
      node = (DefaultMutableTreeNode) parent.getChildAt(i);
      if (node.getUserObject() instanceof VariableDefinition) {
        CodeLine line = ((VariableDefinition) node.getUserObject()).getStartCodeLine();
        if (line.getStartLine() == line_n) list_b.add((IBlock) node.getUserObject());
      }
    }
    this.controller
        .getMainframe()
        .getPanelExplorerView()
        .setSelectedNodes(list_b.toArray(new IBlock[0]));
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
      getSelectedLine();
    }
  }

  @Override
  public void mousePressed(MouseEvent e) {}

  @Override
  public void mouseReleased(MouseEvent e) {}

  @Override
  public void mouseEntered(MouseEvent e) {}

  @Override
  public void mouseExited(MouseEvent e) {}
}
