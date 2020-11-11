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
import java.util.List;
import javax.swing.JTable;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.information.InformationBlock;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.utils.LanguageUtils;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Action class that opens the relevant part of the analysis result
 *
 * @author RIKEN
 */
public class ViewOpenAnalysisLineAction extends ActionBase implements MouseListener {

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public ViewOpenAnalysisLineAction(AppController controller) {
    super(controller);
  }

  /** Open the relevant part of the analysis result */
  private void openAnalysisLine() {
    // Get selected block information
    IBlock block = this.controller.getMainframe().getPanelAnalysisView().getSelectedBlock();
    // Select the relevant part of the structure tree and source view of the selected block.
    viewSelectedBlock(block);

    return;
  }

  /**
   * Open the search result tree path
   *
   * @param path Search result tree path
   */
  public void openSearchLine(TreePath path) {
    if (path == null) return;
    DefaultMutableTreeNode node = (DefaultMutableTreeNode) path.getLastPathComponent();
    if (node == null) return;
    CodeLine line = null;
    SourceFile file = null;
    if (node.getUserObject() instanceof CodeLine) {
      // Source code line
      line = (CodeLine) node.getUserObject();
      // For file search, the next higher node is the file node path
      node = (DefaultMutableTreeNode) node.getParent();
    } else if (node.getUserObject() instanceof SourceFile) {
      // If the node is SourceFile: Select Explorer node
      // create a tree path
      TreePath searchPath = SwingUtils.getTreePath(node);
      // Select a tree path
      this.controller.getMainframe().getPanelExplorerView().setSelectionPath(searchPath);

      // source file
      file = (SourceFile) node.getUserObject();
    }

    // If the node is a line of code
    if (line != null) {
      // Select the selection code line
      viewSourceLine(line);
    } else if (file != null) {
      // open the selected file
      viewSourceFile(file);
    }
    // If the node is a block
    else if (node.getUserObject() != null && node.getUserObject() instanceof IBlock) {
      // Put the selected node in the selected state
      viewSelectedPath(path);
    }

    // Highlight the search string in Source view
    this.controller.setSearchKeywords();
  }

  /**
   * Open the relevant part of the trace result
   *
   * @param block Trace result block
   */
  public void openTraceBlock(IBlock block) {
    // Select the relevant part of the structure tree and source view of the selected block.
    viewSelectedBlock(block);

    // Highlight trace variables in source view
    this.controller.setTraceKeywords();
  }

  /**
   * Select the specified block in the structure tree and the corresponding part in the source view.
   *
   * @param block selection block
   */
  public void viewSelectedBlock(IBlock block) {
    if (block == null) return;

    /********* Original code at 2012/03/21 by @hira
     * if (block instanceof InformationBlock) {
     * IInformation info = ((InformationBlock) block) .getStartBlock ();
     * if (info instanceof IBlock) {
     * block = (IBlock) info;
     * }
     * }
     *
     * // Select the structure tree
     * this.controller.getMainframe (). getPanelExplorerView (). setSelectedNode (block);
     *
     * // Select the selection code line
     * viewSourceBlock (block);
     **********************************************/

    /********* Provisional code: start at 2012/03/21 by @hira **********/
    IBlock[] blocks = new IBlock[2];
    if (block instanceof InformationBlock) {
      IInformation info = ((InformationBlock) block).getStartBlock();
      if (info instanceof IBlock) {
        blocks[0] = (IBlock) info;
      }
      info = ((InformationBlock) block).getEndBlock();
      if (info instanceof IBlock) {
        blocks[1] = (IBlock) info;
      }
    } else {
      blocks = null;
    }

    if (blocks == null) {
      // Select the structure tree
      this.controller.getMainframe().getPanelExplorerView().setSelectedNode(block);
    } else {
      // Select the structure tree
      this.controller
          .getMainframe()
          .getPanelExplorerView()
          .setSelectedNodeArea(blocks[0], blocks[1]);
    }

    // Select the selection code line
    viewSourceBlock(block);

    /********* Provisional code: end at 2012/03/21 by @hira **********/

  }

  /**
   * Select the specified block in the source view.
   *
   * @param code Selection code range
   */
  public void viewSelectedSourceBlock(CodeLine code) {
    // Open the file from the selected source code line information
    try {
      this.controller.openSourceFile(code);
    } catch (Exception ex) {
      this.controller.getErrorInfoModel().addErrorInfo(code, ex.getMessage());
    }
    // Select the selection code line
    CodeLine[] lines = {code};
    this.controller.getMainframe().getPanelSourceView().setSelectedBlock(lines);
  }

  /**
   * Select the specified path in the structure tree and the corresponding part of the source view.
   *
   * @param path Specified path
   */
  public void viewSelectedPath(TreePath path) {
    if (path == null) return;

    // Select the structure tree
    this.controller.getMainframe().getPanelExplorerView().setSelectionPath(path);

    // If the node is a block
    DefaultMutableTreeNode node = (DefaultMutableTreeNode) path.getLastPathComponent();
    if (node.getUserObject() != null && node.getUserObject() instanceof IBlock) {
      IBlock block = (IBlock) node.getUserObject();
      // Select the selection code line
      viewSourceBlock(block);
    }
  }

  /**
   * Display the source code line of the specified block
   *
   * @param block block
   */
  public void viewSourceBlock(IBlock block) {

    if (block == null) return;

    // Selected source code line information
    CodeLine line = block.getStartCodeLine();
    if (line == null) return;

    // Open the file from the selected source code line information
    try {
      this.controller.openSourceFile(line);
    } catch (Exception ex) {
      ex.printStackTrace();
      String msg = ex.getMessage();
      if (msg == null) {
        msg = ex.toString();
      }
      this.controller.getErrorInfoModel().addErrorInfo(line, msg);
    }

    /********* Original code at 2012/03/21 by @hira
     * // Select the selection code line
     * CodeLine [] lines = {line};
     * this.controller.getMainframe (). getPanelSourceView (). setSelectedBlock (lines);
     *******************/

    /********* Provisional code: start at 2012/03/21 by @hira **********/
    // Select the selection code line
    CodeLine[] lines = {line};
    if (block instanceof InformationBlock) {
      CodeLine infostartlines = null;
      CodeLine infoendlines = null;
      IInformation info = ((InformationBlock) block).getStartBlock();
      if (info instanceof IBlock) {
        infostartlines = ((IBlock) info).getStartCodeLine();
      }
      info = ((InformationBlock) block).getEndBlock();
      if (info instanceof IBlock) {
        infoendlines = ((IBlock) info).getStartCodeLine();
      }
      // If the same file, start line <end line, highlight the end block consecutively from the
      // start block.
      CodeLine infoblock = null;
      if (infostartlines.getSourceFile() != null && infoendlines.getSourceFile() != null) {
        if (infostartlines.getSourceFile().equals(infoendlines.getSourceFile())
            && infostartlines.getStartLine() <= infoendlines.getEndLine()) {
          // CodeLine generation for block highlighting
          infoblock = new CodeLine(infostartlines);
          int endlineno = infostartlines.getEndLine();
          if (endlineno < infoendlines.getStartLine()) {
            endlineno = infoendlines.getStartLine();
          }
          if (endlineno < infoendlines.getEndLine()) {
            endlineno = infoendlines.getEndLine();
          }
          infoblock.setEndLine(infoendlines.getEndLine());
        }
      }
      if (infoblock != null) {
        lines = new CodeLine[] {infoblock};
      } else {
        // Highlight the start and end blocks separately
        lines = new CodeLine[] {infostartlines, infoendlines};
      }
    }
    this.controller.getMainframe().getPanelSourceView().setSelectedBlock(lines);

    /********* Provisional code: end at 2012/03/21 by @hira **********/

  }

  /** Clear the source code line of the specified block */
  public void clearSourceBlock() {
    this.controller.getMainframe().getPanelSourceView().clearSelectedBlock();

    return;
  }

  /**
   * Display the source code line of the specified block
   *
   * @param line Selected source code line
   */
  public void viewSourceLine(CodeLine line) {

    // Selected source code line
    if (line == null) return;

    // Open the file from the selected source code line information
    try {
      this.controller.openSourceFile(line);
    } catch (Exception ex) {
      this.controller.getErrorInfoModel().addErrorInfo(line, ex.getMessage());
    }

    // Select the selection code line
    this.controller.getMainframe().getPanelSourceView().setSelectedLine(line);

    LanguageUtils utils = new LanguageUtils(this.controller.getFortranLanguage());
    IBlock[] blocks = utils.getCodeLineBlocks(line);
    if (blocks != null && blocks.length > 0) {
      // Select the structure tree
      this.controller
          .getMainframe()
          .getPanelExplorerView()
          .setSelectedNode(blocks[blocks.length - 1]);
    }
  }

  /**
   * Display the specified source file
   *
   * @param file Selected source file
   */
  public void viewSourceFile(SourceFile file) {

    // Selected source file
    if (file == null) return;

    // Open the selected source file
    try {
      this.controller.openSourceFile(file);
    } catch (Exception ex) {
      this.controller.getErrorInfoModel().addErrorInfo(file, ex.getMessage());
    }
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {
    // open the selected file
    openAnalysisLine();
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
        openAnalysisLine();
      } else if (event.getSource() instanceof JTable) {
        // open the selected file
        openAnalysisLine();
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

  /**
   * Open the relevant part of the trace result
   *
   * @param blocks Trace result block list
   */
  public void openTraceBlocks(IBlock[] blocks) {
    if (blocks == null || blocks.length <= 0) return;

    // Select the structure tree
    this.controller.getMainframe().getPanelExplorerView().setSelectedBlocks(blocks);

    // Select the selection code line
    IBlock block = blocks[blocks.length - 1];
    viewSourceBlock(block);
  }

  /**
   * Select multiple specified blocks in the structure tree and the corresponding part of the source
   * view.
   *
   * @param blocks selection blocks
   */
  public void viewSelectedArea(IBlock[] blocks) {
    if (blocks == null) return;

    // Select the structure tree
    if (blocks.length > 1) {
      this.controller
          .getMainframe()
          .getPanelExplorerView()
          .setSelectedNodeArea(blocks[0], blocks[blocks.length - 1]);
    } else {
      this.controller.getMainframe().getPanelExplorerView().setSelectedNodes(blocks);
    }

    CodeLine blockcode = null;
    if (blocks.length > 0) {
      CodeLine start = new CodeLine(blocks[0].getStartCodeLine(), blocks[0].getEndCodeLine());
      CodeLine end =
          new CodeLine(
              blocks[blocks.length - 1].getStartCodeLine(),
              blocks[blocks.length - 1].getEndCodeLine());
      blockcode = new CodeLine(start, end);
    }
    if (blockcode != null) {
      // Open the file from the selected source code line information
      try {
        this.controller.openSourceFile(blockcode);
      } catch (Exception ex) {
        this.controller.getErrorInfoModel().addErrorInfo(blockcode, ex.getMessage());
      }
      // Select the block of the selected file
      CodeLine[] codes = {blockcode};
      this.controller.getMainframe().getPanelSourceView().setSelectedBlock(codes);
    }

    return;
  }

  /**
   * Select multiple specified blocks in the structure tree and the corresponding part of the source
   * view.
   *
   * @param areas selection block
   */
  public void viewSelectedAreas(List<IBlock[]> areas) {
    if (areas == null) return;

    // Select the structure tree
    for (int i = 0; i < areas.size(); i++) {
      IBlock[] blocks = areas.get(i);
      if (i == 0) {
        if (blocks.length > 1) {
          this.controller
              .getMainframe()
              .getPanelExplorerView()
              .setSelectedNodeArea(blocks[0], blocks[blocks.length - 1]);
        } else {
          this.controller.getMainframe().getPanelExplorerView().setSelectedNodes(blocks);
        }
      } else {
        if (blocks.length > 1) {
          this.controller
              .getMainframe()
              .getPanelExplorerView()
              .addSelectedNodeArea(blocks[0], blocks[blocks.length - 1]);
        } else {
          this.controller.getMainframe().getPanelExplorerView().addSelectedNodes(blocks);
        }
      }
    }

    // Highlight the source view.
    List<CodeLine> listcode = new ArrayList<CodeLine>();
    for (IBlock[] blocks : areas) {
      CodeLine blockcode = null;
      if (blocks != null && blocks.length > 0) {
        CodeLine start = new CodeLine(blocks[0].getStartCodeLine(), blocks[0].getEndCodeLine());
        CodeLine end = start;
        if (blocks[blocks.length - 1] != null) {
          end =
              new CodeLine(
                  blocks[blocks.length - 1].getStartCodeLine(),
                  blocks[blocks.length - 1].getEndCodeLine());
        }
        blockcode = new CodeLine(start, end);
        listcode.add(blockcode);
      }
    }

    if (listcode.size() > 0) {
      for (CodeLine code : listcode) {
        // Open the file from the selected source code line information
        try {
          this.controller.openSourceFile(code);
        } catch (Exception ex) {
          this.controller.getErrorInfoModel().addErrorInfo(code, ex.getMessage());
        }
      }
      // Select the block of the selected file
      CodeLine[] codes = listcode.toArray(new CodeLine[0]);
      this.controller.getMainframe().getPanelSourceView().setSelectedBlock(codes);
    }

    return;
  }
}
