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
package jp.riken.kscope.service;

import java.util.ArrayList;
// import java.util.Collection;
import java.util.Enumeration;
import java.util.List;
import java.util.Set;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import jp.riken.kscope.data.CodeLine;
// import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.Condition;
import jp.riken.kscope.language.ExecutableBody;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.KeywordArgument;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.Selection;
import jp.riken.kscope.language.Substitution;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.utils.LanguageUtils;
import jp.riken.kscope.model.TraceResultModel;

/**
 * Analysis: Perform variable tracing.
 *
 * @author RIKEN
 */
public class AnalysisTraceService extends AnalysisBaseService {

  /** Trace target variable name. */
  private String traceWord;

  /**
   * Constructor.
   *
   * @param fortran Fortran database
   */
  public AnalysisTraceService(Fortran fortran) {
    super(fortran);
  }

  /**
   * Start tracing. <br>
   * Search the target procedure and trace block from the trace target variable name and trace line
   * information.
   *
   * @param line Trace line information
   * @return Trace result model
   */
  public TraceResultModel analysisTraceStart(CodeLine line) {

    if (line == null) {
      return null;
    }
    String traceWrd = this.traceWord.toLowerCase();
    // Search for the corresponding program unit from the CodeLine information.
    LanguageUtils utils = new LanguageUtils(this.fortranDb);
    ProgramUnit currentProc = utils.getCurrentProgramUnit(line);
    if (currentProc == null) {
      return null;
    }

    // Set to reference list model
    IBlock currentblk = currentProc;
    DefaultMutableTreeNode root = new DefaultMutableTreeNode(currentProc);
    if (currentProc instanceof Procedure) {
      Set<IBlock> blks = ((Procedure) currentProc).getRefDefBlocks(traceWrd);
      if (blks != null) {
        for (IBlock blk : blks) {
          this.addBlockToRoot(root, blk);
          // Register if the block corresponding to CodeLine is included.
          int currentLine = blk.getStartCodeLine().getStartLine();
          if (currentLine == line.getStartLine()) {
            currentblk = blk;
          }
        }
      }
    }
    // Consider what to do with the TODO module

    // Generate result display tree
    DefaultTreeModel tree = new DefaultTreeModel(root);

    // Create a trace model
    TraceResultModel modelTrace = new TraceResultModel();
    modelTrace.setTraceWord(traceWrd); // Trace target variable name
    modelTrace.setTreeModel(tree); // Display tree model
    modelTrace.setTitle(traceWrd); // Display title
    modelTrace.setSelectedBlock(currentblk); // Select block

    return modelTrace;
  }

  /**
   * Add a block to root. If the block belongs to a branch, build it including the branch.
   *
   * @param root Root node
   * @param blk Blocks to add
   */
  private void addBlockToRoot(DefaultMutableTreeNode root, IBlock blk) {
    DefaultMutableTreeNode node = new DefaultMutableTreeNode(blk);
    if (blk instanceof Block) {
      Block mother = ((Block) blk).get_mother();
      DefaultMutableTreeNode childNode = new DefaultMutableTreeNode(blk);
      // Continue until you reach the top of the executable statement. Beware of infinite loops
      while (!(mother instanceof ExecutableBody)) {
        DefaultMutableTreeNode mamNode = this.searchTree(root, mother);
        if (mother instanceof Condition) {
          Selection sel = (Selection) mother.get_mother();
          if (!(sel.isSelect())) {
            if (sel.getConditions().get(0).equals(mother)) {
              mamNode = this.searchTree(root, sel);
            }
            mother = sel;
          }
        }
        mamNode.add(childNode);
        mother = mother.get_mother();
        childNode = mamNode;
      }
      root.add(childNode);
      return;
    }
    root.add(node);
  }

  /**
   * If root has a node with blk, return that node.
   *
   * @param node Root node
   * @param blk Block
   * @return node. If not, the newly created node is returned.
   */
  private DefaultMutableTreeNode searchTree(DefaultMutableTreeNode node, Block blk) {
    if (node.getUserObject().equals(blk)) {
      return node;
    }

    // List tree nodes in the forward direction
    Enumeration<?> depth = node.preorderEnumeration();
    while (depth.hasMoreElements()) {
      DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode) depth.nextElement();
      // Do a node search
      if (treeNode == null || treeNode.getUserObject() == null) {
        continue;
      }
      if (treeNode.getUserObject() == blk) {
        return treeNode;
      }
    }
    DefaultMutableTreeNode newnode = new DefaultMutableTreeNode(blk);
    return newnode;
  }

  /**
   * Trace: Do an in. <br>
   * Search for subroutines and function procedures from the trace target variable name and trace
   * block.
   *
   * @param block Trace block (current trace selection block)
   * @return Trace result model list
   */
  public TraceResultModel[] analysisTraceIn(IBlock block) {
    if (block == null) {
      return null;
    }
    List<ProcedureUsage> pus = new ArrayList<ProcedureUsage>();
    ProcedureUsage punit = null;
    if (block instanceof ProcedureUsage) {
      punit = (ProcedureUsage) block;
      pus.add(punit);
    } else if (block instanceof Substitution) {
      Set<ProcedureUsage> funcCalls = ((Substitution) block).getRightValue().getAllFunctions();
      pus.addAll(funcCalls);
    } else {
      return null;
    }

    // Trace model
    List<TraceResultModel> listTrace = new ArrayList<TraceResultModel>();
    for (ProcedureUsage pu : pus) {
      Procedure proc = null; // Definition destination of call statement
      if (pu.getCallDefinition() != null) {
        proc = pu.getCallDefinition();
      } else {
        // If there is no TODO definition, should it be presented?
        return null;
      }
      String actualArg = this.traceWord.toLowerCase(); // Actual argument name in the program unit

      Set<Integer> numArgs = pu.numberOfArg(actualArg);
      numArgsLoop:
      for (int numArg : numArgs) {
        String dummyArg;
        Variable var = proc.getArgument(numArg);
        if (var == null) {
          // End of processing because the corresponding argument cannot be found
          continue numArgsLoop;
        } else {
          dummyArg = var.getName();
        }
        // Check keyword arguments
        if (pu.getArguments().get(numArg) instanceof KeywordArgument) {
          KeywordArgument keywrd = (KeywordArgument) pu.getArguments().get(numArg);
          if (keywrd != null) {
            dummyArg = keywrd.getKeyword();
          }
        }

        // Set to reference list model
        Set<IBlock> refdefs = proc.getRefDefBlocks(dummyArg);
        DefaultMutableTreeNode root = new DefaultMutableTreeNode(proc);
        for (IBlock blk : refdefs) {
          this.addBlockToRoot(root, blk);
        }

        // Tree generation
        DefaultTreeModel tree = new DefaultTreeModel(root);
        TraceResultModel modelTrace = new TraceResultModel();
        modelTrace.setTraceWord(dummyArg); // Trace target variable name
        modelTrace.setTreeModel(tree); // Display tree model
        modelTrace.setTitle(dummyArg); // Display title
        modelTrace.setSelectedBlock(proc); // Select block
        modelTrace.setBlocklabel(
            pu.toDefinitionHTMLString(numArg)); // Label for displaying the trace destination dialog
        listTrace.add(modelTrace);
      }
    }

    return listTrace.toArray(new TraceResultModel[0]);
  }

  /**
   * Trace: Out. <br>
   * Search for the block calling the specified procedure.
   *
   * @param block Trace block (root block of the current trace)
   * @param traceHistory Trace history
   * @return Trace result model list
   */
  public TraceResultModel[] analysisTraceOut(IBlock block, IBlock[] traceHistory) {

    Procedure currentProc = null;
    if (block instanceof Procedure) {
      currentProc = (Procedure) block;
    } else {
      return null;
    }

    String dummyArg = this.traceWord.toLowerCase();
    int numDummyArg = currentProc.getNumOfDummyArgument(dummyArg); // order of dummyArg
    if (numDummyArg < 0) {
      return null;
    }
    // Get the procedure call block calling the Procedure
    Set<ProcedureUsage> calls = currentProc.getCallMember();

    // Create a trace model
    List<TraceResultModel> listTrace = new ArrayList<TraceResultModel>();
    // Perform a trace for each program unit

    for (ProcedureUsage currentCall : calls) {
      Procedure proc = currentCall.getMyProcedure(); // Program unit to which currentCall belongs

      int numActualArg = currentCall.getNumOfActualArgument(dummyArg, numDummyArg);
      if (numActualArg < 0) {
        continue;
      }
      // Convert to a list of variable names corresponding to formal arguments
      Set<String> actualArgs = currentCall.getActualArgument(numActualArg);
      for (String actualArg : actualArgs) {
        Set<IBlock> refdefs = proc.getRefDefBlocks(actualArg);

        DefaultMutableTreeNode root = new DefaultMutableTreeNode(proc);

        if (refdefs != null) {
          for (IBlock refdef : refdefs) {
            // Set to reference list model
            this.addBlockToRoot(root, refdef);
          }
        }

        // Tree generation
        DefaultTreeModel tree = new DefaultTreeModel(root);

        TraceResultModel modelTrace = new TraceResultModel();
        modelTrace.setTraceWord(actualArg); // Trace target variable name
        modelTrace.setTreeModel(tree); // Display tree model
        modelTrace.setTitle(actualArg); // Display title
        modelTrace.setSelectedBlock(currentCall); // Select block
        modelTrace.setBlocklabel(
            currentCall.toHTMLString(
                numActualArg, actualArg)); // Label for displaying the trace destination dialog
        listTrace.add(modelTrace);
      }
    }

    if (listTrace == null || listTrace.size() <= 1) {
      return listTrace.toArray(new TraceResultModel[0]);
    }
    if (traceHistory == null || traceHistory.length <= 2) {
      return listTrace.toArray(new TraceResultModel[0]);
    }

    IBlock lastblock = traceHistory[traceHistory.length - 2];
    TraceResultModel forwardModel = null;
    for (TraceResultModel model : listTrace) {
      if (model.getSelectedBlock() == lastblock) {
        forwardModel = model;
        break;
      }
    }
    if (forwardModel == null) {
      return listTrace.toArray(new TraceResultModel[0]);
    }

    return new TraceResultModel[] {forwardModel};
  }

  /**
   * Get the name of the variable to be traced.
   *
   * @return Traced variable name
   */
  public String getTraceWord() {
    return this.traceWord;
  }

  /**
   * Set the variable name to be traced.
   *
   * @param word Trace target variable name
   */
  public void setTraceWord(String word) {
    this.traceWord = word;
  }
}
