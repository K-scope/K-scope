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
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.Common;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.UseState;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.model.ReferenceModel;

/**
 * Declaration / definition / reference service class. <br>
 * Create a declaration / definition / reference list
 *
 * @author RIKEN
 */
public class AnalysisReferenceService extends AnalysisBaseService {

  /** Reference list model. */
  private ReferenceModel modelReference;

  /**
   * Constructor.
   *
   * @param fortran Fortran database
   */
  public AnalysisReferenceService(Fortran fortran) {
    super(fortran);
  }

  /**
   * Set the reference list model.
   *
   * @param model Reference list model
   */
  public void setModelReference(ReferenceModel model) {
    this.modelReference = model;
  }

  /**
   * Create a reference list.
   *
   * @param variable Reference list
   */
  public void analysisReference(VariableDefinition variable) {
    if (variable == null) {
      return;
    }

    // Set to reference list model
    DefaultMutableTreeNode root = new DefaultMutableTreeNode(variable);
    DefaultMutableTreeNode decNode =
        new DefaultMutableTreeNode(
            Message.getString("analysisreferenceservice.reference.declaration")); // Declaration
    DefaultMutableTreeNode refNode =
        new DefaultMutableTreeNode(
            Message.getString("analysisreferenceservice.reference.reference")); // reference
    DefaultMutableTreeNode defNode =
        new DefaultMutableTreeNode(
            Message.getString("analysisreferenceservice.reference.definition")); // Definition
    root.add(decNode);
    root.add(refNode);
    root.add(defNode);

    // Create a declaration node
    DefaultMutableTreeNode dec = new DefaultMutableTreeNode(variable);
    DefaultMutableTreeNode mother = new DefaultMutableTreeNode(variable.getMother());
    dec.setAllowsChildren(false);
    mother.add(dec);
    decNode.add(mother);

    Set<ProgramUnit> refdefUnit =
        new HashSet<ProgramUnit>(); // Set of program units referenced / defined
    // Create a list of references / definitions by program unit to which the declaration belongs
    // and subprogram units
    refdefUnit.addAll(this.searchChildrenWithScope(variable));

    // Create a list of references / definitions by USE statement
    refdefUnit.addAll(variable.getReferMember());

    for (ProgramUnit pu : refdefUnit) {
      String name = variable.get_name();
      // Check for name conversion by USE statement
      List<UseState> uses = pu.getUseList();
      for (UseState use : uses) {
        name = use.translation(variable);
        if (!(name.equalsIgnoreCase(variable.get_name()))) {
          break;
        }
      }

      // Create a reference list
      Map<String, Set<IBlock>> refs = pu.getRefVariableNames();
      Set<IBlock> blk = refs.get(name);
      if (blk != null) {
        DefaultMutableTreeNode pr = new DefaultMutableTreeNode(pu);
        for (IBlock bk : blk) {
          DefaultMutableTreeNode bl = new DefaultMutableTreeNode(bk);
          pr.add(bl);
        }
        refNode.add(pr);
      }

      // Create a definition list
      Map<String, Set<IBlock>> defs = pu.getDefVariableNames();
      blk = defs.get(name);
      if (blk != null) {
        DefaultMutableTreeNode pr = new DefaultMutableTreeNode(pu);
        for (IBlock bk : blk) {
          DefaultMutableTreeNode bl = new DefaultMutableTreeNode(bk);
          pr.add(bl);
        }
        defNode.add(pr);
      }
    }

    // List for COMMON attribute
    if (this.fortranDb.getCommonMap() != null) {
      ProgramUnit motherUnit = variable.getMother();
      List<Common> comList = motherUnit.getCommonList();
      List<ProgramUnit> comUnits = new ArrayList<ProgramUnit>();
      String comName = "";
      int varidx = 0;
      searchCom:
      for (Common com : comList) {
        varidx = 0;
        for (Variable var : com.getVariables()) {
          if (var.getName().equalsIgnoreCase(variable.get_name())) {
            comName = com.getName();
            comUnits = this.fortranDb.getCommonUnit(comName);
            break searchCom;
          }
          varidx++;
        }
      }
      for (ProgramUnit pu : comUnits) {
        if (!(refdefUnit.contains(pu))) {
          List<Common> puComs = pu.getCommonList();
          String localName = "";
          for (Common cm : puComs) {
            if (cm.getVariables() == null || cm.getVariables().size() <= 0) continue;
            if (cm.getVariables().size() <= varidx) continue;
            if (cm.getName().equalsIgnoreCase(comName)) {
              localName = cm.getVariables().get(varidx).getName();
              break;
            }
          }
          VariableDefinition def = pu.getVariableMap(localName);
          if (def != null) {
            // Declaration
            DefaultMutableTreeNode defCom = new DefaultMutableTreeNode(def);
            DefaultMutableTreeNode motherCom = new DefaultMutableTreeNode(def.getMother());
            defCom.setAllowsChildren(false);
            motherCom.add(defCom);
            decNode.add(motherCom);
            // Create a reference list
            Map<String, Set<IBlock>> refs = pu.getRefVariableNames();
            Set<IBlock> blk = refs.get(localName);
            if (blk != null) {
              DefaultMutableTreeNode pr = new DefaultMutableTreeNode(pu);
              for (IBlock bk : blk) {
                DefaultMutableTreeNode bl = new DefaultMutableTreeNode(bk);
                pr.add(bl);
              }
              refNode.add(pr);
            }

            // Create a definition list
            Map<String, Set<IBlock>> defs = pu.getDefVariableNames();
            blk = defs.get(localName);
            if (blk != null) {
              DefaultMutableTreeNode pr = new DefaultMutableTreeNode(pu);
              for (IBlock bk : blk) {
                DefaultMutableTreeNode bl = new DefaultMutableTreeNode(bk);
                pr.add(bl);
              }
              defNode.add(pr);
            }
          }
        }
      }
    }

    // Tree generation
    DefaultTreeModel tree = new DefaultTreeModel(root);

    // Tree settings
    this.modelReference.setTreeModel(tree);
  }

  /**
   * Returns a set of program units that are valid areas within the program unit to which the
   * specified variable declaration belongs.
   *
   * @param var Variable declaration
   * @return A set of program units. It has at least the module to which the declaration belongs.
   */
  private Set<ProgramUnit> searchChildrenWithScope(VariableDefinition var) {
    Set<ProgramUnit> pus = new HashSet<ProgramUnit>();
    pus.add(var.getMother());
    for (Procedure child : var.getMother().getChildren()) {
      // Add if the subprogram does not have the same name declaration
      if (child.get_variable(var.get_name()) == null) {
        pus.add(child);
      }
      for (Procedure grnd : child.getChildren()) {
        if (grnd.get_variable(var.get_name()) == null) {
          pus.add(grnd);
        }
      }
    }

    return pus;
  }

  /**
   * Create a reference list from the character string selected on the source code.
   *
   * @param line Selected line
   */
  public void analysisReference(CodeLine line) {
    if (line == null) return;
    // Select variable
    String varName = line.getStatement().toLowerCase();

    // Search for the corresponding program unit from the CodeLine information.
    ProgramUnit currentProc = this.getCurrentProcedure(line);

    if (currentProc == null) {
      return;
    }

    // Get the variable declaration
    VariableDefinition varDef = null;
    // Consider whether to target the TODO module
    if (currentProc instanceof Procedure) {
      varDef = ((Procedure) currentProc).getVariableMap(varName);
    } else {
      varDef = currentProc.get_variable(varName);
    }
    // Create a reference list model
    this.analysisReference(varDef);
    return;
  }

  /**
   * Returns the program unit to which the codeline belongs.
   *
   * @param line Code line information
   * @return Program unit. If not, it returns null.
   */
  private ProgramUnit getCurrentProcedure(CodeLine line) {
    SourceFile file = line.getSourceFile();
    int lineNo = line.getStartLine();
    // System.out.println("lineNo " + lineNo);
    // Get the list of program units in file
    List<ProgramUnit> pus = this.fortranDb.getProgramUnits(file);
    // Learn program units including lineNo
    ProgramUnit punit = null;
    for (ProgramUnit pu : pus) {
      int sPos = pu.getStartPos();
      int ePos = pu.getEndPos();
      if (sPos <= lineNo && lineNo <= ePos) {
        punit = pu;
        Collection<Procedure> children = pu.getChildren();
        for (Procedure child : children) {
          sPos = child.getStartPos();
          ePos = child.getEndPos();
          if (sPos <= lineNo && lineNo <= ePos) {
            punit = child;
            Collection<Procedure> children2 = child.getChildren();
            for (Procedure child2 : children2) {
              sPos = child.getStartPos();
              ePos = child.getEndPos();
              if (sPos <= lineNo && lineNo <= ePos) {
                punit = child2;
              }
            }
          }
        }
      }
    }
    return punit;
  }
}
