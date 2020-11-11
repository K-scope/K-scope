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

package jp.riken.kscope.language.utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.ExecutableBody;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.ProgramUnit;

/**
 * Database utility class. Search and retrieve blocks from the database
 *
 * @author RIKEN
 */
public class LanguageUtils {
  /** Fortran database */
  private Fortran fortranDb;

  /** Constructor */
  public LanguageUtils() {}

  /**
   * Constructor
   *
   * @param db database
   */
  public LanguageUtils(Fortran db) {
    this.fortranDb = db;
  }

  /**
   * Searches back the program unit to which it belongs from the code line information.
   *
   * @param line Code line information
   * @return Program unit. If not, it returns null.
   */
  public ProgramUnit getCurrentProgramUnit(CodeLine line) {
    if (line == null) return null;
    if (this.fortranDb == null) return null;

    SourceFile file = line.getSourceFile();
    int lineNo = line.getStartLine();
    // Get the list of program units in file
    List<ProgramUnit> pus = this.fortranDb.getProgramUnits(file);
    if (pus == null) return null;
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

  /**
   * Search the block list of line numbers
   *
   * @param line line number
   * @return Block list of line numbers
   */
  public IBlock[] getCodeLineBlocks(CodeLine line) {
    if (line == null) return null;
    CodeLine[] lines = {line};
    return getCodeLineBlocks(lines);
  }

  /**
   * Search the block list of line numbers
   *
   * @param lines Line number list
   * @return Block list of line numbers
   */
  public IBlock[] getCodeLineBlocks(CodeLine[] lines) {
    if (lines == null) return null;

    List<IBlock> list = new ArrayList<IBlock>();
    for (CodeLine line : lines) {
      ProgramUnit unit = getCurrentProgramUnit(line);
      if (unit == null) continue;
      IBlock[] blocks = unit.searchCodeLine(line);
      if (blocks != null) {
        list.addAll(Arrays.asList(blocks));
      }
    }

    if (list.size() <= 0) return null;
    return list.toArray(new IBlock[0]);
  }

  /**
   * Get the database structure hierarchy. A hierarchical list is a list of children to parents.
   *
   * @param block Database configuration block
   * @return Database structure hierarchical list
   */
  public List<Object> getLanguagePath(Object block) {
    if (block == null) return null;

    // Get the hierarchy of selected nodes
    List<Object> parents = new ArrayList<Object>();
    Object child = block;
    while (child != null) {
      if (child instanceof ExecutableBody) {
        parents.add(child);
        child = ((ExecutableBody) child).getParent();
      } else if (child instanceof Procedure) {
        if (parents.size() > 0) {
          if (parents.get(parents.size() - 1) != child) {
            parents.add(child);
          }
        }
        // Caller CALL statement list
        Set<ProcedureUsage> calls = ((Procedure) child).getCallMember();
        if (calls != null && calls.size() > 0) {
          ProcedureUsage[] array = calls.toArray(new ProcedureUsage[0]);
          // Select the one with the deep parent hierarchy.
          List<Object> listMax = null;
          for (ProcedureUsage useage : array) {
            List<Object> listPath = getLanguagePath(useage);
            if (listPath == null || listPath.size() <= 0) continue;
            // Check if it is a recursive call.
            if (isRecursive(parents, listPath)) continue;
            // Have you reached the program statement?
            if (listPath.get(listPath.size() - 1) instanceof Procedure) {
              if (((Procedure) listPath.get(listPath.size() - 1)).isProgram()) {
                listMax = listPath;
                break;
              }
            }
            if (listMax == null) listMax = listPath;
            else if (listMax.size() < listPath.size()) listMax = listPath;
          }
          if (listMax != null) {
            child = listMax.get(listMax.size() - 1);
            parents.addAll(listMax);
          } else {
            child = null;
          }
        } else {
          child = null;
        }
      } else if (child instanceof IBlock) {
        parents.add(child);
        child = ((IBlock) child).getMotherBlock();
      } else {
        child = null;
      }
    }
    if (parents.size() <= 0) return null;

    return parents;
  }

  /**
   * Check if it is a recursive call.
   *
   * @param parents Original list
   * @param childrens additional list
   * @return true = Recursive call
   */
  private boolean isRecursive(List<Object> parents, List<Object> childrens) {
    if (parents == null || parents.size() <= 0) return false;
    if (childrens == null || childrens.size() <= 0) return false;
    for (Object children : childrens) {
      if (children instanceof Procedure) {
        for (Object parent : parents) {
          if (parent instanceof Procedure) {
            if (children == parent) {
              return true;
            }
          }
        }
      }
    }
    return false;
  }
}
