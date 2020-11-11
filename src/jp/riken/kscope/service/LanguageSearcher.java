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
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.Condition;
import jp.riken.kscope.language.ExecutableBody;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Module;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.Selection;

/**
 * Fortran database search class
 *
 * @author RIKEN
 */
public class LanguageSearcher {

  /** Fortran parsing result storage database. */
  private Fortran fortranDb;
  /** Search code line */
  private CodeLine searchCode;
  /** Search subroutine name */
  private String callname;

  /**
   * Constructor
   *
   * @param db Fortran parsing result storage database
   * @param code Search code line
   */
  public LanguageSearcher(Fortran db, CodeLine code) {
    this.fortranDb = db;
    this.searchCode = code;
  }

  /**
   * Constructor
   *
   * @param db Fortran parsing result storage database
   * @param callname Subroutine name
   */
  public LanguageSearcher(Fortran db, String callname) {
    this.fortranDb = db;
    this.callname = callname;
  }

  /**
   * Get the block that matches the line of code from the Fortran parsing result storage database.
   *
   * @return Match block
   */
  public IBlock searchCodeLine() {
    if (this.searchCode == null) return null;
    if (this.searchCode.getSourceFile() == null) return null;
    if (this.fortranDb == null) return null;
    Map<String, Module> modules = this.fortranDb.getModules();
    if (modules == null || modules.size() <= 0) {
      return null;
    }

    // Perform module search
    Set<String> keySet = modules.keySet();
    for (String key : keySet) {
      Module module = modules.get(key);
      IBlock block = selectCodeLine(module);
      if (block != null) return block;
    }

    return null;
  }

  /**
   * Get the block that matches the search subroutine name from the Fortran parsing result storage
   * database.
   *
   * @return Matching subroutine
   */
  public IBlock[] searchProcedureUsage() {
    if (this.callname == null) return null;
    if (this.fortranDb == null) return null;
    Map<String, Module> modules = this.fortranDb.getModules();
    if (modules == null || modules.size() <= 0) {
      return null;
    }

    // Perform module search
    List<IBlock> list = new ArrayList<IBlock>();
    Set<String> keySet = modules.keySet();
    for (String key : keySet) {
      Module module = modules.get(key);
      IBlock[] blocks = selectProcedureUsage(module);
      if (blocks != null) {
        list.addAll(Arrays.asList(blocks));
      }
    }
    if (list.size() <= 0) return null;
    return list.toArray(new IBlock[0]);
  }

  /**
   * Get the block that matches the search subroutine name from the module.
   *
   * @param module Search module
   * @return Search result block
   */
  private IBlock[] selectProcedureUsage(Module module) {

    if (this.callname == null) return null;
    if (this.fortranDb == null) return null;
    if (module == null) return null;

    // Subprogram unit.
    List<IBlock> list = new ArrayList<IBlock>();
    Procedure[] childrens = module.get_children();
    if (childrens != null) {
      for (Procedure children : childrens) {
        IBlock[] blocks = selectProcedureUsage(children);
        if (blocks != null) {
          list.addAll(Arrays.asList(blocks));
        }
      }
    }
    if (list.size() <= 0) return null;
    return list.toArray(new IBlock[0]);
  }

  /**
   * Get the block that matches the search subroutine name from the procedure.
   *
   * @param procedure Search procedure
   * @return Search result block
   */
  private IBlock[] selectProcedureUsage(Procedure procedure) {

    if (this.callname == null) return null;
    if (this.fortranDb == null) return null;
    if (procedure == null) return null;

    // Subprogram unit.
    List<IBlock> list = new ArrayList<IBlock>();
    Procedure[] childrens = procedure.get_children();
    if (childrens != null) {
      for (Procedure children : childrens) {
        IBlock[] blocks = selectProcedureUsage(children);
        if (blocks != null) {
          list.addAll(Arrays.asList(blocks));
        }
      }
    }

    ExecutableBody body = procedure.getBody();
    IBlock[] blocks = selectProcedureUsage(body);
    if (blocks != null) {
      list.addAll(Arrays.asList(blocks));
    }
    if (list.size() <= 0) return null;
    return list.toArray(new IBlock[0]);
  }

  /**
   * Search from the procedure Search for the block that matches the subroutine name.
   *
   * @param body Search processing block
   * @return Search result block
   */
  private IBlock[] selectProcedureUsage(ExecutableBody body) {
    if (this.callname == null) return null;
    if (this.fortranDb == null) return null;
    if (body == null) return null;

    List<IBlock> list = new ArrayList<IBlock>();
    ArrayList<Block> listBlock = body.getChildren();
    if (listBlock == null) return null;
    for (Block children : listBlock) {
      IBlock[] blocks = null;
      if (children instanceof Selection) {
        blocks = selectProcedureUsage((Selection) children);
      } else {
        blocks = selectProcedureUsage(children);
      }
      if (blocks != null) {
        list.addAll(Arrays.asList(blocks));
      }
    }
    if (list.size() <= 0) return null;
    return list.toArray(new IBlock[0]);
  }

  /**
   * Search the procedure for blocks that match the search code line.
   *
   * @param block Search block
   * @return Search result block
   */
  private IBlock[] selectProcedureUsage(Block block) {
    if (this.callname == null) return null;
    if (this.fortranDb == null) return null;
    if (block == null) return null;

    List<IBlock> list = new ArrayList<IBlock>();
    // Check if it is a ProcedureUsage class
    if (block instanceof ProcedureUsage) {
      String name = ((ProcedureUsage) block).getCallName();
      if (this.callname.equalsIgnoreCase(name)) {
        list.add(block);
      }
    }
    ArrayList<Block> listBlock = block.getChildren();
    if (list != null) {
      for (Block children : listBlock) {
        IBlock[] results = selectProcedureUsage(children);
        if (results != null) {
          list.addAll(Arrays.asList(results));
        }
      }
    }
    if (list.size() <= 0) return null;
    return list.toArray(new IBlock[0]);
  }

  /**
   * Search the module for blocks that match the search code line.
   *
   * @param module Search module
   * @return Search result block
   */
  private IBlock selectCodeLine(Module module) {
    if (this.searchCode == null) return null;
    if (this.fortranDb == null) return null;
    if (module == null) return null;

    // Check if the search block matches the search code line
    if (isMatchCodeLine(module)) return module;

    // Subprogram unit.
    Procedure[] childrens = module.get_children();
    if (childrens != null) {
      for (Procedure children : childrens) {
        IBlock block = selectCodeLine(children);
        if (block != null) return block;
      }
    }

    return null;
  }

  /**
   * Search the procedure for blocks that match the search code line.
   *
   * @param procedure Search procedure
   * @return Search result block
   */
  private IBlock selectCodeLine(Procedure procedure) {
    if (this.searchCode == null) return null;
    if (this.fortranDb == null) return null;
    if (procedure == null) return null;

    // Check if the search block matches the search code line
    if (isMatchCodeLine(procedure)) return procedure;
    // Check if the search block is within the search code line
    if (!isBlockCodeLine(procedure)) return null;

    // Subprogram unit.
    Procedure[] childrens = procedure.get_children();
    if (childrens != null) {
      for (Procedure children : childrens) {
        IBlock block = selectCodeLine(children);
        if (block != null) return block;
      }
    }

    ExecutableBody body = procedure.getBody();
    IBlock block = selectCodeLine(body);

    return block;
  }

  /**
   * Search the procedure for blocks that match the search code line.
   *
   * @param body Search processing block
   * @return Search result block
   */
  private IBlock selectCodeLine(ExecutableBody body) {
    if (this.searchCode == null) return null;
    if (this.fortranDb == null) return null;
    if (body == null) return null;

    ArrayList<Block> list = body.getChildren();
    if (list == null) return null;
    for (Block children : list) {
      IBlock block = null;
      if (children instanceof Selection) {
        block = selectCodeLine((Selection) children);
      } else {
        block = selectCodeLine(children);
      }

      if (block != null) return block;
    }
    return null;
  }

  /**
   * Search the procedure for blocks that match the search code line.
   *
   * @param block Search block
   * @return Search result block
   */
  private IBlock selectCodeLine(Block block) {
    if (this.searchCode == null) return null;
    if (this.fortranDb == null) return null;
    if (block == null) return null;

    // Check if the search block matches the search code line
    if (isMatchCodeLine(block)) return block;
    // Check if the search block is within the search code line
    if (!isBlockCodeLine(block)) return null;

    ArrayList<Block> list = block.getChildren();
    if (list != null) {
      for (Block children : list) {
        IBlock result = selectCodeLine(children);
        if (result != null) return result;
      }
    }

    return null;
  }

  /**
   * Check if the conditional branch statement (IF, SELECT) matches the search code line
   *
   * @param block Search branch block
   * @return Search result block
   */
  private IBlock selectCodeLine(Selection block) {
    if (this.searchCode == null) return null;
    if (this.fortranDb == null) return null;
    if (block == null) return null;

    // Check if the search block matches the search code line
    if (isMatchCodeLine(block)) return block;
    // Check if the search block is within the search code line
    if (!isBlockCodeLine(block)) return null;
    // Conditional statement
    List<Condition> list = block.getConditions();
    if (list != null) {
      for (Condition condition : list) {
        IBlock result = selectCodeLine(condition);
        if (result != null) return result;
      }
    }
    // Search for child elements
    return selectCodeLine((Block) block);
  }

  /**
   * Check if the search block is within the search code line
   *
   * @param block Search block
   * @return true = block range
   */
  private boolean isBlockCodeLine(IBlock block) {
    if (this.searchCode == null) return false;
    if (this.searchCode.getSourceFile() == null) return false;
    if (block == null) return false;
    CodeLine blockCode = block.getStartCodeLine();
    if (blockCode == null) return false;

    // Does the search file match?
    SourceFile file = this.searchCode.getSourceFile();
    if (!file.equals(blockCode.getSourceFile())) return false;

    // Does the search line number exist within the block?
    int searchStartno = this.searchCode.getStartLine();
    int searchEndno = this.searchCode.getEndLine();
    int blockStartno = block.getStartCodeLine().getStartLine();
    int blockEndno = block.getEndCodeLine().getEndLine();

    if (blockStartno > searchEndno || searchStartno > blockEndno) {
      return false;
    }

    return true;
  }

  /**
   * Check if the search block matches the search code line
   *
   * @param block Search block
   * @return true = block range
   */
  private boolean isMatchCodeLine(IBlock block) {
    if (this.searchCode == null) return false;
    if (this.searchCode.getSourceFile() == null) return false;
    if (block == null) return false;
    CodeLine blockCode = block.getStartCodeLine();
    if (blockCode == null) return false;

    // Does the search file match?
    SourceFile file = this.searchCode.getSourceFile();
    if (!file.equals(blockCode.getSourceFile())) return false;

    // Does the search line number match that of the block?
    int searchStartno = this.searchCode.getStartLine();
    // int searchEndno = this.searchCode.getEndLine();
    int blockStartno = block.getStartCodeLine().getStartLine();
    // int blockEndno = block.getEndCodeLine().getEndLine();

    // If the start line matches, it is considered to match.
    if (blockStartno != searchStartno) return false;

    return true;
  }
}
