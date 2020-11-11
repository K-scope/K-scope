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

package jp.riken.kscope.language;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.information.InformationBlock;
import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.information.TextInfo;

/**
 * An abstract class that represents a processing block.
 *
 * @author RIKEN
 */
public abstract class Block implements Serializable, IInformation, IBlock {
  /** Serial number */
  private static final long serialVersionUID = 4754517459536460336L;
  /** Parent block */
  private Block mother;
  /** Start line information */
  private Statement start;
  /** End line information */
  private Statement end;
  /** Child block */
  // Consider whether to stop new TODO children all the time
  private ArrayList<Block> children = new ArrayList<Block>();
  /** Additional information */
  // TODO Consider whether you should stop initializing with null all the time
  private TextInfo information = null;

  /** Constructor. */
  protected Block() {}

  /**
   * Constructor.
   *
   * @param mama Parent block
   */
  public Block(Block mama) {
    this.mother = mama;
  }

  // ++++++++++++++++++++++++++++++++++++++++++++
  /**
   * Set the start code line information.
   *
   * @param lineInfo Start code line information
   */
  protected void set_block_start(CodeLine lineInfo) {
    start = new Statement(lineInfo);
  }

  /**
   * Set the exit code line information.
   *
   * @param lineInfo Exit code line information
   */
  protected void set_block_end(CodeLine lineInfo) {
    end = new Statement(lineInfo);
  }

  /** Get the string representation of the block. */
  @Override
  public String toString() {
    // delete by @hira at 2013/03/01 The additional information registration block was deleted
    // because it changed from [!] To red characters.
    //         String info = "";
    //        if (this.getInformation() != null) {
    //            if (!(this.getInformation().getContent().equals(""))) {
    //                info = "[ ! ] ";
    //            }
    //        }
    // return (info + this.toStringBase());
    return this.get_start_str();
  }

  /**
   * Get the base string representation of the block.
   *
   * @return Block string representation
   */
  protected String toStringBase() {
    // return this.get_start_str().toLowerCase();
    return this.get_start_str();
  }

  /**
   * Set the parent block.
   *
   * @param mama Parent block
   */
  protected void set_mother(Block mama) {
    mother = mama;
  }

  /**
   * Returns the parent block.
   *
   * @return Parent block
   */
  public Block get_mother() {
    return mother;
  }

  // ++++++++++++++++++++++++++++++++++++++++++++
  /**
   * Returns the start Statement.
   *
   * @return Start Statement
   */
  public Statement get_start() {
    return start;
  }

  /**
   * Returns the end Statement.
   *
   * @return End Statement
   */
  public Statement get_end() {
    return end;
  }

  // ++++++++++++++++++++++++++++++++++++++++++++
  /**
   * Returns a string representation of the start line of the block.
   *
   * @return String representation of the start line
   */
  public String get_start_str() {
    return start.get_statement();
  }

  /**
   * Returns a string representation of the end line of the block.
   *
   * @return String representation of the end line
   */
  public String get_end_str() {
    return end.get_statement();
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  protected int get_num_of_child() {
    return children.size();
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  protected void add_child(Block child) {
    child.set_mother(this);
    children.add(child);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  protected Block get_child(int i) {
    return children.get(i);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++
  /**
   * Returns a child element.
   *
   * @return Child element. Returns an empty list if not
   */
  public ArrayList<Block> getChildren() {
    return children;
  }

  protected void getCalls(List<ProcedureUsage> calls) {
    for (int i = 0; i < get_num_of_child(); i++) {
      Block child = this.get_child(i);
      if (child instanceof ProcedureUsage) {
        calls.add((ProcedureUsage) child);
      } else if (child instanceof Selection) {
        for (int j = 0; j < ((Selection) (child)).getNumOfConditions(); j++) {
          Block child_block = ((((Selection) child).getConditions().get(j)));
          child_block.getCalls(calls);
        }

      } else if (child instanceof Substitution) {
        List<ProcedureUsage> funcCalls = ((Substitution) child).getRightValue().getFuncCalls();
        for (ProcedureUsage call : funcCalls) {
          calls.add(call);
        }
      } else {
        child.getCalls(calls);
      }
    }
  }
  // ++++++++++++++++++++++++++++++++++++++++++++
  protected void get_blocks(List<Block> blocks) {
    for (int i = 0; i < get_num_of_child(); i++) {
      Block child = get_child(i);
      if (child instanceof ProcedureUsage) {
        blocks.add(child);
      } else {
        blocks.add(child);
      }
    }
  }

  // -----------Informations------------//
  @Override
  public void setInformation(TextInfo info) {
    this.information = info;
  }

  /**
   * Get additional information
   *
   * @return Additional information
   */
  @Override
  public TextInfo getInformation() {
    return this.information;
  }

  /**
   * Get start line number information
   *
   * @return Start line number information
   */
  @Override
  public CodeLine getStartCodeLine() {
    if (start == null) return null;
    return start.lineInfo;
  }

  /**
   * Get end line number information
   *
   * @return End line number information
   */
  @Override
  public CodeLine getEndCodeLine() {
    if (end == null) return null;
    return end.lineInfo;
  }

  /**
   * Get the namespace (module name.routine name).
   *
   * @return namespace (module name.routine name)
   */
  @Override
  public String getNamespace() {
    String result = "";
    if (this.mother != null) {
      result = mother.getNamespace();
    }
    return result;
  }

  /**
   * Get the start position.
   *
   * @return start position
   */
  @Override
  public int getStartPos() {
    return this.getStartCodeLine().getStartLine();
  }
  /**
   * Set the start position.
   *
   * @param pos Starting position
   */
  @Override
  public void setStartPos(int pos) {
    this.getStartCodeLine().setLine(pos);
  }

  /*
   * TODO: Temporary support.
   * Actually, the end of the program is program.getEndCodeLine.getEndLine
   * Get or delete EndCodeLine of program and StartCodeLine
   * Should be renamed to CodeLine. Suspect.
   */

  /**
   * Get the end position.
   *
   * @return end position
   */
  @Override
  public int getEndPos() {
    return this.getStartCodeLine().getEndLine();
  }
  /**
   * Set the end position.
   *
   * @param pos End position
   */
  @Override
  public void setEndPos(int pos) {
    this.getStartCodeLine().setEndLine(pos);
  }

  /**
   * Search for information blocks that match id.
   *
   * @param id ID
   * @return The information block found. If not found, null is returned.
   */
  public IInformation findInformationBlockBy(String id) {
    IInformation result = null;

    if (this.getID().equals(id)) {
      result = this;
    } else {
      ArrayList<Block> blocks = this.getChildren();
      for (Block block : blocks) {
        result = block.findInformationBlockBy(id);
        if (result != null) {
          break;
        }
      }
    }

    return result;
  }

  /** Delete all additional information. */
  @Override
  public void clearInformation() {
    this.setInformation(null);
    for (Block child : this.children) {
      child.clearInformation();
    }
  }

  /**
   * Generate an additional information block collection.
   *
   * @return Additional information block collection
   */
  public InformationBlocks createInformationBlocks() {
    InformationBlocks result = new InformationBlocks();

    if (this.information != null) {
      InformationBlock block = new InformationBlock(this.information, this, this);
      result.add(block);
    }
    if (this.children != null) {
      for (Block block : this.children) {
        result.addAll(block.createInformationBlocks());
      }
    }
    return result;
  }

  /**
   * Returns a list of its own child blocks.
   *
   * @return A list of child blocks. If not, returns an empty list.
   */
  public List<Block> getBlocks() {
    return this.children;
  }

  /**
   * Get an ID.
   *
   * @return ID
   */
  @Override
  public String getID() {
    String result = "";
    if (this.mother != null) {
      // modify by @hira at 2013/03/01
      // int offset = this.getStartPos() - this.mother.getStartPos();
      int offset = this.mother.indexOfChildren(this);
      result = this.mother.getID() + "$" + offset + ":" + this.toStringBase();
    } else {
      result = this.toStringBase();
    }
    return result;
  }

  /**
   * Get the parent block
   *
   * @return Parent block
   */
  @Override
  public IBlock getMotherBlock() {
    return this.get_mother();
  }

  /**
   * Check if they are the same block. Children must be the same size and the same string.
   *
   * @param block block
   * @return true = match
   */
  public boolean equalsBlocks(Block block) {
    if (block == null) return false;
    if (this.children == null && block.children == null) {
      return true;
    } else if (this.children == null) {
      return false;
    } else if (this.children != null && block.children != null) {
      if (this.children.size() != block.children.size()) {
        return false;
      }
    }
    if (!this.toString().equalsIgnoreCase(block.toString())) {
      return false;
    }

    int count = this.children.size();
    for (int i = 0; i < count; i++) {
      Block thisChildren = this.children.get(i);
      Block destChildren = block.get_child(i);
      if (thisChildren == destChildren) continue;
      else if (thisChildren == null) {
        return false;
      }
      if (!thisChildren.equalsBlocks(destChildren)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Returns the index of the child block. Returns -1 if it does not exist.
   *
   * @param block block
   * @return index
   */
  protected int indexOfChildren(Block block) {
    return this.children.indexOf(block);
  }

  /**
   * Search for the same block
   *
   * @param block IInformation block
   * @return Same block
   */
  public IInformation[] searchInformationBlocks(IInformation block) {
    if (!(block instanceof Block)) {
      return null;
    }
    List<IInformation> list = new ArrayList<IInformation>();
    if (this.equalsBlocks((Block) block)) {
      list.add(this);
    }

    ArrayList<Block> blocks = this.getChildren();
    for (Block blockChildren : blocks) {
      IInformation[] infos = blockChildren.searchInformationBlocks(block);
      if (infos != null) {
        list.addAll(Arrays.asList(infos));
      }
    }
    if (list.size() <= 0) {
      return null;
    }

    return list.toArray(new IInformation[0]);
  }

  /**
   * Check if they are in the same block hierarchy.
   *
   * @param block Check target Block
   * @return true = match
   */
  public boolean equalsLayout(Block block) {
    if (block == null) return false;

    String layoutIdThis = this.getLayoutID();
    String layoutIdBlock = block.getLayoutID();
    if (layoutIdThis == null) return false;
    if (!layoutIdThis.equalsIgnoreCase(layoutIdBlock)) {
      return false;
    }

    if (this.children == null && block.children == null) {
      return true;
    } else if (this.children == null) {
      return false;
    } else if (block.children == null) {
      return false;
    }

    int idThis = 0;
    int idBlock = 0;
    int countThis = this.children.size();
    int countBlock = block.children.size();
    Block thisChildren = null;
    Block blockChildren = null;
    while (true) {
      thisChildren = null;
      blockChildren = null;
      if (idThis < countThis) {
        thisChildren = this.children.get(idThis);
      }
      if (idBlock < countBlock) {
        blockChildren = block.children.get(idBlock);
      }

      if (thisChildren != null) {
        if (thisChildren.getBlockType() != BlockType.SELECTION
            && thisChildren.getBlockType() != BlockType.REPETITION) {
          idThis++;
          continue;
        }
      }
      if (blockChildren != null) {
        if (blockChildren.getBlockType() != BlockType.SELECTION
            && blockChildren.getBlockType() != BlockType.REPETITION) {
          idBlock++;
          continue;
        }
      }

      if (idThis >= countThis || idBlock >= countBlock) {
        break;
      }
      if (thisChildren == null && blockChildren == null) {
        break;
      } else if (thisChildren == null) {
        idThis++;
        continue;
      } else if (blockChildren == null) {
        idBlock++;
        continue;
      }
      if (!thisChildren.equalsLayout(blockChildren)) {
        return false;
      }
      idThis++;
      idBlock++;
    }

    if (thisChildren != null || blockChildren != null) {
      return false;
    }
    return true;
  }

  /**
   * Get the structure ID.
   *
   * @return Structure ID
   */
  @Override
  public String getLayoutID() {
    String result = "";

    BlockType type = this.getBlockType();
    String typeText = null;
    if (type == BlockType.REPETITION) {
      typeText = "do";
    } else if (type == BlockType.SELECTION) {
      if (this instanceof Selection) {
        if (((Selection) this).isSelect()) {
          typeText = "select";
        } else if (((Selection) this).isIF()) {
          typeText = "if";
        } else if (((Selection) this).isWHERE()) {
          typeText = "where";
        }
      }
    } else {
      typeText = type.name().toLowerCase();
    }
    if (this.mother != null) {
      int offset = this.mother.indexOfLayout(this);
      result = this.mother.getLayoutID() + "$" + offset + ":" + typeText;
    } else {
      result = typeText;
    }
    return result;
  }

  /**
   * Returns the index of the child block. The number of occurrences of DO, SELECT, and IF
   * statements Returns -1 if it does not exist.
   *
   * @param block block
   * @return index
   */
  protected int indexOfLayout(Block block) {
    int index = -1;
    for (Block child : this.children) {
      BlockType type = child.getBlockType();
      if (type == BlockType.REPETITION) {
        index++;
      } else if (type == BlockType.SELECTION) {
        index++;
      }
      if (child == block) {
        return index;
      }
    }
    return -1;
  }

  /**
   * Search for structural blocks that match the layoutID.
   *
   * @param id layoutID
   * @return Found structural block
   */
  public IInformation findInformationLayoutID(String id) {
    if (id == null || id.isEmpty()) return null;
    IInformation result = null;
    String layoutId = this.getLayoutID();
    if (layoutId == null) return null;
    if (layoutId.equalsIgnoreCase(id)) {
      result = this;
    } else {
      ArrayList<Block> blocks = this.getChildren();
      for (Block block : blocks) {
        result = block.findInformationLayoutID(id);
        if (result != null) {
          break;
        }
      }
    }

    return result;
  }

  /**
   * Search for blocks of line numbers
   *
   * @param line line number
   * @return Line number block
   */
  public IBlock[] searchCodeLine(CodeLine line) {
    if (line == null) return null;

    CodeLine thisstart = this.getStartCodeLine();
    CodeLine thisend = this.getEndCodeLine();
    Block addblock = null;
    if (line.isOverlap(thisstart, thisend)) {
      addblock = this;
    }
    List<IBlock> list = new ArrayList<IBlock>();
    ArrayList<Block> blocks = this.getChildren();
    if (blocks == null || blocks.size() <= 0) {
      if (addblock != null) {
        list.add(addblock);
      }
    } else {
      for (Block blockChildren : blocks) {
        IBlock[] childlist = blockChildren.searchCodeLine(line);
        if (childlist != null) {
          list.addAll(Arrays.asList(childlist));
        }
      }
      if (list.size() <= 0) {
        if (addblock != null) {
          list.add(addblock);
        }
      }
    }

    if (list.size() <= 0) {
      return null;
    }

    return list.toArray(new IBlock[0]);
  }

  /** Get the variable list. */
  @Override
  public Set<Variable> getAllVariables() {
    Set<Variable> list = new HashSet<Variable>();
    ArrayList<Block> blocks = this.getChildren();
    for (Block block : blocks) {
      Set<Variable> vars = block.getAllVariables();
      if (vars != null) {
        list.addAll(vars);
      }
    }
    if (list.size() <= 0) return null;
    return list;
  }
}
