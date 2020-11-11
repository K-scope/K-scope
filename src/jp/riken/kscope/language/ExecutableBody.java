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

import java.util.ArrayList;
import java.util.List;
import jp.riken.kscope.data.CodeLine;

/**
 * A class that represents the execution statement area of a procedure. No association is made
 * between the target area of the source code and the line information.
 *
 * @author RIKEN
 */
public class ExecutableBody extends Block {
  /** Serial number */
  private static final long serialVersionUID = 1540246071061765356L;
  /** Parent procedure */
  private Procedure parent = null;
  /** Database current cursor block */
  private transient Block currentBlock; // Working member variables used when parsing

  // ++++++++++++++++++++++++++++++++++++++++++++

  /**
   * Constructor.
   *
   * @param prnt Parent program unit
   */
  public ExecutableBody(Procedure prnt) {
    this();
    this.parent = prnt;
  }

  /** Constructor. */
  protected ExecutableBody() {
    super();
    this.set_block_start(new CodeLine("block start", 1));
    this.set_block_end(new CodeLine("block end", 1));
    currentBlock = this;
  }
  /**
   * Get block type.
   *
   * @return BlockType.BODY
   */
  @Override
  public BlockType getBlockType() {
    return BlockType.BODY;
  }
  // ++++++++++++++++++++++++++++++++++++++++++++

  protected List<ProcedureUsage> getCalls() {
    List<ProcedureUsage> calls = new ArrayList<ProcedureUsage>();
    this.getCalls(calls);
    return calls;
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  protected Block[] get_blocks() {
    ArrayList<Block> blocks = new ArrayList<Block>();
    this.get_blocks(blocks);
    Block[] block_array = new Block[blocks.size()];
    for (int i = 0; i < blocks.size(); i++) block_array[i] = blocks.get(i);
    return block_array;
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  /**
   * Returns the current block.
   *
   * @return block
   */
  public Block getCurrentBlock() {
    return this.currentBlock;
  }

  /** @param blk */
  public void setCurrentBlock(Block blk) {
    currentBlock = blk;
  }

  /**
   * Acquisition of parent program unit. <br>
   *
   * @return Parent program unit
   */
  public Procedure getParent() {
    return parent;
  }

  @Override
  protected String toStringBase() {
    return super.toStringBase();
  }

  // ------------------Block-------------------//

  protected void set_start_label(String label) {
    currentBlock.get_start().set_label(label);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++
  protected void set_end_label(String label) {
    currentBlock.get_end().set_label(label);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++
  protected void end_block() {
    currentBlock = currentBlock.get_mother();
  }

  // ---------------Selection----------------//
  /**
   * Set the start of branch processing.
   *
   * @param lineInfo Code line information
   * @param label Row label. If not, set Statement.NO_LABEL.
   * @param type Branch processing type
   */
  protected void start_selection(
      CodeLine lineInfo, String label, jp.riken.kscope.language.Selection.SelectionType type) {
    Selection new_block = new Selection(currentBlock, type);
    start_block(lineInfo, label, new_block);
  }

  /**
   * Set the end of branch processing.
   *
   * @param lineInfo Code line information
   * @param label Row label. If not, set Statement.NO_LABEL.
   */
  protected void end_selection(CodeLine lineInfo, String label) {
    end_block(lineInfo, label);
  }

  /**
   * Setting additional blocks for branch processing. Corresponds to else if statement and else
   * statement.
   *
   * @param cond Conditional expression
   */
  protected void start_condition(Expression cond, CodeLine lineInfo, String label) {
    Condition newCond = new Condition(currentBlock, cond);
    newCond.set_block_start(lineInfo);
    newCond.get_start().set_label(label);
    ((Selection) currentBlock).getConditions().add(newCond);
    currentBlock = newCond;
  }

  protected void end_condition(CodeLine lineInfo, String label) {
    end_block(lineInfo, label);
  }

  // -------------User_defined--------------//

  protected void start_user_defined(CodeLine lineInfo) {
    UserDefined new_block = new UserDefined(currentBlock);
    start_block(lineInfo, new_block);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++
  protected void start_user_defined(CodeLine lineInfo, String label) {
    UserDefined new_block = new UserDefined(currentBlock);
    start_block(lineInfo, label, new_block);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++
  protected void end_user_defined(CodeLine lineInfo) {
    end_block(lineInfo);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++
  protected void end_user_defined(CodeLine lineInfo, String label) {
    end_block(lineInfo, label);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  // -----------Procedure_usage------------//

  protected void add_procedure_usage(ProcedureUsage sub_call) {
    currentBlock.add_child(sub_call);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  /**
   * Set the start line of CALL statement and FUNCTION statement.
   *
   * @param lineInfo Code line information
   */
  protected void start_procedure_usage(CodeLine lineInfo) {
    ProcedureUsage new_block = new ProcedureUsage(currentBlock);
    start_block(lineInfo, new_block);
  }

  /**
   * Set the start line of CALL statement and FUNCTION statement.
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  protected void start_procedure_usage(CodeLine lineInfo, String label) {
    ProcedureUsage new_block = new ProcedureUsage(currentBlock);
    start_block(lineInfo, label, new_block);
  }

  /**
   * Set the start line of CALL statement and FUNCTION statement.
   *
   * @param lineInfo Code line information
   * @param label Row label
   * @param subroutineName CALL subroutine name
   * @param arguments Argument list
   */
  protected void start_procedure_usage(
      CodeLine lineInfo, String label, String subroutineName, List<Expression> arguments) {
    ProcedureUsage new_block = new ProcedureUsage(currentBlock, subroutineName, arguments);
    start_block(lineInfo, label, new_block);
  }

  /**
   * Set the start block.
   *
   * @param lineInfo Start code line information
   * @param blk Start block
   */
  protected void start_block(CodeLine lineInfo, Block blk) {
    blk.set_block_start(lineInfo);

    if (currentBlock == null) return;

    currentBlock.add_child(blk);
    currentBlock = blk;
  }

  /**
   * Set the start block. (With label)
   *
   * @param lineInfo Start code line information
   * @param label Row label
   * @param blk Start block
   */
  protected void start_block(CodeLine lineInfo, String label, Block blk) {
    start_block(lineInfo, blk);
    currentBlock.get_start().set_label(label);
  }

  /**
   * Set the end line of CALL statement and FUNCTION statement.
   *
   * @param lineInfo Code line information
   */
  protected void end_procedure_usage(CodeLine lineInfo) {
    end_block(lineInfo);
  }

  /**
   * Set the end line of CALL statement and FUNCTION statement. (With label)
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  protected void end_procedure_usage(CodeLine lineInfo, String label) {
    end_block(lineInfo, label);
  }

  /**
   * Set the exit code line information.
   *
   * @param lineInfo Exit code line information
   */
  protected void end_block(CodeLine lineInfo) {
    if (currentBlock == null) return;

    currentBlock.set_block_end(lineInfo);
    currentBlock = currentBlock.get_mother();
  }

  /**
   * Set the end block. (With label)
   *
   * @param lineInfo Exit code line information
   * @param label Row label
   */
  protected void end_block(CodeLine lineInfo, String label) {
    currentBlock.set_block_end(lineInfo);
    currentBlock.get_end().set_label(label);
    currentBlock = currentBlock.get_mother();
  }

  /**
   * Set the start line of the DO statement.
   *
   * @param lineInfo Code line information
   */
  protected void start_repetition(CodeLine lineInfo) {
    Repetition new_block = new Repetition(currentBlock);
    start_block(lineInfo, new_block);
  }

  /**
   * Set the end line of the DO statement.
   *
   * @param lineInfo Code line information
   */
  protected void end_repetition(CodeLine lineInfo) {
    end_block(lineInfo);
  }

  /**
   * Set the start line of the DO statement. (With label)
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  protected void start_repetition(CodeLine lineInfo, String label) {
    Repetition new_block = new Repetition(currentBlock);
    start_block(lineInfo, label, new_block);
  }

  /**
   * Set the end line of the DO statement. (With label)
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  protected void end_repetition(CodeLine lineInfo, String label) {
    end_block(lineInfo, label);
  }

  /**
   * Set the CONTINUE statement.
   *
   * @param lineInfo Code line information
   * @param label Row label
   */
  protected void set_continue(CodeLine lineInfo, String label) {
    if (currentBlock.get_start().is_labeled()) {
      if (label.equals(currentBlock.get_start().get_label())) {
        end_repetition(lineInfo, label);
        set_continue(lineInfo, label);
      }
    }
  }

  /**
   * Set the start line of the Substitution statement.
   *
   * @param lineInfo Code line information
   */
  protected void startSubstitution(CodeLine lineInfo) {
    Substitution new_block = new Substitution(currentBlock);
    start_block(lineInfo, new_block);
  }

  protected void endSubstitution(CodeLine lineInfo) {
    end_block(lineInfo);
  }

  /**
   * Set the Return statement.
   *
   * @param lineInfo Code line information
   */
  protected void setReturn(CodeLine lineInfo) {
    Return new_block = new Return(currentBlock);
    start_block(lineInfo, new_block);
    end_block(lineInfo);
  }

  /**
   * Get an ID.
   *
   * @return ID
   */
  @Override
  public String getID() {
    String result = "";
    if (this.parent != null) {
      // modify by @hira at 2013/03/01
      // int offset = this.getStartPos() - this.parent.getStartPos();
      // result = this.parent.getID() + "#" + offset + ":" + this.toStringBase();
      result = this.parent.getID() + ":" + this.toStringBase();
    } else {
      result = this.toStringBase();
    }
    return result;
  }

  /**
   * Get the namespace (module name.routine name).
   *
   * @return namespace (module name.routine name)
   */
  @Override
  public String getNamespace() {
    String result = "";
    if (this.parent != null) {
      result = this.parent.getNamespace();
    }
    return result;
  }

  /**
   * Get the structure ID.
   *
   * @return Structure ID
   */
  @Override
  public String getLayoutID() {
    String result = "";
    if (this.parent != null) {
      result = this.parent.getLayoutID() + ":";
    }
    return result;
  }
}
