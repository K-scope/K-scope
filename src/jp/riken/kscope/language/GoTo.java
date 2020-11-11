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

/**
 * Class corresponding to the control statement that moves to the specified location.
 *
 * @author RIKEN
 */
public class GoTo extends jp.riken.kscope.language.Block {
  /** Serial number */
  private static final long serialVersionUID = 3580553803434734668L;

  private String argument;

  /**
   * Constructor.
   *
   * @param parent Parent block
   */
  public GoTo(Block parent) {
    super(parent);
  }

  /**
   * Constructor.
   *
   * @param parent Parent block
   * @param argmnt Arguments (messages, etc.)
   */
  public GoTo(Block parent, String argmnt) {
    super(parent);
    argument = argmnt;
  }

  /** Constructor. */
  public GoTo() {
    super();
  }

  /**
   * Get block type.
   *
   * @return BlockType.GOTO
   */
  public BlockType getBlockType() {
    return BlockType.GOTO;
  }

  /**
   * A set of arguments.
   *
   * @param str Argument
   */
  public void setArgument(String str) {
    this.argument = str;
  }

  /**
   * Get arguments.
   *
   * @return argument
   */
  public String getArgument() {
    return argument;
  }

  @Override
  public String toString() {
    return this.toStringBase();
  }

  @Override
  protected String toStringBase() {
    if (this.argument == null) {
      return "goto";
    } else {
      return "goto " + this.argument;
    }
  }
}
