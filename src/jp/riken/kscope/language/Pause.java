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
 * The class corresponding to the control statement that suspends the program.
 *
 * @author RIKEN
 */
public class Pause extends jp.riken.kscope.language.Block {
  /** Serial number */
  private static final long serialVersionUID = -2492364246002232811L;

  private String argument;

  /**
   * Constructor.
   *
   * @param parent Parent block
   */
  public Pause(Block parent) {
    super(parent);
  }

  /**
   * Constructor.
   *
   * @param parent Parent block
   * @param argmnt Arguments (messages, etc.)
   */
  public Pause(Block parent, String argmnt) {
    super(parent);
    argument = argmnt;
  }

  /** Constructor. */
  public Pause() {
    super();
  }

  /**
   * Get block type.
   *
   * @return BlockType.PAUSE
   */
  public BlockType getBlockType() {
    return BlockType.PAUSE;
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
   * Get arguments (messages, etc.).
   *
   * @return argument (message etc.)
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
      return "pause";
    } else {
      return "pause " + this.argument;
    }
  }
}
