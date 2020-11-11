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
 * Class corresponding to the control statement that exits the iterative process.
 *
 * @author RIKEN
 */
public class Break extends jp.riken.kscope.language.Block {
  /** Serial number */
  private static final long serialVersionUID = -2588592106946309459L;
  /** Label */
  private String label;

  /**
   * Constructor.
   *
   * @param parent Parent block
   */
  public Break(Block parent) {
    super(parent);
  }

  /**
   * Constructor.
   *
   * @param parent Parent block
   * @param lbl Label
   */
  public Break(Block parent, String lbl) {
    super(parent);
    label = lbl;
  }

  /** Constructor. */
  public Break() {
    super();
  }

  /**
   * Get block type.
   *
   * @return BlockType.BREAK
   */
  public BlockType getBlockType() {
    return BlockType.BREAK;
  }

  /**
   * Get label.
   *
   * @return label
   */
  public String getLabel() {
    return label;
  }

  @Override
  public String toString() {
    return this.toStringBase();
  }

  @Override
  protected String toStringBase() {
    if (this.label == null) {
      return "exit";
    } else {
      return "exit " + this.label;
    }
  }
}
