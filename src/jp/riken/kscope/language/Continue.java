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
 * Control statement class that jumps to the end of iterative processing.
 *
 * @author RIKEN
 */
public class Continue extends jp.riken.kscope.language.Block {
  /** Serial number */
  private static final long serialVersionUID = 1600666700399053469L;
  /** Label */
  private String label;

  /**
   * Constructor.
   *
   * @param parent Parent block
   */
  public Continue(Block parent) {
    super(parent);
  }

  /**
   * Constructor.
   *
   * @param parent Parent block
   * @param lbl Label
   */
  public Continue(Block parent, String lbl) {
    super(parent);
    label = lbl;
  }

  /** Constructor. */
  public Continue() {
    super();
  }

  /**
   * Get block type.
   *
   * @return BlockType.CONTINUE
   */
  public BlockType getBlockType() {
    return BlockType.CONTINUE;
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
      return "cycle";
    } else {
      return "cycle " + this.label;
    }
  }
}
