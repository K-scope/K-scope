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
 * A control statement class that does nothing.
 *
 * @author RIKEN
 */
public class DoNothing extends jp.riken.kscope.language.Block {
  /** Serial number */
  private static final long serialVersionUID = 7666146678194125791L;
  /**
   * Constructor.
   *
   * @param parent Parent block
   */
  public DoNothing(Block parent) {
    super(parent);
  }

  /** Constructor. */
  public DoNothing() {
    super();
  }

  /**
   * Get block type.
   *
   * @return BlockType.DO_NOTHING
   */
  public BlockType getBlockType() {
    return BlockType.DO_NOTHING;
  }

  @Override
  public String toString() {
    return this.toStringBase();
  }

  @Override
  protected String toStringBase() {
    return "continue";
  }
}
