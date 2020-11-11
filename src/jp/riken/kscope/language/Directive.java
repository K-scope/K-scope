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
 * A class that supports Directive.
 *
 * @author RIKEN
 */
public class Directive extends jp.riken.kscope.language.Block {
  /** Serial number */
  private static final long serialVersionUID = -1028405799509858653L;

  private String argument;

  /**
   * Constructor.
   *
   * @param parent Parent block
   */
  public Directive(Block parent) {
    super(parent);
  }

  /**
   * Constructor.
   *
   * @param parent Parent block
   * @param argmnt Arguments (messages, etc.)
   */
  public Directive(Block parent, String argmnt) {
    super(parent);
    argument = argmnt;
  }

  /** Constructor. */
  public Directive() {
    super();
  }

  /**
   * Get block type.
   *
   * @return BlockType.DIRECTIVE
   */
  public BlockType getBlockType() {
    return BlockType.DIRECTIVE;
  }

  /**
   * A set of text.
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
}
