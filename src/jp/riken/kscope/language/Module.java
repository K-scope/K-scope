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
 * Module A class that represents a program unit.
 *
 * @author RIKEN
 */
public class Module extends ProgramUnit {
  /** Serial number */
  private static final long serialVersionUID = -9149612813518825146L;

  /**
   * Constructor.
   *
   * @param m_name Module name
   */
  public Module(String m_name) {
    super("module", m_name);
  }

  /**
   * Get block type.
   *
   * @return BlockType.MODULE
   */
  @Override
  public BlockType getBlockType() {
    return BlockType.MODULE;
  }

  @Override
  protected String toStringBase() {
    return ("Module " + this.get_name());
  }

  /**
   * Returns an array of program units belonging to the module.
   *
   * @return An array of program units. If not, it returns an empty array.
   */
  public Procedure[] get_procedures() {
    Procedure[] subs = new Procedure[super.get_num_of_child()];
    for (int i = 0; i < subs.length; i++) {
      subs[i] = get_children()[i];
    }
    return subs;
  }

  protected Procedure get_procedure(String sub_name) {
    return ((Procedure) super.get_child(sub_name));
  }

  /**
   * Get an ID.
   *
   * @return ID
   */
  @Override
  public String getID() {
    return this.toStringBase();
  }
}
