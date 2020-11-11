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

import java.util.Set;
import jp.riken.kscope.data.CodeLine;

/**
 * Interface to get the row information of the block.
 *
 * @author RIKEN
 */
public interface IBlock {

  /**
   * Get the start line number information.
   *
   * @return Start line number information
   */
  CodeLine getStartCodeLine();

  /**
   * Get the end line number information.
   *
   * @return End line number information
   */
  CodeLine getEndCodeLine();

  /**
   * Returns the block type.
   *
   * @return block type
   */
  BlockType getBlockType();

  /**
   * Returns the block type.
   *
   * @return block type
   */
  IBlock getMotherBlock();

  /** Get the variable list of the expression. Variable list of @return expression */
  Set<Variable> getAllVariables();
}
