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

import jp.riken.kscope.information.TextInfo;

/**
 * Interface for acquiring and setting additional information
 *
 * @author RIKEN
 */
public interface IInformation {
  /**
   * Set additional information. <br>
   *
   * @param info Additional information
   */
  void setInformation(TextInfo info);

  /**
   * Get additional information.
   *
   * @return Additional information
   */
  TextInfo getInformation();

  /**
   * Get the namespace (module name.routine name).
   *
   * @return namespace (module name.routine name)
   */
  String getNamespace();

  /**
   * Get the start position.
   *
   * @return start position
   */
  int getStartPos();
  /**
   * Set the start position.
   *
   * @param pos Starting position
   */
  void setStartPos(int pos);

  /**
   * Get the end position.
   *
   * @return end position
   */
  int getEndPos();
  /**
   * Set the end position.
   *
   * @param pos End position
   */
  void setEndPos(int pos);

  /** Delete all additional information. */
  void clearInformation();

  /**
   * Get an ID.
   *
   * @return ID
   */
  String getID();

  /**
   * Get the structure ID.
   *
   * @return Structure ID
   */
  String getLayoutID();
}
