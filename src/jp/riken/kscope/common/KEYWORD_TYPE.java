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
package jp.riken.kscope.common;

import jp.riken.kscope.Message;

/**
 * Keyword type
 *
 * @author RIKEN
 */
public enum KEYWORD_TYPE {
  // Keyword type
  /** Reserved words, keywords */
  KEYWORD(Message.getString("keyword_type.enum.reserved")), // Reserved words, keywords
  /** Text search */
  SEARCH(Message.getString("keyword_type.enum.textsearch")), // Text search
  /** Trace */
  TRACE(Message.getString("mainmenu.window.analysis.trace")), // trace
  /** Variable access destination memory */
  VARIABLE(
      Message.getString("keyword_type.enum.variablememory")), // Variable access destination memory
  /** Unknown */
  UNKNOWN(Message.getString("explore_panel.enum.unknown")); // unknown

  /** Type name */
  private String name;

  /**
   * Constructor
   *
   * @param name Type name
   */
  private KEYWORD_TYPE(String name) {
    this.name = name;
  }

  /**
   * Get the type name
   *
   * @return type name
   */
  public String getName() {
    return this.name;
  }
}
