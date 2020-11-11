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
 * Explorer (tree) panel identification string
 *
 * @author RIKEN
 */
public enum EXPLORE_PANEL {
  // Panel list
  /** Structural panel */
  LANGUAGE(Message.getString("mainmenu.window.explore.structure")), // Construction
  /** Module panel */
  MODULE(Message.getString("mainmenu.window.explore.module")), // module
  /** Source panel */
  SOURCE(Message.getString("mainmenu.window.explore.source")), // Source
  /** XML panel */
  XML(Message.getString("mainmenu.window.explore.xml")), // XML
  /** Unknown */
  UNKNOWN(Message.getString("explore_panel.enum.unknown")); // unknown

  /** Tab name */
  private String tabname;

  /**
   * Constructor
   *
   * @param tabname Tab name
   */
  private EXPLORE_PANEL(String tabname) {
    this.tabname = tabname;
  }

  /**
   * Get the tab name
   *
   * @return tab name
   */
  public String getTabName() {
    return this.tabname;
  }
}
