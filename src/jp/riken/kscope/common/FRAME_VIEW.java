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
 * Mainframe view screen identification enumeration class
 *
 * @author RIKEN
 */
public enum FRAME_VIEW {
  // View list
  /** main frame */
  MAIN_FRAME(Message.getString("frame_view.enum.main")), // main frame
  /** Explorer view */
  EXPLORE_VIEW(Message.getString("mainmenu.window.explore")), // Explorer view
  /** Source view */
  SOURCE_VIEW(Message.getString("mainmenu.window.source")), // Source view
  /** Analysis view */
  ANALYSIS_VIEW(Message.getString("mainmenu.window.analysis")); // Analysis view

  /** View name */
  private String viewname;

  /**
   * Constructor
   *
   * @param tabname View name
   */
  private FRAME_VIEW(String viewname) {
    this.viewname = viewname;
  }

  /**
   * Get the view name
   *
   * @return View name
   */
  public String getViewName() {
    return this.viewname;
  }
}
