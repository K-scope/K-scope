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

package jp.riken.kscope.menu;

import java.awt.event.ActionListener;
import java.util.EventListener;

/**
 * Tree pop-up menu interface
 *
 * @author RIKEN
 */
public interface ITreePopupMenu {

  /**
   * Get all storage action listeners
   *
   * @return All storage action listener
   */
  public ActionListener getActionTreeCollapseAll();

  /**
   * Get all deployment action listeners
   *
   * @return All deployment action listener
   */
  public ActionListener getActionTreeExpandAll();

  /**
   * Get Selective Deployment Action Listener
   *
   * @return Selective expansion action listener
   */
  public ActionListener getActionTreeExpandSelect();

  /**
   * Get the listener to open the file.
   *
   * @return Open file Listener
   */
  public EventListener getActionOpenFile();

  /**
   * Get the explorer export action
   *
   * @return explorer export action
   */
  public ActionListener getActionExportExplore();
}
