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
package jp.riken.kscope.gui;

import java.io.File;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.properties.SourceProperties;

/**
 * Analysis Information Panel Component Interface. <br>
 * Analysis Information Panel Component Interface
 *
 * @author RIKEN
 */
public interface IAnalisysComponent {

  /**
   * Export analysis information
   *
   * @param file Output file
   */
  public void export(File file);

  /**
   * Set an action listener on the panel. <br>
   * Assign the created action listener to the menu bar to the panel button.
   *
   * @param menu Main menu
   */
  public void setActionListener(MainMenu menu);

  /**
   * Set the analysis information panel identifier
   *
   * @return enumPanel Analysis Information Panel Identifier
   */
  public ANALYSIS_PANEL getEnumPanel();

  /** Clear the model. */
  public void clearModel();

  /** Close the tab */
  public void closeTab();

  /**
   * Get selected source code line information
   *
   * @return Selected source code line information
   */
  public CodeLine getSelectedCodeLine();

  /**
   * Get the selected block
   *
   * @return selection block
   */
  public IBlock getSelectedBlock();

  /**
   * Get additional selection information
   *
   * @return Selectable additional information
   */
  public IInformation getSelectedInformation();

  /**
   * Set source view properties
   *
   * @param properties Source view properties
   */
  void setSourceProperties(SourceProperties properties);

  /** Copy the selection to the clipboard. */
  public void copyClipboard();

  /**
   * Determine if there is information that can be exported
   *
   * @return true: With information false: No information
   */
  public boolean isExportable();
}
