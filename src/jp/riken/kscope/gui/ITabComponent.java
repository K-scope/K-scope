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

// import jp.riken.kscope.common.ANALYSIS_PANEL;

/**
 * Tab component interface. <br>
 * Interface of components that make up all tabs, such as tab pine and text pine of child elements
 *
 * @author RIKEN
 */
public interface ITabComponent {

  /**
   * Get the parent component. <br>
   * Get this.parentCompornent
   *
   * @return Parent component
   */
  public ITabComponent getParentComponent();

  /**
   * Set the parent component. <br>
   * Set to this.parentCompornent.
   *
   * @param component Parent component
   */
  public void setParentComponent(ITabComponent component);

  /**
   * Set focus listener
   *
   * @param listener Focus listener
   */
  public void addTabFocusListener(TabFocusListener listener);

  /** Close tab */
  public void closeTabComponent();
}
