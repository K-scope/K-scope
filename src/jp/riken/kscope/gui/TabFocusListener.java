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

import java.awt.Component;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

/**
 * Focus listener. <br>
 * Get the component that is currently in focus.
 *
 * @author RIKEN
 */
public class TabFocusListener implements FocusListener {

  /** Final focus component */
  private ITabComponent lastTabComponent = null;

  /**
   * Focus acquisition event
   *
   * @param event Event information
   */
  @Override
  public void focusGained(FocusEvent event) {
    Component forcus = event.getComponent();
    if (forcus instanceof ITabComponent) {
      this.lastTabComponent = (ITabComponent) forcus;
    }
  }

  /**
   * Loss of focus event
   *
   * @param event Event information
   */
  @Override
  public void focusLost(FocusEvent event) {}

  /**
   * Get the final focus component.
   *
   * @return Final focus component
   */
  public ITabComponent getLastTabComponent() {
    return this.lastTabComponent;
  }
}
