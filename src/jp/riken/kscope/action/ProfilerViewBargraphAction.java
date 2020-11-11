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

package jp.riken.kscope.action;

import java.awt.event.ActionEvent;
import javax.swing.JCheckBoxMenuItem;
import jp.riken.kscope.service.AppController;

/**
 * Profiler source cost graph display switching action class
 *
 * @author RIKEN
 */
public class ProfilerViewBargraphAction extends ActionBase {

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public ProfilerViewBargraphAction(AppController controller) {
    super(controller);
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {
    if (!(event.getSource() instanceof JCheckBoxMenuItem)) return;

    boolean checked = false;
    // Checkbox menu
    JCheckBoxMenuItem item = (JCheckBoxMenuItem) event.getSource();
    checked = item.isSelected();
    this.controller.getPropertiesProfiler().setVisibleBargraph(checked);

    // Fire a change event
    this.controller.getPropertiesProfiler().firePropertyChange();
  }
}
