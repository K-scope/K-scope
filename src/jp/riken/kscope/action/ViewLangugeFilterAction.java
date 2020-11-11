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
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.common.FILTER_TYPE;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.service.AppController;

/**
 * Structure tree filter action class
 *
 * @author RIKEN
 */
public class ViewLangugeFilterAction extends ActionBase {

  /** Filter type */
  private FILTER_TYPE filter;

  /**
   * Constructor
   *
   * @param controller Application controller
   * @param filter Filter type
   */
  public ViewLangugeFilterAction(AppController controller, FILTER_TYPE filter) {
    super(controller);
    this.filter = filter;
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    boolean checked = false;
    if (event.getSource() instanceof JCheckBoxMenuItem) {
      // Checkbox menu
      JCheckBoxMenuItem item = (JCheckBoxMenuItem) event.getSource();
      checked = item.isSelected();
    }

    // Structural tree model
    if (this.filter != FILTER_TYPE.DEFAULT) {
      if (checked) {
        // add to
        this.controller.addListLanguageFilter(this.filter);
      } else {
        // Delete
        this.controller.removeListLanguageFilter(this.filter);
      }
    } else {
      // Structure tree filter default settings
      FILTER_TYPE[] filters = KscopeProperties.LANGUGE_DEFAULTFILTERS;
      this.controller.setListLanguageFilter(filters);
    }

    // Activate the Structure Tree Panel.
    if (this.controller.getMainframe().getPanelExplorerView().getSelectedEnumPanel()
        != EXPLORE_PANEL.LANGUAGE) {
      this.controller
          .getMainframe()
          .getPanelExplorerView()
          .setSelectedPanel(EXPLORE_PANEL.LANGUAGE);
    }
  }

  /**
   * Get the structure tree filter type
   *
   * @return Structure tree filter type
   */
  public FILTER_TYPE getFilter() {
    return filter;
  }

  /**
   * Set the structure tree filter type.
   *
   * @param filter Structure tree filter type
   */
  public void setFilter(FILTER_TYPE filter) {
    this.filter = filter;
  }
}
