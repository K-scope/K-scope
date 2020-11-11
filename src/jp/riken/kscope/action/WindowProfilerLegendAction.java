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

import java.awt.Frame;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import javax.swing.JComponent;
import jp.riken.kscope.dialog.ProfilerLegendDialog;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.service.AppController;

/**
 * View profiler legend Action class
 *
 * @author RIKEN
 */
public class WindowProfilerLegendAction extends ActionBase {

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public WindowProfilerLegendAction(AppController controller) {
    super(controller);
  }

  /**
   * Check if the action is executable. <br>
   * Check before executing the action and switch the menu enable. <br>
   *
   * @return true = Action can be executed
   */
  @Override
  public boolean validateAction() {
    return true;
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    // Get the parent Frame.
    Frame frame = getWindowAncestor(event);

    // Display the profiler legend dialog.
    showLegendWindow(frame);
  }

  /**
   * Display the profiler legend dialog.
   *
   * @param frame Parent frame
   */
  public void showLegendWindow(Frame frame) {

    // Profiler properties
    ProfilerProperties properties = this.controller.getPropertiesProfiler();

    // Display the profiler legend dialog.
    ProfilerLegendDialog dialog = this.controller.getMainframe().getDialogProfilerLegend();
    if (dialog == null) {
      dialog = new ProfilerLegendDialog(frame, false);
      // Set the display position
      Rectangle dialogRect = dialog.getBounds();
      JComponent view = this.controller.getMainframe().getPanelSourceView();
      Point location = view.getLocationOnScreen();
      int viewWidth = view.getWidth();
      int viewHeight = view.getHeight();
      dialog.setLocation(
          location.x + viewWidth - dialogRect.width, location.y + viewHeight - dialogRect.height);
      this.controller.getMainframe().setDialogProfilerLegend(dialog);
    }
    dialog.setProperties(properties);

    // Dialog display
    dialog.showDialog();
  }
}
