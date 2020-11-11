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

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.Observable;
import java.util.Observer;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.border.BevelBorder;
import jp.riken.kscope.Application;

/**
 * Status bar panel
 *
 * @author RIKEN
 */
public class StatusBarPanel extends javax.swing.JPanel implements Observer {

  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Main message */
  private JLabel lblMessageMain;
  /** Location message */
  private JLabel lblMessageLocation;
  /** Status message */
  private JLabel lblMessageStatus;
  /** progress bar */
  private JProgressBar progressBar;

  /**
   * Progress bar uncertain: Interval time (ms). <br>
   * PROGRESSBAR_CYCLETIME / PROGRESSBAR_INTERVAL = even number
   */
  private final int PROGRESSBAR_INTERVAL = 200;
  /** Progress bar uncertain: Maximum time (ms) */
  private final int PROGRESSBAR_CYCLETIME = 2000;

  /** Constructor */
  public StatusBarPanel() {
    super();
    initGUI();
  }

  /** Initialize the GUI */
  private void initGUI() {
    try {
      GridBagLayout thisLayout = new GridBagLayout();
      thisLayout.rowWeights = new double[] {0.1};
      thisLayout.rowHeights = new int[] {7};
      thisLayout.columnWeights = new double[] {0.1, 0, 0, 0, 0.1, 0};
      thisLayout.columnWidths = new int[] {240, 10, 240, 10, 160, 120};
      this.setLayout(thisLayout);
      this.setPreferredSize(new java.awt.Dimension(626, 34));
      {
        lblMessageMain = new JLabel();
        this.add(
            lblMessageMain,
            new GridBagConstraints(
                0,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.EAST,
                GridBagConstraints.HORIZONTAL,
                new Insets(5, 5, 5, 5),
                0,
                0));
      }
      {
        JSeparator spcMain = new JSeparator(SwingConstants.VERTICAL);
        spcMain.setPreferredSize(new Dimension(4, 22));
        spcMain.setMinimumSize(new Dimension(4, 22));
        spcMain.setBorder(new BevelBorder(BevelBorder.LOWERED));
        this.add(
            spcMain,
            new GridBagConstraints(
                1,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.CENTER,
                GridBagConstraints.VERTICAL,
                new Insets(5, 0, 5, 0),
                0,
                0));
      }
      {
        Dimension labelsize = new Dimension(120, 22);
        lblMessageLocation = new JLabel();
        this.add(
            lblMessageLocation,
            new GridBagConstraints(
                2,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.EAST,
                GridBagConstraints.HORIZONTAL,
                new Insets(5, 5, 5, 5),
                0,
                0));
        lblMessageLocation.setPreferredSize(labelsize);
        lblMessageLocation.setMaximumSize(labelsize);
        lblMessageLocation.setMinimumSize(labelsize);
      }
      {
        JSeparator spcStatus = new JSeparator(SwingConstants.VERTICAL);
        spcStatus.setPreferredSize(new Dimension(4, 22));
        spcStatus.setMinimumSize(new Dimension(4, 22));
        spcStatus.setBorder(new BevelBorder(BevelBorder.LOWERED));
        this.add(
            spcStatus,
            new GridBagConstraints(
                3,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.CENTER,
                GridBagConstraints.VERTICAL,
                new Insets(5, 0, 5, 0),
                0,
                0));
      }
      {
        progressBar = new JProgressBar();
        progressBar.setBorder(new BevelBorder(BevelBorder.LOWERED));
        progressBar.setStringPainted(false);
        progressBar.setPreferredSize(new Dimension(120, 22));
        progressBar.setMaximumSize(new Dimension(120, 22));
        this.add(
            progressBar,
            new GridBagConstraints(
                5,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.CENTER,
                GridBagConstraints.HORIZONTAL,
                new Insets(5, 5, 5, 5),
                0,
                0));
      }
      {
        lblMessageStatus = new JLabel();
        this.add(
            lblMessageStatus,
            new GridBagConstraints(
                4,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.EAST,
                GridBagConstraints.HORIZONTAL,
                new Insets(0, 5, 0, 5),
                0,
                0));
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Set the main message
   *
   * @param message Main message
   */
  private void setMessageMain(final String message) {

    SwingUtilities.invokeLater(
        new Runnable() {
          @Override
          public void run() {
            StatusBarPanel.this.lblMessageMain.setText(message);
          }
        });
  }

  /**
   * Set location message
   *
   * @param message Location message
   */
  private void setMessageLocation(final String message) {

    SwingUtilities.invokeLater(
        new Runnable() {
          @Override
          public void run() {
            StatusBarPanel.this.lblMessageLocation.setText(message);
          }
        });
  }

  /**
   * Set status message
   *
   * @param message Status message
   */
  private void setMessageStatus(final String message) {

    SwingUtilities.invokeLater(
        new Runnable() {
          @Override
          public void run() {
            StatusBarPanel.this.lblMessageStatus.setText(message);
          }
        });
  }

  /**
   * Message, progress update notification
   *
   * @param o Notification source
   * @param arg Notification item
   */
  @Override
  public void update(Observable o, Object arg) {

    try {
      Application.StatusPrint status = (Application.StatusPrint) o;

      // Main message
      String mainMessage = status.getMessageMain();
      setMessageMain(mainMessage);

      // Location message
      String locationMessage = status.getMessageLocation();
      setMessageLocation(locationMessage);

      // Status message
      String statusMessage = status.getMessageStatus();
      setMessageStatus(statusMessage);

      // progress bar
      if (status.isProgressStart()) {
        // Start progress bar
        Integer value = status.getProgressValue();
        Integer min = status.getProgressMin();
        Integer max = status.getProgressMax();
        if (value != null && min != null && max != null) {
          progressBar.setMinimum(min);
          progressBar.setMaximum(max);
          progressBar.setValue(value);
        } else {
          progressBar.setIndeterminate(true);
          UIManager.put("ProgressBar.repaintInterval", new Integer(PROGRESSBAR_INTERVAL));
          UIManager.put("ProgressBar.cycleTime", new Integer(PROGRESSBAR_CYCLETIME));
        }
      } else {
        // Progress bar stop
        progressBar.setMinimum(0);
        progressBar.setMaximum(0);
        progressBar.setValue(0);
        progressBar.setIndeterminate(false);
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
