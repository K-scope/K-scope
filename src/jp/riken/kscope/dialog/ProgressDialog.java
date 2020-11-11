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

package jp.riken.kscope.dialog;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Observable;
import java.util.Observer;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.service.FutureService;

/**
 * Progress display dialog
 *
 * @author RIKEN
 */
public class ProgressDialog extends javax.swing.JDialog implements ActionListener, Observer {

  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** progress bar */
  private JProgressBar progressBar;
  /** Progress message */
  private JLabel lblMessageStatus;
  /** Cancel button */
  private JButton btnCancel;
  /** Close button */
  private JButton btnClose;

  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;

  /** Thread task service */
  private FutureService<Integer> threadService;

  /**
   * Progress bar uncertain: Interval time (ms). <br>
   * PROGRESSBAR_CYCLETIME / PROGRESSBAR_INTERVAL = even number
   */
  private final int PROGRESSBAR_INTERVAL = 200;
  /** Progress bar uncertain: Maximum time (ms) */
  private final int PROGRESSBAR_CYCLETIME = 4000;

  /**
   * Constructor
   *
   * @param frame Parent frame
   */
  public ProgressDialog(JFrame frame) {
    super(frame);
    initGUI();
  }

  /**
   * Constructor
   *
   * @param owner parent frame
   * @param modal true = Show modal dialog
   */
  public ProgressDialog(Frame owner, boolean modal) {
    super(owner, modal);
    initGUI();
  }

  /** Initialize the GUI. */
  private void initGUI() {
    try {
      getContentPane().setLayout(null);
      this.setResizable(false);
      {
        progressBar = new JProgressBar();
        getContentPane().add(progressBar);
        progressBar.setBounds(40, 40, 360, 20);
      }
      {
        lblMessageStatus = new JLabel();
        getContentPane().add(lblMessageStatus);
        lblMessageStatus.setBounds(40, 70, 360, 20);
      }
      {
        btnClose = new JButton();
        getContentPane().add(btnClose);
        btnClose.setText(Message.getString("progressdialog.button.invisible")); // Hide
        btnClose.setBounds(94, 112, 100, 22);
        btnClose.addActionListener(this);
      }
      {
        btnCancel = new JButton();
        getContentPane().add(btnCancel);
        btnCancel.setText(Message.getString("dialog.common.button.cancel")); // Cancel
        btnCancel.setBounds(235, 112, 101, 22);
        btnCancel.addActionListener(this);
      }

      this.setTitle(Message.getString("progressdialog.dialog.title")); // Please wait
      this.setSize(440, 180);

      progressBar.setMinimum(0);
      progressBar.setMaximum(100);
      progressBar.setIndeterminate(true);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Display a dialog.
   *
   * @return Button type when the dialog is closed
   */
  public int showDialog() {

    // Display in the center of the parent frame.
    this.setLocationRelativeTo(this.getOwner());

    // Dialog display
    this.setVisible(true);

    return this.result;
  }

  /**
   * Button click event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {
    if (event.getSource() == this.btnClose) {
      this.result = Constant.OK_DIALOG;
      // Close the dialog.
      dispose();
      return;
    } else if (event.getSource() == this.btnCancel) {
      this.result = Constant.OK_DIALOG;

      // Cancel the thread
      if (this.threadService != null) {
        this.threadService.cancel(true);
      }

      // Close the dialog.
      dispose();
      return;
    }
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
            ProgressDialog.this.lblMessageStatus.setText(message);
          }
        });
  }

  /**
   * Progress update notification
   *
   * @param o Notification source
   * @param arg Notification item
   */
  @Override
  public void update(Observable o, Object arg) {

    Application.StatusPrint status = (Application.StatusPrint) o;

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
  }

  /**
   * Set up thread task service
   *
   * @param threadService Thread task service
   */
  public void setThreadService(FutureService<Integer> threadService) {
    this.threadService = threadService;
  }
}
