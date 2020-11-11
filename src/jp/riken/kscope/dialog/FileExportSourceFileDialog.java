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

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Source file export dialog class
 *
 * @author RIKEN
 */
public class FileExportSourceFileDialog extends javax.swing.JDialog implements ActionListener {
  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Cancel button */
  private JButton btnCancel;
  /** Export button */
  private JButton btnExport;

  /** Output folder */
  private JTextField txtExportFolder;
  /** Browse button */
  private JButton btnRef;
  /** Label that outputs files other than the source */
  private JLabel lblOtherFiles;
  /** Checkbox to output files other than source */
  private JCheckBox chxOtherFiles;
  /** Excluded files */
  private JTextField txtExcludeFiles;

  /** Project folder */
  private File projectFolder;
  /** Error message */
  private String errMsg;

  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;
  /** Screen width */
  private final int DEFAULT_WIDTH = 480;

  /**
   * Constructor
   *
   * @param frame Parent frame
   * @param modal modal mode
   */
  public FileExportSourceFileDialog(Frame frame, boolean modal) {
    super(frame, modal);
    initGUI();
  }

  /**
   * Constructor
   *
   * @param frame Parent frame
   */
  public FileExportSourceFileDialog(JFrame frame) {
    super(frame);
    initGUI();
  }

  /** GUI initialization */
  private void initGUI() {
    try {

      // Button panel
      {
        JPanel panelButtons = new JPanel();
        FlowLayout jPanel1Layout = new FlowLayout();
        jPanel1Layout.setHgap(10);
        jPanel1Layout.setVgap(20);
        panelButtons.setLayout(jPanel1Layout);
        getContentPane().add(panelButtons, BorderLayout.SOUTH);
        panelButtons.setPreferredSize(new java.awt.Dimension(200, 55));

        // Main button size
        java.awt.Dimension buttonSize = new java.awt.Dimension(96, 22);

        {
          btnExport = new JButton();
          panelButtons.add(btnExport);
          String text = "";
          text = Message.getString("mainmenu.file.export"); // Export
          btnExport.setText(text);
          btnExport.setPreferredSize(buttonSize);
          btnExport.setMargin(new Insets(5, 5, 5, 5));
          btnExport.addActionListener(this);
        }
        {
          btnCancel = new JButton();
          panelButtons.add(btnCancel);
          btnCancel.setText(Message.getString("dialog.common.button.cancel")); // Cancel
          btnCancel.setPreferredSize(buttonSize);
          btnCancel.setMargin(new Insets(5, 5, 5, 5));
          btnCancel.addActionListener(this);
        }
      }

      // Content panel
      {
        JPanel panelContent = new JPanel(new BorderLayout());
        getContentPane().add(panelContent, BorderLayout.CENTER);
        GridBagLayout layoutContent = new GridBagLayout();
        layoutContent.rowWeights = new double[] {0.0, 0.0, 0.0, 1.0};
        layoutContent.rowHeights = new int[] {32, 32, 32, 7};
        layoutContent.columnWeights = new double[] {0.0, 0.0, 1.0, 0.0};
        layoutContent.columnWidths = new int[] {14, 32, 220, 50};
        panelContent.setLayout(layoutContent);

        // Output folder panel
        {
          panelContent.add(
              new JLabel(
                  Message.getString(
                      "fileexportsourcefiledialog.label.outputfolder")), // Output folder
              new GridBagConstraints(
                  1,
                  0,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.EAST,
                  GridBagConstraints.NONE,
                  new Insets(20, 0, 0, 8),
                  0,
                  0));
          txtExportFolder = new JTextField();
          panelContent.add(
              txtExportFolder,
              new GridBagConstraints(
                  2,
                  0,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.WEST,
                  GridBagConstraints.HORIZONTAL,
                  new Insets(20, 0, 0, 0),
                  0,
                  0));
          btnRef = new JButton();
          btnRef.setText(Message.getString("dialog.common.button.refer")); // reference
          btnRef.setPreferredSize(new java.awt.Dimension(48, 22));
          btnRef.setMaximumSize(new java.awt.Dimension(48, 22));
          btnRef.setMargin(new Insets(0, 3, 0, 3));
          btnRef.addActionListener(this);
          panelContent.add(
              btnRef,
              new GridBagConstraints(
                  3,
                  0,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.WEST,
                  GridBagConstraints.NONE,
                  new Insets(20, 8, 0, 8),
                  0,
                  0));
        }
        // File panel other than the source file
        {
          lblOtherFiles =
              new JLabel(
                  Message.getString(
                      "fileexportsourcefiledialog.label.excludefile")); // Excluded files
          txtExcludeFiles = new JTextField();
          txtExcludeFiles.setToolTipText(
              Message.getString(
                  "fileexportsourcefiledialog.tooltip.excludefile")); // List the file names to
          // exclude, separated by
          // commas. ...
          chxOtherFiles =
              new JCheckBox(
                  Message.getString(
                      "fileexportsourcefiledialog.checkbox.excludefile"), // Output files other than
                  // source
                  false) {
                /** Serial number */
                private static final long serialVersionUID = 1L;

                @Override
                protected void fireStateChanged() {
                  lblOtherFiles.setEnabled(this.isSelected());
                  txtExcludeFiles.setEnabled(this.isSelected());
                }
              };
          lblOtherFiles.setEnabled(chxOtherFiles.isSelected());
          txtExcludeFiles.setEnabled(chxOtherFiles.isSelected());
          panelContent.add(
              chxOtherFiles,
              new GridBagConstraints(
                  1,
                  1,
                  3,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.WEST,
                  GridBagConstraints.NONE,
                  new Insets(10, 8, 0, 8),
                  0,
                  0));
          panelContent.add(
              lblOtherFiles,
              new GridBagConstraints(
                  1,
                  2,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.EAST,
                  GridBagConstraints.NONE,
                  new Insets(0, 8, 0, 8),
                  0,
                  0));
          panelContent.add(
              txtExcludeFiles,
              new GridBagConstraints(
                  2,
                  2,
                  2,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.WEST,
                  GridBagConstraints.HORIZONTAL,
                  new Insets(0, 8, 0, 8),
                  0,
                  0));
        }
      }

      this.setTitle(
          Message.getString("fileexportsourcefiledialog.dialog.title")); // Source file export
      this.pack();

      Dimension size = new Dimension(DEFAULT_WIDTH, this.getSize().height);
      this.setSize(size);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Setting value error check
   *
   * @return true = No problem
   */
  private boolean chkParams() {
    // Output destination not set NG if it does not exist or is not a directory
    String str = txtExportFolder.getText();
    if (StringUtils.isNullOrEmpty(str)) {
      errMsg =
          Message.getString(
              "fileexportsourcefiledialog.errdlg.msg.outputfolderunset"); // The output destination
      // folder is not set.
      return false;
    }
    File f = new File(str);
    if (!f.exists()) {
      errMsg =
          Message.getString(
              "fileexportsourcefiledialog.errdlg.msg.outputfoldernotexist"); // The specified output
      // folder does not
      // exist.
      return false;
    }
    if (!f.isDirectory()) {
      errMsg =
          Message.getString(
              "fileexportsourcefiledialog.errdlg.msg.outputfoldernotdir"); // The specified output
      // folder is not a
      // directory.
      return false;
    }

    return true;
  }

  @Override
  public void actionPerformed(ActionEvent event) {
    if (event.getSource() == this.btnExport) {
      if (!chkParams()) {
        JOptionPane.showMessageDialog(
            this,
            errMsg,
            Message.getString("dialog.common.error"), // error
            JOptionPane.ERROR_MESSAGE);
      } else {
        result = Constant.OK_DIALOG;
        dispose();
      }
      return;
    } else if (event.getSource() == this.btnCancel) {
      result = Constant.CANCEL_DIALOG;
      dispose();
      return;
    } else if (event.getSource() == this.btnRef) {
      // Display the folder selection dialog.
      File[] selected =
          SwingUtils.showOpenFolderDialog(
              this,
              Message.getString(
                  "fileexportsourcefiledialog.outputfolderselectdialog.title"), // Select output
              // destination
              // folder
              this.projectFolder.getAbsolutePath(),
              false);
      if (selected == null || selected.length <= 0) return;
      this.txtExportFolder.setText(selected[0].getAbsolutePath());
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
   * Set the project folder
   *
   * @param project Folder
   */
  public void setProjectFolder(File projectFolder) {
    if (projectFolder != null && projectFolder.exists() && projectFolder.isDirectory()) {
      this.projectFolder = projectFolder;
    } else {
      String usrFolderPath = System.getProperty("user.dir");
      this.projectFolder = new File(usrFolderPath);
    }
  }

  /**
   * Get the output destination folder path
   *
   * @return Absolute path of project folder
   */
  public String getOutputFolder() {
    return this.txtExportFolder.getText();
  }

  /**
   * Set exclusion file string
   *
   * @param exclude Exclude file string
   */
  public void setExcludeFile(String exclude) {
    txtExcludeFiles.setText(exclude);
  }

  /**
   * Get exclusion file pattern
   *
   * @return Excluded file pattern
   */
  public String getExcludeFilePattern() {
    return this.txtExcludeFiles.getText();
  }

  /**
   * Whether to output other than the source file
   *
   * @return true = Output other than source file
   */
  public boolean isExportOtherFile() {
    return this.chxOtherFiles.isSelected();
  }
}
