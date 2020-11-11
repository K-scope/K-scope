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
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.*;
import javax.swing.BoxLayout;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.BoxView;
import javax.swing.text.ComponentView;
import javax.swing.text.Element;
import javax.swing.text.IconView;
import javax.swing.text.LabelView;
import javax.swing.text.ParagraphView;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledEditorKit;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
// import javax.swing.border.EtchedBorder;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Additional information edit dialog
 *
 * @author RIKEN
 */
public class InformationDialog extends javax.swing.JDialog implements ActionListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Additional Information Edit Pine */
  private JEditorPane editorInformation;
  /** OK button */
  private JButton btnOk;
  /** Cancel button */
  private JButton btnCancel;
  /** Browse button */
  private JButton btnRef;
  /** Clear button */
  private JButton btnClear;
  /** Delete button */
  private JButton btnDelete;
  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;
  /** Project folder */
  private File projectFolder;
  /** Additional information */
  private String information;
  /** Cut button */
  private JButton btnCut;
  /** Copy button */
  private JButton btnCopy;
  /** Paste button */
  private JButton btnPaste;
  /** Additional information: Block */
  private JLabel labelBlock;
  /** Editable */
  private boolean editable;

  /**
   * Constructor
   *
   * @param owner parent frame
   * @param modal true = Show modal dialog
   */
  public InformationDialog(Frame owner, boolean modal) {
    super(owner, modal);
    initGUI();
  }

  /** Initialize the GUI. */
  private void initGUI() {
    try {
      BorderLayout thisLayout = new BorderLayout();
      getContentPane().setLayout(thisLayout);

      {
        // Button panel
        {
          JPanel panelButtons = new JPanel();
          FlowLayout layoutButtons = new FlowLayout();
          layoutButtons.setHgap(10);
          layoutButtons.setVgap(10);
          panelButtons.setLayout(layoutButtons);
          getContentPane().add(panelButtons, BorderLayout.SOUTH);
          panelButtons.setPreferredSize(new java.awt.Dimension(390, 48));

          java.awt.Dimension buttonSize = new java.awt.Dimension(112, 22);
          {
            btnOk = new JButton();
            btnOk.setPreferredSize(buttonSize);
            btnOk.setText(Message.getString("dialog.common.button.ok")); // OK
            btnOk.addActionListener(this);
            panelButtons.add(btnOk);
          }
          {
            btnDelete = new JButton();
            btnDelete.setPreferredSize(buttonSize);
            btnDelete.setText(Message.getString("dialog.common.button.delete")); // Delete
            btnDelete.addActionListener(this);
            panelButtons.add(btnDelete);
          }
          {
            btnCancel = new JButton();
            btnCancel.setPreferredSize(buttonSize);
            btnCancel.setText(Message.getString("dialog.common.button.cancel")); // Cancel
            btnCancel.addActionListener(this);
            panelButtons.add(btnCancel);
          }
        }
        // Additional information
        {
          JPanel panelContent = new JPanel();
          GridBagLayout panelContentLayout = new GridBagLayout();
          panelContentLayout.columnWidths = new int[] {10, 64, 10, 10};
          panelContentLayout.rowHeights = new int[] {32, 20, 10, 10};
          panelContentLayout.columnWeights = new double[] {0.0, 0.0, 1.0, 0.0};
          panelContentLayout.rowWeights = new double[] {0.0, 0.0, 1.0, 0.0};
          getContentPane().add(panelContent, BorderLayout.CENTER);
          panelContent.setLayout(panelContentLayout);
          panelContent.setPreferredSize(new java.awt.Dimension(390, 230));
          // Label
          {
            JLabel label = new JLabel();
            panelContent.add(
                label,
                new GridBagConstraints(
                    1,
                    0,
                    2,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.NONE,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
            label.setText(Message.getString("mainmenu.edit.info")); // Edit additional information
          }
          // Additional information: Label
          {
            JLabel label = new JLabel();
            panelContent.add(
                label,
                new GridBagConstraints(
                    1,
                    1,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.NORTHEAST,
                    GridBagConstraints.NONE,
                    new Insets(0, 0, 0, 7),
                    0,
                    0));
            label.setText(
                Message.getString("informationdialog.label.information")); // Additional information
          }
          // Additional information: Block
          {
            labelBlock = new JLabel();
            panelContent.add(
                labelBlock,
                new GridBagConstraints(
                    2,
                    1,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.CENTER,
                    GridBagConstraints.HORIZONTAL,
                    new Insets(0, 0, 0, 7),
                    0,
                    0));
            labelBlock.setText(Message.getString("informationdialog.label.block")); // BLOCK
          }
          // Additional information text box
          {
            editorInformation = new JEditorPane();
            editorInformation.setEditorKit(new NoWrapEditorKit());

            JScrollPane scrollEditor = new JScrollPane(editorInformation);
            // Tab size = 4
            SwingUtils.setTabSize(editorInformation, 4);
            editorInformation.putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, Boolean.TRUE);
            scrollEditor.setHorizontalScrollBarPolicy(
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollEditor.setVerticalScrollBarPolicy(
                ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
            panelContent.add(
                scrollEditor,
                new GridBagConstraints(
                    2,
                    2,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.CENTER,
                    GridBagConstraints.BOTH,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
          }

          // Button panel
          {
            JPanel panelSubButtons = new JPanel();
            // FlowLayout panelSubButtonsLayout = new FlowLayout();
            // panelSubButtonsLayout.setAlignment(FlowLayout.LEADING);
            // panelSubButtonsLayout.setHgap(10);
            // panelSubButtonsLayout.setVgap(0);
            // panelSubButtons.setLayout(panelSubButtonsLayout);
            // panelContent.add(panelSubButtons, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0,
            // GridBagConstraints.SOUTHEAST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0),
            // 0, 0));

            panelSubButtons.setLayout(new BoxLayout(panelSubButtons, BoxLayout.X_AXIS));

            EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
            Border borderKeyword = new CompoundBorder(titleBorder, new EmptyBorder(0, 0, 0, 0));
            panelSubButtons.setBorder(borderKeyword);

            getContentPane().add(panelSubButtons, BorderLayout.NORTH);

            java.awt.Dimension minSize = new java.awt.Dimension(64, 64);
            {
              Icon icon = ResourceUtils.getIcon("button_copy.png");
              btnCopy = new JButton(icon);
              panelSubButtons.add(btnCopy);
              btnCopy.setPreferredSize(minSize);
              btnCopy.addActionListener(this);
              btnCopy.setContentAreaFilled(false);
              btnCopy.setBorderPainted(false);
            }
            {
              Icon icon = ResourceUtils.getIcon("button_paste.png");
              btnPaste = new JButton(icon);
              panelSubButtons.add(btnPaste);
              btnPaste.setPreferredSize(minSize);
              btnPaste.addActionListener(this);
              btnPaste.setContentAreaFilled(false);
              btnPaste.setBorderPainted(false);
            }
            {
              Icon icon = ResourceUtils.getIcon("button_cut.png");
              btnCut = new JButton(icon);
              panelSubButtons.add(btnCut);
              btnCut.setPreferredSize(minSize);
              btnCut.addActionListener(this);
              btnCut.setContentAreaFilled(false);
              btnCut.setBorderPainted(false);
            }
            {
              Icon icon = ResourceUtils.getIcon("button_filepaste.png");
              btnRef = new JButton(icon);
              panelSubButtons.add(btnRef);
              btnRef.setPreferredSize(minSize);
              btnRef.addActionListener(this);
              btnRef.setContentAreaFilled(false);
              btnRef.setBorderPainted(false);
            }
            {
              Icon icon = ResourceUtils.getIcon("button_clear.png");
              btnClear = new JButton(icon);
              panelSubButtons.add(btnClear);
              btnClear.setPreferredSize(minSize);
              btnClear.addActionListener(this);
              btnClear.setContentAreaFilled(false);
              btnClear.setBorderPainted(false);
            }

            // Tooltip settings
            btnRef.setToolTipText(
                Message.getString("informationdialog.button.filepaste.tooltip")); // Paste file
            btnCut.setToolTipText(
                Message.getString("informationdialog.button.cut.tooltip")); // Cut out
            btnCopy.setToolTipText(
                Message.getString("informationdialog.button.copy.tooltip")); // copy
            btnPaste.setToolTipText(
                Message.getString("informationdialog.button.paste.tooltip")); // pasting
            btnClear.setToolTipText(
                Message.getString("informationdialog.button.clear.tooltip")); // clear
          }
        }
      }
      this.setTitle(Message.getString("mainmenu.edit.info")); // Edit additional information
      this.setSize(640, 400);

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

    // Delete
    if (event.getSource() == this.btnDelete) {
      // Confirm deletion
      int option =
          JOptionPane.showConfirmDialog(
              this,
              Message.getString(
                  "informationdialog.confirmdialog.delete.message"), // Are you sure you want to
              // delete the additional
              // information?
              Message.getString("informationdialog.confirmdialog.delete.title"), // Confirm deletion
              JOptionPane.OK_CANCEL_OPTION);
      if (option != JOptionPane.OK_OPTION) return;

      this.setInformation("");
      this.result = Constant.DELETE_DIALOG;

      // Close the dialog.
      dispose();
      return;
    }
    // OK
    else if (event.getSource() == this.btnOk) {
      // Check registration of additional information
      if (validateInformation()) {
        this.setInformation(this.editorInformation.getText());
        this.result = Constant.OK_DIALOG;
        // Close the dialog.
        dispose();
        return;
      }
    }
    // Cancel
    else if (event.getSource() == this.btnCancel) {
      this.result = Constant.CANCEL_DIALOG;
      // Close the dialog.
      dispose();
      return;
    }
    // Browse button
    else if (event.getSource() == this.btnRef) {
      // Display the file selection dialog.
      File[] selected =
          SwingUtils.showOpenFileDialog(
              this,
              Message.getString("informationdialog.selectfiledialog.title"), // Select file
              projectFolder.getAbsolutePath(),
              null,
              true);
      if (selected == null || selected.length <= 0) return;

      try {
        StringBuffer buf = new StringBuffer();
        int offset = this.editorInformation.getCaretPosition();
        if (offset > 0) {
          // If the caret position is in the middle of the line, insert a line break.
          String prev_char = editorInformation.getDocument().getText(offset - 1, 1);
          if (!("\n".equals(prev_char))) {
            buf.append("\n");
          }
        }
        for (int i = 0; i < selected.length; i++) {
          // Add the file path to the current caret position
          String path = FileUtils.getRelativePath(selected[i], this.projectFolder);
          if (path == null) {
            JOptionPane.showMessageDialog(
                this,
                Message.getString(
                    "informationdialog.errordialog.notexist.message",
                    selected[i]), // [file] does not exist.
                Message.getString("dialog.common.error"), // error
                JOptionPane.ERROR_MESSAGE);
            continue;
          }
          buf.append(path);
          buf.append("\n");
        }
        SimpleAttributeSet attr = new SimpleAttributeSet();
        editorInformation.getDocument().insertString(offset, buf.toString(), attr);
        this.editorInformation.setCaretPosition(offset + buf.length());

        // Move focus to additional information text box
        this.editorInformation.requestFocus();

      } catch (BadLocationException ex) {
        ex.printStackTrace();
      }
    }
    // Cut button
    else if (event.getSource() == this.btnCut) {
      this.editorInformation.cut();
    }
    // Copy button
    else if (event.getSource() == this.btnCopy) {
      this.editorInformation.copy();
    }
    // Paste button
    else if (event.getSource() == this.btnPaste) {
      this.editorInformation.paste();
    }
    // clear
    else if (event.getSource() == this.btnClear) {
      // Clear the information.
      this.editorInformation.setText("");
    }

    return;
  }

  /**
   * Check the input.
   *
   * @return Success or failure
   */
  private boolean validateInformation() {

    // Information
    String content = this.editorInformation.getText();
    content = StringUtils.trim(content);
    if (content == null || content.isEmpty()) {
      JOptionPane.showMessageDialog(
          this,
          Message.getString(
              "informationdialog.errordialog.informationempty.message"), // Please enter additional
          // information.
          Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);
      return false;
    }

    return true;
  }

  /**
   * Set the project folder
   *
   * @param folder Project folder
   */
  public void setProjectFolder(File folder) {
    this.projectFolder = folder;
  }

  /**
   * Get additional information
   *
   * @return Additional information
   */
  public String getInformation() {
    return this.information;
  }

  /**
   * Set additional information block.
   *
   * @param block Additional information block
   */
  public void setBlockName(String block) {
    this.labelBlock.setText(block);
  }

  /**
   * Set additional information.
   *
   * @param information Additional information
   */
  public void setInformation(String information) {
    this.information = information;
    this.editorInformation.setText(information);
  }

  /**
   * Get editability.
   *
   * @return Editable
   */
  public boolean isEditable() {
    return editable;
  }

  /**
   * Set editability.
   *
   * @param editable Editable
   */
  public void setEditable(boolean editable) {
    this.editable = editable;
    if (this.btnOk != null) {
      this.btnOk.setEnabled(this.editable);
    }
    if (this.btnDelete != null) {
      this.btnDelete.setEnabled(this.editable);
    }
  }

  /**
   * Line wrap paragraph view class
   *
   * @author RIKEN
   */
  private class NoWrapParagraphView extends ParagraphView {
    /**
     * Constructor
     *
     * @param elem Elements handled by this view
     */
    public NoWrapParagraphView(Element elem) {
      super(elem);
    }

    /**
     * Calculate the row width size requirement. <br>
     * Set the wrapping size for one line.
     *
     * @param axis Line position
     * @param r Component size and position object
     * @return Component size and position object
     */
    @Override
    protected SizeRequirements calculateMinorAxisRequirements(int axis, SizeRequirements r) {
      SizeRequirements req = super.calculateMinorAxisRequirements(axis, r);
      req.minimum = req.preferred;
      return req;
    }

    /**
     * Fetches the constraint span that flows against the specified child index.
     *
     * @param index Index of the queried view
     * @return View constraint span
     */
    @Override
    public int getFlowSpan(int index) {
      return Integer.MAX_VALUE;
    }
  }

  /**
   * View creation class
   *
   * @author RIKEN
   */
  class NoWrapViewFactory implements ViewFactory {
    /**
     * Create a view based on the element.
     *
     * @param elem Element to be created
     * @return view
     */
    @Override
    public View create(Element elem) {
      String kind = elem.getName();
      if (kind != null) {
        if (kind.equals(AbstractDocument.ContentElementName)) {
          return new LabelView(elem);
        } else if (kind.equals(AbstractDocument.ParagraphElementName)) {
          return new NoWrapParagraphView(elem);
        } else if (kind.equals(AbstractDocument.SectionElementName)) {
          return new BoxView(elem, View.Y_AXIS);
        } else if (kind.equals(StyleConstants.ComponentElementName)) {
          return new ComponentView(elem);
        } else if (kind.equals(StyleConstants.IconElementName)) {
          return new IconView(elem);
        }
      }
      return new LabelView(elem);
    }
  }

  /**
   * Formatted text style
   *
   * @author RIKEN
   */
  @SuppressWarnings("serial")
  class NoWrapEditorKit extends StyledEditorKit {

    /**
     * Get the view creation class.
     *
     * @return View creation class
     */
    @Override
    public ViewFactory getViewFactory() {
      return new NoWrapViewFactory();
    }
  }
}
