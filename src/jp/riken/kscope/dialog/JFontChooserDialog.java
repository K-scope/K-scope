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
import java.awt.Font;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListModel;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;

/**
 * Font selection dialog
 *
 * @author RIKEN
 */
public class JFontChooserDialog extends javax.swing.JDialog
    implements ActionListener, ListSelectionListener, DocumentListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Font preview */
  private JTextField txtPreview;
  /** Font size list */
  private JList<String> lstSize;
  /** Font style list */
  private JList<String> lstStyle;
  /** Font list */
  private JList<String> lstFont;
  /** Font size text box */
  private JTextField txtSize;
  /** Cancel button */
  private JButton btnCancel;
  /** OK button */
  private JButton btnOk;

  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;

  /** Font style */
  private final String FONTSTYLE_PLAIN = "Plain";

  private final String FONTSTYLE_BOLD = "Bold";
  private final String FONTSTYLE_ITALIC = "Italic";
  private final String FONTSTYLE_BOLDITALIC = "Bold Italic";

  /** Constructor */
  public JFontChooserDialog() {
    super();
    initGUI();
    setDefualtFont(null);
  }

  /**
   * Constructor
   *
   * @param owner parent frame
   */
  public JFontChooserDialog(Frame owner) {
    super(owner);
    initGUI();
    setDefualtFont(null);
  }

  /**
   * Constructor
   *
   * @param owner Parent dialog
   * @param modal true = Show modal dialog
   */
  public JFontChooserDialog(Frame owner, boolean modal) {
    super(owner, modal);
    initGUI();
    setDefualtFont(null);
  }

  /**
   * Constructor
   *
   * @param owner Parent dialog
   * @param modal true = Show modal dialog
   * @param deffont Default font
   */
  public JFontChooserDialog(Frame owner, boolean modal, Font deffont) {
    super(owner, modal);
    initGUI();
    setDefualtFont(deffont);
  }

  /**
   * Constructor
   *
   * @param owner Parent dialog
   */
  public JFontChooserDialog(JDialog owner) {
    super(owner);
    initGUI();
    setDefualtFont(null);
  }

  /**
   * Constructor
   *
   * @param owner Parent dialog
   * @param modal true = Show modal dialog
   */
  public JFontChooserDialog(JDialog owner, boolean modal) {
    super(owner, modal);
    initGUI();
    setDefualtFont(null);
  }

  /**
   * Constructor
   *
   * @param owner Parent dialog
   * @param modal true = Show modal dialog
   * @param deffont Default font
   */
  public JFontChooserDialog(JDialog owner, boolean modal, Font deffont) {
    super(owner, modal);
    initGUI();
    setDefualtFont(deffont);
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
   * Returns the selected font. <br>
   * Returns null if the cancel button is clicked.
   *
   * @return Selected font
   */
  public Font getSelectedFont() {
    if (this.result == Constant.CANCEL_DIALOG) return null;

    return this.txtPreview.getFont();
  }

  /** Initialize the GUI. */
  private void initGUI() {
    try {
      // Button panel
      {
        JPanel panelButton = new JPanel();
        FlowLayout panelButtonLayout = new FlowLayout();
        panelButtonLayout.setHgap(10);
        panelButtonLayout.setVgap(10);
        panelButton.setLayout(panelButtonLayout);
        getContentPane().add(panelButton, BorderLayout.SOUTH);
        panelButton.setPreferredSize(new java.awt.Dimension(423, 44));

        // Main button size
        java.awt.Dimension buttonSize = new java.awt.Dimension(96, 22);
        {
          btnOk = new JButton();
          panelButton.add(btnOk);
          btnOk.setText(Message.getString("dialog.common.button.ok")); // OK
          btnOk.setPreferredSize(buttonSize);
          btnOk.addActionListener(this);
        }
        {
          btnCancel = new JButton();
          panelButton.add(btnCancel);
          btnCancel.setText(Message.getString("dialog.common.button.cancel")); // Cancel
          btnCancel.setPreferredSize(buttonSize);
          btnCancel.addActionListener(this);
          btnCancel.setMargin(new Insets(5, 5, 5, 5));
        }
      }
      // Content panel
      {
        JPanel panelContent = new JPanel();
        GridBagLayout panelContentLayout = new GridBagLayout();
        panelContentLayout.rowWeights = new double[] {1.0, 0.3};
        panelContentLayout.rowHeights = new int[] {7, 64};
        panelContentLayout.columnWeights = new double[] {0.1};
        panelContentLayout.columnWidths = new int[] {7};
        panelContent.setLayout(panelContentLayout);
        getContentPane().add(panelContent, BorderLayout.CENTER);
        panelContent.setPreferredSize(new java.awt.Dimension(423, 144));
        // Font panel
        {
          JPanel panelFont = new JPanel();
          TitledBorder titleBorder =
              new TitledBorder(
                  new EtchedBorder(EtchedBorder.LOWERED),
                  Message.getString("jfontchooserdialog.fontpanel.title")); // font
          Border border = new CompoundBorder(new EmptyBorder(7, 7, 0, 7), titleBorder);
          panelFont.setBorder(border);

          GridBagLayout panelFontLayout = new GridBagLayout();
          panelFontLayout.columnWidths = new int[] {7, 96, 74};
          panelFontLayout.rowHeights = new int[] {7, 7, 7};
          panelFontLayout.columnWeights = new double[] {1.0, 0, 0};
          panelFontLayout.rowWeights = new double[] {0.0, 0, 1.0};
          panelContent.add(
              panelFont,
              new GridBagConstraints(
                  0,
                  0,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.CENTER,
                  GridBagConstraints.BOTH,
                  new Insets(0, 0, 0, 0),
                  0,
                  0));
          panelFont.setLayout(panelFontLayout);
          // font name
          {
            JLabel lblName = new JLabel();
            panelFont.add(
                lblName,
                new GridBagConstraints(
                    0,
                    0,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.NONE,
                    new Insets(0, 10, 0, 0),
                    0,
                    0));
            lblName.setText(Message.getString("jfontchooserdialog.fontpanel.title")); // font
          }
          // Font list
          {
            JScrollPane scrollFont = new JScrollPane();
            panelFont.add(
                scrollFont,
                new GridBagConstraints(
                    0,
                    1,
                    1,
                    2,
                    0.0,
                    0.0,
                    GridBagConstraints.CENTER,
                    GridBagConstraints.BOTH,
                    new Insets(0, 10, 10, 0),
                    0,
                    0));
            {
              ListModel<String> llstFontModel =
                  new DefaultComboBoxModel<String>(new String[] {"Item One", "Item Two"});
              lstFont = new JList<String>();
              scrollFont.setViewportView(lstFont);
              lstFont.setModel(llstFontModel);
              lstFont.addListSelectionListener(this);
              lstFont.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
            }
          }
          // Style
          {
            JLabel lblStyle = new JLabel();
            panelFont.add(
                lblStyle,
                new GridBagConstraints(
                    1,
                    0,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.NONE,
                    new Insets(0, 10, 0, 0),
                    0,
                    0));
            lblStyle.setText(
                Message.getString("jfontchooserdialog.fontpanel.label.style")); // style
          }
          // Style list
          {
            JScrollPane scrollStyle = new JScrollPane();
            panelFont.add(
                scrollStyle,
                new GridBagConstraints(
                    1,
                    1,
                    1,
                    2,
                    0.0,
                    0.0,
                    GridBagConstraints.CENTER,
                    GridBagConstraints.BOTH,
                    new Insets(0, 10, 10, 0),
                    0,
                    0));
            {
              ListModel<String> lstStyleModel =
                  new DefaultComboBoxModel<String>(
                      new String[] {
                        FONTSTYLE_PLAIN, FONTSTYLE_BOLD, FONTSTYLE_ITALIC, FONTSTYLE_BOLDITALIC
                      });
              lstStyle = new JList<String>();
              scrollStyle.setViewportView(lstStyle);
              lstStyle.setModel(lstStyleModel);
              lstStyle.addListSelectionListener(this);
              lstStyle.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
            }
          }
          // size
          {
            JLabel lblSize = new JLabel();
            panelFont.add(
                lblSize,
                new GridBagConstraints(
                    2,
                    0,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.NONE,
                    new Insets(0, 10, 0, 0),
                    0,
                    0));
            lblSize.setText(Message.getString("jfontchooserdialog.fontpanel.label.size")); // size
          }
          // size list
          {
            JScrollPane scrollSize = new JScrollPane();
            panelFont.add(
                scrollSize,
                new GridBagConstraints(
                    2,
                    2,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.CENTER,
                    GridBagConstraints.BOTH,
                    new Insets(0, 10, 10, 10),
                    0,
                    0));
            {
              ListModel<String> lstSizeModel =
                  new DefaultComboBoxModel<String>(
                      new String[] {
                        "8", "9", "10", "11", "12", "13", "14", "16", "18", "20", "22", "24", "26",
                        "28", "32", "36", "40", "48", "56", "64", "72"
                      });
              lstSize = new JList<String>();
              scrollSize.setViewportView(lstSize);
              lstSize.setModel(lstSizeModel);
              lstSize.addListSelectionListener(this);
              lstSize.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
            }
          }
          // size text box
          {
            txtSize = new JTextField();
            panelFont.add(
                txtSize,
                new GridBagConstraints(
                    2,
                    1,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.CENTER,
                    GridBagConstraints.HORIZONTAL,
                    new Insets(0, 10, 0, 10),
                    0,
                    0));
            txtSize.setText("10");
            txtSize.getDocument().addDocumentListener(this);
          }
        }
        // Font preview
        {
          JPanel panelPreview = new JPanel();
          TitledBorder titleBorder =
              new TitledBorder(
                  new EtchedBorder(EtchedBorder.LOWERED),
                  Message.getString("jfontchooserdialog.button.label.preview")); // preview
          Border margin = new EmptyBorder(0, 7, 7, 7);
          Border inBorder = new CompoundBorder(titleBorder, margin);
          Border border = new CompoundBorder(new EmptyBorder(7, 7, 0, 7), inBorder);
          panelPreview.setBorder(border);
          Dimension previewSize = new Dimension(433, 80);
          panelPreview.setPreferredSize(previewSize);
          panelPreview.setMinimumSize(previewSize);

          BorderLayout panelPreviewLayout = new BorderLayout();
          panelContent.add(
              panelPreview,
              new GridBagConstraints(
                  0,
                  1,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.CENTER,
                  GridBagConstraints.BOTH,
                  new Insets(0, 0, 0, 0),
                  0,
                  0));
          panelPreview.setLayout(panelPreviewLayout);
          {
            txtPreview = new JTextField();
            panelPreview.add(txtPreview, BorderLayout.CENTER);
            txtPreview.setText(Message.getString("jfontchooserdialog.button.label.sample"));
          }
        }
      }
      this.setTitle(Message.getString("jfontchooserdialog.dialog.title")); // Font selection
      this.setSize(433, 342);

      // Set the font list
      setEnvironmentFontList();

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /** Set the font list. */
  private void setEnvironmentFontList() {
    // Get font name
    GraphicsEnvironment env = GraphicsEnvironment.getLocalGraphicsEnvironment();
    String[] fontNames = env.getAvailableFontFamilyNames();

    // Create a list model
    DefaultListModel<String> model = new DefaultListModel<String>();
    if (fontNames != null && fontNames.length > 0) {
      for (int i = 0; i < fontNames.length; i++) {
        model.addElement(fontNames[i]);
      }
    }
    this.lstFont.setModel(model);
  }

  /**
   * Set the default font
   *
   * @param deffont Default font
   */
  private void setDefualtFont(Font deffont) {

    if (deffont == null) {
      deffont = this.txtPreview.getFont();
    }

    // font name
    this.lstFont.setSelectedValue(deffont.getFamily(), true);

    // Style
    int style = deffont.getStyle();
    if (style == Font.BOLD) {
      this.lstStyle.setSelectedValue(this.FONTSTYLE_BOLD, true);
    } else if (style == Font.ITALIC) {
      this.lstStyle.setSelectedValue(this.FONTSTYLE_ITALIC, true);
    } else if (style == Font.BOLD + Font.ITALIC) {
      this.lstStyle.setSelectedValue(this.FONTSTYLE_BOLDITALIC, true);
    } else {
      this.lstStyle.setSelectedValue(this.FONTSTYLE_PLAIN, true);
    }

    // size
    int size = deffont.getSize();
    this.lstSize.setSelectedValue(String.valueOf(size), true);
  }

  /** Preview the font. */
  private void previewFont() {
    // font name
    String name = this.lstFont.getSelectedValue();
    if (name == null || name.isEmpty()) return;

    // Style
    String stylevalue = this.lstStyle.getSelectedValue();
    int style = Font.PLAIN;
    if (stylevalue != null) {
      if (stylevalue.equalsIgnoreCase(this.FONTSTYLE_BOLD)) {
        style = Font.BOLD;
      } else if (stylevalue.equalsIgnoreCase(this.FONTSTYLE_ITALIC)) {
        style = Font.ITALIC;
      } else if (stylevalue.equalsIgnoreCase(this.FONTSTYLE_BOLDITALIC)) {
        style = Font.BOLD + Font.ITALIC;
      }
    }

    // size
    int size = 0;
    String sizevalue = this.txtSize.getText();
    try {
      size = Integer.parseInt(sizevalue);
    } catch (Exception e) {

    }
    if (size == 0) return;

    // Font settings
    Font font = new Font(name, style, size);
    this.txtPreview.setFont(font);
  }

  /**
   * Button click event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    // OK
    if (event.getSource() == this.btnOk) {
      this.result = Constant.OK_DIALOG;
      // Close the dialog.
      dispose();
      return;
    }
    // Cancel
    else if (event.getSource() == this.btnCancel) {
      this.result = Constant.CANCEL_DIALOG;
      // Close the dialog.
      dispose();
      return;
    }
  }

  /**
   * List change event
   *
   * @param event Event information
   */
  @Override
  public void valueChanged(ListSelectionEvent event) {

    if (event.getSource() == this.lstSize) {
      String value = this.lstSize.getSelectedValue();
      this.txtSize.setText(value);
    }

    // Preview the font.
    previewFont();
  }

  /**
   * Insert size textbox event
   *
   * @param event Event information
   */
  @Override
  public void insertUpdate(DocumentEvent event) {}

  /**
   * Size textbox delete event
   *
   * @param event Event information
   */
  @Override
  public void removeUpdate(DocumentEvent event) {}

  /**
   * Size textbox change event
   *
   * @param event Event information
   */
  @Override
  public void changedUpdate(DocumentEvent event) {
    // Preview the font.
    previewFont();
  }
}
