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
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Rectangle2D;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.component.JColorButton;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Profiler settings dialog
 *
 * @author RIKEN
 */
public class SettingProfilerDialog extends javax.swing.JDialog
    implements ActionListener, TreeSelectionListener {
  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Cancel button */
  private JButton btnCancel;
  /** Apply button */
  private JButton btnApply;
  /** OK button */
  private JButton btnOk;
  /** Profiler settings list */
  private JTree treeProperties;
  /** Profiler setting list data */
  private DefaultTreeModel modelProperties;
  /** Profiler settings panel */
  private JPanel panelProperty;

  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;

  /** Profiler Properties */
  private ProfilerProperties properities;
  /** Content Panel */
  private JPanel panelContent;
  /** Settings panel */
  private JPanel panelSettings;

  /** Panel type */
  private enum PANEL_TYPE {
    /** Cost bar graph color, maximum number of lines to display */
    PROFILER_VIEW,
    /** Cost ruler color */
    PROFILER_RULER,
    /** EPROF measurement section description */
    EPROF_STATEMENT_DISCRIPTION,
    /** EPROF measurement interval: Start */
    EPROF_STATEMENT_START,
    /** EPROF measurement interval: end */
    EPROF_STATEMENT_END
  }

  /**
   * Constructor
   *
   * @param frame Parent frame
   */
  public SettingProfilerDialog(Frame frame) {
    super(frame);
  }

  /**
   * Constructor
   *
   * @param frame Parent frame
   * @param modal true = Show modal dialog
   */
  public SettingProfilerDialog(Frame frame, boolean modal) {
    super(frame, modal);
  }

  /**
   * Constructor
   *
   * @param frame Parent frame
   * @param modal true = Show modal dialog
   * @param properities Profiler configuration properties
   */
  public SettingProfilerDialog(Frame frame, boolean modal, ProfilerProperties properities) {
    super(frame, modal);
    this.properities = properities;
    initGUI();
  }

  /**
   * Set profiler settings.
   *
   * @param properities Profiler configuration properties
   */
  public void setProfilerProperties(ProfilerProperties properities) {
    this.properities = properities;

    return;
  }

  /** Initialize the GUI. */
  private void initGUI() {
    try {
      BorderLayout thisLayout = new BorderLayout();
      thisLayout.setHgap(5);
      thisLayout.setVgap(5);
      getContentPane().setLayout(thisLayout);

      // Button panel
      {
        JPanel panelButtons = new JPanel();
        FlowLayout jPanel1Layout = new FlowLayout();
        jPanel1Layout.setHgap(10);
        jPanel1Layout.setVgap(10);
        panelButtons.setLayout(jPanel1Layout);
        getContentPane().add(panelButtons, BorderLayout.SOUTH);
        panelButtons.setPreferredSize(new java.awt.Dimension(390, 46));

        // Main button size
        java.awt.Dimension buttonSize = new java.awt.Dimension(96, 22);
        {
          btnApply = new JButton();
          btnApply.setText(Message.getString("dialog.common.button.apply")); // Apply
          btnApply.setPreferredSize(buttonSize);
          btnApply.addActionListener(this);
          panelButtons.add(btnApply);
        }
        {
          btnOk = new JButton();
          btnOk.setText(Message.getString("dialog.common.button.ok")); // OK
          btnOk.setPreferredSize(buttonSize);
          btnOk.addActionListener(this);
          panelButtons.add(btnOk);
        }
        {
          btnCancel = new JButton();
          btnCancel.setText(Message.getString("dialog.common.button.cancel")); // Cancel
          btnCancel.setPreferredSize(buttonSize);
          btnCancel.addActionListener(this);
          btnCancel.setMargin(new Insets(5, 5, 5, 5));
          panelButtons.add(btnCancel);
        }
      }

      // Content panel
      {
        panelContent = new JPanel();
        BorderLayout panelContentLayout = new BorderLayout();
        getContentPane().add(panelContent, BorderLayout.CENTER);
        Border border = new EmptyBorder(7, 7, 0, 7);
        panelContent.setBorder(border);
        panelContent.setLayout(panelContentLayout);

        // Property list
        {
          JPanel panelList = new JPanel();
          BorderLayout panelListLayout = new BorderLayout();
          panelList.setLayout(panelListLayout);
          panelContent.add(panelList, BorderLayout.WEST);
          {
            JLabel lblList = new JLabel();
            panelList.add(lblList, BorderLayout.NORTH);
            lblList.setText(
                Message.getString(
                    "settingprofilerdialog.label.propertieslist")); // Property setting list
          }
          {
            JScrollPane scrollList = new JScrollPane();
            scrollList.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
            scrollList.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
            panelList.add(scrollList, BorderLayout.CENTER);
            {
              this.modelProperties = createTreeModel();
              this.treeProperties = new JTree(this.modelProperties);
              scrollList.setViewportView(this.treeProperties);
              // this.treeProperties.setRootVisible(false);

              for (int i = 0; i < this.treeProperties.getRowCount(); i++) {
                this.treeProperties.expandRow(i);
              }

              DefaultTreeCellRenderer r =
                  (DefaultTreeCellRenderer) this.treeProperties.getCellRenderer();
              r.setLeafIcon(null);
              r.setOpenIcon(null);
              r.setClosedIcon(null);
            }
            Dimension size = new Dimension(240, 400);
            scrollList.setPreferredSize(size);
          }
        }
        // Settings panel
        {
          this.panelSettings = new JPanel();
          BorderLayout panelSettingsLayout = new BorderLayout();
          panelContent.add(panelSettings, BorderLayout.CENTER);
          Border borderSettings = new EmptyBorder(0, 7, 0, 0);
          panelSettings.setBorder(borderSettings);
          panelSettings.setLayout(panelSettingsLayout);
          {
            JLabel lblSettings = new JLabel();
            lblSettings.setText(Message.getString("mainmenu.project.config")); // Configuration
            panelSettings.add(lblSettings, BorderLayout.NORTH);
          }
          this.panelProperty = new JPanel();
          panelSettings.add(this.panelProperty, BorderLayout.CENTER);
          panelSettings.add(panelProperty, BorderLayout.CENTER);

          // Setting panel frame
          EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
          Border borderKeyword = new CompoundBorder(titleBorder, new EmptyBorder(17, 7, 0, 7));
          panelProperty.setBorder(borderKeyword);
        }
      }
      setTitle(Message.getString("projectsettingprofileraction.setup.status")); // Profiler settings
      setSize(680, 420);

      // Event
      this.treeProperties.addTreeSelectionListener(this);
      this.treeProperties.setSelectionRow(0);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Create profiler settings tree model.
   *
   * @return Profiler settings tree model
   */
  private DefaultTreeModel createTreeModel() {

    DefaultMutableTreeNode root =
        new DefaultMutableTreeNode(
            Message.getString("projectsettingprofileraction.setup.status")); // Profiler settings
    root.setUserObject(new TopTitlePanel());

    // Cost information display settings
    {
      DefaultMutableTreeNode node = new DefaultMutableTreeNode();
      root.add(node);
      node.setUserObject(new ProfilerViewPanel(this.properities, PANEL_TYPE.PROFILER_VIEW));
    }
    // Cost ruler display settings
    {
      DefaultMutableTreeNode node = new DefaultMutableTreeNode();
      root.add(node);
      node.setUserObject(new ProfilerRulerPanel(this.properities, PANEL_TYPE.PROFILER_RULER));
    }

    // Measurement interval setting
    {
      DefaultMutableTreeNode node = new DefaultMutableTreeNode(); // Measurement interval setting
      root.add(node);
      node.setUserObject(new EprofStatementTitlePanel());
      DefaultMutableTreeNode start = new DefaultMutableTreeNode(); // Start statement
      node.add(start);
      start.setUserObject(
          new EprofStatementPanel(this.properities, PANEL_TYPE.EPROF_STATEMENT_START));
      DefaultMutableTreeNode end = new DefaultMutableTreeNode(); // end statement
      node.add(end);
      end.setUserObject(new EprofStatementPanel(this.properities, PANEL_TYPE.EPROF_STATEMENT_END));
    }
    DefaultTreeModel model = new DefaultTreeModel(root);
    return model;
  }

  /** Profiler settings panel interface */
  private interface ProfilerPropertiesPanel {
    /** Update property settings */
    public void updateProperties();
  }

  /** Profiler settings panel */
  private class TopTitlePanel extends javax.swing.JPanel implements ProfilerPropertiesPanel {

    /** Serial number */
    private static final long serialVersionUID = 1L;

    /** Constructor */
    public TopTitlePanel() {
      initGUI();
    }

    /** Initialize the GUI. */
    private void initGUI() {

      BorderLayout panelSettingsLayout = new BorderLayout();
      this.setLayout(panelSettingsLayout);
      {
        JLabel lblSettings =
            new JLabel(
                Message.getString(
                    "projectsettingprofileraction.setup.status")); // Profiler settings
        this.add(lblSettings, BorderLayout.NORTH);
      }
      JPanel panelProperty = new JPanel();
      this.add(panelProperty, BorderLayout.CENTER);
      GridBagLayout panelPropertyLayout = new GridBagLayout();
      panelPropertyLayout.columnWidths = new int[] {80, 7, 100, 7};
      panelPropertyLayout.rowHeights = new int[] {30, 30, 30, 30, 30, 30, 7};
      panelPropertyLayout.columnWeights = new double[] {0, 0, 1, 0};
      panelPropertyLayout.rowWeights = new double[] {0, 0, 0, 0, 0, 0, 1};
      panelProperty.setLayout(panelPropertyLayout);
      panelProperty.setPreferredSize(new java.awt.Dimension(320, 234));

      // Setting panel frame
      EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
      Border borderKeyword = new CompoundBorder(titleBorder, new EmptyBorder(17, 7, 0, 7));
      panelProperty.setBorder(borderKeyword);

      {
        JLabel label = new JLabel(Message.getString("settingprofilerdialog.discription"));
        panelProperty.add(
            label,
            new GridBagConstraints(
                0,
                0,
                3,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.BOTH,
                new Insets(0, 0, 0, 0),
                0,
                0));
      }
    }

    /** Update property settings */
    @Override
    public void updateProperties() {}

    @Override
    public String toString() {
      return Message.getString("projectsettingprofileraction.setup.status"); // Profiler settings
    }
  }

  /** Measurement statement settings title panel */
  private class EprofStatementTitlePanel extends javax.swing.JPanel
      implements ProfilerPropertiesPanel {

    /** Serial number */
    private static final long serialVersionUID = 1L;
    /** Title, node display */
    private String title =
        Message.getString(
            "settingprofilerdialog.label.setupmesurementrange.title"); // Measurement interval
                                                                       // setting

    /** Constructor */
    public EprofStatementTitlePanel() {
      initGUI();
    }

    /** Initialize the GUI. */
    private void initGUI() {

      BorderLayout panelSettingsLayout = new BorderLayout();
      this.setLayout(panelSettingsLayout);
      {
        JLabel lblSettings = new JLabel(this.title);
        this.add(lblSettings, BorderLayout.NORTH);
      }
      JPanel panelProperty = new JPanel();
      this.add(panelProperty, BorderLayout.CENTER);
      GridBagLayout panelPropertyLayout = new GridBagLayout();
      panelPropertyLayout.columnWidths = new int[] {80, 7, 100, 7};
      panelPropertyLayout.rowHeights = new int[] {30, 30, 30, 30, 30, 30, 7};
      panelPropertyLayout.columnWeights = new double[] {0, 0, 1, 0};
      panelPropertyLayout.rowWeights = new double[] {0, 0, 0, 0, 0, 0, 1};
      panelProperty.setLayout(panelPropertyLayout);
      panelProperty.setPreferredSize(new java.awt.Dimension(320, 234));

      // Setting panel frame
      EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
      Border borderKeyword = new CompoundBorder(titleBorder, new EmptyBorder(17, 7, 0, 7));
      panelProperty.setBorder(borderKeyword);

      {
        JLabel label = new JLabel(Message.getString("settingprofilerdialog.discription.statement"));
        panelProperty.add(
            label,
            new GridBagConstraints(
                0,
                0,
                3,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.BOTH,
                new Insets(0, 0, 0, 0),
                0,
                0));
      }
    }

    /** Update property settings */
    @Override
    public void updateProperties() {}

    @Override
    public String toString() {
      return this.title;
    }
  }

  /** Measurement statement setting panel */
  private class EprofStatementPanel extends javax.swing.JPanel implements ProfilerPropertiesPanel {

    /** Serial number */
    private static final long serialVersionUID = 1L;
    /** Profiler settings */
    private ProfilerProperties properties;
    /** Panel type */
    private PANEL_TYPE type;
    /** Measurement statement: Function name */
    private JTextField eprofFunctionname;
    /** Measurement statement: Measurement statement statement */
    private JTextArea eprofMeasureStatement;
    /** Title, node display */
    private String title;

    /**
     * Constructor
     *
     * @param properties Profiler
     * @param type Panel type
     */
    public EprofStatementPanel(ProfilerProperties properties, PANEL_TYPE type) {
      this.properties = properties;
      this.type = type;
      initGUI();
    }

    /** Initialize the GUI. */
    private void initGUI() {
      BorderLayout panelSettingsLayout = new BorderLayout();
      this.setLayout(panelSettingsLayout);
      String functionname = null;
      String statement = null;
      if (this.type == PANEL_TYPE.EPROF_STATEMENT_START) {
        this.title =
            Message.getString("settingprofilerdialog.label.mesuermentstatement")
                + // Measurement statement
                " : "
                + Message.getString("settingprofilerdialog.label.start"); // :start
        functionname = this.properties.getEprofFunctionStart();
        statement = this.properties.getEprofStatementStart();
      } else if (this.type == PANEL_TYPE.EPROF_STATEMENT_END) {
        this.title =
            Message.getString("settingprofilerdialog.label.mesuermentstatement")
                + // Measurement statement
                " : "
                + Message.getString("settingprofilerdialog.label.end"); // : End
        functionname = this.properties.getEprofFunctionEnd();
        statement = this.properties.getEprofStatementEnd();
      }
      {
        JLabel lblSettings = new JLabel(this.title);
        this.add(lblSettings, BorderLayout.NORTH);
      }
      JPanel panelProperty = new JPanel();
      this.add(panelProperty, BorderLayout.CENTER);
      GridBagLayout panelPropertyLayout = new GridBagLayout();
      panelPropertyLayout.columnWidths = new int[] {80, 7, 100, 7};
      panelPropertyLayout.rowHeights = new int[] {30, 30, 30, 30, 30, 30, 7};
      panelPropertyLayout.columnWeights = new double[] {0, 0, 1, 0};
      panelPropertyLayout.rowWeights = new double[] {0, 0, 0, 0, 0, 0, 1};
      panelProperty.setLayout(panelPropertyLayout);
      panelProperty.setPreferredSize(new java.awt.Dimension(320, 234));

      // Setting panel frame
      EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
      Border borderKeyword = new CompoundBorder(titleBorder, new EmptyBorder(17, 7, 0, 7));
      panelProperty.setBorder(borderKeyword);

      Dimension textSize = new Dimension(50, 20);
      // Measurement statement: function name
      {
        JLabel lblName =
            new JLabel(
                Message.getString(
                    "settingprofilerdialog.label.mesurementfuncname")); // measurement function name
        panelProperty.add(
            lblName,
            new GridBagConstraints(
                0,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.NORTHWEST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
      }
      {
        this.eprofFunctionname = new JTextField(20);
        this.eprofFunctionname.setMinimumSize(textSize);
        this.eprofFunctionname.setPreferredSize(textSize);
        panelProperty.add(
            this.eprofFunctionname,
            new GridBagConstraints(
                2,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.HORIZONTAL,
                new Insets(0, 0, 0, 0),
                0,
                0));
        this.eprofFunctionname.setText(functionname);
      }
      {
        JLabel lblName =
            new JLabel(
                Message.getString(
                    "settingprofilerdialog.label.statement")); // Measurement statement
        panelProperty.add(
            lblName,
            new GridBagConstraints(
                0,
                1,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.NORTHWEST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
      }
      {
        this.eprofMeasureStatement = new JTextArea();
        this.eprofMeasureStatement.setBorder(new EtchedBorder(EtchedBorder.LOWERED));
        // this.eprofMeasureStatement.setMinimumSize(textSize);
        // this.eprofMeasureStatement.setPreferredSize(textSize);
        panelProperty.add(
            this.eprofMeasureStatement,
            new GridBagConstraints(
                2,
                1,
                1,
                3,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.BOTH,
                new Insets(0, 0, 0, 0),
                0,
                0));
        this.eprofMeasureStatement.setText(statement);
      }
      {
        JLabel label = new JLabel(Message.getString("settingprofilerdialog.discription.macros"));
        panelProperty.add(
            label,
            new GridBagConstraints(
                2,
                4,
                3,
                1,
                0.0,
                0.0,
                GridBagConstraints.NORTHWEST,
                GridBagConstraints.BOTH,
                new Insets(0, 0, 0, 0),
                0,
                0));
      }
    }

    /** Update property settings */
    @Override
    public void updateProperties() {
      if (this.type == PANEL_TYPE.EPROF_STATEMENT_START) {
        // Measurement interval statement: Start function name
        this.properties.setEprofFunctionStart(this.eprofFunctionname.getText());
        // Measurement interval statement: Start statement
        this.properties.setEprofStatementStart(this.eprofMeasureStatement.getText());
      } else if (this.type == PANEL_TYPE.EPROF_STATEMENT_END) {
        // Measurement interval statement: End function name
        this.properties.setEprofFunctionEnd(this.eprofFunctionname.getText());
        // Measurement interval statement: End statement
        this.properties.setEprofStatementEnd(this.eprofMeasureStatement.getText());
      }
    }

    @Override
    public String toString() {
      return this.title;
    }
  }

  /** Cost information display setting panel */
  private class ProfilerViewPanel extends javax.swing.JPanel
      implements ProfilerPropertiesPanel, ActionListener {

    /** Serial number */
    private static final long serialVersionUID = 1L;
    /** Profiler settings */
    private ProfilerProperties properties;
    /** Panel type */
    @SuppressWarnings("unused")
    private PANEL_TYPE type;
    /** Maximum number of cost information displayed */
    private JTextField maxCount;
    /** Cost information display color button: Procedure */
    private JColorButton btnColorProcedure;
    /** Cost information display color button: Loop */
    private JColorButton btnColorLoop;
    /** Cost information display color Button: Line */
    private JColorButton btnColorLine;
    /** Title, node display */
    private final String title =
        Message.getString(
            "settingprofilerdialog.label.profilerview.title"); // Cost information display

    /**
     * Constructor
     *
     * @param properties Profiler settings
     * @param type Panel type
     */
    public ProfilerViewPanel(ProfilerProperties properties, PANEL_TYPE type) {
      this.properties = properties;
      this.type = type;
      initGUI();
    }

    /** Initialize the GUI. */
    private void initGUI() {

      BorderLayout panelSettingsLayout = new BorderLayout();
      this.setLayout(panelSettingsLayout);
      {
        JLabel lblSettings = new JLabel();
        lblSettings.setText(this.title);
        this.add(lblSettings, BorderLayout.NORTH);
      }
      JPanel panelProperty = new JPanel();
      this.add(panelProperty, BorderLayout.CENTER);
      GridBagLayout panelPropertyLayout = new GridBagLayout();
      panelPropertyLayout.columnWidths = new int[] {80, 7, 100, 7};
      panelPropertyLayout.rowHeights = new int[] {30, 30, 30, 30, 30, 7};
      panelPropertyLayout.columnWeights = new double[] {0, 0, 1, 0};
      panelPropertyLayout.rowWeights = new double[] {0, 0, 0, 0, 0, 1};
      panelProperty.setLayout(panelPropertyLayout);
      panelProperty.setPreferredSize(new java.awt.Dimension(320, 234));

      // Setting panel frame
      EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
      Border borderKeyword = new CompoundBorder(titleBorder, new EmptyBorder(17, 7, 0, 7));
      panelProperty.setBorder(borderKeyword);

      Dimension textSize = new Dimension(50, 20);
      // Property name
      {
        JLabel lblName =
            new JLabel(
                Message.getString(
                    "settingprofilerdialog.label.maxlinenumber")); // Maximum number of rows
                                                                   // displayed in the table
        panelProperty.add(
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
                new Insets(0, 0, 0, 0),
                0,
                0));
      }
      {
        this.maxCount = new JTextField(5);
        this.maxCount.setMinimumSize(textSize);
        this.maxCount.setPreferredSize(textSize);
        panelProperty.add(
            this.maxCount,
            new GridBagConstraints(
                2,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
        this.maxCount.setText(String.valueOf(this.properties.getCostinfoMaxCount()));
      }
      {
        JLabel lblName =
            new JLabel(
                Message.getString("settingprofilerdialog.label.costinfocolor")
                    + // Cost information display color
                    ":"
                    + Message.getString("profileinfo_type.enum.procedure")); // procedure
        panelProperty.add(
            lblName,
            new GridBagConstraints(
                0,
                1,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
      }
      {
        this.btnColorProcedure = new JColorButton();
        panelProperty.add(
            this.btnColorProcedure,
            new GridBagConstraints(
                2,
                1,
                2,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
        this.btnColorProcedure.addActionListener(this);
        this.btnColorProcedure.setColor(this.properties.getCostinfoBarcolorProcedure());
      }
      {
        JLabel lblName =
            new JLabel(
                Message.getString("settingprofilerdialog.label.costinfocolor")
                    + // Cost information display color
                    ":"
                    + Message.getString("profileinfo_type.enum.loop")); // loop
        panelProperty.add(
            lblName,
            new GridBagConstraints(
                0,
                2,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
      }
      {
        this.btnColorLoop = new JColorButton();
        panelProperty.add(
            this.btnColorLoop,
            new GridBagConstraints(
                2,
                2,
                2,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
        this.btnColorLoop.addActionListener(this);
        this.btnColorLoop.setColor(this.properties.getCostinfoBarcolorLoop());
      }
      {
        JLabel lblName =
            new JLabel(
                Message.getString("settingprofilerdialog.label.costinfocolor")
                    + // Cost information display color
                    ":"
                    + Message.getString("profileinfo_type.enum.line")); // line
        panelProperty.add(
            lblName,
            new GridBagConstraints(
                0,
                3,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
      }
      {
        this.btnColorLine = new JColorButton();
        panelProperty.add(
            this.btnColorLine,
            new GridBagConstraints(
                2,
                3,
                2,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
        this.btnColorLine.addActionListener(this);
        this.btnColorLine.setColor(this.properties.getCostinfoBarcolorLine());
      }
    }

    /** Update property settings */
    @Override
    public void updateProperties() {
      // Maximum number of rows displayed in the table
      this.properties.setCostinfoMaxCount(Integer.parseInt(this.maxCount.getText()));
      // Cost information display color: Procedure
      this.properties.setCostinfoBarcolorProcedure(this.btnColorProcedure.getColor());
      // Cost information display color: Loop
      this.properties.setCostinfoBarcolorLoop(this.btnColorLoop.getColor());
      // Cost information display color: Line
      this.properties.setCostinfoBarcolorLine(this.btnColorLine.getColor());
      // Bar graph color settings
      PROFILERINFO_TYPE.setProfilerProperties(this.properties);
    }

    @Override
    public String toString() {
      return this.title;
    }

    /**
     * Display the color selection dialog
     *
     * @param event Event information
     */
    @Override
    public void actionPerformed(ActionEvent event) {

      // Cost information display color
      if (event.getSource() instanceof JColorButton) {
        JColorButton button = (JColorButton) event.getSource();
        // Color selection dialog
        Color color =
            JColorChooser.showDialog(
                this,
                Message.getString(
                    "settingprofilerdialog.colorchooserdialog.title"), // Color selection
                button.getColor());
        if (color != null) {
          // Set the color for the button
          button.setColor(color);
        }

        return;
      }
    }
  }

  /** Cost ruler display setting panel */
  private class ProfilerRulerPanel extends javax.swing.JPanel
      implements ProfilerPropertiesPanel, ActionListener, ChangeListener {

    /** Serial number */
    private static final long serialVersionUID = 1L;
    /** Profiler settings */
    private ProfilerProperties properties;
    /** Panel type */
    @SuppressWarnings("unused")
    private PANEL_TYPE type;
    /** Cost ruler: Minimum color */
    private JColorButton btnRulerMin;
    /** Cost Ruler: Maximum Color */
    private JColorButton btnRulerMax;
    /** Cost ruler: Code frame color */
    private JColorButton btnBorder;
    /** Cost ruler: Code background color */
    private JColorButton btnBackcolor;
    /** Cost ruler: Code background color: Transparent color */
    private JSlider sliderAlpha;
    /** Title, node display */
    private final String title =
        Message.getString("settingprofilerdialog.label.costruler"); // Cost ruler display

    /**
     * Constructor
     *
     * @param properties Profiler settings
     * @param type Panel type
     */
    public ProfilerRulerPanel(ProfilerProperties properties, PANEL_TYPE type) {
      this.properties = properties;
      this.type = type;
      initGUI();
    }

    /** Initialize the GUI. */
    private void initGUI() {

      BorderLayout panelSettingsLayout = new BorderLayout();
      this.setLayout(panelSettingsLayout);
      {
        JLabel lblSettings = new JLabel();
        lblSettings.setText(this.title);
        this.add(lblSettings, BorderLayout.NORTH);
      }
      JPanel panelProperty = new JPanel();
      this.add(panelProperty, BorderLayout.CENTER);
      GridBagLayout panelPropertyLayout = new GridBagLayout();
      panelPropertyLayout.columnWidths = new int[] {120, 7, 40, 120, 7};
      panelPropertyLayout.rowHeights = new int[] {30, 30, 30, 30, 30, 7};
      panelPropertyLayout.columnWeights = new double[] {0, 0, 0, 1.0, 0};
      panelPropertyLayout.rowWeights = new double[] {0, 0, 0, 0, 0, 1};
      panelProperty.setLayout(panelPropertyLayout);
      panelProperty.setPreferredSize(new java.awt.Dimension(320, 234));

      // Setting panel frame
      EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
      Border borderKeyword = new CompoundBorder(titleBorder, new EmptyBorder(17, 7, 0, 7));
      panelProperty.setBorder(borderKeyword);

      // Code display frame color
      {
        JLabel lblName =
            new JLabel(
                Message.getString(
                    "settingprofilerdialog.label.framecolor")); // KEY32 = Code display frame color
        panelProperty.add(
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
                new Insets(0, 0, 0, 0),
                0,
                0));
      }
      {
        this.btnBorder = new JColorButton();
        panelProperty.add(
            this.btnBorder,
            new GridBagConstraints(
                2,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
        this.btnBorder.setColor(this.properties.getRulerPanelBorderColor());
        this.btnBorder.addActionListener(this);
      }
      // Code display background color
      {
        JLabel lblName =
            new JLabel(
                Message.getString(
                    "settingprofilerdialog.label.background")); // KEY33 = Code display background
                                                                // color
        panelProperty.add(
            lblName,
            new GridBagConstraints(
                0,
                1,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
      }
      {
        this.btnBackcolor = new JColorButton();
        panelProperty.add(
            this.btnBackcolor,
            new GridBagConstraints(
                2,
                1,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
        this.btnBackcolor.setColor(this.properties.getRulerPanelBackColor());
        this.btnBackcolor.addActionListener(this);
      }
      {
        JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
        JLabel label =
            new JLabel(
                Message.getString(
                    "settingprofilerdialog.label.transparency")); // KEY36 = Transparency
        panel.add(label);
        this.sliderAlpha = new JSlider(0, 255);
        panel.add(this.sliderAlpha);
        panelProperty.add(
            panel,
            new GridBagConstraints(
                3,
                1,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.HORIZONTAL,
                new Insets(0, 10, 0, 0),
                0,
                0));
        this.sliderAlpha.setPreferredSize(new Dimension(120, 24));
        this.sliderAlpha.addChangeListener(this);
        Color back = this.properties.getRulerPanelBackColor();
        this.sliderAlpha.setValue(back.getAlpha());
      }
      // Cost ruler: Minimum color
      {
        JLabel lblName =
            new JLabel(
                Message.getString(
                    "settingprofilerdialog.label.costminimun")); // KEY34 = Cost Ruler: Minimum
                                                                 // Color
        panelProperty.add(
            lblName,
            new GridBagConstraints(
                0,
                2,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
      }
      {
        this.btnRulerMin = new JColorButton();
        panelProperty.add(
            this.btnRulerMin,
            new GridBagConstraints(
                2,
                2,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
        this.btnRulerMin.setColor(this.properties.getRulerColorMin());
        this.btnRulerMin.addActionListener(this);
      }
      // Cost ruler: Maximum color
      {
        JLabel lblName =
            new JLabel(
                Message.getString(
                    "settingprofilerdialog.label.costmaximun")); // KEY34 = Cost Ruler: Maximum
                                                                 // Color
        panelProperty.add(
            lblName,
            new GridBagConstraints(
                0,
                3,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
      }
      {
        this.btnRulerMax = new JColorButton();
        panelProperty.add(
            this.btnRulerMax,
            new GridBagConstraints(
                2,
                3,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
        this.btnRulerMax.setColor(this.properties.getRulerColorMax());
        this.btnRulerMax.addActionListener(this);
      }
      // Gradient panel
      {
        // Gradient color
        JPanel panelGradient =
            new JPanel() {
              /** Serial number */
              private static final long serialVersionUID = 1L;

              /** Draw a profile laura gradient. */
              @Override
              protected void paintComponent(Graphics g) {
                super.paintComponent(g);
                Graphics2D g2 = (Graphics2D) g;
                // Drawing area
                Rectangle rectDraw = this.getBounds();
                int step = 256;
                float width = (float) rectDraw.width / (float) step;
                Color minColor = ProfilerRulerPanel.this.btnRulerMin.getColor();
                Color maxColor = ProfilerRulerPanel.this.btnRulerMax.getColor();
                for (int i = 0; i < step; i++) {
                  float pos_x = (float) i * width;
                  Rectangle2D.Float rect2d = new Rectangle2D.Float(pos_x, 0, width, 40);
                  float ratio = (float) i / (float) step;
                  Color valueColor = SwingUtils.getGradientHsbColor(ratio, minColor, maxColor);
                  g2.setColor(valueColor);
                  g2.fill(rect2d);
                }
              }
            };
        panelGradient.setPreferredSize(new Dimension(60, 25));
        panelGradient.setMinimumSize(new Dimension(30, 25));
        panelGradient.setBorder(new LineBorder(Color.BLACK, 1));
        panelProperty.add(
            panelGradient,
            new GridBagConstraints(
                3,
                2,
                1,
                2,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.HORIZONTAL,
                new Insets(0, 10, 0, 0),
                0,
                0));
      }
    }

    /** Update property settings */
    @Override
    public void updateProperties() {
      // Code display frame color
      this.properties.setRulerPanelBorderColor(this.btnBorder.getColor());
      // Code display background color
      Color backcolor = this.btnBackcolor.getColor();
      if (backcolor != null) {
        Color value =
            new Color(
                backcolor.getRed(),
                backcolor.getGreen(),
                backcolor.getBlue(),
                this.sliderAlpha.getValue());
        this.properties.setRulerPanelBackColor(value);
      }
      // Cost ruler: Minimum color
      this.properties.setRulerColorMin(this.btnRulerMin.getColor());
      // Cost ruler: Maximum color
      this.properties.setRulerColorMax(this.btnRulerMax.getColor());
    }

    @Override
    public String toString() {
      return this.title;
    }

    /**
     * Display the color selection dialog
     *
     * @param event Event information
     */
    @Override
    public void actionPerformed(ActionEvent event) {

      // Cost information display color
      if (event.getSource() instanceof JColorButton) {
        JColorButton button = (JColorButton) event.getSource();
        // Color selection dialog
        Color color =
            JColorChooser.showDialog(
                this,
                Message.getString(
                    "settingprofilerdialog.colorchooserdialog.title"), // Color selection
                button.getColor());
        if (color == null) {
          return;
        }

        // Set the color for the button
        if (event.getSource() == this.btnBackcolor) {
          // Set Alpha for background color
          Color back =
              new Color(
                  color.getRed(), color.getGreen(), color.getBlue(), this.sliderAlpha.getValue());
          button.setColor(back);
        } else {
          button.setColor(color);
        }
        this.repaint();
        return;
      }
    }

    /**
     * Background transparent color slider change event
     *
     * @param event Event information
     */
    @Override
    public void stateChanged(ChangeEvent event) {
      if (event.getSource() == this.sliderAlpha) {
        Color color = this.btnBackcolor.getColor();
        Color back =
            new Color(
                color.getRed(), color.getGreen(), color.getBlue(), this.sliderAlpha.getValue());
        this.btnBackcolor.setColor(back);
      }
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

    // Apply / Register
    if (event.getSource() == this.btnOk || event.getSource() == this.btnApply) {
      // Update the coding standard settings.
      for (int i = 0; i < this.treeProperties.getRowCount(); i++) {
        TreePath path = this.treeProperties.getPathForRow(i);
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) path.getLastPathComponent();
        if (node == null) continue;
        if (node.getUserObject() instanceof ProfilerPropertiesPanel) {
          ((ProfilerPropertiesPanel) node.getUserObject()).updateProperties();
        }
      }
      // Fire a change event
      this.properities.firePropertyChange();

      // Close the dialog.
      if (event.getSource() == this.btnOk) {
        this.result = Constant.OK_DIALOG;
        dispose();
      }
      return;
    }
    // close
    else if (event.getSource() == this.btnCancel) {
      this.result = Constant.CANCEL_DIALOG;
      // Close the dialog.
      dispose();
      return;
    }
  }

  /**
   * Setting item tree selection change event
   *
   * @param event Event information
   */
  @Override
  public void valueChanged(TreeSelectionEvent event) {

    DefaultMutableTreeNode node =
        (DefaultMutableTreeNode) this.treeProperties.getLastSelectedPathComponent();
    if (node.getUserObject() == null) return;
    if (!(node.getUserObject() instanceof JPanel)) return;
    JPanel panel = (JPanel) node.getUserObject();
    this.panelSettings.removeAll();
    this.panelSettings.add(panel, BorderLayout.CENTER);
    this.panelContent.updateUI();
  }
}
