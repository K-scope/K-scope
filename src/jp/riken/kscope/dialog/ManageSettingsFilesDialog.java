package jp.riken.kscope.dialog;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.util.LinkedHashMap;
import java.util.Map;
import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JViewport;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.dialog.RemoteBuildPropertiesDialog.CustomCellRenderer;
import jp.riken.kscope.properties.ProjectProperties;
import jp.riken.kscope.properties.RemoteBuildProperties;
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.Yaml;

public class ManageSettingsFilesDialog extends javax.swing.JDialog implements ActionListener {
  /** Serial number */
  private static final long serialVersionUID = 1L;

  private static boolean debug = (System.getenv("DEBUG") != null);
  private static boolean debug_l2 = false;
  private JButton btnOk;
  private JButton btnPlus;
  private JButton btnMinus;
  private JButton btnCopy;
  private int result = Constant.CANCEL_DIALOG;
  private DefaultTableModel modelProperties;
  private final String[] COLUMN_HEADERS = {
    Message.getString("managesettingsfiles.table.key"),
    Message.getString("managesettingsfiles.table.value")
  };
  private String selected_file;
  LinkedHashMap<String, String> settings;
  private JList<String> file_list;
  private DefaultListModel<String> list_model;
  private boolean edited = false; // Values in table has been changed

  public ManageSettingsFilesDialog() {
    if (debug) {
      debug_l2 = (System.getenv("DEBUG").equalsIgnoreCase("high"));
    }
    initGUI();
  }

  public int showDialog() {
    // Center on screen
    this.setLocationRelativeTo(null);
    this.setModal(true);
    // this.toFront();
    this.setVisible(true);

    return this.result;
  }

  private void initGUI() {
    if (debug) {
      System.out.println("called initGUI");
    }
    try {
      BorderLayout thisLayout = new BorderLayout();
      thisLayout.setHgap(5);
      thisLayout.setVgap(5);
      getContentPane().setLayout(thisLayout);

      // OK Button panel
      {
        JPanel panelButtons = new JPanel();
        FlowLayout jPanel1Layout = new FlowLayout();
        jPanel1Layout.setHgap(10);
        jPanel1Layout.setVgap(10);
        panelButtons.setLayout(jPanel1Layout);
        getContentPane().add(panelButtons, BorderLayout.SOUTH);
        panelButtons.setPreferredSize(new java.awt.Dimension(500, 46));
        panelButtons.setBorder(BorderFactory.createLineBorder(Color.white));

        java.awt.Dimension buttonSize = new java.awt.Dimension(96, 22);
        {
          btnOk = new JButton();
          btnOk.setText(Message.getString("dialog.common.button.ok")); // OK
          btnOk.setPreferredSize(buttonSize);
          btnOk.addActionListener(this);
          panelButtons.add(btnOk);
        }
      }

      // Top level panel
      {
        JPanel panelContent = new JPanel();
        BorderLayout panelContentLayout = new BorderLayout();
        getContentPane().add(panelContent, BorderLayout.CENTER);
        // Border border = new EmptyBorder(7,7,0,7);
        panelContent.setLayout(panelContentLayout);

        // Left panel
        {
          JPanel panelList = new JPanel();
          panelList.setPreferredSize(new Dimension(220, 250));
          panelList.setBorder(new EmptyBorder(7, 7, 0, 7));
          BorderLayout panelListLayout = new BorderLayout();
          panelList.setLayout(panelListLayout);
          panelContent.add(panelList, BorderLayout.WEST);

          // Label
          {
            JLabel lblList = new JLabel();
            lblList.setText(Message.getString("managesettingsfiles.filelist.title"));
            panelList.add(lblList, BorderLayout.NORTH);
          }

          // North panel
          JPanel panelListNorth = new JPanel();
          panelListNorth.setLayout(new BorderLayout());

          // List of files with settings in "remote" folder
          list_model = new DefaultListModel<String>();

          refreshListModel();
          file_list = new JList<String>(list_model);
          file_list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
          file_list.setLayoutOrientation(JList.VERTICAL);
          file_list.addListSelectionListener(new SharedListSelectionHandler() {});
          JScrollPane listScroller = new JScrollPane(file_list);
          // listScroller.setPreferredSize(new Dimension(200, 100));
          panelListNorth.add(listScroller, BorderLayout.CENTER);

          // South panel
          JPanel panelListSouth = new JPanel();
          FlowLayout jPanel2Layout = new FlowLayout();
          jPanel2Layout.setHgap(1);
          jPanel2Layout.setVgap(3);
          panelListSouth.setLayout(jPanel2Layout);
          panelListSouth.setPreferredSize(new java.awt.Dimension(0, 35));
          panelListSouth.setBorder(BorderFactory.createLineBorder(Color.white));

          // Buttons
          Dimension button_dimensions = new Dimension(50, 25);
          Dimension button_double_dimensions = new Dimension(100, 25);
          btnPlus = new JButton();
          btnMinus = new JButton();
          btnCopy = new JButton();
          btnPlus.setPreferredSize(button_dimensions);
          btnMinus.setPreferredSize(button_dimensions);
          btnCopy.setPreferredSize(button_double_dimensions);
          btnPlus.setText(Message.getString("managesettingsfiles.button.plus"));
          btnMinus.setText(Message.getString("managesettingsfiles.button.minus"));
          btnCopy.setText(Message.getString("managesettingsfiles.button.copy"));
          btnPlus.addActionListener(this);
          btnMinus.addActionListener(this);
          btnCopy.addActionListener(this);
          panelListSouth.add(btnPlus);
          panelListSouth.add(btnMinus);
          panelListSouth.add(btnCopy);
          panelList.add(panelListNorth, BorderLayout.CENTER);
          panelList.add(panelListSouth, BorderLayout.SOUTH);
        }

        // Right (Settings) panel
        {
          JPanel panelSettings = new JPanel();
          panelSettings.setPreferredSize(new Dimension(500, 250));
          panelSettings.setBorder(new EmptyBorder(7, 0, 0, 7));
          BorderLayout panelSettingsLayout = new BorderLayout();
          panelContent.add(panelSettings, BorderLayout.CENTER);
          panelSettings.setLayout(panelSettingsLayout);

          {
            JLabel lblSettings = new JLabel();
            lblSettings.setText(Message.getString("managesettingsfiles.rightpane.title"));
            panelSettings.add(lblSettings, BorderLayout.NORTH);
          }
          JPanel panelProperty = new JPanel();
          panelSettings.add(panelProperty, BorderLayout.CENTER);
          panelProperty.setLayout(new BorderLayout());

          // Displays settings from "settings" variable
          modelProperties =
              new DefaultTableModel() {
                private static final long serialVersionUID = -6996565435968749645L;

                public int getColumnCount() {
                  return COLUMN_HEADERS.length;
                }

                public int getRowCount() {
                  if (settings == null) return 0;
                  return settings.size();
                }

                public Object getValueAt(int row, int column) {
                  if (column > COLUMN_HEADERS.length) {
                    System.err.println(
                        "Table has "
                            + COLUMN_HEADERS.length
                            + " columns. You asked for column number"
                            + column);
                    return null;
                  }
                  if (column == 0) {
                    return getParameterName(row);
                  } else if (column == 1) {
                    return settings.get(getParameterName(row));
                  }
                  return null;
                }

                public boolean isCellEditable(int row, int column) {
                  if (column == 1) {
                    return true;
                  }
                  return false;
                }

                public void setValueAt(Object value, int row, int column) {
                  if (column > COLUMN_HEADERS.length) {
                    System.err.println(
                        "Table has "
                            + COLUMN_HEADERS.length
                            + " columns. You asked for column number"
                            + column);
                    return;
                  }
                  if (column == 1) {
                    settings.put(getParameterName(row), value.toString());
                    edited = true;
                  }
                  fireTableCellUpdated(row, column);
                }
              };
          // end modelProperties

          modelProperties.setColumnIdentifiers(COLUMN_HEADERS);
          final CustomCellRenderer ccr = new CustomCellRenderer();
          JTable tblProperties =
              new JTable(modelProperties) {
                /**
                 * JTabe class with customizable CellRenderer for hiding passwords. Hides cell in
                 * column 2 if value in column 1 contains string "pass".
                 */
                private static final long serialVersionUID = 1L;

                public TableCellRenderer getCellRenderer(int row, int column) {
                  String value = (String) this.getValueAt(row, 0);
                  if (column == 1 && value.indexOf("pass") >= 0) {
                    return ccr;
                  }
                  return super.getCellRenderer(row, column);
                }
              };
          tblProperties.getColumnModel().getColumn(0).setMinWidth(150);
          tblProperties.getColumnModel().getColumn(1).setMinWidth(250);
          tblProperties.setRowMargin(5);
          tblProperties.setRowHeight(20);

          DefaultTableCellRenderer num_cell_renderer = new DefaultTableCellRenderer();
          num_cell_renderer.setHorizontalAlignment(JLabel.LEFT);
          num_cell_renderer.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 5));
          tblProperties.getColumnModel().getColumn(0).setCellRenderer(num_cell_renderer);

          JScrollPane scrollList = new JScrollPane(tblProperties);
          scrollList.setHorizontalScrollBarPolicy(
              ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
          scrollList.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
          scrollList.setColumnHeader(
              new JViewport() {
                private static final long serialVersionUID = -8778306342340592940L;

                @Override
                public Dimension getPreferredSize() {
                  Dimension d = super.getPreferredSize();
                  d.height = 30;
                  return d;
                }
              });
          panelProperty.add(scrollList, BorderLayout.CENTER);
        }
      }
      setTitle(Message.getString("managesettingsfiles.title"));
      setSize(750, 400);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /** (Re)populate list_model with remote settings files names */
  private void refreshListModel() {
    String[] settings_files = RemoteBuildProperties.getRemoteSettings();
    list_model.clear();
    for (String s : settings_files) {
      list_model.addElement(s);
    }
  }

  /**
   * Get key of parameter number n from TreeMap "settings"
   *
   * @param n
   * @return key String or null
   */
  protected String getParameterName(int n) {
    if (n > settings.size()) {
      System.err.println(
          "Too large row number (" + n + "). Have only " + settings.size() + " settings.");
      return null;
    }
    String key = (String) settings.keySet().toArray()[n];
    return key;
  }

  private LinkedHashMap<String, String> getSettingsFromFile(String filename)
      throws FileNotFoundException {
    Map<String, String> settings = RemoteBuildProperties.getSettingsFromFile(filename);
    LinkedHashMap<String, String> tm_settings = new LinkedHashMap<String, String>(settings);
    return tm_settings;
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    Object target = e.getSource();
    if (target == this.btnOk) {
      if (edited) {
        saveSettingsToFile(this.settings, selected_file);
      }
      this.result = Constant.CLOSE_DIALOG;
      // Close the dialog.
      dispose();
    } else if (target == this.btnPlus) {
      addNewRemoteSettingsFile();
    } else if (target == this.btnMinus) {
      deleteSelectedFile(file_list);
    } else if (target == this.btnCopy) {
      copySelectedFile(file_list);
    }
    if (debug) System.out.println("Refreshing list contents in Remote Settings Files");
    refreshListModel();
    if (debug) System.out.println("actionPerformed() of ManageSettingsFilesDialog exited");
  }

  class SharedListSelectionHandler implements ListSelectionListener {

    @Override
    public void valueChanged(ListSelectionEvent e) {
      if (debug_l2) {
        System.out.println("valueChanged() call");
      }
      String file = file_list.getSelectedValue();
      if (file == null) return;
      if (file == selected_file) return;

      if (debug) System.out.println("Selected " + file);
      try {
        if (edited) {
          saveMySettingsToFile(selected_file);
        }
        edited = false;
        selected_file = file;
        settings = getSettingsFromFile(file);
        modelProperties.fireTableDataChanged();
      } catch (FileNotFoundException ex) {
        System.err.println("File " + file + " not found");
      }
    }
  }

  /***
   * Save settings from ManageSettingsFiles class property "settings"
   * to file with given file name.
   * @param fname
   */
  public void saveMySettingsToFile(String fname) {
    saveSettingsToFile(this.settings, fname);
  }

  /**
   * Save changed values to YAML file
   *
   * @param settings
   * @param fname
   */
  public void saveSettingsToFile(LinkedHashMap<String, String> settings, String fname) {
    // Confirmation dialog
    Object[] options = {
      Message.getString("dialog.common.button.ok"), Message.getString("dialog.common.button.cancel")
    };
    int n =
        JOptionPane.showOptionDialog(
            this,
            Message.getString("managesettingsfiles.confirm"),
            Message.getString("managesettingsfiles.confirm.title"),
            JOptionPane.YES_NO_CANCEL_OPTION,
            JOptionPane.QUESTION_MESSAGE,
            null,
            options,
            options[1]);
    if (debug) System.out.println(n); // OK is 0
    if (n == JOptionPane.YES_OPTION) {
      savingSettings2File(settings, fname);
    }
  }

  /***
   * Saving settins to file with name fname
   * @param settings
   * @param fname
   */
  private void savingSettings2File(LinkedHashMap<String, String> settings, String fname) {
    try {
      if (debug)
        System.out.println(
            "Writing to file " + RemoteBuildProperties.locateRemoteSettingsFile(fname));
      DumperOptions options = new DumperOptions();
      options.setIndent(4);
      options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
      Yaml yaml = new Yaml(options);
      FileWriter writer = new FileWriter(RemoteBuildProperties.locateRemoteSettingsFile(fname));
      yaml.dump(settings, writer);
    } catch (IOException e) {
      System.err.println("Error writing to file " + fname);
      e.printStackTrace();
    }
  }

  /***
   * Delete files, selected in file_list
   * @param file_list
   */
  private void deleteSelectedFile(JList<String> file_list) {
    // Confirm dialog
    Object[] options = {
      Message.getString("dialog.common.button.ok"), Message.getString("dialog.common.button.cancel")
    };
    int n =
        JOptionPane.showOptionDialog(
            this,
            Message.getString("managesettingsfiles.deletefile.confirm.text"),
            Message.getString("managesettingsfiles.deletefile.confirm.title"),
            JOptionPane.YES_NO_CANCEL_OPTION,
            JOptionPane.WARNING_MESSAGE,
            null,
            options,
            options[1]);
    if (debug) System.out.println(n); // OK is 0
    if (n == JOptionPane.OK_OPTION) {
      String fname = file_list.getSelectedValue();
      String f_path = RemoteBuildProperties.locateRemoteSettingsFile(fname);
      if (debug) System.out.println("Deleting file " + f_path);
      try {
        File f = new File(f_path);
        f.delete();
      } catch (Exception e) {
        System.err.println("Error deleting file " + f_path);
        e.printStackTrace();
      }
    }
  }

  /***
   * Create new remote settings file with empty parameters values
   */
  private void addNewRemoteSettingsFile() {
    LinkedHashMap<String, String> dockeriaas_settings = new LinkedHashMap<String, String>();
    LinkedHashMap<String, String> sshconnect_settings = new LinkedHashMap<String, String>();
    dockeriaas_settings.put("server_address", "");
    dockeriaas_settings.put("port", "");
    dockeriaas_settings.put("user", "");
    dockeriaas_settings.put("password", "");
    dockeriaas_settings.put("key", "");
    dockeriaas_settings.put("passphrase", "");
    dockeriaas_settings.put("remote_path", "");
    dockeriaas_settings.put("add_path", "");
    dockeriaas_settings.put("description", "");

    sshconnect_settings.put("server_address", "");
    sshconnect_settings.put("port", "");
    sshconnect_settings.put("user", "");
    sshconnect_settings.put("password", "");
    sshconnect_settings.put("key", "");
    sshconnect_settings.put("passphrase", "");
    sshconnect_settings.put("add_path", "");
    sshconnect_settings.put("remote_path", "");
    sshconnect_settings.put("product_pattern", "*.xml");
    sshconnect_settings.put("description", "");

    String fname = remoteSettingsFileName();
    if (debug) System.out.println("fname ............................. " + fname);
    if (fname.equals(JOptionPane.UNINITIALIZED_VALUE)) {
      return;
    }
    if (fname.indexOf(RemoteBuildProperties.REMOTE_SERVICE_DOCKERIAAS) >= 0) {
      savingSettings2File(dockeriaas_settings, fname);
    } else if (fname.indexOf(RemoteBuildProperties.REMOTE_SERVICE_SSHCONNECT) >= 0) {
      savingSettings2File(sshconnect_settings, fname);
    } else {
      displayErrorDialog("Cannot get remote build service name from file : " + fname);
    }
  }

  /***
   * Copy remote settings from selected file to a new file
   * @param file_list
   */
  private void copySelectedFile(JList<String> file_list) {
    String dst_fname = ProjectProperties.locateRemoteSettingsFile(remoteSettingsFileName());
    String src_fname = ProjectProperties.locateRemoteSettingsFile(file_list.getSelectedValue());
    if (debug) System.out.println("Copy from to file " + src_fname + " to " + dst_fname);
    try {
      Files.copy(new File(src_fname).toPath(), new File(dst_fname).toPath());
    } catch (Exception e) {
      System.err.println("Error copyng file from " + src_fname + " to " + dst_fname);
      e.printStackTrace();
    }
  }

  /***
   * Display dialog for setting name for a new remote settings file
   * @return
   */
  private String remoteSettingsFileName() {
    getRemoteSeriveFileNameDialog dialog = new getRemoteSeriveFileNameDialog();
    String r = dialog.showDialog();
    System.out.println("Result " + r);
    return r;
  }

  class getRemoteSeriveFileNameDialog extends javax.swing.JDialog
      implements PropertyChangeListener {

    private static final long serialVersionUID = 1L;

    String fname;

    private JComboBox<String> serviceField;
    private JTextField textField;
    private JOptionPane optionPane;

    public getRemoteSeriveFileNameDialog() {
      boolean debug = ManageSettingsFilesDialog.debug;
      serviceField = new JComboBox<String>();
      String[] remoteService = {
        RemoteBuildProperties.REMOTE_SERVICE_DOCKERIAAS,
        RemoteBuildProperties.REMOTE_SERVICE_SSHCONNECT
      };
      for (String s : remoteService) {
        serviceField.addItem(s);
      }
      textField = new JTextField(15);

      Object[] GUI_elements = {
        Message.getString("managesettingsfiles.filename.text"), serviceField, textField
      };
      optionPane =
          new JOptionPane(
              GUI_elements, JOptionPane.QUESTION_MESSAGE, JOptionPane.OK_CANCEL_OPTION, null);

      // Make this dialog display it
      setContentPane(optionPane);
      optionPane.addPropertyChangeListener(this);
    }

    public String showDialog() {
      // Center on screen
      this.setLocationRelativeTo(null);
      this.setModal(true);
      this.toFront();
      this.pack();
      this.setVisible(true);
      return optionPane.getValue().toString();
    }

    private String getResult() {
      String r = (String) serviceField.getSelectedItem();
      r = r + RemoteBuildProperties.SETTINGS_PATH_SEPARATOR;
      r = r + textField.getText();
      return r;
    }

    @Override
    public void propertyChange(PropertyChangeEvent e) {
      String prop = e.getPropertyName();
      if (debug) System.out.println("target prop ....................... " + prop);
      if (debug)
        System.out.println("VALUE_PROPERTY .................... " + JOptionPane.VALUE_PROPERTY);
      if (debug)
        System.out.println(
            "INPUT_VALUE_PROPERTY .............. " + JOptionPane.INPUT_VALUE_PROPERTY);
      if (isVisible()
          && e.getSource() == optionPane
          && (JOptionPane.VALUE_PROPERTY.equals(prop)
              || JOptionPane.INPUT_VALUE_PROPERTY.equals(prop))) {
        Object value = optionPane.getValue();
        if (debug) System.out.println("Value ............................. " + value);
        if (value instanceof Integer) {
          if ((Integer) value == JOptionPane.OK_OPTION) {
            optionPane.setValue(getResult());
            if (debug)
              System.out.println("new value ......................... " + optionPane.getValue());
          } else if ((Integer) value == JOptionPane.CANCEL_OPTION) {
            if (debug) System.out.println("Cancelled");
            optionPane.setValue(JOptionPane.UNINITIALIZED_VALUE);
          }
        }
        setVisible(false);
      }
    }
  }

  /***
   * Display error message
   * @param message - Error message
   */
  private void displayErrorDialog(String message) {
    JOptionPane.showMessageDialog(
        this, message, Message.getString("dialog.common.error"), JOptionPane.ERROR_MESSAGE);
  }
}
