package jp.riken.kscope.dialog;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.ListSelectionModel;
import javax.swing.RowFilter;
import javax.swing.RowSorter;
import javax.swing.ScrollPaneConstants;
import javax.swing.SortOrder;
import javax.swing.RowSorter.SortKey;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.dialog.RemoteBuildPropertiesDialog.CustomCellRenderer;
import jp.riken.kscope.properties.ProjectProperties;
import jp.riken.kscope.properties.RemoteBuildProperties;

public class ManageSettingsFilesDialog extends javax.swing.JDialog implements ActionListener {
	/** シリアル番号 */
	private static final long serialVersionUID = 1L;
	private static boolean debug = true; 
	private JButton btnOk;
	private int result = Constant.CANCEL_DIALOG;
	private FileProjectNewDialog FPNdialog;
	private DefaultTableModel modelProperties;
	private final String[] COLUMN_HEADERS = {
			Message.getString("managesettingsfiles.table.key"),
			Message.getString("managesettingsfiles.table.value")			
	};
	private String selected_file;
	TreeMap<String,String> settings;
	private JList<String> file_list;
	private boolean edited = false;  // Values in table has been changed
	
	public ManageSettingsFilesDialog(FileProjectNewDialog FPNdialog) {
		this.FPNdialog = FPNdialog;
		initGUI();
	}
	
	public int showDialog() {
        // Center on screen
        this.setLocationRelativeTo(null);
        this.setModal(true);
        //this.toFront();
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
                    btnOk.setText(Message.getString("dialog.common.button.ok"));  //OK
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
                //Border border = new EmptyBorder(7,7,0,7);
                panelContent.setLayout(panelContentLayout);

                // Left panel
                {
                    JPanel panelList = new JPanel();
                    panelList.setPreferredSize(new Dimension(220, 250));
                    panelList.setBorder(new EmptyBorder(7,7,0,7));
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
                    file_list = new JList<String>(FPNdialog.getRemoteSettings());
                    file_list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                    file_list.setLayoutOrientation(JList.VERTICAL);
                    file_list.addListSelectionListener(new SharedListSelectionHandler()  {
                    	
                    });
                    JScrollPane listScroller = new JScrollPane(file_list);
                    //listScroller.setPreferredSize(new Dimension(200, 100));
                    panelListNorth.add(listScroller,BorderLayout.CENTER);
                    panelList.add(panelListNorth,BorderLayout.CENTER);
                }
                
                // Right (Settings) panel
                {
                	JPanel panelSettings = new JPanel();
                	panelSettings.setPreferredSize(new Dimension(500, 250));
                	panelSettings.setBorder(new EmptyBorder(7,0,0,7));
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
                	modelProperties = new DefaultTableModel() {
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
                                System.err.println("Table has " + COLUMN_HEADERS.length + " columns. You asked for column number" + column);
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
                                System.err.println("Table has " + COLUMN_HEADERS.length + " columns. You asked for column number" + column);
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
                    JTable tblProperties = new JTable(modelProperties) {
                    	/**
    					 * JTabe class with customizable CellRenderer for hiding passwords.
    					 * Hides cell in column 2 if value in column 1 contains string "pass".
    					 */
    					private static final long serialVersionUID = 1L;
    					
    					public TableCellRenderer getCellRenderer(int row, int column) {
    						String value = (String) this.getValueAt(row, 0);
    						if (column == 1 && value.indexOf("pass") >=0 ) { 
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
                    scrollList.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                    scrollList.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
                    scrollList.setColumnHeader(new JViewport() {
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
	
	/**
	 * Get key of parameter number n from TreeMap "settings"
	 * @param n
	 * @return key String or null
	 */
	protected String getParameterName(int n) {
		if (n > settings.size()) {
			System.err.println("Too large row number ("+n+"). Have only "+settings.size()+" settings.");
			return null;
		}
		String key = (String) settings.keySet().toArray()[n];
		return key;
	}
	
	
	private TreeMap<String, String> getSettingsFromFile(String filename) throws FileNotFoundException {
		Map <String,String> settings = RemoteBuildProperties.getSettingsFromFile(filename);
		TreeMap<String, String> tm_settings = new TreeMap<String,String>(settings);
		return tm_settings;
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == this.btnOk) {
			if (edited) {
				saveSettingsToFile(selected_file);
			}
			this.result = Constant.CLOSE_DIALOG;
             // ダイアログを閉じる。
            dispose();            
        }
		if (debug) System.out.println("actionPerformed() of ManageSettingsFilesDialog exited");
	}	
	
	class SharedListSelectionHandler implements ListSelectionListener {

		@Override
		public void valueChanged(ListSelectionEvent e) {
			String file = file_list.getSelectedValue();
			if (file == selected_file) return;
			selected_file = file;
			if (debug) System.out.println("Selected "+file);
			try {
				if (edited) {
					saveSettingsToFile(selected_file);
				}
				edited = false;
				settings = getSettingsFromFile(file);
				modelProperties.fireTableDataChanged();
			}
			catch (FileNotFoundException ex) {
				System.err.println("File "+file+" not found");
			}
		}
	}

	public void saveSettingsToFile(String file) {
		//Custom button text
		Object[] options = {Message.getString("dialog.common.button.ok"),
				Message.getString("dialog.common.button.cancel")};
		int n = JOptionPane.showOptionDialog(this,Message.getString("managesettingsfiles.confirm"),
				Message.getString("managesettingsfiles.confirm.title"),
				JOptionPane.YES_NO_CANCEL_OPTION,
				JOptionPane.QUESTION_MESSAGE,
				null,
				options,
				options[1]);
		// TODO Auto-generated method stub
		System.out.println("Save to file "+file+" "+n);
	}
}
