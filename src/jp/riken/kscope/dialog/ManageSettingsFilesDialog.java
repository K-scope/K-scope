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
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.table.DefaultTableModel;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
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
	
	public ManageSettingsFilesDialog(FileProjectNewDialog FPNdialog) {
		this.FPNdialog = FPNdialog;
		initGUI();
	}
	
	public int showDialog() {
        // Center on screen
        this.setLocationRelativeTo(null);
        //this.pack();
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
                panelButtons.setPreferredSize(new java.awt.Dimension(390, 46));
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
                TitledBorder title = BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.gray), "panelContent");
                title.setTitleJustification(TitledBorder.CENTER);
                panelContent.setBorder(title);
                panelContent.setLayout(panelContentLayout);

                // Left panel
                {
                    JPanel panelList = new JPanel();
                    panelList.setPreferredSize(new Dimension(220, 250));
                    BorderLayout panelListLayout = new BorderLayout();
                    panelList.setLayout(panelListLayout);
                    title = BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.darkGray), "panelList");
                    title.setTitleJustification(TitledBorder.CENTER);
                    panelList.setBorder(title);
                    panelContent.add(panelList, BorderLayout.WEST);
                    
                    // North panel
                    JPanel panelListNorth = new JPanel();
                    panelListNorth.setLayout(new BorderLayout());
                    title = BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.darkGray), Message.getString("managesettingsfiles.dockeriaas.title"));
                    title.setTitleJustification(TitledBorder.CENTER);
                    panelListNorth.setBorder(title);
                    
                    JList<String> docker_list = new JList<String>(FPNdialog.getRemoteSettings());
                    docker_list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                    docker_list.setLayoutOrientation(JList.VERTICAL);
                    JScrollPane listScroller = new JScrollPane(docker_list);
                    //listScroller.setPreferredSize(new Dimension(200, 100));
                    panelListNorth.add(listScroller,BorderLayout.CENTER);
                    panelList.add(panelListNorth,BorderLayout.NORTH);
                }
                
                // Right (Settings) panel
                {
                	JPanel panelSettings = new JPanel();
                	BorderLayout panelSettingsLayout = new BorderLayout();
                	panelContent.add(panelSettings, BorderLayout.CENTER);
                	//Border borderSettings = new EmptyBorder(0,7,0,0);
                	title = BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.darkGray), "panelSettings");
                    title.setTitleJustification(TitledBorder.CENTER);
                	panelSettings.setBorder(title);
                	panelSettings.setLayout(panelSettingsLayout);

                	{
                		JLabel lblSettings = new JLabel();
                		lblSettings.setText(Message.getString("managesettingsfiles.rightpane.title"));
                		panelSettings.add(lblSettings, BorderLayout.NORTH);
                	}
                	JPanel panelProperty = new JPanel();                	
                	panelSettings.add(panelProperty, BorderLayout.CENTER);
                	panelProperty.setLayout(new BorderLayout());
                	//panelProperty.setPreferredSize(new java.awt.Dimension(320, 234));
                	EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
                	Border borderKeyword = new CompoundBorder( titleBorder, new EmptyBorder(17,7,0,7));
                	panelProperty.setBorder(borderKeyword);
                	
                	// Displays settings from "settings" variable
                	modelProperties = new DefaultTableModel() {
                        private static final long serialVersionUID = -6996565435968749645L;

                        public int getColumnCount() {
                            return COLUMN_HEADERS.length;
                        }

                        public int getRowCount() {
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
                            }
                            fireTableCellUpdated(row, column);
                        }
                    };
                    // end modelProperties
                }
            }
            setTitle(Message.getString("managesettingsfiles.title"));
            setSize(640, 300);
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
		Set<String> key_set = settings.keySet();
		String[] keys = (String[]) key_set.toArray();
		if (n > keys.length) {
			System.err.println("Too large row number ("+n+"). Have only "+keys.length+" settings.");
			return null;
		}
		return keys[n];
	}
	
	
	private TreeMap<String, String> getSettingsFromFile(String filename) throws FileNotFoundException {
		Map <String,String> settings = RemoteBuildProperties.getSettingsFromFile(filename);
		TreeMap<String, String> tm_settings = new TreeMap<String,String>(settings);
		return tm_settings;
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == this.btnOk) {
			this.result = Constant.CLOSE_DIALOG;
             // ダイアログを閉じる。
            dispose();            
        }
		if (debug) System.out.println("actionPerformed() of ManageSettingsFilesDialog exited");
	}		
}
