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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.Queue;
import java.util.concurrent.ArrayBlockingQueue;

import javax.swing.BorderFactory;
import javax.swing.BoundedRangeModel;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.JViewport;
//import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;
import javax.swing.border.CompoundBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.plaf.TextUI;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Document;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * コンソール画面クラス
 * @author RIKEN
 */
public class ConsolePanel extends AnalisysPanelBase implements FocusListener, IAnalisysComponent, ComponentListener{

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** タブサイズ */
    private final int TAB_SIZE = 4;
    /** コンソールテキストボックス */
    private JTextPane consoleTextPane;
    /** クリアボタン */
    private JButton btnClear;
    /** ラベル */
    private JLabel label;
    /** コンソール出力キュー */
    private Queue<PrintQueue>  listout;

    /** 標準出力ストリーム:システムデフォルト */
    private PrintStream sysOut = System.out;
    /** 標準エラー出力ストリーム:システムデフォルト */
    private PrintStream sysErr = System.err;
    
    public boolean disable_horizontal_scroll = false;

    /**
     * コンソール出力キュー
     * @author RIKEN
     */
    private class PrintQueue {
        public String text = null;
        public SimpleAttributeSet attr = null;
        public PrintQueue(String text, SimpleAttributeSet attr) {
            this.text = text;
            this.attr = attr;
        }
    }

    private WorkerThread worker;
    /**
     * コンストラクタ
     */
    public ConsolePanel() {
        super();
        initGUI();
        listout = new ArrayBlockingQueue<PrintQueue>(KscopeProperties.CONSOLE_QUEUESIZE);

    }

    /**
     * コンストラクタ
     * @param console		分析情報パネル識別子
     */
    public ConsolePanel(ANALYSIS_PANEL console) {
        super(console);
        initGUI();
        listout = new ArrayBlockingQueue<PrintQueue>(KscopeProperties.CONSOLE_QUEUESIZE);
    }

    /**
     * GUIの初期化を行う
     */
    @SuppressWarnings("serial")
    private void initGUI() {
        try {
            BorderLayout thisLayout = new BorderLayout();
            this.setLayout(thisLayout);
//            setPreferredSize(new Dimension(400, 64));

            // 上部の情報ラベル、ボタンの配置パネル
            {
                JPanel panelTop = new JPanel();
                panelTop.setLayout(new BorderLayout());
                this.add(panelTop, BorderLayout.NORTH);
                panelTop.setBorder(new CompoundBorder(
                                            new LineBorder(Color.BLACK, 1),
                                            BorderFactory.createEmptyBorder(0, 5, 0, 20)));
                // ボタン配置パネル
                {
                    JPanel panelButtons = new JPanel();
                    panelButtons.setLayout(new BoxLayout(panelButtons, BoxLayout.LINE_AXIS));
                    panelTop.add(panelButtons, BorderLayout.EAST);

                    java.awt.Dimension buttonSize = new java.awt.Dimension(24, 24);
                    // クリアボタン
                    {
                        Icon icon = ResourceUtils.getIcon("removeall.gif");
                        btnClear = new JButton(icon);
                        panelButtons.add(btnClear);
                        btnClear.setContentAreaFilled(false);
                        btnClear.setBorderPainted(false);
                        btnClear.setPreferredSize(buttonSize);
                        btnClear.setMinimumSize(buttonSize);
                        btnClear.setMaximumSize(buttonSize);
                        btnClear.addActionListener( new ActionListener() {
                            @Override
                            public void actionPerformed(ActionEvent e) {
                                // コンソールをクリアする。
                                clearConsole();
                            }
                        });
                    }
                }

                // ラベル配置
                {
                    label = new JLabel();
                    panelTop.add(label, BorderLayout.CENTER);
                    //label.setText().getString("");
                }
            }
            // コンソールテキストパイン
            {
                // 読み取り専用とする。
                consoleTextPane = new JTextPane() {
                    @Override
                    public boolean getScrollableTracksViewportWidth() {
                        // 折り返しを行わない
                        try {
                            Object parent = getParent();
                            if (parent instanceof JViewport) {
                                JViewport port = (JViewport) parent;
                                int w = port.getWidth();	// 表示できる範囲(上限)
                                TextUI ui = getUI();
                                if (ui == null) return true;
                                Dimension sz = ui.getPreferredSize(this); // 実際の文字列サイズ
                                if (sz == null) return true;
                                if (sz.width < w) {
                                    return true;
                                }
                            }
                        }
                        catch (Exception ex) {
                            ex.printStackTrace();
                        }
                        return false;
                    }
                };
                consoleTextPane.setEditable(false);
                consoleTextPane.getCaret().setVisible(true);   // キャレットを表示する
                
                // タブサイズを設定する。
                SwingUtils.setTabSize(consoleTextPane, TAB_SIZE);

                final JScrollPane scroll = new JScrollPane();
                scroll.setViewportView(consoleTextPane);
                this.add(scroll, BorderLayout.CENTER);
                
                //disable horizontal scroll
                scroll.getHorizontalScrollBar().addAdjustmentListener(new AdjustmentListener() {
                	BoundedRangeModel brm = scroll.getHorizontalScrollBar().getModel();
                	JScrollBar sb =	scroll.getHorizontalScrollBar();
                	
                	@Override
					public void adjustmentValueChanged(AdjustmentEvent arg0) {
                		if (!brm.getValueIsAdjusting() && disable_horizontal_scroll) sb.setValue(sb.getMinimum());						
					}                		
                });
            }

            //OutputStream os = new JTextAreaOutputStream(consoleTextPane, "UTF-8");
            //System.setOut(new PrintStream(os, true));

            // フォーカスイベントの登録
            this.consoleTextPane.addFocusListener(this);

            this.addComponentListener(this);

            // ツールチップ設定
            btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); //クリア

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    class TextpaneListener implements CaretListener {

    	JScrollPane scroll;

    	public TextpaneListener(JScrollPane scroll) {
    		this.scroll = scroll;    		
    	}

    	@Override
    	public void caretUpdate(CaretEvent e) {
    		SwingUtilities.invokeLater(
    				new Runnable()
    				{
    					public void run()
    					{
    						JScrollBar sb =	scroll.getHorizontalScrollBar();
    						sb.setValue(sb.getMinimum());    						
    					}
    				});
    	}
    }

    

    /**
     * コンソールをクリアする
     */
    public void clearConsole() {
        StyleContext sc = new StyleContext();
        DefaultStyledDocument doc = new DefaultStyledDocument(sc);
        this.consoleTextPane.setDocument(doc);
        consoleTextPane.setCaretPosition(0);
        consoleTextPane.getCaret().setVisible(true);   // キャレットを表示する

        // キューのクリア
        listout.clear();
    }

    private void addQueue(PrintQueue queue) {
        if (!listout.offer(queue)) {
            // 先頭削除
            listout.poll();
            listout.offer(queue);
        }
    }


    /**
     * コンソールに出力する
     * @param text		出力文字列
     * @param error		true=エラー出力
     */
    private void updateTextPane(final String text, final boolean error) {
        Color fontcolor = error ? Color.RED : Color.BLACK;
        updateTextPane(text, fontcolor);
    }

    /**
     * コンソールに出力する
     * @param text		出力文字列
     * @param error		true=エラー出力
     */
    private void updateTextPane(final String text, final Color fontcolor) {

        SimpleAttributeSet attr = new SimpleAttributeSet();
        StyleConstants.setForeground(attr, fontcolor);

        // キューに追加する
        addQueue(new PrintQueue(text, attr));

        // スレッドを起動する
        if (worker == null) {
            try {
				worker = new WorkerThread();
				worker.execute();
			} catch (Exception ex) {
			}
        }
    }

    /**
     * コンソールに出力する
     * @param text		出力文字列
     */
    public void outputString(final String text) {
        Color fontcolor = Color.BLACK;
        updateTextPane(text, fontcolor);
    }

    /**
     * キューの情報をテキストボックスに表示を行うスレッドクラス
     * @author RIKEN
     */
    private class WorkerThread  extends SwingWorker<Object, Object> {

        /**
         * キューの情報をテキストボックスに表示を行う
         * @return      	計算結果
         * @throws Exception		キュー情報の表示エラー
         */
        @Override
        protected Object doInBackground() throws Exception {

        	SwingUtilities.invokeLater(new Runnable() {
        		@Override
        		public void run() {

        			// StyleContext sc = new StyleContext();
        			// DefaultStyledDocument doc = new DefaultStyledDocument(sc);
        			// consoleTextPane.setDocument(doc);

        			Document doc = consoleTextPane.getDocument();
        			// タブサイズを設定する。
        			SwingUtils.setTabSize(consoleTextPane, TAB_SIZE);

        			PrintQueue queue = null;
        			while ((queue = listout.poll()) != null) {
        			//for (PrintQueue queue : listout) {
        				try {
        					doc.insertString(doc.getLength(), queue.text, queue.attr);
        					ConsolePanel.this.validate();
        					ConsolePanel.this.repaint();

        				} catch (BadLocationException e) {
        					throw new RuntimeException(e);
        				}
        			}
        			int len = doc.getLength();
        			if (len > 0) len = len - 1;
        			consoleTextPane.setCaretPosition(len);
        			consoleTextPane.getCaret().setVisible(true);   // キャレットを表示する
        		}
        	});
        	return null;
        }

        @Override
        protected void done()  {
            ConsolePanel.this.worker = null;
        }
    }

    /**
     * System.out/errのフックを取り止める.
     */
    private void terminateSystemStreams() {
        System.setOut(this.sysOut);
        System.setErr(this.sysErr);
    }

    /**
     * System.out/errのフックを行う
     */
    private void redirectSystemStreams() {
        OutputStream stdout = new OutputStream() {
            @Override
            public void write(final int b) throws IOException {
                updateTextPane(String.valueOf((char) b), false);
                this.flush();
            }

            @Override
            public void write(byte[] b, int off, int len) throws IOException {
                updateTextPane(new String(b, off, len), false);
                this.flush();
            }

            @Override
            public void write(byte[] b) throws IOException {
                updateTextPane(new String(b), false);
                this.flush();
            }
        };

        OutputStream errout = new OutputStream() {
            @Override
            public void write(final int b) throws IOException {
                updateTextPane(String.valueOf((char) b), true);
            }

            @Override
            public void write(byte[] b, int off, int len) throws IOException {
                updateTextPane(new String(b, off, len), true);
            }

            @Override
            public void write(byte[] b) throws IOException {
                write(b, 0, b.length);
            }
        };

        System.setOut(new PrintStream(stdout, true));
        System.setErr(new PrintStream(errout, true));

    }

    /**
     * テキストボックス出力クラス（未使用）.
     * @author RIKEN
     *
     */
    public class JTextAreaOutputStream extends OutputStream {
        private ByteArrayOutputStream os;
        /** 出力テキストボックス */
        @SuppressWarnings("unused")
        private JTextPane textBox;
        /** エンコード */
        private String encode;

        /**
         * コンストラクタ
         * @param textArea		テキストボックス
         * @param encode		エンコード
         */
        public JTextAreaOutputStream(JTextPane textArea, String encode) {
            this.textBox = textArea;
            this.encode = encode;
            this.os = new ByteArrayOutputStream();
        }
        /** OutputStream#write(byte[])のオーバーライド */
        @Override
		public void write(int arg) throws IOException {
            this.os.write(arg);
        }
        /**
         * flush()でJTextAreaに書き出す
         */
        @Override
		public void flush() throws IOException {
            // 文字列のエンコード
            final String str = new String(this.os.toByteArray(), this.encode);
            // 実際の書き出し処理
            SwingUtilities.invokeLater(new Runnable(){
                @Override
				public void run() {
                    Color fontcolor = Color.BLACK;
        			updateTextPane(str, fontcolor);

                    /*
                    Document doc = JTextAreaOutputStream.this.textBox.getDocument();
                    SimpleAttributeSet attr = new SimpleAttributeSet();
                    StyleConstants.setForeground(attr, fontcolor);

                    try {
                        doc.insertString(doc.getLength(), str, attr);
                    } catch (BadLocationException e) {
                        throw new RuntimeException(e);
                    }
                    JTextAreaOutputStream.this.textBox.setCaretPosition(doc.getLength() - 1);
                    JTextAreaOutputStream.this.textBox.getCaret().setVisible(true);   // キャレットを表示する
                    */
                }
            });
            // 書き出した内容はクリアする
            this.os.reset();
        }
    }

    /**
     * コンソール出力のOutputStreamを取得する.
     * @return		OutputStream
     */
    public OutputStream getOutputStream() {
        OutputStream os = new JTextAreaOutputStream(consoleTextPane, "UTF-8");
        return os;
    }

    /**
     * フォーカス取得イベント.<br/>
     * フォーカス移動時にキャレットが非表示となるので、フォーカス取得時にキャレット表示を再設定する。
     * @param event		イベント情報
     */
    @Override
    public void focusGained(FocusEvent event) {
        this.consoleTextPane.getCaret().setVisible(true);
    }

    /**
     * フォーカス喪失イベント
     * @param event		イベント情報
     */
    @Override
    public void focusLost(FocusEvent event) { }

    /**
     * 分析情報のエクスポートを行う
     * @param file			出力ファイル
     */
    @Override
    public void export(File file) {
        if (file == null) return;

        String text = this.consoleTextPane.getText();

        try {
            // ファイル出力
            PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));
            pw.println(text);
            pw.close();

        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    /**
     * タブフォーカスリスナを追加する.
     * @param listener		タブフォーカスリスナ
     */
    @Override
    public void addTabFocusListener(TabFocusListener listener) {
        consoleTextPane.addFocusListener(listener);
        btnClear.addFocusListener(listener);
    }

    /**
     * パネルにアクションリスナを設定する.<br/>
     * メニューバーに作成済みのアクションリスナをパネルボタンに割り当てる。
     * @param menu		メニューバー
     */
    @Override
    public void setActionListener(MainMenu menu) {
        // 割り当てボタンなし
    }

    /**
     * モデルのクリアを行う。
     */
    @Override
    public void clearModel() {
        clearConsole();
    }

    /**
     * コンポーネントのリサイズイベント.
     * @param e			イベント情報
     */
    @Override
    public void componentResized(ComponentEvent e) { }

    /**
     * コンポーネントの移動イベント.
     * @param e		イベント情報
     */
    @Override
    public void componentMoved(ComponentEvent e) { }

    /**
     * コンポーネントの表示イベント
     * @param e		イベント情報
     */
    @Override
    public void componentShown(ComponentEvent e) {
        // コンポーネントが非表示となっる時も表示イベントが発生する
        if (isTabVisible()) {
            // System出力をコンソールテキストペインに出力する。
            redirectSystemStreams();
        }
    }

    /**
     * コンポーネントの非表示イベント
     * @param e		イベント情報
     */
    @Override
    public void componentHidden(ComponentEvent e) {
        // コンポーネントが非表示となっても、コンソールタブが閉じていなければ、フックは継続する。
        if (!isTabVisible()) {
            // System.out/errのフックを取り止める.
            terminateSystemStreams();
        }
    }


    /**
     * コンソールタブが表示されているかチェックする. <br/>
     * アクティブではなくとも、タブに含まれているれば、trueとする
     * @return		true=タブが表示、含まれている
     */
    private boolean isTabVisible() {
        int index = ((AnalysisView)this.getParentComponent()).getTabIndex(this.getEnumPanel());
        return (index >= 0);
    }


    /**
     * タブのクローズを行う
     */
    @Override
    public void closeTab() {
        // System.out/errのフックを取り止める.
        terminateSystemStreams();
    }

    /**
     * 選択ソースコード行情報を取得する
     * @return		選択ソースコード行情報
     */
    @Override
    public CodeLine getSelectedCodeLine() {
        return null;
    }

    /**
     * 選択ブロックを取得する
     * @return		選択ブロック
     */
    @Override
    public IBlock getSelectedBlock() {
        return null;
    }

    /**
     * 選択付加情報を取得する
     * @return		選択付加情報
     */
    @Override
    public IInformation getSelectedInformation() {
        return null;
    }

    /**
     * ソースビュープロパティを設定する
     * @param properties		ソースビュープロパティ
     */
    @Override
    public void setSourceProperties(SourceProperties properties) {}

    /**
     * 選択項目をクリップボードにコピーする.
     */
    @Override
    public void copyClipboard() {
        this.consoleTextPane.copy();
    }

    /**
     * エクスポートするデータがあるか否か
     */
	@Override
	public boolean isExportable() {
		String text = this.consoleTextPane.getText();
		return (!text.isEmpty());
	}

	/**
	 * キューの文字列をJTextAreaに書き出す
	 */
	public void flush() {
		try {
			OutputStream out = this.getOutputStream();
			if (out != null) {
				out.flush();
			}
		} catch (Exception ex) { }
	}

}
