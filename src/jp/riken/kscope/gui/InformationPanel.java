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
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.FlowLayout;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.net.URL;
import java.util.Observable;
import java.util.Observer;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.border.CompoundBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.html.HTML;

import jp.riken.kscope.Message;
import jp.riken.kscope.action.EditInformationEditAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.model.InformationModel;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 付加情報パネルクラス
 * @author riken
 *
 */
public class InformationPanel extends AnalisysPanelBase implements Observer, IAnalisysComponent, HyperlinkListener, ActionListener, MouseListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** 編集ボタン */
    private JButton btnEdit;
    /** ファイルを開くボタン */
    private JButton btnOpenFile;
    /** エクスポートボタン */
    private JButton btnExport;
    /** ロックボタン */
    private JButton btnLock;
    /** 付加情報ラベル */
    private JLabel label;
    /** コンテンツボックス */
    private Box contentInfo;
    /** 余白ボックス */
    private final Component glue = Box.createVerticalGlue();
    /** スクロールパイン */
    private JScrollPane scrollInfo;

    /** 付加情報モデル */
    private InformationModel model;

    /** 展開ボタンアイコン */
    private Icon expand_icon = ResourceUtils.getIcon("expand_arrow.gif");
    /** 収納ボタンアイコン */
    private Icon collapse_icon = ResourceUtils.getIcon("collapse_arrow.gif");

    /** 表示ロック状態 */
    private boolean viewLock = false;

    /** 選択付加情報ノードパネル */
    private NodePanel selectedInfo;

    /** 付加情報編集アクション */
    private EditInformationEditAction actionEdit;
    /** 付加情報パネル下マージン */
    private final int EDITPANE_BOTTOM_MERGE = 16;
    /** 選択パネル背景色 */
    private Color colorSelectedPanel;

    /**
     * コンストラクタ
     */
    public InformationPanel() {
        super();

        // 初期化を行う。
        initialize();

    }

    /**
     * コンストラクタ
     * @param panel		分析情報パネル識別子
     */
    public InformationPanel(ANALYSIS_PANEL panel) {
        super(panel);

        // 初期化を行う。
        initialize();

    }

    /**
     * 初期化を行う。
     */
    private void initialize() {

        // モデルの生成を行う
        model = new InformationModel();
        // オブザーバを設定する。
        model.addObserver(this);

        // GUI初期化を行う。
        initGUI();

    }


    /**
     * GUI初期化を行う。
     */
    private void initGUI() {
        try {
            BorderLayout thisLayout = new BorderLayout();
            this.setLayout(thisLayout);
            setPreferredSize(new Dimension(400, 0));

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
                    // 編集ボタン
                    {
                        Icon icon = ResourceUtils.getIcon("edit_info.gif");
                        btnEdit = new JButton(icon);
                        panelButtons.add(btnEdit);
                        btnEdit.setContentAreaFilled(false);
                        btnEdit.setBorderPainted(false);
                        btnEdit.setPreferredSize(buttonSize);
                        btnEdit.setMinimumSize(buttonSize);
                        btnEdit.setMaximumSize(buttonSize);
                    }
                    // 余白設定
                    //panelButtons.add(Box.createHorizontalStrut(5));
                    {
                        Icon icon = ResourceUtils.getIcon("openfile.gif");
                        btnOpenFile = new JButton(icon);
                        btnOpenFile.setContentAreaFilled(false);
                        btnOpenFile.setBorderPainted(false);
                        btnOpenFile.setPreferredSize(buttonSize);
                        btnOpenFile.setMinimumSize(buttonSize);
                        btnOpenFile.setMaximumSize(buttonSize);
                        panelButtons.add(btnOpenFile);
                    }
                    // 余白設定
                    //panelButtons.add(Box.createHorizontalStrut(5));
                    {
                        Icon icon = ResourceUtils.getIcon("save.gif");
                        btnExport = new JButton(icon);
                        btnExport.setContentAreaFilled(false);
                        btnExport.setBorderPainted(false);
                        btnExport.setPreferredSize(buttonSize);
                        btnExport.setMinimumSize(buttonSize);
                        btnExport.setMaximumSize(buttonSize);
                        panelButtons.add(btnExport);
                    }
                    // 余白設定
                    //panelButtons.add(Box.createHorizontalStrut(5));
                    {
                        Icon icon = ResourceUtils.getIcon("unlock.gif");
                        btnLock = new JButton(icon);
                        btnLock.setContentAreaFilled(false);
                        btnLock.setBorderPainted(false);
                        btnLock.setPreferredSize(buttonSize);
                        btnLock.setMinimumSize(buttonSize);
                        btnLock.setMaximumSize(buttonSize);
                        panelButtons.add(btnLock);
                        btnLock.addActionListener(this);
                    }
                }
                // ラベル配置
                {
                    label = new JLabel();
                    panelTop.add(label, BorderLayout.CENTER);
                    label.setText("");
                }
            }
            {
                {
                    // 付加情報パネル
                    contentInfo = Box.createVerticalBox();
                    //contentInfo.setBorder(BorderFactory.createLineBorder(Color.RED, 1));
                    contentInfo.setOpaque(false);

                    // スクロールパイン
                    scrollInfo = new JScrollPane(contentInfo);
                    scrollInfo.getVerticalScrollBar().setUnitIncrement(25);
                    scrollInfo.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
                    scrollInfo.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                    scrollInfo.getViewport().setBackground(Color.WHITE);

                    add(scrollInfo);

                }
            }

            // ツールチップ設定
            btnEdit.setToolTipText(Message.getString("mainmenu.edit")); //編集
            btnExport.setToolTipText(Message.getString("mainmenu.file.export")); //エクスポート
            btnOpenFile.setToolTipText(Message.getString("informationpanel.tooltip.openblock")); //情報箇所を開く
            btnLock.setToolTipText(Message.getString("informationpanel.tooltip.lock")); //表示ロック

            // ボタンイベント
            btnEdit.addActionListener(this);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * 付加情報モデルの変更通知イベント
     * @param o			通知元
     * @param arg		通知項目
     */
    @Override
    public void update(Observable o, Object arg) {

        // 表示ロック状態であれば、付加情報モデルの変更を行わない
        if (this.viewLock) {
            // 選択付加情報の編集の反映のみ行う
            if (selectedInfo == null) return;

            // 表示の更新
            selectedInfo.refresh();
            return;
        }

        // 付加情報モデルを更新する
        updateModel();

        return;
    }

    /**
     * 付加情報モデルを更新する
     */
    public void updateModel() {

        // 付加情報のクリア
        clearComponent();

        if (this.model == null) return;

        int count = this.model.getInformationListCount();
        for (int i=0; i<count; i++) {
            String name = this.model.getInformationNode(i).toString();
            String htmlContent = this.model.getInformationHtmlContent(i);
            IInformation info = this.model.getInformationNode(i);

            // 付加情報を追加する
            addInformation(name, htmlContent, info);
        }

        return;
    }

    /**
     * 付加情報を追加する
     * @param name			付加情報名
     * @param htmlText		付加情報HTML本文
     * @param info			付加情報設定ブロック
     */
    public void addInformation(String name, String htmlText, IInformation info) {

        // 追加コンポーネントの作成
        JComponent component = makeRowsPanel(name, htmlText, info);

        // コンテンツパネルにコンポーネントを追加する
        addComponent(component);

    }


    /**
     * コンテンツパネルをクリアする
     */
    public void clearComponent() {
        this.contentInfo.removeAll();

        // 再描画
        this.refresh();

        // 選択付加情報ブロックのクリア
        this.selectedInfo = null;

        // 表示ロック状態の切替
        toggleLockButton(false);
    }

    /**
     * コンテンツパネルにコンポーネントを追加する
     * @param component		追加コンポーネント
     */
    private void addComponent(final JComponent component) {
        // 追加コンポーネントサイズの変更
        component.setMaximumSize(new Dimension(Short.MAX_VALUE, component.getPreferredSize().height));

        // 追加コンポーネントは左詰めで配置する
        component.setAlignmentX(Component.LEFT_ALIGNMENT);

        // コンポーネントの追加
        this.contentInfo.remove(glue);
//        this.contentInfo.add(Box.createVerticalStrut(5));
        this.contentInfo.add(component);
        this.contentInfo.add(glue);

        // 再描画
        this.refresh();

        EventQueue.invokeLater(new Runnable() {
            @Override public void run() {
                component.scrollRectToVisible(component.getBounds());
                scrollInfo.getViewport().setViewPosition(new Point(0, 0));
            }
        });

        return;
    }

    /**
     * 再描画を行う
     */
    private void refresh() {

        // 再描画
        this.contentInfo.revalidate();
        this.validate();
        this.repaint();

    }


    /**
     * 付加情報パネルの追加
     * @param name			付加情報：名前
     * @param htmlinfo		付加情報：HTML形式情報
     * @param info			付加情報設定ブロック
     * @return			付加情報パネル
     */
    private JComponent makeRowsPanel(String name, String htmlinfo, IInformation info) {
        // 付加情報パネル
        NodePanel rows = new NodePanel(info);
        rows.setLayout(new BoxLayout(rows, BoxLayout.Y_AXIS));
        rows.setOpaque(false);
        rows.setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
        rows.addMouseListener(this);

        // 名前パネル
        JPanel panelName = new JPanel();
        panelName.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 0));
        panelName.setOpaque(false);

        // 付加情報展開ボタン：初期表示は展開ボタン
        java.awt.Dimension buttonSize = new java.awt.Dimension(18, 18);
        JButton button = new JButton(expand_icon);
        button.setContentAreaFilled(false);
        button.setBorderPainted(false);
        button.setPreferredSize(buttonSize);
        button.setMinimumSize(buttonSize);
        button.setMaximumSize(buttonSize);

        // 名前ラベル
        panelName.add(button);
        JLabel label = new JLabel(name);
        label.setOpaque(false);
        panelName.add(label);

        // 付加情報本文パネル
        JPanel panelEditor = new JPanel();
        panelEditor.setLayout(new BorderLayout());
        panelEditor.setOpaque(false);

        // 付加情報表示テキストボックス
        JEditorPane editor = new JEditorPane() {
            /** シリアル番号 */
            private static final long serialVersionUID = 1L;

            @Override
            public void updateUI() {
                super.updateUI();
                setEditorHeight();
            }

            @Override
            public void doLayout() {
                super.doLayout();
                setEditorHeight();
            }

            /**
             * 付加情報表示テキストボックスの高さを本文に合わせて拡張する
             */
            private void setEditorHeight() {
                if (this.getDocument() == null) return;
                if (this.getDocument().getLength() <= 0) return;
                if (getUI() == null) return;

                Rectangle r = null;
                try {
                    r = this.modelToView( this.getDocument().getLength() );
                } catch (BadLocationException e1) { }

                if (r != null) {
                    Dimension editorSize = this.getPreferredSize();
                    editorSize.height = r.y + r.height + EDITPANE_BOTTOM_MERGE;
                    this.setPreferredSize(editorSize);
                }

            }
        };
        // 付加情報表示テキストボックスはHTML表示とする。
        Dimension editorSize = new java.awt.Dimension(400, 22);
        editor.setEditable(false);
        editor.setContentType("text/html");
        editor.putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, Boolean.TRUE);
        editor.setOpaque(true);
        editor.setPreferredSize(editorSize);
        editor.setText(htmlinfo);

        // イベントリスナ追加
        editor.addHyperlinkListener(this);
        editor.addMouseListener(this);

        editor.validate();
        editor.updateUI();

        // 付加情報表示テキストボックスのボーダー設定
        LineBorder border = new LineBorder(Color.GRAY, 1, false);
        editor.setBorder(border);

        // 付加情報本文パネルの余白設定
        panelEditor.add(Box.createVerticalStrut(5), BorderLayout.NORTH);
        panelEditor.add(Box.createHorizontalStrut(40), BorderLayout.WEST);
        panelEditor.add(Box.createHorizontalStrut(40), BorderLayout.EAST);
        panelEditor.add(Box.createVerticalStrut(5), BorderLayout.SOUTH);
        panelEditor.add(editor, BorderLayout.CENTER);

        // 付加情報展開ボタンのアクションリスナの設定
        button.addActionListener(new InformationExpandAction(panelEditor));

        // 付加情報パネルに名前パネル、本文パネルの追加
        rows.add(panelName);
        rows.add(panelEditor);

        // テキストペインを設定する。
        rows.setEditorPane(editor);

        return rows;
    }


    /**
     * 付加情報モデルを取得する
     * @return		付加情報モデル
     */
    public InformationModel getModel() {
        return model;
    }


    /**
     * フォーカスリスナを設定する
     * @param listener		フォーカスリスナ
     */
    @Override
    public void addTabFocusListener(TabFocusListener listener) {
        this.addFocusListener(listener);

        // 子コンポーネントにもフォーカスリスナを設定する
        if (this.contentInfo != null) {
            this.contentInfo.addFocusListener(listener);
            this.btnEdit.addFocusListener(listener);
            this.btnExport.addFocusListener(listener);
        }
    }


    /**
     * エクスポートを行う
     */
    @Override
    public void export(File file) {
        if (this.model == null) return;

        model.writeFile(file);
    }

    /**
     * パネルにアクションリスナを設定する.<br/>
     * メニューバーに作成済みのアクションリスナをパネルボタンに割り当てる。
     * @param menu		メニューバー
     */
    @Override
    public void setActionListener(MainMenu menu) {
        // 付加情報編集アクション
        this.actionEdit = menu.getActionEditInformation();

        // 分析情報エクスポートアクション
        this.btnExport.addActionListener(menu.getActionExportAnalysis());

        // 該当箇所を開く
        this.btnOpenFile.addActionListener((ActionListener) menu.getActionOpenAnalysisLine());
    }

    /**
     * モデルのクリアを行う。
     */
    @Override
    public void clearModel() {
        // 画面のクリア
        clearComponent();

        // モデルのクリア
        this.model.clearInformation();
    }

    /**
     * 付加情報展開ボタンアクションリスナ
     * @author riken
     */
    private class InformationExpandAction implements ActionListener {
        /** 付加情報本文パネル */
        private JPanel panelEditor;

        /**
         * コンストラクタ
         * @param panel		表示切替を行う付加情報本文パネル
         */
        public InformationExpandAction(JPanel panel) {
            this.panelEditor = panel;
        }

        /**
         * ボタンのクリックイベント
         * @param event		イベント情報
         */
        @Override
        public void actionPerformed(ActionEvent event) {
            JButton btn = (JButton) event.getSource();
            // 付加情報本文パネルの表示をトグルする
            if (panelEditor.isVisible()) {
                // 付加情報本文パネルを非表示する
                btn.setIcon(collapse_icon);
                panelEditor.setVisible(false);
            }
            else {
                // 付加情報本文パネルを表示する
                btn.setIcon(expand_icon);
                panelEditor.setVisible(true);
            }

            return;
        }
    }

    /**
     * 付加情報ノードパネル
     * @author riken
     */
    private class NodePanel extends JPanel {

        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /** 付加情報ノード */
        private IInformation info;
        /** 付加情報表示テキストボックス */
        private JEditorPane editorPane;

        /**
         * コンストラクタ
         * @param info		付加情報ブロック
         */
        public NodePanel(IInformation info) {
            this.info = info;
        }

        /**
         * 付加情報ノードを取得する
         * @return		付加情報ノード
         */
        public IInformation getInfo() {
            return this.info;
        }

        /**
         * 付加情報表示テキストボックスを設定する
         * @param editor 		付加情報表示テキストボックス
         */
        public void setEditorPane(JEditorPane editor) {
            this.editorPane = editor;
        }

        /**
         * 付加情報の表示を再描画する
         */
        public void refresh() {
            if (info == null) return;
            if (info.getInformation() == null) return;
            if (info.getInformation().getContent() == null) return;

            String content = info.getInformation().getContent();
            content = InformationPanel.this.model.createHtmlContent(content);
            this.editorPane.setText(content);

            // 再描画
            InformationPanel.this.refresh();
        }
    }

    /**
     * マウスクリックイベント
     * @param event		マウスイベント情報
     */
    @Override
    public void mouseClicked(MouseEvent event) {
        // クリックチェック
        if (SwingUtilities.isLeftMouseButton(event)) {
            NodePanel panel = null;
            if (event.getSource() instanceof NodePanel) {
                panel = (NodePanel)event.getSource();
            }
            else if (event.getSource() instanceof JEditorPane) {
                // 親の親パネル
                Container cont = ((JEditorPane)event.getSource()).getParent().getParent();
                if (cont instanceof NodePanel) {
                    panel = (NodePanel)cont;
                }
            }

            // 選択付加情報ブロックを設定する
            this.selectedInfo = panel;

            // 選択パネルの背景色を設定する.
            setSelectedBackgroud(this.selectedInfo);

            // ダブルクリック
            if (event.getClickCount() == 2) {
                // 該当個所を開く
                this.btnOpenFile.doClick();
            }
        }
    }

    /**
     * マウスボタンダウンイベント
     * @param e		マウスイベント情報
     */
    @Override
    public void mousePressed(MouseEvent e) { }

    /**
     * マウスボタンアップイベント
     * @param e		マウスイベント情報
     */
    @Override
    public void mouseReleased(MouseEvent e) {}

    /**
     * マウスオーバーイベント
     * @param e		マウスイベント情報
     */
    @Override
    public void mouseEntered(MouseEvent e) {}

    /**
     * マウスアウトイベント
     * @param e		マウスイベント情報
     */
    @Override
    public void mouseExited(MouseEvent e) {}

    /**
     * ハイパーリンクアクションイベント
     * @param event			イベント情報
     */
    @Override
    public void hyperlinkUpdate(HyperlinkEvent event) {
        // ハイパーリンククリックイベント
        if(event.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
            // リンクファイル
            URL url = event.getURL();

            // 起動プログラムの取得（起動プログラムはclass属性にセットされている）
            javax.swing.text.Element elem = event.getSourceElement();
            String program = SwingUtils.getAttributeValue(elem, HTML.Tag.A, HTML.Attribute.CLASS);
            String comment = SwingUtils.getAttributeValue(elem, HTML.Tag.A, HTML.Attribute.COMMENT);
            String[] args = null;
            if (comment != null && !comment.isEmpty()) {
                args = new String[]{comment};
            }
            // 外部プログラムの実行
            String errMsg = SwingUtils.processOpenProgram(url.toString(), program, args);
            if (errMsg != null && !errMsg.isEmpty()) {
                // エラーメッセージ
                JFrame frame = (JFrame)SwingUtilities.getWindowAncestor(this);
                JOptionPane.showMessageDialog(
                        frame,
                        errMsg,
                        Message.getString("dialog.common.error"),
                        JOptionPane.ERROR_MESSAGE);
            }
        }

        return;
    }

    /**
     * タブのクローズを行う
     */
    @Override
    public void closeTab() { }

    /**
     * 選択ソースコード行情報を取得する
     * @return		選択ソースコード行情報
     */
    @Override
    public CodeLine getSelectedCodeLine() {
        IBlock block = getSelectedBlock();
        if (block == null) return null;
        CodeLine line = block.getStartCodeLine();
        return line;
    }


    /**
     * ボタンクリックイベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        if (event.getSource() == this.btnLock) {
            // 表示ロック状態の切替
            toggleLockButton(!this.viewLock);

            // ロック状態では無いので、最新の付加情報を表示する
            if (!this.viewLock) {
                updateModel();
            }
        }
        else if (event.getSource() == this.btnEdit) {
            if (this.actionEdit == null) return;
            if (this.selectedInfo == null) return;
            if (this.selectedInfo.getInfo() == null) return;

            // 編集を行う
            this.actionEdit.editInformation(this.selectedInfo.getInfo());
        }
    }

    /**
     * ロックボタンのトグルを入れ替える
     * @param lock		true = ロック状態
     */
    private void toggleLockButton(boolean lock) {

        // 表示ロック状態の切替
        this.viewLock = lock;

        Icon icon = null;
        if (this.viewLock) {
            // 表示ロック状態にする
            icon = ResourceUtils.getIcon("lock.gif");
        }
        else {
            // 表示ロック状態解除にする
            icon = ResourceUtils.getIcon("unlock.gif");

        }
        this.btnLock.setIcon(icon);
    }


    /**
     * 選択付加情報を取得する
     * @return		選択付加情報
     */
    @Override
    public IInformation getSelectedInformation() {
        if (this.selectedInfo == null) return null;
        return selectedInfo.getInfo();
    }

    /**
     * 選択ブロックを取得する
     * @return		選択ブロック
     */
    @Override
    public IBlock getSelectedBlock() {
        if (this.selectedInfo == null) return null;
        IInformation info = this.selectedInfo.getInfo();
        if (info instanceof IBlock) {
            IBlock block = (IBlock) info;
            return block;
        }
        return null;
    }

    /**
     * ソースビュープロパティを設定する
     * @param properties		ソースビュープロパティ
     */
    @Override
    public void setSourceProperties(SourceProperties properties) {
    	// 選択パネルの背景色
    	this.colorSelectedPanel = properties.getBackgoundView2Color();
        // 選択パネルの背景色を設定する.
        setSelectedBackgroud(this.selectedInfo);
    }

    /**
     * 選択項目をクリップボードにコピーする.
     */
    @Override
    public void copyClipboard() {
        if (this.selectedInfo == null) return;
        if (this.selectedInfo.getInfo() == null) return;
        if (this.selectedInfo.getInfo().getInformation() == null) return;
        IInformation info = this.selectedInfo.getInfo();
        String text = info.getInformation().getContent();

        // クリップボードにコピーする
        SwingUtils.copyClipboard(text);
    }

    /**
     * エクスポート可能か否か
     */
	@Override
	public boolean isExportable() {
		if (this.model == null) return false;
		return (!this.model.isEmpty());
	}

	/**
	 * 選択パネルの背景色を設定する.
	 */
	private void setSelectedBackgroud(JPanel panel) {
        // すべてクリア
		if (this.contentInfo != null) {
			SwingUtils.setBackgroundChildPanel(this.contentInfo, null);
		}
        // 選択パネルの背景色を選択色に変更する
        if (panel != null) {
            SwingUtils.setBackgroundChildPanel(panel, this.colorSelectedPanel);
        }
	}
}


