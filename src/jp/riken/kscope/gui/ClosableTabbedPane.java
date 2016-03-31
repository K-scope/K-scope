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
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JViewport;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

//import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.FRAME_VIEW;

/**
 * 閉じるボタンつきタブペイン
 * @author RIKEN
 *
 */
public abstract class ClosableTabbedPane extends JTabbedPane  implements ITabComponent, ChangeListener, ActionListener {
    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** 閉じるボタンアイコン */
    private static final Icon icon = new CloseTabIcon();

    /** 親コンポーネント */
    private ITabComponent parentCompornent = null;

    /** ビューの識別子 */
    private FRAME_VIEW viewType;

    /** フォーカスリスナ.<br/>
     * 後で追加されたタブ用に待避しておく.
     */
    TabFocusListener focusListener = null;


    /**
     * コンストラクタ
     * @param   type		ビューの識別子
     */
    public ClosableTabbedPane(FRAME_VIEW type) {
        super();

        // タブレイアウトをスクロール表示とする。
        this.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);

        // タブの変更イベント登録
        this.addChangeListener(this);

        // ビューの識別子
        this.viewType = type;
    }

    /**
     * タブを閉じる
     * @param index		タブインデックス
     */
    protected abstract void closeTab(int index);

    /**
     * アクティブタブを切り替える。
     * @param index		アクティブタブインデックス
     */
    @Override
    public void setSelectedIndex(int index) {
        super.setSelectedIndex(index);

        try {
            // アクティブタブのみ閉じるボタンを表示する。
            changeSelectedTab(index);

            JViewport vp = null;
            for(Component c : this.getComponents()) {
                if("TabbedPane.scrollableViewport".equals(c.getName())) {
                    vp = (JViewport)c;
                    break;
                }
            }
            if(vp==null) return;

            final JViewport viewport = vp;
            Dimension dimen = this.getSize();
            Rectangle rect = this.getBoundsAt(index);
            int gw = (dimen.width-rect.width)/2;
            if (gw > 0) {
                rect.grow(gw, 0);
                viewport.scrollRectToVisible(rect);
            }
        } catch (Exception ex) {
        }
    }

    /**
     * タブにコンポーネントを追加する。
     * @param title		タブ表示文字列
     * @param content	追加コンポーネント
     */
    @Override
    public void addTab(String title, final Component content) {
        // タブラベル部分の作成 (タブ表示文字列+閉じるボタン)
        JPanel tab = createTabComponent(title);

        // タブにコンポーネントを追加する。
        super.addTab(title, content);

        // タブラベルの設定
        setTabComponentAt(getTabCount()-1, tab);

        // フォーカスリスナの設定
        if (content instanceof ITabComponent) {
            // 親コンポーネントを設定する。
            ((ITabComponent)content).setParentComponent(this);

            if (this.focusListener != null) {
                ((ITabComponent)content).addTabFocusListener(this.focusListener);
            }
        }

        return ;
    }

    /**
     *
     * タブにコンポーネントを追加する。
     * @param title		タブ表示文字列
     * @param content	追加コンポーネント
     * @param index   追加インデックス
     */
    public void insertTab(String title, TraceResultPanel content, int index) {
        // タブラベル部分の作成 (タブ表示文字列+閉じるボタン)
        JPanel tab = createTabComponent(title);

        super.insertTab(title, null, content, null, index);

        // タブラベルの設定
        setTabComponentAt(index, tab);

        // フォーカスリスナの設定
        if (content instanceof ITabComponent) {
            // 親コンポーネントを設定する。
            ((ITabComponent)content).setParentComponent(this);

            if (this.focusListener != null) {
                ((ITabComponent)content).addTabFocusListener(this.focusListener);
            }
        }
    }

    /**
     * タブラベル部分の作成 (タブ表示文字列+閉じるボタン)
     * @param title			タイトル
     * @return		タブラベル部分
     */
    private JPanel createTabComponent(String title) {
        // タブラベル部分の作成 (タブ表示文字列+閉じるボタン)
        JPanel tab = new JPanel(new BorderLayout());
        tab.setOpaque(false);

        // タブ表示文字列ラベル
        JLabel label = new JLabel(title);
        label.setBorder(BorderFactory.createEmptyBorder(0,0,0,4));

        // 閉じるボタン
        JButton button = new JButton(icon);
        button.setBorder(BorderFactory.createEmptyBorder());
        button.setContentAreaFilled(false);
        // 閉じるボタンクリック時のアクション
        button.addActionListener(this);
        tab.add(label,  BorderLayout.WEST);
        tab.add(button, BorderLayout.EAST);
        tab.setBorder(BorderFactory.createEmptyBorder(2,1,1,1));

        return tab;
    }


    /**
     * タブラベルを設定する.
     * @param index		タブインデックス
     * @param title		タブ表示ラベル
     */
    public void setTabTitle(int index, String title) {
        Component comp = this.getTabComponentAt(index);
        if (!(comp instanceof JPanel)) return;
        JPanel tab = (JPanel)comp;
        int count = tab.getComponentCount();
        for (int i=0; i<count; i++) {
            Component tabComp = tab.getComponent(i);
            if (tabComp instanceof JLabel) {
                ((JLabel)tabComp).setText(title);
            }
        }
        return;
    }

    /**
     * アクティブタブの変更イベント
     * @param event      イベント発生ソース
     */
    @Override
    public void stateChanged(ChangeEvent event) {
        JTabbedPane tabbedpane = (JTabbedPane)event.getSource();
        int index = tabbedpane.getSelectedIndex();
//        System.out.println("selected tab => "+tabbedpane.getSelectedIndex());

        // アクティブタブのみ閉じるボタンを表示する。
        changeSelectedTab(index);

    }

    /**
     * タブを閉じるイベント
     * @param event      イベント発生ソース
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        Object obj = event.getSource();
        if (!(obj instanceof JButton)) return;
        JButton button = (JButton)obj;
        Component tabComp = button.getParent();
        int index = this.indexOfTabComponent(tabComp);
        if (index >= 0) {
            closeTab(index);
        }
    }

    /**
     * アクティブタブのみ閉じるボタンを表示する.<br/>
     * 非アクティブタブの閉じるボタンは非表示とする。
     * @param activeIndex		アクティブタブインデックス
     */
    private void changeSelectedTab(int activeIndex) {
        if (activeIndex < 0) return;

        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            JPanel tab = (JPanel) this.getTabComponentAt(i);
            if (tab == null) return;
            Component[] components = tab.getComponents();
            if (components == null) return;
            for (Component comp : components) {
                if (comp instanceof JButton) {
                    JButton button = (JButton)comp;
                    if (i==activeIndex) {
                        button.setVisible(true);
                    }
                    else {
                        button.setVisible(false);
                    }
                }
            }
        }
    }


    /**
     * 閉じるボタンアイコン
     * @author RIKEN
     *
     */
    private static class CloseTabIcon implements Icon {
        private int width;			///< アイコン幅
        private int height;			///< アイコン高さ

        /**
         * コンストラクタ
         */
        public CloseTabIcon() {
            // アイコンサイズは16x16に設定
            width  = 16;
            height = 16;
        }

        /**
         * 閉じるアイコンを描画する.<br/>
         * ×マークを描画する。
         */
        @Override
        public void paintIcon(Component c, Graphics g, int x, int y) {
            g.translate(x, y);

            //TYPE1
            /*
            g.setColor(Color.BLACK);
            g.drawLine(4,  4, 11, 11);
            g.drawLine(4,  5, 10, 11);
            g.drawLine(5,  4, 11, 10);
            g.drawLine(11, 4,  4, 11);
            g.drawLine(11, 5,  5, 11);
            g.drawLine(10, 4,  4, 10);
            */

            //TYPE2
            g.setColor(new Color(0x99, 0x99, 0x99));
            for(int i=2; i < 3; i++){
              g.drawLine(3+i, 3, 12, 12-i);
              g.drawLine(3, 3+i, 12-i, 12);
              g.drawLine(3+i, 12, 12, 3+i);
              g.drawLine(3, 12-i, 12-i, 3);
            }
            g.setColor(new Color(0x44, 0x44, 0x44));
            for(int i=0; i < 2; i++){
              g.drawLine(4+i, 4, 11, 11-i);
              g.drawLine(4, 4+i, 11-i, 11);
              g.drawLine(4+i, 11, 11, 4+i);
              g.drawLine(4, 11-i, 11-i, 4);
            }

            g.translate(-x, -y);
        }

        /**
         * 閉じるアイコン幅を取得する。
         */
        @Override
        public int getIconWidth() {
            return width;
        }

        /**
         * 閉じるアイコン高さを取得する。
         */
        @Override
        public int getIconHeight() {
            return height;
        }
    }

    /**
     * 親コンポーネントを取得する.
     * @return		親コンポーネント
     */
    @Override
    public ITabComponent getParentComponent() {
        return this.parentCompornent;
    }

    /**
     * 親コンポーネントを設定する.
     * @param component		親コンポーネント
     */
    @Override
    public void setParentComponent(ITabComponent component) {
        this.parentCompornent = component;
    }

    /**
     * フォーカスリスナを設定する
     * @param listener		フォーカスリスナ
     */
    @Override
    public void addTabFocusListener(TabFocusListener listener) {

        focusListener = listener;
        this.addFocusListener(listener);

        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component tab = this.getComponentAt(i);
            if (tab instanceof ITabComponent) {
                ((ITabComponent)tab).addTabFocusListener(listener);
            }
        }
    }

    /**
     * ビューの識別子を取得する
     * @return    ビューの識別子
     */
    public FRAME_VIEW getViewType() {
        return this.viewType;
    }

    /**
     * ビューの識別子を設定する
     * @param type		ビューの識別子
     */
    public void setViewType(FRAME_VIEW type) {
        this.viewType = type;
    }

}


