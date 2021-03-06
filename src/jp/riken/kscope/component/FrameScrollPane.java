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
package jp.riken.kscope.component;

import javax.swing.*;
import java.awt.*;

import jp.riken.kscope.utils.SwingUtils;

/**
 * フレームスクロールパインクラス.
 * 右側、下側にビューを設置可能なスクロールパイン
 * @author RIKEN
 */
public class FrameScrollPane extends JScrollPane {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** 行フッタービューポート */
    protected JViewport rowFooter;
    /** 列フッタービューポート */
    protected JViewport columnFooter;
    /** 左側角コンポーネント */
    protected Component horizontalCornerLeft;
    /** 右側角コンポーネント  */
    protected Component horizontalCornerRight;
    /** 上側角コンポーネント */
    protected Component verticalCornerTop;
    /** 下側角コンポーネント */
    protected Component verticalCornerBottom;
    /** 横スクロールバーの全体表示フラグ */
    private boolean horizontalScrollBarCoversWhole;
    /** 縦スクロールバーの全体表示フラグ */
    private boolean verticalScrollBarCoversWhole;
    /** プロパティ変更通知キー：横スクロールバーの全体表示変更 */
    public static final String PROPERTY_HORIZONTAL_SCROLL_BAR_COVERS_WHOLE = "horizontalScrollBarCoversWhole";
    /** プロパティ変更通知キー：縦スクロールバーの全体表示変更 */
    public static final String PROPERTY_VERTICAL_SCROLL_BAR_COVERS_WHOLE = "verticalScrollBarCoversWhole";
    /** 左上角と右上角を同一高さにするフラグ */
    private boolean columnHeadersUnified;
    /** 左下角と右下角を同一高さにするフラグ */
    private boolean columnFootersUnified;
    /** プロパティ変更通知キー：左上角と右上角を同一高さにするフラグ変更 */
    public static final String PROPERTY_COLUMN_HEADERS_UNIFIED = "columnHeadersUnified";
    /** プロパティ変更通知キー：左下角と右下角を同一高さにするフラグ変更 */
    public static final String PROPERTY_COLUMN_FOOTERS_UNIFIED = "columnFootersUnified";

    /**
     * コンストラクタ
     * @param view      スクロールペイン内のビューポートに配置するコンポーネント
     * @param verticalPolicy    縦スクロールバー表示条件
     * @param horizontalPolicy  横スクロールバー表示条件
     */
    public FrameScrollPane(Component view, int verticalPolicy, int horizontalPolicy) {
        setLayout(new FrameScrollPaneLayout.UIResource());
        setVerticalScrollBarPolicy(verticalPolicy);
        setHorizontalScrollBarPolicy(horizontalPolicy);
        setViewport(createViewport());
        setVerticalScrollBar(createVerticalScrollBar());
        setHorizontalScrollBar(createHorizontalScrollBar());
        if (null != view) {
            setViewportView(view);
        }
        setOpaque(true);
        updateUI();

        if (!getComponentOrientation().isLeftToRight()) {
            viewport.setViewPosition(new Point(Integer.MAX_VALUE, 0));
        }
    }


    /**
     * コンストラクタ
     * @param view      スクロールペイン内のビューポートに配置するコンポーネント
     */
    public FrameScrollPane(Component view) {
        this(view, VERTICAL_SCROLLBAR_AS_NEEDED, HORIZONTAL_SCROLLBAR_AS_NEEDED);
    }


    /**
     * コンストラクタ
     * @param verticalPolicy    縦スクロールバー表示条件
     * @param horizontalPolicy  横スクロールバー表示条件
     */
    public FrameScrollPane(int verticalPolicy, int horizontalPolicy) {
        this(null, verticalPolicy, horizontalPolicy);
    }


    /**
     * コンストラクタ
     */
    public FrameScrollPane() {
        this(null, VERTICAL_SCROLLBAR_AS_NEEDED, HORIZONTAL_SCROLLBAR_AS_NEEDED);
    }

    /**
     * 行フッタービューポートを取得する。
     * @return 行フッタービューポート
     */
    public JViewport getRowFooter() {
        return this.rowFooter;
    }


    /**
     * 行フッタービューポートを設定する
     * @param footer 行フッタービューポート
     */
    public void setRowFooter(JViewport footer) {
        JViewport old = getRowFooter();
        this.rowFooter = footer;
        if (null != footer) {
            add(footer, FrameScrollPaneLayout.ROW_FOOTER);
        }
        else if (null != old) {
            remove(old);
        }
        firePropertyChange("rowFooter", old, footer);
        revalidate();
        repaint();

        // コストバー表示:ONにてSourceCodePanelのViewportが先頭に移動してしまう対策 at 2013/05/15 by @hira
        // SwingUtils.synchronizeView(footer, getViewport(), SwingConstants.VERTICAL);
        SwingUtils.synchronizeView(getViewport(), footer, SwingConstants.VERTICAL);
    }

    /**
     * 行ヘッダービューポートを設定する
     * @param rowHeader 行ヘッダービューポート
     */
    @Override
    public void setRowHeader(JViewport rowHeader) {
        super.setRowHeader(rowHeader);
        SwingUtils.synchronizeView(rowHeader, getViewport(), SwingConstants.VERTICAL);
    }

    /**
     * 行フッターコンポーネントを設定する
     * @param view 行フッターコンポーネント
     */
    public void setRowFooterView(Component view) {
        if (null == getRowFooter()) {
            setRowFooter(createViewport());
        }
        getRowFooter().setView(view);
    }


    /**
     * 列フッタービューポートを取得する
     * @return 列フッタービューポート
     */
    public JViewport getColumnFooter() {
        return this.columnFooter;
    }


    /**
     * 列フッタービューポートを設定する
     * @param footer 列フッタービューポート
     */
    public void setColumnFooter(JViewport footer) {
        JViewport old = getColumnFooter();
        this.columnFooter = footer;
        if (null != footer) {
            add(footer, FrameScrollPaneLayout.COLUMN_FOOTER);
        }
        else if (null != old) {
            remove(old);
        }
        firePropertyChange("columnFooter", old, footer);

        revalidate();
        repaint();
        
        // コストバー表示:ONにてSourceCodePanelのViewportが先頭に移動してしまう対策 at 2013/05/15 by @hira
        // SwingUtils.synchronizeView(footer, getViewport(), SwingConstants.HORIZONTAL);
        SwingUtils.synchronizeView(getViewport(), footer, SwingConstants.HORIZONTAL);
    }

    /**
     * 列ヘッダービューポートを設定する
     * @param columnHeader		列ヘッダービューポート
     */
    @Override
    public void setColumnHeader(JViewport columnHeader) {
        super.setColumnHeader(columnHeader);
        SwingUtils.synchronizeView(this.columnHeader, getViewport(), SwingConstants.HORIZONTAL);
    }

    /**
     * 列フッターコンポーネントを設定する     *
     * @param view 列フッターコンポーネント
     */
    public void setColumnFooterView(Component view) {
        if (null == getColumnFooter()) {
            setColumnFooter(createViewport());
        }
        getColumnFooter().setView(view);
    }

    /**
     * 角のコンポーネントを取得する
     * @param key   角の識別文字列
     * @return  角のコンポーネント
     */
    public Component getScrollBarCorner(String key) {
        boolean isLeftToRight = getComponentOrientation().isLeftToRight();
        if (key.equals(FrameScrollPaneLayout.HORIZONTAL_LEADING)) {
            key = isLeftToRight ? FrameScrollPaneLayout.HORIZONTAL_LEFT : FrameScrollPaneLayout.HORIZONTAL_RIGHT;
        }
        else if (key.equals(FrameScrollPaneLayout.HORIZONTAL_TRAILING)) {
            key = isLeftToRight ? FrameScrollPaneLayout.HORIZONTAL_RIGHT : FrameScrollPaneLayout.HORIZONTAL_LEFT;
        }

        if (key.equals(FrameScrollPaneLayout.HORIZONTAL_LEFT)) {
            return horizontalCornerLeft;
        }
        else if (key.equals(FrameScrollPaneLayout.HORIZONTAL_RIGHT)) {
            return horizontalCornerRight;
        }
        else if (key.equals(FrameScrollPaneLayout.VERTICAL_BOTTOM)) {
            return verticalCornerBottom;
        }
        else if (key.equals(FrameScrollPaneLayout.VERTICAL_TOP)) {
            return verticalCornerTop;
        }
        else {
            return null;
        }
    }


    /**
     * 角のコンポーネントを設定する
     *
     * @param key    角の識別文字列
     * @param corner 角のコンポーネント
     */
    public void setScrollBarCorner(String key, Component corner) {
        Component old;
        boolean isLeftToRight = getComponentOrientation().isLeftToRight();
        if (key.equals(FrameScrollPaneLayout.HORIZONTAL_LEADING)) {
            key = isLeftToRight ? FrameScrollPaneLayout.HORIZONTAL_LEFT : FrameScrollPaneLayout.HORIZONTAL_RIGHT;
        }
        else if (key.equals(FrameScrollPaneLayout.HORIZONTAL_TRAILING)) {
            key = isLeftToRight ? FrameScrollPaneLayout.HORIZONTAL_RIGHT : FrameScrollPaneLayout.HORIZONTAL_LEFT;
        }

        if (key.equals(FrameScrollPaneLayout.HORIZONTAL_LEFT)) {
            old = horizontalCornerLeft;
            horizontalCornerLeft = corner;
        }
        else if (key.equals(FrameScrollPaneLayout.HORIZONTAL_RIGHT)) {
            old = horizontalCornerRight;
            horizontalCornerRight = corner;
        }
        else if (key.equals(FrameScrollPaneLayout.VERTICAL_TOP)) {
            old = verticalCornerTop;
            verticalCornerTop = corner;
        }
        else if (key.equals(FrameScrollPaneLayout.VERTICAL_BOTTOM)) {
            old = verticalCornerBottom;
            verticalCornerBottom = corner;
        }
        else {
            throw new IllegalArgumentException("invalid scroll bar corner key");
        }

        if (null != old) {
            remove(old);
        }
        if (null != corner) {
            add(corner, key);
        }
        if (corner != null) corner.setComponentOrientation(getComponentOrientation());
        firePropertyChange(key, old, corner);
        revalidate();
        repaint();
    }

    /**
     * 再描画を行う。
     */
    @Override
    public void updateUI() {
        super.updateUI();
        setLayout(new FrameScrollPaneLayout.UIResource());
        LookAndFeel.installBorder(this, "FrameScrollPane.border");
    }

    /**
     * 縦スクロールバーの全体表示フラグを取得する。
     * @return		縦スクロールバーの全体表示フラグ
     */
    public boolean isVerticalScrollBarCoversWhole() {
        return this.verticalScrollBarCoversWhole;
    }

    /**
     * 横スクロールバーの全体表示フラグを設定する。
     * @param whole		横スクロールバーの全体表示フラグ
     */
    public void setHorizontalScrollBarCoversWhole(boolean whole) {
        boolean old = this.horizontalScrollBarCoversWhole;
        if (old != whole) {
            this.horizontalScrollBarCoversWhole = whole;
            firePropertyChange(PROPERTY_HORIZONTAL_SCROLL_BAR_COVERS_WHOLE, old, this.horizontalScrollBarCoversWhole);
            invalidate();
            doLayout();
            if (getHorizontalScrollBar() != null) {
                getHorizontalScrollBar().doLayout();
            }
        }
    }

    /**
     * 横スクロールバーの全体表示フラグを取得する。
     * @return		横スクロールバーの全体表示フラグ
     */
    public boolean isHorizontalScrollBarCoversWhole() {
        return this.horizontalScrollBarCoversWhole;
    }

    /**
     * 縦スクロールバーの全体表示フラグを設定する。
     * @param whole		縦スクロールバーの全体表示フラグ
     */
    public void setVerticalScrollBarCoversWhole(boolean whole) {
        boolean old = this.verticalScrollBarCoversWhole;
        if (old != whole) {
            this.verticalScrollBarCoversWhole = whole;
            firePropertyChange(PROPERTY_VERTICAL_SCROLL_BAR_COVERS_WHOLE, old, this.verticalScrollBarCoversWhole);
            invalidate();
            doLayout();
            if (getVerticalScrollBar() != null) {
                getVerticalScrollBar().doLayout();
            }
        }
    }

    /**
     * 左上角と右上角を同一高さにするフラグを取得する
     * @return true=左上角と右上角を同一高さにする
     */
    public boolean isColumnHeadersUnified() {
        return this.columnHeadersUnified;
    }

    /**
     * 左上角と右上角を同一高さにするフラグを設定する
     * @param  unified     true=左上角と右上角を同一高さにする
     */
    public void setColumnHeadersUnified(boolean unified) {
        boolean old = this.columnHeadersUnified;
        if (old != unified) {
            this.columnHeadersUnified = unified;
            firePropertyChange(PROPERTY_COLUMN_HEADERS_UNIFIED, old, this.horizontalScrollBarCoversWhole);
            invalidate();
            doLayout();
        }
    }

    /**
     * 左下角と右下角を同一高さにするフラグを取得する
     * @return true=左下角と右下角を同一高さにする
     */
    public boolean isColumnFootersUnified() {
        return this.columnFootersUnified;
    }

    /**
     * 左下角と右下角を同一高さにするフラグを設定する
     * @param unified true=左下角と右下角を同一高さにする
     */
    public void setColumnFootersUnified(boolean unified) {
        boolean old = this.columnFootersUnified;
        if (old != unified) {
            this.columnFootersUnified = unified;
            firePropertyChange(PROPERTY_COLUMN_FOOTERS_UNIFIED, old, this.horizontalScrollBarCoversWhole);
            invalidate();
            doLayout();
        }
    }

}
