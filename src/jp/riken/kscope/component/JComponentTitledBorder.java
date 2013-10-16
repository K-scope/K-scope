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

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

import javax.swing.JComponent;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;

/**
 * コンポーネント付きボーダクラス
 * @author riken
 */
public class JComponentTitledBorder implements Border, MouseListener, MouseMotionListener, SwingConstants {
	/** ボーダ上のコンポーネントのX方向オフセット */
    private static final int offset = 5;
    /** ボーダ上のコンポーネント */
    private final Component comp;
    /** コンテナ */
    private final JComponent container;
    /** ボーダ */
    private final Border border;

    /**
     * コンストラクタ
     * @param comp			ボーダ上のコンポーネント
     * @param container		コンテナ
     * @param border		ボーダ
     */
    public JComponentTitledBorder(Component comp, JComponent container, Border border) {
        this.comp      = comp;
        this.container = container;
        this.border    = border;
        if(comp instanceof JComponent) {
            ((JComponent)comp).setOpaque(true);
        }
        if (this.container != null) {
            this.container.addMouseListener(this);
            this.container.addMouseMotionListener(this);
        }
    }

    /**
     * ボーダを透明描画とするか取得する。
     * @return		透明描画
     */
    @Override
    public boolean isBorderOpaque() {
        return true;
    }

    /**
     * ボーダを描画する
     * @param c			コンポーネント
     * @param g			グラフィック
     * @param x			X位置
     * @param y			Y位置
     * @param width		幅
     * @param height	高さ
     */
    @Override
    public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
        Insets borderInsets = border.getBorderInsets(c);
        Insets insets = getBorderInsets(c);
        int temp = (insets.top-borderInsets.top)/2;
        border.paintBorder(c, g, x, y+temp, width, height-temp);
        Dimension size = comp.getPreferredSize();
        Rectangle rect = new Rectangle(offset, 0, size.width, size.height);
        SwingUtilities.paintComponent(g, comp, (Container)c, rect);
        comp.setBounds(rect);
    }

    /**
     * ボーダ余白を取得する
     * @param c		コンポーネント
     * @return		余白
     */
    @Override
    public Insets getBorderInsets(Component c) {
        Dimension size = comp.getPreferredSize();
        Insets insets = border.getBorderInsets(c);
        insets.top = Math.max(insets.top, size.height);
        return insets;
    }

    /**
     * イベントをディスパッチする
     * @param me		マウスイベント情報
     */
    private void dispatchEvent(MouseEvent me) {
        Component src = me.getComponent();
        comp.dispatchEvent(SwingUtilities.convertMouseEvent(src, me, comp));
        src.repaint();
    }
    /**
     * マウスクリックイベント
     * @param me		マウスイベント情報
     */
    @Override
    public void mouseClicked(MouseEvent me) {
        dispatchEvent(me);
    }
    /**
     * マウスオーバーイベント
     * @param me		マウスイベント情報
     */
    @Override
    public void mouseEntered(MouseEvent me) {
        dispatchEvent(me);
    }

    /**
     * マウスアウトイベント
     * @param me		マウスイベント情報
     */
    @Override
    public void mouseExited(MouseEvent me) {
        dispatchEvent(me);
    }

    /**
     * マウスボタンダウンイベント
     * @param me		マウスイベント情報
     */
    @Override
    public void mousePressed(MouseEvent me) {
        dispatchEvent(me);
    }

    /**
     * マウスボタンアップイベント
     * @param me		マウスイベント情報
     */
    @Override public void mouseReleased(MouseEvent me) {
        dispatchEvent(me);
    }

    /**
     * マウス移動イベント
     * @param me		マウスイベント情報
     */
    @Override public void mouseMoved(MouseEvent me) {
        dispatchEvent(me);
    }

    /**
     * マウスドラッグイベント
     * @param me		マウスイベント情報
     */
    @Override public void mouseDragged(MouseEvent me) {
        dispatchEvent(me);
    }
}

