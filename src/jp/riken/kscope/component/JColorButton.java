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

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;

import javax.swing.Icon;
import javax.swing.JButton;

/**
 * カラーボタン
 * @author RIKEN
 */
public class JColorButton extends JButton {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** 表示カラー */
    private Color color;

    /**
     * コンストラクタ
     */
    public JColorButton() {
        super();
        this.setColor(null);
        initGUI();
    }

    /**
     * コンストラクタ
     * @param color		カラー設定
     */
    public JColorButton(Color color) {
        super();
        this.setColor(color);
        initGUI();
    }

    /**
     * GUIの初期化を行う。
     */
    private void initGUI() {
        java.awt.Dimension size = new java.awt.Dimension(48, 22);
        this.setSize(size);
        this.setPreferredSize(size);
        this.setMinimumSize(size);
        this.setMaximumSize(size);
        this.setMargin(new Insets(5, 5, 5, 5));
    }


    /**
     * ボタンカラーを取得する
     * @return color		ボタンカラー
     */
    public Color getColor() {
        return color;
    }

    /**
     * ボタンカラーを設定する
     * @param color 	ボタンカラー
     */
    public void setColor(Color color) {
        this.color = color;
        this.setIcon(new ColorIcon(color));
    }

    /**
     * ボタン表示文字列をnullとする.<br/>
     * カラーボタンには文字列は表示しない。
     * @return		null固定
     */
    @Override
    public String getText() {
        return null;
    }

    /**
     * カラーボタン用のカラーアイコン作成クラス
     * @author RIKEN
     */
    private static class ColorIcon implements Icon {
    	/** アイコン幅 */
        private int width;
        /** アイコン高さ */
        private int height;
        /** カラー */
        private Color color;

        /**
         * コンストラクタ
         */
        public ColorIcon() {
            // アイコンサイズは32x12に設定
            width  = 32;
            height = 12;
        }
        /**
         * コンストラクタ
         * @param color		アイコンカラー
         */
        public ColorIcon(Color color) {
            this();
            this.color = color;
        }

        /**
         * カラーアイコンを描画する.
         */
        @Override
        public void paintIcon(Component c, Graphics g, int x, int y) {
            if (this.color == null) return;

            g.translate(x, y);
            g.setColor(this.color);
            g.fillRect(0,  0, width, height);
            g.translate(-x, -y);
        }

        /**
         * カラーアイコン幅を取得する。
         */
        @Override
        public int getIconWidth() {
            return width;
        }

        /**
         * カラーアイコン高さを取得する。
         */
        @Override
        public int getIconHeight() {
            return height;
        }
    }
}
