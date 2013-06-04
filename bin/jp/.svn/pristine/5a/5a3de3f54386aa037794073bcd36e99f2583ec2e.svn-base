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
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.BorderFactory;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.JTableHeader;


/**
 * ストライプテーブルコンポーネント
 * @author riken
 */
public class JStripeTable extends JTable implements MouseListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** 行高さ */
    private final int ROW_HEIGHT = 22;

    /** 奇数行の背景色 */
    private final Color EVEN_COLOR = new Color(240, 240, 255);

    /** セルの左側マージン */
    private final int CELL_MARGIN = 5;
    /** 列サイズ変更フラグ */
    private boolean resizing = false;

    /**
     * コンストラクタ
     */
    public JStripeTable() {
        this(SwingConstants.LEFT);
    }

    /**
     * コンストラクタ
     * @param   alignments   列の表示配置
     * @param   defultalign   デフォルト表示配置
     */
    public JStripeTable(int[] alignments, int defultalign) {
        super();

        // テーブル高さを拡張する
        this.setFillsViewportHeight(true);

        // セルグリッドを表示しない
        this.setShowGrid(false);

        // 編集不可に設定
        this.setDefaultEditor(Object.class, null);
        // 横スクロールバー表示の為
        this.setAutoResizeMode(AUTO_RESIZE_OFF);

        // 行高さの変更
        // 行
        this.setRowHeight(ROW_HEIGHT);

        // 偶数、奇数行で背景色をストライプ描画する
        StripeTableRenderer renderer = new StripeTableRenderer(alignments, defultalign, CELL_MARGIN);
        this.setDefaultRenderer(Object.class, renderer);

        // ヘッダー
        JTableHeader header = this.getTableHeader();
        // テーブルの列移動を不許可にする。
        header.setReorderingAllowed(false);

        // ヘッダーのレンダリング設定
        header.setDefaultRenderer(renderer);

        this.getTableHeader().addMouseListener(this);

    }

    /**
     * コンストラクタ
     * @param   alignment   セルのデフォルト表示配置
     */
    public JStripeTable(int alignment) {
        this(null, alignment);
    }
/*
    @Override
    public boolean getScrollableTracksViewportWidth() {
        // System.out.println("width=" + getPreferredSize().width + "<" + getParent().getWidth() + "=" + (getPreferredSize().width < getParent().getWidth()));
        if (resizing) {
            if (getPreferredSize().width - getParent().getWidth() > -50) {
                return false;
            }
        }

        return getPreferredSize().width < getParent().getWidth();
    }
*/

    /**
     * 偶数、奇数行で背景色をストライプ描画クラス
     * @author riken
     *
     */
    class StripeTableRenderer extends DefaultTableCellRenderer {

        /** シリアル番号 */
        private static final long serialVersionUID = 1L;
        /** 配置 */
        private int[] alignments;
        /** 配置 */
        private int default_alignment;
        /** マージン */
        private int margin;

        /**
         * コンストラクタ
         * @param defaultalign		デフォルト配置
         * @param margin		マージン
         */
        public StripeTableRenderer(int defaultalign, int margin) {
            this.default_alignment = defaultalign;
            this.margin = margin;
        }

        /**
         * コンストラクタ
         * @param alignments		列配置
         * @param defaultalign		デフォルト配置
         * @param margin		マージン
         */
        public StripeTableRenderer(int[] alignments, int defaultalign, int margin) {
            this.alignments = alignments;
            this.default_alignment = defaultalign;
            this.margin = margin;
        }


        /**
         * セルを偶数、奇数行で背景色をストライプ描画を行う
         * @param table			テーブル
         * @param value			セル値
         * @param isSelected	選択状態
         * @param hasFocus		フォーカス状態
         * @param row			行インデックス
         * @param column		列インデックス
         * @return				描画コンポーネント
         */
        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
            super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

            // 偶数、奇数行で背景色をストライプ描画する。
            if (row == -1) {
                // ヘッダー
                setForeground(table.getTableHeader().getForeground());
                setBackground(table.getTableHeader().getBackground());
            }
            else if(isSelected) {
                setForeground(table.getSelectionForeground());
                setBackground(table.getSelectionBackground());
            }
            else {
                setForeground(table.getForeground());
                setBackground((row%2==0)?EVEN_COLOR:table.getBackground());
            }

            if (margin > 0) {
                setFont( table.getFont() );
                // 境界線の設定、左寄せ時のスペースを確保する
                Border border = null;
                if (row == -1) {
                    // ヘッダー
                    border = UIManager.getBorder( "TableHeader.cellBorder" );
                }
                else {
                    border = table.getBorder();
                    if (border instanceof LineBorder) {
                        border = new LineBorder(Color.WHITE, 0, false);
                    }
                    // ヘッダー以外のボーダは描画しない
                    border = null;
                }
                if (border != null) {
                    Border setBorder = BorderFactory.createCompoundBorder( border, new EmptyBorder( 0, margin, 0, 0 ) );
                    setBorder( setBorder );
                }
                // 改行を含むテキスト表示は、先頭の１行のみとする.
                String text = value!=null?(String)value.toString():"";
                String br = System.getProperty("line.separator");
                if (text.indexOf(br) < 0)  {
                	setValue( value );
                }
                else {
                    String[] lines = text.split(br);
                	setValue( lines[0] );
                }
            }

            // 文字配置
            if (this.alignments == null) {
                // 数値セルは右寄せとする
                setHorizontalAlignment(getCellHorizontalAlignment(value));
            }
            else {
                if (this.alignments.length > column) {
                    setHorizontalAlignment(this.alignments[column]);
                }
                else {
                    setHorizontalAlignment(this.default_alignment);
                }
            }
            return this;
        }

        /**
         * セルの値により左寄せ、右寄せを行う。
         * @param value			セル値
         * @return			SwingConstants.LEFT or SwingConstants.RIGHT
         */
        private int getCellHorizontalAlignment(Object value) {
            if (value instanceof Integer) return SwingConstants.RIGHT;
            if (value instanceof Long) return SwingConstants.RIGHT;
            if (value instanceof Float) return SwingConstants.RIGHT;
            if (value instanceof Double) return SwingConstants.RIGHT;
            if (value instanceof Number) return SwingConstants.RIGHT;

            return this.default_alignment;
        }
    }

    /**
     * マウスクリックイベント
     * @param event		マウスイベント情報
     */
    @Override
    public void mouseClicked(MouseEvent event) { }

    /**
     * マウスボタンダウンイベント
     * @param e		マウスイベント情報
     */
    @Override
    public void mousePressed(MouseEvent e) {
        if (e.getButton() == MouseEvent.BUTTON1) {
            resizing = true;
        }
    }

    /**
     * マウスボタンアップイベント
     * @param e		マウスイベント情報
     */
    @Override
    public void mouseReleased(MouseEvent e) {
        resizing = false;
    }

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
    public void mouseExited(MouseEvent e) {
        resizing = false;
    }



}
