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
import java.awt.Font;

import javax.swing.JComponent;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SearchOption;
import jp.riken.kscope.utils.StringUtils;

/**
 * 検索ツリークラス
 * @author riken
 */
public class SearchTree extends ObjectTree {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** 検索条件 */
    private SearchOption searchOption;
    /** 表示前景色 */
    private Color forecolor;
    /** 表示背景色 */
    private Color backcolor;
    /** スタイル */
    private int fontstyle = Font.PLAIN;

    /**
     * コンストラクタ
     */
    public SearchTree() {
        super();
        // ツリーのノード描画クラスの設定
        SearchTreeCellRenderer renderer = new SearchTreeCellRenderer();
        this.setCellRenderer( renderer);
    }


    /**
     * 検索ツリーノードのレンダリングクラス
     * @author riken
     */
    public class SearchTreeCellRenderer extends ObjectTreeCellRenderer {

        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /**
         * コンストラクタ
         */
        public SearchTreeCellRenderer() {

        }

        /**
         * ツリー内のノードを描画方法を決定するメソッド
         *
         * @param t
         *            ペイントしているツリー
         * @param value
         *            表示される値
         * @param selected
         *            ノードが選択された場合にtrue
         * @param expanded
         *            展開されている場合にtrue
         * @param leaf
         *            要素が葉の場合にtrue
         * @param row
         *            ノードのインデックス
         * @param hasFocus
         *            指定のノードにフォーカスがある場合にtrue
         * @return 指定の値を描画するpaint()メソッドがあるコンポーネント
         * @see javax.swing.tree.TreeCellRenderer
         * @see javax.swing.tree.DefaultTreeCellRenderer
         */
        @Override
        public Component getTreeCellRendererComponent(JTree t, Object value,
                boolean selected, boolean expanded, boolean leaf, int row,
                boolean hasFocus) {

            JComponent c = (JComponent) super.getTreeCellRendererComponent(t, value, selected,
                    expanded, leaf, row, hasFocus);

            Object obj = ((DefaultMutableTreeNode) value).getUserObject();
            // 検索条件が設定されているか, 検索対象ノードであるか？
            if (validateSearch(obj)) {
                String text = getText();
                String prefix = null;
                if (obj instanceof CodeLine) {
                    text = ((CodeLine)obj).getStatement();
                    prefix = String.valueOf(((CodeLine)obj).getStartLine());
                    prefix = prefix + " : ";
                }
                if (SearchTree.this.searchOption.isVariable()) {
                    // 変数(=トレース)検索
                    text = StringUtils.searchWordToHtml(
                                            text,
                                            SearchTree.this.searchOption.getSearchText(),
                                            SearchTree.this.forecolor, SearchTree.this.backcolor,
                                            SearchTree.this.fontstyle);
                }
                else {
                    // テキスト検索
                    text = StringUtils.searchTextToHtml(
                                            text,
                                            SearchTree.this.searchOption.getSearchText(),
                                            SearchTree.this.forecolor, SearchTree.this.backcolor,
                                            SearchTree.this.fontstyle,
                                            SearchTree.this.searchOption.isSensitivecase(),
                                            SearchTree.this.searchOption.isRegex(),
                                            SearchTree.this.searchOption.isWord());
                }
                if (prefix != null) {
                    if (text.indexOf("<html>") == 0) {
                        text = "<html>" + prefix + text.substring(6);
                    }
                    else {
                        text = prefix + text;
                    }
                }
                this.setText(text);
            }

            return c;
        }

    }

    /**
     * 表示前景色を取得する
     * @return		表示前景色
     */
    public Color getForecolor() {
        return this.forecolor;
    }

    /**
     * 表示前景色を設定する
     * @param color		表示前景色
     */
    public void setForecolor(Color color) {
        this.forecolor = color;
    }

    /**
     * 表示背景色を取得する
     * @return		表示背景色
     */
    public Color getBackcolor() {
        return this.backcolor;
    }

    /**
     * 表示背景色を設定する
     * @param color		表示背景色
     */
    public void setBackcolor(Color color) {
        this.backcolor = color;
    }

    /**
     * フォントスタイルを取得する
     * @return	フォントスタイル
     */
    public int getFontstyle() {
        return fontstyle;
    }

    /**
     * フォントスタイルを設定する
     * @param fontstyle		フォントスタイル
     */
    public void setFontstyle(int fontstyle) {
        this.fontstyle = fontstyle;
    }

    /**
     * ツリーモデルを設定する
     * @param model		ツリーモデル
     */
    public void setModel(SearchTreeModel model) {
        super.setModel(model);

        // 検索条件を設定する
        this.searchOption = model.getSearchOption();
    }

    /**
     * 検索条件が設定されているかチェックする.
     * @param    node		ノードユーザオブジェクト
     * @return		true=検索条件設定済み
     */
    private boolean validateSearch(Object node) {
        if (this.searchOption == null) return false;
        if (this.searchOption.getSearchText() == null) return false;
        if (this.searchOption.getSearchText().isEmpty()) return false;
        if (this.searchOption.getSearchClass() != null) {
            // 検索対象のノードオブジェクトであるかチェックする。
            return (this.searchOption.getSearchClass().isInstance(node));
        }
        return true;
    }

    /**
     * 検索条件を取得する
     * @return		検索条件
     */
    public SearchOption getSearchOption() {
        return searchOption;
    }

    /**
     * 検索条件を設定する
     * @param searchOption		検索条件
     */
    public void setSearchOption(SearchOption searchOption) {
        this.searchOption = searchOption;
    }
}


