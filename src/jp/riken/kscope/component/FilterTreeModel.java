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

import java.util.List;

import javax.swing.tree.DefaultTreeModel;

import jp.riken.kscope.common.FILTER_TYPE;


/**
 * フィルタツリーモデルクラス
 * @author riken
 *
 */
public class FilterTreeModel extends DefaultTreeModel {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** ノードフィルタのクラス */
    private List<FILTER_TYPE> listFilter;

    /**
     * コンストラクタ
     * @param node		ルートノード
     */
    public FilterTreeModel(FilterTreeNode node) {
        super(node);
    }

    /**
     * ノードフィルタを実行する
     */
    public void find() {
        if (this.root != null) {
            FilterTreeNode node = (FilterTreeNode) root;
            // フィルタを設定
            node.setListFilter(this.listFilter);

            // ノード検索
            node.find();

            // ツリー変更イベント
            Object[] path = { root };
            fireTreeStructureChanged(this, path, null, null);
        }
    }

    /**
     * 親ノードの子ノード数を取得する
     * @param   parent    親ノード
     * @return		子ノード数
     */
    @Override
    public int getChildCount(Object parent) {
        if (parent instanceof FilterTreeNode) {
            return (((FilterTreeNode) parent).getChildCount());
        }
        return 0;
    }

    /**
     * 親ノードの子ノードを取得する
     * @param   parent    親ノード
     * @param   index    子ノードインデックス
     * @return		子ノード
     */
    @Override
    public Object getChild(Object parent, int index) {
        if (parent instanceof FilterTreeNode) {
            return (((FilterTreeNode) parent).getChildAt(index));
        }
        return null;
    }


    /**
     * ノードフィルタを取得する
     * @return		ノードフィルタ
     */
    public List<FILTER_TYPE> getListFilter() {
        return listFilter;
    }

    /**
     * ノードフィルタを設定する
     * @param list		ノードフィルタ
     */
    public void setListFilter(List<FILTER_TYPE> list) {
        this.listFilter = list;
    }
}


