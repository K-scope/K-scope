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

import javax.swing.tree.TreeNode;

import jp.riken.kscope.data.SearchOption;


/**
 * 検索フィルタツリーモデルクラス
 * @author riken
 *
 */
public class SearchTreeModel extends FilterTreeModel {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** 検索条件 */
    private SearchOption searchOption;
    /** 検索ノード */
    private TreeNode[] searchNodes;
    /** フィルタ適用フラグ */
    private boolean applyFilter;

    /**
     * コンストラクタ
     * @param node		ルートノード
     */
    public SearchTreeModel(SearchTreeNode node) {
        super((SearchTreeNode)node);
        this.applyFilter = false;
    }

    /**
     * ノード検索を実行する
     */
    @Override
    public void find() {
        if (this.root != null) {
            SearchTreeNode node = (SearchTreeNode) root;
            // 検索条件を設定
            node.setSearchOption(this);
            node.setApplyFilter(this.applyFilter);
            if (this.applyFilter) {
	            // フィルタを設定
	            node.setListFilter(this.getListFilter());
            }

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
        if (parent instanceof SearchTreeNode) {
            return (((SearchTreeNode) parent).getChildCount());
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
        if (parent instanceof SearchTreeNode) {
            return (((SearchTreeNode) parent).getChildAt(index));
        }
        return null;
    }

    /**
     * 検索ノードを取得する
     * @return searchNodes		検索ノード
     */
    public TreeNode[] getSearchNodes() {
        return searchNodes;
    }

    /**
     * 検索ノードを設定する
     * @param searchNodes 		検索ノード
     */
    public void setSearchNodes(TreeNode[] searchNodes) {
        this.searchNodes = searchNodes;
    }

    /**
     * 検索条件を取得する.
     * @return		検索条件
     */
    public SearchOption getSearchOption() {
        return this.searchOption;
    }

    /**
     * 検索条件を設定する.
     * @param searchOption		検索条件
     */
    public void setSearchOption(SearchOption searchOption) {
        this.searchOption = searchOption;
    }


    /**
     * ノードフィルタを適用する
     * @param filter	true=ノードフィルタを適用する
     */
    public void setApplyFilter(boolean filter) {
    	applyFilter = filter;
    }
}


