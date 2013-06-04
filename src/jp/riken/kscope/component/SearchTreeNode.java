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

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;

import jp.riken.kscope.data.SearchOption;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;


/**
 * 検索フィルタノードクラス
 * @author riken
 */
public class SearchTreeNode extends FilterTreeNode {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** 検索条件 */
    private SearchOption searchOption;
    /** 検索ノード */
    private TreeNode[] searchNodes;
    /** フィルタ適用フラグ */
    private boolean applyFilter;

    /**
     * ノードの追加フラグ
     * true=ノードを親ノードに追加する。ノードが検索一致、又は、子ノードが存在する場合
     */
    private boolean passed = true;

    /** 検索一致子ノードリスト */
    private List<SearchTreeNode> filteredChildren = new ArrayList<SearchTreeNode>();

    /**
     * ノードの検索結果フラグ
     * true=ノードが検索結果に一致
     */
    private boolean match = false;

    /**
     * コンストラクタ
     * @param userObject		ノードユーザオブジェクト
     */
    public SearchTreeNode(Object userObject) {
        super(userObject);
    }

    /**
     * コンストラクタ
     * @param node		ツリーノード
     */
    public SearchTreeNode(DefaultMutableTreeNode node) {
        this(node.getUserObject());
        for (int i=0; i<node.getChildCount(); i++) {
            DefaultMutableTreeNode child = (DefaultMutableTreeNode)node.getChildAt(i);
            SearchTreeNode searchChild = new SearchTreeNode(child);
            // 検索条件を設定する。
            searchChild.setSearchOption(this);
            // 子ノードの追加
            this.add(searchChild);
        }
    }

    /**
     * コンストラクタ
     */
    public SearchTreeNode() {
        super();
    }

    /**
     * ノード検索を行う
     */
    @Override
    public void find() {
        passed = false;
        filteredChildren.clear();
        if (!validateSearch()) {
            // 検索条件無しであるので、無条件追加
            passed = true;
            // 子ノード検索
            passFilterDown();
        } else if (pass(this)) {
            // 検索一致によるノード追加
            passed = true;
            // 子ノード検索
            passFilterDown();
        } else {
            // 子ノード検索
            passFilterDown();
            passed = filteredChildren.size() != 0;
        }
    }

    /**
     * 追加ノードであるかチェックする.<br/>
     * true=検索一致ノードである.<br/>
     * true=検索条件がない.<br/>
     * @param node		ノード
     * @return			true=追加ノード
     */
    protected boolean pass(SearchTreeNode node) {

    	if (this.applyFilter) {
    		// ノードフィルタチェック
	    	if (!this.pass((FilterTreeNode)node)) {
	    		return false;
	    	}
    	}

        // 検索条件チェック
        if (!validateSearch()) {
            return true;
        }

        // 検索対象ノードであるかチェックする
        if (!isSearchNode(node)) {
            return false;
        }

        // 検索対象ノードクラスであるかチェックする
        Object obj = node.getUserObject();
        if (!isSearchClass(obj)) {
            return false;
        }
        // ノード文字列
        String nodeText = obj.toString();
        if (obj instanceof File) {
            File file = (File)obj;
            if (node.getParent() == null) {
                nodeText = file.getAbsolutePath();
            }
            else {
                nodeText = file.getName();
            }
        }
        if (nodeText == null) return false;

        boolean result = false;
        if (this.searchOption.isVariable()) {
            // 変数(=トレース)検索
            result = StringUtils.existsSearchWord(nodeText, this.searchOption.getSearchText());
        }
        else {
            // テキスト検索
            result = StringUtils.existsSearchText(
                                    nodeText,
                                    this.searchOption.getSearchText(),
                                    this.searchOption.isSensitivecase(),
                                    this.searchOption.isRegex(),
                                    this.searchOption.isWord());
        }
        // ノードの検索結果
        this.match = result;

        return result;
    }

    /**
     * 検索対象ノードであるかチェックする.<br/>
     * 検索ノードが設定されていなければ、すべてtrueとする.<br/>
     * 検索ノードと同じノードか、子ノードを検索対象ノードとする
     * @param node		検索対象ノード
     * @return			true=検索対象ノードである
     */
    private boolean isSearchNode(SearchTreeNode node) {
        if (searchNodes == null) return true;

        // 検索ノードであるか
        for (TreeNode searchNode : this.searchNodes) {
            if (!(searchNode instanceof DefaultMutableTreeNode)) {
                continue;
            }
            if (((DefaultMutableTreeNode)searchNode).getUserObject() == node.getUserObject()) {
                return true;
            }
            if (SwingUtils.isChildNode((DefaultMutableTreeNode)searchNode, node)) {
                return true;
            }
        }

        return false;
    }

    /**
     * 子ノードの検索を行う.
     */
    private void passFilterDown() {
        int realChildCount = super.getChildCount();
        for (int i = 0; i < realChildCount; i++) {
            SearchTreeNode realChild = (SearchTreeNode) super.getChildAt(i);
            // 検索条件を設定する。
            realChild.setSearchOption(this);
            realChild.setApplyFilter(this.applyFilter);
            if (this.applyFilter) {
	            // フィルタを設定
	            realChild.setListFilter(this.getListFilter());
            }

            realChild.find();
            if (realChild.isPassed()) {
                filteredChildren.add(realChild);
            }
        }
    }

    /**
     * 子ノードを追加する
     * @param node		追加子ノード
     */
    public void add(SearchTreeNode node) {
        super.add(node);
        node.find();
        if (node.isPassed()) {
            filteredChildren.add(node);
        }
    }

    /**
     * 子ノードを削除する
     * @param childIndex		子ノードインデックス
     */
    @Override
    public void remove(int childIndex) {
        // 検索条件チェック
        if (!validateSearch()) {
            // as child indexes might be inconsistent..
            throw new IllegalStateException(
                    "Can't remove while the filter is active");
        }
        super.remove(childIndex);
    }

    /**
     * 子ノード数を取得する
     * @return		子ノード数
     */
    @Override
    public int getChildCount() {
        // 検索条件チェック
        if (!validateSearch()) {
            return super.getChildCount();
        }
        return (filteredChildren.size());
    }


    /**
     * 子ノードを取得する
     * @return		子ノードインデックス
     */
    @Override
    public SearchTreeNode getChildAt(int index) {
        // 検索条件チェック
        if (!validateSearch()) {
            return (SearchTreeNode) super.getChildAt(index);
        }
        return filteredChildren.get(index);
    }

    /**
     * 親ノードへ追加ノードであるか取得する.
     * @return			true=追加ノード
     */
    public boolean isPassed() {
        return passed;
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
     * 検索条件を設定する
     * @param node		検索ノード
     */
    public void setSearchOption(SearchTreeNode node) {

        // 検索条件を設定
        this.searchOption = node.searchOption;
        // 検索ノードを設定する
        this.setSearchNodes(node.searchNodes);
    }

    /**
     * 検索条件を設定する
     * @param model		検索モデル
     */
    public void setSearchOption(SearchTreeModel model) {
        // 検索条件を設定
        this.searchOption = model.getSearchOption();
        // 検索ノードを設定する
        this.setSearchNodes(model.getSearchNodes());
    }

    /**
     * ノードの検索結果を取得する
     * @return		ノードの検索結果
     */
    public boolean isMatch() {
        return match;
    }

    /**
     * 検索条件が設定されているかチェックする.
     * @return		true=検索条件設定済み
     */
    private boolean validateSearch() {
        if (this.searchOption == null) return false;
        if (this.searchOption.getSearchText() == null) return false;
        if (this.searchOption.getSearchText().isEmpty()) return false;

        if (this.applyFilter) {
	        // フィルタの適用チェック
	        return this.validateFilter();
        }
        else {
        	// フィルタ未設定
        	return true;
        }
    }

    /**
     * 検索対象のノードオブジェクトであるかチェックする。
     * @param    node		ノードユーザオブジェクト
     * @return		true=検索対象
     */
    private boolean isSearchClass(Object node) {
        if (this.searchOption == null) return false;
        if (this.searchOption.getSearchClass() != null) {
            // 検索対象のノードオブジェクトであるかチェックする。
            return (this.searchOption.getSearchClass().isInstance(node));
        }
        return true;
    }

    /**
     * ノードフィルタを適用する
     * @param filter	true=ノードフィルタを適用する
     */
    public void setApplyFilter(boolean filter) {
    	applyFilter = filter;
    }
}
