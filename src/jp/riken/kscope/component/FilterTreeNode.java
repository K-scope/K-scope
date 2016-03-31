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

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.swing.tree.DefaultMutableTreeNode;

import jp.riken.kscope.common.FILTER_TYPE;


/**
 * フィルタノードクラス
 * @author RIKEN
 */
public class FilterTreeNode extends DefaultMutableTreeNode {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /**
     * ノードの追加フラグ
     * true=ノードを親ノードに追加する。ノードがフィルタ一致、又は、子ノードが存在する場合
     */
    private boolean passed = true;

    /** フィルタ一致子ノードリスト */
    private List<FilterTreeNode> filteredChildren = new ArrayList<FilterTreeNode>();

    /** ノードフィルタのクラス */
    private List<FILTER_TYPE> listFilter;

    /** 子孫ノードの未展開の深さ */
    private int depth;

    /**
     * コンストラクタ
     * @param userObject		ノードユーザオブジェクト
     */
    public FilterTreeNode(Object userObject) {
        super(userObject);
        depth = 0;
    }

    /**
     * コンストラクタ
     * @param userObject		ノードユーザオブジェクト
     * @param depth				ノード
     */
    public FilterTreeNode(Object userObject, int depth) {
        super(userObject);
        this.depth = depth;
    }

    /**
     * コンストラクタ
     */
    public FilterTreeNode() {
        super();
    }

    /**
     * ノードフィルタを行う
     */
    public void find() {
        passed = false;
        filteredChildren.clear();
        // フィルタの有無チェック
        if (!validateFilter()) {
            // フィルタ無しであるので、無条件追加
            passed = true;
            // 子ノード検索
            passFilterDown();
        } else if (pass(this)) {
            // フィルタクラス一致によるノード追加
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
     * true=フィルタ一致ノードである.<br/>
     * true=フィルタ条件がない.<br/>
     * @param node		ノード
     * @return			true=追加ノード
     */
    protected boolean pass(FilterTreeNode node) {

        // フィルタの有無チェック
        if (!validateFilter()) {
            return true;
        }

        // フィルタ対象ノードクラスであるかチェックする
        Object obj = node.getUserObject();
        if (!isFilter(obj)) {
            return false;
        }
        return true;
    }

    /**
     * 子ノードのフィルタを行う.
     */
    private void passFilterDown() {
        int childCount = super.getChildCount();
        for (int i = 0; i < childCount; i++) {
            FilterTreeNode child = (FilterTreeNode) super.getChildAt(i);
            // フィルタを設定する。
            child.setListFilter(this.listFilter);

            child.find();
            if (child.isPassed()) {
                filteredChildren.add(child);
            }
        }
    }

    /**
     * 子ノードを追加する
     * @param node		追加子ノード
     * @return  追加子ノード
     */
    public FilterTreeNode add(FilterTreeNode node) {
    	FilterTreeNode result = node;
    	if (!containsChild(node)) {
	    	int index = super.getChildCount();
	        super.insert(node, index);
    	}
    	else {
    		result = (FilterTreeNode)equalsChild(node);
    	}

        // フィルタを設定する。
        node.setListFilter(this.listFilter);

        node.find();
        if (node.isPassed()) {
        	if (!containsList(node, filteredChildren)) {
        		filteredChildren.add(node);
        	}
        }
        return result;
    }

    /**
     * 子ノードを削除する
     * @param childIndex		子ノードインデックス
     */
    @Override
    public void remove(int childIndex) {
        // フィルタの有無チェック
        if (!validateFilter()) {
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
        // フィルタの有無チェック
        if (!validateFilter()) {
            return super.getChildCount();
        }
        return (filteredChildren.size());
    }


    /**
     * 子ノードを取得する
     * @return		子ノードインデックス
     */
    @Override
    public FilterTreeNode getChildAt(int index) {
        // フィルタの有無チェック
        if (!validateFilter()) {
            return (FilterTreeNode) super.getChildAt(index);
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
     * フィルタが設定されているかチェックする.
     * @return		true=フィルタ設定済み
     */
    protected boolean validateFilter() {
        if (this.listFilter == null) return false;
        if (this.listFilter.contains(FILTER_TYPE.ALL)) {
            // すべて表示であるので、フィルタ適用なし
            return false;
        }

        return true;
    }

    /**
     * フィルタ対象のノードオブジェクトであるかチェックする。
     * @param    node		ノードユーザオブジェクト
     * @return		true=フィルタ対象ノード(表示ノード)
     */
    private boolean isFilter(Object node) {
        if (this.listFilter == null) return false;

        // フィルタ対象のノードオブジェクトであるかチェックする。
        for (FILTER_TYPE filter : this.listFilter) {
            if (filter.isFilter(node)) {
                return true;
            }
        }

        return false;
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

    /**
     * 子孫ノードの未展開の深さを取得する
     * @return	子孫ノードの未展開の深さ
     */
    public int getDepth() {
    	return this.depth;
    }

    /**
     * 子孫ノードの未展開の深さを設定する
     * @param depth	子孫ノードの未展開の深さ
     */
    public void setDepth(int depth) {
    	this.depth = depth;
    }

    /**
     * 子要素をすべて削除する
     */
	@Override
	public void removeAllChildren() {
		// 一時的にフィルタを削除する。
		List<FILTER_TYPE> filters = this.listFilter;
		this.listFilter = null;
		for (int i = super.getChildCount()-1; i >= 0; i--) {
		    super.remove(i);
		}
		filteredChildren.clear();
		this.listFilter = filters;
	}

	/**
	 * 子ノードに存在するかチェックする.
	 * @param child		対象ノード
	 * @return			true=子ノードである
	 */
	private boolean containsChild(FilterTreeNode child) {
		return (equalsChild(child) != null);
	}

	/**
	 * 子ノードから同一ノードを取得する.
	 * @param child		対象ノード
	 * @return		同一ノード
	 */
	private DefaultMutableTreeNode equalsChild(FilterTreeNode child) {
		if (child == null) {
		    return null;
		}
    	try {
		    if (super.getChildCount() == 0) {
			    return null;
		    }
			for (int i=0; i<super.getChildCount(); i++) {
				if (super.getChildAt(i) == null) continue;
				if (super.getChildAt(i) == child) {
					return (DefaultMutableTreeNode)super.getChildAt(i);
				}
				if (((DefaultMutableTreeNode)super.getChildAt(i)).getUserObject() == child.getUserObject()) {
					return (DefaultMutableTreeNode)super.getChildAt(i);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;


	}

	/**
	 * ノードリストにチェックノードが存在するかチェックする
	 * @param child		チェックノード
	 * @param list		ノードリスト
	 * @return			true=存在する
	 */
	private boolean containsList(FilterTreeNode child, List<FilterTreeNode> list) {
		if (child == null) {
		    return false;
		}
		if (list == null || list.size() <= 0) {
			return false;
		}
    	try {
    		CopyOnWriteArrayList<FilterTreeNode> copyList = new CopyOnWriteArrayList<FilterTreeNode>(list);
    		for (FilterTreeNode node : copyList) {
				if (child == null) return false;
				if (node == null) continue;
				if (node == child) {
					return true;
				}
				if (node.getUserObject() == child.getUserObject()) {
					return true;
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return false;
	}
}
