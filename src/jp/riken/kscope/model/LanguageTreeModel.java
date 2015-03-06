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

package jp.riken.kscope.model;

import java.util.ArrayList;
import java.util.List;
import java.util.Observable;

import javax.swing.SwingUtilities;

import jp.riken.kscope.common.FILTER_TYPE;
import jp.riken.kscope.component.FilterTreeModel;
import jp.riken.kscope.component.FilterTreeNode;
import jp.riken.kscope.language.Program;


/**
 * 構造ツリーモデル
 * @author RIKEN
 *
 */
public class LanguageTreeModel extends Observable {

    /** ツリーモデル */
    private FilterTreeModel treeModel;
    /** 構造ツリーフィルタ */
    private List<FILTER_TYPE> listFilter;
    /** データベース */
    private Program languageDb;

    /**
     * コンストラクタ
     */
    public LanguageTreeModel() {
        clearTreeModel();
    }


    /**
     * モデルの変更を通知する
     */
    public void notifyModel() {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                setChanged();
                notifyObservers();
                clearChanged();
            }
        });
    }

    /**
     * 構造ツリーをクリアする。
     */
    public void clearTreeModel() {
        FilterTreeNode rootNode = new FilterTreeNode("Structure tree");
        treeModel = new FilterTreeModel(rootNode);

        notifyModel();
    }

    /**
     * ルートノードを取得する
     * @return		ルートノード
     */
    public FilterTreeNode getRootNode() {
        return (FilterTreeNode) treeModel.getRoot();
    }

    /**
     * ツリーモデルを取得する
     * @return		ツリーモデル
     */
    public FilterTreeModel getTreeModel() {
        return treeModel;
    }

    /**
     * 構造ツリーフィルタを取得する
     * @return		構造ツリーフィルタ
     */
    public List<FILTER_TYPE> getListFilter() {
        return listFilter;
    }

    /**
     * 構造ツリーフィルタを設定する
     * @param list		構造ツリーフィルタ
     */
    public void setListFilter(FILTER_TYPE[] list) {
        this.listFilter = new ArrayList<FILTER_TYPE>();
        if (list != null) {
            this.listFilter.addAll(java.util.Arrays.asList(list));
        }

        notifyModel();
    }

    /**
     * ツリーモデルのフィルタを実行する.
     */
    public void filter() {
        if (treeModel != null) {
            // フィルタを設定する
            treeModel.setListFilter(this.listFilter);

            // フィルタ実行
            treeModel.find();
        }
    }

    /**
     * 構造情報ツリーが設定済みであるかチェックする.
     * @return		true=設定済み
     */
    public boolean isSetLanguageTree() {
    	FilterTreeNode root = getRootNode();
    	if (root == null) return false;
    	if (root.getChildCount() <= 0) return false;

    	return true;
    }


	/**
	 * データベースを取得する
	 * @return データベース
	 */
	public Program getLanguageDb() {
		return this.languageDb;
	}

	/**
	 * データベースを設定する.
	 * @param languageDb データベース
	 */
	public void setLanguageDb(Program language) {
		this.languageDb = language;
	}
}


