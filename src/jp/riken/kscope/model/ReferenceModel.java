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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Observable;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;

import jp.riken.kscope.Message;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 参照一覧モデル
 * @author riken
 *
 */
public class ReferenceModel extends Observable {

    /** 参照一覧ツリーモデル */
    private DefaultTreeModel treeModel;

    /** タイトル */
    private String title;

    /**
     * コンストラクタ
     */
    public ReferenceModel() {
        clearTreeModel();
    }


    /**
     * モデルの変更を通知する
     */
    public void notifyModel() {
        this.setChanged();
        this.notifyObservers();
        this.clearChanged();
    }

    /**
     * 参照一覧ツリーをクリアする。
     */
    public void clearTreeModel() {
        DefaultMutableTreeNode rootNode = new DefaultMutableTreeNode(Message.getString("mainmenu.analysis.dec-def-ref")); //宣言・参照一覧
        treeModel = new DefaultTreeModel(rootNode);
        this.title = null;

        notifyModel();
    }

    /**
     * ルートノードを取得する
     * @return		ルートノード
     */
    public DefaultMutableTreeNode getRootNode() {
        return (DefaultMutableTreeNode) treeModel.getRoot();
    }

    /**
     * ツリーモデルを取得する
     * @return		ツリーモデル
     */
    public DefaultTreeModel getTreeModel() {
        return treeModel;
    }

    /**
     * ツリーモデルを設定する
     * @param tree		ツリーモデル
     */
    public void setTreeModel(DefaultTreeModel tree) {
        this.treeModel = tree;

        // ツリーの更新
        notifyModel();
    }

    /**
     * タイトルを取得する
     * @return	タイトル
     */
    public String getTitle() {
        return title;
    }

    /**
     * タイトルを設定する
     * @param title		タイトル
     */
    public void setTitle(String title) {
        this.title = title;
    }

    /**
     * テーブル情報をファイル出力する。
     * @param   file   出力ファイル
     */
    public void writeFile(File file) {

        // ルートノード
        DefaultMutableTreeNode root = getRootNode();
        if (root == null) return;
        if (root.getChildCount() <= 0) return;

        try {
            // ファイル出力
            PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));

            // ツリーをCSV文字列にする。
            String buf = SwingUtils.toCsv(root);
            // ファイル出力
            pw.print(buf);

            pw.close();
        } catch (IOException ex) {
            ex.printStackTrace();
        }

    }

    /**
     * モデルが空か否か
     * @return	空か否か（true: 空，false: データあり）
     */
    public boolean isEmpty() {
    	DefaultMutableTreeNode root = getRootNode();
    	if (root == null) return true;
    	return (root.getChildCount() < 1);
    }
}


