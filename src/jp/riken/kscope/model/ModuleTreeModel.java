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

import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;

import jp.riken.kscope.language.Program;
import jp.riken.kscope.utils.SwingUtils;


/**
 * モジュールツリーモデル
 * @author riken
 *
 */
public class ModuleTreeModel extends Observable {

    /** ツリールートノード */
    private DefaultMutableTreeNode rootNode;
    /** ツリーモデル */
    private DefaultTreeModel treeModel;
    /** データベース */
    private Program languageDb;

    /**
     * コンストラクタ
     */
    public ModuleTreeModel() {
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
     * モジュールツリーをクリアする。
     */
    public void clearTreeModel() {
        rootNode = new DefaultMutableTreeNode("Module tree");
        treeModel = new DefaultTreeModel(rootNode);
        notifyModel();
    }

    /**
     * ツリールートノードを取得する
     * @return		ツリールートノード
     */
    public DefaultMutableTreeNode getRootNode() {
        return rootNode;
    }

    /**
     * ツリーモデルを取得する
     * @return		ツリーモデル
     */
    public DefaultTreeModel getTreeModel() {
        return treeModel;
    }

    /**
     * ツリー情報をファイル出力する。
     * @param   file   出力ファイル
     */
    public void writeFile(File file) {
        // ルートノード
        if (this.treeModel == null) return;
        DefaultMutableTreeNode root = (DefaultMutableTreeNode) this.treeModel.getRoot();
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


