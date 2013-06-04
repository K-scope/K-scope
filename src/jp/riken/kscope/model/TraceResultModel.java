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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Observable;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;

import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.utils.SwingUtils;

/**
 * トレース結果モデル
 * @author riken
 *
 */
public class TraceResultModel extends Observable {

    /** トレースツリーモデル */
    private DefaultTreeModel treeModel;
    /** タイトル */
    private String title;
    /** トレース対象変数名 */
    private String traceWord;
    /** ブロック選択表示ラベル */
    private String blocklabel;
    /** トレース選択ブロック */
    private IBlock selectedBlock;
    /** トレースパス */
    private IBlock[] tracePath;

    /**
     * コンストラクタ
     */
    public TraceResultModel() {
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
     * トレースツリーをクリアする。
     */
    public void clearTreeModel() {
        this.treeModel = null;
        this.title = null;
        this.traceWord = null;
        this.blocklabel = null;
        this.selectedBlock = null;
        this.tracePath = null;

        notifyModel();
    }

    /**
     * トレースルートブロックを取得する.
     * @return		トレースルートブロック
     */
    public IBlock getRootBlock() {
        DefaultMutableTreeNode node = getRootNode();
        if (node == null) return null;
        if (node.getUserObject() instanceof IBlock) {
            return (IBlock)node.getUserObject();
        }
        return null;
    }

    /**
     * ルートノードを取得する
     * @return		ルートノード
     */
    public DefaultMutableTreeNode getRootNode() {
        if (treeModel == null) return null;
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
     * タイトルを取得する
     * @return	タイトル
     */
    public String getTitle() {
        // return title;
        if (this.traceWord == null || this.traceWord.length() <= 0) {
            return title;
        }
        if (this.tracePath == null || this.tracePath.length <= 0) {
            return title;
        }
        StringBuffer buf = new StringBuffer();
        for (IBlock block : this.tracePath) {
            if (block == null) continue;
            if (block instanceof Procedure) {
                buf.append(((Procedure)block).get_name());
                buf.append("->");
            }
        }
        buf.append(this.traceWord);

        return buf.toString();
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
     * ツリーモデルを設定する
     * @param tree		ツリーモデル
     */
    public void setTreeModel(DefaultTreeModel tree) {
        this.treeModel = tree;

        // ツリーの更新
        notifyModel();
    }

    /**
     * トレース対象変数名を取得する.
     * @return		トレース対象変数名
     */
    public String getTraceWord() {
        return traceWord;
    }

    /**
     * トレース対象変数名を設定する.
     * @param word		トレース対象変数名
     */
    public void setTraceWord(String word) {
        this.traceWord = word;
    }

    /**
     * ブロック選択表示ラベルを取得する.
     * @return		ブロック選択表示ラベル
     */
    public String getBlocklabel() {
        return blocklabel;
    }

    /**
     * ブロック選択表示ラベルを設定する.
     * @param blocklabel		ブロック選択表示ラベル
     */
    public void setBlocklabel(String blocklabel) {
        this.blocklabel = blocklabel;
    }

    /**
     * トレース選択ブロックを取得する
     * @return		トレース選択ブロック
     */
    public IBlock getSelectedBlock() {
        return selectedBlock;
    }

    /**
     * トレース選択ブロックを設定する
     * @param selectedBlock		トレース選択ブロック
     */
    public void setSelectedBlock(IBlock selectedBlock) {
        this.selectedBlock = selectedBlock;
    }

    /**
     * 同一トレースであるかチェックする.<br/>
     * @param model		比較対象トレースモデル
     * @return			true=同一トレース結果
     */
    public boolean equalsTrace(TraceResultModel model) {
        // トレース結果が存在しない場合は、不一致
        if (this.traceWord ==  null) return false;
        if (this.treeModel == null) return false;
        if (this.getRootBlock() == null) return false;

        // トレース変数のチェック
        if (!this.traceWord.equals(model.getTraceWord())) {
            return false;
        }

        // トレース結果ルートノードが一致していること
        if (this.getRootBlock() != model.getRootBlock()) {
            return false;
        }

        return true;
    }


    /**
     * トレースパスを取得する
     * @return 		トレースパス
     */
    public IBlock[] getTracePath() {
        return tracePath;
    }


    /**
     * トレースパスを設定する
     * @param tracePath 		トレースパス
     */
    public void setTracePath(IBlock[] tracePath) {
        this.tracePath = tracePath;
    }

    /**
     * トレースパスを追加する
     * @param block		追加ブロック
     */
    public void addTracePath(IBlock block) {
        if (block == null) return;
        List<IBlock> list = new ArrayList<IBlock>();
        if (this.tracePath != null && this.tracePath.length > 0) {
            list.addAll(Arrays.asList(this.tracePath));
        }
        list.add(block);

        this.tracePath = list.toArray(new IBlock[0]);
    }

    /**
     * トレースパスを追加する
     * @param blocks		追加ブロックリスト
     */
    public void addTracePaths(IBlock[] blocks) {
        if (blocks == null || blocks.length <= 0) return;
        List<IBlock> list = new ArrayList<IBlock>();
        if (this.tracePath != null && this.tracePath.length > 0) {
            list.addAll(Arrays.asList(this.tracePath));
        }
        list.addAll(Arrays.asList(blocks));

        this.tracePath = list.toArray(new IBlock[0]);
    }


    /**
     * モデルが空か否か
     * @return	空か否か（ture: 空，false: データあり）
     */
    public boolean isEmpty() {
    	DefaultMutableTreeNode root = getRootNode();
    	if (root == null) return true;
    	return (root.getChildCount() < 1);
    }
}


