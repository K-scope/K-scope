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
import java.util.List;
import java.util.Observable;

import javax.swing.tree.DefaultMutableTreeNode;

import jp.riken.kscope.Message;
import jp.riken.kscope.component.SearchTreeModel;
import jp.riken.kscope.component.SearchTreeNode;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 検索結果情報モデル
 * @author riken
 *
 */
public class SearchResultModel extends Observable {

    /** トレースツリーモデル */
    private SearchTreeModel treeModel;
    /** タイトル */
    private String title;
    /** 検索結果情報リスト */
    private List<CodeLine> listResult = null;

    /** 検索文字列 */
    private String searchText;
    /** 大文字・小文字の区別(true=大文字・小文字の区別を行う) */
    private boolean sensitivecase;
    /** 正規表現 */
    private boolean regex;
    /** 単語検索 */
    private boolean word;

    /**
     * コンストラクタ
     */
    public SearchResultModel() {
        super();

        // 検索結果のクリア
        clearSearchResult();
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
    public SearchTreeModel getTreeModel() {
        return treeModel;
    }


    /**
     * 検索結果を追加する
     * @param result          検索結果
     */
    public void addSearchResult(CodeLine result) {

        // 検索結果を追加する。
        if (this.listResult == null) {
            this.listResult = new ArrayList<CodeLine>();
        }
        this.listResult.add(result);
        // モデルの変更を通知
        notifyModel();
    }


    /**
     * 検索結果をクリアする。
     */
    public void clearSearchResult() {
        if (this.listResult == null) {
            this.listResult = new ArrayList<CodeLine>();
        }
        this.listResult.clear();

        SearchTreeNode rootNode = new SearchTreeNode(Message.getString("mainmenu.window.analysis.search")); //検索結果
        this.treeModel = new SearchTreeModel(rootNode);

        this.title = null;
        this.searchText = null;

        // モデルの変更を通知
        notifyModel();
    }

    /**
     * 検索結果リストを取得する
     * @return		検索結果リスト
     */
    public List<CodeLine> getSearchResultList() {
        return this.listResult;
    }

    /**
     * 検索結果リスト数を取得する
     * @return		検索結果リスト数
     */
    public int getSearchResultListCount() {
        if (this.listResult == null) return 0;
        return this.listResult.size();
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
     * @param file		出力ファイル
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
     * 大文字・小文字の区別を取得する
     * @return		大文字・小文字の区別
     */
    public boolean isSensitivecase() {
        return sensitivecase;
    }

    /**
     * 大文字・小文字の区別を設定する
     * @param sensitivecase		大文字・小文字の区別
     */
    public void setSensitivecase(boolean sensitivecase) {
        this.sensitivecase = sensitivecase;
    }

    /**
     * 正規表現を取得する
     * @return		正規表現
     */
    public boolean isRegex() {
        return regex;
    }

    /**
     * 正規表現を設定する
     * @param regex		正規表現
     */
    public void setRegex(boolean regex) {
        this.regex = regex;
    }

    /**
     * 単語検索を取得する
     * @return		単語検索
     */
    public boolean isWord() {
        return word;
    }

    /**
     * 単語検索を設定する
     * @param word		単語検索
     */
    public void setWord(boolean word) {
        this.word = word;
    }

    /**
     * 検索文字列を取得する
     * @return		検索文字列
     */
    public String getSearchText() {
        return searchText;
    }

    /**
     * 検索文字列を設定する
     * @param searchText		検索文字列
     */
    public void setSearchText(String searchText) {
        this.searchText = searchText;
    }
    
    /**
     * モデルが空か否か
     * @return		空か否か（true: 空,false: データあり）
     */
    public boolean isEmpty() {
    	DefaultMutableTreeNode root = getRootNode();
    	if (root == null) return true;
    	if (root.getChildCount() < 1) return true;
    	return false;
    }

}


