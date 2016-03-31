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

import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;

import jp.riken.kscope.Message;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 変数有効域モデル
 * @author RIKEN
 *
 */
public class ScopeModel extends Observable {

    /** テーブルヘッダーリスト */
    private String[] HEADER_COLUMNS = {"", Message.getString("mainmenu.analysis.variablescope")}; //変数有効域

    /**
     * ブロック演算カウントテーブル列サイズ
     * -1=非表示とする
     */
    private int[] HEADER_COLUMNS_PREFERREDWIDTH = {-1, 640};

    /** タイトル */
    private String title;

    /**
     * 変数有効域情報
     */
    private List<IBlock> listScope;

    /**
     * コンストラクタ
     */
    public ScopeModel() {
        super();
    }

    /**
     * モデルの変更を通知する
     */
    private void notifyModel() {
        this.setChanged();
        this.notifyObservers();
        this.clearChanged();
    }


    /**
     * 変数有効域デフォルトテーブルモデルを取得する
     * @return        変数有効域デフォルトテーブルモデル
     */
    public DefaultTableModel getScopeDefaultTableModel() {
        // テーブルモデルの作成
        DefaultTableModel tableModel = new DefaultTableModel();
        tableModel.setColumnIdentifiers(HEADER_COLUMNS);
        return tableModel;
    }


    /**
     * 変数有効域データを設定する
     * @param list        変数有効域データ
     */
    public void setScope(IBlock[] list) {
        // テーブルモデルのクリア
        this.listScope = new ArrayList<IBlock>();

        if (list != null) {
            this.listScope.addAll(java.util.Arrays.asList(list));
        }

        // モデルの変更を通知
        notifyModel();
    }


    /**
     * テーブルモデルをクリアする。
     */
    public void clear() {
        // テーブルモデルのクリア
        this.listScope = new ArrayList<IBlock>();
        this.title = null;

        // モデルの変更を通知
        notifyModel();
    }

    /**
     * タイトルを取得する
     * @return    タイトル
     */
    public String getTitle() {
        return title;
    }

    /**
     * タイトルを設定する
     * @param title        タイトル
     */
    public void setTitle(String title) {
        this.title = title;
    }


    /**
     * テーブル情報をファイル出力する。
     * @param file        出力ファイル
     */
    public void writeFile(File file) {

        try {

            // ブロック演算カウント
            if (this.listScope == null || this.listScope.size() <= 0) return;

            // ファイル出力
            PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));

            // テーブルを出力する
            String buf = SwingUtils.toCsv(getTableModel());
            pw.print(buf);

            pw.close();
        } catch (IOException ex) {
            ex.printStackTrace();
        }

    }

    /**
     * テーブルモデルを取得する
     * @return        テーブルモデル
     */
    public DefaultTableModel getTableModel() {
        return createTableModel();
    }


    /**
     * テーブルモデルを作成する
     */
    private DefaultTableModel createTableModel() {
        // テーブルモデルの作成
        DefaultTableModel tableModel = getScopeDefaultTableModel();
        if (this.listScope == null) return tableModel;

        for (IBlock area : this.listScope) {
            // テーブル行配列の作成
            Object[] row = new Object[HEADER_COLUMNS.length];

            // 演算カウントブロック
            int col = 0;
            row[0] = area;
            row[1] = area.toStringModuleScope();
            // テーブル行追加
            tableModel.addRow(row);
        }

        return tableModel;
    }

    /**
     * モデルが空か否か
     * @return    空か否か（true: 空，false: データあり）
     */
    public boolean isEmpty() {
        if (this.listScope == null) return true;
        return (this.listScope.size() < 1);
    }

    /**
     * 変数有効域テーブル列幅を設定する.
     * @param columnModel        テーブル列モデル
     */
    public void setTableColumnWidth(DefaultTableColumnModel columnModel) {
        for (int i=0; i<columnModel.getColumnCount(); i++) {
            // 列取得
            TableColumn column = columnModel.getColumn(i);
            if (HEADER_COLUMNS_PREFERREDWIDTH.length >= i) {
                if (HEADER_COLUMNS_PREFERREDWIDTH[i] >= 0) {
                    column.setPreferredWidth(HEADER_COLUMNS_PREFERREDWIDTH[i]);
                    column.setResizable(true);
                }
                else {
                    column.setMinWidth(0);
                    column.setMaxWidth(0);
                    column.setPreferredWidth(0);
                    column.setResizable(false);
                }
            }
        }
    }
}



