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

//import javax.swing.JOptionPane;
import javax.swing.table.DefaultTableModel;

import jp.riken.kscope.Message;
import jp.riken.kscope.utils.SwingUtils;

/**
 * プロパティテーブルモデル
 * @author RIKEN
 *
 */
public class PropertiesTableModel extends Observable {

    /** テーブルヘッダーリスト */
    private String[] HEADER_COLUMNS = {Message.getString("propertiestablemodel.header_columns.name"),   //項目
                                       Message.getString("settingprojectdialog.column_header.value")};  //値

    /** タイトル */
    private String title;


    /** プロパティテーブルモデル */
    private DefaultTableModel tableModel;

    /**
     * コンストラクタ
     */
    public PropertiesTableModel() {
        super();

        // テーブルモデルの作成
        createTableModel();
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
     * テーブルモデルを取得する
     * @return		テーブルモデル
     */
    public DefaultTableModel getTableModel() {
        return tableModel;
    }


    /**
     * テーブルモデルを作成する
     */
    private void createTableModel() {
        // テーブルモデルの作成
        tableModel = new DefaultTableModel();
        tableModel.setColumnIdentifiers(HEADER_COLUMNS);
    }

    /**
     * ヘッダー列リストを取得する。
     * @return		ヘッダー列リスト
     */
    public String[] getHeaderColumns() {
        return HEADER_COLUMNS;
    }

    /**
     * テーブル行を追加する
     * @param item		項目名
     * @param value		値
     */
    public void addProperties(String item, String value) {
        String [] row = {item, value};
        tableModel.addRow(row);

        // モデルの変更を通知
        notifyModel();
    }

    /**
     * テーブル行を追加する
     * @param item		項目名
     * @param value		値
     */
    public void setProperties(String item[], String value[]) {
        // テーブルモデルの作成
        createTableModel();
        if (item == null || item.length<= 0) return;

        int count = item.length;
        for (int i=0; i<count; i++) {
            String [] row = {item[i], value[i]};
            tableModel.addRow(row);
        }

        // モデルの変更を通知
        notifyModel();
    }


    /**
     * テーブルモデルをクリアする。
     */
    public void clearProperties() {
        // テーブルモデルの作成
        createTableModel();
        this.title = null;

        // モデルの変更を通知
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
     * @param file		出力ファイル
     */
    public void writeFile(File file) {
        if (this.tableModel == null) return;
        if (isEmpty()) return;

        try {
            // ファイル出力
            PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));

            // テーブルデータ
            String buf = SwingUtils.toCsv(this.tableModel);
            // ファイル出力
            pw.print(buf);

            pw.close();

        } catch (IOException ex) {
            ex.printStackTrace();
        }

    }
    
    /**
     * モデルが空か否か
     * @return	空か否か（ture: 空，false: データあり）
     */
    public boolean isEmpty() {
    	if (this.tableModel == null) return true;
    	return (this.tableModel.getRowCount() < 1);
    }

}


