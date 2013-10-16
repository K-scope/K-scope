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
import java.util.List;
import java.util.Observable;

import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.profiler.ProfilerMeasureInfo;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 測定タイマ情報テーブルモデル
 * @author riken
 *
 */
public class ProfilerMeasureModel extends Observable {

    /** テーブルヘッダーリスト */
    private String[] HEADER_COLUMNS = {"",
    		Message.getString("eprofstatementdialog.dialog.desc"), //測定区間設定
    		Message.getString("profilercosttablemodel.header_columns.filename"), //ファイル名
    		Message.getString("profilercosttablemodel.header_columns.linenumber")}; //行番号

    /**
     * テーブル列サイズ
     * -1=非表示とする
     */
    private int[] HEADER_COLUMNS_PREFERREDWIDTH = { -1, 240, 300, 80 };
    /**
     * テーブル列最小サイズ.<br/>
     * -1=非表示とする
     */
    private int[] HEADER_COLUMNS_MINWIDTH = {-1, 80, 80, 40};

    /** タイトル */
    private String title;

    /** 測定区間情報 */
    private ProfilerMeasureInfo measureInfo;

    /**
     * コンストラクタ
     */
    public ProfilerMeasureModel() {
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
     * @return		変数有効域デフォルトテーブルモデル
     */
    public DefaultTableModel getScopeDefaultTableModel() {
        // テーブルモデルの作成
        // テーブルモデルの作成
        DefaultTableModel tableModel = new DefaultTableModel();
        tableModel.setColumnIdentifiers(HEADER_COLUMNS);
        return tableModel;
    }


    /**
     * 測定区間情報を設定する
     * @param info   測定区間情報
     */
    public void setMeasureInfo(ProfilerMeasureInfo info) {
        this.measureInfo = info;

        // モデルの変更を通知
        notifyModel();
    }


    /**
     * 測定区間情報を取得する
     * @return  測定区間情報
     */
    public ProfilerMeasureInfo getMeasureInfo() {
        return this.measureInfo;
    }

    /**
     * テーブルモデルをクリアする。
     */
    public void clearModel() {
        // テーブルモデルのクリア
        if (this.measureInfo != null) {
            this.measureInfo.clearMeasureInfo();
        }
        this.measureInfo = null;
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

        try {

            // タイマ情報
            if (this.measureInfo == null) return;

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
     * @return		テーブルモデル
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
        if (this.measureInfo == null) return tableModel;

        // タイマデータの取得
        List<ProfilerMeasureInfo.MeasureData> list = this.measureInfo.getMeasureList();
        if (list == null || list.size() <= 0) return tableModel;
        for (ProfilerMeasureInfo.MeasureData data : list) {
            // テーブル行配列の作成
            Object[] cols = new Object[HEADER_COLUMNS.length];
            cols[0] = data;
            cols[1] = data.toStringParam();
            CodeLine code = data.getMeasureArea();
            if (code != null) {
                cols[2] = code.getSourceFile().getPath();
                cols[3] = code.getLineno();
            }
            // テーブル行追加
            tableModel.addRow(cols);
        }

        return tableModel;
    }

    /**
     * 列幅を設定する
     * @param columnModel		テーブル列モデル
     */
    public void setTableColumnWidth(DefaultTableColumnModel columnModel) {
        for (int i=0; i<columnModel.getColumnCount(); i++) {
            // 列取得
            TableColumn column = columnModel.getColumn(i);
            if (HEADER_COLUMNS_PREFERREDWIDTH.length >= i) {
                if (HEADER_COLUMNS_PREFERREDWIDTH[i] >= 0) {
                    column.setPreferredWidth(HEADER_COLUMNS_PREFERREDWIDTH[i]);
                }
                else {
                    column.setMinWidth(0);
                    column.setMaxWidth(0);
                    column.setPreferredWidth(0);
                    column.setResizable(false);
                }
            }
            if (HEADER_COLUMNS_MINWIDTH.length >= i) {
                if (HEADER_COLUMNS_MINWIDTH[i] >= 0) {
                    column.setMinWidth(HEADER_COLUMNS_MINWIDTH[i]);
                }
            }
        }
    }

    /**
     * 測定区間データの削除
     * @param data		削除測定区間データ
     */
    public void removeMeasureData(ProfilerMeasureInfo.MeasureData data) {
    	if (data == null) return;
        if (this.measureInfo == null) return;
        this.measureInfo.removeMeasureData(data);

        // モデルの変更を通知
        notifyModel();
    }

    /**
     * モデルが空か否か
     * @return	空か否か（true: 空，false: データあり）
     */
    public boolean isEmpty() {
    	if (this.measureInfo == null) return true;
    	return (this.measureInfo.getMeasureDataCount() < 1);
    }
}



