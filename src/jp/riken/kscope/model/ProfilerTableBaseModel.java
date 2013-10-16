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

import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;

import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.gui.ISourceBargraph;
import jp.riken.kscope.profiler.ProfilerBaseData;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.utils.SwingUtils;

/**
 * プロファイラ:情報基底モデル
 * @author riken
 */
public abstract class ProfilerTableBaseModel extends Observable {

    /** プロファイラ情報タイプ */
    private PROFILERINFO_TYPE enumInfo;
    /** プロファイラプロパティ */
    private ProfilerProperties properties;

    /**
     * コンストラクタ
     * @param type		プロファイラ情報タイプ
     */
    public ProfilerTableBaseModel(PROFILERINFO_TYPE type) {
        this.enumInfo = type;
    }

    /**
     * プロファイラ情報タイプを取得する
     * @return		プロファイラ情報タイプ
     */
    public PROFILERINFO_TYPE getEnumInfo() {
        return enumInfo;
    }

    /**
     * プロファイラ情報タイプを設定する
     * @param type		プロファイラ情報タイプ
     */
    public void setEnumInfo(PROFILERINFO_TYPE type) {
        this.enumInfo = type;
    }

    /**
     * テーブルモデルをクリアする。
     */
    public abstract void clearModel();


    /**
     * 選択プロファイル情報を設定する
     * @param 	info        選択プロファイル情報
     */
    public abstract void setSelectedInfo(ProfilerBaseData info);

    /**
     * 選択プロファイル情報を取得する
     * @return		選択プロファイル情報
     */
    public abstract ProfilerBaseData getSelectedInfo();

    /**
     * タイトルを取得する
     * @return	タイトル
     */
    public abstract String getTitle();

    /**
     * タイトルを設定する
     * @param title		タイトル
     */
    public abstract void setTitle(String title);

    /**
     * プロファイル情報マップ数を取得する
     * @return		プロファイル情報マップ数
     */
    public abstract int getInfoMapCount();


    /**
     * プロファイル情報マップキー名を取得する
     * @param   index    マップインデックス
     * @return		コスト情報マップキー名
     */
    public abstract String getInfoMapKey(int index);

    /**
     * プロファイル情報サブタイトルを取得する
     * @param   index    マップインデックス
     * @return		サブタイトル
     */
    public abstract String getSubTitle(int index);

    /**
     * テーブルモデルを取得する
     * @param index		コスト情報マップインデックス
     * @return		テーブルモデル
     */
    public abstract DefaultTableModel getInfoTableModel(int index);


    /**
     * ヘッダー推奨列幅リストを取得する。
     * @return		ヘッダー推奨列幅
     */
    protected abstract int[] getHeaderColumnsPreferredWidth();

    /**
     * ヘッダー最小列幅リストを取得する。
     * @return		ヘッダー最小列幅
     */
    protected abstract int[] getHeaderColumnsMinWidth();

    /**
     * プロファイラデータを設定する
     * @param key		プロファイラデータキー
     * @param infos		プロファイラデータ
     */
    public abstract void setProfilerData(String key, ProfilerBaseData[] infos);

    /**
     * プロファイラバーグラフデータを取得する
     * @return   プロファイラバーグラフデータ
     */
    public abstract ISourceBargraph[] getSelectedBargraph();

    /**
     * 列幅を設定する
     * @param columnModel		テーブル列モデル
     */
    public void setTableColumnWidth(DefaultTableColumnModel columnModel) {
        boolean[] visibled = getVisibledColumns();
        int[] preferredWidth = getHeaderColumnsPreferredWidth();
        int[] minWidth = getHeaderColumnsMinWidth();
        for (int i=0; i<columnModel.getColumnCount(); i++) {
            // 列取得
            TableColumn column = columnModel.getColumn(i);
            if (preferredWidth.length >= i) {
                if (preferredWidth[i] >= 0 && visibled[i]) {
                    column.setPreferredWidth(preferredWidth[i]);
                }
                else {
                    column.setMinWidth(0);
                    column.setMaxWidth(0);
                    column.setPreferredWidth(0);
                    column.setResizable(false);
                }
            }
            if (minWidth.length > i) {
                if (minWidth[i] >= 0) {
                    column.setMinWidth(minWidth[i]);
                }
            }
        }
    }

    /**
     * テーブル情報をファイル出力する。
     * @param   file   出力ファイル
     */
    public void writeFile(File file) {

        try {
            boolean[] visibled = getVisibledColumns();
            // ファイル出力
            PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));
            int mapsize = getInfoMapCount();
            if (mapsize > 0) {
                for(int i=0; i<mapsize; i++) {
                    // キー文字列
                    String key = getSubTitle(i);
                    pw.println(key);

                    // テーブルモデルの取得
                    DefaultTableModel table = this.getInfoTableModel(i);

                    // テーブルを出力する
                    String buf = SwingUtils.toCsv(table, visibled);
                    pw.print(buf);
                    pw.println();
                }
            }
            pw.close();
        } catch (IOException ex) {
            ex.printStackTrace();
        }

    }

    /**
     * プロファイラプロパティを設定する
     * @param properties		プロファイラプロパティ
     */
    public void setProfilerProperties(ProfilerProperties properties) {
        this.properties = properties;
        // モデルの変更を通知する
        notifyModel();
    }

    /**
     * プロファイラプロパティを取得する
     * @return properties		プロファイラプロパティ
     */
    public ProfilerProperties getProfilerProperties() {
        return this.properties;
    }

    /**
     * 選択プロファイルデータのテキストデータを取得する
     * @return		選択テキストデータ
     */
    public abstract String getSelectedText();

    /**
     * ヘッダー列リストを取得する。
     * @return		ヘッダー列リスト
     */
    public abstract String[] getHeaderColumns();

    /**
     * ヘッダー列の表示状態を取得する
     * @return		ヘッダー列表示状態リスト
     */
    public abstract boolean[] getVisibledColumns();

    /**
     * ヘッダー列の表示状態を設定する
     * @param col		ヘッダー列番号
     * @param checked   表示状態
     */
    public abstract void setVisibledColumns(int col, boolean checked);

    /**
     * モデルの変更を通知する
     */
    protected abstract void notifyModel();

    /**
     * 列配置を取得する
     * @return   列配置
     */
    public abstract int[] getTableColumnAlignments();
    
    /**
     * モデルが空か否か
     * @return	空か否か（true: 空，false: データあり）
     */
    public boolean isEmpty() {
    	return (getInfoMapCount() < 1);
    }
    
    /**
     * Viewがソート状態か否かを返す
     * @param sort
     */
    public void setViewSort(boolean sort) {
    	return;
    }
}
