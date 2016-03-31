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
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 変数特性情報一覧モデル
 * @author RIKEN
 *
 */
public class VariableTableModel extends Observable {

    /**
     * テーブルヘッダーリスト(15列).<br/>
     * 1列目はVariableDefinition情報とする。
     */
    private String[] HEADER_COLUMNS = {"", "type", "name", "data type", "access specifier", "parameter", "init value",
    		"size", "intent", "optional", "pointer/target", "save", "common", "allocatable",
    		Message.getString("mainmenu.window.analysis.information")}; // 付加情報

    /**
     * テーブル列サイズ
     * -1=非表示とする
     */
    private int[] HEADER_COLUMNS_PREFERREDWIDTH = { -1, 40, 120, 80, 120, 80,
            80, 80, 80, 80, 60, 60, 80, 80, 160 };
    /**
     * テーブル列最小サイズ.<br/>
     * -1=非表示とする
     */
    private int[] HEADER_COLUMNS_MINWIDTH = {-1, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80};

    /** タイトル */
    private String title;

    /**
     * プロシージャ変数特性情報リスト
     */
    private List<ProcedureInfo> listProcedureInfo;
    /** 選択変数宣言文 */
    private VariableDefinition selectedVariable;


    /**
     * プロシージャ変数特性情報クラス
     * @author RIKEN
     */
    public class ProcedureInfo {
        private IBlock block;
        private List<VariableInfo> listInfo;

        /**
         * コンストラクタ
         * @param block			ブロック
         */
        public ProcedureInfo(IBlock block) {
            this.block = block;
            listInfo = null;
        }

        /**
         * コンストラクタ
         * @param block			ブロック
         * @param info			変数特性情報リスト
         */
        public ProcedureInfo(IBlock block, List<VariableInfo> info) {
            this.block = block;
            this.listInfo = info;
        }

        /**
         * ブロックを取得する
         * @return		ブロック
         */
        public IBlock getBlock() {
            return this.block;
        }

        /**
         * 変数特性情報リストを取得する
         * @return		変数特性情報リスト
         */
        public List<VariableInfo> getListInfo() {
            return this.listInfo;
        }

        /**
         * 変数特性情報リストに追加する
         * @param info		変数特性情報
         */
        public void addVariableInfo(VariableInfo info) {
            if (info == null) return;
            if (listInfo == null) {
                listInfo = new ArrayList<VariableInfo>();
            }
            VariableDefinition var = info.getVariable();
            if (var == null) return;
            for (VariableInfo in : this.listInfo) {
                VariableDefinition in_var = in.getVariable();
                if (var == in_var) {
                    return;
                }
            }
            this.listInfo.add(info);
        }

        /**
         * テーブルモデルを取得する
         * @return		テーブルモデル
         */
        public DefaultTableModel getTableModel() {
            // テーブルモデルの作成
            DefaultTableModel tableModel = getDefaultTableModel();
            if (listInfo == null) return tableModel;

            for (VariableInfo info : listInfo) {
                Object[] rows = new Object[tableModel.getColumnCount()];
                // 1列目はVariableDefinition：非表示
                rows[0] = info.getVariable();
                String[] attrs = info.getAttributes();
                for (int i=0;i<attrs.length; i++) {
                    if (rows.length <= i+1) break;
                    rows[i+1] = attrs[i];
                }
                tableModel.addRow(rows);
            }

            return tableModel;
        }
    }

    /**
     * 変数特性情報クラス
     * @author RIKEN
     */
    public class VariableInfo {
        /** 変数宣言文・構造体クラス */
        private VariableDefinition variable;
        /** 変数特性情報 */
        private String[] attributes;

        /**
         * コンストラクタ
         * @param variable			変数宣言文・構造体クラス
         * @param attributes		変数特性情報
         */
        public VariableInfo(VariableDefinition variable, String[] attributes) {
            this.variable = variable;
            this.attributes = attributes;
        }

        /**
         * 変数宣言文・構造体クラスを取得する
         * @return		変数宣言文・構造体クラス
         */
        public VariableDefinition getVariable() {
            return this.variable;
        }

        /**
         * 変数特性情報を取得する
         * @return		変数特性情報
         */
        public String[] getAttributes() {
            return this.attributes;
        }
    }

    /**
     * コンストラクタ
     */
    public VariableTableModel() {
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
     * プロシージャ変数特性情報リスト数を取得する
     * @return		プロシージャ変数特性情報リスト数
     */
    public int getListProcedureInfoCount() {
        if (this.listProcedureInfo == null) {
            return 0;
        }
        return this.listProcedureInfo.size();
    }


    /**
     * プロシージャ変数特性情報を取得する
     * @param   index    リストインデックス
     * @return		プロシージャ変数特性情報
     */
    public ProcedureInfo getProcedureInfo(int index) {
        if (this.listProcedureInfo == null) {
            return null;
        }
        if (this.listProcedureInfo.size() <= index) return null;

        return this.listProcedureInfo.get(index);
    }

    /**
     * テーブルモデルを取得する
     * @return		テーブルモデル
     */
    public DefaultTableModel getDefaultTableModel() {
        // テーブルモデルの作成
        DefaultTableModel tableModel = new DefaultTableModel(HEADER_COLUMNS, 0);
//        tableModel.setColumnIdentifiers(HEADER_COLUMNS);
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
     * ヘッダー列リストを取得する。
     * @return		ヘッダー列リスト
     */
    public String[] getHeaderColumns() {
        return HEADER_COLUMNS;
    }

    /**
     * ヘッダー列幅リストを取得する。
     * @return		ヘッダー列幅
     */
    public int[] getHeaderColumnsWidth() {
        return HEADER_COLUMNS_PREFERREDWIDTH;
    }

    /**
     * テーブル行を追加する
     * @param block			プロシージャブロック
     * @param variable		変数宣言クラス
     * @param infos		変数特性情報
     */
    public void addVariableInfo(IBlock block, VariableDefinition variable, String[] infos) {

        if (infos == null) return;

        // 変数特性情報クラスの生成
        VariableInfo info = new VariableInfo(variable, infos);

        // プロシージャ変数特性情報の検索
        ProcedureInfo procInfo = getProcedureInfo(block);
        if (procInfo == null) {
            procInfo = new ProcedureInfo(block);
            procInfo.addVariableInfo(info);
            if (this.listProcedureInfo == null) {
                this.listProcedureInfo = new ArrayList<ProcedureInfo>();
            }
            this.listProcedureInfo.add(procInfo);
        }
        else {
            procInfo.addVariableInfo(info);
        }

        // モデルの変更を通知
        notifyModel();
    }

    /**
     * プロシージャ変数特性情報をキー文字列にて検索する
     * @param block		プロシージャ変数特性情報ブロック
     * @return			プロシージャ変数特性情報
     */
    private ProcedureInfo getProcedureInfo(IBlock block) {
        if (block == null) return null;
        if (this.listProcedureInfo == null) return null;

        for (ProcedureInfo info : this.listProcedureInfo) {
            IBlock infoblock = info.getBlock();
            if (block == infoblock) {
                return info;
            }
        }

        return null;
    }


    /**
     * テーブルモデルをクリアする。
     */
    public void clearVariable() {
        // テーブルモデルのクリア
        if (this.listProcedureInfo != null) {
            this.listProcedureInfo = new ArrayList<ProcedureInfo>();
        }
        // タイトルのクリア
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
     * @param   file   出力ファイル
     */
    public void writeFile(File file) {

        try {
            // ファイル出力
            PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));

            if (this.listProcedureInfo != null) {
                for (ProcedureInfo info : this.listProcedureInfo) {
                    IBlock block = info.getBlock();
                    if (block != null) {
                        pw.println(block.toString());
                    }
                    // テーブルを出力する
                    String buf = SwingUtils.toCsv(info.getTableModel());
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
     * 選択変数宣言文を設定する
     * @param 	variable        選択変数宣言文
     */
    public void setSelectedVariable(VariableDefinition variable) {
        this.selectedVariable = variable;
    }

    /**
     * 選択変数宣言文を取得する
     * @return		選択変数宣言文
     */
    public VariableDefinition getSelectedVariable() {
        return this.selectedVariable;
    }

    /**
     * モデルが空かどうかを返す
     * @return	空か否か(true: 空，false: データあり)
     */
    public boolean isEmpty() {
    	if (this.listProcedureInfo == null) return true;
    	return (this.listProcedureInfo.size() < 1);
    }

}


