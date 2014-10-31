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

import jp.riken.kscope.data.OperationCount;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 演算カウントモデル
 * @author RIKEN
 *
 */
public class OperandTableModel extends Observable {

    /**
     * ブロック演算カウントテーブルヘッダーリスト
     * 1列目はBlock情報とする。
     */
    //private String[] HEADER_COLUMNS = {"", "Block", "(Ld+St)/FLOP", "Load(F)", "Store(F)", "FLOP", "add(F)", "mul(F)", "intrinsic(F)"};
	private String[] HEADER_COLUMNS = {"", "Block", "FLOP", "add(F)", "sub(F)", "mul(F)", "div(F)", "intrinsic(F)"};

    /**
     * ブロック演算カウントテーブル列サイズ
     * -1=非表示とする
     */
    private int[] HEADER_COLUMNS_PREFERREDWIDTH = {-1, 160, 80, 80, 80, 80, 80, 80};

    /** タイトル */
    private String title;

    /**
     * ブロック演算カウント情報リスト
     */
    private List<OperandBlock> listOperandBlock;


    /**
     * ブロック演算カウント情報クラス
     * @author RIKEN
     */
    public class OperandBlock {
        /** ブロックノード */
        private IBlock block;
        /** 演算カウント */
        private OperationCount count;

        /**
         * コンストラクタ
         */
        public OperandBlock() {
        }

        /**
         * コンストラクタ
         * @param block			ブロック
         */
        public OperandBlock(IBlock block) {
            this.block = block;
        }


        /**
         * コンストラクタ
         * @param block			ブロック
         * @param count		演算カウント
         */
        public OperandBlock(IBlock block, OperationCount count) {
            this.block = block;
            this.count = count;
        }

        /**
         * ブロックを取得する
         * @return		ブロック
         */
        public IBlock getBlock() {
            return this.block;
        }

        /**
         * 演算カウントを取得する
         * @return		演算カウント
         */
        public OperationCount getCount() {
            return count;
        }

        /**
         * 演算カウントを設定する
         * @param count		演算カウント
         */
        public void setCount(OperationCount count) {
            this.count = count;
        }

        /**
         * ブロックノードを設定する
         * @param block		ブロックノード
         */
        public void setBlock(IBlock block) {
            this.block = block;
        }

    }

    /**
     * コンストラクタ
     */
    public OperandTableModel() {
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
     * ブロック演算カウント情報リスト数を取得する
     * @return		ブロック演算カウント情報リスト数
     */
    public int getListOperandBlockCount() {
        if (this.listOperandBlock == null) {
            return 0;
        }
        return this.listOperandBlock.size();
    }


    /**
     * ブロック演算カウント情報を取得する
     * @param   index    リストインデックス
     * @return		ブロック演算カウント情報
     */
    public OperandBlock getOperandBlock(int index) {
        if (this.listOperandBlock == null) {
            return null;
        }
        if (this.listOperandBlock.size() <= index) return null;

        return this.listOperandBlock.get(index);
    }


    /**
     * ブロック演算カウントテーブルモデルを取得する
     * @return		ブロック演算カウントテーブルモデル
     */
    public DefaultTableModel getBlockDefaultTableModel() {
        // テーブルモデルの作成
        DefaultTableModel tableModel = new DefaultTableModel(HEADER_COLUMNS, 0);
        return tableModel;
    }


    /**
     * ブロック演算カウントテーブル列幅を設定する.<br/>
     * 演算カウントテーブルはすべて固定列幅とする
     * @param columnModel		テーブル列モデル
     */
    public void setTableColumnWidth(DefaultTableColumnModel columnModel) {
        for (int i=0; i<columnModel.getColumnCount(); i++) {
            // 列取得
            TableColumn column = columnModel.getColumn(i);
            if (HEADER_COLUMNS_PREFERREDWIDTH.length >= i) {
                if (HEADER_COLUMNS_PREFERREDWIDTH[i] >= 0) {
                    column.setMinWidth(HEADER_COLUMNS_PREFERREDWIDTH[i]);
                    column.setMaxWidth(HEADER_COLUMNS_PREFERREDWIDTH[i]);
                    column.setPreferredWidth(HEADER_COLUMNS_PREFERREDWIDTH[i]);
                    column.setResizable(false);
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


    /**
     * ブロック演算カウントテーブル行を追加する
     * @param block		ブロック
     * @param count	ブロック演算カウント
     */
    public void addOperandBlock(IBlock block, OperationCount count) {

        if (block == null) return;
        if (count == null) return;

        // ブロック演算カウント情報の生成
        OperandBlock info = new OperandBlock(block, count);

        if (this.listOperandBlock == null) {
            this.listOperandBlock = new ArrayList<OperandBlock>();
        }
        this.listOperandBlock.add(info);

        // モデルの変更を通知
        notifyModel();
    }


    /**
     * テーブルモデルをクリアする。
     */
    public void clearOperand() {
        // テーブルモデルのクリア
        this.listOperandBlock = new ArrayList<OperandBlock>();
        // タイトルのクリア
        title = null;

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

            // ブロック演算カウント
            if (this.listOperandBlock == null || this.listOperandBlock.size() <= 0) return;

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
        DefaultTableModel tableModel = new DefaultTableModel();
        tableModel.setColumnIdentifiers(HEADER_COLUMNS);

        // 演算カウントリストからテーブルモデルの作成を行う。
        if (listOperandBlock == null) return tableModel;

        for (OperandBlock opblock : this.listOperandBlock) {
            // テーブル行配列の作成
            Object[] row = new Object[HEADER_COLUMNS.length];

            // 演算カウントブロック
            int col = 0;
            row[col++] = opblock.getBlock();;
            // ループ名
            row[col++] = opblock.getCount().getName();
            // F
            row[col++] = opblock.getCount().getF();
            // add
            row[col++] = opblock.getCount().getAdd();
            // sub
            row[col++] = opblock.getCount().getSub();
            // mul
            row[col++] = opblock.getCount().getMul();
            // div
            row[col++] = opblock.getCount().getDiv();
            // Intrinsic
            row[col++] = opblock.getCount().getIntrinsic();

            // テーブル行追加
            tableModel.addRow(row);
        }

        return tableModel;
    }

    /**
     * モデルが空か否か
     * @return	空か否か（true: 空，false: データあり）
     */
    public boolean isEmpty() {
    	if (this.listOperandBlock == null) return true;
    	return (this.listOperandBlock.size() < 1);
    }

}



