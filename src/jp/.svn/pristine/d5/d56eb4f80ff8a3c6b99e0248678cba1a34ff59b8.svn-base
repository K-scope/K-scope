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
import jp.riken.kscope.information.InformationBlock;
import jp.riken.kscope.information.ReplacementResult;
import jp.riken.kscope.information.ReplacementResult.RESULT_STATUS;
import jp.riken.kscope.information.ReplacementResults;
import jp.riken.kscope.information.TextInfo;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.Break;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.model.VariableTableModel.ProcedureInfo;
import jp.riken.kscope.model.VariableTableModel.VariableInfo;

/**
 * 付加情報差替結果一覧モデル
 * @author riken
 *
 */
public class ReplacementResultTableModel extends Observable {

    /**
     * テーブルヘッダーリスト(4列).<br/>
     * 1列目はIInformationとする。
     */
    private String[] HEADER_COLUMNS = {"",
        Message.getString("replacementresulttablemodel.header_columns.infotext"), //付加情報概要
        Message.getString("replacementresulttablemodel.header_columns.old"), //(旧)構造情報
        Message.getString("replacementresulttablemodel.header_columns.new"), //(新)構造情報
        ""
        };

    /**
     * テーブル列サイズ
     * -1=非表示とする
     */
    private int[] HEADER_COLUMNS_PREFERREDWIDTH = { -1, 200, 300, 300, -1};
    /**
     * テーブル列最小サイズ.<br/>
     * -1=非表示とする
     */
    private int[] HEADER_COLUMNS_MINWIDTH = {-1, 100, 100, 100, -1 };

    /** タイトル */
    private String title;

    /**
     * 付加情報差替結果リスト
     */
    private List<ReplacementResultBlock> listReplacementResultBlock;
    /** 選択付加情報 */
    private IInformation selectedIInformation;
    /** 付加情報差替結果の表示順番 */
    public final RESULT_STATUS[] STATUSVIEW_ORDER = {RESULT_STATUS.FAILURE, RESULT_STATUS.UNSURE, RESULT_STATUS.SUCCESS};
    /** 選択付加情報差替結果 */
    private ReplacementResult selectedReplacementResult;

    /**
     * 付加情報差替結果クラス
     * @author riken
     */
    public class ReplacementResultBlock {
    	/** 付加情報差替結果 */
    	private RESULT_STATUS status;
    	/** 付加情報差替情報リスト */
        private List<ReplacementResult> listInfo;

        /**
         * コンストラクタ
         * @param status         付加情報差替結果
         */
        public ReplacementResultBlock(RESULT_STATUS status) {
            this.status = status;
            this.listInfo = null;
        }

        /**
         * コンストラクタ
         * @param status         付加情報差替結果
         * @param info          付加情報差替情報リスト
         */
        public ReplacementResultBlock(RESULT_STATUS status, List<ReplacementResult> info) {
            this.status = status;
            this.listInfo = info;
        }

        /**
         * 付加情報差替情報リストを取得する
         * @return      付加情報差替情報リスト
         */
        public List<ReplacementResult> getListReplacementResult() {
            return this.listInfo;
        }

        /**
         * 付加情報差替情報リストに追加する
         * @param info      付加情報差替情報
         */
        public void addReplacementResult(ReplacementResult info) {
            if (listInfo == null) {
                listInfo = new ArrayList<ReplacementResult>();
            }
            this.listInfo.add(info);
        }

        /**
         * テーブルモデルを取得する
         * @return      テーブルモデル
         */
        public DefaultTableModel getTableModel() {
            // テーブルモデルの作成
            DefaultTableModel tableModel = getDefaultTableModel();
            if (listInfo == null) return tableModel;

            for (ReplacementResult info : listInfo) {
                Object[] rows = new Object[tableModel.getColumnCount()];
                // 1列目はIInformation：非表示
                if (info.getCurrentInformation() != null) {
                	rows[0] = info.getCurrentInformation();
                }
                else {
                	rows[0] = info.getOldStartPosition();
                }
                if (info.getInformation() != null) {
                	rows[1] = info.getInformation().getAbstract();
                }
                if (info.getOldStartPosition() != null) {
                	rows[2] = toInformationString(info.getOldStartPosition(), info.getOldEndPosition());
                }
                else {
                	rows[2] = "-";
                }
                if (info.getCurrentStartPosition() != null) {
                	rows[3] = toInformationString(info.getCurrentStartPosition(), info.getCurrentEndPosition());
                }
                else {
                	rows[3] = "-";
                }
                rows[4] = info;
                tableModel.addRow(rows);
            }

            return tableModel;
        }

		/**
		 * 付加情報差替結果を取得する.
		 * @return status		付加情報差替結果
		 */
		public RESULT_STATUS getStatus() {
			return this.status;
		}

		/**
		 * 付加情報差替結果メッセージを取得する.
		 * @return		付加情報差替結果メッセージ
		 */
		public String getStatusText() {
			if (this.status == RESULT_STATUS.FAILURE) {
				return Message.getString("replacementresulttablemodel.status.failure"); //対応不明
			}
			else if (this.status == RESULT_STATUS.SUCCESS) {
				return Message.getString("replacementresulttablemodel.status.sucsess"); //差替実行
			}
			else if (this.status == RESULT_STATUS.UNSURE) {
				return Message.getString("replacementresulttablemodel.status.unsure"); //差替不確定
			}
			return null;
		}

		/**
		 * 付加情報差替情報リストサイズを取得する.
		 * @return		付加情報差替情報リストサイズ
		 */
		public int size() {
			if (this.listInfo == null) return 0;
			return this.listInfo.size();
		}
    }

    /**
     * コンストラクタ
     */
    public ReplacementResultTableModel() {
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
     * プロシージャ結果リスト数を取得する
     * @return      プロシージャ結果リスト数
     */
    public int getReplacementResultCount() {
        if (this.listReplacementResultBlock == null) return 0;
        if (this.listReplacementResultBlock.size() <= 0) return 0;
        int count = 0;
        for (ReplacementResultBlock info: this.listReplacementResultBlock) {
            if (info != null && info.listInfo != null && info.listInfo.size() > 0) {
            	count += info.listInfo.size();
            }
        }
        return count;
    }


    /**
     * プロシージャ結果を取得する
     * @param   index    リストインデックス
     * @return      プロシージャ結果
     */
    public ReplacementResultBlock getReplacementResultBlock(int index) {
        if (this.listReplacementResultBlock == null) {
            return null;
        }
        if (this.listReplacementResultBlock.size() <= index) return null;

        return this.listReplacementResultBlock.get(index);
    }

    /**
     * テーブルモデルを取得する
     * @return      テーブルモデル
     */
    public DefaultTableModel getDefaultTableModel() {
        // テーブルモデルの作成
        DefaultTableModel tableModel = new DefaultTableModel(HEADER_COLUMNS, 0);
        return tableModel;
    }


    /**
     * 列幅を設定する
     * @param columnModel       テーブル列モデル
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
     * @return      ヘッダー列リスト
     */
    public String[] getHeaderColumns() {
        return HEADER_COLUMNS;
    }

    /**
     * ヘッダー列幅リストを取得する。
     * @return      ヘッダー列幅
     */
    public int[] getHeaderColumnsWidth() {
        return HEADER_COLUMNS_PREFERREDWIDTH;
    }

    /**
     * テーブルモデルをクリアする。
     */
    public void clearVariable() {
        // テーブルモデルのクリア
        if (this.listReplacementResultBlock != null) {
            this.listReplacementResultBlock = new ArrayList<ReplacementResultBlock>();
        }
        // タイトルのクリア
        this.title = null;

        // モデルの変更を通知
        notifyModel();
    }

    /**
     * タイトルを取得する
     * @return  タイトル
     */
    public String getTitle() {
        return title;
    }

    /**
     * タイトルを設定する
     * @param title     タイトル
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

            if (this.listReplacementResultBlock != null) {

            	for (RESULT_STATUS status : STATUSVIEW_ORDER) {
            		ReplacementResultBlock info = getReplacementResultBlock(status);
            		if (info == null || info.size() <= 0) continue;
            		// 差替結果
                    pw.println(info.getStatusText());
                    // ヘッダー列を出力する(1列目は非出力)
                    for (int i=1; i<HEADER_COLUMNS.length; i++) {
                        pw.print(HEADER_COLUMNS[i]);
                        if (i<HEADER_COLUMNS.length-1) pw.print(",");
                    }
                    pw.println();

                    List<ReplacementResult> list = info.getListReplacementResult();
                    if (list == null) continue;
                    for (ReplacementResult var : list) {
                        pw.print(var.getInformation().getAbstract());
                        pw.print(",");
                        if (var.getOldStartPosition() == null) {
                            pw.print("-");
                        } else {
	                        String oldBlock = toInformationString(var.getOldStartPosition(), var.getOldEndPosition());
	                        pw.print(oldBlock);
                        }
                        pw.print(",");
                        if (var.getCurrentStartPosition() == null) {
                            pw.print("-");
                        } else {
                            String newBlock = toInformationString(var.getCurrentStartPosition(), var.getCurrentEndPosition());
                            pw.print(newBlock);
                        }
                        pw.println();
                    }
                    pw.println();
                }
            }
            pw.close();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }


    /**
     * 選択付加情報を設定する
     * @param   variable  選択付加情報
     */
    public void setSelectedIInformation(IInformation info) {
        this.selectedIInformation = info;
    }

    /**
     * 選択付加情報を取得する
     * @return      選択付加情報
     */
    public IInformation getSelectedIInformation() {
        return this.selectedIInformation;
    }

    /**
     * 差替結果リストを追加する。
     * @param results 結果リスト
     */
    public void addReplacementResultBlock(ReplacementResults results) {
    	if (results == null) {
    		clearReplacement();
    		this.selectedIInformation = null;
    	}
    	else {
	        for (ReplacementResult res: results) {
	            // プロシージャ差替結果情報の検索
	            ReplacementResultBlock procInfo = getReplacementResultBlock(res.getStatus());
	            procInfo.addReplacementResult(res);
	        }
    	}

        // モデルの変更を通知
        notifyModel();
    }

    /**
     * 付加情報差替結果ブロックを取得する.
     * @param status		差替結果
     * @return		付加情報差替結果ブロック
     */
    public ReplacementResultBlock getReplacementResultBlock(RESULT_STATUS status) {
        if (this.listReplacementResultBlock == null) {
            this.listReplacementResultBlock = new ArrayList<ReplacementResultBlock>();
        }

        for (ReplacementResultBlock info: this.listReplacementResultBlock) {
            if (info.getStatus() == status) {
            	return info;
            }
        }

        // 新たに付加情報差替結果ブロックの作成
        ReplacementResultBlock block = new ReplacementResultBlock(status);
        this.listReplacementResultBlock.add(block);
        return block;
	}

    /**
     * テーブルモデルをクリアする。
     */
    public void clearReplacement() {
        this.listReplacementResultBlock = new ArrayList<ReplacementResultBlock>();
        notifyModel();
    }

    /**
     * モデルが空か否か
     * @return	空か否か（true: 空，false: でーたあり）
     */
    public boolean isEmpty() {
    	if (this.listReplacementResultBlock == null) return true;
    	return (this.listReplacementResultBlock.size() < 1);
    }

    /**
     * IInformationの表示文字列を取得する.
     * @param startinfo		IInformation開始行情報
     * @param endinfo		IInformation終了行情報
     * @return			文字列表現
     */
    private String toInformationString(IInformation startinfo, IInformation endinfo) {
    	if (startinfo == null) return null;
    	String msg = startinfo.getNamespace() + "[" + startinfo.getStartPos() + "]::";
    	if (startinfo == endinfo || endinfo == null) {
	    	msg += startinfo.toString();
    	}
    	else {
    		InformationBlock block = new InformationBlock(null, startinfo, endinfo);
    		msg += block.toString();
    	}
    	return msg;
    }

	/**
	 * 選択付加情報差替結果を取得する
	 * @return selectedReplacementResult
	 */
	public ReplacementResult getSelectedReplacementResult() {
		return selectedReplacementResult;
	}

	/**
	 * @param selectedReplacementResult セットする selectedReplacementResult
	 */
	public void setSelectedReplacementResult(ReplacementResult selected) {
		this.selectedReplacementResult = selected;
	}
}
