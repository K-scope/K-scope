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
import java.util.Enumeration;
import java.util.List;
import java.util.Observable;

import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import javax.swing.tree.DefaultMutableTreeNode;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.RequiredByteFlopResult;
import jp.riken.kscope.information.TextInfo;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.properties.MemorybandProperties.UNIT_TYPE;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 要求Byte/FLOPテーブルモデル
 * @author riken
 *
 */
public class RequiredByteFlopModel extends Observable {

    /** タイトル */
    private String title;
    /** 要求Byte/FLOP算出結果リスト:追加毎にグループ化する. */
    private List<List<RequiredByteFlopResult>> listResults;
    /** 算出単位 */
    private UNIT_TYPE unitType;
    /** 構造ツリーモデル. */
    private LanguageTreeModel modelLanguageTree = null;

    /**
     * 要求Byte/FLOPテーブルヘッダーリスト : 29列
     * 1列目はBlock情報とする。
     */
    private String[] HEADER_COLUMNS = {"", "Block", "Load(B)", "Store(B)", "FLOP",
                                       "Required B/F", "Throughput(GB/s)", "Effective B/F", "Peak Ratio(%)", "Memory Variable",
                                       "L1 Variable", "L2 Variable", "Register Variable", "Custom Variable",
                                       "add(F)", "sub(F)", "mul(F)", "div(F)",
                                       "intrinsic(F)", "Performance(GFLOPS)", "Store Mode", "Memory Band Width(GB/s)",
                                       "L1 Band Width(GB/s)", "L2 Band Width(GB/s)", "Register Band Width(GB/s)", "Custom Band Width(GB/s)", "Memory Coef",
                                       "L1 Coef", "L2 Coef", "Register Coef", "Custom Coef",
                                       Message.getString("mainmenu.window.analysis.information") // 付加情報
                                       };
    /**
     * ブロック演算カウントテーブル列サイズ : 31列
     * -1=非表示とする
     */
    private int[] HEADER_COLUMNS_PREFERREDWIDTH = {
                                       -1, 240, 100, 100, 100,
                                       120, 120, 120, 120, 120,
                                       120, 120, 120, 120,
                                       120, 120, 120, 120,
                                       120, 160, 120, 160,
                                       160, 160, 160, 160, 100,
                                       100, 100, 100, 100, 240};

    /**
     * コンストラクタ
     */
    public RequiredByteFlopModel() {
        super();
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
     * 要求Byte/FLOP算出結果リストのグループ数を取得する
     * @return		要求Byte/FLOP算出結果リスト数
     */
    public int getListResultGroupCount() {
        if (this.listResults == null) {
            return 0;
        }
        return this.listResults.size();
    }


    /**
     * 要求Byte/FLOP算出結果を取得する
     * @param   groupId    グループインデックス
     * @param   index    リストインデックス
     * @return		要求Byte/FLOP算出結果
     */
    public RequiredByteFlopResult getRequiredByteFlopResult(int groupId, int index) {
        if (this.listResults == null) {
            return null;
        }
        if (this.listResults.size() <= groupId) return null;
        List<RequiredByteFlopResult> group = this.listResults.get(groupId);
        if (group == null || group.size() <= index) return null;
        return group.get(index);
    }

    /**
     * 要求Byte/FLOP算出結果グループからブロックの一致する要求Byte/FLOP算出結果を取得する
     * @param   group    グループリスト
     * @param   block    ブロック
     * @return		要求Byte/FLOP算出結果
     */
    public RequiredByteFlopResult getRequiredByteFlopResult(List<RequiredByteFlopResult> group, Object block) {
        if (group == null || block == null) return null;
        for (RequiredByteFlopResult result : group) {
        	if (result.getBlock() == block) {
        		return result;
        	}
        }
        return null;
    }

    /**
     * 要求Byte/FLOP算出結果テーブルモデルを取得する
     * @return		要求Byte/FLOP算出結果テーブルモデル
     */
    public DefaultTableModel getBlockDefaultTableModel() {
        // テーブルモデルの作成
        DefaultTableModel tableModel = new DefaultTableModel(HEADER_COLUMNS, 0);
        return tableModel;
    }


    /**
     * 要求Byte/FLOP算出結果テーブル列幅を設定する.<br/>
     * 要求Byte/FLOP算出結果テーブルはすべて固定列幅とする
     * @param columnModel		テーブル列モデル
     */
    public void setTableColumnWidth(DefaultTableColumnModel columnModel) {
        for (int i=0; i<columnModel.getColumnCount(); i++) {
            // 列取得
            TableColumn column = columnModel.getColumn(i);
            if (HEADER_COLUMNS_PREFERREDWIDTH.length >= i) {
                if (HEADER_COLUMNS_PREFERREDWIDTH[i] >= 0) {
                    //column.setMinWidth(HEADER_COLUMNS_PREFERREDWIDTH[i]);
                    //column.setMaxWidth(HEADER_COLUMNS_PREFERREDWIDTH[i]);
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


    /**
     * 要求Byte/FLOP算出結果テーブル行を追加する
     * @param results		要求Byte/FLOP算出結果リスト
     */
    public void addRequiredByteFlopResults(RequiredByteFlopResult[] results) {
        if (results == null) return;
        if (this.listResults == null) {
            this.listResults = new ArrayList<List<RequiredByteFlopResult>>();
        }
        List<RequiredByteFlopResult> list = new ArrayList<RequiredByteFlopResult>();
        list.addAll(Arrays.asList(results));
        this.listResults.add(list);

        // モデルの変更を通知
        notifyModel();
    }

    /**
     * テーブルモデルをクリアする。
     */
    public void clearModel() {
        // テーブルモデルのクリア
        this.listResults = new ArrayList<List<RequiredByteFlopResult>>();
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
            // 要求Byte/FLOP算出結果テーブル
            if (this.listResults == null || this.listResults.size() <= 0) return;

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
     * @return    テーブルモデル
     */
    private DefaultTableModel createTableModel() {
        // テーブルモデルの作成
        DefaultTableModel tableModel = new DefaultTableModel();
        tableModel.setColumnIdentifiers(HEADER_COLUMNS);
        // 演算カウントリストからテーブルモデルの作成を行う。
        if (this.listResults == null) return tableModel;

        for (List<RequiredByteFlopResult> group : this.listResults) {
        	// ブロックリストを作成する
        	List<Object> listObj = new ArrayList<Object>();
        	for (RequiredByteFlopResult result : group) {
        		listObj.add(result.getBlock());
        	}
        	// ツリーノードを作成する
        	DefaultMutableTreeNode root = SwingUtils.createTreeNode(this.modelLanguageTree.getRootNode(), listObj);

            // ツリーノードを順方向で列挙
            Enumeration<?> depth = root.preorderEnumeration();
            while(depth.hasMoreElements()) {
                DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode)depth.nextElement();
                if (treeNode == null || treeNode.getUserObject() == null) {
                    continue;
                }
                RequiredByteFlopResult result = getRequiredByteFlopResult(group, treeNode.getUserObject());
                if (result == null) continue;

                int depthCount = treeNode.getLevel() - 1;
                if (depthCount < 0) continue;
                String depthText = StringUtils.repeat(" ", depthCount*4);

                // テーブル行配列の作成
                Object[] row = new Object[HEADER_COLUMNS.length];
                int col = 0;
                // 要求Byte/FLOP算出結果
                row[col++] = result;
                // Block
                row[col++] = depthText + result.getBlock().toString();
                // Load(B)
                row[col++] = result.getLoad();
                // Store(B)
                row[col++] = result.getStore();
                // FLOP
                row[col++] = result.getOperand();
                // Required B/F
                float required = 0.0F;
                if (unitType == UNIT_TYPE.FLOP_BYTE) {
                	required = result.getRequiredFB();
                }
                else {
                	required = result.getRequiredBF();
                }
                row[col++] = Float.valueOf(String.format("%.2f", required));
                // Throughput(GB/s)
                float throughput = result.getThroughput();
                row[col++] = Float.valueOf(String.format("%.2f", throughput));
                // Effective B/F
                float effective = 0.0F;
                if (unitType == UNIT_TYPE.FLOP_BYTE) {
                	effective = result.getEffectiveFB();
                }
                else {
                	effective = result.getEffectiveBF();
                }
                row[col++] = Float.valueOf(String.format("%.2f", effective));
                // Peak Ratio(%)
                float peak = 0.0F;
                if (unitType == UNIT_TYPE.FLOP_BYTE) {
                	peak = result.getPeakFB();
                }
                else {
                	peak = result.getPeakBF();
                }
                peak *= 100.0;
                row[col++] = Float.valueOf(String.format("%.2f", peak));
                // Memory Variable
                row[col++] = result.getMemoryCount();
                // L1 Variable
                row[col++] = result.getL1Count();
                // L2 Variable
                row[col++] = result.getL2Count();
                // Register Variable
                row[col++] = result.getRegisterCount();
                // Custom Variable
                row[col++] = result.getCustomCount();
                // add(F)
                row[col++] = result.getAddCount();
                // sub(F)
                row[col++] = result.getSubCount();
                // mul(F)
                row[col++] = result.getMulCount();
                // div(F)
                row[col++] = result.getDivCount();
                // intrinsic(F)
                row[col++] = result.getIntrinsicCount();
                // Performance(GFLOPS)
                row[col++] = result.getPerformance();
                // Store Mode
                row[col++] = result.getStoreMode();
                // Memory MBW(GB/s)
                row[col++] = result.getMemoryMBW();
                // L1 MBW(GB/s)
                row[col++] = result.getL1MBW();
                // L2 MBW(GB/s)
                row[col++] = result.getL2MBW();
                // Register MBW(GB/s)
                row[col++] = result.getRegisterMBW();
                // Custom MBW(GB/s)
                row[col++] = result.getCustomMBW();
                // Memory Coef
                row[col++] = result.getMemoryCoef();
                // L1 Coef
                row[col++] = result.getL1Coef();
                // L2 Coef
                row[col++] = result.getL2Coef();
                // Register Coef
                row[col++] = result.getRegisterCoef();
                // Custom Coef
                row[col++] = result.getCustomCoef();
                // 付加情報
                if (result.getBlock() != null && result.getBlock() instanceof IInformation) {
                	TextInfo info = ((IInformation)result.getBlock()).getInformation();
                	if (info != null && !StringUtils.isNullOrEmpty(info.getContent())) {
                        // row[col++] = info.getAbstract();
                		row[col++] = info;
                	}
                	else {
                		row[col++] = "no info";
                	}
                }
                // テーブル行追加
                tableModel.addRow(row);
            }
        }

        return tableModel;
    }

    /**
     * モデルが空か否か
     * @return	空か否か（true: 空，false: データあり）
     */
    public boolean isEmpty() {
    	if (this.listResults == null) return true;
    	return (this.listResults.size() < 1);
    }

	/**
	 * 算出単位を取得する.
	 * @return 算出単位
	 */
	public UNIT_TYPE getUnitType() {
		return unitType;
	}
	/**
	 * 算出単位を設定する.
	 * @param type 算出単位
	 */
	public void setUnitType(UNIT_TYPE type) {
		this.unitType = type;

		// ヘッダー表示単位を変更する.
		for (int i=0; i<this.HEADER_COLUMNS.length; i++) {
			if (type == UNIT_TYPE.FLOP_BYTE) {
				this.HEADER_COLUMNS[i] = this.HEADER_COLUMNS[i].replaceAll("B/F", "F/B");
			}
			else {
				this.HEADER_COLUMNS[i] = this.HEADER_COLUMNS[i].replaceAll("F/B", "B/F");
			}
		}
	}

	/**
	 * 構造ツリーモデルを取得する.
	 * @return 構造ツリーモデル.
	 */
	public LanguageTreeModel getModelLanguageTree() {
		return this.modelLanguageTree;
	}

	/**
	 * 構造ツリーモデルを設定する.
	 * @param model    構造ツリーモデル.
	 */
	public void setModelLanguageTree(LanguageTreeModel model) {
		this.modelLanguageTree = model;
	}


}



