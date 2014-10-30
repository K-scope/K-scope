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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableModel;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.gui.ISourceBargraph;
import jp.riken.kscope.profiler.ProfilerBaseData;
import jp.riken.kscope.profiler.ProfilerDprofData;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.utils.StringUtils;

/**
 * プロファイラ:コスト情報モデル
 * @author RIKEN
 *
 */
public class ProfilerCostTableModel extends ProfilerTableBaseModel {

    /**
     * テーブルヘッダーリスト(5列):コスト情報(手続).<br/>
     * 1列目はコスト情報とする。
     */
    private String[] HEADER_COLUMNS_PROCEDURE = {"", 
    		Message.getString("profilercallgraphmodel.header_columns.sampling"), //サンプリング数 
    		Message.getString("profilercallgraphmodel.header_columns.total-percentage"), //全体に占める割合(%) 
    		Message.getString("profileinfo_type.enum.procedure"), //手続 
    		Message.getString("profilercosttablemodel.header_columns.filename"), //ファイル名 
    		Message.getString("profilercosttablemodel.header_columns.linenumber")}; //行番号
    /**
     * テーブルヘッダーリスト(5列):コスト情報(ループ).<br/>
     * 1列目はコスト情報とする。
     */
    private String[] HEADER_COLUMNS_LOOP = {"", 
    		Message.getString("profilercallgraphmodel.header_columns.sampling"), //サンプリング数 
    		Message.getString("profilercallgraphmodel.header_columns.total-percentage"), //全体に占める割合(%) 
    		Message.getString("profileinfo_type.enum.loop"), //ループ 
    		Message.getString("profilercosttablemodel.header_columns.filename"), //ファイル名 
    		Message.getString("profilercosttablemodel.header_columns.linenumber")}; //行番号
    /**
     * テーブルヘッダーリスト(5列):コスト情報(ライン).<br/>
     * 1列目はコスト情報とする。
     */
    private String[] HEADER_COLUMNS_LINE = {"", 
    		Message.getString("profilercallgraphmodel.header_columns.sampling"), //サンプリング数 
    		Message.getString("profilercallgraphmodel.header_columns.total-percentage"), //全体に占める割合(%) 
    		Message.getString("profileinfo_type.enum.line"), //ライン 
    		Message.getString("profilercosttablemodel.header_columns.filename"), //ファイル名 
    		Message.getString("profilercosttablemodel.header_columns.linenumber")}; //行番号
    /** テーブル列の表示状態 */
    private boolean[] visibledcolumns = {false, true, true, true, true, true};
    /**
     * テーブル列サイズ
     * -1=非表示とする
     */
    private int[] HEADER_COLUMNS_PREFERREDWIDTH = { -1, 120, 140, 240, 160, 80 };

    /**
     * テーブル列最小サイズ.<br/>
     * -1=非表示とする
     */
    private int[] HEADER_COLUMNS_MINWIDTH = {-1, 80, 80, 80, 80, 80};

    /**
     * テーブル列配置.<br/>
     */
    private int[] COLUMNS_ALIGNMENTS = {SwingConstants.LEFT, SwingConstants.RIGHT, SwingConstants.RIGHT, SwingConstants.LEFT, SwingConstants.LEFT, SwingConstants.LEFT};

    /** タイトル */
    private String title;

    /**
     * プロファイラ:コスト情報マップ
     * キー：プロファイラのプロセス、スレッドを表記する一意の文字列
     * 値：プロシージャ、ループ、ラインのコスト情報リスト
     */
    private Map<String, List<ProfilerDprofData>> mapCostInfo;
    /** 選択コスト情報 */
    private ProfilerBaseData selectedCostInfo;
    /** ビューのソート状態  */
    private boolean viewSort = false;

    /**
     * コンストラクタ
     * @param type		プロファイラ情報タイプ
     */
    public ProfilerCostTableModel(PROFILERINFO_TYPE type) {
        super(type);
    }

    /**
     * モデルの変更を通知する
     */
    @Override
    protected void notifyModel() {
        this.setChanged();
        this.notifyObservers();
        this.clearChanged();
    }

    /**
     * コスト情報マップ数を取得する
     * @return		コスト情報マップ数
     */
    @Override
    public int getInfoMapCount() {
        if (this.mapCostInfo == null) {
            return 0;
        }
        return this.mapCostInfo.size();
    }


    /**
     * コスト情報マップキー名を取得する
     * @param   index    マップインデックス
     * @return		コスト情報マップキー名
     */
    @Override
    public String getInfoMapKey(int index) {
        if (this.mapCostInfo == null) {
            return null;
        }
        if (this.mapCostInfo.size() <= index) return null;
        Set<String> keySet = this.mapCostInfo.keySet();
        int i = 0;
        for (String key : keySet) {
            if (i == index) {
                return key;
            }
            i++;
        }
        return null;
    }


    /**
     * コスト情報リストを取得する
     * @param   index    マップインデックス
     * @return		コスト情報リスト
     */
    public List<ProfilerDprofData> getInfoMapValue(int index) {
        return getInfoMap(getInfoMapKey(index));
    }

    /**
     * コスト情報リストを取得する
     * @param   key    マップキー
     * @return		コスト情報リスト
     */
    public List<ProfilerDprofData> getInfoMap(String key) {
        if (this.mapCostInfo == null) {
            return null;
        }
        if (key == null) return null;
        return this.mapCostInfo.get(key);
    }

    /**
     * テーブルモデルを取得する
     * @return		テーブルモデル
     */
    public DefaultTableModel getDefaultTableModel() {
        // テーブルモデルの作成
        String[] header = getHeaderColumns();
        DefaultTableModel tableModel = new DefaultTableModel(header, 0);
        return tableModel;
    }


    /**
     * ヘッダー列リストを取得する。
     * @return		ヘッダー列リスト
     */
    @Override
    public String[] getHeaderColumns() {
        // テーブルモデルの作成
        String[] header = HEADER_COLUMNS_PROCEDURE;
        if (this.getEnumInfo() == PROFILERINFO_TYPE.COST_PROCEDURE) {
            header = HEADER_COLUMNS_PROCEDURE;
        }
        else if (this.getEnumInfo() == PROFILERINFO_TYPE.COST_LOOP) {
            header = HEADER_COLUMNS_LOOP;
        }
        else if (this.getEnumInfo() == PROFILERINFO_TYPE.COST_LINE) {
            header = HEADER_COLUMNS_LINE;
        }
        return header;
    }

    /**
     * ヘッダー推奨列幅リストを取得する。
     * @return		ヘッダー推奨列幅
     */
    @Override
    protected int[] getHeaderColumnsPreferredWidth() {
        return HEADER_COLUMNS_PREFERREDWIDTH;
    }

    /**
     * ヘッダー最小列幅リストを取得する。
     * @return		ヘッダー最小列幅
     */
    @Override
    protected int[] getHeaderColumnsMinWidth() {
        return HEADER_COLUMNS_MINWIDTH;
    }

    /**
     * コスト情報を追加する
     * @param key			コスト情報キー
     * @param info			コスト情報
     */
    public void addCostInfo(String key, ProfilerDprofData info) {

        if (key == null) return;
        if (info == null) return;

        // コスト情報マップの生成
        if (this.mapCostInfo == null) {
            this.mapCostInfo = new TreeMap<String, List<ProfilerDprofData>>();
        }
        List<ProfilerDprofData> list = this.mapCostInfo.get(key);
        if (list == null) {
            list = new ArrayList<ProfilerDprofData>();
            this.mapCostInfo.put(key, list);
        }
        list.add(info);

        // モデルの変更を通知
        notifyModel();
    }

    /**
     * プロファイラデータを設定する
     * @param key			コスト情報キー
     * @param infos			コスト情報リスト
     */
    @Override
    public void setProfilerData(String key, ProfilerBaseData[] infos) {

        if (key == null) return;
        // コスト情報マップの生成
        if (this.mapCostInfo == null) {
            this.mapCostInfo = new TreeMap<String, List<ProfilerDprofData>>();
        }

        // コスト情報リストがnullの場合は、コスト情報削除
        if (infos == null) {
            if (this.mapCostInfo.containsKey(key)) {
                this.mapCostInfo.remove(key);
            }
            return;
        }

        List<ProfilerDprofData> list = null;
        if (this.mapCostInfo.containsKey(key)) {
            list = this.mapCostInfo.get(key);
        }
        else {
            list = new ArrayList<ProfilerDprofData>();
            this.mapCostInfo.put(key, list);
        }
        list.clear();
        for (ProfilerBaseData data : infos) {
            if (data instanceof ProfilerDprofData) {
                list.add((ProfilerDprofData)data);
            }
        }

        // モデルの変更を通知
        notifyModel();
    }


    /**
     * テーブルモデルをクリアする。
     */
    @Override
    public void clearModel() {
        // コスト情報マップのクリア
        if (this.mapCostInfo != null) {
            this.mapCostInfo = new TreeMap<String, List<ProfilerDprofData>>();
        }
        // タイトルのクリア
        this.title = null;
        // 選択プロファイラデータのクリア
        this.selectedCostInfo = null;
        // モデルの変更を通知
        notifyModel();
    }

    /**
     * タイトルを取得する
     * @return	タイトル
     */
    @Override
    public String getTitle() {
        return title;
    }

    /**
     * タイトルを設定する
     * @param title		タイトル
     */
    @Override
    public void setTitle(String title) {
        this.title = title;
    }


    /**
     * 選択コスト情報を設定する
     * @param 	info        選択コスト情報
     */
    @Override
    public void setSelectedInfo(ProfilerBaseData info) {
        this.selectedCostInfo = info;
    }

    /**
     * 選択コスト情報を取得する
     * @return		選択コスト情報
     */
    @Override
    public ProfilerBaseData getSelectedInfo() {
        return this.selectedCostInfo;
    }


    /**
     * テーブルモデルを取得する
     * @param index		コスト情報マップインデックス
     * @return		テーブルモデル
     */
    public DefaultTableModel getInfoTableModel(int index) {
        return getCostInfoTableModel(this.getInfoMapKey(index));
    }

    /**
     * テーブルモデルを取得する
     * @param key		コスト情報識別文字列
     * @return		テーブルモデル
     */
    public DefaultTableModel getCostInfoTableModel(String key) {
        if (key == null) return null;
        // テーブルモデルの作成
        DefaultTableModel tableModel = getDefaultTableModel();
        List<ProfilerDprofData> list = this.mapCostInfo.get(key);

        int maxcount = 0;
        if (this.getProfilerProperties() != null) {
            maxcount = this.getProfilerProperties().getCostinfoMaxCount();
        }
        final String INDENT = "    ";   // ネストのインデント空白4
        for (ProfilerDprofData info : list) {
            Object[] cols = new Object[tableModel.getColumnCount()];
            // 1列目はProfilerCostInfo：非表示
            cols[0] = info;
            cols[1] = (int)info.getSampling();
            float value = new BigDecimal(String.valueOf(info.getRatio()*100)).setScale(ProfilerProperties.COST_RATIO_SCALE, BigDecimal.ROUND_HALF_UP).floatValue();
            String format = "%.0" + ProfilerProperties.COST_RATIO_SCALE + "f";
            cols[2] = String.format(format, value);
            // シンボル名
            String name = info.getSymbol();
            int nest = info.getNestLevel();
            // シンボル名をネスト表示する
            name = StringUtils.repeat(INDENT, nest) + name;
            cols[3] = String.valueOf(name);
            if (info.getCodeLine().getSourceFile() != null) {
                cols[4] = info.getCodeLine().getSourceFile().getFile().getName();
            }
            if (info.getCodeLine() != null) {
                cols[5] = info.getCodeLine().getLineno();
            }
            tableModel.addRow(cols);

            // 最大表示行数（初期値が0の場合は最大表示行数設定なし）
            maxcount--;
            if (maxcount == 0) {
                break;
            }
        }

        return tableModel;
    }


    /**
     * プロファイル情報サブタイトルを取得する
     * @param   index    マップインデックス
     * @return		サブタイトル
     */
    @Override
    public String getSubTitle(int index) {
        String key = getInfoMapKey(index);
        return key;
    }

    /**
     * プロファイラバーグラフデータを取得する
     * @return   プロファイラバーグラフデータ
     */
    @Override
    public ISourceBargraph[] getSelectedBargraph() {
        // 選択プロファイラデータの取得
        if (mapCostInfo == null) return null;

        List<ProfilerDprofData> selected = null;
        if (this.selectedCostInfo == null) {
            // 未選択状態であるので、最初のプロファイルデータの取得を行う
            selected = getInfoMapValue(0);
        }
        else {
            // 選択プロファイルデータの属するリストを取得する
            Set<String> keySet = this.mapCostInfo.keySet();
            MAP_LABEL:for (String key : keySet) {
                List<ProfilerDprofData> datas = this.mapCostInfo.get(key);
                for (ProfilerDprofData data : datas) {
                    if (data == this.selectedCostInfo) {
                        selected = datas;
                        break MAP_LABEL;
                    }
                }
            }
        }
        if (selected == null) return null;

        return selected.toArray(new ISourceBargraph[0]);
    }

    /**
     * 選択プロファイルデータのテキストデータを取得する
     * @return		選択テキストデータ
     */
    @Override
    public String getSelectedText() {
        if (this.selectedCostInfo == null) return null;
        if (!(this.selectedCostInfo instanceof ProfilerDprofData)) return null;
        ProfilerDprofData info = (ProfilerDprofData)this.selectedCostInfo;
        StringBuffer buf = new StringBuffer();
        // ヘッダー:1列目はデータ列であるので除外
        String[] header = getHeaderColumns();
        for (int i=1; i<header.length; i++) {
            if (visibledcolumns[i]) {
                buf.append(header[i]);
                buf.append(", ");
            }
        }
        buf.delete(buf.length()-2, buf.length());
        buf.append("\n");

        // サンプリング数
        if (visibledcolumns[1]) {
            buf.append(info.getSampling());
            buf.append(", ");
        }
        // 全体に占める割合(%)
        if (visibledcolumns[2]) {
            float value = new BigDecimal(String.valueOf(info.getRatio()*100)).setScale(ProfilerProperties.COST_RATIO_SCALE, BigDecimal.ROUND_HALF_UP).floatValue();
            String format = "%.0" + ProfilerProperties.COST_RATIO_SCALE + "f";
            String text = String.format(format, value);
            buf.append(text);
            buf.append(", ");
        }
        // シンボル名
        if (visibledcolumns[3]) {
            buf.append(info.getSymbol());
            buf.append(", ");
        }
        // ファイル名
        if (visibledcolumns[4]) {
            if (info.getCodeLine().getSourceFile() != null) {
                buf.append(info.getCodeLine().getSourceFile().getFile().getName());
            }
            buf.append(", ");
        }
        // 行番号
        if (visibledcolumns[5]) {
            if (info.getCodeLine() != null) {
                buf.append(info.getCodeLine().getLineno());
            }
            buf.append(", ");
        }
        buf.delete(buf.length()-2, buf.length());
        return buf.toString();
    }


    /**
     * ヘッダー列の表示状態を取得する
     * @return		ヘッダー列表示状態リスト
     */
    @Override
    public boolean[] getVisibledColumns() {
        return this.visibledcolumns;
    }

    /**
     * ヘッダー列の表示状態を設定する
     * @param col		ヘッダー列番号
     * @param checked   表示状態
     */
    @Override
    public void setVisibledColumns(int col, boolean checked) {
        if (this.visibledcolumns.length <= col) return;
        this.visibledcolumns[col] = checked;
        this.notifyModel();
    }

    /**
     * テーブル列配置を取得する
     * @return		テーブル列配置
     */
    @Override
    public int[] getTableColumnAlignments() {
        return this.COLUMNS_ALIGNMENTS;
    }
    
    /**
     * ソートフラグを設定する
     * @return		void
     */
    @Override
    public void setViewSort(boolean sort) {
    	this.viewSort = sort;

    	ProfilerCostTableModelComparator comp = null;
    	if (this.viewSort) {
	    	comp = new ProfilerCostTableModelComparator(ProfilerCostTableModelComparator.SORT_MODE.BY_LINE);
    	} else {
    		comp = new ProfilerCostTableModelComparator(ProfilerCostTableModelComparator.SORT_MODE.BY_COST);
    	}
    	for (Map.Entry<String, List<ProfilerDprofData>> e : mapCostInfo.entrySet()) {
    		Collections.sort(e.getValue(), comp);
    	}
    	
    	notifyModel();
    }

}


