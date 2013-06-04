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
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableModel;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.gui.ISourceBargraph;
import jp.riken.kscope.profiler.ProfilerBaseData;
import jp.riken.kscope.profiler.ProfilerDprofData;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.utils.StringUtils;

/**
 * プロファイラ:コールグラフ情報モデル
 * @author riken
 *
 */
public class ProfilerCallGraphModel  extends ProfilerTableBaseModel {

    /**
     * テーブルヘッダーリスト(4列):コールグラフ情報.<br/>
     * 1列目はコスト情報とする。
     */
    private String[] HEADER_COLUMNS_CALLGRAPH = {"",
    		Message.getString("profilercallgraphmodel.header_columns.sampling"), //サンプリング数
    		Message.getString("profilercallgraphmodel.header_columns.total-percentage"), //全体に占める割合(%)
    		Message.getString("profilercallgraphmodel.header_columns.symbol"), }; //シンボル名
    /** テーブル列の表示状態 */
    private boolean[] visibledcolumns = {false, true, true, true};
    /**
     * テーブル列サイズ
     * -1=非表示とする
     */
    private int[] HEADER_COLUMNS_PREFERREDWIDTH = { -1, 120, 140, 480};

    /**
     * テーブル列最小サイズ.<br/>
     * -1=非表示とする
     */
    private int[] HEADER_COLUMNS_MINWIDTH = {-1, 80, 80, 80};
    /**
     * テーブル列配置.<br/>
     */
    private int[] COLUMNS_ALIGNMENTS = {SwingConstants.LEFT, SwingConstants.RIGHT, SwingConstants.RIGHT, SwingConstants.LEFT};

    /** タイトル */
    private String title;

    /**
     * プロファイラ:コールグラフマップ
     * キー：プロファイラのプロセス、スレッドを表記する一意の文字列=ファイル名
     * 値：コールグラフシンボル情報リスト
     */
    private Map<String, List<ProfilerDprofData>> mapInfo;
    /** 選択コールグラフ情報 */
    private ProfilerBaseData selectedInfo;
    /** 表示コールグラフパネル識別子 */
    private ANALYSIS_PANEL enumPanel;

    /**
     * コンストラクタ
     * @param type		プロファイラ情報タイプ
     */
    public ProfilerCallGraphModel(PROFILERINFO_TYPE type) {
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
     * コールグラフ情報マップ数を取得する
     * @return		コールグラフ情報マップ数
     */
    @Override
    public int getInfoMapCount() {
        if (this.mapInfo == null) {
            return 0;
        }
        return this.mapInfo.size();
    }


    /**
     * コールグラフ情報マップキー名を取得する
     * @param   index    コールグラフインデックス
     * @return		コールグラフ情報マップキー名
     */
    @Override
    public String getInfoMapKey(int index) {
        if (this.mapInfo == null) {
            return null;
        }
        if (this.mapInfo.size() <= index) return null;
        Set<String> keySet = this.mapInfo.keySet();
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
     * コールグラフ情報リストを取得する
     * @param   index    マップインデックス
     * @return		コールグラフ情報リスト
     */
    public List<ProfilerDprofData> getInfoMapValue(int index) {
        return getInfoMap(getInfoMapKey(index));
    }

    /**
     * コールグラフ情報リストを取得する
     * @param   key    マップキー
     * @return		コールグラフ情報リスト
     */
    public List<ProfilerDprofData> getInfoMap(String key) {
        if (this.mapInfo == null) {
            return null;
        }
        if (key == null) return null;
        return this.mapInfo.get(key);
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
        String[] header = HEADER_COLUMNS_CALLGRAPH;
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
     * コールグラフ情報を追加する
     * @param key			コールグラフ情報キー
     * @param info			コールグラフ情報
     */
    public void addInfo(String key, ProfilerDprofData info) {

        if (key == null) return;
        if (info == null) return;

        // コスト情報マップの生成
        if (this.mapInfo == null) {
            this.mapInfo = new TreeMap<String, List<ProfilerDprofData>>();
        }
        List<ProfilerDprofData> list = this.mapInfo.get(key);
        if (list == null) {
            list = new ArrayList<ProfilerDprofData>();
            this.mapInfo.put(key, list);
        }
        list.add(info);

        // モデルの変更を通知
        notifyModel();
    }

    /**
     * プロファイラデータを設定する
     * @param key		プロファイラデータキー
     * @param infos		プロファイラデータ
     */
    /**
     * コールグラフ情報を設定する
     * @param key			コールグラフ情報キー
     * @param infos			コールグラフ情報リスト
     */
    @Override
    public void setProfilerData(String key, ProfilerBaseData[] infos) {

        if (key == null) return;
        // コスト情報マップの生成
        if (this.mapInfo == null) {
            this.mapInfo = new TreeMap<String, List<ProfilerDprofData>>();
        }

        // コスト情報リストがnullの場合は、コスト情報削除
        if (infos == null) {
            if (this.mapInfo.containsKey(key)) {
                this.mapInfo.remove(key);
            }
            return;
        }

        List<ProfilerDprofData> list = null;
        if (this.mapInfo.containsKey(key)) {
            list = this.mapInfo.get(key);
        }
        else {
            list = new ArrayList<ProfilerDprofData>();
            this.mapInfo.put(key, list);
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
        if (this.mapInfo != null) {
            this.mapInfo = new TreeMap<String, List<ProfilerDprofData>>();
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
     * 選択コールグラフ情報を設定する
     * @param 	info        選択コールグラフ情報
     */
    public void setSelectedInfo(ProfilerBaseData info) {
        this.selectedInfo = info;
    }

    /**
     * 選択コールグラフ情報を取得する
     * @return		選択コールグラフ情報
     */
    public ProfilerBaseData getSelectedInfo() {
        return this.selectedInfo;
    }

    /**
     * 表示コストパネル識別子を取得する
     * @return		表示コストパネル識別子
     */
    public ANALYSIS_PANEL getEnumPanel() {
        return enumPanel;
    }

    /**
     * 表示コストパネル識別子を設定する
     * @param panel		表示コストパネル識別子
     */
    public void setEnumPanel(ANALYSIS_PANEL panel) {
        this.enumPanel = panel;
    }


    /**
     * テーブルモデルを取得する
     * @param index		コールグラフ情報マップインデックス
     * @return		テーブルモデル
     */
    public DefaultTableModel getInfoTableModel(int index) {
        return getInfoTableModel(this.getInfoMapKey(index));
    }

    /**
     * テーブルモデルを取得する
     * @param key		コスト情報識別文字列
     * @return		テーブルモデル
     */
    public DefaultTableModel getInfoTableModel(String key) {
        if (key == null) return null;
        // テーブルモデルの作成
        DefaultTableModel tableModel = getDefaultTableModel();
        List<ProfilerDprofData> list = this.mapInfo.get(key);

        final String INDENT = "    ";   // ネストのインデント空白4
        for (ProfilerDprofData info : list) {
            Object[] cols = new Object[tableModel.getColumnCount()];
            // 1列目はProfilerCostInfo：非表示
            cols[0] = info;
            cols[1] = (int)info.getSampling();
            float value = new BigDecimal(String.valueOf(info.getRatio()*100)).setScale(ProfilerProperties.COST_RATIO_SCALE, BigDecimal.ROUND_HALF_UP).floatValue();
            String format = "%.0" + ProfilerProperties.COST_RATIO_SCALE + "f";
            String text = String.format(format, value);
            cols[2] = text;
            // シンボル名
            String name = info.getSymbol();
            int nest = info.getNestLevel();
            // シンボル名をネスト表示する
            name = StringUtils.repeat(INDENT, nest) + name;
            cols[3] = String.valueOf(name);
            tableModel.addRow(cols);
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
        return null;
    }

    /**
     * 選択プロファイルデータのテキストデータを取得する
     * @return		選択テキストデータ
     */
    @Override
    public String getSelectedText() {
        if (this.selectedInfo == null) return null;
        if (!(this.selectedInfo instanceof ProfilerDprofData)) return null;
        ProfilerDprofData info = (ProfilerDprofData)this.selectedInfo;
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
        if (visibledcolumns[3]) {
            // シンボル名
            buf.append(info.getSymbol());
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

}


