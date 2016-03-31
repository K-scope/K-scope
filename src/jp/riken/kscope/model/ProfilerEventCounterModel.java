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

import java.text.DecimalFormat;
import java.util.ArrayList;
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
import jp.riken.kscope.profiler.ProfilerEprofData;
import jp.riken.kscope.profiler.eprof.HardwareMonitorInfo;
import jp.riken.kscope.profiler.eprof.HardwarePaTable;

/**
 * プロファイラ:イベントカウンタ情報モデル
 * @author RIKEN
 *
 */
public class ProfilerEventCounterModel extends ProfilerTableBaseModel {

    /**
     * テーブルヘッダーリスト(5列):ハードウェアモニタ情報（ＰＡ情報）テーブル:Cacheのテーブル<br/>
     * 1列目はプロファイラ情報とする。
     */
    private String[] HEADER_COLUMNS_CACHE = {"",
    		Message.getString("profilereventcountermodel.header_columns_cache.threadnum"), //スレッド番号
    		Message.getString("profilereventcountermodel.header_columns_cache.elapsedtime"), //経過時間(s)
    		Message.getString("profilereventcountermodel.header_columns_cache.usertime"), //ユーザ時間(s)
    		Message.getString("profilereventcountermodel.header_columns_cache.num-instruction-exe"), //命令実行数
    		Message.getString("profilereventcountermodel.header_columns_cache.num-load-store"), //ロード/ストア命令数
    		Message.getString("profilereventcountermodel.header_columns_cache.num-prefetch"), //prefetch命令数
    		Message.getString("profilereventcountermodel.header_columns_cache.num-SIMD-load-store"), //SIMD ロード/ストア命令数
    		Message.getString("profilereventcountermodel.header_columns_cache.num-L1-cachemisses"), //L1データキャッシュミス数
    		Message.getString("profilereventcountermodel.header_columns_cache.num-L2-cachedemandmisses"), //L2キャッシュdemandミス数
    		Message.getString("profilereventcountermodel.header_columns_cache.num-L2-cacheprefetchmisses"), //L2キャッシュprefetchミス数
    		Message.getString("profilereventcountermodel.header_columns_cache.num-dataaccessMDTLBmisses")}; //データアクセスMDTLBミス数
    /**
     * テーブルヘッダーリスト(5列):ハードウェアモニタ情報（ＰＡ情報）テーブル:Instructionsのテーブル<br/>
     * 1列目はプロファイラ情報とする。
     */
    private String[] HEADER_COLUMNS_INSTRUCTIONS = {"",
    		Message.getString("profilereventcountermodel.header_columns_cache.threadnum"), //スレッド番号
    		Message.getString("profilereventcountermodel.header_columns_cache.elapsedtime"), //経過時間(s)
    		Message.getString("profilereventcountermodel.header_columns_cache.usertime"), //ユーザ時間(s)
    		Message.getString("profilereventcountermodel.header_columns_cache.num-instruction-exe"), //命令実行数
    		Message.getString("profilereventcountermodel.header_columns_cache.num-load-store"), //ロード/ストア命令数
    		Message.getString("profilereventcountermodel.header_columns_instructions.num-floating-point"), //浮動小数点演算命令数
    		Message.getString("profilereventcountermodel.header_columns_instructions.num-high-speed"), //高速演算命令数
    		Message.getString("profilereventcountermodel.header_columns_cache.num-SIMD-load-store"), //SIMD ロード/ストア命令数
    		Message.getString("profilereventcountermodel.header_columns_instructions.num-SIMD-floating-point"), //SIMD浮動小数点演算命令数
    		Message.getString("profilereventcountermodel.header_columns_instructions.num-SIMD-high-speed")}; //SIMD高速演算命令数
    /**
     * テーブルヘッダーリスト(5列):ハードウェアモニタ情報（ＰＡ情報）テーブル:MEM_accessのテーブル<br/>
     * 1列目はプロファイラ情報とする。
     */
    private String[] HEADER_COLUMNS_MEM_ACCESS = {"",
    		Message.getString("profilereventcountermodel.header_columns_cache.threadnum"), //スレッド番号
    		Message.getString("profilereventcountermodel.header_columns_cache.elapsedtime"), //経過時間(s)
    		Message.getString("profilereventcountermodel.header_columns_cache.usertime"), //ユーザ時間(s)
    		Message.getString("profilereventcountermodel.header_columns_cache.num-instruction-exe"), //命令実行数
    		Message.getString("profilereventcountermodel.header_columns_cache.num-load-store"), //ロード/ストア命令数
    		Message.getString("profilereventcountermodel.header_columns_cache.num-prefetch"), //prefetch命令数
    		Message.getString("profilereventcountermodel.header_columns_cache.num-SIMD-load-store"), //SIMD ロード/ストア命令数
    		Message.getString("profilereventcountermodel.header_columns_cache.num-L2-cachedemandmisses"), //L2キャッシュdemandミス数
    		Message.getString("profilereventcountermodel.header_columns_cache.num-L2-cacheprefetchmisses"), //L2キャッシュprefetchミス数
    		Message.getString("profilereventcountermodel.header_columns_mem.num-L2-cachedemandmiss"), //L2キャッシュdemandミスライトバック数
    		Message.getString("profilereventcountermodel.header_columns_mem.num-L2-prefetchmiss")}; //"L2キャッシュprefetchミスライトバック数"
    /**
     * テーブルヘッダーリスト(5列):ハードウェアモニタ情報（ＰＡ情報）テーブル:Performanceのテーブル<br/>
     * 1列目はプロファイラ情報とする。
     */
    private String[] HEADER_COLUMNS_PERFORMANCE = {"",
    		Message.getString("profilereventcountermodel.header_columns_cache.threadnum"), //スレッド番号
    		Message.getString("profilereventcountermodel.header_columns_cache.elapsedtime"), //経過時間(s)
    		Message.getString("profilereventcountermodel.header_columns_cache.usertime"), //ユーザ時間(s)
    		Message.getString("profilereventcountermodel.header_columns_performance.num-cycles"), //サイクル数
    		Message.getString("profilereventcountermodel.header_columns_performance.num-cycles0"), //命令完了数0サイクル数
    		Message.getString("profilereventcountermodel.header_columns_performance.num-cycles1"), //命令完了数1サイクル数
    		Message.getString("profilereventcountermodel.header_columns_performance.floating-point.num-cycles0"), //浮動小数点数演算:命令完了数0サイクル数
    		Message.getString("profilereventcountermodel.header_columns_performance.waitmem.num-cycles0"), //メモリアクセスデータ待ち:命令完了数0サイクル数
    		Message.getString("profilereventcountermodel.header_columns_performance.num-cycles-L2-cachemiss"), //L2キャッスミス待ちサイクル数
    		Message.getString("profilereventcountermodel.header_columns_performance.CSE-empty.num-cycles0"), //CSE空:命令完了数0サイクル数
    		Message.getString("profilereventcountermodel.header_columns_performance.CSE-empty-store.num-cycles0")}; //CSE空・ストアポートフル:命令完了数0サイクル数
    /**
     * テーブルヘッダーリスト(5列):ハードウェアモニタ情報（ＰＡ情報）テーブル:Statisticsのテーブル<br/>
     * 1列目はプロファイラ情報とする。
     */
    private String[] HEADER_COLUMNS_STATISTICS = {"",
    		Message.getString("profilereventcountermodel.header_columns_cache.threadnum"), //スレッド番号
    		Message.getString("profilereventcountermodel.header_columns_cache.elapsedtime"), //経過時間(s)
    		Message.getString("profilereventcountermodel.header_columns_cache.usertime"), //ユーザ時間(s)
    		Message.getString("profilereventcountermodel.header_columns_cache.num-instruction-exe"), //命令実行数
    		Message.getString("profilereventcountermodel.header_columns_instructions.num-floating-point"), //浮動小数点演算命令数
    		Message.getString("profilereventcountermodel.header_columns_instructions.num-high-speed"), //高速演算命令数
    		Message.getString("profilereventcountermodel.header_columns_cache.num-SIMD-load-store"), //SIMD ロード/ストア命令数
    		Message.getString("profilereventcountermodel.header_columns_instructions.num-SIMD-floating-point"), //SIMD浮動小数点演算命令数
    		Message.getString("profilereventcountermodel.header_columns_instructions.num-SIMD-high-speed"), //SIMD高速演算命令数
    		Message.getString("profilereventcountermodel.header_columns_statistics.datatransfer-r"), //メモリCPU間データ転送量(r)
    		Message.getString("profilereventcountermodel.header_columns_statistics.datatransfer-w")}; //メモリCPU間データ転送量(w)
    /** テーブル列の表示状態 */
    private boolean[] visibledcolumns = {false, true, true, true, true, true, true, true, true, true, true, true};
    /**
     * テーブル列配置.<br/>
     */
    private int[] COLUMNS_ALIGNMENTS = {SwingConstants.LEFT, SwingConstants.RIGHT, SwingConstants.RIGHT, SwingConstants.RIGHT, SwingConstants.RIGHT, SwingConstants.RIGHT, SwingConstants.RIGHT, SwingConstants.RIGHT, SwingConstants.RIGHT, SwingConstants.RIGHT, SwingConstants.RIGHT, SwingConstants.RIGHT};

    // テーブル列サイズ -1=非表示とする
    /** テーブル列サイズ : ハードウェアモニタ情報（ＰＡ情報）テーブル:Cacheのテーブル */
    private int[] HEADER_COLUMNS_PREFERREDWIDTH_CACHE         = { -1, 100, 100, 100, 140, 140, 140, 160, 160, 180, 180, 180 };
    /** テーブル列サイズ : ハードウェアモニタ情報（ＰＡ情報）テーブル:Instructionsのテーブル */
    private int[] HEADER_COLUMNS_PREFERREDWIDTH_INSTRUCTIONS  = { -1, 100, 100, 100, 140, 140, 140, 140, 160, 180, 180};
    /** テーブル列サイズ : ハードウェアモニタ情報（ＰＡ情報）テーブル:MEM_accessのテーブル */
    private int[] HEADER_COLUMNS_PREFERREDWIDTH_MEM_ACCESS    = { -1, 100, 100, 100, 140, 140, 140, 160, 180, 180, 260, 260 };
    /** テーブル列サイズ : ハードウェアモニタ情報（ＰＡ情報）テーブル:Performanceのテーブル */
    private int[] HEADER_COLUMNS_PREFERREDWIDTH_PERFORMANCE   = { -1, 100, 100, 100, 140, 140, 140, 240, 300, 200, 200, 300 };
    /** テーブル列サイズ : ハードウェアモニタ情報（ＰＡ情報）テーブル:Statisticsのテーブル */
    private int[] HEADER_COLUMNS_PREFERREDWIDTH_STATISTICS    = { -1, 100, 100, 100, 140, 140, 100, 160, 180, 140, 180, 200 };

    /**
     * テーブル列最小サイズ.<br/>
     * -1=非表示とする
     */
    private int[] HEADER_COLUMNS_MINWIDTH =  { -1, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80};

    /** タイトル */
    private String title;

    /**
     * プロファイラ:イベントカウンタ情報マップ
     * キー：プロファイラのプロセス、スレッドを表記する一意の文字列
     * 値：イベントカウンタ情報リスト
     */
    private Map<String, List<ProfilerEprofData>> mapInfo;
    /** 選択イベントカウンタ情報 */
    private ProfilerBaseData selectedInfo;
    /** 小数点表示最大値:指数表示との切替 */
    private double MAX_EXPONENT = 1000000.0;
    /**
     * コンストラクタ
     * @param type		プロファイラ情報タイプ
     */
    public ProfilerEventCounterModel(PROFILERINFO_TYPE type) {
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
     * イベントカウンタ情報マップ数を取得する.
     * Eprofの場合、マップ数xプロファイラ情報リスト数とする
     * @return		イベントカウンタ情報マップ数
     */
    @Override
    public int getInfoMapCount() {
        if (this.mapInfo == null) {
            return 0;
        }

        Set<String> keySet = this.mapInfo.keySet();
        int count = 0;
        for (String key : keySet) {
            List<ProfilerEprofData> list = this.mapInfo.get(key);
            if (list == null) continue;
            count += list.size();
        }
        return count;
    }


    /**
     * イベントカウンタ情報マップキー名を取得する
     * Eprofの場合、キー名はマップキー+'/'+カウンタグループ名とする
     * @param   index    マップインデックス
     * @return		イベントカウンタ情報マップキー名
     */
    @Override
    public String getInfoMapKey(int index) {
        if (this.mapInfo == null) {
            return null;
        }
        if (this.getInfoMapCount() <= index) return null;
        Set<String> keySet = this.mapInfo.keySet();
        int i = 0;
        for (String key : keySet) {
            List<ProfilerEprofData> list = this.mapInfo.get(key);
            if (list == null) continue;
            for (ProfilerEprofData data : list) {
                if (i == index) {
                    // カウンタグループ名
                    String name = data.getSymbol();
                    return key + "/" + name;
                }
                i++;
            }

        }
        return null;
    }


    /**
     * イベントカウンタ情報リストを取得する
     * @param   index    マップインデックス
     * @return		イベントカウンタ情報リスト
     */
    public List<ProfilerEprofData> getInfoMapValue(int index) {
        return getInfoMap(getInfoMapKey(index));
    }

    /**
     * イベントカウンタ情報リストを取得する
     * Eprofの場合、キー名はマップキー+'/'+カウンタグループ名とする
     * @param   key    マップキー
     * @return		イベントカウンタ情報リスト
     */
    public List<ProfilerEprofData> getInfoMap(String key) {
        if (this.mapInfo == null) {
            return null;
        }
        if (key == null) return null;
        String[] keys = key.split("/");
        if (keys == null || keys.length != 2) return null;
        List<ProfilerEprofData> list = this.mapInfo.get(keys[0]);
        if (list == null) return null;
        // Eprofの場合, イベントカウンタ情報１つだけのはず。
        List<ProfilerEprofData> result = new ArrayList<ProfilerEprofData>();
        for (ProfilerEprofData data : list) {
            String name = data.getSymbol();
            if (name.equals(keys[1])) {
                result.add(data);
            }
        }
        return result;
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
        // ヘッダー列リスト
        String[] header = HEADER_COLUMNS_STATISTICS;
        if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_CACHE) {
            header = HEADER_COLUMNS_CACHE;
        }
        else if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_INSTRUCTIONS) {
            header = HEADER_COLUMNS_INSTRUCTIONS;
        }
        else if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_MEM_ACCESS) {
            header = HEADER_COLUMNS_MEM_ACCESS;
        }
        else if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_PERFORMANCE) {
            header = HEADER_COLUMNS_PERFORMANCE;
        }
        else if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_STATISTICS) {
            header = HEADER_COLUMNS_STATISTICS;
        }

        return header;
    }

    /**
     * ヘッダー推奨列幅リストを取得する。
     * @return		ヘッダー推奨列幅
     */
    @Override
    protected int[] getHeaderColumnsPreferredWidth() {
        // ヘッダー列リスト
        int[] header = HEADER_COLUMNS_PREFERREDWIDTH_STATISTICS;
        if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_CACHE) {
            header = HEADER_COLUMNS_PREFERREDWIDTH_CACHE;
        }
        else if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_INSTRUCTIONS) {
            header = HEADER_COLUMNS_PREFERREDWIDTH_INSTRUCTIONS;
        }
        else if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_MEM_ACCESS) {
            header = HEADER_COLUMNS_PREFERREDWIDTH_MEM_ACCESS;
        }
        else if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_PERFORMANCE) {
            header = HEADER_COLUMNS_PREFERREDWIDTH_PERFORMANCE;
        }
        else if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_STATISTICS) {
            header = HEADER_COLUMNS_PREFERREDWIDTH_STATISTICS;
        }
        return header;
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
     * イベントカウンタ情報を追加する
     * @param key			イベントカウンタ情報キー
     * @param info			イベントカウンタ情報
     */
    public void addInfo(String key, ProfilerEprofData info) {

        if (key == null) return;
        if (info == null) return;

        // イベントカウンタ情報マップの生成
        if (this.mapInfo == null) {
            this.mapInfo = new TreeMap<String, List<ProfilerEprofData>>();
        }
        List<ProfilerEprofData> list = this.mapInfo.get(key);
        if (list == null) {
            list = new ArrayList<ProfilerEprofData>();
            this.mapInfo.put(key, list);
        }
        list.add(info);

        // モデルの変更を通知
        notifyModel();
    }

    /**
     * イベントカウンタ情報を設定する
     * @param key			イベントカウンタ情報キー
     * @param infos			イベントカウンタ情報リスト
     */
    @Override
    public void setProfilerData(String key, ProfilerBaseData[] infos) {

        if (key == null) return;
        // イベントカウンタ情報マップの生成
        if (this.mapInfo == null) {
            this.mapInfo = new TreeMap<String, List<ProfilerEprofData>>();
        }

        // イベントカウンタ情報リストがnullの場合は、イベントカウンタ情報削除
        if (infos == null) {
            if (this.mapInfo.containsKey(key)) {
                this.mapInfo.remove(key);
            }
            return;
        }

        List<ProfilerEprofData> list = null;
        if (this.mapInfo.containsKey(key)) {
            list = this.mapInfo.get(key);
        }
        else {
            list = new ArrayList<ProfilerEprofData>();
            this.mapInfo.put(key, list);
        }
        list.clear();
        for (ProfilerBaseData data : infos) {
            if (data instanceof ProfilerEprofData) {
                list.add((ProfilerEprofData)data);
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
        // イベントカウンタ情報マップのクリア
        if (this.mapInfo != null) {
            this.mapInfo = new TreeMap<String, List<ProfilerEprofData>>();
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
     * 選択イベントカウンタ情報を設定する
     * @param 	info        選択イベントカウンタ情報
     */
    @Override
    public void setSelectedInfo(ProfilerBaseData info) {
        this.selectedInfo = info;
    }

    /**
     * 選択イベントカウンタ情報を取得する
     * @return		選択イベントカウンタ情報
     */
    @Override
    public ProfilerBaseData getSelectedInfo() {
        return this.selectedInfo;
    }


    /**
     * テーブルモデルを取得する
     * @param index		イベントカウンタ情報マップインデックス
     * @return		テーブルモデル
     */
    public DefaultTableModel getInfoTableModel(int index) {
        return getInfoTableModel(this.getInfoMapKey(index));
    }

    /**
     * テーブルモデルを取得する
     * @param key		イベントカウンタ情報識別文字列
     * @return		テーブルモデル
     */
    public DefaultTableModel getInfoTableModel(String key) {
        if (key == null) return null;
        // テーブルモデルの作成
        DefaultTableModel tableModel = getDefaultTableModel();
        List<ProfilerEprofData> list = getInfoMap(key);

        // Eprofの場合, イベントカウンタ情報１つだけのはず。
        for (ProfilerEprofData info : list) {
            HardwareMonitorInfo hardwareInfo = info.getHardwareInfo();
            if (hardwareInfo == null) continue;
            List<HardwarePaTable> paInfo = hardwareInfo.getPaInfo();
            if (paInfo == null) continue;
            for (HardwarePaTable pa : paInfo) {
                int columncount = tableModel.getColumnCount();
                Object[] cols = new Object[columncount];
                // 1列目はProfilerEprofData：非表示
                cols[0] = info;
                // スレッド番号
                cols[1] = pa.getThreadno();
                // ハードウェアモニタ情報（ＰＡ情報）テーブル
                double[] patable = pa.getPaTable();
                for (int i=0; i<patable.length; i++) {
                    if (i+2>=columncount) break;
                    String text = formatDouble(patable[i]);
                    cols[i+2] = text;
                }
                tableModel.addRow(cols);
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
        // ファイル名 + グループ名
        String[] keys = key.split("/");
        if (keys == null || keys.length != 2) return null;
        // Eprofの場合, グループ名
        String subtitle = keys[1];
        if (this.mapInfo.size() > 1) {
            // 複数ファイルが存在する場合は、ファイル名を付加する。
            subtitle += ":" + keys[0];
        }
        return subtitle;
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
        if (!(this.selectedInfo instanceof ProfilerEprofData)) return null;
        ProfilerEprofData info = (ProfilerEprofData)this.selectedInfo;
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

        HardwareMonitorInfo hardwareInfo = info.getHardwareInfo();
        if (hardwareInfo == null) return null;
        List<HardwarePaTable> paInfo = hardwareInfo.getPaInfo();
        if (paInfo == null) return null;
        for (HardwarePaTable pa : paInfo) {
            if (visibledcolumns[1]) {
                // スレッド番号
                buf.append(pa.getThreadno());
                buf.append(", ");
            }
            // ハードウェアモニタ情報（ＰＡ情報）テーブル
            double[] patable = pa.getPaTable();
            for (int i=0; i<patable.length; i++) {
                if (visibledcolumns[i+2]) {
                    String text = formatDouble(patable[i]);
                    buf.append(text);
                    buf.append(", ");
                }
            }
            buf.delete(buf.length()-2, buf.length());
            buf.append("\n");
        }
        return buf.toString();
    }

    /**
     * double値を表示用テキストに書式化する.
     * @param value		double値
     * @return			書式化テキスト
     */
    private String formatDouble(double value) {
        String text = null;
        if (value >= MAX_EXPONENT) {
            // 指数表示
            DecimalFormat decimal = new DecimalFormat("0.000E00");
            text = decimal.format(value);
        }
        else {
            // 小数点表示
            DecimalFormat decimal = new DecimalFormat("#.###");
            text = decimal.format(value);
        }
        return text;
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


