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

package jp.riken.kscope.profiler;

import java.awt.Color;
import java.math.BigDecimal;

import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.gui.ISourceBargraph;
import jp.riken.kscope.properties.ProfilerProperties;

/**
 * 基本プロファイラ(DProf)データクラス.
 * Dprofのプロファイラデータ
 * @author riken
 */
public class ProfilerDprofData extends ProfilerBaseData implements ISourceBargraph {

    /** シンボル名/カウンタグループ名:Dprof */
    private String symbol;
    // Dprof
    /** ネストレベル:Dprof  */
    private int nestLevel;
    /** サンプリング回数:Dprof */
    private float sampling;
    /** 全体に対する割合:Dprof */
    private float ratio;
    /** 累計サンプリング回数:Dprof */
    private float sumSampling;

    /**
     * コンストラクタ
     */
    public ProfilerDprofData() {
        super();
        this.symbol = null;
        this.sampling = 0.0F;
        this.ratio = 0.0F;
    }

    /**
     * シンボル名を取得する
     * @return		シンボル名
     */
    public String getSymbol() {
        return symbol;
    }

    /**
     * シンボル名を設定する
     * @param name		シンボル名
     */
    public void setSymbol(String name) {
        this.symbol = name;
    }

    /**
     * サンプリング回数を取得する
     * @return		サンプリング回数
     */
    public float getSampling() {
        return sampling;
    }

    /**
     * サンプリング回数を設定する
     * @param count		サンプリング回数
     */
    public void setSampling(float count) {
        this.sampling = count;
    }

    /**
     * 全体に対する割合を設定する
     * @return		全体に対する割合
     */
    public float getRatio() {
        return ratio;
    }

    /**
     * 全体に対する割合を設定する
     * @param value		全体に対する割合
     */
    public void setRatio(float value) {
        this.ratio = value;
    }

    /**
     * バーグラフ横に表示する文字列を取得する
     * @return		表示文字列
     */
    @Override
    public String getBarText() {
        // 少数点以下2桁表示

        float value = new BigDecimal(String.valueOf(this.getRatio()*100)).setScale(ProfilerProperties.COST_RATIO_SCALE, BigDecimal.ROUND_HALF_UP).floatValue();
        int scale = ProfilerProperties.COST_RATIO_SCALE;
        if (value >= 100.0) {
        	scale = 1;
        }
        String format = "%.0" + scale + "f%%";
        String text = String.format(format, value);
        return text;
    }

    /**
     * バーグラフの値を取得する.
     * Max=1.0 〜 Min=0.0
     * @return		バーグラフの値
     */
    @Override
    public float getBarValue() {
        return this.ratio;
    }

    /**
     * バーグラフの対象ソースファイルを取得する
     * @return		対象ソースファイル
     */
    @Override
    public SourceFile getSourceFile() {
        if (this.getCodeLine() == null) return null;
        return this.getCodeLine().getSourceFile();
    }

    /**
     * バーグラフの表示色を取得する
     * @return		バーグラフ表示色
     */
    @Override
    public Color getBarColor() {
        return this.getInfoType().getBarColor();
    }


    /**
     * ネストレベル:Dprof
     * @return ネストレベル:Dprof
     */
    public int getNestLevel() {
        return nestLevel;
    }

    /**
     * ネストレベル:Dprof
     * @param nest ネストレベル:Dprof
     */
    public void setNestLevel(int nest) {
        this.nestLevel = nest;
    }

    /**
     * 累計サンプリング回数
     * @return 累計サンプリング回数
     */
    public float getSumSampling() {
        return sumSampling;
    }

    /**
     * 累計サンプリング回数
     * @param count      累計サンプリング回数
     */
    public void setSumSampling(float count) {
        this.sumSampling = count;
    }

    /**
     * バーグラフのタイプ名を取得する
     * @return		バーグラフタイプ名
     */
	@Override
	public String getTypeName() {
        return this.getInfoType().getShortName();
	}

}


