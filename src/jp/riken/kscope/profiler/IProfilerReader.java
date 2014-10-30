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

import java.io.File;

/**
 * プロファイラファイルの読込クラス
 * @author RIKEN
 */
public interface IProfilerReader {

    /**
     * プロファイラファイルから読み込みを行う
     * @param profilerfile		プロファイラファイル
     * @param endian			エンディアン設定　LITTLE_ENDIAN:0x00 BIG_ENDIAN:0x01;
     * @throws Exception   読込例外
     */
    public void readFile(File profilerfile, int endian) throws Exception;

    /**
     * プロファイラファイルから読み込みを行う
     * @param profilerfile		プロファイラファイル
     * @throws Exception		読込例外
     */
    public void readFile(File profilerfile) throws Exception;

    /**
     * ファイルのエンディアンを設定する
     * @param endian		エンディアン設定　LITTLE_ENDIAN:0x00 BIG_ENDIAN:0x01;
     */
    public void setEndian(int endian);

    /**
     * 読込プロファイラファイル
     * @return 読込プロファイラファイル
     */
    public File getProfFile();

    /**
     * コスト情報リスト:ラインを取得する
     * @return		コスト情報リスト:ライン(Dprof)
     */
    public ProfilerDprofData[] getCostInfoLine();


    /**
     * コスト情報リスト:ループを取得する
     * @return		コスト情報リスト:ループ(Dprof)
     */
    public ProfilerDprofData[] getCostInfoLoop();

    /**
     * コスト情報リスト:手続を取得する
     * @return		コスト情報リスト:手続(Dprof)
     */
    public ProfilerDprofData[] getCostInfoProcedure();

    /**
     * コールグラフ情報(Dprof)を取得する
     * @return		コールグラフ情報(Dprof)
     */
    public ProfilerDprofData[] getDprofCallGraphInfo();

    /**
     * イベントカウンタ情報(Eprof)を取得する
     * @return		イベントカウンタ情報(Eprof)
     */
    public ProfilerEprofData[] getEprofEventCounterInfo();

    /**
     * プロファイラマジックキーを取得する
     * @return		マジックキー
     */
    public String getFileType();

    /**
     * PAイベント指定値(EPRFのみ)を取得する.
     *     Cache
     *     Instructions
     *     MEM_access
     *     Performance
     *     Statistics
     * @return counterGroup		PAイベント指定値(EPRFのみ)
     */
    public String getPaEventName();
}


