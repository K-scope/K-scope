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

package jp.riken.kscope.profiler.common;

/**
 * マジックキー情報を保持する
 *
 * @author RIKEN
 */
public class MagicKey {
    private String id;
    private short add_mode;
    private short ver;

    /**
     * ファイル識別文字を取得する
     * @return ファイル識別文字 “DPRF” or “RPRF”or“GPRF”
     */
    public String getId() {
        return id;
    }

    /**
     * アドレスモードを取得する
     * @return アドレスモード 0:32bit  1:64bit
     */
    public short getAdd_mode() {
        return add_mode;
    }

    /**
     * バージョンを取得する
     * @return バージョン
     *<br>
     *<br>
     *ver（上位１バイト：製品種別,下位１バイト：バージョン(連番)）<br>
     *0x0001 (理研版・PL-PACK V2)<br>
     *0x0101 (Pleiades版)<br>
     *0x0102 (PQ V2)<br>
     *0x0201 (EM64T版=PG V1,V2)<br>
     *0x0301 (PW V3)<br>
     *0x0302 (PG V3)<br>
     *0x0401 (PETA 互換プロファイラ、VarunaGE)<br>
     *0x0411 (PETA 基本プロファイラ、VarunaGE)<br>
     *0x0501 (PCC  基本プロファイラ)<br>
     *0x0412 (PETA 基本プロファイラ、VarunaGE PT3)<br>
     *
     */
    public short getVer() {
        return ver;
    }

    /**
     * ファイル識別文字を設定する
     * @param id
     *            設定するファイル識別文字 “DPRF” or “RPRF”or“GPRF”
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * アドレスモードを設定する
     * @param add_mode
     *            設定するアドレスモード 0:32bit  1:64bit
     */
    public void setAdd_mode(short add_mode) {
        this.add_mode = add_mode;
    }

    /**
     * バージョンを設定する
     * @param ver
     *            設定するバージョン
     */
    public void setVer(short ver) {
        this.ver = ver;
    }

}
