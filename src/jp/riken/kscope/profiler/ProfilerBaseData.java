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

import java.util.ArrayList;
import java.util.List;

import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;

/**
 * プロファイラデータ基底クラス
 * @author riken
 */
public abstract class ProfilerBaseData {

    /** コード行情報 */
    private CodeLine code;
    /** ブロック情報 */
    private List<IBlock[]> areas;
    /** プロファイラ情報タイプ識別子 */
    private PROFILERINFO_TYPE infoType;

    /**
     * コンストラクタ
     */
    public ProfilerBaseData() {
        this.code = null;
        this.areas = null;
    }


    /**
     * コード行情報を取得する
     * @return		コード行情報
     */
    public CodeLine getCodeLine() {
        return code;
    }

    /**
     * コード行情報を設定する
     * @param code		コード行情報
     */
    public void setCodeLine(CodeLine code) {
        this.code = code;
    }

    /**
     * ブロック情報を取得する
     * @return		ブロック情報
     */
    public List<IBlock[]> getAreas() {
        return this.areas;
    }

    /**
     * ブロック情報を設定する
     * @param areas		ブロック情報
     */
    public void setAreas(List<IBlock[]> areas) {
        this.areas = new ArrayList<IBlock[]>();
        this.areas.addAll(areas);
    }

    /**
     * ブロック情報を取得する
     * @return		ブロック情報
     */
    public IBlock[] getBlocks() {
        if (this.areas == null || this.areas.size() <= 0) return null;
        IBlock[] blocks = this.areas.get(0);
        return blocks;
    }

    /**
     * ブロック情報を設定する
     * @param blocks		ブロック情報
     */
    public void setBlocks(IBlock[] blocks) {
        if (blocks == null) return;
        this.areas = new ArrayList<IBlock[]>();
        this.areas.add(blocks);
    }

    /**
     * ブロック情報を取得する
     * @return		ブロック情報
     */
    public IBlock getBlock() {
        if (this.areas == null || this.areas.size() <= 0) return null;
        IBlock[] blocks = this.areas.get(0);
        return blocks[0];
    }

    /**
     * ブロック情報を設定する
     * @param block		ブロック情報
     */
    public void setBlock(IBlock block) {
        if (block == null) return;
        this.areas = new ArrayList<IBlock[]>();
        IBlock[] blocks = new IBlock[]{block};
        this.areas.add(blocks);
    }

    /**
     * コスト情報タイプ識別子を取得する
     * @param type コスト情報タイプ識別子
     */
    public void setInfoType(PROFILERINFO_TYPE type) {
        this.infoType = type;
    }

    /**
     * コスト情報タイプ識別子を設定する
     * @return コスト情報タイプ識別子
     */
    public PROFILERINFO_TYPE getInfoType() {
        return this.infoType;
    }


}
