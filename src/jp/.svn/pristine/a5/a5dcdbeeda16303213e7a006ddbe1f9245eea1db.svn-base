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

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;

/**
 * 詳細プロファイラ測定区間情報クラス
 * @author riken
 */
public class ProfilerMeasureInfo {

    /** 測定区間データリスト */
    private List<MeasureData> listMeasure;

    /**
     * コンストラクタ
     */
    public ProfilerMeasureInfo() {
        this.listMeasure = new ArrayList<MeasureData>();
    }

    /**
     * 測定区間データを追加する
     * @param data		測定区間データ
     * @return    追加測定区間データ
     */
    public MeasureData addMeasureData(MeasureData data) {
        this.listMeasure.add(data);
        return data;
    }

    /**
     * 測定区間データを追加する
     *
     * @param code		測定区間
     * @param name		グループ名
     * @return    追加測定区間データ
     */
    public MeasureData addMeasureData(CodeLine code, String name) {
        MeasureData data = new MeasureData(name, code);
        this.listMeasure.add(data);
        return data;
    }

    /**
     * 測定区間データを追加する
     *
     * @param code		測定区間
     * @param name		グループ名
     * @param number		詳細番号
     * @param level		プライオリティレベル
     * @return    追加測定区間データ
     */
    public MeasureData addMeasureData(CodeLine code, String name, String number, String level) {
        MeasureData data = new MeasureData(name, code);
        if (number != null) {
            data.setNumber(number);
        }
        if (level != null) {
            data.setLevel(level);
        }
        this.listMeasure.add(data);
        return data;
    }

    /**
     * 測定区間データを追加する
     *
     * @param blocks		測定区間{開始ブロック〜終了ブロック}
     * @param name		グループ名
     * @return    追加測定区間データ
     */
    public MeasureData addMeasureData(IBlock[] blocks, String name) {
        MeasureData data = new MeasureData(name, blocks);
        this.listMeasure.add(data);
        return data;
    }

    /**
     * 測定区間データを追加する
     *
     * @param blocks		測定区間{開始ブロック〜終了ブロック}
     * @param name		グループ名
     * @param number		詳細番号
     * @param level		プライオリティレベル
     * @return    追加測定区間データ
     */
    public MeasureData addMeasureData(IBlock[] blocks, String name, String number, String level) {
        MeasureData data = new MeasureData(name, blocks);
        if (number != null) {
            data.setNumber(number);
        }
        if (level != null) {
            data.setLevel(level);
        }
        this.listMeasure.add(data);
        return data;
    }
    /**
     * 測定区間データリストを追加する
     * @param list		測定区間データリスト
     */
    public void addMeasureData(List<MeasureData> list) {
        this.listMeasure.addAll(list);
    }

    /**
     * 測定区間データリストを設定する
     * @param list		測定区間データリスト
     */
    public void setMeasureList(List<MeasureData> list) {
        this.listMeasure = new ArrayList<MeasureData>();
        this.listMeasure.addAll(list);
    }

    /**
     * 測定区間データリストを取得する
     * @return		測定区間データリスト
     */
    public List<MeasureData> getMeasureList() {
        return this.listMeasure;
    }

    /**
     * 測定区間データリスト数を取得する
     * @return		測定区間データリスト数
     */
    public int getMeasureDataCount() {
        if (this.listMeasure == null) return 0;
        return this.listMeasure.size();
    }

    /**
     * 測定区間データを取得する
     * @param id		インデックス
     * @return		測定区間データ
     */
    public MeasureData getMeasureData(int id) {
        if (this.listMeasure == null) return null;
        if (this.listMeasure.size() <= id) return null;
        return this.listMeasure.get(id);
    }

    /**
     * 測定区間データリストをクリアする。
     */
    public void clearMeasureInfo() {
        if (this.listMeasure == null) return;
        this.listMeasure = new ArrayList<MeasureData>();
    }

    /**
     * 測定区間データリストからデータを削除する
     * @param id		削除インデックス
     */
    public void removeMeasureData(int id) {
        if (this.listMeasure == null) return;
        if (this.listMeasure.size() <= id) return;
        this.listMeasure.remove(id);
    }

    /**
     * 測定区間データリストからデータを削除する
     * @param data		削除測定区間データ
     */
    public void removeMeasureData(MeasureData data) {
        if (this.listMeasure == null) return;
        this.listMeasure.remove(data);
    }


    /**
     * 測定区間データリストから同一グループ名が存在するかチェックする
     * @param name		 グループ名
     * @return		true=同一グループ名が存在する
     */
    public boolean containsMeasureData(String name) {
        if (this.listMeasure == null) return false;
        if (name == null) return false;
        for (MeasureData data : this.listMeasure) {
            String group = data.getGroupname();
            if (group.equals(name)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 測定区間データクラス
     */
    public class MeasureData {
        /** グループ名 */
        private String groupname;
        /** 詳細番号 */
        private String number;
        /** プライオリティレベル */
        private String level;
        /** 測定区間挿入行 */
        private CodeLine measureCodeLine;
        /** 測定区間挿入ブロック */
        private IBlock[] measureBlocks;

        /**
         * コンストラクタ
         * @param code		測定区間挿入行情報
         */
        public MeasureData(CodeLine code) {
            this.measureCodeLine = code;
        }

        /**
         * コンストラクタ
         * @param blocks		測定区間挿入ブロック {開始ブロック〜終了ブロック}
         */
        public MeasureData(IBlock[] blocks) {
            this.measureBlocks = blocks;
        }


        /**
         * コンストラクタ
         * @param name		グループ名
         * @param code		測定区間挿入行情報
         */
        public MeasureData(String name, CodeLine code) {
            this.groupname = name;
            this.measureCodeLine = code;
        }

        /**
         * コンストラクタ
         * @param name		グループ名
         * @param blocks		測定区間挿入ブロック{開始ブロック〜終了ブロック}
         */
        public MeasureData(String name, IBlock[] blocks) {
            this.groupname = name;
            this.measureBlocks = blocks;
        }

        /**
         * グループ名を取得する
         * @return		グループ名
         */
        public String getGroupname() {
            return groupname;
        }

        /**
         * グループ名を取得する
         * @param name		グループ名
         */
        public void setGroupname(String name) {
            this.groupname = name;
        }

        /**
         * 測定区間挿入行を取得する
         * @return 測定区間挿入行
         */
        public CodeLine getMeasureCodeLine() {
            return this.measureCodeLine;
        }

        /**
         * 測定区間挿入行を設定する。
         * @param code		測定区間挿入行
         */
        public void setMeasureCodeLine(CodeLine code) {
            this.measureCodeLine = code;
        }

        /**
         * 測定区間挿入ブロックを取得する
         * @return 測定区間挿入ブロック
         */
        public IBlock[] getMeasureBlocks() {
            return this.measureBlocks;
        }

        /**
         * 測定区間挿入ブロックを設定する。
         * @param blocks		測定区間挿入ブロック{開始ブロック〜終了ブロック}
         */
        public void setMeasureBlocks(IBlock[] blocks) {
            this.measureBlocks = blocks;
        }

        /**
         * 詳細番号
         * @return 詳細番号
         */
        public String getNumber() {
            return number;
        }

        /**
         * 詳細番号
         * @param number 詳細番号
         */
        public void setNumber(String number) {
            this.number = number;
        }

        /**
         * プライオリティレベル
         * @return     プライオリティレベル
         */
        public String getLevel() {
            return level;
        }

        /**
         * プライオリティレベル
         * @param level プライオリティレベル
         */
        public void setLevel(String level) {
            this.level = level;
        }

        /**
         * 測定区間挿入範囲を取得する.
         * 測定区間挿入行, 測定区間挿入ブロックから挿入範囲を取得する.
         * @return 測定区間挿入範囲
         */
        public CodeLine getMeasureArea() {
            if (this.measureCodeLine != null) {
                return this.measureCodeLine;
            }
            else if (this.measureBlocks != null) {
                // 開始ブロック
                CodeLine startcode = new CodeLine(this.measureBlocks[0].getStartCodeLine(), this.measureBlocks[0].getEndCodeLine());
                // 終了ブロック
                CodeLine endtcode = new CodeLine(this.measureBlocks[this.measureBlocks.length-1].getStartCodeLine(), this.measureBlocks[this.measureBlocks.length-1].getEndCodeLine());
                CodeLine area = new CodeLine(startcode, endtcode);
                return area;
            }

            return null;
        }

        /**
         * パラメータを文字列出力する
         * @return		パラメータ
         */
        public String toStringParam() {
            StringBuffer buf = new StringBuffer();
            /** グループ名 */
            if (this.groupname != null && !this.groupname.isEmpty()) {
                String rep_name = this.groupname;
                // 削除 2012/05/21：ダブルクォートで囲んでいたらダブルクォートは付けない。
  //              if (!(rep_name.startsWith("\"") && rep_name.endsWith("\""))) {
  //                  rep_name = "\"" + rep_name + "\"";
  //              }
                buf.append(rep_name);
            }
            /** 詳細番号 */
            if (this.number != null && !this.number.isEmpty()) {
                if (buf.length() > 0) buf.append(", ");
                buf.append(this.number);
            }
            /** プライオリティレベル */
            if (this.level != null && !this.level.isEmpty()) {
                if (buf.length() > 0) buf.append(", ");
                buf.append(this.level);
            }
            return buf.toString();
        }
    }
}



