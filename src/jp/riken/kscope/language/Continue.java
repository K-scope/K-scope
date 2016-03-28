/*
 * K-scope
 * Copyright 2012-2015 RIKEN, Japan
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

package jp.riken.kscope.language;

/**
 * 繰り返し処理の最後に飛ぶ制御文クラス。
 *
 * @author RIKEN
 * @version    2015/03/15     C言語, FortranにてtoString出力文字列の変更
 *
 */
public class Continue extends jp.riken.kscope.language.Block {
    /** シリアル番号 */
    private static final long serialVersionUID = 1600666700399053469L;
    /** ラベル */
    private String label;

    /**
     * コンストラクタ。
     *
     * @param parent
     *          親ブロック
     */
    public Continue(Block parent) {
        super(parent);
    }

    /**
     * コンストラクタ。
     *
     * @param parent
     *          親ブロック
     * @param lbl
     *          ラベル
     */
    public Continue(Block parent, String lbl) {
        super(parent);
        label = lbl;
    }

    /**
     * コンストラクタ。
     * @param lbl       ラベル
     */
    public Continue(String lbl) {
        label = lbl;
    }

    /**
     * コンストラクタ。
     */
    public Continue() {
        super();
    }

    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.CONTINUE
     */
    public BlockType getBlockType() {
        return BlockType.CONTINUE;
    }

    /**
     * ラベルの取得。
     *
     * @return ラベル
     */
    public String getLabel() {
        return label;
    }

    @Override
    public String toString() {
        return this.toStringBase();
    }
    @Override
    protected String toStringBase() {
        if (this.isFortran()) {
            if (this.label == null) {
                return "cycle";
            } else {
                return "cycle " + this.label;
            }
        }
        else {
            return "continue";
        }
    }

}
