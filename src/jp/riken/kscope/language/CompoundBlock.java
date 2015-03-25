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
 * 複文クラス。
 *
 * @author RIKEN
 * @version    2015/03/15     C言語用複文（空文）新規作成
 *
 */
public class CompoundBlock extends jp.riken.kscope.language.Block {

    /** シリアル番号 */
    private static final long serialVersionUID = 8400596882091353264L;

    /**
     * コンストラクタ。
     *
     * @param parent
     *          親ブロック
     */
    public CompoundBlock(Block parent) {
        super(parent);
    }

    /**
     * コンストラクタ。
     */
    public CompoundBlock() {
        super();
    }

    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.DO_NOTHING
     */
    public BlockType getBlockType() {
        return BlockType.COMPOUND;
    }

    @Override
    public String toString() {
        return this.toStringBase();
    }

    @Override
    protected String toStringBase() {
        return "{";
    }
}
