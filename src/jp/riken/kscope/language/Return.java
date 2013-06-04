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

package jp.riken.kscope.language;

/**
 * 手続きの終了を表すクラス。
 * 
 * @author RIKEN
 * 
 */
public class Return extends Block {
	/** シリアル番号 */
	private static final long serialVersionUID = 2144326483810706905L;
    /**
     * 
     * コンストラクタ。
     */
    Return() {
        super();
    }

    /**
     * 
     * コンストラクタ。
     */
    Return(Block mama) {
        super(mama);
    }
    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.RETURN
     */
    public BlockType getBlockType() {
        return BlockType.RETURN;
    }
    @Override
    public String toString() {
        return this.toStringBase();
    }
    @Override
    protected String toStringBase() {
        return "return";
    }
}
