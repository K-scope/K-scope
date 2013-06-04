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
 * プログラムを一時停止する制御文に対応したクラス。
 * 
 * @author RIKEN
 * 
 */
public class Pause extends jp.riken.kscope.language.Block {
	/** シリアル番号 */
	private static final long serialVersionUID = -2492364246002232811L;
    private String argument;

    /**
     * コンストラクタ。
     *
     * @param parent
     *          親ブロック
     */
    public Pause(Block parent) {
        super(parent);
    }

    /**
     * コンストラクタ。
     * 
     * @param parent
     *            親ブロック
     * @param argmnt
     *            引数（メッセージ等）
     */
    public Pause(Block parent, String argmnt) {
        super(parent);
        argument = argmnt;
    }

    /**
     * コンストラクタ。
     */
    public Pause() {
        super();
    }

    /**
     * ブロックタイプの取得。
     * 
     * @return BlockType.PAUSE
     */
    public BlockType getBlockType() {
        return BlockType.PAUSE;
    }

    /**
     * 引数のセット。
     * 
     * @param str
     *            引数
     */
    public void setArgument(String str) {
        this.argument = str;
    }

    /**
     * 引数（メッセージ等）の取得。
     * 
     * @return 引数（メッセージ等）
     */
    public String getArgument() {
        return argument;
    }

    @Override
    public String toString() {
        return this.toStringBase();
    }
    @Override
    protected String toStringBase() {
        if (this.argument == null) {
            return "pause";
        } else {
            return "pause " + this.argument;
        }
    }
}
