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
 * Directiveに対応したクラス。
 *
 * @author RIKEN
 *
 */
public class Directive extends jp.riken.kscope.language.Block {
    /** シリアル番号 */
    private static final long serialVersionUID = -1028405799509858653L;
    private String argument;

    /**
     * コンストラクタ。
     *
     * @param parent
     *          親ブロック
     */
    public Directive(Block parent) {
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
    public Directive(Block parent, String argmnt) {
        super(parent);
        argument = argmnt;
    }

    /**
     * コンストラクタ。
     */
    public Directive() {
        super();
    }

    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.DIRECTIVE
     */
    public BlockType getBlockType() {
        return BlockType.DIRECTIVE;
    }

    /**
     * 本文のセット。
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


    /**
     * Directiveクラスのコピーの作成を行う
     * @param    dest    コピー元Directive
     */
    public Directive clone()  {
        Directive dest = new Directive();
        dest.clone(this);
        dest.argument = this.argument;
        return dest;
    }
}
