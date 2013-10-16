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
 * モジュールプログラム単位を表現するクラス。
 *
 * @author RIKEN
 *
 */
public class Module extends ProgramUnit {
	/** シリアル番号 */
	private static final long serialVersionUID = -9149612813518825146L;

    /**
     * コンストラクタ。
     *
     * @param m_name
     *            モジュール名
     */
    public Module(String m_name) {
        super("module", m_name);
    }

    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.MODULE
     */
    @Override
    public BlockType getBlockType() {
        return BlockType.MODULE;
    }

    @Override
    protected String toStringBase() {
        return ("Module " + this.get_name());
    }

    /**
     * モジュールに属するプログラム単位の配列を返す。
     *
     * @return プログラム単位の配列。無ければ空の配列を返す。
     */
    public Procedure[] get_procedures() {
        Procedure[] subs = new Procedure[super.get_num_of_child()];
        for (int i = 0; i < subs.length; i++) {
            subs[i] = get_children()[i];
        }
        return subs;
    }

    protected Procedure get_procedure(String sub_name) {
        return ((Procedure) super.get_child(sub_name));
    }

    /**
     * IDを取得する。
     *
     * @return ID
     */
    @Override
    public String getID() {
        return this.toStringBase();
    }
}
