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
 * 配列式を表現するクラス
 * @author RIKEN
 *
 */
public class ArrayExpression extends Substitution {
    /** シリアル番号 */
    private static final long serialVersionUID = -8525500860227966833L;
    /**
     * コンストラクタ。
     */
    public ArrayExpression() {
        super();
    }
    /**
     * コンストラクタ。
     * @param mama 親ブロック
     */
    public ArrayExpression(Block mama) {
        super(mama);
    }

}