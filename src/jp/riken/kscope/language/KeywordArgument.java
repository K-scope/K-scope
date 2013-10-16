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
 * キーワード引数を表現するためのクラス。
 * @author RIKEN
 *
 */
public class KeywordArgument extends Expression {
	/** シリアル番号 */
	private static final long serialVersionUID = 7613378389238504275L;
    private String keyword = "";

    /**
     * コンストラクタ。
     */
    public KeywordArgument() {
        super();
    }

    /**
     * キーワードを取得する。
     * @return キーワード
     */
    public String getKeyword() {
        return keyword;
    }

    /**
     * キーワードをセットする。
     * @param key キーワード
     */
    public void setKeyword(String key) {
        this.keyword = key;
    }

    /**
     * コンストラクタ
     * @param key		キーワード
     * @param expr		コピー元式クラス
     */
    public KeywordArgument(String key, Expression expr) {
        this.keyword = key;

        this.setLine(expr.getLine());
        this.setVariableType(expr.getType());
        this.getVariables().addAll(expr.getVariables());
        this.getFuncCalls().addAll(expr.getFuncCalls());
        this.setAddCount(expr.getAddCount());
        this.setSubCount(expr.getSubCount());
        this.setMulCount(expr.getMulCount());
        this.setDivCount(expr.getDivCount());
        this.setPowCount(expr.getPowCount());
    }
}
