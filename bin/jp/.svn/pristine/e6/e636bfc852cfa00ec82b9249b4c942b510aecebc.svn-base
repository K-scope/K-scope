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

package jp.riken.kscope.language.generic;

import java.io.Serializable;
import java.util.List;

import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.ProcedureUsage;

/**
 * 総称関数の仮引数リストクラス。
 *
 * @author RIKEN
 *
 */
public class Arguments extends java.util.ArrayList<Argument> implements
        Serializable {
	/** シリアル番号 */
	private static final long serialVersionUID = -6787020569904689390L;

    /**
     * 引数の型と属性が適合しているか。
     *
     * @param procedure
     *         対象となる関数
     *
     * @return true:  適合している。<br>
     *         false: 適合していない。
     */
    public boolean isSameArguments(ProcedureUsage procedure) {
        if (procedure == null) { return false; }
        return this.isSameArguments(procedure.getArguments());
    }

    /**
     * 実引数の型と属性が適合しているか。
     *
     * @param actualArguments
     *         実引数リスト
     *
     * @return true:  適合している。<br>
     *         false: 適合していない。
     */
    public boolean isSameArguments(List<Expression> actualArguments) {
        boolean result = true;
        if (actualArguments == null) { return false; }
        if (actualArguments.size() > this.size()) { return false; }

        int index = 0;
        for (int i = 0; i < actualArguments.size(); i++) {
            if (!actualArguments.get(i).getType().matches(
                    this.get(i).getType())) {
                result = false;
                index = i;
                break;
            }
        }

        if (result && index < this.size()) {
            if (this.get(index).getAttribute()
                    instanceof
                    jp.riken.kscope.language.fortran.VariableAttribute) {
                jp.riken.kscope.language.fortran.VariableAttribute attribute
                  = (jp.riken.kscope.language.fortran.VariableAttribute)
                    this.get(index).getAttribute();
                result = attribute.hasOptional();
            }
        }

        return result;
    }

}
