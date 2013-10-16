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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.information.InformationBlocks;

/**
 *
 * 式を表現するためのクラス.
 *
 * @author RIKEN
 *
 */
public class Expression implements Serializable {
    /** シリアル番号 */
    private static final long serialVersionUID = -9199930499046575735L;
    /** 式の文字列表現 */
    private String line;
    /** データ型 */
    private IVariableType type;
    /** 変数リスト */
    private List<Variable> variables;
    /** 呼出関数リスト */
    private List<ProcedureUsage> funcCalls;
    /** 加算カウント */
    private int addCount = 0;
    /** 減算カウント */
    private int subCount = 0;
    /** 乗算カウント */
    private int mulCount = 0;
    /** 除算カウント */
    private int divCount = 0;
    /** べき算カウント */
    private int powCount = 0;
    /** 変数の親ブロック : 変数が属する代入文(Substitution),構文 */
    private IBlock parentStatement;

    /**
     * コンストラクタ.
     */
    public Expression() {
        variables = new ArrayList<Variable>();
        funcCalls = new ArrayList<ProcedureUsage>();
    }
    /**
     * コンストラクタ.
     *
     * @param val
     *   対象となる式
     */
    public Expression(String val) {
        line = val;
        variables = new ArrayList<Variable>();
        funcCalls = new ArrayList<ProcedureUsage>();
    }


    /**
     * 式の結果のデータ型をセットする。
     *
     * @param tp 結果の型
     */
    public void setVariableType(IVariableType tp) {
        type = tp;
    }
    /**
     * 式の結果のデータ型を返す。
     * @return データ型
     */
    public IVariableType getType() {
        return type;
    }

    /**
     * 式に現れる変数のリストをセットする。
     * @param vars 変数のリスト
     */
    public void setVariables(List<Variable> vars) {
        this.variables = vars;
    }
    /**
     * 式に現れる変数を追加する。
     * @param var 変数
     */
    public void addVariable(Variable var) {
        variables.add(var);
    }

    /**
     * 式に現れる変数のリストを返す。
     *
     * @return 変数のリスト。無ければ空のリストを返す。
     */
    public List<Variable> getVariables() {
        return variables;
    }

    /**
     * 式に含まれる全ての変数のセットを返す。変数の添字・手続呼出の添字も対象とする。 再帰呼出。
     *
     * @return 変数のセット。無ければ空のセットを返す。
     */
    public Set<Variable> getAllVariables() {
        Set<Variable> vars = new HashSet<Variable>(this.variables);
        for (Variable var: this.variables) {
            vars.addAll(var.getAllVariables());
        }
        List<ProcedureUsage> calls = this.funcCalls;
        if (calls != null) {
            for (ProcedureUsage call : calls) {
                List<Expression> args = call.getArguments();
                for (Expression arg : args) {
                    vars.addAll(arg.getAllVariables());
                }
            }
        }
        return vars;
    }
    /**
     * 式に含まれる全ての手続呼出のセットを返す。変数および手続呼出の添字も対象とする。 再帰呼び出し。
     *
     * @return 手続呼出のセット。無ければ空のセットを返す。
     */
    public Set<ProcedureUsage> getAllFunctions() {
        Set<ProcedureUsage> pus = new HashSet<ProcedureUsage>();
        for (Variable var: this.variables) {
        	Set<ProcedureUsage> list = var.getAllFunctions();
            if (list != null && list.size() > 0) {
                pus.addAll(list);
            }
        }
        pus.addAll(this.funcCalls);
        for (ProcedureUsage call : this.funcCalls) {
            List<Expression> args = call.getArguments();
            for (Expression arg : args) {
                pus.addAll(arg.getAllFunctions());
            }
        }
        return pus;
    }

    /**
     * 式に現れる関数呼び出しのリストをセットする。
     * @param fCalls 関数呼び出しのリスト
     */
    public void setFuncCalls(List<ProcedureUsage> fCalls) {
        this.funcCalls = fCalls;
    }
    /**
     * 式に現れる関数呼び出しを追加する。
     * @param call 関数呼び出し
     */
    public void addFuncCall(ProcedureUsage call) {
        funcCalls.add(call);
    }

    /**
     * 右辺に含まれる関数呼び出しのリストを返す。
     *
     * @return 関数呼び出しのリスト。無ければ空のリストを返す。
     */
    public List<ProcedureUsage> getFuncCalls() {
        return funcCalls;
    }

    /**
     * 対象となる式を返す.
     *
     * @return
     *   対象となる式
     */
    @Override
    public String toString() {
        return line;
    }
    /**
     * 式の文字列表現を返す。
     * @return 式の文字列表現
     */
    public String getLine() {
        return line;
    }
    /**
     * 式の文字列表現をセットする。
     * @param ln 式の文字列表現
     */
    public void setLine(String ln) {
        this.line = ln;
    }


    /** 加算カウント. */
    public void incrementAdd() {
        addCount++;
    }
    /** 減算カウント. */
    public void incrementSub() {
        subCount++;
    }
    /** 乗算カウント. */
    public void incrementMul() {
        mulCount++;
    }
    /** 除算カウント. */
    public void incrementDiv() {
        divCount++;
    }
    /** 累算カウント. */
    public void incrementPow() {
        powCount++;
    }

    /** 加算カウントを返す。
     * @return 加算カウント
     */
    public int getAddCount() {
        return addCount;
    }
    /** 減算カウントを返す。
     * @return 減算カウント
     */
    public int getSubCount() {
        return subCount;
    }
    /** 乗算カウントを返す。
     * @return 乗算カウント
     */
    public int getMulCount() {
        return mulCount;
    }
    /** 除算カウントを返す。
     * @return 除算カウント
     */
    public int getDivCount() {
        return divCount;
    }
    /** 累算カウントを返す。
     * @return 累算カウント
     */
    public int getPowCount() {
        return powCount;
    }

    /**
     * 加算カウントをセットする。
     * @param add 加算カウント
     */
    public void setAddCount(int add) {
        this.addCount = add;
    }
    /**
     * 減算カウントをセットする。
     * @param sub 加算カウント
     */
    public void setSubCount(int sub) {
        this.subCount = sub;
    }
    /**
     * 乗算カウントをセットする。
     * @param mul 乗算カウント
     */
    public void setMulCount(int mul) {
        this.mulCount = mul;
    }
    /**
     * 除算カウントをセットする。
     * @param div 除算カウント
     */
    public void setDivCount(int div) {
        this.divCount = div;
    }
    /**
     * 累算カウントをセットする。
     * @param pow 累算カウント
     */
    public void setPowCount(int pow) {
        this.powCount = pow;
    }

    /**
     * 式に、ある変数が含まれるかを内部の関数呼び出しを含めて再帰的に判定し、真偽値を返す。
     *
     * @param name
     *            変数名
     * @return 変数が含まれれば真。それ以外は偽。
     */
    public boolean hasVariable(String name) {
        List<Variable> vars = this.getVariables();
        for (Variable var : vars) {
            if (var.getName().equalsIgnoreCase(name)) {
                return true;
            }
        }
        List<ProcedureUsage> funcs = this.getFuncCalls();
        for (ProcedureUsage func : funcs) {
            List<Expression> funcArgs = func.getArguments();
            for (Expression funcArg : funcArgs) {
                if (funcArg.hasVariable(name)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * 付加情報ブロックコレクションを生成する。
     *
     * @return 付加情報ブロックコレクション
     */
    public InformationBlocks createInformationBlocks() {
        InformationBlocks result = new InformationBlocks();
        if (this.funcCalls != null) {
            for (ProcedureUsage pu : this.funcCalls) {
                result.addAll(pu.createInformationBlocks());
            }
        }
        return result;
    }
    /**
     * idにマッチした情報ブロックを検索する。
     * @param id
     *          ID
     * @return 見つかった情報ブロック。見つからなかった場合はnullが返ります。
     */
    public IInformation findInformationBlockBy(String id) {
        IInformation result = null;
        if (this.funcCalls != null) {
            for (ProcedureUsage pu : this.funcCalls) {
                result = pu.findInformationBlockBy(id);
                if (result != null) { break; }
            }
        }
        return result;
    }
    /**
     * 自身が配列式か判定する。
     * @return 真偽値。配列式であれば真
     */
    public boolean isArrayExpression() {
        List<Variable> vars = this.getVariables();
        for (Variable var: vars) {
            if (var.isArrayExpression()) {
                return true;
            }
        }
        return false;
    }

    /**
     * 同一の式であるかチェックする.
     * 式の文字列表現が同じかチェックする.
     * @param destExp		式
     * @return			true=同一
     */
	public boolean equalsExpression(Expression destExp) {
		if (destExp == null) return false;
		String thisLine = this.getLine();
		String destLine = destExp.getLine();
		if (thisLine == destLine) {
			return true;
		}
		else if (thisLine == null) {
			return false;
		}
		else if (!thisLine.equalsIgnoreCase(destLine)) {
			return false;
		}
		return true;
	}


	/**
	 * 同一ブロックを検索する
	 * @param block			IInformationブロック
	 * @return		同一ブロック
	 */
	public IInformation[] searchInformationBlocks(IInformation block) {

		List<IInformation> list = new ArrayList<IInformation>();
        if (this.funcCalls != null) {
            for (ProcedureUsage call : this.funcCalls) {
            	IInformation[] infos = call.searchInformationBlocks(block);
            	if (infos != null) {
    	        	list.addAll(Arrays.asList(infos));
    	        }
            }
        }
        if (list.size() <= 0) {
        	return null;
        }

		return list.toArray(new IInformation[0]);
	}


	/**
	 * 親ブロックを取得する.
	 * 親ブロックは変数が属する代入文(Substitution),構文とする.
	 * @return 親ブロック
	 */
	public IBlock getParentStatement() {
		return parentStatement;
	}

	/**
	 * 親ブロックを設定する.
	 * 親ブロックは変数が属する代入文(Substitution),構文とする.
	 * @param parent 親ブロック
	 */
	public void setParentStatement(IBlock parent) {
		this.parentStatement = parent;
		// 子変数に対して設定する
		if (this.variables != null) {
			for (Variable var : this.variables) {
				if (var == null) continue;
				var.setParentStatement(parent);
			}
		}
		if (this.funcCalls != null) {
			for (ProcedureUsage call : this.funcCalls) {
				if (call == null) continue;
                List<Expression> args = call.getArguments();
                for (Expression arg : args) {
                	arg.setParentStatement(parent);
                }
			}
		}
	}


    /**
     * 加算カウントを返す.
     * 子変数、添字、関数引数もカウントする.
     * @return 加算カウント
     */
    public int getOperandAddCount() {
    	int count = this.addCount;
    	if (this.variables != null) {
	        for (Variable var: this.variables) {
	        	count += var.getOperandAddCount();
	        }
    	}
        List<ProcedureUsage> calls = this.funcCalls;
        if (calls != null) {
            for (ProcedureUsage call : calls) {
                List<Expression> args = call.getArguments();
                for (Expression arg : args) {
                	count += arg.getOperandAddCount();
                }
            }
        }
        return count;
    }
    /**
     * 減算カウントを返す.
     * 子変数、添字、関数引数もカウントする.
     * @return 減算カウント
     */
    public int getOperandSubCount() {
    	int count = this.subCount;
    	if (this.variables != null) {
	        for (Variable var: this.variables) {
	        	count += var.getOperandSubCount();
	        }
    	}
        List<ProcedureUsage> calls = this.funcCalls;
        if (calls != null) {
            for (ProcedureUsage call : calls) {
                List<Expression> args = call.getArguments();
                for (Expression arg : args) {
                	count += arg.getOperandSubCount();
                }
            }
        }
        return count;
    }
    /**
     * 乗算カウントを返す.
     * 子変数、添字、関数引数もカウントする.
     * @return 乗算カウント
     */
    public int getOperandMulCount() {
    	int count = this.mulCount;
    	if (this.variables != null) {
	        for (Variable var: this.variables) {
	        	count += var.getOperandMulCount();
	        }
    	}
        List<ProcedureUsage> calls = this.funcCalls;
        if (calls != null) {
            for (ProcedureUsage call : calls) {
                List<Expression> args = call.getArguments();
                for (Expression arg : args) {
                	count += arg.getOperandMulCount();
                }
            }
        }
        return count;
    }

    /**
     * 除算カウントを返す.
     * 子変数、添字、関数引数もカウントする.
     * @return 除算カウント
     */
    public int getOperandDivCount() {
    	int count = this.divCount;
    	if (this.variables != null) {
	        for (Variable var: this.variables) {
	        	count += var.getOperandDivCount();
	        }
    	}
        List<ProcedureUsage> calls = this.funcCalls;
        if (calls != null) {
            for (ProcedureUsage call : calls) {
                List<Expression> args = call.getArguments();
                for (Expression arg : args) {
                	count += arg.getOperandDivCount();
                }
            }
        }
        return count;
    }

    /**
     * 累算カウントを返す.
     * 子変数、添字、関数引数もカウントする.
     * @return 累算カウント
     */
    public int getOperandPowCount() {
    	int count = this.powCount;
    	if (this.variables != null) {
	        for (Variable var: this.variables) {
	        	count += var.getOperandPowCount();
	        }
    	}
        List<ProcedureUsage> calls = this.funcCalls;
        if (calls != null) {
            for (ProcedureUsage call : calls) {
                List<Expression> args = call.getArguments();
                for (Expression arg : args) {
                	count += arg.getOperandPowCount();
                }
            }
        }
        return count;
    }

}
