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

package jp.riken.kscope.language.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.data.BlockList;
import jp.riken.kscope.data.OperationCount;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.Substitution;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.properties.OperationProperties;

/**
 * 演算数カウントユーティリティクラス
 * @author RIKEN
 */
public class OperationCounterUtils {

    /** 組込み関数演算カウントプロパティ */
    private OperationProperties propertiesOperation;
    /** 演算カウント結果 */
    private OperandCountResult result;

	/**
	 * コンストラクタ
	 */
	public OperationCounterUtils() {
	}

	/**
	 * コンストラクタ
	 * @param   properties    組込み関数演算カウントプロパティ
	 */
	public OperationCounterUtils(OperationProperties properties) {
		this.propertiesOperation = properties;
	}

	/**
	 * 左辺変数リストを取得する.
	 * @return		左辺変数リスト
	 */
	public List<Variable> getLeftVariables() {
		if (this.result == null) return null;
		return this.result.getLeftVar();
	}

	/**
	 * 右辺変数リストを取得する.
	 * @return		右辺変数リスト
	 */
	public List<Variable> getRightVariables() {
		if (this.result == null) return null;
		return this.result.getRightVar();
	}

	/**
	 * 左辺変数リスト数を取得する.
	 * @return		左辺変数リスト数
	 */
	public int getLeftVariableCount() {
		if (this.result == null) return 0;
		if (this.result.getLeftVar() == null) return 0;
		return this.result.getLeftVar().size();
	}

	/**
	 * 右辺変数リスト数を取得する.
	 * @return		右辺変数リスト数
	 */
	public int getRightVariableCount() {
		if (this.result == null) return 0;
		if (this.result.getRightVar() == null) return 0;
		return this.result.getRightVar().size();
	}

	/**
	 * 加算演算数を取得する.
	 * @return		加算演算数
	 */
	public int getAddCount() {
		if (this.result == null) return 0;
		return this.result.getCountAdd();
	}

	/**
	 * 減算演算数を取得する.
	 * @return		減産演算数
	 */
	public int getSubCount() {
		if (this.result == null) return 0;
		return this.result.getCountSub();
	}

	/**
	 * 乗算演算数を取得する.
	 * @return		乗算演算数
	 */
	public int getMulCount() {
		if (this.result == null) return 0;
		return this.result.getCountMul();
	}

	/**
	 * 除算演算数を取得する.
	 * @return		除算演算数
	 */
	public int getDivCount() {
		if (this.result == null) return 0;
		return this.result.getCountDiv();
	}

	/**
	 * べき算演算数を取得する.
	 * べき算数 * pow(加算+乗算)とする.
	 * @return		べき算演算数
	 */
	public int getPowCount() {
		if (this.result == null) return 0;
		int count = this.result.getCountPow();
		if (this.propertiesOperation == null) return count;
        OperationCount pow = propertiesOperation.getOperationProperty("pow");
        int flop = 1;
        if (pow != null) {
        	flop = pow.getAdd() + pow.getMul();
        }
		return count*flop;
	}

	/**
	 * 組込関数演算数を取得する.
	 * 組込関数(加算+乗算)とする.
	 * @return		関数演算数
	 */
	public int getFunctionCount() {
		if (this.result == null) return 0;
		if (this.result.getFunctions() == null) return 0;
		int count = this.result.getFunctions().size();
		if (count <= 0) return 0;
		if (this.propertiesOperation == null) return count;
		int flop = 0;
		for (ProcedureUsage func : this.result.getFunctions()) {
			String name = func.getCallName();
            OperationCount opc = propertiesOperation.getOperationProperty(name);
            if (opc != null) {
            	flop += opc.getAdd() + opc.getMul();
            }
		}
		return flop;
	}

	/**
	 * べき乗+関数演算数を取得する.
	 * @return		べき乗+関数演算数
	 */
	public int getIntrinsicCount() {
		return this.getPowCount() + getFunctionCount();
	}

	/**
	 * 加算演算FLOPを取得する.
	 * @return		加算演算FLOP
	 */
	public int getAddFlop() {
		int count = this.getAddCount();
		if (this.propertiesOperation == null) return count;
		int flop = this.propertiesOperation.getFlopAdd();
		return count*flop;
	}

	/**
	 * 減算演算FLOPを取得する.
	 * @return		減産演算FLOP
	 */
	public int getSubFlop() {
		int count = this.getSubCount();
		if (this.propertiesOperation == null) return count;
		int flop = this.propertiesOperation.getFlopSub();
		return count*flop;
	}

	/**
	 * 乗算演算FLOPを取得する.
	 * @return		乗算演算FLOP
	 */
	public int getMulFlop() {
		int count = this.getMulCount();
		if (this.propertiesOperation == null) return count;
		int flop = this.propertiesOperation.getFlopMul();
		return count*flop;
	}

	/**
	 * 除算演算FLOPを取得する.
	 * @return		除算演算FLOP
	 */
	public int getDivFlop() {
		int count = this.getDivCount();
		if (this.propertiesOperation == null) return count;
		int flop = this.propertiesOperation.getFlopDiv();
		return count*flop;
	}

	/**
	 * べき算演算FLOPを取得する.
	 * @return		べき算演算FLOP
	 */
	public int getPowFlop() {
		if (this.result == null) return 0;
		int count = this.result.getCountPow();
		if (this.propertiesOperation == null) return count;
        OperationCount pow = propertiesOperation.getOperationProperty("pow");
        int flop = 1;
        if (pow != null) {
    		int add = this.propertiesOperation.getFlopAdd();
    		int mul = this.propertiesOperation.getFlopMul();
        	flop = pow.getAdd()*add + pow.getMul()*mul;
        }
		return count*flop;
	}

	/**
	 * 関数リストFLOPを取得する.
	 * @return		関数リストFLOP
	 */
	public int getFunctionFlop() {
		int count = this.getFunctionCount();
		if (count <= 0) return 0;
		if (this.propertiesOperation == null) return count;
		int flop = 0;
		int add = this.propertiesOperation.getFlopAdd();
		int mul = this.propertiesOperation.getFlopMul();
		for (ProcedureUsage func : this.result.getFunctions()) {
			String name = func.getCallName();
            OperationCount opc = propertiesOperation.getOperationProperty(name);
            if (opc != null) {
            	flop += opc.getAdd()*add + opc.getMul()*mul;
            }
		}
		return flop;
	}

	/**
	 * べき乗+関数演算FLOPを取得する.
	 * @return		べき乗+関数演算FLOP
	 */
	public int getIntrinsicFlop() {
		return this.getPowFlop() + getFunctionFlop();
	}

	/**
	 * すべての演算FLOPの合計を取得する
	 * @return	演算FLOP合計
	 */
	public int getOperandFlop() {
		return getAddFlop() + getSubFlop() + getMulFlop() + getDivFlop() + getIntrinsicFlop();
	}

    /**
     * Blockの演算数をカウントする.
     * @param block 処理ブロック
     * @return カウント結果
     */
    public OperandCountResult countBlock(IBlock block) {
    	if (block == null) return null;
    	this.result = this.countChildren(block);

        return result;
    }

    /**
     * ブロックの子要素に対して演算数をカウントする。
     * @param block ブロック
     * @return カウント結果
     * @version         2015/09/01   by @hira
     *                  leftVarのデータ型をSetに変更、leftfuncの追加
     */
    private OperandCountResult countChildren(IBlock block) {
    	if (block == null) return null;
    	OperandCountResult result = new OperandCountResult();
        if (block instanceof Substitution) {
            Expression left = ((Substitution)block).getLeftValue();
            Set<Variable> leftVariables = left.getAllVariables();
            for (Variable vr : leftVariables) {
                //配列ならばロードに数える
                if (vr.isMemoryAccess()) {
                    result.addLeftVariable(vr);
        		}
            }
            List<ProcedureUsage> leftFuncCalls = left.getFuncCalls();
            for (ProcedureUsage pu : leftFuncCalls) {
                result.addLeftFunction(pu);
            }

            Expression right = ((Substitution) block).getRightValue();
            Set<Variable> rightVariables = right.getAllVariables();
            for (Variable vr : rightVariables) {
                //配列ならばロードに数える
                if (vr.isMemoryAccess()) {
                	result.addRightVariable(vr);
                }
            }
            List<ProcedureUsage> rightFuncCalls = right.getFuncCalls();
            for (ProcedureUsage pu : rightFuncCalls) {
            	result.addRightFunction(pu);
            }
            result.incrementAdd(right.getOperandAddCount());
            result.incrementSub(right.getOperandSubCount());
            result.incrementMul(right.getOperandMulCount());
            result.incrementDiv(right.getOperandDivCount());
            result.incrementPow(right.getOperandPowCount());
        } else if (block instanceof Block) {
            List<IBlock> children = ((Block) block).getChildren();
            for (IBlock bk : children) {
            	OperandCountResult childrenResult = this.countChildren(bk);
            	if (childrenResult != null) {
            		result.addCount(childrenResult);
            	}
            }
        } else if (block instanceof Procedure) {
        	Procedure proc = (Procedure)block;
        	OperandCountResult childrenResult = this.countChildren(proc.getBody());
        	if (childrenResult != null) {
        		result.addCount(childrenResult);
        	}
	    } else if (block instanceof BlockList) {
	        List<IBlock> list = ((BlockList)block).getBlocks();
	        if (list != null) {
		        for (IBlock item : list) {
		        	OperandCountResult itemResult = this.countChildren(item);
		        	if (itemResult != null) {
		        		result.addCount(itemResult);
		        	}
		        }
	        }
	    }

        return result;
    }

    /**
     * 演算カウント結果クラス
     * @author RIKEN
     * @version         2015/09/01   by @hira
     *                  leftFunctionsの追加
     */
    public class OperandCountResult {
    	/** 演算カウント:加算 */
        private int countAdd;
    	/** 演算カウント:減算 */
        private int countSub;
        /** 演算カウント:乗算 */
        private int countMul;
        /** 演算カウント:除算 */
        private int countDiv;
        /** 演算カウント:べき算 */
        private int countPow;
        /** 左辺変数表現文字列リスト */
        private List<Variable> leftVars;
        /** 右辺変数表現文字列リスト */
        private List<Variable> rightVars;
        /** 関数呼出リスト:左辺 */
        private List<ProcedureUsage> leftFunctions;
        /** 関数呼出リスト:右辺 */
        private List<ProcedureUsage> rightFunctions;

        /**
         * コンストラクタ
         */
        public OperandCountResult() {
        	this.countAdd = 0;
        	this.countSub = 0;
        	this.countMul = 0;
        	this.countDiv = 0;
        	this.countPow = 0;
        	this.leftVars = new ArrayList<Variable>();
        	this.rightVars = new ArrayList<Variable>();
            this.leftFunctions = new ArrayList<ProcedureUsage>();
        	this.rightFunctions = new ArrayList<ProcedureUsage>();

        }

        /**
         * 演算カウント結果を追加する
         * @param result		演算カウント結果
         */
        public void addCount(OperandCountResult result) {
        	if (result == null) return;
        	this.countAdd += result.countAdd;
        	this.countSub += result.countSub;
        	this.countMul += result.countMul;
        	this.countDiv += result.countDiv;
        	this.countPow  += result.countPow;
        	if (result.leftVars != null) {
        		for (Variable var : result.leftVars) {
        			this.addLeftVariable(var);
        		}
        	}
        	if (result.rightVars != null) {
        		for (Variable var : result.rightVars) {
        			this.addRightVariable(var);
        		}
        	}
            if (result.leftFunctions != null) {
                for (ProcedureUsage call : result.leftFunctions) {
                    this.addLeftFunction(call);
                }
            }
        	if (result.rightFunctions != null) {
        		for (ProcedureUsage call : result.rightFunctions) {
        			this.addRightFunction(call);
        		}
        	}
		}

		/**
         * 加算演算数を取得する.
         * @return		加算演算数
         */
        public int getCountAdd() {
        	return this.countAdd;
        }

        /**
         * 減算演算数を取得する.
         * @return		減算演算数
         */
        public int getCountSub() {
        	return this.countSub;
        }

        /**
         * 乗算演算数を取得する.
         * @return		乗算演算数
         */
        public int getCountMul() {
        	return this.countMul;
        }

        /**
         * 除算演算数を取得する.
         * @return		除算演算数
         */
        public int getCountDiv() {
        	return this.countDiv;
        }

        /**
         * べき乗演算数を取得する.
         * @return		べき乗演算数
         */
        public int getCountPow() {
        	return this.countPow;
        }

        /**
         * 加算演算数を加算する.
         * @param count		加算演算数
         * @return    加算演算数
         */
        public int incrementAdd(int count) {
        	this.countAdd += count;
        	return this.countAdd;
        }

        /**
         * 減算演算数を加算する.
         * @param count		加算演算数
         * @return    減算演算数
         */
        public int incrementSub(int count) {
        	this.countSub += count;
        	return this.countSub;
        }

        /**
         * 乗算演算数を加算する.
         * @param count		加算演算数
         * @return    乗算演算数
         */
        public int incrementMul(int count) {
        	this.countMul += count;
        	return this.countMul;
        }

        /**
         * 除算演算数を加算する.
         * @param count		加算演算数
         * @return    除算演算数
         */
        public int incrementDiv(int count) {
        	this.countDiv += count;
        	return this.countDiv;
        }

        /**
         * べき乗演算数を加算する.
         * @param count		加算演算数
         * @return    べき乗演算数
         */
        public int incrementPow(int count) {
        	this.countPow += count;
        	return this.countPow;
        }

        /**
         * 演算カウント:加算を設定する.
         * @param count		演算カウント:加算
         */
        public void setCountAdd(int count) {
            this.countAdd = count;
        }
        /**
         * 演算カウント:減算を設定する.
         * @param count		演算カウント:減算
         */
        public void setCountSub(int count) {
            this.countSub = count;
        }

        /**
         * 演算カウント:乗算を設定する.
         * @param count		演算カウント:乗算
         */
        public void setCountMul(int count) {
            this.countMul = count;
        }

        /**
         * 演算カウント:除算を設定する.
         * @param count		演算カウント:除算
         */
        public void setCountDiv(int count) {
            this.countDiv = count;
        }

        /**
         * 演算カウント:べき算を設定する.
         * @param count		演算カウント:べき算
         */
        public void setCountPow(int count) {
            this.countPow = count;
        }

        /**
         * 関数呼出リストを取得する.
         * @return		関数呼出リスト
         */
        public List<ProcedureUsage> getFunctions() {
            List<ProcedureUsage> functions = new ArrayList<ProcedureUsage>();
            functions.addAll(this.leftFunctions);
            functions.addAll(this.rightFunctions);
            return functions;
        }

        /**
         * 関数呼出リスト:左辺を取得する.
         * @return        関数呼出リスト:左辺
         */
        public List<ProcedureUsage> getLeftFunctions() {
            return this.leftFunctions;
        }

        /**
         * 関数呼出リスト:右辺を取得する.
         * @return        関数呼出リスト:右辺
         */
        public List<ProcedureUsage> getRightFunctions() {
            return this.rightFunctions;
        }

        /**
         * 右辺変数リストを取得する.
         * @return		右辺変数リスト
         */
        public List<Variable> getRightVar() {
            return this.rightVars;
        }

        /**
         * 左辺変数表現文字列リストを取得する.
         * @return		左辺変数表現文字列リスト
         */
        public List<Variable> getLeftVar() {
            return this.leftVars;
        }

        /**
         * 左辺変数リストに追加する.
         * @param  var		左辺変数
         */
        public void addLeftVariable(Variable var) {
        	if (var == null) return;
        	if (containsVariable(this.leftVars, var)) {
        		return;
        	}
        	this.leftVars.add(var);
        }

        /**
         * 右辺変数リストに追加する.
         * @param  var		右辺変数
         */
        public void addRightVariable(Variable var) {
        	if (var == null) return;
        	if (containsVariable(this.rightVars, var)) {
        		return;
        	}
        	this.rightVars.add(var);
        }

        /**
         * 左辺関数呼出リストに追加する.
         * @param  call        関数呼出
         */
        public void addLeftFunction(ProcedureUsage call) {
            if (call == null) return;
            this.leftFunctions.add(call);
        }

        /**
         * 右辺関数呼出リストに追加する.
         * @param  call		関数呼出
         */
        public void addRightFunction(ProcedureUsage call) {
        	if (call == null) return;
        	this.rightFunctions.add(call);
        }

        /**
         * 変数リストから同一の変数が存在するかチェックする.
         * @param list		変数リスト
         * @param var		検索変数
         * @return			true=同一変数が存在する
         */
        private boolean containsVariable(List<Variable> list, Variable var) {
        	if (list == null || list.size() <= 0) return false;
        	if (var == null) return false;
        	List<Variable> results = OperationCounterUtils.this.getVariables(list, var);
        	if (results != null && results.size() > 0) return true;

        	return false;
        }
    }

	/**
	 * 変数リストから変数と同じ変数リストを取得する
	 * @param listVar		変数リスト
	 * @param value		変数
	 * @return			true=変数リスト
	 */
	public List<Variable> getVariables(List<Variable> listVar, Variable value) {
		if (listVar == null || listVar.size() <= 0) return null;
		if (value == null) return null;
		if (value.getVariableString() == null) return null;
		List<Variable> listEquals = new ArrayList<Variable>();
		for (Variable var : listVar) {
			if (var == null) continue;
			// 定義が同じであること
			if (value.getDefinition() != var.getDefinition()) continue;
			if (value.equalsVariable(var)) {
				listEquals.add(var);
			}
		}
		if (listEquals.size() <= 0) {
			return null;
		}
		return listEquals;
	}

	/**
	 * 組込み関数演算カウントプロパティを設定する.
	 * @param properties    組込み関数演算カウントプロパティ
	 */
	public void setPropertiesOperation(OperationProperties properties) {
		this.propertiesOperation = properties;
	}


}
