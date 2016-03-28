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
package jp.riken.kscope.service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.data.OperationCount;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.Condition;
import jp.riken.kscope.language.ExecutableBody;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IDeclarations;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.Selection;
import jp.riken.kscope.language.Substitution;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.utils.OperationCounterUtils;
import jp.riken.kscope.model.OperandTableModel;
import jp.riken.kscope.properties.OperationProperties;

/**
 * 分析：演算カウントを行う
 * @author RIKEN
 */
public class AnalysisOperandService extends AnalysisBaseService {

    /** 演算カウントテーブルモデル */
    private OperandTableModel modelOperand;

    /** 組込み関数演算カウントプロパティ */
    private OperationProperties propertiesOperand;

    /**
     * コンストラクタ
     * @param fortran            フォートランデータベース
     */
    public AnalysisOperandService(Fortran fortran) {
        super(fortran);
    }

    /**
     * 演算カウントテーブルモデルを設定する
     * @param modelOperand        演算カウントテーブルモデル
     */
    public void setModelOperand(OperandTableModel modelOperand) {
        this.modelOperand = modelOperand;
    }

    /**
     * 組込み関数演算カウントプロパティを取得する
     * @param propertiesOperand        組込み関数演算カウントプロパティ
     */
    public void setPropertiesOperand(OperationProperties propertiesOperand) {
        this.propertiesOperand = propertiesOperand;
    }

    /**
     * 演算カウントを取得する
     * @param   blocks         ブロックリスト
     */
    public void analysisOperand(IBlock[] blocks) {
        if (blocks == null) {
            return;
        }
        Collection<IBlock> blockList = java.util.Arrays.asList(blocks);
        // 変数宣言であればブロックのセットを取得
        if (blocks[0] instanceof VariableDefinition) {
            blockList = this.getBlocks((VariableDefinition) blocks[0]);
        }

        this.modelOperand.setTitle(blocks[0].toString());
        for (IBlock block: blockList) {

            // ブロック名の取得
            StringBuilder loopName = new StringBuilder();
            IBlock nameBlock = block;
            BlockType bkType = nameBlock.getBlockType();
            String bkTypeString = bkType.toString();
            if (nameBlock instanceof Condition) {
                nameBlock = ((Condition) block).get_mother();
            }
            if (bkType == BlockType.REPETITION) {
                bkTypeString = "DO";
            } else if (nameBlock instanceof Selection) {
                Selection selec = (Selection) nameBlock;
                if (selec.isIF()) {
                    bkTypeString = "IF";
                } else if (selec.isSelect()) {
                    bkTypeString = "SELECT";
                } else if (selec.isWHERE()) {
                    bkTypeString = "WHERE";
                }
            } else if (bkType == BlockType.PROCEDUREUSAGE) {
                bkTypeString = "CALL";
            }
            loopName.append(bkTypeString);
            if (block.getStartCodeLine() != null) {
                loopName.append(" " + block.getStartCodeLine().getStartLine());
            }
            if (block instanceof Block) {
                String label = ((Block) block).get_start().get_label();
                if (!(label.equalsIgnoreCase("NO_LABEL"))) {
                    loopName.append(" " + label);
                }
            }

            // ブロックのカウント取得
            OperationCounterUtils utils = new OperationCounterUtils(this.propertiesOperand);
            utils.countBlock(block);

            // 結果のセット
            OperationCount loop = new OperationCount();
            loop.setName(loopName.toString());
            loop.setF(utils.getOperandFlop());
            loop.setAdd(utils.getAddFlop());
            loop.setSub(utils.getSubFlop());
            loop.setMul(utils.getMulFlop());
            loop.setDiv(utils.getDivFlop());
            loop.setIntrinsic(utils.getIntrinsicFlop());
            this.modelOperand.addOperandBlock(block, loop);
        }
    }

    /**
     * 指定した変数宣言が参照・定義されている制御ブロックのセットを返す。
     * @param varDef 変数宣言
     * @return 制御ブロックのセット。無ければ空のリストを返す。
     */
    private Set<IBlock> getBlocks(VariableDefinition varDef) {
        Set<IBlock> set = new LinkedHashSet<IBlock>();
        String varName = varDef.get_name();
        IDeclarations proc = varDef.getScopeDeclarationsBlock();
        if (proc == null) {
            return set;
        }
        Set<IBlock> blks = proc.getRefDefBlocks(varName);
        for (IBlock block: blks) {
            if (block instanceof Substitution) {
                Substitution sub = (Substitution) block;
                if (!(sub.get_mother() instanceof ExecutableBody)) {
                    set.add(sub.get_mother());
                } else {
                    //TODO 他のどのブロックにも属さない代入文は個別にセットする。プログラムによっては非常に多くなる恐れがある。
                    set.add(sub);
                }
            }
        }

        if (proc instanceof IBlock) {
            // 内部副プログラムに対する処理
            List<IBlock> pus = ((IBlock)proc).getChildren();
            for (IBlock pr: pus) {
                if (!(pr instanceof IDeclarations)) continue;
                if (!(((IDeclarations)pr ).getVariableDefinitionMap().containsKey(varName))) {
                    blks = ((IDeclarations)pr ).getRefDefBlocks(varName);
                    for (IBlock block: blks) {
                        if (block instanceof Substitution) {
                            Substitution sub = (Substitution) block;
                            if (!(sub.get_mother() instanceof ExecutableBody)) {
                                set.add(sub.get_mother());
                            } else {
                                //TODO 他のどのブロックにも属さない代入文は個別にセットする。プログラムによっては非常に多くなる恐れがある。
                                set.add(sub);
                            }
                        }
                    }
                }
            }
        }

        return set;
    }

    /**
     * Blockの演算数をカウントする。
     *
     * @param block 処理ブロック
     * @return カウント情報の整数配列
     */
    @SuppressWarnings("unused")
    private CountResult countBlock(IBlock block) {
        CountResult result = new CountResult();
        this.countChildren(block, result);
        return result;
    }

    /**
     * ブロックの子要素に対して演算数をカウントする。
     * @param block ブロック
     * @param result カウント結果
     * @version         2015/09/01   by @hira
     *                  leftVarのデータ型をSetに変更、leftfuncの追加
     */
    private void countChildren(IBlock block, CountResult result) {
        int[] count = result.getCount();
        Set<String> leftVar = result.getLeftVar();
        List<String> leftfunc = result.getLeftFunc();
        Set<String> rightVar = result.getRightVar();
        List<String> rightfunc = result.getRightFunc();

        if (block instanceof Substitution) {
            Expression left = ((Substitution) block).getLeftValue();
            List<Variable> leftVariables = left.getVariables();
            for (Variable vr : leftVariables) {
                //配列ならばロードに数える
                if (vr.getDimensionIndexValue() != null) {
                    leftVar.add(vr.getVariableString());
                }
            }
            List<ProcedureUsage> leftFuncCalls = left.getFuncCalls();
            for (ProcedureUsage pu : leftFuncCalls) {
                leftfunc.add(pu.getCallName());
            }

            Expression right = ((Substitution) block).getRightValue();
            List<Variable> rightVariables = right.getVariables();
            for (Variable vr : rightVariables) {
                //配列ならばロードに数える
                if (vr.getDimensionIndexValue() != null) {
                    rightVar.add(vr.getVariableString());
                }
            }
            List<ProcedureUsage> rightFuncCalls = right.getFuncCalls();
            for (ProcedureUsage pu : rightFuncCalls) {
                rightfunc.add(pu.getCallName());
            }
            count[0] += right.getAddCount() + right.getSubCount();
            count[1] += right.getMulCount() + right.getDivCount();
            count[2] += right.getPowCount();
            result.setCount(count);

        } else {
            List<IBlock> children = block.getChildren();
            for (IBlock bk : children) {
                this.countChildren(bk, result);
            }
        }
    }

    /**
     * 演算カウント、参照変数リストの探索結果クラス
     * @version         2015/09/01   by @hira
     *                  leftVarのデータ型をSetに変更、leftfuncの追加
     */
    private class CountResult {
        private int[] count = new int[3];// [0]=add+sub,[1]=mul+div,[2]=pow
        private Set<String> leftVar = new HashSet<String>();
        private List<String> leftfunc = new ArrayList<String>();
        private Set<String> rightVar = new HashSet<String>();
        private List<String> rightfunc = new ArrayList<String>();

        private void setCount(int[] count) {
            this.count = count;
        }

        private int[] getCount() {
            return count;
        }

        private List<String> getLeftFunc() {
            return leftfunc;
        }

        private List<String> getRightFunc() {
            return rightfunc;
        }

        private Set<String> getRightVar() {
            return rightVar;
        }

        private Set<String> getLeftVar() {
            return leftVar;
        }
    }
}
