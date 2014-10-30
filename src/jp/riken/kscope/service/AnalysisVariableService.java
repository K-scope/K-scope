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

import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.ExecutableBody;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IVariableType;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.VariableDimension;
import jp.riken.kscope.language.fortran.Type;
import jp.riken.kscope.language.fortran.VariableAttribute;
import jp.riken.kscope.language.fortran.VariableAttribute.IntentAttribute;
import jp.riken.kscope.language.fortran.VariableAttribute.PointerAttribute;
import jp.riken.kscope.language.fortran.VariableAttribute.ScopeAttribute;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.language.fortran.VariableType.PrimitiveDataType;
import jp.riken.kscope.model.VariableTableModel;
import jp.riken.kscope.utils.StringUtils;

/**
 * 変数特性一覧の作成を行うサービスクラス
 *
 * @author RIKEN
 */
public class AnalysisVariableService extends AnalysisBaseService {

    /** 変数特性一覧テーブルモデル */
    private VariableTableModel modelVariable;

    /**
     * コンストラクタ
     *
     * @param fortran
     *            フォートランデータベース
     */
    public AnalysisVariableService(Fortran fortran) {
        super(fortran);
    }

    /**
     * 変数特性一覧テーブルモデルを設定する
     *
     * @param modelVariable
     *            変数特性一覧テーブルモデル
     */
    public void setModelVariable(VariableTableModel modelVariable) {
        this.modelVariable = modelVariable;
    }

    /**
     * 変数特性一覧を作成する.<br/>
     * フォートランデータベースと分析対象のブロックから変数特性一覧を作成する.<br/>
     * 作成した変数特性一覧は、変数特性一覧テーブルモデルに設定する。
     *
     * @param blocks
     *            ブロックリスト
     */
    public void analysisVariable(IBlock[] blocks) {

        if (blocks == null) {
            return;
        }

        for (IBlock block : blocks) {
            // ブロックが属するプログラム単位へ変換する
            while (block instanceof Block) {
                if (block instanceof ExecutableBody) {
                    block = ((ExecutableBody) block).getParent();
                } else {
                    block = block.getMotherBlock();
                }
            }
            if (block == null) {
                return;
            }
            this.modelVariable.setTitle(block.toString());
            if (block instanceof ProgramUnit) {
                VariableDefinition[] list = ((ProgramUnit) block)
                        .get_variables();
                for (VariableDefinition vardef : list) {
                    String[] infos = makeVariableDefinitionInfo(vardef);
                    this.modelVariable.addVariableInfo(block, vardef, infos);
                }
                // 構造体のセット
                Set<Type> types = this.getTypeList(list);
                for (Type tp : types) {
                    List<VariableDefinition> defs = tp.getDefinitions();
                    for (VariableDefinition def : defs) {
                        String[] infos = makeVariableDefinitionInfo(def);
                        this.modelVariable.addVariableInfo(tp, def, infos);
                    }
                }
                // 内部副プログラムのセット
                Collection<Procedure> children = ((ProgramUnit) block)
                        .getChildren();
                for (Procedure child : children) {
                    VariableDefinition[] varlist = child.get_variables();
                    for (VariableDefinition vardef : varlist) {
                        String[] infos = makeVariableDefinitionInfo(vardef);
                        this.modelVariable
                                .addVariableInfo(child, vardef, infos);
                    }
                    // 構造体のセット
                    Set<Type> tps = this.getTypeList(varlist);
                    for (Type tp : tps) {
                        List<VariableDefinition> defs = tp.getDefinitions();
                        for (VariableDefinition def : defs) {
                            String[] infos = makeVariableDefinitionInfo(def);
                            this.modelVariable.addVariableInfo(tp, def, infos);
                        }
                    }
                }
            }
        }
    }

    /**
     * 変数宣言の配列から、含まれるTypeのセットを返す。
     *
     * @param list
     *            変数宣言の配列
     * @return Typeクラスのセット。無ければ空のセットを返す。
     */
    private Set<Type> getTypeList(VariableDefinition[] list) {
        Set<Type> set = new HashSet<Type>();
        if (list != null) {
            for (int i = 0; i < list.length; i++) {
                if (list[i].getType() instanceof VariableType) {
                    VariableType type = (VariableType) list[i].getType();
                    if (type.getPrimitiveDataType() == PrimitiveDataType.TYPE) {
                        if (type.getType() != null) {
                            set.add(type.getType());
                        }
                    }
                }
            }
        }
        return set;
    }

    /**
     * 変数宣言の配列を、スカラー・配列の順にソートする。
     *
     * @param list
     *            変数宣言の配列
     * @return ソートされた変数宣言のセット。無ければ空のセットを返す。
     */
    @SuppressWarnings("unused")
    private Set<VariableDefinition> sort(VariableDefinition[] list) {
        if (list == null) {
            return new LinkedHashSet<VariableDefinition>();
        }
        Set<VariableDefinition> set = new LinkedHashSet<VariableDefinition>();
        Set<VariableDefinition> scalars = new LinkedHashSet<VariableDefinition>();
        Set<VariableDefinition> arrays = new LinkedHashSet<VariableDefinition>();

        for (int i = 0; i < list.length; i++) {
            if (list[i].isScalar()) {
                scalars.add(list[i]);
            } else {
                arrays.add(list[i]);
            }
        }
        set.addAll(scalars);
        set.addAll(arrays);
        return set;
    }

    /**
     * 変数特性一覧を作成する.<br/>
     * フォートランデータベースと分析対象の変数宣言から変数特性一覧を作成する.<br/>
     * 作成した変数特性一覧は、変数特性一覧テーブルモデルに設定する。
     *
     * @param vars
     *            変数宣言リスト
     */
    public void analysisVariable(VariableDefinition[] vars) {
        this.modelVariable.setTitle("VariableDefinition List");
        //Set<VariableDefinition> varSet = this.sort(vars);
        for (VariableDefinition vardef : vars) {
            IBlock block = vardef.getMother(); // 変数の属するプロシージャ
            String[] infos = makeVariableDefinitionInfo(vardef);
            this.modelVariable.addVariableInfo(block, vardef, infos);
        }
    }

    private String[] makeVariableDefinitionInfo(VariableDefinition var) {
        List<String> infos = new ArrayList<String>();
        // スカラーか配列か
        if (var.isScalar()) {
            infos.add("scalar");
        } else {
            infos.add("array");
        }
        // 変数名
        infos.add(var.get_name());
        // データ型
        IVariableType tp = var.getVariableType();
        infos.add(tp.toString());

        // 各種属性のセット
        VariableAttribute att = (VariableAttribute) var.getAttribute();

        // スコープ属性
        ScopeAttribute sc;
        if (att == null) {
            sc = ScopeAttribute.NONE;
        } else {
            sc = att.getScope();
        }
        if (sc == ScopeAttribute.PRIVATE) {
            infos.add("private");
        } else if (sc == ScopeAttribute.PUBLIC) {
            infos.add("public");
        } else {
            infos.add("default");
        }

        // パラメータ属性
        if (att != null && att.hasParameter()) {
            infos.add("parameter");
        } else {
            infos.add("no param");
        }

        // 初期値
        if (var.getInitValue() == null) {
            infos.add("no value");
        } else {
            infos.add(var.getInitValue());
        }

        if (var.isScalar()) {
            infos.add("1");
        } else {
            VariableDimension dim = var.getVariableDimension();
            infos.add(dim.toString());
        }

        if (att == null) {
            infos.add("no intent");
            infos.add("no opt");
            infos.add("no pointer");
            infos.add("no save");
            // common属性
            ProgramUnit pu = var.getMother();
            String common = null;
            if (pu != null) {
                common = pu.getCommonName(var.get_name());
            }
            if (common == null) {
                infos.add("no common");
            } else {
                infos.add("com : " + common);
            }
            infos.add("no alloc");
        } else {
            // intent属性
            IntentAttribute ia = att.getIntent();
            if (ia == IntentAttribute.NONE) {
                infos.add("no intent");
            } else if (ia == IntentAttribute.IN) {
                infos.add("in");
            } else if (ia == IntentAttribute.OUT) {
                infos.add("out");
            } else if (ia == IntentAttribute.INOUT) {
                infos.add("inout");
            }

            // optional属性
            if (att.hasOptional()) {
                infos.add("optional");
            } else {
                infos.add("no opt");
            }

            // pointer/target属性
            PointerAttribute pa = att.getPointerOrTarget();
            if (pa == PointerAttribute.NONE) {
                infos.add("no pointer");
            } else if (pa == PointerAttribute.POINTER) {
                infos.add("pointer");
            } else if (pa == PointerAttribute.TARGET) {
                infos.add("target");
            }

            // save属性
            if (att.hasSave()) {
                infos.add("save");
            } else {
                infos.add("no save");
            }

            // common属性
            ProgramUnit pu = var.getMother();
            String common = null;
            if (pu != null) {
                common = pu.getCommonName(var.get_name());
            }
            if (common == null) {
                infos.add("no common");
            } else {
                infos.add("com : " + common);
            }

            // allocatable属性
            if (att.hasAllocatable()) {
                infos.add("allocatable");
            } else {
                infos.add("no alloc");
            }
        }

        // 付加情報
        if (var.getInformation() != null && !StringUtils.isNullOrEmpty(var.getInformation().getContent())) {
            infos.add(var.getInformation().getContent());
        } else {
            infos.add("no info");
        }
        return infos.toArray(new String[0]);
    }
}
