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

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import jp.riken.kscope.language.Common;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.Module;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.UseState;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.fortran.VariableAttribute;
import jp.riken.kscope.language.fortran.VariableAttribute.ScopeAttribute;
import jp.riken.kscope.model.ScopeModel;

/**
 * 変数有効域サービスクラス.<br/>
 * 変数有効域テーブルを作成する
 *
 * @author riken
 */
public class AnalysisScopeService extends AnalysisBaseService {

    /** 変数有効域モデル */
    private ScopeModel modelScope;

    /**
     * コンストラクタ
     *
     * @param fortran
     *            フォートランデータベース
     */
    public AnalysisScopeService(Fortran fortran) {
        super(fortran);
    }

    /**
     * 変数有効域モデルを設定する
     *
     * @param model
     *            変数有効域モデル
     */
    public void setModelScope(ScopeModel model) {
        this.modelScope = model;
    }

    /**
     * 変数有効域を作成する
     *
     * @param variable
     *            変数有効域変数
     */
    public void analysisScope(VariableDefinition variable) {

        if (variable == null) {
            return;
        }

        String varName = variable.get_name();
        ProgramUnit pu = variable.getMother();
        if (pu == null) return;

        Set<String> result = new HashSet<String>();
        // 宣言が属するプログラム単位を追加
        StringBuilder scope = new StringBuilder(pu.get_name());
        ProgramUnit mthr = pu.get_mother();
        while (mthr != null) {
            scope.insert(0, mthr.get_name() + ".");
            mthr = mthr.get_mother();
        }
        result.add(scope.toString());
        for (Procedure child : pu.getChildren()) {
            // 副プログラムに同一の名前の宣言が無ければ追加
            if (child.get_variable(varName) == null) {
                result.add(scope.toString() + "." + child.get_name());
            }
            for (Procedure grnd : child.getChildren()) {
                if (grnd.get_variable(varName) == null) {
                    result.add(scope.toString() + "." + child.get_name() + "."
                            + grnd.get_name());
                }
            }
        }

        // private属性かチェック
        VariableAttribute att = (VariableAttribute) variable.getAttribute();
        if (!(att != null && att.getScope() == ScopeAttribute.PRIVATE)) {

            // COMMON文に含まれるかチェック
            this.searchCOMMON(variable, result);

            // 各モジュールのUSE文を探索
            this.searchUSE(variable, result);
        }

        // 変数有効域テーブルの設定
        String[] areas = null;
        if (result.size() > 0) {
            result = new TreeSet<String>(result);
            areas = result.toArray(new String[0]);
        }
        this.modelScope.setTitle(variable.toString());
        this.modelScope.setScope(areas);
    }

    /**
     * @param var
     *            変数宣言
     * @param res
     *            結果のセット
     */
    private void searchCOMMON(VariableDefinition var, Set<String> res) {
        String varName = var.get_name();
        ProgramUnit pu = var.getMother();
        List<Common> commons = pu.getCommonList();
        for (Common cm : commons) {
            if (cm.contains(varName)) {
                List<ProgramUnit> pus = this.fortranDb.getCommonUnit(cm
                        .getName());
                for (ProgramUnit comm : pus) {
                    StringBuilder scopeName = new StringBuilder(comm.get_name());
                    ProgramUnit mthr = comm.get_mother();
                    while (mthr != null) {
                        scopeName.insert(0, mthr.get_name() + ".");
                        mthr = mthr.get_mother();
                    }
                    res.add(scopeName.toString());
                    for (ProgramUnit chld : comm.getChildren()) {
                        if (chld.get_variable(varName) == null) {
                            res.add(scopeName.toString() + "."
                                    + chld.get_name());
                        }
                        for (ProgramUnit chld2 : chld.getChildren()) {
                            if (chld2.get_variable(varName) == null) {
                                res.add(scopeName.toString() + "."
                                        + chld.get_name() + "."
                                        + chld2.get_name());
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * @param var
     *            変数宣言
     * @param res
     *            結果のセット
     */
    private void searchUSE(VariableDefinition var, Set<String> res) {
        String varName = var.get_name();
        ProgramUnit pu = var.getMother();
        Collection<Module> modules = this.fortranDb.getModules().values();
        for (Module mod : modules) {
            boolean flag = false;
            List<UseState> ul = mod.getUseList();
            // moduleをチェック
            for (UseState us : ul) {
                if (us.getModuleName().equalsIgnoreCase(pu.get_name())) {
                    if (us.hasOnlyMember()) {
                        Set<String> om = us.getOnlyMember();
                        for (String oName : om) {
                            if (oName.equalsIgnoreCase(varName)) {
                                res.add(mod.get_name());
                                flag = true;
                            }
                        }
                    } else {
                        res.add(mod.get_name());
                        flag = true;
                    }
                }
            }
            // module副プログラムをチェック
            for (Procedure child : mod.getChildren()) {
                boolean flag2 = false;
                List<UseState> ul2 = child.getUseList();
                for (UseState us : ul2) {
                    if (us.getModuleName().equalsIgnoreCase(pu.get_name())) {
                        if (us.hasOnlyMember()) {
                            Set<String> om = us.getOnlyMember();
                            for (String oName : om) {
                                if (oName.equalsIgnoreCase(varName)) {
                                    res.add(mod.get_name() + "."
                                            + child.get_name());
                                    flag2 = true;
                                }
                            }
                        } else {
                            res.add(mod.get_name() + "." + child.get_name());
                            flag2 = true;
                        }
                    }
                }
                // モジュールでUSEされ、かつモジュール副プログラムでUSEされていない場合
                if (flag2 == false && flag == true) {
                    if (child.get_variable(varName) == null) {
                        res.add(mod.get_name() + "." + child.get_name());
                    }
                }
                // moduleの内部副プログラムをチェック
                for (Procedure grnd : child.getChildren()) {
                    boolean flag3 = false;
                    List<UseState> ul3 = grnd.getUseList();
                    for (UseState us : ul3) {
                        if (us.getModuleName().equalsIgnoreCase(pu.get_name())) {
                            if (us.hasOnlyMember()) {
                                Set<String> om = us.getOnlyMember();
                                for (String oName : om) {
                                    if (oName.equalsIgnoreCase(var
                                            .get_name())) {
                                        res.add(mod.get_name() + "."
                                                + child.get_name() + "." + grnd.get_name());
                                        flag3 = true;
                                    }
                                }
                            } else {
                                res.add(mod.get_name() + "."
                                        + child.get_name() + "." + grnd.get_name());
                                flag3 = true;
                            }
                        }
                    }
                    // モジュールまたはモジュール副プログラムでUSEされ、かつ内部副プログラムでUSEされていない場合
                    if (flag3 == false && (flag == true || flag2 == true)) {
                        if (grnd.get_variable(var.get_name()) == null) {
                            res.add(mod.get_name() + "." + child.get_name()
                                    + "." + grnd.get_name());
                        }
                    }
                }
            }
        }
    }
}
