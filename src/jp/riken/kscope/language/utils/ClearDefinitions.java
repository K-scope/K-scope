/*
 * K-scope
 * Copyright 2012-2015 RIKEN, Japan
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

import jp.riken.kscope.language.*;
import jp.riken.kscope.language.fortran.*;
import jp.riken.kscope.language.fortran.VariableType.PrimitiveDataType;
import jp.riken.kscope.language.generic.*;

/**
 * データベースリセットクラス.
 * サブルーチン、関数、変数の定義参照をクリアする.
 * @author RIKEN
 * @version    2015/03/15     探索履歴リストチェックメソッドの追加
 */
public class ClearDefinitions implements ILanguageEntry {
    /** 探索履歴リスト */
    private List<Object> listVisit;
    /** Fortranデータベース */
    private Fortran language;
    /** クリア対象モジュールマップ <旧モジュール, 新モジュール> */
    private java.util.Map<Module, Module> mapClearModule;

    /**
     * コンストラクタ
     * @param    Fortranデータベース
     */
    public ClearDefinitions(Fortran language) {
        this.language = language;
        this.listVisit = new ArrayList<Object>();
    }

    /**
     * Fortranデータベースを取得する.
     * @return		Fortranデータベース
     */
    public Fortran getLanguage() {
        return language;
    }

    /**
     * Fortranデータベースを設定する.
     * @param language		Fortranデータベース
     */
    public void setLanguage(Fortran language) {
        this.language = language;
    }

    /**
     * 探索リストを設定する.
     * @param list	探索リスト
     */
    @Override
    public List<Object> getListVisit() {
        return this.listVisit;
    }

    /**
     * 探索リストを設定する.
     * @param list	探索リスト
     */
    @Override
    public void setListVisit(List<Object> list) {
        this.listVisit = list;
    }

    /**
     * 探索履歴リストに存在するかチェックする.
     * @param obj		探索オブジェクト
     * @return		true=追加済み
     */
    @Override
    public boolean containsListVisit(Object obj) {
        return this.listVisit.contains(obj);
    }

    /**
     * VariableDefinitionマップをクリアする.
     */
    @Override
    public void entry(Module entry) {
        // VariableDefinitionマップをクリアする.
        entry((ProgramUnit)entry);
    }

    /**
     * ProcedureUsageクラスリストをクリアする.
     */
    @Override
    public void entry(Procedure entry) {
        if (entry == null) return;
        if (entry.getCallMember() == null) return;
        if (entry.getCallMember().size() <= 0) return;
        if (this.containsClearProcedure(entry)) {
            // ProcedureUsageクラスリストをクリアする.
            entry.setCallMember(null);
        }
        else if (!this.containsLanguageProcedure(entry)) {
            // ProcedureUsageクラスリストをクリアする.
            entry.setCallMember(null);
        }
        // VariableDefinitionマップをクリアする.
        entry((ProgramUnit)entry);
    }

    /**
     * VariableDefinitionマップをクリアする.
     * @param entry		ProgramUnit
     */
    public void entry(ProgramUnit entry) {
        if (entry == null) return;
        java.util.Map<String, VariableDefinition> map = entry.getVariableMap();
        java.util.Iterator<String> itr = map.keySet().iterator();
        while(itr.hasNext()) {
            String key = itr.next();
            VariableDefinition def = map.get(key);
            if (containsClearVariableDefinition(def)) {
                itr.remove();
            }
            else if (!containsLanguageVariableDefinition(def)) {
                itr.remove();
            }
        }
    }

    /**
     * 変数定義をクリアする.
     */
    @Override
    public void entry(Variable entry) {
        if (entry == null) return;
        VariableDefinition def = entry.getDefinition();
        if (def == null) return;
        if (containsClearVariableDefinition(def)) {
            entry.setDefinition(null);
        }
        else if (!containsLanguageVariableDefinition(def)) {
            entry.setDefinition(null);
        }
        return;
    }

    /**
     * CALL文Procedure定義をクリアする.
     */
    @Override
    public void entry(ProcedureUsage entry) {
        if (entry == null) return;
        if (entry.getCallDefinition() == null) return;
        Procedure callDefinition = entry.getCallDefinition();
        if (this.containsClearProcedure(callDefinition)) {
            // CALL文Procedure定義をクリアする.
            entry.setCallDefinition(null);
        }
        else if (!this.containsLanguageProcedure(callDefinition)) {
            // CALL文Procedure定義をクリアする.
            entry.setCallDefinition(null);
        }
    }

    /**
     * 変数を参照しているプログラムをクリアする.
     */
    @Override
    public void entry(VariableDefinition entry) {
        if (entry == null) return;
        Set<ProgramUnit> refer = entry.getReferMember();
        if (refer == null || refer.size() <= 0) return;
        java.util.Iterator<ProgramUnit> iter = refer.iterator();
        while(iter.hasNext()){
            ProgramUnit prog = iter.next();
            if (containsClearProgramUnit(prog)) {
                iter.remove();
            }
            else if (!containsLanguageProgramUnit(prog)) {
                iter.remove();
            }
        }
        return;
    }

    /**
     * 構造体定義をクリアする.
     */
    @Override
    public void entry(VariableType entry) {
        if (entry == null) return;
        if (entry.getPrimitiveDataType() == PrimitiveDataType.TYPE) {
            Type type = entry.getType();
            if (type != null && type.getMotherBlock() != null) {
                if (containsClearBlock(type.getMotherBlock())) {
                    entry.setType(null);
                }
                else if (!containsLanguageBlock(type.getMotherBlock())) {
                    entry.setType(null);
                }
            }
        }
    }


    @Override
    public void entry(Break entry) { }

    @Override
    public void entry(Common entry) { }

    @Override
    public void entry(Condition entry) { }

    @Override
    public void entry(Continue entry) { }

    @Override
    public void entry(Data entry) { }

    @Override
    public void entry(Directive entry) { }

    @Override
    public void entry(DoNothing entry) { }

    @Override
    public void entry(DynamicAllocation entry) { }

    @Override
    public void entry(DynamicDeallocation entry) { }

    @Override
    public void entry(DynamicNullification entry) { }

    @Override
    public void entry(Equivalence entry) { }

    @Override
    public void entry(ExecutableBody entry) { }

    @Override
    public void entry(GoTo entry) { }

    @Override
    public void entry(Pause entry) { }

    @Override
    public void entry(Procedures entry) { }

    @Override
    public void entry(Repetition entry) { }

    @Override
    public void entry(Return entry) { }

    @Override
    public void entry(Selection entry) { }

    @Override
    public void entry(Substitution entry) { }

    @Override
    public void entry(Termination entry) { }

    @Override
    public void entry(UserDefined entry) { }

    @Override
    public void entry(UseState entry) { }

    @Override
    public void entry(ProcedureWithNameOnly entry) { }

    @Override
    public void entry(VariableAttribute entry) { }

    @Override
    public void entry(VariableDimension entry) { }

    @Override
    public void entry(DimensionIndex entry) { }

    @Override
    public void entry(Expression entry) { }

    @Override
    public void entry(ProcedureItem entry) { }

    @Override
    public void entry(Type entry) { }

    @Override
    public void entry(Structure entry) {}

    @Override
    public void entry(Union entry) { }

    /**
     * クリア対象モジュールを設定する
     * @param list		クリア対象モジュールマップ <旧モジュール, 新モジュール>
     */
    public void setListClearModule(java.util.Map<Module, Module> mapModule) {
        this.mapClearModule = mapModule;
    }

    /**
     * モジュールがクリア対象モジュールであるかチェックする.
     * @param module		クリアチェックモジュール
     * @return		true=クリアモジュール
     */
    private boolean containsClearModule(Module module) {
        if (this.mapClearModule == null) return false;
        if (module == null) return false;
        return this.mapClearModule.containsKey(module);
    }

    /**
     * クリア対象のモジュールに含まれるProcedureであるかチェックする.
     * @param definition		Procedure
     * @return		true=クリアモジュールのProcedure
     */
    private boolean containsClearProcedure(Procedure definition) {
        Module module = getParentModule(definition);
        if (module == null) return false;
        return containsClearModule(module);
    }

    /**
     * クリア対象のモジュールに含まれるVariableDefinitionであるかチェックする.
     * @param definition		VariableDefinition
     * @return		true=クリアモジュールのVariableDefinition
     */
    private boolean containsClearVariableDefinition(VariableDefinition definition) {
        Module module = getParentModule(definition);
        if (module == null) return false;
        return containsClearModule(module);
    }

    /**
     * クリア対象のモジュールに含まれるProgramUnitであるかチェックする.
     * @param definition		ProgramUnit
     * @return		true=クリアモジュールのProgramUnit
     */
    private boolean containsClearProgramUnit(ProgramUnit definition) {
        if (definition == null) return false;
        if (definition instanceof Procedure) {
            return containsClearProcedure((Procedure)definition);
        }
        else if (definition instanceof Module) {
            return containsClearModule((Module)definition);
        }
        return false;
    }

    /**
     * クリア対象のモジュールに含まれるBlockであるかチェックする.
     * @param definition		Block
     * @return		true=クリアモジュールのBlock
     */
    private boolean containsClearBlock(IBlock definition) {
        if (definition == null) return false;
        if (definition instanceof Procedure) {
            return containsClearProcedure((Procedure)definition);
        }
        else if (definition instanceof Module) {
            return containsClearModule((Module)definition);
        }
        else if (definition instanceof VariableDefinition) {
            return containsClearVariableDefinition((VariableDefinition)definition);
        }
        return false;
    }

    /**
     * モジュールがデータベースのモジュールであるかチェックする.
     * @param module		クリアチェックモジュール
     * @return		true=データベースモジュール
     */
    private boolean containsLanguageModule(Module module) {
        if (module == null) return false;
        Module langModule = this.language.module(module.get_name());
        return (module == langModule);
    }

    /**
     * データベースのモジュールに含まれるProcedureであるかチェックする.
     * @param definition		Procedure
     * @return		true=クリアモジュールのProcedure
     */
    private boolean containsLanguageProcedure(Procedure definition) {
        Module module = getParentModule(definition);
        if (module == null) return false;
        return containsLanguageModule(module);
    }

    /**
     * データベースのモジュールに含まれるVariableDefinitionであるかチェックする.
     * @param definition		VariableDefinition
     * @return		true=クリアモジュールのVariableDefinition
     */
    private boolean containsLanguageVariableDefinition(VariableDefinition definition) {
        Module module = getParentModule(definition);
        if (module == null) return false;
        return containsLanguageModule(module);
    }

    /**
     * データベースのモジュールに含まれるProgramUnitであるかチェックする.
     * @param definition		ProgramUnit
     * @return		true=クリアモジュールのProgramUnit
     */
    private boolean containsLanguageProgramUnit(ProgramUnit definition) {
        if (definition == null) return false;
        if (definition instanceof Procedure) {
            return containsLanguageProcedure((Procedure)definition);
        }
        else if (definition instanceof Module) {
            return containsLanguageModule((Module)definition);
        }
        return false;
    }

    /**
     * データベースのモジュールに含まれるBlockであるかチェックする.
     * @param definition		Block
     * @return		true=クリアモジュールのBlock
     */
    private boolean containsLanguageBlock(IBlock definition) {
        if (definition == null) return false;
        if (definition instanceof Procedure) {
            return containsLanguageProcedure((Procedure)definition);
        }
        else if (definition instanceof Module) {
            return containsLanguageModule((Module)definition);
        }
        else if (definition instanceof VariableDefinition) {
            return containsLanguageVariableDefinition((VariableDefinition)definition);
        }
        return false;
    }

    /**
     * Procedureの親モジュールを取得する
     * @param definition		Procedure
     * @return		モジュール
     */
    private Module getParentModule(Procedure definition) {
        if (definition == null) return null;
        ProgramUnit block = definition.get_mother();
        if (block == null) return null;
        while (true) {
            if (block.get_mother() == null) {
                break;
            }
            block = block.get_mother();
        }

        if (!(block instanceof Module)) {
            return null;
        }

        return (Module)block;
    }


    /**
     * VariableDefinitionの親モジュールを取得する
     * @param definition		VariableDefinition
     * @return		モジュール
     */
    private Module getParentModule(VariableDefinition definition) {
        if (definition == null) return null;
        ProgramUnit block = definition.getMother();
        if (block == null) return null;
        while (true) {
            if (block.get_mother() == null) {
                break;
            }
            block = block.get_mother();
        }

        if (!(block instanceof Module)) {
            return null;
        }

        return (Module)block;
    }
}
