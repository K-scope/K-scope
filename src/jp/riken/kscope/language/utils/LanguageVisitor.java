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
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jp.riken.kscope.Application;
import jp.riken.kscope.language.*;
import jp.riken.kscope.language.fortran.*;
import jp.riken.kscope.language.generic.*;

/**
 * Fortranデータベース探索クラス
 * @author RIKEN
 */
public class LanguageVisitor implements ILanguageEntry {

    /** Fortranデータベース */
    private Fortran language;
    /** Entryインターフェイス */
    private ILanguageEntry visitor;
    /** 探索履歴リスト */
    private List<Object> listVisit;
    /** 循環を判定するための作業用セット */
    private ArrayList<Procedure> recursiveProcedures;

    /**
     * コンストラクタ
     * @param visitor		Entryインターフェイス
     */
    public LanguageVisitor(ILanguageEntry visitor) {
        this.language = visitor.getLanguage();
        this.visitor = visitor;
        this.setListVisit(visitor.getListVisit());
        this.recursiveProcedures = new ArrayList<Procedure>();
    }


    /**
     * データベースを探索する.
     */
    public void entry() {

        Application.status.setMessageStatus("validate database...");
        String[] moduleNames = language.get_module_name();
        if (moduleNames == null || moduleNames.length <= 0) return;

        for (int i = 0; i < moduleNames.length; i++) {
            Application.status.setMessageStatus("validate database..." + moduleNames[i]);
            Module current_module = language.module(moduleNames[i]);
            entry(current_module);
        }
        Application.status.setMessageStatus("validate database...done");

        return;
    }

    @Override
    public void entry(Module module) {
        addVisitList(module);
        visitor.entry(module);

        clearProcedure();

        entryProgramUnit(module);

        clearProcedure();
        removeVisitList(module);
        return;
    }

    /**
     * ProgramUnitを探索する.
     * @param program		ProgramUnit
     */
    public void entryProgramUnit(ProgramUnit program) {

        Collection<Procedure> procs = program.getChildren();
        if (procs != null && procs.size() > 0) {
            entry(procs.toArray(new Procedure[0]));
        }
        Map<String,VariableDefinition> variables = program.getVariables();
        for (String name : variables.keySet()) {
            VariableDefinition variable = variables.get(name);
            entry(variable);
        }
        List<Type> types = program.getTypeList();
        if (types != null && types.size() > 0) {
            entry(types.toArray(new Type[0]));
        }
        List<Common> commons =  program.getCommonList();
        if (commons != null && commons.size() > 0) {
            entry(commons.toArray(new Common[0]));
        }
        List<UseState> uses =  program.getUseList();
        if (uses != null && uses.size() > 0) {
            entry(uses.toArray(new UseState[0]));
        }

        List<Data> datas =  program.getDataList();
        if (datas != null && datas.size() > 0) {
            entry(datas.toArray(new Data[0]));
        }
        List<Procedures> inters =  program.getInterfaceList();
        if (inters != null && inters.size() > 0) {
            entry(inters.toArray(new Procedures[0]));
        }
        List<Equivalence> equvalences =  program.getEquivalenceList();
        if (equvalences != null && equvalences.size() > 0) {
            entry(equvalences.toArray(new Equivalence[0]));
        }
        List<Directive> directives =  program.getDirectiveList();
        if (directives != null && directives.size() > 0) {
            entry(directives.toArray(new Directive[0]));
        }

    }

    /**
     * サブルーチン,関数リストを探索する.
     * @param procedures	サブルーチン,関数リスト
     */
    private void entry(Procedure[] procedures) {
        if (procedures == null || procedures.length <= 0) return;
        for (Procedure proc : procedures) {
            entry(proc);
        }
        return;
    }

    @Override
    public void entry(Procedure procedure) {
        if (procedure == null) return;
        if (containsProcedure(procedure)) {
            return;
        }
        if (this.containsListVisit(procedure)) return;

        addVisitList(procedure);
        visitor.entry(procedure);

        Variable[] variables = procedure.get_args();
        if (variables != null && variables.length > 0) {
            entry(variables);
        }

        entryProgramUnit(procedure);

        ExecutableBody body = procedure.getBody();
        if (body != null) {
            entry(body);
        }

        List<ProcedureUsage> calls = procedure.getCalls();
        if (calls != null && calls.size() > 0) {
            entry(calls.toArray(new ProcedureUsage[0]));
        }

        removeVisitList(procedure);
        return;
    }

    /**
     * サブルーチン、関数が再帰呼出リストに含まれているかチェックする.
     * 再帰呼出リストに含まれていない場合は、再帰呼出リストに追加する.
     * @param procedure		サブルーチン、関数
     * @return		true=再帰呼出リストに含まれている
     */
    private boolean containsProcedure(Procedure procedure) {
        if (this.recursiveProcedures == null || this.recursiveProcedures.size() <= 0) {
            this.recursiveProcedures = new ArrayList<Procedure>();
        }
        if (this.recursiveProcedures.contains(procedure)) {
            return true;
        }
        // 再帰呼出リストに追加
        this.recursiveProcedures.add(procedure);

        return false;
    }

    /**
     * 再帰呼出リストをクリアする.
     */
    private void clearProcedure() {
        if (this.recursiveProcedures == null || this.recursiveProcedures.size() <= 0) {
            return;
        }
        // 循環リストクリア
        this.recursiveProcedures.clear();

        return;
    }

    /**
     * CALL文、関数呼出リストを探索する.
     * @param calls		CALL文、関数呼出リスト
     */
    private void entry(ProcedureUsage[] calls) {
        if (calls == null || calls.length <= 0) return;
        for (ProcedureUsage call : calls) {
            Procedure callDefinition = call.getCallDefinition();
            entry(callDefinition);
        }

        return;
    }

    /**
     * COMMON文リストを探索する.
     * @param commons		COMMON文リスト
     */
    private void entry(Common[] commons) {
        if (commons == null || commons.length <= 0) return;
        for (Common com : commons) {
            entry(com);
        }
        return;
    }

    /**
     * USE文リストを探索する.
     * @param uses		USE文リスト
     */
    private void entry(UseState[] uses) {
        if (uses == null || uses.length <= 0) return;
        for (UseState use : uses) {
            entry(use);
        }
        return;
    }

    /**
     * DATA文リストを探索する.
     * @param datas		DATA文リスト
     */
    private void entry(Data[] datas) {
        if (datas == null || datas.length <= 0) return;
        for (Data data : datas) {
            entry(data);
        }
        return;
    }

    /**
     * 総称関数群(interface文)リストを探索する.
     * @param inters		総称関数群(interface文)リスト
     */
    private void entry(Procedures[] inters) {
        if (inters == null || inters.length <= 0) return;
        for (Procedures inter : inters) {
            entry(inter);
        }
        return;
    }

    /**
     * EQUIVALENCE文リストを探索する.
     * @param equivalences		EQUIVALENCE文リスト
     */
    private void entry(Equivalence[] equivalences) {
        if (equivalences == null || equivalences.length <= 0) return;
        for (Equivalence equiv : equivalences) {
            entry(equiv);
        }
        return;
    }

    /**
     * DIRECTIVE文リストを探索する.
     * @param directives		DIRECTIVE文リスト
     */
    private void entry(Directive[] directives) {
        if (directives == null || directives.length <= 0) return;
        for (Directive dir : directives) {
            entry(dir);
        }
        return;
    }

    @Override
    public void entry(ProcedureUsage call) {
        if (call == null) return;
        if (this.containsListVisit(call)) return;

        addVisitList(call);
        visitor.entry(call);

        Procedure callDefinition = call.getCallDefinition();
        entry(callDefinition);

        List<Expression> args = call.getArguments();
        if (args != null && args.size() > 0) {
            for (Expression arg : args) {
                entry(arg);
            }
        }

        removeVisitList(call);
        return;
    }

    @Override
    public void entry(ExecutableBody body) {
        if (body == null) return;
        visitor.entry(body);
        List<Block> blocks = body.getChildren();
        for (Block children : blocks) {
            if (children != null) {
                entryBlock(children);
            }
        }
        return;
    }

    /**
     * Blockリストを探索する.
     * @param blocks		Blockリスト
     */
    private void entryBlocks(Block[] blocks) {
        if (blocks == null || blocks.length <= 0) return;
        for (Block children : blocks) {
            if (children != null) {
                entryBlock(children);
            }
        }
        return;
    }

    /**
     * Blockを探索する
     * @param block		Blockクラスオブジェクト
     */
    private void entryBlock(Block block) {
        if (block == null) return;
        if (block instanceof Break) {
            entry((Break)block);
        }
        else if (block instanceof Common) {
            entry((Common)block);
        }
        else if (block instanceof Condition) {
            entry((Condition)block);
        }
        else if (block instanceof Data) {
            entry((Data)block);
        }
        else if (block instanceof Directive) {
            entry((Directive)block);
        }
        else if (block instanceof DoNothing) {
            entry((DoNothing)block);
        }
        else if (block instanceof DynamicAllocation) {
            entry((DynamicAllocation)block);
        }
        else if (block instanceof DynamicDeallocation) {
            entry((DynamicDeallocation)block);
        }
        else if (block instanceof DynamicNullification) {
            entry((DynamicNullification)block);
        }
        else if (block instanceof Equivalence) {
            entry((Equivalence)block);
        }
        else if (block instanceof ExecutableBody) {
            entry((ExecutableBody)block);
        }
        else if (block instanceof GoTo) {
            entry((GoTo)block);
        }
        else if (block instanceof Pause) {
            entry((Pause)block);
        }
        else if (block instanceof Procedures) {
            entry((Procedures)block);
        }
        else if (block instanceof ProcedureUsage) {
            entry((ProcedureUsage)block);
        }
        else if (block instanceof Repetition) {
            entry((Repetition)block);
        }
        else if (block instanceof Return) {
            entry((Return)block);
        }
        else if (block instanceof Selection) {
            entry((Selection)block);
        }
        else if (block instanceof Substitution) {
            entry((Substitution)block);
        }
        else if (block instanceof Termination) {
            entry((Termination)block);
        }
        else if (block instanceof UserDefined) {
            entry((UserDefined)block);
        }
        else if (block instanceof UseState) {
            entry((UseState)block);
        }
        else if (block instanceof Continue) {
            entry((Continue)block);
        }

        List<Block> childrenblocks = block.getChildren();
        if (childrenblocks != null && childrenblocks.size() > 0) {
            entryBlocks(childrenblocks.toArray(new Block[0]));
        }
        return;
    }


    @Override
    public void entry(Break block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);
        removeVisitList(block);
        return;
    }

    @Override
    public void entry(Common block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);
        List<Variable> variables = block.getVariables();
        entry(variables.toArray(new Variable[0]));

        removeVisitList(block);
        return;
    }

    @Override
    public void entry(Condition block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);
        Expression exp = block.getExpression();
        entry(exp);

        removeVisitList(block);
        return;
    }

    @Override
    public void entry(Data block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);
        List<Variable> variables = block.getVariables();
        entry(variables.toArray(new Variable[0]));
        List<Expression> values  = block.getValues();
        if (values != null && values.size() > 0) {
            for (Expression value : values) {
                entry(value);
            }
        }

        removeVisitList(block);
        return;
    }

    @Override
    public void entry(DoNothing block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);
        removeVisitList(block);
        return;
    }

    @Override
    public void entry(Directive block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);
        removeVisitList(block);
        return;
    }


    @Override
    public void entry(DynamicAllocation block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);
        Map<Variable, VariableDimension> targets = block.getTarget();
        for (Variable variable : targets.keySet()) {
            VariableDimension dim = targets.get(variable);
            entry(variable);
            entry(dim);
        }
        Variable error = block.getError();
        entry(error);

        removeVisitList(block);
        return;
    }

    @Override
    public void entry(DynamicDeallocation block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);
        List<Variable> targets = block.getTarget();
        entry(targets.toArray(new Variable[0]));

        Variable error = block.getError();
        entry(error);

        removeVisitList(block);
        return;
    }

    @Override
    public void entry(DynamicNullification block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);
        List<Variable> targets = block.getTarget();
        entry(targets.toArray(new Variable[0]));

        removeVisitList(block);
        return;
    }


    @Override
    public void entry(Equivalence block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);
        List<Variable> targets = block.getVariables();
        entry(targets.toArray(new Variable[0]));

        removeVisitList(block);
        return;
    }

    @Override
    public void entry(GoTo block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);

        removeVisitList(block);
        return;
    }

    @Override
    public void entry(Pause block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);

        removeVisitList(block);
        return;
    }


    @Override
    public void entry(Procedures block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);

        Set<IProcedureItem> items = block.getProcedures();
        for (IProcedureItem item : items) {
            if (item instanceof ProcedureItem) {
                entry((ProcedureItem)item);
            }
            else if (item instanceof ProcedureWithNameOnly) {
                entry((ProcedureWithNameOnly)item);
            }
        }

        removeVisitList(block);
        return;
    }

    @Override
    public void entry(ProcedureItem block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);
        Arguments args = block.getArguments();
        for (Argument arg : args) {
            if (arg.getAttribute() instanceof VariableAttribute) {
                entry((VariableAttribute)arg.getAttribute());
            }
        }

        removeVisitList(block);
        return;
    }

    @Override
    public void entry(Repetition block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);
        Variable iterator = block.getIterator();
        entry(iterator);
        Expression initIterator = block.getInitIterator();
        entry(initIterator);
        Expression endCondition = block.getEndCondition();
        entry(endCondition);
        Expression step = block.getStep();
        entry(step);

        removeVisitList(block);
        return;
    }

    @Override
    public void entry(Return block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);

        removeVisitList(block);
        return;
    }

    @Override
    public void entry(Selection block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);
        List<Condition> conditions = block.getConditions();
        if (conditions != null && conditions.size() > 0) {
            for (Condition condition : conditions) {
                entry(condition);
            }
        }

        Expression caseCondition = block.getCaseCondition();
        entry(caseCondition);

        removeVisitList(block);
        return;
    }

    @Override
    public void entry(Substitution block) {
        if (block == null) return;

        addVisitList(block);
        visitor.entry(block);
        Variable leftVar = block.getLeftValue();
        entry(leftVar);
        Expression rightVar = block.getRightValue();
        entry(rightVar);

        removeVisitList(block);
        return;
    }

    @Override
    public void entry(Termination block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);

        removeVisitList(block);
        return;
    }


    @Override
    public void entry(UserDefined block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);

        removeVisitList(block);
        return;
    }


    @Override
    public void entry(UseState block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);

        removeVisitList(block);
        return;
    }

    @Override
    public void entry(Variable variable) {
        if (variable == null) return;
        addVisitList(variable);
        visitor.entry(variable);

        VariableDefinition definition = variable.getDefinition();
        entry(definition);

        removeVisitList(variable);
        return;
    }

    /**
     * 変数宣言文リストを探索する.
     * @param variables		変数宣言文リスト
     */
    private void entry(Variable[] variables) {
        if (variables == null || variables.length <= 0) return;
        for (Variable var : variables) {
            entry(var);
        }
        return;
    }

    @Override
    public void entry(VariableDefinition definition) {
        if (definition == null) return;
        addVisitList(definition);
        visitor.entry(definition);

        if (definition.getType() instanceof VariableType) {
            entry((VariableType)definition.getType());
        }
        if (definition.getAttribute() instanceof VariableAttribute) {
            entry((VariableAttribute)definition.getAttribute());
        }
        VariableDimension dim = definition.getVariableDimension();
        entry(dim);

        removeVisitList(definition);
        return;
    }

    @Override
    public void entry(VariableType varType) {
        if (varType == null) return;
        addVisitList(varType);
        visitor.entry(varType);

        Expression kind = varType.getKind();
        if (kind != null) {
            entry(kind);
        }
        Expression len = varType.getLen();
        if (len != null) {
            entry(len);
        }
        Type type = varType.getType();
        if (type != null) {
            entry(type);
        }
        Structure structure = varType.getStructure();
        if (structure != null) {
            entry(structure);
        }
        Union union = varType.getUnion();
        if (union != null) {
            entry(union);
        }

        removeVisitList(varType);
        return;
    }

    @Override
    public void entry(VariableAttribute attr) {
        if (attr == null) return;
        addVisitList(attr);
        visitor.entry(attr);

        removeVisitList(attr);
        return;
    }

    @Override
    public void entry(VariableDimension dim) {
        if (dim == null) return;
        addVisitList(dim);
        visitor.entry(dim);

        DimensionIndex[] indices = dim.getIndex();
        if (indices != null && indices.length > 0) {
            for (DimensionIndex idx : indices) {
                entry(idx);
            }
        }

        removeVisitList(dim);
        return;
    }

    @Override
    public void entry(DimensionIndex idx) {
        if (idx == null) return;
        addVisitList(idx);
        visitor.entry(idx);

        if (idx.get_start() != null) {
            entry(idx.get_start());
        }
        if (idx.get_end() != null) {
            entry(idx.get_end());
        }

        removeVisitList(idx);
        return;
    }

    @Override
    public void entry(Expression exp) {
        if (exp == null) return;
        addVisitList(exp);
        visitor.entry(exp);

        List<Variable> variables = exp.getVariables();
        if (variables != null && variables.size() > 0) {
            for (Variable var : variables) {
                entry(var);
            }
        }
        List<ProcedureUsage> calls = exp.getFuncCalls();
        if (calls != null && calls.size() > 0) {
            for (ProcedureUsage var : calls) {
                entry(var);
            }
        }

        removeVisitList(exp);
        return;
    }

    /**
     * TYPE文リストを探索する.
     * @param types		TYPE文リスト
     */
    private void entry(Type[]  types) {
        if (types == null || types.length <= 0) return;
        for (Type type : types) {
            entry(type);
        }
        return;
    }

    @Override
    public void entry(Type  type) {
        if (type == null) return;
        addVisitList(type);
        visitor.entry(type);
        List<VariableDefinition> varDefs = type.getDefinitions();
        if (varDefs != null && varDefs.size() > 0) {
            for (VariableDefinition vardef : varDefs) {
                entry(vardef);
            }
        }
        removeVisitList(type);
        return;
    }


    @Override
    public void entry(Structure structure) {
        if (structure == null) return;
        if (this.containsListVisit(structure)) return;

        addVisitList(structure);
        visitor.entry(structure);
        List<VariableDefinition> varDefs = structure.getDefinitions();
        if (varDefs != null && varDefs.size() > 0) {
            for (VariableDefinition vardef : varDefs) {
                entry(vardef);
            }
        }
        removeVisitList(structure);
        return;
    }

    @Override
    public void entry(Union union) {
        if (union == null) return;
        addVisitList(union);
        visitor.entry(union);
        Set<jp.riken.kscope.language.fortran.Map> maps = union.getMaps();
        for (jp.riken.kscope.language.fortran.Map map : maps) {
            List<VariableDefinition> varDefs = map.getDefinitions();
            if (varDefs != null && varDefs.size() > 0) {
                for (VariableDefinition vardef : varDefs) {
                    entry(vardef);
                }
            }
        }

        removeVisitList(union);
        return;
    }

    @Override
    public void entry(Continue block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);

        removeVisitList(block);
        return;
    }


    @Override
    public void entry(ProcedureWithNameOnly block) {
        if (block == null) return;
        addVisitList(block);
        visitor.entry(block);
        Procedure procedure = block.getDeclaration();
        entry(procedure);

        removeVisitList(block);
        return;
    }

    /**
     * 探索履歴リストに追加する.
     * @param block		探索履歴リスト
     */
    private void addVisitList(Object block) {
        if (this.listVisit == null) {
            this.listVisit = new ArrayList<Object>();
        }
        this.listVisit.add(block);
    }

    /**
     * 探索履歴リストから削除する.
     * @param block		探索履歴リスト
     */
    private void removeVisitList(Object block) {
        if (this.listVisit == null) return;
        this.listVisit.remove(block);
    }

    /**
     * 探索履歴リストを取得する.
     * @return		探索履歴リスト
     */
    @Override
    public List<Object> getListVisit() {
        return this.listVisit;
    }


    /**
     * 探索履歴リストを設定する
     * @param list		探索履歴リスト
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
        if (this.listVisit == null) return false;
        return this.listVisit.contains(obj);
    }

    @Override
    public Fortran getLanguage() {
        return this.language;
    }


    @Override
    public void setLanguage(Fortran language) {
        this.language = language;
    }

    /**
     * 付加情報ブロックの探索を行う
     * @param info		付加情報ブロック
     */
    public void entryInformation(IInformation info) {
        if (info == null) return;
        if (info instanceof Block) {
            entryBlock((Block)info);
        }
        else if (info instanceof Procedure) {
            entry((Procedure)info);
        }
        else if (info instanceof Module) {
            entry((Module)info);
        }
        else if (info instanceof VariableDefinition) {
            entry((VariableDefinition)info);
        }
        return;
    }


}
