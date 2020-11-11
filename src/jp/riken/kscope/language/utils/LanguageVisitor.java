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
 * Fortran database search class
 * @author RIKEN
 */
public class LanguageVisitor implements ILanguageEntry {

	/** Fortran database */
	private Fortran language;
	/** Entry interface */
	private ILanguageEntry visitor;
	/** Search history list */
	private List<Object> listVisit;
    /** Working set for determining circulation */
    private ArrayList<Procedure> recursiveProcedures;

	/**
* Constructor
* @param visitor Entry interface
*/
	public LanguageVisitor(ILanguageEntry visitor) {
		this.language = visitor.getLanguage();
		this.visitor = visitor;
		this.setListVisit(visitor.getListVisit());
		this.recursiveProcedures = new ArrayList<Procedure>();
	}


    /**
     * Search the database.
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
* Explore ProgramUnit.
* @param program ProgramUnit
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
* Search subroutines and function lists.
* @param procedures Subroutine, function list
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
* Check if the subroutine or function is included in the recursive call list.
* If it is not included in the recursive call list, add it to the recursive call list.
* @param procedure Subroutines, functions
* @return true = included in recursive call list
*/
    private boolean containsProcedure(Procedure procedure) {
    	if (this.recursiveProcedures == null || this.recursiveProcedures.size() <= 0) {
    		this.recursiveProcedures = new ArrayList<Procedure>();
    	}
    	if (this.recursiveProcedures.contains(procedure)) {
    		return true;
    	}
    	// Add to recursive call list
    	this.recursiveProcedures.add(procedure);

		return false;
	}

    /**
     * Clear the recursive call list.
     */
    private void clearProcedure() {
    	if (this.recursiveProcedures == null || this.recursiveProcedures.size() <= 0) {
    		return;
    	}
    	// Clear the circular list
    	this.recursiveProcedures.clear();

		return;
	}

    /**
     * CALL statement, search function call list.
     * @param calls CALL statement, function call list
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
* Search the COMMON statement list.
* @param commons COMMON statement list
*/
    private void entry(Common[] commons) {
    	if (commons == null || commons.length <= 0) return;
        for (Common com : commons) {
        	entry(com);
        }
        return;
    }

    /**
     * Search the USE statement list.
     * @param uses USE statement list
     */
    private void entry(UseState[] uses) {
    	if (uses == null || uses.length <= 0) return;
        for (UseState use : uses) {
        	entry(use);
        }
        return;
    }

    /**
     * Search the DATA statement list.
     * @param datas DATA statement list
     */
    private void entry(Data[] datas) {
    	if (datas == null || datas.length <= 0) return;
        for (Data data : datas) {
        	entry(data);
        }
        return;
    }

    /**
     * Search the generic function group (interface statement) list.
     * @param inters Generic function group (interface statement) list
     */
    private void entry(Procedures[] inters) {
    	if (inters == null || inters.length <= 0) return;
        for (Procedures inter : inters) {
        	entry(inter);
        }
        return;
    }

    /**
     * Search the EQUIVALENCE statement list.
     * @param equivalences EQUIVALENCE statement list
     */
    private void entry(Equivalence[] equivalences) {
    	if (equivalences == null || equivalences.length <= 0) return;
        for (Equivalence equiv : equivalences) {
        	entry(equiv);
        }
        return;
    }

    /**
     * Search the DIRECTIVE statement list.
     * @param directives DIRECTIVE statement list
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
* Search the Block list.
* @param blocks Block list
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
     * Search for Block
     * @param block Block class object
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
     * Search the variable declaration statement list.
     * @param variables Variable declaration statement list
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
* Search the TYPE statement list.
* @param types TYPE statement list
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
* Add to the search history list.
* @param block Search history list
*/
	private void addVisitList(Object block) {
		if (this.listVisit == null) return;
		this.listVisit.add(block);
	}

	/**
* Delete from the search history list.
* @param block Search history list
*/
	private void removeVisitList(Object block) {
		if (this.listVisit == null) return;
		this.listVisit.remove(block);
	}

	@Override
	public List<Object> getListVisit() {
		return this.listVisit;
	}


	@Override
	public void setListVisit(List<Object> list) {
		this.listVisit = list;
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
* Search for additional information blocks
* @param info Additional information block
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
