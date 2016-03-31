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
package jp.riken.kscope.service;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.Common;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.UseState;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.utils.LanguageUtils;
import jp.riken.kscope.model.ReferenceModel;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 宣言・定義・参照サービスクラス.<br/>
 * 宣言・定義・参照一覧を作成する
 *
 * @author RIKEN
 */
public class AnalysisReferenceService extends AnalysisBaseService {

    /** 参照一覧モデル. */
    private ReferenceModel modelReference;

    /**
     * コンストラクタ.
     *
     * @param fortran
     *            フォートランデータベース
     */
    public AnalysisReferenceService(Fortran fortran) {
        super(fortran);
    }

    /**
     * 参照一覧モデルを設定する.
     *
     * @param model
     *            参照一覧モデル
     */
    public void setModelReference(ReferenceModel model) {
        this.modelReference = model;
    }

    /**
     * 参照一覧を作成する.
     *
     * @param variable           変数定義
     * @param  variable_name     変数名
     */
    public void analysisReference(VariableDefinition variable) {
        this.analysisReference(variable, null);
    }

    /**
     * 参照一覧を作成する.
     *
     * @param variable           変数定義
     * @param  variable_name     変数名：構造体メンバの場合は必須
     */
    public void analysisReference(VariableDefinition variable, String variable_name) {
        if (variable == null) {
            return;
        }

        // 参照一覧モデルに設定する
        DefaultMutableTreeNode root = null;
        if (variable_name != null && !variable_name.isEmpty()) {
            root = new DefaultMutableTreeNode(variable.toString() + " : " + variable_name);
        }
        else {
            root = new DefaultMutableTreeNode(variable);
        }
        DefaultMutableTreeNode decNode = new DefaultMutableTreeNode(Message.getString("analysisreferenceservice.reference.declaration")); //宣言
        DefaultMutableTreeNode refNode = new DefaultMutableTreeNode(Message.getString("analysisreferenceservice.reference.reference")); //参照
        DefaultMutableTreeNode defNode = new DefaultMutableTreeNode(Message.getString("analysisreferenceservice.reference.definition")); //定義
        root.add(decNode);
        root.add(refNode);
        root.add(defNode);

        // 宣言ノードの作成
        DefaultMutableTreeNode dec = new DefaultMutableTreeNode(variable);
        IBlock dec_proc = variable.getScopeDeclarationsBlock();
        if (dec_proc == null) {
            dec_proc = variable.getMother();
        }
        DefaultMutableTreeNode mother = new DefaultMutableTreeNode(dec_proc);
        dec.setAllowsChildren(false);
        mother.add(dec);
        decNode.add(mother);

        Set<IBlock> refdefUnit = new LinkedHashSet<IBlock>();// 参照・定義しているプログラム単位のセット
        // 宣言が属するプログラム単位、および副プログラム単位による参照・定義の一覧を作成する
        refdefUnit.addAll(this.searchChildrenWithScope(variable));

        // USE文による参照・定義の一覧を作成する
        refdefUnit.addAll(variable.getReferMember());

        if (variable_name == null || variable_name.isEmpty()) {
            variable_name = variable.get_name();
        }

        ProgramUnit ref_proc = null;
        ProgramUnit def_proc = null;
        DefaultMutableTreeNode ref_proc_node = null;
        DefaultMutableTreeNode def_proc_node = null;
        for (IBlock pu : refdefUnit) {
            if (!(pu instanceof ProgramUnit)) continue;
            String name = variable.get_name();
            ProgramUnit proc = (ProgramUnit)pu;

            // USE文による名前の変換が無いかチェック
            List<UseState> uses = proc.getUseList();
            for (UseState use : uses) {
                variable_name = use.translation(variable);
                if (!(variable_name.equalsIgnoreCase(variable.get_name()))) {
                    break;
                }
            }

            // 参照一覧を作成する
            Set<IBlock> ref_blks = proc.getRefVariableName(variable_name);
            if (ref_blks != null) {
                if (ref_proc != proc) {
                    ref_proc = proc;
                    ref_proc_node = new DefaultMutableTreeNode(ref_proc);
                }
                for (IBlock bk : ref_blks) {
                    // 変数定義の変数が存在しているかチェックする
                    Variable found_var = this.getVariable(bk, variable);
                    if (found_var != null) {
                        DefaultMutableTreeNode bl = new DefaultMutableTreeNode(bk);
                        ref_proc_node.add(bl);
                    }
                }
                if (ref_proc_node.getChildCount() > 0) {
                    refNode.add(ref_proc_node);
                    }
                }

            // 定義一覧を作成する
            Set<IBlock> def_blks = proc.getDefVariableName(variable_name);
            if (def_blks != null) {
                if (def_proc != proc) {
                    def_proc = proc;
                    def_proc_node = new DefaultMutableTreeNode(def_proc);
                }
                for (IBlock bk : def_blks) {
                    // 変数定義の変数が存在しているかチェックする
                    Variable found_var = this.getVariable(bk, variable);
                    if (found_var != null) {
                        DefaultMutableTreeNode bl = new DefaultMutableTreeNode(bk);
                        def_proc_node.add(bl);
                    }
                }
                if (def_proc_node.getChildCount() > 0) {
                    defNode.add(def_proc_node);
                }
            }
        }

        // COMMON属性の場合の一覧
        if (this.fortranDb.getCommonMap() != null
            && variable.getMother() instanceof ProgramUnit) {

            ProgramUnit motherUnit = (ProgramUnit)variable.getMother();
            List<Common> comList = motherUnit.getCommonList();
            List<ProgramUnit> comUnits = new ArrayList<ProgramUnit>();
            String comName = "";
            int varidx = 0;
            searchCom:
                for (Common com: comList) {
                    varidx = 0;
                    for (Variable var: com.getVariables()) {
                        if (var.getName().equalsIgnoreCase(variable.get_name())) {
                            comName = com.getName();
                            comUnits = this.fortranDb.getCommonUnit(comName);
                            break searchCom;
                        }
                        varidx++;
                    }
                }
            for (ProgramUnit pu: comUnits) {
                if (!(refdefUnit.contains(pu))) {
                    List<Common> puComs = pu.getCommonList();
                    String localName = "";
                    for (Common cm: puComs) {
                        if (cm.getVariables() == null || cm.getVariables().size() <= 0) continue;
                        if (cm.getVariables().size() <= varidx) continue;
                        if (cm.getName().equalsIgnoreCase(comName)) {
                            localName = cm.getVariables().get(varidx).getName();
                            break;
                        }
                    }
                    VariableDefinition[] defs = pu.searchVariableDefinitionFromMap(localName);
                    if (defs != null && defs.length > 0) {
                        VariableDefinition def = defs[0];
                        // 宣言
                        DefaultMutableTreeNode defCom = new DefaultMutableTreeNode(def);
                        DefaultMutableTreeNode motherCom = new DefaultMutableTreeNode(def.getMother());
                        defCom.setAllowsChildren(false);
                        motherCom.add(defCom);
                        decNode.add(motherCom);
                        // 参照一覧を作成する
                        Set<IBlock> ref_blks = pu.getRefVariableName(localName);
                        if (ref_blks != null) {
                            DefaultMutableTreeNode pr = new DefaultMutableTreeNode(pu);
                            for (IBlock bk : ref_blks) {
                                DefaultMutableTreeNode bl = new DefaultMutableTreeNode(bk);
                                pr.add(bl);
                            }
                            refNode.add(pr);
                        }

                        // 定義一覧を作成する
                        Set<IBlock> def_blks = pu.getDefVariableName(localName);
                        if (def_blks != null) {
                            DefaultMutableTreeNode pr = new DefaultMutableTreeNode(pu);
                            for (IBlock bk : def_blks) {
                                DefaultMutableTreeNode bl = new DefaultMutableTreeNode(bk);
                                pr.add(bl);
                            }
                            defNode.add(pr);
                        }
                    }
                }
            }
        }

        // 行番号でソートを行う
        SwingUtils.sortBlockTreeNode(refNode);
        SwingUtils.sortBlockTreeNode(defNode);

        // ツリーの生成
        DefaultTreeModel tree = new DefaultTreeModel(root);

        // ツリーの設定
        this.modelReference.setTreeModel(tree);
    }

    /**
     * 指定した変数宣言の属するプログラム単位内において、有効域となるプログラム単位のセットを返す。
     *
     * @param var            変数宣言
     * @return プログラム単位のセット。少なくとも宣言が属するモジュールを要素に持つ。
     */
    private Set<IBlock> searchChildrenWithScope(VariableDefinition var) {
        Set<IBlock> blocks = new LinkedHashSet<IBlock>();
        if (var.getMother() instanceof ProgramUnit) {
            blocks.add(var.getMother());
        }

        String var_name = var.get_name();
        // 子ProgramUnitブロックを取得する
        ProgramUnit parent = var.getScopeDeclarationsBlock();
        if (parent == null) return blocks;
        List<IBlock> child_blocks = parent.getChildren();
        if (child_blocks != null) {
            for (IBlock child : child_blocks) {
                if (!(child instanceof ProgramUnit)) continue;
                // 同一の名前の宣言が無ければ追加
                if (((ProgramUnit)child).get_variable(var_name) == null) {
                    blocks.add((IBlock)child);
                }
            }
        }

        return blocks;
    }

    /**
     * ソースコード上で選択された文字列から参照一覧を作成する.
     *
     * @param line
     *            選択行
     */
    public void analysisReference(CodeLine line) {
        if (line == null) return;
        // 選択変数

        String varName = line.getStatement();
        // 変数名を正規化する：struct[n].no -> struct.no
        varName = LanguageUtils.normalizeVariableName(varName);

        VariableDefinition varDef = null;
        IBlock block = this.getCurrentBlock(line);
        if (block != null) {
            Set<Variable> list = block.getAllVariables();
            if (list == null && block.getMotherBlock() != null) {
                list = block.getMotherBlock().getAllVariables();
            }
            if (list != null) {
                for (Variable var : list) {
                    if (var == null) continue;
                    String name = LanguageUtils.normalizeVariableName(var.getName());
                    if (name == null) continue;
                    //if (name.equalsIgnoreCase(varName)) {
                    if (var.getName().equalsIgnoreCase(varName)) {
                        varDef = var.getDefinition();
                    }
                    if (varDef != null) break;
                }
            }
        }

        if (varDef == null) {
            // CodeLineの情報から、対応するプログラム単位を探索する。
            ProgramUnit current_block = this.getCurrentDeclarations(line);

            if (current_block == null) {
                // エラーメッセージ:languageservice.procedure.error=[%s]は存在しません。
                String error_message = Message.getString("kscope.error.notfound.line");
                this.addErrorInfo(line, error_message);
                return;
            }

            // 変数宣言を取得する
            while (current_block != null) {
                varDef = current_block.get_variable(varName);
                if (varDef == null) {
                    Variable[] vars = current_block.searchVariableFromMap(varName);
                    if (vars != null) {
                        for (Variable var : vars) {
                            if (var == null) continue;
                            if (var.getParentStatement() == null) continue;
                            CodeLine var_line = var.getParentStatement().getStartCodeLine();
                            if (line.equalsLineno(var_line)) {
                                varDef = current_block.getVariableMap(var);
                            }
                            if (varDef != null) break;
                        }
                    }
                }
                if (varDef != null) break;
                if (((IBlock)current_block).getMotherBlock() == null) break;
                current_block = ((IBlock)current_block).getMotherBlock().getScopeDeclarationsBlock();
            }
        }

        // 参照一覧モデルを作成する
        this.analysisReference(varDef, varName);
        return;
    }

    /**
     * コードラインが属するプログラム単位を返す。
     *
     * @param line
     *            コード行情報
     * @return プログラム単位。無ければnullを返す。
     */
    private ProgramUnit getCurrentProcedure(CodeLine line) {
        if (line == null) return null;
        SourceFile file = line.getSourceFile();
        int lineNo = line.getStartLine();
        // System.out.println("lineNo " + lineNo);
        // fileにあるプログラム単位のリストを取得
        List<ProgramUnit> pus = this.fortranDb.getProgramUnits(file);
        // lineNoを含むプログラム単位を習得
        ProgramUnit punit = null;
        for (ProgramUnit pu : pus) {
            int sPos = pu.getStartPos();
            int ePos = pu.getEndPos();
            if (sPos <= lineNo && lineNo <= ePos) {
                punit = pu;
                List<IBlock> childrens = pu.getChildren();
                for (IBlock child : childrens) {
                    if (!(child instanceof ProgramUnit)) continue;
                    sPos = ((ProgramUnit)child).getStartPos();
                    ePos = ((ProgramUnit)child).getEndPos();
                    if (sPos <= lineNo && lineNo <= ePos) {
                        punit = (ProgramUnit)child;
                        List<IBlock> children2 = child.getChildren();
                        for (IBlock child2 : children2) {
                            if (!(child2 instanceof ProgramUnit)) continue;
                            sPos = ((ProgramUnit)child2).getStartPos();
                            ePos = ((ProgramUnit)child2).getEndPos();
                            if (sPos <= lineNo && lineNo <= ePos) {
                                punit = (ProgramUnit)child2;
                            }
                        }
                    }
                }
            }
        }
        return punit;
    }


    /**
     * コードラインが属するプログラム単位を返す。
     *
     * @param line
     *            コード行情報
     * @return プログラム単位。無ければnullを返す。
     */
    private ProgramUnit getCurrentDeclarations(CodeLine line) {
        if (line == null) return null;

        // lineNoを含むプログラム単位を習得
        ProgramUnit punit = this.getCurrentProcedure(line);
        if (punit == null) {
            // Moduleから検索する
            punit = this.getCurrentModule(line);
        }
        if (punit == null) {
            return null;
        }

        // 変数宣言ブロックを取得する
        Set<ProgramUnit> list = punit.getDeclarationsBlocks();
        if (list == null) return punit;

        ProgramUnit current_dec = null;
        for (ProgramUnit dec : list) {
            if (!(dec instanceof IBlock)) continue;
            CodeLine start = ((IBlock)dec).getStartCodeLine();
            CodeLine end = ((IBlock)dec).getEndCodeLine();
            if (start == null) continue;
            if (end == null) end = start;
            if (line.isOverlap(start, end)) {
                current_dec = dec;
            }
        }

        if (current_dec == null) return punit;

        return current_dec;
    }


    /**
     * コードラインが属するプログラム単位を返す。
     *
     * @param line
     *            コード行情報
     * @return プログラム単位。無ければnullを返す。
     */
    private IBlock getCurrentBlock(CodeLine line) {
        if (line == null) return null;

        // lineNoを含むプログラム単位を習得
        ProgramUnit punit = this.getCurrentProcedure(line);
        if (punit == null) {
            // Moduleから検索する
            punit = this.getCurrentModule(line);
        }
        if (punit == null) {
            return null;
        }

        IBlock current_block = this.getCurrentBlock(line, punit);

        if (current_block == null) return punit;

        return current_block;
    }

    /**
     * コードラインが属するプログラム単位を返す。
     *
     * @param line
     *            コード行情報
     * @return プログラム単位。無ければnullを返す。
     */
    private IBlock getCurrentBlock(CodeLine line, IBlock block) {
        if (block == null) return null;

        IBlock current_block = null;

        if (block instanceof ProgramUnit) {
            VariableDefinition[] defs = ((ProgramUnit)block).get_variables();
            if (defs != null) {
                for (VariableDefinition def : defs) {
                    CodeLine start = def.getStartCodeLine();
                    CodeLine end = def.getEndCodeLine();
                    if (start == null) continue;
                    if (end == null) end = start;
                    if (line.isOverlap(start, end)) {
                        current_block = def;
                    }
                }
            }
        }

        if (block.getBlockType() == BlockType.PROCEDURE) {
            IBlock body_block = this.getCurrentBlock(line, ((Procedure)block).getBody());
            if (body_block.getBlockType() == BlockType.BODY) {
                body_block = null;
            }
            if (body_block != null) {
                current_block = body_block;
            }
        }

        List<IBlock> list = block.getChildren();
        for (IBlock item : list) {
            CodeLine start = item.getStartCodeLine();
            CodeLine end = item.getEndCodeLine();
            if (start == null) continue;
            if (end == null) end = start;
            if (line.isOverlap(start, end)) {
                IBlock child_block = this.getCurrentBlock(line, item);
                if (child_block != null) {
                    current_block = child_block;
                }
            }
        }

        if (current_block == null) return block;

        return current_block;
    }

    /**
     * コードラインが属するモジュール(ファイル)単位を返す。
     *
     * @param line            コード行情報
     * @return モジュール(ファイル)単位。無ければnullを返す。
     */
    private ProgramUnit getCurrentModule(CodeLine line) {
        if (line == null) return null;
        SourceFile file = line.getSourceFile();
        int lineNo = line.getStartLine();

        // fileにあるプログラム単位のリストを取得
        ProgramUnit pus = this.fortranDb.getModule(file);

        return pus;
    }

    /**
     * ブロック中の変数から変数定義と一致する変数を取得する
     * @param block        検索ブロック
     * @param def        変数定義
     * @return            変数
     */
    private Variable getVariable(IBlock block, VariableDefinition def) {
        if (block == null) return null;
        if (def == null) return null;
        Set<Variable> vars = block.getBlockVariables();
        if (vars == null) return null;
        for (Variable var : vars) {
            if (var == null) continue;
            if (var.getDefinition() == null) continue;
            if (var.getDefinition() == def) {
                return var;
            }
        }

        return null;
    }
}
