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
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.Common;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.UseState;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.model.ReferenceModel;

/**
 * 宣言・定義・参照サービスクラス.<br/>
 * 宣言・定義・参照一覧を作成する
 *
 * @author riken
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
     * @param variable
     *            参照一覧
     */
    public void analysisReference(VariableDefinition variable) {
        if (variable == null) {
            return;
        }

        // 参照一覧モデルに設定する
        DefaultMutableTreeNode root = new DefaultMutableTreeNode(variable);
        DefaultMutableTreeNode decNode = new DefaultMutableTreeNode(Message.getString("analysisreferenceservice.reference.declaration")); //宣言
        DefaultMutableTreeNode refNode = new DefaultMutableTreeNode(Message.getString("analysisreferenceservice.reference.reference")); //参照
        DefaultMutableTreeNode defNode = new DefaultMutableTreeNode(Message.getString("analysisreferenceservice.reference.definition")); //定義
        root.add(decNode);
        root.add(refNode);
        root.add(defNode);

        // 宣言ノードの作成
        DefaultMutableTreeNode dec = new DefaultMutableTreeNode(variable);
        DefaultMutableTreeNode mother = new DefaultMutableTreeNode(variable.getMother());
        dec.setAllowsChildren(false);
        mother.add(dec);
        decNode.add(mother);

        Set<ProgramUnit> refdefUnit = new HashSet<ProgramUnit>();// 参照・定義しているプログラム単位のセット
        // 宣言が属するプログラム単位、および副プログラム単位による参照・定義の一覧を作成する
        refdefUnit.addAll(this.searchChildrenWithScope(variable));

        // USE文による参照・定義の一覧を作成する
        refdefUnit.addAll(variable.getReferMember());

        for (ProgramUnit pu : refdefUnit) {
            String name = variable.get_name();
            // USE文による名前の変換が無いかチェック
            List<UseState> uses = pu.getUseList();
            for (UseState use : uses) {
                name = use.translation(variable);
                if (!(name.equalsIgnoreCase(variable.get_name()))) {
                    break;
                }
            }

            // 参照一覧を作成する
            Map<String, Set<IBlock>> refs = pu.getRefVariableNames();
            Set<IBlock> blk = refs.get(name);
            if (blk != null) {
                DefaultMutableTreeNode pr = new DefaultMutableTreeNode(pu);
                for (IBlock bk : blk) {
                    DefaultMutableTreeNode bl = new DefaultMutableTreeNode(bk);
                    pr.add(bl);
                }
                refNode.add(pr);
            }

            // 定義一覧を作成する
            Map<String, Set<IBlock>> defs = pu.getDefVariableNames();
            blk = defs.get(name);
            if (blk != null) {
                DefaultMutableTreeNode pr = new DefaultMutableTreeNode(pu);
                for (IBlock bk : blk) {
                    DefaultMutableTreeNode bl = new DefaultMutableTreeNode(bk);
                    pr.add(bl);
                }
                defNode.add(pr);
            }
        }

        // COMMON属性の場合の一覧
        if (this.fortranDb.getCommonMap() != null) {
            ProgramUnit motherUnit = variable.getMother();
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
                    VariableDefinition def = pu.getVariableMap(localName);
                    if (def != null) {
                        // 宣言
                        DefaultMutableTreeNode defCom = new DefaultMutableTreeNode(
                                def);
                        DefaultMutableTreeNode motherCom = new DefaultMutableTreeNode(
                                def.getMother());
                        defCom.setAllowsChildren(false);
                        motherCom.add(defCom);
                        decNode.add(motherCom);
                        // 参照一覧を作成する
                        Map<String, Set<IBlock>> refs = pu.getRefVariableNames();
                        Set<IBlock> blk = refs.get(localName);
                        if (blk != null) {
                            DefaultMutableTreeNode pr = new DefaultMutableTreeNode(pu);
                            for (IBlock bk : blk) {
                                DefaultMutableTreeNode bl = new DefaultMutableTreeNode(bk);
                                pr.add(bl);
                            }
                            refNode.add(pr);
                        }

                        // 定義一覧を作成する
                        Map<String, Set<IBlock>> defs = pu.getDefVariableNames();
                        blk = defs.get(localName);
                        if (blk != null) {
                            DefaultMutableTreeNode pr = new DefaultMutableTreeNode(pu);
                            for (IBlock bk : blk) {
                                DefaultMutableTreeNode bl = new DefaultMutableTreeNode(bk);
                                pr.add(bl);
                            }
                            defNode.add(pr);
                        }
                    }
                }
            }
        }

        // ツリーの生成
        DefaultTreeModel tree = new DefaultTreeModel(root);

        // ツリーの設定
        this.modelReference.setTreeModel(tree);
    }

    /**
     * 指定した変数宣言の属するプログラム単位内において、有効域となるプログラム単位のセットを返す。
     *
     * @param var
     *            変数宣言
     * @return プログラム単位のセット。少なくとも宣言が属するモジュールを要素に持つ。
     */
    private Set<ProgramUnit> searchChildrenWithScope(VariableDefinition var) {
        Set<ProgramUnit> pus = new HashSet<ProgramUnit>();
        pus.add(var.getMother());
        for (Procedure child : var.getMother().getChildren()) {
            // 副プログラムに同一の名前の宣言が無ければ追加
            if (child.get_variable(var.get_name()) == null) {
                pus.add(child);
            }
            for (Procedure grnd : child.getChildren()) {
                if (grnd.get_variable(var.get_name()) == null) {
                    pus.add(grnd);
                }
            }
        }

        return pus;
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
        String varName = line.getStatement().toLowerCase();

        // CodeLineの情報から、対応するプログラム単位を探索する。
        ProgramUnit currentProc = this.getCurrentProcedure(line);

        if (currentProc == null) {
            return;
        }

        // 変数宣言を取得する
        VariableDefinition varDef = null;
        // TODO モジュールを対象とするか要検討
        if (currentProc instanceof Procedure) {
            varDef = ((Procedure) currentProc).getVariableMap(varName);
        } else {
            varDef = currentProc.get_variable(varName);
        }
        // 参照一覧モデルを作成する
        this.analysisReference(varDef);
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
                Collection<Procedure> children = pu.getChildren();
                for (Procedure child : children) {
                    sPos = child.getStartPos();
                    ePos = child.getEndPos();
                    if (sPos <= lineNo && lineNo <= ePos) {
                        punit = child;
                        Collection<Procedure> children2 = child.getChildren();
                        for (Procedure child2 : children2) {
                            sPos = child.getStartPos();
                            ePos = child.getEndPos();
                            if (sPos <= lineNo && lineNo <= ePos) {
                                punit = child2;
                            }
                        }
                    }
                }
            }
        }
        return punit;
    }
}
