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
import java.util.Enumeration;
import java.util.List;
import java.util.Set;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.Condition;
import jp.riken.kscope.language.ExecutableBody;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.KeywordArgument;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.Selection;
import jp.riken.kscope.language.Substitution;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.utils.LanguageUtils;
import jp.riken.kscope.model.TraceResultModel;

/**
 * 分析：変数トレースを行う。
 *
 * @author riken
 */
public class AnalysisTraceService extends AnalysisBaseService {

    /** トレース対象変数名. */
    private String traceWord;

    /**
     * コンストラクタ.
     *
     * @param fortran
     *            フォートランデータベース
     */
    public AnalysisTraceService(Fortran fortran) {
        super(fortran);
    }

    /**
     * トレースを開始する.<br/>
     * トレース対象変数名, トレース行情報から対象プロシージャ、トレースブロックを探索する.
     *
     * @param line
     *            トレース行情報
     * @return トレース結果モデル
     */
    public TraceResultModel analysisTraceStart(CodeLine line) {

        if (line == null) {
            return null;
        }
        String traceWrd = this.traceWord.toLowerCase();
        // CodeLineの情報から、対応するプログラム単位を探索する。
        LanguageUtils utils = new LanguageUtils(this.fortranDb);
        ProgramUnit currentProc = utils.getCurrentProgramUnit(line);
        if (currentProc == null) {
            return null;
        }

        // 参照一覧モデルに設定する
        IBlock currentblk = currentProc;
        DefaultMutableTreeNode root = new DefaultMutableTreeNode(currentProc);
        if (currentProc instanceof Procedure) {
            Set<IBlock> blks = ((Procedure) currentProc)
                    .getRefDefBlocks(traceWrd);
            if (blks != null) {
                for (IBlock blk : blks) {
                    this.addBlockToRoot(root, blk);
                    // CodeLineに対応するブロックが含まれれば登録する。
                    int currentLine = blk.getStartCodeLine().getStartLine();
                    if (currentLine == line.getStartLine()) {
                        currentblk = blk;
                    }
                }
            }
        }
        // TODO モジュールの場合どうするか要検討

        // 結果表示ツリーの生成
        DefaultTreeModel tree = new DefaultTreeModel(root);

        // トレースモデルの作成
        TraceResultModel modelTrace = new TraceResultModel();
        modelTrace.setTraceWord(traceWrd); // トレース対象変数名
        modelTrace.setTreeModel(tree); // 表示ツリーモデル
        modelTrace.setTitle(traceWrd); // 表示タイトル
        modelTrace.setSelectedBlock(currentblk); // 選択ブロック

        return modelTrace;
    }

    /**
     * rootにブロックを追加する。ブロックが分岐に属する場合は分岐を含めて構築する。
     *
     * @param root
     *            ルートノード
     * @param blk
     *            追加するブロック
     */
    private void addBlockToRoot(DefaultMutableTreeNode root, IBlock blk) {
        DefaultMutableTreeNode node = new DefaultMutableTreeNode(blk);
        if (blk instanceof Block) {
            Block mother = ((Block) blk).get_mother();
            DefaultMutableTreeNode childNode = new DefaultMutableTreeNode(blk);
            // 実行文のトップに行くまで続ける。無限ループに注意
            while (!(mother instanceof ExecutableBody)) {
                DefaultMutableTreeNode mamNode = this.searchTree(root, mother);
                if (mother instanceof Condition) {
                    Selection sel = (Selection) mother.get_mother();
                    if (!(sel.isSelect())) {
                        if (sel.getConditions().get(0).equals(mother)) {
                            mamNode = this.searchTree(root, sel);
                        }
                        mother = sel;
                    }
                }
                mamNode.add(childNode);
                mother = mother.get_mother();
                childNode = mamNode;
            }
            root.add(childNode);
            return;
        }
        root.add(node);
    }

    /**
     * rootにblkを持つノードがあればそのノードを返す。
     *
     * @param node
     *            ルートノード
     * @param blk
     *            ブロック
     * @return ノード。無ければ新たに生成したノードを返す。
     */
    private DefaultMutableTreeNode searchTree(DefaultMutableTreeNode node, Block blk) {
        if (node.getUserObject().equals(blk)) {
            return node;
        }

        // ツリーノードを順方向で列挙
        Enumeration<?> depth = node.preorderEnumeration();
        while(depth.hasMoreElements()) {
            DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode)depth.nextElement();
            // ノード検索を行う
            if (treeNode == null || treeNode.getUserObject() == null) {
                continue;
            }
            if (treeNode.getUserObject() == blk) {
            	return treeNode;
            }
        }
        DefaultMutableTreeNode newnode = new DefaultMutableTreeNode(blk);
        return newnode;
    }


    /**
     * トレース:インを行う.<br/>
     * トレース対象変数名, トレースブロックからサブルーチン、関数のプロシージャを探索する.
     *
     * @param block
     *            トレースブロック(現在のトレースの選択ブロック)
     * @return トレース結果モデルリスト
     */
    public TraceResultModel[] analysisTraceIn(IBlock block) {
        if (block == null) {
            return null;
        }
        List<ProcedureUsage> pus = new ArrayList<ProcedureUsage>();
        ProcedureUsage punit = null;
        if (block instanceof ProcedureUsage) {
            punit = (ProcedureUsage) block;
            pus.add(punit);
        } else if (block instanceof Substitution) {
            Set<ProcedureUsage> funcCalls = ((Substitution) block)
                    .getRightValue().getAllFunctions();
            pus.addAll(funcCalls);
        } else {
            return null;
        }

        // トレースモデル
        List<TraceResultModel> listTrace = new ArrayList<TraceResultModel>();
        for (ProcedureUsage pu: pus) {
            Procedure proc = null; // call文の定義先
            if (pu.getCallDefinition() != null) {
                proc = pu.getCallDefinition();
            } else {
                // TODO 定義がない場合、それを提示すべきか
                return null;
            }
            String actualArg = this.traceWord.toLowerCase(); // プログラム単位内での実引数名

            Set<Integer> numArgs = pu.numberOfArg(actualArg);
            numArgsLoop:
                for (int numArg : numArgs) {
                    String dummyArg;
                    Variable var = proc.getArgument(numArg);
                    if (var == null) {
                        // 対応する引数が見つからないので処理終了
                        continue numArgsLoop;
                    } else {
                        dummyArg = var.getName();
                    }
                // キーワード引数をチェック
                if (pu.getArguments().get(numArg) instanceof KeywordArgument) {
                    KeywordArgument keywrd = (KeywordArgument) pu
                            .getArguments().get(numArg);
                    if (keywrd != null) {
                        dummyArg = keywrd.getKeyword();
                    }
                }

                    // 参照一覧モデルに設定する
                Set<IBlock> refdefs = proc.getRefDefBlocks(dummyArg);
                    DefaultMutableTreeNode root = new DefaultMutableTreeNode(proc);
                for (IBlock blk : refdefs) {
                    this.addBlockToRoot(root, blk);
                    }

                    // ツリーの生成
                    DefaultTreeModel tree = new DefaultTreeModel(root);
                    TraceResultModel modelTrace = new TraceResultModel();
                    modelTrace.setTraceWord(dummyArg); // トレース対象変数名
                    modelTrace.setTreeModel(tree); // 表示ツリーモデル
                    modelTrace.setTitle(dummyArg); // 表示タイトル
                    modelTrace.setSelectedBlock(proc); // 選択ブロック
                    modelTrace.setBlocklabel(pu.toDefinitionHTMLString(numArg)); // トレース先ダイアログ表示用のラベル
                    listTrace.add(modelTrace);
                }
        }

        return listTrace.toArray(new TraceResultModel[0]);
    }

    /**
     * トレース:アウトを行う。<br/>
     * 指定した手続きを呼び出しているブロックを探索する。
     *
     * @param block
     *            トレースブロック(現在のトレースのルートブロック)
     * @param traceHistory
     *            トレース履歴
     * @return トレース結果モデルリスト
     */
    public TraceResultModel[] analysisTraceOut(IBlock block,
            IBlock[] traceHistory) {

        Procedure currentProc = null;
        if (block instanceof Procedure) {
            currentProc = (Procedure) block;
        } else {
            return null;
        }

        String dummyArg = this.traceWord.toLowerCase();
        int numDummyArg = currentProc.getNumOfDummyArgument(dummyArg); // dummyArgの順番
        if (numDummyArg < 0) {
            return null;
        }
        // Procedureをcallしている手続き呼び出しブロックを取得する
        Set<ProcedureUsage> calls = currentProc.getCallMember();

        // トレースモデルの作成
        List<TraceResultModel> listTrace = new ArrayList<TraceResultModel>();
        // 各プログラム単位に対してトレースを実施する

        for (ProcedureUsage currentCall : calls) {
            Procedure proc = currentCall.getMyProcedure(); // currentCallが属するプログラム単位

            int numActualArg = currentCall.getNumOfActualArgument(dummyArg,
                    numDummyArg);
            if (numActualArg < 0) {
                continue;
            }
            // 仮引数に対応する変数名のリストに変換する
            Set<String> actualArgs = currentCall
                    .getActualArgument(numActualArg);
            for (String actualArg : actualArgs) {
                Set<IBlock> refdefs = proc.getRefDefBlocks(actualArg);

                DefaultMutableTreeNode root = new DefaultMutableTreeNode(proc);

                if (refdefs != null) {
                    for (IBlock refdef : refdefs) {
                        // 参照一覧モデルに設定する
                        this.addBlockToRoot(root, refdef);
                    }
                }

                // ツリーの生成
                DefaultTreeModel tree = new DefaultTreeModel(root);

                TraceResultModel modelTrace = new TraceResultModel();
                modelTrace.setTraceWord(actualArg); // トレース対象変数名
                modelTrace.setTreeModel(tree); // 表示ツリーモデル
                modelTrace.setTitle(actualArg); // 表示タイトル
                modelTrace.setSelectedBlock(currentCall); // 選択ブロック
                modelTrace.setBlocklabel(currentCall.toHTMLString(numActualArg,
                        actualArg)); // トレース先ダイアログ表示用のラベル
                listTrace.add(modelTrace);

            }
        }

        if (listTrace == null || listTrace.size() <= 1) {
            return listTrace.toArray(new TraceResultModel[0]);
        }
        if (traceHistory == null || traceHistory.length <= 2) {
            return listTrace.toArray(new TraceResultModel[0]);
        }

        IBlock lastblock = traceHistory[traceHistory.length - 2];
        TraceResultModel forwardModel = null;
        for (TraceResultModel model : listTrace) {
            if (model.getSelectedBlock() == lastblock) {
                forwardModel = model;
                break;
            }
        }
        if (forwardModel == null) {
            return listTrace.toArray(new TraceResultModel[0]);
        }

        return new TraceResultModel[] { forwardModel };
    }

    /**
     * トレース対象変数名を取得する.
     *
     * @return トレース対象変数名
     */
    public String getTraceWord() {
        return this.traceWord;
    }

    /**
     * トレース対象変数名を設定する.
     *
     * @param word
     *            トレース対象変数名
     */
    public void setTraceWord(String word) {
        this.traceWord = word;
    }

}
