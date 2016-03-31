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
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.FILTER_TYPE;
import jp.riken.kscope.component.FilterTreeNode;
import jp.riken.kscope.component.SearchTreeModel;
import jp.riken.kscope.component.SearchTreeNode;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SearchOption;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.Condition;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.Selection;
import jp.riken.kscope.language.Substitution;
import jp.riken.kscope.model.SearchResultModel;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;
import jp.riken.kscope.utils.TextFileReader;

/**
 * 分析:検索サービスクラス
 * @author RIKEN
 */
public class AnalysisSearchService extends AnalysisBaseService {

    /** 検索文字列 */
    private String searchText;
    /** 大文字・小文字の区別(true=大文字・小文字の区別を行う) */
    private boolean sensitivecase;
    /** 正規表現 */
    private boolean regex;
    /** 単語検索 */
    private boolean word;
    /**  変数検索(=トレース) */
    private boolean variable;
    /** 検索ツリーノード */
    private TreeNode[] searchNodes;
    /** 検索結果モデル */
    private SearchResultModel searchModel;
    /** 検索元エクスプローラツリーノード */
    DefaultMutableTreeNode exploreTreeNode;
    /** ツリー生成において循環を判定するための作業用セット */
    private HashSet<String> recursiveSub = new HashSet<String>();
    /** スレッド実行フラグ true:実行継続/false:中止. */
    private boolean m_running = true;
    /** 検索ファイルリスト */
    private SourceFile[] searchFiles;
    /** 検索済みプロシージャリスト */
    private Map<Procedure, DefaultMutableTreeNode> searchedProcedures;
    /** 検索結果ノード数 */
    private int searchedNodeCount;
    /** 最大検索結果数 */
    private final int MAX_SEARCHED_NODECOUNT = 1024;
    /** エラーメッセージ */
    private String errorMessage;

    /**
     * コンストラクタ
     */
    public AnalysisSearchService() {
        this.setErrorMessage(null);
    }


    /**
     * ツリー検索を行う
     */
    public void searchTree() {
        if (this.exploreTreeNode == null) return;

        // 検索ツリーノードを生成する
        SearchTreeNode searchRoot = new SearchTreeNode(this.exploreTreeNode);

        // 検索ツリーモデル
        SearchTreeModel treeModel = this.searchModel.getTreeModel();
        treeModel.setRoot(searchRoot);

        // 検索条件を作成
        SearchOption search = getSearchOption();

        // 検索条件を設定
        treeModel.setApplyFilter(false);        // add バグ修正 by @hira at 201511/01
        treeModel.setSearchOption(search);
        treeModel.setSearchNodes(this.searchNodes);

        // ツリー検索を実行
        treeModel.find();

        // 検索タイトル
        String title = Message.getString("analysissearchservice.searchword", this.searchText); //検索ワード
        searchModel.setTitle(title);
        // 検索条件を設定
        searchModel.setSearchText(this.searchText);

        searchModel.notifyModel();
    }


    /**
     * 検索文字列を取得する
     * @return        検索文字列
     */
    public String getSearchText() {
        return searchText;
    }

    /**
     * 検索文字列を設定する
     * @param text        検索文字列
     */
    public void setSearchText(String text) {
        this.searchText = text;
    }
    /**
     * 大文字・小文字の区別を取得する
     * @return        大文字・小文字の区別
     */
    public boolean isSensitivecase() {
        return sensitivecase;
    }

    /**
     * 大文字・小文字の区別を設定する
     * @param sensitivecase        大文字・小文字の区別
     */
    public void setSensitivecase(boolean sensitivecase) {
        this.sensitivecase = sensitivecase;
    }

    /**
     * 正規表現を取得する
     * @return        正規表現
     */
    public boolean isRegex() {
        return regex;
    }

    /**
     * 正規表現を設定する
     * @param regex        正規表現
     */
    public void setRegex(boolean regex) {
        this.regex = regex;
    }

    /**
     * 単語検索を取得する
     * @return        単語検索
     */
    public boolean isWord() {
        return word;
    }

    /**
     * 単語検索を設定する
     * @param word        単語検索
     */
    public void setWord(boolean word) {
        this.word = word;
    }

    /**
     * 変数検索(=トレース)を取得する
     * @return        変数検索(=トレース)
     */
    public boolean isVariable() {
        return variable;
    }

    /**
     * 変数検索(=トレース)を設定する
     * @param variable        変数検索(=トレース)
     */
    public void setVariable(boolean variable) {
        this.variable = variable;
    }


    /**
     * 検索ツリーノードを取得する
     * @return searchNodes        検索ツリーノード
     */
    public TreeNode[] getSearchNodes() {
        return searchNodes;
    }

    /**
     * 検索ツリーノードを設定する
     * @param searchNodes         検索ツリーノード
     */
    public void setSearchNodes(TreeNode[] searchNodes) {
        this.searchNodes = searchNodes;
    }


    /**
     * 検索結果モデルを取得する
     * @return searchModel        検索結果モデル
     */
    public SearchResultModel getSearchModel() {
        return searchModel;
    }


    /**
     * 検索結果モデルを設定する
     * @param searchModel         検索結果モデル
     */
    public void setSearchModel(SearchResultModel searchModel) {
        this.searchModel = searchModel;
    }

    /**
     * 検索元エクスプローラツリーノードを設定する
     * @param exploreTreeNode        検索元エクスプローラツリーノード
     */
    public void setExploreTreeNode(DefaultMutableTreeNode exploreTreeNode) {
        this.exploreTreeNode = exploreTreeNode;
    }

    /**
     * ソース検索を行う
     */
    public void searchFile() {
        searchFile(this.searchFiles);
    }

    /**
     * ソース検索を行う
     * @param files        検索対象ファイル
     */
    public void searchFile(SourceFile[] files) {
        if (this.exploreTreeNode == null) return;
        if (files == null) return;
        Application.status.setProgressStart(true);

        // 検索結果と一致するコード行を取得する
        ArrayList<CodeLine> list = new ArrayList<CodeLine>();
        for (SourceFile file : files) {
            // キャンセルチェック
            if (this.isCancel()) {
                break;
            }
            Application.status.setMessageStatus("searching... : " + file.toString());

            CodeLine[] codes = readSourceFile(file);
            if (codes == null || codes.length <= 0) continue;
            for (CodeLine code : codes) {
                // 最大検索数に達しているかチェックする.
                if (this.searchedNodeCount > this.MAX_SEARCHED_NODECOUNT) {
                    String msg = Message.getString("analysissearchservice.error.maxsearchedcount");
                    this.addErrorInfo(msg);
                    this.setErrorMessage(msg);
                    cancelRunning();    // 検索を中止する.
                    break;
                }

                String line = code.getStatement();
                // 検索結果と一致するか
                boolean result = false;
                if (this.variable) {
                    // 変数(=トレース)検索
                    result = StringUtils.existsSearchWord(line, this.searchText);
                }
                else {
                    // テキスト検索
                    result = StringUtils.existsSearchText(
                                            line,
                                            this.searchText,
                                            this.sensitivecase, this.regex,
                                            this.word);
                }
                if (result) {
                    list.add(code);
                    this.searchedNodeCount++;
                }
            }
        }

        // 検索ツリーノードを生成する
        SearchTreeNode searchRoot = new SearchTreeNode(this.exploreTreeNode);

        // ツリーノードを順方向で列挙
        Enumeration<?> depth = searchRoot.preorderEnumeration();
        while(depth.hasMoreElements()) {
            SearchTreeNode treeNode = (SearchTreeNode)depth.nextElement();
            if (treeNode == null || treeNode.getUserObject() == null) continue;
            if (!(treeNode.getUserObject() instanceof SourceFile)) continue;
            // 検索対象のソースファイルであるか
            addNodeSearchCodeLine(treeNode, list);
        }

        // 検索ツリーモデル
        SearchTreeModel treeModel = this.searchModel.getTreeModel();
        treeModel.setRoot(searchRoot);

        // 検索条件を作成
        SearchOption search = getSearchOption();
        // ソース検索の場合はソースコード行に検索クラスを設定する
        search.setSearchClass(CodeLine.class);

        // 検索条件を設定
        treeModel.setSearchOption(search);
        treeModel.setSearchNodes(this.searchNodes);
        // フィルタを適用しない
        treeModel.setApplyFilter(false);

        // ツリー検索を実行
        treeModel.find();

        // 検索タイトル
        String title = Message.getString("analysissearchservice.searchword", this.searchText); //検索ワード :
        searchModel.setTitle(title);
        // 検索条件を設定
        searchModel.setSearchText(this.searchText);

        Application.status.setProgressStart(false);
        Application.status.setMessageStatus(null);

        searchModel.notifyModel();
    }

    /**
     * ソースファイルノードに検索結果コード行を追加する
     * @param node        ソースファイルノード
     * @param list        検索結果コード行リスト
     */
    private void addNodeSearchCodeLine(SearchTreeNode node, ArrayList<CodeLine> list) {

        if (node == null || node.getUserObject() == null) return;
        if (!(node.getUserObject() instanceof SourceFile)) return;
        if (list == null || list.size() <= 0) return;

        // ツリーノードのソースファイル
        SourceFile file = (SourceFile) node.getUserObject();
        if (file == null) return;
        for (CodeLine line : list) {
            // 一致したソースファイルノードにコード行を追加する
            if (file.equals(line.getSourceFile())) {
                // 子ノードにコード行を追加する
                node.add(new SearchTreeNode(line));
            }
        }

        return;
    }


    /**
     * ファイルからコード行を取得する
     * @param file            ソースファイル
     * @return                コード行リスト
     */
    private CodeLine[] readSourceFile(SourceFile file) {

        ArrayList<CodeLine> list = new ArrayList<CodeLine>();
        try {
            TextFileReader reader = new TextFileReader(file.getFile());
            String line;
            int line_no = 0;
            while ((line = reader.readLine()) != null) {
                line_no++;
                // コードラインの生成、追加を行う
                String fn = null;
                if (file != null) {
                    fn = file.getPath();
                }
                list.add(new CodeLine(file, line.trim(), line_no, fn));
            }

        } catch (Exception e) {
            return null;
        }

        if (list.size() <= 0) return null;

        // 読込コード行リスト
        return list.toArray(new CodeLine[0]);
    }

    /**
     * データベースから検索文字列を検索してツリーに反映する.
     * 構造ツリーはすべて展開していないので、データベースから検索する.
     */
    public void searchLanguage() {

        if (this.exploreTreeNode == null) return;

        // フィルタリストを取得する。
        List<FILTER_TYPE> listFilter = null;
        if (exploreTreeNode instanceof FilterTreeNode) {
            listFilter = ((FilterTreeNode)exploreTreeNode).getListFilter();
        }
        // 検索ツリールートノードを生成する
        SearchTreeNode searchRoot = new SearchTreeNode(this.exploreTreeNode.getUserObject());

        // 検索ツリーモデル
        SearchTreeModel treeModel = this.searchModel.getTreeModel();
        treeModel.setRoot(searchRoot);

        // 検索条件を作成
        SearchOption search = getSearchOption();

        // 検索条件を設定
        treeModel.setSearchOption(search);
        treeModel.setSearchNodes(this.searchNodes);

        Application.status.setProgressStart(true);
        this.searchedNodeCount = 0;
        // ツリー検索を実行
        if (this.exploreTreeNode.getChildCount() > 0) {
            DefaultMutableTreeNode child = (DefaultMutableTreeNode)this.exploreTreeNode.getChildAt(0);
            Block searchblock = null;
            SearchTreeNode node = null;
            if (child.getUserObject() instanceof Procedure) {
                searchblock = ((Procedure)child.getUserObject()).getBody();
                SearchTreeNode mainNode = new SearchTreeNode(child.getUserObject());
                searchRoot.add(mainNode);
                node = mainNode;
            }
            else if (child.getUserObject() instanceof Block) {
                searchblock = (Block)child.getUserObject();
                node = searchRoot;
            }
            if (searchblock != null) {
                // 検索実行
                DefaultMutableTreeNode findnode = searchBlocks(searchblock, (DefaultMutableTreeNode)node, this.searchNodes);
                if (findnode != null) {
                    for (int i=0; i<findnode.getChildCount(); i++) {
                        node.add(new SearchTreeNode((DefaultMutableTreeNode)findnode.getChildAt(i)));
                    }
                    // フィルタを適用して再検索
                    treeModel.setApplyFilter(true);
                    treeModel.setListFilter(listFilter);
                    treeModel.find();
                }
            }
        }
        Application.status.setProgressStart(false);
        Application.status.setMessageStatus(null);

        // 検索タイトル
        String title = Message.getString("analysissearchservice.searchword", this.searchText); //検索ワード
        searchModel.setTitle(title);
        // 検索条件を設定
        searchModel.setSearchText(this.searchText);

        searchModel.notifyModel();
    }

    /**
     * 検索条件クラスを作成する.
     * @return        検索条件クラス
     */
    private SearchOption getSearchOption() {
        // 検索条件を作成
        SearchOption search = new SearchOption();
        search.setSearchText(this.searchText);
        search.setRegex(this.regex);
        search.setWord(this.word);
        search.setSensitivecase(this.sensitivecase);
        search.setVariable(this.variable);

        return search;
    }

    /**
     * 再帰的に処理ブロックを探索し、子ノードを生成する。
     * @param block 処理ブロック
     * @param parent  追加ノード
     * @param searchNodeList    検索対象ノードリスト
     * @return     検索結果ノード
     */
    private DefaultMutableTreeNode searchBlocks(IBlock block, DefaultMutableTreeNode parent, TreeNode[] searchNodeList) {
        if (block == null) return null;
        // キャンセルチェック
        if (this.isCancel()) {
            return null;
        }
        // 検索対象ブロックであるかチェックする.
        if (!isSearchBlock(parent, searchNodeList)) {
            return null;
        }
        // 最大検索数に達しているかチェックする.
        if (this.searchedNodeCount > this.MAX_SEARCHED_NODECOUNT) {
            String msg = Message.getString("analysissearchservice.error.maxsearchedcount");
            this.addErrorInfo(msg);
            this.setErrorMessage(msg);
            cancelRunning();    // 検索を中止する.
            return null;
        }

        DefaultMutableTreeNode current = new DefaultMutableTreeNode(parent);
        current.setParent((DefaultMutableTreeNode)parent.getParent());
        List<IBlock> blocks = block.getBlocks();
        for (IBlock blk: blocks) {
            // 対象が手続き呼び出しの場合
            if (blk instanceof ProcedureUsage) {
                ProcedureUsage call = (ProcedureUsage) blk;
                String callName = call.getCallName();
                if (callName == null || callName.isEmpty()) continue;

                DefaultMutableTreeNode child = new DefaultMutableTreeNode(call);
                child.setParent(parent);
                // 検索対象ブロックであるかチェックする.
                if (!isSearchBlock(child, searchNodeList)) continue;

                if (call.getCallDefinition() != null) {
                    Procedure proc = call.getCallDefinition();
                    DefaultMutableTreeNode procNode = new DefaultMutableTreeNode(proc);
                    procNode.setParent(child);

                    // 検索対象ブロックであるかチェックする.
                    if (!isSearchBlock(procNode, searchNodeList)) continue;
                    Application.status.setMessageStatus("searching... : " + proc.toString());

                    // 検査済みチェック
                    DefaultMutableTreeNode procClone = null;
                    if (containsSearchedProcedures(proc)) {
                        procClone = SwingUtils.cloneTreeNode(getSearchedProcedureNode(proc));
                        if (procClone == null) {
                            continue;
                        }
                    }

                    // 循環のチェック
                    if (!SwingUtils.recursiveTreeNode(procNode)
                        && !this.recursiveSub.contains(callName)) {
                        if (procClone != null) {
                            child.add(procClone);
                            child.setParent(null);
                            current.add(child);
                            int nodecount = SwingUtils.getAllChildCount(current.getRoot());
                            this.searchedNodeCount += nodecount;
                            continue;
                        }

                        recursiveSub.add(callName);
                        DefaultMutableTreeNode find = searchBlocks(proc.getBody(), procNode, searchNodeList);
                        if (find != null && find.getChildCount() > 0) {
                            while (find.getChildCount() > 0) {
                                procNode.add((DefaultMutableTreeNode)find.getChildAt(0));
                                this.searchedNodeCount++;
                            }
                        }
                        recursiveSub.remove(callName);
                    }

                    if (isMatchText(procNode.toString()) || procNode.getChildCount() > 0) {
                        procNode.setParent(null);
                        child.add(procNode);
                        this.searchedNodeCount++;
                    }
                    else {
                        procNode = null;
                    }
                    // 検査済みプロシージャの追加
                    addSearchedProcedures(proc, procNode);
                }
                if (isMatchText(child.toString()) || child.getChildCount() > 0) {
                    child.setParent(null);
                    current.add(child);
                    this.searchedNodeCount++;
                }
            } else if (blk instanceof Selection) {
                Selection select = (Selection) blk;
                DefaultMutableTreeNode child = new DefaultMutableTreeNode(select);
                child.setParent(parent);
                // 検索対象ブロックであるかチェックする.
                if (!isSearchBlock(child, searchNodeList)) continue;
                // SELECT文の場合
                if (select.isSelect()) {
                    for (Condition cond : select.getConditions()) {
                        DefaultMutableTreeNode child2 = new DefaultMutableTreeNode(cond);
                        child2.setParent(child);
                        // 検索対象ブロックであるかチェックする.
                        if (!isSearchBlock(child2, searchNodeList)) continue;
                        DefaultMutableTreeNode find = searchBlocks(cond, child2, searchNodeList);
                        if (find != null && find.getChildCount() > 0) {
                            while (find.getChildCount() > 0) {
                                child2.add((DefaultMutableTreeNode)find.getChildAt(0));
                                this.searchedNodeCount++;
                            }
                        }
                        if (isMatchText(cond.toString()) || child2.getChildCount() > 0) {
                            child2.setParent(null);
                            child.add(child2);
                            this.searchedNodeCount++;
                        }
                    }
                    if (isMatchText(child.toString()) || child.getChildCount() > 0) {
                        child.setParent(null);
                        current.add(child);
                        this.searchedNodeCount++;
                    }
                // IF,WHERE文の場合
                } else {
                    Block cond0 = select.getConditions().get(0);
                    DefaultMutableTreeNode find = searchBlocks(cond0, child, searchNodeList);
                    if (find != null && find.getChildCount() > 0) {
                        while (find.getChildCount() > 0) {
                            child.add((DefaultMutableTreeNode)find.getChildAt(0));
                            this.searchedNodeCount++;
                        }
                    }
                    if (isMatchText(child.toString()) || child.getChildCount() > 0) {
                        child.setParent(null);
                        current.add(child);
                        this.searchedNodeCount++;
                    }
                    for (int j = 1; j < select.getConditions().size(); j++) {
                        Condition cond = select.getConditions().get(j);
                        DefaultMutableTreeNode condnode = new DefaultMutableTreeNode(cond);
                        condnode.setParent(child);
                        // 検索対象ブロックであるかチェックする.
                        if (!isSearchBlock(condnode, searchNodeList)) continue;
                        find = searchBlocks(cond, condnode, searchNodeList);
                        if (find != null && find.getChildCount() > 0) {
                            while (find.getChildCount() > 0) {
                                condnode.add((DefaultMutableTreeNode)find.getChildAt(0));
                                this.searchedNodeCount++;
                            }
                        }
                        if (isMatchText(condnode.toString()) || condnode.getChildCount() > 0) {
                            condnode.setParent(null);
                            current.add(condnode);
                            this.searchedNodeCount++;
                        }
                    }
                }
            } else if (blk instanceof Substitution) {
                DefaultMutableTreeNode child = new DefaultMutableTreeNode(blk);
                child.setParent(parent);
                // 検索対象ブロックであるかチェックする.
                if (!isSearchBlock(child, searchNodeList)) continue;
                DefaultMutableTreeNode find = searchBlocks(blk, current, searchNodeList);
                if (isMatchText(child.toString()) || child.getChildCount() > 0) {
                    child.setParent(null);
                    current.add(child);
                    this.searchedNodeCount++;
                }
                if (find != null && find.getChildCount() > 0) {
                    while (find.getChildCount() > 0) {
                        current.add((DefaultMutableTreeNode)find.getChildAt(0));
                        this.searchedNodeCount++;
                    }
                }
            } else {
                DefaultMutableTreeNode child = new DefaultMutableTreeNode(blk);
                child.setParent(parent);
                // 検索対象ブロックであるかチェックする.
                if (!isSearchBlock(child, searchNodeList)) continue;
                DefaultMutableTreeNode find = searchBlocks(blk, child, searchNodeList);
                if (find != null && find.getChildCount() > 0) {
                    while (find.getChildCount() > 0) {
                        child.add((DefaultMutableTreeNode)find.getChildAt(0));
                        this.searchedNodeCount++;
                    }
                }
                if (isMatchText(child.toString()) || child.getChildCount() > 0) {
                    child.setParent(null);
                    current.add(child);
                    this.searchedNodeCount++;
                }
            }
        }
        if (current.getChildCount() <= 0) {
            return null;
        }
        current.setParent(null);

        return current;
    }

    /**
     * 検索条件に従って、文字列検索を行う.
     * @param nodeText        検索対象文字列
     * @return            true=一致
     */
    private boolean isMatchText(String nodeText) {
        boolean result = false;
        SearchOption searchOption = getSearchOption();
        if (searchOption.isVariable()) {
            // 変数(=トレース)検索
            result = StringUtils.existsSearchWord(nodeText, searchOption.getSearchText());
        }
        else {
            // テキスト検索
            result = StringUtils.existsSearchText(
                                    nodeText,
                                    searchOption.getSearchText(),
                                    searchOption.isSensitivecase(),
                                    searchOption.isRegex(),
                                    searchOption.isWord());
        }
        return result;
    }

    /**
     * スレッドの実行がキャンセルであるかチェックする
     * @return    true=キャンセル
     */
    public boolean isCancel() {
        return !this.m_running;
    }

    /**
     * スレッドの実行をキャンセルする。
     */
    public void cancelRunning() {
        m_running = false;
    }

    /**
     * 検索対象ブロックであるかチェックする.
     * 検索ノードリストがnullの場合は、全検索対象
     * @param parent            検索ブロックノード
     * @param searchNodeList        検索ノードリスト
     * @return    true=検索対象ブロック
     */
    private boolean isSearchBlock(DefaultMutableTreeNode parent, TreeNode[] searchNodeList) {
        if (parent == null) return false;
        if (searchNodeList == null || searchNodeList.length <= 0) return true;
        Object[] parentBlocks = parent.getUserObjectPath();

        SEARCH_LOOP: for (TreeNode searchnode : searchNodeList) {
            TreeNode node = searchnode;
            if (!(node instanceof DefaultMutableTreeNode)) continue;
            Object[] searchBlocks = ((DefaultMutableTreeNode)node).getUserObjectPath();

            // 検索ノードリストの親ブロックであるかチェックする
            if (parentBlocks.length <= searchBlocks.length) {
                for (int i=0; i<parentBlocks.length; i++) {
                    Object parentObj = parentBlocks[i];
                    if (parentObj instanceof DefaultMutableTreeNode) {
                        parentObj = ((DefaultMutableTreeNode)parentObj).getUserObject();
                    }
                    Object searchObj = searchBlocks[i];
                    if (searchObj instanceof DefaultMutableTreeNode) {
                        searchObj = ((DefaultMutableTreeNode)searchObj).getUserObject();
                    }
                    if (parentObj != searchObj) {
                        continue SEARCH_LOOP;
                    }
                }
                return true;
            }
            else {
                for (int i=0; i<searchBlocks.length; i++) {
                    Object parentObj = parentBlocks[i];
                    if (parentObj instanceof DefaultMutableTreeNode) {
                        parentObj = ((DefaultMutableTreeNode)parentObj).getUserObject();
                    }
                    Object searchObj = searchBlocks[i];
                    if (searchObj instanceof DefaultMutableTreeNode) {
                        searchObj = ((DefaultMutableTreeNode)searchObj).getUserObject();
                    }
                    if (parentObj != searchObj) {
                        continue SEARCH_LOOP;
                    }
                }
                return true;
            }
        }

        return false;
    }

    /**
     * 検索ファイルリストを取得する.
     * @return        検索ファイルリスト
     */
    public SourceFile[] getSearchFiles() {
        return searchFiles;
    }

    /**
     * 検索ファイルリストを設定する.
     * @param files    検索ファイルリスト
     */
    public void setSearchFiles(SourceFile[] files) {
        this.searchFiles = files;
    }

    /**
     * 検索済みプロシージャリストに追加済みかチェックする.
     * @param proc        プロシージャ
     * @return            true=追加済み
     */
    private boolean containsSearchedProcedures(Procedure proc) {
        if (this.searchedProcedures == null) return false;
        return this.searchedProcedures.containsKey(proc);
    }

    /**
     * 検索済みプロシージャリストからツリーノードを取得する.
     * @param proc        検索済みプロシージャ
     * @return            ツリーノード
     */
    private DefaultMutableTreeNode getSearchedProcedureNode(Procedure proc) {
        if (this.searchedProcedures == null) return null;
        return this.searchedProcedures.get(proc);
    }

    /**
     * 検索済みプロシージャリストに追加する.
     * @param proc        プロシージャ
     * @param node        ツリーノード
     */
    private void addSearchedProcedures(Procedure proc, DefaultMutableTreeNode node) {
        if (this.searchedProcedures == null) {
            this.searchedProcedures = new HashMap<Procedure, DefaultMutableTreeNode>();
        }
        this.searchedProcedures.put(proc, node);
    }


    /**
     * エラーメッセージを取得する.
     * @return エラーメッセージ
     */
    public String getErrorMessage() {
        return errorMessage;
    }


    /**
     * エラーメッセージを設定する
     * @param message    エラーメッセージ
     */
    public void setErrorMessage(String message) {
        this.errorMessage = message;
    }
}
