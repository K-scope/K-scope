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

import java.beans.PropertyChangeListener;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.component.FilterTreeNode;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.exception.LanguageException;
import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.information.InformationBase;
import jp.riken.kscope.information.InformationBlock;
import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.information.TextInfo;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.Break;
import jp.riken.kscope.language.Common;
import jp.riken.kscope.language.CompoundBlock;
import jp.riken.kscope.language.Condition;
import jp.riken.kscope.language.Continue;
import jp.riken.kscope.language.Data;
import jp.riken.kscope.language.DynamicAllocation;
import jp.riken.kscope.language.DynamicDeallocation;
import jp.riken.kscope.language.DynamicNullification;
import jp.riken.kscope.language.Equivalence;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.GoTo;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IVariableType;
import jp.riken.kscope.language.Module;
import jp.riken.kscope.language.Pause;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.Repetition;
import jp.riken.kscope.language.Return;
import jp.riken.kscope.language.Selection;
import jp.riken.kscope.language.Statement;
import jp.riken.kscope.language.Substitution;
import jp.riken.kscope.language.Termination;
import jp.riken.kscope.language.UseState;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.VariableDimension;
import jp.riken.kscope.language.fortran.Type;
import jp.riken.kscope.language.fortran.VariableAttribute.ScopeAttribute;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.language.fortran.VariableType.PrimitiveDataType;
import jp.riken.kscope.language.generic.Argument;
import jp.riken.kscope.language.generic.IProcedureItem;
import jp.riken.kscope.language.generic.ProcedureItem;
import jp.riken.kscope.language.generic.ProcedureWithNameOnly;
import jp.riken.kscope.language.generic.Procedures;
import jp.riken.kscope.language.utils.LanguageVisitor;
import jp.riken.kscope.language.utils.ValidateLanguage;
import jp.riken.kscope.model.FileTreeModel;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.ModuleTreeModel;
import jp.riken.kscope.model.PropertiesTableModel;
import jp.riken.kscope.parser.IAnalyseParser;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.utils.Logger;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;
import jp.riken.kscope.xcodeml.FactoryXcodeMLParser;


/**
 * データベースの構築、探索を行うクラス。
 * @author RIKEN
 * @version    2015/03/15      C言語対応による変更
 */
public class LanguageService extends BaseService {

    /** 読込ソースファイル. */
    private SourceFile[] files = null;
    /** フォートラン構文解析結果格納データベース. */
    private Fortran fortranDb = null;
    /** フォートラン-XcodeML構文解析パーサー. */
    private IAnalyseParser fortranParser = null;
    /** C言語-XcodeML構文解析パーサー. */
    private IAnalyseParser clangParser = null;
    /** 構造ツリーモデル. */
    private LanguageTreeModel modelLanguage = null;
    /** モジュールツリーモデル. */
    private ModuleTreeModel modelModule = null;
    /** ソースファイルツリーモデル. */
    private FileTreeModel modelFile = null;
    /** XMLファイルツリーモデル. */
    private FileTreeModel modelXml = null;
    /** プロジェクトフォルダ */
    private File projectFolder = null;

    /** ツリーにおいて既に追加されたプログラム単位を格納する作業用セット */
    private HashSet<ProgramUnit> checkProgramUnitFlag = new HashSet<ProgramUnit>();

    /** ツリー生成において循環を判定するための作業用セット */
    private HashSet<String> recursiveSub = new HashSet<String>();

    /** 性能領域定義されたブロックを判定・追加するための作業用メンバー. */
    private String regionStart;
    private int regionStartNum;
    private String regionEnd;
    private int regionEndNum;
    private InformationBase regionInfo;
    private Map<String, List<InformationBlock>> regionMap = new HashMap<String, List<InformationBlock>>();
    private InformationBlocks regionInfos;

    /** スレッド実行フラグ true:実行継続/false:中止. */
    private volatile boolean m_running = true;
    /** 構造ツリー展開深さ:デフォルト=2,  */
    private int treeDepth = 2;
    /** データベースシリアライズストリーム */
    private volatile ObjectInputStream languageStream;
    /** デバッグ判断フラグ */
    private final boolean _DEBUG = true;

    /**
     * コンストラクタ.
     */
    public LanguageService() {
        this.files = null;
        this.fortranDb = null;
        // XcodeMLパーサーを作成する.
        this.initializeXcodemlPerser();
    }

    /**
     * コンストラクタ.
     * @param fortran            フォートラン構文解析結果格納データベース
     */
    public LanguageService(Fortran fortran) {
        this.files = null;
        this.fortranDb = fortran;
        // XcodeMLパーサーを作成する.
        this.initializeXcodemlPerser();
    }

    /**
     * コンストラクタ.
     *
     * @param files            読込ソースファイル
     * @param fortran            フォートラン構文解析結果格納データベース
     */
    public LanguageService(SourceFile[] files, Fortran fortran) {
        this.files = files;
        this.fortranDb = fortran;
        this.fortranParser = null;
        this.clangParser = null;
        // XcodeMLパーサーを作成する.
        this.initializeXcodemlPerser();
    }

    /**
     * 構造ツリーモデルを設定する.
     * @param model		構造ツリーモデル
     */
    public void setLanguageTreeModel(LanguageTreeModel model) {
        this.modelLanguage = model;
    }

    /**
     * モジュールツリーモデルを設定する.
     * @param model		モジュールツリーモデル
     */
    public void setModuleTreeModel(ModuleTreeModel model) {
        this.modelModule = model;
    }

    /**
     * ソースファイルツリーモデルを設定する.
     * @param model		ソースファイルツリーモデル
     */
    public void setSourceTreeModel(FileTreeModel model) {
        this.modelFile = model;
    }

    /**
     * XMLファイルツリーモデルを設定する.
     * @param model		XMLファイルツリーモデル
     */
    public void setXmlTreeModel(FileTreeModel model) {
        this.modelXml = model;
    }

    /**
     * スレッド実行 ソースファイルから読込を行い、構文解析を行う。
     */
    public void parseSourceFile() {
        if (this.files == null) return;
        if (this.fortranDb == null) return;

        // キャンセルチェック
        if (this.isCancel()) {
            return;
        }
        // 解析ファイルから重複ファイルを削除する
        List<SourceFile> filelist = validFileList(this.files);
        if (filelist == null) return;

        try {
            ArrayList<SourceFile> sourceFileList = new ArrayList<SourceFile>();
            for (SourceFile file : filelist) {
                try {
                    // フォートラン/C言語-XcodeML構文解析パーサーの選択
                    IAnalyseParser xcodemlParser = null;
                    FILE_TYPE lang_type = FactoryXcodeMLParser.parserLanguage(file);
                    if (lang_type == FILE_TYPE.FORTRANLANG) {
                        xcodemlParser = this.fortranParser;
                    }
                    else if (lang_type == FILE_TYPE.CLANG) {
                        xcodemlParser = this.clangParser;
                    }
                    else {
                        // XcodeMLパーサの生成に失敗しました(languge属性の取得失敗)。
                        throw new LanguageException(
                                new Exception(Message.getString("xcodemlparserstax.error.null")),
                                file);
                    }

                    String filename = file.toString();
                    Pattern pattern = Pattern.compile("^[^\\.].*$");
                    Matcher matcher = pattern.matcher(filename);
                    boolean b = matcher.matches();
                    if (b) {
                        Application.status.setMessageStatus(filename);

                        // ソースファイルからファイルを読み込む
                        xcodemlParser.readFile(file);

                        // 読込コード行を構文解析する。
                        xcodemlParser.parseFile(fortranDb);

                        // オリジナルフォートランソースファイルの取得
                        sourceFileList.add(xcodemlParser.getLanguageFile());

                        // パースエラーの取得
                        if (xcodemlParser.getErrorInfos() != null) {
                            this.addErrorInfos(xcodemlParser.getErrorInfos());
                        }
                    }
                } catch (LanguageException lang_ex) {
                    Logger.error(lang_ex);
                    Logger.error(lang_ex.getCodeInfo());

                    // エラー箇所の情報をセットする
                    this.addErrorInfo(lang_ex);

                    // エラーメッセージ
                    //String error_message = lang_ex.getMessage();
                    //lang_ex.printStackTrace();
                    //JOptionPane.showMessageDialog(null, error_message, "Analyse Error", JOptionPane.ERROR_MESSAGE);
                }

                // キャンセルチェック
                if (this.isCancel()) {
                    return;
                }
            }

            Application.status.setProgressStart(true);
            Application.status.setMessageStatus("set source file...");

            // オリジナルフォートランソースファイルをソースツリーに設定する
            // delete at 2013/03/01 by @hira
//            ArrayList<SourceFile> sourceFileList = new ArrayList<SourceFile>();
//            if (modelFile != null && listLangFile.size() > 0) {
//                for (int i=0; i<listLangFile.size(); i++) {
//                    sourceFileList.add(new SourceFile(listLangFile.get(i)));
//                }
//            }

            Application.status.setMessageStatus("analysys database...");
            fortranDb.analyseDB();

            // キャンセルチェック
            if (this.isCancel()) {
                return;
            }

            // データベースの検証を行う.
            {
                ValidateLanguage validate = new ValidateLanguage(fortranDb);
                LanguageVisitor visitor = new LanguageVisitor(validate);
                visitor.entry();
                int error = validate.analyseTypes();
                if (error > 0) {
                    this.getErrorInfoModel().addErrorInfos(validate.getErrorList());
                    String msg = Message.getString("validatelanguage.final.error", error);
                    this.addErrorInfo(msg);
                }
            }

            // ソースファイルリストの設定
            fortranDb.setSourceFileList(sourceFileList);

            // エクスプローラビューをセットする
            setExplorerView();

            Application.status.setProgressStart(false);
            Application.status.setMessageStatus(Message.getString("languageservice.parsesourcefile.finalize.status")); //構造解析:終了

        } catch (LanguageException lang_ex) {
            Logger.error(lang_ex);
            Logger.error(lang_ex.getCodeInfo());
            lang_ex.printStackTrace();

            String error_message = lang_ex.getMessage();

            // エラー箇所の情報をセットする
            this.addErrorInfo(lang_ex);

            // エラーメッセージ
            // JOptionPane.showMessageDialog(null, error_message, "Analyse Error", JOptionPane.ERROR_MESSAGE);
        } catch (InterruptedException ex) {
            // キャンセルによる終了
            this.addErrorInfo(Message.getString("languageservice.parsesourcefile.cancel")); //キャンセルによる終了
            Application.status.setMessageStatus(Message.getString("languageservice.parsesourcefile.cancel")); //キャンセルによる終了

        } catch (Exception ex) {
            Logger.error(ex);
            ex.printStackTrace();

            String error_message = ex.getMessage();
            if (error_message == null) {
                error_message = ex.toString();
            }
            // エラーメッセージ
            // JOptionPane.showMessageDialog(null, error_message, "Analyse Error", JOptionPane.ERROR_MESSAGE);

            // エラー箇所の情報をセットする
            this.addErrorInfo(error_message);
        }
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
        if (this.fortranParser != null) {
            this.fortranParser.setCancel(true);
        }
        if (this.clangParser != null) {
            this.clangParser.setCancel(true);
        }
        if (this.fortranDb != null) {
            this.fortranDb.setCancel(true);
        }
        try {
            if (this.languageStream != null) {
                this.languageStream.close();
                this.languageStream = null;
            }
        } catch (IOException ex) {
        }
    }

    /**
     * パーサにステータス表示用リスナを登録する。
     *
     * @param listener      ステータス表示用リスナ
     */
    public void addStatusPropertyChangeListener(PropertyChangeListener listener) {
        if (this.fortranParser != null) {
            this.fortranParser.addPropertyChangeListener(listener);
        }
        if (this.clangParser != null) {
            this.clangParser.addPropertyChangeListener(listener);
        }
    }

    /**
     * 構造ツリーを作成する
     */
    public void writeTree() {
        // 性能領域の読み込み
        try {
            FileReader in = new FileReader("properties/performance.ksx");
            BufferedReader br = new BufferedReader(in);
            String line;
            int cnt = 0;
            while ((line = br.readLine()) != null) {
                if (cnt == 0) {
                    this.regionStart = line;
                } else if (cnt == 1) {
                    this.regionStartNum = Integer.valueOf(line);
                } else if (cnt == 2) {
                    this.regionEnd = line;
                } else if (cnt == 3) {
                    this.regionEndNum = Integer.valueOf(line);
                } else if (cnt == 4) {
                    this.regionInfo = new TextInfo(line);
                }
                cnt++;
            }
            br.close();
            in.close();
            this.regionInfos = new InformationBlocks();
        } catch (FileNotFoundException e) {
            //e.printStackTrace();
        } catch (IOException e) {
            //e.printStackTrace();
        }

        int depth = 0;
        // モデルにデータベースをセットする
        this.modelLanguage.setLanguageDb(this.fortranDb);
        DefaultMutableTreeNode root = this.modelLanguage.getRootNode();
        writeTree("main", root, true, depth);
        if (this.regionInfos != null) {
            this.fortranDb.setInformationBlocks(this.regionInfos);
        }
        return;
    }

    /**
     * 構造ツリーを作成する
     * @param   block      ルートブロック
     * @return		true=成功
     */
    public boolean writeTree(IBlock block) {
        if (!(block instanceof Procedure)) {
            return false;
        }
        // モデルにデータベースをセットする
        this.modelLanguage.setLanguageDb(this.fortranDb);
        DefaultMutableTreeNode root = this.modelLanguage.getRootNode();
        checkProgramUnitFlag.clear();
        if (block instanceof Procedure) {
            checkProgramUnitFlag.add((Procedure) block);

            int depth = 0;
            FilterTreeNode node = new FilterTreeNode(block, depth);
            root.add(node);
            writeBlocks(((Procedure) block).getBody(), node, true, depth+1);
        }
        return true;
    }


    /**
     * 指定したサブルーチン以下のルーチンツリーをJTreeにて構築する。
     *
     * @param procName
     *            サブルーチン名
     * @param root
     *            ルートノード
     * @param flag
     *            子ノードを探索する場合はtrue
     * @param depth  子孫ノード数
     * @return  成否
     */
    private boolean writeTree(String procName, DefaultMutableTreeNode root, boolean flag, int depth) {
        checkProgramUnitFlag.clear();
        String name;
        if (procName.equalsIgnoreCase("main")) {
            name = fortranDb.getMainName();
        } else {
            name = procName;
        }
        Procedure proc = fortranDb.search_subroutine(name);
        checkProgramUnitFlag.add(proc);
        if (proc == null) {
            // エラーメッセージ:languageservice.procedure.error=[%s]は存在しません。
            String error_message = Message.getString("languageservice.procedure.error", procName);

            // エラー箇所の情報をセットする
            this.addErrorInfo(error_message);
            return false;
        }
        FilterTreeNode node = new FilterTreeNode(proc, depth);
        root.add(node);
        writeBlocks(proc.getBody(), node, flag, depth+1);
        return true;
    }

    /**
     * 再帰的に処理ブロックを探索し、子ノードを生成する。
     * @param block 処理ブロック
     * @param parent 親ノード
     * @param flag 手続き呼び出しの宣言を展開するフラグ。展開する場合はtrue
     * @param depth  子孫ノード数
     */
    public void writeBlocks(Block block, FilterTreeNode parent, boolean flag, int depth) {
        FilterTreeNode child;
        // 展開階層は2とする. 展開階層が0以下の場合はすべて子孫ノードを読み込む
        if (this.treeDepth > 0 && depth > this.treeDepth) return;

        List<Block> blocks = block.getBlocks();
        for (Block blk: blocks) {
            // 対象が手続き呼び出しの場合
            if (blk instanceof ProcedureUsage) {
                ProcedureUsage call = (ProcedureUsage) blk;
                String callName = call.getCallName();
                if (callName.equalsIgnoreCase("")) {
                    continue;
                }
                child = new FilterTreeNode(call, depth);
                child = parent.add(child);
                // 性能領域に対応するコール文かチェック
                this.chechPerformance(call);
                if (flag) {
                    if (call.getCallDefinition() != null) {
                        Procedure proc = call.getCallDefinition();
                        FilterTreeNode procNode = new FilterTreeNode(proc, depth+1);
                        procNode = child.add(procNode);
                        boolean flagToWriteOnce = false; //手続の宣言を最初だけ生成する場合はtrue
                        if (flagToWriteOnce) {
                            if (this.checkProgramUnitFlag.contains(proc)) {
                                continue;
                            }
                            this.checkProgramUnitFlag.add(proc);
                        }
                        // 循環のチェック
                        if (SwingUtils.recursiveTreeNode(procNode)) {
                            continue;
                        }
                        else if (this.recursiveSub.contains(callName)) {
                            continue;
                        } else {
                            Application.status.setMessageStatus("writeBlock " + callName + "...");
                            recursiveSub.add(callName);
                            writeBlocks(proc.getBody(), procNode, flag, depth+2);
                            recursiveSub.remove(callName);
                        }
                    }
                }
            } else if (blk instanceof Selection) {
                Selection selec = (Selection) blk;
                child = new FilterTreeNode(selec, depth);
                child = parent.add(child);
                // SELECT文の場合
                if (selec.isSelect()) {
                    for (Condition cond : selec.getConditions()) {
                        FilterTreeNode child2 = new FilterTreeNode(cond, depth+1);
                        child2 = child.add(child2);
                        writeBlocks(cond, child2, flag, depth+2);
                    }
                    // IF,WHERE文の場合
                } else {
                    if (selec.getConditions() != null && selec.getConditions().size() > 0) {
                        Block cond0 = selec.getConditions().get(0);
                        writeBlocks(cond0, child, flag, depth+1);
                        for (int j = 1; j < selec.getConditions().size(); j++) {
                            Condition cond = selec.getConditions().get(j);
                            child = new FilterTreeNode(cond, depth);
                            child = parent.add(child);
                            writeBlocks(cond, child, flag, depth+1);
                        }
                    }
                }
            } else if (blk instanceof Substitution) {
                child = new FilterTreeNode(blk, depth);
                child = parent.add(child);
                int childdepth = depth+1;
                // 代入文の関数呼び出しが存在する場合は、必ず挿入する様にする.
                if (childdepth > this.treeDepth) {
                    List<Block> childblocks = block.getBlocks();
                    if (childblocks != null && childblocks.size() > 0) {
                        childdepth = this.treeDepth;
                    }
                }
                writeBlocks(blk, parent, flag, childdepth); // 代入文の関数呼び出しはparentの子要素とする

            } else if (blk instanceof CompoundBlock) {
                // for debug
                if (this._DEBUG) {
                    child = new FilterTreeNode(blk, depth);
                    child = parent.add(child);
                    writeBlocks(blk, child, flag, depth+1);
                    continue;
                }

                // 複文の場合、ツリー上に表現しない
                writeBlocks(blk, parent, flag, depth);

            } else {
                child = new FilterTreeNode(blk, depth);
                child = parent.add(child);
                writeBlocks(blk, child, flag, depth+1);
            }
        }
    }

    /**
     * 再帰的に処理ブロックを探索し、子ノードを生成する。
     * @param proc 処理ブロック
     * @param parent 親ノード
     * @param flag 手続き呼び出しの宣言を展開するフラグ。展開する場合はtrue
     * @param depth  子孫ノード数
     */
    public void writeProcedure(Procedure proc, FilterTreeNode parent, boolean flag, int depth) {
        if (!flag) return;

        String callName = proc.get_name();
        boolean flagToWriteOnce = false; //手続の宣言を最初だけ生成する場合はtrue
        if (flagToWriteOnce) {
            if (this.checkProgramUnitFlag.contains(proc)) {
                return;
            }
            this.checkProgramUnitFlag.add(proc);
        }
        // 循環のチェック
        if (SwingUtils.recursiveTreeNode(parent)) {
            return;
        }
        if (this.recursiveSub.contains(callName)) {
            return;
        } else {
            recursiveSub.add(callName);
            writeBlocks(proc.getBody(), parent, flag, depth+1);
            recursiveSub.remove(callName);
        }
    }

    /**
     * 再帰的に処理ブロックを探索し、子ノードを生成する。
     * @param call 処理ブロック
     * @param parent 親ノード
     * @param flag 手続き呼び出しの宣言を展開するフラグ。展開する場合はtrue
     * @param depth  子孫ノード数
     */
    public void writeProcedureUsage(ProcedureUsage call, FilterTreeNode parent, boolean flag, int depth) {
        String callName = call.getCallName();
        if (callName.equalsIgnoreCase("")) {
            return;
        }
        // 性能領域に対応するコール文かチェック
        this.chechPerformance(call);
        if (flag) {
            if (call.getCallDefinition() != null) {
                Procedure proc = call.getCallDefinition();
                FilterTreeNode procNode = new FilterTreeNode(proc, depth+1);
                procNode = parent.add(procNode);
                writeProcedure(proc, procNode, flag, depth);
            }
        }
    }


    /**
     * 再帰的に処理ブロックを探索し、子ノードを生成する。
     * @param select 処理ブロック
     * @param parent 親ノード
     * @param flag 手続き呼び出しの宣言を展開するフラグ。展開する場合はtrue
     * @param depth  子孫ノード数
     */
    public void writeSelection(Selection select, FilterTreeNode parent, boolean flag, int depth) {
        // SELECT文の場合
        if (select.isSelect()) {
            for (Condition cond : select.getConditions()) {
                FilterTreeNode child2 = new FilterTreeNode(cond, depth+1);
                child2 = parent.add(child2);
                writeBlocks(cond, child2, flag, depth+2);
            }
        // IF,WHERE文の場合
        } else {
            Block cond0 = select.getConditions().get(0);
            writeBlocks(cond0, parent, flag, depth+1);
        }
    }

    /**
     * 指定された手続き呼び出しが、性能領域を定義するものかチェックし、
     * 該当すれば追加する。
     * @param call 手続き呼び出し
     */
    private void chechPerformance(ProcedureUsage call) {
        if (call.getCallName().equalsIgnoreCase(this.regionStart)) {
            String key = call.getArguments().get(this.regionStartNum - 1).toString();
            if (key != null) {
                InformationBlock blk = new InformationBlock(this.regionInfo, call, null);
                List<InformationBlock> blks = this.regionMap.get(key);
                if (blks == null) {
                    blks = new ArrayList<InformationBlock>();
                    this.regionMap.put(key, blks);
                }
                blks.add(blk);
            }
        } else if (call.getCallName().equalsIgnoreCase(this.regionEnd)) {
            String key = call.getArguments().get(this.regionEndNum - 1).toString();
            if (key != null) {
                List<InformationBlock> blks = this.regionMap.get(key);
                if (blks != null) {
                    InformationBlock current = blks.get(blks.size() - 1);
                    current.setEndBlock(call);
                    this.regionInfos.add(current);
                    blks.remove(blks.size() - 1);
                }
            }
        }
    }

    /**
     * モジュールツリーの作成を行う.
     */
    public void setFortranModules() {

        // モデルにデータベースをセットする
        this.modelModule.setLanguageDb(this.fortranDb);
        DefaultMutableTreeNode root = this.modelModule.getRootNode();

        String[] moduleName = this.fortranDb.get_module_name();
        Arrays.sort(moduleName, new Comparator<String>() {
            @Override
            public int compare(String obj0, String obj1) {
                return obj0.compareTo(obj1);
            }
        });
        Module currentModule = this.fortranDb.module("NO_MODULE");
        DefaultMutableTreeNode child = new DefaultMutableTreeNode(currentModule);
        root.add(child);
        Procedure[] subs = currentModule.get_procedures();
        // mainを最初にノードにaddする。
        List<Procedure> subsList = new ArrayList<Procedure>(Arrays.asList(subs));
        for (Procedure proc: subsList) {
            if (proc.isProgram()) {    // program文であるか
                subsList.remove(proc);
                subsList.add(0, proc);
                break;
            }
        }
        subs = subsList.toArray(new Procedure[0]);

        setSubroutines(child, subs);

        for (int i = 0; i < moduleName.length; i++) {
            if (!(moduleName[i].equalsIgnoreCase("NO_MODULE"))) {
                currentModule = this.fortranDb.module(moduleName[i]);
                child = new DefaultMutableTreeNode(currentModule);
                root.add(child);
                // use文の表示
                List<UseState> uses = currentModule.getUseList();
                DefaultMutableTreeNode usesNode = new DefaultMutableTreeNode("use");
                if (uses.size() > 0) {
                    child.add(usesNode);
                    for (UseState use: uses) {
                        DefaultMutableTreeNode useNode = new DefaultMutableTreeNode(use);
                        usesNode.add(useNode);
                    }
                }
                // interface文の表示
                List<Procedures> interfaces = currentModule.getInterfaceList();
                for (Procedures in: interfaces) {
                    DefaultMutableTreeNode varChild = new DefaultMutableTreeNode(in);
                    child.add(varChild);
                    for (IProcedureItem item: in.getProcedures()) {
                        DefaultMutableTreeNode itemNode = new DefaultMutableTreeNode(item);
                        varChild.add(itemNode);
                    }
                }
                // type文の表示
                List<Type> types = currentModule.getTypeList();
                for (Type tp : types) {
                    DefaultMutableTreeNode varChild = new DefaultMutableTreeNode(
                            tp);
                    child.add(varChild);
                    for (VariableDefinition item : tp.getDefinitions()) {
                        DefaultMutableTreeNode itemNode = new DefaultMutableTreeNode(
                                item);
                        varChild.add(itemNode);
                    }
                }
                // common文の表示
                DefaultMutableTreeNode comRoot = new DefaultMutableTreeNode("common");
                List<Common> comList = currentModule.getCommonList();
                if (comList.size() > 0) {
                    child.add(comRoot);
                for (Common cm: comList) {
                    DefaultMutableTreeNode comNode = new DefaultMutableTreeNode(cm);
                    comRoot.add(comNode);
                }
                }
                //TODO equivalence文の表示
                //TODO data文の表示
                VariableDefinition[] vars = currentModule.get_variables();
                for (int j = 0; j < vars.length; j++) {
                    DefaultMutableTreeNode varChild = new DefaultMutableTreeNode(
                            vars[j]);
                    child.add(varChild);
                }
                subs = currentModule.get_procedures();
                setSubroutines(child, subs);
            }
        }
    }

    /**
     * プロシージャノードを追加する
     * @param child			追加ノード
     * @param subs			プロシージャ要素
     */
    private void setSubroutines(DefaultMutableTreeNode child, Procedure[] subs) {
        if (subs == null) {
            return;
        }
        for (int i = 0; i < subs.length; i++) {
            DefaultMutableTreeNode child2 = new DefaultMutableTreeNode(subs[i]);
            child.add(child2);
            // use文の表示
            List<UseState> uses = subs[i].getUseList();
            DefaultMutableTreeNode usesNode = new DefaultMutableTreeNode("use");
            if (uses.size() > 0) {
                child2.add(usesNode);
                for (UseState use: uses) {
                    DefaultMutableTreeNode useNode = new DefaultMutableTreeNode(use);
                    usesNode.add(useNode);
                }
            }
            // interface文の表示
            List<Procedures> interfaces = subs[i].getInterfaceList();
            for (Procedures in: interfaces) {
                DefaultMutableTreeNode varChild = new DefaultMutableTreeNode(in);
                child2.add(varChild);
                for (IProcedureItem item: in.getProcedures()) {
                    DefaultMutableTreeNode itemNode = new DefaultMutableTreeNode(item);
                    varChild.add(itemNode);
                }
            }
            // common文の表示
            List<Common> comList = subs[i].getCommonList();
            DefaultMutableTreeNode comRoot = new DefaultMutableTreeNode("common");
            if (comList.size() > 0) {
                child.add(comRoot);
                for (Common cm: comList) {
                    DefaultMutableTreeNode comNode = new DefaultMutableTreeNode(cm);
                    child2.add(comNode);
                }
            }
            VariableDefinition[] vars = subs[i].get_variables();
            for (int j = 0; j < vars.length; j++) {
                DefaultMutableTreeNode varChild = new DefaultMutableTreeNode(
                        vars[j]);
                child2.add(varChild);
            }
            Procedure[] subChildren = subs[i].get_children();
            setSubroutines(child2, subChildren);
        }

    }


    /**
     * フォートランデータベースから指定プロシージャをテキスト出力する。
     * @param file			出力ファイル
     * @param blocks		出力プロシージャリスト
     */
    public void exportLanguage(File file, IBlock[] blocks) {
        // 出力プロシージャチェック
        List<String> procs = new ArrayList<String>();
        if (blocks != null) {
            for (IBlock block : blocks) {
                if (block instanceof Procedure && ((Procedure) block).get_name() != null) {
                    procs.add(((Procedure) block).get_name());
                }
            }
        }

        // 出力プロシージャが存在しないので、ルートから検索する
        if (procs.size() <= 0) {
            if (this.fortranDb.getMainName() != null) {
                procs.add(this.fortranDb.getMainName());
            }
        }
        if (procs.size() <= 0) {
            // エラーメッセージ
            // languageservice.exportlanguage.procedure.error=出力プロシージャが存在しません。
            // languageservice.error=エラー
            JOptionPane.showMessageDialog(null,
                    Message.getString("languageservice.exportlanguage.procedure.error"), //出力プロシージャが存在しません。
                    Message.getString("languageservice.error"), //エラー
                    JOptionPane.ERROR_MESSAGE);
        }

        try {
            FileOutputStream fos = new FileOutputStream(file);
            OutputStreamWriter osw = new OutputStreamWriter(fos);
            BufferedWriter bw = new BufferedWriter(osw);
            StringBuffer buf = new StringBuffer();
            for (String proc : procs) {
                String text = writeText(proc);
                buf.append(text);
                buf.append("\n");
            }
            bw.write(buf.toString());
            bw.close();
            osw.close();
            fos.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * 指定したサブルーチン以下のルーチンツリーをテキスト出力する
     * @param   procName       プロシージャ名
     * @return   ツリーテキスト表現
     */
    private String writeText(String procName) {

        checkProgramUnitFlag.clear();

        Procedure proc = this.fortranDb.search_subroutine(procName);
        if (proc == null) {
            // エラーメッセージ : languageservice.procedure.error=[%s]は存在しません。
            JOptionPane.showMessageDialog(null,
                    Message.getString("languageservice.procedure.error", procName),
                    Message.getString("languageservice.error"),
                    JOptionPane.ERROR_MESSAGE);
            return null;
        }
        boolean flag = true;
        StringBuilder out = new StringBuilder();
        checkProgramUnitFlag.add(proc);
        if (proc.isFortran() && proc.get_name().equals(this.fortranDb.getMainName())) {
            out.append("program " + procName + "\n");
            writeTexts(proc.getBody(), 0, flag, out);
            out.append("end program\n");
            return out.toString();
        }
        else if (proc.isClang()) {
            out.append(proc.toString() + "\n");
            writeTexts(proc.getBody(), 0, flag, out);
            out.append("}\n");
            return out.toString();
        }

        out.append(procName + "\n");
        writeTexts(proc.getBody(), 0, flag, out);
        out.append("end\n");
        return out.toString();
    }

    /**
     * 再帰的に処理ブロックを取得し構造をテキスト出力する。
     *
     * @param parent
     *            対象となるブロックの配列
     * @param depth
     *            ツリーのネストの深さ
     * @param flag
     *            コール先を出力対象とするか判定するフラグ。真ならば探索する。
     * @param out
     *            出力String
     */
    private void writeTexts(Block parent, int depth, boolean flag, StringBuilder out) {
        final String STATEMENT_LEFT = " {\n";
        final String STATEMENT_RIGHT = "}\n";
        String end_left = "\n";
        boolean is_clang = parent.isClang();
        if (is_clang) end_left = STATEMENT_LEFT;

        StringBuilder indent = new StringBuilder("");
        for (int i = 0; i < depth; i++) {
            indent.append("|  ");
        }
        String indentString = indent.toString();
        for (Block block : parent.getBlocks()) {
            // 関数呼び出しに対する処理
            if (block instanceof ProcedureUsage) {
                ProcedureUsage call = (ProcedureUsage) block;
                String callName = call.getCallName();
                out.append(indentString + "+-" + call);
                if (flag && call.getCallDefinition() != null) {
                    out.append(end_left);
                }
                else {
                    out.append("\n");
                }
                // フラグが真なら呼び出し手続きに対して処理を実行する
                if (flag) {
                    if (call.getCallDefinition() != null) {
                        Procedure proc = call.getCallDefinition();
                        out.append(indentString + "|  " + proc + end_left);
                        boolean flagToWriteOnce = false; // 手続の宣言を最初だけ生成する場合はtrue
                        if (flagToWriteOnce) {
                            if (this.checkProgramUnitFlag.contains(proc)) {
                                continue;
                            }
                            this.checkProgramUnitFlag.add(proc);
                        }
                        // 循環のチェック
                        if (this.recursiveSub.contains(callName)) {
                            continue;
                        } else {
                            this.recursiveSub.add(callName);
                            writeTexts(proc.getBody(), depth + 1, flag, out);
                            recursiveSub.remove(callName);
                        }
                        if (is_clang) {
                            out.append(indentString + "|  " + STATEMENT_RIGHT);
                        }
                        else {
                            out.append(indentString + "|  end\n");
                        }
                    }
                }
                // 分岐に対する処理
            } else if (block instanceof Selection) {
                // TODO 条件式のExpressionに対応する
                Selection selec = (Selection) block;
                if (selec.isSelect()) {
                    out.append(indentString + "T-" + selec + end_left);
                    for (Condition cond : selec.getConditions()) {
                        out.append(indentString + "+-" + cond + end_left);
                        writeTexts(cond, depth + 2, flag, out);
                    }
                    if (is_clang) {
                        out.append(indentString + "V-" + STATEMENT_RIGHT);
                    }
                    else {
                        out.append(indentString + "V-endselect\n");
                    }
                } else {
                    out.append(indentString + "T-" + selec + end_left);
                    writeTexts(selec.getConditions().get(0), depth + 1,
                            flag, out);
                    for (int j = 1; j < selec.getConditions().size(); j++) {
                        out.append(indentString + "+-"
                                + selec.getConditions().get(j) + "\n");
                        writeTexts(selec.getConditions().get(j),
                                depth + 1, flag, out);
                    }
                    if (is_clang) {
                        out.append(indentString + "V-" + STATEMENT_RIGHT);
                    }
                    else {
                        out.append(indentString + "V-endif\n");
                    }
                }
                // 反復に対する処理
            } else if (block instanceof Repetition) {
                out.append(indentString + "T-" + block + end_left);
                writeTexts(block, depth + 1, flag, out);
                if (is_clang) {
                    out.append(indentString + "V-" + STATEMENT_RIGHT);
                }
                else {
                    out.append(indentString + "V-enddo\n");
                }
                // 子要素を持たない制御文に対する処理
            } else if (block instanceof Break || block instanceof GoTo
                    || block instanceof Pause || block instanceof Return
                    || block instanceof Termination
                    || block instanceof Continue) {
                out.append(indentString + "+-" + block + "\n");
            } else if (block instanceof Substitution) {
                writeTexts(block, depth, flag, out);
            } else if (block instanceof CompoundBlock) {
                // 複文（空文）
                writeTexts(block, depth, flag, out);
            }
        }
        return;
    }

    /**
     * ノードオブジェクトのプロパティを設定する
     * @param node			ノードオブジェクト
     * @param model			プロパティモデル
     */
    public void setProperties(Object node, PropertiesTableModel model) {

        List<String> items = new ArrayList<String>();
        List<String> values = new ArrayList<String>();

        if (node instanceof IBlock) {
            String[] blockItems = new String[]{
                Message.getString("languageservice.properties.classname"), //クラス名
                Message.getString("languageservice.properties.file"), //ファイル
                Message.getString("languageservice.properties.linenumber"), //行番号
                Message.getString("languageservice.properties.statement")}; //ステートメント

            items.addAll(java.util.Arrays.asList(blockItems));

            // クラス名
            values.add(node.getClass().getName());
            // ファイルパス
            values.add(getSourceFilePath((IBlock) node));
            // 行番号
            values.add(getCodeLine((IBlock) node));
            // ステートメント
            values.add(((IBlock) node).toString());

            if (node instanceof Module) {
                items.addAll(Arrays.asList(
                        Message.getString("languageservice.properties.language")));
                // 言語
                values.add(((Module)node).getFileType().toString());
            }
            else if (node instanceof VariableDefinition) {
                items.addAll(Arrays.asList(
                        Message.getString("languageservice.properties.variablename"), //変数名
                        Message.getString("languageservice.properties.datatype"), //データ型
                        Message.getString("languageservice.properties.attribute"))); //属性
                // 変数名
                values.add(((VariableDefinition) node).get_name());
                // データ型
                String var_type = "";
                if (((VariableDefinition) node).getType() != null) {
                    var_type = ((VariableDefinition) node).getType().toString();
                }
                values.add(var_type);

                // 属性
                String attribute = null;
                if (((VariableDefinition) node).getAttribute() != null) {
                    attribute = ((VariableDefinition) node).getAttribute().toString();
                }
                values.add(attribute);
            }
            else if (node instanceof Continue) {
                items.add(Message.getString("languageservice.properties.label")); //ラベル
                // ラベル
                String label = ((Continue) node).get_start().get_label();
                if (Statement.NO_LABEL.equalsIgnoreCase(label)) {
                    label = null;
                }
                values.add(label);
            }
            else if (node instanceof Break) {
                items.add(Message.getString("languageservice.properties.doname")); //DO構文名
                // DO構文名
                String label = ((Break) node).get_start().get_label();
                if (Statement.NO_LABEL.equalsIgnoreCase(label)) {
                    label = null;
                }
                values.add(label);
            }
            else if (node instanceof GoTo) {
                items.add(Message.getString("languageservice.properties.statementnumber")); //文番号
                // 文番号
                String argument = ((GoTo) node).getArgument();
                values.add(argument);
            }
            else if (node instanceof Pause) {
                items.add(Message.getString("languageservice.properties.pausecode")); //PAUSE-CODE
                // pause-code
                String argument = ((Pause) node).getArgument();
                values.add(argument);
            }
            else if (node instanceof Termination) {
                items.add(Message.getString("languageservice.properties.statuscode")); //終了コード
                // 終了コード
                String argument = ((Termination) node).getArgument();
                values.add(argument);
            }
            else if (node instanceof ProcedureUsage) {
                items.add(Message.getString("languageservice.properties.actualargument")); //実引数
                // 実引数
                List<Expression> args = ((ProcedureUsage) node).getArguments();
                StringBuffer argument = new StringBuffer();
                if (args != null) {
                    int count = 0;
                    for (Expression expr : args) {
                        if (count > 0) argument.append(", ");
                        argument.append(expr.getLine());
                        count++;
                    }
                }
                values.add(argument.toString());
            }
            else if (node instanceof Procedure) {
                items.addAll(Arrays.asList(
                        Message.getString("languageservice.properties.dummyargument"), //仮引数
                        Message.getString("languageservice.properties.returnvalue"), //戻り値
                        Message.getString("languageservice.properties.attribute"), //属性
                        Message.getString("languageservice.properties.result"))); //結果
                // 仮引数
                Variable[] args = ((Procedure) node).get_args();
                StringBuffer argument = new StringBuffer();
                if (args != null) {
                    int count = 0;
                    for (Variable var : args) {
                        if (count > 0) argument.append(", ");
                        argument.append(var.getName());
                        count++;
                    }
                }
                values.add(argument.toString());

                // 戻り値
                IVariableType varType = ((Procedure) node).getReturnValueType();
                StringBuffer buf = new StringBuffer();
                if (varType != null) {
                    buf.append(varType.toString());
                }
                values.add(buf.toString());

                // 属性
                buf = new StringBuffer();
                ScopeAttribute scope = ((Procedure) node).getScope();
                if (scope == ScopeAttribute.PUBLIC) {
                    if (buf.length() > 0) buf.append(",");
                    buf.append("public");
                }
                if (scope == ScopeAttribute.PRIVATE) {
                    if (buf.length() > 0) buf.append(",");
                    buf.append("private");
                }
                if (((Procedure) node).has_attribute("recursive")) {
                    if (buf.length() > 0) buf.append(",");
                    buf.append("recursive");
                }
                values.add(buf.toString());

                // result
                values.add(((Procedure) node).getResult());
            }
            else if (node instanceof DynamicNullification) {
                items.add(Message.getString("languageservice.properties.pointervariable")); //ポインタ変数
                // ポインタ変数
                List<Variable> args = ((DynamicNullification) node).getTarget();
                StringBuffer argument = new StringBuffer();
                if (args != null) {
                    int count = 0;
                    for (Variable expr : args) {
                        if (count > 0) argument.append(", ");
                        argument.append(expr.getName());
                        count++;
                    }
                }
                values.add(argument.toString());
            }
            else if (node instanceof DynamicAllocation) {
                items.add(Message.getString("languageservice.properties.allocationvariable")); //割付変数
                // 割付変数
                Map<Variable, VariableDimension> args = ((DynamicAllocation) node).getTarget();
                StringBuffer argument = new StringBuffer();
                if (args != null) {
                    int count = 0;
                    Set<Variable> keySet = args.keySet();
                    Iterator<Variable> keyIte = keySet.iterator();
                    while(keyIte.hasNext()) {
                        if (count > 0) argument.append(", ");
                        Variable key = keyIte.next();
                        VariableDimension value = args.get(key);
                        argument.append(key.toString());
                        if (value != null) {
                            argument.append(value.toString());
                        }
                        count++;
                    }
                }
                values.add(argument.toString());
            }
            else if (node instanceof DynamicDeallocation) {
                items.add(Message.getString("languageservice.properties.deallocationvariable")); //破棄変数
                // 破棄変数
                List<Variable> args = ((DynamicDeallocation) node).getTarget();
                StringBuffer argument = new StringBuffer();
                if (args != null) {
                    int count = 0;
                    for (Variable var : args) {
                        if (count > 0) argument.append(", ");
                        argument.append(var.toString());
                        count++;
                    }
                }
                values.add(argument.toString());
            }
            else if (node instanceof Selection) {
                if (((Selection)node).isSelect()) {
                    items.addAll(Arrays.asList(
                            Message.getString("languageservice.properties.conditional"), //条件式
                            Message.getString("languageservice.properties.case"))); //CASE文
                    // 条件式
                    Expression caseCondition = ((Selection)node).getCaseCondition();
                    values.add(caseCondition.toString());
                }
                else if (((Selection)node).isIF()) {
                    items.addAll(Arrays.asList(Message.getString("languageservice.properties.conditional"))); //条件式

                }
                StringBuffer buf = new StringBuffer();
                List<Condition> conditions = ((Selection)node).getConditions();
                if (conditions != null) {
                    int count = 0;
                    for (Condition cond : conditions) {
                        if (count > 0) buf.append(", ");
                        buf.append("(");
                        if (cond.getExpression() != null) {
                            buf.append(cond.conditionToString());
                        }
                        else {
                            if (((Selection)node).isSelect()) {
                                buf.append("default");
                            }
                            else if (((Selection)node).isIF()) {
                                buf.append("else");
                            }

                        }
                        buf.append(")");
                        count++;
                    }
                }
                values.add(buf.toString());
            }

            /***  for debug  at 2012/01/22 by @hira  */
            // デバッグ出力
            // printProperties((IBlock)node, 0);
            /***  for debug  at 2012/01/22 by @hira  */
        }
        else {
            String[] blockItems = new String[]{
                Message.getString("languageservice.properties.classname"), //クラス名
                Message.getString("languageservice.properties.statement")}; //ステートメント
            items.addAll(java.util.Arrays.asList(blockItems));

            // クラス名
            values.add(node.getClass().getName());
            // ステートメント
            values.add(node.toString());
        }

        // プロパティモデルに設定する
        // プロパティパネルへの通知はObserverにて通知される。
        model.setTitle(node.toString());
        model.setProperties(items.toArray(new String[0]), values.toArray(new String[0]));

    }

    /**
     * ブロックのソースファイルパスを取得する
     * @param node		ブロック
     * @return		ソースファイルパス
     */
    private String getSourceFilePath(IBlock node) {

        if (node == null) return null;
        if (node.getStartCodeLine() == null) return null;

        // クラス名
        String classname = node.getClass().getName();

        // ファイル
        String errMsg = null;
        if (node.getStartCodeLine() == null) {
            errMsg = Message.getString("languageservice.sourcefilepath.block.error", classname); //の行ブロックが取得できませんでした。
        }
        if (errMsg != null) {
            // 変数宣言文の場合は、エラーメッセージを表示しない
            // 暗黙の型宣言の時は、ファイル、行番号はない
            if (!(node instanceof VariableDefinition)) {
                this.addErrorInfo(errMsg);
            }
            return null;
        }

//        return node.getStartCodeLine().getSourceFile().getFile().getPath();
        return node.getStartCodeLine().getStrSourceFile();

    }

    /**
     * ブロックの行番号を取得する
     * @param node		ブロック
     * @return		行番号
     */
    private String getCodeLine(IBlock node) {

        if (node == null) return null;
        if (node.getStartCodeLine() == null) return null;

        String line = "";
        int start = node.getStartCodeLine().getStartLine();
        int end = 0;
        if (node.getEndCodeLine() != null) {
            end = node.getEndCodeLine().getEndLine();
        }
        if (start > 0) {
            line = String.valueOf(start);
            if (start < end) {
                line += ":" + String.valueOf(end);
            }
        }

        return line;
    }

    /**
     * Languageクラスのデシリアライズを行う
     * @param folder		Languageクラスのシリアライズフォルダ
     */
    public void readLanguage(File folder) {
        // languageservice.deserialize.start.status=Languageデシリアライズ:開始
        Application.status.setMessageStatus(Message.getString("languageservice.deserialize.start.status"));
        Application.status.setProgressStart(true);

        /*
         *  folderからLanguageクラスのデシリアライズを行う
         */
        try {
            if (!new File(folder.getPath() + File.separator + KscopeProperties.DATABASE_FILE).exists()) {
                String error = Message.getString("languageservice.readlanguage.notexists.database.error", KscopeProperties.DATABASE_FILE);
                throw new LanguageException(error);
            }
            // (2012/5/24) changed by Tomiyama
            languageStream = new ObjectInputStream(new FileInputStream(folder.getPath() + File.separator + KscopeProperties.DATABASE_FILE));
            this.fortranDb = (Fortran) languageStream.readObject();
            languageStream.close();
            languageStream = null;

            this.fortranDb.analyseDB();

            // ソースファイルリストの設定を行う
            List<SourceFile> listSrc = this.fortranDb.getProcedureFileList();
            SourceFile[] sourceFiles = null;
            if (listSrc != null) {
                sourceFiles = listSrc.toArray(new SourceFile[0]);
            }

            if (sourceFiles != null && sourceFiles.length > 0) {
                this.modelFile.setSourceFile(sourceFiles);
            }

            /*
             * Languageクラスのデシリアライズ後、 構造ツリー、モジュールツリーの作成を行う。
             */
            // 構造ツリーへの作成
            writeTree();
            this.modelLanguage.notifyModel();

            // モジュールツリーの作成
            setFortranModules();
            this.modelModule.notifyModel();

        } catch (LanguageException lang_ex) {
            Logger.error(lang_ex);
            Logger.error(lang_ex.getCodeInfo());
            // lang_ex.printStackTrace();
            // エラー箇所の情報をセットする
            this.addErrorInfo(lang_ex);
            throw  lang_ex;
        } catch (IOException io_ex) {
            if (!this.isCancel()) {
                Logger.error(io_ex);
                io_ex.printStackTrace();
                String error_message = io_ex.getMessage();
                if (error_message == null) {
                    error_message = io_ex.toString();
                }
                // エラー箇所の情報をセットする
                this.addErrorInfo(error_message);
            }
        } catch (Exception ex) {
            Logger.error(ex);
            ex.printStackTrace();
            String error_message = ex.getMessage();
            if (error_message == null) {
                error_message = ex.toString();
            }
            // エラー箇所の情報をセットする
            this.addErrorInfo(error_message);
        }

        Application.status.setProgressStart(false);
        // languageservice.deserialize.done.status=Languageデシリアライズ:終了
        Application.status.setMessageStatus(Message.getString("languageservice.deserialize.done.status"));
    }


    /**
     * Languageクラスのシリアライズを行う
     * @param folder		Languageクラスのシリアライズフォルダ
     */
    public void writeLanguage(File folder) {
        // languageservice.serialize.start.status=Languageシリアライズ:開始
        Application.status.setMessageStatus(Message.getString("languageservice.serialize.start.status"));
        Application.status.setProgressStart(true);

        /*
         *  folderにLanguageクラスのシリアライズを行う
         */
        try {
            // (2012/5/24) added by Tomiyama
            ObjectOutputStream oos = new ObjectOutputStream(
                    new FileOutputStream(folder.getPath() + File.separator + KscopeProperties.DATABASE_FILE));
            oos.writeObject(this.fortranDb);
            oos.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }

        Application.status.setProgressStart(false);
        // languageservice.serialize.done.status=Languageシリアライズ:終了
        Application.status.setMessageStatus(Message.getString("languageservice.serialize.done.status"));
    }


    /**
     * フォートラン構文解析結果格納データベースを取得する
     * @return		フォートラン構文解析結果格納データベース
     */
    public Fortran getFortranLanguage() {
        return fortranDb;
    }

    /**
     * フォートラン構文解析結果格納データベースを設定する
     * @param fDb		フォートラン構文解析結果格納データベース
     */
    public void setFortranLanguage(Fortran fDb) {
        this.fortranDb = fDb;
    }

    /**
     * プロジェクトフォルダを取得する
     * @return		プロジェクトフォルダ
     */
    public File getProjectFolder() {
        return projectFolder;
    }

    /**
     * プロジェクトフォルダを設定する
     * @param folder		プロジェクトフォルダ
     */
    public void setProjectFolder(File folder) {
        this.projectFolder = folder;

        // プロジェクトフォルダを設定する
        if (this.fortranParser != null)
            this.fortranParser.setBaseFolder(this.projectFolder);
        if (this.clangParser != null)
            this.clangParser.setBaseFolder(this.projectFolder);
    }

    /**
     * ソースファイルを設定する
     * @param files
     */
    public void setSourceFiles(SourceFile [] files) {
        this.files = files;
    }

    /**
     * XcodeMLパーサーを作成する.
     * Fortran,C言語用のXcodeMLパーサを生成する
     */
    private void initializeXcodemlPerser() {
        // Fortran用のXcodeMLパーサ
        this.fortranParser = new jp.riken.kscope.xcodeml.fortran.XcodeMLParserStax();
        // C言語用のXcodeMLパーサ
        this.clangParser = new jp.riken.kscope.xcodeml.clang.XcodeMLParserStax();

        this.fortranParser.setCancel(false);
        this.fortranParser.setConfirmInclude(true);

        this.clangParser.setCancel(false);
        this.clangParser.setConfirmInclude(true);

        return;
    }

    /**
     * パースが実行できるかチェックする
     * @return   true=実行可
     */
    public boolean canParse() {
        if (this.files == null) return false;
        if (this.files.length <= 0) return false;
        if (this.fortranDb == null) return false;
        if (this.projectFolder == null) return false;
        if (!this.projectFolder.exists()) return false;
        if (!this.projectFolder.isDirectory()) return false;
        if (this.modelFile == null) return false;
        if (this.modelLanguage == null) return false;

        return true;
    }

    /**
     * エクスプローラビューをセットする.
     */
    public void setExplorerView() {
        if (this.fortranDb == null) return;

        // キャンセルチェック
        if (this.isCancel()) return;

        // ソースファイルリスト
        List<SourceFile> listSrc = this.fortranDb.getProcedureFileList();
        SourceFile[] srcs = null;
        if (listSrc != null) {
            srcs = listSrc.toArray(new SourceFile[0]);
        }

        Application.status.setMessageStatus("creating XML List...");
        // XMLファイルリスト
        SourceFile[] xmls = null;
        if (srcs != null && srcs.length > 0) {
            List<SourceFile> listXml = new ArrayList<SourceFile>();
            xmls = new SourceFile[srcs.length];
            for (int i=0; i<srcs.length; i++) {
                if (srcs[i] != null && srcs[i].getRelationFile() != null
                    && !listXml.contains(srcs[i].getRelationFile())) {
                    listXml.add(srcs[i].getRelationFile());
                }
            }
            if (listXml.size() > 0) {
                xmls = listXml.toArray(new SourceFile[0]);
            }
        }

        // キャンセルチェック
        if (this.isCancel()) return;
        Application.status.setMessageStatus("creating Source tree...");
        // ソースファイルツリーの作成
        if (this.modelFile != null) {
            this.modelFile.clearTreeModel();
            this.modelFile.setSourceFile(srcs);
        }

        // キャンセルチェック
        if (this.isCancel()) return;
        Application.status.setMessageStatus("creating XML tree...");
        // XMLファイルツリーの作成
        if (this.modelXml != null) {
            this.modelXml.clearTreeModel();
            this.modelXml.setSourceFile(xmls);
        }

        // キャンセルチェック
        if (this.isCancel()) return;
        Application.status.setMessageStatus("creating Language tree...");
        // 構造ツリーへの作成
        if (this.modelLanguage != null) {
            this.modelLanguage.clearTreeModel();
            writeTree();
            this.modelLanguage.notifyModel();
        }

        // キャンセルチェック
        if (this.isCancel()) return;
        Application.status.setMessageStatus("creating Module tree...");
        // モジュールツリーの作成
        if (this.modelModule != null) {
            this.modelModule.clearTreeModel();
            setFortranModules();
            this.modelModule.notifyModel();
        }

        return;
    }

    /**
     * ファイルリストから重複ファイルを削除する.
     * @param list		ファイルリスト
     * @return          重複削除ファイルリスト
     */
    private List<SourceFile> validFileList(SourceFile[] list) {
        if (list == null) return null;
        List<SourceFile> results = new ArrayList<SourceFile>();
        for (SourceFile file : list) {
            if (!results.contains(file)) {
                results.add(file);
            }
        }
        if (results.size() <= 0) return null;
        return results;
    }

    /**
     * 構造ツリー展開深さをクリアする.
     */
    public void clearTreeDepth() {
        this.treeDepth = 0;
    }

}


