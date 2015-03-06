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
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.Condition;
import jp.riken.kscope.language.ExecutableBody;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Module;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.Selection;

/**
 * フォートランデータベース検索クラス
 * @author RIKEN
 */
public class LanguageSearcher {

    /** フォートラン構文解析結果格納データベース. */
    private Fortran fortranDb;
    /** 検索コード行 */
    private CodeLine searchCode;
    /** 検索サブルーチン名 */
    private String callname;

    /**
     * コンストラクタ
     * @param db		フォートラン構文解析結果格納データベース
     * @param code		検索コード行
     */
    public LanguageSearcher(Fortran db, CodeLine code) {
        this.fortranDb = db;
        this.searchCode = code;
    }

    /**
     * コンストラクタ
     * @param db		フォートラン構文解析結果格納データベース
     * @param callname	サブルーチン名
     */
    public LanguageSearcher(Fortran db, String callname) {
        this.fortranDb = db;
        this.callname = callname;
    }

    /**
     * フォートラン構文解析結果格納データベースからコード行と一致するブロックの取得を行う。
     * @return			一致ブロック
     */
    public IBlock searchCodeLine() {
        if (this.searchCode == null) return null;
        if (this.searchCode.getSourceFile() == null) return null;
        if (this.fortranDb == null) return null;
        Map<String, Module> modules = this.fortranDb.getModules();
        if (modules == null || modules.size() <= 0) {
            return null;
        }

        // モジュール探索を行う
        Set<String> keySet = modules.keySet();
        for (String key : keySet) {
            Module module = modules.get(key);
            IBlock block = selectCodeLine(module);
            if (block != null) return block;
        }

        return null;
    }

    /**
     * フォートラン構文解析結果格納データベースから検索サブルーチン名と一致するブロックの取得を行う。
     * @return			一致サブルーチン
     */
    public IBlock[] searchProcedureUsage() {
        if (this.callname == null) return null;
        if (this.fortranDb == null) return null;
        Map<String, Module> modules = this.fortranDb.getModules();
        if (modules == null || modules.size() <= 0) {
            return null;
        }

        // モジュール探索を行う
        List<IBlock> list = new ArrayList<IBlock>();
        Set<String> keySet = modules.keySet();
        for (String key : keySet) {
            Module module = modules.get(key);
            IBlock[] blocks = selectProcedureUsage(module);
            if (blocks != null) {
                list.addAll(Arrays.asList(blocks));
            }
        }
        if (list.size() <= 0) return null;
        return list.toArray(new IBlock[0]);
    }

    /**
     * モジュールから検索サブルーチン名と一致するブロックの取得を行う。
     * @param module		検索モジュール
     * @return			検索結果ブロック
     */
    private IBlock[] selectProcedureUsage(Module module) {

        if (this.callname == null) return null;
        if (this.fortranDb == null) return null;
        if (module == null) return null;

        // 副プログラム単位.
        List<IBlock> list = new ArrayList<IBlock>();
        Procedure[] childrens = module.get_children();
        if (childrens != null) {
            for (Procedure children : childrens) {
                IBlock[] blocks = selectProcedureUsage(children);
                if (blocks != null) {
                    list.addAll(Arrays.asList(blocks));
                }
            }
        }
        if (list.size() <= 0) return null;
        return list.toArray(new IBlock[0]);
    }

    /**
     * 手続きから検索サブルーチン名と一致するブロックの取得を行う。
     * @param procedure		検索プロシージャ
     * @return			検索結果ブロック
     */
    private IBlock[] selectProcedureUsage(Procedure procedure) {

        if (this.callname == null) return null;
        if (this.fortranDb == null) return null;
        if (procedure == null) return null;

        // 副プログラム単位.
        List<IBlock> list = new ArrayList<IBlock>();
        Procedure[] childrens = procedure.get_children();
        if (childrens != null) {
            for (Procedure children : childrens) {
                IBlock[] blocks = selectProcedureUsage(children);
                if (blocks != null) {
                    list.addAll(Arrays.asList(blocks));
                }
            }
        }

        ExecutableBody body = procedure.getBody();
        IBlock[] blocks = selectProcedureUsage(body);
        if (blocks != null) {
            list.addAll(Arrays.asList(blocks));
        }
        if (list.size() <= 0) return null;
        return list.toArray(new IBlock[0]);
    }

    /**
     * 手続きから検索サブルーチン名と一致するブロックを検索する。
     * @param body		検索処理ブロック
     * @return			検索結果ブロック
     */
    private IBlock[] selectProcedureUsage(ExecutableBody body) {
        if (this.callname == null) return null;
        if (this.fortranDb == null) return null;
        if (body == null) return null;

        List<IBlock> list = new ArrayList<IBlock>();
        ArrayList<Block> listBlock = body.getChildren();
        if (listBlock == null) return null;
        for (Block children : listBlock) {
            IBlock[] blocks = null;
            if (children instanceof Selection) {
                blocks = selectProcedureUsage((Selection)children);
            }
            else {
                blocks = selectProcedureUsage(children);
            }
            if (blocks != null) {
                list.addAll(Arrays.asList(blocks));
            }
        }
        if (list.size() <= 0) return null;
        return list.toArray(new IBlock[0]);
    }


    /**
     * 手続きから検索コード行と一致するブロックを検索する。
     * @param block		検索ブロック
     * @return			検索結果ブロック
     */
    private IBlock[] selectProcedureUsage(Block block) {
        if (this.callname == null) return null;
        if (this.fortranDb == null) return null;
        if (block == null) return null;

        List<IBlock> list = new ArrayList<IBlock>();
        // ProcedureUsageクラスであるかチェックする
        if (block instanceof ProcedureUsage) {
            String name = ((ProcedureUsage)block).getCallName();
            if (this.callname.equalsIgnoreCase(name)) {
                list.add(block);
            }
        }
        ArrayList<Block> listBlock = block.getChildren();
        if (list != null) {
            for (Block children : listBlock) {
                IBlock[] results = selectProcedureUsage(children);
                if (results != null) {
                    list.addAll(Arrays.asList(results));
                }
            }
        }
        if (list.size() <= 0) return null;
        return list.toArray(new IBlock[0]);
    }

    /**
     * モジュールから検索コード行と一致するブロックを検索する。
     * @param module		検索モジュール
     * @return			検索結果ブロック
     */
    private IBlock selectCodeLine(Module module) {
        if (this.searchCode == null) return null;
        if (this.fortranDb == null) return null;
        if (module == null) return null;

        // 検索ブロックが検索コード行と一致するかチェックする
        if (isMatchCodeLine(module)) return module;

        // 副プログラム単位.
        Procedure[] childrens = module.get_children();
        if (childrens != null) {
            for (Procedure children : childrens) {
                IBlock block = selectCodeLine(children);
                if (block != null) return block;
            }
        }

        return null;
    }

    /**
     * 手続きから検索コード行と一致するブロックを検索する。
     * @param procedure		検索プロシージャ
     * @return			検索結果ブロック
     */
    private IBlock selectCodeLine(Procedure procedure) {
        if (this.searchCode == null) return null;
        if (this.fortranDb == null) return null;
        if (procedure == null) return null;

        // 検索ブロックが検索コード行と一致するかチェックする
        if (isMatchCodeLine(procedure)) return procedure;
        // 検索ブロックが検索コード行の範囲であるかチェックする
        if (!isBlockCodeLine(procedure)) return null;

        // 副プログラム単位.
        Procedure[] childrens = procedure.get_children();
        if (childrens != null) {
            for (Procedure children : childrens) {
                IBlock block = selectCodeLine(children);
                if (block != null) return block;
            }
        }

        ExecutableBody body = procedure.getBody();
        IBlock block = selectCodeLine(body);

        return block;
    }

    /**
     * 手続きから検索コード行と一致するブロックを検索する。
     * @param body		検索処理ブロック
     * @return			検索結果ブロック
     */
    private IBlock selectCodeLine(ExecutableBody body) {
        if (this.searchCode == null) return null;
        if (this.fortranDb == null) return null;
        if (body == null) return null;

        ArrayList<Block> list = body.getChildren();
        if (list == null) return null;
        for (Block children : list) {
            IBlock block = null;
            if (children instanceof Selection) {
                block = selectCodeLine((Selection)children);
            }
            else {
                block = selectCodeLine(children);
            }

            if (block != null) return block;
        }
        return null;
    }


    /**
     * 手続きから検索コード行と一致するブロックを検索する。
     * @param block		検索ブロック
     * @return			検索結果ブロック
     */
    private IBlock selectCodeLine(Block block) {
        if (this.searchCode == null) return null;
        if (this.fortranDb == null) return null;
        if (block == null) return null;

        // 検索ブロックが検索コード行と一致するかチェックする
        if (isMatchCodeLine(block)) return block;
        // 検索ブロックが検索コード行の範囲であるかチェックする
        if (!isBlockCodeLine(block)) return null;

        ArrayList<Block> list = block.getChildren();
        if (list != null) {
            for (Block children : list) {
                IBlock result = selectCodeLine(children);
                if (result != null) return result;
            }
        }

        return null;
    }

    /**
     * 条件分岐文(IF, SELECT)と検索コード行が一致するかチェックする
     * @param block		検索分岐ブロック
     * @return			検索結果ブロック
     */
    private IBlock selectCodeLine(Selection block) {
        if (this.searchCode == null) return null;
        if (this.fortranDb == null) return null;
        if (block == null) return null;

        // 検索ブロックが検索コード行と一致するかチェックする
        if (isMatchCodeLine(block)) return block;
        // 検索ブロックが検索コード行の範囲であるかチェックする
        if (!isBlockCodeLine(block)) return null;
        // 条件文
        List<Condition> list = block.getConditions();
        if (list != null) {
            for (Condition condition : list) {
                IBlock result = selectCodeLine(condition);
                if (result != null) return result;
            }
        }
        // 子要素の検索を行う
        return selectCodeLine((Block)block);
    }

    /**
     * 検索ブロックが検索コード行の範囲であるかチェックする
     * @param block		検索ブロック
     * @return			true=ブロック範囲
     */
    private boolean isBlockCodeLine(IBlock block) {
        if (this.searchCode == null) return false;
        if (this.searchCode.getSourceFile() == null) return false;
        if (block == null) return false;
        CodeLine blockCode = block.getStartCodeLine();
        if (blockCode == null) return false;

        // 検索ファイルが一致しているか
        SourceFile file = this.searchCode.getSourceFile();
        if (!file.equals(blockCode.getSourceFile())) return false;

        // 検索行番号がブロックの範囲内に存在するか？
        int searchStartno = this.searchCode.getStartLine();
        int searchEndno = this.searchCode.getEndLine();
        int blockStartno = block.getStartCodeLine().getStartLine();
        int blockEndno = block.getEndCodeLine().getEndLine();

        if (blockStartno > searchEndno || searchStartno > blockEndno) {
            return false;
        }

        return true;
    }

    /**
     * 検索ブロックが検索コード行と一致するかチェックする
     * @param block		検索ブロック
     * @return			true=ブロック範囲
     */
    private boolean isMatchCodeLine(IBlock block) {
        if (this.searchCode == null) return false;
        if (this.searchCode.getSourceFile() == null) return false;
        if (block == null) return false;
        CodeLine blockCode = block.getStartCodeLine();
        if (blockCode == null) return false;

        // 検索ファイルが一致しているか
        SourceFile file = this.searchCode.getSourceFile();
        if (!file.equals(blockCode.getSourceFile())) return false;

        // 検索行番号がブロックのと一致するか？
        int searchStartno = this.searchCode.getStartLine();
        //int searchEndno = this.searchCode.getEndLine();
        int blockStartno = block.getStartCodeLine().getStartLine();
        //int blockEndno = block.getEndCodeLine().getEndLine();

        // 開始行が一致したら、一致したと見なす。
        if (blockStartno != searchStartno) return false;

        return true;
    }
}
