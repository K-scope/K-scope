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
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jp.riken.kscope.data.BlockData;
import jp.riken.kscope.data.BlockList;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.Common;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IDeclarations;
import jp.riken.kscope.language.Module;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.UseState;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.fortran.VariableAttribute;
import jp.riken.kscope.language.fortran.VariableAttribute.ScopeAttribute;
import jp.riken.kscope.language.utils.LanguageUtils;
import jp.riken.kscope.model.ScopeModel;

/**
 * 変数有効域サービスクラス.<br/>
 * 変数有効域テーブルを作成する
 *
 * @author RIKEN
 */
public class AnalysisScopeService extends AnalysisBaseService {

    /** 変数有効域モデル */
    private ScopeModel modelScope;

    /**
     * 変数有効域の探索方法
     * true = 変数の使用域の探索
     * false = 変数の言語使用有効域の探索（デフォルト）
     */
    private boolean scope_actual;

    /**
     * コンストラクタ
     *
     * @param fortran
     *            フォートランデータベース
     */
    public AnalysisScopeService(Fortran fortran) {
        super(fortran);
        this.scope_actual = false;
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
        if (variable == null) return;

        // 変数有効域を探索する
        List<IBlock> result = this.analysisScope(variable, null);

        // ソート実行
        result = LanguageUtils.sortBlock(result);

        // 変数有効域テーブルの設定
        String title = variable.toString();
        if (this.scope_actual) {
            title = "[Reference-based] " + title;
        }
        else {
            title = "[Rigorous] " + title;
        }
        this.modelScope.setTitle(title);
        if (result != null) {
            this.modelScope.setScope(result.toArray(new IBlock[0]));
        }
        return;
    }


    /**
     * 変数有効域を作成する
     * @param variable           変数有効域変数
     */
    private List<IBlock> analysisScope(VariableDefinition variable, IBlock search_block) {

        if (variable == null) {
            return null;
        }

        boolean has_searchblock = (search_block != null);
        String varName = variable.get_name();
        if (search_block == null) {
            search_block = variable.getMother();
        }
        if (search_block == null) return null;

        List<IBlock> result = new ArrayList<IBlock>();
        if (search_block instanceof Module
            && ((Module)search_block).get_procedures() != null) {
            Procedure[] procs = ((Module)search_block).get_procedures();
            for (Procedure block : procs) {
                List<IBlock> list = this.analysisScope(variable, block);
                if (list != null) {
                    result = this.addBlockList(result, list);
                }
            }
        }
        else {
            boolean is_fortran = search_block.isFortran();
            // 子宣言ブロックを取得する
            IBlock data = new BlockData(search_block);
            int var_lineno = variable.getStartCodeLine().getStartLine();
            if (!has_searchblock) {
                if (!is_fortran) {
                    ((BlockData)data).setStartLineno(var_lineno);
                }
            }
            IBlock current_block = new BlockList();
            ((BlockList)current_block).addBlock(data);

            Set<IDeclarations> child_decs = current_block.getDeclarationsBlocks();
            if (child_decs != null) {
                List<IBlock> list = new ArrayList<IBlock>();
                boolean exists_block = false;
                for (IDeclarations child : child_decs) {
                    if (!(child instanceof IBlock)) continue;
                    if (child instanceof Module) continue;
                    int child_lineno = ((IBlock)child).getStartCodeLine().getStartLine();

                    // 副プログラムに同一の名前の宣言が無ければ追加：Procedureのみ
                    VariableDefinition dup_def = child.get_variable(varName);
                    if (dup_def == variable) {
                        exists_block = true;
                        continue;     // 同一変数
                    }
                    // 同一名変数が仮引数であるかチェックする。
                    boolean is_argument = this.isArgumentVariable(dup_def);
                    if (is_argument) {
                        // 仮引数の定義であるので、対象外
                        continue;
                    }
                    boolean is_global = this.isGlobalVariable(variable);
                    boolean is_dup_global = this.isGlobalVariable(dup_def);
                    if (is_global) {
                        if (var_lineno > child_lineno) continue;    // 変数定義より前行のブロックは対象外
                    }
                    // 同名変数が使用されているかチェックする
                    boolean is_marge = this.isActualBlock(
                                (IBlock)child,
                                variable,
                                dup_def);


                    if (this.scope_actual && !is_marge) {
                        if (exists_block) is_marge = true;
                        else continue;
                        /*
                        if (dup_def == null) continue;
                        else {
                            if (((IBlock)child).getBlockType() == BlockType.PROCEDURE
                                || (child instanceof BlockList
                                    && ((BlockList)child).hasBlockType(BlockType.PROCEDURE))) {
                                current_block = null;
                                break;
                            }
                            is_marge = true;
                        }
                        */
                    }
                    if (this.scope_actual && is_marge) {
                        exists_block = true;
                    }
                    if (!this.scope_actual) {
                        is_marge = true;
                        exists_block = true;
                    }
                    if (dup_def != null) {
                        IBlock marge_blocks = null;
                        if (is_marge) {
                            // 同一名変数が存在しているので、ブロックから削除する
                            BlockData child_data = new BlockData((IBlock)child);
                            if (!is_fortran) {
                                int lineno = dup_def.getStartCodeLine().getStartLine();
                                child_data.setStartLineno(lineno);
                            }
                            marge_blocks = this.margeBlocks(current_block, child_data);
                        }
                        if (marge_blocks != null) {
                            current_block = marge_blocks;
                            exists_block = true;
                        }
                        else {
                            // 副プログラムに同一の名前が存在しているので対象外
                            if (((IBlock)child).getBlockType() == BlockType.PROCEDURE
                                || (child instanceof BlockList
                                    && ((BlockList)child).hasBlockType(BlockType.PROCEDURE))) {
                                current_block = null;
                                break;
                            }
                        }
                    }
                    // 副プログラムのみ追加する。
                    else if (child instanceof Procedure) {
                        if (child != search_block
                            && !(search_block instanceof BlockList
                                && ((BlockList)search_block).contains((IBlock)child))) {
                            List<IBlock> child_list = this.analysisScope(variable, (IBlock)child);
                            if (child_list != null) {
                                result = this.addBlockList(result, child_list);
                            }
                        }
                    }
                }
                if (exists_block && current_block != null) {
                    list.add(current_block);
                }
                if (list.size() > 0) {
                    result = this.addBlockList(result, list);
                }
            }
            else {
                // 宣言が属するプログラム単位を追加
                if (current_block.getBlockType() == BlockType.PROCEDURE
                    || current_block.getBlockType() == BlockType.COMPOUND) {
                    result.add(current_block);
                }
                else if (current_block instanceof BlockList) {
                    if (((BlockList)current_block).hasBlockType(BlockType.PROCEDURE)
                        || ((BlockList)current_block).hasBlockType(BlockType.COMPOUND)) {
                        result.add(current_block);
                    }
                }
            }
        }

        // private属性かチェック
        VariableAttribute att = (VariableAttribute) variable.getAttribute();
        if (!(att != null && att.getScope() == ScopeAttribute.PRIVATE)) {

            // COMMON文に含まれるかチェック
            {
                List<IBlock> list = this.searchCOMMON(variable);
                if (list != null) {
                    result = this.addBlockList(result, list);
                }
            }
            // 各モジュールのUSE文を探索
            if (!has_searchblock) {
                List<IBlock> list = this.searchUSE(
                                            variable,
                                            has_searchblock?search_block:null);
                if (list != null) {
                    result = this.addBlockList(result, list);
                }
            }
        }

        return result;
    }

    /**
     * COMMONブロックを検索する：Fortran用
     * @param var            変数宣言
     * @return            結果のセット
     */
    private List<IBlock>  searchCOMMON(VariableDefinition variable) {
        if (variable == null) return null;
        if (variable.getMother() == null) return null;
        if (!(variable.getMother() instanceof ProgramUnit)) return null;
        String varName = variable.get_name();
        ProgramUnit search_block = (ProgramUnit)variable.getMother();
        if (search_block == null) return null;
        String proc_name = search_block.get_name();

        List<IBlock> result = new ArrayList<IBlock>();
        List<Common> commons = search_block.getCommonList();
        for (Common cm : commons) {
            if (cm.contains(varName)) {
                List<ProgramUnit> pus = this.fortranDb.getCommonUnit(cm.getName());
                for (ProgramUnit comm : pus) {
                    result.add(comm);
                    List<IBlock> list = this.searchChildrenProcedure(comm);
                    if (list != null) {
                        result = this.addBlockList(result, list);
                    }
                }
            }
        }

        if (result.size() <= 0) return null;

        return result;
    }

    /**
     * USE(Fortran), include(C言語)ブロックを検索する
     * @param variable          変数宣言
     * @return            結果のセット
     */
    private List<IBlock> searchUSE(VariableDefinition variable, IBlock search_block) {
        if (variable == null) return null;
        if (variable.getMother() == null) return null;
        VariableAttribute att = (VariableAttribute) variable.getAttribute();
        if (att != null && att.getScope() == ScopeAttribute.PRIVATE) {
            // private変数は対象外
            return null;
        }
        // Module, ヘッダーファイルのグローバル変数のみが対象
        if (!(variable.getMother() instanceof ProgramUnit)) return null;
        if (search_block != null
            && !(search_block instanceof ProgramUnit)) return null;
        String varName = variable.get_name();
        String mod_name = null;
        IDeclarations dec_block = variable.getScopeDeclarationsBlock();
        if (dec_block != null && dec_block instanceof ProgramUnit) {
            mod_name = ((ProgramUnit)dec_block).get_name();
        }

        // 検索対象ブロック
        List<IBlock> search_blocks = new ArrayList<IBlock>();
        if (search_block != null) {
            search_blocks.add(search_block);
        }
        else {
            // すべてのモジュールから参照しているモジュールを検索する
            for(Module mod : this.fortranDb.getModules().values()){
                search_blocks.add(mod);
            }
        }
        if (search_blocks.size() <= 0) return null;

        List<IBlock> result = new ArrayList<IBlock>();
        for (IBlock block : search_blocks) {
            if (!(block instanceof ProgramUnit)) continue;
            // moduleをチェック
            boolean flag = this.isUseModule(variable, block);
            if (flag == true) {
                // モジュールから探索
                List<IBlock> list = this.analysisScope(variable, block);
                if (list != null) {
                    result = this.addBlockList(result, list);
                }

                // Set<IBlock> list = this.searchChildrenProcedure(block);
                // if (list != null) {
                //    result.addAll(list);
                // }
                continue;
            }

            // module副プログラムを個別に探索
            for (IBlock child : block.getChildren()) {
                if (!(child instanceof Procedure)) continue;
                List<IBlock> use_blocks = this.searchUSE(variable, (ProgramUnit)child);
                if (use_blocks != null) {
                    // 副プログラムから探索
                    for (IBlock use_block : use_blocks) {
                        List<IBlock> list = this.analysisScope(variable, use_block);
                        if (list != null) {
                            result = this.addBlockList(result, list);
                        }
                    }
                }
            }
        }

        if (result.size() <= 0) return null;

        return result;
    }

    /**
     * 子孫副プログラムをすべて取得する
     * @param block        検索ブロック
     * @return        子孫副プログラム
     */
    private List<IBlock> searchChildrenProcedure(IBlock block) {
        if (block == null) return null;
        if (block.getChildren() == null) return null;

        List<IBlock> result = new ArrayList<IBlock>();
        // 子副プログラムをすべて追加
        for (IBlock child : block.getChildren()) {
            if (!(child instanceof Procedure)) continue;
            List<IBlock> list = this.searchChildrenProcedure(child);
            if (list != null) {
                result.addAll(list);
            }
        }
        if (result.size() <= 0) return null;

        return result;
    }

    /**
     * 元ブロックから除外ブロックを削除したブロックを取得する。
     * @param src_block            元ブロック
     * @param exclude_block        除外ブロック
     * @return            マージBlockListクラス
     */
    private IBlock margeBlocks(IBlock src_block, IBlock exclude_block) {
        if (src_block == null) return null;
        if (src_block.getStartCodeLine() == null) return null;
        if (exclude_block == null) return src_block;
        if (exclude_block.getStartCodeLine() == null) return src_block;

        if (src_block instanceof BlockList) {
            List<IBlock> marge_list = new ArrayList<IBlock>();
            List<IBlock> list = ((BlockList) src_block).getBlocks();
            for (IBlock item : list) {
                IBlock block = margeBlocks(item, exclude_block);
                if (block == null) continue;
                if (block instanceof BlockList && ((BlockList) block).getBlockCount() > 0) {
                    marge_list.addAll(((BlockList) block).getBlocks());
                }
                else {
                    marge_list.add(block);
                }
            }
            if (marge_list.size() <= 0) {
                return null;
            }
            BlockList block_list = new BlockList();
            block_list.setBlocks(marge_list);
            return block_list;
        }
        else {
            int src_start_lineno = src_block.getStartCodeLine().getStartLine();
            int src_end_lineno = this.getEndLineno(src_block);
            int dest_start_lineno = exclude_block.getStartCodeLine().getStartLine();
            int dest_end_lineno = this.getEndLineno(exclude_block);

            // 重複があるかチェックする
            if (src_end_lineno <= dest_start_lineno) {
                return src_block;
            }
            if (src_start_lineno >= dest_end_lineno) {
                return src_block;
            }
            if (src_start_lineno >= dest_start_lineno
                && src_end_lineno <= dest_end_lineno) {
                return null;
            }
            int marge_start_lineno0 = src_start_lineno;
            int marge_end_lineno0 = src_end_lineno;
            int marge_start_lineno1 = -1;
            int marge_end_lineno1 = -1;
            if (src_start_lineno > dest_start_lineno) {
                marge_start_lineno0 = src_start_lineno;
                marge_end_lineno0 = dest_end_lineno;
            }
            else {
                marge_start_lineno0 = src_start_lineno;
                marge_end_lineno0 = dest_start_lineno;
                if (src_end_lineno > dest_end_lineno) {
                    marge_start_lineno1 = dest_end_lineno;
                    marge_end_lineno1 = src_end_lineno;
                }
            }
            marge_start_lineno0 = this.getBlockLineno(src_block, marge_start_lineno0, 0);
            marge_end_lineno0 = this.getBlockLineno(src_block, marge_end_lineno0, -1);
            marge_start_lineno1 = this.getBlockLineno(src_block, marge_start_lineno1, +1);
            marge_end_lineno1 = this.getBlockLineno(src_block, marge_end_lineno1, 0);

            BlockList block_list = new BlockList();
            if (marge_start_lineno0 > 0 && marge_end_lineno0 > 0
                && marge_end_lineno0-marge_start_lineno0 >= 0) {
                CodeLine marge_start_line = new CodeLine(src_block.getStartCodeLine());
                marge_start_line.setLine(marge_start_lineno0);
                marge_start_line.setEndLine(marge_end_lineno0);
                BlockData block = new BlockData(marge_start_line, src_block);
                block_list.addBlock(block);
            }

            if (marge_start_lineno1 > 0 && marge_end_lineno1 > 0
                && marge_end_lineno1-marge_start_lineno1 >= 0) {
                CodeLine marge_start_line = new CodeLine(src_block.getStartCodeLine());
                marge_start_line.setLine(marge_start_lineno1);
                marge_start_line.setEndLine(marge_end_lineno1);
                BlockData block = new BlockData(marge_start_line, src_block);
                block_list.addBlock(block);
            }
            if (block_list.getBlockCount() <= 0) return null;

            return block_list;
        }
    }

    /**
     * ブロックの最終行番号を取得する
     * @param block        ブロック
     * @return        ブロック最終行番号
     */
    private int getEndLineno(IBlock block) {
        if (block == null) return -1;
        if (block.getStartCodeLine() == null) return -1;
        int start_lineno = block.getStartCodeLine().getStartLine();
        int end_lineno = block.getStartCodeLine().getEndLine();
        if (block.getEndCodeLine() != null) {
            int lineno = block.getEndCodeLine().getEndLine();
            if (end_lineno < lineno) {
                end_lineno = lineno;
            }
            lineno = block.getEndCodeLine().getStartLine();
            if (end_lineno < lineno) {
                end_lineno = lineno;
            }
        }
        if (end_lineno < start_lineno) {
            end_lineno = start_lineno;
        }
        return end_lineno;
    }


    /**
     * ブロック中の行番号を取得する
     * @param block        ブロック
     * @param line        行番号
     * @param offset_lineno       -1 = 行番号未満に存在するブロックを取得する
     *                            0  = 行番号以上に存在するブロックを取得する
     *                            +1  = 行番号より大きい行に存在するブロックを取得する
     * @return        ブロック行番号, ブロック中に行番号が存在しない場合、-1を返す。
     */
    private int getBlockLineno(IBlock block, int lineno, int offset_lineno) {
        if (block == null) return -1;
        if (lineno <= 0) return -1;
        if (block.getStartCodeLine() == null) return -1;
        int start_lineno = block.getStartCodeLine().getStartLine();
        int end_lineno = this.getEndLineno(block);
        if (start_lineno > lineno) return -1;
        if (offset_lineno == 0) {
            if (start_lineno == lineno) return lineno;
            if (end_lineno == lineno) return lineno;
        }

        List<IBlock> list = new ArrayList<IBlock>();

        if (block.getBlockType() == BlockType.PROCEDURE) {
            list.add(block);        // 自身ブロックを追加する。
        }
        list.addAll(block.getChildren());
        Procedure proc = null;
        if (block instanceof BlockData
            && ((BlockData)block).getCurrentBlock() instanceof Procedure) {
            proc = (Procedure)((BlockData)block).getCurrentBlock();
        }
        else if (block instanceof Procedure) {
            proc = (Procedure)block;
        }

        int prev_lineno = -1;
        int next_lineno = end_lineno;
        int block_lineno = -1;
        if (list != null) {
            for (IBlock child : list) {
                if (child == null) continue;
                if (child.getStartCodeLine() == null) continue;
                if (proc != null && child instanceof VariableDefinition) {
                    // 引数は行取得対象外
                    if (proc.isArgumentVariableDefinition((VariableDefinition)child)) {
                        continue;
                    }
                }
                block_lineno = child.getStartCodeLine().getStartLine();
                if (offset_lineno <= 0) {
                    if (block_lineno >= lineno) {
                        if (child.getBlockType() == BlockType.PROCEDURE) {
                            // 関数より前行を取得ことはない
                            return block_lineno + offset_lineno;
                        }
                        continue;
                    }
                    else if (prev_lineno == -1 || prev_lineno < block_lineno) prev_lineno = block_lineno;
                }
                if (offset_lineno > 0) {
                    if (block_lineno > lineno) {
                        if (next_lineno > block_lineno) next_lineno = block_lineno;
                        continue;
                    }
                }
                if (child != block) {
                    block_lineno = this.getBlockLineno(child, lineno, offset_lineno);
                }
                if (offset_lineno <= 0) {
                    if (block_lineno >= lineno) continue;
                }
                if (offset_lineno > 0) {
                    if (block_lineno > lineno) {
                        if (next_lineno > block_lineno) next_lineno = block_lineno;
                        continue;
                    }
                }
                if (block_lineno > 0) {
                    if (prev_lineno == -1 || prev_lineno < block_lineno) prev_lineno = block_lineno;
                }
            }
        }
        if (block instanceof Procedure) {
            return this.getBlockLineno(((Procedure)block).getBody(), lineno, offset_lineno);
        }
        if (offset_lineno == 0) {
            if (block_lineno >= lineno) return block_lineno;
        }
        if (offset_lineno > 0) {
            if (next_lineno > lineno) return next_lineno;
        }
/*
        if (prev_lineno <= 0) {
            if (block.getMotherBlock() != null
                && block.getMotherBlock().getBlockType() == BlockType.PROCEDURE) {
                if (offset_lineno >= 0) {
                    // 親関数の修了コードを返す
                    return block.getMotherBlock().getEndCodeLine().getEndLine();
                }
                else {
                    // 親関数の開始コードを返す。
                    return block.getMotherBlock().getStartCodeLine().getStartLine();
                }
            }
        }
*/
        return prev_lineno;
    }

    /**
     * 元ブロックリストに追加ブロックリストを追加する。
     * 同一判断はtoStringModuleScopeの文字列比較にて行う。
     * @param src_list        元ブロックリスト
     * @param add_list        追加ブロックリスト
     * @return                追加結果ブロックリスト
     */
    private List<IBlock> addBlockList(List<IBlock> src_list, List<IBlock> add_list) {
        if (add_list == null) return src_list;

        for (IBlock add_item : add_list) {
            src_list = this.addBlock(src_list, add_item);
        }

        return src_list;
    }

    /**
     * 元ブロックリストに追加ブロックを追加する。
     * 同一判断はtoStringModuleScopeの文字列比較にて行う。
     * @param src_list        元ブロックリスト
     * @param add_block        追加ブロック
     * @return                追加結果ブロックリスト
     */
    private List<IBlock> addBlock(List<IBlock> src_list, IBlock add_block) {
        if (add_block == null) return src_list;
        List<IBlock> result = new ArrayList<IBlock>();
        if (src_list == null) {
            result.add(add_block);
            return result;
        }

        boolean exists = false;
        for (IBlock src_item : src_list) {
            String src_text = src_item.toStringModuleScope();
            String add_text = add_block.toStringModuleScope();
            if (src_text.equals(add_text)) {
                exists = true;
                break;
            }
        }
        if (!exists) {
            src_list.add(add_block);
        }

        return src_list;
    }

    /**
     * 関数に同名変数が使用されているかチェックする。
     * Fortranは対象外とする
     * @param block        検索ブロック
     * @param variable        検索変数定義
     * @param dup_def        同名変数定義
     * @return            true=使用している
     */
    private boolean isActualBlock(IBlock block,
                VariableDefinition variable,
                VariableDefinition dup_def) {
        if (block == null) return false;
        if (block.getStartCodeLine() == null) return false;
        if (variable == null) return false;
        if (variable.getStartCodeLine() == null) return false;

        // Fortranは対象外とする
        if (block.isFortran()) return true;

        // 関数のみ
        if (!(block instanceof IDeclarations)) return true;
        IDeclarations proc = (IDeclarations)block;
        CodeLine proc_line = block.getStartCodeLine();
        SourceFile proc_file = proc_line.getSourceFile();

        String var_name = variable.get_name();
        boolean found_line = false;
        Set<IBlock> block_list = new HashSet<IBlock>();
        Set<IBlock> ref_blocks = proc.getRefVariableName(var_name);
        Set<IBlock> def_blocks = proc.getDefVariableName(var_name);
        if (ref_blocks != null) {
            block_list.addAll(ref_blocks);
        }
        if (def_blocks != null) {
            block_list.addAll(def_blocks);
        }

        Set<IBlock> var_list = new HashSet<IBlock>();
        for (IBlock line_block : block_list) {
            Set<Variable> list = line_block.getAllVariables();
            for (Variable var : list) {
                if (var.getDefinition() == variable) {
                    var_list.add(line_block);
                    break;
                }
            }
        }

        if (dup_def == null || dup_def.getStartCodeLine() == null) {
            return (var_list.size() > 0);
        }

        CodeLine def_line = dup_def.getStartCodeLine();
        SourceFile def_file = def_line.getSourceFile();
        if (var_list.size() > 0) {
            if (!def_file.equals(proc_file)) return true;
        }

        for (IBlock line_block : var_list) {
            CodeLine line = line_block.getStartCodeLine();
            if (line == null) continue;
            if (def_line.compareTo(line) > 0) {
                return true;
            }
        }

        return false;
    }

    /**
     * 変数は使用可能なモジュールであるかチェックする
     * @param variable        変数宣言文
     * @param block            モジュール・プロシージャブロック
     * @return            use文による変数使用可能ブロック
     */
    private boolean isUseModule(VariableDefinition variable, IBlock block) {
        if (variable == null) return false;
        if (block == null) return false;
        if (!(block instanceof ProgramUnit)) return false;

        String varName = variable.get_name();
        String mod_name = null;
        IDeclarations dec_block = variable.getScopeDeclarationsBlock();
        if (dec_block != null && dec_block instanceof ProgramUnit) {
            mod_name = ((ProgramUnit)dec_block).get_name();
        }

        List<UseState> ul = ((ProgramUnit)block).getUseList();
        if (ul == null) return false;
        for (UseState us : ul) {
            if (us.equalsModuleName(mod_name)) {
                if (us.hasOnlyMember()) {
                    if (us.containsMember(varName)) {
                        return true;
                    }
                }
                else {
                    return true;
                }
            }
            else {
                Module mod = this.fortranDb.module(us.getModuleName());
                if (mod != null) {
                    if (this.isUseModule(variable, mod)) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    /**
     * 変数有効域の探索方法を取得する
     * true = 変数の使用域の探索
     * false = 変数の言語使用有効域の探索
     */
    public boolean isScopeActual() {
        return this.scope_actual;
    }

    /**
     * 変数有効域の探索方法を設定する
     * true = 変数の使用域の探索
     * false = 変数の言語使用有効域の探索
     */
    public void setScopeActual(boolean scope_actual) {
        this.scope_actual = scope_actual;
    }

    /**
     * グローバル変数であるかチェックする
     * @param variable        変数定義
     * @return        true=グローバル定義
     */
    private boolean isGlobalVariable(VariableDefinition variable) {
        if (variable == null) return false;
        IDeclarations block = variable.getScopeDeclarationsBlock();
        if (((IBlock)block).getBlockType() == BlockType.MODULE) {
            return true;
        }

        return false;
    }

    /**
     * 関数引数の変数であるかチェックする
     * @param variable        変数定義
     * @return        true=関数引数の変数定義
     */
    private boolean isArgumentVariable(VariableDefinition variable) {
        if (variable == null) return false;
        if (variable.getMother() == null) return false;
        if (!(variable.getMother() instanceof Procedure)) return false;

        Procedure proc = (Procedure)variable.getMother();
        Variable arg_vars[] = proc.get_args();
        if (arg_vars == null) return false;
        for (Variable arg : arg_vars) {
            VariableDefinition arg_def = arg.getDefinition();
            if (arg_def == variable) {
                return true;
            }
        }

        return false;
    }
}
