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
import java.util.List;
import java.util.Set;

import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.Common;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Module;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.UseState;
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
     * コンストラクタ
     *
     * @param fortran
     *            フォートランデータベース
     */
    public AnalysisScopeService(Fortran fortran) {
        super(fortran);
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
        this.modelScope.setTitle(variable.toString());
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

        List<IBlock> result = new ArrayList<IBlock>();
        boolean has_searchblock = (search_block != null);
        String varName = variable.get_name();
        if (search_block == null) {
            search_block = variable.getScopeDeclarationsBlock();
            // 自身の宣言モジュール・手続を追加する
            result = this.addBlock(result, search_block);
        }
        if (search_block == null) return null;

        if (search_block instanceof Module) {
            if (((Module)search_block).get_procedures() != null) {
                Procedure[] procs = ((Module)search_block).get_procedures();
                for (Procedure block : procs) {
                    List<IBlock> list = this.analysisScope(variable, block);
                    if (list != null) {
                        result = this.addBlockList(result, list);
                    }
                }
            }
            // モジュール自身が有効であるか
            VariableDefinition dup_def = ((Module)search_block).get_variable(varName);
            if (dup_def == null) {
                if (((Module)search_block).getVariableDefinitionMap() != null
                    && ((Module)search_block).getVariableDefinitionMap().size() > 0) {
                    result = this.addBlock(result, search_block);
                }
            }
        }

        else if (search_block instanceof ProgramUnit) {
            ProgramUnit proc = (ProgramUnit)search_block;
            VariableDefinition dup_def = proc.get_variable(varName);
            if (dup_def == null || dup_def == variable) {
                // 副プログラムを追加する
                result = this.addBlock(result, search_block);

                // 子宣言ブロックを取得する
                List<IBlock> child_decs = ((ProgramUnit)search_block).getChildren();
                if (child_decs != null) {
                    for (IBlock child : child_decs) {
                        if (!(child instanceof ProgramUnit)) continue;
                        proc = (ProgramUnit)child;
                        // 副プログラムに同一の名前の宣言が無ければ追加：Procedureのみ
                        dup_def = proc.get_variable(varName);
                        if (dup_def == variable) continue;     // 同一変数
                        if (dup_def != null) continue;

                        // 副プログラムを追加する
                        result = this.addBlock(result, child);

                        // 内部副プログラムから検索する
                        if (child != search_block) {
                            List<IBlock> child_list = this.analysisScope(variable, (IBlock)child);
                            if (child_list != null) {
                                result = this.addBlockList(result, child_list);
                            }
                        }
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
                    Set<ProgramUnit> list = comm.getDeclarationsBlocks();
                    if (list != null) {
                        for (ProgramUnit item : list) {
                            result = this.addBlock(result, item);
                        }
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
        String mod_name = ((ProgramUnit)variable.getMother()).get_name();

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
        ProgramUnit dec_block = variable.getScopeDeclarationsBlock();
        if (dec_block != null) {
            mod_name = dec_block.get_name();
        }

        List<UseState> ul = ((ProgramUnit)block).getUseList();
        if (ul == null) return false;
        for (UseState us : ul) {
            String us_name = us.getModuleName();
            if (us_name == null) continue;
            if (us_name.equalsIgnoreCase(mod_name)) {
                if (us.hasOnlyMember()) {
                    if (us.containsMember(varName)) {
                        return true;
                    }
                }
                // rename対応 at 2016/03/28 by @hira
                else if (us.hasTranslation(varName)) {
                    String rename = us.translation(variable);
                    if (rename != null) {
                        return true;
                    }
                }
                else {
                    return true;
                }
            }
            else {
                Module mod = this.fortranDb.module(us_name);
                if (mod != null) {
                    if (this.isUseModule(variable, mod)) {
                        return true;
                    }
                }
            }
        }

        return false;
    }
}
