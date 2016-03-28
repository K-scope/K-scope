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

package jp.riken.kscope.language.utils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.exception.ParseException;
import jp.riken.kscope.language.ExecutableBody;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.parser.LineSpliter;
import jp.riken.kscope.utils.LanguageTokenizer;

/**
 * データベースユーティリティクラス.
 * データベースからブロックの検索、取得等を行う
 * @author RIKEN
 */
public class LanguageUtils {
    /** フォートランデータベース */
    private Fortran fortranDb;

    /**
     * コンストラクタ
     */
    public LanguageUtils() {
    }

    /**
     * コンストラクタ
     * @param db        データベース
     */
    public LanguageUtils(Fortran db) {
        this.fortranDb = db;
    }


    /**
     * コード行情報から、それが属するプログラム単位を探索し返す。
     *
     * @param line
     *            コード行情報
     * @return プログラム単位。無ければnullを返す。
     */
    public ProgramUnit getCurrentProgramUnit(CodeLine line) {
        if (line == null) return null;
        if (this.fortranDb == null) return null;

        SourceFile file = line.getSourceFile();
        int lineNo = line.getStartLine();
        // fileにあるプログラム単位のリストを取得
        List<ProgramUnit> pus = this.fortranDb.getProgramUnits(file);
        if (pus == null) return null;
        // lineNoを含むプログラム単位を習得
        ProgramUnit punit = null;
        for (ProgramUnit pu : pus) {
            int sPos = pu.getStartPos();
            int ePos = pu.getEndPos();
            if (sPos <= lineNo && lineNo <= ePos) {
                punit = pu;
                List<IBlock> blocks = pu.getChildren();
                for (IBlock child : blocks) {
                    if (!(child instanceof ProgramUnit)) continue;
                    sPos = ((ProgramUnit)child).getStartPos();
                    ePos = ((ProgramUnit)child).getEndPos();
                    if (sPos <= lineNo && lineNo <= ePos) {
                        punit = (ProgramUnit)child;
                        List<IBlock> blocks2 = child.getChildren();
                        for (IBlock child2 : blocks2) {
                            if (!(child2 instanceof ProgramUnit)) continue;
                            sPos = ((ProgramUnit)child2).getStartPos();
                            ePos = ((ProgramUnit)child2).getEndPos();
                            if (sPos <= lineNo && lineNo <= ePos) {
                                punit = (Procedure)child2;
                            }
                        }
                    }
                }
            }
        }
        return punit;
    }

    /**
     * 行番号のブロックリストを検索する
     * @param line            行番号
     * @return        行番号のブロックリスト
     */
    public IBlock[] getCodeLineBlocks(CodeLine line) {
        if (line == null) return null;
        CodeLine[] lines = {line};
        return getCodeLineBlocks(lines);
    }


    /**
     * 行番号のブロックリストを検索する
     * @param lines            行番号リスト
     * @return        行番号のブロックリスト
     */
    public IBlock[] getCodeLineBlocks(CodeLine[] lines) {
        if (lines == null) return null;

        List<IBlock> list = new ArrayList<IBlock>();
        for (CodeLine line : lines) {
            ProgramUnit unit = getCurrentProgramUnit(line);
            if (unit == null) continue;
            IBlock[] blocks = unit.searchCodeLine(line);
            if (blocks != null) {
                list.addAll(Arrays.asList(blocks));
            }
        }

        if (list.size() <= 0) return null;
        return list.toArray(new IBlock[0]);
    }


    /**
     * データベース構造階層を取得する.
     * 階層リストは子から親のリストである.
     * @param block        データベース構成ブロック
     * @return            データベース構造階層リスト
     */
    public List<Object> getLanguagePath(Object block) {
        if (block == null) return null;

        // 選択ノードの階層を取得する
        List<Object> parents = new ArrayList<Object>();
        Object child = block;
        while (child != null) {
            if (child instanceof ExecutableBody) {
                parents.add(child);
                child = ((ExecutableBody)child).getParent();
            }
            else if (child instanceof Procedure) {
                if (parents.size() > 0) {
                    if (parents.get(parents.size()-1) != child) {
                        parents.add(child);
                    }
                }
                // 呼出元CALL文リスト
                Set<ProcedureUsage> calls = ((Procedure)child).getCallMember();
                if (calls != null && calls.size() > 0) {
                    ProcedureUsage[] array = calls.toArray(new ProcedureUsage[0]);
                    // 親の階層の深いものを選択する。
                    List<Object> listMax = null;
                    for (ProcedureUsage useage : array) {
                        List<Object> listPath = getLanguagePath(useage);
                        if (listPath == null || listPath.size() <= 0) continue;
                        // 再帰呼出となっていないかチェックする.
                        if (isRecursive(parents, listPath)) continue;
                        // program文に達しているか
                        if (listPath.get(listPath.size()-1) instanceof Procedure) {
                            if (((Procedure)listPath.get(listPath.size()-1)).isProgram()) {
                                listMax = listPath;
                                break;
                            }
                        }
                        if (listMax == null) listMax = listPath;
                        else if (listMax.size() < listPath.size()) listMax = listPath;
                    }
                    if (listMax != null ) {
                        child = listMax.get(listMax.size()-1);
                        parents.addAll(listMax);
                    }
                    else {
                        child = null;
                    }
                }
                else {
                    child = null;
                }
            }
            else if (child instanceof IBlock) {
                parents.add(child);
                child = ((IBlock)child).getMotherBlock();
            }
            else {
                child = null;
            }
        }
        if (parents.size() <= 0) return null;

        return parents;
    }

    /**
     * 再帰呼出となっていないかチェックする.
     * @param parents        元リスト
     * @param childrens        追加リスト
     * @return        true=再帰呼出
     */
    private boolean isRecursive(List<Object> parents, List<Object> childrens) {
        if (parents == null || parents.size() <= 0) return false;
        if (childrens == null || childrens.size() <= 0) return false;
        for (Object children : childrens) {
            if (children instanceof Procedure) {
                for (Object parent : parents) {
                    if (parent instanceof Procedure) {
                        if (children == parent) {
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    /**
     * 変数、構造体変数名をメンバー変数に分割する。
     * 配列式は取得しない。
     * student.no  = {student, no}
     * student[a+1].no  = {student, no}
     * @param name        変数名
     * @return            分割変数リスト
     */
    public static String[] splitVariableNames(String name) {
        if (name == null || name.isEmpty()) return null;

        List<String> list = new ArrayList<String>();
        try {
            LanguageTokenizer spliter = new LanguageTokenizer(name);

            // C言語構造体メンバ参照
            spliter.useDelimiter(".");
            spliter.useDelimiter("->");
            // Fortran構造体メンバ参照
            spliter.useDelimiter("%");
            // C言語配列表現
            spliter.useDelimiter("[");
            spliter.useDelimiter("]");
            // Fortran配列表現
            spliter.useDelimiter("(");
            spliter.useDelimiter(")");
            // C言語アドレス演算子
            spliter.useDelimiter("&");
            spliter.eolIsSignificant(true);

            int ttype;
            int quote = 0;
            while ((ttype = spliter.nextToken()) != LanguageTokenizer.LT_EOF) {
                if (ttype == LanguageTokenizer.LT_EOL
                        || ttype == LanguageTokenizer.LT_EOF)
                    break;
                switch (ttype) {
                case LanguageTokenizer.LT_WORD:
                    if (quote == 0) {
                        list.add(spliter.sval);
                    }
                    break;
                case LanguageTokenizer.LT_DELIM:
                    if (spliter.sval == "[" || spliter.sval == "(") {
                        quote++;
                    }
                    if (spliter.sval == "]" || spliter.sval == ")") {
                        quote--;
                    }

                    break;
                default:
                    break;
                }
            }

        } catch (IOException ex) {
            return null;
        }
        if (list.size() <= 0) {
            return null;
        }

        return list.toArray(new String[0]);
    }

    /**
     * 変数、構造体変数名をメンバー変数に分割し、構造体メンバ表記を取得する。
     * 配列式は分割しない。
     * student.no  = {student, student.no}
     * student[a+1].no  = {student, student.no}
     * @param name        変数名
     * @return            分割構造体メンバリスト
     */
    public static String[] splitVariableMembers(String name) {
        // 変数、構造体変数名をメンバー変数に分割する.
        String list[] = splitVariableNames(name);
        if (list == null || list.length <= 0) return null;

        List<String> var_list = new ArrayList<String>();
        String buf = "";
        for (String var : list) {
            if (!buf.isEmpty()) buf += ".";
            buf += var;
            var_list.add(buf);
        }

        return var_list.toArray(new String[0]);
    }

    /**
     * 変数、構造体変数名を正規化する。
     * 配列式、ポインター参照を削除する。
     * student->no  = student.no
     * student[a+1].no  = student.no
     * @param name        変数名
     * @return            正規化変数名
     */
    public static String normalizeVariableName(String name) {
        if (name == null) return null;

        String[] list = splitVariableMembers(name);
        if (list == null || list.length <= 0) return null;

        // リストが正規化変数名
        return list[list.length-1];
    }

    /**
     *
     * @param list
     * @return
     */
    public static List<IBlock> sortBlock(List<IBlock> list) {

        Collections.sort(list, new Comparator<IBlock>(){
            public int compare(IBlock block1, IBlock block2) {
                if (block1 == null) return 0;
                if (block2 == null) return 0;
                CodeLine code1 = block1.getStartCodeLine();
                CodeLine code2 = block2.getStartCodeLine();
                if (code1 == null) return 0;
                if (code2 == null) return 0;

                return code1.compareTo(code2);
            }
        });

        return list;
    }


    /**
     * 変数の名前と宣言のマップから宣言文を検索する。
     * 構造体変数を考慮して、変数名を分解して先頭の変数名を取得し、検索する。
     * @param name        変数名
     * @param var_map        宣言のマップ
     * @return            宣言文
     */
    public static VariableDefinition[] searchVariableDefinitionFromMap(
                        String name,
                        HashMap<Variable, VariableDefinition> var_map) {
        if (name == null || name.isEmpty()) return null;
        if (var_map == null || var_map.size() <= 0) return null;

        // 構造体変数を考慮して変数名を分解する
        String[] list = LanguageUtils.splitVariableNames(name);
        if (list == null || list.length <= 0) {
            return null;
        }

        // 先頭の変数にて宣言を検索する:構造体宣言のみ、構造体メンバはvar_mapにはない。
        String search_name = list[0];
        List<VariableDefinition> defs = new ArrayList<VariableDefinition>();
        for (Variable var : var_map.keySet()) {
            if (var == null) continue;
            if (var.getName() == null) continue;
            VariableDefinition def = var_map.get(var);
            if (def == null) continue;
            String var_name = def.transferDeclarationName(var.getName());
            list = LanguageUtils.splitVariableNames(var_name);
            if (list == null || list.length <= 0) continue;
            var_name = list[0];
            if (search_name.equals(var_name)) {
                defs.add(def);
            }
        }
        if (defs.size() <= 0) return null;

        return defs.toArray(new VariableDefinition[0]);
    }


    /**
     * 変数の名前と宣言のマップから宣言文を検索する。
     * 構造体変数を考慮して、変数名を分解して先頭の変数名を取得し、検索する。
     * @param name        変数名
     * @param var_map        宣言のマップ
     * @return            宣言文
     */
    public static VariableDefinition[] searchVariablesDefinition(
                        String name,
                        Map<String, VariableDefinition> var_map) {
        if (name == null || name.isEmpty()) return null;
        if (var_map == null || var_map.size() <= 0) return null;

        // 直接探索する
        List<VariableDefinition> defs = new ArrayList<VariableDefinition>();
        VariableDefinition def = var_map.get(name);
        if (def != null) {
            defs.add(def);
            return defs.toArray(new VariableDefinition[0]);
        }

        // 構造体変数を考慮して変数名を分解する
        String[] mem_list = LanguageUtils.splitVariableNames(name);
        if (mem_list == null || mem_list.length <= 0) {
            return null;
        }

        // 先頭の変数にて宣言を検索する:構造体宣言のみ、構造体メンバはvar_mapにはない。
        String search_name = mem_list[0];
        for (String var_name : var_map.keySet()) {
            if (var_name == null) continue;
            def = var_map.get(var_name);
            if (def == null) continue;
            var_name = def.transferDeclarationName(var_name);
            String[] list = LanguageUtils.splitVariableNames(var_name);
            if (list == null || list.length <= 0) continue;
            var_name = list[0];
            if (search_name.equals(var_name)) {
                def = def.getStructMember(mem_list);
                if (def != null && !defs.contains(def)) {
                    defs.add(def);
                }
            }
        }
        if (defs.size() <= 0) return null;

        return defs.toArray(new VariableDefinition[0]);
    }

    /**
     * 変数の名前と宣言のマップから宣言文を検索する。
     * 構造体変数を考慮して、変数名を分解して先頭の変数名を取得し、検索する。
     * @param name        変数名
     * @param var_map        宣言のマップ
     * @return            変数
     */
    public static Variable[] searchVariableFromMap(
                        String name,
                        HashMap<Variable, VariableDefinition> var_map) {
        if (name == null || name.isEmpty()) return null;
        if (var_map == null || var_map.size() <= 0) return null;

        // 構造体変数を考慮して変数名を分解する
        String[] list = LanguageUtils.splitVariableNames(name);
        if (list == null || list.length <= 0) {
            return null;
        }

        // 先頭の変数にて宣言を検索する:構造体宣言のみ、構造体メンバはvar_mapにはない。
        String search_name = list[0];
        List<Variable> vars = new ArrayList<Variable>();
        for (Variable var : var_map.keySet()) {
            if (var == null) continue;
            if (var.getName() == null) continue;
            VariableDefinition def = var_map.get(var);
            if (def == null) continue;
            String var_name = def.transferDeclarationName(var.getName());
            list = LanguageUtils.splitVariableNames(var_name);
            if (list == null || list.length <= 0) continue;
            var_name = list[0];
            if (search_name.equals(var_name)) {
                vars.add(var);
            }
        }
        if (vars.size() <= 0) return null;

        return vars.toArray(new Variable[0]);
    }

    /**
     * 親ブロックの子ブロックであるかチェックする。
     * 同一ブロックである場合は、trueを返す。
     * @param parent_block    親ブロック
     * @param child_block     子ブロック
     * @return        true=親ブロックの子ブロック、又は同一ブロック
     */
    public static boolean isParentBlock(IBlock parent_block, IBlock child_block) {
        if (parent_block == null) return false;
        if (child_block == null) return false;

        IBlock current_block = child_block;
        while (current_block != null) {
            if (parent_block == current_block) return true;
            current_block = current_block.getMotherBlock();
        }

        return false;
    }
}
