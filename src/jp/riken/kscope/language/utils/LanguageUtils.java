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
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.ExecutableBody;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.ProgramUnit;

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
     * @param db		データベース
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

	/**
	 * 行番号のブロックリストを検索する
	 * @param line			行番号
	 * @return		行番号のブロックリスト
	 */
    public IBlock[] getCodeLineBlocks(CodeLine line) {
    	if (line == null) return null;
    	CodeLine[] lines = {line};
		return getCodeLineBlocks(lines);
    }


	/**
	 * 行番号のブロックリストを検索する
	 * @param lines			行番号リスト
	 * @return		行番号のブロックリスト
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
     * @param block		データベース構成ブロック
     * @return			データベース構造階層リスト
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
     * @param parents		元リスト
     * @param childrens		追加リスト
     * @return		true=再帰呼出
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

}
