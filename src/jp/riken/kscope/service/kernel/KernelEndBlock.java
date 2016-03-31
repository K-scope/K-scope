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
package jp.riken.kscope.service.kernel;

import jp.riken.kscope.language.IBlock;

/**
 * カーネルブロックの修了ブロッククラス
 * @author RIKEN
 *
 */
public class KernelEndBlock extends  KernelBlock {

    /**
     * コンストラクタ
     * @param block    カーネル抽出ブロック
     */
    public KernelEndBlock(IBlock block) {
        super(block);
    }


    /**
     * カーネル出力コード
     */
    @Override
    public String toString() {
        IBlock end_block = this.getKernelBlock();
        if (end_block == null) return null;

        String code = end_block.toEndString();
        String indent_column = this.getCurrentIndent();
        code = indent_column + code;

        return code;
    }

}


