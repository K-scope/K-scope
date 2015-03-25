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

package jp.riken.kscope.language;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.information.InformationBlocks;

/**
 * 手続き呼び出しを表現するクラス。
 *
 * @author RIKEN 
 * @version    2015/03/15     関数名同一チェックメソッド追加
 *
 */
public class ProcedureUsage extends Block {
    /** シリアル番号 */
    private static final long serialVersionUID = 2385929813029019761L;
    /** サブルーチン、関数呼出名 */
    private String callName;
    /** サブルーチン、関数定義 */
    private transient Procedure callDefinition;
    /** 仮引数 */
    private List<Expression> arguments;
    /** 組込関数フラグ : true=組込関数 */
    private boolean intrinsic = false;
    /** 関数呼出フラグ : true=関数呼出 */
    private boolean isFunctionCall = false;

    /**
     *
     * コンストラクタ。
     *
     * @param mama
     *            親ブロック
     */
    ProcedureUsage(Block mama) {
        super(mama);
    }

    /**
     * コンストラクタ。
     *
     * @param mama
     *            親ブロック
     * @param subroutineName
     *            CALLサブルーチン名
     * @param argmnts
     *            引数リスト
     */
    public ProcedureUsage(Block mama, String subroutineName, List<Expression> argmnts) {
        super(mama);
        callName = subroutineName;
        arguments = argmnts;
    }

    /**
     * コンストラクタ。
     *
     * @param subroutineName
     *            CALLサブルーチン名
     * @param argmnts
     *            引数リスト
     */
    public ProcedureUsage(String subroutineName, List<Expression> argmnts) {
        super();
        callName = subroutineName;
        arguments = argmnts;
    }

    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.PROCEDUREUSAGE
     */
    @Override
    public BlockType getBlockType() {
        return BlockType.PROCEDUREUSAGE;
    }

    @Override
    public String toString() {
        String info = "";
        // delete by @hira at 2013/03/01
        // if (this.getInformation() != null) {
        //    if (!(this.getInformation().getContent().equals(""))) {
        //        info = "[ ! ] ";
        //    }
        // }
        return (info + this.toStringBase());
    }

    @Override
    protected String toStringBase() {
        StringBuilder myString = new StringBuilder();
        String call = "";
        if (this.isFortran()) {
            // Fortran表記
            call = "call ";
            if (this.isFunctionCall) {
                call = "function call ";
            }
        }

        myString.append(call + super.toString());
        /*
        myString.append(call + this.callName);
        if (this.arguments != null) {
            myString.append("(");
            for (Expression arg : arguments) {
                myString.append(arg.toString());
                myString.append(", ");
            }
            myString.replace(myString.length() - 2, myString.length(), ")");
        }
        */
        return myString.toString();
    }

    /**
     * 呼び出している手続きの宣言をセットする。
     * @param proc 手続きの宣言
     */
    public void setCallDefinition(Procedure proc) {
        callDefinition = proc;
        if (proc != null) {
            proc.addCallMember(this);
        }
    }

    /**
     * 組込み関数の呼出しであるとセットする。
     */
    public void setIntrinsic(){
        this.intrinsic = true;
    }

    /**
     * 呼び出し関数名の取得
     *
     * @return call_name 呼び出し関数名
     */
    public String getCallName() {
        return callName;
    }


    /**
     * 呼び出す手続の宣言を返す。
     *
     * @return 手続の宣言
     */
     public Procedure getCallDefinition() {
        return callDefinition;
    }

    /**
     * 実引数のリストを返す。無ければ空のリストを返す。
     *
     * @return 実引数のリスト
     */
    public List<Expression> getArguments() {
        if (this.arguments == null) {
            return new ArrayList<Expression>();
        }
        return arguments;
    }

    /**
     * 実引数のリストを設定する。
     * @param    args   実引数のリスト
     */
    public void setArguments(List<Expression> args) {
        this.arguments = args;
    }

    /**
     * 指定した名前の変数を実引数に含むか調べ、該当する添字の順番のsetを返す。
     *
     * @param varName
     *            変数の名前
     * @return 添字順番のset（値は0以上）。該当する変数を持たなければ空のsetを返す。
     */
    public Set<Integer> numberOfArg(String varName) {
        Set<Integer> nums = new HashSet<Integer>();
        List<Expression> args = this.getArguments();
        int i = 0;
        for (Expression arg : args) {
            if (arg.hasVariable(varName)) {
                nums.add(i);
            }
            i++;
        }
        return nums;
    }

    /**
     * 組込関数であるか取得する
     * @return		true=組込関数
     */
    public boolean isIntrinsic() {
        return intrinsic;
    }

    /**
     * 関数定義の文字列表現を取得する.
     * @return    関数定義の文字列表現
     */
    private String definitionToString() {
        if (this.callDefinition == null) {
            return "No Definition";
        } else {
            return this.callDefinition.toString();
        }
    }
    /**
     * 自身の宣言のHTML表現を返す。numArgに対応する引数をハイライトする。
     * @param numArg 引数の添字順番
     * @return 自身の宣言のHTML表現文字列。
     */
    public String toDefinitionHTMLString(int numArg) {
        StringBuilder html = new StringBuilder();
        html.append("<html>" + this.definitionToString());
        if (this.arguments != null) {
            html.append("(");
            int count = 0;
            for (Expression arg : arguments) {
                if (count == numArg) {
                    // TODO 本来は、本メソッドの引数としてハイライト対象のStringを受け取り、そこだけを赤くするような処理があったほうが良い
                    html.append("<span style='color:red;'>");
                    html.append(arg.toString());
                    html.append("</span>");
                } else {
                    html.append(arg.toString());
                }
                html.append(", ");
                count++;
            }
            html.replace(html.length() - 2, html.length(), ")");
        }
        html.append("</html>");
        return html.toString();
    }

    /**
     * 自身のHTML表現を返す。numArgに対応する引数をハイライトする。
     *
     * @param numArg
     *            引数の添字順番
     * @param name
     *            引数の名前
     * @return 自身のHTML表現文字列。
     */
    public String toHTMLString(int numArg, String name) {
        StringBuilder html = new StringBuilder();
        html.append("<html>" + "call " + this.callName);
        if (this.arguments != null) {
            html.append("(");
            int count = 0;
            for (Expression arg : arguments) {
                if (count == numArg) {
                    // TODO
                    // 本来は、本メソッドの引数としてハイライト対象のStringを受け取り、そこだけを赤くするような処理があったほうが良い
                    html.append("<span style='color:red;'>");
                    html.append(arg.toString());
                    html.append("</span>");
                } else {
                    html.append(arg.toString());
                }
                html.append(", ");
                count++;
            }
            html.replace(html.length() - 2, html.length(), ")");
        }
        html.append("</html>");
        return html.toString();
    }

    /**
     * 属するプログラム単位を返す。
     *
     * @return プログラム単位。得られなければnullを返す。
     */
    public Procedure getMyProcedure() {
        Block me = this;
        while (me != null) {
            me = me.get_mother();
            if (me instanceof ExecutableBody) {
                return ((ExecutableBody) me).getParent();
            }
        }
        return null;
    }


    /**
     * 指定された数字が指す順番の実引数に含まれる変数のセットを返す。
     *
     * @param numActualArg
     *            実引数の順番
     * @return 引数に含まれる変数名のセット。無ければ空のセットを返す。
     */
    public Set<String> getActualArgument(int numActualArg) {
        Set<String> varNames = new HashSet<String>();
        if (this.arguments != null) {
            Expression arg = this.arguments.get(numActualArg);
            if (arg != null) {
                Set<Variable> vars = arg.getAllVariables();
                for (Variable var: vars) {
                    varNames.add(var.getName());
                }
            }
        }
        return varNames;
    }


    /**
     * 指定された仮引数名とその順番に対応した実引数の順番を返す。
     *
     * @param dummyArg
     *            仮引数名
     * @param numDummyArg
     *            仮引数の順番
     * @return 実引数の順番。対応が見つからなければ-1を返す。
     */
    public int getNumOfActualArgument(String dummyArg, int numDummyArg) {
        if (this.arguments == null) {
            return -1;
        }
        int cnt = 0;
        for (Expression ex : this.arguments) {
            if (ex instanceof KeywordArgument) {
                KeywordArgument keywrd = (KeywordArgument) ex;
                if (keywrd.getKeyword().equalsIgnoreCase(dummyArg)) {
                    return cnt;
                }
            } else {
                if (cnt == numDummyArg) {
                    return cnt;
                }
            }
            cnt++;
        }
        return -1;
    }


    /**
     * 関数呼び出しであることを設定する。
     */
    public void setTypeIsFunction() {
        this.isFunctionCall = true;
    }

    /**
     * 付加情報ブロックコレクションを生成する。
     *
     * @return 付加情報ブロックコレクション
     */
    @Override
    public InformationBlocks createInformationBlocks() {
        InformationBlocks result = new InformationBlocks();
        result.addAll(super.createInformationBlocks());
        if (this.callDefinition != null) {
            // 自己参照の場合は外す
            if (!this.getNamespace().equals(this.callDefinition.getNamespace()))
            {
                result.addAll(this.callDefinition.createInformationBlocks());
            }
        }
        if (this.arguments != null) {
            for (Expression argument : this.arguments) {
                result.addAll(argument.createInformationBlocks());
            }
        }
        return result;
    }

    /**
     * idにマッチした情報ブロックを検索する。
     * @param id
     *          ID
     * @return 見つかった情報ブロック。見つからなかった場合はnullが返ります。
     */
    @Override
    public IInformation findInformationBlockBy(String id) {
        IInformation result = super.findInformationBlockBy(id);

        if (result == null && this.getID().equals(id)) {
            result = this;
        }

        if (result == null && this.callDefinition != null) {
            // 自己参照の場合は外す
            if (!this.getNamespace().equals(this.callDefinition.getNamespace()))
            {
                result = this.callDefinition.findInformationBlockBy(id);
            }
        }
        if (result == null && this.arguments != null) {
            for (Expression argument : this.arguments) {
                result = argument.findInformationBlockBy(id);
                if (result != null) { break; }
            }
        }

        return result;
    }


    /**
     * 同一ブロックであるかチェックする.
     * @param block		ブロック
     * @return		true=一致
     */
    @Override
    public boolean equalsBlocks(Block block) {
        if (block == null) return false;
        if (!(block instanceof ProcedureUsage)) return false;
        if (!super.equalsBlocks(block)) return false;

        String thisname = this.callName;
        String destname = ((ProcedureUsage)block).callName;
        if (!thisname.equalsIgnoreCase(destname)) {
            return false;
        }
        if (this.arguments != null && ((ProcedureUsage)block).arguments != null) {
            if (this.arguments.size() == ((ProcedureUsage)block).arguments.size()) {
                for (int i=0; i<this.arguments.size(); i++) {
                    Expression thisArg = this.arguments.get(i);
                    Expression destArg = ((ProcedureUsage)block).arguments.get(i);
                    if (thisArg == destArg) {
                        continue;
                    }
                    else if (thisArg == null) {
                        return false;
                    }
                    else if (!thisArg.equalsExpression(destArg)) {
                        return false;
                    }
                }
            }
        }
        else if (this.arguments != null || ((ProcedureUsage)block).arguments != null) {
            return false;
        }

        return true;
    }


    /**
     * 同一ブロックを検索する
     * @param block			IInformationブロック
     * @return		同一ブロック
     */
    @Override
    public IInformation[] searchInformationBlocks(IInformation block) {
        List<IInformation> list = new ArrayList<IInformation>();
        {
            IInformation[] infos = super.searchInformationBlocks(block);
            if (infos != null) {
                list.addAll(Arrays.asList(infos));
            }
        }

        if (this.callDefinition != null) {
            // 自己参照の場合は外す
            if (!this.getNamespace().equals(this.callDefinition.getNamespace())) {
                IInformation[] infos = this.callDefinition.searchInformationBlocks(block);
                if (infos != null) {
                    list.addAll(Arrays.asList(infos));
                }
            }
        }
        if (this.arguments != null) {
            for (Expression argument : this.arguments) {
                IInformation[] infos = argument.searchInformationBlocks(block);
                if (infos != null) {
                    list.addAll(Arrays.asList(infos));
                }
            }
        }

        if (list.size() <= 0) {
            return null;
        }

        return list.toArray(new IInformation[0]);
    }


    /**
     * プログラム名、モジュール名、サブルーチン名、関数名が同一であるかチェックする.
     * ファイルタイプから文字列に一致条件を変更する.
     *     C言語  : 大文字・小文字を区別する.
     *     Fortran : 大文字・小文字を区別しない.
     * @param value		チェックプログラム名、モジュール名、サブルーチン名、関数名
     * @return		true = 一致
     */
    public boolean equalsName(String value) {
        if (value == null) return false;
        if (this.callName == null) return false;
        if (this.isClang()) {
            return (value.equals(this.callName));
        }
        else {
            return (value.equalsIgnoreCase(this.callName));
        }
    }
}
