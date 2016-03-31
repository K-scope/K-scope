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

package jp.riken.kscope.xcodeml.xml;

/**
 * エラー識別子
 * @author RIKEN
 */
public enum EnumError {
    /**
     * 成功
     */
    SUCCESS {
        @Override
        public String message() {
            return "Success.";
        }

        @Override
        public boolean isError() {
            return false;
        }

        @Override
        public String format(Object... args) {
            assert (args.length == 0);
            return message();
        }
    },

    /**
     * ノードタイプ欠如エラー
     */
    XCODEML_TYPE_NOT_FOUND {
        @Override
        public String message() {
            return "Type definition for '%1$s' is not found in type table.";
        }

        /**
         * 書式付きメッセージ
         * @param args
         *            args[0]: Type name.
         * @return  メッセージ
         */
        @Override
        public String format(Object... args) {
            assert (args.length == 1);
            assert (args[0] instanceof String);
            return String.format(message(), args);
        }
    },

    /**
     * ノード名欠如エラー
     */
    XCODEML_NAME_NOT_FOUND {
        @Override
        public String message() {
            return "name '%1$s' is not found in symbol table.";
        }

        /**
         * 書式付きメッセージ
         * @param args
         *            args[0]: Symbol name.
         * @return  メッセージ
         */
        @Override
        public String format(Object... args) {
            assert (args.length == 1);
            assert (args[0] instanceof String);
            return String.format(message(), args);
        }
    },

    /**
     * ノードタイプ不一致エラー
     */
    XCODEML_TYPE_MISMATCH {
        @Override
        public String message() {
            return "Reference type of '%1$s' is defined as '%2$s', but it must be '%3$s'.";
        }

        /**
         * 書式付きメッセージ
         * @param args
         *            args[0]: Type name. (basic, function, struct, Fint, etc...) <br/>
         *            args[1]: Actual type. (basic, function, struct, etc...)<br/>
         *            args[2]: Expect type. (basic, function, struct, etc...)<br/>
         * @return  メッセージ
         */
        @Override
        public String format(Object... args) {
            assert (args.length == 3);
            assert (args[0] instanceof String);
            assert (args[1] instanceof String);
            assert (args[2] instanceof String);
            return String.format(message(), args);
        }
    },

    /**
     * ノード属性エラー
     */
    XCODEML_NEED_ATTR {
        @Override
        public String message() {
            return "The necessary '%1$s' attribute for '%2$s' "
                    + "element of this context does not exist or is empty.";
        }

        /**
         * 書式付きメッセージ
         * @param args
         *            args[0]: Attribute name.<br/>
         *            args[1]: Element name.
         * @return  メッセージ
         */
        @Override
        public String format(Object... args) {
            assert (args.length == 2);
            assert (args[0] instanceof String);
            assert (args[1] instanceof String);
            return String.format(message(), args);
        }
    },

    /**
     * ノード不明エラー
     */
    XCODEML_SEMANTICS {
        @Override
        public String message() {
            return "Detected a semantic error of XcodeML/F during handling of '%1$s' element.";
        }

        /**
         * @param args
         *            args[0]: Element name.
         */
        @Override
        public String format(Object... args) {
            assert (args.length == 1);
            assert (args[0] instanceof String);
            return String.format(message(), args);
        }
    },

    /**
     * ノード参照エラー
     */
    XCODEML_CYCLIC_TYPE {
        @Override
        public String message() {
            return "Type of '%1$s' has a cyclic type definition.";
        }

        /**
         * @param args
         *            args[0]: Type name. (basic, function, struct, Fint,
         *            etc...)
         */
        @Override
        public String format(Object... args) {
            assert (args.length == 1);
            assert (args[0] instanceof String);
            return String.format(message(), args);
        }
    },
    ;

    /**
     * メッセージを取得する
     * @return		メッセージ
     */
    public abstract String message();

    /**
     * 書式付きメッセージの取得を行う
     * @param args   エラーメッセージ
     * @return		メッセージ
     */
    public abstract String format(Object... args);

    /**
     * エラーであるかチェックする
     * @return		true=エラー
     */
    public boolean isError() {
        return true;
    }
}
