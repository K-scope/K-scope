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
package jp.riken.kscope.properties;

import java.io.File;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.utils.ResourceUtils;

/**
 * カーネル抽出プロパティ
 * @author RIKEN
 *
 */
public class KernelProperties extends PropertiesBase {
    private static final long serialVersionUID = 1L;

    public static String KERNEL_TEMPLATES_DIR = "templates";
    public static String DEFAULT_KERNEL_DIR = "kernel" +File.separator+ "default";
    public static String TEMPLATES_FILES[] = {"ksx_declarations.f90"};
    public static String MODULE_EXT = "f90";
    public static int DEFAULT_INDELT_COLUMN = 4 ;
    public static String MODULE_NAME_LOCAL = "kscope_mod_local";
    public static String MODULE_FILE_LOCAL = "kscope_mod_local.f90";
    public static String MODULE_TEMPLATE = "kscope_module.f90";
    public static String PROCEDURES_TEMPLATE = "kscope_procedures.f90";
    public static String DECLARATIONS_TEMPLATE = "kscope_declarations.f90";
    public static String VARIABLE_BOUNDS_TEMPLATE = "kscope_variable_bounds.f90";
    public static String MAKEFILE_TEMPLATE = "Makefile";
    public static String README_TEMPLATE = "kernel_info.yml";
    public static String FORTRAN_COMMENT = "!";

    /** 除外変数・関数・COMMON文宣言文：MPI */
    private String[] EXCLUDE_MPI_STATEMENTS = {
                            // external function
                            "mpi_null_copy_fn",
                            "mpi_null_delete_fn",
                            "mpi_comm_null_copy_fn",
                            "mpi_comm_null_delete_fn",
                            "mpi_type_null_copy_fn",
                            "mpi_type_null_delete_fn",
                            "mpi_dup_fn",
                            "mpi_comm_dup_fn",
                            "mpi_type_dup_fn",
                            "mpi_win_null_copy_fn",
                            "mpi_win_null_delete_fn",
                            "mpi_win_dup_fn",
                            "mpi_conversion_fn_null",
                            // common
                            "mpi_fortran_bottom",
                            "mpi_fortran_in_place",
                            "mpi_fortran_argv_null",
                            "mpi_fortran_argvs_null",
                            "mpi_fortran_errcodes_ignore",
                            "mpi_fortran_status_ignore",
                            "mpi_fortran_statuses_ignore",
                            "mpi_fortran_unweighted",
                            "mpi_fortran_weights_empty",
                            "mpifcmb",
                            "mpipriv"
                            };

    /** 除外変数・関数宣言文：OMP */
    private String[] EXCLUDE_OMP_STATEMENTS = {
                            "omp_init_lock",
                            "omp_init_nest_lock",
                            "omp_destroy_lock",
                            "omp_destroy_nest_lock",
                            "omp_set_lock",
                            "omp_set_nest_lock",
                            "omp_unset_lock",
                            "omp_unset_nest_lock",
                            "omp_set_dynamic",
                            "omp_set_nested",
                            "omp_set_num_threads",
                            "omp_get_dynamic",
                            "omp_get_nested",
                            "omp_test_lock",
                            "omp_in_parallel",
                            "omp_get_max_threads",
                            "omp_get_num_procs",
                            "omp_get_num_threads",
                            "omp_get_thread_num",
                            "omp_test_nest_lock",
                            "omp_get_wtick",
                            "omp_get_wtime",
                            "omp_set_schedule",
                            "omp_get_schedule",
                            "omp_get_thread_limit",
                            "omp_set_max_active_levels",
                            "omp_get_max_active_levels",
                            "omp_get_level",
                            "omp_get_ancestor_thread_num",
                            "omp_get_team_size",
                            "omp_get_active_level",
                            "omp_in_final"
                            };

    /** CALL未定義関数キーワード */
    private String[] EXCLUDE_CALL_KEYWORDS = {"mpi_", "omp_"};
    private String[] INCLUDE_MPI_FILENAMES = {"mpif.h", "mpiof.h",
            "mpif-config.h", "mpif-constants.h", "mpif-ext.h", "mpif-externals.h",
            "mpif-handles.h", "mpif-io-constants.h", "mpif-io-handles.h", "mpif-sentinels.h",
            "mpif-sizeof.h"};
    private String[] INCLUDE_OMP_FILENAMES = {"omp_lib.h"};

    /** 生成カーネル出力パス */
    public static String KERNEL_OUTPUT_DIR = "kscope_kernel";

    /** Fortran桁数制限 */
    private int limit_column = 120;
    /** インデント */
    private int indent = DEFAULT_INDELT_COLUMN;


    /**
     * コンストラクタ
     *
     * @throws Exception
     *             プロパティ読込エラー
     */
    public KernelProperties() throws Exception {
    }


    @Override
    public void firePropertyChange() {
    }


    /**
     * テンプレートファイルリストを取得する
     * @return        テンプレートファイルリスト
     */
    public Map<String, InputStream> getTemplateResourceStreams() {
        // リソースファイルの読込
        Map<String, InputStream> streams = ResourceUtils.getInputStreams(
                                KernelProperties.DEFAULT_KERNEL_DIR);

        if (streams == null) {
            streams = ResourceUtils.getInputStreams(
                    KERNEL_TEMPLATES_DIR + File.separator
                    + KernelProperties.DEFAULT_KERNEL_DIR);
        }

        return streams;
    }


    /**
     * テンプレートファイルリストを取得する
     * @return        テンプレートファイルリスト
     */
    public List<String> getTemplateNames() {
        // リソースファイルの読込
        List<String> files = ResourceUtils.getTemplateFileList(
                                KernelProperties.DEFAULT_KERNEL_DIR);

        if (files == null) {
            files = ResourceUtils.getTemplateFileList(
                    KERNEL_TEMPLATES_DIR + File.separator
                    + KernelProperties.DEFAULT_KERNEL_DIR);
        }

        if (files == null) {
            return null;
        }

        return files;
    }

    /* (非 Javadoc)
     * @see java.util.Properties#getProperty(java.lang.String)
     */
    @Override
    public String getProperty(String key) {
        // TODO 自動生成されたメソッド・スタブ
        return super.getProperty(key);
    }


    /* (非 Javadoc)
     * @see java.util.Properties#getProperty(java.lang.String, java.lang.String)
     */
    @Override
    public String getProperty(String key, String defaultValue) {
        // TODO 自動生成されたメソッド・スタブ
        return super.getProperty(key, defaultValue);
    }


    /**
     * Fortran桁数制限を取得する.
     * @return Fortran桁数制限
     */
    public int getLimitColumn() {
        return this.limit_column;
    }


    /**
     * Fortran桁数制限を設定する
     * @param limit_column Fortran桁数制限
     */
    public void setLimitColumn(int limit_column) {
        this.limit_column = limit_column;
    }


    /**
     * インデントを取得する.
     * @return   インデント
     */
    public int getIndent() {
        return this.indent;
    }


    /**
     * インデントを設定する.
     * @param indent     インデント
     */
    public void setIndent(int indent) {
        this.indent = indent;
    }

    /**
     * コメント文接頭語を取得する.
     * @return        コメント文接頭語
     */
    public String getLanguageCommnet() {
        return KernelProperties.FORTRAN_COMMENT;
    }


    /**
     * テンプレートファイルパスを取得する
     * @return        テンプレートファイルパス
     */
    public java.net.URL getTemplateUrl() {
        // リソースファイルの読込
        java.net.URL url = ResourceUtils.getTemplateResourceUrl(
                                KernelProperties.DEFAULT_KERNEL_DIR);

        if (url == null) {
            url = ResourceUtils.getTemplateResourceUrl(
                    KERNEL_TEMPLATES_DIR + File.separator
                    + KernelProperties.DEFAULT_KERNEL_DIR);
        }

        if (url != null) {
            try {
                File template_uri = new File(url.toURI());
                if (template_uri.exists()) {
                    return url;
                }
            } catch (Exception ex) {}
        }

        File template_file = ResourceUtils.getResourceFile(KernelProperties.DEFAULT_KERNEL_DIR);
        if (template_file == null) {
            template_file = ResourceUtils.getResourceFile(
                    KERNEL_TEMPLATES_DIR + File.separator
                    + KernelProperties.DEFAULT_KERNEL_DIR);
        }
        if (template_file == null) {
            return null;
        }
        if (template_file.exists()) {
            try {
                return template_file.toURI().toURL();
            } catch (Exception ex) {}
        }

        return null;
    }

    /**
     * 出力除外テンプレートであるかチェックする.
     * @param template_name        テンプレート名
     * @return        true=除外テンプレート
     */
    public boolean excludeTemplate(String template_name) {
        if (template_name == null) return true;
        if (template_name.endsWith(KernelProperties.PROCEDURES_TEMPLATE)) return true;
        if (template_name.endsWith(KernelProperties.DECLARATIONS_TEMPLATE)) return true;
        if (template_name.endsWith(KernelProperties.VARIABLE_BOUNDS_TEMPLATE)) return true;

        return false;
    }

    /**
     * カーネル出力除外文であるかチェックする
     * @param statement        チェック文
     * @return        true=カーネル出力除外文
     */
    public boolean isExcludeStatement(String statement) {
        if (statement == null) return false;

        String line = statement.toLowerCase();
        for (String exclude : this.EXCLUDE_MPI_STATEMENTS) {
            if (line.indexOf(exclude) >= 0) return true;
        }

        for (String exclude : this.EXCLUDE_OMP_STATEMENTS) {
            if (line.indexOf(exclude) >= 0) return true;
        }

        return false;
    }

    /**
     * CALL文未定義サブルーチン・関数の対象外であるかチェックする
     * CALL文の接頭語が"mpi_"又は"omp_"である場合は除外とする。
     * @param call_name        CALL文
     * @return        true=未定義サブルーチン・関数の対象外
     */
    public boolean isExcludeUndefinedCall(String call_name) {

        String line = call_name.toLowerCase();
        for (String exclude : this.EXCLUDE_CALL_KEYWORDS) {
            if (line.indexOf(exclude) == 0) return true;
        }

        return false;
    }

    /**
     * インクルードファイルであるかチェックする
     * @param source_file    ソースファイル
     * @return        true=インクルードファイル
     */
    public boolean isIncludeFile(SourceFile source_file) {
        if (source_file == null) return false;
        if (source_file.getFile() == null) return false;
        File file = source_file.getFile();
        String name = file.getName();
        for (String include : this.INCLUDE_MPI_FILENAMES) {
            if (include.equals(name)) return true;
        }
        for (String include : this.INCLUDE_OMP_FILENAMES) {
            if (include.equals(name)) return true;
        }

        return false;
    }

}


