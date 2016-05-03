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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;
import org.apache.velocity.runtime.RuntimeConstants;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.Common;
import jp.riken.kscope.language.DimensionIndex;
import jp.riken.kscope.language.Equivalence;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Module;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.Program;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.Substitution;
import jp.riken.kscope.language.UseState;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.VariableDimension;
import jp.riken.kscope.language.fortran.Type;
import jp.riken.kscope.language.fortran.VariableAttribute;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.language.fortran.VariableType.PrimitiveDataType;
import jp.riken.kscope.language.generic.IProcedureItem;
import jp.riken.kscope.language.generic.ProcedureItem;
import jp.riken.kscope.language.generic.Procedures;
import jp.riken.kscope.language.utils.LanguageUtils;
import jp.riken.kscope.properties.KernelProperties;
import jp.riken.kscope.service.kernel.KernelBlock;
import jp.riken.kscope.service.kernel.KernelBlocks;
import jp.riken.kscope.service.kernel.KernelContext;
import jp.riken.kscope.service.kernel.FortranFormattedWriter;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.StringUtils;

/**
 * カーネル生成サービス
 * @author RIKEN
 *
 */
public class KernelService extends BaseService {
    /** フォートラン構文解析結果格納データベース. */
    private Fortran fortranDb;
    /** カーネル出力フォルダ */
    private File kernelFolder;
    /** カーネル設定プロパティ */
    private KernelProperties properties;

    /**
     * カーネル化を行うモジュール
     * 元モジュールからコピーを行ったモジュール
     */
    private List<Module>  kernel_modules;

    /** カーネルの親モジュール */
    private Module kernel_parent_module = null;

    /**
     * カーネルのUSE文リスト
     */
    private List<UseState> kernel_uselist;

    /**
     * カーネルファイル入出力変数定義文
     */
    private List<VariableDefinition> fileio_definitions;

    /**
     * モジュール・ローカル変数一覧:データベースの変数インスタンス
     */
    private List<Variable>  source_variables;

    /**
     * モジュール・ローカル変数一覧:データベースの変数インスタンス
     */
    private List<VariableDefinition>  source_definitions;

    /**
     * 再帰呼出スタック
     * USE文検索、その他の再帰検索スタック
     */
    private List<String> recursive;

    /**
     * 再帰構造体 : ファイル入出力サポート対象外
     */
    private List<VariableDefinition> recursive_type;

    /**
     * 未定義CALL文
     */
    private List<ProcedureUsage> undefined_call;

    /**
     * エラーメッセージ
     * 継続不可能、カーネル抽出失敗のエラーメッセージ
     */
    private List<String> error_messages;

    /**
     * 警告メッセージ
     * カーネルの実行時に修正を要する警告メッセージ
     */
    private List<String> warning_messages;

    /**
     * 注意メッセージ
     * カーネルの実行時に修正が必要かもしれない注意メッセージ
     */
    private List<String> attention_messages;

    /**
     * 情報メッセージ
     * カーネル抽出にてユーザに提供する必要のあるメッセージ
     */
    private List<String> information_messages;

    /**
     * コンストラクタ
     * @throws Exception
     */
    public KernelService() throws Exception {
        this.properties = new KernelProperties();
        this.recursive = new ArrayList<String>();
        this.undefined_call = new ArrayList<ProcedureUsage>();
    }


    /**
     * カーネル生成を行う
     * @param  select_blocks   カーネル選択ブロック
     * @param  folder          カーネル出力フォルダ
     * @return     カーネル出力フォルダ
     * @throws IOException
     */
    public File buildKernel(List<IBlock> select_blocks, File folder) throws Exception {
        if (this.properties == null) return null;
        if (select_blocks == null || select_blocks.size() <= 0) return null;

        // カーネル選択ブロックをチェック、抽出ブロックを取得する
        List<IBlock> kernel_blocks = this.getKernelBlocks(select_blocks, true);
        if (kernel_blocks == null || kernel_blocks.size() <= 0) return null;

        // File[] template_files = this.properties.getTemplateFiles();
        List<String> template_files = this.properties.getTemplateNames();
        if (template_files == null || template_files.size() <= 0) {
            String msg = "can not found kernel template files.";
            this.addErrorInfo(msg);
            throw new Exception(msg);
        }

        // 出力フォルダを生成する。
        File outdir = this.makeOutputDirectory(select_blocks, folder);
        if (outdir == null) {
            return null;
        }

        // 抽出カーネルのプロシージャの取得
        ProgramUnit kernel_proc = kernel_blocks.get(0).getScopeDeclarationsBlock();
        String kernel_procname = kernel_proc.get_name();

        //Velocityコンテキストに値を設定
        KernelContext context = factoryVelocityContext();

        // テンプレートパス
        String name = template_files.get(0);
        String template_path = new File(name).getParent();
        if (template_path == null) template_path = "";
        context.put("template_path", template_path);

        // カーネルコードブロック
        context.putKernelBlocks("kernek_blocks", kernel_blocks);

        // カーネル化モジュールの作成
        this.createKernelModules(kernel_blocks);
        if (this.kernel_modules == null || this.kernel_modules.size() <= 0) {
            throw new Exception("カーネルモジュールの作成ができませんでした。");
        }

        // カーネルUSE文リスト
        List<UseState> kernel_uses = this.createKernelUseStates(kernel_blocks);
        context.putKernelBlocks("kernel_uses", kernel_uses);

        // 入出力変数定義一覧 : 元ソース属性の付いた入出力変数
        List<VariableDefinition> original_definitions = this.getFileioDeclarations(kernel_uses);
        context.putKernelBlocks("original_definitions", original_definitions);

        // モジュールの変数のソート、変数定義の属性変更を行う。 real(kind(0d0))をreal(8)にする
        this.normalizeDefinition();

        // 入出力変数定義一覧
        List<VariableDefinition> fileio_definitions = this.getFileioDeclarations(kernel_uses);
        context.putKernelBlocks("fileio_definitions", fileio_definitions);

        // 入出力構造体定義
        List<jp.riken.kscope.language.fortran.Type> fileio_types = this.getFileioTypes();
        context.putKernelBlocks("fileio_types", fileio_types);

        // 入出力構造体定義,PARAMETERのUSE文
        List<UseState> fileio_uses = this.createFileioUses();
        context.putKernelBlocks("fileio_uses", fileio_uses);

        // TYPE定義順番にソートを行う
        sortKernelModuleType();

        // カーネルコードの検証を行う。
        validateKernel(kernel_blocks);

        // MPI実行 : include mpif.hを追加するフラグ
        context.put("has_includempi", this.hasIncludeMpi());

        // MPI実行 : include omp_lib.hを追加するフラグ
        context.put("has_includeomp", this.hasIncludeOmp());

        // メタ情報出力データの設定
        this.setMetaInfo(context, select_blocks);

        List<File> file_list  = new ArrayList<File>();
        List<File> mod_filelist = new ArrayList<File>();
        String makefile = null;
        try {
            // for (File template : template_files) {
            for (String template_name : template_files) {
                // 出力対象外テンプレートであるか
                if (this.properties.excludeTemplate(template_name)) {
                    continue;        // 対象外
                }
                // モジュール、手続出力
                else if (template_name.endsWith(KernelProperties.MODULE_TEMPLATE)) {
                    // モジュールテンプレートから出力を行う.
                    mod_filelist = this.evaluateModules(
                                                context,
                                                template_name,
                                                outdir);
                    file_list.addAll(mod_filelist);
                }
                // Makefile
                // else if (KernelProperties.MAKEFILE_TEMPLATE.equalsIgnoreCase(template.getName())) {
                else if (template_name.endsWith(KernelProperties.MAKEFILE_TEMPLATE)) {
                    makefile = template_name;
                }
                // カーネル出力
                else {
                    // その他テンプレートから出力を行う.
                    File file = this.evaluateKernel(
                                            context,
                                            template_name,
                                            outdir);
                    // 出力モジュールの追加
                    if (file != null) file_list.add(file);
                }
            }

            // 生成モジュールファイル
            List<String> module_files = this.getModuleFilenames(mod_filelist);
            context.put("module_files", module_files);
            // Makefile出力
            if (makefile != null) {
                this.evaluateKernel(context, makefile, outdir);
            }

        } catch (Exception ex) {
            this.addErrorInfo(ex);
            throw ex;
        }

        return folder;
    }

    /**
     * メタ情報出力データの設定
     * @param context
     * @param kernel_blocks
     */
    private void setMetaInfo(VelocityContext context, List<IBlock> kernel_blocks) {
        if (context == null) return;
        if (kernel_blocks == null) return;
        if (kernel_blocks.size() <= 0) return;

        CodeLine start_line = kernel_blocks.get(0).getStartCodeLine();
        CodeLine end_line = kernel_blocks.get(kernel_blocks.size()-1).getEndCodeLine();
        if (start_line == null || start_line.getSourceFile() == null) {
            return;
        }
        String filename = start_line.getSourceFile().getFile().getName();
        ProgramUnit proc = kernel_blocks.get(0).getScopeDeclarationsBlock();

        // create_datetime : ${current_time}
        // filename : ${source_filename}
        context.put("source_filename", filename);
        // procedure : ${source_procedurename}
        if (proc != null) {
            context.put("source_procedurename", proc.get_name());
        }
        // start_lineno : ${source_startlineno}
        if (start_line != null) {
            context.put("source_startlineno", start_line.getStartLine());
        }
        // end_lineno : ${source_endlineno}
        if (end_line != null) {
            context.put("source_endlineno", end_line.getEndLine());
        }
        // error :${error_messages}
        context.put("error_messages", this.error_messages);
        // warning : ${warning_messages}
        context.put("warning_messages", this.warning_messages);
        // attention:${attention_messages}
        context.put("attention_messages", this.attention_messages);
        // information:${information_messages}
        context.put("information_messages", this.information_messages);

        return;
    }


    /**
     * モジュールテンプレートの出力を行う.
     * @param context        共通テンプレートコンテキスト
     * @param template        テンプレート
     * @param outdir        出力パス
     * @return   出力ファイルリスト
     * @throws IOException
     */
    private List<File> evaluateModules(
                KernelContext context,
                String template_name,
                File outdir) throws Exception {
        if (this.kernel_modules == null) return null;

        // ファイル毎にまとめる
        List<KernelBlocks> list = new ArrayList<KernelBlocks>();
        for (Module kernel_module : this.kernel_modules) {
            SourceFile mod_file = kernel_module.getSourceFile();
            if (mod_file == null) continue;
            boolean add_block = false;
            for (KernelBlocks block : list) {
                SourceFile blk_file = block.getSourceFile();
                if (blk_file.equals(mod_file)) {
                    block.addKernelBlock(new KernelBlock(kernel_module, kernel_module));
                    add_block = true;
                    break;
                }
            }
            if (add_block) continue;
            KernelBlocks kernels = new KernelBlocks();
            kernels.addKernelBlock(new KernelBlock(kernel_module, kernel_module));
            list.add(kernels);
        }

        // USE文の依存関係順にソートする.
        list = this.sortKernelBlocks(list);

        // テンプレート出力
        List<File> filelist = new ArrayList<File>();
        for (KernelBlocks kernels : list) {
            this.evaluateModule(context, template_name, outdir, kernels);
            SourceFile kernel_file = kernels.getSourceFile();
            if (kernel_file != null && kernel_file.getFile() != null) {
                filelist.add(kernel_file.getFile());
            }
        }

        if (filelist.size() <= 0) return null;

        return filelist;
    }


    /**
     * 手続テンプレートの出力を行う.
     * @param context        共通テンプレートコンテキスト
     * @param template        テンプレート
     * @param outdir        出力パス
     * @return   出力ファイルリスト
     * @throws IOException
     */
    private List<Module> evaluateProcedures(
                KernelContext context,
                String template_name,
                File outdir) throws Exception {
        if (this.kernel_modules == null) return null;

        List<Module> out_modules = new ArrayList<Module>();
        for (Module kernel_module :this.kernel_modules) {
            String mod_name = kernel_module.get_name();
            // NO_MODULEのみを出力
            if (!Program.NO_MODULE.equalsIgnoreCase(mod_name)) continue;
            this.evaluateModule(context, template_name, outdir, kernel_module);
            out_modules.add(kernel_module);
        }

        if (out_modules.size() <= 0) return null;

        return out_modules;
    }

    /**
     * モジュールテンプレートの出力を行う.
     * @param def_map        モジュール変数宣言文マップ
     * @param context        共通テンプレートコンテキスト
     * @param template        テンプレート
     * @param outdir        出力パス
     * @return   出力ファイルリスト
     * @throws IOException
     */
    private Module evaluateModule(
                KernelContext context,
                String template_name,
                File outdir,
                Module kernel_module) throws Exception {
        if (kernel_module == null) return null;

        try {
            // Writer生成
            File mod_file = kernel_module.getModuleFile();
            FortranFormattedWriter fileWriter = this.factoryFileWriter(outdir, mod_file);
            if (fileWriter == null || fileWriter.getOutputFilename() == null) {
                return null;
            }
            context.setLineWriter(fileWriter);

            //Velocityの初期化
            VelocityEngine velocity = this.factoryVelocityEngine();
            if (velocity == null) return null;

            // モジュール
            String mod_name = kernel_module.get_name();

            // カーネル出力モジュールの生成
            context.putKernelBlock("module", kernel_module);
            context.put("module_name", mod_name);

            // テンプレート適用
            velocity.mergeTemplate(template_name, "UTF-8", context, fileWriter);

            // ファイル出力
            fileWriter.flush();
            fileWriter.writeFile();
            fileWriter.close();

            return kernel_module;

        } catch (Exception ex) {
            ex.printStackTrace();
            String msg = "Error : can not create kernel file.[tmplate file = " + template_name + "]";
            this.addErrorInfo(msg);
            return null;
        } finally {
            context.setLineWriter(null);
        }

    }

    /**
     * モジュールテンプレートの出力を行う.
     * @param def_map        モジュール変数宣言文マップ
     * @param context        共通テンプレートコンテキスト
     * @param template        テンプレート
     * @param outdir        出力パス
     * @return   出力ファイルリスト
     * @throws IOException
     */
    private File evaluateModule(
                KernelContext context,
                String template_name,
                File outdir,
                KernelBlocks kernel_blocks) throws Exception {
        if (kernel_blocks == null) return null;

        try {
            // Writer生成
            File mod_file = kernel_blocks.getSourceFile().getFile();
            FortranFormattedWriter fileWriter = this.factoryFileWriter(outdir, mod_file);
            if (fileWriter == null || fileWriter.getOutputFilename() == null) {
                return null;
            }
            context.setLineWriter(fileWriter);

            //Velocityの初期化
            VelocityEngine velocity = this.factoryVelocityEngine();
            if (velocity == null) return null;

            // カーネル出力モジュールの生成
            context.putKernelBlocks("modules", kernel_blocks);

            // テンプレート適用
            velocity.mergeTemplate(template_name, "UTF-8", context, fileWriter);

            // ファイル出力
            fileWriter.flush();
            fileWriter.writeFile();
            fileWriter.close();

            return mod_file;

        } catch (Exception ex) {
            ex.printStackTrace();
            String msg = "Error : can not create kernel file.[tmplate file = " + template_name + "]";
            this.addErrorInfo(msg);
            return null;
        } finally {
            context.setLineWriter(null);
        }
    }

    /**
     * モジュールテンプレートの出力を行う.
     * @param def_map        モジュール変数宣言文マップ
     * @param context        共通テンプレートコンテキスト
     * @param template        テンプレート
     * @param outdir        出力パス
     * @throws IOException
     */
    private File evaluateKernel(
                    KernelContext context,
                    String template_name,
                    File outdir) throws Exception {

        try {
            // Writerの生成
            FortranFormattedWriter fileWriter = this.factoryFileWriter(outdir, new File(template_name));
            if (fileWriter == null || fileWriter.getOutputFilename() == null) {
                return null;
            }
            context.setLineWriter(fileWriter);

            //Velocityの初期化
            VelocityEngine velocity = this.factoryVelocityEngine();
            if (velocity == null) return null;

            // テンプレート適用
            velocity.mergeTemplate(template_name, "UTF-8", context, fileWriter);

            // ファイル出力
            fileWriter.flush();
            fileWriter.writeFile();
            fileWriter.close();

            return new File(fileWriter.getOutputFilename());

        } catch (Exception ex) {
            ex.printStackTrace();
            String msg = "Error : can not create kernel file.[tmplate file = " + template_name + "]";
            this.addErrorInfo(msg);
            return null;
        } finally {
            context.setLineWriter(null);
        }
    }

    /**
     * ファイル出力フォルダを生成する
     * @param  kernel_blocks   カーネル抽出ブロック
     * @param  folder          カーネル出力フォルダ
     * @return     カーネル出力フォルダ
     * @return
     */
    private File makeOutputDirectory(List<IBlock> blocks, File folder) {
        // 出力ディレクトリを取得する
        File outdir = this.getOutputDirectory(blocks, folder);
        if (outdir.exists()) {
            return outdir;
        }
        if (outdir.mkdirs()) {
            return outdir;
        }

        return null;
    }

    /**
     * 出力ディレクトリを取得する
     * @param blocks        カーネルブロック
     * @return        出力ディレクトリ
     */
    public File getOutputDirectory(List<IBlock> blocks, File folder) {
        if (blocks == null || blocks.size() <= 0) return null;
        String path = folder.getPath();
        if (!path.endsWith(KernelProperties.KERNEL_OUTPUT_DIR)) {
            path = path + File.separator + KernelProperties.KERNEL_OUTPUT_DIR;
        }
        CodeLine blockline0 = blocks.get(0).getStartCodeLine();
        CodeLine blockline1 = blocks.get(blocks.size()-1).getEndCodeLine();
        if (blockline0 == null || blockline0.getSourceFile() == null) {
            return null;
        }
        // 書式：filename_startlineno-endlineno
        String block_dir = blockline0.getSourceFile().getFile().getName();
        block_dir += "_" + blockline0.getStartLine();
        if (blockline0.getStartLine() != blockline1.getEndLine()) {
            block_dir += "-" + blockline1.getEndLine();
        }
        path = path + File.separator + block_dir;
        File outdir = new File(path);

        return outdir;
    }

    /**
     * 選択ブロックの変数を取得する
     * 変数文をKernelBlockクラスに変換する
     * @param kernel_blocks        選択ブロック
     * @return        変数宣言文
     */
    private List<Variable> getVariables(List<IBlock> kernel_blocks) {
        if (kernel_blocks == null || kernel_blocks.size() <= 0) return null;

        List<VariableDefinition> def_list = new ArrayList<VariableDefinition>();
        List<Variable> kernel_list = new ArrayList<Variable>();
        for (IBlock block : kernel_blocks) {
            Set<Variable> vars = block.getAllVariables();
            for (Variable var : vars) {
                VariableDefinition def = var.getDefinition();
                if (def != null && !def_list.contains(def)) {
                    def_list.add(def);
                    kernel_list.add(var);
                }
            }
        }
        if (kernel_list.size() <= 0) return null;

        // ソートを行う
        Collections.sort(kernel_list, new Comparator<Variable>(){
            public int compare(Variable block1, Variable block2) {
                if (block1 == null) return 0;
                if (block2 == null) return 0;
                String code1 = block1.toString();
                String code2 = block2.toString();
                if (code1 == null) return 0;
                if (code2 == null) return 0;

                return code1.compareToIgnoreCase(code2);
            }
        });

        return kernel_list;
    }


    /**
     * カーネル抽出を行うブロックを取得する.
     * @param blocks        選択ブロック
     * @param expand        true=子ブロックを展開する
     * @return        カーネル抽出を行うブロック
     */
    public List<IBlock> getKernelBlocks(List<IBlock> blocks, boolean expand) {
        if (blocks == null) return null;
        if (blocks.size() <= 0) return null;

        boolean error = false;
        // 同一サブルーチン・関数内のブロックであること
        IBlock base_proc = blocks.get(0).getScopeDeclarationsBlock();
        for (IBlock block : blocks) {
            if (block.getScopeDeclarationsBlock() != base_proc) {
                // 同一のサブルーチン・関数内のコードを選択してください
                String msg = Message.getString("kernel.select.diffrentblock.error.message");
                this.addErrorInfo(block.getStartCodeLine(), msg);
                error = true;
            }
        }
        if (error) return null;

        // Procedureクラスは選択不可
        List<IBlock> list = new ArrayList<IBlock>();
        if (blocks.size() > 1) {
            for (IBlock block : blocks) {
                if (block.getBlockType() == BlockType.MODULE
                    || block.getBlockType() == BlockType.PROCEDURE) {
                    // サブルーチン・関数は選択できません
                    String msg = Message.getString("kernel.select.procedure.error.message");
                    this.addErrorInfo(block.getStartCodeLine(), msg);
                    error = true;
                }
            }
            if (error) return null;
        }
        else if (blocks.get(0).getBlockType() == BlockType.PROCEDURE) {
            list = ((Procedure)blocks.get(0)).getBody().getChildren();
            if (list == null || list.size() <= 0) {
                // 出力カーネルがありません。
                String msg = Message.getString("kernel.select.empty.error.message");
                this.addErrorInfo(blocks.get(0).getStartCodeLine(), msg);
                return null;
            }
            if (expand) {
                return list;
            }
            else {
                return blocks;
            }
        }

        // 子ブロックは対象外とする
        for (IBlock block : blocks) {
            if (block.getBlockType() == BlockType.PROCEDUREUSAGE) {
                if (((ProcedureUsage)block).getIsFunctionCall()
                    && block.getMotherBlock() != null) {
                    // 関数呼出であるので親ブロックを取得する
                    block = block.getMotherBlock();
                }
            }
            if (block.getBlockType() == BlockType.CONDITION) {
                // 条件文であるので、親IF文を取得する
                block = block.getMotherBlock();
            }

            boolean is_childblock = false;
            for (IBlock parent : blocks) {
                if (block == parent) continue;
                if (LanguageUtils.isParentBlock(parent, block)) {
                    is_childblock = true;
                    break;
                }
            }
            if (!is_childblock && !list.contains(block)) {
                list.add(block);
            }
        }

        if (list.size() <= 0) return null;

        list = LanguageUtils.sortBlock(list);
        return list;
    }


    /**
     * モジュールのUSE文リストを作成する
     * 依存関係をチェックして、依存順に並べる
     * @param def_map        モジュール変数リスト
     * @return        USE文リスト
     */
    private List<Module> createUseList(List<String> mod_names) {
        if (mod_names == null || mod_names.size() <= 0) return null;

        List<Module> list = this.sortModuleUse(mod_names);
        if (list == null || list.size() <= 0) return null;

        List<Module> remove_list = new ArrayList<Module>();
        for (Module module : list) {
            String mod_name = module.get_name();
            if (KernelProperties.MODULE_NAME_LOCAL.equalsIgnoreCase(mod_name)) {
                continue;
            }

            List<UseState> use_list = module.getUseList();
            if (use_list != null && use_list.size() > 0) {
                for (UseState use : use_list) {
                    String use_name = use.getModuleName();
                    if (use_name == null) continue;
                    Module use_mod = this.fortranDb.module(use_name);
                    if (use_mod == null) continue;
                    if (list.contains(use_mod)) {
                        // 他のモジュールにてUSEされているので、カーネルuse文には出力しない。
                        remove_list.add(use_mod);
                    }
                }
            }
        }

        // 他のモジュールにてUSEされているモジュールの削除
        for (Module module : remove_list) {
            list.remove(module);
        }
        if (list.size() <= 0) return null;

        return list;
    }

    /**
     * モジュールのUSE文からモジュールの依存関係をチェックして、依存順に並べる
     * @param def_map        モジュール変数リスト
     * @return        依存順モジュール
     */
    private List<Module> sortModuleUse(List<String> mod_names) {
        if (mod_names == null || mod_names.size() <= 0) return null;

        List<Module> list = new ArrayList<Module>();
        for (String mod_name : mod_names) {
            Module mod = null;
            if (KernelProperties.MODULE_NAME_LOCAL.equalsIgnoreCase(mod_name)) {
                mod = new Module(mod_name);
            }
            else {
                mod = this.fortranDb.module(mod_name);
            }
            if (mod == null) continue;
            list.add(mod);
        }
        if (list.size() <= 0) return null;

        Collections.sort(list, new Comparator<Module>(){
            public int compare(Module module1, Module module2) {
                if (module1 == null) return 0;
                if (module2 == null) return 0;
                String mod1_name = module1.get_name();
                String mod2_name = module2.get_name();

                if (KernelProperties.MODULE_NAME_LOCAL.equalsIgnoreCase(mod1_name)) {
                    return +1;
                }
                if (KernelProperties.MODULE_NAME_LOCAL.equalsIgnoreCase(mod2_name)) {
                    return -1;
                }

                List<UseState> use1_list = module1.getUseList();
                List<UseState> use2_list = module2.getUseList();
                if (use1_list != null && use1_list.size() > 0) {
                    for (UseState use : use1_list) {
                        String use_name = use.getModuleName();
                        if (use_name == null) continue;
                        if (use_name.equalsIgnoreCase(mod2_name)) {
                            // module1のuse文にてmodule2を使用している
                            return +1;
                        }
                    }
                }
                if (use2_list != null && use2_list.size() > 0) {
                    for (UseState use : use2_list) {
                        String use_name = use.getModuleName();
                        if (use_name == null) continue;
                        if (use_name.equalsIgnoreCase(mod1_name)) {
                            // module2のuse文にてmodule1を使用している
                            return -1;
                        }
                    }
                }
                // モジュール名順
                return 0;
            }
        });

        return list;
    }

    /**
     * モジュールのUSE文からモジュールの依存関係をチェックして、依存順に並べる
     * @param def_map        モジュール変数リスト
     * @return        依存順モジュール
     */
    private List<Module> sortModule(List<Module> modules) {
        if (modules == null || modules.size() <= 0) return null;
        int count = modules.size();

        for (int i=0; i<count; i++) {
            for (int j=i+1; j<count; j++) {
                Module module1 = modules.get(i);
                Module module2 = modules.get(j);
                if (module1 == null) continue;
                if (module2 == null) continue;

                List<UseState> use1_list = KernelService.this.getAllUseStates(module1);
                List<UseState> use2_list = KernelService.this.getAllUseStates(module2);

                int compare = 0;
                int use1_count = 0;
                int use2_count = 0;
                if (use1_list != null) use1_count = use1_list.size();
                if (use2_list != null) use2_count = use2_list.size();
                if (use1_count == 0 && use2_count > 0) compare = -1;
                else if (use1_count > 0 && use2_count == 0) compare = +1;
                else if (use1_count == 0 && use2_count == 0) compare = 0;
                else {
                    // USE文の依存関係をチェックする
                    compare = comparatorModuleUse(module1, module2);
                }
                if (compare == +1) {
                    modules.remove(j);
                    modules.add(i, module2);
                    j = i;
                }
            }
        }

        return modules;
    }

    /**
     * モジュールのUSE文からモジュールの依存関係をチェックして、依存順に並べる
     * @param def_map        モジュール変数リスト
     * @return        依存順モジュール
     */
    private List<KernelBlocks> sortKernelBlocks(List<KernelBlocks> kernel_blocks) {
        if (kernel_blocks == null || kernel_blocks.size() <= 0) return null;
        int count = kernel_blocks.size();

        for (int i=0; i<count; i++) {
            for (int j=i+1; j<count; j++) {
                KernelBlocks blocks1 = kernel_blocks.get(i);
                KernelBlocks blocks2 = kernel_blocks.get(j);
                if (blocks1 == null) continue;
                if (blocks2 == null) continue;
                List<UseState> use1_list = this.getAllUseStates(blocks1);
                List<UseState> use2_list = this.getAllUseStates(blocks2);

                int compare = 0;
                int use1_count = 0;
                int use2_count = 0;
                if (use1_list != null) use1_count = use1_list.size();
                if (use2_list != null) use2_count = use2_list.size();
                if (use1_count == 0 && use2_count > 0) compare = -1;
                else if (use1_count > 0 && use2_count == 0) compare = +1;
                else if (use1_count == 0 && use2_count == 0) compare = 0;
                else {
                    // USE文の依存関係をチェックする
                    compare = comparatorBlocksUse(blocks1, blocks2);
                }
                if (compare == +1) {
                    kernel_blocks.remove(j);
                    kernel_blocks.add(i, blocks2);
                    j = i;
                }
            }
        }

        return kernel_blocks;
    }

    /**
     * USE文の依存関係をチェックする
     * @return
     */
    private int comparatorModuleUse(ProgramUnit module1, ProgramUnit module2) {
        if (module1 == null) return 0;
        if (module2 == null) return 0;
        String module1_name = module1.get_name();
        String module2_name = module2.get_name();
        if (module1_name == null) return 0;
        if (module2_name == null) return 0;

        int compare = 0;
        // kscope_mod_localを最後にする。
        if (KernelProperties.MODULE_NAME_LOCAL.equalsIgnoreCase(module1_name)) {
            compare = +1;
        }
        if (KernelProperties.MODULE_NAME_LOCAL.equalsIgnoreCase(module2_name)) {
            compare = -1;
        }
        if (compare != 0) return compare;

        List<UseState> use1_list = this.getAllUseStates(module1);
        List<UseState> use2_list = this.getAllUseStates(module2);

        if (compare == 0) {
            if (use1_list != null && use1_list.size() > 0) {
                for (UseState use : use1_list) {
                    String use_name = use.getModuleName();
                    if (use_name == null) continue;
                    if (use_name.equalsIgnoreCase(module2_name)) {
                        // module1のuse文にてmodule2を使用している
                        compare = +1;
                        break;
                    }
                    /*
                    // USE文のモジュールとmodule2を比較する
                    Module use_module = this.getKernelModule(use_name);
                    compare = this.comparatorModuleUse(use_module, module2);
                    if (compare != 0) {
                        break;
                    }
                    */
                }
            }
        }

        if (compare == 0) {
            if (use2_list != null && use2_list.size() > 0) {
                for (UseState use : use2_list) {
                    String use_name = use.getModuleName();
                    if (use_name == null) continue;
                    if (use_name.equalsIgnoreCase(module1_name)) {
                        // module2のuse文にてmodule1を使用している
                        compare = -1;
                        break;
                    }
                    /*
                    // USE文のモジュールとmodule1を比較する
                    Module use_module = this.getKernelModule(use_name);
                    compare = this.comparatorModuleUse(module1, use_module);
                    if (compare != 0) {
                        break;
                    }
                    */
                }
            }
        }

        return compare;

    }


    /**
     * USE文の依存関係をチェックする
     * @return
     */
    private int comparatorBlocksUse(KernelBlocks kernel_blocks1, KernelBlocks kernel_blocks2) {
        if (kernel_blocks1 == null) return 0;
        if (kernel_blocks2 == null) return 0;

        int compare = 0;
        for (KernelBlock block1 : kernel_blocks1) {
            if (!(block1.getKernelBlock() instanceof ProgramUnit)) continue;
            for (KernelBlock block2 : kernel_blocks2) {
                if (!(block2.getKernelBlock() instanceof ProgramUnit)) continue;

                // USE文の依存関係をチェックする
                compare = this.comparatorModuleUse(
                                (ProgramUnit)block1.getKernelBlock(),
                                (ProgramUnit)block2.getKernelBlock());
                if (compare > 0) {
                    return compare;
                }
            }
        }

        return compare;
    }

    /**
     * カーネル化モジュールを作成する。
     * @param kernel_blocks        カーネルブロック
     * @return        カーネル化モジュール
     * @throws Exception
     */
    private List<Module> createKernelModules(List<IBlock> kernel_blocks) throws Exception {
        if (kernel_blocks == null || kernel_blocks.size() <= 0) return null;

        // 抽出カーネルのプロシージャの取得
        ProgramUnit kernel_proc = kernel_blocks.get(0).getScopeDeclarationsBlock();
        String kernel_procname = kernel_proc.get_name();
        Module parent_mod = kernel_proc.getParentModule();

        // モジュール、ローカル変数, 定義
        this.source_variables = new ArrayList<Variable>();
        this.source_definitions = new ArrayList<VariableDefinition>();

        // 変数定義一覧
        List<VariableDefinition> kernel_def_list = new ArrayList<VariableDefinition>();
        for (IBlock block : kernel_blocks) {
            List<Variable> vars = this.getAllVariables(block);
            if (vars != null) {
                // データベース変数追加
                this.source_variables.addAll(vars);

                for (Variable var : vars) {
                    VariableDefinition def = var.getDefinition();
                    if (def == null) continue;
                    if (def.isStruct() || def.isStructMember()) {
                        // 構造体は親構造体定義を取得する
                        def = def.getStructDefinition();
                    }
                    if (kernel_def_list.contains(def)) continue;
                    kernel_def_list.add(def);
                }
            }
        }

        // 手続呼出一覧
        List<Procedure> proc_list = this.getAllFunctions(kernel_blocks, null);

        // 手続内の変数一覧
        if (proc_list != null) {
            for (Procedure proc : proc_list) {
                List<Variable> vars = this.getAllVariables(proc);
                if (vars == null) continue;

                // データベース変数追加
                this.source_variables.addAll(vars);

                for (Variable var : vars) {
                    VariableDefinition def = var.getDefinition();
                    if (def == null) continue;
                    if (def.isStruct() || def.isStructMember()) {
                        // 構造体は親構造体定義を取得する
                        def = def.getStructDefinition();
                    }
                    // ローカル変数は除外
                    if (!def.hasExternal()) {
                        if (def.getScopeDeclarationsBlock() == proc) continue;
                    }
                    if (var.getBlockType() == BlockType.PROCEDURE) {
                        Procedure var_proc = (Procedure)var.getParentStatement();
                        if (var_proc.isArgumentVariableDefinition(def)) {
                            continue;
                        }
                    }
                    if (kernel_def_list.contains(def)) continue;
                    kernel_def_list.add(def);
                }
            }
        }

        // 外部定義手続
        List<Procedure> externalProcedures = new ArrayList<Procedure>();
        List<VariableDefinition> externalDefinitions = new ArrayList<VariableDefinition>();
        if (proc_list != null) {
            for (Procedure proc : proc_list) {
                // 手続の外部定義
                String proc_name = proc.get_name();
                VariableDefinition def = kernel_proc.get_variable(proc_name);
                if (def == null) continue;
                if (!kernel_def_list.contains(def)) {
                    kernel_def_list.add(def);
                    externalProcedures.add(proc);
                    externalDefinitions.add(def);
                }
            }
        }
        if (kernel_def_list != null) {
            for (VariableDefinition def : kernel_def_list) {
                if (!def.hasExternal()) continue;
                String name = def.get_name();
                Procedure proc = this.fortranDb.getProcedure(name);
                if (proc == null) continue;
                if (!proc_list.contains(proc)) {
                    proc_list.add(proc);
                    externalProcedures.add(proc);
                    externalDefinitions.add(def);
                }
            }

        }

        // COMMON文,DATA文の変数を取得する.
        List<VariableDefinition> common_defs = this.getCommonVariableDefinition(kernel_def_list);
        if (common_defs != null) {
            for (VariableDefinition def : common_defs) {
                if (def == null) continue;
                if (kernel_def_list.contains(def)) continue;
                kernel_def_list.add(def);
            }
        }

        if (kernel_def_list != null) {
            // データベース変数定義追加
            this.source_definitions.addAll(kernel_def_list);
        }

        // ソートを行う
        kernel_def_list = this.sortVariableDefinition(kernel_def_list);

        // 変数定義モジュールの追加
        List<Module> list = new ArrayList<Module>();
        if (kernel_def_list != null) {
            for (VariableDefinition def : kernel_def_list) {
                // カーネルローカル変数
                IBlock def_block = def.getScopeDeclarationsBlock();
                if (def_block == null) continue;

                // 外部関数の変数定義であるか
                boolean externel = externalDefinitions.contains(def);

                // カーネルモジュールの追加
                Module def_mod = null;
                if (def_block.getBlockType() == BlockType.PROCEDURE
                    && (def_block == kernel_proc
                        || def_block == kernel_proc.getMotherBlock()) ) {
                    // ローカル変数モジュールに変数定義の追加
                    def_mod = this.createKernelLocalModule();
                    def_mod = this.addKernelVariableDefinition(def_mod, def, externel);
                    if (!list.contains(def_mod)) {
                        list.add(def_mod);
                    }
                }
                else if (def_block.getBlockType() == BlockType.MODULE) {
                    // 変数定義追加
                    def_mod = this.addKernelVariableDefinition(def, externel);
                    if (def_mod == null) {
                        String msg = "変数定義文のモジュールを取得できませんでした。";
                        msg += "[definition=" + def.toString() + "]";
                        this.addErrorInfo(msg);
                        continue;
                    }
                    if (!list.contains(def_mod)) {
                        list.add(def_mod);
                    }
                    if (parent_mod == def_block) {
                        this.kernel_parent_module = def_mod;
                    }
                }
            }

            // 構造体定義文の追加
            for (VariableDefinition def : kernel_def_list) {
                if (!def.isStruct()) continue;

                // 構造体定義モジュールの追加
                List<Module> type_mods = this.addKernelTypeDefinition(def, kernel_proc);
                if (type_mods != null) {
                    for (Module kernel_mod : type_mods) {
                        if (!list.contains(kernel_mod)) {
                            list.add(kernel_mod);
                        }
                    }
                }
            }
        }
        else {
            // 空のローカル変数モジュール作成
            Module local_mod = this.createKernelLocalModule();
            if (!list.contains(local_mod)) {
                list.add(local_mod);
            }
        }

        // 手続の追加
        if (proc_list != null) {
            for (Procedure proc : proc_list) {
                Module mod = null;
                if (kernel_proc == proc.get_mother()) {
                    mod = this.addKernelLocalProcedure(proc);
                }
                else {
                    // 手続追加
                    mod = this.addKernelProcedure(proc);
                }
                if (mod == null) {
                    // [エラー]副プログラムのモジュールを取得できませんでした。
                    String msg = Message.getString("kernel.notfound_procedure.error.message");
                    msg += "[procedure =" + proc.toString() + "]";
                    this.addErrorInfo(msg);
                    continue;
                }
                if (!list.contains(mod)) {
                    list.add(mod);
                }
            }
        }

        // inculde 'mpif.h'を設定する
        Module mpi_mod = this.setIncludeMpi(kernel_blocks);
        if (mpi_mod != null && !list.contains(mpi_mod)) {
            list.add(mpi_mod);
        }

        // inculde 'omp_lib.h'を設定する
        Module omp_mod = this.setIncludeOmp(kernel_blocks);
        if (omp_mod != null && !list.contains(omp_mod)) {
            list.add(omp_mod);
        }

        if (list.size() <= 0) return null;

        // モジュールのUSE文を追加する
        this.setKernelUseStates(kernel_proc);

        // モジュールのDATA文を追加する
        this.setKernelDataList();

        // モジュールのINTERFACE文を追加する
        this.setKernelInterfaces(kernel_blocks);

        // モジュールのCOMMON文を追加する
        this.setKernelCommon(kernel_blocks);

        // モジュールのEQUIVALENCE文を追加する
        this.setKernelEquivalence(kernel_blocks);

        return list;
    }

    /**
     * inculde 'mpif.h'を設定する
     * @param kernel_blocks        カーネルブロック
     * @return  inculde 'mpif.h'の設定モジュール
     */
    private Module setIncludeMpi(List<IBlock> kernel_blocks) {
        if (kernel_blocks == null || kernel_blocks.size() <= 0) return null;

        // 抽出カーネルのプロシージャの取得
        ProgramUnit kernel_proc = kernel_blocks.get(0).getScopeDeclarationsBlock();
        String kernel_procname = kernel_proc.get_name();
        Module parent_mod = kernel_proc.getParentModule();
        if (parent_mod != null
            && parent_mod.get_name() != null && parent_mod.isNoModule()) {
            parent_mod = null;
        }

        Module mpi_module = null;
        // カーネル抽出ブロックにinculde 'mpif.h'が存在する場合
        if (kernel_proc.hasIncludeMpi()) {
            // ローカルモジュールにinculde 'mpif.h'を設定する
            Module local_mod = this.getKernelLocalModule();
            if (local_mod == null) {
                local_mod = this.createKernelLocalModule();
            }
            local_mod.putIncludeMpi(null);
            mpi_module = local_mod;
        }
        // 親モジュールにinculde 'mpif.h'が存在する場合
        else if (parent_mod != null && parent_mod.hasIncludeMpi()) {
            String name = parent_mod.get_name();
            Module kernel_mod = this.getKernelModule(name);
            if (kernel_mod == null) {
                kernel_mod = this.addKernelModule(parent_mod);
            }
            kernel_mod.putIncludeMpi(null);
            mpi_module = kernel_mod;
        }

        return mpi_module;
    }


    /**
     * inculde 'mpif.h'が設定されているかチェックする。
     * すべてのカーネルモジュール、サブルーチンからinculde 'mpif.h'が設定されているかチェックする。
     * @return  true=inculde 'mpif.h'が設定されている
     */
    private boolean hasIncludeMpi() {
        if (this.kernel_modules == null) return false;

        for (Module kernel_module :this.kernel_modules) {
            if (kernel_module.hasIncludeMpi()) {
                return true;
            }
            Procedure[] procs = kernel_module.get_children();
            if (procs == null) continue;
            for (Procedure proc : procs) {
                if (proc.hasIncludeMpi()) {
                    return true;
                }
            }
        }
        return false;
    }


    /**
     * inculde 'omp_lib.h'を設定する
     * @param kernel_blocks        カーネルブロック
     * @return  inculde 'omp_lib.h'の設定モジュール
     */
    private Module setIncludeOmp(List<IBlock> kernel_blocks) {
        if (kernel_blocks == null || kernel_blocks.size() <= 0) return null;

        // 抽出カーネルのプロシージャの取得
        ProgramUnit kernel_proc = kernel_blocks.get(0).getScopeDeclarationsBlock();
        String kernel_procname = kernel_proc.get_name();
        Module parent_mod = kernel_proc.getParentModule();
        if (parent_mod != null
            && parent_mod.get_name() != null && parent_mod.isNoModule()) {
            parent_mod = null;
        }

        Module omp_module = null;
        // カーネル抽出ブロックにinculde 'omp_lib.h'が存在する場合
        if (kernel_proc.hasIncludeOmp()) {
            // ローカルモジュールにinculde 'omp_lib.h'を設定する
            Module local_mod = this.getKernelLocalModule();
            if (local_mod == null) {
                local_mod = this.createKernelLocalModule();
            }
            local_mod.putIncludeOmp(null);
            omp_module = local_mod;
        }
        // 親モジュールにinculde 'omp_lib.h'が存在する場合
        else if (parent_mod != null && parent_mod.hasIncludeOmp()) {
            String name = parent_mod.get_name();
            Module kernel_mod = this.getKernelModule(name);
            if (kernel_mod == null) {
                kernel_mod = this.addKernelModule(parent_mod);
            }
            kernel_mod.putIncludeOmp(null);
            omp_module = kernel_mod;
        }

        return omp_module;
    }

    /**
     * inculde 'omp_lib.h'が設定されているかチェックする。
     * すべてのカーネルモジュール、サブルーチンからinculde 'omp_lib.h'が設定されているかチェックする。
     * @return  true=inculde 'omp_lib.h'が設定されている
     */
    private boolean hasIncludeOmp() {
        if (this.kernel_modules == null) return false;

        for (Module kernel_module :this.kernel_modules) {
            if (kernel_module.hasIncludeOmp()) {
                return true;
            }
            Procedure[] procs = kernel_module.get_children();
            if (procs == null) continue;
            for (Procedure proc : procs) {
                if (proc.hasIncludeOmp()) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * UseStateをモジュール毎にマージする.
     * @param uses
     * @return
     */
    private List<UseState> margeUseState(List<UseState> uses) {
        if (uses == null || uses.size() <= 0) return null;

        List<UseState> marge_list = new ArrayList<UseState>();
        for (UseState use : uses) {
            String name = use.getModuleName();
            Set<String> onlys = use.getOnlyMember();
            // 既存UseStateであるか
            UseState found_use = null;
            for (UseState item : marge_list) {
                if (name.equalsIgnoreCase(item.getModuleName())) {
                    found_use = item;
                }
            }
            if (found_use == null) {
                marge_list.add(use);
            }
            else {
                for (String only : onlys) {
                    found_use.addOnlyMember(only);
                }
            }
        }
        if (marge_list.size() <= 0) return null;

        return marge_list;
    }

    /**
     * データベースを取得する
     * @return fortranDb        データベース
     */
    public Fortran getFortranDb() {
        return this.fortranDb;
    }


    /**
     * データベースを設定する
     * @param fortranDb   データベース
     */
    public void setFortranDb(Fortran fortranDb) {
        this.fortranDb = fortranDb;
    }


    /**
     * 選択ブロックのカーネルコードを取得する
     * @param kernel_blocks        選択ブロック
     * @return        変数宣言文
     */
    private KernelBlock createKernelDefinition(VariableDefinition def) {
        if (def == null) return null;

        VariableAttribute def_attr = (VariableAttribute)def.getAttribute();
        VariableDefinition module_def = new VariableDefinition(def);
        // public属性追加
        String attr = VariableAttribute.ScopeAttribute.PUBLIC.toString().toLowerCase();
        module_def.addVariableAttributes(attr);
        // private属性削除
        attr = VariableAttribute.ScopeAttribute.PRIVATE.toString().toLowerCase();
        module_def.removeVariableAttributes(attr);
        // save属性追加
        if (def_attr == null || !def_attr.hasParameter()) {
            attr = VariableAttribute.ATTRIBUTE_SAVE.toLowerCase();
            module_def.addVariableAttributes(attr);
        }
        // intent属性削除
        attr = VariableAttribute.ATTRIBUTE_INTENT.toLowerCase();
        module_def.removeVariableAttributes(attr);
        // 配列パラメータ
        VariableDimension dim = module_def.getVariableDimension();
        if (dim != null && dim.getIndex() != null) {
            Set<Variable> vars = dim.getAllVariables();
            boolean has_parameter = false;
            if (vars != null) {
                // 配列インデックスがparameterであるかチェックする
                for (Variable var : vars) {
                    VariableDefinition var_def = var.getDefinition();
                    if (var_def == null) continue;
                    if (var_def.getAttribute() == null) continue;
                    if (((VariableAttribute)var_def.getAttribute()).hasParameter()) {
                        has_parameter = true;
                    }
                    else {
                        has_parameter = false;
                        break;
                    }
                }
            }
            else {
                DimensionIndex[] indexes = dim.getIndex();
                for (DimensionIndex idx : indexes) {
                    if (idx == null) continue;
                    if (idx.toString() != null && !idx.toString().isEmpty()) {
                        has_parameter = true;
                        break;
                    }
                }
            }
            if (!has_parameter) {
                // 配列変数の削除
                dim.clearIndices();
                // allocatable属性追加
                String alloc_attr = VariableAttribute.ATTRIBUTE_ALLOCATABLE.toString().toLowerCase();
                module_def.addVariableAttributes(alloc_attr);
            }
        }

        // real(kind(0d0))をreal(8)にする
        VariableType type = (VariableType)module_def.getVariableType();
        if (type != null && type.isKindDouble()) {
            type.setKind(new Expression("8"));
        }

        KernelBlock kernel = new KernelBlock(module_def);

        return kernel;
    }


    /**
     * 入出力変数定義一覧を取得する.
     * PARAMETER属性の変数を除外する
     * @param kernel_uses        カーネルUSE文
     * @return        入出力変数定義一覧
     */
    private List<VariableDefinition> getFileioDeclarations(
                    List<UseState> kernel_uses) {
        if (kernel_uses == null || kernel_uses.size() <= 0) return null;
        if (this.kernel_modules == null || this.kernel_modules.size() <= 0) return null;

        // すべてのモジュール変数の取得
        List<VariableDefinition> def_list = this.getAllKernelDefinitions();
        if (def_list == null) return null;

        // スコープのチェック、PARAMETER, PRIVATE属性チェックを行う
        ListIterator<VariableDefinition> itr = def_list.listIterator();
        while (itr.hasNext()) {
            boolean is_remove = false;
            VariableDefinition def = itr.next();
            VariableAttribute attr = (VariableAttribute) def.getAttribute();
            VariableType type = (VariableType) def.getVariableType();
            String var_name = def.get_name();

            // ソースファイルが存在していること
            if (!def.hasSourceFile()) is_remove = true;
            // PARAMETER属性の変数は除外
            else if (attr != null && attr.hasParameter()) is_remove = true;
            // スコープのチェックを行う:スコープの対象外は除外
            else if (!this.validateScope(kernel_uses, def)) is_remove = true;
            // external属性が付いている場合は除外
            else if (def.hasExternal())  is_remove = true;

            if (is_remove) {
                itr.remove();
            }

            // 構造体の場合、再帰構造ではないこと
            if (def.isStruct() && type != null && type.getType() != null) {
                jp.riken.kscope.language.fortran.Type typedef = type.getType();
                if (typedef.hasRecursiveMember()) {
                    itr.remove();
                    this.addRecursiceType(def);
                }
            }
            String rename_name = null;
            for (UseState use : kernel_uses) {
                String trans_name = use.translation(def);
                if (!trans_name.equals(var_name)) {
                    // renameされている
                    rename_name = trans_name;
                    break;
                }
            }
            if (rename_name != null) {
                // renameされた名前で入出力変数を作成する
                VariableDefinition rename_def = new VariableDefinition(def);
                rename_def.set_name(rename_name);
                itr.set(rename_def);
            }
        }
        if (def_list.size() <= 0) return null;

        // ソートを行う
        def_list = this.sortVariableDefinition(def_list);

        // カーネルファイル入出力変数定義文
        this.fileio_definitions = new ArrayList<VariableDefinition>();
        this.fileio_definitions.addAll(def_list);

        return def_list;
    }

    /**
     * カーネル化モジュールのすべての変数定義文を取得する
     * @return        すべての変数定義文
     */
    private List<VariableDefinition> getAllKernelDefinitions() {
        if (this.kernel_modules == null || this.kernel_modules.size() <= 0) return null;

        // すべてのモジュール変数の取得
        List<VariableDefinition> def_list = new ArrayList<VariableDefinition>();
        for (Module mod : this.kernel_modules) {
            VariableDefinition[] defs = mod.get_variables();
            def_list.addAll(Arrays.asList(defs));
        }
        if (def_list.size() <= 0) return null;

        return def_list;
    }


    /**
     * ブロック配下のすべての変数を探索する.
     * @param blocks        ブロック
     * @return        探索変数
     */
    private List<Variable> getAllVariables(List<IBlock> blocks) {

        List<Variable> var_list = new ArrayList<Variable>();
        for (IBlock block : blocks) {
            List<Variable> vars = this.getAllVariables(block);
            if (vars != null) {
                var_list.addAll(vars);
            }
        }
        if (var_list.size() <= 0) return null;

        return var_list;
    }


    /**
     * ブロック配下のすべての変数を探索する.
     * @param blocks        ブロック
     * @return        探索変数
     */
    private List<Variable> getAllVariables(IBlock block) {
        if (block == null) return null;

        List<Variable> var_list = new ArrayList<Variable>();
        Set<Variable> vars = block.getAllVariables();
        if (vars != null && vars.size() > 0) {
            var_list.addAll(vars);
            for (Variable var : vars) {
                VariableDefinition def = var.getDefinition();
                List<Variable> var_defs = this.getAllVariables(def);
                if (var_defs != null && var_defs.size() > 0) {
                    var_list.addAll(var_defs);
                }
            }
        }

        List<IBlock> blocks = block.getChildren();
        if (blocks != null) {
            for (IBlock child_block : blocks) {
                if (child_block.getBlockType() != BlockType.PROCEDUREUSAGE) continue;
                ProcedureUsage call = (ProcedureUsage)child_block;
                Procedure proc = call.getCallDefinition();
                if (proc == null) continue;
                List<Variable> child_vars = this.getAllVariables(proc);
                if (child_vars != null) {
                    var_list.addAll(child_vars);
                }
            }
        }

        if (block.getBlockType() == BlockType.PROCEDUREUSAGE) {
            ProcedureUsage call = (ProcedureUsage)block;
            Procedure proc = call.getCallDefinition();
            if (proc != null) {
                List<Variable> child_vars = this.getAllVariables(proc);
                if (child_vars != null) {
                    var_list.addAll(child_vars);
                }
            }
        }

        if (var_list.size() <= 0) return null;

        return var_list;
    }


    /**
     * 呼出手続一覧を取得する.
     * 呼出サブルーチン・関数内の子孫呼出手続まで検索する。
     * @param block            探索ブロック
     * @param proc_list        取得呼出一覧
     * @return        取得呼出一覧
     */
    private List<Procedure> getAllFunctions(IBlock block, List<Procedure> proc_list) {
        if (block == null) return null;

        // 呼出手続一覧
        if (proc_list == null) {
            proc_list = new ArrayList<Procedure>();
        }
        if (block.getBlockType() == BlockType.PROCEDUREUSAGE) {
            // CALL名
            String name = ((ProcedureUsage)block).getCallName();

            Procedure call_def = ((ProcedureUsage)block).getCallDefinition();
            if (call_def != null) {
                if (!proc_list.contains(call_def)) {
                    proc_list.add(call_def);
                    // 呼出手続一覧
                    this.getAllFunctions(call_def, proc_list);
                }
            }
            else {
                // 引数渡しの手続であるかチェックする
                boolean is_argment = this.isExternalArgment((ProcedureUsage)block);
                if (!is_argment) {
                    // 未定義CALL文
                    if (!this.undefined_call.contains((ProcedureUsage)block)) {
                        this.undefined_call.add((ProcedureUsage)block);
                    }
                }

                // XcodeML/FのUSE文のrenameバグ対策
                // USE文から同名サブルーチン定義を検索する。
                this.getUndefinedFunctions((ProcedureUsage)block, proc_list);
            }
        }
        List<ProcedureUsage> calls = block.getCalls();
        if (calls != null) {
            for (ProcedureUsage call : calls) {
                Procedure proc = call.getCallDefinition();
                if (proc != null) {
                    if (!proc_list.contains(proc)) {
                        proc_list.add(proc);
                        // 呼出手続一覧
                        this.getAllFunctions(proc, proc_list);
                    }
                }
                else {
                    // 引数渡しの手続であるかチェックする
                    boolean is_argment = this.isExternalArgment(call);
                    if (!is_argment) {
                        // 未定義CALL文
                        if (!this.undefined_call.contains(call)) {
                            this.undefined_call.add(call);
                        }
                    }

                    // XcodeML/FのUSE文のrenameバグ対策
                    // USE文から同名サブルーチン定義を検索する。
                    this.getUndefinedFunctions(call, proc_list);
                }
            }
        }

        List<IBlock> childs = block.getChildren();
        if (childs != null) {
            for (IBlock child : childs) {
                // 呼出手続一覧
                this.getAllFunctions(child, proc_list);
            }
        }

        return proc_list;
    }

    /**
     * CALL文のサブルーチン未定義の場合、USE文から同名サブルーチン定義を検索する。
     * XcodeML/FのUSE文のrenameバグ対策
     * @param call        CALL文
     * @param proc_list        サブルーチン定義リスト
     * @return        サブルーチン定義リスト
     */
    private List<Procedure> getUndefinedFunctions(ProcedureUsage call, List<Procedure> proc_list) {
        if (call == null) return null;
        // CALL名
        String call_name = call.getCallName();

        // XcodeML/FのUSE文のrenameバグ対策
        // 関数、モジュールのUSE文を取得する
        List<UseState> uses = this.getScopeUseList(call);
        if (uses != null && uses.size() > 0) {
            for (UseState use : uses) {
                if (!use.hasOnlyMember(call_name)
                    || !use.hasTranslation(call_name)) continue;
                String use_name = use.getModuleName();
                Module use_mod = this.fortranDb.module(use_name);
                if (use_mod == null) continue;
                Procedure call_def = (Procedure)use_mod.get_child(call_name);
                if (call_def == null) continue;
                if (!proc_list.contains(call_def)) {
                    // 関数追加
                    proc_list.add(call_def);
                    // 呼出手続一覧
                    this.getAllFunctions(call_def, proc_list);
                }
            }
        }

        return proc_list;
    }


    /**
     * 呼出手続一覧を取得する
     * @param block            探索ブロック
     * @param proc_list        取得呼出一覧
     * @return        取得呼出一覧
     */
    private List<Procedure> getAllFunctions(List<IBlock> blocks, List<Procedure> proc_list) {

        // 呼出手続一覧
        if (proc_list == null) {
            proc_list = new ArrayList<Procedure>();
        }
        for (IBlock block : blocks) {
            this.getAllFunctions(block, proc_list);
        }

        return proc_list;
    }


    /**
     * 構造体定義文を検索する。
     * @param var_def        構造体宣言文
     * @return        構造体定義モジュール・プロシージャ
     */
    private ProgramUnit searchStructDefinition(
                            VariableDefinition var_def,
                            ProgramUnit proc) {
        if (var_def == null) return null;
        if (var_def.getMother() == null) return null;
        if (!var_def.isStruct()) return null;

        String type_name = var_def.getType().getName();
        if (proc == null) {
            proc = var_def.getMother().getScopeDeclarationsBlock();
            this.recursive = new ArrayList<String>();
        }
        if (proc == null) return null;

        // 構造体定義文の探索
        while(proc != null) {
            if (proc.getType(type_name) != null) {
                return proc;
            }

            // USE文モジュールの再帰呼出チェック
            if (this.recursive.contains(proc.get_name())) {
                return null;
            }
            this.recursive.add(proc.get_name());

            // USE先から検索
            List<UseState> uses = proc.getUseList();
            if (uses != null) {
                for (UseState use : uses) {
                    String use_name = use.getModuleName();
                    Module use_mod = this.fortranDb.module(use_name);
                    if (use_mod == null) continue;
                    if (use_mod.getType(type_name) != null) {
                        return use_mod;
                    }
                    // USE先モジュールのUSE文から検索
                    ProgramUnit use_proc = this.searchStructDefinition(
                                                var_def, use_mod);
                    if (use_proc != null) {
                        return use_proc;
                    }
                }
            }
            proc = proc.get_mother();
        }

        return null;
    }

    /**
     * ローカル変数カーネル化モジュールを追加する
     * @return        ローカル変数カーネル化モジュール
     */
    private Module createKernelLocalModule() {
        // 既存であるかチェックする
        String mod_name = KernelProperties.MODULE_NAME_LOCAL;
        Module kernel_mod = this.getKernelModule(mod_name);
        if (kernel_mod != null) return kernel_mod;

        // モジュールを作成
        kernel_mod = new Module(mod_name);
        if (this.kernel_modules == null) {
            this.kernel_modules = new ArrayList<Module>();
        }
        this.kernel_modules.add(kernel_mod);

        // ローカル変数モジュールの出力ファイル名を設定する
        CodeLine line = new CodeLine(
                            new SourceFile(KernelProperties.MODULE_FILE_LOCAL),
                            1,1,null);
        kernel_mod.set_start(line);
        kernel_mod.set_end(line);

        return kernel_mod;
    }

    /**
     * カーネル化モジュールを追加する
     * モジュールを作成してカーネル化リストに追加する.
     * 追加済みであれば追加しない。追加済みモジュールを返す。
     * @param mod_name        モジュール名
     * @return        カーネル化モジュール
     */
    private Module getKernelLocalModule() {
        String mod_name = KernelProperties.MODULE_NAME_LOCAL;
        Module kernel_mod = this.getKernelModule(mod_name);
        return kernel_mod;
    }


    /**
     * カーネル化モジュールを取得する。
     * @param mod_name        モジュール名
     * @return        カーネル化モジュール
     */
    private Module getKernelModule(String mod_name) {
        if (mod_name == null) return null;
        if (this.kernel_modules == null) return null;

        for (Module mod : this.kernel_modules) {
            String kernel_name = mod.get_name();
            if (mod_name.equalsIgnoreCase(kernel_name)) {
                return mod;
            }
        }
        return null;
    }


    /**
     * カーネル化モジュールを取得する。
     * @param mod_name        モジュール名
     * @return        カーネル化モジュール
     */
    private Module getKernelModule(Module module) {
        if (module == null) return null;
        if (this.kernel_modules == null) return null;

        String mod_name = module.get_name();
        SourceFile mod_file = null;
        if (module.getStartCodeLine() != null) {
            mod_file = module.getStartCodeLine().getSourceFile();
        }
        for (Module mod : this.kernel_modules) {
            String kernel_name = mod.get_name();
            if (!mod_name.equalsIgnoreCase(kernel_name)) continue;
            if (!Program.NO_MODULE.equalsIgnoreCase(mod_name)) {
                return mod;
            }
            // NO_MODULEの場合、ファイル名を比較する
            CodeLine line = mod.getStartCodeLine();
            if (line == null) continue;
            SourceFile file = line.getSourceFile();
            if (file == null) continue;
            if (file.equals(mod_file)) {
                return mod;
            }
        }
        return null;
    }


    /**
     * カーネル化モジュールが追加済みであるかチェックする
     * @param mod_name        カーネル化モジュール名
     * @return        true=追加済み
     */
    private boolean containsKernelModule(String mod_name) {
        if (mod_name == null) return false;
        Module kernel_mod = this.getKernelModule(mod_name);
        return (kernel_mod != null);
    }

    /**
     * カーネル化モジュールを削除する.
     * @param mod_name        カーネル化モジュール名
     */
    private void removeKernelModule(String mod_name) {
        if (mod_name == null) return;
        if (this.kernel_modules == null) return;

        Iterator<Module> itr = this.kernel_modules.iterator();
        while (itr.hasNext()) {
            Module kernel_mod = itr.next();
            if (mod_name.equalsIgnoreCase(kernel_mod.get_name())) {
                itr.remove();
            }
        }

        return;
    }


    /**
     * カーネル化モジュールに変数定義文を追加する
     * 追加済みであれば追加しない。
     * @param var_def        変数定義
     * @param externel       true=外部関数定義
     * @return        カーネル化モジュール
     */
    private Module addKernelVariableDefinition(
                    VariableDefinition var_def,
                    boolean externel) {
        if (var_def == null) return null;
        // モジュール
        ProgramUnit proc = var_def.getScopeDeclarationsBlock();
        Module kernel_mod = null;
        if (proc.getBlockType() == BlockType.MODULE
            && !Program.NO_MODULE.equalsIgnoreCase(proc.get_name())) {
            kernel_mod = this.addKernelModule((Module)proc);
        }
        else {
            // モジュールではないこと、NO_MODULEのことはない
            // kernel_mod = this.addKernelNoModule(proc);
        }
        if (kernel_mod == null) return null;

        // 変数定義文を追加
        VariableDefinition kernel_def = kernel_mod.get_variable(var_def.get_name());
        if (kernel_def != null) return kernel_mod;

        VariableDefinition new_def = new VariableDefinition(var_def);
        if (externel) {
            new_def.addVariableAttributes(VariableAttribute.FunctionPositionAttribute.EXTERNAL.toString());
        }
        kernel_mod.set_variable_def(new_def);

        return kernel_mod;
    }

    /**
     * カーネル化モジュールに変数定義文を追加する
     * 追加済みであれば追加しない。
     * @param module       カーネル化モジュール
     * @param var_def        変数定義
     * @param externel       true=外部関数定義
     * @return        カーネル化モジュール
     */
    private Module addKernelVariableDefinition(
                    Module module,
                    VariableDefinition var_def,
                    boolean externel) {
        if (module == null) return null;
        if (var_def == null) return null;

        // 既存であるかチェックする
        Module kernel_mod = this.getKernelModule(module);
        if (kernel_mod == null) {
            kernel_mod = this.addKernelModule(module);
        }
        if (kernel_mod == null) return null;

        VariableDefinition kernel_def = kernel_mod.get_variable(var_def.get_name());
        if (kernel_def != null) return kernel_mod;

        // 変数定義生成
        VariableDefinition new_def = new VariableDefinition(var_def);
        // 外部関数属性追加
        if (externel) {
            new_def.addVariableAttributes(VariableAttribute.FunctionPositionAttribute.EXTERNAL.toString());
        }
        // 変数定義を追加する
        kernel_mod.set_variable_def(new_def);

        return kernel_mod;
    }


    /**
     * カーネル化モジュールに構造体定義文を追加する
     * 追加済みであれば追加しない。
     * @param def       構造体変数定義
     * @param kernel_proc       カーネル親プロシージャ
     * @return        カーネル化モジュール
     */
    private List<Module> addKernelTypeDefinition(
                    VariableDefinition def,
                    ProgramUnit kernel_proc) {
        if (def == null) return null;
        if (!def.isStruct()) return null;

        List<Module> list = new ArrayList<Module>();
        // 構造体定義の探索
        List<jp.riken.kscope.language.fortran.Type> types = this.getAllTypeDefinition(def);
        if (types == null) return null;

        for (jp.riken.kscope.language.fortran.Type typedef : types) {
            ProgramUnit type_proc = typedef.getMotherBlock().getScopeDeclarationsBlock();
            if (type_proc == null) continue;

            // 構造体定義の追加
            Module kernel_mod = null;
            if (type_proc == kernel_proc) {
                // ローカルモジュールに追加
                kernel_mod = this.createKernelLocalModule();
            }
            else {
                if (type_proc.getBlockType() != BlockType.MODULE) {
                    type_proc = type_proc.getParentModule();
                }
                if (type_proc != null
                    && type_proc.getBlockType() == BlockType.MODULE) {
                    // カーネル化モジュールの追加
                    kernel_mod = this.addKernelModule((Module)type_proc);
                }
            }
            if (kernel_mod != null) {
                kernel_mod = this.addKernelTypeDefinition(kernel_mod, typedef);
                list.add(kernel_mod);
            }
        }

        if (list.size() <= 0) return null;

        return list;
    }


    /**
     * メンバを含めた構造体定義文リストを取得する
     * @param def       構造体変数定義
     * @return        構造体定義文リスト
     */
    private List<jp.riken.kscope.language.fortran.Type> getAllTypeDefinition(VariableDefinition def) {
        if (def == null) return null;
        if (!def.isStruct()) return null;

        List<jp.riken.kscope.language.fortran.Type> list = new ArrayList<jp.riken.kscope.language.fortran.Type>();
        // 構造体定義の探索
        ProgramUnit type_proc = this.searchStructDefinition(def, null);
        if (type_proc == null) return null;
        String def_name = def.getType().getName();
        jp.riken.kscope.language.fortran.Type typedef = type_proc.getType(def_name);
        if (typedef == null) return null;
        list.add(typedef);

        // メンバの構造体を検索する
        List<VariableDefinition> members = typedef.getDefinitions();
        if (members != null && members.size() > 0) {
            for (VariableDefinition mem : members) {
                if (!mem.isStruct()) continue;
                jp.riken.kscope.language.fortran.Type mem_typedef = ((VariableType)mem.getType()).getType();
                if (mem_typedef == null) continue;
                if (mem_typedef.hasRecursiveMember()) continue;

                String mem_typename = mem.getType().getName();
                boolean exists = false;
                for (jp.riken.kscope.language.fortran.Type type : list) {
                    if (mem_typename.equalsIgnoreCase(type.getName())) {
                        exists = true;
                        break;
                    }
                }
                if (!exists) {
                    List<jp.riken.kscope.language.fortran.Type> types = this.getAllTypeDefinition(mem);
                    if (types == null || types.size() <= 0) continue;
                    list.addAll(types);
                }
            }
        }

        if (list.size() <= 0) return null;

        return list;
    }

    /**
     * カーネル化モジュールに構造体定義文を追加する
     * 追加済みであれば追加しない。
     * @param module       カーネル化モジュール
     * @param type_def        構造体定義
     * @return        カーネル化モジュール
     */
    private Module addKernelTypeDefinition(
                    Module module,
                    Type type_def) {
        if (module == null) return null;
        if (type_def == null) return null;

        // 既存であるかチェックする
        Module kernel_mod = this.getKernelModule(module);
        if (kernel_mod == null) {
            kernel_mod = this.addKernelModule(module);
        }
        if (kernel_mod == null) return null;

        Type kernel_type = kernel_mod.getType(type_def.getName());
        if (kernel_type != null) return kernel_mod;

        // 構造体定義を追加する
        kernel_mod.addTypeDefinition(new Type(type_def));

        return kernel_mod;
    }


    /**
     * カーネル化モジュールにサブルーチン・関数を追加する
     * 追加済みであれば追加しない。
     * @param proc        サブルーチン・関数
     * @return        カーネル化モジュール
     */
    private Module addKernelProcedure(Procedure proc) {
        if (proc == null) return null;

        Procedure kernel_proc = new Procedure(proc);
        String proc_name = proc.get_name();
        // モジュール
        ProgramUnit def_proc = proc.get_mother();
        if (def_proc == null) return null;
        if (def_proc.getBlockType() == BlockType.PROCEDURE) {
            // 親副プログラムを追加する
            kernel_proc = new Procedure((Procedure)def_proc);
            // 内部副プログラム
            def_proc = def_proc.get_mother();
        }
        if (def_proc == null) return null;
        if (!(def_proc instanceof Module)) return null;

        Module kernel_mod = null;
        if (def_proc.getBlockType() == BlockType.MODULE) {
            if (Program.NO_MODULE.equalsIgnoreCase(def_proc.get_name())) {
                // NO_MODULE
                kernel_mod = this.addKernelNoModule(kernel_proc);
            }
            else {
                kernel_mod = this.addKernelModule((Module)def_proc);
            }
        }
        else {
            // NO_MODULE
            kernel_mod = this.addKernelNoModule(kernel_proc);
        }
        if (kernel_mod == null) return null;

        // 追加済みであるか
        if (kernel_mod.get_child(proc_name) != null) return kernel_mod;

        // サブルーチン・関数を追加する
        kernel_mod.put_child(kernel_proc);

        return kernel_mod;
    }


    /**
     * カーネル化ローカルモジュールにサブルーチン・関数を追加する
     * 追加済みであれば追加しない。
     * @param mod_name       カーネル化モジュール名
     * @param proc        サブルーチン・関数
     * @return        カーネル化モジュール
     */
    private Module addKernelLocalProcedure(Procedure proc) {
        if (proc == null) return null;

        Procedure kernel_proc = new Procedure(proc);
        String proc_name = proc.get_name();

        // ローカル変数モジュールにサブルーチン・関数の追加
        Module local_mod = this.createKernelLocalModule();
        if (local_mod == null) return null;

        // 追加済みであるか
        if (local_mod.get_child(proc_name) != null) return local_mod;

        // サブルーチン・関数を追加する
        local_mod.put_child(kernel_proc);

        return local_mod;
    }


    /**
     * カーネル化モジュールを追加する
     * モジュールを作成してカーネル化リストに追加する.
     * 追加済みであれば追加しない。追加済みモジュールを返す。
     * @param mod_name        モジュール名
     * @return        カーネル化モジュール
     */
    private Module addKernelModule(Module module) {
        if (module == null) return null;
        // 既存であるかチェックする
        Module kernel_mod = this.getKernelModule(module);
        if (kernel_mod != null) {
            return kernel_mod;
        }

        // モジュールを作成
        String mod_name = module.get_name();

        // NO_MODULEのファイル名
        SourceFile file = module.getStartCodeLine().getSourceFile();
        String ext = FileUtils.getFileExtension(file.getFile());
        CodeLine new_line = null;
        if (ext.equalsIgnoreCase("f")) {
            // f90拡張子に変更する
            File f90_file = FileUtils.changeFileExtension(file.getFile(), "f90");
            new_line = new CodeLine(
                                new SourceFile(f90_file),
                                module.getStartCodeLine().getStartLine(),
                                module.getStartCodeLine().getEndLine(),
                                null);
        }


        // カーネル化モジュールを追加
        kernel_mod = new Module(mod_name);
        if (this.kernel_modules == null) {
            this.kernel_modules = new ArrayList<Module>();
        }
        this.kernel_modules.add(kernel_mod);

        // ファイル、行情報をコピーする
        kernel_mod.copyStatements(module);
        if (new_line != null) {
            // ブロックのファイル名を設定する。
            kernel_mod.set_start(new_line);
            kernel_mod.set_end(new_line);
        }

        // include mpif.hを持つか
        if (module.hasIncludeMpi()) {
            String mpi_scope = module.getScopeIncludeMpi();
            kernel_mod.putIncludeMpi(mpi_scope);
        }

        // include omp_lib.hを持つか
        if (module.hasIncludeOmp()) {
            String omp_scope = module.getScopeIncludeOmp();
            kernel_mod.putIncludeOmp(omp_scope);
        }
        return kernel_mod;
    }

    /**
     * NO_MODULEのカーネル化モジュールを作成する。
     * @param block        ブロック
     * @return        NO_MODULEカーネル化モジュール
     */
    private Module addKernelNoModule(IBlock block) {
        if (block == null) return null;
        if (block.getStartCodeLine() == null) return null;
        if (block.getStartCodeLine().getSourceFile() == null) return null;
        if (block.getStartCodeLine().getSourceFile().getFile() == null) return null;

        Module no_module = new Module(Program.NO_MODULE);

        // NO_MODULEのファイル名
        SourceFile file = block.getStartCodeLine().getSourceFile();
        // f90拡張子に変更する
        File f90_file = FileUtils.changeFileExtension(file.getFile(), "f90");
        CodeLine line = new CodeLine(
                            new SourceFile(f90_file),
                            block.getStartCodeLine().getStartLine(),
                            block.getStartCodeLine().getEndLine(),
                            null);

        // ブロックのファイル名を設定する。
        no_module.set_start(line);
        no_module.set_end(line);

        Module kernel_mod = this.getKernelModule(no_module);
        if (kernel_mod != null) {
            return kernel_mod;
        }
        kernel_mod = this.addKernelModule(no_module);

        return kernel_mod;
    }


    /**
     * USE文の連携を探索してUSE文を追加する。
     * @throws Exception
     */
    private void setKernelUseStates(ProgramUnit kernel_proc) throws Exception {
        if (this.kernel_modules == null) return;

        for (Module mod : this.kernel_modules) {
            ProgramUnit src_proc = null;
            if (KernelProperties.MODULE_NAME_LOCAL.equalsIgnoreCase(mod.get_name())) {
                src_proc = kernel_proc;
                Module kernel_module = kernel_proc.getParentModule();
                String mod_name = kernel_module.get_name();
                Set<Variable> vars = mod.getAllVariables();
                if (vars != null) {
                    for (Variable var : vars) {
                        VariableDefinition def = var.getDefinition();
                        if (def == null) continue;
                        if (def.getScopeDeclarationsBlock() == null) continue;
                        if (def.getScopeDeclarationsBlock().getBlockType() != BlockType.MODULE) continue;
                        if (mod_name.equalsIgnoreCase(def.getScopeDeclarationsBlock().get_name())) {
                            mod.addUse(new UseState(mod_name));
                            break;
                        }
                    }
                }
            }
            else {
                src_proc = this.fortranDb.module(mod.get_name());
            }
            if (src_proc == null) continue;
            // 再帰検索用のスタック
            this.recursive.clear();
            while (src_proc != null) {
                List<UseState> chain_uses = this.searchChainUseStates(src_proc);
                if (chain_uses != null) {
                    // USE文を追加する
                    for (UseState use : chain_uses) {
                        UseState new_use = new UseState(use);
                        mod.addUse(new_use);
                    }
                }
                src_proc = (ProgramUnit)src_proc.getMotherBlock();
            }
        }

        // USE文の連携をチェックして重複USE文を削除する。
        this.normalizeUseStates();

    }

    /**
     * USE文の連携を探索してUSE文を検索する。
     * @param    search_proc    USE文探索プログラム
     */
    private List<UseState> searchChainUseStates(ProgramUnit search_proc) {
        if (search_proc == null) return null;
        if (this.kernel_modules == null) return null;
        List<UseState> search_uses = search_proc.getUseList();
        if (search_uses == null) return null;

        List<UseState> found_list = new ArrayList<UseState>();
        for (UseState use : search_uses) {
            for (Module mod : this.kernel_modules) {
                if (mod.get_name() == null) continue;
                if (Program.NO_MODULE.equalsIgnoreCase(mod.get_name())) continue;
                if (mod.get_name().equalsIgnoreCase(use.getModuleName())) {
                    found_list.add(use);
                }
            }
        }

        // 探索残のUSE文が存在するかチェックする。
        List<UseState> remain_list = new ArrayList<UseState>();
        for (UseState use : search_uses) {
            if (!found_list.contains(use)) {
                remain_list.add(use);
            }
        }
        if (remain_list.size() <= 0) {
            // 探索残のUSE文が存在しない。
            if (found_list.size() <= 0) return null;
            return found_list;
        }

        // 探索残のUSE文の先にカーネル化モジュールが存在するかチェックする
        for (UseState remain_use : remain_list) {
            String use_name = remain_use.getModuleName();
            if (this.recursive.contains(use_name)) continue;    // 再帰検索ブロック
            this.recursive.add(use_name);
            Module use_mod = this.fortranDb.module(use_name);
            if (use_mod == null) continue;
            List<UseState> list = this.searchChainUseStates(use_mod);
            if (list == null) continue;
            for (UseState use : list) {
                use_mod = this.fortranDb.module(use.getModuleName());
                if (use_mod == null) continue;
                boolean exists = false;
                for (UseState found_use : found_list) {
                    if (use.getModuleName().equalsIgnoreCase(found_use.getModuleName())) {
                        exists = true;
                        break;
                    }
                }
                if (!exists) {
                    found_list.add(use);
                }
            }
        }

        if (found_list.size() <= 0) return null;

        return found_list;
    }

    /**
     * USE文の連携をチェックして重複USE文を削除する。
     * @throws Exception
     */
    private void normalizeUseStates() throws Exception {
        if (this.kernel_modules == null) return;

        for (Module mod : this.kernel_modules) {
            List<UseState> uses = mod.getUseList();
            List<UseState> normalize_uses = normalizeUseStates(uses);
            mod.setUseList(normalize_uses);

            Procedure[] procs = mod.get_children();
            if (procs != null && procs.length > 0) {
                for (Procedure proc : procs) {
                    List<UseState> proc_uses = proc.getUseList();
                    List<UseState> normalize_procuses = this.normalizeUseStates(proc_uses);
                    proc.setUseList(normalize_procuses);
                }
            }
        }
    }

    /**
     * USE文の連携をチェックして重複USE文を削除する。
     * @throws Exception
     */
    private List<UseState> normalizeUseStates(List<UseState> uses) throws Exception {
        if (uses == null) return null;
        if (this.kernel_modules == null) return null;

        List<UseState> normalize_uses = new ArrayList<UseState>(uses);
        // 重複モジュールが存在していないかチェックする
        List<UseState> remove_uses = new ArrayList<UseState>();
        for (UseState src_use : uses) {
            String src_name = src_use.getModuleName();
            if (remove_uses.contains(src_use)) continue;
            // カーネルモジュールが存在しているか
            if (this.getKernelModule(src_name) == null) {
                // カーネルモジュールが存在していないので削除
                remove_uses.add(src_use);
                continue;
            }
            for (UseState dest_use : uses) {
                if (src_use == dest_use) continue;
                if (remove_uses.contains(dest_use)) continue;
                // 削除側の変数をマージする:モジュール名が異なればマージしない。
                String dest_name = dest_use.getModuleName();
                if (src_name.equalsIgnoreCase(dest_name)) {
                    src_use.margeUseState(dest_use);
                    remove_uses.add(dest_use);
                }
            }
        }
        if (remove_uses.size() > 0) {
            normalize_uses.removeAll(remove_uses);
        }

        if (normalize_uses.size() <= 0) return null;

        // USE文のONLY変数が存在しているかチェックする。
        Iterator<UseState> itr = normalize_uses.iterator();
        while (itr.hasNext()) {
            UseState use = itr.next();
            // USE文のONLY句をチェックしてモジュールに存在しない変数を削除する。
            if (normalizeUseStates(use) == null) {
                // ONLY句の変数がすべて存在しないので削除
                itr.remove();
                continue;
            }
        }

        return normalize_uses;
    }


    /**
     * USE文のONLY句をチェックしてモジュールに存在しない変数を削除する。
     * @throws Exception
     */
    private UseState normalizeUseStates(UseState use) throws Exception {
        if (use == null) return null;
        if (this.kernel_modules == null) return null;

        // USE文の変数が存在しているかチェックする。
        String mod_name = use.getModuleName();
        Module mod = this.getKernelModule(mod_name);
        if (mod == null) {
            // [エラー]カーネルモジュールが存在しません。
            String msg = Message.getString("kernel.validate.notfound_module.error.message");
            msg += "[modulename=" + mod_name + "]";
            this.addErrorInfo(msg);
            throw new Exception(msg);
        }

        Set<String> members = new HashSet<String>();
        Set<String> onlys = use.getOnlyMember();
        Map<String, String> trans = use.getTranslationNameReverse();
        if (onlys != null) members.addAll(onlys);
        if (trans != null) members.addAll(trans.keySet());
        if (members.size() <= 0) return use;

        Iterator<String> itr = members.iterator();
        while (itr.hasNext()) {
            String var_name = itr.next();
            if (!this.hasDefinition(mod, var_name)) {
                itr.remove();
                use.removeMember(var_name);
            }
        }

        if (members.size() <= 0) return null;

        return use;
    }

    /**
     * USE文の連携を探索してUSE文を検索する。
     */
    private List<UseState> getAllKernelUseStates(ProgramUnit kernel_mod) {
        if (kernel_mod == null) return null;
        if (this.kernel_modules == null) return null;
        List<UseState> search_uses = new ArrayList<UseState>();
        List<UseState> uses = kernel_mod.getUseList();
        if (uses != null) search_uses.addAll(uses);
        if (kernel_mod.get_children() != null) {
            for (Procedure proc : kernel_mod.get_children()) {
                uses = proc.getUseList();
                if (uses != null) search_uses.addAll(uses);
            }
        }
        if (search_uses.size() <= 0) return null;

        List<UseState> found_list = new ArrayList<UseState>();
        found_list.addAll(search_uses);

        // USE文の先のモジュールからUSE文の検索
        for (UseState use : search_uses) {
            String use_name = use.getModuleName();
            if (this.recursive.contains(use_name)) continue;    // 再帰検索ブロック
            this.recursive.add(use_name);
            Module mod = this.getKernelModule(use_name);
            List<UseState> list = this.getAllKernelUseStates(mod);
            if (list != null) {
                found_list.addAll(list);
            }
        }

        if (found_list.size() <= 0) return null;

        return found_list;
    }


    /**
     * カーネルのUSE文リストを作成する
     * @param kernel_blocks        カーネルブロック
     * @return    カーネルのUSE文リスト
     * @throws Exception
     */
    private List<UseState> createKernelUseStates(List<IBlock> kernel_blocks) throws Exception {
        if (kernel_blocks == null || kernel_blocks.size() <= 0) return null;
        if (this.kernel_modules == null) return null;

        // 抽出カーネルのプロシージャの取得
        ProgramUnit kernel_proc = kernel_blocks.get(0).getScopeDeclarationsBlock();
        String kernel_procname = kernel_proc.get_name();

        // USE文リスト
        List<UseState> use_list = new ArrayList<UseState>();
        ProgramUnit proc = kernel_proc;
        // 親モジュールを追加する
        Module parent_mod = proc.getParentModule();
        if (parent_mod != null) {
            String mod_name = parent_mod.get_name();
            // NO_MODULEは対象外
            if (!Program.NO_MODULE.equalsIgnoreCase(mod_name)) {
                if (this.containsKernelModule(mod_name)) {
                    UseState parent_use = new UseState(mod_name);
                    use_list.add(parent_use);
                }
            }
        }

        // 関数、モジュールのUSE文を取得する
        List<UseState> proc_uses = this.getScopeUseList(proc);
        if (proc_uses != null) {
            for (UseState use : proc_uses) {
                use_list.add(new UseState(use));
            }
        }

        // USE文のモジュールが存在するかチェックする
        Iterator<UseState> itr = use_list.iterator();
        while (itr.hasNext()) {
            UseState use = itr.next();
            String mod_name = use.getModuleName();
            if (this.containsKernelModule(mod_name)) {
                // ONLY句の存在しない変数の削除
                if (this.normalizeUseStates(use) == null) {
                    // ONLY句の変数がすべて存在しないので削除
                    itr.remove();
                    continue;
                }
            }
            else {
                // モジュールが存在しないので削除する
                itr.remove();
                continue;
            }
        }

        if (this.containsKernelModule(KernelProperties.MODULE_NAME_LOCAL)) {
            // ローカルモジュール追加する
            UseState local_use = new UseState(KernelProperties.MODULE_NAME_LOCAL);
            use_list.add(local_use);
        }
        if (use_list.size() <= 0) return null;

        // カーネルのUSE文リスト
        this.kernel_uselist = new ArrayList<UseState>();
        this.kernel_uselist.addAll(use_list);

        return use_list;
    }


    /**
     * ローカル変数の属性を追加、削除する.
     * ローカル変数をモジュール変数定義に変更する
     * @param def       ローカル変数宣言文
     * @return        変数宣言文
     */
    private VariableDefinition setLocal2ModuleDefinition(VariableDefinition def) {
        if (def == null) return null;

        VariableAttribute def_attr = (VariableAttribute)def.getAttribute();
        VariableDefinition module_def = new VariableDefinition(def);
        // public属性追加
        String attr = VariableAttribute.ScopeAttribute.PUBLIC.toString().toLowerCase();
        module_def.addVariableAttributes(attr);
        // private属性削除
        attr = VariableAttribute.ScopeAttribute.PRIVATE.toString().toLowerCase();
        module_def.removeVariableAttributes(attr);
        // save属性追加
        // if (def_attr == null || !def_attr.hasParameter()) {
        //    attr = VariableAttribute.ATTRIBUTE_SAVE.toLowerCase();
        //    module_def.addVariableAttributes(attr);
        //}
        // intent属性削除
        attr = VariableAttribute.ATTRIBUTE_INTENT.toLowerCase();
        module_def.removeVariableAttributes(attr);
        // OPTIONAL属性削除
        attr = VariableAttribute.ATTRIBUTE_OPTIONAL.toLowerCase();
        module_def.removeVariableAttributes(attr);
        // pointer
        boolean has_pointer = false;
        if (def_attr != null) {
            has_pointer = def_attr.hasPointer();
        }
        // 配列パラメータ
        VariableDimension dim = module_def.getVariableDimension();
        if (dim != null && dim.getIndex() != null) {
            Set<Variable> vars = dim.getAllVariables();
            boolean has_parameter = false;
            if (vars != null) {
                // 配列インデックスがparameterであるかチェックする
                for (Variable var : vars) {
                    VariableDefinition var_def = var.getDefinition();
                    if (var_def == null) continue;
                    if (var_def.getAttribute() == null) continue;
                    if (((VariableAttribute)var_def.getAttribute()).hasParameter()) {
                        has_parameter = true;
                    }
                    else {
                        has_parameter = false;
                        break;
                    }
                }
            }
            else {
                DimensionIndex[] indexes = dim.getIndex();
                for (DimensionIndex idx : indexes) {
                    if (idx == null) continue;
                    if (idx.toString() != null && !idx.toString().isEmpty()) {
                        has_parameter = true;
                        break;
                    }
                }
            }
            if (!has_parameter) {
                // 配列変数の削除
                dim.clearIndices();

                // pointerにはallocatable属性追加不可
                if (!has_pointer) {
                    // allocatable属性追加
                    String alloc_attr = VariableAttribute.ATTRIBUTE_ALLOCATABLE.toString().toLowerCase();
                    module_def.addVariableAttributes(alloc_attr);
                }
            }
        }

        // characterのlen=*
        if (def.getType() != null) {
            VariableType var_type = (VariableType)def.getType();
            if (var_type.getPrimitiveDataType() == PrimitiveDataType.CHARACTER) {
                if (var_type.getLen() != null) {
                    String len = var_type.getLen().getLine();
                    if (len == "*") {
                        // allocatable属性追加
                        // String alloc_attr = VariableAttribute.ATTRIBUTE_ALLOCATABLE.toString().toLowerCase();
                        // module_def.addVariableAttributes(alloc_attr);
                    }
                }
            }
        }

        // real(kind(0d0))をreal(8)にする
        normalizeDefinition(module_def);

        return module_def;
    }


    /**
     * モジュールの変数のソート、変数定義の属性変更を行う。
     * real(kind(0d0))をreal(8)にする
     */
    private void normalizeDefinition() {
        if (this.kernel_modules == null) return;

        // 変数のソートを行う
        for (Module mod : this.kernel_modules) {
            VariableDefinition[] defs = mod.get_variables();
            if (defs == null || defs.length <= 0) continue;
            List<VariableDefinition> def_list = new ArrayList<VariableDefinition>();
            def_list.addAll(Arrays.asList(defs));
            // ソートを行う
            this.sortVariableDefinition(def_list);
            mod.clear_variable();
            for (VariableDefinition def : def_list) {
                // real(kind(0d0))をreal(8)にする
                def = normalizeDefinition(def);
                mod.put_variable(def);
            }
        }

        // ローカルモジュールの変数属性変更
        Module local_mod = this.getKernelLocalModule();
        if (local_mod != null) {
            this.normalizeDefinition(local_mod);
        }

        // カーネルの親モジュールの変数属性変更
        if (this.kernel_parent_module != null) {
            this.normalizeDefinition(this.kernel_parent_module);
        }

        return;
    }


    /**
     * モジュールの変数のソート、変数定義の属性変更を行う。
     * real(kind(0d0))をreal(8)にする
     * @param   module    変数定義の属性変更を行うモジュール
     */
    private void normalizeDefinition(Module module) {
        if (module == null) return;

        // モジュールの変数をグローバル変数にする
        VariableDefinition[] defs = module.get_variables();
        if (defs != null && defs.length > 0) {
            module.clear_variable();
            for (int i=0; i<defs.length; i++) {
                VariableDefinition global_def = this.setLocal2ModuleDefinition(defs[i]);
                if (global_def == null) continue;
                module.put_variable(global_def);
            }
        }

        return;
    }

    /**
     * real(kind(0d0))をreal(8)にする
     */
    private VariableDefinition normalizeDefinition(VariableDefinition def) {
        if (def == null) return null;

        // real(kind(0d0))をreal(8)にする
        VariableType type = (VariableType)def.getVariableType();
        if (type != null && type.isKindDouble()) {
            type.setKind(new Expression("8"));
        }

        return def;
    }

    /**
     * VelocityEngineを生成する
     * @return        生成VelocityEngine
     */
    private VelocityEngine factoryVelocityEngine() {
        java.net.URL url = this.properties.getTemplateUrl();


        //Velocityの初期化
        VelocityEngine velocity = new VelocityEngine();

        // テンプレートの検索パスを設定する
        velocity.setProperty(RuntimeConstants.RESOURCE_LOADER, "file,jar");

        // リソースローダー：ローカルファイル
        try {
            String file_resource_path = null;
            File resource_file = new File(url.toURI());
            if (resource_file.exists()) {
                file_resource_path = resource_file.getPath();
            }
            if (file_resource_path != null) {
                velocity.setProperty("file.resource.loader.class", "org.apache.velocity.runtime.resource.loader.FileResourceLoader");
                velocity.setProperty("file.resource.loader.path", file_resource_path);
                // for debug
                // System.out.println("factoryVelocityEngine::file_resource_path = " + file_resource_path);
            }
        } catch (Exception ex) {}

        // リソースローダー：JARファイル
        try {
            java.net.URL jar_url = ResourceUtils.getKscopeJarUrl();
            File jar_file = null;
            if (jar_url != null) {
                jar_file = new File(jar_url.toURI().getPath());
            }
            if (jar_file != null && jar_file.exists() && jar_file.isFile()) {
                velocity.setProperty("jar.resource.loader.class", "org.apache.velocity.runtime.resource.loader.JarResourceLoader");
                String jar_path = jar_url.getPath();
                velocity.setProperty("jar.resource.loader.path", "jar:file:" + jar_path);
                // for debug
                // System.out.println("factoryVelocityEngine::jar_url.getPath() = " + jar_path);
            }
        } catch (Exception ex) { }

        velocity.init();

        return velocity;
    }

    /**
     * LineFormattedWriterを生成する
     * @param outdir
     * @param mod_file
     * @return
     */
    private FortranFormattedWriter factoryFileWriter(
                    File outdir,
                    File mod_file) {

        if (outdir == null) return null;
        if (outdir.getPath() == null) return null;
        if (outdir.getPath().isEmpty()) return null;
        if (mod_file == null) return null;
        if (mod_file.getPath() == null) return null;
        if (mod_file.getPath().isEmpty()) return null;

        // 出力F90書式設定
        FortranFormattedWriter fileWriter = new FortranFormattedWriter();
        fileWriter.setLimitColumn(this.properties.getLimitColumn());
        fileWriter.addComments(this.properties.getLanguageCommnet());

        // 出力ファイル
        String outfile = outdir.getPath() + File.separator + mod_file.getPath();
        fileWriter.setOutputFilename(outfile);

        return fileWriter;
    }

    /**
     * VelocityContextを生成する
     * @return        生成VelocityEngine
     */
    private KernelContext factoryVelocityContext() {
        KernelContext context = new KernelContext();
        // インデント
        int indent = KernelProperties.DEFAULT_INDELT_COLUMN;
        if (this.properties != null && this.properties.getIndent() > 0) {
            indent = this.properties.getIndent();
        }
        String indent_space = StringUtils.repeat(" ", indent);
        context.put("indent_space", indent_space);

        // 現在日時
        String current_time = StringUtils.getCurrentDateTimeIso8601();
        context.put("current_time", current_time);

        // カーネルプロパティ
        context.setProperties(this.properties);

        return context;
    }


    /**
     * カーネルの検証を行う
     * @return        true=検証成功
     */
    private boolean validateKernel(List<IBlock> kernel_blocks) {
        if (this.kernel_modules == null) return false;
        if (this.fileio_definitions == null) return false;

        // すべてのモジュール変数の取得
        List<VariableDefinition> def_list = this.getAllKernelDefinitions();
        if (def_list == null) return false;

        if (this.undefined_call != null && this.undefined_call.size() > 0) {
            for (ProcedureUsage call : this.undefined_call) {
                String name = call.getCallName();
                name = name.toLowerCase();
                if (call.isIntrinsic()) continue;
                // MPI, OMP関数は除く
                if (this.properties != null) {
                    if (this.properties.isExcludeUndefinedCall(name)) {
                        continue;
                    }
                }
                // [エラー]サブルーチン・関数の定義がありません。
                String msg = Message.getString("kernel.validate.undefined_call.error.message");
                msg += "[call=" + call.getCallName() + "]";
                this.addErrorInfo(call.getStartCodeLine(), msg);
            }
        }

        // ファイル入出力変数ではない変数の検索
        List<VariableDefinition> noneinit_defs = new ArrayList<VariableDefinition>();
        List<VariableDefinition> pointer_defs = new ArrayList<VariableDefinition>();
        List<VariableDefinition> unlen_defs = new ArrayList<VariableDefinition>();
        for (VariableDefinition def : def_list) {
            VariableAttribute attr = (VariableAttribute) def.getAttribute();
            VariableType type = (VariableType) def.getVariableType();
            if (type == null) continue;
            // サブルーチン・関数型は除外
            if (type.getPrimitiveDataType() == PrimitiveDataType.VOID) continue;
            // external属性は除外
            if (def.hasExternal()) continue;
            // PARAMETER属性の変数は除外
            if (attr.hasParameter()) {
                continue;
            }
            // POINTER属性の変数
            if (attr.hasPointer()) {
                pointer_defs.add(def);
            }
            boolean exists_fileio = false;
            for (VariableDefinition fileio_def : this.fileio_definitions) {
                if (this.equalsVariableDefinitions(def, fileio_def)) {
                    exists_fileio = true;
                    break;
                }
            }
            if (!exists_fileio) {
                // 初期値が設定されていないこと
                if (def.getInitValue() == null) {
                    noneinit_defs.add(def);
                }
            }

            // character(len=*)
            if (type.getPrimitiveDataType() == PrimitiveDataType.CHARACTER) {
                if (type.getLen() != null) {
                    String len = type.getLen().getLine();
                    if (len == "*") {
                        unlen_defs.add(def);
                    }
                }
            }
        }

        // 変数初期化警告・注意メッセージ
        for (VariableDefinition var_def : noneinit_defs) {
            // 変数が左辺で初期化されているか？
            if (this.isLeftVariable(var_def)) {
                // [注意]変数が初期化されていない可能性があります。
                String msg = Message.getString("kernel.validate.initialize.attention.message");
                msg += "[variable=" + var_def.get_name() + "]";
                this.addAttentionInfo(var_def.getStartCodeLine(), msg);
            }
            else {
                // [警告]変数が初期化されていません。
                String msg = Message.getString("kernel.validate.initialize.warning.message");
                msg += "[variable=" + var_def.get_name() + "]";
                this.addWarningInfo(var_def.getStartCodeLine(), msg);
            }
        }

        // ローカル構造体定義が使用されているか？
        List<jp.riken.kscope.language.fortran.Type> local_types = this.getLocalTypes(kernel_blocks);
        if (local_types != null && local_types.size() > 0) {
            // [警告]ローカルの構造体型定義の為SEQUENCE文を追加しました。アプリケーション側にもSEQUENCE文を追加してください。
            String msg = Message.getString("kernel.validate.typedefinition.warning.message");
            for (jp.riken.kscope.language.fortran.Type type : local_types) {
                if (type.isSequence()) continue;
                this.addWarningInfo(type.getStartCodeLine(), msg);
                type.setSequence(true);
            }
        }

        if (this.recursive_type != null && this.recursive_type.size() > 0) {
            for (VariableDefinition def : this.recursive_type) {
                // [警告]自己参照の構造体はファイル入出力をサポートしません。
                String msg = Message.getString("kernel.validate.recursive_type.warning.message");
                msg += "[variable=" + def.get_name() + "]";
                this.addWarningInfo(def.getStartCodeLine(), msg);
            }
        }

        // character(len=*)
        for (VariableDefinition var_def : unlen_defs) {
            // [警告]character変数のlengthが不定です。
            String msg = Message.getString("kernel.validate.character_length.warning.message");
            msg += "[variable=" + var_def.get_name() + "]";
            this.addAttentionInfo(var_def.getStartCodeLine(), msg);
        }

        // allocate文が存在するか
        List<IBlock> allocates = this.getAllocateBlocks(kernel_blocks);
        if (allocates != null && allocates.size() > 0) {
            // [注意]allocate文が使用されています。再実行に注意してください。
            String msg = Message.getString("kernel.validate.allocate.attention.message");
            for (IBlock block : allocates) {
                this.addAttentionInfo(block.getStartCodeLine(), msg);
            }
        }

        // ポインタ使用メッセージ
        for (VariableDefinition var_def : pointer_defs) {
            // [注意]ポインタ変数は実行結果が異なる可能性があります。
            String msg = Message.getString("kernel.validate.pointer.attention.message");
            msg += "[variable=" + var_def.get_name() + "]";
            this.addAttentionInfo(var_def.getStartCodeLine(), msg);
        }

        // include mpif.hを追加するか
        boolean has_includempi = this.hasIncludeMpi();
        if (has_includempi) {
            // [情報]インクルード文を追加しました。
            String msg = Message.getString("kernel.validate.include.information.message");
            msg += "[include 'mpif.h']";
            this.addInformationInfo(msg);
        }

        // include omp_lib.hを追加するか
        boolean has_includeomp = this.hasIncludeOmp();
        if (has_includeomp) {
            // [情報]インクルード文を追加しました。
            String msg = Message.getString("kernel.validate.include.information.message");
            msg += "[include 'omp_lib.h']";
            this.addInformationInfo(msg);
        }

        return true;
    }

    /**
     * 変数が左辺にて使用されているかチェックする
     * @param var_def        変数定義
     * @return        true=変数が左辺で使用されている
     */
    private boolean isLeftVariable(VariableDefinition var_def) {
        if (var_def == null) return false;
        if (this.source_variables == null) return false;

        for (Variable var : this.source_variables) {
            VariableDefinition src_def = var.getDefinition();
            if (src_def == null) continue;
            if (src_def.isStruct() || src_def.isStructMember()) {
                // 構造体は親構造体定義を取得する
                src_def = src_def.getStructDefinition();
            }
            // 同一変数であるか？
            if (!this.equalsVariableDefinitions(var_def, src_def)) continue;

            IBlock parent = var.getParentStatement();
            if (parent != null
                && parent.getBlockType() == BlockType.SUBSTITUTION) {
                if (((Substitution)parent).isLeftVariable(var.getName())) {
                    return true;
                }
            }
        }
        return false;
    }


    /**
     * スコープのチェックを行う.
     * 選択カーネルプロシージャ
     * @param kernel_uses        カーネルUSE文
     * @param var_def        変数定義文
     * @return        true=スコープあり
     */
    private boolean validateScope(
                    List<UseState> kernel_uses,
                    VariableDefinition var_def) {
        if (kernel_uses == null || kernel_uses.size() <= 0) return false;
        if (var_def == null) return false;

        for (UseState use : kernel_uses) {
            if (this.validateScope(use, var_def)) {
                return true;
            }
        }
        return false;
    }


    /**
     * スコープのチェックを行う.
     * 選択カーネルプロシージャ
     * @param kernel_uses        カーネルUSE文
     * @param var_def        変数定義文
     * @return        true=スコープあり
     */
    private boolean validateScope(
                    UseState kernel_use,
                    VariableDefinition var_def) {
        if (kernel_use == null) return false;
        if (var_def == null) return false;

        String var_name = var_def.get_name();
        String use_modname = kernel_use.getModuleName();
        Module use_mod = this.getKernelModule(use_modname);
        if (use_mod == null) return false;

        VariableAttribute attr = (VariableAttribute) var_def.getAttribute();
        // PRIVATE属性の変数は除外
        if (attr != null && attr.hasPrivate()) {
            return false;
        }

        // 変数定義と同一モジュールであるか
        VariableDefinition mod_def = null;
        Set<String> onlys = kernel_use.getOnlyMember();
        boolean child_serch = false;
        if (onlys == null || onlys.size() <= 0) {
            // モジュールから変数検索
            mod_def = use_mod.get_variable(var_name);
            child_serch = true;
        }
        else {
            for (String only_name : onlys) {
                if (!only_name.equalsIgnoreCase(var_name)) continue;
                mod_def = use_mod.get_variable(var_name);
                // ONLY句がある場合はチェックを行う変数がONLY句に存在しなければならない。
                child_serch = true;
                break;
            }
        }

        if (mod_def != null) {
            // 同一変数定義であるかチェックする
            if (this.equalsVariableDefinitions(var_def, mod_def)) {
                // 同一変数
                return true;
            }
        }

        if (child_serch) {
            List<UseState> mod_uses = use_mod.getUseList();
            if (this.validateScope(mod_uses, var_def)) {
                return true;
            }
        }

        return false;
    }

    /**
     * 同一変数定義文であるかチェックする。
     * 変数名、モジュール名が同一であるかチェックする
     * @param def1        変数定義文1
     * @param def2        変数定義文3
     * @return        true=同一変数定義
     */
    private boolean equalsVariableDefinitions(VariableDefinition def1, VariableDefinition def2) {
        if (def1 == null) return false;
        if (def2 == null) return false;

        // 同一変数定義であるかチェックする
        String varname1 = def1.get_name();
        String varname2 = def2.get_name();
        if (!varname1.equalsIgnoreCase(varname2)) {
            if (this.kernel_uselist != null) {
                String rename1 = varname1;
                String rename2 = varname2;
                for (UseState use : this.kernel_uselist) {
                    rename1 = use.translation(def1);
                    rename2 = use.translation(def2);
                    if (!varname1.equals(rename1) || !varname2.equals(rename2)) {
                        break;
                    }
                }
                if (!rename1.equalsIgnoreCase(rename2)) {
                    return false;
                }
            }
        }

        String modname1 = null;
        String modname2 = null;
        if (def1.getScopeDeclarationsBlock() != null)
            modname1 = def1.getScopeDeclarationsBlock().get_name();
        if (def2.getScopeDeclarationsBlock() != null)
            modname2 = def2.getScopeDeclarationsBlock().get_name();
        if (modname1 != null) {
            if (!modname1.equalsIgnoreCase(modname2)) return false;
        }

        // 同一変数
        return true;
    }

    /**
     * エラーメッセージを設定する
     * @param errorMessage         エラーメッセージ
     */
    public void addErrorInfo(String errorMessage) {
        super.addErrorInfo(errorMessage);
        if (this.error_messages == null) {
            this.error_messages = new ArrayList<String>();
        }
        this.error_messages.add(errorMessage);
    }

    /**
     * エラーメッセージを設定する
     * @param line         エラー行情報
     * @param warningMessage         エラーメッセージ
     */
    public void addErrorInfo(CodeLine line, String errorMessage) {
        super.addErrorInfo(line, errorMessage);
        if (this.error_messages == null) {
            this.error_messages = new ArrayList<String>();
        }
        this.error_messages.add(errorMessage);
    }

    /**
     * 警告メッセージを設定する
     * @param warningMessage         警告メッセージ
     */
    public void addWarningInfo(String warningMessage) {
        super.addErrorInfo(warningMessage);
        if (this.warning_messages == null) {
            this.warning_messages = new ArrayList<String>();
        }
        this.warning_messages.add(warningMessage);
    }

    /**
     * 警告メッセージを設定する
     * @param line         エラー行情報
     * @param warningMessage         警告メッセージ
     */
    public void addWarningInfo(CodeLine line, String warningMessage) {
        super.addErrorInfo(line, warningMessage);
        if (this.warning_messages == null) {
            this.warning_messages = new ArrayList<String>();
        }
        this.warning_messages.add(warningMessage);
    }

    /**
     * 注意メッセージを設定する
     * @param attentionMessage         注意メッセージ
     */
    public void addAttentionInfo(String attentionMessage) {
        super.addErrorInfo(attentionMessage);
        if (this.attention_messages == null) {
            this.attention_messages = new ArrayList<String>();
        }
        this.attention_messages.add(attentionMessage);
    }


    /**
     * 注意メッセージを設定する
     * @param line         エラー行情報
     * @param attentionMessage         注意メッセージ
     */
    public void addAttentionInfo(CodeLine line, String attentionMessage) {
        super.addErrorInfo(line, attentionMessage);
        if (this.attention_messages == null) {
            this.attention_messages = new ArrayList<String>();
        }
        this.attention_messages.add(attentionMessage);
    }

    /**
     * 情報メッセージを設定する
     * @param message         情報メッセージ
     */
    public void addInformationInfo(String message) {
        super.addErrorInfo(message);
        if (this.information_messages == null) {
            this.information_messages = new ArrayList<String>();
        }
        this.information_messages.add(message);
    }


    /**
     * 情報メッセージを設定する
     * @param line         エラー行情報
     * @param message         情報メッセージ
     */
    public void addInformationInfo(CodeLine line, String message) {
        super.addErrorInfo(line, message);
        if (this.information_messages == null) {
            this.information_messages = new ArrayList<String>();
        }
        this.information_messages.add(message);
    }


    /**
     * VariableDefinitionのソートを行う.
     * @param list
     * @return
     */
    private List<VariableDefinition> sortVariableDefinition(List<VariableDefinition> list) {

        // ソートを行う
        list = LanguageUtils.sortVariableDefinition(list);
        if (list == null || list.size() <= 0) return null;

        // ローカル変数を先頭にする
        Collections.sort(list, new Comparator<VariableDefinition>(){
            public int compare(VariableDefinition block1, VariableDefinition block2) {
                if (block1 == null) return 0;
                if (block2 == null) return 0;
                String name1 = block1.get_name();
                String name2 = block2.get_name();
                ProgramUnit proc1 = block1.getScopeDeclarationsBlock();
                ProgramUnit proc2 = block2.getScopeDeclarationsBlock();
                if (proc1 == null) return 0;
                if (proc2 == null) return 0;
                String proc1_name = proc1.get_name();
                String proc2_name = proc2.get_name();
                if (proc1_name.equalsIgnoreCase(proc2_name)) return 0;

                if (KernelProperties.MODULE_NAME_LOCAL.equalsIgnoreCase(proc1_name)) {
                    return -1;
                }
                if (KernelProperties.MODULE_NAME_LOCAL.equalsIgnoreCase(proc2_name)) {
                    return +1;
                }

                // カーネルUSE文順番にソートする
                if (KernelService.this.kernel_uselist != null) {
                    int use1_idx = -1;
                    int use2_idx = -1;
                    for (int i=0; i<KernelService.this.kernel_uselist.size(); i++) {
                        UseState use = KernelService.this.kernel_uselist.get(i);
                        String use_name = use.getModuleName();
                        if (proc1_name.equalsIgnoreCase(use_name)) use1_idx = i;
                        if (proc2_name.equalsIgnoreCase(use_name)) use2_idx = i;
                        if (use1_idx != -1 && use2_idx != -1) {
                            break;
                        }
                    }
                    if (use1_idx == -1) return +1;
                    if (use2_idx == -1) return -1;
                    return (use1_idx - use2_idx);
                }

                return 0;
            }
        });

        return list;
    }

    /**
     * エラーが発生しているかチェックする
     * @return        true=エラー発生
     */
    public boolean existsErrorInfo() {
        if (this.error_messages == null) return false;
        return (this.error_messages.size() > 0);
    }

    /**
     * 入出力構造体定義のみのUSE文、PARAMETERのみのUSE文を作成する
     * @return
     */
    private List<UseState> createFileioUses() {
        if (this.fileio_definitions == null) return null;

        List<UseState> list = new ArrayList<UseState>();
        // motherに宣言Moduleが設定されているType文リスト
        List<jp.riken.kscope.language.fortran.Type> type_defs = this.getFileioTypes();
        if (type_defs != null) {
            for (jp.riken.kscope.language.fortran.Type type : type_defs) {
                IBlock mod = type.getMotherBlock();
                if (mod == null) continue;
                if (mod.getBlockType() != BlockType.MODULE) continue;

                // USE文を作成する
                String mod_name = ((Module)mod).get_name();
                String type_name = type.getName();

                boolean exists = true;
                UseState use = this.getUseState(list, mod_name);
                if (use == null) {
                    use = new UseState();
                    exists = false;
                }
                use.setModuleName(mod_name);
                use.addOnlyMember(type_name);
                if (!exists) list.add(use);

                // TYPE定義文にPUBLIC属性を付加する
                jp.riken.kscope.language.fortran.Type mod_type = ((Module)mod).getType(type_name);
                if (mod_type != null) {
                    mod_type.setPublic(true);
                }
            }
        }

        // カーネルファイル入出力変数定義文ないのPARAMETER変数を取得する
        TreeMap<String, VariableDefinition> params = new TreeMap<String, VariableDefinition>();
        for (VariableDefinition def : this.fileio_definitions) {
            Set<Variable> vars = def.getAllVariables();
            if (vars == null) continue;
            for (Variable var : vars) {
                if (var == null) continue;
                VariableDefinition param_def = var.getDefinition();
                if (param_def == null) continue;
                String param_name = param_def.get_name();
                if (param_def.getAttribute() == null) continue;
                VariableAttribute attr = (VariableAttribute)param_def.getAttribute();
                if (!attr.hasParameter()) continue;
                params.put(param_name, param_def);
            }
        }
        for (VariableDefinition param_def : params.values()) {
            String name = param_def.get_name();
            IBlock mod = param_def.getMotherBlock();
            if (mod == null) continue;
            if (mod.getBlockType() != BlockType.MODULE) continue;

            // USE文を作成する
            String mod_name = ((Module)mod).get_name();
            boolean exists = true;
            UseState use = this.getUseState(list, mod_name);
            if (use == null) {
                use = new UseState();
                exists = false;
            }
            use.setModuleName(mod_name);
            use.addOnlyMember(name);
            if (!exists) list.add(use);
        }

        if (list.size() <= 0) return null;

        return list;
    }

    private UseState getUseState(List<UseState> list, String mod_name) {
        if (list == null || list.size() <= 0) return null;
        if (mod_name == null) return null;
        for (UseState use : list) {
            String use_name = use.getModuleName();
            if (mod_name.equals(use_name)) {
                return use;
            }
        }
        return null;
    }

    /**
     * 入出力構造体定義一覧を取得する
     * @return
     */
    private List<jp.riken.kscope.language.fortran.Type> getFileioTypes() {
        if (this.fileio_definitions == null) return null;

        List<jp.riken.kscope.language.fortran.Type> list = new ArrayList<jp.riken.kscope.language.fortran.Type>();
        List<jp.riken.kscope.language.fortran.Type> types = new ArrayList<jp.riken.kscope.language.fortran.Type>();
        for (VariableDefinition def : this.fileio_definitions) {
            List<jp.riken.kscope.language.fortran.Type> mem_types = this.getAllTypeDefinition(def);
            if (mem_types == null) continue;

            List<ProgramUnit> parent_procs = new ArrayList<ProgramUnit>();
            ProgramUnit def_proc = def.getScopeDeclarationsBlock();
            if (def_proc == null) continue;
            parent_procs.add(def_proc);
            if (def_proc != def.getParentProgram()) {
                parent_procs.add(def.getParentProgram());
            }

            for (jp.riken.kscope.language.fortran.Type mem : mem_types) {
                jp.riken.kscope.language.fortran.Type type_def = this.setTypeWithModule(mem, parent_procs);
                if (type_def == null) continue;
                types.add(type_def);
            }
        }

        if (types == null || types.size() <= 0) return null;

        for (jp.riken.kscope.language.fortran.Type type_def : types) {
            // 追加済みであるか
            boolean exists = false;
            for (jp.riken.kscope.language.fortran.Type item : list) {
                String type_name = item.getName();
                if (type_name.equalsIgnoreCase(type_def.getName())) {
                    exists = true;
                    break;
                }
            }
            if (!exists) {
                list.add(type_def);
            }
        }

        if (list.size() <= 0) return null;

        return list;
    }


    /**
     * 入出力構造体定義一覧を取得する
     * @return
     */
    private List<VariableDefinition> getFileioVariableDefinitionOfType() {
        if (this.fileio_definitions == null) return null;

        List<VariableDefinition> list = new ArrayList<VariableDefinition>();
        for (VariableDefinition def : this.fileio_definitions) {
            if (!def.isStruct()) continue;
            VariableType var_type = (VariableType)def.getType();
            if (var_type == null) continue;
            jp.riken.kscope.language.fortran.Type type_def = var_type.getType();
            if (type_def == null) continue;
            list.add(def);
        }
        if (list.size() <= 0) return null;

        return list;
    }


    /**
     * 構造体の定義モジュール、手続を設定する.
     * 定義モジュール、手続がない場合はnullを返す.
     * @return    定義モジュール設定済み構造体
     */
    private
    jp.riken.kscope.language.fortran.Type setTypeWithModule(
                    VariableDefinition def,
                    List<ProgramUnit> parent_procs) {
        if (this.kernel_modules == null) return null;
        if (def == null) return null;
        if (!def.isStruct()) return null;
        VariableType var_type = (VariableType)def.getType();
        if (var_type == null) return null;
        jp.riken.kscope.language.fortran.Type type_def = var_type.getType();
        if (type_def == null) return null;
        if (type_def.getName() == null) return null;

        return setTypeWithModule(type_def, parent_procs);
    }

    /**
     * 構造体の定義モジュール、手続を設定する.
     * 定義モジュール、手続がない場合はnullを返す.
     * @return    定義モジュール設定済み構造体
     */
    private
    jp.riken.kscope.language.fortran.Type setTypeWithModule(
                jp.riken.kscope.language.fortran.Type type_def,
                List<ProgramUnit> parent_procs) {
        if (this.kernel_modules == null) return null;
        if (type_def == null) return null;
        if (type_def.getName() == null) return null;

        List<ProgramUnit> scope_procs = new ArrayList<ProgramUnit>();
        scope_procs.addAll(parent_procs);
        for (ProgramUnit proc : parent_procs) {
            List<UseState> uses = this.getAllKernelUseStates(proc);
            if (uses == null) continue;
            for (UseState use : uses) {
                String mod_name = use.getModuleName();
                Module mod = this.getKernelModule(mod_name);
                if (mod != null) {
                    scope_procs.add(mod);
                }
            }
        }

        // 構造体定義の親モジュールを検索する
        jp.riken.kscope.language.fortran.Type type = type_def.clone();
        for (ProgramUnit scope_proc : scope_procs) {
            List<jp.riken.kscope.language.fortran.Type> mod_types = scope_proc.getTypeList();
            if (mod_types == null || mod_types.size() <= 0) continue;
            for (jp.riken.kscope.language.fortran.Type mod_type : mod_types) {
                String type_name = mod_type.getName();
                if (type.getName().equalsIgnoreCase(type_name)) {
                    type.setMotherBlock(scope_proc);
                }
            }
        }

        return type;
    }

    /**
     * Module内のすべてのUSE文を取得する。
     * @param   proc      取得プログラム
     */
    private List<UseState> getAllUseStates(ProgramUnit proc) {
        if (proc == null) return null;

        List<UseState> list = new ArrayList<UseState>();
        List<UseState> uses = proc.getUseList();
        if (uses != null) {
            list.addAll(uses);
        }

        // 副プログラムから検索する
        Procedure childs[] = proc.get_children();
        if (childs != null) {
            for (Procedure child : childs) {
                List<UseState> child_uses = this.getAllUseStates(child);
                if (child_uses != null) {
                    list.addAll(child_uses);
                }
            }
        }

        if (list.size() <= 0) return null;

        return list;
    }


    /**
     * KernelBlocks内のすべてのUSE文を取得する。
     * @param   kernel_blocks      カーネルブロック群
     */
    private List<UseState> getAllUseStates(KernelBlocks kernel_blocks) {
        if (kernel_blocks == null) return null;

        List<UseState> list = new ArrayList<UseState>();
        for (KernelBlock block : kernel_blocks) {
            if (block.getKernelBlock() instanceof ProgramUnit) {
                List<UseState> uses = this.getAllKernelUseStates((ProgramUnit)block.getKernelBlock());
                if (uses != null) {
                    list.addAll(uses);
                }
            }
        }

        if (list.size() <= 0) return null;

        return list;
    }


    /**
     * DATA文を追加する。
     * モジュールのDATA文のみ追加する。
     * サブルーチン・関数のDATA文はサブルーチン・関数単位でコピーしているので追加する必要はない。
     * @throws Exception
     */
    private void setKernelDataList() throws Exception {
        if (this.kernel_modules == null) return;

        for (Module mod : this.kernel_modules) {
            // ローカルモジュールは対象外
            ProgramUnit src_proc = this.fortranDb.module(mod.get_name());
            if (src_proc == null) continue;

            List<jp.riken.kscope.language.Data> src_datas = src_proc.getDataList();
            if (src_datas == null || src_datas.size() <= 0) continue;

            // 変数定義されているDATA文のみ返す。
            for (jp.riken.kscope.language.Data data : src_datas ) {
                List<Variable> var_list = data.getVariables();
                List<Expression> value_list = data.getValues();
                if (var_list == null) continue;
                if (value_list == null) continue;
                List<Variable> new_vars = new ArrayList<Variable>();
                List<Expression> new_values = new ArrayList<Expression>();
                for (int i=0; i<var_list.size(); i++) {
                    Variable var = var_list.get(i);
                    if (var == null) continue;
                    String var_name = var.getName();
                    if (mod.get_variable(var_name) != null) {
                        new_vars.add(var);
                        new_values.add(value_list.get(i));
                    }
                }
                if (new_vars.size() <= 0) continue;
                // 定義されている変数のみのDATA文を作成する
                jp.riken.kscope.language.Data new_data = new jp.riken.kscope.language.Data(data);
                new_data.setVariables(var_list);
                new_data.setValues(value_list);

                // DATA文を追加する
                mod.addData(new_data);
            }
        }
        return;
    }



    /**
     * INTERFACE文を追加する。
     * @param  kernel_blocks        カーネルブロック
     * @throws Exception
     */
    private void setKernelInterfaces(List<IBlock> kernel_blocks) throws Exception {
        if (this.kernel_modules == null) return;

        // 抽出カーネルのプロシージャの取得
        ProgramUnit kernel_proc = kernel_blocks.get(0).getScopeDeclarationsBlock();

        for (Module mod : this.kernel_modules) {
            ProgramUnit src_proc = null;
            List<ProcedureUsage> call_list = null;
            if (KernelProperties.MODULE_NAME_LOCAL.equalsIgnoreCase(mod.get_name())) {
                src_proc = kernel_proc;
                call_list = this.getProcedureUsages(kernel_blocks, null);
            }
            else {
                src_proc = this.fortranDb.module(mod.get_name());
                call_list = this.getProcedureUsages(kernel_blocks, null);
            }
            if (src_proc == null) continue;
            if (call_list == null) continue;

            // interface文を取得する
            List<Procedures>  inter_list = src_proc.getInterfaceList();
            if (inter_list == null || inter_list.size() <= 0) continue;

            // interface文のサブルーチン・関数が存在するかチェックする
            for (Procedures inter : inter_list) {
                Procedures new_inter = new Procedures(inter);
                Set<IProcedureItem>  procs = new_inter.getProcedures();
                Iterator<IProcedureItem> itr = procs.iterator();
                while (itr.hasNext()) {
                    ProcedureItem item = (ProcedureItem)itr.next();
                    String proc_name = item.getName();
                    boolean exists = false;
                    for (ProcedureUsage call : call_list) {
                        if (call.getCallName().equalsIgnoreCase(proc_name)) {
                            exists = true;
                            break;
                        }
                    }
                    if (!exists) {
                        itr.remove();
                    }
                }
                if (procs.size() > 0) {
                    // interface追加
                    mod.addInterface(new_inter);
                }
            }
        }
    }


    /**
     * 手続呼出一覧を取得する.
     * 呼出サブルーチン・関数内の子孫呼出手続までは検索しない
     * @param block            探索ブロック
     * @param proc_list        取得呼出一覧
     * @return        取得呼出一覧
     */
    private List<ProcedureUsage> getProcedureUsages(List<IBlock> blocks, List<ProcedureUsage> call_list) {
        if (blocks == null) return null;
        if (call_list == null) {
            call_list = new ArrayList<ProcedureUsage>();
        }

        for (IBlock block : blocks) {
            this.getProcedureUsages(block, call_list);
        }
        if (call_list == null || call_list.size() <= 0) return null;
        return call_list;
    }


    /**
     * 手続呼出一覧を取得する.
     * 呼出サブルーチン・関数内の子孫呼出手続までは検索しない
     * @param block            探索ブロック
     * @param proc_list        取得呼出一覧
     * @return        取得呼出一覧
     */
    private List<ProcedureUsage> getProcedureUsages(IBlock block, List<ProcedureUsage> call_list) {
        if (block == null) return null;

        // 手続呼出一覧
        if (call_list == null) {
            call_list = new ArrayList<ProcedureUsage>();
        }
        if (block.getBlockType() == BlockType.PROCEDUREUSAGE) {
            if (!call_list.contains(block)) {
                call_list.add((ProcedureUsage)block);
            }
        }
        List<ProcedureUsage> calls = block.getCalls();
        if (calls != null) {
            for (ProcedureUsage call : calls) {
                if (!call_list.contains(call)) {
                    call_list.add(call);
                }
            }
        }

        List<IBlock> childs = block.getChildren();
        if (childs != null) {
            for (IBlock child : childs) {
                // 呼出手続一覧
                this.getProcedureUsages(child, call_list);
            }
        }
        if (call_list == null || call_list.size() <= 0) return null;

        return call_list;
    }

    /**
     * 子ブロックからallocate文を取得する
     * @param blocks        カーネルブロック
     * @return        allocate文
     */
    private List<IBlock> getAllocateBlocks(List<IBlock> blocks) {
        if (blocks == null) return null;

        List<IBlock> list = this.getAllBlocks(blocks);
        if (list == null || list.size() <= 0) return null;

        List<IBlock> allocates = new ArrayList<IBlock>();
        for (IBlock block : list) {
            if (block.getBlockType() == BlockType.DYNAMIC_ALLOCATION) {
                allocates.add(block);
            }
        }

        if (allocates.size() <= 0) return null;

        return allocates;
    }

    /**
     * 子ブロックを取得する
     * @param blocks        ブロックリスト
     * @return        子ブロックリスト
     */
    private List<IBlock> getAllBlocks(List<IBlock> blocks) {
        if (blocks == null) return null;

        List<IBlock> list = new ArrayList<IBlock>();
        for (IBlock block : blocks) {
            if (block == null) continue;
            list.add(block);

            List<IBlock> children = block.getChildren();
            if (children == null || children.size() <= 0) continue;

            List<IBlock> child_list = this.getAllBlocks(children);
            list.addAll(child_list);
        }

        if (list.size() <= 0) return null;

        return list;
    }


    /**
     * 入出力構造体定義一覧を取得する
     * @return
     */
    private List<jp.riken.kscope.language.fortran.Type> getLocalTypes(List<IBlock> kernel_blocks) {
        if (kernel_blocks == null || kernel_blocks.size() <= 0) return null;

        // 抽出カーネルのプロシージャの取得
        ProgramUnit kernel_proc = kernel_blocks.get(0).getScopeDeclarationsBlock();
        List<jp.riken.kscope.language.fortran.Type> fileio_types = this.getFileioTypes();
        if (fileio_types == null || fileio_types.size() <= 0) return null;

        List<jp.riken.kscope.language.fortran.Type> local_types = new ArrayList<jp.riken.kscope.language.fortran.Type>();
        for (jp.riken.kscope.language.fortran.Type type : fileio_types) {
            if (type.getMotherBlock() == null) continue;
            if (type.getMotherBlock().getBlockType() != BlockType.MODULE) continue;
            Module mod = (Module)type.getMotherBlock();
            if (!KernelProperties.MODULE_NAME_LOCAL.equals(mod.get_name())) continue;
            jp.riken.kscope.language.fortran.Type local_type = kernel_proc.getType(type.getName());
            if (local_type == null) continue;
            local_types.add(local_type);
        }
        if (local_types.size() <= 0) return null;

        return local_types;
    }


    /**
     * モジュールのTYPE文を宣言順に並べる
     */
    private void sortKernelModuleType() {
        if (this.kernel_modules == null) return;

        for (Module mod : this.kernel_modules) {
            List<jp.riken.kscope.language.fortran.Type>  types = mod.getTypeList();
            if (types == null || types.size() <= 0) continue;

            Collections.sort(types, new Comparator<jp.riken.kscope.language.fortran.Type>(){
                public int compare(Type type1, Type type2) {
                    if (type1 == null) return 0;
                    if (type2 == null) return 0;
                    String name1 = type1.getName();
                    String name2 = type2.getName();

                    CodeLine line1 = type1.getStartCodeLine();
                    CodeLine line2 = type2.getStartCodeLine();
                    if (line1 == null) return -1;
                    else if (line2 == null) return +1;
                    return line1.compareTo(line2);
                }
            });
        }

        return;
    }

    /**
     * 再帰構造体の変数を追加する.
     * @param def        追加再帰構造体の変数
     */
    private void addRecursiceType(VariableDefinition def) {
        if (def == null) return;
        if (!def.isStruct()) return;
        if (def.get_name() == null || def.get_name().isEmpty()) return;

        if (this.recursive_type == null) {
            this.recursive_type = new ArrayList<VariableDefinition>();
        }
        boolean exists = false;
        for (VariableDefinition rec_type : this.recursive_type) {
            String name = rec_type.get_name();
            if (name.equalsIgnoreCase(def.get_name())) {
                exists = true;
                break;
            }
        }

        if (exists) return;
        this.recursive_type.add(def);

        return;
    }


    /**
     * Common文,DATA文に使用されている変数の定義文を取得する.
     * @param kernel_def_list        カーネル使用変数定義
     * @return        Common文に使用されている変数
     */
    private List<VariableDefinition> getCommonVariableDefinition(List<VariableDefinition> kernel_def_list) {
        if (kernel_def_list == null) return null;

        List<VariableDefinition> list = new ArrayList<VariableDefinition>();

        // モジュール、サブルーチンからCOMMON文を検索する
        Set<Common> common_list = new HashSet<Common>();
        for (VariableDefinition def : kernel_def_list) {
            List<Common> commons = this.serachCommonList(def);
            if (commons == null || commons.size() <= 0) continue;
            common_list.addAll(commons);
        }

        // 変数定義を取得する
        for (Common common : common_list) {
            List<Variable> vars = common.getVariables();
            for (Variable var : vars) {
                VariableDefinition def = var.getDefinition();
                if (def == null) continue;
                if (!list.contains(def)) {
                    list.add(def);
                }
            }
        }

        // モジュール、サブルーチンからDATA文を検索する
        Set<jp.riken.kscope.language.Data> data_list = new HashSet<jp.riken.kscope.language.Data>();
        for (VariableDefinition def : kernel_def_list) {
            List<jp.riken.kscope.language.Data> datas = this.serachDataList(def);
            if (datas == null || datas.size() <= 0) continue;
            data_list.addAll(datas);
        }

        // 変数定義を取得する
        for (jp.riken.kscope.language.Data data : data_list) {
            Set<Variable> vars = data.getAllVariables();
            for (Variable var : vars) {
                VariableDefinition def = var.getDefinition();
                if (def == null) continue;
                if (!list.contains(def)) {
                    list.add(def);
                }
            }
        }

        if (list.size() <= 0) return null;
        return list;
    }

    /**
     * 変数定義の変数を含むCOMMON文を取得する。
     * @param var_def        検索変数定義文
     * @return        COMMON文リスト
     */
    private List<Common> serachCommonList(VariableDefinition var_def) {
        if (var_def == null) return null;
        if (var_def.get_name() == null) return null;

        ProgramUnit proc = var_def.getScopeDeclarationsBlock();
        if (proc == null) return null;

        // モジュール、サブルーチンからCOMMON文を検索する
        List<Common> list = new ArrayList<Common>();
        List<Common> commons = proc.getCommonList();
        if (commons == null || commons.size() <= 0) return null;
        for (Common comm : commons) {
            List<Variable> vars = comm.getVariables();
            boolean exists = false;
            for (Variable var : vars) {
                VariableDefinition comm_def = var.getDefinition();
                if (comm_def == var_def) {
                    exists = true;
                    break;
                }
            }

            if (exists) {
                list.add(comm);
            }
        }
        if (list.size() <= 0) return null;

        return list;
    }


    /**
     * 変数定義の変数を含むDATA文を取得する。
     * @param var_def        検索変数定義文
     * @return        DATA文リスト
     */
    private List<jp.riken.kscope.language.Data> serachDataList(VariableDefinition var_def) {
        if (var_def == null) return null;
        if (var_def.get_name() == null) return null;

        ProgramUnit proc = var_def.getScopeDeclarationsBlock();
        if (proc == null) return null;

        // モジュール、サブルーチンからCOMMON文を検索する
        List<jp.riken.kscope.language.Data> list = new ArrayList<jp.riken.kscope.language.Data>();
        List<jp.riken.kscope.language.Data> datas = proc.getDataList();
        if (datas == null || datas.size() <= 0) return null;
        for (jp.riken.kscope.language.Data data : datas) {
            List<Variable> vars = data.getVariables();
            boolean exists = false;
            for (Variable var : vars) {
                VariableDefinition data_def = var.getDefinition();
                if (data_def == var_def) {
                    exists = true;
                    break;
                }
            }

            if (exists) {
                list.add(data);
            }
        }
        if (list.size() <= 0) return null;

        return list;
    }

    /**
     * 変数定義の変数を含むEQUIVALENCE 文を取得する。
     * @param var_def        検索変数定義文
     * @return        EQUIVALENCE 文リスト
     */
    private List<Equivalence> serachEquivalenceList(VariableDefinition var_def) {
        if (var_def == null) return null;
        if (var_def.get_name() == null) return null;

        ProgramUnit proc = var_def.getScopeDeclarationsBlock();
        if (proc == null) return null;

        // モジュール、サブルーチンからCOMMON文を検索する
        List<Equivalence> list = new ArrayList<Equivalence>();
        List<Equivalence> equivalences = proc.getEquivalenceList();
        if (equivalences == null || equivalences.size() <= 0) return null;
        for (Equivalence equiv : equivalences) {
            List<Variable> vars = equiv.getVariables();
            boolean exists = false;
            for (Variable var : vars) {
                VariableDefinition equiv_def = var.getDefinition();
                if (equiv_def == var_def) {
                    exists = true;
                    break;
                }
            }

            if (exists) {
                list.add(equiv);
            }
        }
        if (list.size() <= 0) return null;

        return list;
    }


    /**
     * COMMON文を追加する。
     * モジュールのCOMMON文のみ追加する。
     * サブルーチン・関数のCOMMON文はサブルーチン・関数単位でコピーしているので追加する必要はない。
     */
    private void setKernelCommon(List<IBlock> kernel_blocks) {
        if (kernel_blocks == null) return;
        if (this.kernel_modules == null) return;

        for (Module mod : this.kernel_modules) {
            ProgramUnit src_proc = this.fortranDb.module(mod.get_name());
            if (src_proc == null) continue;

            List<Common> src_commons = src_proc.getCommonList();
            if (src_commons == null || src_commons.size() <= 0) continue;

            // 変数定義されているCOMMON文のみ返す。
            for (Common common : src_commons ) {
                boolean add_common = this.hasVariableDefinition(mod, common.getVariables());
                if (add_common) {
                    // COMMON文を追加する
                    mod.addCommon(new Common(common));
                }
            }
        }

        // ローカルCOMMON文
        Module local_mod = this.getKernelLocalModule();
        if (local_mod != null) {
            // 抽出カーネルのプロシージャの取得
            ProgramUnit kernel_proc = kernel_blocks.get(0).getScopeDeclarationsBlock();

            List<Common> src_commons = kernel_proc.getCommonList();
            if (src_commons != null && src_commons.size() > 0) {
                // 変数定義されているCOMMON文のみ返す。
                for (Common common : src_commons ) {
                    boolean add_common = this.hasVariableDefinition(local_mod, common.getVariables());
                    if (add_common) {
                        // COMMON文を追加する
                        local_mod.addCommon(new Common(common));
                    }
                }
            }
        }

        return;
    }


    /**
     * EQUIVALENCE文を追加する。
     * モジュールのEQUIVALENCE文のみ追加する。
     * サブルーチン・関数のEQUIVALENCE文はサブルーチン・関数単位でコピーしているので追加する必要はない。
     */
    private void setKernelEquivalence(List<IBlock> kernel_blocks) {
        if (kernel_blocks == null) return;
        if (this.kernel_modules == null) return;

        for (Module mod : this.kernel_modules) {
            ProgramUnit src_proc = this.fortranDb.module(mod.get_name());
            if (src_proc == null) continue;

            List<Equivalence> src_equivalences = src_proc.getEquivalenceList();
            if (src_equivalences == null || src_equivalences.size() <= 0) continue;

            // 変数定義されているEQUIVALENCE文のみ返す。
            for (Equivalence equiv : src_equivalences ) {
                Equivalence new_equiv = this.createEquivalence(equiv, mod);
                if (new_equiv != null) {
                    // EQUIVALENCE文を追加する
                    mod.addEquivalence(new_equiv);
                }
            }
        }

        // ローカルEQUIVALENCE文
        Module local_mod = this.getKernelLocalModule();
        if (local_mod != null) {
            // 抽出カーネルのプロシージャの取得
            ProgramUnit kernel_proc = kernel_blocks.get(0).getScopeDeclarationsBlock();

            List<Equivalence> src_equivalences = kernel_proc.getEquivalenceList();
            if (src_equivalences != null && src_equivalences.size() > 0) {
                // 変数定義されているEQUIVALENCE文のみ返す。
                for (Equivalence equiv : src_equivalences ) {
                    Equivalence new_equiv = this.createEquivalence(equiv, local_mod);
                    if (new_equiv != null) {
                        // EQUIVALENCE文を追加する
                        local_mod.addEquivalence(new_equiv);
                    }
                }
            }
        }

        return;
    }


    /**
     * Equivalence文を作成する。
     * @param src_equiv        元Equivalence文
     * @param proc            挿入プロシージャ
     * @return        new Equivalence
     */
    private Equivalence createEquivalence(Equivalence src_equiv, ProgramUnit proc) {
        if (src_equiv == null) return null;
        if (proc == null) return null;

        Equivalence new_equiv = new Equivalence(src_equiv);
        List<Variable> src_vars = new_equiv.getVariables();
        if (src_vars == null || src_vars.size() <= 0) return null;

        // 変数が存在しているかチェックする
        List<Variable> equiv_vars = new ArrayList<Variable>();
        for (Variable var : src_vars) {
            if (proc.get_variable(var.getName()) != null) {
                equiv_vars.add(var);
            }
        }
        // 2つ以上の変数が必要
        if (equiv_vars.size() < 2) return null;
        new_equiv.setVariables(equiv_vars);

        return new_equiv;
    }


    /**
     * 変数リストの定義が存在しているかチェックする.
     * @param module        モジュール、プロシージャ
     * @param var_list        変数リスト
     * @return        定義が存在している。
     */
    private boolean hasVariableDefinition(ProgramUnit module, List<Variable> var_list) {
        if (module == null) return false;
        if (var_list == null) return false;

        for (Variable var : var_list) {
            String var_name = var.getName();
            if (module.get_variable(var_name) != null) {
                return true;
            }
        }

        return false;
    }



    private List<String> getModuleFilenames(List<File> mod_filelist) {
        if (mod_filelist == null || mod_filelist.size() <= 0) return null;

        List<String> list = new ArrayList<String>();
        for (File file : mod_filelist) {
            String name = file.toString();
            name = name.replace("\\", "/");
            list.add(name);
        }
        if (list.size() <= 0) return null;

        return list;
    }

    /**
     * 自身、及び親プロシージャのUSE文リストを取得する
     * @param proc        探索プロシージャ
     * @return        USE文リスト
     */
    private List<UseState> getScopeUseList(IBlock block) {
        if (block == null) return null;

        // USE文リスト
        List<UseState> use_list = new ArrayList<UseState>();
        // 関数、モジュールのUSE文を取得する
        IBlock current = block;
        while (current != null) {
            if (current instanceof ProgramUnit) {
                List<UseState> proc_uses = ((ProgramUnit)current).getUseList();
                if (proc_uses != null) {
                     use_list.addAll(proc_uses);
                }
            }
            current = current.getMotherBlock();
        }
        if (use_list.size() <= 0) return null;

        return use_list;
    }


    /**
     * モジュール定義名（変数、関数）がモジュールに定義されているかチェックする。
     * モジュール内のUSE文の先のモジュールに定義されているかチェックする。
     * @param module        探索モジュール
     * @param var_name        モジュール定義名
     * @return        true=モジュールに定義されている
     */
    private boolean hasDefinition(Module module, String var_name)  throws Exception {
        if (module == null) return false;
        if (var_name == null) return false;

        // 変数、手続が存在しているかチェックする。
        if (module.get_variable(var_name) != null) return true;
        if (module.get_child(var_name) != null) return true;

        List<UseState> uses = module.getUseList();
        if (uses == null || uses.size() <= 0) return false;
        for (UseState use : uses) {
            String mod_name = use.getModuleName();
            Module use_mod = this.getKernelModule(mod_name);
            if (use_mod == null) {
                // [エラー]カーネルモジュールが存在しません。
                String msg = Message.getString("kernel.validate.notfound_module.error.message");
                msg += "[modulename=" + mod_name + "]";
                this.addErrorInfo(msg);
                throw new Exception(msg);
            }
            String trans_name = null;
            if (use.hasOnlyMember(var_name))  trans_name = var_name;
            else {
                trans_name = use.getTranslationName(var_name);
            }
            if (trans_name == null) continue;

            if (this.hasDefinition(use_mod, trans_name)) {
                return true;
            }
        }

        return false;
    }

    /**
     * CALL文の手続がサブルーチン・関数の引数であるかチェックする。
     * @param call        CALL文
     * @return        true=サブルーチン・関数の引数
     */
    private boolean isExternalArgment(ProcedureUsage call) {
        if (call == null) return false;
        if (call.getCallName() == null) return false;

        String call_name = call.getCallName();
        // 引数渡しの手続の場合は行う。
        ProgramUnit parent = call.getScopeDeclarationsBlock();
        if (parent == null) return false;
        if (!(parent.getBlockType() == BlockType.PROCEDURE)) return false;

        // 引数の取得
        Variable[] vars = ((Procedure)parent).get_args();
        if (vars == null) return false;
        for (Variable var : vars) {
            VariableDefinition def = var.getDefinition();
            if (def == null) continue;
            if (!def.hasExternal()) continue;
            String arg_name = var.getName();
            if (arg_name.equalsIgnoreCase(call_name)) {
                return true;
            }
        }

        return false;
    }
}



