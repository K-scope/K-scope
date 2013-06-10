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
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JOptionPane;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.exception.LanguageException;
import jp.riken.kscope.information.InformationBlock;
import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.information.ReplacementResult;
import jp.riken.kscope.information.ReplacementResult.RESULT_STATUS;
import jp.riken.kscope.information.ReplacementResults;
import jp.riken.kscope.information.TextInfo;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.Module;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.utils.InformationEntry;
import jp.riken.kscope.language.utils.LanguageVisitor;
import jp.riken.kscope.language.utils.ValidateLanguage;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.model.ReplacementResultTableModel;
import jp.riken.kscope.utils.Logger;
import jp.riken.kscope.utils.SwingUtils;
import jp.riken.kscope.xcodeml.XcodeMLParserStax;


/**
 * Makefileの実行クラス.
 * Makefileを実行し、データベースの再構築を行う.
 * @author riken
 */
public class ProjectMakeService  extends BaseService {
	/** makeコマンド */
	private String makecommands[];
	/** makeコマンド実行フォルダ */
	private File workdirectory;
	/** makeコマンド出力ストリーム */
	private OutputStream outStream;
	/** プロジェクトモデル */
	private ProjectModel projectModel;
	/** 付加情報差替情報モデル */
    private ReplacementResultTableModel replaceModel;
    /** フォートラン構文解析結果格納データベース:現在のデータベース. */
    private Fortran currentDb = null;
    /** XMLファイル検索パスリスト */
    private List<File> listSearchPath;
    /** スレッド実行フラグ true:実行継続/false:中止. */
    private boolean m_running = true;
    /** SSHconnectを使用　*/
    private boolean useSSHconnect = false;

    /**
     * コンストラクタ.
     * @param commands            Makeコマンド
     */
    public ProjectMakeService(String commands[]) {
    	this.makecommands = commands;
    }

    /**
     * コンストラクタ.
     * @param commands            Makeコマンド
     * @param work            Makeコマンド実行フォルダ
     */
    public ProjectMakeService(String commands[], File work) {
    	this.makecommands = commands;
        this.workdirectory = work;
    }
    
    /**
     * コンストラクタ.
     * @param commands            Makeコマンド
     * @param work            Makeコマンド実行フォルダ
     * @param useSSHconnect	SSHconnectを使用するか否か
     */
    public ProjectMakeService(String commands[], File work, boolean useSSHconnect) {
    	this.makecommands = commands;
        this.workdirectory = work;
        this.useSSHconnect = useSSHconnect;
    }

    /**
     * 構造情報再実行を行う.
     * @return 		成否
     */
    public boolean rebuild()  {
        Application.status.setProgressStart(true);
        if (this.listSearchPath == null || this.listSearchPath.size() <= 0) {
        	this.addErrorInfo("can not set search path list.");
        	return false;
        }

		try {
			// XMLファイルリスト取得
			ProjectService service = new ProjectService();
			SourceFile[] listBuildXml = service.getSourceFiles(this.listSearchPath.toArray(new File[0]),
														FILE_TYPE.XCODEML_XML,
														true);
			// ソースファイルが存在するかチェックする.
			listBuildXml = validateXmlFiles(listBuildXml);
			if (listBuildXml == null || listBuildXml.length <= 0) {
				// 中間コード(XML)がありませんでした。
				String msg = Message.getString("projectmakeservice.rebuild.notexists.xml");
				this.addErrorInfo(msg);
				return false;
			}

			// 現在のファイルリスト
			List<SourceFile> listCurrentSrc = this.currentDb.getSourceFileList();
			// 現在のXMLファイルリスト
			List<SourceFile> listCurrentXml = new ArrayList<SourceFile>();
			for (SourceFile src : listCurrentSrc) {
				SourceFile xml = src.getRelationFile();
				listCurrentXml.add(xml);
			}
			// 更新ファイルリスト
			SourceFile[] listUpdate = getUpdateFiles(listCurrentXml.toArray(new SourceFile[0]), listBuildXml);
			SourceFile[] listDelete = getDeleteFiles(listCurrentXml.toArray(new SourceFile[0]), listBuildXml);
			if ( (listUpdate == null || listUpdate.length <= 0)
				&& (listDelete == null || listDelete.length <= 0) ) {
				// 更新ファイルがありませんでした。
				String msg = Message.getString("projectmakeservice.rebuild.notexists.updatefile");
				this.addErrorInfo(msg);
				return false;
			}

			// 更新ファイルのパースを行う.
			Fortran buildDb = parseSourceFile(listUpdate);

			// データベースのモジュールコピーを行う
			copyModules(buildDb, this.currentDb, listDelete);

			// 変数定義の関連付けを行う.
			buildDb.analyseDB();

            // データベースの検証を行う.
            ValidateLanguage validate = new ValidateLanguage(buildDb);
            LanguageVisitor visitor = new LanguageVisitor(validate);
            visitor.entry();
            int error = validate.analyseTypes();
            if (error > 0) {
            	this.getErrorInfoModel().addErrorInfos(validate.getErrorList());
            	String msg = Message.getString("validatelanguage.final.error", error);
            	this.addErrorInfo(msg);
            }

			// 付加情報の差替を行う
            this.replaceModel.clearReplacement();
			ReplacementResults results = copyInformations(buildDb, this.currentDb);
			if (results != null) {
	            // 付加情報差替結果をパネルにセットする。
	            this.replaceModel.setTitle(Message.getString("projectmakeservice.replaceinformation.title")); //差替結果一覧
	            this.replaceModel.addReplacementResultBlock(results);
			}

			// 作成データベースを元のデータベースにコピーする
			this.currentDb.copyShallow(buildDb);

			// XMLファイルリストを設定する
			this.projectModel.setListXmlFile(listBuildXml);

	        return true;

        } catch (LanguageException lang_ex) {
            Logger.error(lang_ex);
            Logger.error(lang_ex.getCodeInfo());
            lang_ex.printStackTrace();
            // エラー箇所の情報をセットする
            this.addErrorInfo(lang_ex);
	        return false;
        } catch (InterruptedException ex) {
            // キャンセルによる終了 : projectmakeservice.rebuild.cancel=キャンセルにより中断しました。
        	String msg = Message.getString("projectmakeservice.rebuild.cancel");
            this.addErrorInfo(msg);
            Application.status.setMessageStatus(msg);
            return false;
        } catch (Exception ex) {
            Logger.error(ex);
            ex.printStackTrace();

            String error_message = ex.getMessage();
            if (error_message == null) {
                error_message = ex.toString();
            }
            // エラー箇所の情報をセットする
            this.addErrorInfo(error_message);
            return false;
        } finally {
        	Application.status.setProgressStart(false);
        }

    }

    /**
     * XMLファイルを検証する.
     * ソースファイルが存在するかチェックする.
     * @param xmlfiles
     * @return		フォートランソースファイルリスト
     */
	private SourceFile[] validateXmlFiles(SourceFile[] xmlfiles) {
		if (xmlfiles == null) return null;

        // XMLパーサの作成
        XcodeMLParserStax fortranParser = new XcodeMLParserStax();
		List<SourceFile> list = new ArrayList<SourceFile>();
		for (SourceFile xml : xmlfiles) {
            try {
				if (xml.getRelationFile() != null && xml.getRelationFile().getFile() != null) {
					SourceFile file = xml.getRelationFile();
					if (file.getFile().exists()) {
						list.add(xml);
					}
					continue;
				}
				// XMLファイルをパースする.
	            fortranParser.readFile(xml);
	            fortranParser.parseSourceFile();
	            SourceFile source = fortranParser.getLanguageFile();
	            if (source != null && source.getFile() != null) {
	            	if (source.getFile().exists()) {
	            		list.add(xml);
	            	}
				}

            } catch (LanguageException lang_ex) { }
		}

		if (list.size() <= 0) return null;
		return list.toArray(new SourceFile[0]);
	}

	/**
     * 削除ファイルリストを取得する.
     * @param currents		現在のファイルリスト
     * @param builds			再構築のファイルリスト
     * @return					削除ファイルリスト
     */
    private SourceFile[] getDeleteFiles(SourceFile[] currents, SourceFile[] builds) {
    	if (currents == null || currents.length <= 0) return null;
    	if (builds == null || builds.length <= 0) return null;

    	// 削除ファイルリスト
    	List<SourceFile> listDelete = new ArrayList<SourceFile>();
    	List<SourceFile> listCurrent = Arrays.asList(currents);
    	List<SourceFile> listBuild = Arrays.asList(builds);
    	for (SourceFile file : listCurrent) {
    		if (!listBuild.contains(file)) {
    			listDelete.add(file);
    		}
    	}
    	if (listDelete.size() <= 0) {
    		return null;
    	}

		return listDelete.toArray(new SourceFile[0]);
	}

    /**
     * 更新ファイルリストを取得する.
     * 更新日付が変更されているファイル、追加されたファイルを取得する
     * @param currents		現在のファイルリスト
     * @param builds			再構築のファイルリスト
     * @return					更新・追加ファイルリスト
     */
	private SourceFile[] getUpdateFiles(SourceFile[] currents, SourceFile[] builds) {
    	if (builds == null || builds.length <= 0) return null;
    	if (currents == null || currents.length <= 0) return builds;

    	// 更新ファイルリスト
    	List<SourceFile> listUpdate = new ArrayList<SourceFile>();
    	List<SourceFile> listCurrent = Arrays.asList(currents);
    	List<SourceFile> listBuild = Arrays.asList(builds);
    	for (SourceFile file : listBuild) {
    		// 追加ファイル
    		if (!listCurrent.contains(file)) {
    			listUpdate.add(file);
    			continue;
    		}
    		// 更新日付チェック
    		Date currentDate = null;
    		Date buildDate = file.getModifyDate();
    		int idx = listCurrent.indexOf(file);
    		if (idx >= 0) {
    			SourceFile currentFile = listCurrent.get(idx);
        		if (currentFile != null) {
        			currentDate = currentFile.getModifyDate();
        		}
    		}
    		if (currentDate == null) {
    			listUpdate.add(file);
    		}
    		else if (buildDate == null) {
    			listUpdate.add(file);
    		}
    		else if (!currentDate.equals(buildDate)) {
    			listUpdate.add(file);
    		}
    	}
    	if (listUpdate.size() <= 0) {
    		return null;
    	}

		return listUpdate.toArray(new SourceFile[0]);
	}

    /**
     * スレッドの実行をキャンセルする。
     */
    public void cancelRunning() {
        m_running = false;
    }

    /**
     * スレッドの実行がキャンセルであるかチェックする
     * @return    true=キャンセル
     */
    public boolean isCancel() {
        return !this.m_running;
    }

    /**
     * makeコマンド出力ストリームを設定する.
     * @param out		makeコマンド出力ストリーム
     */
	public void setOutputStream(OutputStream out) {
		this.outStream = out;
	}

	/**
	 * フォートランデータベースを設定する
	 * @param fortran		フォートランデータベース
	 */
	public void setFortranLanguage(Fortran fortran) {
		this.currentDb = fortran;
	}

	/**
	 * makeコマンドを実行する.
	 * @return		true = 正常終了、又は継続実行
	 * @throws Exception
	 */
	public boolean executeMakeCommand() throws Exception {
		// ステータスメッセージ
        Application.status.setProgressStart(true);
        if (this.makecommands == null || this.makecommands.length <= 0) return false;
        String[] new_makecommands = null;
        String command = "";
        if (this.useSSHconnect) {
        	// inject SSHconnect call
        	int insert_commands = 3;
        	new_makecommands = new String [makecommands.length + insert_commands];
        	new_makecommands[0] = "java";
        	new_makecommands[1] = "-jar";
        	new_makecommands[2] = "SSHconnect.jar";
        	for (int i=0; i < insert_commands; i++) {
        		if (command.length() > 0) command = command + " ";
        		command = command + new_makecommands[i];
        	}
        	for (int i=0; i < this.makecommands.length; i++) {
        		if (!command.isEmpty()) command += " ";
        		new_makecommands[i + insert_commands] = this.makecommands[i]; // <-- new make commands initialization
        		command += new_makecommands[i + insert_commands];
        	}
        } else {
        	for (int i=0; i<this.makecommands.length; i++) {
        		if (!command.isEmpty()) command += " ";
        		command += this.makecommands[i];        		
        	}
        }
        Application.status.setMessageStatus(command);

        // makeコマンド実行
    	int result = -1;
		try {
			if (this.useSSHconnect) result = SwingUtils.processRun(new_makecommands, this.workdirectory, this.outStream);
			else result = SwingUtils.processRun(this.makecommands, this.workdirectory, this.outStream);
			if (result != 0) { // 中間コードの生成に失敗した場合は継続するか確認
				if (JOptionPane.showConfirmDialog(null,
						Message.getString("projectmakeservice.executemakecommand.continue.message"),
						Message.getString("projectmakeservice.executemakecommand.error"),
						JOptionPane.YES_NO_OPTION) != JOptionPane.YES_OPTION) {
					// ステータスメッセージ
			        Application.status.setProgressStart(false);
					return false;
				}
			}
			Application.status.setProgressStart(false);
			return true;
		} catch (Exception ex) {
			ex.printStackTrace();
			throw ex;
		}
	}

	/**
	 * XMLファイル検索パスリストを設定する.
	 * @param list    XMLファイル検索パスリスト
	 */
	public void setListSearchPath(List<File> list) {
		this.listSearchPath = list;
	}

	/**
	 * XMLファイルをパースする.
	 * @param updateFiles		更新XMLファイルリスト
	 * @return  フォートランデータベース
	 * @throws InterruptedException		割り込み例外
	 */
	private Fortran parseSourceFile(SourceFile[] updateFiles) throws InterruptedException {

        // フォートランデータベース
        Fortran fortranDb = new Fortran();
        if (updateFiles == null) return fortranDb;

        // XMLパーサの作成
        XcodeMLParserStax fortranParser = new XcodeMLParserStax();
        ArrayList<SourceFile> sourceFileList = new ArrayList<SourceFile>();
        for (SourceFile file : updateFiles) {
            try {
                String filename = file.toString();
                Application.status.setMessageStatus(filename);

                // ソースファイルからファイルを読み込む
                fortranParser.readFile(file);

                // 読込コード行を構文解析する。
                fortranParser.parseFile(fortranDb);

                // オリジナルフォートランソースファイルの取得
                sourceFileList.add(fortranParser.getLanguageFile());

                // パースエラーの取得
                if (fortranParser.getErrorInfos() != null) {
                	this.addErrorInfos(fortranParser.getErrorInfos());
                }
            } catch (Exception lang_ex) {
                // エラー箇所の情報をセットする
                this.addErrorInfo(lang_ex);
            }

            // キャンセルチェック
            if (this.isCancel()) {
                return null;
            }
        }

        // ソースファイルリストの設定
        fortranDb.setSourceFileList(sourceFileList);

        return fortranDb;
	}

	/**
	 * モジュールコピーを行う.
	 * @param buildDb		ビルドデータベース
	 * @param originalDb		元データベース
	 * @param deletes	削除XMLリスト
	 * @return				true=success
	 */
    private boolean copyModules(Fortran buildDb, Fortran originalDb, SourceFile[] deletes) {
    	if (buildDb == null) return false;
    	if (originalDb == null) return false;
    	Map<String, Module> buildModules = buildDb.getModules();
    	Map<String, Module> currentModules = originalDb.getModules();
    	if (currentModules == null || currentModules.size() <= 0) {
    		return true;
    	}
    	// 削除対象XMLファイルリスト
    	List<SourceFile> listDelete = null;
    	if (deletes != null && deletes.length > 0) {
    		listDelete = Arrays.asList(deletes);
    	}

		// 削除モジュールリスト
		List<Module> deleteModules = new ArrayList<Module>();
		// 更新モジュールマップ <旧モジュール, 新モジュール>
		HashMap<Module, Module> mapUpdate = new HashMap<Module,Module>();

		// 現在のデータベースからビルドデータベースへモジュールのコピーを行う.
    	CURRENT_LOOP : for ( String key : currentModules.keySet() ) {
    		Module module = currentModules.get( key );
    		if (module == null) continue;
    		// モジュールのコンパイルXMLファイルを取得する.
    		SourceFile xml = getXmlFile(module, originalDb.getSourceFileList());
    		// 削除対象のモジュールであるか
    		if (xml != null && listDelete != null && listDelete.contains(xml)) {
		    	deleteModules.add(module);
		    	continue;
    		}
    		// 更新対象のモジュールであるか
    		if (xml != null) {
    	    	for ( String buildKey : buildModules.keySet() ) {
    	    		Module buildModule = buildModules.get( buildKey );
    	    		if (buildModule == null) continue;
    	    		// モジュールのコンパイルXMLファイルを取得する.
    	    		SourceFile buildXml = getXmlFile(buildModule, buildDb.getSourceFileList());
    	    		if (xml.equals(buildXml)) {
    	    			// 更新モジュール
    	    			mapUpdate.put(module, buildModule);
    	    			continue CURRENT_LOOP;
    	    		}
    	    	}
    		}
    		// 同一モジュール名が存在するか？
    		Module updateModule = buildDb.module(module.get_name());
    		if (updateModule != null) {
                if ("NO_MODULE".equalsIgnoreCase(updateModule.get_name()) ) {
                	if (updateModule.isEmpty()) {
                		updateModule = null;
                	}
                }
    		}
    		if (updateModule != null) {
    			// 更新モジュール
    			mapUpdate.put(module, updateModule);
    			continue;
    		}
    		// モジュールコピー
    		buildDb.addModule(module);
    	}

		// メインプログラム名のコピー
		if (buildDb.getMainName() == null || buildDb.getMainName().isEmpty()) {
			buildDb.setMainName(originalDb.getMainName());
		}

	    // ソースファイルリストのコピー
	    ArrayList<SourceFile> listOrgFile = originalDb.getSourceFileList();
	    ArrayList<SourceFile> listBuildFile = buildDb.getSourceFileList();
	    if (listOrgFile != null) {
		    if (listBuildFile == null) {
		    	listBuildFile = new ArrayList<SourceFile>();
		    	buildDb.setSourceFileList(listBuildFile);
		    }
	    	for (SourceFile file : listOrgFile) {
	    		SourceFile xml = file.getRelationFile();
	    		if (listDelete != null) {
	    			if (listDelete.contains(xml)) {
	    				continue;
	    			}
	    		}
	    		if (!listBuildFile.contains(file)) {
	    			listBuildFile.add(file);
	    		}
	    	}
	    }

	    // COMMONマップ
	    Map<String, List<ProgramUnit>> mapCommon = originalDb.getCommonMap();
	    if (mapCommon != null) {
	    	CURRENT_LOOP : for ( String key : mapCommon.keySet() ) {
	    		List<ProgramUnit> list = mapCommon.get( key );
		    	for ( ProgramUnit unit : list ) {
		    		// モジュール、サブルーチンが削除モジュールリストに含まれているか？
		    		for (Module module : deleteModules) {
		    			if (module.containsChildren(unit)) {
		    				continue CURRENT_LOOP;
		    			}
		    		}
		    		// モジュール、サブルーチンが更新モジュールリストに含まれているか？
		    		for ( Module module : mapUpdate.keySet() ) {
			    		if (module.containsChildren(unit)) {
			    			// 更新モジュールに含まれているので、パースでcommonMapに追加されているはずである。
		    				continue CURRENT_LOOP;
			    		}
		    		}
		    		// 削除、更新モジュール、サブルーチンでないので、モジュールコピーで存在しているはずである。
		    		boolean existsModule = false;
	    	    	for ( String buildKey : buildModules.keySet() ) {
	    	    		Module buildModule = buildModules.get( buildKey );
	    	    		if (buildModule == null) continue;
			    		if (buildModule.containsChildren(unit)) {
			    			existsModule = true;
			    			break;
			    		}
	    	    	}
	    	    	if (!existsModule) {
	    	    		// COMMON文の宣言モジュールが見つからない
	    	    		throw new LanguageException("not found ProgramUnit of COMMON[name=" + key + "].", unit.getStartCodeLine());
	    	    	}

	    	    	// COMMON文追加
	    	    	buildDb.addCommonMap(key, unit);
		    	}
	    	}
	    }

	    // 変数定義、サブルーチン参照をクリアする
	    // ClearDefinitions validate = new ClearDefinitions(buildDb);
	    // validate.setListClearModule(mapUpdate);
        // LanguageVisitor visitor = new LanguageVisitor(validate);
        // visitor.entry();

		return true;
	}

    /**
     * モジュールのコンパイルXMLファイルを取得する.
     * @param module		モジュール
     * @param list		ソースファイルリスト
     * @return		XMLファイル
     */
    private SourceFile getXmlFile(Module module, List<SourceFile> list) {
    	if (module == null) return null;
		if (module.getStartCodeLine() == null) return null;
		SourceFile src = module.getStartCodeLine().getSourceFile();
    	if (src == null) return null;
	    SourceFile xml = src.getRelationFile();
	    if (xml == null && list != null) {
	    	for (SourceFile file : list) {
	    		if (src.equals(file)) {
	    			xml = file.getRelationFile();
	    			break;
	    		}
	    	}
	    }
    	return xml;
    }


	/**
	 * 付加情報のコピーを行う.
	 * @param destDb		コピー先データベース
	 * @param srcDb		    コピー元データベース
	 * @return				付加情報差替結果
	 */
    private ReplacementResults copyInformations(Fortran destDb, Fortran srcDb) {
    	if (srcDb == null) return null;
    	if (destDb == null) return null;

        ReplacementResults listResults = new ReplacementResults();

    	// すべての付加情報を取得する
    	InformationBlocks destInfos = null;
    	InformationBlocks srcInfos = null;
    	{
	        InformationEntry entry = new InformationEntry(destDb);
	        LanguageVisitor visitor = new LanguageVisitor(entry);
	        visitor.entry();
	        destInfos = entry.getInformationBlocks();
    	}
    	{
	        InformationEntry entry = new InformationEntry(srcDb);
	        LanguageVisitor visitor = new LanguageVisitor(entry);
	        visitor.entry();
	        srcInfos = entry.getInformationBlocks();
    	}
    	// InformationBlocks destInfos = destDb.getInformationBlocksAll();
    	// InformationBlocks srcInfos = srcDb.getInformationBlocksAll();
    	if (srcInfos == null) return null;
    	for (InformationBlock info : srcInfos) {
    		if (info == null) continue;
    		if (info.getInformation() == null) continue;
    		if (info.getInformation().getContent() == null) continue;
    		if (info.getInformation().getContent().isEmpty()) continue;

    		// コピー済みであるかチェックする.
    		if (containsInformationBlock(destInfos, info)) {
				// 付加情報差替:成功
    			ReplacementResult result = new ReplacementResult(ReplacementResult.RESULT_STATUS.UNSURE,
										info.getInformation(),
										info, info,
										info, info);
    			listResults.add(result);
    			continue;
    		}
    		IInformation[] infoBlocks = {info.getStartBlock(), info.getEndBlock()};
    		ReplacementResult[] results = new ReplacementResult[]{null, null};
    		for (int i=0; i<2; i++) {
    			if (i == 1 && infoBlocks[0] == infoBlocks[1]) {
    				results[1] = results[0];
    				break;
    			}
    			if (isBlock(infoBlocks[i])) {
		            // DO, IF, SELECT文の付加情報のコピーを行う。
	    			results[i] = copyInformationBlock(infoBlocks[i], destDb, srcDb);
	    			if (results[i] == null) {
	    				break;
	    			}
    			}
    			else {
		            // 代入文、その他の付加情報のコピーを行う。
	    			results[i] = copyInformationStatement(infoBlocks[i], destDb, srcDb);
	    			if (results[i] == null) {
	    				break;
	    			}
    			}
    		}

    		if (results[0] == null || results[1] == null) {
    			// 付加情報差替不可
    			results[0] = new ReplacementResult(ReplacementResult.RESULT_STATUS.FAILURE,
    											info.getInformation(),
    											null, null,
    											infoBlocks[0], infoBlocks[1]);
    			listResults.add(results[0]);
    			continue;
    		}
    		else if (infoBlocks[0] == infoBlocks[1]) {
    			// 付加情報差替結果追加
    			listResults.add(results[0]);
    		}
    		else {
    			// 付加情報ブロックを生成して追加。
    			IInformation newinfo = destDb.getInformation(results[0].getCurrentStartPosition(), results[1].getCurrentEndPosition());
    			newinfo.setInformation(info.getInformation());

    			// 範囲指定付加情報の場合
    			ReplacementResult result = margeReplacementResults(newinfo, results[0], results[1]);
    			// 付加情報差替結果追加
    			listResults.add(result);
    		}
    	}
    	if (listResults.size() <= 0) {
    		return null;
    	}
        return listResults;
    }

	/**
	 * 付加情報のコピーを行う：DO,IF,SELECT文.
	 * @param targetInfo	コピー元付加情報
	 * @param destDb		コピー先データベース
	 * @param srcDb		    コピー元データベース
	 * @return				付加情報差替結果
	 */
    private ReplacementResult copyInformationBlock(IInformation targetInfo, Fortran destDb, Fortran srcDb) {
    	if (targetInfo == null) return null;
    	if (destDb == null) return null;
    	if (srcDb == null) return null;

    	ReplacementResult result = null;

        // 付加情報のコピーを行う。
		// (1) 更新対象のモジュールであるか = [programDest, programSrc]
		ProgramUnit[] programInfos = getInformationProgramUnits(targetInfo, destDb, srcDb);
		if (programInfos == null) {
			return null;
		}

		// (2) 同じモジュール、サブルーチンであれば付加情報をコピーする.
		result = copyInformation(targetInfo, programInfos[0], programInfos[1]);
    	if (result != null) return result;

    	// 付加情報と同一ブロックを検索する
    	IInformation[] destBlocks = programInfos[0].searchInformationBlocks(targetInfo);
    	IInformation[] srcBlocks = programInfos[1].searchInformationBlocks(targetInfo);
    	if (destBlocks != null && srcBlocks != null) {
    		if (destBlocks.length == srcBlocks.length) {
    			// (3) 同じ構造ブロックに付加情報をコピーする.
    			result = copyInformationBlock(targetInfo, destBlocks, srcBlocks);
	        	if (result != null) return result;
    		}
    	}

    	// (4-2)同一構造であるかチェックする。
    	if (programInfos[1].equalsLayout(programInfos[0])) {
			// (4-2)同じ構造位置に付加情報をコピーする.
    		result = copyLayoutInformationBlock(targetInfo, programInfos[0], programInfos[1]);
        	if (result != null) return result;
    	}

		return null;
    }


	/**
	 * 付加情報のコピーを行う：代入文,その他構文.
	 * @param targetInfo	コピー元付加情報
	 * @param destDb		コピー先データベース
	 * @param srcDb		    コピー元データベース
	 * @return				付加情報差替結果
	 */
    private ReplacementResult copyInformationStatement(IInformation targetInfo, Fortran destDb, Fortran srcDb) {
    	if (targetInfo == null) return null;
    	if (destDb == null) return null;
    	if (srcDb == null) return null;

    	ReplacementResult result = null;

        // 付加情報のコピーを行う。
		// (1) 更新対象のモジュールであるか = [programDest, programSrc]
		ProgramUnit[] programInfos = getInformationProgramUnits(targetInfo, destDb, srcDb);
		if (programInfos == null) {
			return null;
		}

		// (2) 同じモジュール、サブルーチンであれば付加情報をコピーする.
		result = copyInformation(targetInfo, programInfos[0], programInfos[1]);
    	if (result != null) return result;

    	// 付加情報と同一ブロックを検索する
    	IInformation[] destBlocks = programInfos[0].searchInformationBlocks(targetInfo);
    	IInformation[] srcBlocks = programInfos[1].searchInformationBlocks(targetInfo);
    	if (destBlocks != null && srcBlocks != null) {
    		if (destBlocks.length == srcBlocks.length) {
    			// (3) 同じ構造ブロックに付加情報をコピーする.
    			result = copyInformationStatement(targetInfo, destBlocks, srcBlocks);
	        	if (result != null) return result;
    		}
    	}

    	// (4-2)同一構造であるかチェックする。
    	if (programInfos[1].equalsLayout(programInfos[0])) {
			// (4-2)同じ構造位置に付加情報をコピーする.
    		result = copyLayoutInformationStatement(targetInfo, destBlocks, srcBlocks);
        	if (result != null) return result;
    	}

		return null;
    }


    /**
     * 付加情報と同一ブロック構造位置に付加情報をコピーする：DO,IF,SELECT文.
     * @param info		付加情報
     * @param destBlocks		コピー先ブロックリスト
     * @param srcBlocks			コピー元ブロックリスト
     * @return			付加情報差替結果
     */
	private ReplacementResult copyInformationBlock(IInformation info,
										IInformation[] destBlocks,
										IInformation[] srcBlocks) {
		if (info == null) return null;
		if (destBlocks == null) return null;
		if (srcBlocks == null) return null;
		// ブロックリスト数が異なれば付加情報コピー不可とする.
		if (destBlocks.length != srcBlocks.length) return null;

		ReplacementResult result = null;
		for (int i=0; i<srcBlocks.length; i++) {
			IInformation srcblock = srcBlocks[i];
			IInformation destblock = destBlocks[i];
			// 付加情報ブロックであるか？
			if (info != srcblock) continue;

			boolean ismatch = false;
			if (srcBlocks.length == 1) {
				// (3) 同じ構造ブロックに付加情報をコピーする.
				ismatch = true;
			}
			else {
				// (4-1) ブロック構造をチェックする
				ismatch = equalsParentLayout(destblock, srcblock);
			}
			if (ismatch) {
				// 付加情報のコピー
	        	TextInfo infoText = null;
	        	if (info.getInformation() != null) {
	        		infoText = new TextInfo(info.getInformation().getContent());
	        	}
				destblock.setInformation(infoText);
				// 付加情報差替:不確定
				result = new ReplacementResult(ReplacementResult.RESULT_STATUS.UNSURE,
										info.getInformation(),
										destblock, destblock,
										srcblock, srcblock);
			}
			break;
		}

		return result;
	}


    /**
     * 付加情報と同一ブロック構造位置に付加情報をコピーする：代入文,その他構文.
     * @param info		付加情報
     * @param destBlocks		コピー先ブロックリスト
     * @param srcBlocks			コピー元ブロックリスト
     * @return			付加情報差替結果
     */
	private ReplacementResult copyInformationStatement(IInformation info,
										IInformation[] destBlocks,
										IInformation[] srcBlocks) {
		if (info == null) return null;
		if (destBlocks == null) return null;
		if (srcBlocks == null) return null;
		if (destBlocks.length <= 0) return null;
		if (srcBlocks.length <= 0) return null;
		if (destBlocks.length != srcBlocks.length) return null;

		ReplacementResult result = null;
		IInformation srcblock = null;
		IInformation destblock = null;
		for (int i=0; i<srcBlocks.length; i++) {
			// 付加情報ブロックであるか？
			if (info == srcBlocks[i]) {
				srcblock = srcBlocks[i];
				destblock = destBlocks[i];
				break;
			}
		}
		if (srcblock == null || destblock == null) return null;
		if (!(srcblock instanceof IBlock)) return null;
		if (!(destblock instanceof IBlock)) return null;

		if (((IBlock)srcblock).getMotherBlock() != null
			&& ((IBlock)destblock).getMotherBlock() != null) {
			// 親ブロックが同じであること.
			String parentSrcText = ((IBlock)srcblock).getMotherBlock().toString();
			String parentDestText = ((IBlock)destblock).getMotherBlock().toString();
			if (!parentSrcText.equalsIgnoreCase(parentDestText)) {
				return null;
			}
			// ブロック階層が同じであること.
			if (!equalsParentLayout(destblock, srcblock)) {
				return null;
			}
		}
		else if (((IBlock)srcblock).getMotherBlock() != null
				|| ((IBlock)destblock).getMotherBlock() != null) {
			return null;
		}
		if (srcblock != null && destblock != null) {
			// 付加情報のコピー
        	TextInfo infoText = null;
        	if (info.getInformation() != null) {
        		infoText = new TextInfo(info.getInformation().getContent());
        	}
			destblock.setInformation(infoText);
			// 付加情報差替:不確定
			result = new ReplacementResult(ReplacementResult.RESULT_STATUS.UNSURE,
									info.getInformation(),
									destblock, destblock,
									srcblock, srcblock);
		}

		return result;
	}


	/**
	 * 付加情報のProgramUnitを取得する.
	 * @param info		コピー付加情報
	 * @param destDb		コピー先モジュール
	 * @param srcDb		    コピー元モジュール
	 * @return				付加情報のProgramUnit[programDest, programSrc]
	 */
    private ProgramUnit[] getInformationProgramUnits(IInformation info, Fortran destDb, Fortran srcDb) {
    	if (info == null) return null;
    	if (destDb == null) return null;
    	if (srcDb == null) return null;

    	Map<String, Module> srcModules = srcDb.getModules();
    	if (srcModules == null || srcModules.size() <= 0) {
    		return null;
    	}
    	ProgramUnit[] programInfos = null;
    	for ( String key : srcModules.keySet() ) {
    		Module srcModule = srcModules.get( key );
    		if (srcModule == null) continue;
    		Module destModule = destDb.module( key );
    		if (destModule == null) continue;

			// (1) 更新対象のモジュールであるか = [programDest, programSrc]
			programInfos = getInformationProgramUnits(info, destModule, srcModule);
			if (programInfos != null) {
				break;
			}
    	}
    	return programInfos;
    }

	/**
	 * 付加情報のProgramUnitを取得する.
	 * @param info		コピー付加情報
	 * @param destModule		コピー先モジュール
	 * @param srcModule		    コピー元モジュール
	 * @return				付加情報のProgramUnit[programDest, programSrc]
	 */
    private ProgramUnit[] getInformationProgramUnits(IInformation info, Module destModule, Module srcModule) {
    	if (info == null) return null;
    	if (destModule == null) return null;
    	if (srcModule == null) return null;

        // ネームスペース（モジュール＋サブルーチン）のProgramUnitを取得する
    	String namespace = info.getNamespace();
    	if (namespace == null || namespace.isEmpty()) {
    		return null;
    	}
        ProgramUnit programDest = getProgramUnitByNamespace(destModule, namespace);
        ProgramUnit programSrc = getProgramUnitByNamespace(srcModule, namespace);

        if (programDest == null || programSrc == null) {
        	return null;
        }
        return new ProgramUnit[]{programDest, programSrc};
    }

	/**
	 * 付加情報のコピーを行う：ProgramUnit.
	 * @param info			コピー付加情報
	 * @param destUnit		コピー先モジュール
	 * @param srcUnit		コピー元モジュール
	 * @return				付加情報差替結果
	 */
    private ReplacementResult copyInformation(IInformation info, ProgramUnit destUnit, ProgramUnit srcUnit) {
    	if (info == null) return null;
    	if (destUnit == null) return null;
    	if (srcUnit == null) return null;

    	ReplacementResult result = null;
    	if (info instanceof ProgramUnit) {
    		// namespaceのチェックを行う
    		String infoname = info.getNamespace();
    		String destname = destUnit.getNamespace();
    		String srcname = srcUnit.getNamespace();
    		if (!infoname.equalsIgnoreCase(destname)) return null;
    		if (!infoname.equalsIgnoreCase(srcname)) return null;
            // 付加情報生成
        	TextInfo infoText = null;
        	if (info.getInformation() != null) {
        		infoText = new TextInfo(info.getInformation().getContent());
        	}
        	destUnit.setInformation(infoText);
            // 付加情報差替成功
        	result = new ReplacementResult(ReplacementResult.RESULT_STATUS.SUCCESS,
											infoText, destUnit, destUnit, info, info);
    	}
    	else if (destUnit instanceof Module && srcUnit instanceof Module) {
    		result = copyInformationModule(info, (Module)destUnit, (Module)srcUnit);
		}
		else if (destUnit instanceof Procedure && srcUnit instanceof Procedure) {
    		result = copyInformationProcedure(info, (Procedure)destUnit, (Procedure)srcUnit);
		}

		return result;
    }


	/**
	 * 付加情報のコピーを行う：モジュール.
	 * @param info		コピー付加情報
	 * @param destModule		コピー先モジュール
	 * @param srcModule		    コピー元モジュール
	 * @return				付加情報差替結果
	 */
    private ReplacementResult copyInformationModule(IInformation info, Module destModule, Module srcModule) {
    	if (info == null) return null;
    	if (destModule == null) return null;
    	if (srcModule == null) return null;

        // ２つのProgramUnitが同じであるかチェックする.
    	ReplacementResult element = null;
        if (srcModule.equalsBlocks(destModule)) {
        	IInformation target = destModule.findBlock(info);
        	if (target == null) return null;
            // 付加情報生成
        	TextInfo infoText = null;
        	if (info.getInformation() != null) {
        		infoText = new TextInfo(info.getInformation().getContent());
        	}
        	target.setInformation(infoText);
            // 付加情報差替成功
            element = new ReplacementResult(ReplacementResult.RESULT_STATUS.SUCCESS,
											infoText, target, target, info, info);
        }
        return element;
    }


	/**
	 * 付加情報のコピーを行う：プロシージャ.
	 * @param info		コピー付加情報
	 * @param destProcedure		コピー先プロシージャ
	 * @param srcProcedure		    コピー元プロシージャ
	 * @return				付加情報差替結果
	 */
    private ReplacementResult copyInformationProcedure(IInformation info, Procedure destProcedure, Procedure srcProcedure) {
    	if (info == null) return null;
    	if (destProcedure == null) return null;
    	if (srcProcedure == null) return null;

        // ２つのProgramUnitが同じであるかチェックする.
    	ReplacementResult element = null;
        if (srcProcedure.equalsBlocks(destProcedure)) {
        	IInformation target = destProcedure.findBlock(info);
        	if (target == null) return null;
            // 付加情報生成
        	TextInfo infoText = null;
        	if (info.getInformation() != null) {
        		infoText = new TextInfo(info.getInformation().getContent());
        	}
            target.setInformation(infoText);
            // 付加情報差替成功
            element = new ReplacementResult(ReplacementResult.RESULT_STATUS.SUCCESS,
											infoText, target, target, info, info);
        }
        return element;
    }

	/**
	 * 同一構造位置に付加情報のコピーを行う：DO,IF,SELECT文.
	 * @param info		付加情報
	 * @param destUnit		コピー先ProgramUnit
	 * @param srcUnit		    コピー元ProgramUnit
	 * @return				付加情報差替結果
	 */
    private ReplacementResult copyLayoutInformationBlock(IInformation info, ProgramUnit destUnit, ProgramUnit srcUnit) {
    	if (info == null) return null;
    	if (destUnit == null) return null;
    	if (srcUnit == null) return null;

    	String layoutId = info.getLayoutID();
    	IInformation destInfo = destUnit.findInformationLayoutID(layoutId);
    	IInformation srcInfo = srcUnit.findInformationLayoutID(layoutId);
    	if (srcInfo == null) return null;
    	if (destInfo == null) return null;
    	if (srcInfo != info) return null;

        // 付加情報生成
    	TextInfo infoText = null;
    	if (info.getInformation() != null) {
    		infoText = new TextInfo(info.getInformation().getContent());
    	}
        destInfo.setInformation(infoText);
        // 付加情報差替成功
        ReplacementResult result = new ReplacementResult(ReplacementResult.RESULT_STATUS.UNSURE,
										infoText, destInfo, destInfo, srcInfo, srcInfo);

		return result;
    }


	/**
	 * 同一構造位置に付加情報のコピーを行う：代入文,その他構文.
	 * @param targetinfo		付加情報
     * @param destBlocks		コピー先ブロックリスト
     * @param srcBlocks			コピー元ブロックリスト
	 * @return				付加情報差替結果
	 */
    private ReplacementResult copyLayoutInformationStatement(
    				IInformation targetinfo,
					IInformation[] destBlocks,
					IInformation[] srcBlocks) {
    	if (targetinfo == null) return null;
    	if (destBlocks == null) return null;
    	if (srcBlocks == null) return null;

    	String layoutId = targetinfo.getLayoutID();
		List<IInformation> srcinfos = new ArrayList<IInformation>();
		List<IInformation> destinfos = new ArrayList<IInformation>();
    	for (IInformation info : destBlocks) {
    		String id = info.getLayoutID();
    		if (layoutId.equalsIgnoreCase(id)) {
    			destinfos.add(info);
    		}
    	}
    	for (IInformation info : srcBlocks) {
    		String id = info.getLayoutID();
    		if (layoutId.equalsIgnoreCase(id)) {
    			srcinfos.add(info);
    		}
    	}
    	if (srcinfos.size() != destinfos.size()) {
    		return null;
    	}
    	IInformation destinfo = null;
    	IInformation srcinfo = null;
    	for (int i=0; i<srcinfos.size(); i++) {
    		if (targetinfo == srcinfos.get(i)) {
    			srcinfo = srcinfos.get(i);
    			destinfo = destinfos.get(i);
    			break;
    		}
    	}
        // 付加情報生成
    	TextInfo infoText = null;
    	if (targetinfo.getInformation() != null) {
    		infoText = new TextInfo(targetinfo.getInformation().getContent());
    	}
    	destinfo.setInformation(infoText);
        // 付加情報差替成功
        ReplacementResult result = new ReplacementResult(ReplacementResult.RESULT_STATUS.UNSURE,
										infoText, destinfo, destinfo, srcinfo, srcinfo);

		return result;
    }

    /**
     * ネームスペース（モジュール＋サブルーチン）のProgramUnitを取得する
     * @param destDb		検索対象データベース
     * @param namespace		検索ネームスペース
     * @return				ProgramUnit
     */
    private ProgramUnit getProgramUnitByNamespace(Fortran destDb, String namespace) {
    	if (destDb == null) return null;
    	Map<String, Module> modules = destDb.getModules();
    	if (modules == null || modules.size() <= 0) {
    		return null;
    	}

    	ProgramUnit unit = null;
    	for ( String key : modules.keySet() ) {
    		Module module = modules.get( key );
    		unit = module.findProgramUnitBy(namespace);
    		break;
    	}
    	return unit;
    }

    /**
     * ネームスペース（モジュール＋サブルーチン）のProgramUnitを取得する
     * @param destModule		検索対象モジュール
     * @param namespace		検索ネームスペース
     * @return				ProgramUnit
     */
    private ProgramUnit getProgramUnitByNamespace(Module destModule, String namespace) {
    	if (destModule == null) return null;
    	ProgramUnit unit = destModule.findProgramUnitBy(namespace);
    	return unit;
    }


	/**
	 * InformationBlocksにInformationBlockが含まれているかチェックする.
	 * @param infos		InformationBlocks(=InformationBlockリスト)
	 * @param src		検索InformationBlock
	 * @return			true=含まれる
	 */
	private boolean containsInformationBlock(InformationBlocks infos, InformationBlock src) {
		if (infos == null) return false;
		if (src == null) return false;

		for (InformationBlock info : infos) {
			if (src.getStartBlock() != info.getStartBlock()) continue;
			if (src.getEndBlock() != info.getEndBlock()) continue;
			return true;
		}

		return false;
	}

	/**
	 * 付加情報差替情報モデルを取得する.
	 * @return replaceModel		付加情報差替情報モデルを
	 */
	public ReplacementResultTableModel getReplaceModel() {
		return this.replaceModel;
	}

	/**
	 * 付加情報差替情報モデルをセットする
	 * @param model    付加情報差替情報モデル
	 */
	public void setReplaceModel(ReplacementResultTableModel model) {
		this.replaceModel = model;
	}

	/**
	 * 付加情報差替結果のマージを行う.
	 * @param info		付加情報
	 * @param resultStart   開始付加情報差替結果
	 * @param resultEnd     終了付加情報差替結果
	 * @return		マージ付加情報差替結果
	 */
	private ReplacementResult margeReplacementResults(IInformation info,
											ReplacementResult resultStart,
											ReplacementResult resultEnd) {
		RESULT_STATUS[] statusOrder = {RESULT_STATUS.FAILURE, RESULT_STATUS.UNSURE, RESULT_STATUS.SUCCESS};
		int statusId = 2;
		for (int i=0; i<statusOrder.length; i++) {
			if (resultStart.getStatus() == statusOrder[i]) {
				statusId = i;
				break;
			}
		}
		for (int i=0; i<statusOrder.length; i++) {
			if (resultEnd.getStatus() == statusOrder[i]) {
				if (statusId > i) {
					statusId = i;
				}
				break;
			}
		}
		RESULT_STATUS status = statusOrder[statusId];
		if (status == RESULT_STATUS.SUCCESS) {
			status = RESULT_STATUS.UNSURE;
			// 終了ステータス:○の場合、同一サブルーチンかチェックする.
			if (info instanceof InformationBlock) {
				IInformation start = ((InformationBlock)info).getStartBlock();
				IInformation end = ((InformationBlock)info).getEndBlock();
				if (start != null && end != null
					&& start.getNamespace() != null && end.getNamespace() != null) {
					if (start.getNamespace().equalsIgnoreCase(end.getNamespace())) {
						status = RESULT_STATUS.SUCCESS;
					}
				}
			}
		}
		ReplacementResult result = new ReplacementResult(status,
										info.getInformation(),
										resultStart.getCurrentStartPosition(),
										resultEnd.getCurrentEndPosition(),
										resultStart.getOldStartPosition(),
										resultEnd.getOldEndPosition());

		return result;
	}


	/**
	 * DO,IF,SELECT文であるかチェックする.
	 * @param  info   付加情報ブロック
	 * @return    DO,IF,SELECT文
	 */
	private boolean isBlock(IInformation info) {
		if (!(info instanceof IBlock)) return false;
		if (((IBlock)info).getBlockType() == BlockType.SELECTION
			|| ((IBlock)info).getBlockType() == BlockType.REPETITION
			|| ((IBlock)info).getBlockType() == BlockType.CONDITION) {
			return true;
		}

		return false;

	}

	/**
	 * 同じブロック階層であるかチェックする.
	 * 親ブロック階層が同じであるかチェックする.
	 * 親ブロックの種別のみチェックする.
	 * @param destinfo		ブロック１
	 * @param srcinfo		ブロック２
	 * @return			true=同一ブロック構造
	 */
	private boolean equalsParentLayout(IInformation destinfo, IInformation srcinfo) {
		if (destinfo == null) return false;
		if (srcinfo == null) return false;
		if (!(destinfo instanceof IBlock)) return false;
		if (!(srcinfo instanceof IBlock)) return false;

		if (((IBlock)srcinfo).getBlockType() != ((IBlock)destinfo).getBlockType()) {
			return false;
		}

		IBlock srcparent = ((IBlock)srcinfo).getMotherBlock();
		IBlock destparent = ((IBlock)destinfo).getMotherBlock();

		// ブロック構造をチェックする
		while (srcparent != null && destparent != null) {
			if (srcparent.getBlockType() != destparent.getBlockType()) {
				return false;
			}
			srcparent = srcparent.getMotherBlock();
			destparent = destparent.getMotherBlock();
			if (srcparent == null && destparent == null) {
				return true;
			}
			else if (srcparent == null || destparent == null) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 同じブロック構造であるかチェックする.
	 * 親ブロック構造が同じであるかチェックする.
	 * 親ブロックの構文も同じであること.
	 * @param destinfo		ブロック１
	 * @param srcinfo		ブロック２
	 * @return			true=同一ブロック構造
	 */
	private boolean equalsParentBlock(IInformation destinfo, IInformation srcinfo) {
		if (destinfo == null) return false;
		if (srcinfo == null) return false;
		if (!(destinfo instanceof IBlock)) return false;
		if (!(srcinfo instanceof IBlock)) return false;

		if (!srcinfo.toString().equalsIgnoreCase(destinfo.toString())) {
			return false;
		}

		IBlock srcparent = ((IBlock)srcinfo).getMotherBlock();
		IBlock destparent = ((IBlock)destinfo).getMotherBlock();

		// ブロック構造をチェックする
		while (srcparent != null && destparent != null) {
			if (!srcparent.toString().equalsIgnoreCase(destparent.toString())) {
				return false;
			}
			srcparent = srcparent.getMotherBlock();
			destparent = destparent.getMotherBlock();
			if (srcparent == null && destparent == null) {
				return true;
			}
			else if (srcparent == null || destparent == null) {
				return false;
			}
		}
		return true;
	}

	/**
	 * プロジェクトモデルを設定する.
	 * @param model 		プロジェクトモデル
	 */
	public void setProjectModel(ProjectModel model) {
		this.projectModel = model;
	}
}


