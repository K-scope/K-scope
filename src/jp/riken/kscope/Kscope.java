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
package jp.riken.kscope;

import java.io.IOException;
import java.io.InputStream;
//import java.lang.reflect.Method;

import java.awt.EventQueue;
import java.awt.Toolkit;



//import java.util.Locale;
import javax.swing.JOptionPane;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.plaf.metal.MetalLookAndFeel;

import jp.riken.kscope.ThemeWindows;
import jp.riken.kscope.gui.MainFrame;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.utils.Logger;
import jp.riken.kscope.utils.ResourceUtils;

/**
 * メインクラス
 *
 * @author RIKEN
 */
public class Kscope {

    /**
     * メインメソッド
     *
     * @param args 起動引数
     */
    public static void main(String args[]) {
        // 初期設定
        // MacOSXでのJava実行環境用のシステムプロパティの設定.
        boolean debug = (System.getenv("DEBUG")!= null);
        String version = KscopeProperties.APPLICATION_VERSION;
        if (debug) {
            System.out.println("Java: "+ System.getProperty("java.version"));
        }
        System.out.println(KscopeProperties.APPLICATION_NAME+" v"+version);
        if (debug) {
            System.out.println("For debugging use DEBUG env var: "
                    + "\"high\" - much debug info, "
                    + "\"extreme\" - too much debug info,"
                    + " any other value, but not empy - less debug info.\nCurrently DEBUG is set to \""+System.getenv("DEBUG")+"\"");
        }
        if (KscopeProperties.isMac()) {
            // JFrameにメニューをつけるのではなく、一般的なOSXアプリ同様に画面上端のスクリーンメニューにする.
            System.setProperty("apple.laf.useScreenMenuBar", "true");
            System.setProperty("com.apple.macos.smallTabs", "true");
            try {
                UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            } catch (Exception ex) {
                ex.printStackTrace();
                return;
            }
            // スクリーンメニュー左端に表記されるアプリケーション名を設定する (何も設定しないとクラス名になる。)
            String title = Message.getString("application.name");
            System.setProperty( "com.apple.mrj.application.apple.menu.about.name",title);
        }
        else if (KscopeProperties.isWindows()) {
            try {
                UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                MetalLookAndFeel.setCurrentTheme(new ThemeWindows());
                UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
            } catch (Exception ex) {
                ex.printStackTrace();
                return;
            }
        }
        // Linuxの場合は設定しない。
        else {
                /*
            try {
                    String LandF;
                    LandF = UIManager.getSystemLookAndFeelClassName();
                       // Can set GTK Look and Feel, but it is very slow with X11
                // LandF = "com.sun.java.swing.plaf.gtk.GTKLookAndFeel";
                    UIManager.setLookAndFeel(LandF);
            }
            catch (UnsupportedLookAndFeelException ex) {
                ex.printStackTrace();
                return;
            }
            catch (ClassNotFoundException ex) {
                ex.printStackTrace();
                return;
            }
            catch (InstantiationException ex) {
                ex.printStackTrace();
                return;
            }
            catch (IllegalAccessException ex) {
                 ex.printStackTrace();
                return;
            } */
        }

        Kscope app = new Kscope();
        app.initApp();

        // メインフレーム表示
        EventQueue.invokeLater(new Runnable() {
            @Override
            public void run() {
                try {
                    showMainFrame();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        });
    }

    /**
     * メインフレームを表示する。
     */
    private static void showMainFrame() {

        try {
            // メインフレームの作成
            MainFrame frame = new MainFrame();

            // コントローラの作成
            AppController ctrl = new AppController();

            // メインフレームの初期化
            frame.initialize(ctrl);

            // コントローラの初期化
            ctrl.initialize(frame);

            String status = Message.getString("go.status.start");
            Application.status.setMessageMain(status);

            // メインフレームの表示
            frame.setVisible(true);

        } catch (Exception ex) {
            ex.printStackTrace();

            // エラーメッセージ
            String title = Message.getString("go.message.start.error.title");
            String msg = ex.getMessage();
            if (msg == null) {
                msg = ex.toString();
            }
            JOptionPane.showMessageDialog(
                    null,
                    msg,
                    title,
                    JOptionPane.ERROR_MESSAGE);

            // 終了
            System.exit(-1);

            return;
        }
    }

    /**
     * アプリケーションの初期化を行う。
     */
    private void initApp() {
        // ログの初期化を行う。
        // true=デバッグモードログ出力（CONFIGレベルも出力する)
        inititalizeLogging(true);

        KscopeProperties.PROPERTIES_FILE = KscopeProperties.PROPERTIES_FILE_DEFAULT;

        try {
            ResourceUtils.setRootAppClass(this.getClass());
            ResourceUtils.addFile("resources/icons");
            // add class path at 2015/12/01 by @hira
            ResourceUtils.addFile("templates");

        } catch (IOException e) {
            JOptionPane.showMessageDialog(null, e, "Error", JOptionPane.ERROR_MESSAGE);
            e.printStackTrace();
        }

        // プロパティファイルの読込
        KscopeProperties.loadXml();
    }

    /**
     * 出力ログ設定
     *
     * @param debugMode 出力デバッグモードフラグ
     */
    private void inititalizeLogging(boolean debugMode) {
        // ログ設定
        InputStream logProp = getClass().getResourceAsStream("logging.properties");

        String appName = this.getClass().getName();
        Logger.configure(logProp, appName, debugMode);

        // システムプロパティ値出力
        String[] props = {"java.version", "java.vm.version",
            "java.runtime.version", "java.vendor", "java.compiler",
            "os.name", "os.version", "os.arch", "user.home", "java.home",
            "java.class.path",};

        try {
            for (int i = 0; i < props.length; i++) {
                Logger.info(props[i] + "=" + System.getProperty(props[i]));
            }
        } catch (Exception e) {
            e.printStackTrace();
            Logger.logOff();
        }

        String desktopProps = "awt.multiClickInterval";
        Logger.info(desktopProps + "=" + Toolkit.getDefaultToolkit().getDesktopProperty(desktopProps));
    }

}
