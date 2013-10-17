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
package jp.riken.kscope.dialog;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.geom.Rectangle2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.utils.SwingUtils;


/**
 * プロファイラ凡例ダイアログ
 * @author riken
 */
public class ProfilerLegendDialog extends javax.swing.JDialog implements PropertyChangeListener {
    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** プロファイラ設定 */
    private ProfilerProperties properties;
    /** コスト表示:手続色ラベル */
    private JLabel lblColorProcedure;
    /** コスト表示:ループ色ラベル */
    private JLabel lblColorLoop;
    /** コスト表示:ライン色ラベル */
    private JLabel lblColorLine;

    /**
     * コンストラクタ
    
     *
     * @param owner	親フレーム
     * @param modal	true=モーダルダイアログを表示する
     */
    public ProfilerLegendDialog(Frame owner, boolean modal) {
        super(owner, modal);
        initGUI();
    }

    /**
     * コンストラクタ
     *
     * @param owner	親フレーム
     * @param modal	true=モーダルダイアログを表示する
     * @param properties	プロファイラプロパティ
     */
    public ProfilerLegendDialog(Frame owner, boolean modal, ProfilerProperties properties) {
        super(owner, modal);
        setProperties(properties);
        initGUI();
    }

    /**
     * GUI初期化を行う。
     */
    private void initGUI() {

        try {
            GridBagLayout thisLayout = new GridBagLayout();
            thisLayout.columnWidths = new int[]{80};
            thisLayout.rowHeights = new int[]{30, 30};
            thisLayout.columnWeights = new double[]{0.0};
            thisLayout.rowWeights = new double[]{0, 0};
            getContentPane().setLayout(thisLayout);

            // バーグラフ表示色
            {
                JPanel panel = new JPanel();
                GridBagLayout layout = new GridBagLayout();
                layout.columnWidths = new int[]{40, 40};
                layout.rowHeights = new int[]{30, 30, 30};
                layout.columnWeights = new double[]{1.0, 1.0};
                layout.rowWeights = new double[]{0, 0, 0};
                panel.setLayout(layout);
                panel.setBorder(new TitledBorder(
                        Message.getString("profilerlegenddialog.costsourcecolor.title")));  // コスト情報表示色
                this.add(panel, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));

                Dimension color_size = new Dimension(40, 22);
                // 手続
                {
                    JLabel lblName = new JLabel(PROFILERINFO_TYPE.COST_PROCEDURE.getShortName()); //手続
                    panel.add(lblName, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 10), 0, 0));
                    lblColorProcedure = new JLabel();
                    lblColorProcedure.setBorder(new LineBorder(Color.BLACK, 1));
                    lblColorProcedure.setOpaque(true);
                    lblColorProcedure.setPreferredSize(color_size);
                    panel.add(lblColorProcedure, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                }
                // ループ
                {
                    JLabel lblName = new JLabel(PROFILERINFO_TYPE.COST_LOOP.getShortName()); //ループ
                    panel.add(lblName, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 10), 0, 0));
                    lblColorLoop = new JLabel();
                    lblColorLoop.setBorder(new LineBorder(Color.BLACK, 1));
                    lblColorLoop.setOpaque(true);
                    lblColorLoop.setPreferredSize(color_size);
                    panel.add(lblColorLoop, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                }
                // ライン
                {
                    JLabel lblName = new JLabel(PROFILERINFO_TYPE.COST_LINE.getShortName()); //ライン
                    panel.add(lblName, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 10), 0, 0));
                    lblColorLine = new JLabel();
                    lblColorLine.setBorder(new LineBorder(Color.BLACK, 1));
                    lblColorLine.setOpaque(true);
                    lblColorLine.setPreferredSize(color_size);
                    panel.add(lblColorLine, new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                }
            }

            // コストルーラ表示色
            {
                JPanel panel = new JPanel();
                GridBagLayout layout = new GridBagLayout();
                layout.columnWidths = new int[]{40, 40, 40};
                layout.rowHeights = new int[]{30, 30};
                layout.columnWeights = new double[]{0.0, 1.0, 0.0};
                layout.rowWeights = new double[]{0, 0};
                panel.setLayout(layout);
                panel.setBorder(new TitledBorder(
                        Message.getString("profilerlegenddialog.costrulercolor.title")));  // コストルーラ表示色
                this.add(panel, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(10, 0, 0, 0), 0, 0));

                // グラデーション色
                JPanel panelGradient = new JPanel() {
                    /**
                     * シリアル番号
                     */
                    private static final long serialVersionUID = 1L;

                    /**
                     * プロファイラルーラのグラデーションを描画する.
                     */
                    @Override
                    protected void paintComponent(Graphics g) {
                        super.paintComponent(g);

                        if (ProfilerLegendDialog.this.properties == null) {
                            return;
                        }
                        Graphics2D g2 = (Graphics2D) g;
                        // 描画領域
                        Rectangle rectDraw = this.getBounds();
                        int step = 256;
                        float width = (float) rectDraw.width / (float) step;
                        Color minColor = ProfilerLegendDialog.this.properties.getRulerColorMin();
                        Color maxColor = ProfilerLegendDialog.this.properties.getRulerColorMax();
                        for (int i = 0; i < step; i++) {
                            float pos_x = (float) i * width;
                            Rectangle2D.Float rect2d = new Rectangle2D.Float(pos_x, 0, width, 40);
                            float ratio = (float) i / (float) step;
                            Color valueColor = SwingUtils.getGradientHsbColor(ratio, minColor, maxColor);
                            g2.setColor(valueColor);
                            g2.fill(rect2d);
                        }
                    }
                };
                panelGradient.setPreferredSize(new Dimension(200, 40));
                panelGradient.setBorder(new LineBorder(Color.BLACK, 1));
                panel.add(panelGradient, new GridBagConstraints(0, 0, 3, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));

                // 凡例ラベル
                JLabel lblMin = new JLabel("min");
                panel.add(lblMin, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                JLabel lblMax = new JLabel("max");
                panel.add(lblMax, new GridBagConstraints(2, 1, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            }
            this.setTitle(Message.getString("profilerlegenddialog.costinfolegend.title"));  // コスト情報凡例
            this.setSize(220, 260);
            this.setResizable(false);

            // プロファイラプロパティの設定色を表示する.
            paintProfilerProperties();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * ダイアログを表示する。
     *
     * @return ダイアログの閉じた時のボタン種別
     */
    public int showDialog() {
        // ダイアログ表示
        this.setVisible(true);
        return 0;
    }

    /**
     * プロファイラプロパティの設定色を表示する.
     */
    private void paintProfilerProperties() {
        if (this.properties == null) {
            return;
        }
        lblColorProcedure.setBackground(this.properties.getCostinfoBarcolorProcedure());
        lblColorLoop.setBackground(this.properties.getCostinfoBarcolorLoop());
        lblColorLine.setBackground(this.properties.getCostinfoBarcolorLine());
        this.repaint();
    }

    /**
     * プロファイラプロパティの変更通知
     */
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        paintProfilerProperties();
    }

    /**
     * プロファイラプロパティを設定する
     *
     * @param properties プロファイラプロパティ
     */
    public void setProperties(ProfilerProperties properties) {
        this.properties = properties;
        if (this.properties != null) {
            this.properties.removePropertyChangeListener(this);
            this.properties.addPropertyChangeListener(this);
        }
        paintProfilerProperties();
    }
}
