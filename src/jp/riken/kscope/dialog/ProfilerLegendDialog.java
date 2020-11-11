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
 * Profiler Legend Dialog
 *
 * @author RIKEN
 */
public class ProfilerLegendDialog extends javax.swing.JDialog implements PropertyChangeListener {
  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Profiler settings */
  private ProfilerProperties properties;
  /** Cost display: Procedure color label */
  private JLabel lblColorProcedure;
  /** Cost display: Loop color label */
  private JLabel lblColorLoop;
  /** Cost display: Line color label */
  private JLabel lblColorLine;

  /**
   * Constructor
   *
   * @param owner parent frame
   * @param modal true = Show modal dialog
   */
  public ProfilerLegendDialog(Frame owner, boolean modal) {
    super(owner, modal);
    initGUI();
  }

  /**
   * Constructor
   *
   * @param owner parent frame
   * @param modal true = Show modal dialog
   * @param properties Profiler properties
   */
  public ProfilerLegendDialog(Frame owner, boolean modal, ProfilerProperties properties) {
    super(owner, modal);
    setProperties(properties);
    initGUI();
  }

  /** Initialize the GUI. */
  private void initGUI() {

    try {
      GridBagLayout thisLayout = new GridBagLayout();
      thisLayout.columnWidths = new int[] {80};
      thisLayout.rowHeights = new int[] {30, 30};
      thisLayout.columnWeights = new double[] {0.0};
      thisLayout.rowWeights = new double[] {0, 0};
      getContentPane().setLayout(thisLayout);

      // Bar graph display color
      {
        JPanel panel = new JPanel();
        GridBagLayout layout = new GridBagLayout();
        layout.columnWidths = new int[] {40, 40};
        layout.rowHeights = new int[] {30, 30, 30};
        layout.columnWeights = new double[] {1.0, 1.0};
        layout.rowWeights = new double[] {0, 0, 0};
        panel.setLayout(layout);
        panel.setBorder(
            new TitledBorder(
                Message.getString(
                    "profilerlegenddialog.costsourcecolor.title"))); // Cost information display
                                                                     // color
        this.add(
            panel,
            new GridBagConstraints(
                0,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.CENTER,
                GridBagConstraints.HORIZONTAL,
                new Insets(0, 0, 0, 0),
                0,
                0));

        Dimension color_size = new Dimension(40, 22);
        // Procedure
        {
          JLabel lblName = new JLabel(PROFILERINFO_TYPE.COST_PROCEDURE.getShortName()); // procedure
          panel.add(
              lblName,
              new GridBagConstraints(
                  0,
                  0,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.EAST,
                  GridBagConstraints.NONE,
                  new Insets(0, 0, 0, 10),
                  0,
                  0));
          lblColorProcedure = new JLabel();
          lblColorProcedure.setBorder(new LineBorder(Color.BLACK, 1));
          lblColorProcedure.setOpaque(true);
          lblColorProcedure.setPreferredSize(color_size);
          panel.add(
              lblColorProcedure,
              new GridBagConstraints(
                  1,
                  0,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.WEST,
                  GridBagConstraints.NONE,
                  new Insets(0, 0, 0, 0),
                  0,
                  0));
        }
        // loop
        {
          JLabel lblName = new JLabel(PROFILERINFO_TYPE.COST_LOOP.getShortName()); // loop
          panel.add(
              lblName,
              new GridBagConstraints(
                  0,
                  1,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.EAST,
                  GridBagConstraints.NONE,
                  new Insets(0, 0, 0, 10),
                  0,
                  0));
          lblColorLoop = new JLabel();
          lblColorLoop.setBorder(new LineBorder(Color.BLACK, 1));
          lblColorLoop.setOpaque(true);
          lblColorLoop.setPreferredSize(color_size);
          panel.add(
              lblColorLoop,
              new GridBagConstraints(
                  1,
                  1,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.WEST,
                  GridBagConstraints.NONE,
                  new Insets(0, 0, 0, 0),
                  0,
                  0));
        }
        // line
        {
          JLabel lblName = new JLabel(PROFILERINFO_TYPE.COST_LINE.getShortName()); // line
          panel.add(
              lblName,
              new GridBagConstraints(
                  0,
                  2,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.EAST,
                  GridBagConstraints.NONE,
                  new Insets(0, 0, 0, 10),
                  0,
                  0));
          lblColorLine = new JLabel();
          lblColorLine.setBorder(new LineBorder(Color.BLACK, 1));
          lblColorLine.setOpaque(true);
          lblColorLine.setPreferredSize(color_size);
          panel.add(
              lblColorLine,
              new GridBagConstraints(
                  1,
                  2,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.WEST,
                  GridBagConstraints.NONE,
                  new Insets(0, 0, 0, 0),
                  0,
                  0));
        }
      }

      // Cost ruler display color
      {
        JPanel panel = new JPanel();
        GridBagLayout layout = new GridBagLayout();
        layout.columnWidths = new int[] {40, 40, 40};
        layout.rowHeights = new int[] {30, 30};
        layout.columnWeights = new double[] {0.0, 1.0, 0.0};
        layout.rowWeights = new double[] {0, 0};
        panel.setLayout(layout);
        panel.setBorder(
            new TitledBorder(
                Message.getString(
                    "profilerlegenddialog.costrulercolor.title"))); // Cost ruler display color
        this.add(
            panel,
            new GridBagConstraints(
                0,
                1,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.CENTER,
                GridBagConstraints.HORIZONTAL,
                new Insets(10, 0, 0, 0),
                0,
                0));

        // Gradient color
        JPanel panelGradient =
            new JPanel() {
              /** Serial number */
              private static final long serialVersionUID = 1L;

              /** Draw a profile laura gradient. */
              @Override
              protected void paintComponent(Graphics g) {
                super.paintComponent(g);

                if (ProfilerLegendDialog.this.properties == null) {
                  return;
                }
                Graphics2D g2 = (Graphics2D) g;
                // Drawing area
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
        panel.add(
            panelGradient,
            new GridBagConstraints(
                0,
                0,
                3,
                1,
                0.0,
                0.0,
                GridBagConstraints.CENTER,
                GridBagConstraints.BOTH,
                new Insets(0, 0, 0, 0),
                0,
                0));

        // Legend label
        JLabel lblMin = new JLabel("min");
        panel.add(
            lblMin,
            new GridBagConstraints(
                0,
                1,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
        JLabel lblMax = new JLabel("max");
        panel.add(
            lblMax,
            new GridBagConstraints(
                2,
                1,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.EAST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
      }
      this.setTitle(
          Message.getString(
              "profilerlegenddialog.costinfolegend.title")); // Cost information legend
      this.setSize(220, 260);
      this.setResizable(false);

      // Display the profiler property setting color.
      paintProfilerProperties();

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Display a dialog.
   *
   * @return Button type when the dialog is closed
   */
  public int showDialog() {
    // Dialog display
    this.setVisible(true);
    return 0;
  }

  /** Display the profiler property setting color. */
  private void paintProfilerProperties() {
    if (this.properties == null) {
      return;
    }
    lblColorProcedure.setBackground(this.properties.getCostinfoBarcolorProcedure());
    lblColorLoop.setBackground(this.properties.getCostinfoBarcolorLoop());
    lblColorLine.setBackground(this.properties.getCostinfoBarcolorLine());
    this.repaint();
  }

  /** Profiler property change notification */
  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    paintProfilerProperties();
  }

  /**
   * Set profiler properties
   *
   * @param properties Profiler properties
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
