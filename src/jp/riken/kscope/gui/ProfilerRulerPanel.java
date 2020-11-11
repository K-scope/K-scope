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
package jp.riken.kscope.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Observable;
import java.util.Observer;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.SwingUtilities;
import javax.swing.border.LineBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.model.SourceCodeModel;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Profile Laura Panel Class
 *
 * @author RIKEN
 */
public class ProfilerRulerPanel extends JPanel
    implements ChangeListener, MouseListener, Observer, MouseMotionListener, ComponentListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Display cost information name */
  private JLabel labelName;
  /** Cost display panel */
  private JPanel panelProfiler;
  /** Corner panel */
  private JPanel panelConer;
  /** Source Code Panel */
  private ScrollCodePane panelScrollCode;
  /** Source code display panel */
  private JPanel panelCodeView;
  /** Profiler data */
  private List<ISourceBargraph> listData;
  /** Profiler Properties */
  private ProfilerProperties properties;
  /** Default width */
  private int DEFLALT_WIDTH = 60;
  /** Source View Panel Default Height */
  private int DEFLALT_VIEWHEIGHT = 1;
  /** Profiler data display height */
  private float PROFILER_DATA_MINHEIGHT = 3.0F;
  /** Source code display panel border color = Color.GRAY */
  private final Color DEFAULT_BORDERCOLORPANEL = Color.GRAY;
  /** Source code display panel background color new Color (220, 220, 220, 64) */
  @SuppressWarnings("unused")
  private final Color DEFAULT_BACKCOLORPANEL = new Color(220, 220, 220, 64);
  /** Profiler data: Maximum value */
  private float maxValue;
  /** Profiler data: Minimum value */
  private float minValue;

  /** Constructor */
  public ProfilerRulerPanel() {
    super();
    initGUI();
  }

  /**
   * Constructor
   *
   * @param panelCode Source code panel
   */
  public ProfilerRulerPanel(ScrollCodePane panelCode) {
    super();
    this.panelScrollCode = panelCode;
    initGUI();
  }

  /**
   * Initialize. <br>
   * Place the profiler ruler panel.
   */
  private void initGUI() {
    try {
      BorderLayout thisLayout = new BorderLayout();
      this.setLayout(thisLayout);

      // Profiler display cost name
      {
        this.labelName = new JLabel();
        this.add(this.labelName, BorderLayout.NORTH);
        this.labelName.setBorder(new LineBorder(Color.BLACK, 1));
        this.labelName.setText("Procedure");
        Dimension size = new Dimension(DEFLALT_WIDTH, 16);
        this.labelName.setPreferredSize(size);
        this.labelName.setHorizontalAlignment(JLabel.CENTER);
        Font org = this.labelName.getFont();
        Font font = org.deriveFont(Font.PLAIN, (float) 11.0);
        this.labelName.setFont(font);
      }
      // Profiler preview panel
      {
        this.panelProfiler =
            new JPanel() {
              /** Serial number */
              private static final long serialVersionUID = 1L;

              /** Draw profiler data. */
              @Override
              protected void paintComponent(Graphics g) {
                super.paintComponent(g);
                paintProfilerData(g);
              }
            };
        this.add(this.panelProfiler, BorderLayout.CENTER);
        this.panelProfiler.setBorder(new LineBorder(Color.BLACK, 1));
        this.panelProfiler.setLayout(null);
      }
      // Corner panel
      {
        this.panelConer = new JPanel();
        this.panelConer.setOpaque(true);
        this.add(this.panelConer, BorderLayout.SOUTH);
      }
      // Source code display panel
      {
        this.panelCodeView = new JPanel();
        this.panelCodeView.setBorder(new LineBorder(DEFAULT_BORDERCOLORPANEL, 1));
        // this.panelCodeView.setBackground(DEFAULT_BACKCOLORPANEL);
        this.panelCodeView.setBackground(Color.RED);
        this.panelProfiler.add(this.panelCodeView);
      }

      // Source code panel scroll event
      if (this.panelScrollCode != null) {
        this.panelScrollCode.getViewport().addChangeListener(this);
        // Set the observer.
        if (this.panelScrollCode.getModel() != null) {
          this.panelScrollCode.getModel().addObserver(this);
        }
      }
      // Mouse click event
      this.panelProfiler.addMouseListener(this);
      this.panelProfiler.addMouseMotionListener(this);
      // Panel resizing event
      this.addComponentListener(this);

      // Set the source code panel display position.
      setPanelsBounds();

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /** Source code panel scroll change event */
  @Override
  public void stateChanged(ChangeEvent e) {
    // Set the source code panel display position.
    setPanelsBounds();
  }

  /** Set the source code panel display position. */
  private void setPanelsBounds() {
    if (this.panelScrollCode == null) return;

    // Corner panel
    JScrollBar barVertical = this.panelScrollCode.getVerticalScrollBar();
    JScrollBar barHorizontal = this.panelScrollCode.getHorizontalScrollBar();
    int cornerhight = 0;
    if (barVertical != null && barVertical.isShowing()) {
      cornerhight = barVertical.getWidth();
    }
    if (barHorizontal != null && barHorizontal.isShowing()) {
      cornerhight += barHorizontal.getHeight();
    }
    Dimension sizeCorner = new Dimension(DEFLALT_WIDTH, cornerhight);
    this.panelConer.setPreferredSize(sizeCorner);

    // redraw
    this.revalidate();
    this.repaint();

    // Source code panel
    if (barVertical != null && barVertical.isShowing()) {
      // Display position
      Rectangle viewrect = this.panelScrollCode.getViewport().getViewRect();
      Rectangle coderect = this.panelScrollCode.getSourcePane().getBounds();
      int height = this.panelProfiler.getHeight();
      float top = (float) viewrect.y / (float) coderect.height * (float) height;
      float bottom =
          (float) (viewrect.y + viewrect.height) / (float) coderect.height * (float) height;
      float panelHeight = bottom - top;
      if (panelHeight < DEFLALT_VIEWHEIGHT) {
        panelHeight = DEFLALT_VIEWHEIGHT;
      }
      Rectangle rect = new Rectangle(1, (int) top, DEFLALT_WIDTH - 2, (int) panelHeight);
      this.panelCodeView.setBounds(rect);
      this.panelCodeView.setVisible(true);
    } else {
      this.panelCodeView.setVisible(false);
    }
  }

  /**
   * Source view model change notification event
   *
   * @param o Notification source
   * @param arg Notification item
   */
  @Override
  public void update(Observable o, Object arg) {
    if (!(o instanceof SourceCodeModel)) return;
    SourceCodeModel observer = (SourceCodeModel) o;

    clearProfilerData();
    List<ISourceBargraph> list = observer.getListBarData();
    if (list == null) {
      return;
    }
    float max = observer.getMaxValue();
    float min = observer.getMinValue();

    // Set profiler data
    setProfilerData(list, max, min);

    this.repaint();
    this.updateUI();
  }

  /**
   * Set profiler data
   *
   * @param list Profiler data
   * @param max Profiler data: Maximum
   * @param min Profiler data: Minimum
   */
  public void setProfilerData(List<ISourceBargraph> list, float max, float min) {
    if (this.listData == null) {
      this.listData = new ArrayList<ISourceBargraph>();
      return;
    }
    this.listData.addAll(list);
    this.maxValue = max;
    this.minValue = min;

    // Sort profiler data in ascending order
    Collections.sort(this.listData, new AscendingComparator());
  }

  /** Clear the profiler. */
  public void clearProfilerData() {
    if (this.listData == null) {
      this.listData = new ArrayList<ISourceBargraph>();
    }
    this.listData.clear();

    this.repaint();
    this.updateUI();
  }

  /**
   * Draw profiler data.
   *
   * @param g graphics object
   */
  protected void paintProfilerData(Graphics g) {
    Graphics2D g2 = (Graphics2D) g;
    // Drawing area
    Rectangle rectDraw = this.panelProfiler.getBounds();
    // clear
    g2.clearRect(rectDraw.x, rectDraw.y, rectDraw.width, rectDraw.height);
    // Data label
    this.labelName.setText("");

    // data check
    if (this.listData == null) return;
    if (this.listData.size() <= 0) return;
    if (maxValue - minValue == 0.0) return;

    // Data label
    this.labelName.setText(this.listData.get(0).getTypeName());

    // Number of lines to display
    int rows = this.panelScrollCode.getSourcePane().getEndLine();
    // Height of drawing bar
    float barheight = PROFILER_DATA_MINHEIGHT;
    if ((float) rectDraw.height / (float) rows > barheight) {
      // Set the height of the drawing bar to one line height.
      barheight = (float) rectDraw.height / (float) rows;
    }

    // Draw at line position
    // Profiler data has been sorted in ascending order, so draw from the bar with the smallest
    // value.
    Color minColor = this.properties.getRulerColorMin();
    Color maxColor = this.properties.getRulerColorMax();
    for (int i = 0; i < this.listData.size(); i++) {
      ISourceBargraph data = this.listData.get(i);
      CodeLine code = data.getCodeLine();
      int start = code.getStartLine();
      float value = data.getBarValue();

      float pos_y = (float) start / (float) rows * (float) rectDraw.height - barheight / 2;
      if (pos_y <= 0) {
        pos_y = 0;
      }
      Rectangle2D.Float rect2d = new Rectangle2D.Float(0.0F, pos_y, DEFLALT_WIDTH, barheight);
      float ratio = (value - minValue) / (maxValue - minValue);
      Color valueColor = SwingUtils.getGradientHsbColor(ratio, minColor, maxColor);
      g2.setColor(valueColor);
      g2.fill(rect2d);
    }
  }

  /**
   * Mouse click event. Display the line number of the click position in the source view.
   *
   * @param event Mouse event
   */
  @Override
  public void mouseClicked(MouseEvent event) {
    // Click, display drag line number
    setSourceLinePosition(event);
  }

  /**
   * Mouse button press event
   *
   * @param e Mouse event
   */
  @Override
  public void mousePressed(MouseEvent event) {}

  /**
   * Mouse button release event
   *
   * @param e Mouse event
   */
  @Override
  public void mouseReleased(MouseEvent e) {}

  /**
   * Event where the mouse cursor enters the area
   *
   * @param e Mouse event
   */
  @Override
  public void mouseEntered(MouseEvent event) {}

  /**
   * Event when the mouse cursor leaves the area
   *
   * @param e Mouse event
   */
  @Override
  public void mouseExited(MouseEvent e) {}

  /** Register the observer of the source code model. */
  public void addObserver() {
    if (this.panelScrollCode != null && this.panelScrollCode.getModel() != null) {
      // Set the observer.
      this.panelScrollCode.getModel().addObserver(this);
    }
  }

  /** Sort profiler data in ascending order. */
  public class AscendingComparator implements Comparator<ISourceBargraph> {

    /** Compare profiler data. */
    @Override
    public int compare(ISourceBargraph o1, ISourceBargraph o2) {
      float value1 = o1.getBarValue();
      float value2 = o2.getBarValue();
      if (value1 > value2) return 1;
      else if (value1 == value2) return 0;
      else return -1;
    }
  }

  /**
   * Set profiler properties.
   *
   * @param properties Profiler properties
   */
  public void setProfilerProperties(ProfilerProperties properties) {
    this.properties = properties;

    // Code panel
    this.panelCodeView.setBorder(new LineBorder(this.properties.getRulerPanelBorderColor(), 1));
    this.panelCodeView.setBackground(this.properties.getRulerPanelBackColor());
    this.repaint();
  }

  /**
   * Mouse drag event
   *
   * @param event Mouse event
   */
  @Override
  public void mouseDragged(MouseEvent event) {
    // Click, display drag line number
    setSourceLinePosition(event);
  }

  /**
   * Display click and drag line numbers
   *
   * @param event Mouse event
   */
  private void setSourceLinePosition(MouseEvent event) {
    try {
      if (!SwingUtilities.isLeftMouseButton(event)) return;

      // Mouse click position
      Point point = event.getPoint();
      int height = this.panelProfiler.getHeight();
      // Number of lines to display
      int docend = this.panelScrollCode.getSourcePane().getEndLine();
      // Click line number
      int lineheight = (int) ((float) point.y / (float) height * (float) docend);
      lineheight += 1;

      // Display the click line number
      this.panelScrollCode.setLinePosition(lineheight);

    } catch (Exception ex) {
    }
  }

  /**
   * Mouse movement event
   *
   * @param event Mouse event
   */
  @Override
  public void mouseMoved(MouseEvent e) {}

  /** Resize event */
  @Override
  public void componentResized(ComponentEvent e) {
    // Set the source code panel display position.
    setPanelsBounds();
  }

  /** Moving event */
  @Override
  public void componentMoved(ComponentEvent e) {}

  /** Display event */
  @Override
  public void componentShown(ComponentEvent e) {}

  /** Hidden event */
  @Override
  public void componentHidden(ComponentEvent e) {}
}
