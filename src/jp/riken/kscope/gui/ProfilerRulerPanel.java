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
 * プロファイラルーラパネルクラス
 * @author riken
 */
public class ProfilerRulerPanel extends JPanel implements ChangeListener, MouseListener, Observer, MouseMotionListener, ComponentListener {

    /** シリアル番号 */
	private static final long serialVersionUID = 1L;

	/** 表示コスト情報名 */
	private JLabel labelName;
	/** コスト表示パネル */
	private JPanel panelProfiler;
	/** コーナーパネル */
	private JPanel panelConer;
	/** ソースコードパネル */
    private ScrollCodePane panelScrollCode;
	/** ソースコード表示パネル */
    private JPanel panelCodeView;
    /** プロファイラデータ */
    private List<ISourceBargraph> listData;
    /** プロファイラプロパティ */
    private ProfilerProperties properties;
	/** デフォルト幅 */
	private int DEFLALT_WIDTH = 60;
	/** ソースビューパネルデフォルト高さ */
	private int DEFLALT_VIEWHEIGHT = 1;
    /** プロファイラデータ表示高さ */
	private float PROFILER_DATA_MINHEIGHT = 3.0F;
	/** ソースコード表示パネルボーダー色 = Color.GRAY*/
	private final Color DEFAULT_BORDERCOLORPANEL = Color.GRAY;
	/** ソースコード表示パネル背景色  new Color(220, 220, 220, 64) */
	private final Color DEFAULT_BACKCOLORPANEL = new Color(220, 220, 220, 64);
	/** プロファイラデータ:最大値 */
	private float maxValue;
	/** プロファイラデータ:最小値 */
	private float minValue;

	/**
	 * コンストラクタ
	 */
	public ProfilerRulerPanel() {
        super();
        initGUI();
    }

	/**
	 * コンストラクタ
	 * @param panelCode     ソースコードパネル
	 */
	public ProfilerRulerPanel(ScrollCodePane panelCode) {
        super();
        this.panelScrollCode = panelCode;
        initGUI();
    }

    /**
     * 初期化を行う.<br/>
     * プロファイラルーラパネルを配置する。
     */
    private void initGUI() {
        try {
            BorderLayout thisLayout = new BorderLayout();
            this.setLayout(thisLayout);

            // プロファイラ表示コスト名
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
            // プロファイラプレビューパネル
            {
            	this.panelProfiler = new JPanel() {
            		/** シリアル番号 */
					private static final long serialVersionUID = 1L;

					/**
            		 * プロファイラデータを描画する.
            		 */
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
            // コーナーパネル
            {
            	this.panelConer = new JPanel();
                this.panelConer.setOpaque(true);
                this.add(this.panelConer, BorderLayout.SOUTH);
            }
            // ソースコード表示パネル
            {
            	this.panelCodeView = new JPanel();
                this.panelCodeView.setBorder(new LineBorder(DEFAULT_BORDERCOLORPANEL, 1));
                //this.panelCodeView.setBackground(DEFAULT_BACKCOLORPANEL);
                this.panelCodeView.setBackground(Color.RED);
                this.panelProfiler.add(this.panelCodeView);
            }

            // ソースコードパネルのスクロールイベント
            if (this.panelScrollCode != null) {
            	this.panelScrollCode.getViewport().addChangeListener(this);
                // オブザーバを設定する。
            	if (this.panelScrollCode.getModel() != null) {
            		this.panelScrollCode.getModel().addObserver(this);
            	}
            }
            // マウスクリックイベント
            this.panelProfiler.addMouseListener(this);
            this.panelProfiler.addMouseMotionListener(this);
            // パネルのサイズ変更イベント
            this.addComponentListener(this);

    		// ソースコードパネル表示位置を設定する。
    		setPanelsBounds();

	    } catch (Exception e) {
	        e.printStackTrace();
	    }
    }

	/**
	 * ソースコードパネルのスクロール変更イベント
	 */
	@Override
	public void stateChanged(ChangeEvent e) {
		// ソースコードパネル表示位置を設定する。
		setPanelsBounds();
	}

	/**
	 * ソースコードパネル表示位置を設定する。
	 */
	private void setPanelsBounds() {
        if (this.panelScrollCode == null) return;

        // コーナーパネル
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

        // 再描画
        this.revalidate();
        this.repaint();

        // ソースコードパネル
        if (barVertical != null && barVertical.isShowing()) {
	        // 表示位置
	        Rectangle viewrect = this.panelScrollCode.getViewport().getViewRect();
	        Rectangle coderect = this.panelScrollCode.getSourcePane().getBounds();
	        int height = this.panelProfiler.getHeight();
	        float top = (float)viewrect.y/(float)coderect.height * (float)height;
	        float bottom = (float)(viewrect.y+viewrect.height)/(float)coderect.height * (float)height;
	        float panelHeight = bottom-top;
	        if (panelHeight < DEFLALT_VIEWHEIGHT) {
	        	panelHeight = DEFLALT_VIEWHEIGHT;
	        }
	        Rectangle rect = new Rectangle(1, (int)top, DEFLALT_WIDTH-2, (int)panelHeight);
	        this.panelCodeView.setBounds(rect);
	        this.panelCodeView.setVisible(true);
        }
        else {
        	this.panelCodeView.setVisible(false);
        }
	}

    /**
     * ソースビューモデルの変更通知イベント
     * @param o			通知元
     * @param arg		通知項目
     */
	@Override
	public void update(Observable o, Object arg) {
		if (!(o instanceof SourceCodeModel)) return;
		SourceCodeModel observer = (SourceCodeModel)o;

        clearProfilerData();
        List<ISourceBargraph> list = observer.getListBarData();
        if (list == null) {
            return;
        }
        float max = observer.getMaxValue();
        float min = observer.getMinValue();

        // プロファイラデータをセットする
        setProfilerData(list, max, min);

        this.repaint();
        this.updateUI();
	}

    /**
     * プロファイラデータを設定する
     * @param list			プロファイラデータ
     * @param max			プロファイラデータ:最大値
     * @param min			プロファイラデータ:最小値
     */
    public void setProfilerData(List<ISourceBargraph> list, float max, float min) {
        if (this.listData == null) {
            this.listData = new ArrayList<ISourceBargraph>();
            return;
        }
        this.listData.addAll(list);
        this.maxValue = max;
        this.minValue = min;

        // プロファイラデータを昇順に並べ替えを行う
        Collections.sort(this.listData, new AscendingComparator());
    }

    /**
     * プロファイラをクリアする。
     */
    public void clearProfilerData() {
        if (this.listData == null) {
            this.listData = new ArrayList<ISourceBargraph>();
        }
        this.listData.clear();

        this.repaint();
        this.updateUI();
    }

	/**
	 * プロファイラデータを描画する.
	 * @param g    グラフィックスオブジェクト
	 */
	protected void paintProfilerData(Graphics g) {
		Graphics2D g2 = (Graphics2D)g;
		// 描画領域
		Rectangle rectDraw = this.panelProfiler.getBounds();
		// クリア
		g2.clearRect(rectDraw.x, rectDraw.y, rectDraw.width, rectDraw.height);
		// データラベル
		this.labelName.setText("");

		// データチェック
		if (this.listData == null) return;
		if (this.listData.size() <= 0) return;
		if (maxValue - minValue == 0.0) return;

		// データラベル
		this.labelName.setText(this.listData.get(0).getTypeName());

        // 表示行数
        int rows = this.panelScrollCode.getSourcePane().getEndLine();
        // 描画バーの高さ
		float barheight = PROFILER_DATA_MINHEIGHT;
		if ((float)rectDraw.height/(float)rows > barheight ) {
			// 描画バーの高さを1行高さにする.
			barheight = (float)rectDraw.height/(float)rows;
		}

        // 行位置に描画
        // プロファイラデータは昇順に並べ替え済みであるので、値の小さいバーから描画する。
        Color minColor = this.properties.getRulerColorMin();
        Color maxColor = this.properties.getRulerColorMax();
		for (int i=0; i<this.listData.size(); i++) {
			ISourceBargraph data = this.listData.get(i);
            CodeLine code = data.getCodeLine();
            int start = code.getStartLine();
			float value = data.getBarValue();

			float pos_y = (float)start/(float)rows * (float)rectDraw.height - barheight/2;
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
	 * マウスクリックイベント.
	 * クリック位置の行番号をソースビューに表示する。
	 * @param event マウスイベント
	 */
	@Override
	public void mouseClicked(MouseEvent event) {
		// クリック, ドラッグ行番号を表示する
		setSourceLinePosition(event);
	}

	/**
	 * マウスボタンのプレスイベント
	 * @param e マウスイベント
	 */
	@Override
	public void mousePressed(MouseEvent event) { }

	/**
	 * マウスボタンのリリースイベント
	 * @param e マウスイベント
	 */
	@Override
	public void mouseReleased(MouseEvent e) { }

	/**
	 * マウスカーソルが領域に入ったイベント
	 * @param e マウスイベント
	 */
	@Override
	public void mouseEntered(MouseEvent event) { }

	/**
	 * マウスカーソルが領域から出たイベント
	 * @param e マウスイベント
	 */
	@Override
	public void mouseExited(MouseEvent e) { }

	/**
	 * ソースコードモデルのオブザーバを登録する。
	 */
	public void addObserver() {
        if (this.panelScrollCode != null && this.panelScrollCode.getModel() != null) {
            // オブザーバを設定する。
       		this.panelScrollCode.getModel().addObserver(this);
        }
	}

	/**
	 * プロファイラデータを昇順に並べ替えを行う。
	 */
	public class AscendingComparator implements Comparator<ISourceBargraph> {

		/**
		 * プロファイラデータの比較を行う.
		 */
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
     * プロファイラプロパティを設定する.
     * @param properties		プロファイラプロパティ
     */
	public void setProfilerProperties(ProfilerProperties properties) {
		this.properties = properties;

		// コードパネル
        this.panelCodeView.setBorder(new LineBorder(this.properties.getRulerPanelBorderColor(), 1));
        this.panelCodeView.setBackground(this.properties.getRulerPanelBackColor());
		this.repaint();
	}

	/**
	 * マウスドラッグイベント
	 * @param event マウスイベント
	 */
	@Override
	public void mouseDragged(MouseEvent event) {
		// クリック, ドラッグ行番号を表示する
		setSourceLinePosition(event);
	}

	/**
	 * クリック, ドラッグ行番号を表示する
	 * @param event マウスイベント
	 */
	private void setSourceLinePosition(MouseEvent event) {
		try {
			if (!SwingUtilities.isLeftMouseButton(event)) return;

			// マウスクリック位置
			Point point = event.getPoint();
			int height = this.panelProfiler.getHeight();
			// 表示行数
			int docend = this.panelScrollCode.getSourcePane().getEndLine();
			// クリック行番号
			int lineheight = (int)((float)point.y / (float)height * (float)docend);
			lineheight += 1;

			// クリック行番号を表示する
			this.panelScrollCode.setLinePosition(lineheight);

		} catch (Exception ex) {
		}
	}


	/**
	 * マウス移動イベント
	 * @param event マウスイベント
	 */
	@Override
	public void mouseMoved(MouseEvent e) { }

	/**
	 * サイズの変更イベント
	 */
	@Override
	public void componentResized(ComponentEvent e) {
		// ソースコードパネル表示位置を設定する。
		setPanelsBounds();
	}

	/**
	 * 移動イベント
	 */
	@Override
	public void componentMoved(ComponentEvent e) { }

	/**
	 * 表示イベント
	 */
	@Override
	public void componentShown(ComponentEvent e) { }

	/**
	 * 非表示イベント
	 */
	@Override
	public void componentHidden(ComponentEvent e) { }

}
