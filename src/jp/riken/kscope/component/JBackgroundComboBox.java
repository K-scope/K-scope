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
package jp.riken.kscope.component;

import java.awt.Color;
import java.awt.Component;

import javax.swing.*;
import javax.swing.plaf.metal.MetalComboBoxButton;

/**
 * 背景色付きコンボボックス.
 * コンボボックスのテキストエリアは編集（入力）は不可.
 * @author riken
 */
public class JBackgroundComboBox extends JComboBox {

	/** シリアル番号 */
	private static final long serialVersionUID = 1L;

	/** コンボボックスデータ */
	class ColorComboData {
		/** コンボボックステキスト */
		String text;
		/** コンボボックス背景色 */
		Color color;
		/**
		 * コンストラクタ
		 * @param text		テキスト
		 * @param color		背景色
		 */
		public ColorComboData(String text, Color color) {
			this.text = text;
			this.color = color;
		}

		/**
		 * コンボボックスデータの文字列表現.
		 * テキストを返す.
		 */
		@Override
		public String toString() {
			return this.text;
		}

	}

	/**
	 * コンストラクタ
	 */
	public JBackgroundComboBox() {
		super();
		setRenderer(new ColorRenderer());
		this.setOpaque(true);
		this.setEditable(true);
	}

	@Override
	public void setSelectedItem(Object object) {
		// super.setSelectedItem(object);
		// setBackground((Color)colors.get(anObject));
		ColorComboData data = null;
		Color color = null;
		if (object == null) {
			super.setSelectedIndex(0);
		}
		else if (object instanceof ColorComboData) {
			super.setSelectedItem(object);
			data = (ColorComboData)object;
			color = data.color;
		}
		else {
			int count = this.getItemCount();
			for (int i=0; i<count; i++) {
				data = (ColorComboData)this.getItemAt(i);
				if (data.text.equalsIgnoreCase(object.toString())) {
					super.setSelectedIndex(i);
					color = data.color;
					break;
				}
				data = null;
			}
		}
        JTextField text = ((JTextField) getEditor().getEditorComponent());
		if (color != null) {
	        text.setBackground(color);
		}
		else {
			text.setBackground(Color.white);
		}
        //text.setEditable(false);
        text.setEnabled(false);
        text.setDisabledTextColor(Color.black);

        Component[] comp = this.getComponents();
        for (int i = 0; i < comp.length; i++) {
            if (comp[i] instanceof MetalComboBoxButton) {
                MetalComboBoxButton coloredArrowsButton = (MetalComboBoxButton) comp[i];
                coloredArrowsButton.setBackground(null);
                break;
            }
        }
	}

	/**
	 * コンボボックスリストセル描画クラス
	 * @author riken
	 */
	class ColorRenderer extends JLabel implements javax.swing.ListCellRenderer {
		/** シリアル番号 */
		private static final long serialVersionUID = 1L;

		/**
		 * コンストラクタ
		 */
		public ColorRenderer() {
			this.setOpaque(true);
		}

		@Override
		public Component getListCellRendererComponent(JList list, Object value, int index,
				boolean isSelected, boolean cellHasFocus) {
			// add class check for mac osx by @hira at 2013/05/30
			if (!(value instanceof ColorComboData)) {
				return this;
			}
			ColorComboData data = (ColorComboData)value;
			list.setSelectionBackground(null);
			list.setSelectionForeground(null);

			if(isSelected){
				setBorder(BorderFactory.createEtchedBorder());
			} else {
				setBorder(null);
			}
			if (data.color != null) {
				setBackground(data.color);
			}
			else {
				setBackground(Color.white);
			}
			setText(data.text);
			return this;
		}
	}


	@Override
	public Object getSelectedItem() {
		if (super.getSelectedItem() == null) return null;
		String text = null;
		if (super.getSelectedItem() instanceof ColorComboData) {
			ColorComboData data = (ColorComboData)super.getSelectedItem();
			if (data == null) return null;
			text = data.text;
		}
		else {
			text = super.getSelectedItem().toString();
		}
		return text;
	}

	/**
	 * コンボボックスデータを追加する.
	 * @param text		テキスト
	 * @param color		背景色
	 */
	public void addItem(String text, Color color) {
		ColorComboData data = new ColorComboData(text, color);
		addItem(data);
	}


	@Override
	public void addItem(Object anObject) {
		if (anObject instanceof ColorComboData) {
			super.addItem(anObject);
		}
	}

}

