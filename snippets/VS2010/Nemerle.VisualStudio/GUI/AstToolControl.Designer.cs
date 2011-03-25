namespace Nemerle.VisualStudio.GUI
{
	partial class AstToolControl
	{
		/// <summary> 
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.IContainer components = null;

		/// <summary> 
		/// Clean up any resources being used.
		/// </summary>
		/// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
		protected override void Dispose(bool disposing)
		{
			if (disposing && (components != null))
			{
				components.Dispose();
			}
			base.Dispose(disposing);
		}

		#region Component Designer generated code

		/// <summary> 
		/// Required method for Designer support - do not modify 
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.components = new System.ComponentModel.Container();
			this.panel1 = new System.Windows.Forms.Panel();
			this._buildTypedtreeCountLabel = new System.Windows.Forms.Label();
			this._displayType = new System.Windows.Forms.ComboBox();
			this._checkCountLabel = new System.Windows.Forms.Label();
			this._col = new System.Windows.Forms.Label();
			this._line = new System.Windows.Forms.Label();
			this._autoUpdateCheckBox = new System.Windows.Forms.CheckBox();
			this._grid = new System.Windows.Forms.DataGridView();
			this._astColumn = new System.Windows.Forms.DataGridViewTextBoxColumn();
			this._toolTip = new System.Windows.Forms.ToolTip(this.components);
			this.panel1.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this._grid)).BeginInit();
			this.SuspendLayout();
			// 
			// panel1
			// 
			this.panel1.Controls.Add(this._buildTypedtreeCountLabel);
			this.panel1.Controls.Add(this._displayType);
			this.panel1.Controls.Add(this._checkCountLabel);
			this.panel1.Controls.Add(this._col);
			this.panel1.Controls.Add(this._line);
			this.panel1.Controls.Add(this._autoUpdateCheckBox);
			this.panel1.Dock = System.Windows.Forms.DockStyle.Top;
			this.panel1.Location = new System.Drawing.Point(0, 0);
			this.panel1.Name = "panel1";
			this.panel1.Size = new System.Drawing.Size(245, 46);
			this.panel1.TabIndex = 0;
			// 
			// _buildTypedtreeCountLabel
			// 
			this._buildTypedtreeCountLabel.AutoSize = true;
			this._buildTypedtreeCountLabel.Location = new System.Drawing.Point(212, 4);
			this._buildTypedtreeCountLabel.Name = "_buildTypedtreeCountLabel";
			this._buildTypedtreeCountLabel.Size = new System.Drawing.Size(13, 13);
			this._buildTypedtreeCountLabel.TabIndex = 3;
			this._buildTypedtreeCountLabel.Text = "0";
			this._toolTip.SetToolTip(this._buildTypedtreeCountLabel, "Build typed tree count");
			// 
			// _displayType
			// 
			this._displayType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this._displayType.Items.AddRange(new object[] {
            "Tokens",
            "AST"});
			this._displayType.Location = new System.Drawing.Point(3, 20);
			this._displayType.Margin = new System.Windows.Forms.Padding(2);
			this._displayType.Name = "_displayType";
			this._displayType.Size = new System.Drawing.Size(158, 21);
			this._displayType.TabIndex = 2;
			// 
			// _checkCountLabel
			// 
			this._checkCountLabel.AutoSize = true;
			this._checkCountLabel.Location = new System.Drawing.Point(172, 4);
			this._checkCountLabel.Name = "_checkCountLabel";
			this._checkCountLabel.Size = new System.Drawing.Size(13, 13);
			this._checkCountLabel.TabIndex = 1;
			this._checkCountLabel.Text = "0";
			this._toolTip.SetToolTip(this._checkCountLabel, "Check count");
			// 
			// _col
			// 
			this._col.AutoSize = true;
			this._col.Location = new System.Drawing.Point(132, 4);
			this._col.Name = "_col";
			this._col.Size = new System.Drawing.Size(13, 13);
			this._col.TabIndex = 1;
			this._col.Text = "0";
			this._toolTip.SetToolTip(this._col, "Current char offset");
			// 
			// _line
			// 
			this._line.AutoSize = true;
			this._line.Location = new System.Drawing.Point(93, 4);
			this._line.Name = "_line";
			this._line.Size = new System.Drawing.Size(13, 13);
			this._line.TabIndex = 1;
			this._line.Text = "0";
			this._toolTip.SetToolTip(this._line, "Current Line");
			// 
			// _autoUpdateCheckBox
			// 
			this._autoUpdateCheckBox.AutoSize = true;
			this._autoUpdateCheckBox.Location = new System.Drawing.Point(3, 3);
			this._autoUpdateCheckBox.Name = "_autoUpdateCheckBox";
			this._autoUpdateCheckBox.Size = new System.Drawing.Size(84, 17);
			this._autoUpdateCheckBox.TabIndex = 0;
			this._autoUpdateCheckBox.Text = "Auto update";
			this._autoUpdateCheckBox.UseVisualStyleBackColor = true;
			this._autoUpdateCheckBox.CheckedChanged += new System.EventHandler(this._autoUpdateCheckBox_CheckedChanged);
			// 
			// _grid
			// 
			this._grid.AllowUserToAddRows = false;
			this._grid.AllowUserToDeleteRows = false;
			this._grid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
			this._grid.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this._astColumn});
			this._grid.Dock = System.Windows.Forms.DockStyle.Fill;
			this._grid.Location = new System.Drawing.Point(0, 46);
			this._grid.MultiSelect = false;
			this._grid.Name = "_grid";
			this._grid.ReadOnly = true;
			this._grid.Size = new System.Drawing.Size(245, 269);
			this._grid.TabIndex = 1;
			this._grid.VirtualMode = true;
			this._grid.CellClick += new System.Windows.Forms.DataGridViewCellEventHandler(this._grid_CellClick);
			this._grid.CellValueNeeded += new System.Windows.Forms.DataGridViewCellValueEventHandler(this._grid_CellValueNeeded);
			this._grid.CellFormatting += new System.Windows.Forms.DataGridViewCellFormattingEventHandler(this._grid_CellFormatting);
			this._grid.CellToolTipTextNeeded += new System.Windows.Forms.DataGridViewCellToolTipTextNeededEventHandler(this._grid_CellToolTipTextNeeded);
			// 
			// _astColumn
			// 
			this._astColumn.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
			this._astColumn.HeaderText = "AST";
			this._astColumn.Name = "_astColumn";
			this._astColumn.ReadOnly = true;
			// 
			// AstToolControl
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.Controls.Add(this._grid);
			this.Controls.Add(this.panel1);
			this.Name = "AstToolControl";
			this.Size = new System.Drawing.Size(245, 315);
			this.panel1.ResumeLayout(false);
			this.panel1.PerformLayout();
			((System.ComponentModel.ISupportInitialize)(this._grid)).EndInit();
			this.ResumeLayout(false);

		}

		#endregion

		private System.Windows.Forms.Panel panel1;
		private System.Windows.Forms.DataGridView _grid;
		private System.Windows.Forms.CheckBox _autoUpdateCheckBox;
		private System.Windows.Forms.DataGridViewTextBoxColumn _astColumn;
		private System.Windows.Forms.Label _col;
		private System.Windows.Forms.Label _line;
		private System.Windows.Forms.ToolTip _toolTip;
		private System.Windows.Forms.Label _checkCountLabel;
		private System.Windows.Forms.ComboBox _displayType;
		private System.Windows.Forms.Label _buildTypedtreeCountLabel;
	}
}
