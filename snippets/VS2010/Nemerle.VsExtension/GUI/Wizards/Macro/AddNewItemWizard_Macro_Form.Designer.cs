namespace Nemerle.VisualStudio.GUI.Wizards
{
	partial class AddNewItemWizard_Macro_Form
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

		#region Windows Form Designer generated code

		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.components = new System.ComponentModel.Container();
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(AddNewItemWizard_Macro_Form));
			this._macroPhaseLabel = new System.Windows.Forms.Label();
			this._macroPhaseBomboBox = new System.Windows.Forms.ComboBox();
			this._macroTypeComboBox = new System.Windows.Forms.ComboBox();
			this.label1 = new System.Windows.Forms.Label();
			this._defineSyntaxCheckBox = new System.Windows.Forms.CheckBox();
			this._macroAttributeSettingsGroupBox = new System.Windows.Forms.GroupBox();
			this._validOnComboBox = new System.Windows.Forms.ComboBox();
			this.label2 = new System.Windows.Forms.Label();
			this._okButton = new System.Windows.Forms.Button();
			this._cancelButton = new System.Windows.Forms.Button();
			this._grid = new System.Windows.Forms.DataGridView();
			this.NameColl = new System.Windows.Forms.DataGridViewTextBoxColumn();
			this.TypeColl = new System.Windows.Forms.DataGridViewComboBoxColumn();
			this.DefaultValueColl = new System.Windows.Forms.DataGridViewTextBoxColumn();
			this._errorProvider = new System.Windows.Forms.ErrorProvider(this.components);
			this._toolTip = new System.Windows.Forms.ToolTip(this.components);
			this.label3 = new System.Windows.Forms.Label();
			this._macroAttributeSettingsGroupBox.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this._grid)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this._errorProvider)).BeginInit();
			this.SuspendLayout();
			// 
			// _macroPhaseLabel
			// 
			this._macroPhaseLabel.AutoSize = true;
			this._macroPhaseLabel.Location = new System.Drawing.Point(16, 20);
			this._macroPhaseLabel.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this._macroPhaseLabel.Name = "_macroPhaseLabel";
			this._macroPhaseLabel.Size = new System.Drawing.Size(70, 13);
			this._macroPhaseLabel.TabIndex = 5;
			this._macroPhaseLabel.Text = "Macro &Phase";
			// 
			// _macroPhaseBomboBox
			// 
			this._macroPhaseBomboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this._macroPhaseBomboBox.FormattingEnabled = true;
			this._macroPhaseBomboBox.Location = new System.Drawing.Point(88, 17);
			this._macroPhaseBomboBox.Margin = new System.Windows.Forms.Padding(2);
			this._macroPhaseBomboBox.Name = "_macroPhaseBomboBox";
			this._macroPhaseBomboBox.Size = new System.Drawing.Size(142, 21);
			this._macroPhaseBomboBox.TabIndex = 6;
			this._toolTip.SetToolTip(this._macroPhaseBomboBox, resources.GetString("_macroPhaseBomboBox.ToolTip"));
			this._macroPhaseBomboBox.SelectedIndexChanged += new System.EventHandler(this._macroPhaseBomboBox_SelectedIndexChanged);
			// 
			// _macroTypeComboBox
			// 
			this._macroTypeComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this._macroTypeComboBox.FormattingEnabled = true;
			this._macroTypeComboBox.Location = new System.Drawing.Point(76, 5);
			this._macroTypeComboBox.Margin = new System.Windows.Forms.Padding(2);
			this._macroTypeComboBox.Name = "_macroTypeComboBox";
			this._macroTypeComboBox.Size = new System.Drawing.Size(166, 21);
			this._macroTypeComboBox.TabIndex = 2;
			this._toolTip.SetToolTip(this._macroTypeComboBox, resources.GetString("_macroTypeComboBox.ToolTip"));
			this._macroTypeComboBox.SelectedIndexChanged += new System.EventHandler(this._macroTypeComboBox_SelectedIndexChanged);
			// 
			// label1
			// 
			this.label1.AutoSize = true;
			this.label1.Location = new System.Drawing.Point(9, 7);
			this.label1.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(64, 13);
			this.label1.TabIndex = 1;
			this.label1.Text = "Macro &Type";
			// 
			// _defineSyntaxCheckBox
			// 
			this._defineSyntaxCheckBox.AutoSize = true;
			this._defineSyntaxCheckBox.Location = new System.Drawing.Point(76, 29);
			this._defineSyntaxCheckBox.Margin = new System.Windows.Forms.Padding(2);
			this._defineSyntaxCheckBox.Name = "_defineSyntaxCheckBox";
			this._defineSyntaxCheckBox.Size = new System.Drawing.Size(141, 17);
			this._defineSyntaxCheckBox.TabIndex = 3;
			this._defineSyntaxCheckBox.Text = "Define &Syntax Extension";
			this._defineSyntaxCheckBox.UseVisualStyleBackColor = true;
			this._defineSyntaxCheckBox.CheckedChanged += new System.EventHandler(this._defineSyntaxCheckBox_CheckedChanged);
			// 
			// _macroAttributeSettingsGroupBox
			// 
			this._macroAttributeSettingsGroupBox.Controls.Add(this._validOnComboBox);
			this._macroAttributeSettingsGroupBox.Controls.Add(this.label2);
			this._macroAttributeSettingsGroupBox.Controls.Add(this._macroPhaseBomboBox);
			this._macroAttributeSettingsGroupBox.Controls.Add(this._macroPhaseLabel);
			this._macroAttributeSettingsGroupBox.Location = new System.Drawing.Point(12, 50);
			this._macroAttributeSettingsGroupBox.Margin = new System.Windows.Forms.Padding(2);
			this._macroAttributeSettingsGroupBox.Name = "_macroAttributeSettingsGroupBox";
			this._macroAttributeSettingsGroupBox.Padding = new System.Windows.Forms.Padding(2);
			this._macroAttributeSettingsGroupBox.Size = new System.Drawing.Size(238, 69);
			this._macroAttributeSettingsGroupBox.TabIndex = 4;
			this._macroAttributeSettingsGroupBox.TabStop = false;
			this._macroAttributeSettingsGroupBox.Text = "Macro &Attribute settings";
			// 
			// _validOnComboBox
			// 
			this._validOnComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this._validOnComboBox.FormattingEnabled = true;
			this._validOnComboBox.Items.AddRange(new object[] {
            "Assembly",
            "Type",
            "Method",
            "Property",
            "Field",
            "Event",
            "Parameter"});
			this._validOnComboBox.Location = new System.Drawing.Point(88, 41);
			this._validOnComboBox.Margin = new System.Windows.Forms.Padding(2);
			this._validOnComboBox.Name = "_validOnComboBox";
			this._validOnComboBox.Size = new System.Drawing.Size(142, 21);
			this._validOnComboBox.TabIndex = 8;
			this._validOnComboBox.SelectedIndexChanged += new System.EventHandler(this._validOnComboBox_SelectedIndexChanged);
			// 
			// label2
			// 
			this.label2.AutoSize = true;
			this.label2.Location = new System.Drawing.Point(38, 44);
			this.label2.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.label2.Name = "label2";
			this.label2.Size = new System.Drawing.Size(47, 13);
			this.label2.TabIndex = 7;
			this.label2.Text = "&Valid On";
			// 
			// _okButton
			// 
			this._okButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this._okButton.Location = new System.Drawing.Point(329, 306);
			this._okButton.Margin = new System.Windows.Forms.Padding(2);
			this._okButton.Name = "_okButton";
			this._okButton.Size = new System.Drawing.Size(85, 27);
			this._okButton.TabIndex = 11;
			this._okButton.Text = "&Finish";
			this._okButton.UseVisualStyleBackColor = true;
			this._okButton.Click += new System.EventHandler(this._okButton_Click);
			// 
			// _cancelButton
			// 
			this._cancelButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this._cancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this._cancelButton.Location = new System.Drawing.Point(418, 307);
			this._cancelButton.Margin = new System.Windows.Forms.Padding(2);
			this._cancelButton.Name = "_cancelButton";
			this._cancelButton.Size = new System.Drawing.Size(85, 26);
			this._cancelButton.TabIndex = 12;
			this._cancelButton.Text = "&Cancel";
			this._cancelButton.UseVisualStyleBackColor = true;
			// 
			// _grid
			// 
			this._grid.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
			this._grid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
			this._grid.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.NameColl,
            this.TypeColl,
            this.DefaultValueColl});
			this._grid.Location = new System.Drawing.Point(12, 139);
			this._grid.Name = "_grid";
			this._grid.Size = new System.Drawing.Size(490, 163);
			this._grid.TabIndex = 10;
			this._grid.CellValidating += new System.Windows.Forms.DataGridViewCellValidatingEventHandler(this._grid_CellValidating);
			// 
			// NameColl
			// 
			this.NameColl.HeaderText = "Name";
			this.NameColl.Name = "NameColl";
			this.NameColl.Width = 200;
			// 
			// TypeColl
			// 
			this.TypeColl.HeaderText = "Type";
			this.TypeColl.Items.AddRange(new object[] {
            "PExpr",
            "Token",
            "params list[PExpr]",
            "params array[PExpr]",
            "bool",
            "byte",
            "decimal",
            "double",
            "float",
            "int",
            "long",
            "sbyte",
            "short",
            "string",
            "uint",
            "ulong",
            "ushort"});
			this.TypeColl.Name = "TypeColl";
			this.TypeColl.Width = 120;
			// 
			// DefaultValueColl
			// 
			this.DefaultValueColl.HeaderText = "Default Value";
			this.DefaultValueColl.Name = "DefaultValueColl";
			// 
			// _errorProvider
			// 
			this._errorProvider.ContainerControl = this;
			// 
			// _toolTip
			// 
			this._toolTip.AutomaticDelay = 0;
			this._toolTip.AutoPopDelay = 32767;
			this._toolTip.InitialDelay = 700;
			this._toolTip.IsBalloon = true;
			this._toolTip.ReshowDelay = 200;
			this._toolTip.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info;
			this._toolTip.ToolTipTitle = "Help";
			// 
			// label3
			// 
			this.label3.AutoSize = true;
			this.label3.Location = new System.Drawing.Point(11, 123);
			this.label3.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.label3.Name = "label3";
			this.label3.Size = new System.Drawing.Size(60, 13);
			this.label3.TabIndex = 9;
			this.label3.Text = "Pa&rameters";
			// 
			// AddNewItemWizard_Macro_Form
			// 
			this.AcceptButton = this._okButton;
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.CancelButton = this._cancelButton;
			this.ClientSize = new System.Drawing.Size(514, 338);
			this.Controls.Add(this.label3);
			this.Controls.Add(this._grid);
			this.Controls.Add(this._cancelButton);
			this.Controls.Add(this._okButton);
			this.Controls.Add(this._macroAttributeSettingsGroupBox);
			this.Controls.Add(this._defineSyntaxCheckBox);
			this.Controls.Add(this.label1);
			this.Controls.Add(this._macroTypeComboBox);
			this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
			this.Margin = new System.Windows.Forms.Padding(2);
			this.MinimizeBox = false;
			this.Name = "AddNewItemWizard_Macro_Form";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Macro Wizard";
			this._macroAttributeSettingsGroupBox.ResumeLayout(false);
			this._macroAttributeSettingsGroupBox.PerformLayout();
			((System.ComponentModel.ISupportInitialize)(this._grid)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this._errorProvider)).EndInit();
			this.ResumeLayout(false);
			this.PerformLayout();

		}

		#endregion

		private System.Windows.Forms.Label _macroPhaseLabel;
		private System.Windows.Forms.ComboBox _macroPhaseBomboBox;
		private System.Windows.Forms.ComboBox _macroTypeComboBox;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.CheckBox _defineSyntaxCheckBox;
		private System.Windows.Forms.GroupBox _macroAttributeSettingsGroupBox;
		private System.Windows.Forms.ComboBox _validOnComboBox;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.Button _okButton;
		private System.Windows.Forms.Button _cancelButton;
		private System.Windows.Forms.DataGridView _grid;
		private System.Windows.Forms.ErrorProvider _errorProvider;
		private System.Windows.Forms.DataGridViewTextBoxColumn NameColl;
		private System.Windows.Forms.DataGridViewComboBoxColumn TypeColl;
		private System.Windows.Forms.DataGridViewTextBoxColumn DefaultValueColl;
		private System.Windows.Forms.ToolTip _toolTip;
		private System.Windows.Forms.Label label3;
	}
}