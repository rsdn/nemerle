namespace Nemerle.VisualStudio.GUI
{
	partial class ImplementMembersForm
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
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ImplementMembersForm));
			this.pbImplement = new System.Windows.Forms.Button();
			this.pbCancel = new System.Windows.Forms.Button();
			this._grid = new System.Windows.Forms.DataGridView();
			this.SoutceName = new System.Windows.Forms.DataGridViewTextBoxColumn();
			this.Implement = new System.Windows.Forms.DataGridViewCheckBoxColumn();
			this.Explicit = new System.Windows.Forms.DataGridViewCheckBoxColumn();
			this.AccessModifier = new System.Windows.Forms.DataGridViewComboBoxColumn();
			this.ImplementationName = new System.Windows.Forms.DataGridViewTextBoxColumn();
			this.Signature = new System.Windows.Forms.DataGridViewTextBoxColumn();
			this.panel1 = new System.Windows.Forms.Panel();
			this.imageList1 = new System.Windows.Forms.ImageList(this.components);
			this._cbGenerateXmlDoc = new System.Windows.Forms.CheckBox();
			this._cbGenerateRegion = new System.Windows.Forms.CheckBox();
			((System.ComponentModel.ISupportInitialize)(this._grid)).BeginInit();
			this.panel1.SuspendLayout();
			this.SuspendLayout();
			// 
			// pbImplement
			// 
			this.pbImplement.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.pbImplement.Location = new System.Drawing.Point(443, 7);
			this.pbImplement.Name = "pbImplement";
			this.pbImplement.Size = new System.Drawing.Size(91, 23);
			this.pbImplement.TabIndex = 0;
			this.pbImplement.Text = "&Implement";
			this.pbImplement.UseVisualStyleBackColor = true;
			this.pbImplement.Click += new System.EventHandler(this.pbImplement_Click);
			// 
			// pbCancel
			// 
			this.pbCancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.pbCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.pbCancel.Location = new System.Drawing.Point(540, 7);
			this.pbCancel.Name = "pbCancel";
			this.pbCancel.Size = new System.Drawing.Size(75, 23);
			this.pbCancel.TabIndex = 1;
			this.pbCancel.Text = "&Cancel";
			this.pbCancel.UseVisualStyleBackColor = true;
			// 
			// _grid
			// 
			this._grid.AllowUserToAddRows = false;
			this._grid.AllowUserToDeleteRows = false;
			this._grid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
			this._grid.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.SoutceName,
            this.Implement,
            this.Explicit,
            this.AccessModifier,
            this.ImplementationName,
            this.Signature});
			this._grid.Dock = System.Windows.Forms.DockStyle.Fill;
			this._grid.Location = new System.Drawing.Point(0, 0);
			this._grid.Name = "_grid";
			this._grid.Size = new System.Drawing.Size(620, 362);
			this._grid.TabIndex = 6;
			// 
			// SoutceName
			// 
			this.SoutceName.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.None;
			this.SoutceName.Frozen = true;
			this.SoutceName.HeaderText = "Soutce name";
			this.SoutceName.MinimumWidth = 20;
			this.SoutceName.Name = "SoutceName";
			this.SoutceName.ReadOnly = true;
			this.SoutceName.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable;
			this.SoutceName.Width = 250;
			// 
			// Implement
			// 
			this.Implement.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.ColumnHeader;
			this.Implement.Frozen = true;
			this.Implement.HeaderText = "Implement";
			this.Implement.Name = "Implement";
			this.Implement.Width = 61;
			// 
			// Explicit
			// 
			this.Explicit.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.ColumnHeader;
			this.Explicit.HeaderText = "Explicit";
			this.Explicit.Name = "Explicit";
			this.Explicit.Width = 46;
			// 
			// AccessModifier
			// 
			this.AccessModifier.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.AllCells;
			this.AccessModifier.DisplayStyle = System.Windows.Forms.DataGridViewComboBoxDisplayStyle.Nothing;
			this.AccessModifier.HeaderText = "Access modifier";
			this.AccessModifier.Name = "AccessModifier";
			this.AccessModifier.Width = 78;
			// 
			// ImplementationName
			// 
			this.ImplementationName.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.AllCells;
			this.ImplementationName.HeaderText = "Implementation name";
			this.ImplementationName.Name = "ImplementationName";
			this.ImplementationName.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable;
			this.ImplementationName.Width = 102;
			// 
			// Signature
			// 
			this.Signature.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.AllCells;
			this.Signature.HeaderText = "Signature";
			this.Signature.Name = "Signature";
			this.Signature.ReadOnly = true;
			this.Signature.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable;
			this.Signature.Width = 58;
			// 
			// panel1
			// 
			this.panel1.Controls.Add(this._cbGenerateRegion);
			this.panel1.Controls.Add(this._cbGenerateXmlDoc);
			this.panel1.Controls.Add(this.pbCancel);
			this.panel1.Controls.Add(this.pbImplement);
			this.panel1.Dock = System.Windows.Forms.DockStyle.Bottom;
			this.panel1.Location = new System.Drawing.Point(0, 362);
			this.panel1.Name = "panel1";
			this.panel1.Size = new System.Drawing.Size(620, 38);
			this.panel1.TabIndex = 7;
			// 
			// imageList1
			// 
			this.imageList1.ColorDepth = System.Windows.Forms.ColorDepth.Depth24Bit;
			this.imageList1.ImageSize = new System.Drawing.Size(16, 16);
			this.imageList1.TransparentColor = System.Drawing.Color.Magenta;
			// 
			// _cbGenerateXmlDoc
			// 
			this._cbGenerateXmlDoc.AutoSize = true;
			this._cbGenerateXmlDoc.Location = new System.Drawing.Point(11, 10);
			this._cbGenerateXmlDoc.Name = "_cbGenerateXmlDoc";
			this._cbGenerateXmlDoc.Size = new System.Drawing.Size(161, 17);
			this._cbGenerateXmlDoc.TabIndex = 2;
			this._cbGenerateXmlDoc.Text = "Generate xml documentation";
			this._cbGenerateXmlDoc.UseVisualStyleBackColor = true;
			// 
			// _cbGenerateRegion
			// 
			this._cbGenerateRegion.AutoSize = true;
			this._cbGenerateRegion.Checked = true;
			this._cbGenerateRegion.CheckState = System.Windows.Forms.CheckState.Unchecked;
			this._cbGenerateRegion.Location = new System.Drawing.Point(178, 9);
			this._cbGenerateRegion.Name = "_cbGenerateRegion";
			this._cbGenerateRegion.Size = new System.Drawing.Size(102, 17);
			this._cbGenerateRegion.TabIndex = 3;
			this._cbGenerateRegion.Text = "Generate region";
			this._cbGenerateRegion.UseVisualStyleBackColor = true;
			// 
			// ImplementMembersForm
			// 
			this.AcceptButton = this.pbImplement;
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.CancelButton = this.pbCancel;
			this.ClientSize = global::Nemerle.VisualStudio.Properties.Settings.Default.ImplementMembersSize;
			this.Controls.Add(this._grid);
			this.Controls.Add(this.panel1);
			this.DataBindings.Add(new System.Windows.Forms.Binding("ClientSize", global::Nemerle.VisualStudio.Properties.Settings.Default, "ImplementMembersSize", true, System.Windows.Forms.DataSourceUpdateMode.OnPropertyChanged));
			this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
			this.MinimizeBox = false;
			this.Name = "ImplementMembersForm";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Implement unimplemented members of implemented interfaces";
			this.Load += new System.EventHandler(this.ImplementMembersForm_Load);
			((System.ComponentModel.ISupportInitialize)(this._grid)).EndInit();
			this.panel1.ResumeLayout(false);
			this.panel1.PerformLayout();
			this.ResumeLayout(false);

		}

		#endregion

		private System.Windows.Forms.Button pbImplement;
		private System.Windows.Forms.Button pbCancel;
		private System.Windows.Forms.DataGridView _grid;
		private System.Windows.Forms.Panel panel1;
		private System.Windows.Forms.ImageList imageList1;
		private System.Windows.Forms.DataGridViewTextBoxColumn SoutceName;
		private System.Windows.Forms.DataGridViewCheckBoxColumn Implement;
		private System.Windows.Forms.DataGridViewCheckBoxColumn Explicit;
		private System.Windows.Forms.DataGridViewComboBoxColumn AccessModifier;
		private System.Windows.Forms.DataGridViewTextBoxColumn ImplementationName;
		private System.Windows.Forms.DataGridViewTextBoxColumn Signature;
		private System.Windows.Forms.CheckBox _cbGenerateRegion;
		private System.Windows.Forms.CheckBox _cbGenerateXmlDoc;

	}
}