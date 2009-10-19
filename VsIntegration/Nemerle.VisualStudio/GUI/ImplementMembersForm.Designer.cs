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
			this.pbImplement = new System.Windows.Forms.Button();
			this.pbCancel = new System.Windows.Forms.Button();
			this._grid = new System.Windows.Forms.DataGridView();
			this.ItfMemberNameCol = new System.Windows.Forms.DataGridViewTextBoxColumn();
			this.AddImplCol = new System.Windows.Forms.DataGridViewCheckBoxColumn();
			this.Explicit = new System.Windows.Forms.DataGridViewCheckBoxColumn();
			this.AccessMods = new System.Windows.Forms.DataGridViewComboBoxColumn();
			this.ImplName = new System.Windows.Forms.DataGridViewTextBoxColumn();
			((System.ComponentModel.ISupportInitialize)(this._grid)).BeginInit();
			this.SuspendLayout();
			// 
			// pbImplement
			// 
			this.pbImplement.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.pbImplement.Location = new System.Drawing.Point(454, 308);
			this.pbImplement.Name = "pbImplement";
			this.pbImplement.Size = new System.Drawing.Size(91, 23);
			this.pbImplement.TabIndex = 0;
			this.pbImplement.Text = "&Implement";
			this.pbImplement.UseVisualStyleBackColor = true;
			// 
			// pbCancel
			// 
			this.pbCancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.pbCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.pbCancel.Location = new System.Drawing.Point(551, 308);
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
			this._grid.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
									| System.Windows.Forms.AnchorStyles.Left)
									| System.Windows.Forms.AnchorStyles.Right)));
			this._grid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
			this._grid.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.ItfMemberNameCol,
            this.AddImplCol,
            this.Explicit,
            this.AccessMods,
            this.ImplName});
			this._grid.Location = new System.Drawing.Point(12, 12);
			this._grid.Name = "_grid";
			this._grid.Size = new System.Drawing.Size(614, 251);
			this._grid.TabIndex = 6;
			// 
			// ItfMemberNameCol
			// 
			this.ItfMemberNameCol.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.AllCells;
			this.ItfMemberNameCol.Frozen = true;
			this.ItfMemberNameCol.HeaderText = "Soutce name";
			this.ItfMemberNameCol.Name = "ItfMemberNameCol";
			this.ItfMemberNameCol.ReadOnly = true;
			this.ItfMemberNameCol.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable;
			this.ItfMemberNameCol.Width = 86;
			// 
			// AddImplCol
			// 
			this.AddImplCol.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.ColumnHeader;
			this.AddImplCol.HeaderText = "Implement";
			this.AddImplCol.Name = "AddImplCol";
			this.AddImplCol.Width = 72;
			// 
			// Explicit
			// 
			this.Explicit.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.ColumnHeader;
			this.Explicit.HeaderText = "Explicit";
			this.Explicit.Name = "Explicit";
			this.Explicit.Width = 52;
			// 
			// AccessMods
			// 
			this.AccessMods.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.AllCells;
			this.AccessMods.HeaderText = "Access modifier";
			this.AccessMods.Name = "AccessMods";
			this.AccessMods.Width = 89;
			// 
			// ImplName
			// 
			this.ImplName.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.AllCells;
			this.ImplName.HeaderText = "Implementation name";
			this.ImplName.Name = "ImplName";
			this.ImplName.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable;
			this.ImplName.Width = 121;
			// 
			// ImplementMembersForm
			// 
			this.AcceptButton = this.pbImplement;
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.CancelButton = this.pbCancel;
			this.ClientSize = new System.Drawing.Size(638, 343);
			this.Controls.Add(this._grid);
			this.Controls.Add(this.pbCancel);
			this.Controls.Add(this.pbImplement);
			this.MinimizeBox = false;
			this.Name = "ImplementMembersForm";
			this.Text = "ImplementMembersForm";
			this.Load += new System.EventHandler(this.ImplementMembersForm_Load);
			((System.ComponentModel.ISupportInitialize)(this._grid)).EndInit();
			this.ResumeLayout(false);

		}

		#endregion

		private System.Windows.Forms.Button pbImplement;
		private System.Windows.Forms.Button pbCancel;
		private System.Windows.Forms.DataGridView _grid;
		private System.Windows.Forms.DataGridViewTextBoxColumn ItfMemberNameCol;
		private System.Windows.Forms.DataGridViewCheckBoxColumn AddImplCol;
		private System.Windows.Forms.DataGridViewCheckBoxColumn Explicit;
		private System.Windows.Forms.DataGridViewComboBoxColumn AccessMods;
		private System.Windows.Forms.DataGridViewTextBoxColumn ImplName;

	}
}