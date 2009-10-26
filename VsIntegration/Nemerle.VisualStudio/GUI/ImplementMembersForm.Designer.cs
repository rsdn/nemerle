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
			this.ItfMemberNameCol = new System.Windows.Forms.DataGridViewTextBoxColumn();
			this.AddImplCol = new System.Windows.Forms.DataGridViewCheckBoxColumn();
			this.Explicit = new System.Windows.Forms.DataGridViewCheckBoxColumn();
			this.AccessMods = new System.Windows.Forms.DataGridViewComboBoxColumn();
			this.ImplName = new System.Windows.Forms.DataGridViewTextBoxColumn();
			this.Signature = new System.Windows.Forms.DataGridViewTextBoxColumn();
			this.panel1 = new System.Windows.Forms.Panel();
			this.imageList1 = new System.Windows.Forms.ImageList(this.components);
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
            this.ItfMemberNameCol,
            this.AddImplCol,
            this.Explicit,
            this.AccessMods,
            this.ImplName,
            this.Signature});
			this._grid.Dock = System.Windows.Forms.DockStyle.Fill;
			this._grid.Location = new System.Drawing.Point(0, 0);
			this._grid.Name = "_grid";
			this._grid.Size = new System.Drawing.Size(620, 362);
			this._grid.TabIndex = 6;
			// 
			// ItfMemberNameCol
			// 
			this.ItfMemberNameCol.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.None;
			this.ItfMemberNameCol.Frozen = true;
			this.ItfMemberNameCol.HeaderText = "Soutce name";
			this.ItfMemberNameCol.MinimumWidth = 20;
			this.ItfMemberNameCol.Name = "ItfMemberNameCol";
			this.ItfMemberNameCol.ReadOnly = true;
			this.ItfMemberNameCol.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable;
			this.ItfMemberNameCol.Width = 250;
			// 
			// AddImplCol
			// 
			this.AddImplCol.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.ColumnHeader;
			this.AddImplCol.Frozen = true;
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
			this.AccessMods.DisplayStyle = System.Windows.Forms.DataGridViewComboBoxDisplayStyle.Nothing;
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
			// Signature
			// 
			this.Signature.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.AllCells;
			this.Signature.HeaderText = "Signature";
			this.Signature.Name = "Signature";
			this.Signature.ReadOnly = true;
			this.Signature.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable;
			this.Signature.Width = 66;
			// 
			// panel1
			// 
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
			this.ResumeLayout(false);

		}

		#endregion

		private System.Windows.Forms.Button pbImplement;
		private System.Windows.Forms.Button pbCancel;
    private System.Windows.Forms.DataGridView _grid;
    private System.Windows.Forms.Panel panel1;
    private System.Windows.Forms.ImageList imageList1;
    private System.Windows.Forms.DataGridViewTextBoxColumn ItfMemberNameCol;
    private System.Windows.Forms.DataGridViewCheckBoxColumn AddImplCol;
    private System.Windows.Forms.DataGridViewCheckBoxColumn Explicit;
    private System.Windows.Forms.DataGridViewComboBoxColumn AccessMods;
    private System.Windows.Forms.DataGridViewTextBoxColumn ImplName;
    private System.Windows.Forms.DataGridViewTextBoxColumn Signature;

	}
}