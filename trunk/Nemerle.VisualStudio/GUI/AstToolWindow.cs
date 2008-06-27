using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell;
using Nemerle.Compiler;
using Nemerle.Completion2;
using Nemerle.VisualStudio.Project;

namespace Nemerle.VisualStudio.GUI
{
	/// <summary>
	/// This class implements the tool window exposed by this package and hosts a user control.
	///
	/// In Visual Studio tool windows are composed of a frame (implemented by the shell) and a pane, 
	/// usually implemented by the package implementer.
	///
	/// This class derives from the ToolWindowPane class provided from the MPF in order to use its 
	/// implementation of the IVsWindowPane interface.
	/// </summary>
	[Guid("721e9eb7-98fa-4efb-9e17-d60a6894ccd0")]
	public class AstToolWindow : ToolWindowPane
	{
		static AstToolWindow _astToolWindow;
		public static AstToolWindow AstTool
		{
			get { return _astToolWindow; }
		}

		// This is the user control hosted by the tool window; it is exposed to the base class 
		// using the Window property. Note that, even if this class implements IDispose, we are
		// not calling Dispose on this object. This is because ToolWindowPane calls Dispose on 
		// the object returned by the Window property.
		private AstToolControl _control;

		public int BuildTypedtreeCount
		{
			get { return _control.BuildTypedtreeCount; }
			set { _control.BuildTypedtreeCount = value; }
		}

		/// <summary>
		/// Standard constructor for the tool window.
		/// </summary>
		public AstToolWindow()
			:
				base(null)
		{
			_astToolWindow = this;
			// Set the window title reading it from the resources.
			this.Caption = "AST";
			// Set the image that will appear on the tab of the window frame
			// when docked with an other window
			// The resource ID correspond to the one defined in the resx file
			// while the Index is the offset in the bitmap strip. Each image in
			// the strip being 16x16.
			this.BitmapResourceID = 301;
			this.BitmapIndex = 2;


			_control = new AstToolControl();
		}

		protected override void OnClose()
		{
			_astToolWindow = null;
			base.OnClose();
		}

		public bool IsAutoUpdate
		{
			get { return _control.IsAutoUpdate; }
		}

		public void Activate(int line, int col)
		{
			_control.Activate(line, col);
		}

		/// <summary>
		/// This property returns the handle to the user control that should
		/// be hosted in the Tool Window.
		/// </summary>
		override public IWin32Window Window
		{
			get { return (IWin32Window)_control; }
		}

		internal void ShowInfo(Nemerle.VisualStudio.LanguageService.NemerleSource source)
		{
			_control.ShowInfo(source);
		}
	}
}
