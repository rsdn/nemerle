using System.Runtime.InteropServices;
using System.ComponentModel;
using System.Diagnostics;
using Microsoft.VisualStudio;
using System.Drawing.Design;
using System.Windows.Forms.Design;

namespace Nemerle.VisualStudio.Project.PropertyPages
{
	/// <summary>
	/// Defines the properties on the debug property page and the logic the binds the properties to project data (load and save)
	/// </summary>
	[ComVisible(true)]
	[Guid(NemerleConstants.DebugPropertyPageGuidString)]
	public class NemerleDebugPropertyPage : PropertyPageBase
	{
		#region ctor

		public NemerleDebugPropertyPage()
		{
			Name = Resources.DebugCaption;
		}
		
		#endregion

		#region fields

		private string _startProgram;
		private string _workingDirectory;
		private string _cmdArgs;

		#endregion

		#region properties

		[SRCategoryAttribute(SR.DebugCaption)]
		[SRDisplayName(SR.StartProgram)]
		[SRDescriptionAttribute(SR.StartProgramDescription)]
		[Editor(typeof(FileNameEditor), typeof(UITypeEditor))]
		public string StartProgram
		{
			get { return _startProgram; }
			set { _startProgram = value; IsDirty = true; }
		}

		[SRCategoryAttribute(SR.DebugCaption)]
		[SRDisplayName(SR.WorkingDirectory)]
		[SRDescriptionAttribute(SR.WorkingDirectoryDescription)]
		[Editor(typeof(FolderNameEditor), typeof(UITypeEditor))]
		public string WorkingDirectory
		{
			get { return _workingDirectory; }
			set { _workingDirectory = value; IsDirty = true; }
		}

		[SRCategoryAttribute(SR.DebugCaption)]
		[SRDisplayName(SR.CmdArgs)]
		[SRDescriptionAttribute(SR.CmdArgsDescription)]
		public string CmdArgs
		{
			get { return _cmdArgs; }
			set { _cmdArgs = value; IsDirty = true; }
		}

		#endregion

		#region overridden methods

		protected override void BindProperties()
		{
			if (ProjectMgr == null)
			{
				Debug.Assert(false);
				return;
			}

			_startProgram = GetPropertyValue(DebugPropertyPageTag.StartProgram);
			_workingDirectory = GetPropertyValue(DebugPropertyPageTag.WorkingDirectory);
			_cmdArgs = GetPropertyValue(DebugPropertyPageTag.CmdArgs);
		}

		protected override int ApplyChanges()
		{
			if (ProjectMgr == null)
			{
				Debug.Assert(false);
				return VSConstants.E_INVALIDARG;
			}

			SetPropertyValue(DebugPropertyPageTag.StartProgram, _startProgram);
			SetPropertyValue(DebugPropertyPageTag.WorkingDirectory, _workingDirectory);
			SetPropertyValue(DebugPropertyPageTag.CmdArgs, _cmdArgs);

			IsDirty = false;

			return VSConstants.S_OK;
		}
		#endregion

		#region Utility methods

		string GetPropertyValue(DebugPropertyPageTag propertyTag)
		{
			var propertyName = propertyTag.ToString();
			return GetPropertyValue(propertyName);
		}
		
		void SetPropertyValue(DebugPropertyPageTag propertyTag, string value)
		{
			var propertyName = propertyTag.ToString();
			SetConfigProperty(propertyName, value);
		} 
		
		#endregion
	}
}
