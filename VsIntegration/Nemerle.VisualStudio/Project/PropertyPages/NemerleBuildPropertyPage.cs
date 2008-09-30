using System.Runtime.InteropServices;
using System.Diagnostics;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using MSSR = Microsoft.VisualStudio.Project.SR;
using System.ComponentModel;

namespace Nemerle.VisualStudio.Project.PropertyPages
{
	internal static class Consts
	{
		public const string DefineConstants = "DefineConstants";
		public const string OutputPath = "OutputPath";
	}

	[ComVisible(true)]
	[Guid(NemerleConstants.BuildPropertyPageGuidString)]
	public class NemerleBuildPropertyPage : PropertyPageBase
	{ //BuildPropertyPage xxx = null;
		#region ctor

		public NemerleBuildPropertyPage()
		{
			Name = SR.BuildCaption;
		}
		
		#endregion

		#region fields

		private string defineConstants;
		private string outputPath;
		
		#endregion

		#region properties

		[SRCategoryAttribute(SR.BuildCaption)]
		[SRDisplayName(SR.DefineConstants)]
		[SRDescriptionAttribute(SR.DefineConstantsDescription)]
		public string DefineConstants
		{
			get { return defineConstants; }
			set { defineConstants = value; IsDirty = true; }
		}

		[RefreshProperties(RefreshProperties.All)]
		[SRCategoryAttribute(SR.BuildCaption)]
		[LocDisplayName(MSSR.OutputPath)]
		[SRDescriptionAttribute(MSSR.OutputPathDescription)]
		public string OutputPath
		{
			get { return this.outputPath; }
			set { this.outputPath = value; this.IsDirty = true; }
		}

		[SRCategoryAttribute(SR.BuildCaption)]
		[SRDisplayName(SR.OutputPathFinalValue)]
		[SRDescriptionAttribute(MSSR.OutputPathDescription)]
		public string OutputPathFinalValue
		{
			get { return Evaluate(outputPath); }
		}

		#endregion

		#region overridden methods
		
		protected override void BindProperties()
		{
			if (ProjectMgr != null)
			{
				defineConstants = GetPropertyValue(Consts.DefineConstants);
				outputPath = GetPropertyValue(Consts.OutputPath);
			}
		}

		protected override int ApplyChanges()
		{
			if (this.ProjectMgr == null)
			{
				Debug.Assert(false);
				return VSConstants.E_INVALIDARG;
			}

			SetConfigProperty(Consts.DefineConstants, defineConstants);
			SetConfigProperty(Consts.OutputPath, outputPath);
			IsDirty = false;

			return VSConstants.S_OK;
		}
		#endregion
	}
}
