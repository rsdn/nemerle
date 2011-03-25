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
		public const string DocumentationFile = "DocumentationFile";
		public const string DebugSymbols = "DebugSymbols";
	}

	[ComVisible(true)]
	[Guid(NemerleConstants.BuildPropertyPageGuidString)]
	public class NemerleBuildPropertyPage : PropertyPageBase
	{ //BuildPropertyPage xxx = null;
		#region ctor

		public NemerleBuildPropertyPage()
		{
			Name = Resources.BuildCaption;
		}
		
		#endregion

		#region fields

		private string defineConstants;
		private string outputPath;
		private string docFile;
		private bool debugSymbols;

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

		[SRCategoryAttribute(SR.BuildCaption)]
		[SRDisplayName(SR.DecumentationFile)]
		[SRDescriptionAttribute(SR.DecumentationFileDescription)]
		public string DecumentationFile
		{
			get { return docFile; }
			set
			{
				docFile = string.IsNullOrEmpty(value)? null: value;
				IsDirty = true;
			}
		}

		[RefreshProperties(RefreshProperties.All)]
		[SRCategoryAttribute(SR.BuildCaption)]
		[LocDisplayName(MSSR.OutputPath)]
		[SRDescriptionAttribute(MSSR.OutputPathDescription)]
		public string OutputPath
		{
			get { return outputPath; }
			set { outputPath = value; IsDirty = true; }
		}

		[SRCategoryAttribute(SR.BuildCaption)]
		[SRDisplayName(SR.OutputPathFinalValue)]
		[SRDescriptionAttribute(MSSR.OutputPathDescription)]
		public string OutputPathFinalValue
		{
			get { return Evaluate(outputPath); }
		}
		[SRCategoryAttribute(SR.BuildCaption)]
		[SRDisplayName(SR.DebugSymbols)]
		[SRDescriptionAttribute(SR.DebugSymbolsDescription)]
		public bool DebugSymbols
		{
			get { return debugSymbols; }
			set { debugSymbols = value; IsDirty = true; }
		}

		#endregion

		#region overridden methods
		
		protected override void BindProperties()
		{
			if (ProjectMgr != null)
			{
				defineConstants   = GetPropertyValue(Consts.DefineConstants);
				outputPath        = GetPropertyValue(Consts.OutputPath);
				docFile           = GetPropertyValue(Consts.DocumentationFile);

				var s = GetPropertyValue(Consts.DebugSymbols);
				var found = bool.TryParse(s, out debugSymbols);
				
				// default behavior of msbuild when "DebugSymbols" element is not defined
				if (found == false)
				{
					debugSymbols = false;
					foreach (var c in this.GetProjectConfigurations())
					{
						if(c.ConfigName == "Debug")
						{
							debugSymbols = true;
							break;
						}
					}
				}
			}
		}

		protected override int ApplyChanges()
		{
			if (ProjectMgr == null)
			{
				Debug.Fail("No project manager?!");
				return VSConstants.E_INVALIDARG;
			}

			var projNode = (NemerleProjectNode)ProjectMgr;

			SetConfigProperty(Consts.DefineConstants, defineConstants);
			SetConfigProperty(Consts.OutputPath, outputPath);
			SetPropertyValue (Consts.DocumentationFile, docFile);
            SetConfigProperty(Consts.DebugSymbols, debugSymbols.ToString().ToLower());

			IsDirty = false;

			projNode.ProjectInfo.Engine.RequestOnReloadProject();

			return VSConstants.S_OK;
		}
		#endregion
	}
}
