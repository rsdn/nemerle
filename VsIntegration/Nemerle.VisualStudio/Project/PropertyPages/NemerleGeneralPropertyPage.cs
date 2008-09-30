using System;
using System.Runtime.InteropServices;

using MSP = Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio;
using EnvDTE80;
using EnvDTE;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using Microsoft.VisualStudio.Shell.Interop;

namespace Nemerle.VisualStudio.Project.PropertyPages
{
	/// <include file='doc\PropertyPages.uex' path='docs/doc[@for="GeneralPropertyPage"]/*' />
	[ComVisible(true)]
	[Guid(NemerleConstants.GeneralPropertyPageGuidString)]
	public class NemerleGeneralPropertyPage : PropertyPageBase, IInternalExtenderProvider
	{
		#region constants

		static class Consts
		{
			public const string AssemblyName = "AssemblyName";
			public const string OutputType = "OutputType";
			public const string RootNamespace = "RootNamespace";
			public const string StartupObject = "StartupObject";
			public const string ApplicationIcon = "ApplicationIcon";
			public const string TargetPlatform = "TargetPlatform";
			public const string TargetPlatformLocation = "TargetPlatformLocation";
		}

		#endregion

		#region Fields

		string _assemblyName;
		MSP.OutputType _outputType;
		string _defaultNamespace;
		string _startupObject;
		string _applicationIcon;
		MSP.PlatformType _targetPlatform = MSP.PlatformType.v2;
		string _targetPlatformLocation;

		#endregion

		#region ctor

		public NemerleGeneralPropertyPage()
		{
			Name = SR.GetString(SR.GeneralCaption);
		}

		#endregion

		#region Overriden Methods

		public override string GetClassName()
		{
			return GetType().FullName;
		}

		protected override void BindProperties()
		{
			if (ProjectMgr == null)
			{
				Debug.Assert(false);
				return;
			}

			_assemblyName = GetPropertyValue(Consts.AssemblyName);

			var outputType = GetPropertyValue(Consts.OutputType);

			if (outputType != null && outputType.Length > 0)
			{
				try
				{
					_outputType = (MSP.OutputType)Enum.Parse(typeof(MSP.OutputType), outputType);
				}
				catch //Should only fail if project file is corrupt
				{
				}
			}

			_defaultNamespace = GetPropertyValue(Consts.RootNamespace);
			_startupObject = GetPropertyValue(Consts.StartupObject);
			_applicationIcon = GetPropertyValue(Consts.ApplicationIcon);

			string targetPlatform = GetPropertyValue(Consts.TargetPlatform);

			if (targetPlatform != null && targetPlatform.Length > 0)
			{
				try
				{
					_targetPlatform =
						(MSP.PlatformType)Enum.Parse(typeof(MSP.PlatformType), targetPlatform);
				}
				catch
				{
				}
			}

			_targetPlatformLocation = GetPropertyValue(Consts.TargetPlatformLocation);
		}

		protected override int ApplyChanges()
		{
			if (ProjectMgr == null)
			{
				Debug.Assert(false);
				return VSConstants.E_INVALIDARG;
			}

			SetPropertyValue(Consts.AssemblyName,           _assemblyName);
			SetPropertyValue(Consts.OutputType,             _outputType.ToString());
			SetPropertyValue(Consts.RootNamespace,          _defaultNamespace);
			SetPropertyValue(Consts.StartupObject,          _startupObject);
			SetPropertyValue(Consts.ApplicationIcon,        _applicationIcon);
			SetPropertyValue(Consts.TargetPlatform,         _targetPlatform.ToString());
			SetPropertyValue(Consts.TargetPlatformLocation, _targetPlatformLocation);

			IsDirty = false;

			return VSConstants.S_OK;
		}

		#endregion

		#region exposed properties

		[RefreshProperties(RefreshProperties.All)]
		[SRCategoryAttribute(SR.Application)]
		[SRDisplayName(SR.AssemblyName)]
		[SRDescriptionAttribute(SR.AssemblyNameDescription)]
		public string AssemblyName
		{
			get { return _assemblyName; }
			set { _assemblyName = value; IsDirty = true; }
		}

		[RefreshProperties(RefreshProperties.All)]
		[SRCategoryAttribute(SR.Application)]
		[SRDisplayName(SR.OutputType)]
		[SRDescriptionAttribute(SR.OutputTypeDescription)]
		public MSP.OutputType OutputType
		{
			get { return _outputType; }
			set { _outputType = value; IsDirty = true; }
		}

		[SRCategoryAttribute(SR.Application)]
		[SRDisplayName(SR.DefaultNamespace)]
		[SRDescriptionAttribute(SR.DefaultNamespaceDescription)]
		public string DefaultNamespace
		{
			get { return _defaultNamespace; }
			set { _defaultNamespace = value; IsDirty = true; }
		}

		[SRCategoryAttribute(SR.Application)]
		[SRDisplayName(SR.StartupObject)]
		[SRDescriptionAttribute(SR.StartupObjectDescription)]
		public string StartupObject
		{
			get { return _startupObject; }
			set { _startupObject = value; IsDirty = true; }
		}

		[SRCategoryAttribute(SR.Application)]
		[SRDisplayName(SR.ApplicationIcon)]
		[SRDescriptionAttribute(SR.ApplicationIconDescription)]
		public string ApplicationIcon
		{
			get { return _applicationIcon; }
			set { _applicationIcon = value; IsDirty = true; }
		}

		[SRCategoryAttribute(SR.Project)]
		[SRDisplayName(SR.ProjectFile)]
		[SRDescriptionAttribute(SR.ProjectFileDescription)]
		[MSP.AutomationBrowsable(false)]
		public string ProjectFile
		{
			get { return Path.GetFileName(ProjectMgr.ProjectFile); }
		}

		[SRCategoryAttribute(SR.Project)]
		[SRDisplayName(SR.ProjectFolder)]
		[SRDescriptionAttribute(SR.ProjectFolderDescription)]
		[MSP.AutomationBrowsable(false)]
		public string ProjectFolder
		{
			get { return Path.GetDirectoryName(ProjectMgr.ProjectFolder); }
		}

		[SRCategoryAttribute(SR.Project)]
		[SRDisplayName(SR.OutputFile)]
		[SRDescriptionAttribute(SR.OutputFileDescription)]
		[MSP.AutomationBrowsable(false)]
		public string OutputFile
		{
			get { return Evaluate(_assemblyName) + NemerleProjectNode.GetOutputExtension(_outputType); }
		}

		[SRCategoryAttribute(SR.Project)]
		[SRDisplayName(SR.TargetPlatform)]
		[SRDescriptionAttribute(SR.TargetPlatformDescription)]
		[MSP.AutomationBrowsable(false)]
		public MSP.PlatformType TargetPlatform
		{
			get { return _targetPlatform; }
			set { _targetPlatform = value; IsDirty = true; }
		}

		[SRCategoryAttribute(SR.Project)]
		[SRDisplayName(SR.TargetPlatformLocation)]
		[SRDescriptionAttribute(SR.TargetPlatformLocationDescription)]
		[MSP.AutomationBrowsable(false)]
		public string TargetPlatformLocation
		{
			get { return _targetPlatformLocation; }
			set { _targetPlatformLocation = value; IsDirty = true; }
		}

		#endregion

		#region IInternalExtenderProvider Members

		bool IInternalExtenderProvider.CanExtend(string extenderCATID, string extenderName, object extendeeObject)
		{
			IVsHierarchy outerHierarchy = MSP.HierarchyNode.GetOuterHierarchy(ProjectMgr);

			if (outerHierarchy is IInternalExtenderProvider)
				return ((IInternalExtenderProvider)outerHierarchy).CanExtend(
					extenderCATID, extenderName, extendeeObject);

			return false;
		}

		object IInternalExtenderProvider.GetExtender(
			string extenderCATID,
			string extenderName,
			object extendeeObject,
			IExtenderSite extenderSite,
			int cookie)
		{
			IVsHierarchy outerHierarchy = MSP.HierarchyNode.GetOuterHierarchy(ProjectMgr);

			if (outerHierarchy is IInternalExtenderProvider)
				return ((IInternalExtenderProvider)outerHierarchy).GetExtender(
					extenderCATID, extenderName, extendeeObject, extenderSite, cookie);

			return null;
		}

		object IInternalExtenderProvider.GetExtenderNames(string extenderCATID,
			object extendeeObject)
		{
			IVsHierarchy outerHierarchy = MSP.HierarchyNode.GetOuterHierarchy(ProjectMgr);

			if (outerHierarchy is IInternalExtenderProvider)
				return ((IInternalExtenderProvider)outerHierarchy).GetExtenderNames(extenderCATID, extendeeObject);

			return null;
		}

		#endregion

		#region ExtenderSupport

		[Browsable(false)]
		[MSP.AutomationBrowsable(false)]
		public virtual string ExtenderCATID
		{
			get
			{
				Guid catid = ProjectMgr.ProjectMgr.GetCATIDForType(GetType());

				if (Guid.Empty.CompareTo(catid) == 0)
					throw new NotImplementedException();

				return catid.ToString("B");
			}
		}

		[Browsable(false)]
		[MSP.AutomationBrowsable(false)]
		public object ExtenderNames
		{
			get
			{
				ObjectExtenders extenderService = (ObjectExtenders)ProjectMgr.GetService(typeof(ObjectExtenders));
				return extenderService.GetExtenderNames(ExtenderCATID, this);
			}
		}

		public object get_Extender(string extenderName)
		{
			ObjectExtenders extenderService = (ObjectExtenders)ProjectMgr.GetService(typeof(ObjectExtenders));
			return extenderService.GetExtender(ExtenderCATID, extenderName, this);
		}

		#endregion
	}
}
