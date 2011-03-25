using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Nemerle.VisualStudio.FileCodeModel;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using System.CodeDom.Compiler;
using System.Diagnostics.CodeAnalysis;

namespace Nemerle.VisualStudio.LanguageService
{
	class NemerleSourceFileInfo
	{
		private string _fileName;
		private uint _itemId;
		private IVsIntellisenseProjectHost _hostProject;
		private EnvDTE.FileCodeModel _fileCodeModel;
		private CodeDomProvider _codeProvider;
		private EnvDTE.ProjectItem _projectItem;

		public NemerleSourceFileInfo(string name, uint id)
		{
			_fileName = name;
			_itemId = id;
		}

		public uint ItemId
		{
			get { return _itemId; }
		}

		public string Name
		{
			[SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
			get { return _fileName; }
		}

		public IVsIntellisenseProjectHost HostProject
		{
			[SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
			get 
			{ 
				return _hostProject; 
			}

			set
			{
				if (_hostProject != value)
				{
					_fileCodeModel = null;
				}

				_hostProject = value;
			}
		}

		public CodeDomProvider CodeProvider
		{
			[SuppressMessage("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
			get 
			{ 
				return _codeProvider; 
			}

			set
			{
				if (value != _codeProvider)
				{
					_fileCodeModel = null;
				}

				_codeProvider = value;
			}
		}

		public EnvDTE.FileCodeModel FileCodeModel
		{
			get 
			{
				if (_fileCodeModel != null)
					return _fileCodeModel;

				// Verify that the host project is set.
				if (_hostProject == null)
				{
					throw new System.InvalidOperationException();
				}

				// Get the hierarchy from the host project.
				object propValue;
				ErrorHandler.ThrowOnFailure(_hostProject.GetHostProperty((uint)HOSTPROPID.HOSTPROPID_HIERARCHY, out propValue));
				IVsHierarchy hierarchy = propValue as IVsHierarchy;
				if (hierarchy == null)
					throw new System.InvalidOperationException();

				_fileCodeModel = new NemerleSingleFileCodeModel(hierarchy, _itemId, _projectItem, _codeProvider);

				return _fileCodeModel; 
			}
		}

		public EnvDTE.ProjectItem ProjectItem
		{
			get
			{
				if (_projectItem != null)
					return _projectItem;

				// Verify that the host project is set.
				if (_hostProject == null)
				{
					throw new System.InvalidOperationException();
				}

				// Get the hierarchy from the host project.
				object propValue;
				ErrorHandler.ThrowOnFailure(_hostProject.GetHostProperty((uint)HOSTPROPID.HOSTPROPID_HIERARCHY, out propValue));
				IVsHierarchy hierarchy = propValue as IVsHierarchy;
				if (hierarchy == null)
					throw new System.InvalidOperationException();

				// Try to get the extensibility object for the item.
				// NOTE: here we assume that the __VSHPROPID.VSHPROPID_ExtObject property returns a VSLangProj.VSProjectItem
				// or a EnvDTE.ProjectItem object. No other kind of extensibility is supported.
				propValue = null;
				ErrorHandler.ThrowOnFailure(hierarchy.GetProperty(_itemId, (int)__VSHPROPID.VSHPROPID_ExtObject, out propValue));

				VSLangProj.VSProjectItem vsprojItem = propValue as VSLangProj.VSProjectItem;
				if (null == vsprojItem)
				{
					_projectItem = propValue as EnvDTE.ProjectItem;
				}
				else
				{
					_projectItem = vsprojItem.ProjectItem;
				}

				return _projectItem;
			}
		}
	}
}
