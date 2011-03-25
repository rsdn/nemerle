/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.IO;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

using EnvDTE;
using Microsoft.VisualStudio.TextManager.Interop;

using CDCodeNamespace = System.CodeDom.CodeNamespace;
using VSCodeNamespace = EnvDTE.CodeNamespace;
using ErrorHandler = Microsoft.VisualStudio.ErrorHandler;
using Nemerle.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Designer.Interfaces;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Shell.Interop;

namespace Nemerle.VisualStudio.FileCodeModel
{

	/// <summary>
	/// Provides a FileCodeModel based upon the representation of the program obtained via CodeDom.
	/// 
	/// There are 3 ways a document can be edited that the code model needs to handle:
	///     1. The user edits the document inside of VS.  Here we don't need
	///        to update the code model until the next call back to manipulate it.
	///     2. A script runs which uses EditPoint's to update the text of the document.  
	///     3. The user uses the FileCodeModel to add members to the document.
	/// 
	/// </summary>
	internal class NemerleFileCodeModel : FileCodeModelBase
	{
		private CodeDomProvider _codeDomProvider;
		private FileNode _fileNode;

		public NemerleFileCodeModel(EnvDTE.ProjectItem projectItem, FileNode fileNode)
			: base(projectItem)
		{
			_fileNode = fileNode;
			_codeDomProvider = GetCodeDomProvider();
		}

		private CodeDomProvider GetCodeDomProvider()
		{
			if (_fileNode is NemerleFileNode)
				return ((NemerleFileNode)_fileNode).CodeDomProvider;
			else
			{
				// более универсальный способ получить CodeDomProvider. Скопирован из примера интеграции IronPython
				using (ServiceProvider sp = new ServiceProvider(_fileNode.OleServiceProvider))
				{
					IVSMDCodeDomProvider smdProvider = sp.GetService(typeof(Microsoft.VisualStudio.Shell.Interop.SVSMDCodeDomProvider)) as IVSMDCodeDomProvider;

					if (null == smdProvider)
						return null;

					return smdProvider.CodeDomProvider as CodeDomProvider;
				}
			}
		}

		protected override void Initialize()
		{
			var projectInfo = Nemerle.VisualStudio.Project.ProjectInfo.FindProject(_fileNode.Url);

			if (projectInfo != null)
				projectInfo.Engine.ProcessPendingTypesTreeRequest();

			CompileUnit = _codeDomProvider.Parse(null);
		}

		internal override void FlushChanges()
		{
			bool contextAlreadyOpenned = DTE.UndoContext.IsOpen;
			if (!contextAlreadyOpenned)
				DTE.UndoContext.Open("Undo Code Merge", false);

			try
			{
				_codeDomProvider.GenerateCodeFromCompileUnit(CompileUnit, null, null);
			}
			finally
			{
				if (!contextAlreadyOpenned)
					DTE.UndoContext.Close();
			}
		}
	}
}
