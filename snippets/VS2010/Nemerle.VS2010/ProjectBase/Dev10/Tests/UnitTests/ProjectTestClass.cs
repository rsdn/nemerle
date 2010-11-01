/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.Runtime.InteropServices;

namespace Microsoft.VisualStudio.Project.UnitTests
{
	/// <summary>
	/// We want to test interfaces on ProjectNode which is an
	/// abstract class, therefore we need a derived class
	/// </summary>
	[ComVisible(true)]
	[Guid("82B6E04F-DBE2-45ba-B881-36C6109D6D1E")]
	internal class ProjectTestClass : VisualStudio.Project.ProjectNode
	{
		public ProjectTestClass()
		{
			// This would normally be done by default, but since we don't do official loading initialize it ourselves
			this.ProjectMgr = this;

			// To avoid having to mock the SCC stuff simply disable it
			this.IsSccDisabled = true;
			this.DisableQueryEdit = true;
		}

		public override Guid ProjectGuid
		{
			get { return this.GetType().GUID; }
		}

		public override string ProjectType
		{
			get { return "FakeProjectForUnitTestPurpose"; }
		}
	}
}
