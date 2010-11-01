/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Project.Automation;
using System.Runtime.Versioning;

namespace Microsoft.VisualStudio.Project.Samples.NestedProject.UnitTests
{
	[System.Diagnostics.DebuggerStepThrough()]
	[System.CodeDom.Compiler.GeneratedCodeAttribute("Microsoft.VisualStudio.TestTools.UnitTestGeneration", "1.0.0.0")]
	internal class BaseAccessor
	{

		protected Microsoft.VisualStudio.TestTools.UnitTesting.PrivateObject m_privateObject;

		protected BaseAccessor(object target, Microsoft.VisualStudio.TestTools.UnitTesting.PrivateType type)
		{
			m_privateObject = new Microsoft.VisualStudio.TestTools.UnitTesting.PrivateObject(target, type);
		}

		protected BaseAccessor(Microsoft.VisualStudio.TestTools.UnitTesting.PrivateType type)
			:
				this(null, type)
		{
		}

		internal virtual object Target
		{
			get
			{
				return m_privateObject.Target;
			}
		}

		public override string ToString()
		{
			return this.Target.ToString();
		}

		public override bool Equals(object obj)
		{
			if(typeof(BaseAccessor).IsInstanceOfType(obj))
			{
				obj = ((BaseAccessor)(obj)).Target;
			}
			return this.Target.Equals(obj);
		}

		public override int GetHashCode()
		{
			return this.Target.GetHashCode();
		}
	}


	[System.Diagnostics.DebuggerStepThrough()]
	[System.CodeDom.Compiler.GeneratedCodeAttribute("Microsoft.VisualStudio.TestTools.UnitTestGeneration", "1.0.0.0")]
	internal class VisualStudio_Project_Samples_GeneralPropertyPageAccessor : BaseAccessor
	{

		protected static Microsoft.VisualStudio.TestTools.UnitTesting.PrivateType m_privateType = new Microsoft.VisualStudio.TestTools.UnitTesting.PrivateType(typeof(GeneralPropertyPage));

		internal VisualStudio_Project_Samples_GeneralPropertyPageAccessor(GeneralPropertyPage target)
			:
				base(target, m_privateType)
		{
		}

		internal string assemblyName
		{
			get
			{
				string ret = ((string)(m_privateObject.GetField("assemblyName")));
				return ret;
			}
			set
			{
				m_privateObject.SetField("assemblyName", value);
			}
		}

		internal OutputType outputType
		{
			get
			{
				OutputType ret = ((OutputType)(m_privateObject.GetField("outputType")));
				return ret;
			}
			set
			{
				m_privateObject.SetField("outputType", value);
			}
		}

		internal string defaultNamespace
		{
			get
			{
				string ret = ((string)(m_privateObject.GetField("defaultNamespace")));
				return ret;
			}
			set
			{
				m_privateObject.SetField("defaultNamespace", value);
			}
		}

		internal string startupObject
		{
			get
			{
				string ret = ((string)(m_privateObject.GetField("startupObject")));
				return ret;
			}
			set
			{
				m_privateObject.SetField("startupObject", value);
			}
		}

		internal string applicationIcon
		{
			get
			{
				string ret = ((string)(m_privateObject.GetField("applicationIcon")));
				return ret;
			}
			set
			{
				m_privateObject.SetField("applicationIcon", value);
			}
		}

		internal FrameworkName targetFrameworkMoniker
		{
			get
			{
				FrameworkName ret = ((FrameworkName)(m_privateObject.GetField("targetFrameworkMoniker")));
				return ret;
			}
			set
			{
				m_privateObject.SetField("targetFrameworkMoniker", value);
			}
		}

		internal void BindProperties()
		{
			object[] args = new object[0];
			m_privateObject.Invoke("BindProperties", new System.Type[0], args);
		}

		internal int ApplyChanges()
		{
			object[] args = new object[0];
			int ret = ((int)(m_privateObject.Invoke("ApplyChanges", new System.Type[0], args)));
			return ret;
		}
	}
	[System.Diagnostics.DebuggerStepThrough()]
	[System.CodeDom.Compiler.GeneratedCodeAttribute("Microsoft.VisualStudio.TestTools.UnitTestGeneration", "1.0.0.0")]
	internal class VisualStudio_Project_Samples_NestedProjectPackageAccessor : BaseAccessor
	{

		protected static Microsoft.VisualStudio.TestTools.UnitTesting.PrivateType m_privateType = new Microsoft.VisualStudio.TestTools.UnitTesting.PrivateType(typeof(NestedProjectPackage));

		internal VisualStudio_Project_Samples_NestedProjectPackageAccessor(NestedProjectPackage target)
			:
				base(target, m_privateType)
		{
		}

		internal void Initialize()
		{
			object[] args = new object[0];
			m_privateObject.Invoke("Initialize", new System.Type[0], args);
		}
	}
	[System.Diagnostics.DebuggerStepThrough()]
	[System.CodeDom.Compiler.GeneratedCodeAttribute("Microsoft.VisualStudio.TestTools.UnitTestGeneration", "1.0.0.0")]
	internal class VisualStudio_Project_Samples_OANestedProjectPropertyAccessor : BaseAccessor
	{

		protected static Microsoft.VisualStudio.TestTools.UnitTesting.PrivateType m_privateType = new Microsoft.VisualStudio.TestTools.UnitTesting.PrivateType(typeof(OANestedProjectProperty));

		internal VisualStudio_Project_Samples_OANestedProjectPropertyAccessor(OANestedProjectProperty target)
			:
				base(target, m_privateType)
		{
		}

		internal OAProperties parent
		{
			get
			{
				OAProperties ret = ((OAProperties)(m_privateObject.GetField("parent")));
				return ret;
			}
			set
			{
				m_privateObject.SetField("parent", value);
			}
		}

		internal string name
		{
			get
			{
				string ret = ((string)(m_privateObject.GetField("name")));
				return ret;
			}
			set
			{
				m_privateObject.SetField("name", value);
			}
		}

		internal static OANestedProjectProperty CreatePrivate(OANestedProjectProperties parent, string name)
		{
			object[] args = new object[] {
                parent,
                name};
			Microsoft.VisualStudio.TestTools.UnitTesting.PrivateObject priv_obj = new Microsoft.VisualStudio.TestTools.UnitTesting.PrivateObject(typeof(OANestedProjectProperty), new System.Type[] {
                    typeof(OANestedProjectProperties),
                    typeof(string)}, args);
			return ((OANestedProjectProperty)(priv_obj.Target));
		}
	}
	[System.Diagnostics.DebuggerStepThrough()]
	[System.CodeDom.Compiler.GeneratedCodeAttribute("Microsoft.VisualStudio.TestTools.UnitTestGeneration", "1.0.0.0")]
	internal class VisualStudio_Project_Samples_ResourcesDescriptionAttributeAccessor : BaseAccessor
	{

		protected static Microsoft.VisualStudio.TestTools.UnitTesting.PrivateType m_privateType = new Microsoft.VisualStudio.TestTools.UnitTesting.PrivateType("Microsoft.VisualStudio.Project.Samples.NestedProject", "Microsoft.VisualStudio.Project.Samples.NestedProject.ResourcesDescriptionAttribute");

		internal VisualStudio_Project_Samples_ResourcesDescriptionAttributeAccessor(object target) :
			base(target, m_privateType)
		{
		}

		internal bool replaced
		{
			get
			{
				bool ret = ((bool)(m_privateObject.GetField("replaced")));
				return ret;
			}
			set
			{
				m_privateObject.SetField("replaced", value);
			}
		}

		internal string Description
		{
			get
			{
				string ret = ((string)(m_privateObject.GetProperty("Description")));
				return ret;
			}
		}

		internal static global::System.ComponentModel.DescriptionAttribute CreatePrivate(string description)
		{
			object[] args = new object[] {
                description};
			Microsoft.VisualStudio.TestTools.UnitTesting.PrivateObject priv_obj = new Microsoft.VisualStudio.TestTools.UnitTesting.PrivateObject("Microsoft.VisualStudio.Project.Samples.NestedProject", "Microsoft.VisualStudio.Project.Samples.NestedProject.ResourcesDescriptionAttribute", new System.Type[] {
                    typeof(string)}, args);
			return ((global::System.ComponentModel.DescriptionAttribute)(priv_obj.Target));
		}
	}
	[System.Diagnostics.DebuggerStepThrough()]
	[System.CodeDom.Compiler.GeneratedCodeAttribute("Microsoft.VisualStudio.TestTools.UnitTestGeneration", "1.0.0.0")]
	internal class VisualStudio_Project_Samples_ResourcesCategoryAttributeAccessor : BaseAccessor
	{

		protected static Microsoft.VisualStudio.TestTools.UnitTesting.PrivateType m_privateType = new Microsoft.VisualStudio.TestTools.UnitTesting.PrivateType("Microsoft.VisualStudio.Project.Samples.NestedProject", "Microsoft.VisualStudio.Project.Samples.NestedProject.ResourcesCategoryAttribute");

		internal VisualStudio_Project_Samples_ResourcesCategoryAttributeAccessor(object target) :
			base(target, m_privateType)
		{
		}

		internal static global::System.ComponentModel.CategoryAttribute CreatePrivate(string category)
		{
			object[] args = new object[] {
                category};
			Microsoft.VisualStudio.TestTools.UnitTesting.PrivateObject priv_obj = new Microsoft.VisualStudio.TestTools.UnitTesting.PrivateObject("Microsoft.VisualStudio.Project.Samples.NestedProject", "Microsoft.VisualStudio.Project.Samples.NestedProject.ResourcesCategoryAttribute", new System.Type[] {
                    typeof(string)}, args);
			return ((global::System.ComponentModel.CategoryAttribute)(priv_obj.Target));
		}

		internal string GetLocalizedString(string value)
		{
			object[] args = new object[] {
                value};
			string ret = ((string)(m_privateObject.Invoke("GetLocalizedString", new System.Type[] {
                    typeof(string)}, args)));
			return ret;
		}
	}
	[System.Diagnostics.DebuggerStepThrough()]
	[System.CodeDom.Compiler.GeneratedCodeAttribute("Microsoft.VisualStudio.TestTools.UnitTestGeneration", "1.0.0.0")]
	internal class VisualStudio_Project_Samples_LocDisplayNameAttributeAccessor : BaseAccessor
	{

		protected static Microsoft.VisualStudio.TestTools.UnitTesting.PrivateType m_privateType = new Microsoft.VisualStudio.TestTools.UnitTesting.PrivateType("Microsoft.VisualStudio.Project.Samples.NestedProject", "Microsoft.VisualStudio.Project.Samples.NestedProject.LocDisplayNameAttribute");

		internal VisualStudio_Project_Samples_LocDisplayNameAttributeAccessor(object target) :
			base(target, m_privateType)
		{
		}

		internal string name
		{
			get
			{
				string ret = ((string)(m_privateObject.GetField("name")));
				return ret;
			}
			set
			{
				m_privateObject.SetField("name", value);
			}
		}

		internal string DisplayName
		{
			get
			{
				string ret = ((string)(m_privateObject.GetProperty("DisplayName")));
				return ret;
			}
		}

		internal static global::System.ComponentModel.DisplayNameAttribute CreatePrivate(string name)
		{
			object[] args = new object[] {
                name};
			Microsoft.VisualStudio.TestTools.UnitTesting.PrivateObject priv_obj = new Microsoft.VisualStudio.TestTools.UnitTesting.PrivateObject("Microsoft.VisualStudio.Project.Samples.NestedProject", "Microsoft.VisualStudio.Project.Samples.NestedProject.LocDisplayNameAttribute", new System.Type[] {
                    typeof(string)}, args);
			return ((global::System.ComponentModel.DisplayNameAttribute)(priv_obj.Target));
		}
	}
	[System.Diagnostics.DebuggerStepThrough()]
	[System.CodeDom.Compiler.GeneratedCodeAttribute("Microsoft.VisualStudio.TestTools.UnitTestGeneration", "1.0.0.0")]
	internal class VisualStudio_Project_Samples_NestedProjectFactoryAccessor : BaseAccessor
	{

		protected static Microsoft.VisualStudio.TestTools.UnitTesting.PrivateType m_privateType = new Microsoft.VisualStudio.TestTools.UnitTesting.PrivateType(typeof(NestedProjectFactory));

		internal VisualStudio_Project_Samples_NestedProjectFactoryAccessor(NestedProjectFactory target) :
			base(target, m_privateType)
		{
		}

		internal ProjectNode CreateProject()
		{
			object[] args = new object[0];
			ProjectNode ret = ((ProjectNode)(m_privateObject.Invoke("CreateProject", new System.Type[0], args)));
			return ret;
		}
	}
	[System.Diagnostics.DebuggerStepThrough()]
	[System.CodeDom.Compiler.GeneratedCodeAttribute("Microsoft.VisualStudio.TestTools.UnitTestGeneration", "1.0.0.0")]
	internal class VisualStudio_Project_Samples_NestedProjectNodeAccessor : BaseAccessor
	{

		protected static Microsoft.VisualStudio.TestTools.UnitTesting.PrivateType m_privateType = new Microsoft.VisualStudio.TestTools.UnitTesting.PrivateType(typeof(NestedProjectNode));

		internal VisualStudio_Project_Samples_NestedProjectNodeAccessor(NestedProjectNode target) :
			base(target, m_privateType)
		{
		}

		internal object Object
		{
			get
			{
				object ret = ((object)(m_privateObject.GetProperty("Object")));
				return ret;
			}
		}

		internal global::System.Guid[] GetConfigurationIndependentPropertyPages()
		{
			object[] args = new object[0];
			global::System.Guid[] ret = ((global::System.Guid[])(m_privateObject.Invoke("GetConfigurationIndependentPropertyPages", new System.Type[0], args)));
			return ret;
		}

		internal global::System.Guid[] GetPriorityProjectDesignerPages()
		{
			object[] args = new object[0];
			global::System.Guid[] ret = ((global::System.Guid[])(m_privateObject.Invoke("GetPriorityProjectDesignerPages", new System.Type[0], args)));
			return ret;
		}

		internal global::System.Guid[] GetConfigurationDependentPropertyPages()
		{
			object[] args = new object[0];
			global::System.Guid[] ret = ((global::System.Guid[])(m_privateObject.Invoke("GetConfigurationDependentPropertyPages", new System.Type[0], args)));
			return ret;
		}
	}
}
