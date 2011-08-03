using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Project;
using System.ComponentModel;
using System.Runtime.InteropServices;
using System.IO;
using System.Reflection;

namespace Nemerle.VisualStudio.Project.References
{
	[CLSCompliant(false), ComVisible(true)]
	public class NemerleReferenceNodeProperties : ReferenceNodeProperties
	{
		public NemerleReferenceNodeProperties(HierarchyNode node)
			: base(node)
		{
		}

		[SRCategoryAttribute(Microsoft.VisualStudio.Project.SR.Misc)]
		[LocDisplayName(Microsoft.VisualStudio.Project.SR.CopyToLocal)]
		[SRDescriptionAttribute(Microsoft.VisualStudio.Project.SR.CopyToLocalDescription)]
		public new bool CopyToLocal
		{
			get
			{
				string copyLocal = this.GetProperty(ProjectFileConstants.Private, "False");
				if (copyLocal == null || copyLocal.Length == 0)
					return false; // false by defaulte!
				return bool.Parse(copyLocal);
			}
			set
			{
				this.SetProperty(ProjectFileConstants.Private, value.ToString());
			}
		}

		[DisplayName("Hint Path")]
		[Description("Hint Path with all expended properties.")]
		[CategoryAttribute("Misc")]
		[Browsable(true)]
		[AutomationBrowsable(true)]
		public string HintPath
		{
			get
			{
				var data = Node.ItemNode.Item.GetMetadata("HintPath");
				return data == null ? "<No hint path available>" : data.EvaluatedValue;
			}
		}

		[DisplayName("Unevaluated Hint Path")]
		[Description("Hint Path without expending properties.")]
		[CategoryAttribute("Misc")]
		[Browsable(true)]
		[AutomationBrowsable(true)]
		public string UnevaluatedHintPath
		{
			get
			{
				var data = Node.ItemNode.Item.GetMetadata("HintPath");
				return data == null ? "<No hint path available>" : data.UnevaluatedValue;
			}
		}

		[DisplayName("Include")]
		[Description("Unevaluated MSBuild 'Include' item value.")]
		[CategoryAttribute("Misc")]
		[Browsable(true)]
		[AutomationBrowsable(true)]
		public string UnevaluatedInclude
		{
			get
			{
				return Node.ItemNode.Item.UnevaluatedInclude;
			}
		}

		string GetAssemblyPath()
		{
			var path = FullPath;

			if (string.IsNullOrEmpty(path))
				return "<Assembly unresolved>";

			if (!File.Exists(path))
				return "<Assembly not exists>";

			return path;
		}


		[DisplayName("Runtime Version")]
		[Description("Version of the common language runtime (CLR) or version of Mono runtime this assembly compiled against.")]
		[CategoryAttribute("Misc")]
		[Browsable(true)]
		[AutomationBrowsable(true)]
		public string RuntimeVersion
		{
			get
			{
				var path = GetAssemblyPath();

				if (path[0] == '<')
					return path;

				Assembly assm = Assembly.ReflectionOnlyLoadFrom(path);
				return assm.ImageRuntimeVersion;
			}
		}

		[DisplayName("Version")]
		[Description("Version of referenced assembly.")]
		[CategoryAttribute("Misc")]
		[Browsable(true)]
		[AutomationBrowsable(true)]
		public string Version
		{
			get
			{
				var assmRef = Node as NemerleAssemblyReferenceNode;

				if (assmRef != null && assmRef.ResolvedAssembly != null)
					if (assmRef.ResolvedAssembly.Version == null)
					{
						var path = GetAssemblyPath();

						if (path[0] == '<')
							return path;

						Assembly assm = Assembly.ReflectionOnlyLoadFrom(path);
						return new AssemblyName(assm.FullName).Version.ToString();
					}
					else
						return assmRef.ResolvedAssembly.Version.ToString();

				return "<Assembly unresolved>";
			}
		}
		[DisplayName("Specific Version")]
		[Description("Indicates whether this reference is to a specific version of an assembly.")]
		[DefaultValue(false)]
		[Category("Misc")]
		public bool SpecificVersion
		{
			get
			{
				string property = GetProperty("SpecificVersion", null);
				var assm = (Microsoft.VisualStudio.Project.Automation.OAAssemblyReference)Node.Object;
				if (string.IsNullOrEmpty(property))
				{
					string property2 = GetProperty("Include", null);
					return property2.IndexOf("Version=", StringComparison.Ordinal) >= 0;
				}

				return bool.Parse(property);
			}

			set
			{
				var assemblyReferenceNode = (AssemblyReferenceNode)base.Node;

				if (value)
				{
					if (assemblyReferenceNode.ResolvedAssembly != null)
					{
						Node.ItemNode.Rename(assemblyReferenceNode.ResolvedAssembly.FullName);
						SetProperty("SpecificVersion", "True");
						return;
					}
				}
				else
				{
					if (assemblyReferenceNode.ResolvedAssembly != null)
					{
						Node.ItemNode.Rename(assemblyReferenceNode.ResolvedAssembly.Name);
						SetProperty("SpecificVersion", "False");
					}
				}
			}
		}
	}
}
