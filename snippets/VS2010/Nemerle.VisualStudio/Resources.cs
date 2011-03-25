using System;
using System.ComponentModel;
using System.Diagnostics;
using System.Globalization;
using System.Resources;
using System.Threading;

namespace Nemerle.VisualStudio
{
	[AttributeUsage(AttributeTargets.All)]
	internal sealed class SRDescriptionAttribute : DescriptionAttribute
	{
		bool replaced = false;

		public SRDescriptionAttribute(string description) : base(description) {}

		public override string Description
		{
			get
			{
				if (!replaced)
				{
					replaced = true;
					DescriptionValue = SR.GetString(base.Description);
				}
				return base.Description;
			}
		}
	}

		[AttributeUsage(AttributeTargets.All)]
		sealed class SRDisplayNameAttribute : DisplayNameAttribute
		{
			string name;

			public SRDisplayNameAttribute(string name)
			{
				this.name = name;
			}

			public override string DisplayName
			{
				get
				{
					string result = SR.GetString(name);

					if (result == null)
					{
						Debug.Assert(false, "String resource '" + name + "' is missing");
						result = name;
					}

					return result;
				}
			}
		}


	[AttributeUsage(AttributeTargets.All)]
	internal sealed class SRCategoryAttribute : CategoryAttribute
	{
		public SRCategoryAttribute(string category) : base(category) {}

		protected override string GetLocalizedString(string value)
		{
			return SR.GetString(value);
		}
	}

	internal sealed class SR
	{
		internal const string Application                       = "Application";
		internal const string ApplicationIcon                   = "ApplicationIcon";
		internal const string ApplicationIconDescription        = "ApplicationIconDescription";
		internal const string AssemblyName                      = "AssemblyName";
		internal const string AssemblyNameDescription           = "AssemblyNameDescription";
		internal const string BuildCaption                      = "BuildCaption";
		internal const string CmdArgs                           = "CmdArgs";
		internal const string CmdArgsDescription                = "CmdArgsDescription";
		internal const string DebugCaption                      = "DebugCaption";
		internal const string DefaultNamespace                  = "DefaultNamespace";
		internal const string DefaultNamespaceDescription       = "DefaultNamespaceDescription";
		internal const string DefineConstants                   = "DefineConstants";
		internal const string DefineConstantsDescription        = "DefineConstantsDescription";
		internal const string DecumentationFile                 = "DecumentationFile";
		internal const string DecumentationFileDescription      = "DecumentationFileDescription";
		internal const string GeneralCaption                    = "GeneralCaption";
		internal const string OutputFile                        = "OutputFile";
		internal const string OutputFileDescription             = "OutputFileDescription";
		internal const string OutputPathFinalValue              = "OutputPathFinalValue";
		internal const string OutputType                        = "OutputType";
		internal const string OutputTypeDescription             = "OutputTypeDescription";
		internal const string Project                           = "Project";
		internal const string ProjectFile                       = "ProjectFile";
		internal const string ProjectFileDescription            = "ProjectFileDescription";
		internal const string ProjectFileExtensionFilter        = "ProjectFileExtensionFilter";
		internal const string ProjectFolder                     = "ProjectFolder";
		internal const string ProjectFolderDescription          = "ProjectFolderDescription";
		internal const string StartProgram                      = "StartProgram";
		internal const string StartProgramDescription           = "StartProgramDescription";
		internal const string StartupObject                     = "StartupObject";
		internal const string StartupObjectDescription          = "StartupObjectDescription";
		internal const string TargetPlatform                    = "TargetPlatform";
		internal const string TargetPlatformDescription         = "TargetPlatformDescription";
		internal const string TargetPlatformLocation            = "TargetPlatformLocation";
		internal const string TargetPlatformLocationDescription = "TargetPlatformLocationDescription";
		internal const string WorkingDirectory                  = "WorkingDirectory";
		internal const string WorkingDirectoryDescription       = "WorkingDirectoryDescription";
		internal const string DebugSymbols                      = "DebugSymbols";
		internal const string DebugSymbolsDescription           = "DebugSymbolsDescription";

		static SR     loader = null;
		static object s_InternalSyncObject;

		ResourceManager resources;

		static object InternalSyncObject
		{
			get
			{
				if (s_InternalSyncObject == null)
				{
					object o = new object();
					Interlocked.CompareExchange(ref s_InternalSyncObject, o, null);
				}

				return s_InternalSyncObject;
			}
		}

		internal SR()
		{
			resources = Nemerle.VisualStudio.Resources.ResourceManager;
		}

		static SR GetLoader()
		{
			if (loader == null)
			{
				lock (InternalSyncObject)
				{
					if (loader == null)
						loader = new SR();
				}
			}

			return loader;
		}

		static CultureInfo Culture
		{
			get
			{
				return null
				/*use ResourceManager default, CultureInfo.CurrentUICulture*/;
			}
		}

		public static ResourceManager Resources
		{
			 get { return GetLoader().resources; }
		}

		public static string GetString(string name, params object[] args)
		{
			SR sys = GetLoader();

			if (sys == null)
				return null;

			string res = sys.resources.GetString(name, Culture);

			if (args != null && args.Length > 0)
				return String.Format(CultureInfo.CurrentCulture, res, args);
			else
				return res;
		}

		public static string GetString(string name)
		{
			SR sys = GetLoader();
			return sys == null? null: sys.resources.GetString(name, Culture);
		}

		public static object GetObject(string name)
		{
			SR sys = GetLoader();
			return sys == null? null: sys.resources.GetObject(name, Culture);
		}
	}
}