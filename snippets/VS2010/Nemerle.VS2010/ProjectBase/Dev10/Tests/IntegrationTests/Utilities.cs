/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.Collections.Generic;
using System.ComponentModel.Design;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Text;
using EnvDTE;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VisualStudio.Project.Automation;

namespace Microsoft.VisualStudio.Project.IntegrationTests
{
	public class Utilities
	{
		internal static string[] BadFileNames = new string[] { "#¤&%\"¤&\"%", "c:\\temp\\?&&&", "c:\\temp\\CON", "c:\\temp\\AUX", "c:\\temp\\LPT1", "c:\\temp\\LPT5", "c:\\temp\\COM1", "c:\\temp\\COM5", "c:\\temp\\PRN", "c:\\temp\\NUL", "c:\\temp\\CLOCK$", 
			"c:\\temp\\LPT0.txt","c:\\temp\\CON.txt", "c:\\temp\\AUX.txt", "c:\\temp\\LPT1.txt", "c:\\temp\\LPT5.txt", "c:\\temp\\COM1.txt", "c:\\temp\\COM5.txt", "c:\\temp\\PRN.txt", "c:\\temp\\NUL.txt", "c:\\temp\\CLOCK$.txt",
		"c:\\temp\\fieNam.??t", "c:\\temp\\FileName.#xt" };
		internal static string[] FileNamesThatShouldPass = new string[] { "MyLPT1", "LPT1My", "Auxiliary", "MYNULL", "NULLMY" };
		internal static string LongFileName = "123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789_1";

		internal static Guid NestedProjectGuid = new Guid("1CFCDD29-C8A2-4556-9C2E-5ABBD9952B20");
		internal static Guid SampleLanguageProjectGuid = new Guid("B6207E59-9758-4e9c-8AE3-AE4FACF71C82");

		public static Guid CsProjectGuid
		{
			get { return new Guid("FAE04EC0-301F-11D3-BF4B-00C04F79EFBC"); }
		}
		public static Guid VbProjectGuid
		{
			get { return new Guid("F184B08F-C81C-45F6-A57F-5ABD9991F28F"); }
		}
		public static Guid VcProjectGuid
		{
			get { return new Guid("8BC9CEB8-8B4A-11D0-8D11-00A0C91BC942"); }
		}

		public static HierarchyNode GetChildItem(ProjectNode project, string url)
		{
			HierarchyNode node = null;
			MethodInfo findChildMethod = project.GetType().GetMethod("FindChild", BindingFlags.NonPublic | BindingFlags.Instance);
			node = (HierarchyNode)findChildMethod.Invoke(project, new object[] { url });

			return node;

		}

		public static ProjectNode CreateMyNestedProject(IServiceProvider sp, DTE dte, string projectName, string destination, bool exclusive)
		{
			CreateSampleProject(dte, "Application.nestedproj", destination, projectName, NestedProjectGuid, exclusive);

			OAProject automation = Utilities.FindExtObject(sp, Utilities.NestedProjectGuid, projectName) as OAProject;
			Assert.IsNotNull(automation, "Failed to create a project using automation");

			return automation.Project;
		}

		/// <summary>
		/// Creates a new sample project
		/// </summary>
		public static void CreateSampleProject(DTE dte, string relTemplateLocation, string destination, string projectName, Guid projectType, bool exclusive)
		{
			Assert.IsNotNull(dte, "invalid value of dte");
			Assert.IsFalse(String.IsNullOrEmpty(projectName), "Project name is not set for creating a new project.");
			Assert.IsFalse(String.IsNullOrEmpty(relTemplateLocation), "The template location is invalid.");
			Assert.IsFalse(String.IsNullOrEmpty(projectName), "Invalid project name");
			Assert.IsFalse(Guid.Empty == projectType, "Invalid project type");

			//create new project from template
			Solution solution = dte.Solution;
			string templatePath = solution.get_TemplatePath(projectType.ToString("B"));
			templatePath = Path.Combine(templatePath, relTemplateLocation);
			Assert.IsTrue(File.Exists(templatePath), "Could not find templateFile");
			solution.AddFromTemplate(templatePath, destination, projectName, exclusive);
		}

		public static EnvDTE.Project FindExtObject(IServiceProvider sp, Guid guid, string projectName)
		{
			IVsSolution solutionService = sp.GetService(typeof(SVsSolution)) as IVsSolution;
			IEnumHierarchies enumHierarchies;
			uint enumFlags = (uint)(__VSENUMPROJFLAGS.EPF_ALLINSOLUTION | __VSENUMPROJFLAGS.EPF_MATCHTYPE);
			solutionService.GetProjectEnum(enumFlags, ref guid, out enumHierarchies);
			if(enumHierarchies != null)
			{
				// Loop projects found
				IVsHierarchy[] hierarchy = new IVsHierarchy[1];
				uint fetched = 0;
				while(enumHierarchies.Next(1, hierarchy, out fetched) == VSConstants.S_OK && fetched == 1)
				{
					object name;
					hierarchy[0].GetProperty((uint)VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_Name, out name);
					if(string.Compare(projectName, (string)name) == 0)
					{
						object extObject;
						hierarchy[0].GetProperty((uint)VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_ExtObject, out extObject);
						return extObject as EnvDTE.Project;
					}
				}
			}

			return null;
		}

		public static IVsHierarchy GetHierarchyOfUniqueName(IServiceProvider serviceProvider, string uniqueName)
		{
			IVsSolution solutionService = serviceProvider.GetService(typeof(SVsSolution)) as IVsSolution;
			IVsHierarchy hierarchy;
			solutionService.GetProjectOfUniqueName(uniqueName, out hierarchy);
			return hierarchy;
		}

		public static string GetResourceStringFromTheProjectAssembly(string resourceID)
		{
			Type type = typeof(ProjectNode).Assembly.GetType(typeof(ProjectNode).Namespace + ".SR", true, true);
			MethodInfo getString = type.GetMethod("GetString", BindingFlags.Static | BindingFlags.Public, null, new Type[] { typeof(string), typeof(CultureInfo) }, new ParameterModifier[] { });

			return getString.Invoke(null, new object[] { resourceID, CultureInfo.CurrentUICulture }) as string;
		}


		internal static FileNode AddFile(ProjectNode project, string fileNameWithoutExtension, string extension)
		{
			string newModelFilePath = TestUtils.GetNewFileName(project.ProjectFolder, fileNameWithoutExtension, extension);

			VSADDRESULT[] result = new VSADDRESULT[1];
			project.AddItem(VSConstants.VSITEMID_ROOT, VSADDITEMOPERATION.VSADDITEMOP_OPENFILE, String.Empty, 1, new string[] { newModelFilePath }, IntPtr.Zero, result);

			string fileName = Path.GetFileName(newModelFilePath);

			for(HierarchyNode n = project.FirstChild; n != null; n = n.NextSibling)
			{
				if(n is FileNode)
				{
					string nodeFileName = Path.GetFileName(n.GetMkDocument());

					if(nodeFileName == fileName)
					{
						return n as FileNode;
					}
				}
			}

			return null;
		}

		/// <summary>
		/// Selects all nodes of type T in the project.
		/// </summary>
		/// <typeparam name="T"></typeparam>
		/// <param name="project"></param>
		/// <returns>The id's of the selected nodes.</returns>
		internal static List<uint> SelectOrUnselectNodes<T>(ProjectNode project, bool select)
			where T : HierarchyNode
		{
			List<uint> selectedNodeIds = new List<uint>();
			IVsUIHierarchyWindow uiHierarchy = VsShellUtilities.GetUIHierarchyWindow(project.Site, HierarchyNode.SolutionExplorer);
			if(uiHierarchy == null)
			{
				return selectedNodeIds;
			}

			List<T> nodes = GetNodesOfType<T>(project);
			if(nodes.Count > 0)
			{
				foreach(T node in nodes)
				{
					uiHierarchy.ExpandItem(project, node.ID, (select) ? EXPANDFLAGS.EXPF_SelectItem : EXPANDFLAGS.EXPF_UnSelectItem);
					selectedNodeIds.Add(node.ID);
				}
			}

			return selectedNodeIds;
		}

		/// <summary>
		/// Selects all nodes of type T in the project.
		/// </summary>
		/// <typeparam name="T"></typeparam>
		/// <param name="project"></param>
		/// <returns>The id's of the selected nodes.</returns>
		internal static T SelectAnyNodeOfType<T>(ProjectNode project)
			where T : HierarchyNode
		{
			IVsUIHierarchyWindow uiHierarchy = VsShellUtilities.GetUIHierarchyWindow(project.Site, HierarchyNode.SolutionExplorer);
			if(uiHierarchy == null)
			{
				return null;
			}

			List<T> nodes = GetNodesOfType<T>(project);
			if(nodes.Count > 0)
			{
				uiHierarchy.ExpandItem(project, nodes[0].ID, EXPANDFLAGS.EXPF_SelectItem);
				return nodes[0];
			}

			return null;
		}

		/// <summary>
		/// Performs the EXPANDFLAGS action on a node with nodeID.
		/// </summary>
		internal static void ManipulateNode(ProjectNode project, uint nodeID, EXPANDFLAGS flag)
		{
			IVsUIHierarchyWindow uiHierarchy = VsShellUtilities.GetUIHierarchyWindow(project.Site, HierarchyNode.SolutionExplorer);
			if(uiHierarchy == null)
			{
				return;
			}

			uiHierarchy.ExpandItem(project, nodeID, flag);
		}

		/// <summary>
		/// Get the nodes of type T 
		/// </summary>
		/// <typeparam name="T"></typeparam>
		/// <param name="parent"></param>
		/// <param name="T"></param>
		/// <returns></returns>
		internal static List<T> GetNodesOfType<T>(HierarchyNode parent)
			where T : HierarchyNode
		{
			List<T> nodes = new List<T>();

			FindNodesOfType<T>(parent, nodes);

			return nodes;
		}

		internal static ProjectReferenceNode AddProjectReference(ProjectNode project, ProjectNode projectReference)
		{
			VSCOMPONENTSELECTORDATA selectorData = new VSCOMPONENTSELECTORDATA();

			selectorData.bstrFile = projectReference.ProjectFolder;
			IVsSolution solution = (IVsSolution)project.Site.GetService(typeof(IVsSolution));
			solution.GetProjrefOfItem(projectReference, VSConstants.VSITEMID_ROOT, out selectorData.bstrProjRef);
			selectorData.bstrTitle = projectReference.Caption;
			selectorData.type = VSCOMPONENTTYPE.VSCOMPONENTTYPE_Project;

			// Get the ReferenceContainerNode for this project.
			IReferenceContainer container = project.GetReferenceContainer();
			container.AddReferenceFromSelectorData(selectorData);

			MethodInfo mi = typeof(ReferenceContainerNode).GetMethod("FindChild", BindingFlags.NonPublic | BindingFlags.Instance);

			return mi.Invoke(container, new object[] { projectReference.GetMkDocument() }) as ProjectReferenceNode;
		}

		internal static AssemblyReferenceNode AddAssemblyReference(ProjectNode project, string assemblyReference)
		{
			VSCOMPONENTSELECTORDATA selectorData = new VSCOMPONENTSELECTORDATA();

			selectorData.bstrFile = assemblyReference;
			selectorData.bstrTitle = Path.GetFileNameWithoutExtension(assemblyReference);
			selectorData.type = VSCOMPONENTTYPE.VSCOMPONENTTYPE_File;

			// Get the ReferenceContainerNode for this project.
			IReferenceContainer container = project.GetReferenceContainer();
			container.AddReferenceFromSelectorData(selectorData);

			MethodInfo mi = typeof(ReferenceContainerNode).GetMethod("FindChild", BindingFlags.NonPublic | BindingFlags.Instance);

			return mi.Invoke(container, new object[] { assemblyReference }) as AssemblyReferenceNode;
		}

		internal static ComReferenceNode AddComReference(ProjectNode project, VSCOMPONENTSELECTORDATA selectorData)
		{

			// Get the ReferenceContainerNode for this project.
			IReferenceContainer container = project.GetReferenceContainer();
			container.AddReferenceFromSelectorData(selectorData);

			// Now find the refererence added.
			ReferenceContainerNode containerNode = container as ReferenceContainerNode;
			for(HierarchyNode n = containerNode.FirstChild; n != null; n = n.NextSibling)
			{
				if(n is ComReferenceNode)
				{
					ComReferenceNode refererenceNode = n as ComReferenceNode;

					// We check if the name is the same and the type guid is the same
					if(refererenceNode.TypeGuid == selectorData.guidTypeLibrary && String.Compare(refererenceNode.Caption, selectorData.bstrTitle, StringComparison.OrdinalIgnoreCase) == 0)
					{
						return refererenceNode;
					}
				}
			}

			throw new InvalidOperationException("The Com Refererence added cannot be found");
		}

		/// <summary>
		/// Retrives the nested hierarchy.
		/// </summary>
		/// <param name="project"></param>
		/// <returns></returns>
		internal static IVsHierarchy GetNestedHierarchy(ProjectNode node, string nestedProjectName)
		{
			EnvDTE.Project project = node.GetAutomationObject() as EnvDTE.Project;
			EnvDTE.ProjectItem nestedProjectItem = project.ProjectItems.Item(nestedProjectName);
			EnvDTE.Project nestedProject = nestedProjectItem.SubProject;

			IServiceProvider serviceProvider = node.Site;
			IVsSolution ivsSolution = (IVsSolution)serviceProvider.GetService(typeof(IVsSolution));
			IVsHierarchy nestedHierarchy = null;
			ErrorHandler.ThrowOnFailure(ivsSolution.GetProjectOfUniqueName(nestedProject.UniqueName, out nestedHierarchy));

			return nestedHierarchy;

		}

		internal static EnvDTE.Project GetAutomationObject(IVsHierarchy hierarchy)
		{
			object extensibility;
			ErrorHandler.ThrowOnFailure(hierarchy.GetProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_ExtObject, out extensibility));
			return extensibility as EnvDTE.Project;
		}

		/// <summary>
		/// Invokes SetEditLabel and checkes if the exception with the error message is caught.
		/// </summary>
		internal static bool CheckForSetEditLabelBadFileName<T>(HierarchyNode node, string badFileName, string expectedMessage)
					where T : Exception
		{
			bool badFileNameCaught = false;
			IVsExtensibility extensibility = node.GetService(typeof(IVsExtensibility)) as IVsExtensibility;
			extensibility.EnterAutomationFunction();
			try
			{
				node.SetEditLabel(badFileName);
			}
			catch(Exception e)
			{
				if((e is T) && (e.Message == expectedMessage || e.Message.Contains(expectedMessage)))
				{
					badFileNameCaught = true;
				}
			}
			finally
			{
				extensibility.ExitAutomationFunction();
			}

			return badFileNameCaught;
		}

		private static void FindNodesOfType<T>(HierarchyNode parent, List<T> nodes)
			where T : HierarchyNode
		{
			for(HierarchyNode n = parent.FirstChild; n != null; n = n.NextSibling)
			{
				if(n is T)
				{
					T nodeAsT = (T)n;
					nodes.Add(nodeAsT);
				}

				FindNodesOfType<T>(n, nodes);
			}
		}

		internal static DirectoryInfo GetWinDir()
		{
			// Get the value of the environment variable windir
			string windir = System.Environment.GetEnvironmentVariable("windir");
			return new DirectoryInfo(windir);
		}

		/// <summary>
		/// Create a new folderNode as a child node to a containernode (folder or project)
		/// </summary>
		/// <param name="project"></param>
		/// <param name="folderPath"></param>
		/// <returns></returns>
		internal static FolderNode CreateFolder(ProjectNode project, string folderPath, HierarchyNode containerNode)
		{
			MethodInfo createFolder = typeof(ProjectNode).GetMethod("CreateFolderNode", BindingFlags.NonPublic | BindingFlags.Instance, null, new Type[] { typeof(string) }, null);
			FolderNode folderNode = createFolder.Invoke(project, new object[] { folderPath }) as FolderNode;
			containerNode.AddChild(folderNode);
			//Create Directory associated to this FolderNode
			folderNode.CreateDirectory();

			return folderNode;
		}

		/// <summary>
		/// Returns the full path of the assembly a project builds.
		/// </summary>
		/// <param name="project">The project whose build asembly to retrive.</param>
		/// <returns>The full path of the assembly this project builds.</returns>
		internal static string GetOutputPath(EnvDTE.Project automationProjectObject)
		{
			// Get the configuration manager from the project.
			EnvDTE.ConfigurationManager confManager = automationProjectObject.ConfigurationManager;
			if(null == confManager)
			{
				return null;
			}

			// Get the active configuration.
			EnvDTE.Configuration config = confManager.ActiveConfiguration;
			if(null == config)
			{
				return null;
			}

			// Get the output path for the current configuration.
			EnvDTE.Property outputPathProperty = config.Properties.Item("OutputPath");
			if(null == outputPathProperty)
			{
				return null;
			}

			string outputPath = outputPathProperty.Value.ToString();

			// Ususally the output path is relative to the project path, but it is possible
			// to set it as an absolute path. If it is not absolute, then evaluate its value
			// based on the project directory.
			if(!System.IO.Path.IsPathRooted(outputPath))
			{
				string projectDir = System.IO.Path.GetDirectoryName(automationProjectObject.FullName);
				outputPath = System.IO.Path.Combine(projectDir, outputPath);
			}

			// Now get the name of the assembly from the project.
			// Some project system throw if the property does not exist. We expect an ArgumentException.
			EnvDTE.Property assemblyNameProperty = null;
			try
			{
				assemblyNameProperty = automationProjectObject.Properties.Item("OutputFileName");
			}
			catch(ArgumentException)
			{
			}

			if(null == assemblyNameProperty)
			{
				return null;
			}
			else
			{
				outputPath = System.IO.Path.Combine(outputPath, assemblyNameProperty.Value.ToString());
			}

			// build the full path adding the name of the assembly to the output path.

			return outputPath;
		}

		/// <summary>
		/// Creates a new C# class library project.
		/// </summary>
		/// <param name="projectName">Name of new project.</param>
		/// <returns>New project.</returns>
		public static EnvDTE.Project CreateCSharpLibraryProject(DTE dte, string projectName)
		{
			// - get template file
			string templateFile = ((EnvDTE80.Solution2)dte.Solution).GetProjectTemplate("ClassLibrary.zip", "CSharp");

			// - project name and directory
			string solutionDirectory = Directory.GetParent(dte.Solution.FullName).FullName;
			string projectDirectory = TestUtils.GetNewDirectoryName(solutionDirectory, projectName);

			// - create the C# project
			EnvDTE.Project returnedProject = dte.Solution.AddFromTemplate(templateFile, projectDirectory, projectName, false);

			// - find the newly created project (for some reason, AddFromTemplate doesn't return the project)
			string simpleProjectDirectory = Path.GetFileName(projectDirectory);
			string relativeProjectFileName = simpleProjectDirectory + @"\" + projectName + ".csproj";

			EnvDTE.Project csharpProject = null;
			try
			{
				csharpProject = dte.Solution.Projects.Item(relativeProjectFileName);
			}
			catch(ArgumentException)
			{
			}
			Assert.IsTrue(csharpProject != null, string.Format("Failed to find newly created project. Relative project name: {0}. Solution.Projects.Count: {1}", relativeProjectFileName, dte.Solution.Projects.Count));
			return csharpProject;
		}

		/// <summary>
		/// Get reference to IVsOutputWindowPane interface from pane guid. The method will create the pane if it is not already created.
		/// </summary>
		/// <param name="guidPane">A guid for the pane.</param>
		/// <param name="paneName">The name of the pane.</param>
		/// <param name="visible">Set the visibility state of the pane.</param>
		/// <param name="clearWithSolution">Should the pane be cleared with solution. It is used if the pane will be created by this method.</param>
		/// <returns>A reference to an IVsOutputWindowPane interface.</returns>
		public static IVsOutputWindowPane GetOutputWindowpane(IServiceProvider serviceProvider, Guid guidPane, string paneName, bool visible, bool clearWithSolution)
		{
			IVsOutputWindow outputWindow = serviceProvider.GetService(typeof(IVsOutputWindow)) as IVsOutputWindow;
			if(outputWindow == null)
			{
				throw new InvalidOperationException("Could not get the IVsOutputWindow");
			}

			IVsOutputWindowPane outputWindowPane = null;
			int hr = outputWindow.GetPane(ref guidPane, out outputWindowPane);

			if(ErrorHandler.Failed(hr) && outputWindowPane == null)
			{
				if(ErrorHandler.Succeeded(outputWindow.CreatePane(ref guidPane, paneName, visible ? 1 : 0, clearWithSolution ? 1 : 0)))
				{
					outputWindow.GetPane(ref guidPane, out outputWindowPane);
				}
			}
			else
			{
				if(!visible)
				{
					outputWindowPane.Hide();
				}
				else
				{
					outputWindowPane.Activate();
				}
			}

			return outputWindowPane;
		}
	}

	/// <summary>
	/// </summary>
	public static partial class TestUtils
	{
		#region Constants
		private const int cmdIdAddFirst = 4609; //0x1201;
		#endregion

		#region Methods: Handling embedded resources
		/// <summary>
		/// Gets the embedded file identified by the resource name, and converts the
		/// file into a string.
		/// </summary>
		/// <param name="resourceName">In VS, is DefaultNamespace.FileName?</param>
		/// <returns></returns>
		public static string GetEmbeddedStringResource(Assembly assembly, string resourceName)
		{
			string result = null;

			// Use the .NET procedure for loading a file embedded in the assembly
			Stream stream = assembly.GetManifestResourceStream(resourceName);
			if(stream != null)
			{
				// Convert bytes to string
				byte[] fileContentsAsBytes = new byte[stream.Length];
				stream.Read(fileContentsAsBytes, 0, (int)stream.Length);
				result = Encoding.Default.GetString(fileContentsAsBytes);
			}
			else
			{
				// Embedded resource not found - list available resources
				Debug.WriteLine("Unable to find the embedded resource file '" + resourceName + "'.");
				Debug.WriteLine("  Available resources:");
				foreach(string aResourceName in assembly.GetManifestResourceNames())
				{
					Debug.WriteLine("    " + aResourceName);
				}
			}

			return result;
		}
		/// <summary>
		/// 
		/// </summary>
		/// <param name="embeddedResourceName"></param>
		/// <param name="baseFileName"></param>
		/// <param name="fileExtension"></param>
		/// <returns></returns>
		public static void WriteEmbeddedResourceToFile(Assembly assembly, string embeddedResourceName, string fileName)
		{
			// Get file contents
			string fileContents = GetEmbeddedStringResource(assembly, embeddedResourceName);
			if(fileContents == null)
				throw new ApplicationException("Failed to get embedded resource '" + embeddedResourceName + "' from assembly '" + assembly.FullName);

			// Write to file
			StreamWriter sw = new StreamWriter(fileName);
			sw.Write(fileContents);
			sw.Close();
		}

		/// <summary>
		/// Writes an embedded resource to a file.
		/// </summary>
		/// <param name="assembly">The name of the assembly that the embedded resource is defined.</param>
		/// <param name="embeddedResourceName">The name of the embedded resource.</param>
		/// <param name="fileName">The file to write the embedded resource's content.</param>
		public static void WriteEmbeddedResourceToBinaryFile(Assembly assembly, string embeddedResourceName, string fileName)
		{
			// Get file contents
			Stream stream = assembly.GetManifestResourceStream(embeddedResourceName);
			if(stream == null)
				throw new InvalidOperationException("Failed to get embedded resource '" + embeddedResourceName + "' from assembly '" + assembly.FullName);

			// Write to file
			BinaryWriter sw = null;
			FileStream fs = null;
			try
			{
				byte[] fileContentsAsBytes = new byte[stream.Length];
				stream.Read(fileContentsAsBytes, 0, (int)stream.Length);

				FileMode mode = FileMode.CreateNew;
				if(File.Exists(fileName))
				{
					mode = FileMode.Truncate;
				}

				fs = new FileStream(fileName, mode);

				sw = new BinaryWriter(fs);
				sw.Write(fileContentsAsBytes);
			}
			finally
			{
				if(fs != null)
				{
					fs.Close();
				}
				if(sw != null)
				{
					sw.Close();
				}
			}
		}

		#endregion

		#region Methods: Handling temporary files and directories
		/// <summary>
		/// Returns the first available file name on the form
		///   [baseFileName]i.[extension]
		/// where [i] starts at 1 and increases until there is an available file name
		/// in the given directory. Also creates an empty file with that name to mark
		/// that file as occupied.
		/// </summary>
		/// <param name="directory">Directory that the file should live in.</param>
		/// <param name="baseFileName"></param>
		/// <param name="extension">may be null, in which case the .[extension] part
		/// is not added.</param>
		/// <returns>Full file name.</returns>
		public static string GetNewFileName(string directory, string baseFileName, string extension)
		{
			// Get the new file name
			string fileName = GetNewFileOrDirectoryNameWithoutCreatingAnything(directory, baseFileName, extension);

			// Create an empty file to mark it as taken
			StreamWriter sw = new StreamWriter(fileName);

			sw.Write("");
			sw.Close();
			return fileName;
		}
		/// <summary>
		/// Returns the first available directory name on the form
		///   [baseDirectoryName]i
		/// where [i] starts at 1 and increases until there is an available directory name
		/// in the given directory. Also creates the directory to mark it as occupied.
		/// </summary>
		/// <param name="directory">Directory that the file should live in.</param>
		/// <param name="baseDirectoryName"></param>
		/// <returns>Full directory name.</returns>
		public static string GetNewDirectoryName(string directory, string baseDirectoryName)
		{
			// Get the new file name
			string directoryName = GetNewFileOrDirectoryNameWithoutCreatingAnything(directory, baseDirectoryName, null);

			// Create an empty directory to make it as occupied
			Directory.CreateDirectory(directoryName);

			return directoryName;
		}

		/// <summary>
		/// 
		/// </summary>
		/// <param name="directory"></param>
		/// <param name="baseFileName"></param>
		/// <param name="extension"></param>
		/// <returns></returns>
		private static string GetNewFileOrDirectoryNameWithoutCreatingAnything(string directory, string baseFileName, string extension)
		{
			// - get a file name that we can use
			string fileName;
			int i = 1;

			string fullFileName = null;
			while(true)
			{
				// construct next file name
				fileName = baseFileName + i;
				if(extension != null)
					fileName += '.' + extension;

				// check if that file exists in the directory
				fullFileName = Path.Combine(directory, fileName);

				if(!File.Exists(fullFileName) && !Directory.Exists(fullFileName))
					break;
				else
					i++;
			}

			return fullFileName;
		}
		#endregion

		#region Methods: Handling solutions
		/// <summary>
		/// Closes the currently open solution (if any), and creates a new solution with the given name.
		/// </summary>
		/// <param name="solutionName">Name of new solution.</param>
		public static void CreateEmptySolution(IServiceProvider serviceProvider, string destination, string solutionName)
		{
			IVsSolution solutionService = CloseCurrentSolution(serviceProvider);

			// Create solution
			solutionService.CreateSolution(destination, solutionName, (uint)__VSCREATESOLUTIONFLAGS.CSF_SILENT);
			solutionService.SaveSolutionElement((uint)__VSSLNSAVEOPTIONS.SLNSAVEOPT_ForceSave, null, 0);
		}

		/// <summary>
		/// Closes the currently open solution
		/// </summary>
		public static IVsSolution CloseCurrentSolution(IServiceProvider serviceProvider)
		{
			// Get solution service
			IVsSolution solutionService = (IVsSolution)serviceProvider.GetService(typeof(IVsSolution));

			// Close already open solution
			solutionService.CloseSolutionElement((uint)__VSSLNSAVEOPTIONS.SLNSAVEOPT_NoSave, null, 0);

			return solutionService;
		}

		public static void ForceSaveSolution(IServiceProvider serviceProvider)
		{
			// Get solution service
			IVsSolution solutionService = (IVsSolution)serviceProvider.GetService(typeof(IVsSolution));

			// Force-save the solution
			solutionService.SaveSolutionElement((uint)__VSSLNSAVEOPTIONS.SLNSAVEOPT_ForceSave, null, 0);
		}
		#endregion

		#region Methods: Handling project items
		/// <summary>
		/// Saves the file for an open document.
		/// </summary>
		/// <param name="fileName"></param>
		public static void SaveDocument(IServiceProvider serviceProvider, string fileName)
		{
			// Get document cookie and hierarchy for the file
			IVsRunningDocumentTable runningDocumentTableService = (IVsRunningDocumentTable)serviceProvider.GetService(typeof(IVsRunningDocumentTable));
			uint docCookie;
			IntPtr docData;
			IVsHierarchy hierarchy;
			uint itemId;
			runningDocumentTableService.FindAndLockDocument(
				(uint)Microsoft.VisualStudio.Shell.Interop._VSRDTFLAGS.RDT_NoLock,
				fileName,
				out hierarchy,
				out itemId,
				out docData,
				out docCookie);

			// Save the document
			IVsSolution solutionService = (IVsSolution)serviceProvider.GetService(typeof(IVsSolution));
			solutionService.SaveSolutionElement((uint)__VSSLNSAVEOPTIONS.SLNSAVEOPT_ForceSave, hierarchy, docCookie);
		}
		/// <summary>
		/// 
		/// </summary>
		/// <param name="fullFileName"></param>
		public static void CloseInEditorWithoutSaving(IServiceProvider serviceProvider, string fullFileName)
		{
			#region Input validation
			Debug.Assert(serviceProvider != null, "ServiceProvider cannot be null");
			Debug.Assert(fullFileName != null && fullFileName.Length > 0, "fullFileName cannot be empty");
			#endregion

			// Get the RDT service
			IVsRunningDocumentTable runningDocumentTableService = (IVsRunningDocumentTable)serviceProvider.GetService(typeof(IVsRunningDocumentTable));
			Debug.Assert(runningDocumentTableService != null, "Failed to get the Running Document Table Service");

			// Get our document cookie and hierarchy for the file
			uint docCookie;
			IntPtr docData;
			IVsHierarchy hierarchy;
			uint itemId;
			runningDocumentTableService.FindAndLockDocument(
				(uint)Microsoft.VisualStudio.Shell.Interop._VSRDTFLAGS.RDT_NoLock,
				fullFileName,
				out hierarchy,
				out itemId,
				out docData,
				out docCookie);

			// Get the SolutionService
			IVsSolution solutionService = serviceProvider.GetService(typeof(IVsSolution)) as IVsSolution;
			Debug.Assert(solutionService != null, "Failed to get IVsSolution service");

			// Close the document
			solutionService.CloseSolutionElement(
				(uint)__VSSLNSAVEOPTIONS.SLNSAVEOPT_NoSave,
				hierarchy,
				docCookie);
		}
		#endregion

		#region Methods: Handling commands
		/// <summary>
		/// Executes a Command (menu item) in the given context
		/// </summary>
		/// <param name="dte"></param>
		/// <param name="cmd"></param>
		public static void ExecuteCommand(EnvDTE.DTE dte, CommandID cmd)
		{
			object Customin = null;
			object Customout = null;
			string guidString = cmd.Guid.ToString("B").ToUpper();
			int cmdId = cmd.ID;
			dte.Commands.Raise(guidString, cmdId, ref Customin, ref Customout);
		}
		#endregion

		#region Methods: Loading packages
		public static IVsPackage LoadPackage(IServiceProvider serviceProvider, Guid packageGuid)
		{
			IVsShell shellService = (IVsShell)serviceProvider.GetService(typeof(SVsShell));
			IVsPackage package;
			shellService.LoadPackage(ref packageGuid, out package);
			Assert.IsNotNull(package, "Failed to load package");
			return package;
		}
		#endregion
	}
}
