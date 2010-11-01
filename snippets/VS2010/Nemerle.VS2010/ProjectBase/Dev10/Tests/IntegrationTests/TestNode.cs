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
using System.Diagnostics;
using System.IO;
using System.Reflection;
using EnvDTE;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VSSDK.Tools.VsIdeTesting;
using Microsoft.VisualStudio.Project.Automation;

namespace Microsoft.VisualStudio.Project.IntegrationTests
{
	/// <summary>
	/// Unit testt targeting nodes.
	/// </summary>
	[TestClass]
	public class TestNode : BaseTest
	{
		[TestMethod()]
		public void TestCreatingFileNode()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				string newFileName = TestUtils.GetNewFileName(project.ProjectFolder, "test", "cs");
				ProjectElement item = project.CreateMsBuildFileItem(newFileName, "Compile");
				FileNode node = project.CreateFileNode(item);
				Assert.IsNotNull(node);
			});
		}

		/// <summary>
		/// Verify the NodeProperties
		/// </summary>
		[TestMethod()]
		public void TestNodeProperties()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				string newFileName = TestUtils.GetNewFileName(project.ProjectFolder, "test", "cs");
				ProjectElement item = project.CreateMsBuildFileItem(newFileName, "Content");
				FileNode node = project.CreateFileNode(item);
				FileNodeProperties props = node.NodeProperties as FileNodeProperties;
				Assert.IsNotNull(props);

				// Test the build action.
				Assert.IsTrue(props.BuildAction == BuildAction.Content, "The BuildAction build action should be set to compile model");
				props.BuildAction = BuildAction.Compile;
				Assert.IsTrue(props.BuildAction == BuildAction.Compile, "BuildAction has not been set correctly in the project file");
				props.BuildAction = BuildAction.EmbeddedResource;
				Assert.IsTrue(props.BuildAction == BuildAction.EmbeddedResource, "BuildAction has not been set correctly in the project file");
				props.BuildAction = BuildAction.None;
				Assert.IsTrue(props.BuildAction == BuildAction.None, "BuildAction has not been set correctly in the project file");
			});
		}

		[TestMethod()]
		public void TestRenameOfFileNode()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				//Create a project and get the first filenode
				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				FileNode fileNode = GetFirstFileNode(project);

				// Try to see if case only change succeeds
				string fileNameWithCaseOnlyChange = fileNode.FileName.ToUpper();
				fileNode.SetEditLabel(fileNameWithCaseOnlyChange);
				Assert.IsTrue((String.Compare(fileNode.FileName, fileNameWithCaseOnlyChange, StringComparison.Ordinal) == 0), "SetEditLabel failed since the file comparison test failed");

				string oldUrl = fileNode.Url;
				string goodFileName = "test.cs";

				fileNode.SetEditLabel(goodFileName);

				Assert.IsTrue(NativeMethods.IsSamePath(fileNode.Url, Path.Combine(project.ProjectFolder, goodFileName)), "SetEditLabel failed since the Url test failed");
				Assert.IsTrue((String.Compare(fileNode.FileName, goodFileName, StringComparison.Ordinal) == 0), "SetEditLabel failed since the file comparison test failed");
			});
		}

		[TestMethod()]
		public void TestSingleFileGeneratorOnNodes()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				//Create a project and get the first filenode
				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				MethodInfo mi = typeof(FileNode).GetMethod("CreateSingleFileGenerator", BindingFlags.Instance | BindingFlags.NonPublic);

				List<FileNode> nodes = Utilities.GetNodesOfType<FileNode>(project);
				foreach(FileNode node in nodes)
				{
					ISingleFileGenerator generator = mi.Invoke(node, new object[] { }) as ISingleFileGenerator;
					string extension = Path.GetExtension(node.GetMkDocument());
					if(String.Compare(extension, ".moxl", StringComparison.OrdinalIgnoreCase) == 0)
					{
						Assert.IsNull(generator, "There should be no single file generators defined for a moxl file");
						Assert.IsFalse(node.NodeProperties is SingleFileGeneratorNodeProperties, "If no generators are supported then the properties should not be of type SingleFileGeneratorNodeProperties");
					}
					else if(String.Compare(extension, ".cs", StringComparison.OrdinalIgnoreCase) == 0)
					{
						Assert.IsNotNull(generator, "There should be a single file generator defined for a cs file");
						Assert.IsTrue(node.NodeProperties is SingleFileGeneratorNodeProperties, "The properties for a node supporting single file generators should be of type SingleFileGeneratorNodeProperties");
					}
				}
			});
		}

		/// <summary>
		/// Verify that certain rename condition are not allowed with bad filename
		/// </summary>
		[TestMethod()]
		public void TestRenameWithBadFileName()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				FileNode fileNode = null;
				List<FileNode> nodes = Utilities.GetNodesOfType<FileNode>(project);

				if(nodes.Count > 0)
				{
					fileNode = nodes[0];
				}

				string errorMessage = Utilities.GetResourceStringFromTheProjectAssembly("ErrorInvalidFileName");

				foreach(string newBadFileName in Utilities.BadFileNames)
				{
					Assert.IsTrue(Utilities.CheckForSetEditLabelBadFileName<InvalidOperationException>(fileNode, newBadFileName, errorMessage), "The file named " + newBadFileName + " could be saved as");
				}

				string badFileName = "  ";
				Assert.IsTrue(Utilities.CheckForSetEditLabelBadFileName<InvalidOperationException>(fileNode, badFileName, errorMessage), "The file named " + badFileName + " could be saved as");

				badFileName = "..\\" + fileNode.FileName;
				Assert.IsTrue(Utilities.CheckForSetEditLabelBadFileName<InvalidOperationException>(fileNode, badFileName, errorMessage), "The file named " + badFileName + " could be saved as");

				badFileName = "....";
				Assert.IsTrue(Utilities.CheckForSetEditLabelBadFileName<InvalidOperationException>(fileNode, badFileName, errorMessage), "The file named " + badFileName + " could be saved as");

				errorMessage = String.Format(System.Globalization.CultureInfo.CurrentCulture, Utilities.GetResourceStringFromTheProjectAssembly("PathTooLong"), Utilities.LongFileName);
				Assert.IsTrue(Utilities.CheckForSetEditLabelBadFileName<InvalidOperationException>(fileNode, Utilities.LongFileName, errorMessage), "The file named " + Utilities.LongFileName + " could be saved");
			});
		}

		#region Test SaveAs
		/// <summary>
		/// Test SaveAs moving document into different location
		/// </summary>
		[TestMethod()]
		public void TestSaveAsDifferentLocation()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				//Create a project and get the first filenode
				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				FileNode fileNode = GetFirstFileNode(project);

				//Create folder
				string folderPath = Path.Combine(project.ProjectFolder, "Test");
				Assert.IsFalse(File.Exists(folderPath));
				FolderNode folderNode = Utilities.CreateFolder(project, folderPath, project);

				//Add new item in folder
				string newFileName = TestUtils.GetNewFileName(folderNode.GetMkDocument(), "test", "cs");
				ProjectItem item = ((OAFolderItem)folderNode.GetAutomationObject()).ProjectItems.AddFromFile(newFileName);
				Assert.IsNotNull(item, "Could not get the project item for the file just added");

				//open the item before we can do the SaveAs op
				Window window = item.Open(EnvDTE.Constants.vsViewKindPrimary);
				Assert.IsNotNull(window, "Did not get a reference to the window for the file just opened");
				if(!window.Visible)
					window.Visible = true;

				//SaveAs into project folder
				string updatedFileName = Path.Combine(project.ProjectFolder, item.Name);
				Trace.WriteLine(updatedFileName);
				item.SaveAs(updatedFileName);

				//Verify Caption of open file
				Assert.IsTrue(string.Compare(window.Caption, item.Name, true) == 0, "Caption of window does not match the new filename");

				//Verify full path to document
				Assert.IsTrue(string.Compare(window.Document.FullName, updatedFileName, true) == 0, "FullName of document is not as expected");
			});
		}

		/// <summary>
		/// Test SaveAs leaving document in same location
		/// </summary>
		[TestMethod()]
		public void TestSaveAsSameLocation()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				//Create a project and get the first filenode
				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				FileNode fileNode = GetFirstFileNode(project);

				//open the item before we can do the SaveAs op
				ProjectItem item = ((OAFileItem)fileNode.GetAutomationObject());
				Window window = item.Open(EnvDTE.Constants.vsViewKindPrimary);
				Assert.IsNotNull(window, "Did not get a reference to the window for the file just opened");
				if(!window.Visible)
					window.Visible = true;

				//SaveAs
				string newNameOfFile = "Test.cs";
				string updatedFileName = Path.Combine(project.ProjectFolder, newNameOfFile);
				item.SaveAs(updatedFileName);

				//Verify Caption in window of the file renamed
				Assert.IsTrue(string.Compare(window.Caption, newNameOfFile, true) == 0, "Caption of window does not match the new filename");

				//Verify full path to document
				Assert.IsTrue(string.Compare(window.Document.FullName, updatedFileName, true) == 0, "FullName of document is not as expected");
			});
		}

		/// <summary>
		/// Test SaveAs - document saved in existing folder (part of the project)
		/// </summary>
		[TestMethod()]
		public void TestSaveAsInExistingSubFolder()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				//Create a project and get the first filenode
				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				FileNode fileNode = GetFirstFileNode(project);

				//open the item before we can do the SaveAs op
				ProjectItem item = ((OAFileItem)fileNode.GetAutomationObject());
				Window window = item.Open(EnvDTE.Constants.vsViewKindPrimary);
				Assert.IsNotNull(window, "Did not get a reference to the window for the file just opened");
				if(!window.Visible)
					window.Visible = true;

				//Create new subfolder
				string newFolderName = "NewFolder1";
				FolderNode folderNode = (FolderNode)Utilities.CreateFolder(project, newFolderName, project);
				Assert.IsNotNull(folderNode, "Could note create new folder node");

				//SaveAs to SubFolder
				string newNameOfFile = "Test.cs";
				string relPathToFile = newFolderName + "\\" + "Test.cs";
				string updatedFileName = Path.Combine(project.ProjectFolder, relPathToFile);
				item.SaveAs(updatedFileName);
				//Verify Caption in window of the file renamed
				Assert.IsTrue(string.Compare(window.Caption, newNameOfFile, true) == 0, "Caption of window does not match the new filename");
				//Verify full path to document
				Assert.IsTrue(string.Compare(window.Document.FullName, updatedFileName, true) == 0, "FullName of document is not as expected");
			});
		}

		/// <summary>
		/// Test SaveAs - document saved in new sub folder (not part of the project)
		/// </summary>
		[TestMethod()]
		public void TestSaveAsInNewDirectory()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				//Create a project and get the first filenode
				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				FileNode fileNode = GetFirstFileNode(project);

				//open the item before we can do the SaveAs op
				ProjectItem item = ((OAFileItem)fileNode.GetAutomationObject());
				Window window = item.Open(EnvDTE.Constants.vsViewKindPrimary);
				Assert.IsNotNull(window, "Did not get a reference to the window for the file just opened");
				if(!window.Visible)
					window.Visible = true;

				//Create new directory
				string newDirectoryName = "NewDirectory1";
				string pathToDir = Path.Combine(project.ProjectFolder, newDirectoryName);
				DirectoryInfo dirInfo = Directory.CreateDirectory(pathToDir);

				//SaveAs
				string newNameOfFile = "Test.cs";
				string relPathToFile = newDirectoryName + "\\" + "Test.cs";
				string updatedFileName = Path.Combine(project.ProjectFolder, relPathToFile);
				item.SaveAs(updatedFileName);

				//Verify Caption in window of the file renamed
				Assert.IsTrue(string.Compare(window.Caption, newNameOfFile, true) == 0, "Caption of window does not match the new filename");

				//Verify full path to document
				Assert.IsTrue(string.Compare(window.Document.FullName, updatedFileName, true) == 0, "FullName of document is not as expected");
			});
		}

		#endregion

		#region helper methods
		private FileNode GetFirstFileNode(ProjectNode project)
		{
			List<FileNode> nodes = Utilities.GetNodesOfType<FileNode>(project);

			foreach(FileNode node in nodes)
			{
				if(String.Compare(Path.GetExtension(node.GetMkDocument()), ".cs", StringComparison.OrdinalIgnoreCase) == 0)
				{
					return node;
				}
			}

			return null;
		}

		/// <summary>
		/// Find the first instance of a filenode with a given caption
		/// </summary>
		/// <param name="caption"></param>
		/// <returns></returns>
		private FileNode FindFileNode(ProjectNode project, string caption)
		{
			foreach(FileNode n in Utilities.GetNodesOfType<FileNode>(project))
			{
				if(string.Compare(n.Caption, caption, true) == 0)
					return n;
			}
			return null;
		}
		#endregion
	}
}
