/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.Collections;
using System.IO;
using System.Reflection;
using EnvDTE;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VSSDK.Tools.VsIdeTesting;
using Microsoft.VisualStudio.Project.Automation;
using Microsoft.VisualStudio.Project.Samples.NestedProject;

namespace Microsoft.VisualStudio.Project.IntegrationTests
{
	/// <summary>
	/// Test of the automation classes OAProject and OAProjectItem
	/// </summary>
	[TestClass]
	public class TestAutomation : BaseTest
	{
		#region test EnvDTE.Project
		/// <summary>
		/// Tests the interface methods on ENVDTE.Property
		/// </summary>
		[TestMethod]
		public void TestAutomationOnProject()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				EnvDTE.Project automation = Utilities.FindExtObject(sp, Utilities.NestedProjectGuid, TestContext.TestName);

				// Test save and Save As
				#region Save with bad input
				this.BadFileNameChecks(automation, true);
				string badFileName = "AnotherFileNotThesameAsTheProjectFile.nestedProj";
				Assert.IsTrue(this.CheckForSaveWithBadFileName<InvalidOperationException>(automation, true, badFileName, String.Empty), "The file named " + badFileName + " could be saved");
				#endregion

				ProjectNode project = ((OANestedProject)automation).Project;
				automation.Save(project.Url);
				int isDirty;
				project.IsDirty(out isDirty);
				Assert.IsTrue(isDirty == 0, "The project was not saved correctly from automation");

				#region save as bad input
				this.BadFileNameChecks(automation, false);
				badFileName = @"..\..\";
				Assert.IsTrue(this.CheckForSaveWithBadFileName<InvalidOperationException>(automation, true, badFileName, String.Empty), "The file named " + badFileName + " could be saved");
				#endregion

				string goodFileName = "ANewProjectFile.nestedproj";
				automation.SaveAs(goodFileName);
				Assert.IsTrue((String.Compare(project.ProjectFile, goodFileName, StringComparison.OrdinalIgnoreCase) == 0), "Save as failed since the file comparison test failed");

				this.TestFileNamesThatShouldPassForSaveAs(automation);
			});
		}
		#endregion

		#region test EnvDTE.ProjectItem
		/// <summary>
		/// Tests on the EnvDTE.ProjectItem implementation.
		/// </summary>
		[TestMethod()]
		public void TestAutomationOnProjectItem()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				OAProject automation = Utilities.FindExtObject(sp, Utilities.NestedProjectGuid, TestContext.TestName) as OAProject;
				Assert.IsNotNull(automation, "Failed to create a project using automation");

				ProjectNode project = automation.Project;

				// Get the AssemblyInfo.cs, try to open it and then ask using automation that it is opened.
				EnvDTE.ProjectItem item = automation.ProjectItems.Item("AssemblyInfo.cs");
				Assert.IsNotNull(item, "Could not retrieve AssemblyInfo.cs");

				EnvDTE.Window window = item.Open(VSConstants.LOGVIEWID_Primary.ToString());
				Assert.IsNotNull(window, "Could not open the AssemblyInfo.cs");
				window.Activate();

				bool isOpen = item.get_IsOpen(VSConstants.LOGVIEWID_Primary.ToString());
				Assert.IsTrue(isOpen, "The AssemblyInfo.cs file should have been opened");

				// Now save it
				item.Save("");

				Assert.IsTrue(item.Saved, "The renamed AssemblyInfo.cs has not been saved");

				// Get the Document
				EnvDTE.Document document = item.Document;
				Assert.IsNotNull(document, "Could not retrieve the document object");
				Assert.IsTrue(document.Name == "AssemblyInfo.cs", "The document for the file item is incorrect. It's name should be AssemblyInfo.cs");

				// Try the properties on a nested item
				EnvDTE.ProjectItem nestedProject = automation.ProjectItems.Item("ANestedProject");
				EnvDTE.ProjectItem nestedProjectItem = nestedProject.ProjectItems.Item("Program.cs");
				EnvDTE.Properties nesteditemsProps = nestedProjectItem.Properties;
				EnvDTE.Property nestedItemProperty = nesteditemsProps.Item("BuildAction");
				Assert.IsNotNull(nestedItemProperty, "Could not retrieve the BuildAction property from the nested project item");
				nestedItemProperty.Value = BuildAction.Content;
				Assert.AreEqual((BuildAction)nestedItemProperty.Value, BuildAction.Content);

				// Now try the properties on the top project item
				EnvDTE.Properties props = item.Properties;
				Assert.IsNotNull(props, "Could not retrieve the BuildAction property from the nested project item");

				EnvDTE.Property itemProperty = props.Item("BuildAction");
				Assert.IsNotNull(itemProperty, "Could not retrieve the BuildAction property from the nested project item");
				Assert.IsFalse(itemProperty is OANullProperty, "Could not retrieve the BuildAction property from the nested project item");
				itemProperty.Value = BuildAction.Content;
				Assert.AreEqual(itemProperty.Value, BuildAction.Content);

				// Now save as
				Assert.IsTrue(item.SaveAs("AssemblyInfo1.cs"), "The file AssemblyInfo.cs could not be reanmed to AssemblyInfo1.cs");
				Assert.IsTrue(item.Name == "AssemblyInfo1.cs", "File item has been renamed to AssemblyInfo1.cs but the Name property has not");

				// Now try the Program.cs. That should not be opened
				EnvDTE.ProjectItem item1 = automation.ProjectItems.Item("Program.cs");

				Assert.IsNotNull(item1, "Could not retrieve AssemblyInfo.cs");

				isOpen = item1.get_IsOpen(VSConstants.LOGVIEWID_Primary.ToString());

				Assert.IsFalse(isOpen, "The Program.cs should not have been opened");

				// Now get the Reference folder as a project item and expand it.
				EnvDTE.ProjectItem references = automation.ProjectItems.Item("References");
				references.ExpandView();

				// Check that actually it was expanded.
				IVsUIHierarchyWindow uiHierarchy = VsShellUtilities.GetUIHierarchyWindow(project.Site, HierarchyNode.SolutionExplorer);
				System.Reflection.MethodInfo mi = typeof(ProjectNode).GetMethod("FindChild", BindingFlags.NonPublic | BindingFlags.Instance);
				ReferenceContainerNode containerNode = (ReferenceContainerNode)mi.Invoke(project, new object[] { "References" });

				__VSHIERARCHYITEMSTATE state;
				uint stateAsInt;
				uiHierarchy.GetItemState(project, (uint)containerNode.ID, (uint)__VSHIERARCHYITEMSTATE.HIS_Expanded, out stateAsInt);
				state = (__VSHIERARCHYITEMSTATE)stateAsInt;
				Assert.IsTrue(state == __VSHIERARCHYITEMSTATE.HIS_Expanded, "The References folder has not been expanded");
			});
		}

		[TestMethod()]
		public void TestAutomationOnProjectItems()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				OAProject automation = Utilities.FindExtObject(sp, Utilities.NestedProjectGuid, TestContext.TestName) as OAProject;
				Assert.IsNotNull(automation, "Failed to create a project using automation");

				string newModelFilePath = TestUtils.GetNewFileName(TestContext.TestDir, TestContext.TestName, "model");
				EnvDTE.ProjectItem pi = automation.ProjectItems.AddFromFileCopy(Path.GetFullPath(newModelFilePath));
				Assert.IsNotNull(pi, "Failed to create a modeling project item through automation");
				Assert.IsTrue(pi.Name == Path.GetFileName(newModelFilePath), "ProjectItems AddFromFileCopy has not returned the item that was added");

				bool[] found = new bool[3];
				// Test enumerators on project items
				foreach(EnvDTE.ProjectItem item in automation.ProjectItems)
				{
					if(item.Name == Path.GetFileName(newModelFilePath))
					{
						found[0] = true;
					}
					else if(item.Name == "Program.cs")
					{
						found[1] = true;
					}
					else if(item.Name == "AssemblyInfo.cs")
					{
						found[2] = true;
					}
				}

				foreach(bool foundValue in found)
				{
					Assert.IsTrue(foundValue, "The iterator on item collection has not been implemented correctly");
				}

				/*****Test the AddFolder method*****/
				//Add a simple folder to the project
				ProjectItem folder = automation.ProjectItems.AddFolder("directory", null);
				//Add a subfolder to the folder
				ProjectItem subfolder = folder.ProjectItems.AddFolder("subdirectory", string.Empty);
				//Add another subfolder to that folder
				subfolder.ProjectItems.AddFolder("subsubdirectory", EnvDTE.Constants.vsProjectItemKindPhysicalFolder);

				//Verify that we have the following structure:
				//Project
				//	-directory
				//		-subdirectory
				//			-subsubdirectory
				ProjectItem directory = automation.ProjectItems.Item("directory") as ProjectItem;
				ProjectItem subdirectory = directory.ProjectItems.Item("subdirectory") as ProjectItem;
				ProjectItem subsubdirectory = subdirectory.ProjectItems.Item("subsubdirectory") as ProjectItem;
				Assert.IsNotNull(directory);
				Assert.IsNotNull(subdirectory);
				Assert.IsNotNull(subsubdirectory);


				bool argumentExceptionThrown = false;
				try
				{
					//We expect virtual folders to fail this way.
					folder.ProjectItems.AddFolder("virtualfolder", EnvDTE.Constants.vsProjectItemKindVirtualFolder);
				}
				catch(ArgumentException) { argumentExceptionThrown = true; }
				Assert.IsTrue(argumentExceptionThrown);

				argumentExceptionThrown = false;
				try
				{
					//Verify that you can't add a folder where a node with that name already exists
					folder.ProjectItems.AddFolder("subdirectory", string.Empty);
				}
				catch(ArgumentException) { argumentExceptionThrown = true; }
				Assert.IsTrue(argumentExceptionThrown);
			});
		}

		/// <summary>
		/// Tests on the EnvDTE.ProjectItem implementation.
		/// </summary>
		[TestMethod()]
		public void TestMoreAutomationOnProjectItems()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode projectNode = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				EnvDTE.Project project = projectNode.GetAutomationObject() as EnvDTE.Project;
				EnvDTE80.Solution2 solution = project.DTE.Solution as EnvDTE80.Solution2;
				string templateName = solution.GetProjectItemTemplate("CodeFile.zip", "CSharp");

				ProjectItem item = project.ProjectItems.AddFromTemplate(templateName, "TestFile.cs");
				this.CheckForItem(projectNode, item);

				// Now add a folder and add an item to the folder.
				ProjectItem folder = project.ProjectItems.AddFolder("Directory", null);
				item = folder.ProjectItems.AddFromTemplate(templateName, "TestFile1.cs");

				this.CheckForItem(projectNode, item);
			});
		}
		#endregion

		#region test configuration independent Project Properties
		[TestMethod()]
		public void TestAutomationOnProjectProperties()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode projectNode = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				//get the automation model of the project
				EnvDTE.Project project = projectNode.GetAutomationObject() as EnvDTE.Project;

				//get properties collection
				EnvDTE.Properties projectProperties = project.Properties;
				Assert.IsNotNull(projectProperties, "Could not get an instance of the project properties object");

				//test the FullPath property returns the actual project path + project filename
				string fullPathFromProjectNode = projectNode.ProjectFolder + "\\";
				EnvDTE.Property fullPath = projectProperties.Item(ProjectAutomationProperyNames.FullPath);
				Assert.IsFalse(String.IsNullOrEmpty(fullPath.Value.ToString()), "FullPath property is null or has not been assigned any value");
				Assert.IsTrue(0 == String.Compare(fullPath.Value.ToString(), fullPathFromProjectNode, true), "FullPath property does not return the correct Value");
			});
		}
		#endregion

		#region tests for property interafce method calls for EnvDTE.Properties
		/// <summary>
		/// Tests the interface methods on ENVDTE.Property
		/// </summary>
		[TestMethod()]
		public void TestInterfaceMethodsOnProperty()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				OAProject automation = Utilities.FindExtObject(sp, Utilities.NestedProjectGuid, TestContext.TestName) as OAProject;
				Assert.IsNotNull(automation, "Failed to create a project using automation");

				ProjectNode projectNode = automation.Project;

				// Get Project Property object
				EnvDTE.Property property = automation.Properties.Item("RootNamespace");
				Assert.IsNotNull(property, "Could not retrieve valid RootNamespace property");
				Assert.IsFalse(property is OANullProperty, "Could not retrieve valid RootNamespace property");
				object retValue = property.Application;
				Assert.IsNull(retValue);

				Assert.IsTrue((string)property.Value == "Application", "Failed to retrieve the Value property.");
				property.Value = "Test1";
				Assert.AreEqual(property.Value, "Test1");


				// Get Collection object from property object
				EnvDTE.Properties properties = property.Collection;
				Assert.IsNotNull(properties, "Collection property failed to retrieve an object");

				// Get the DTE
				retValue = property.DTE;
				Assert.IsNotNull(retValue);

				// Get the Indexed value
				retValue = property.get_IndexedValue(1, 2, 3, 4);
				Assert.IsNull(retValue);

				property.let_Value(1);
				Assert.AreEqual(property.Value, "1");

				// Check the name.
				string name = property.Name;
				Assert.IsNotNull(name);
				Assert.IsTrue(name == "RootNamespace", "RootNamespace property was not set correctly");

				short numIndeces = property.NumIndices;
				//Currently it gives back 0
				//It must be Assertd when the method changes
				Assert.IsTrue(numIndeces == 0);

				// Assert the Object property
				retValue = property.Object;
				Assert.AreEqual(retValue, property.Value);
				property.Object = "test1";
				retValue = property.Object;
				Assert.AreEqual(retValue, "test1");

				// Test the parent property
				EnvDTE.Properties parent = property.Parent;
				Assert.IsTrue(parent is OAProperties, "Parent property failed to return the parent of a property");

				//It does nothing currently. Cannot be Assertd.
				property.set_IndexedValue(1, 2, 3, 4, 5);

				// Try a non string value on the Value.
				ArrayList list = new ArrayList();
				property.Value = list;
				retValue = property.Value;
				Assert.AreEqual(retValue, list.ToString());

				// Test the iterators for enumeration.
				// We are interested to see that we advance with the iteration.
				bool[] found = new bool[2];

				foreach(EnvDTE.Property aProperty in automation.Properties)
				{

					if(aProperty.Name == "RootNamespace")
					{
						found[0] = true;
					}
					else if(aProperty.Name == "AssemblyName")
					{
						found[1] = true;
					}
				}

				foreach(bool foundValue in found)
				{
					Assert.IsTrue(foundValue, "The iterator on property collection has not been implemented correctly");
				}
			});
		}
		#endregion

		#region test Project Item Properties for hierarchy nodes of type FileNode
		[TestMethod()]
		public void TestFileNodeRelatedItemProperties()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode projectNode = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				// get the project object
				EnvDTE.Project project = projectNode.GetAutomationObject() as EnvDTE.Project;

				//get the projectitem object of the program.cs file which is part of the project
				string filename = "program.cs";
				ProjectItem projectItem = project.ProjectItems.Item(filename);
				Assert.IsNotNull(projectItem, "Could not retrieve the projectitem object from the collection of items");

				//get properties collection
				EnvDTE.Properties properties = projectItem.Properties;
				Assert.IsNotNull(properties, "Could not get an instance of the projectitem properties object");

				CompareProperties(properties, typeof(FileNodeAutomationPropertyNames));
			});
		}
		#endregion

		#region test Project Item Properties for hierarchy nodes of type ReferenceNode
		[TestMethod()]
		public void TestReferenceNodeRelatedItemProperties()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode projectNode = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				// get the project object
				EnvDTE.Project project = projectNode.GetAutomationObject() as EnvDTE.Project;

				//get the projectitem object of the first reference object
				string referenceName = "System";
				ProjectItem projectItem = project.ProjectItems.Item("References").ProjectItems.Item(referenceName);
				Assert.IsNotNull(projectItem, "Could not retrieve the projectitem object from the collection of references");

				//get properties collection
				EnvDTE.Properties properties = projectItem.Properties;
				Assert.IsNotNull(properties, "Could not get an instance of the projectitem properties object");

				CompareProperties(properties, typeof(ReferenceNodeAutomationPropertyNames));
			});
		}
		#endregion

		#region test Project Item Properties for hierarchy nodes of type FolderNode
		[TestMethod()]
		public void TestFolderNodeRelatedProjectItemProperties()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode projectNode = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				// get the project object
				EnvDTE.Project project = projectNode.GetAutomationObject() as EnvDTE.Project;

				// add new folder to the project
				System.Reflection.MethodInfo mi = typeof(HierarchyNode).GetMethod("AddNewFolder", BindingFlags.NonPublic | BindingFlags.Instance);
				mi.Invoke(projectNode, null);

				//Get the new foldernode object
				mi = typeof(ProjectNode).GetMethod("FindChild", BindingFlags.NonPublic | BindingFlags.Instance);
				FolderNode folderNode = (FolderNode)mi.Invoke(projectNode, new object[] { "NewFolder1" });
				Assert.IsNotNull(folderNode, "Could not find a folder node in the projec");

				//get automation object from FolderNode
				ProjectItem projectItem = (ProjectItem)folderNode.GetAutomationObject();

				//get properties collection
				EnvDTE.Properties properties = projectItem.Properties;
				Assert.IsNotNull(properties, "Could not get an instance of the projectitem properties object");

				CompareProperties(properties, typeof(FolderNodeAutomationPropertyNames));
			});
		}
		#endregion

		#region test Configuration dependent properties for ProjectNode object
		//[TestMethod()]
		// This test is failing because of a bug in VS 2008 RTM. 
		//TODO: Enable after VS 2008 SP1
		public void TestAutomationOnConfigDependentProperties()
		{
			const int expectedConfigs = 2;

			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode projectNode = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				// get the config object for active configuration
				EnvDTE.Project project = projectNode.GetAutomationObject() as EnvDTE.Project;
				ConfigurationManager configManager = project.ConfigurationManager;
				Assert.IsNotNull(configManager, "Could not get configutation Manager object from project object");
				Assert.AreEqual(configManager.Count, expectedConfigs, "Wrong number of configs");
				Configuration activeConfig = configManager.ActiveConfiguration;
				Assert.IsNotNull(activeConfig, "Could not get active configuration");

				//get properties collection
				EnvDTE.Properties properties = activeConfig.Properties;
				Assert.IsNotNull(properties, "Could not get an instance of the properties object from active configuration");

				CompareProperties(properties, typeof(ProjectConfigPropertyNames));
			});
		}
		#endregion

		#region helpers
		private void CompareProperties(EnvDTE.Properties properties, Type propertyNamesType)
		{
			// test number of properties in the collection
			FieldInfo[] propertyNamesFieldInfo;
			propertyNamesFieldInfo = propertyNamesType.GetFields();
			Assert.AreEqual(properties.Count, propertyNamesFieldInfo.Length, "Number of properties does not match expected number of properties");

			//test that we have the expected set of property names
			foreach(FieldInfo info in propertyNamesFieldInfo)
			{
				string propertyName = info.Name as string;
				Property prop = properties.Item(propertyName);
				Assert.IsNotNull(prop, String.Format("Property %0 not for found in collection", propertyName));
			}
		}

		private bool CheckForSaveWithBadFileName<T>(EnvDTE.Project project, bool isSave, string badFileName, string expectedMessage)
			where T : Exception
		{
			bool badFileNameCaught = false;
			try
			{

				if(isSave)
				{
					project.Save(badFileName);
				}
				else
				{
					project.SaveAs(badFileName);
				}
			}
			catch(Exception e)
			{
				if((e is T))
				{
					if((expectedMessage.Length > 0) && (e.Message == expectedMessage || e.Message.Contains(expectedMessage)) || expectedMessage.Length == 0)
					{
						badFileNameCaught = true;
					}
				}
			}

			return badFileNameCaught;
		}

		private void BadFileNameChecks(EnvDTE.Project automation, bool forSave)
		{


			string errorMessage = Utilities.GetResourceStringFromTheProjectAssembly("ErrorInvalidFileName");
			foreach(string newBadFileName in Utilities.BadFileNames)
			{
				Assert.IsTrue(this.CheckForSaveWithBadFileName<InvalidOperationException>(automation, forSave, newBadFileName, errorMessage), "The file named " + newBadFileName + " could be saved");
			}
			string badFileName = "....nestedProj";
			Assert.IsTrue(this.CheckForSaveWithBadFileName<InvalidOperationException>(automation, forSave, badFileName, errorMessage), "The file named " + badFileName + " could be saved");

			badFileName = "  ";
			Assert.IsTrue(this.CheckForSaveWithBadFileName<InvalidOperationException>(automation, forSave, badFileName, errorMessage), "The file named " + badFileName + " could be saved");

			badFileName = ".....";
			Assert.IsTrue(this.CheckForSaveWithBadFileName<InvalidOperationException>(automation, forSave, badFileName, errorMessage), "The file named " + badFileName + " could be saved");

			errorMessage = String.Format(System.Globalization.CultureInfo.CurrentCulture, Utilities.GetResourceStringFromTheProjectAssembly("PathTooLong"), Path.Combine(Path.GetDirectoryName(automation.FullName), Utilities.LongFileName));
			Assert.IsTrue(this.CheckForSaveWithBadFileName<InvalidOperationException>(automation, forSave, Utilities.LongFileName, errorMessage), "The file named " + Utilities.LongFileName + " could be saved");
		}

		private void TestFileNamesThatShouldPassForSaveAs(EnvDTE.Project project)
		{
			foreach(string fileName in Utilities.FileNamesThatShouldPass)
			{
				string goodFileName = fileName + ".nestedproj";
				project.SaveAs(goodFileName);
				Assert.IsTrue((String.Compare(project.FileName, goodFileName, StringComparison.OrdinalIgnoreCase) == 0), "Save as failed since the file comparison test failed");
			}
		}

		private void CheckForItem(ProjectNode projectNode, ProjectItem item)
		{
			Assert.IsTrue(item != null, "No project item has been added from template to the modeling project");
			Assert.IsTrue(String.Compare(Path.GetExtension(item.Name), ".cs", StringComparison.OrdinalIgnoreCase) == 0, "The item has not been added as a moxl file!");

			VSDOCUMENTPRIORITY[] priority = new VSDOCUMENTPRIORITY[1];
			uint itemid;
			int found;
			projectNode.IsDocumentInProject(item.get_FileNames(1), out found, priority, out itemid);
			Assert.IsTrue(found == 1, "The item " + item.Name + " has not been added to the project");
		}
		#endregion
	}

	#region internal helper classes
	/// <summary>
	/// A list of named properties returned in the properties collection for a ProjectNode object
	/// </summary>
	internal class ProjectAutomationProperyNames
	{
		public static string FileName = "FileName";
		public static string FullPath = "FullPath";
	}

	/// <summary>
	/// A list of named properties returned in the properties collection for a FileNode object
	/// </summary>
	internal class FileNodeAutomationPropertyNames
	{
		public static string BuildAction = "BuildAction";
		public static string FileName = "Filename";
		public static string FullPath = "FullPath";
		public static string CustomTool = "CustomTool";
		public static string CustomToolNamespace = "CustomToolNamespace";
		public static string Extension = "Extension";
		public static string ExtenderCATID = "ExtenderCATID";
	}

	/// <summary>
	/// A list of named properties returned in the properties collection for a ReferenceNode object
	/// </summary>
	internal class ReferenceNodeAutomationPropertyNames
	{
		public static string Name = "Name";
		public static string CopyToLocal = "CopyToLocal";
		public static string FullPath = "FullPath";
		public static string ExtenderCATID = "ExtenderCATID";
	}

	/// <summary>
	/// A list of named properties returned in the properties collection for a FolderNode object
	/// </summary>
	internal class FolderNodeAutomationPropertyNames
	{
		public static string FileName = "FileName";
		public static string FullPath = "FullPath";
		public static string ExtenderCATID = "ExtenderCATID";
	}

	internal class ProjectConfigPropertyNames
	{
		public const string OutputPath = "OutputPath";
	}
	#endregion

}