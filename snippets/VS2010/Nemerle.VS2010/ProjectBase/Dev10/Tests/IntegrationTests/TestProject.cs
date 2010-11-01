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
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using EnvDTE;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VSSDK.Tools.VsIdeTesting;
using Microsoft.VisualStudio.Project.Automation;
using MSBuild = Microsoft.Build.Evaluation;

namespace Microsoft.VisualStudio.Project.IntegrationTests
{
	/// <summary>
	/// Unit Test of the My Nested Project Class
	/// </summary>
	[TestClass]
	public class TestProject : BaseTest
	{
		private const string BuildEngineRef = "Microsoft.Build.Engine";
		private static string ResourceLocation = typeof(TestProject).Namespace;
		private const string EmbeddedProjectName = "Project.nestedproj";
		private static string EmbeddedResourceProjectLocation = ResourceLocation + "." + EmbeddedProjectName;

		/// <summary>
		/// Create MBFLibrary Project using DTE.
		/// Get Nested Projects, and the subprojects of the nested projects
		/// Add new file to the nestedProject
		/// </summary>
		[TestMethod()]
		public void TestProjectCreationWithDTE()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				// test nested project item
				EnvDTE.ProjectItem nestedProject = ((OAProject)project.GetAutomationObject()).ProjectItems.Item("ANestedProject");

				Assert.IsNotNull(nestedProject, "Failed to retrieve nested projects");

				Assert.IsNotNull(nestedProject.SubProject, "Failed to retrieve nested projects's subproject");

				// Add a cs file to the nested project.
				string newFileName = Path.GetFullPath(TestUtils.GetNewFileName(project.ProjectFolder, "GetClassWithCodeModel", "cs"));

				EnvDTE.ProjectItem newFile = nestedProject.SubProject.ProjectItems.AddFromFile(newFileName);
				CodeModel codeModel = nestedProject.SubProject.CodeModel;
				Assert.IsNotNull(codeModel, "Failed to retrieve nested projects's subproject");

				FileCodeModel fileCodeModel = newFile.FileCodeModel;

				CodeClass cc = fileCodeModel.AddClass("MyClass", 0, null, null, vsCMAccess.vsCMAccessPublic);

				IEnumerator enumerator = fileCodeModel.CodeElements.GetEnumerator();

				bool found = false;
				while(enumerator.MoveNext())
				{
					CodeElement element = enumerator.Current as CodeElement;

					if(element.Name == "MyClass")
					{
						found = true;
						break;
					}
				}

				Assert.IsTrue(found, "Could not find the class in the code model associated to the nested project");
			});
		}

		/// <summary>
		/// Create Project using SolutionExplorer
		/// Verify the created project
		/// </summary>
		[TestMethod()]
		public void TestProjectCreationInSolutionExplorer()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				try
				{
					string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
					ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);
				}
				catch(Exception e)
				{
					Assert.Fail("Failed to retrieve a project. Reason :", e.Message);
				}
			});
		}

		/// <summary>
		/// Create project and check if the hasProjectOpened flag is set.
		/// </summary>
		[TestMethod()]
		public void TestHasProjectOpened()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				PropertyInfo pi = typeof(ProjectNode).GetProperty("HasProjectOpened", BindingFlags.Instance | BindingFlags.NonPublic);
				bool hasProjectOpened = (bool)pi.GetValue(project, new object[] { });
				Assert.IsTrue(hasProjectOpened, "The event listener for opening the project has failed");
			});
		}

		/// <summary>
		/// Load Project using SolutionExplorer
		/// Get the AutomationObject of the project
		/// Verify the object
		/// </summary>
		[TestMethod()]
		public void TestGetAutomationObject()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				object retValue = project.GetAutomationObject();

				Assert.IsNotNull(retValue);
			});
		}

		[TestMethod()]
		public void TestAddChild()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				string newFileName = "output.txt";

				FileNode node = project.CreateFileNode(newFileName);
				Assert.IsNotNull(node);

				project.AddChild(node);

				Assert.IsTrue(node == project.FindChildByProjectElement(node.ItemNode));
			});
		}

		[TestMethod()]
		public void TestBuild()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				Assert.IsTrue(project.Build("Build") == MSBuildResult.Successful);
			});
		}

		[TestMethod]
		public void TestDesignTimeAssemblyResolution()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				string[] assemblySpecs = new[] { "System", "System.Core" };
				VsResolvedAssemblyPath[] resolvedPaths = new VsResolvedAssemblyPath[assemblySpecs.Length];
				uint resolvedCount;
				project.GetProjectOptions(); // force the initial build
				Marshal.ThrowExceptionForHR(project.ResolveAssemblyPathInTargetFx(assemblySpecs, (uint)assemblySpecs.Length, resolvedPaths, out resolvedCount));
				Assert.AreEqual(assemblySpecs.Length, (int)resolvedCount);
				for (int i = 0; i < resolvedCount; i++)
				{
					Assert.IsTrue(resolvedPaths[i].bstrResolvedAssemblyPath.Contains(resolvedPaths[i].bstrOrigAssemblySpec));
				}
			});
		}

		[TestMethod()]
		public void TestDeletingAProjectItem()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				List<uint> filenodeids = Utilities.SelectOrUnselectNodes<FileNode>(project, true);
				Debug.Assert(filenodeids.Count > 0, "There is no file node availabale");
				FileNode node = project.NodeFromItemId(filenodeids[0]) as FileNode;
				ProjectElement element = node.ItemNode;
				string path = node.GetMkDocument();
				node.Remove(true);

				//Now see if it is removed from the UI.
				HierarchyNode deletedNode = project.NodeFromItemId(filenodeids[0]);
				Assert.IsTrue(deletedNode == null, "File has not been removed correctly");

				// See if it has been removed from the procject file.
				MethodInfo mi = typeof(ProjectElement).GetMethod("HasItemBeenDeleted", BindingFlags.Instance | BindingFlags.NonPublic);
				bool hasBeenDeleted = (bool)mi.Invoke(element, new object[] { });
				Assert.IsTrue(hasBeenDeleted, "File has not been removed correctly from the project file.");

				// See if it has been deleted physically
				Assert.IsFalse(File.Exists(path), "File has not been removed correctly from the disk.");
			});
		}

		/// <summary>
		/// Tests default action on a FileNode.
		/// </summary>
		[TestMethod()]
		public void TestDoDefaultActionOnFileNode()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				MethodInfo doDefaultAction = typeof(ProjectNode).GetMethod("DoDefaultAction", BindingFlags.NonPublic | BindingFlags.Instance);

				// select the model file, close it and then do deafult action on it. test whether it has opened it.
				List<uint> filenodeids = Utilities.SelectOrUnselectNodes<FileNode>(project, true);

				foreach(uint id in filenodeids)
				{
					FileNode node = project.NodeFromItemId(id) as FileNode;
					Assert.IsNotNull(node);
					string document = node.GetMkDocument();

					if(String.Compare(Path.GetExtension(document), ".cs", StringComparison.OrdinalIgnoreCase) == 0)
					{
						VsShellUtilities.SaveFileIfDirty(project.Site, document);

						MethodInfo getDocumentManager = typeof(FileNode).GetMethod("GetDocumentManager", BindingFlags.NonPublic | BindingFlags.Instance);
						DocumentManager manager = getDocumentManager.Invoke(node, new object[] { }) as DocumentManager;

						// Close the node.
						Assert.IsTrue(manager.Close(__FRAMECLOSE.FRAMECLOSE_SaveIfDirty) == VSConstants.S_OK);

						// Be sure the node is selected.
						Utilities.ManipulateNode(project, node.ID, EXPANDFLAGS.EXPF_SelectItem);

						// Invoke opening of the node.
						doDefaultAction.Invoke(node, new object[] { });

						// Check whether the document has been opened.
						Guid logicalView = Guid.Empty;
						IVsUIHierarchy hierOpen;
						uint itemId;
						IVsWindowFrame windowFrame;
						Assert.IsTrue(VsShellUtilities.IsDocumentOpen(node.ProjectMgr.Site, document, logicalView, out hierOpen, out itemId, out windowFrame), "DoDeafult action did not open the file");
						Assert.IsTrue(itemId == node.ID, "We did not open the correct document");
						Assert.IsNotNull(windowFrame, "Do deafult action did not retun a window frame");
						Assert.IsTrue(windowFrame.IsVisible() == VSConstants.S_OK, "Do deafult action did not show a window frame");
					}
				}
			});
		}

		[TestMethod()]
		public void TestSaveAsOnProjectFile()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				MethodInfo saveAs = project.GetType().GetMethod("SaveAs", BindingFlags.Instance | BindingFlags.NonPublic);

				#region test for bad filenames
				this.TestBadFileNameForSaveAs(sp, project, saveAs);
				#endregion

				string oldUrl = project.Url;
				string goodFileName = project.ProjectFolder + "\\test.nestedproj";
				saveAs.Invoke(project, new object[] { goodFileName });

				Assert.IsTrue(NativeMethods.IsSamePath(project.Url, goodFileName), "Save as failed since the Url test failed");
				Assert.IsTrue((String.Compare(project.ProjectFile, "test.nestedProj", StringComparison.OrdinalIgnoreCase) == 0), "Save as failed since the file comparison test failed");

				this.TestFileNameThatHasToPassForSaveAs(project, saveAs);

				// Now close the project and reopen with the renamed file.
				string projectFullPath = project.Url;
				IVsSolution solutionService = (IVsSolution)sp.GetService(typeof(IVsSolution));
				// Close already open solution
				solutionService.CloseSolutionElement((uint)__VSSLNSAVEOPTIONS.SLNSAVEOPT_SaveIfDirty, null, 0);

				EnvDTE.Project newProject = dte.Solution.AddFromFile(projectFullPath, true);

				Assert.IsTrue(newProject != null, "failed to load the renamed project");
			});
		}

		[TestMethod()]
		public void TestRenameOfProjectFile()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				string extension = Path.GetExtension(project.ProjectFile);
				#region test for bad filenames
				this.TestBadFileNameForSetEditLabel(project);
				#endregion

				string oldUrl = project.Url;
				string goodFileName = "test";

				project.SetEditLabel(goodFileName);
				Assert.IsTrue(NativeMethods.IsSamePath(project.Url, Path.Combine(project.ProjectFolder, goodFileName + ".nestedProj")), "SetEditLabel failed since the Url test failed");

				Assert.IsTrue((String.Compare(project.ProjectFile, "test" + extension, StringComparison.Ordinal) == 0), "SetEditLabel failed since the file comparison test failed. Expected that " + project.ProjectFile + " equals test.nestedProj");

				// Now do a case only change
				project.SetEditLabel("Test");
				Assert.IsTrue((String.Compare(project.ProjectFile, "Test" + extension, StringComparison.Ordinal) == 0), "SetEditLabel failed since the file comparison test failed. Expected that " + project.ProjectFile + " equals Test.nestedProj");


				this.TestFileNameThatHasToPassForSetEditLabel(project);

				// Now close the project and reopen with the renamed file.
				string projectFullPath = project.Url;
				IVsSolution solutionService = (IVsSolution)sp.GetService(typeof(IVsSolution));
				// Close already open solution
				solutionService.CloseSolutionElement((uint)__VSSLNSAVEOPTIONS.SLNSAVEOPT_SaveIfDirty, null, 0);

				EnvDTE.Project newProject = dte.Solution.AddFromFile(projectFullPath, true);

				Assert.IsTrue(newProject != null, "failed to load the renamed project");
			});
		}

		[TestMethod()]
		public void TestRenameOfRenamedProjectFile()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				string projectName = project.GetMkDocument();
				string newprojectName = Path.Combine(project.ProjectFolder, Path.GetFileNameWithoutExtension(projectName) + "_temp");

				// Rename the project file on disk
				File.Move(projectName, newprojectName);

				// Now rename it within the solution explorer. This will popup the dialog box that the file cannot be renamed. 
				// We are going to catch that dialog box.
				string resourceText = Utilities.GetResourceStringFromTheProjectAssembly("FileOrFolderCannotBeFound");

				string message = String.Format(System.Globalization.CultureInfo.CurrentCulture, resourceText, project.ProjectFile);

				Assert.IsTrue(Utilities.CheckForSetEditLabelBadFileName<InvalidOperationException>(project, "NewFileName", message), "The messagebox for not being able to rename a file that has been deleted has never popped up");
			});
		}

		[TestMethod()]
		public void TestBuildEngineAndProject()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);
                
				FieldInfo fi;
				PropertyInfo pi;
				pi = typeof(ProjectNode).GetProperty("BuildEngine", BindingFlags.Instance | BindingFlags.NonPublic);
				pi.SetValue(project, MSBuild.ProjectCollection.GlobalProjectCollection, new object[] { });
				fi = typeof(ProjectNode).GetField("buildEngine", BindingFlags.Instance | BindingFlags.NonPublic);
                Assert.AreEqual<MSBuild.ProjectCollection>(MSBuild.ProjectCollection.GlobalProjectCollection, fi.GetValue(project) as MSBuild.ProjectCollection);
                project.SetProjectFileDirty(false);
                
                MSBuild.Project newBuildProject = new MSBuild.Project(MSBuild.ProjectCollection.GlobalProjectCollection);
                newBuildProject.Save(Path.GetTempFileName());
                pi = typeof(ProjectNode).GetProperty("BuildProject", BindingFlags.Instance | BindingFlags.NonPublic);
				pi.SetValue(project, newBuildProject, new object[] { });
                fi = typeof(ProjectNode).GetField("buildProject", BindingFlags.Instance | BindingFlags.NonPublic);
				Assert.AreEqual<MSBuild.Project>(newBuildProject, fi.GetValue(project) as MSBuild.Project);

                

			});
		}

		[TestMethod()]
		public void TestUnloadReloadOfProject()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				Utilities.SelectOrUnselectNodes<ProjectNode>(project, true);

				// Save everything.
				IVsSolution solutionService = (IVsSolution)sp.GetService(typeof(IVsSolution));
				solutionService.SaveSolutionElement((uint)__VSSLNSAVEOPTIONS.SLNSAVEOPT_SaveIfDirty, project, 0);

				string projectFullpath = project.GetMkDocument();
				object customin = null;
				object customout = null;
				dte.Commands.Raise(VsMenus.guidStandardCommandSet97.ToString("B"), (int)Microsoft.VisualStudio.VSConstants.VSStd97CmdID.UnloadProject, ref customin, ref customout);
				// Check to see if the project is unloaded
				Assert.IsTrue(project.IsClosed, "The project has not been unloaded");

				dte.Commands.Raise(VsMenus.guidStandardCommandSet97.ToString("B"), (int)Microsoft.VisualStudio.VSConstants.VSStd97CmdID.ReloadProject, ref customin, ref customout);

				// Check to see if the project is reloaded. we cannot use the instance for the project since that is zombied at this point.
				IVsHierarchy ourHierarchy;
				solutionService.GetProjectOfUniqueName(projectFullpath, out ourHierarchy);
				Assert.IsTrue(ourHierarchy is IProjectEventsListener, "Our hierarchy has not been reloaded successfully");

				// Check to see if the nested project is there.
				EnvDTE.Project projectDTE = Utilities.GetAutomationObject(ourHierarchy);
				Assert.IsNotNull(projectDTE.ProjectItems.Item("ANestedProject"), "The nested project has not been loaded correctly.");

				// Check that bug 106520 does not happen anymore. We will check that the parent project is not dirty.
				int isDirty;
				((IPersistFileFormat)ourHierarchy).IsDirty(out isDirty);
				Assert.IsTrue(isDirty == 0, "The parent project is dirtied after it has been reloaded");
			});
		}

		[TestMethod()]
		public void TestLoadingOfProjectWithDuplicateItems()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				string path = project.GetMkDocument();

				// It would be nice if we could just copy the embedded resource to the opened project file, but implicit reload of nested projects crashes on non SP1 versions.
				// For now we are going to close the project and reopen it with the embedded project.
				IVsSolution solutionService = (IVsSolution)sp.GetService(typeof(IVsSolution));

				solutionService.CloseSolutionElement((uint)__VSSLNSAVEOPTIONS.SLNSAVEOPT_ForceSave, project, 0);

				TestUtils.WriteEmbeddedResourceToBinaryFile(typeof(TestProject).Assembly, EmbeddedResourceProjectLocation, path);

				IntPtr projectPtr = IntPtr.Zero;
				Guid guid = Guid.Empty;
				Guid iid = new Guid("{00000000-0000-0000-C000-000000000046}"); // IID_IUnknown;

				try
				{
					ErrorHandler.ThrowOnFailure(solutionService.CreateProject(ref guid, path, null, null, (uint)__VSCREATEPROJFLAGS.CPF_OPENFILE, ref iid, out projectPtr));

					// Now see that we have only unique items. We do not care much about canonicalization issues in this test.
					this.CheckForUniqueCaptions(project);
				}
				finally
				{
					if(projectPtr != IntPtr.Zero)
					{
						Marshal.Release(projectPtr);
					}
				}
			});
		}

		[TestMethod()]
		public void TestRenameOfNestedProject()
		{
			UIThreadInvoker.Invoke((ThreadInvoker)delegate()
			{
				//Get the global service provider and the dte
				IServiceProvider sp = VsIdeTestHostContext.ServiceProvider;
				DTE dte = (DTE)sp.GetService(typeof(DTE));

				string destination = Path.Combine(TestContext.TestDir, TestContext.TestName);
				ProjectNode project = Utilities.CreateMyNestedProject(sp, dte, TestContext.TestName, destination, true);

				IVsHierarchy nestedProjectHierarchy = Utilities.GetNestedHierarchy(project, "ANestedProject");

				if(nestedProjectHierarchy == null)
				{
					throw new InvalidOperationException("The nested project has not been loaded corectly");
				}

				// Get the id 
				NestedProjectNode nestedProjectNode = Utilities.GetNodesOfType<NestedProjectNode>(project)[0];

				project.SetProperty(nestedProjectNode.ID, (int)__VSHPROPID.VSHPROPID_EditLabel, "NewProject");

				this.VerifyNestedProjectRename(project, nestedProjectNode, nestedProjectHierarchy, "NewProject");

				// Now do an indirect rename through the property window.
				object extensibility;
				ErrorHandler.ThrowOnFailure(nestedProjectHierarchy.GetProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_ExtObject, out extensibility));
				EnvDTE.Project nestedAutomationProject = extensibility as EnvDTE.Project;

				EnvDTE.Property fileNameProperty = nestedAutomationProject.Properties.Item("FileName");
				fileNameProperty.Value = "Project";
				this.VerifyNestedProjectRename(project, nestedProjectNode, nestedProjectHierarchy, "Project");
			});
		}

		private bool CheckForSaveAsOnBadFileName<T>(IServiceProvider sp, ProjectNode project, MethodInfo saveAs, string badFileName, string expectedMessage)
			where T : Exception
		{
			bool badFileNameCaught = false;
			IVsExtensibility extensibility = sp.GetService(typeof(IVsExtensibility)) as IVsExtensibility;
			extensibility.EnterAutomationFunction();
			try
			{
				saveAs.Invoke(project, new object[] { badFileName });
			}
			catch(Exception e)
			{
				if((e.InnerException is T) && (e.InnerException.Message == expectedMessage || e.InnerException.Message.Contains(expectedMessage)))
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

		private void TestBadFileNameForSaveAs(IServiceProvider sp, ProjectNode project, MethodInfo saveAs)
		{
			string errorMessage = String.Format(System.Globalization.CultureInfo.CurrentCulture, Utilities.GetResourceStringFromTheProjectAssembly("PathTooLong"), Utilities.LongFileName);
			Assert.IsTrue(this.CheckForSaveAsOnBadFileName<InvalidOperationException>(sp, project, saveAs, Utilities.LongFileName, errorMessage), "The file named " + Utilities.LongFileName + " could be saved");

			string badFileName = "....";
			errorMessage = Utilities.GetResourceStringFromTheProjectAssembly("FileNameCannotContainALeadingPeriod");
			Assert.IsTrue(this.CheckForSaveAsOnBadFileName<InvalidOperationException>(sp, project, saveAs, badFileName, errorMessage), "The file named " + badFileName + " could be saved as");

			badFileName = "..\\" + project.ProjectFile;
			errorMessage = String.Format("The project file can only be saved into the project location '{0}'.", project.ProjectFolder);
			Assert.IsTrue(this.CheckForSaveAsOnBadFileName<InvalidOperationException>(sp, project, saveAs, badFileName, errorMessage), "The file named " + badFileName + " could be saved as");

			errorMessage = Utilities.GetResourceStringFromTheProjectAssembly("ErrorInvalidFileName");

			foreach(string newBadFileName in Utilities.BadFileNames)
			{
				Assert.IsTrue(this.CheckForSaveAsOnBadFileName<InvalidOperationException>(sp, project, saveAs, newBadFileName, errorMessage), "The file named " + newBadFileName + " could be saved as");
			}
		}

		private void TestBadFileNameForSetEditLabel(ProjectNode project)
		{
			string errorMessage = Utilities.GetResourceStringFromTheProjectAssembly("ErrorInvalidFileName");

			foreach(string newBadFileName in Utilities.BadFileNames)
			{
				Assert.IsTrue(Utilities.CheckForSetEditLabelBadFileName<InvalidOperationException>(project, newBadFileName, errorMessage), "The file named " + newBadFileName + " could be saved as");
			}

			string badFileName = "  ";
			Assert.IsTrue(Utilities.CheckForSetEditLabelBadFileName<InvalidOperationException>(project, badFileName, errorMessage), "The file named " + badFileName + " could be saved as");

			badFileName = "..\\" + project.ProjectFile;
			Assert.IsTrue(Utilities.CheckForSetEditLabelBadFileName<InvalidOperationException>(project, badFileName, errorMessage), "The file named " + badFileName + " could be saved as");

			badFileName = "....";
			Assert.IsTrue(Utilities.CheckForSetEditLabelBadFileName<InvalidOperationException>(project, badFileName, errorMessage), "The file named " + badFileName + " could be saved as");

			errorMessage = String.Format(System.Globalization.CultureInfo.CurrentCulture, Utilities.GetResourceStringFromTheProjectAssembly("PathTooLong"), Utilities.LongFileName);
			Assert.IsTrue(Utilities.CheckForSetEditLabelBadFileName<InvalidOperationException>(project, Utilities.LongFileName, errorMessage), "The file named " + Utilities.LongFileName + " could be saved");
		}

		private void TestFileNameThatHasToPassForSetEditLabel(ProjectNode project)
		{

			foreach(string fileName in Utilities.FileNamesThatShouldPass)
			{
				string goodFileName = fileName + ".nestedproj";
				project.SetEditLabel(goodFileName);
				Assert.IsTrue(NativeMethods.IsSamePath(project.Url, Path.Combine(project.ProjectFolder, goodFileName)), "SetEditLabel failed since the Url test failed");
			}
		}

		private void TestFileNameThatHasToPassForSaveAs(ProjectNode project, MethodInfo saveAs)
		{
			foreach(string fileName in Utilities.FileNamesThatShouldPass)
			{
				string goodFileName = project.ProjectFolder + "\\" + fileName + ".nestedproj";
				saveAs.Invoke(project, new object[] { goodFileName });

				Assert.IsTrue(NativeMethods.IsSamePath(project.Url, goodFileName), "Save as failed since the Url test failed");
			}
		}

		private void TestImplicitNestedProjectReload(IServiceProvider sp, ProjectNode project, int dialogAnswer)
		{
			// Save everything.
			IVsSolution solutionService = (IVsSolution)sp.GetService(typeof(IVsSolution));
			solutionService.SaveSolutionElement((uint)__VSSLNSAVEOPTIONS.SLNSAVEOPT_SaveIfDirty, project, 0);

			IVsProject3 nestedProject = Utilities.GetNestedHierarchy(project, "ANestedProject") as IVsProject3;

			if(nestedProject == null)
			{
				throw new InvalidOperationException("The nested project has not been loaded corectly");
			}

			string nestedProjectFileName = null;
			nestedProject.GetMkDocument(VSConstants.VSITEMID_ROOT, out nestedProjectFileName);

			if(nestedProjectFileName == null)
			{
				throw new InvalidOperationException("The nested project file name could not been retrieved corectly");
			}

			string resourceText = Utilities.GetResourceStringFromTheProjectAssembly("QueryReloadNestedProject");

			// Create the messageBoxListener Thread. This will bring up the reload of the nested project file.
			// In this scenario we will answer dialogAnswer. Also we rely on the exact messagebox text here.
			string message = String.Format(System.Globalization.CultureInfo.CurrentCulture, resourceText, nestedProjectFileName);

			DialogBoxPurger purger = new DialogBoxPurger(dialogAnswer, message);
			bool result = false;
			try
			{

				purger.Start();
				this.AddReferenceExternallyToTheProjectFile(nestedProjectFileName);
			}
			finally
			{
				result = purger.WaitForDialogThreadToTerminate();
			}

			if(!result)
			{
				throw new InvalidOperationException("The messagebox for relaoding the nested project file has never popped up");
			}

			// Check to see if the nested project is there.
			EnvDTE.Project projectDTE = Utilities.GetAutomationObject(project);
			EnvDTE.ProjectItem item = projectDTE.ProjectItems.Item("ANestedProject");

			Assert.IsNotNull(item, "The nested project has not been loaded correctly.");
			EnvDTE.Project nestedAutomationProject = item.SubProject;

			// Now check to see if we can find the added reference
			VSLangProj.VSProject automationProject = nestedAutomationProject.Object as VSLangProj.VSProject;
			if(nestedAutomationProject == null)
			{
				throw new InvalidOperationException("The nested project is not a vs language project");
			}

			// Get references collection
			VSLangProj.References references = automationProject.References;

			IEnumerator enumerator = references.GetEnumerator();
			bool found = false;
			while(enumerator.MoveNext())
			{
				VSLangProj.Reference reference = enumerator.Current as VSLangProj.Reference;
				if(reference.Name == BuildEngineRef)
				{
					found = true;
				}
			}

			if(dialogAnswer == NativeMethods.IDYES)
			{
				Assert.IsTrue(found, "The nested project file has not been reloaded correctly");
			}
			else
			{
				Assert.IsFalse(found, "The nested project file has been reloaded but was asked not to do that.");
			}

		}

		private void AddReferenceExternallyToTheProjectFile(string nestedProjectFileName)
		{
			using(FileStream fs = new FileStream(nestedProjectFileName, FileMode.Open, FileAccess.ReadWrite))
			{
				TextReader reader = new StreamReader(fs);
				string content = reader.ReadToEnd();
				int index = content.IndexOf("</Project>");
				if(index == -1)
				{
					throw new InvalidOperationException("The project file does not have an end tag.");
				}

				fs.Position = index + 1;
				TextWriter writer = new StreamWriter(fs);
				string stringToWrite = Environment.NewLine + "<ItemGroup>" + Environment.NewLine + " <Reference Include=\"" + BuildEngineRef + "\"/>" + Environment.NewLine + "</ItemGroup>" + Environment.NewLine + "</Project>";
				writer.Write(stringToWrite);
				writer.Flush();
			}
		}

		private void CheckForUniqueCaptions(HierarchyNode parent)
		{
			// Define a set for storing captions. We are going to add the Captions to this set. 
			// The add operation will fail if duplicates are found.
			Dictionary<string, HierarchyNode> itemsPerNode = new Dictionary<string, HierarchyNode>();
			for(HierarchyNode n = parent.FirstChild; n != null; n = n.NextSibling)
			{
				itemsPerNode.Add(n.Caption, n);
				this.CheckForUniqueCaptions(n);
			}
		}

		private void VerifyNestedProjectRename(ProjectNode project, NestedProjectNode nestedProject, IVsHierarchy nestedHierarchy, string testName)
		{
			Assert.IsTrue(nestedProject.Caption == testName, "Failed to rename the nested project to " + testName);

			string nestedProjectPath;
			((IVsProject3)nestedHierarchy).GetMkDocument(VSConstants.VSITEMID_ROOT, out nestedProjectPath);

			Assert.IsTrue(NativeMethods.IsSamePath(nestedProject.Url, nestedProjectPath), "Failed to rename the nested project file to " + nestedProjectPath);

            MSBuild.Project buildProject = typeof(ProjectNode).GetProperty("BuildProject", BindingFlags.Instance | BindingFlags.NonPublic).GetValue(project, new object[] { }) as MSBuild.Project;

			foreach(MSBuild.ProjectItem item in buildProject.Items)
			{
				if(String.Compare(item.ItemType, ProjectFileConstants.SubProject, StringComparison.OrdinalIgnoreCase) == 0)
				{
					string name = Path.GetFileNameWithoutExtension(item.EvaluatedInclude);
					Assert.IsTrue(name == testName, "Failed to rename the nested project to " + testName);
				}
			}
		}
	}
}
