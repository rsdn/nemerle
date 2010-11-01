/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.IO;
using System.Reflection;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.VsSDK.UnitTestLibrary;
using MSBuild = Microsoft.Build.BuildEngine;

namespace Microsoft.VisualStudio.Project.Samples.NestedProject.UnitTests
{
	/// <summary>
	///This is a test class for VisualStudio.Project.Samples.NestedProject.OANestedProject and is intended
	///to contain all VisualStudio.Project.Samples.NestedProject.OANestedProject Unit Tests.
	///</summary>
	[TestClass()]
	public class AutomationTests : BaseTest
	{
		#region Fields

		private OANestedProject nestedProject;
		private OANestedProjectProperties projectProperties;
		#endregion Fields

		#region Tests Initialization && Cleanup
        [ClassInitialize]
        public static void TestClassInitialize(TestContext context)
        {
            fullPathToClassTemplateFile = Path.Combine(context.TestDeploymentDir, fullPathToClassTemplateFile);
            fullPathToProjectFile = Path.Combine(context.TestDeploymentDir, fullPathToProjectFile);
            fullPathToTargetFile = Path.Combine(context.TestDeploymentDir, fullPathToTargetFile);
        }

		/// <summary>
		/// Runs before the test to allocate and configure resources needed 
		/// by all tests in the test class.
		/// </summary>
		[TestInitialize()]
		public override void Initialize()
		{
            base.Initialize();

            //init the automation objects
			nestedProject = new OANestedProject(projectNode);
			projectProperties = (OANestedProjectProperties)nestedProject.Properties;
		}
		#endregion

		#region The tests for the OANestedProject && OANestedProjectProperties classes
		#region Constructors tests
		/// <summary>
		/// The test for OANestedProject default constructor.
		///</summary>
		[TestMethod()]
		public void ConstructorTest()
		{
			Assert.IsNotNull(nestedProject, "OANestedProject instance was uninitialized.");
			Assert.IsNotNull(nestedProject.Project, "OANestedProject Project property was uninitialized.");
		}
		#endregion Constructors tests

		#region Properties tests
		/// <summary>
		/// The test for Properties property.
		///</summary>
		[TestMethod()]
		public void PropertiesTest()
		{
			Assert.IsNotNull(nestedProject.Properties, "Node Properties was uninitialized.");
			Assert.IsTrue((nestedProject.Properties is OANestedProjectProperties),
				"Returned Node Properties was initialized by unexpected type value.");
		}
		#endregion Properties tests
		#endregion The tests for the OANestedProject && OANestedProjectProperties classes

		#region The tests for the OANestedProjectProperty class

		#region Constructors tests
		/// <summary>
		/// The test for OANestedProjectProperty explicit default constructor.
		///</summary>
		[TestMethod()]
		public void DefaultConstructorTest()
		{
			OANestedProjectProperty target = new OANestedProjectProperty();
			Assert.IsNotNull(target, "The OANestedProjectProperty instance was not created successfully.");
		}

		/// <summary>
		/// The test for OANestedProjectProperty internal constructor.
		///</summary>
		[TestMethod()]
		public void InternalConstructorTest()
		{

			Assert.IsNotNull(nestedProject.Properties, "Node Properties was uninitialized.");
			Assert.IsTrue((nestedProject.Properties is OANestedProjectProperties), "Returned Node Properties was initialized by unexpected type value.");

			string name = "Some random name";
			OANestedProjectProperty testProperty = VisualStudio_Project_Samples_OANestedProjectPropertyAccessor.CreatePrivate(projectProperties, name);
			Assert.IsNotNull(testProperty, "The OANestedProjectProperty instance was not created successfully.");
		}
		#endregion Constructors tests

		#region Properties tests

		/// <summary>
		/// The test for the Application property.
		/// </summary>
		/// <remarks>This property marked as "Microsoft Internal Use Only" and returns null.</remarks>
		[TestMethod()]
		public void ApplicationPropertyTest()
		{
			string name = "Some random name";
			OANestedProjectProperty testProperty = VisualStudio_Project_Samples_OANestedProjectPropertyAccessor.CreatePrivate(projectProperties, name);
			Assert.IsNotNull(testProperty, "The OANestedProjectProperty instance was not created successfully.");
			Assert.IsNull(testProperty.Application, "Application property was returned as initialized value.");
		}
		/// <summary>
		/// The test for the Parent property.
		/// </summary>
		[TestMethod()]
		public void ParentPropertyTest()
		{
			string name = "Some random name";
			OANestedProjectProperty testProperty = VisualStudio_Project_Samples_OANestedProjectPropertyAccessor.CreatePrivate(projectProperties, name);
			Assert.IsNotNull(testProperty, "The OANestedProjectProperty instance was not created successfully.");
			Assert.AreEqual(projectProperties, testProperty.Parent, "ProjectProperty Parent was initialized by unexpected value.");
		}
		/// <summary>
		/// The test for the Collection property.
		/// </summary>
		[TestMethod()]
		public void CollectionPropertyTest()
		{
			string name = "Some random name";
			OANestedProjectProperty testProperty = VisualStudio_Project_Samples_OANestedProjectPropertyAccessor.CreatePrivate(projectProperties, name);
			Assert.IsNotNull(testProperty, "The OANestedProjectProperty instance was not created successfully.");
			Assert.AreEqual(projectProperties, testProperty.Collection, "ProjectProperty Collection was initialized by unexpected value.");
		}
		/// <summary>
		/// The test for the Parent DTE property.
		/// </summary>
		[TestMethod()]
		public void ParentDTEPropertyTest()
		{
			string name = "Some random name";
			OANestedProjectProperty testProperty = VisualStudio_Project_Samples_OANestedProjectPropertyAccessor.CreatePrivate(projectProperties, name);
			Assert.IsNotNull(testProperty, "The OANestedProjectProperty instance was not created successfully.");
			Assert.AreEqual(projectProperties.DTE, testProperty.DTE, "ProjectProperty Parent.DTE was initialized by unexpected value.");
		}
		/// <summary>
		/// The test for the Name property.
		/// </summary>
		[TestMethod()]
		public void NamePropertyTest()
		{
			string name = "Some random name";
			OANestedProjectProperty testProperty = VisualStudio_Project_Samples_OANestedProjectPropertyAccessor.CreatePrivate(projectProperties, name);
			Assert.IsNotNull(testProperty, "The OANestedProjectProperty instance was not created successfully.");
			Assert.AreEqual(name, testProperty.Name, "ProjectProperty Name was initialized by unexpected value.");
		}
		/// <summary>
		/// The test for the get_IndexValue() method.
		/// </summary>
		/// <remarks>Probably method get_IndexValue() is uncompleted.</remarks>
		[TestMethod()]
		public void get_IndexedValueTest()
		{
			string name = "Some Random Name";
			OANestedProjectProperty testProperty = VisualStudio_Project_Samples_OANestedProjectPropertyAccessor.CreatePrivate(projectProperties, name);
			Assert.IsNotNull(testProperty, "The OANestedProjectProperty instance was not created successfully.");

			object actualValue = testProperty.get_IndexedValue(null, null, null, null);
			Assert.IsNull(actualValue, "Method get_IndexValue was returned unexpected value.");
		}
		/// <summary>
		/// The test for the set_IndexValue() method.
		/// </summary>
		/// <remarks>Probably method set_IndexValue() is uncompleted.</remarks>
		[TestMethod()]
		public void set_IndexedValueTest()
		{
			string name = "Some Random Name";
			OANestedProjectProperty testProperty = VisualStudio_Project_Samples_OANestedProjectPropertyAccessor.CreatePrivate(projectProperties, name);
			Assert.IsNotNull(testProperty, "The OANestedProjectProperty instance was not created successfully.");

			// simply call this method
			testProperty.set_IndexedValue(null, null, null, null, null);
		}
		/// <summary>
		/// This method tests Object and dependent on Value properties.
		/// </summary>
		[TestMethod()]
		public void ObjectAndValuePropertiesTest()
		{
			string name = "SomeRandomName";
			OANestedProjectProperty testProperty = VisualStudio_Project_Samples_OANestedProjectPropertyAccessor.CreatePrivate(projectProperties, name);
			Assert.IsNotNull(testProperty, "The OANestedProjectProperty instance was not created successfully.");

			testProperty.Object = name;

			Assert.AreEqual((object)name, testProperty.Object, "ProjectProperty Object was initialized by unexpected value.");
			Assert.AreEqual((object)name, testProperty.Value, "ProjectProperty Value was initialized by unexpected value.");
			Assert.IsTrue(nestedProject.IsDirty, "After property changing IsDirty flag was not set to the false");
		}
		/// <summary>
		/// The test for the Value property in scenario when assigned to integer value.
		/// </summary>
		[TestMethod()]
		public void ValueAsIntegerPropertyTest()
		{
			string name = "SomeRandomName";
			OANestedProjectProperty testProperty = VisualStudio_Project_Samples_OANestedProjectPropertyAccessor.CreatePrivate(projectProperties, name);
			Assert.IsNotNull(testProperty, "The OANestedProjectProperty instance was not created successfully.");

			int expectedValue = 77777;
			testProperty.Value = expectedValue;

			Assert.AreEqual(expectedValue.ToString(), testProperty.Value, "ProjectProperty Value was initialized by unexpected value.");
			Assert.IsTrue(nestedProject.IsDirty, "After property changing IsDirty flag was not set to the false");
		}
		/// <summary>
		/// The test for the Value property in scenario when assigned to null referenced value.
		/// </summary>
		[TestMethod()]
		[ExpectedException(typeof(ArgumentNullException))]
		public void ValueAsNullPropertyTest()
		{
			string name = "SomeRandomName";
			OANestedProjectProperty testProperty = VisualStudio_Project_Samples_OANestedProjectPropertyAccessor.CreatePrivate(projectProperties, name);
			Assert.IsNotNull(testProperty, "The OANestedProjectProperty instance was not created successfully.");

			testProperty.Value = null;
		}
		/// <summary>
		/// The test for the let_Value method.
		/// </summary>
		[TestMethod()]
		public void let_ValueMethodTest()
		{
			string name = "SomeRandomName";
			OANestedProjectProperty testProperty = VisualStudio_Project_Samples_OANestedProjectPropertyAccessor.CreatePrivate(projectProperties, name);
			Assert.IsNotNull(testProperty, "The OANestedProjectProperty instance was not created successfully.");

			int expectedValue = 77777;
			testProperty.let_Value(expectedValue);

			Assert.AreEqual(expectedValue.ToString(), testProperty.Value, "ProjectProperty Value was initialized by unexpected value.");
			Assert.IsTrue(nestedProject.IsDirty, "After property changing IsDirty flag was not set to the false");
		}
		/// <summary>
		/// The test method for the NumIndices property.
		/// </summary>
		/// <remarks>This property always returns zero value.</remarks>
		[TestMethod()]
		public void NumIndicesPropertyTest()
		{
			string name = "Some Random Name";
			short expectedValue = 0;
			OANestedProjectProperty testProperty = VisualStudio_Project_Samples_OANestedProjectPropertyAccessor.CreatePrivate(projectProperties, name);
			Assert.IsNotNull(testProperty, "The OANestedProjectProperty instance was not created successfully.");
			Assert.AreEqual(expectedValue, testProperty.NumIndices, "Property NumIndices was returned unexpected value.");
		}
		#endregion Properties tests

		#endregion The tests for the OANestedProjectProperty class
	}
}
