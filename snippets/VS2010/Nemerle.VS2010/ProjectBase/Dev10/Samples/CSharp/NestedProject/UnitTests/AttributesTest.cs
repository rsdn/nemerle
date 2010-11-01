/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.ComponentModel;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using DescriptionAttribute = System.ComponentModel.DescriptionAttribute;

namespace Microsoft.VisualStudio.Project.Samples.NestedProject.UnitTests
{
	/// <summary>
	///This is a test class for VisualStudio.Project.Samples.NestedProject.ResourcesDescriptionAttribute and is intended
	///to contain all VisualStudio.Project.Samples.NestedProject.ResourcesDescriptionAttribute Unit Tests
	///</summary>
	[TestClass()]
	public class AttributesTest
	{
		#region Fields
		private TestContext testContextInstance;
		#endregion Fields

		#region Properties
		/// <summary>
		///Gets or sets the test context which provides
		///information about and functionality for the current test run.
		///</summary>
		public TestContext TestContext
		{
			get
			{
				return testContextInstance;
			}
			set
			{
				testContextInstance = value;
			}
		}
		#endregion Properties

		#region The tests for the ResourcesDescriptionAttribute class
		#region Constructors tests
		/// <summary>
		///The test for ResourcesDescriptionAttribute().
		///</summary>
		[TestMethod()]
		public void ConstructorTest()
		{
			string description = "Some Attribute Description";

			DescriptionAttribute target = VisualStudio_Project_Samples_ResourcesDescriptionAttributeAccessor.CreatePrivate(description);
			Assert.IsNotNull(target, "ResourcesDescriptionAttribute instance was not created successfully.");
		}
		#endregion Constructors tests

		#region Properties tests
		/// <summary>
		/// The test for Description property
		///</summary>
		[TestMethod()]
		public void DescriptionAttributeTest()
		{
			string description = "AssemblyName";
			DescriptionAttribute target = VisualStudio_Project_Samples_ResourcesDescriptionAttributeAccessor.CreatePrivate(description);
			Assert.IsNotNull(target, "ResourcesDescriptionAttribute instance was not created successfully.");

			VisualStudio_Project_Samples_ResourcesDescriptionAttributeAccessor accessor = new VisualStudio_Project_Samples_ResourcesDescriptionAttributeAccessor(target);

			Assert.IsNotNull(accessor.Description, "Description property value was uninitialized.");
		}
		#endregion Properties tests
		#endregion The tests for the ResourcesDescriptionAttribute class

		#region The tests for the ResourcesCategoryAttribute class
		#region Constructors tests
		/// <summary>
		///A test for ResourcesCategoryAttribute (string)
		///</summary>
		[TestMethod()]
		public void DefaultConstructorTest()
		{
			string category = "AssemblyName";
			CategoryAttribute target = VisualStudio_Project_Samples_ResourcesCategoryAttributeAccessor.CreatePrivate(category);
			Assert.IsNotNull(target, "CategoryAttribute instance was not created successfully.");
		}
		#endregion Constructors tests

		#region Mathod tests
		/// <summary>
		/// The test for GetLocalizedString() method.
		///</summary>
		[TestMethod()]
		public void GetLocalizedStringTest()
		{
			string category = "AssemblyName";
			CategoryAttribute target = VisualStudio_Project_Samples_ResourcesCategoryAttributeAccessor.CreatePrivate(category);
			VisualStudio_Project_Samples_ResourcesCategoryAttributeAccessor accessor = new VisualStudio_Project_Samples_ResourcesCategoryAttributeAccessor(target);

			string actual = accessor.GetLocalizedString(category);
			Assert.IsNotNull(actual, String.Format("GetLocalizedString() for {0} category was uninitialized.", category));
		}
		#endregion Mathod tests
		#endregion The tests for the ResourcesCategoryAttribute class

		#region The tests for the LocDisplayNameAttribute class
		#region Constructors tests
		/// <summary>
		/// The test for LocDisplayNameAttribute() default constructor.
		///</summary>
		[TestMethod()]
		public void LocDisplayNameDefaultConstructorTest()
		{
			string name = "AssemblyName";
			DisplayNameAttribute target = VisualStudio_Project_Samples_LocDisplayNameAttributeAccessor.CreatePrivate(name);
			Assert.IsNotNull(target, "DisplayNameAttribute instance was not created successfully.");
		}
		#endregion Constructors tests

		#region Properties tests
		/// <summary>
		/// The test for the DisplayName property.
		///</summary>
		[TestMethod()]
		public void DisplayNameTest()
		{
			string name = "AssemblyName";
			DisplayNameAttribute target = VisualStudio_Project_Samples_LocDisplayNameAttributeAccessor.CreatePrivate(name);

			Assert.IsNotNull(target.DisplayName, String.Format("DisplayName property for \"{0}\" attribute name was uninitialized.", name));
		}
		/// <summary>
		/// The test for the DisplayName property with not existing corresponding resource string.
		///</summary>
		[TestMethod()]
		public void DisplayNameWithNotExistingNameResourceStrTest()
		{
			string name = "Some not existing resource string name";
			DisplayNameAttribute target = VisualStudio_Project_Samples_LocDisplayNameAttributeAccessor.CreatePrivate(name);

			Assert.AreEqual(name, target.DisplayName, String.Format("DisplayName property for \"{0}\" attribute name was initialized by unexpected value.", name));
		}
		#endregion Properties tests
		#endregion The tests for the LocDisplayNameAttribute class
	}
}
