using System;
using System.CodeDom;

using Nemerle.Completion2;
using NUnit.Framework;
using Nemerle.VsIntegration.Tests.Utils;
using Nemerle.VsIntegration.Tests.Utils.Tests;

[assembly: CLSCompliant(true)]

namespace Nemerle.VsIntegration.Tests
{
	[TestFixture]
	public class CodeDomParser
	{
		[SetUp]
		public void SetUp()
		{
			EngineCallbackStub callback = new EngineCallbackStub(
        new [] {
				"mscorlib",
				"System",
				"System.Data, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089",
				"System.Drawing, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a",
				"System.Windows.Forms, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" },
        new string[0]
			);

			IIdeEngine engine = EngineFactory.Create(callback, new TraceWriter(), false);
		}

		[Test]
		public void Form1()
		{
			CodeCompileUnit compileUnit = ParseHelper.Parse(TestEnum.Form1);
			
			ParseHelper.CheckCompileUnit(compileUnit, 1);

			CodeNamespace codeNamespace = compileUnit.Namespaces[0];

			ParseHelper.CheckNamespace(codeNamespace, "WindowsApplication", 1);

			CodeTypeDeclaration type = codeNamespace.Types[0];

			ParseHelper.CheckClass(type, "Form1", MemberAttributes.Public, 3, "System.Windows.Forms.Form");
			ParseHelper.CheckField(type.Members[0], "components", "System.ComponentModel.IContainer", MemberAttributes.Private);
			ParseHelper.CheckCtor(type.Members[1], MemberAttributes.Public, 0);
			ParseHelper.CheckMethod(type.Members[2], "InitializeComponent", "System.Void", MemberAttributes.Private, 0);

			// prints code
			ParseHelper.Provider.GenerateCodeFromCompileUnit(compileUnit, Console.Out, ParseHelper.DefaultOptions);
		}

		[Test]
		public void Form2()
		{
			CodeCompileUnit compileUnit = ParseHelper.Parse(TestEnum.Form2);
			
			ParseHelper.CheckCompileUnit(compileUnit, 1);

			CodeNamespace codeNamespace = compileUnit.Namespaces[0];

			ParseHelper.CheckNamespace(codeNamespace, "WindowsApplication", 1);

			CodeTypeDeclaration type = codeNamespace.Types[0];

			ParseHelper.CheckClass(type, "Form2", MemberAttributes.Public, 3, "System.Windows.Forms.Form");
			ParseHelper.CheckField(type.Members[0], "components", "System.ComponentModel.IContainer", MemberAttributes.Private);
			ParseHelper.CheckCtor(type.Members[1], MemberAttributes.Public, 0);
			ParseHelper.CheckMethod(type.Members[2], "InitializeComponent", "System.Void", MemberAttributes.Private, 0);

			// prints code
			ParseHelper.Provider.GenerateCodeFromCompileUnit(compileUnit, Console.Out, ParseHelper.DefaultOptions);
		}

		[Test]
		public void Form3()
		{
			CodeCompileUnit compileUnit = ParseHelper.Parse(TestEnum.Form3);
			
			ParseHelper.CheckCompileUnit(compileUnit, 1);

			CodeNamespace codeNamespace = compileUnit.Namespaces[0];

			ParseHelper.CheckNamespace(codeNamespace, "WindowsApplication", 1);

			CodeTypeDeclaration type = codeNamespace.Types[0];

			ParseHelper.CheckClass(type, "Form3", MemberAttributes.Public, 5, "System.Windows.Forms.Form");
			ParseHelper.CheckField(type.Members[0], "button1", "System.Windows.Forms.Button", MemberAttributes.FamilyOrAssembly);
			ParseHelper.CheckField(type.Members[1], "errorProvider1", "System.Windows.Forms.ErrorProvider", MemberAttributes.Family);
			ParseHelper.CheckField(type.Members[2], "components", "System.ComponentModel.IContainer", MemberAttributes.Public);
			ParseHelper.CheckCtor(type.Members[3], MemberAttributes.Public, 0);
			ParseHelper.CheckMethod(type.Members[4], "InitializeComponent", "System.Void", MemberAttributes.Private, 0);

			// prints code
			ParseHelper.Provider.GenerateCodeFromCompileUnit(compileUnit, Console.Out, ParseHelper.DefaultOptions);
		}

		[Test]
		public void Form4()
		{
			CodeCompileUnit compileUnit = ParseHelper.Parse(TestEnum.Form4);
			
			ParseHelper.CheckCompileUnit(compileUnit, 1);

			CodeNamespace codeNamespace = compileUnit.Namespaces[0];

			ParseHelper.CheckNamespace(codeNamespace, "WindowsApplication", 1);

			CodeTypeDeclaration type = codeNamespace.Types[0];

			ParseHelper.CheckClass(type, "Form4", MemberAttributes.Public, 5, "System.Windows.Forms.Form");
			ParseHelper.CheckField(type.Members[0], "comboBox1", "System.Windows.Forms.ComboBox", MemberAttributes.Private);
			ParseHelper.CheckField(type.Members[1], "treeView1", "System.Windows.Forms.TreeView", MemberAttributes.Private);
			ParseHelper.CheckField(type.Members[2], "components", "System.ComponentModel.IContainer", MemberAttributes.Private);
			ParseHelper.CheckCtor(type.Members[3], MemberAttributes.Public, 0);
			ParseHelper.CheckMethod(type.Members[4], "InitializeComponent", "System.Void", MemberAttributes.Private, 0);

			// prints code
			ParseHelper.Provider.GenerateCodeFromCompileUnit(compileUnit, Console.Out, ParseHelper.DefaultOptions);
		}

		[Test]
		public void Form5()
		{
			CodeCompileUnit compileUnit = ParseHelper.Parse(TestEnum.Form5);
			
			ParseHelper.CheckCompileUnit(compileUnit, 1);

			CodeNamespace codeNamespace = compileUnit.Namespaces[0];

			ParseHelper.CheckNamespace(codeNamespace, "WindowsApplication", 1);

			CodeTypeDeclaration type = codeNamespace.Types[0];

			ParseHelper.CheckClass(type, "Form5", MemberAttributes.Public, 5, "System.Windows.Forms.Form");
			ParseHelper.CheckField(type.Members[0], "button1", "System.Windows.Forms.Button", MemberAttributes.Private);
			ParseHelper.CheckCtor(type.Members[1], MemberAttributes.Public, 0);
			ParseHelper.CheckMethod(type.Members[2], "InitializeComponent", "System.Void", MemberAttributes.Private, 0);
			ParseHelper.CheckMethod(type.Members[3], "Form5_Load", "System.Void", MemberAttributes.Private, 2);
			ParseHelper.CheckMethod(type.Members[4], "button1_Click", "System.Void", MemberAttributes.Private, 2);

			// prints code
			ParseHelper.Provider.GenerateCodeFromCompileUnit(compileUnit, Console.Out, ParseHelper.DefaultOptions);
		}
	}
}
