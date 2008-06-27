using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.IO;
using NUnit.Framework;

using Nemerle.Compiler.Utils;
using Nemerle.VsIntegration.Tests.Properties;
using Nemerle.VsIntegration.Tests.Utils.Tests;
using System.Globalization;

namespace Nemerle.VsIntegration.Tests.Utils
{
	internal static class ParseHelper
	{
		/// <summary>
		/// Gets the default options for code generator.
		/// </summary>
		/// <value>The default options.</value>
		public static CodeGeneratorOptions DefaultOptions
		{
			get
			{
				CodeGeneratorOptions options = new CodeGeneratorOptions();
				options.VerbatimOrder = true;
				options.IndentString = "\t";
				options.BracingStyle = "C";
				return options;
			}
		}

		/// <summary>
		/// Gets the CodeDom provider.
		/// </summary>
		/// <value>The provider.</value>
		public static CodeDomProvider Provider
		{
			get { return new NemerleCodeDomProvider(); }
		}

		/// <summary>
		/// Parses the specified test.
		/// </summary>
		/// <param name="test">The test.</param>
		/// <returns></returns>
		public static CodeCompileUnit Parse(TestEnum test)
		{
			Stream codeStream = GetCodeStream(test);

			using (StreamReader rd = new StreamReader(codeStream))
				return Parse(rd);
		}

		/// <summary>
		/// Gets the code stream.
		/// </summary>
		/// <param name="test">The test.</param>
		/// <returns></returns>
		public static Stream GetCodeStream(TestEnum test)
		{
			string testName = test.ToString() + ".n";
			Stream codeStream = typeof (ParseHelper).Assembly.GetManifestResourceStream(test.GetType(), testName);

			Assert.IsNotNull(codeStream, 
				String.Format(CultureInfo.CurrentCulture, Settings.Default.NotFoundCodeStream, testName));
			return codeStream;
		}

		/// <summary>
		/// Parses the specified code.
		/// </summary>
		/// <param name="code">The code.</param>
		/// <returns></returns>
		public static CodeCompileUnit Parse(TextReader code)
		{
			return Provider.Parse(code);
		}

		#region CodeDom helpers

		/// <summary>
		/// Checks the namespace.
		/// </summary>
		/// <param name="codeNamespace">The code namespace.</param>
		/// <param name="name">The namespace name.</param>
		/// <param name="types">The count of types in the <paramref name="codeNamespace"/>.</param>
		/// <param name="usings">The namespace usings.</param>
		public static void CheckNamespace(CodeNamespace codeNamespace, string name, int types, params string[] usings)
		{
			Assert.AreEqual(name, codeNamespace.Name, Settings.Default.InvalidNamespace);

			Assert.AreEqual(types, codeNamespace.Types.Count,
				String.Format(CultureInfo.CurrentCulture, Settings.Default.WrongNumberTypes, name));

			if (usings != null)
			{
				Assert.AreEqual(usings.Length, codeNamespace.Imports.Count,
					String.Format(CultureInfo.CurrentCulture, Settings.Default.WrongNumberUsings, name));

				for (int i = 0; i < usings.Length; i++)
				{
					Assert.AreEqual(usings[i], codeNamespace.Imports[i].Namespace, 
						String.Format(CultureInfo.CurrentCulture, Settings.Default.InvalidUsing, name));
				}
			}
			else
			{
				Assert.AreEqual(0, codeNamespace.Imports.Count, "Wrong number of usings for " + name);
			}
		}

		/// <summary>
		/// Checks the class.
		/// </summary>
		/// <param name="codeType">The code type.</param>
		/// <param name="name">The class name.</param>
		/// <param name="modifiers">The class modifiers.</param>
		/// <param name="members">The count of members in the <paramref name="codeType"/>.</param>
		/// <param name="baseTypes">The class base types.</param>
		public static void CheckClass(CodeTypeDeclaration codeType, string name, MemberAttributes modifiers, int members, params string[] baseTypes)
		{
			Assert.IsTrue(codeType.IsClass, 
				String.Format(CultureInfo.CurrentCulture, Settings.Default.IsNotClass, codeType.Name));
			Assert.AreEqual(name, codeType.Name, Settings.Default.InvalidName);
			Assert.AreEqual(modifiers, codeType.Attributes,
				String.Format(CultureInfo.CurrentCulture, Settings.Default.InvalidAccessModifier, name));

			if (baseTypes != null)
			{
				Assert.AreEqual(baseTypes.Length, codeType.BaseTypes.Count,
					String.Format(CultureInfo.CurrentCulture, Settings.Default.WrongNumberBaseTypes, name));
				for (int i = 0; i < baseTypes.Length; i++)
				{
					Assert.AreEqual(baseTypes[i], codeType.BaseTypes[i].BaseType,
						String.Format(CultureInfo.CurrentCulture, Settings.Default.InvalidBaseType, name));
				}
			}
			else
			{
				Assert.AreEqual(0, codeType.BaseTypes.Count,
					String.Format(CultureInfo.CurrentCulture, Settings.Default.WrongNumberBaseTypes, name));
			}

			Assert.AreEqual(members, codeType.Members.Count,
				String.Format(CultureInfo.CurrentCulture, Settings.Default.WrongNumberMembers, name));
		}

		/// <summary>
		/// Checks the method.
		/// </summary>
		/// <param name="member">The code method.</param>
		/// <param name="name">The method name.</param>
		/// <param name="returnType">The method return type.</param>
		/// <param name="modifiers">The method modifiers.</param>
		/// <param name="statements">The count of parameters in the <paramref name="member"/>.</param>
		public static void CheckMethod(CodeTypeMember member, string name, string returnType, MemberAttributes modifiers, int statements)
		{
			CodeMemberMethod method = member as CodeMemberMethod;
			Assert.IsNotNull(method, 
				String.Format(CultureInfo.CurrentCulture, "{0} is not a method.", member.Name));
			Assert.AreEqual(name, method.Name, Settings.Default.InvalidName);
			Assert.AreEqual(returnType, method.ReturnType.BaseType,
				String.Format(CultureInfo.CurrentCulture, Settings.Default.InvalidReturnType, name));
			Assert.AreEqual(modifiers, method.Attributes,
				String.Format(CultureInfo.CurrentCulture, Settings.Default.InvalidAccessModifier, name));
			Assert.AreEqual(statements, method.Parameters.Count,
				String.Format(CultureInfo.CurrentCulture, Settings.Default.InvalidParametersCount, name));
		}

		/// <summary>
		/// Checks the constructor.
		/// </summary>
		/// <param name="member">The code constructor.</param>
		/// <param name="modifiers">The constructor modifiers.</param>
		/// <param name="statements">The count of parameters in the <paramref name="member"/>.</param>
		public static void CheckCtor(CodeTypeMember member, MemberAttributes modifiers, int statements)
		{
			CodeConstructor ctor = member as CodeConstructor;
			Assert.IsNotNull(ctor,
				String.Format(CultureInfo.CurrentCulture, "{0} is not a constructor.", member.Name));
			Assert.AreEqual(".ctor", ctor.Name, Settings.Default.InvalidName);
            Assert.AreEqual(modifiers, ctor.Attributes,
				String.Format(CultureInfo.CurrentCulture, Settings.Default.InvalidAccessModifier, ctor.Name));
            Assert.AreEqual(statements, ctor.Parameters.Count,
				String.Format(CultureInfo.CurrentCulture, Settings.Default.InvalidParametersCount, ctor.Name));
		}

		/// <summary>
		/// Checks the field.
		/// </summary>
		/// <param name="member">The code field.</param>
		/// <param name="name">The field name.</param>
		/// <param name="type">The field type.</param>
		/// <param name="modifiers">The field modifiers.</param>
		public static void CheckField(CodeTypeMember member, string name, string type, MemberAttributes modifiers)
		{
			CodeMemberField field = member as CodeMemberField;
			Assert.IsNotNull(field,
				String.Format(CultureInfo.CurrentCulture, "{0} is not a field.", member.Name));
			Assert.AreEqual(name, field.Name, Settings.Default.InvalidName);
			Assert.AreEqual(type, field.Type.BaseType, 
				String.Format(CultureInfo.CurrentCulture, Settings.Default.InvalidType, name));
            Assert.AreEqual(modifiers, field.Attributes,
				String.Format(CultureInfo.CurrentCulture, Settings.Default.InvalidAccessModifier, name));
		}

		/// <summary>
		/// Checks the compile unit.
		/// </summary>
		/// <param name="compileUnit">The compile unit.</param>
		/// <param name="namespaces">The count of namespaces in the <paramref name="compileUnit"/>.</param>
		public static void CheckCompileUnit(CodeCompileUnit compileUnit, int namespaces)
		{
			Assert.IsNotNull(compileUnit, Settings.Default.CompileUnitEmpty);
			Assert.AreEqual(namespaces, compileUnit.Namespaces.Count, Settings.Default.WrongNumberWorkspaces);
		}

		#endregion
	}
}