using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.IO;
using System.Windows.Forms;
using Nemerle.Completion;

namespace RSDN.Nemerle.Utils
{
	public partial class Form1 : Form
	{
		private StringWriter console = new StringWriter();

		public Form1()
		{
			InitializeComponent();

			Console.SetError(console);
			Console.SetOut(console);

			Engine.Init();

			AppDomain.CurrentDomain.UnhandledException += CurrentDomain_UnhandledException;
		}

		void CurrentDomain_UnhandledException(object sender, UnhandledExceptionEventArgs e)
		{
			MessageBox.Show("UnhandledException", e.ExceptionObject.ToString());
		}

		private void btnParse_Click(object sender, EventArgs e)
		{
			txtErrors.Clear();
			txtGenerated.Clear();

			try
			{
				NemerleCodeDomProvider codeProvider = new NemerleCodeDomProvider();
				CodeCompileUnit unit = codeProvider.Parse(new StringReader(txtSource.Text));

				if (unit != null)
				{
					StringWriter wr = new StringWriter();
					CodeGeneratorOptions options = new CodeGeneratorOptions();
					codeProvider.GenerateCodeFromCompileUnit(unit, wr, options);
					txtGenerated.Text = wr.ToString();
				}
				else
					throw new InvalidOperationException("CodeCompileUnit is null.");
			}
			catch (Exception ex)
			{
				txtGenerated.AppendText("Error");
				txtGenerated.AppendText(Environment.NewLine);
				txtGenerated.AppendText(ex.ToString());
			}
			finally
			{
				txtErrors.Text = console.ToString();
			}
		}

		private void btnFillCode_Click(object sender, EventArgs e)
		{
			Stream stream = typeof(Form1).Assembly.GetManifestResourceStream("RSDN.Nemerle.Utils.Tests.Form1.n");
			using (StreamReader rd = new StreamReader(stream))
			{
				txtSource.Text = rd.ReadToEnd();
			}
		}

		private void btnCompile_Click(object sender, EventArgs e)
		{
			txtErrors.Clear();
			txtGenerated.Clear();

			try
			{
				NemerleCodeDomProvider codeProvider = new NemerleCodeDomProvider();
				CodeCompileUnit unit = codeProvider.Parse(new StringReader(txtSource.Text));

				if (unit != null)
				{
					CompilerParameters parameters = new CompilerParameters();
					parameters.ReferencedAssemblies.Add("Nemerle.dll");
					parameters.ReferencedAssemblies.Add("System.dll");
					parameters.ReferencedAssemblies.Add("System.Data.dll");
					parameters.ReferencedAssemblies.Add("System.Drawing.dll");
					parameters.ReferencedAssemblies.Add("System.Windows.Forms.dll");
					parameters.ReferencedAssemblies.Add("System.Xml.dll");
					parameters.GenerateExecutable = false;
					parameters.CompilerOptions = "/nowarn:1701,1702 /define:DEBUG;TRACE /debug+";
					CompilerResults results = codeProvider.CompileAssemblyFromDom(parameters, unit);

					if (results.Errors.Count > 0)
					{
						txtGenerated.AppendText("Errors:");

						foreach (CompilerError error in results.Errors)
						{
							txtGenerated.AppendText(Environment.NewLine);
							txtGenerated.AppendText(error.ToString());
						}
					}

					if (results.Output.Count > 0)
					{
						txtGenerated.AppendText("Output:");

						foreach (string line in results.Output)
						{
							txtGenerated.AppendText(Environment.NewLine);
							txtGenerated.AppendText(line);
						}
					}
				}
				else
					throw new InvalidOperationException("CodeCompileUnit is null.");
			}
			catch (Exception ex)
			{
				txtGenerated.AppendText("Error");
				txtGenerated.AppendText(Environment.NewLine);
				txtGenerated.AppendText(ex.ToString());
			}
			finally
			{
				txtErrors.Text = console.ToString();
			}
		}
	}
}