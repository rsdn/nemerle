using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using Microsoft.VisualStudio.TemplateWizard;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;

namespace Nemerle.VisualStudio.GUI.Wizards
{
	public class AddNewItemWizard_Macro : IWizard
	{
		#region IWizard Members

		void IWizard.BeforeOpeningFile(EnvDTE.ProjectItem projectItem)
		{
		}

		void IWizard.ProjectFinishedGenerating(EnvDTE.Project project)
		{
		}

		void IWizard.ProjectItemFinishedGenerating(EnvDTE.ProjectItem projectItem)
		{
		}

		void IWizard.RunFinished()
		{
		}

		void IWizard.RunStarted(object automationObject, Dictionary<string, string> replacementsDictionary, WizardRunKind runKind, object[] customParams)
		{
			var wizard = new AddNewItemWizard_Macro_Form();

			if (wizard.ShowDialog() != DialogResult.OK)
				throw Marshal.GetExceptionForHR(VSConstants.OLE_E_PROMPTSAVECANCELLED);

			wizard.MacroType.FillReplacementsDictionary(replacementsDictionary);
		}

		bool IWizard.ShouldAddProjectItem(string filePath)
		{
			return true;
		}

		#endregion
	}
}
