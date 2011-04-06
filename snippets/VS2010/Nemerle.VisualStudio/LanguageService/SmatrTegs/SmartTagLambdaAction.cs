using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Language.Intellisense;
using System.Collections.ObjectModel;
using System.Windows.Media;
using Microsoft.VisualStudio.Text;

namespace SmartTagTest
{
	class SmartTagLambdaAction : ISmartTagAction
	{
		Action _action;

		public SmartTagLambdaAction(string displayText, Action action)
		{
			_action     = action;
			DisplayText = displayText;
			IsEnabled   = true;
		}

		#region ISmartTagAction Members

		public ReadOnlyCollection<SmartTagActionSet> ActionSets
		{
			get; set;
		}

		public string DisplayText { get; private set; }

		public ImageSource Icon { get; set; }

		public bool IsEnabled { get; set; }

		void ISmartTagAction.Invoke() { _action(); }

		#endregion
	}
}
