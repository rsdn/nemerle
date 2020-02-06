using System;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
using Microsoft.VisualStudio.Project;

namespace Nemerle.VisualStudio.Project.PropertyPages
{
	[CLSCompliant(false), ComVisible(true)]
	public abstract class PropertyPageBase : SettingsPage
	{
		#region Utils methods

		protected string GetPropertyValue(string propertyName)
		{
			var prop = ProjectMgr.BuildProject.GetPropertyValue(propertyName);

			if (prop == null)
				return "";

			return prop;
		}

		protected void SetPropertyValue(string propertyName, string value)
		{
			ProjectMgr.SetProjectProperty(propertyName, value);
		}


		Regex _replaceVarsRx = new Regex(@"\$\s*\(\s*(\w+)\s*\)",
			RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.Compiled);

		/// <summary>
		/// Expand property in value.
		/// </summary>
		/// <param name="value">Value whith $(...) splaces.</param>
		protected string Evaluate(string value)
		{
			var res = ProjectMgr.BuildProject.ExpandString(value);
			return res;
			//var props = ProjectMgr.BuildProject.EvaluatedProperties;
			//var res = _replaceVarsRx.Replace(value, match => {
			//  var prop = props[match.Groups[1].Value];
			//  return prop == null ? "" : prop.FinalValue;
			//});
			//return res;
		}

		#endregion
	}
}
