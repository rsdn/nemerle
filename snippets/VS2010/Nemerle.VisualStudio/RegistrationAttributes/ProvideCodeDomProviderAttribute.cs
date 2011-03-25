using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Shell;
using System.Globalization;

namespace Nemerle.VisualStudio.RegistrationAttributes
{
	// Регистрирует в реестре CodeDomProvider. 
	// Запись в реестре используется методом  
	// Microsoft.VisualStudio.Designer.Shell.DesignerService.DesignerService.CreateProviderFromRegistry(...)
	class ProvideCodeDomProviderAttribute : RegistrationAttribute
	{
		private const string _valueName = "CodeDomProvider";

		public ProvideCodeDomProviderAttribute(Type factoryType, Type providerType)
		{
			if (factoryType == null)
				throw new ArgumentNullException("factoryType");

			if (providerType == null)
				throw new ArgumentNullException("providerType");
			
			_factoryType = factoryType;
			_codeDomProviderType = providerType;
		}

		private Type _codeDomProviderType = null;
		public Type CodeDomProviderType
		{
			get { return _codeDomProviderType; }
			set { _codeDomProviderType = value; }
		}

		private Type _factoryType;
		public Type FactoryType
		{
			get { return _factoryType; }
		}

		private string ProjectRegKey
		{
			get { return string.Format(CultureInfo.InvariantCulture, @"Projects\{0}", new object[] { this.FactoryType.GUID.ToString("B") }); }
		}
 
		public override void Register(RegistrationContext context)
		{
			if (_codeDomProviderType != null)
			{
				using (Key projectKey = context.CreateKey(ProjectRegKey))
				{
					var typeName = string.Format(CultureInfo.InvariantCulture, "{0}, {1}", _codeDomProviderType.FullName, _codeDomProviderType.Assembly.GetName().Name);

					projectKey.SetValue(_valueName, typeName);
				}
			}
		}

		public override void Unregister(RegistrationContext context)
		{
			context.RemoveValue(this.ProjectRegKey, _valueName);
			context.RemoveKeyIfEmpty(this.ProjectRegKey);
		}
	}
}
