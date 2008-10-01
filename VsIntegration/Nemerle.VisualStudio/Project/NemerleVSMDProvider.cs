using System;
using Microsoft.VisualStudio.Designer.Interfaces;
using Nemerle.Compiler;

namespace Nemerle.VisualStudio.Project
{
	internal class NemerleVSMDProvider : IVSMDCodeDomProvider
	{
		private readonly NemerleFileNodeCodeDomProvider _provider;

		public NemerleVSMDProvider()
		{
		}

		public NemerleVSMDProvider(NemerleFileNodeCodeDomProvider provider)
		{
			_provider = provider;
		}

		#region IVSMDCodeDomProvider Members
		object IVSMDCodeDomProvider.CodeDomProvider
		{
			get { return _provider; }
		}
		#endregion
	}
}
