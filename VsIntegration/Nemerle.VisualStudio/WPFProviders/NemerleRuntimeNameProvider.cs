using System;

using Microsoft.Windows.Design.Host;

namespace Nemerle.VisualStudio.WPFProviders
{
	class NemerleRuntimeNameProvider : RuntimeNameProvider
	{
		public override string CreateValidName(string proposal)
		{
			return proposal;
		}

		public override bool IsExistingName(string name)
		{
			//We will get uniqueness in the XAML file via the matchScope predicate.
			//In a more complete implementation, this method would verify that there isn't
			//a member in the code behind file with the given name.
			return false;
		}

		public override RuntimeNameFactory NameFactory
		{
			get { return new NemerleRuntimeNameFactory(); }
		}
	}
}
