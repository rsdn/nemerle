using System.IO;

using Microsoft.VisualStudio.Project;

namespace Nemerle.VisualStudio.Project
{
	class NemerleTokenProcessor : TokenProcessor
	{
		public override void UntokenFile(string source, string destination)
		{
			// VladD2:
			// If file is located in subdirectory, we need to create this subdirectory.
			// TokenProcessor does not take it into account.
			//
			string destDir = Path.GetDirectoryName(destination);

			if (!Directory.Exists(destDir))
				Directory.CreateDirectory(destDir);

			base.UntokenFile(source, destination);
		}
	}
}
