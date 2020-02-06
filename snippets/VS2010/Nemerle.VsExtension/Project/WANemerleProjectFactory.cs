using System.Runtime.InteropServices;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// This class is a 'fake' project factory that is used by WAP to register
	/// WAP specific information about Nemerle projects.
	/// </summary>
	[Guid(NemerleConstants.WAProjectFactoryGuidString)]
	public class WANemerleProjectFactory
	{
	}
}