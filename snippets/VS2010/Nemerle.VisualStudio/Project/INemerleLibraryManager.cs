using System;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio.Shell.Interop;

namespace Nemerle.VisualStudio.Project
{
	/// <summary>
	/// This interface defines the service that finds Nemerle files inside 
	/// a hierarchy and builds the informations to expose to the class view or 
	/// object browser.
	/// </summary>
	[Guid(NemerleConstants.LibraryManagerServiceGuidString)]
	public interface INemerleLibraryManager
	{
		void RegisterHierarchy  (IVsHierarchy hierarchy);
		void UnregisterHierarchy(IVsHierarchy hierarchy);
	}
}
