using System.Runtime.InteropServices;

namespace ComInteropHelpers
{
	[ComVisible(true)]
	[Guid("FD22DD9F-592E-41b1-B6D6-135B78E10809")]
	[InterfaceType(ComInterfaceType.InterfaceIsIDispatch)]
	public interface IProjectConfigProperties
	{
		[DispId(1)] string OutputPath { get; set; }
	}

	[ComVisible(true)]
	[Guid("7C240194-660B-484a-ACE2-C03A9381A99C")]
	[ClassInterface(ClassInterfaceType.None)]
	public class ProjectConfigPropertiesComWrapper : IProjectConfigProperties
	{
		public ProjectConfigPropertiesComWrapper() { }

		public ProjectConfigPropertiesComWrapper(IProjectConfigProperties delegator)
		{ _delegator = delegator; }

		IProjectConfigProperties _delegator;

		string IProjectConfigProperties.OutputPath
		{
			get { return _delegator.OutputPath; }
			set { _delegator.OutputPath = value; }
		}
	}
}
