using System;
using System.Diagnostics;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Security.Permissions;
using System.Resources;

[assembly: AssemblyTitle("Microsoft.VisualStudio.Project")]
[assembly: AssemblyDescription("MPF Implementation of VS Projects")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("Microsoft")]
[assembly: AssemblyProduct("Microsoft.VisualStudio.Project")]
[assembly: AssemblyCopyright("Copyright © Microsoft 2008")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]

[assembly: ComVisible(false)]
[assembly: CLSCompliant(false)]

[assembly: Guid("084954ec-af04-4ea3-b166-b1fced604dc8")]

[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyFileVersion("1.0.0.0")]

[assembly: SecurityPermissionAttribute(SecurityAction.RequestMinimum, Assertion = true)]
[assembly: IsolatedStorageFilePermissionAttribute(SecurityAction.RequestMinimum, Unrestricted = true)]
[assembly: UIPermissionAttribute(SecurityAction.RequestMinimum, Unrestricted = true)]
[assembly: PermissionSetAttribute(SecurityAction.RequestRefuse, Unrestricted = false)]
[assembly: FileIOPermissionAttribute(SecurityAction.RequestMinimum, Unrestricted = true)]
[assembly: ReflectionPermissionAttribute(SecurityAction.RequestMinimum, Unrestricted = true)]
[assembly: EventLogPermissionAttribute(SecurityAction.RequestMinimum, Unrestricted = true)]
[assembly: EnvironmentPermissionAttribute(SecurityAction.RequestMinimum, Unrestricted = true)]
[assembly: RegistryPermissionAttribute(SecurityAction.RequestRefuse)]
[assembly: NeutralResourcesLanguageAttribute("en")]
