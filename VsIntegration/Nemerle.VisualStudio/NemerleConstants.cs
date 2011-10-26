namespace Nemerle.VisualStudio
{
	public static class NemerleConstants
	{
		// These constants are parts of PLK.
		// If you need to change it, you will have to request new PLK as well.
		//
  	public const string PackageGuidString = "CF7296F1-47E5-4915-83A0-8C44961F0981";
		public const string TypeLibGuidString = "0D502008-A831-4841-8022-2D7DDFC4E63C";
		public const string PLKProductName    = "Nemerle VS Integration";
#if DEBUG
		public const string VisualStudioRegistryRoot = "Software\\Microsoft\\VisualStudio\\9.0Exp";
		public const short  PLKResourceId     = 10508;
#else
		public const string VisualStudioRegistryRoot = "Software\\Microsoft\\VisualStudio\\9.0";
		public const short  PLKResourceId     = 10408;
#endif

		public const string PLKMinEdition     = "standard";
		public const string PLKCompanyName    = "NA";
		public const string PLKProductVersion = "1.0";

		public const string LanguageId        = "Nemerle";
		public const string LanguageName      = "Nemerle";
		public const string FileExtension     = ".n";
		public const string FileExtensionOnly = "n";
		public const string ProjectExtension  = "nproj";
		public const string ProductName       = "Nemerle Visual Studio Integration";
		public const string ProductDetails    = "Nemerle Visual Studio Integration\r\nVersion 1.0";

		// Attention, GUID LanguageServiceGuidString also has to be changed there:
		// Nemerle.VsIntegration\CodeSnippets\SnippetsIndex.xml
		//
		public const string LanguageServiceGuidString                = "EDCC3B79-0BAD-11DB-BC1A-00112FDE8B61";
		public const string LibraryManagerGuidString                 = "EDCC3B7A-0BAD-11DB-BC1A-00112FDE8B61";
		public const string LibraryManagerServiceGuidString          = "EDCC3B7B-0BAD-11DB-BC1A-00112FDE8B61";
		public const string ProjectNodeGuidString                    = "EDCC3B7C-0BAD-11DB-BC1A-00112FDE8B61";
		public const string OAFileItemGuidString                     = "EDCC3B7D-0BAD-11DB-BC1A-00112FDE8B61";
		public const string BuildPropertyPageGuidString              = "EDCC3B7E-0BAD-11DB-BC1A-00112FDE8B61";
		public const string EditorFactoryGuidString                  = "EDCC3B7F-0BAD-11DB-BC1A-00112FDE8B61";
		public const string FileNodePropertiesGuidString             = "EDCC3B80-0BAD-11DB-BC1A-00112FDE8B61";
		public const string LibraryGuidString                        = "EDCC3B81-0BAD-11DB-BC1A-00112FDE8B61";
		public const string ProjectCmdSetGuidString                  = "D6DDF8E8-9A9E-425C-AB18-7BBCC70A6489";
		public const string LanguageIntellisenseProviderGuidString   = "EDCC3B83-0BAD-11DB-BC1A-00112FDE8B61";
		public const string ProjectFactoryGuidString                 = "EDCC3B85-0BAD-11DB-BC1A-00112FDE8B61";
		public const string WAProjectFactoryGuidString               = "B3918BC1-983B-430a-BCEF-8F4CD6871C58";
		public const string FolderNodePropertiesGuidString           = "EDCC3B86-0BAD-11DB-BC1A-00112FDE8B61";
		public const string ProjectNodePropertiesGuidString          = "EDCC3B87-0BAD-11DB-BC1A-00112FDE8B61";
		public const string GeneralPropertyPageGuidString            = "EDCC3B88-0BAD-11DB-BC1A-00112FDE8B61";
		public const string DebugPropertyPageGuidString              = "EDCC3B89-0BAD-11DB-BC1A-00112FDE8B61";
		public const string CSCodeBehindEventBindingGuidString       = "349C5856-65DF-11DA-9384-00065B846F21";
		public const string WACodeBehindCodeGeneratorGuidString      = "349c5859-65df-11da-9384-00065b846f21";
		public const string NemerleWACodeBehindEventBindingGuidString = "99A7DAC8-2FEE-4E25-8339-DD716A811524";

		public const string IntellisenseProviderGuidString           = "A8B57672-DC5E-4915-A31A-B1C428B29FE1";

		public const string ProjectImageListName                     = "Nemerle.VisualStudio.Resources.NemerleImageList.bmp";

		public const string MacroReference                           = "MacroReference";
		public const string MacroProjectReference                    = "MacroProjectReference";

		/// <summary>
		/// Indexes to the embedded NemerleImageList.bmp image list.
		/// </summary>
		public static class ImageListIndex
		{
			public const int NemerleSource  = 0;
			public const int NemerleProject = 1;
			public const int NemerleForm = 2;
			public const int NemerleMacroReferences = 3;
		}
	}
}
