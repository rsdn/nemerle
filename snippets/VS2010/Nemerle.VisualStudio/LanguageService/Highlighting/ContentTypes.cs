using Microsoft.VisualStudio.Utilities;
using System.ComponentModel.Composition;

namespace Nemerle.VisualStudio.LanguageService
{
  internal static class ContentTypes
  {
    [Export, Name("nemerle"), BaseDefinition("code")]
    public static ContentTypeDefinition NemerleCode = null;

    [Export, FileExtension(NemerleConstants.FileExtension), ContentType("nemerle")]
    public static FileExtensionToContentTypeDefinition NemerleFileExtensionDefinition = null;
  }
}
