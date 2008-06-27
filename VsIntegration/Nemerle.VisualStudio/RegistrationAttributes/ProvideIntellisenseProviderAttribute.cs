using System;
using System.Globalization;
using Microsoft.VisualStudio.Shell;

namespace Nemerle.VisualStudio.RegistrationAttributes
{
    /// <summary>
    /// This attribute adds a intellisense provider for a specific language 
    /// type. 
    /// For Example:
    ///   [HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\VisualStudio\8.0Exp\Languages\IntellisenseProviders\
    ///		[Custom_Provider]
    /// 
    /// </summary>
    [AttributeUsage(AttributeTargets.Class, AllowMultiple = true, Inherited = true)]
    public sealed class ProvideIntellisenseProviderAttribute : RegistrationAttribute {
        private Type _providerType;
        private string _providerName;
        private string _addItemLanguageName; //AddItemLanguageName
        private string _defaultExtension;//DefaultExtension
        private string _shortLanguageName; //ShortLanguageName
        private string _templateFolderName; //TemplateFolderName
        private string _additionalExtensions = null; //AdditionalExtensions

        /// <summary>
        /// Creates a new Intellisense provider registration attribute.
        /// </summary>
        /// <param name="providerGuid">Provider Guid</param>
        /// <param name="providerName">Provider Name</param>
        /// <param name="addItemLanguageName">Add Item Language</param>
        /// <param name="defaultExtension">Default extension</param>
        /// <param name="shortLanguageName">Short Language Name</param>
        /// <param name="templateFolderName">Template Folder Name</param>
        public ProvideIntellisenseProviderAttribute(Type provider, string providerName, string addItemLanguageName, string defaultExtension, string shortLanguageName, string templateFolderName) {
            if (null == provider) {
                throw new ArgumentNullException("provider");
            }
            if (string.IsNullOrEmpty(providerName)) {
                throw new ArgumentNullException("providerName");
            }
            _providerType = provider;
            _providerName = providerName;
            _addItemLanguageName = addItemLanguageName;
            _defaultExtension = defaultExtension;
            _shortLanguageName = shortLanguageName;
            _templateFolderName = templateFolderName;
        }

        /// <summary>
        /// Gets the Type of the intellisense provider.
        /// </summary>
        public Type Provider {
            get { return _providerType; }
        }

        /// <summary>
        /// Get the Guid representing the generator type
        /// </summary>
        public Guid ProviderGuid {
            get { return _providerType.GUID; }
        }

        /// <summary>
        /// Get the ProviderName
        /// </summary>
        public string ProviderName {
            get { return _providerName; }
        }

        /// <summary>
        /// Get item language
        /// </summary>
        public string AddItemLanguageName {
            get { return _addItemLanguageName; }
        }

        /// <summary>
        /// Get the Default extension
        /// </summary>
        public string DefaultExtension {
            get { return _defaultExtension; }
        }

        /// <summary>
        /// Get the short language name
        /// </summary>
        public string ShortLanguageName {
            get { return _shortLanguageName; }
        }

        /// <summary>
        /// Get the tempalte folder name
        /// </summary>
        public string TemplateFolderName {
            get { return _templateFolderName; }
        }

        /// <summary>
        /// Get/Set Additional extensions
        /// </summary>
        public string AdditionalExtensions {
            get { return _additionalExtensions; }
            set { _additionalExtensions = value; }
        }


        /// <summary>
        /// Property that gets the provider base key name
        /// </summary>
        private string ProviderRegKey {
            get { return string.Format(CultureInfo.InvariantCulture, @"Languages\IntellisenseProviders\{0}", ProviderName); }
        }
        /// <summary>
        ///     Called to register this attribute with the given context.  The context
        ///     contains the location where the registration inforomation should be placed.
        ///     It also contains other information such as the type being registered and path information.
        /// </summary>
        public override void Register(RegistrationContext context) {
            if (null == context) {
                throw new ArgumentNullException("context");
            }
            using (Key childKey = context.CreateKey(ProviderRegKey)) {
                childKey.SetValue("GUID", ProviderGuid.ToString("B"));
                childKey.SetValue("AddItemLanguageName", AddItemLanguageName);
                childKey.SetValue("DefaultExtension", DefaultExtension);
                childKey.SetValue("ShortLanguageName", ShortLanguageName);
                childKey.SetValue("TemplateFolderName", TemplateFolderName);
                if (!string.IsNullOrEmpty(AdditionalExtensions)) {
                    childKey.SetValue("AdditionalExtensions", AdditionalExtensions);
                }
            }
        }

        /// <summary>
        /// Unregister this file extension.
        /// </summary>
        /// <param name="context"></param>
        public override void Unregister(RegistrationContext context) {
            if (null != context) {
                context.RemoveKey(ProviderRegKey);
            }
        }
    }
}
