using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Project;
using System.Drawing;
using System.Windows.Forms;



namespace Nemerle.VS2010
{
    class NemerleProjectNode : ProjectNode
    {
        private Nemerle_VS2010Package package;

        private static ImageList imageList;

        static NemerleProjectNode()
        {
            imageList =        Utilities.GetImageList(
                typeof(NemerleProjectNode).Assembly.GetManifestResourceStream("Nemerle.VS2010.Resources.Console.bmp"));
        }

        internal static int imageIndex;
        public override int ImageIndex
        {
            get { return imageIndex; }
        }

        public NemerleProjectNode(Nemerle_VS2010Package package)
        {
            this.package = package;

            imageIndex = this.ImageHandler.ImageList.Images.Count;

            foreach (Image img in imageList.Images)
            {
                this.ImageHandler.AddImage(img);
            }

        }

        protected override Guid[] GetConfigurationIndependentPropertyPages()
        {
            Guid[] result = new Guid[1];
            result[0] = typeof(GeneralPropertyPage).GUID;
            return result;
        }
        protected override Guid[] GetPriorityProjectDesignerPages()
        {
            Guid[] result = new Guid[1];
            result[0] = typeof(GeneralPropertyPage).GUID;
            return result;
        }
        public override Guid ProjectGuid
        {
            get { return GuidList.guidNemerle_VS2010ProjectFactory; }
        }
        public override string ProjectType
        {
            get { return "NemerleProjectType"; }
        }

        public override void AddFileFromTemplate(
            string source, string target)
        {
            this.FileTemplateProcessor.UntokenFile(source, target);
            this.FileTemplateProcessor.Reset();
        }

    }
}
