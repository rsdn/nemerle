using System;

using EnvDTE;

namespace Nemerle.VisualStudio.FileCodeModel
{
	internal abstract class CodeElementBase : CodeElement
	{
		DTE                      _dte;
		ProjectItem              _projectItem;
		readonly CodeElementBase _parent;
		TextPoint                _nullTextPoint = new NullTextPoint();

		public CodeElementBase(DTE dte, ProjectItem projectItem)
			: this(dte, projectItem, null)
		{
		}

		public CodeElementBase(DTE dte, ProjectItem projectItem,
			CodeElementBase parent)
		{
			_dte         = dte;
			_projectItem = projectItem;
			_parent      = parent;
		}

		public abstract string      Name { get; set; }
		public abstract vsCMElement Kind { get; }

		protected      CodeElement      Parent        { get { return _parent; } }
		public         DTE              DTE           { get { return _dte; } }
		public virtual string           FullName      { get { return Name; } }
		public         ProjectItem      ProjectItem   { get { return _projectItem; } }
		public virtual bool             IsCodeType    { get { return false; } }
		//SourceOutliner control (and other samples), rely on the vsCMInfoLocationProject value when choosing either display the element or not.
		public virtual vsCMInfoLocation InfoLocation  { get { return vsCMInfoLocation.vsCMInfoLocationProject; } }
		public virtual TextPoint        StartPoint    { get { return _nullTextPoint; } }
		public virtual TextPoint        EndPoint      { get { return _nullTextPoint; } }
		public virtual CodeElements     Collection    { get { throw new NotImplementedException(); } }
		public virtual CodeElements     Children      { get { return null; } }
		public         string           Language      { get { return NemerleConstants.LanguageServiceGuidString; } }
		public virtual object           ExtenderNames { get { return null; } }
		public virtual string           ExtenderCATID { get { return String.Empty; } }

		public virtual object get_Extender(string ExtenderName)
		{
			throw new NotImplementedException();
		}

		public virtual TextPoint GetStartPoint(vsCMPart Part)
		{
			throw new NotImplementedException();
		}

		public virtual TextPoint GetEndPoint(vsCMPart Part)
		{
			throw new NotImplementedException();
		}
	}
}