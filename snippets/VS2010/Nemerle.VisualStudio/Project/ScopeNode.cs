using System;
using System.Collections.Generic;

namespace Nemerle.VisualStudio.Project
{
	public class ScopeNode
	{
		private List<ScopeNode> _nested;

		public IList<ScopeNode> NestedScopes
		{
			get { return _nested; }
		}

		public virtual string Name
		{
			get { return ""; }
		}

		public virtual string Doc
		{
			get { return ""; }
		}

		//public virtual Location Start
		//{
		//  get
		//  {
		//	return Location.None;
		//  }
		//}
		//public virtual Location End
		//{
		//  get
		//  {
		//	return Location.None;
		//  }
		//}

		public void Add(ScopeNode node)
		{
			if (_nested == null)
				_nested = new List<ScopeNode>();

			_nested.Add(node);
		}
	}
}
