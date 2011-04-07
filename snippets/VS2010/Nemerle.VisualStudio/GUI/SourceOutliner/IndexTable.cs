/***************************************************************************

A derivative work based on the SourceOutliner Power Toy sample.

Copyright (c) 2006 Microsoft Corporation. All rights reserved.

***************************************************************************/

using EnvDTE;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Windows.Forms;

namespace Nemerle.VisualStudio.GUI.SourceOutliner
{
	/// <summary>
	/// Class that manages a single search criterion, specifically a code element type.
	/// </summary>
	public class SearchCriteria
	{
		private CodeElementType _elementFilter;

		/// <summary>
		/// Initializes a new instance of the SearchCriteria class.
		/// </summary>
		/// <param name="e">A CodeElementType object.</param>
		public SearchCriteria(CodeElementType e)
		{
			_elementFilter = e;
		}

		/// <summary>
		/// Gets or sets the code element type.
		/// </summary>
		/// <returns>A CodeElementType object.</returns>
		public CodeElementType ElementFilter
		{
			get
			{
				return (_elementFilter);
			}
			set
			{
				_elementFilter = value;
			}
		}
	}

	/// <summary>
	/// Class that manages sorting.
	/// </summary>
	class SortCriteria
	{
		/// <summary>
		/// An enumeration of available sorts.
		/// </summary>
		/// <remarks>
		/// ALPHA means to sort alphabetically by name. 
		/// LOCATION means to sort by the code element's place in the source file.
		/// </remarks>
		public enum SortType
		{
			ALPHA,
			LOCATION
		}

		private SortType _sortType;

		/// <summary>
		/// Initializes a new instance of the SortCriteria class.
		/// </summary>
		/// <param name="t">A SortType value.</param>
		public SortCriteria(SortType t)
		{
			_sortType = t;
		}

		/// <summary>
		/// Gets the sort type.
		/// </summary>
		/// <returns>The SortType associated with the class.</returns>
		public SortType SortOrder
		{
			get
			{
				return (_sortType);
			}
		}
	}

	/// <summary>
	/// Class that manages an index table for a CodeElementWrapperArray.
	/// </summary>
	class CodeElementWrapperArrayIndexTable : ICloneable
	{
		protected List<int> _list;
		protected CodeElementWrapperArray _codeElementWrapperArray;

		/// <summary>
		/// Initializes a new instance of the CodeElementWrapperArrayIndexTable class.
		/// </summary>
		/// <param name="a">A CodeElementWrapperArray object.</param>
		public CodeElementWrapperArrayIndexTable(CodeElementWrapperArray a)
		{
			_list = new List<int>();
			_codeElementWrapperArray = a;
		}

		/// <summary>
		/// Initializes a new instance of the CodeElementWrapperArrayIndexTable
		/// class and copies another instance to it.
		/// </summary>
		/// <param name="copy">The CodeElementWrapperArrayIndexTable object to copy.</param>
		public CodeElementWrapperArrayIndexTable(CodeElementWrapperArrayIndexTable copy)
		{
			_list = new List<int>(copy._list);
			_codeElementWrapperArray = copy._codeElementWrapperArray;
		}

		/// <summary>
		/// Adds an index to the index list.
		/// </summary>
		/// <param name="index">The index to add.</param>
		public void Add(int index)
		{
			_list.Add(index);
		}

		/// <summary>
		/// Returns the count of the number of indexes in the list.
		/// </summary>
		/// <returns>The index count.</returns>
		public int Size()
		{
			return (_list.Count);
		}

		/// <summary>
		/// Gets the CodeElementWrapper object at the indicated index.
		/// </summary>
		/// <param name="i">The desired index.</param>
		/// <returns>A CodeElementWrapper object.</returns>
		public CodeElementWrapper this[int i]
		{
			get
			{
				return (_codeElementWrapperArray[_list[i]]);
			}
		}

		/// <summary>
		/// Gets the count of the number of indexes in the list.
		/// </summary>
		/// <returns>The index count.</returns>
		public int Count
		{
			get
			{
				return (_list.Count);
			}
		}

		/// <summary>
		///  A nested class for comparisons.
		/// </summary>
		class MyComparer : System.Collections.Generic.Comparer<int>
		{
			CodeElementWrapperArray _sortArray;
			SortCriteria _criteria;

			/// <summary>
			/// Initializes a new instance of the MyComparer class.
			/// </summary>
			/// <param name="a">A CodeElementWrapperArray object.</param>
			/// <param name="s">A SortCriteria object.</param>
			public MyComparer(CodeElementWrapperArray a, SortCriteria s)
			{
				_sortArray = a;
				_criteria = s;
			}

			/// <summary>
			/// Compares two strings in the array by ElementName.
			/// </summary>
			/// <param name="x">The index of the first string for the comparison.</param>
			/// <param name="y">The index of the second string for the comparison.</param>
			/// <returns>
			/// If the array is sorted by alpha, returns negative if x is less than y, else 0 if x and y are equal, else positive if x is greater than y.
			/// If the array is sorted by location, returns the distance between x and y.
			/// </returns>
			public override int Compare(int x, int y)
			{
				if (_criteria.SortOrder == SortCriteria.SortType.ALPHA)
				{
					return (String.Compare(_sortArray[x].ElementName, _sortArray[y].ElementName, true));

					// Don't use CodeElement here, because it may no longer be a valid object.
					// If you make a change to an element in the text window, the IDE fires
					// the "changed" event and gives you a new code element object, and the old code element 
					// object is no longer valid. In other words, do not use syntax like this:
					//   return (String.Compare(_sortArray[x].CodeElement.Name, _sortArray[y].CodeElement.Name, true));
					//
					// Don't use UniqueElementID, which works in C# but in VB has an internal meaning
					// that has nothing to do with sort order.  In other words, do not use syntax like this:
					//   return (String.Compare(_sortArray[x].UniqueElementId, _sortArray[y].UniqueElementId, true));
				}
				else
				{
					return (_sortArray[x].Location - _sortArray[y].Location);
				}
			}
		}

		/// <summary>
		/// Sorts the index table using a sort criterion.
		/// </summary>
		/// <param name="crit">The sort criterion.</param>
		public void Sort(SortCriteria crit)
		{
			MyComparer myComparer = new MyComparer(_codeElementWrapperArray, crit);
			_list.Sort(myComparer);
		}

		/// <summary>
		/// Copies this object to a new instance.
		/// </summary>
		/// <returns>A new instance that is a copy of this one.</returns>
		public object Clone()
		{
			return (new CodeElementWrapperArrayIndexTable(this));
		}
	}

	/// <summary>
	/// Class that provides an index table for CodeElementWrapperArray objects.
	/// </summary>
	/// <remarks>
	/// Provides access to a code element that is stored in the CodeElementWrapperArrayIndexTable.
	/// </remarks>
	class IndexTable : IEnumerable<CodeElementWrapper>
	{
		// The elements of this table are kept sorted alphabetically.
		protected CodeElementWrapperArrayIndexTable indexTable;
		// Index of the first code element that matches the user's typed input.
		protected int startIndex;
		// Index of the last code element that matches the user's typed input.
		protected int endIndex;

		protected String prevFilterString;

		/// <summary>
		/// Initializes a new instance of the IndexTable class.
		/// </summary>
		/// <param name="indexTable">A CodeElementWrapperArrayIndexTable object.</param>
		protected IndexTable(CodeElementWrapperArrayIndexTable indexTable)
		{
			Debug.Assert(indexTable != null);

			this.indexTable = indexTable;
		}

		/// <summary>
		/// Resets the pointers that delineate the starting and ending 
		/// code elements that match the user's typed input.
		/// </summary>
		/// <remarks>
		/// The pointers are reset to the beginning and end of the list.
		/// </remarks>
		protected void Reset()
		{
			startIndex = 0;
			endIndex = indexTable.Count - 1;
		}

		/// <summary>
		/// Sets the pointers that delineate the starting and ending 
		/// code elements that match the user's typed input.
		/// </summary>
		/// <param name="str">The user's typed input.</param>
		/// <remarks>
		/// The list is kept in alphabetical order, so if the user types "t",
		/// this sets startIndex to the first code element whose name begins with "t",
		/// and sets endIndex to the last code element whose name begins with "t".
		/// </remarks>
		protected void FilterTable(string str)
		{
			// Start over if the typed input is not additional letters added to the end of str.
			if (prevFilterString != null && !str.StartsWith(prevFilterString))
			{
				Reset();
			}

			if (str == String.Empty)
			{
				Reset();
				return;
			}

			prevFilterString = str;

			int i;
			for (i = startIndex; i <= endIndex; i++)
			{
				if (indexTable[i].CodeElement.Name.StartsWith(str, StringComparison.CurrentCultureIgnoreCase))
				{
					startIndex = i;
					break;
				}
			}

			// If no match found, then display nothing.
			if (i > endIndex)
			{
				startIndex = 0;
				endIndex = -1;
				return;
			}

			// Now, find the end.
			for (i = startIndex + 1; i <= endIndex; i++)
			{
				if (!indexTable[i].CodeElement.Name.StartsWith(str, StringComparison.CurrentCultureIgnoreCase))
				{
					endIndex = i - 1;
					break;
				}
			}
		}

		/// <summary>
		/// Populates a TreeView control using the code elements 
		/// delineated by the current filter string. 
		/// </summary>
		/// <param name="tv">The TreeView to populate, normally the FilterView tree.</param>
		/// <remarks>
		/// The TreeView is cleared first. 
		/// FilterTable() must be called before calling this method. 
		/// </remarks>
		public void AddEntriesToTreeControl(TreeView tv)
		{
			tv.Nodes.Clear();
			tv.BeginUpdate();

			for (int i = startIndex; i <= endIndex; i++)
			{
				tv.Nodes.Add(indexTable[i]);
			}
			tv.EndUpdate();
		}

		/// <summary>
		/// Gets the count of the number of indexes in the table.
		/// </summary>
		public int Size()
		{
			return (indexTable.Size());
		}

		/// <summary>
		/// Provides an iterator for enumeration.
		/// </summary>
		/// <returns>An indexTable element.</returns>
		public IEnumerator<CodeElementWrapper> GetEnumerator()
		{
			for (int i = startIndex; i <= endIndex; i++)
			{
				yield return (indexTable[i]);
			}
		}

		/// <summary>
		/// Provides an enumerator.
		/// </summary>
		/// <returns>An IEnumerator object that can be used to iterate through the collection.</returns>
		IEnumerator IEnumerable.GetEnumerator()
		{
			return (GetEnumerator());
		}
	}
}