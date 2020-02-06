using System.Collections.Generic;
using Microsoft.VisualStudio.Package;

using Nemerle.Completion2;
using Nemerle.Compiler;

namespace Nemerle.VisualStudio.LanguageService
{
	/// <summary>
	/// <see cref="MethodTipInfo"/> wrapper.
	/// </summary>
	public class NemerleMethods : Methods
	{
		private readonly MethodTipInfo _info;

		public NemerleMethods(MethodTipInfo info)
		{
			_info = info;
		}

		///<summary>
		/// Gets the character that separates parameters in a method's parameter list.
		///</summary>
		public override string Delimiter    { get { return ", ";  } }

		///<summary>
		/// Gets whether a method's return type comes before or after the method signature.
		///</summary>
		public override bool   TypePrefixed { get { return false; } }

		///<summary>
		///Gets the string to place before the return type of a method.
		///</summary>
		public override string TypePrefix   { get { return ": ";  } }

		///<summary>
		/// Gets the number of overloaded method signatures represented in this collection.
		///</summary>
		///<returns> The number of signatures in the collection. </returns>
		public override int GetCount()
		{
			return _info.GetCount();
		}

		///<summary>
		/// Gets the index of the initial method signature to show.
		///</summary>
		///<returns> The index of the first method signature to show
		/// the user when the IntelliSense method tip is displayed.</returns>
		public override int DefaultMethod
		{
			get { return _info.DefaultMethod; }
		}

		///<summary>
		/// Gets the description of the specified method signature.
		///</summary>
		///<param name="index">[in] An index into the internal list
		/// to the desired method signature.</param>
		///<returns> The description of the specified method signature,
		/// or null if the method signature does not exist.</returns>
		public override string GetDescription(int index)
		{
			return _info.GetDescription(index);
		}

		///<summary>
		/// Gets the return type of the specified method signature.
		///</summary>
		///<param name="index">[in] An index into the list of method signatures.</param>
		///<returns> The return type of the specified method signature, or null.</returns>
		public override string GetType(int index)
		{
			return _info.GetType(index);
		}

		///<summary>
		/// Gets the number of parameters on the specified method signature.
		///</summary>
		///<param name="index">[in] An index into the list of method signatures.</param>
		///<returns> The number of parameters on the specified
		/// method signature, or -1.</returns>
		public override int GetParameterCount(int index)
		{
			return _info.GetParameterCount(index);
		}

		///<summary>
		/// Gets information about the specified parameter on the specified method signature.
		///</summary>
		///<param name="index">[in] An index into the list of method signatures.</param>
		///<param name="parameter">[in] An index into the parameter list of the specified method signature.</param>
		///<param name="name">[out] Returns the name of the parameter.</param>
		///<param name="display">[out] Returns the parameter name and type formatted for display.</param>
		///<param name="description">[out] Returns a string containing a description of the parameter.</param>
		public override void GetParameterInfo(int index, int parameter,
			out string name, out string display, out string description)
		{
			var info = _info.GetParameterInfo(index, parameter);

			name        = info.Field0;
			display     = info.Field1;
			description = info.Field2;
		}

		///<summary>
		/// Gets the name of the specified method signature.
		///</summary>
		///<param name="index">[in] The index of the method whose name is to be returned.</param>
		///<returns> The name of the specified method, or null.</returns>
		public override string GetName(int index)
		{
			return _info.GetName(index);
		}

		public Location       StartName       { get { return _info.StartName;       } }
		public Location       StartParameters { get { return _info.StartParameters; } }
		public List<Location> NextParameters  { get { return _info.NextParameters;  } }
		public Location       EndParameters   { get { return _info.EndParameters;   } }
	}
}
