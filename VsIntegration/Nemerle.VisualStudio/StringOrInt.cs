using System;

namespace Nemerle.VisualStudio
{
	/// <summary>
	/// This class represents a piece of information that can be expressed either by string
	/// or integer. For example, one can reference DataTable.Columns by name or index.
	/// Or we may select a contract by its number (string) or ID (integer).
	/// </summary>
	public class StringOrInt
	{
		private string	_stringValue;
		private int		_intValue;
		
		/// <summary>
		/// Constructs new instance of <see cref="StringOrInt"/> out of <see cref="string"/>
		/// </summary>
		/// <param name="value">Value</param>
		public StringOrInt(string value)
		{
			_stringValue = value;
			_intValue = 0;
		}

		/// <summary>
		/// Constructs new instance of <see cref="StringOrInt"/> out of <see cref="int"/>
		/// </summary>
		/// <param name="value">Value</param>
		public StringOrInt(int value)
		{
			_stringValue = null;
			_intValue = value;
		}
		
		/// <summary>
		/// Gets value indicating if value of this instance is string.
		/// </summary>
		public bool IsString
		{
			get{ return _stringValue != null;}
		}
		
		/// <summary>
		/// Gets string value of instance. If value isn't string, <see cref="InvalidOperationException"/
		/// is thrown.
		/// </summary>
		public string StringValue
		{
			get
			{
				if(_stringValue == null)
					throw new InvalidOperationException("Value is specified by integer.");
				return _stringValue;
			}
		}

		/// <summary>
		/// Gets integer value of instance. If value isn't integer, <see cref="InvalidOperationException"/
		/// is thrown.
		/// </summary>
		public int IntValue
		{
			get
			{
				if(_stringValue != null)
					throw new InvalidOperationException("Value is specified by string.");
				return _intValue;
			}
		}
		
		/// <summary>
		/// Implicit conversion operator.
		/// </summary>
		/// <param name="str">String value to convert from.</param>
		/// <returns>New instance of <see cref="StringOrInt"/></returns>
		public static implicit operator StringOrInt(string str)
		{
			return new StringOrInt(str);
		}

		
		/// <summary>
		/// Implicit conversion operator.
		/// </summary>
		/// <param name="num">Integer value to convert from.</param>
		/// <returns>New instance of <see cref="StringOrInt"/></returns>
		public static implicit operator StringOrInt(int num)
		{
			return new StringOrInt(num);
		}

		
	}
}

