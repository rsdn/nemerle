using System;

namespace Nemerle.VisualStudio
{
	/// <summary>
	/// A helper class for checking arguments.
	/// </summary>
	public static class ErrorHelper
	{
		/// <summary>
		/// Throws ArgumentException when value is null or value isn't validType.
		/// </summary>
		/// <param name="value">test object.</param>
		/// <param name="validType">valid type for test object.</param>
		/// <param name="paramName">parameter name for ArgumentException.</param>
		public static void ThrowIsNullOrInvalidType(object value, Type validType, string paramName)
		{
			ThrowIsNull(value, paramName);

			Type valueType = value.GetType();

			if (!valueType.Equals(validType) && !validType.IsAssignableFrom(valueType))
				throw new ArgumentException(
					String.Format("Expected '{0}' type, but receives '{1}' type.", validType, valueType), paramName);
		}

		/// <summary>
		/// Throws ArgumentException when value is null or value isn't validType.
		/// </summary>
		/// <param name="value">The value.</param>
		/// <param name="validType">valid type for test object.</param>
		/// <param name="paramName">parameter name for ArgumentException.</param>
		public static void ThrowIsNullOrInvalidType(Type value, Type validType, string paramName)
		{
			ThrowIsNull(value, paramName);

			if (!value.Equals(validType) && !validType.IsAssignableFrom(value))
				throw new ArgumentException(
					String.Format("Expected '{0}' type, but receives '{1}' type.", validType, value), paramName);
		}

		/// <summary>
		/// Throws ArgumentException when string value is null or empty.
		/// </summary>
		/// <param name="value">string value.</param>
		/// <param name="paramName">parameter name for ArgumentException.</param>
		/// <param name="message">Message.</param>
		public static void ThrowIsNullOrEmpty(string value, string paramName, string message)
		{
			if (value == null)
				throw new ArgumentNullException(paramName);
			if (value == "")
				throw new ArgumentException(message, paramName);
		}

		/// <summary>
		/// Throws ArgumentException when string value is null or empty.
		/// </summary>
		/// <param name="value">string value.</param>
		/// <param name="paramName">parameter name for ArgumentException.</param>
		public static void ThrowIsNullOrEmpty(string value, string paramName)
		{
			ThrowIsNullOrEmpty(value, paramName, "The parameter '" + paramName + "' can't be empty.");
		}

		/// <summary>
		/// Throws ArgumentNullException when value is null.
		/// </summary>
		/// <param name="value">test object.</param>
		/// <param name="paramName">parameter name for ArgumentNullException.</param>
		public static void ThrowIsNull(object value, string paramName)
		{
			ThrowIsNull(value, paramName, null);
		}

		public static void ThrowIfFalse(bool condition, string paramName)
		{
			if (!condition)
				throw new ArgumentException(paramName);
		}

		/// <summary>
		/// Throws ArgumentNullException when value is null.
		/// </summary>
		/// <param name="value">test object.</param>
		/// <param name="paramName">parameter name for ArgumentNullException.</param>
		/// <param name="message">text message for ArgumentNullException.</param>
		public static void ThrowIsNull(object value, string paramName, string message)
		{
			if (value == null)
				if (message == null)
					throw new ArgumentNullException(paramName);
				else
					throw new ArgumentNullException(paramName, message);
		}

		public static void ThrowIfPathNullOrEmpty(string filePath, string paramName)
		{
			ThrowIsNullOrEmpty(filePath, paramName, "Path can't be empty.");
		}

		public static bool Failed(int hr)
		{
			return Microsoft.VisualStudio.ErrorHandler.Failed(hr);
		}

		public static bool Succeeded(int hr)
		{
			return Microsoft.VisualStudio.ErrorHandler.Succeeded(hr);
		}

		public static int ThrowOnFailure(int hr)
		{
			return Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(hr);
		}

		public static int ThrowOnFailure(int hr, params int[] expectedHRFailure)
		{
			return Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(hr, expectedHRFailure);
		}

	}
}
