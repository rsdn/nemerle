namespace Nemerle.VisualStudio.Helpers
{
	public static class StringUtils
	{
		public static bool IsNullOrWhiteSpace(string value)
		{
			if (ReferenceEquals(null, value))
				return true;

			for (var i = 0; i < value.Length; ++i)
				if (!char.IsWhiteSpace(value[i]))
					return false;

			return true;
		}
	}
}