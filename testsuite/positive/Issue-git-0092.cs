// REFERENCE: System.Data

using System.Data.SqlTypes;
using System.Data;

public class P
{
	public readonly static IDataReader _dataReader = null;

	public static SqlString GetSqlString(int index)
	{
		// SqlString and string have a common subtype - IComparable. But we need must to use
		// implicite convertion instead, as their common supertype is not compatible with the type of return value.
		return _dataReader.IsDBNull(index)
			? SqlString.Null // type is SqlString
			: _dataReader.GetString(index); // type is string 
	}

	static void Main()
	{
	}
}