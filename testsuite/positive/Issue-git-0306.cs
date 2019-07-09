// REFERENCE: System.Core
// REFERENCE: System.Data

using System;
using System.Collections.Generic;
using System.Data;
public class Program
{
	public IEnumerable<int> RunQuery(IDataContextInfo dataContextInfo)
	{
		try
		{
			yield return 42;
		}
		finally
		{
				dataContextInfo.DataContext.Dispose();
		}
	}

	public static IEnumerable<T> Map<T>(
		IEnumerable<IDataReader> data,
		object[] ps,
		Func<IDataReader, object[], T> mapper)
	{
		foreach (var dr in data)
			yield return mapper(dr, ps);
	}

	static void Main()
	{
	}
}

public interface IDataContext : IDisposable
{
	IDataReader ExecuteReader(object query);
	void ReleaseQuery(object query);
}
public interface IDataContextInfo
{
	IDataContext DataContext { get; }
	string ContextID { get; }
	bool DisposeContext { get; }
	IDataContextInfo Clone();
}

