/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.IO;

namespace Microsoft.VisualStudio.Project.UnitTests
{
	internal class TestDirectory : IDisposable
	{
		private string path;
		private string originalCurrentDir;

		public TestDirectory(string subPath)
		{
			if(string.IsNullOrEmpty(subPath))
			{
				throw new ArgumentNullException("subPath");
			}

			path = System.IO.Path.Combine(System.IO.Path.GetTempPath(), subPath);
			ClearDirectory(path);
			Directory.CreateDirectory(path);
			originalCurrentDir = Environment.CurrentDirectory;
		}

		public void Dispose()
		{
			if(!string.IsNullOrEmpty(path))
			{
				if(!string.IsNullOrEmpty(originalCurrentDir))
				{
					Environment.CurrentDirectory = originalCurrentDir;
				}
				ClearDirectory(path);
				path = null;
			}
		}

		public string Path
		{
			get { return path; }
		}

		private static void ClearDirectory(string directoryPath)
		{
			if(!Directory.Exists(directoryPath))
			{
				return;
			}

			foreach(string fileName in Directory.GetFiles(directoryPath))
			{
				File.SetAttributes(System.IO.Path.Combine(directoryPath, fileName), FileAttributes.Normal);
			}
			foreach(string subDir in Directory.GetDirectories(directoryPath))
			{
				ClearDirectory(System.IO.Path.Combine(directoryPath, subDir));
			}
			Directory.Delete(directoryPath, true);
		}
	}
}
