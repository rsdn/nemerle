/***************************************************************************

Copyright (c) Microsoft Corporation. All rights reserved.
This code is licensed under the Visual Studio SDK license terms.
THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.

***************************************************************************/

using System;
using System.IO;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Microsoft.VisualStudio.Project.UnitTests
{
	[TestClass]
	public class TokenProcessorTest
	{
		[TestMethod]
		public void UntokenFileBadParameters()
		{
			TokenProcessor processor = new TokenProcessor();
			Assert.IsTrue(Utilities.HasFunctionThrown<ArgumentNullException>(delegate { processor.UntokenFile(null, null); }));
			Assert.IsTrue(Utilities.HasFunctionThrown<ArgumentNullException>(delegate { processor.UntokenFile(@"C:\SomeFile", null); }));
			Assert.IsTrue(Utilities.HasFunctionThrown<ArgumentNullException>(delegate { processor.UntokenFile(null, @"C:\SomeFile"); }));
			Assert.IsTrue(Utilities.HasFunctionThrown<ArgumentNullException>(delegate { processor.UntokenFile(@"C:\SomeFile", string.Empty); }));
			Assert.IsTrue(Utilities.HasFunctionThrown<ArgumentNullException>(delegate { processor.UntokenFile(string.Empty, @"C:\SomeFile"); }));
			Assert.IsTrue(Utilities.HasFunctionThrown<ArgumentNullException>(delegate { processor.UntokenFile(string.Empty, string.Empty); }));

			using(TestDirectory dir = new TestDirectory(@"MPFProjectTests\UntokenFileBadParameters"))
			{
				string sourcePath = Path.Combine(dir.Path, "NotExistingSource");
				string destinationPath = Path.Combine(dir.Path, "NotExistingDestination");
				Assert.IsTrue(Utilities.HasFunctionThrown<FileNotFoundException>(delegate { processor.UntokenFile(sourcePath, destinationPath); }));
			}
		}

		[TestMethod]
		public void UntokenFilePreserveEncoding()
		{
			using(TestDirectory dir = new TestDirectory(@"MPFProjectTests\UntokenFilePreserveEncoding"))
			{
				TokenProcessor processor = new TokenProcessor();

				// Test 1: Unicode file.
				string sourcePath = Path.Combine(dir.Path, "UnicodeSource");
				string destinationPath = Path.Combine(dir.Path, "UnicodeDestination");
				Encoding expectedEncoding = Encoding.Unicode;
				File.WriteAllText(sourcePath, "Test", expectedEncoding);
				processor.UntokenFile(sourcePath, destinationPath);
				Encoding actualEncoding;
				using(StreamReader reader = new StreamReader(destinationPath, Encoding.ASCII, true))
				{
					// Read the content to force the encoding detection.
					reader.ReadToEnd();
					actualEncoding = reader.CurrentEncoding;
				}
				Assert.AreEqual<Encoding>(expectedEncoding, actualEncoding);

				// Test 2: UTF8 file.
				sourcePath = Path.Combine(dir.Path, "UTF8Source");
				destinationPath = Path.Combine(dir.Path, "UTF8Destination");
				expectedEncoding = Encoding.UTF8;
				File.WriteAllText(sourcePath, "Test", expectedEncoding);
				processor.UntokenFile(sourcePath, destinationPath);
				using(StreamReader reader = new StreamReader(destinationPath, Encoding.ASCII, true))
				{
					// Read the content to force the encoding detection.
					reader.ReadToEnd();
					actualEncoding = reader.CurrentEncoding;
				}
				Assert.AreEqual<Encoding>(expectedEncoding, actualEncoding);

				// Test 3: ASCII file.
				sourcePath = Path.Combine(dir.Path, "AsciiSource");
				destinationPath = Path.Combine(dir.Path, "AsciiDestination");
				expectedEncoding = Encoding.ASCII;
				File.WriteAllText(sourcePath, "Test", expectedEncoding);
				processor.UntokenFile(sourcePath, destinationPath);
				using(StreamReader reader = new StreamReader(destinationPath, Encoding.ASCII, true))
				{
					// Read the content to force the encoding detection.
					reader.ReadToEnd();
					actualEncoding = reader.CurrentEncoding;
				}
				Assert.AreEqual<Encoding>(expectedEncoding, actualEncoding);
			}
		}
	}
}
