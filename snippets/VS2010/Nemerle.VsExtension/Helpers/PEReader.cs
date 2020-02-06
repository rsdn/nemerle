using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

namespace Nemerle.VisualStudio.Helpers
{
	/// <summary>
	/// Written to make reading the information from a PE (Portable Executable)
	/// easier and simple.
	/// </summary>
	public class PEReader : IDisposable
	{
		private IMAGE_DATA_DIRECTORY[] dataDirectory = new IMAGE_DATA_DIRECTORY[0x10];
		private string[] directoryTypeStrings = new string[] { "Export Table", "Import Table", "Resource Table", "Exception Table", "Certificate Table", "Base Relocation Table", "Debug Directory", "Architecture Specific Data", "Global Pointer Register", "Thread Local Storage Table", "Load Configuration Table", "Bound Import Table", "Import Address Table", "Delay Load Import Descriptors", "COM Runtime Descriptor", "Reserved" };
		#region Constants

		public const uint IMAGE_DIRECTORY_ENTRY_BASERELOC = 5;
		public const uint IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT = 11;
		public const uint IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR = 14;
		public const uint IMAGE_DIRECTORY_ENTRY_COPYRIGHT = 7;
		public const uint IMAGE_DIRECTORY_ENTRY_DEBUG = 6;
		public const uint IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT = 13;
		public const uint IMAGE_DIRECTORY_ENTRY_EXCEPTION = 3;
		public const uint IMAGE_DIRECTORY_ENTRY_EXPORT = 0;
		public const uint IMAGE_DIRECTORY_ENTRY_GLOBALPTR = 8;
		public const uint IMAGE_DIRECTORY_ENTRY_IAT = 12;
		public const uint IMAGE_DIRECTORY_ENTRY_IMPORT = 1;
		public const uint IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG = 10;
		public const uint IMAGE_DIRECTORY_ENTRY_RESOURCE = 2;
		public const uint IMAGE_DIRECTORY_ENTRY_SECURITY = 4;
		public const uint IMAGE_DIRECTORY_ENTRY_TLS = 9;
		public const uint IMAGE_DLLCHARACTERISTICS_NO_BIND = 0x800;
		public const uint IMAGE_DLLCHARACTERISTICS_NO_SEH = 0x400;
		public const uint IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE = 0x8000;
		public const uint IMAGE_DLLCHARACTERISTICS_WDM_DRIVER = 0x2000;
		public const ushort IMAGE_FILE_32BIT_MACHINE = 0x100;
		public const ushort IMAGE_FILE_AGGRESIVE_WS_TRIM = 0x10;
		public const ushort IMAGE_FILE_BYTES_REVERSED_HI = 0x8000;
		public const ushort IMAGE_FILE_BYTES_REVERSED_LO = 0x80;
		public const ushort IMAGE_FILE_DEBUG_STRIPPED = 0x200;
		public const ushort IMAGE_FILE_DLL = 0x2000;
		public const ushort IMAGE_FILE_EXECUTABLE_IMAGE = 2;
		public const ushort IMAGE_FILE_LARGE_ADDRESS_AWARE = 0x20;
		public const ushort IMAGE_FILE_LINE_NUMS_STRIPPED = 4;
		public const ushort IMAGE_FILE_LOCAL_SYMS_STRIPPED = 8;
		public const ushort IMAGE_FILE_MACHINE_ALPHA = 0x184;
		public const ushort IMAGE_FILE_MACHINE_ALPHA64 = 0x284;
		public const ushort IMAGE_FILE_MACHINE_AM33 = 0x1d3;
		public const ushort IMAGE_FILE_MACHINE_AMD64 = 0x8664;
		public const ushort IMAGE_FILE_MACHINE_ARM = 0x1c0;
		public const ushort IMAGE_FILE_MACHINE_AXP64 = 0x284;
		public const ushort IMAGE_FILE_MACHINE_CEE = 0xc0ee;
		public const ushort IMAGE_FILE_MACHINE_CEF = 0xcef;
		public const ushort IMAGE_FILE_MACHINE_EBC = 0xebc;
		public const ushort IMAGE_FILE_MACHINE_I386 = 0x14c;
		public const ushort IMAGE_FILE_MACHINE_IA64 = 0x200;
		public const ushort IMAGE_FILE_MACHINE_M32R = 0x9041;
		public const ushort IMAGE_FILE_MACHINE_MIPS16 = 0x266;
		public const ushort IMAGE_FILE_MACHINE_MIPSFPU = 870;
		public const ushort IMAGE_FILE_MACHINE_MIPSFPU16 = 0x466;
		public const ushort IMAGE_FILE_MACHINE_POWERPC = 0x1f0;
		public const ushort IMAGE_FILE_MACHINE_POWERPCFP = 0x1f1;
		public const ushort IMAGE_FILE_MACHINE_R10000 = 360;
		public const ushort IMAGE_FILE_MACHINE_R3000 = 0x162;
		public const ushort IMAGE_FILE_MACHINE_R4000 = 0x166;
		public const ushort IMAGE_FILE_MACHINE_SH3 = 0x1a2;
		public const ushort IMAGE_FILE_MACHINE_SH3DSP = 0x1a3;
		public const ushort IMAGE_FILE_MACHINE_SH3E = 420;
		public const ushort IMAGE_FILE_MACHINE_SH4 = 0x1a6;
		public const ushort IMAGE_FILE_MACHINE_SH5 = 0x1a8;
		public const ushort IMAGE_FILE_MACHINE_THUMB = 450;
		public const ushort IMAGE_FILE_MACHINE_TRICORE = 0x520;
		public const ushort IMAGE_FILE_MACHINE_UNKNOWN = 0;
		public const ushort IMAGE_FILE_MACHINE_WCEMIPSV2 = 0x169;
		public const ushort IMAGE_FILE_NET_RUN_FROM_SWAP = 0x800;
		public const ushort IMAGE_FILE_RELOCS_STRIPPED = 1;
		public const ushort IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP = 0x400;
		public const ushort IMAGE_FILE_SYSTEM = 0x1000;
		public const ushort IMAGE_FILE_UP_SYSTEM_ONLY = 0x4000;
		public const int IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 0x10;
		public const int IMAGE_SIZEOF_FILE_HEADER = 20;
		public const uint IMAGE_SUBSYSTEM_EFI_APPLICATION = 10;
		public const uint IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER = 11;
		public const uint IMAGE_SUBSYSTEM_EFI_ROM = 13;
		public const uint IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER = 12;
		public const uint IMAGE_SUBSYSTEM_NATIVE = 1;
		public const uint IMAGE_SUBSYSTEM_NATIVE_WINDOWS = 8;
		public const uint IMAGE_SUBSYSTEM_OS2_CUI = 5;
		public const uint IMAGE_SUBSYSTEM_POSIX_CUI = 7;
		public const uint IMAGE_SUBSYSTEM_UNKNOWN = 0;
		public const uint IMAGE_SUBSYSTEM_WINDOWS_CE_GUI = 9;
		public const uint IMAGE_SUBSYSTEM_WINDOWS_CUI = 3;
		public const uint IMAGE_SUBSYSTEM_WINDOWS_GUI = 2;
		public const uint IMAGE_SUBSYSTEM_XBOX = 14;
		private FileStream inputExe;
		private BinaryReader inputReader;
		private bool isExeLoaded = false;
		private IMAGE_DOS_HEADER m_dosHeader;
		private IMAGE_FILE_HEADER m_fileHeader;
		private IMAGE_SECTION_HEADER[] m_sectionHeaders;
		private IMAGE_OPTIONAL_HEADER32 optionalHeader32;
		#endregion

		public void CloseExecutable()
		{
			if (this.isExeLoaded)
			{
				this.inputExe.Close();
			}
			this.isExeLoaded = false;
		}

		public bool DoesSectionExist(string sectionName)
		{
			int count = this.m_fileHeader.NumberOfSections - 1;
			for (int i = 0; i <= count; i++)
			{
				if (this.m_sectionHeaders[i].Name == sectionName)
				{
					return true;
				}
			}
			return false;
		}

		~PEReader()
		{
			Dispose();
		}

		public byte[] GetSectionDataByName(string sectionName)
		{
			int count = this.m_fileHeader.NumberOfSections - 1;
			for (int i = 0; i <= count; i++)
			{
				if (this.m_sectionHeaders[i].Name == sectionName)
				{
					this.inputExe.Position = this.m_sectionHeaders[i].PointerToRawData;
					return this.inputReader.ReadBytes((int)this.m_sectionHeaders[i].SizeOfRawData);
				}
			}
			return null;
		}

		public bool LoadExecutable(string fileName)
		{
			bool LoadExecutable;
			try
			{
				this.inputExe = new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.Read);
				this.inputReader = new BinaryReader(this.inputExe);
				this.ReadMZHeader();
				if (this.m_dosHeader.PEHeaderAddress > 0L)
				{
					this.inputExe.Position = this.m_dosHeader.PEHeaderAddress + 4L;
					this.ReadFileHeader();
					this.ReadSectionHeaders();
				}
				this.isExeLoaded = true;
				LoadExecutable = true;
			}
			catch (Exception exception1)
			{
				Exception ex = exception1;
				LoadExecutable = false;
				return LoadExecutable;
			}
			return LoadExecutable;
		}

		private bool ReadFileHeader()
		{
			bool ReadFileHeader;
			try
			{
				this.m_fileHeader.Machine = this.inputReader.ReadUInt16();
				this.m_fileHeader.NumberOfSections = this.inputReader.ReadUInt16();
				this.m_fileHeader.TimeDateStamp = this.inputReader.ReadUInt32();
				this.m_fileHeader.PointerToSymbolTable = this.inputReader.ReadUInt32();
				this.m_fileHeader.NumberOfSymbols = this.inputReader.ReadUInt32();
				this.m_fileHeader.SizeOfOptionalHeader = this.inputReader.ReadUInt16();
				this.m_fileHeader.Characteristics = this.inputReader.ReadUInt16();
				if (this.m_fileHeader.SizeOfOptionalHeader > 0)
				{
					return this.ReadPEHeader();
				}
				ReadFileHeader = true;
			}
			catch (Exception exception1)
			{
				Exception ex = exception1;
				ReadFileHeader = false;
				return ReadFileHeader;
			}
			return ReadFileHeader;
		}

		private bool ReadMZHeader()
		{
			bool ReadMZHeader;
			try
			{
				int someVar1;
				this.m_dosHeader.Magic = this.inputReader.ReadUInt16();
				this.m_dosHeader.SizeOfLastPage = this.inputReader.ReadUInt16();
				this.m_dosHeader.NumberOfPages = this.inputReader.ReadUInt16();
				this.m_dosHeader.Relocations = this.inputReader.ReadUInt16();
				this.m_dosHeader.SizeOfHeader = this.inputReader.ReadUInt16();
				this.m_dosHeader.MinimumExtraParagraphs = this.inputReader.ReadUInt16();
				this.m_dosHeader.MaximumExtraParagraphs = this.inputReader.ReadUInt16();
				this.m_dosHeader.InitialSSValue = this.inputReader.ReadUInt16();
				this.m_dosHeader.InitialSPValue = this.inputReader.ReadUInt16();
				this.m_dosHeader.Checksum = this.inputReader.ReadUInt16();
				this.m_dosHeader.InitialIPValue = this.inputReader.ReadUInt16();
				this.m_dosHeader.InitialCSValue = this.inputReader.ReadUInt16();
				this.m_dosHeader.RelocationTableAddress = this.inputReader.ReadUInt16();
				this.m_dosHeader.OverlayNumber = this.inputReader.ReadUInt16();
				int i = 0;
				do
				{
					this.inputReader.ReadUInt16();
					i++;
					someVar1 = 3;
				}
				while (i <= someVar1);
				this.m_dosHeader.OemIdentifier = this.inputReader.ReadUInt16();
				this.m_dosHeader.OemInformation = this.inputReader.ReadUInt16();
				int i1 = 0;
				do
				{
					this.inputReader.ReadUInt16();
					i1++;
					someVar1 = 9;
				}
				while (i1 <= someVar1);
				this.m_dosHeader.PEHeaderAddress = this.inputReader.ReadUInt32();
				ReadMZHeader = true;
			}
			catch (Exception exception1)
			{
				Exception ex = exception1;
				ReadMZHeader = false;
				return ReadMZHeader;
			}
			return ReadMZHeader;
		}

		private bool ReadPEHeader()
		{
			bool ReadPEHeader;
			try
			{
				this.optionalHeader32.Magic = this.inputReader.ReadUInt16();
				this.optionalHeader32.MajorLinkerVersion = this.inputReader.ReadByte();
				this.optionalHeader32.MinorLinkerVersion = this.inputReader.ReadByte();
				this.optionalHeader32.SizeOfCode = this.inputReader.ReadUInt32();
				this.optionalHeader32.SizeOfInitializedData = this.inputReader.ReadUInt32();
				this.optionalHeader32.SizeOfUninitializedData = this.inputReader.ReadUInt32();
				this.optionalHeader32.AddressOfEntryPoint = this.inputReader.ReadUInt32();
				this.optionalHeader32.BaseOfCode = this.inputReader.ReadUInt32();
				this.optionalHeader32.BaseOfData = this.inputReader.ReadUInt32();
				this.optionalHeader32.ImageBase = this.inputReader.ReadUInt32();
				this.optionalHeader32.SectionAlignment = this.inputReader.ReadUInt32();
				this.optionalHeader32.FileAlignment = this.inputReader.ReadUInt32();
				this.optionalHeader32.MajorOperatingSystemVersion = this.inputReader.ReadUInt16();
				this.optionalHeader32.MinorOperatingSystemVersion = this.inputReader.ReadUInt16();
				this.optionalHeader32.MajorImageVersion = this.inputReader.ReadUInt16();
				this.optionalHeader32.MinorImageVersion = this.inputReader.ReadUInt16();
				this.optionalHeader32.MajorSubsystemVersion = this.inputReader.ReadUInt16();
				this.optionalHeader32.MinorSubsystemVersion = this.inputReader.ReadUInt16();
				this.optionalHeader32.Win32VersionValue = this.inputReader.ReadUInt32();
				this.optionalHeader32.SizeOfImage = this.inputReader.ReadUInt32();
				this.optionalHeader32.SizeOfHeaders = this.inputReader.ReadUInt32();
				this.optionalHeader32.CheckSum = this.inputReader.ReadUInt32();
				this.optionalHeader32.Subsystem = this.inputReader.ReadUInt16();
				this.optionalHeader32.DllCharacteristics = this.inputReader.ReadUInt16();
				this.optionalHeader32.SizeOfStackReserve = this.inputReader.ReadUInt32();
				this.optionalHeader32.SizeOfStackCommit = this.inputReader.ReadUInt32();
				this.optionalHeader32.SizeOfHeapReserve = this.inputReader.ReadUInt32();
				this.optionalHeader32.SizeOfHeapCommit = this.inputReader.ReadUInt32();
				this.optionalHeader32.LoaderFlags = this.inputReader.ReadUInt32();
				this.optionalHeader32.NumberOfRvaAndSizes = this.inputReader.ReadUInt32();
				Console.WriteLine("Magic: {0}", this.optionalHeader32.Magic);
				Console.WriteLine("Number of Directories: {0}", this.optionalHeader32.ImageBase);
				int count = this.dataDirectory.Length - 1;
				for (int i = 0; i <= count; i++)
				{
					this.dataDirectory[i].Type = this.directoryTypeStrings[i];
					this.dataDirectory[i].VirtualAddress = this.inputReader.ReadUInt32();
					this.dataDirectory[i].Size = this.inputReader.ReadUInt32();
				}
				ReadPEHeader = true;
			}
			catch (Exception exception1)
			{
				Exception ex = exception1;
				ReadPEHeader = false;
				return ReadPEHeader;
			}
			return ReadPEHeader;
		}

		private bool ReadSectionHeaders()
		{
			bool ReadSectionHeaders;
			try
			{
				this.m_sectionHeaders = new IMAGE_SECTION_HEADER[(this.m_fileHeader.NumberOfSections - 1) + 1];
				int count = this.m_fileHeader.NumberOfSections - 1;
				for (int i = 0; i <= count; i++)
				{
					byte[] sectionNameBuffer = this.inputReader.ReadBytes(8);
					string sectionName = Encoding.ASCII.GetString(sectionNameBuffer);
					string sectionNameClean = sectionName.Substring(0, sectionName.IndexOf("\0"));
					this.m_sectionHeaders[i].Name = sectionNameClean;
					this.m_sectionHeaders[i].VirtualSize = this.inputReader.ReadUInt32();
					this.m_sectionHeaders[i].VirtualAddress = this.inputReader.ReadUInt32();
					this.m_sectionHeaders[i].SizeOfRawData = this.inputReader.ReadUInt32();
					this.m_sectionHeaders[i].PointerToRawData = this.inputReader.ReadUInt32();
					this.m_sectionHeaders[i].PointerToRelocations = this.inputReader.ReadUInt32();
					this.m_sectionHeaders[i].PointerToLinenumbers = this.inputReader.ReadUInt32();
					this.m_sectionHeaders[i].NumberOfRelocations = this.inputReader.ReadUInt16();
					this.m_sectionHeaders[i].NumberOfLinenumbers = this.inputReader.ReadUInt16();
					this.m_sectionHeaders[i].Characteristics = this.inputReader.ReadUInt32();
				}
				ReadSectionHeaders = true;
			}
			catch (Exception exception1)
			{
				Exception ex = exception1;
				ReadSectionHeaders = false;
				return ReadSectionHeaders;
			}
			return ReadSectionHeaders;
		}

		public IMAGE_DATA_DIRECTORY[] DataDirectories
		{
			get
			{
				return this.dataDirectory;
			}
		}

		public IMAGE_DOS_HEADER DOSHeader
		{
			get
			{
				return this.m_dosHeader;
			}
		}

		public IMAGE_FILE_HEADER FileHeader
		{
			get
			{
				return this.m_fileHeader;
			}
		}

		public IMAGE_OPTIONAL_HEADER32 PEHeader
		{
			get
			{
				return this.optionalHeader32;
			}
		}

		public IMAGE_SECTION_HEADER[] SectionHeaders
		{
			get
			{
				return this.m_sectionHeaders;
			}
		}

		[StructLayout(LayoutKind.Sequential)]
		public struct IMAGE_DATA_DIRECTORY
		{
			public string Type;
			public uint VirtualAddress;
			public uint Size;
		}

		[StructLayout(LayoutKind.Sequential)]
		public struct IMAGE_DOS_HEADER
		{
			public ushort Magic;
			public ushort SizeOfLastPage;
			public ushort NumberOfPages;
			public ushort Relocations;
			public ushort SizeOfHeader;
			public ushort MinimumExtraParagraphs;
			public ushort MaximumExtraParagraphs;
			public ushort InitialSSValue;
			public ushort InitialSPValue;
			public ushort Checksum;
			public ushort InitialIPValue;
			public ushort InitialCSValue;
			public ushort RelocationTableAddress;
			public ushort OverlayNumber;
			public ushort OemIdentifier;
			public ushort OemInformation;
			public uint PEHeaderAddress;
		}

		[StructLayout(LayoutKind.Sequential)]
		public struct IMAGE_FILE_HEADER
		{
			public ushort Machine;
			public ushort NumberOfSections;
			public uint TimeDateStamp;
			public uint PointerToSymbolTable;
			public uint NumberOfSymbols;
			public ushort SizeOfOptionalHeader;
			public ushort Characteristics;
		}

		[StructLayout(LayoutKind.Sequential)]
		public struct IMAGE_OPTIONAL_HEADER32
		{
			public ushort Magic;
			public byte MajorLinkerVersion;
			public byte MinorLinkerVersion;
			public uint SizeOfCode;
			public uint SizeOfInitializedData;
			public uint SizeOfUninitializedData;
			public uint AddressOfEntryPoint;
			public uint BaseOfCode;
			public uint BaseOfData;
			public uint ImageBase;
			public uint SectionAlignment;
			public uint FileAlignment;
			public ushort MajorOperatingSystemVersion;
			public ushort MinorOperatingSystemVersion;
			public ushort MajorImageVersion;
			public ushort MinorImageVersion;
			public ushort MajorSubsystemVersion;
			public ushort MinorSubsystemVersion;
			public uint Win32VersionValue;
			public uint SizeOfImage;
			public uint SizeOfHeaders;
			public uint CheckSum;
			public ushort Subsystem;
			public ushort DllCharacteristics;
			public uint SizeOfStackReserve;
			public uint SizeOfStackCommit;
			public uint SizeOfHeapReserve;
			public uint SizeOfHeapCommit;
			public uint LoaderFlags;
			public uint NumberOfRvaAndSizes;
			public PEReader.IMAGE_DATA_DIRECTORY[] DataDirectory;
		}

		[StructLayout(LayoutKind.Sequential)]
		public struct IMAGE_OPTIONAL_HEADER64
		{
			public ushort Magic;
			public byte MajorLinkerVersion;
			public byte MinorLinkerVersion;
			public uint SizeOfCode;
			public uint SizeOfInitializedData;
			public uint SizeOfUninitializedData;
			public uint AddressOfEntryPoint;
			public uint BaseOfCode;
			public ulong ImageBase;
			public uint SectionAlignment;
			public uint FileAlignment;
			public ushort MajorOperatingSystemVersion;
			public ushort MinorOperatingSystemVersion;
			public ushort MajorImageVersion;
			public ushort MinorImageVersion;
			public ushort MajorSubsystemVersion;
			public ushort MinorSubsystemVersion;
			public uint Win32VersionValue;
			public uint SizeOfImage;
			public uint SizeOfHeaders;
			public uint CheckSum;
			public ushort Subsystem;
			public ushort DllCharacteristics;
			public ulong SizeOfStackReserve;
			public ulong SizeOfStackCommit;
			public ulong SizeOfHeapReserve;
			public ulong SizeOfHeapCommit;
			public uint LoaderFlags;
			public uint NumberOfRvaAndSizes;
			public PEReader.IMAGE_DATA_DIRECTORY[] DataDirectory;
		}

		[StructLayout(LayoutKind.Sequential)]
		public struct IMAGE_SECTION_HEADER
		{
			public string Name;
			public uint PhysicalAddress;
			public uint VirtualSize;
			public uint VirtualAddress;
			public uint SizeOfRawData;
			public uint PointerToRawData;
			public uint PointerToRelocations;
			public uint PointerToLinenumbers;
			public ushort NumberOfRelocations;
			public ushort NumberOfLinenumbers;
			public uint Characteristics;
		}

		#region IDisposable Members

		public void Dispose()
		{
			if (this.isExeLoaded)
			{
				CloseExecutable();
				this.inputReader.Close();
				this.inputExe = null;
				this.inputReader = null;
			}
		}

		#endregion

		public static bool IsConsole(string path)
		{
			using (var peReader = new PEReader())
			{
				if (peReader.LoadExecutable(path))
				{
					switch ((uint)peReader.PEHeader.Subsystem)
					{
						case PEReader.IMAGE_SUBSYSTEM_WINDOWS_CUI:
						case PEReader.IMAGE_SUBSYSTEM_POSIX_CUI:
							return true;
						//case PEReader.IMAGE_SUBSYSTEM_WINDOWS_GUI:
						default:
							break;
					}
				}
				else
					throw new ApplicationException("The start program '" + path + "' not executable file!");
			}

			return false;
		}
	} 
}
