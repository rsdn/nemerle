// NemerleStudio.cpp : Defines the entry point for the application.
//

#include "stdafx.h"
#include "NemerleStudio.h"

#define MAX_LOADSTRING 100

typedef int (__cdecl  *STARTFCN)(LPSTR, LPWSTR, int, GUID *, WCHAR *pszSettings);
typedef int (__cdecl  *SETUPFCN)(LPSTR, LPWSTR, GUID *);
typedef int (__cdecl  *REMOVEFCN)(LPSTR, LPWSTR);

#define wzAppPath L"NemerleStudio-1.0.0"

void ShowNoComponentError(HINSTANCE hInstance)
{
	WCHAR szErrorString[1000];
	WCHAR szCaption[1000];
	LoadStringW(hInstance, IDS_ERR_MSG_FATAL, szErrorString, 1000);
	LoadStringW(hInstance, IDS_ERR_FATAL_CAPTION, szCaption, 1000);

	MessageBoxW(NULL, szErrorString, szCaption, MB_OK|MB_ICONERROR);
}

// Helper function to convert a unicode string to an ANSI one
static char* W2A(const wchar_t* pwsz)
{
    if (NULL == pwsz)
        return NULL;

    // Get the size of the buffer needed to store the converted string.
    int ret = WideCharToMultiByte(CP_ACP, 0, pwsz, -1, NULL, 0, NULL, NULL);
    if (0 == ret)
    {
        return NULL;
    }

    // Get the size of the buffer.
    int bufferSize = ret + 1;
    if (bufferSize < ret)
        return NULL;

    // Allocate the buffer.
    char* ansiBuffer = new char[bufferSize];
    if (NULL == ansiBuffer)
        return NULL;

    if (0 == WideCharToMultiByte(CP_ACP, 0, pwsz, -1, ansiBuffer, bufferSize, NULL, NULL))
    {
        delete [] ansiBuffer;
        return NULL;
    }
    return ansiBuffer;
}

int APIENTRY _tWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow)
{
	UNREFERENCED_PARAMETER(hPrevInstance);
	UNREFERENCED_PARAMETER(lpCmdLine);

	int nRetVal = -1;
	WCHAR szExeFilePath[MAX_PATH];
	HKEY hKeyAppEnv90Hive = NULL;

	if(RegOpenKeyExW(HKEY_LOCAL_MACHINE, L"Software\\Microsoft\\AppEnv\\9.0", 0, KEY_READ, &hKeyAppEnv90Hive) == ERROR_SUCCESS)
	{
		DWORD dwType;
		DWORD dwSize = MAX_PATH;
		RegQueryValueExW(hKeyAppEnv90Hive, L"AppenvStubDLLInstallPath", NULL, &dwType, (LPBYTE)szExeFilePath, &dwSize);
		RegCloseKey(hKeyAppEnv90Hive);
	}

	if(GetFileAttributesW(szExeFilePath) == INVALID_FILE_ATTRIBUTES)
	{
		//If we cannot find it at a registered location, then try in the same directory as the application
		GetModuleFileNameW(NULL, szExeFilePath, MAX_PATH);
		WCHAR *pszStartOfFileName = wcsrchr(szExeFilePath, '\\');
		if(!pszStartOfFileName)
		{
			return -1;
		}
		*pszStartOfFileName = 0;
		wcscat_s(szExeFilePath, MAX_PATH, L"\\appenvstub.dll");

		if(GetFileAttributesW(szExeFilePath) == INVALID_FILE_ATTRIBUTES)
		{
			//If the file cannot be found in the same directory as the calling exe, then error out.
			ShowNoComponentError(hInstance);
			return -1;
		}
	}

	HMODULE hModStubDLL = LoadLibraryW(szExeFilePath);
	if(!hModStubDLL)
	{
		ShowNoComponentError(hInstance);
		return -1;
	}

	//Check to see if the /setup arg was passed. If so, then call the Setup method 
	//	to prepare the registry for the AppID.
	int nArgs = 0;
	bool fDoSetup = false;
	bool fDoRemove = false;
	LPWSTR *szArglist = CommandLineToArgvW(GetCommandLineW(), &nArgs);
	for(int i = 0 ; i < nArgs ; i++)
	{
		if(_wcsicmp(szArglist[i], L"/setup") == 0)
		{
			fDoSetup = true;
		}
		if(_wcsicmp(szArglist[i], L"/remove") == 0)
		{
			fDoRemove = true;
		}
	}
	LocalFree(szArglist);

	if(fDoSetup && fDoRemove)
	{
		//Cannot have both /setup and /remove on the command line at the same time.
		return -1;
	}


	if(fDoSetup)
	{
		WCHAR szExeFilePath[MAX_PATH];

		SETUPFCN Setup = (SETUPFCN)GetProcAddress(hModStubDLL, "Setup");
		if(!Setup)
		{
			ShowNoComponentError(hInstance);
			return -1;
		}

		nRetVal = Setup(W2A(lpCmdLine), wzAppPath, NULL);

		//Store the path to this program in the registry. This is necessary in the event that a service pack
		//  is released for Visual Studio Shell Isolated, or if an update to a package and corresponding 
		//  pkgdef file is released. If this information is removed, then updating for these changes may 
		//  not be possible.
		const wchar_t* szRegKeyPath = L"Software\\Microsoft\\AppEnv\\9.0\\Apps\\" wzAppPath;

		GetModuleFileNameW(NULL, szExeFilePath, MAX_PATH);
		HKEY hRegKeyExeFilePath = NULL;
		if(RegCreateKeyExW(HKEY_LOCAL_MACHINE, szRegKeyPath, 0, NULL, REG_OPTION_NON_VOLATILE, KEY_READ|KEY_WRITE, NULL, &hRegKeyExeFilePath, NULL) == ERROR_SUCCESS)
		{
			RegSetValueExW(hRegKeyExeFilePath, L"StubExePath", NULL, REG_SZ, (LPBYTE)szExeFilePath, (wcslen(szExeFilePath)+1)*sizeof(WCHAR));
			RegCloseKey(hRegKeyExeFilePath);
		}
	}
	else if(fDoRemove)
	{
		REMOVEFCN Remove = (REMOVEFCN)GetProcAddress(hModStubDLL, "Remove");
		if(!Remove)
		{
			ShowNoComponentError(hInstance);
			return -1;
		}

		nRetVal = Remove(W2A(lpCmdLine), wzAppPath);
	}
	else
	{
		STARTFCN Start = (STARTFCN)GetProcAddress(hModStubDLL, "Start");
		if(!Start)
		{
			ShowNoComponentError(hInstance);
			return -1;
		}

		nRetVal = Start(W2A(lpCmdLine), wzAppPath, nCmdShow, NULL, NULL);
	}

	FreeLibrary(hModStubDLL);

	return nRetVal;
}
