fpc -dDLLEXPORT -n -B -Os -CD RTLLib.pas 
dlltool --input-def rtllib_dll.def --dllname rtllib.dll --output-lib librtllib_dll.a 
fpc -dDLLIMPORT -n -B -Os test.pas 
strip test.exe
strip rtllib.dll
