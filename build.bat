fpc -CD RTLLib.pas 
dlltool --input-def rtllib_dll.def --dllname rtllib.dll --output-lib librtllib_dll.a 
fpc test.pas 
