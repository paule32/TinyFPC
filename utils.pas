// ---------------------------------------------------------------------------------------
// Copyright(c) 2025 Jens Kallup
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy of this
// software and associated documentation files(the "Software"), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify,
// merge, publish, distribute, sublicense, and /or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to the following
// conditions :
// 
// The above copyright notice and this permission notice shall be included in all copies
// or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
// CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
// OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// ---------------------------------------------------------------------------------------

unit Utils;

{ Copyright (c) 2024 fibodevy https://github.com/fibodevy | License: LGPL }

{$mode ObjFPC}{$H+}

interface

uses Windows;

// returns the command-line parameter at index i as a unicodestring
function paramstr(i: integer): unicodestring;
// returns iftrue if expression is true, otherwise returns iffalse; works with any type T
generic function ifthen<T>(expression: boolean; iftrue, iffalse: T): T;
// returns iftrue if expression is true, otherwise returns iffalse; works with integer values
function ifthen(expression: boolean; iftrue, iffalse: ptrint): ptrint; inline;
// returns iftrue if expression is true, otherwise returns iffalse; works with strings
function ifthen(expression: boolean; iftrue, iffalse: string): string; inline;
// returns the current working directory as a unicodestring
function get_cwd: unicodestring;
// open file with given path, mode, and optional share parameter
function fopen(path: unicodestring; mode: char='o'; share: dword=dword(-1)): hwnd;
// flush file to hdd; ensure physical write to disk
function fflush(handle: hwnd): boolean; inline;
// close file associated with handle
function fclose(handle: hwnd): boolean; inline;
// move file cursor by 'move' bytes from specified mode position
function fseek(handle: hwnd; move: int64; mode: dword=FILE_CURRENT): boolean; inline;
// get file size in bytes
function fsize(handle: hwnd): qword; inline;
// get current cursor position within the file
function fpos(handle: hwnd): qword; inline;
// check if file cursor has reached the end of the file
function feof(handle: hwnd): boolean; inline;
// write buffer data to file; returns number of bytes written
function fwrite(handle: hwnd; data: pointer; len: dword): dword; inline;
// write string data to file; returns number of bytes written
function fwrite(handle: hwnd; data: string): dword; inline;
// alias for fwrite; writes buffer data to file
function fputs(handle: hwnd; data: pointer; len: dword): dword; inline;
// alias for fwrite; writes string data to file
function fputs(handle: hwnd; data: string): dword; inline;
// read 'len' bytes into buffer; returns bytes read
function fread(handle: hwnd; data: pointer; len: dword): dword; inline;
// read 'len' bytes; returns read bytes as a string
function fread(handle: hwnd; len: dword): string; inline;
// read until specified char or length limit; store in buffer
function fgets(handle: hwnd; data: pointer; &until: char=#0; limit: dword=1024*32): dword;
// read until specified char or length limit; return as string
function fgets(handle: hwnd; &until: char=#0; limit: dword=1024*32): string; inline;
// returns file size for a file handle
function filesize(h: hwnd): qword; inline;
// returns file size for a specified path
function filesize(path: unicodestring): int64;
// reads and returns contents of the file at path
function file_get_contents(path: unicodestring): string;
// writes data to a file with given length; appends if specified
function file_put_contents(path: unicodestring; data: pointer; len: dword; append: boolean=false): boolean;
// writes string content to file; appends if specified
function file_put_contents(path: unicodestring; content: string; append: boolean=false): boolean; inline;
// checks if path is a file
function is_file(path: unicodestring): boolean;
// checks if path is a directory
function is_dir(path: unicodestring): boolean;
// checks if file exists at path (returns true also for dirs)
function file_exists(path: unicodestring): boolean; inline;
// checks if directory exists at path
function dir_exists(path: unicodestring): boolean; inline;
// updates file timestamp or creates it unless dontcreate is true
function touch(path: unicodestring; dontcreate: boolean=false): boolean;

implementation

function paramstr(i: integer): unicodestring;
var                         
  cmdline: PWideChar;
  argc: integer;
  argv: PPWideChar;
begin
  cmdline := GetCommandLineW;
  argv := CommandLineToArgvW(cmdline, argc);
  if (i >= 0) and (i <= argc-1) then result := argv[i] else result := '';
end;

generic function ifthen<T>(expression: boolean; iftrue, iffalse: T): T;
begin
  if expression then result := iftrue else result := iffalse;
end;

function ifthen(expression: boolean; iftrue, iffalse: ptrint): ptrint; inline;
begin
  result := specialize ifthen<ptrint>(expression, iftrue, iffalse);
end;

function ifthen(expression: boolean; iftrue, iffalse: string): string; inline;
begin
  result := specialize ifthen<string>(expression, iftrue, iffalse);
end;

function get_cwd: unicodestring;
var
  d: dword;
  p: pointer;
begin
  result := '';
  d := GetCurrentDirectoryW(0, nil);
  if d = 0 then exit;
  getmem(p, d*2);
  d := GetCurrentDirectoryW(d, p);
  if d = 0 then exit;
  result := unicodestring(PWideChar(p));
  freemem(p);
end;

function fopen(path: unicodestring; mode: char='o'; share: dword=dword(-1)): hwnd;
var
  access, creation: dword;
begin
  result := 0;
  case mode of
    // for accessing a device
    'n': begin
      access := 0;
      creation := OPEN_EXISTING;
      if share = dword(-1) then share := FILE_SHARE_READ or FILE_SHARE_WRITE;
    end;
    'r': begin
      access := GENERIC_READ;
      creation := OPEN_EXISTING;
      if share = dword(-1) then share := FILE_SHARE_READ;
    end;
    'o': begin
      access := GENERIC_READ or GENERIC_WRITE;
      creation := OPEN_ALWAYS;
      if share = dword(-1) then share := FILE_SHARE_READ;
    end;
    'w': begin
      access := GENERIC_WRITE;
      creation := CREATE_ALWAYS;
      if share = dword(-1) then share := FILE_SHARE_READ;
    end;
    'a': begin
      access := GENERIC_WRITE;
      creation := OPEN_ALWAYS;
      if share = dword(-1) then share := FILE_SHARE_READ;
    end;
  else
    // invalid mode
    exit;
  end;
  result := CreateFileW(PWideChar(path), access, share, nil, creation, 0, 0);
  if result = INVALID_HANDLE_VALUE then exit(0);

  if mode = 'a' then begin
    SetFilePointerEx(result, 0, nil, FILE_END);
  end;
end;

function fflush(handle: hwnd): boolean; inline;
begin
  result := FlushFileBuffers(handle);
end;

function fclose(handle: hwnd): boolean; inline;
begin
  result := CloseHandle(handle);
end;

function fseek(handle: hwnd; move: int64; mode: dword=FILE_CURRENT): boolean; inline;
begin
  result := SetFilePointerEx(handle, move, nil, mode);
end;

function fsize(handle: hwnd): qword; inline;
begin
  if not GetFileSizeEx(handle, @result) then result := 0;
end;

function fpos(handle: hwnd): qword; inline;
begin
  if not SetFilePointerEx(handle, 0, @result, FILE_CURRENT) then result := 0;
end;

function feof(handle: hwnd): boolean; inline;
begin
  result := fpos(handle) = filesize(handle);
end;

function fwrite(handle: hwnd; data: pointer; len: dword): dword; inline;
begin
  WriteFile(handle, data, len, @result, nil);
end;

function fwrite(handle: hwnd; data: string): dword; inline;
begin
  result := fwrite(handle, @data[1], length(data));
end;

function fputs(handle: hwnd; data: pointer; len: dword): dword; inline;
begin
  result := fwrite(handle, data, len);
end;

function fputs(handle: hwnd; data: string): dword; inline;
begin
  result := fwrite(handle, @data[1], length(data));
end;

function fread(handle: hwnd; data: pointer; len: dword): dword; inline;
var
  d: dword;
begin
  result := 0;
  if ReadFile(handle, data, len, @d, nil) then result := d;
end;

function fread(handle: hwnd; len: dword): string; inline;
var
  i: int64;
begin
  setlength(result, len);
  i := fread(handle, @result[1], len);
  setlength(result, i);
end;

function fgets(handle: hwnd; data: pointer; &until: char=#0; limit: dword=1024*32): dword;
var
  s: string;
  p: integer;
  q: qword;
begin
  result := 0;
  fillchar(data^, limit, 0);
  q := fpos(handle);
  s := fread(handle, limit);
  if s = '' then exit;
  p := pos(&until, s);
  if p >= 1 then begin
    move(s[1], data^, p);
    fseek(handle, q+p, FILE_BEGIN);
    result := p;
    exit;
  end;
end;

function fgets(handle: hwnd; &until: char=#0; limit: dword=1024*32): string; inline;
var
  i: int64;
begin
  setlength(result, limit);
  i := fgets(handle, @result[1], &until, limit);
  setlength(result, i);
end;

function filesize(h: hwnd): qword; inline;
begin
  result := fsize(h);
end;

function filesize(path: unicodestring): int64;
var
  h: hwnd;
begin
  result := -1;
  h := fopen(path, 'r');
  if h = 0 then exit;
  result := fsize(h);
  fclose(h);
end;

function file_get_contents(path: unicodestring): string;
var
  h: hwnd;
  q: qword;
begin
  result := '';
  h := fopen(path, 'r');
  if h = 0 then exit;
  q := fsize(h);
  setlength(result, q);
  fread(h, @result[1], q);
  fclose(h);
end;

function file_put_contents(path: unicodestring; data: pointer; len: dword; append: boolean=false): boolean;
var
  h: hwnd;
begin
  result := false;
  // @@todo: create dir if not exist
  h := fopen(path, ifthen(append, 'a', 'w')[1]);
  if h = 0 then exit;
  fwrite(h, data, len);
  fflush(h);
  fclose(h);
  result := true;
end;

function file_put_contents(path: unicodestring; content: string; append: boolean=false): boolean; inline;
begin
  result := file_put_contents(path, @content[1], length(content), append);
end;

function is_file(path: unicodestring): boolean;
var
  d: dword;
  u: unicodestring;
begin
  // handles long paths (32 kB)
  if (length(path) >= 2) and not (path[2] = ':') and not (path[1] = '\'){\\share} then begin
    u := '\\?\'+get_cwd+'\'+path;
  end else u := path;
  d := GetFileAttributesW(@u[1]);
  result := not (d = INVALID_FILE_ATTRIBUTES) and not ((d and FILE_ATTRIBUTE_DIRECTORY) > 0);
end;

function is_dir(path: unicodestring): boolean;
var
  d: dword;
  u: unicodestring;
begin
  // handles long paths (32 kB)
  if (length(path) >= 2) and not (path[2] = ':') and not (path[1] = '\'){\\share} then begin
    u := '\\?\'+get_cwd+'\'+path;
  end else u := path;
  d := GetFileAttributesW(@u[1]);
  result := not (d = INVALID_FILE_ATTRIBUTES) and ((d and FILE_ATTRIBUTE_DIRECTORY) > 0);
end;

function file_exists(path: unicodestring): boolean; inline;
begin
  result := is_file(path) or is_dir(path);
end;

function dir_exists(path: unicodestring): boolean; inline;
begin
  result := is_dir(path);
end;

function touch(path: unicodestring; dontcreate: boolean=false): boolean;
var
  h: hwnd;
  st: SYSTEMTIME;
  ft: FILETIME;
begin
  result := false;
  if file_exists(path) then begin
    h := fopen(path);
    try
      GetSystemTime(@st);
      if SystemTimeToFileTime(@st, @ft) then begin
        result := SetFileTime(h, nil, nil, @ft);
      end;
    finally
      fclose(h);
    end;
  end else
  if not dontcreate then file_put_contents(path, '');
end;

end.
