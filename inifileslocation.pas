(* Lazarus+FPC 0.9.24+2.2.0 to 0.9.30+2.6.0. On Linux for ARM, PPC, SPARC, x86. *)

unit inifileslocation;

(* Various information derived from GECOS etc.                                  *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

TYPE	TSchemeSplitter= CLASS(TStringList)
			 PRIVATE
                           FUNCTION getDelimitedText2(CONST delim: STRING): STRING;
                           PROCEDURE setDelimitedText2(CONST delim: STRING; CONST str: STRING);
			 PROTECTED
                           FUNCTION GetColonText: STRING;
                           PROCEDURE SetColonText(CONST str: STRING);
                           FUNCTION GetCommaText: STRING;
                           PROCEDURE SetCommaText(CONST str: STRING);
                           FUNCTION GetDelimitedText: STRING;
                           PROCEDURE SetDelimitedText(CONST str: STRING);
                         PUBLIC
			   TextDelimiter: STRING;
                           PROPERTY ColonText: STRING READ GetColonText WRITE SetColonText;
                           PROPERTY CommaText: STRING READ GetCommaText WRITE SetCommaText;
                           PROPERTY DelimitedText: STRING READ GetDelimitedText WRITE SetDelimitedText;
			   CONSTRUCTOR Create;
			   DESTRUCTOR Destroy; OVERRIDE;
                         END;

(* This is a hacked variant of a TStringList to split a colon-separated list    *)
(* such as is used to specify a set of schemes. If the first character is a     *)
(* colon the resultant initial blank string is discarded, in all other          *)
(* circumstances blank strings (i.e. resulting from a pair of adjacent colons)  *)
(* are assumed to be significant.			                        *)

FUNCTION UserShortName: STRING;

(* Return the user name, suitable for password validation etc.                  *)

FUNCTION UserFullName: STRING;

(* Return the user or account name as available.                                *)

FUNCTION UserHome: STRING;

(* Return the user's home directory, blank if there's a problem.	        *)

(* If this is a unix-style system, i.e. including Linux, Solaris etc., return   *)
(* one or more fully-qualified per-user configuration file names applying       *)
(* appropriate conventions. If it is a non-unix system then return "best        *)
(* guesses" which might be displayed "greyed out", but try to preserve the      *)
(* number of items.                                                             *)
//
function UnixUserConfigNames(base: string= ''): TStringList;

(* If this is a Windows system, i.e. including NT, XP etc., return one or more  *)
(* fully-qualified per-user configuration file names applying appropriate       *)
(* conventions. If it is a non-unix system then return "best guesses" which     *)
(* might be displayed "greyed out", but try to preserve the number of items.    *)
//
function WindowsUserConfigNames(base: string= ''): TStringList;

(* Return per-user configuration file names for all supported operating         *)
(* systems. Those not relevant to the current system may be recognised by       *)
(* having an appended space, these are typically displayed "greyed out".        *)
//
function UserConfigNames(base: string= ''): TStringList;

(* Return the name of a single configuration file, appropriate to all users.    *)
//
function GlobalConfigName(base: string= ''): string;

(* Return the name of the "best" existing configuration file, optionally        *)
(* including the global as well as the user files and an offset (0 is the best  *)
(* file, 1 is the next-best and so on.                                          *)
//
function BestConfigName(base: string= ''; global: boolean= false; offset: integer= 0): string;

(* The name being looked at is not in a user's local directory, so is global.   *)
//
function ConfigNameIsGlobal(const name: string): boolean;

(* Test code to allow at least some of the above to be explored.                *)
//
procedure ConfigTestLocation(base: string= '');

(* Return the hostname, optionally fully qualified by the domain name. This is
  an alternative to Unix.GetHostName.
*)
function GetHostName(fullyQualified: boolean= false): string;

(* Return the domain name based on the content of local files etc., do not
  attempt any DNS lookup etc. that might fail due to local system configuration
  or network issues. This is an alternative to Unix.GetDomainName, which might
  only work if NIS/YP is configured.
*)
function GetDomainName(): string;

(* Clear any cached names or file locations, for example if the caller has just *)
(* had setuid root state revoked.                                               *)
//
procedure ClearCachedNames;


implementation

USES
(*$IFDEF UNIX   *)
        BaseUnix, Unix;
(*$ELSE         *)
        Windows;
(*$ENDIF        *)

(*$IFDEF WIN32 *)
CONST	env= 'USERPROFILE';
(*$ELSE		*)
CONST	env= 'HOME';
(*$ENDIF	*)


CONSTRUCTOR TSchemeSplitter.Create;

BEGIN
  INHERITED Create;
  Duplicates:= dupIgnore;
  TextDelimiter:= ','
END { TSchemeSplitter.Create } ;


DESTRUCTOR TSchemeSplitter.Destroy;

BEGIN
  INHERITED Destroy
END { TSchemeSplitter.Destroy } ;


FUNCTION TSchemeSplitter.getDelimitedText2(CONST delim: STRING): STRING;

(* This is similar to TStringList's CommaText handling except delimits          *)
(* strictly, i.e. does not attempt to break on spaces, escape quotes and so on.	*)

VAR	i: INTEGER;

BEGIN
  RESULT:= '';
  FOR i:= 0 TO Count - 1 DO
    RESULT:= RESULT + Strings[i] + delim;
  IF RESULT <> '' THEN
    SetLength(RESULT, Length(RESULT) - Length(delim))
END { TSchemeSplitter.getDelimitedText2 } ;


PROCEDURE TSchemeSplitter.setDelimitedText2(CONST delim: STRING; CONST str: STRING);

(* This is similar to TStringList's CommaText handling except delimits          *)
(* strictly, i.e. does not attempt to break on spaces, escape quotes and so on.	*)

VAR	scratch1, scratch2: STRING;
	p: INTEGER;

BEGIN
  Clear;
  scratch1:= str;
  WHILE TRUE DO BEGIN
    p:= Pos(delim, scratch1);
    IF p <= 0 THEN BEGIN
      Add(scratch1);
      BREAK
    END ELSE BEGIN
      scratch2:= scratch1;
      SetLength(scratch2, p - 1);
      System.Delete(scratch1, 1, p + (Length(delim) - 1));
      Add(scratch2)
    END
  END
END { TSchemeSplitter.setDelimitedText2 } ;


FUNCTION TSchemeSplitter.GetColonText: STRING;

(* This is similar to TStringList's CommaText handling except delimits	        *)
(* strictly, i.e. does not attempt to break on spaces, escape quotes and so on.	*)

BEGIN
  RESULT:= getDelimitedText2(':')
END { TSchemeSplitter.GetColonText } ;


PROCEDURE TSchemeSplitter.SetColonText(CONST str: STRING);

(* This is similar to TStringList's CommaText handling except delimits	        *)
(* strictly, i.e. does not attempt to break on spaces, escape quotes and so on.	*)

BEGIN
  setDelimitedText2(':', str)
END { TSchemeSplitter.SetColonText } ;


FUNCTION TSchemeSplitter.GetCommaText: STRING;

(* This is similar to TStringList's CommaText handling except delimits          *)
(* strictly, i.e. does not attempt to break on spaces, escape quotes and so on.	*)

BEGIN
  RESULT:= getDelimitedText2(',')
END { TSchemeSplitter.GetCommaText } ;


PROCEDURE TSchemeSplitter.SetCommaText(CONST str: STRING);

(* This is similar to TStringList's CommaText handling except delimits	        *)
(* strictly, i.e. does not attempt to break on spaces, escape quotes and so on.	*)

BEGIN
  setDelimitedText2(',', str)
END { TSchemeSplitter.SetCommaText } ;


FUNCTION TSchemeSplitter.GetDelimitedText: STRING;

(* This is similar to TStringList's CommaText handling except delimits	        *)
(* strictly, i.e. does not attempt to break on spaces, escape quotes and so on.	*)

BEGIN
  RESULT:= getDelimitedText2(TextDelimiter)
END { TSchemeSplitter.GetDelimitedText } ;


PROCEDURE TSchemeSplitter.SetDelimitedText(CONST str: STRING);

(* This is similar to TStringList's CommaText handling except delimits	        *)
(* strictly, i.e. does not attempt to break on spaces, escape quotes and so on.	*)

BEGIN
  setDelimitedText2(TextDelimiter, str)
END { TSchemeSplitter.SetDelimitedText } ;


  //############################################################################
 //      1         2         3         4         5         6         7         8
// 45678901234567890123456789012345678901234567890123456789012345678901234567890


(*$IFDEF DELPHI	*)

FUNCTION getEnvironmentVariable(CONST name: STRING): STRING;

(* For compatibility with FPC/Lazarus.					        *)

(* Liberated from List3264.                                                     *)

VAR	str: STRING[255];
	len: INTEGER;

BEGIN
  len:= 255;
  FillChar(str, 255, 0);
  RESULT:= '';
  TRY
    len:= Windows.GetEnvironmentVariable(PChar(name), @str[1], len);
    SetLength(str, len)
  EXCEPT
  END;
  RESULT:= Trim(str)
END { getEnvironmentVariable } ;

(*$ENDIF	*)


VAR     cachedUserShortName: STRING;


FUNCTION UserShortName: STRING;

(* Return the user name, suitable for password validation etc.                  *)

(* Liberated from List3264.                                                     *)

(*$IFNDEF UNIX *)
VAR	scratchBuffer: ARRAY[0..2047] OF CHAR;
(*$IFDEF DELPHI *)
	scratchLength: INTEGER;
(*$ELSE         *)
	scratchLength: LONGWORD;
(*$ENDIF        *)
(*$ELSE		*)
VAR	passwd, line: TSchemeSplitter;
        i: INTEGER;
(*$ENDIF	*)

BEGIN
  IF cachedUserShortName <> '' THEN BEGIN
    RESULT:= cachedUserShortName;
    EXIT
  END;
(*$IFNDEF UNIX	*)
  FillChar(scratchBuffer, SIZEOF(scratchBuffer), #0);
  scratchLength:= SIZEOF(scratchBuffer) - 1;
  GetUserName(@scratchBuffer, scratchLength);
  RESULT:= StrPas(scratchBuffer);
(*$ELSE	        *)

(* This is only run once so doesn't have to be particularly fast.	        *)

  RESULT:= '';
  passwd:= TSchemeSplitter.Create;
  line:= TSchemeSplitter.Create;
  line.Duplicates:= dupAccept;
  TRY
    TRY
      passwd.LoadFromFile('/etc/passwd'); (* Break into lines		        *)
      FOR i:= 0 TO passwd.Count DO BEGIN
        line.Clear;
        line.ColonText:= passwd[i];	(* Break into fields		        *)
        TRY
          IF StrToInt(line[2]) {%H- 4035 } = fpGetuid THEN BEGIN
            RESULT:= line[0];
            BREAK			(* Break out of loop		        *)
          END
        EXCEPT
        END
      END
    EXCEPT
    END
  FINALLY
    line.Free;
    passwd.Free
  END;
(*$ENDIF	*)
  cachedUserShortName:= RESULT
END { UserShortName } ;


VAR     cachedUserFullName: STRING;


FUNCTION UserFullName: STRING;

(* Return the user or account name as available.                                *)

(* Liberated from List3264, modified for GECOS extraction.                      *)

(*$IFNDEF UNIX *)
VAR	scratchBuffer: ARRAY[0..2047] OF CHAR;
(*$IFDEF DELPHI *)
	scratchLength: INTEGER;
(*$ELSE         *)
	scratchLength: LONGWORD;
(*$ENDIF        *)
(*$ELSE		*)
VAR	passwd, line, gecos: TSchemeSplitter;
        i: INTEGER;
(*$ENDIF	*)

BEGIN
  IF cachedUserFullName <> '' THEN BEGIN
    RESULT:= cachedUserFullName;
    EXIT
  END;
(*$IFNDEF UNIX	*)
  FillChar(scratchBuffer, SIZEOF(scratchBuffer), #0);
  scratchLength:= SIZEOF(scratchBuffer) - 1;
  GetUserName(@scratchBuffer, scratchLength);
  RESULT:= StrPas(scratchBuffer);
(*$ELSE	        *)

(* This is only run once so doesn't have to be particularly fast.	        *)

  RESULT:= '';
  passwd:= TSchemeSplitter.Create;
  line:= TSchemeSplitter.Create;
  line.Duplicates:= dupAccept;
  TRY
    TRY
      passwd.LoadFromFile('/etc/passwd'); (* Break into lines		        *)
      FOR i:= 0 TO passwd.Count DO BEGIN
        line.Clear;
        line.ColonText:= passwd[i];	(* Break into fields		        *)
        TRY
          IF StrToInt(line[2]) {%H- 4035 } = fpGetuid THEN BEGIN
            RESULT:= line[0];
            IF line[4] <> '' THEN       (* GECOS field not blank                *)
              IF Pos(',', line[4]) < 1 THEN (* Name only- Debian?               *)
                RESULT:= line[4]
              ELSE BEGIN                (* Multipart- Slackware or classic unix *)
                gecos:= TSchemeSplitter.Create;
                TRY
                  gecos.CommaText:= line[4];
                  IF gecos[0] <> '' THEN
                    RESULT:= gecos[0]
                FINALLY
                  gecos.Free
                END
              END;
            BREAK			(* Break out of loop		        *)
          END
        EXCEPT
        END
      END
    EXCEPT
    END
  FINALLY
    line.Free;
    passwd.Free
  END;
(*$ENDIF	*)
  cachedUserFullName:= RESULT
END { UserFullName } ;


VAR     cachedUserHome: STRING;


FUNCTION UserHome: STRING;

(* Return the user's home directory, blank if there's a problem.	        *)
(*                                                                              *)
(* I'm aware of GetAppConfigDir() and GetAppConfigFile() but don't like their   *)
(* choice of extension etc., however as with much of this program there might   *)
(* be other facilities hidden away in the FPC/Lazarus libraries that could be   *)
(* used to reduce verbosity.                                                    *)

VAR	passwd, line: TSchemeSplitter;
        i: INTEGER;

BEGIN
  IF cachedUserHome <> '' THEN BEGIN    (* It's been cached.                    *)
    RESULT:= cachedUserHome;
    EXIT
  END;

(* This is the simple case: we're not running as a privileged user so trust the *)
(* shell's HOME variable even though it can be changed.                         *)

  TRY
    RESULT:= SysUtils.GetEnvironmentVariable(env)
  EXCEPT
    RESULT:= ''
  END;

(* Depending on the operating system handle the more difficult case where we    *)
(* are running as a privileged user, possibly setuid, so must be stricter about *)
(* the location of the configuration file, by overriding the value from the     *)
(* shell variable e.g. by inspecting the actual user's entry in /etc/passwd.    *)

(*$IFDEF UNIX   *)
  IF fpGetEuid = 0 THEN BEGIN
    RESULT:= '';
    passwd:= TSchemeSplitter.Create;
    line:= TSchemeSplitter.Create;
    line.Duplicates:= dupAccept;
    TRY
      TRY
        passwd.LoadFromFile('/etc/passwd'); (* Break into lines		        *)
        FOR i:= 0 TO passwd.Count DO BEGIN
          line.Clear;
          line.ColonText:= passwd[i];	(* Break into fields		        *)
          TRY
            IF StrToInt(line[2]) {%H- 4035 } = fpGetuid THEN BEGIN
              RESULT:= line[5];
              BREAK			(* Break out of loop		        *)
            END
          EXCEPT
          END
        END
      EXCEPT
      END
    FINALLY
      line.Free;
      passwd.Free
    END;

(* If all else fails then use the shell variable after all.                     *)

    IF RESULT = '' THEN
      TRY
        RESULT:= GetEnvironmentVariable(env)
      EXCEPT
        RESULT:= ''
      END
  END;
(*$ELSE         *)

(* I'm not particularly serious about this program running under Win-32 so am   *)
(* prepared to accept the shell variable as being good enough.                  *)

(*$ENDIF        *)
  cachedUserHome:= RESULT		(* Cache the result                     *)
END { UserHome } ;


(* If this is a unix-style system, i.e. including Linux, Solaris etc., return   *)
(* one or more fully-qualified per-user configuration file names applying       *)
(* appropriate conventions. If it is a non-unix system then return "best        *)
(* guesses" which might be displayed "greyed out", but try to preserve the      *)
(* number of items.                                                             *)
//
function UnixUserConfigNames(base: string= ''): TStringList;

const	path1= '/.config/';             (* These for Unix                       *)
	path2= '/.';
        ext= '.conf';

//TODO: Space as parameter returns directory name(s).

begin
  if base = '' then                     (* Space as parameter suppresses this   *)
    base := ExtractFileName(ParamStr(0));
  base := Trim(base);
  SetLength(base, Length(base) - Length(ExtractFileExt(base)));
  result := TStringList.Create;
{$ifdef UNIX }
  result.Append(UserHome + path1 + base + ext);
  result.Append(UserHome + path2 + base + ext)
{$else       }
  result.Append('/home/' + UserShortName + path1 + base + ext);
  result.Append('/home/' + UserShortName + path2 + base + ext)
{$endif UNIX }
end { UnixUserConfigNames } ;


(* If this is a Windows system, i.e. including NT, XP etc., return one or more  *)
(* fully-qualified per-user configuration file names applying appropriate       *)
(* conventions. If it is a non-unix system then return "best guesses" which     *)
(* might be displayed "greyed out", but try to preserve the number of items.    *)
//
function WindowsUserConfigNames(base: string= ''): TStringList;

const	path1= '\';                     (* This for Windows                     *)
        ext= '.conf';

//TODO: Space as parameter returns directory name(s).

begin
  if base = '' then                     (* Space as parameter suppresses this   *)
    base := ExtractFileName(ParamStr(0));
  base := Trim(base);
  SetLength(base, Length(base) - Length(ExtractFileExt(base)));
  result := TStringList.Create;
{$ifdef UNIX }
  result.Append('c:\Documents and Settings\' + UserShortName + path1 + base + ext)
{$else       }
  result.Append(UserHome + path1 + base + ext)
{$endif UNIX }
end { WindowsUserConfigNames } ;


(* Return per-user configuration file names for all supported operating         *)
(* systems. Those not relevant to the current system may be recognised by       *)
(* having an appended space, these are typically displayed "greyed out".        *)
//
function UserConfigNames(base: string= ''): TStringList;

var     i: integer;
        result2: TStringList;

//TODO: Space as parameter returns directory name(s).

begin
  result := UnixUserConfigNames(base);
{$ifndef UNIX }
  for i := 0 to result.Count - 1 do
    result[i] := result[i] + ' ';
{$endif UNIX  }
  result2 := WindowsUserConfigNames(base);
  try
    for i := 0 to result2.Count - 1 do
{$ifdef UNIX  }
      result.Append(result2[i] + ' ')
{$else        }
      result.Append(result2[i])
{$endif UNIX  }
  finally
    result2.Free
  end
end { UserConfigNames } ;


(* Return the name of a single configuration file, appropriate to all users.    *)
//
function GlobalConfigName(base: string= ''): string;

//TODO: Space as parameter returns directory name(s).

begin
  if base = '' then                     (* Space as parameter suppresses this   *)
    base := ExtractFileName(ParamStr(0));
  base := Trim(base);
  SetLength(base, Length(base) - Length(ExtractFileExt(base)));
{$ifdef UNIX  }
  result := '/etc/' + base + '.conf'
{$else        }
  result := 'c:\Documents and Settings\All Users\' + base + '.conf'
{$endif UNIX  }
end { GlobalConfigName } ;


(* Return the name of the "best" existing configuration file, optionally        *)
(* including the global as well as the user files and an offset (0 is the best  *)
(* file, 1 is the next-best and so on.                                          *)
//
function BestConfigName(base: string= ''; global: boolean= false; offset: integer= 0): string;

var     scratch: TStringList;
        i: integer;

//TODO: Space as parameter returns directory name(s).

begin
  offset := Abs(offset);
{$ifdef UNIX  }
  scratch := UnixUserConfigNames(base);
{$else        }
  scratch := WindowsUserConfigNames(base);
{$endif UNIX  }
  try
    if global then
      scratch.Insert(0, GlobalConfigName(base));
    i := 0;
    while i < scratch.Count do
      if FileExists(scratch[i]) then
        Inc(i)
      else
        scratch.Delete(i);
    if offset < scratch.Count then
      result := scratch[offset]
    else
      result := ''
  finally
    scratch.Free
  end
end { BestConfigName } ;


(* The name being looked at is not in a user's local directory, so is global.   *)
//
function ConfigNameIsGlobal(const name: string): boolean;

begin
{$ifdef UNIX    }
  result := Pos('/' + UserShortName + '/', name) = 0
{$else          }
  result := Pos('\' + LowerCase(UserShortName) + '\', LowerCase(name)) = 0
{$endif         }
end { ConfigNameIsGlobal } ;


(* Test code to allow at least some of the above to be explored.                *)
//
procedure ConfigTestLocation(base: string= '');

var     scratch: TStringList;
        i: integer;


  procedure dontOpt(str: string);

  begin
    str := str
  end { dontOpt } ;

begin
  dontOpt(UserShortName);
  dontOpt(UserFullName);
  dontOpt(UserHome);
  scratch := UserConfigNames;
  try
    for i := 0 to scratch.Count - 1 do
      dontOpt(scratch[i])
  finally
    scratch.Free
  end;
  dontOpt(GlobalConfigName(base));
  dontOpt(BestConfigName(base, true, 0));
  dontOpt(BestConfigName(base, true, 1));
  dontOpt(BestConfigName(base, true, 2));
  dontOpt(BestConfigName(base, true, 3))
end { ConfigTestLocation } ;


var
  cachedUserName, cachedUserNameFQ: string;


(* Return the hostname, optionally fully qualified by the domain name. This is
  an alternative to Unix.GetHostName.
*)
function GetHostName(fullyQualified: boolean= false): string;

begin
  if cachedUserName = '' then
    cachedUserName := Unix.GetHostName;
  if (cachedUserNameFQ = '') and fullyQualified then
    cachedUserNameFQ := cachedUserName + '.' + GetDomainName();
  if fullyQualified then
    result := cachedUserNameFQ
  else
    result := cachedUserName
end { GetHostName } ;


var
  cachedDomainName: string;


(* Return the domain name based on the content of local files etc., do not
  attempt any DNS lookup etc. that might fail due to local system configuration
  or network issues. This is an alternative to Unix.GetDomainName, which might
  only work if NIS/YP is configured.
*)
function GetDomainName(): string;

var
  host, line: string;
  hostsFile: Text;
  i: integer;

begin
  if cachedDomainName = '' then begin
    cachedDomainName := Unix.GetDomainName;
    if (cachedDomainName = '') or (LowerCase(cachedDomainName) = '(none)') then begin
      cachedDomainName := '';
      host := GetHostName(false);
      try
        AssignFile(hostsFile, '/etc/hosts');
        Reset(hostsFile);
        try
          while not Eof(hostsFile) do begin
            ReadLn(hostsFile, line);
            for i := 1 to Length(line) do
              if line[i] < ' ' then
                line[i] := ' ';
            i := Pos(host + '.', line);
            if i > 0 then begin         (* Got FQDN for this host               *)
              Delete(line, 1, i - 1);   (* Chop anything at start               *)
              i := Pos(' ', line);
              if i > 1 then             (* Chop anything at end                 *)
                SetLength(line, i - 1);
              Delete(line, 1, Length(host) + 1);
              cachedDomainName := line;
              break                     (* while loop via finally block         *)
            end
          end
        finally
          Close(hostsFile)
        end
      except
      end
    end
  end;
  result := cachedDomainName
end { GetDomainName } ;


(* Clear any cached names or file locations, for example if the caller has just *)
(* had setuid root state revoked.                                               *)
//
procedure ClearCachedNames;

begin
  cachedUserShortName:= '';
  cachedUserFullName:= '';
  cachedUserHome:= '';
  cachedUserName := '';
  cachedUserNameFQ := '';
  cachedDomainName := ''
end { ClearCachedNames } ;


initialization
  ClearCachedNames
end.

