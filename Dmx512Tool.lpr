(* Lazarus+FPC 2.1.0+3.0.4 on Linux Lazarus+FPC 2.1.0+3.0.4 on Linux Lazarus+FP *)

program Dmx512Tool;

(* This is a very simple program to drive a set of DMX512 slave devices, and    *)
(* normally assumes the presence of a configuration file in the user's home     *)
(* directory.                                                     MarkMLl       *)

(* To the extent that it is tested at all, it has been used to exercise a 120W  *)
(* "moving head" believed to be made by V-Show. As such the meaning of the      *)
(* value which can be written to each channel (i.e. register given by an offset *)
(* with an assumed base address) is believed to be as below:

14ch    16ch

 1       1      Pan             0-255       Pan 0-540 degrees
         2      Pan fine        0-255       Pan fine
 2       3      Tilt            0-255       Tilt 0-270 degrees
         4      Tilt fine       0-255       Tilt fine
 3       5      Pan/tilt speed  0-255       Fast to slow
 4       6      Dimmer          0-255       Dark to bright
 5       7      Strobe          0-7         Turn on
                                8-250       Slow to fast
                                251-255     Turn on
 6       8      Colour          0-15        White
                                16-31       Colour 1
                                32-47       .
                                48-63       .
                                64-79       .
                                80-95       .
                                96-111      .
                                112-127     Colour 7
                                128-191     Clockwise slow to fast
                                192-255     Anticlockwise slow to fast
 7       9      Colour effect   0-10        No function
                                11-209      Fine tuning
                                210-255     Shake fast to slow
 8      10      Static gobos    0-5         Open
                                6-11        Gobo 1
                                12-17       .
                                18-23       .
                                24-29       .
                                30-35       .
                                36-41       .
                                42-47       .
                                48-53       .
                                54-59       Gobo 9
                                60-65       Gobo 9 shake
                                66-71       .
                                72-77       .
                                78-83       .
                                84-89       .
                                90-95       .
                                96-101      .
                                102-107     .
                                108-113     Gobo 1 shake
                                114-119     Open
                                120-187     Clockwise slow to fast
                                188-255     Anticlockwise slow to fast
 9      11      Rotating gobos  0-15        Open
                                16-31       Gobo 1
                                32-47       .
                                48-63       .
                                64-79       .
                                80-95       .
                                96-127      Gobo 6
                                128-191     Clockwise slow to fast
                                192-255     Anticlockwise slow to fast
10      12      Gobo rotation   0-15        Open
                                16-95       Gobo fine 0-180 degrees
                                96-135      Gobo rotating 0-90
                                136-155     Gobo rotating 0-180
                                156-175     Gobo rotating 0-360
                                176-215     Clockwise slow to fast
                                216-255     Anticlockwise slow to fast
11      13      Focus           0-255       Gobo hazy to clear
12      14      Prism           0-63        Open
                                64-255      Prism in
13      15      Prism rotation  0-15        Stop
                                16-135      Clockwise slow to fast
                                136-255     Anticlockwise slow to fast
14      16      Setting mode    0-7         No action
                                8-131       Auto mode
                                132-249     Sound mode
                                250-253     Reset
                                254-255     No function
                                                                                *)
(* Note that the industry convention is that the address and channel are 1-     *)
(* based, but I'm using a zero-based buffer internally. Hence if the entered    *)
(* address is 1 this will be corrected to refer to the bottom of the block to   *)
(* be transmitted, channel 1 has offset zero relative to this and so on. The    *)
(* configuration file in which state is retained reflects the internal buffer,  *)
(* i.e. is zero-based.                                                          *)

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, IniFilesLocation, IniFilesExtended, LocateFtdiPort,
                                        LocatePorts, Serial, Termio, BaseUnix;

label
  valueOnly, offsetAndValue;

const
  projectName= 'Dmx512Tool';
  ignoreSpeedError= true;
  repeatDwell= 75;                      (* Milliseconds                         *)

var
  repeatCount: integer= 50;             (* Very roughly five seconds            *)

type
  TValues= array[-1..511] of byte;      (* Main buffer, start byte + 512 values *)
  TOffset= 0..255;
  TOffsets= set of TOffset;

var
  confName, portName: string;
  profile: string= 'Default';
  address, offset, nextParam, finalParam: integer;
  values: TValues;


(* If there is a .ini file use it to populate a state structure. If there is no
  .ini file then tell the user to create one, try to auto-detect the port, and
  zero the remainder of the state structure.
*)
procedure readConfig(const confName: string);

var
  confFile: TIniFileEx;
  i: integer;

begin
  if confName <> '' then
    confFile := TIniFileEx.Create(confName)
  else
    confFile := nil;
  if (confName <> '') and (confFile <> nil) then try
    if (ParamCount() > 0) and (ParamStr(1)[1] in ['A'..'Z', 'a'..'z']) then
      profile := ParamStr(1);
    WriteLn('Using ' + confName + ' to save state between runs, profile "' + profile + '".');
    confFile := TIniFileEx.Create(confName);
    portName := confFile.ReadString(profile, 'Port', FindFtdiPort());
    address := confFile.ReadInteger(profile, 'Address', 0) mod 256;
    offset := confFile.ReadInteger(profile, 'Offset', 0) mod 256;
    values[-1] := 0;                    (* Start byte                           *)
    for i := 0 to 511 do
      values[i] := confFile.ReadIntegerAlt3(profile, '', 'Values', i, '', 0) mod 256
  finally
    FreeAndNil(confFile)
  end else begin
    WriteLn('No configuration file, create ~/.config/' + projectName + '.conf ');
    WriteLn('or ~/.' + projectName +'.conf to retain state between runs.');
    portName := FindFtdiPort();
    address := 0;
    offset := 0;
    for i := -1 to 511 do
      values[i] := 0
  end
end { readConfig } ;


(* If there is a .ini file and no error opening the port etc. save the state
  structure. Note that for 512 channels this will be comparatively slow, so
  avoid writing zero values since they can be inferred as the default.
*)
procedure writeConfig(const confName: string);

var
  confFile: TIniFileEx;
  i: integer;

begin
  if confName <> '' then
    confFile := TIniFileEx.Create(confName)
  else
    confFile := nil;
  if (confName <> '') and (confFile <> nil) then try
    if finalParam <> ParamCount() then
      profile := ParamStr(ParamCount());
    confFile := TIniFileEx.Create(confName);
    confFile.WriteString(profile, 'Port', portName);
    confFile.WriteInteger(profile, 'Address', address);
    confFile.WriteInteger(profile, 'Offset', offset);
    for i := 0 to 511 do
      if values[i] <> 0 then            (* Note ReadIntegerAlt3() defaults zero *)
        confFile.WriteInteger3(profile, 'Values', i, '', values[i])
      else
        confFile.DeleteKey(profile, 'Values[' + IntToStr(i) + ']')
  finally
    FreeAndNil(confFile)
  end
end { writeConfig } ;


function parseAddress(param: integer): integer;

begin
  if TryStrToInt(ParamStr(param), result) then
    result := (result - 1) mod 512
  else begin
    WriteLn('Unable to parse address "', ParamStr(param), '" as an integer.');
    Halt(9)
  end
end { parseAddress } ;


function parseOffset(param: integer): integer; // TOffsets;

type
  TOp= (none, comma, dash);

var
  first: integer;

begin
//  result := [];

// TODO : Allow range or sequence of offsets.
// I've stalled here because of the way that the program currently stores the
// last explicit offset as state in the configuration file.

  if TryStrToInt(ParamStr(param), first) then
    first := (first - 1) mod 512
  else begin
    WriteLn('Unable to parse channel "', ParamStr(param), '" as an integer.');
    Halt(9)
  end;
  result := first;
//  result := result + [first]
end { parseOffset } ;


function parseValue(param, current: integer): integer;

begin

// TODO : Allow sequence of values to match number of offsets, otherwise duplicate single value.
// A sequence of multiple values which doesn't match the number of offsets is
// an error. No provision here for double-width values, e.g. a 16-bit pan or
// tilt parameter, noting that this might need consideration of endianness.

  case ParamStr(param) of
    '+': result := (current + 1) mod 256;
    '-': if current = 0 then
           result := 256
         else
           result := current - 1
  otherwise
    if ParamStr(param)[1] in ['+', '-'] then begin
      if TryStrToInt(ParamStr(param), result) then
        result += current
      else begin
        WriteLn('Unable to parse value "', ParamStr(param), '" as an integer.');
        Halt(9)
      end
    end else
      if not TryStrToInt(ParamStr(param), result) then
        Halt(9);
    while result < 0 do
      result += 256;
    result := result mod 256
  end
end { parseValue } ;


(* Now open the port. On error exit without saving state.
*)
function writeDmx512(): boolean;

var
  serPort: TSerialHandle;

// Put non-standard speed setting code in SerialComms and note it for possible
// inclusion in Serial.pp.

{
Placeholder: discussion of arbitrary speeds on Linux >=2.6
https://sourceware.org/bugzilla/show_bug.cgi?id=10339 in particular comment 5.
This (interestingly) also mentions MIDI's 31,250 defining it as 1/32 Mbps.

There's a whole lot of stuff not defined by FPC, including BOTHER (for most CPUs)
and termios2. See /usr/include/asm-generic/termbits.h
}

  function setNonStandardSpeed(port: TSerialHandle; speed: longint): boolean;

  const
{$if not declared(TCGETS2) }
    TCGETS2= $542A;                     (* Linux include/uapi/asm-generic/ioctls.h *)
    TCSETS2= $542B;
//    TCSETSW2= $542C;
//    TCSETSF2= $542D;
{$endif                    }

  var
    tios: TTermios;                     (* Actually termios2 from around 2.6.4  *)
    rc: integer;

  begin
    rc := fpIoctl(port, TCGETS2, @tios);
    result := rc = 0;

  (* Whether this works or not depends on both the kernel version and the       *)
  (* capabilities of the device-specific driver.                                *)

    if not ignoreSpeedError then begin
      Assert(tios.c_ispeed = 230400, 'API error: cannot read initial (standard) speed, status '
                                                        + IntToStr(rc) +'.');
      Assert(tios.c_ospeed = 230400, 'API error: initial (standard) speed mismatch.')
    end;
    tios.c_ispeed := speed;
    tios.c_ospeed := speed;
    rc := fpIoctl(port, TCSETS2, @tios);
    result := result and (rc = 0)
  end { setNonStandardSpeed } ;


  procedure diag(const values: TValues; first, final: integer);

  var
    i: integer;

  begin
    for i := first to final do begin
      if (i = -1) or (i mod 16 = 0) then
        Write((i + 1):3, ' ');
      Write(HexStr(values[i], 2), ' ');
      if (i + 1) mod 16 = 0 then
        WriteLn
    end
  end { diag } ;


begin
  result := true;
  serPort:= SerOpenLocked(portName);
  if serPort >= 0 then try
    SerSetParams(serPort, 230400, 0, NoneParity, 2, []);

(* Set a non-standard speed of 250 kBit/sec/. On error exit without saving      *)
(* state, this may be caused by a device which has no arbitrary-speed API or    *)
(* by Linux <v2.6.20: I'm thinking in particular of things like the SheevaPlug  *)
(* or NSLU2 here which were shipped in that era and might possibly be still     *)
(* found with the original kernel and libraries.                                *)
(*                                                                              *)
(* If we can't set the speed then consider that (250,000 - 230,400) / 250,000   *)
(* is 7.84%. That's 12.75 bits before the timing error definitely hits, so      *)
(* since this is async hence resets the timing on every start bit it might      *)
(* /possibly/ be OKish for 9 bits (i.e. start bit plus 8 data bits).            *)

    if setNonStandardSpeed(serPort, 250000) or ignoreSpeedError then begin

(********************************************************************************)
(*                                                                              *)
(* DIRE WARNING: even if the speed can be set correctly, since the serial API   *)
(* doesn't have a chance in Hell of getting the 100 uSec break or 12 uSec MAB   *)
(* anywhere near right the code below is likely to be unreliable in conjunction *)
(* with at least some slave devices, consider this to be the bare minimum to    *)
(* suit the Linux serial API and investigate lower-level peripheral access      *)
(* (i.e. libusb etc.) in the event of problems.                                 *)
(*                                                                              *)
(********************************************************************************)

      diag(values, -1, 511);
      SerSetDtr(serPort, true);         (* Just in case...                      *)
      SerSetRts(serPort, true);
      Sleep(10);                        (* Allow settling to non-break state    *)
      while repeatCount > 0 do begin

(* Linux originally didn't allow finely-timed breaks, but at some point it      *)
(* seems to have been added. Hence -ve times (in mSec), originally intended for *)
(* Solaris, appear to work.                                                     *)

        SerBreak(serPort, -1, false);   (* Break, 100 uSec minimum              *)
        Sleep(1);                       (* Mark-after-break, 12 uSec minimum    *)
        SerWrite(serPort, values, 513); (* First byte is start code, normally 0 *)
        SerDrain(serPort);
        if repeatDwell >= 0 then        (* Sleep() completely skipped by -1     *)
          Sleep(Abs(repeatDwell));
        repeatCount -= 1
      end;
      Sleep(100)                        (* Make sure OS and device are drained  *)
    end else begin
      WriteLn('Unable to set DMX512 standard (Linux non-standard) speed, aborting.');
      result := false
    end
  finally
    SerClose(serPort)
  end else                              (* Port open failed, don't save state   *)
    result := false
end { writeDmx512 } ;


begin
  ExitCode := 0;
  if ParamCount() > 0 then
    case LowerCase(Copy(ParamStr(1), 1, 2)) of
      '/?',
      '-?',
      '/h',
      '-h',
      '--': begin
              WriteLn();
              WriteLn('Usage: ', projectName, ' [OPTIONS] [DEVICE] [[[ADDRESS] CHANNEL] VALUE]');
              WriteLn();
              WriteLn('Send a block to one or more DMX512 daisy-chained nodes.');
              WriteLn();
              WriteLn('DEVICE is the name of a serial device such as /dev/ttyUSB0.');
              WriteLn();
              WriteLn('ADDRESS and CHANNEL are in the range 1..512.');
              WriteLn();
              WriteLn('VALUE is in the range 0..255, optionally prefixed by + or - as an increment.');
              WriteLn();
              WriteLn('Supported options are as below:');
              WriteLn();
              WriteLn('  --help         This help text.');
              WriteLn();
              WriteLn('Exit status:');
              WriteLn();

(* These are borrowed, inherited or quite simply pinched from the MS2115B meter *)
(* reader, and possibly earlier from a USB exerciser and the LG600 mouse state  *)
(* dumper. They're not a bad set for a device which is expected to respond to a *)
(* query, but are rather less good for something like a chain of DMX512 devices *)
(* using 3-pin connectors hence with no way of responding to the controller.    *)

              WriteLn(' 0  Normal termination');
//              WriteLn(' 1  Cannot parse device identifier');
              WriteLn(' 2  Named device cannot be opened');
//              WriteLn(' 3  Named device is unresponsive');
//              WriteLn(' 4  Data access error');
//              WriteLn(' 5  Data format error');
              WriteLn(' 9  Bad command-line parameters');
//              WriteLn('10  Unable to create or start SCPI server');
              WriteLn();
              Halt
            end
    otherwise
    end;
  confName := BestConfigName(projectName, false);
  readConfig(confName);

(* If the first parameter looks like a device name then put it into the state   *)
(* structure. Otherwise use what's in there already.                            *)
(*                                                                              *)
(* I know: this doesn't allow for both a port name and a non-default profile.   *)
(* Just assume that's why I've not tried to document non-default profiles.      *)

  if (ParamCount() > 0) and (ParamStr(1)[1] = '/') then
    portName := ParamStr(1);

(* Parse the remaining parameters from right to left as value, register/offset  *)
(* and address/base.                                                            *)

  nextParam := 1;
  if (ParamCount() > 0) and (ParamStr(1)[1] in ['A'..'Z', 'a'..'z', '/']) then
    nextParam += 1;                     (* Skip first param (device name etc.)  *)
  finalParam := ParamCount();
  if ParamStr(finalParam)[1] in ['A'..'Z', 'a'..'z'] then
    finalParam -= 1;                    (* Ignore final param (write profile)   *)
//  WriteLn('Parameters at ', nextParam, ' to ', finalParam, ' count ', finalParam - (nextParam - 1));
//  Halt;
  case finalParam - (nextParam - 1) of
    0: ;                                (* Use values etc. from config file     *)
    1: begin
valueOnly:
         if finalParam >= nextParam then
           values[address + offset] := parseValue(nextParam, values[address + offset])
       end;
    2: begin
offsetAndValue:
         if finalParam >= nextParam then
           offset := parseOffset(nextParam);
         nextParam += 1;
         goto valueOnly                 (* Any sane language would allow a case *)
       end                              (* selector to be treated as a label in *)
  otherwise                             (* this context.                        *)
    if finalParam >= nextParam then
      address := parseAddress(nextParam);
    nextParam += 1;
    goto offsetAndValue
  end;
  WriteLn('Device:  ', portName);
  WriteLn('Address: ', address + 1);
  WriteLn('Channel: ', offset + 1);
  WriteLn('Value:   ', values[address + offset]);

(* So to summarise: if there's no parameters other than the optional port name  *)
(* write the values from the configuration file, if there's a single parameter  *)
(* then update the same value that was changed last time, and so on.            *)

// TODO : Breakout using ^C will prevent configuration being written.

  if not writeDmx512() then begin
    confName := '';                     (* Error inhibits config write          *)
    ExitCode := 2
  end;
  writeConfig(confName)
end.

