unit Config;

(*
    OpenAcoon - An OpenSource Internet-Search-Engine
    Copyright (C) 1999-2008 Acoon GmbH

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License version 2 as
    published by the Free Software Foundation.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

-------------------------------------------------------------------------------

    The purpose of this unit is to provide an interface to configuration-data
    contained in openacoon.config.default and openacoon.config.

    The filename can be easily changed from "openacoon.* " to anything else
    by changing the "cConfigName" constant.

    Configuration info is read at program-start, so any changes made to the
    config-files while the program is running WILL NOT BE NOTICED automatically.
    If a program runs long enough to make this a problem, it can call
    ConfigReadInfo to re-read the config-files prior to accessing any setting.

-------------------------------------------------------------------------------

ChangeLog:
27-Apr-2008 MikeS: Written this unit.

*)

interface

procedure ConfigReadInfo;
function ConfigReadString(const ASetting: string):string;
function ConfigReadStringDefault(const ASetting, ADefault: string):string;
function ConfigReadInteger(const ASetting: string):integer;
function ConfigReadIntegerDefault(const ASetting: string;
    ADefault: integer):integer;


implementation

uses
    Classes,
    SyncObjs,
    SysUtils;

const
    // cConfigName is the first part of the configuration-file's filename.
    // Actual filenames are cConfigName+'.config.default' and
    // cConfigName+'.config'. If you are using this unit in any software
    // different from OpenAcoon, then you need to change this constant.
    cConfigName = 'openacoon';

var
    // Settings and Values are used to store the setting-names and
    // value-strings.
    Settings,Values: tStringList;

    // CriticalSection is used to ensure thread-safe access to
    // the Settings and Values variables.
    CriticalSection: tCriticalSection;



// If no entry for ASetting can be found, then ConfigReadString will return an
// empty string. This function is not by itself thread-safe, but calls
// ConfigReadStringDefault which IS thread-safe.
function ConfigReadString(const ASetting: string):string;
begin
    Result:=ConfigReadStringDefault(ASetting,'');
end;



// If no entry for ASetting can be found, then ConfigReadStringDefault will
// return ADefault. This function is thread-safe.
function ConfigReadStringDefault(const ASetting, ADefault: string):string;
var
    i: integer;
begin
    // Ensure that no other threads are currently accessing the
    // Settings and Values variables.
    CriticalSection.Enter;

    // Find the entry number.
    i:=Settings.IndexOf(LowerCase(ASetting));
    if i=-1 then
        Result:=ADefault // Return ADefault if setting isn't found.
    else
        Result:=Values.Strings[i]; // Otherwise return the value.

    // Re-enable access from other threads to Settings and Values.
    CriticalSection.Leave;
end;



// If no entry for ASetting can be found or if its value is not a valid
// integer-value, then ConfigReadInteger will return 0. This function is
// not by itself thread-safe, but calls ConfigReadIntegerDefault, which IS
// thread-safe.
function ConfigReadInteger(const ASetting: string):integer;
begin
    Result:=ConfigReadIntegerDefault(ASetting,0);
end;



// If no entry for ASetting can be found or if its value is not a valid
// integer-value, then ConfigReadIntegerDefault will return ADefault.
// This function is thread-safe.
function ConfigReadIntegerDefault(const ASetting: string;
    ADefault: integer):integer;
var
    i: integer;
begin
    // Ensure that no other threads are currently accessing the
    // Settings and Values variables.
    CriticalSection.Enter;

    // Find the entry number.
    i:=Settings.IndexOf(LowerCase(ASetting));
    if i=-1 then
        Result:=ADefault // Return ADefault if setting isn't found.
    else
    begin
        try // Otherwise return the value.
            Result:=StrToIntDef(Values.Strings[i],ADefault);
        except
            Result:=ADefault; // Return ADefault if StrToIntDef failed.
        end;
    end;

    // Re-enable access from other threads to Settings and Values
    CriticalSection.Leave;
end;



// ConfigReadSingleFile reads ONE configuration-file and stores its data
// in the Settings and Values variables. ConfigReadSingleFile ist NOT
// thread-safe! The caller must ensure thread-safety!
procedure ConfigReadSingleFile(const AFileName: string);
var
    ConfigFile: TextFile;
    s: string;
    i,EntryNumber: integer;
    ThisSetting,ThisValue: string;
begin
    try
        // Open the config-file.
        AssignFile(ConfigFile,AFileName);
        Reset(ConfigFile);

        // Keep reading the config-file until end-of-file is reached.
        while not eof(ConfigFile) do
        begin
            ReadLn(ConfigFile,s);
            i:=Pos('=',s); // Look for an equal-sign.

            // Process only if the line doesn't start with ";" and contains
            // an equal-sign.
            if (copy(s,1,1)<>';') and (i>0) then
            begin
                // Extract setting from line. Remove leading and trailing
                // spaces, and convert to all lower-case characters.
                ThisSetting:=LowerCase(Trim(copy(s,1,i-1)));

                // Extract value from line. Remove leading and trailing spaces.
                ThisValue:=Trim(copy(s,i+1,Length(s)-i));

                // Check to see if ThisSetting already exists.
                EntryNumber:=Settings.IndexOf(ThisSetting);

                if EntryNumber<>-1 then
                    // This setting already exists. Overwrite the old value.
                    Values.Strings[EntryNumber]:=ThisValue
                else
                begin // New setting. Add it and also add the value.
                    Settings.Add(ThisSetting);
                    Values.Add(ThisValue);
                end;
            end;
        end;

        // We're done. Close the file.
        CloseFile(ConfigFile);
    except
    end;
end;



// ConfigReadInfo will read both "*.config.default" and "*.config" and place
// the info from those files in internal data-structure for easy reference.
procedure ConfigReadInfo;
begin
    // Ensure that no other threads are currently accessing the
    // Settings and Values variables.
    CriticalSection.Enter;

    // Now we can safely clear any previously existing config-info.
    Settings.Clear;
    Values.Clear;

    // Read the "*.config.default" and "*.config".
    ConfigReadSingleFile(cConfigName+'.config.default');
    ConfigReadSingleFile(cConfigName+'.config');

    // Re-enable access from other threads to Settings and Values.
    CriticalSection.Leave;
end;


begin
    // Initialize the Settings and Values StringLists.
    Settings:=tStringList.Create;
    Settings.Sorted:=false;
    Settings.Duplicates:=dupAccept;
    Values:=tStringList.Create;
    Values.Sorted:=false;
    Values.Duplicates:=dupAccept;

    // Create the CriticalSection that is used to make all of this thread-safe.
    CriticalSection:=tCriticalSection.Create;

    // And now read the configuration files.
    ConfigReadInfo;
end.
