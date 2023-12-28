/*
 * FUEL(isp) is a fast usable embeddable lisp interpreter.
 *
 * Copyright (c) 2023 Michael Neuroth
 *
 * Permission is hereby granted, free of charge, to any person obtaining 
 * a copy of this software and associated documentation files (the "Software"), 
 * to deal in the Software without restriction, including without limitation 
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, 
 * and/or sell copies of the Software, and to permit persons to whom the 
 * Software is furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included 
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
 * OTHER DEALINGS IN THE SOFTWARE.
 * 
 * */

import LispScope.TextWriter;
using StringTools;

import LispException.LispException;

/// <summary>
/// Constant to transport line number info in exception
/// </summary>
/*public const string*/var LineNo = "LineNo";

/// <summary>
/// Constant to transport the start position info of the current statement in exception
/// </summary>
/*public const string*/var StartPos = "StartPos";

/// <summary>
/// Constant to transport the stop position info of the current statement in exception
/// </summary>
/*public const string*/var StopPos = "StopPos";

/// <summary>
/// Constant to transport module name and path info in exception
/// </summary>
/*public const string*/var ModuleName = "ModuleName";

/// <summary>
/// Constant to transport a stack info in exception
/// </summary>
/*public const string*/var StackInfo = "StackInfo";

/// <summary>
/// Constant for the command line module
/// </summary>
/*public const string*/var CommandLineModule = "command-line";

var LibraryPath:String = "";

var DirectorySeparatorChar:String = "/";

var Console:TextWriter = new TextWriter();

class Ref<T> {
    public var value:T;

    public function new(val:T) {
        value = val;
    }
}

class TupleReturn<T1,T2> {
    public var value1:T1;
    public var value2:T2;

    public function new(_value1:T1, _value2:T2) {
        value1 = _value1;
        value2 = _value2;
    }
}

class TripleReturn<T1,T2,T3> {
    public var value1:T1;
    public var value2:T2;
    public var value3:T3;

    public function new(_value1:T1, _value2:T2, _value3:T3) {
        value1 = _value1;
        value2 = _value2;
        value3 = _value3;
    }
}

class ArrayExtender {
    public static function First(arr:Array<Dynamic>):Dynamic {
        if (arr.length == 0) {
            throw new LispException("Array<Dynamic> has no elements!");
        }
        return arr[0];
    }
    public static function Last(arr:Array<Dynamic>):Dynamic {
        if (arr.length == 0) {
            throw new LispException("Array<Dynamic> has no elements!");
        }
        return arr[arr.length - 1];
    }
    public static function FirstOrDefault(arr:Array<Dynamic>):Dynamic {
        if (arr.length == 0) {
            return null;
        }
        return arr[0];
    }
    public static function Add(arr:Array<Dynamic>, item:Dynamic) {
        arr.push(item);
    }
    public static function Insert(arr:Array<Dynamic>, pos:Int, item:Dynamic) {
        arr.insert(pos, item);
    }
    public static function RemoveAt(arr:Array<Dynamic>, pos:Int) {
        arr.splice(pos, 1);
    }
    public static function ToList(arr:Array<Dynamic>):Array<Dynamic> {
        return arr;
    }
    public static function Skip(arr:Array<Dynamic>, count:Int):Array<Dynamic> {
        arr = arr.slice(count);
        return arr;
    }
    public static function AddRange(arr:Array<Dynamic>, other:Array<Dynamic>):Array<Dynamic> {
        for(elem in other) {
            arr.push(elem);
        }
        return arr;
    }
    public static function CopyTo(arr:Array<Dynamic>, other:Array<Dynamic>, index:Int):Array<Dynamic> {
        arr = other.copy();
        return arr;
    }
    public static function ElementAt(arr:Array<Dynamic>, index:Int):Dynamic {
        return arr[index];
    }
    public static function Clear(arr:Array<Dynamic>):Dynamic {
        arr = [];
        return arr;
    }
    public static function FindIndex(arr:Array<Dynamic>, findFcn:Dynamic):Int {
        for(i in 0...arr.length) {
            if (findFcn(arr[i])) {
                return i;
            }
        }
        return -1;
    }
}

class StringExtender {
    public static function reverse(s:String):String {
        var temp = s.split('');
        temp.reverse();
        return temp.join('');
    }
    public static function ToUpper(s:String):String {
        return s.toUpperCase();
    }
    public static function ToLower(s:String):String {
        return s.toLowerCase();
    }
    public static function Format(value:String, values:Array<Any>) {
        //see: https://stackoverflow.com/questions/49104997/string-substitution-string-formating-in-haxe
        var ereg:EReg = ~/(\{(\d{1,2})\})/g;
        while (ereg.match(value)) {
            value = ereg.matchedLeft() + values[Std.parseInt(ereg.matched(2))] + ereg.matchedRight();
        }
        return value;
    }
    public static function Append(s:String, other:String):String {
        s = s + other;
        return s;
    }
    public static function StartsWith(s:String, other:String):Bool {
        return s.indexOf(other) == 0;
    }
    public static function EndsWith(s:String, other:String):Bool {
        return s.lastIndexOf(other) == s.length - other.length;
    }
    public static function Contains(s:String, other:String):Bool {
        return s.indexOf(other) > 0;
    }
    public static function Trim(s:String):String {
        return s.trim();
    }
    public static function Equals(s:String, other:String):Bool {
        return s == other;
    }
    public static function Split(s:String, splitter:String):Array<String> {
        return s.split(splitter);
    }
}

class MapExtender {
    public static function TryGetValue(map:haxe.ds.StringMap<Dynamic>/*Map<String,Dynamic>*/, name:String, value:Ref<Dynamic>):Bool {
        if (map.exists(name)) {
            var val = map.get(name);
            value.value = val;
            return true;
        }
        return false;
    }
}

class LispExceptionExtender {
    public static function AddTokenInfos(exc:LispException, token:LispToken) {
// TODO        
    }
}
/*
function CompareToT<T>(val1:T, val2:T):Int {
    if(val1 == val2) {
        return 0;
    }
    if(val1 < val2) {
        return -1;        
    }
    if(val1 > val2) {
        return 1;        
    }
    return 0;
}
*/
function CompareToInt(val1:Int, val2:Int):Int {
    if(val1 == val2) {
        return 0;
    }
    if(val1 < val2) {
        return -1;        
    }
    if(val1 > val2) {
        return 1;        
    }
    return 0;
}

function CompareToFloat(val1:Float, val2:Float):Int {
    //if(val1 == val2) {
    //    return 0;
    //}
    if(val1 < val2) {
        return -1;        
    }
    if(val1 > val2) {
        return 1;        
    }
    return 0;
}

function StringCompare(val1:String, val2:String):Int {
    // <0   val1 < val2
    // ==0  val1 == val2
    // >0   val1 > val2
    //if(val1 == val2) {
    //    return 0;
    //}
    if(val1 < val2) {
        return -1;        
    }
    if(val1 > val2) {
        return 1;        
    }
    return 0;
}

function IsNullOrEmpty(val:String):Bool {
    return val == null || val.length == 0;
}

function ToLispVariant(val:Dynamic) {
    var ret:LispVariant = cast(val, LispVariant);
    return ret;
}

/// <summary>
/// Gets the script files from program arguments.
/// Returns all elements of the given args array which does not start with a "-".
/// </summary>
/// <param name="args">The arguments.</param>
/// <returns>Array of string names</returns>
function GetScriptFilesFromProgramArgs(/*string[]*/ args:Array<String>):Array<String>  // string[]
{
    return args.filter(function (s) { return s.indexOf("-") != 0; });
//    return new Array<String>();  //args.Where(s => !s.StartsWith("-")).ToArray();
}

/// <summary>
/// Reads a file or returns an empty string.
/// </summary>
/// <param name="fileName">Name of the file.</param>
/// <returns>Content of the file as string</returns>
function ReadFileOrEmptyString(fileName:String):String
{
    var exists:Bool = false;
    try
    {
#if (node || sys)        
        exists = sys.FileSystem.exists(fileName);
#end        
    }
    catch (ArgumentException)
    {
        exists = false;
    }
#if (node || sys)
    var content = exists ? sys.io.File.getContent(fileName) : /*string.Empty*/"";
#else
    var content = "(println \"WARNING: this platform does not support reading files!\")";
#end    
    return content;
}

/// <summary>
/// Decorates the code with a block.
/// </summary>
/// <param name="code">The code.</param>
/// <param name="offset">The position offset created by the decorated code.</param>
/// <returns>Decorated code.</returns>
function DecorateWithBlock(code:String, /*out*/ offset:Ref<Int>):String
{
    var /*const string*/ block = "(do ";
    offset.value = block.length;
    return block + code + "\n)";
}

function CurrentTickCount():Float
{
#if (node || sys)
    return Sys.cpuTime();
#else
    return -1;
#end
}

/// <summary>
/// Show the version of this FUEL interpreter.
/// </summary>
/// <param name="output">The output stream.</param>
/*public static*/ function ShowVersion(output:LispScope.TextWriter):Void
{
    output.WriteLine();
    output.WriteLine(Lisp.Name + " " + Lisp.Version + " (for " + Lisp.Platform + ") from " + Lisp.Date + ", " + Lisp.Copyright);
    output.WriteLine();
}

/// <summary>
/// Show informations about this FUEL interperter.
/// </summary>
/// <param name="output">The output stream.</param>
/*public static*/ function ShowAbout(output:LispScope.TextWriter):Void
{
    ShowVersion(output);
    output.WriteLine(Lisp.Info);
    output.WriteLine();
}

function CastDynamicToLispVariant(value:Dynamic):LispVariant
{
    var variant:LispVariant = value;
    return variant;
}

function CastDynamicToLispScope(value:Dynamic):LispScope
{
    var scope:LispScope = value;
    return scope;
}

function GetTargetLanguage():String
{
    var value = "unknown";
#if cpp
    value = "C++";
#end
#if cs
    value = "C#";
#end
#if eval
    value = "Interpreter";
#end
#if hl
    value = "HashLink";
#end
#if java
    value = "Java";
#end
#if js
    value = "JavaScript";
#if node
    value += "-Node";
#end
#end
#if lua
    value = "Lua";
#end
#if neko
    value = "Neko";
#end
#if pip
    value = "PHP";
#end
#if python
    value = "Python";
#end
#if swf
    value = "SWF";
#end
    return value;
}
