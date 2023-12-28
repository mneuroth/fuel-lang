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

package;

import haxe.Exception;

import LispUtils.TupleReturn;

//enum Results {
//    ParseResult(ok:Bool, value:Float);
//}

/*
class NumParserReturn<T> {
    public var ok: Bool;
    public var value: T;

    public function new(_ok:Bool, _value:T) {
        ok = _ok;
        value = _value;
    }
}

function TryParseIntOld(val:String):NumParserReturn<Int> {
    var result = Std.parseInt(val);
    if(result == null) {
        return new NumParserReturn(false, null);
    }
    return new NumParserReturn(true, result);
}

function TryParseFloatOld(val:String):NumParserReturn<Float> {
    var result = Std.parseFloat(val);
    if(Math.isNaN(result)) {
        return new NumParserReturn(false, null);
    }
    return new NumParserReturn(true, result);
}
*/

function TryParseInt(val:String):TupleReturn<Bool,Int> {
    try {
        if (val.indexOf(".")<0) {
            var result = haxe.Json.parse(val);
            if(result is Int) {  // Type.typeof(result) == TInt
                return new TupleReturn<Bool,Int>(true, result);
            }
        }
    } catch(e:Exception) {
        //trace(e);
    }
    return new TupleReturn<Bool,Int>(false, /*null*/0);
}

function TryParseFloat(val:String):TupleReturn<Bool,Float> {
    try {
        var result = haxe.Json.parse(val);
        if(result is Float) {  // Type.typeof(result) == TFloat
            return new TupleReturn<Bool,Float>(true, result);
        }
    } catch(e:Exception) {
        //trace(e);
    }
    return new TupleReturn<Bool,Float>(false, /*null*/0.0);
}


/// <summary>
/// Type for lisp tokens.
/// </summary>
/*public*/ enum LispTokenType
{
    ListStart;
    ListEnd;
    Symbol;
    String;
    Int;
    Double;
    Quote;
    QuasiQuote;
    UnQuote;
    UnQuoteSplicing;
    True;
    False;
    Comment;
    Nil;
}

/// <summary>
/// Interface for a lisp token.
/// </summary>
/// <remark>
/// This interface is intended to remove the dependency to the LispToken class in the future.
/// </remark>
/*public*/ interface ILispTokenInterface
{
    /// <summary>
    /// Gets the type of the token.
    /// </summary>
    /// <value>
    /// The type.
    /// </value>
    public var Type(default, null):LispTokenType;

    /// <summary>
    /// Gets the value.
    /// </summary>
    /// <value>
    /// The value.
    /// </value>
    public var Value(default, null):Dynamic;

    /// <summary>
    /// Gets the start position of the token.
    /// </summary>
    /// <value>
    /// The start position.
    /// </value>
    public var StartPos(default, null):Int;
}

/// <summary>
/// Lisp token.
/// </summary>
/*public*/ class LispToken implements ILispTokenInterface
{
    public static inline var StringStart:String = "\"";
    public static inline var Quote:String = "'";
    public static inline var Quasiquote:String = "`";
    public static inline var Unquote:String = ",";
    public static inline var Unquotesplicing:String = ",@";
    public static inline var Nil:String = "NIL";

    /// <summary>
    /// Gets the type of the token.
    /// </summary>
    /// <value>
    /// The type.
    /// </value>
    public var Type(default, default):LispTokenType; // { get; private set; }

    /// <summary>
    /// Gets the value.
    /// </summary>
    /// <value>
    /// The value.
    /// </value>
    public var Value(default, default):Dynamic; //system.Object; //{ get; private set; }

    /// <summary>
    /// Gets or sets the start position of the token.
    /// </summary>
    /// <value>
    /// The start position.
    /// </value>
    public var StartPos(default, default):Int; //{ get; set; }

    /// <summary>
    /// Gets or sets the stop position of the token.
    /// </summary>
    /// <value>
    /// The stop position.
    /// </value>
    public var StopPos(default, default):Int; //{ get; set; }

    /// <summary>
    /// Gets or sets the line no of the token.
    /// </summary>
    /// <value>
    /// The line no.
    /// </value>
    public var LineNo(default, default):Int; //{ get; set; }

    /// <summary>
    /// Initializes a new instance of the <see cref="LispToken"/> class.
    /// </summary>
    /// <param name="text">The text of the token.</param>
    /// <param name="start">The start position.</param>
    /// <param name="stop">The stop position.</param>
    /// <param name="lineNo">The line no.</param>
    public function new(text:String, start:Int, stop:Int, lineNo:Int)
    {
        var tempResultFloat:TupleReturn<Bool,Float>;
        var tempResultInt:TupleReturn<Bool,Int>;

        StartPos = start;
        StopPos = stop;
        LineNo = lineNo;
        Value = text;

        if (text.indexOf(StringStart) == 0)
        {
            Type = LispTokenType.String;
            Value = text.substr(1, text.length - 2);
        }
        else if (text == Quote)
        {
            Type = LispTokenType.Quote;
        }
        else if (text == Quasiquote)
        {
            Type = LispTokenType.QuasiQuote;
        }
        else if (text == Unquote)
        {
            Type = LispTokenType.UnQuote;
        }
        else if (text == Unquotesplicing)
        {
            Type = LispTokenType.UnQuoteSplicing;
        }
        else if (text == "(")
        {
            Type = LispTokenType.ListStart;
        }
        else if (text == ")")
        {
            Type = LispTokenType.ListEnd;
        }
        else if ((tempResultInt = TryParseInt(text)).value1/*ok*/)
        {
            Type = LispTokenType.Int;
            Value = tempResultInt.value2;
        }
        else if ((tempResultFloat = TryParseFloat(text)).value1/*ok*/)
        {
// TODO: CallByRef Klasse anlegen, via Templates, liefert einen Typ zurück
            Type = LispTokenType.Double;
            Value = tempResultFloat.value2;
        }
        else if (text == "true" || text == "#t")
        {
            Type = LispTokenType.True;
            Value = true;
        }
        else if (text == "false" || text == "#f")
        {
            Type = LispTokenType.False;
            Value = false;
        }
        else if (text.toUpperCase() == Nil)
        {
            Type = LispTokenType.Nil;
            Value = null;
        }
        else if (text.indexOf(";") == 0)
        {
            Type = LispTokenType.Comment;
            Value = text;
        }
        else
        {
            Type = LispTokenType.Symbol;
            Value = text;
        }
    }

    /// <summary>
    /// Returns a <see cref="System.String" /> that represents this instance.
    /// </summary>
    /// <returns>
    /// A <see cref="System.String" /> that represents this instance.
    /// </returns>   
    public /*override*/ function ToStr():String
    {
        if (Type == LispTokenType.Nil)
        {
            return Nil;
        }
        return Value/*.ToStr()*/;
    }
}

