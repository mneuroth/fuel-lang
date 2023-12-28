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

using LispUtils;
using StringTools;

class LispFunctionWrapper {
    public var Signature:String;
    public var Documentation:String;
    public var ModuleName:String;
    public var IsBuiltIn:Bool;
    public var IsSpecialForm:Bool;
    public var IsEvalInExpand:Bool;

    //public Func<object[], LispScope, LispVariant> Function { get; private set; }
    public var Function:Dynamic;

    public function new(func:Dynamic = null, signature:String = null, documentation:String = null, isBuiltin:Bool = true, isSpecialForm:Bool = false, isEvalInExpand:Bool = false, moduleName:String = "<builtin>") {
        Function = func;
        Signature = signature;
        Documentation = documentation;
        IsBuiltIn = isBuiltin;
        IsSpecialForm = isSpecialForm;
        IsEvalInExpand = isEvalInExpand;
        ModuleName = moduleName;
    }    

    public var FormatedDoc(get, never):String;
    function get_FormatedDoc():String
    {
        /*const string*/var separator = "\n\n";
        /*const string*/var splitter = "-------------------------------------------------" + separator;
        return GetFormatedHelpString(separator, splitter);
    }

    public var HtmlFormatedDoc(get, never):String;
    function get_HtmlFormatedDoc():String
    {
        /*const string*/var separator = "<br><br>";
        /*const string*/var splitter = "<hr>" + "<br>";
        return GetFormatedHelpString(separator, splitter, function (s) { return "<b>" + s + "</b>"; }, function (s) { return "<code>" + s + "</code>"; });
    }

    private function GetFormatedHelpString(separator:String, splitter:String, /*Func<string, string>*/ nameDecorator:Dynamic = null, /*Func<string, string>*/ syntaxDecorator:Dynamic = null):String
    {
        if (nameDecorator == null)
        {
            nameDecorator = function (s) { return s; }
        }
        if (syntaxDecorator == null)
        {
            syntaxDecorator = function (s) { return s; }
        }
        var name = "???";
        var signature = (Signature != null ? Signature : ""/*string.Empty*/);
        if (signature.length > 0 && signature.startsWith("("))
        {
            var len = signature.indexOf(" "/*, StringComparison.Ordinal*/);
            // process commands like: (doc)
            if (len < 0)
            {
                len = signature.indexOf(")"/*, StringComparison.Ordinal*/) - 1;
            }
            name = nameDecorator(signature.substr(1, len));
        }
        name += IsSpecialForm ? " [special form]" : ""/*string.Empty*/;
        name += separator;
        var syntax = syntaxDecorator("Syntax: " + signature) + separator;
        var doc = (Documentation != null ? Documentation : "<not available>");
        doc += separator;
        return splitter + name + syntax + doc + "\n";
        return "???";
    }
}

/* public*/ enum LispType
{
    Undefined;
    Nil;
    Bool;
    Int;
    Double;
    String;
    List;
    Function;
    Symbol;
    NativeObject;
    //Array;
    LValue;
    Error;
}

function ToTypeId(Type:LispType):Int {
    switch(Type) {
        case Undefined:
            return 1;
        case Nil:
            return 2;
        case Bool:
            return 3;
        case Int:
            return 4;
        case Double:
            return 5;
        case String:
            return 6;
        case List:
            return 7;
        case Function:
            return 8;
        case Symbol:
            return 9;
        case NativeObject:
            return 10;
        //case Array:
        case LValue:
            return 11;
        case Error:
            return 12;
    }
}

function ToStringT(Type:LispType):String {
    switch(Type) {
        case Undefined:
            return "Undefined";
        case Nil:
            return "Nil";
        case Bool:
            return "Bool";
        case Int:
            return "Int";
        case Double:
            return "Double";
        case String:
            return "String";
        case List:
            return "List";
        case Function:
            return "Function";
        case Symbol:
            return "Symbol";
        case NativeObject:
            return "NativeObject";
        //case Array:
        case LValue:
            return "LValue";
        case Error:
            return "Error";
    }
}

/*public*/ enum LispUnQuoteModus
{
    None;
    UnQuote;
    UnQuoteSplicing;
}

function TypeOf(obj:Dynamic):LispType
{
    if ((obj is Int) && Type.typeof(obj)==TInt/*&& !(obj is Float)*/)     // Remark: on some (compiler) platforms a float is also an int !
    {
        return LispType.Int;
    }
    if (obj is Float)
    {
        return LispType.Double;
    }
    if (obj is Bool)
    {
        return LispType.Bool;
    }
    if (obj is String)
    {
        return LispType.String;
    }
    if (obj is /*IEnumerable<object>*/Array)
    {
        return LispType.List;
    }    
    //if (obj is IEnumerable)        // needed for .NET 3.5
    //{
    //    return LispType.List;
    //}
    if (obj is LispFunctionWrapper)
    {
        return LispType.Function;
    }
    if (obj is LispVariant)
    {
        return obj.ValueType;
    }    
    if (obj is LispToken.LispToken)
    {
        return LispType.Symbol;
    }
    return LispType.Undefined;
}

function TypeOfToken(token:LispToken):LispType
{
    if (token.Type==LispToken.LispTokenType.Int)
    {
        return LispType.Int;
    }
    else if (token.Type==LispToken.LispTokenType.Double)
    {
        return LispType.Double;
    }
    else if (token.Type==LispToken.LispTokenType.String)
    {
        return LispType.String;
    }
    else if (token.Type==LispToken.LispTokenType.True || token.Type==LispToken.LispTokenType.False)
    {
        return LispType.Bool;
    }
    else 
    {
        return TypeOf(token.Value);
    }
    return LispType.Undefined;
}

class LispVariant {
    public static var Tolerance:Float = 1e-8;

    public var CachedFunction:LispVariant;

    public var IsUnQuoted:LispUnQuoteModus;
    public var Value:Dynamic;
    public var ValueType:LispType;    

    public var Token:LispToken;

    public function new(type:LispType, /*object*/ value:Dynamic = null, unQuoted:LispUnQuoteModus = LispUnQuoteModus.None) {
        this.ValueType = type;
        this.Value = value;
        this.IsUnQuoted = unQuoted;
    }
    public static function forValue(value:Dynamic=null, valueType:LispType=null):LispVariant {
        var valType = valueType != null ? valueType : TypeOf(value);
        var newObj = new LispVariant(valType, value);
        if (value == null) {
            newObj.ValueType = LispType.Nil;
        }
        if(value is LispVariant) {
            var _value = cast(value, LispVariant);
            if (_value != null)
            {              
                newObj.ValueType = _value.ValueType;
                newObj.Value = _value.Value;
                newObj.IsUnQuoted = _value.IsUnQuoted;
                newObj.Token = _value.Token;
            }
        }
        return newObj;
    }
    public static function forToken(token:LispToken, /*object*/ unQuoted:LispUnQuoteModus=None):LispVariant {
        var newObj = new LispVariant(/*TypeOf(token.Value)*/TypeOfToken(token), token.Value);
        newObj.Token = token;
        if (token.Type == LispToken.LispTokenType.Nil)
        {
            newObj.ValueType = LispType.Nil;
        }
        if (token.Type == LispToken.LispTokenType.Symbol)
        {
            newObj.ValueType = LispType.Symbol;
        }
        return newObj;
    }

    public var TypeString(get, never):String;    
    function get_TypeString() {
        if (ValueType == LispType.NativeObject)
        {
// TODO -> implement later            
//            if (NativeObjectValue is Dictionary<object, object>)
//            {
//                return "NativeDictionary";
//            }
            return ValueType+"<"+Value.GetType()+">";                    
        }
        return ToStringT(ValueType);
    }

    public var IsNil(get, never):Bool;
    function get_IsNil() { return ValueType == LispType.Nil; }

    public var IsError(get, never):Bool;
    function get_IsError() { return ValueType == LispType.Error; }

    public var IsUndefined(get, never):Bool;
    function get_IsUndefined() { return ValueType == LispType.Undefined; }

    public var IsString(get, never):Bool;
    function get_IsString() { return ValueType == LispType.String; }

    public var IsDouble(get, never):Bool;
    function get_IsDouble() { return ValueType == LispType.Double; }

    public var IsInt(get, never):Bool;
    function get_IsInt() { return ValueType == LispType.Int; }

    public var IsNumber(get, never):Bool;
    function get_IsNumber() { return IsInt || IsDouble; }

    public var IsBool(get, never):Bool;
    function get_IsBool() { return ValueType == LispType.Bool; }

    public var IsList(get, never):Bool;
    function get_IsList() { return ValueType == LispType.List || ValueType == LispType.Nil; }

    public var IsFunction(get, never):Bool;
    function get_IsFunction() { return ValueType == LispType.Function; }

    public var IsSymbol(get, never):Bool;
    function get_IsSymbol() { return ValueType == LispType.Symbol; }

    public var IsNativeObject(get, never):Bool;
    function get_IsNativeObject() { return ValueType == LispType.NativeObject; }

    public var IsLValue(get, never):Bool;
    function get_IsLValue() { return ValueType == LispType.LValue; }

    /// <summary>
    /// Creates a new value representing an error.
    /// </summary>
    /// <param name="errorMessage">The error message.</param>
    /// <returns>The value</returns>
    public static function CreateErrorValue(errorMessage:String):LispVariant
    {
        return new LispVariant(LispType.Error, errorMessage);
    }

    /// <summary>
    /// Gets the LispType for the object.
    /// </summary>
    /// <param name="obj">The object to determin the type for.</param>
    /// <returns>The LispType</returns>
    public static function GetTypeFor(obj:Dynamic):LispType
    {
        if(obj is Int)
        {
            return LispType.Int;
        }
        if (obj is Float)
        {
            return LispType.Double;
        }
        if (obj is Bool)
        {
            return LispType.Bool;
        }
        if (obj is String)
        {
            return LispType.String;
        }
        if (obj is Array)
        {
            return LispType.List;
        }
        return LispType.Undefined;
    }

    /// <summary>
    /// Compares to other object.
    /// </summary>
    /// <param name="other">The other.</param>
    /// <returns></returns>
    public function CompareTo(other:Dynamic):Int
    {
        if (other is LispVariant)
        {
            var otherVariant = cast(other, LispVariant);
            if (IsNumber && otherVariant.IsNumber)
            {
                if (IsDouble || otherVariant.IsDouble)
                {
                    return LispUtils.CompareToFloat(ToDouble(), otherVariant.ToDouble());
                }
                return LispUtils.CompareToInt(IntValue, otherVariant.IntValue);
            }
            // all other types will be compared like a string
            return LispUtils.StringCompare(StringValue, otherVariant.StringValue);  //string.Compare(StringValue, otherVariant.StringValue, StringComparison.Ordinal);
        }            
        return CompareTo(LispVariant.forValue(other));
    }

    public var FunctionValue(get, never):LispFunctionWrapper;
    function get_FunctionValue() {
        if (ValueType != LispType.Function)
        {
            throw CreateInvalidCastException("function", "not found");
        }
        return cast(Value, LispFunctionWrapper);
    }

    public var ListValue(get, never):Array<Dynamic>;  //IEnumerable<object>
    function get_ListValue() {
        // Nil is an empty list () !
        if (ValueType == LispType.Nil)
        {
            return new Array<Dynamic>();  //List<object>();
        }
        if (ValueType == LispType.NativeObject && NativeObjectValue is /*IEnumerable<object>*/Array)
        {
            return cast(NativeObjectValue, Array<Dynamic>);  //(IEnumerable<object>)
        }
        if (ValueType != LispType.List)
        {
            throw CreateInvalidCastException("list");
        }
        return cast(Value, Array<Dynamic>);  //((IEnumerable)Value).Cast<object>();
    }

    public var ListRef(get, never):Array<Dynamic>;  //List<object>
    function get_ListRef() {
        // Nil is an empty list () !
        if (ValueType == LispType.Nil)
        {
            return new Array<Dynamic>();  //List<object>();
        }
        if (ValueType == LispType.NativeObject && NativeObjectValue is /*IEnumerable<object>*/Array)
        {
            return cast(NativeObjectValue, Array<Dynamic>);  //(List<object>)
        }
        if (ValueType != LispType.List)
        {
            throw CreateInvalidCastException("list");
        }
        return cast(Value, Array<Dynamic>);  //List<object>
    }

    public var DoubleValue(get, never):Float;
    function get_DoubleValue() {
        if (ValueType != LispType.Double)
        {
            throw CreateInvalidCastException("double");
        }
        return cast(Value, Float);
    }
    
    public var IntValue(get, never):Int;
    function get_IntValue() {
        if (ValueType != LispType.Int)
        {
            throw CreateInvalidCastException("int");
        }
        return cast(Value, Int);
    }    

    public var BoolValue(get, never):Bool;
    function get_BoolValue() {
        if (ValueType != LispType.Bool)
        {
            throw CreateInvalidCastException("bool");
        }
        return cast(Value, Bool);
    }

    public var NativeObjectValue(get, never):Dynamic;
    function get_NativeObjectValue() {
        if (ValueType != LispType.NativeObject && ValueType != LispType.Nil)
        {
            throw CreateInvalidCastException("native object");
        }
        return Value;
    }

    public function ToBool():Bool
    {
        if (IsBool)
        {
            return BoolValue;
        }
        if (IsInt)
        {
            return IntValue != 0;
        }
        if (IsDouble)
        {
            return Math.abs(DoubleValue) > Tolerance;
        }
        throw CreateInvalidCastException("bool", CanNotConvertTo(TypeString, "bool"));
    }

    public function ToInt():Int
    {
        if (IsBool)
        {
            return BoolValue ? 1 : 0;
        }
        if (IsInt)
        {
            return IntValue;
        }
        if (IsDouble)
        {
            return Std.int(DoubleValue);
        }
        if (IsString)
        {
            return Std.parseInt(StringValue); //Convert.ToInt32(StringValue, CultureInfo.InvariantCulture);
        }
        throw CreateInvalidCastException("int", CanNotConvertTo(TypeString, "int"));
    }
    
    public function ToDouble():Float
    {
        if (IsBool)
        {
            return BoolValue ? 1.0 : 0.0;
        }
        if (IsInt)
        {
            return IntValue; //cast(IntValue, Float); //(IntValue: Float);
        }
        if (IsDouble)
        {
            return DoubleValue;
        }
        if (IsString)
        {
            return Std.parseFloat(StringValue); //Convert.ToDouble(StringValue, CultureInfo.InvariantCulture);
        }
        throw CreateInvalidCastException("double", CanNotConvertTo(TypeString, "double"));
    }

    public var StringValue(get, never):String;
    function get_StringValue() {
        return Std.string(Value);
    }

    public function ToStr():String
    {
        if (IsSymbol)
        {
            return Std.string(Value);  //.ToStr();
        }
        if (IsString)
        {
            return StringValue;
        }
        if (IsInt)
        {
            return Std.string(IntValue);  //.ToStr(CultureInfo.InvariantCulture);
        }
        if (IsDouble)
        {
            return Std.string(DoubleValue);  //.ToStr(CultureInfo.InvariantCulture);
        }
        if (IsBool)
        {
            return BoolValue ? "#t" : "#f";
        }
        if (IsNil)
        {
            return LispToken.Nil;
        }
        if (IsList)
        {
            return ExpandContainerToString(ListValue);
        }
        if (IsFunction)
        {
            return "function " + (FunctionValue.Signature != null ? FunctionValue.Signature : "<unknown>");
        }
/* TODO -> implement later          
        if (IsNativeObject)
        {
            return NativeObjectStringRepresentation;
        }
*/        
        if (IsUndefined)
        {
            return "<undefined>";
        }
        if (IsError)
        {
            return "Error: " + Value;
        }
        return "?";
    }

    /// <summary>
    /// Comverts this value into a string representation used by the debugger module.
    /// </summary>
    /// <returns>The string representation</returns>
    public function ToStringDebugger():String
    {
        if (IsString)
        {
            return "\"" + StringValue + "\"";
        }
        return ToStr();
    }

    private static function ExpandContainerToString(/*object*/ maybeContainer:Dynamic):String
    {
        var ret = "";  //""string.Empty;

        if (maybeContainer is /*IEnumerable<object>*/Array)
        {
            var container = /*(IEnumerable<object>)*/cast(maybeContainer, Array<Dynamic>);
            for (item in container)
            {
                if (ret.length > 0)
                {
                    ret += " ";
                }
                ret += ExpandContainerToString(item);
            }
            ret = "(" + ret + ")";
        }
        else
        {
            ret += ExpandItemForContainer(maybeContainer);
        }

        return ret;
    }

    private static function ExpandItemForContainer(/*object*/ item:Dynamic):String
    {
        if (item is LispVariant)
        {
            var variant = cast(item, LispVariant);
            if (variant.IsString)
            {
                return variant.ToStringDebugger();
            }
        }
        return item.ToStr();
    }

    public function SymbolCompare(other:Dynamic) 
    {
        if (other is LispVariant)
        {
            return Value == cast(other, LispVariant).Value;  //Value.Equals(((LispVariant)other).Value);
        }
        return false;
    }

    private function CreateInvalidCastException(name:String, msg:String = "no"):haxe.Exception
    {
        var exception = new LispException('Invalid cast for $msg, value=$StringValue $name');
        exception.AddTokenInfos(Token);
        return exception;
    }

    private static function CreateInvalidOperationException(operation:String, l:LispVariant, r:LispVariant):haxe.Exception
    {
        var exception = new LispException('no $operation operator for types ${l.ValueType} and ${r.ValueType}');
        exception.AddTokenInfos(l.Token);
        return exception;
    }

    private static function CanNotConvertTo(type:String, val:String) {
        return 'can not convert $type to $val';
    }

    public function Add(/*object*/ value:Dynamic):Void
    {
        if (ValueType != LispType.List)
        {
            throw CreateInvalidCastException("list");
        }
        var list = cast(Value, Array<Dynamic>);  //List<object>
        list.Add(value);
    }

    public static function op_add(l:LispVariant, r:LispVariant):LispVariant
    {
        if (l.IsString || r.IsString)
        {
            return LispVariant.forValue(l.StringValue + r.StringValue, LispType.String);
        }
        if (l.IsDouble || r.IsDouble)
        {
            var ret:Float = l.ToDouble() + r.ToDouble();
            return LispVariant.forValue(ret, LispType.Double);
        }
        if (l.IsInt || r.IsInt)
        {
            return LispVariant.forValue(l.ToInt() + r.ToInt(), LispType.Int);
        }
        if (l.IsList && r.IsList)
        {
            var newList = new Array<Dynamic>();  //List<object>();
            newList.AddRange(l.ListValue);
            newList.AddRange(r.ListValue);
            return LispVariant.forValue(newList);
        }
        throw CreateInvalidOperationException("+", l, r);
    }

    public static function op_minus(l:LispVariant, r:LispVariant):LispVariant
    {
        if (l.IsDouble || r.IsDouble)
        {
            return LispVariant.forValue(l.ToDouble() - r.ToDouble(), LispType.Double);
        }
        if (l.IsInt || r.IsInt)
        {
            return LispVariant.forValue(l.ToInt() - r.ToInt(), LispType.Int);
        }
        throw CreateInvalidOperationException("-", l, r);
    }
    
    public static function op_mul(l:LispVariant, r:LispVariant):LispVariant
    {
        if (l.IsDouble || r.IsDouble)
        {
            return LispVariant.forValue(l.ToDouble() * r.ToDouble(), LispType.Double);
        }
        if (l.IsInt || r.IsInt)
        {
            return LispVariant.forValue(l.ToInt() * r.ToInt(), LispType.Int);
        }
        throw CreateInvalidOperationException("*", l, r);
    }

    public static function op_divide(l:LispVariant, r:LispVariant):LispVariant
    {
        if (l.IsDouble || r.IsDouble)
        {
            return LispVariant.forValue(l.ToDouble() / r.ToDouble(), LispType.Double);
        }
        if (l.IsInt || r.IsInt)
        {
            return LispVariant.forValue(l.ToInt() / r.ToInt(), LispType.Int);
        }
        throw CreateInvalidOperationException("/", l, r);
    }

    public static function op_modulo(l:LispVariant, r:LispVariant):LispVariant
    {
        if (l.IsDouble || r.IsDouble)
        {
            return LispVariant.forValue(l.ToDouble() % r.ToDouble(), LispType.Double);
        }
        if (l.IsInt || r.IsInt)
        {
            return LispVariant.forValue(l.ToInt() % r.ToInt(), LispType.Int);
        }
        throw CreateInvalidOperationException("%", l, r);
    }

    public static function op_less(l:LispVariant, r:LispVariant):LispVariant
    {
        if (l.IsDouble || r.IsDouble)
        {
            return LispVariant.forValue(l.ToDouble() < r.ToDouble());
        }
        if (l.IsInt || r.IsInt)
        {
            return LispVariant.forValue(l.ToInt() < r.ToInt());
        }
        if (l.IsString || r.IsString)
        {
            return LispVariant.forValue(LispUtils.StringCompare(l.ToStr(), r.ToStr()) < 0);
        }
        throw CreateInvalidOperationException("< or >", l, r);
    }

    public static function op_greater(l:LispVariant, r:LispVariant):LispVariant
    {
        return op_less(r, l);
    }

    public static function op_less_than(l:LispVariant, r:LispVariant):LispVariant
    {
        if (l.IsDouble || r.IsDouble)
        {
            return LispVariant.forValue(l.ToDouble() <= r.ToDouble());
        }
        if (l.IsInt || r.IsInt)
        {
            return LispVariant.forValue(l.ToInt() <= r.ToInt());
        }
        if (l.IsString || r.IsString)
        {
            return LispVariant.forValue(LispUtils.StringCompare(l.ToStr(), r.ToStr()) <= 0);
        }
        throw CreateInvalidOperationException("<= or >=", l, r);
    }

    public static function op_greater_than(l:LispVariant, r:LispVariant):LispVariant
    {
        return op_less_than(r, l);
    }

    public static function op_equal(l:LispVariant, r:LispVariant):LispVariant
    {
        return LispVariant.forValue(EqualOp(r, l));
    }

    public static function op_not_equal(l:LispVariant, r:LispVariant):LispVariant
    {
        return LispVariant.forValue(!EqualOp(r, l));
    }

    public static function EqualOp(l:LispVariant, r:LispVariant):Bool
    {
        if (l.IsNativeObject && r.IsNativeObject)
        {
            return l.NativeObjectValue == r.NativeObjectValue;
        }
        if (l.IsSymbol || r.IsSymbol)
        {
            return l.IsSymbol && r.IsSymbol && (l.ToStr() == r.ToStr());
        }
        if (l.IsBool && r.IsBool)
        {
            return l.BoolValue == r.BoolValue;
        }
        if (l.IsNil || r.IsNil)
        {
            return l.IsNil && r.IsNil;
        }
        if (l.IsList && r.IsList)
        {
            return l.ListValue == r.ListValue;  //l.ListValue.SequenceEqual(r.ListValue);
        }
        if (l.IsUndefined || r.IsUndefined)
        {
            return l.IsUndefined && r.IsUndefined;
        }
        if (l.IsString || r.IsString)
        {
            return l.ToStr() == r.ToStr();
        }
        if (l.IsDouble || r.IsDouble)
        {
            return Math.abs(l.ToDouble() - r.ToDouble()) < Tolerance;
        }
        if (l.IsInt || r.IsInt)
        {
            return l.ToInt() == r.ToInt();
        }
        throw CreateInvalidOperationException("==", l, r);
    }    
}

@:forward(Value)
abstract OpLispVariant(LispVariant) {
    public inline function new(val:LispVariant) {
        this = val;
    }

    @:op(A+B)
    public function add(rightVal:OpLispVariant):OpLispVariant {
        return new OpLispVariant(LispVariant.forValue(this.Value + rightVal.Value));
    }

    @:op(A-B)
    public function sub(rightVal:OpLispVariant):LispVariant {
        return LispVariant.forValue(this.Value - rightVal.Value);
    }

    @:op(A*B)
    public function mult(rightVal:OpLispVariant):LispVariant {
        return LispVariant.forValue(this.Value * rightVal.Value);
    }
}
