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

//import sys.io.File;
using LispUtils;
using LispUtils.Ref;
using LispEnvironment;
using LispToken;
using LispVariant;
using StringTools;

class TextWriter {
    public var Text:Ref<String>;

    public function new(textBuffer:Ref<String> = null) {
        Text = textBuffer;
    }

    public function WriteLine(text:String="", ?a1, ?a2, ?a3) {
        if (Text != null) 
        {
            Text.value += text + "\n";
        }
#if (node || sys)
        else 
        {
            //File.write(text);
            // var temp = text;
            // if( a1 is null ) {
            //     temp += a1;
            // }
            Sys.println(text);
        }
#else
        else 
        {
            trace(text);
        }
#end
    }
    public function Write(text:String="", ?a1, ?a2, ?a3) {
        if (Text != null) 
        {
            Text.value += text;
        }
#if (node || sys)
        else
        {
            //File.write("stdout").writeString(text);
            Sys.print(text);
        }
#else
        else
        {
            trace(text);
        }
#end
    }
    public function Flush():Void {
#if (node || sys)
        Sys.stdout().flush();
#else
        trace("Flush() is not supported!");
#end        
    }
}

class TextReader {
    public function new() {
    }

    public function ReadLine():String {
#if (node || sys)
        return Sys.stdin().readLine();
#else
        trace("ReadLine() is not supported!");
        return "";
#end
    }
}
 
class LispScope extends haxe.ds.StringMap<Dynamic>/*Map<String,Dynamic>*/ {
    //public var _map:Map<String,Dynamic> = new Map<String,Dynamic>();

    public var Debugger:Dynamic;

    public var Tracing:Bool;

    /// <summary>
    /// Gets and sets all tokens of the current script,
    /// used for debugging purpose and for showing the 
    /// position of an error.
    /// </summary>
    public var Tokens:Array<LispToken>;
    
    /// <summary>
    /// Gets and sets the next and previous scope,
    /// used for debugging purpose to show the 
    /// call stack
    /// </summary>
    public var Next:LispScope;  //{ get; private set; }
    public var Previous:LispScope;  //{ get; set; }

    public var ClosureChain:LispScope;

    public var ModuleName:String;

    public var IsInEval:Bool;

    public var CurrentToken:LispToken;

    public var Name:String;

    public var GlobalScope:LispScope;
    public var Output:TextWriter;
    public var Input:TextReader;

    public var IsInReturn:Bool;

    public var NeedsLValue:Bool;

    /// <summary>
    /// Gets or sets user data.
    /// Needed for debugging support --> set function name to LispScope
    /// </summary>
    /// <value> The user data. </value>
    public var UserData:Dynamic;

    /// <summary>
    /// Gets or sets the user documentation information.
    /// </summary>
    /// <value>The user documentation.</value>
    public var UserDoc:TupleReturn<String,String>;

    public var CurrentLineNo(get, null):Int;    
    function get_CurrentLineNo():Int
    {
        return CurrentToken != null ? CurrentToken.LineNo : -1;
    }
    
    public function get_value(name:String):LispVariant
    {
        #if cs
        var mm:haxe.ds.StringMap<Dynamic> = this;
        var ret:LispVariant = mm.get(name);
        #else
        var ret = get(name);
        #end
        return ret;        
    }

    public function get_scope(name:String):LispScope
    {
        #if cs
        var mm:haxe.ds.StringMap<Dynamic> = this;
        var ret:LispScope = mm.get(name);
        #else
        var ret = get(name);
        #end
        return ret;        
    }
    
    public function new() {
        super();
    }

    private static function init(ret:LispScope, fcnName:String, globalScope:LispScope = null, moduleName:String = null):LispScope {
        ret.Name = fcnName;
        ret.GlobalScope = globalScope != null ? globalScope : ret;
        ret.ModuleName = moduleName;
        if (ret.ModuleName == null && globalScope != null)
        {
            ret.ModuleName = globalScope.ModuleName;
        }
        ret.CurrentToken = null;
        ret.Input = new TextReader();  //Console.In;
        ret.Output = new TextWriter(); //Console.Out;
        return ret;
    }

    public static function forFunction(fcnName:String, globalScope:LispScope = null, moduleName:String = null):LispScope {
        var ret = new LispScope();
        return init(ret, fcnName, globalScope, moduleName);
    }

    public function ContainsKey(key:String):Bool {
        return exists(key);
    }

    public function PushNextScope(nextScope:LispScope)
    {
        Next = nextScope;
        nextScope.Previous = this;
    }

    public function PopNextScope()
    {
        Next.Previous = null;
        Next = null;
    }

    /// <summary>
    /// Resolves the given element in this scope.
    /// </summary>
    /// <param name="elem">The element.</param>
    /// <param name="isFirst">Is the element the first one in the list.</param>
    /// <returns>Resolved value or null</returns>
    public function ResolveInScopes(elem:Dynamic, isFirst:Bool):Dynamic
    {
        var result = new Ref<Dynamic>(null);

        // try to access the cached function value (speed optimization)
        var elemAsVariant:LispVariant = elem /*as LispVariant*/;
        if (elemAsVariant != null && elemAsVariant.CachedFunction != null)
        {
            return elemAsVariant.CachedFunction;
        }            

        var name = elem.ToStr();
        var foundClosureScope = new Ref<LispScope>(new LispScope());
        // first try to resolve in this scope
        if (this.TryGetValue(name, /*out*/ result))
        {
            UpdateFunctionCache(elemAsVariant, result.value, isFirst);
        }
        // then try to resolve in global scope
        else if (GlobalScope != null &&
                 GlobalScope.TryGetValue(name, /*out*/ result))
        {
            UpdateFunctionCache(elemAsVariant, result.value, isFirst);
        }
        // then try to resolve in closure chain scope(s)
        else if (IsInClosureChain(name, /*out*/ foundClosureScope, /*out*/ result))
        {
            UpdateFunctionCache(elemAsVariant, result.value, isFirst);
        }
        // then try to resolve in scope of loaded modules
        else if (LispEnvironment.IsInModules(name, GlobalScope))
        {
            result.value = LispEnvironment.GetFunctionInModules(name, GlobalScope);
        }
        else
        {
            // activate this code if symbols must be resolved in parameter evaluation --> (println blub)
            //if (elemAsVariant != null && elemAsVariant.IsSymbol && name != "fuellib")
            //{
            //    throw new LispException('Could not resolve symbol $name');
            //}
            result.value = elem;
        }

        return result.value;
    }

    /// <summary>
    /// Searches the given symbol in the scope environment and 
    /// sets the value if found.
    /// Throws an exception if the symbol is not found in the scope 
    /// environment.
    /// </summary>
    /// <param name="symbolName">Name of the symbol.</param>
    /// <param name="value">The value.</param>
    /// <exception cref="LispException">Symbol  + symbolName +  not found</exception>
    public function SetInScopes(symbolName:String, /*object*/ value:Dynamic)
    {
        var foundClosureScope = new Ref<LispScope>(null);
        var val =new Ref<Dynamic>(null);
        if (!LispUtils.IsNullOrEmpty(symbolName))
        {
            if (ContainsKey(symbolName))
            {
                this.set(symbolName, value);
            }
            else if (IsInClosureChain(symbolName, /*out*/ foundClosureScope, /*out*/ val))
            {
                foundClosureScope.value.set(symbolName, value);
            }
            else if (GlobalScope != null && GlobalScope.ContainsKey(symbolName))
            {
                GlobalScope.set(symbolName, value);
            }
            else
            {
                throw LispException.fromScope("Symbol " + symbolName + " not found", this);
            }
        }
    }

    public function GetCallStackSize():Int
    {
        var current:LispScope = this;
        var i = 0;
        do
        {
            current = current.Previous;
            i++;
        } while (current != null);
        return i;
    }

    public function DumpStack(currentLevel:Int = -1):Void
    {
        var stackInfo = DumpStackToString(currentLevel);
        Output.WriteLine(stackInfo);
    }

    public function DumpStackToString(currentLevel:Int=-1):String 
    {
        var ret = "";  //string.Empty;
        var current:LispScope = this;
        var i = GetCallStackSize();
        do
        {
            var currentItem = currentLevel == i ? "-->" : "   ";

            ret = '${currentItem}${i} name=${current.Name} lineno=${current.CurrentLineNo} module=${current.ModuleName}\n' + ret;  //TODO ,3 ,5 ,-35 ,-4
            current = current.Previous;
            i--;
        } while (current != null);
        return ret;
    }

    public function DumpVars():Void
    {
        Dump(function (v:LispVariant) { return !v.IsFunction || (v.IsFunction && !v.FunctionValue.IsBuiltIn); });
    }

    public function DumpFunctions():Void
    {
        Dump(function (v:LispVariant) { return v.IsFunction; }, function (v:LispVariant) { return " : module=" + ExtractModuleName(v.FunctionValue.ModuleName); });

        ProcessMetaScope(LispEnvironment.Modules, function (module)
        {
            var mod = cast(module.Value, LispScope);
            if (mod != null)
            {
                mod.DumpFunctions();
            }
        });
    }

    public function DumpMacros():Void
    {
        ProcessMetaScope(LispEnvironment.Macros, function (_macro) { Output.WriteLine(_macro.Key); });
    }

    public function DumpBuiltinFunctions():Void
    {
        Dump(function (v:LispVariant) { return v.IsFunction && v.FunctionValue.IsBuiltIn; });
    }

    public function DumpBuiltinFunctionsHelp():Void
    {
        Dump(function (v:LispVariant) { return v.IsFunction && v.FunctionValue.IsBuiltIn; }, function (v:LispVariant) { return v.FunctionValue.Documentation; }, /*showHelp:*/ true);
    }

    public function DumpBuiltinFunctionsHelpFormated():Void
    {
        Dump(function (v:LispVariant) { return v.IsFunction && v.FunctionValue.IsBuiltIn; }, /*sort:*/ true, /*format:*/ function (v:LispVariant) { return v.FunctionValue.FormatedDoc; });
    }

    public function DumpModules():Void
    {
        ProcessMetaScope(LispEnvironment.Modules, function (module) { return Output.WriteLine(module.Key); });
    }

    public function GetPreviousToken(token:LispToken):LispToken
    {
        var previous:LispToken = null;
        if (Tokens != null)
        {
            for (item in Tokens)
            {
                if (item == token)
                {
                    return previous;
                }
                previous = item;
            }
        }
        return null;
    }
    
    private static function UpdateFunctionCache(elemAsVariant:LispVariant, /*object*/ value:Dynamic, isFirst:Bool)
    {
        var valueAsVariant:LispVariant = value /*as LispVariant*/;
        if (isFirst && elemAsVariant != null && valueAsVariant != null && valueAsVariant.IsFunction)
        {
            //if (elemAsVariant.CachedFunction != null)
            //{
            //    throw new LispException("Cache already set !!!");
            //}
            elemAsVariant.CachedFunction = valueAsVariant;
        }
    }

    public function DumpBuiltinFunctionsHelpHtmlFormated()
    {
        Output.WriteLine("<html>");
        Output.WriteLine("<head>");
        Output.WriteLine("<title>");
        Output.WriteLine("Documentation of fuel language");
        Output.WriteLine("</title>");
        Output.WriteLine("</head>");
        Output.WriteLine("<h2>Documentation of builtin functions of the fuel language:</h2>");
        Output.WriteLine("<body>");
        Dump(function (v:LispVariant):Bool { return v.IsFunction && v.FunctionValue.IsBuiltIn; }, null, /*sort:*/ true, false, /*format:*/function (v:LispVariant) { return v.FunctionValue.HtmlFormatedDoc; });
        Output.WriteLine("</body>");
        Output.WriteLine("</html>");
    }

    public function GetFunctionsHelpFormated(functionName:String, /*Func<string, string, bool>*/ select:Dynamic = null):String
    {
        var result = "";  //string.Empty;
        for (key in keys())
        {
            if (select != null)
            {
                if (select(key, functionName))
                {
                    var value = LispUtils.CastDynamicToLispVariant(get(key));  // cast(get(key), LispVariant)  //(LispVariant)this[key];
                    result += value.FunctionValue.FormatedDoc;
                }
            }
            else if (key.startsWith(functionName))
            {
                var value = LispUtils.CastDynamicToLispVariant(get(key));  //cast(get(key), LispVariant);  //(LispVariant)this[key];
                result += value.FunctionValue.FormatedDoc;
            }
        }
        return result;
    }
    
    /// <summary>
    /// Determines whether the given name is available in the closure chain.
    /// </summary>
    /// <param name="name">The name.</param>
    /// <param name="closureScopeFound">The closure scope found.</param>
    /// <param name="value">The found value.</param>
    /// <returns>True if name was found.</returns>
    private function IsInClosureChain(name:String, /*out LispScope*/ closureScopeFound:Ref<LispScope>, /*out object*/ value:Ref<Dynamic>):Bool
    {
        if (ClosureChain != null)
        {
            if (ClosureChain.TryGetValue(name, /*out*/ value))
            {
                closureScopeFound.value = ClosureChain;
                return true;
            }
            return ClosureChain.IsInClosureChain(name, /*out*/ closureScopeFound, /*out*/ value);
        }

        closureScopeFound.value = null;
        value.value = null;
        return false;
    }

    private static function ExtractModuleName(moduleName:String):String
    {
        if (moduleName != null)
        {
            var temp = moduleName.startsWith(LispEnvironment.EvalStrTag) ? moduleName.substr(LispEnvironment.EvalStrTag.length, moduleName.indexOf(":", LispEnvironment.EvalStrTag.length) - LispEnvironment.EvalStrTag.length) : moduleName;
            return temp;
        }
        return moduleName;
    }

    private function ProcessMetaScope(metaScope:String, /*Action<KeyValuePair<string, object>>*/ action:Dynamic):Void
    {
        if (ContainsKey(metaScope))
        {
            var items = LispUtils.CastDynamicToLispScope(get(metaScope));  //cast(get(metaScope), LispScope);
            if (items != null)
            {
                for (/*KeyValuePair<string, object>*/ item in items)
                {
                    action(item);
                }
            }
        }
    }

    private function Dump(/*Func<LispVariant, bool>*/ select:Dynamic, /*Func<LispVariant, string>*/ show:Dynamic = null, showHelp:Bool = false, sort:Bool = false, /*Func<LispVariant, string>*/ format:Dynamic = null):Void
    {
        //var keys = keys();  //Keys.ToList();
        //trace("==========>",keys);
        if (sort)
        {
//TODO            keys.Sort();                
            // var temp_keys = new Array<String>();
            // for (key in keys())
            // {
            //     temp_keys.push(this.get(key));
            // }
            // temp_keys = temp_keys.sort( funtion(x, y) -> x )
        }
        for (key in keys())
        {
            if (!key.startsWith(LispEnvironment.MetaTag))
            {
                var value:LispVariant = LispUtils.CastDynamicToLispVariant(get_value(key));  //cast(get(key), LispVariant);  //(LispVariant)this[key];
                if (select(value))
                {
                    if (format != null)
                    {
                        Output.WriteLine('${format(value)}');
                    }
                    else
                    {
                        var info:String = show != null ? show(value) : "" /*string.Empty*/;
                        if (showHelp)
                        {
                            Output.WriteLine('${key} --> ${value.FunctionValue.Signature}');  //TODO ,20
                            if (!/*string*/LispUtils.IsNullOrEmpty(info))
                            {
                                Output.WriteLine('${""}     ${info}');  //TODO ,20
                            }
                        }
                        else
                        {
                            Output.WriteLine('${key} --> ${value.ToStringDebugger()} : ${value.TypeString} ${info}');   //TODO ,20  ,-40
                        }
                    }
                }
            }
        }
    }
}
