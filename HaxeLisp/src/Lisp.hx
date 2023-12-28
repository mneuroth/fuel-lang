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

import LispUtils;
import LispVariant;

class Lisp {
    public /*const*/static var ProgramName = "fuel";

    public /*const*/static var Name = "FUEL(isp)";
    public /*const*/static var Version = "v0.99.6";
    public /*const*/static var Date = "16.12.2023";
    public /*const*/static var Copyright = "(C) by Michael Neuroth";

    public /*const*/static var Platform = "Haxe";  //".NET/C#";

    public /*const*/static var License = "MIT-License";
    public /*const*/static var LicenseUrl = "http://opensource.org/licenses/MIT";

    public /*const*/static var Info = Name + " is a fast usable embeddable lisp interpreter";

    /// <summary>
    /// Evals the specified lisp code.
    /// An exception may occure if the lisp code is invalid.
    /// </summary>
    /// <param name="lispCode">The lisp code.</param>
    /// <param name="scope">The scope.</param>
    /// <param name="moduleName">The module name and path.</param>
    /// <param name="tracing">if set to <c>true</c> [tracing].</param>
    /// <param name="onlyMacroExpand">if set to <c>true</c> [macro expanding].</param>
    /// <param name="nativeItems">The dictionary with native items.</param>
    /// <returns>The result of the script evaluation</returns>
    public static function Eval(lispCode:String, scope:LispScope = null, moduleName:String = null, tracing = false, onlyMacroExpand = false, /*Dictionary<string, object>*/ nativeItems:haxe.ds.StringMap<Dynamic> = null):LispVariant
    {
        // first create global scope, needed for macro expanding
        var currentScope = scope==null ? LispEnvironment.CreateDefaultScope() : scope;
        currentScope.ModuleName = moduleName;
        currentScope.Tracing = tracing;
        RegisterNativeObjects(nativeItems, currentScope);
        var offset = new Ref<Int>(0);
        var code = LispUtils.DecorateWithBlock(lispCode, /*out*/ offset);
        var ast:Dynamic = LispParser.Parse(code, offset.value, currentScope);
#if ENABLE_COMPILE_TIME_MACROS 
        var expandedAst = LispInterpreter.ExpandMacros(ast, currentScope);
#else
        var expandedAst:Dynamic = ast;
#end
        var result:LispVariant = null;
        if (onlyMacroExpand)
        {
            result = new LispVariant(expandedAst);
        }
        else
        {
            result = LispInterpreter.EvalAst(expandedAst, currentScope);
        }
        return result;
    }

    /// <summary>
    /// Evals the specified lisp code.
    /// All exceptions will be filtered and an error value will be returned.
    /// </summary>
    /// <param name="lispCode">The lisp code.</param>
    /// <param name="moduleName">The current module name.</param>
    /// <param name="verboseErrorOutput">if set to <c>true</c> [verbose error output].</param>
    /// <param name="tracing">if set to <c>true</c> [tracing].</param>
    /// <param name="onlyMacroExpand">if set to <c>true</c> [macro expanding].</param>
    /// <returns>The result</returns>
    public static function SaveEval(lispCode:String, moduleName:String = null, verboseErrorOutput:Bool = false, tracing:Bool = false, onlyMacroExpand:Bool = false):LispVariant
    {
        var result:LispVariant;
        try
        {
            result = Eval(lispCode, /*scope:*/ null, /*moduleName:*/ moduleName, /*tracing:*/ tracing, /*onlyMacroExpand:*/ onlyMacroExpand);
        }
        catch (exc:/*haxe.*/LispException)
        {
            trace(exc);
            Console.WriteLine('\nError executing script.\n\n${exc.message} --> line=${exc.ExcData.get(LispUtils.LineNo)} start=${exc.ExcData.get(LispUtils.StartPos)} stop=${exc.ExcData.get(LispUtils.StopPos)} module=${exc.ExcData.get(LispUtils.ModuleName)}');
            var stackInfo = exc.ExcData.get(LispUtils.StackInfo);
            Console.WriteLine('\nCallstack:\n${stackInfo != null ? stackInfo : "<not available>"}');
            if (verboseErrorOutput)
            {
                Console.WriteLine("\nNative callstack:");
                Console.WriteLine('Exception in eval(): ${exc} \ndata=${exc.ExcData}');
            }
            trace("EXCEPTION", exc);
            result = LispVariant.CreateErrorValue(exc.message);
        }
        return result;
    }

    private static function RegisterNativeObjects(/*Dictionary<string, object>*/ nativeItems:haxe.ds.StringMap<Dynamic>, currentScope:LispScope)
    {
        if (nativeItems != null)
        {
            for (/*KeyValuePair<string, object>*/ item in nativeItems)
            {
                currentScope.set(item.Key, new LispVariant(LispType.NativeObject, item.Value));
            }
        }
    }
}
