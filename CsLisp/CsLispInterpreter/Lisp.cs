/*
 * FUEL(isp) is a fast usable embeddable lisp interpreter.
 *
 * Copyright (c) 2016 Michael Neuroth
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

/*
 *          Source Code     "(do (def a 42) (print a))"
 *            
 *        ---------------
 *        |  Tokenizer  |
 *        ---------------
 *        
 *        List<LispTokens>
 *          
 *        ---------------
 *        |    Parser   |
 *        ---------------
 *        
 *        AST = List<object> object=LispVariant|List<object>
 * using
 *        -----------------
 *    --- | Expand Macros | 
 *    |   -----------------
 *    |    
 *    |   AST = List<object> object=LispVariant|List<object>
 *    |
 *    |   --------------------           ---------------             --------
 *    --> | Interpreter/Eval |   --->    | Environment |     --->    | .NET |
 *        --------------------           ---------------             --------
 * 
 */

using System;
using System.Collections.Generic;

namespace CsLisp
{
    /// <summary>
    /// The FUEL lisp interpreter.
    /// </summary>
    public class Lisp
    {
        #region constants

        public const string ProgramName = "fuel";

        public const string Name = "FUEL(isp)";
        public const string Version = "v0.99.3";
        public const string Date = "20.9.2018";
        public const string Copyright = "(C) by Michael Neuroth";

        public const string Platform = ".NET/C#";

        public const string License = "MIT-License";
        public const string LicenseUrl = "http://opensource.org/licenses/MIT";

        public const string Info = Name + " is a fast usable embeddable lisp interpreter";

        #endregion

        #region evaluation

        /// <summary>
        /// Evals the specified lisp code.
        /// An exception may occure if the lisp code is invalid.
        /// </summary>
        /// <param name="lispCode">The lisp code.</param>
        /// <param name="scope">The scope.</param>
        /// <param name="moduleName">The module name and path.</param>
        /// <param name="tracing">if set to <c>true</c> [tracing].</param>
        /// <param name="nativeItems">The dictionary with native items.</param>
        /// <returns>The result of the script evaluation</returns>
        public static LispVariant Eval(string lispCode, LispScope scope = null, string moduleName = null, bool tracing = false, Dictionary<string, object> nativeItems = null)
        {
            // first create global scope, needed for macro expanding
            var currentScope = scope ?? LispEnvironment.CreateDefaultScope();
            currentScope.ModuleName = moduleName;
            currentScope.Tracing = tracing;            
            RegisterNativeObjects(nativeItems, currentScope);
            int offset;
            string code = LispUtils.DecorateWithBlock(lispCode, out offset);
            var ast = LispParser.Parse(code, offset, currentScope);
#if ENABLE_COMPILE_TIME_MACROS 
            var expandedAst = LispInterpreter.ExpandMacros(ast, currentScope);
#else
            var expandedAst = ast;
#endif
            var result = LispInterpreter.EvalAst(expandedAst, currentScope);
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
        /// <returns>The result</returns>
        public static LispVariant SaveEval(string lispCode, string moduleName = null, bool verboseErrorOutput = false, bool tracing = false)
        {
            LispVariant result;
            try
            {
                result = Eval(lispCode, scope: null, moduleName: moduleName, tracing: tracing);
            }
            catch (Exception exc)
            {
                Console.WriteLine("\nError executing script.\n\n{0} --> line={1} start={2} stop={3} module={4}", exc.Message, exc.Data[LispUtils.LineNo], exc.Data[LispUtils.StartPos], exc.Data[LispUtils.StopPos], exc.Data[LispUtils.ModuleName]);
                var stackInfo = exc.Data[LispUtils.StackInfo];
                Console.WriteLine("\nCallstack:\n{0}", stackInfo != null ? stackInfo : "<not available>");
                if (verboseErrorOutput)
                {
                    Console.WriteLine("\nNative callstack:");
                    Console.WriteLine("Exception in eval(): {0} \ndata={1}", exc, exc.Data);
                }
                result = LispVariant.CreateErrorValue(exc.Message);
            }
            return result;
        }

        #endregion

        #region private methods

        private static void RegisterNativeObjects(Dictionary<string, object> nativeItems, LispScope currentScope)
        {
            if (nativeItems != null)
            {
                foreach (KeyValuePair<string, object> item in nativeItems)
                {
                    currentScope[item.Key] = new LispVariant(LispType.NativeObject, item.Value);
                }
            }
        }

        #endregion
    }
}
