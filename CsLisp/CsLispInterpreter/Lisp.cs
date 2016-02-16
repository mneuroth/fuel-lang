﻿using System;

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
        public const string Version = "v0.99.1";
        public const string Date = "16.2.2016";
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
        /// <param name="updateFinishedFlag">Flag which indicates request to update the finished flag at the scop.</param>
        /// <returns>The result of the script evaluation</returns>
        public static LispVariant Eval(string lispCode, LispScope scope = null, string moduleName = null, bool updateFinishedFlag = true)
        {
            // first create global scope, needed for macro expanding
            var globalScope = scope ?? LispEnvironment.CreateDefaultScope();
            globalScope.ModuleName = moduleName;
            var ast = LispParser.Parse(lispCode, globalScope);
            var expandedAst = LispInterpreter.ExpandMacros(ast, globalScope);
            var result = LispInterpreter.EvalAst(expandedAst, globalScope);
            if (updateFinishedFlag)
            {
                globalScope.Finished = true; // needed for debugging support                
            }
            return result;
        }

        /// <summary>
        /// Evals the specified lisp code.
        /// All exceptions will be filtered and an error value will be returned.
        /// </summary>
        /// <param name="lispCode">The lisp code.</param>
        /// <param name="scope">The scope.</param>
        /// <param name="moduleName">The current module name.</param>
        /// <param name="verboseErrorOutput">if set to <c>true</c> [verbose error output].</param>
        /// <returns>The result</returns>
        public static LispVariant SaveEval(string lispCode, LispScope scope = null, string moduleName = null, bool verboseErrorOutput = false)
        {
            LispVariant result;
            try
            {
                result = Eval(lispCode, scope, moduleName);
            }
            catch (Exception exc)
            {
                Console.WriteLine("\nError executing script.\n\n{0} data={1}", exc.Message, exc.Data[LispUtils.ModuleName]);
                if (verboseErrorOutput)
                {
                    Console.WriteLine("\nCallstack:");
                    Console.WriteLine("Exception in eval(): {0} \ndata={1}", exc, exc.Data);
                }
                result = LispVariant.CreateErrorValue(exc.Message);
            }
            return result;
        }

        #endregion
    }
}
