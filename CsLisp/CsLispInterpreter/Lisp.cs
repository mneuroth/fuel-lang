using System;

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
        public const string Date = "11.1.2016";
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
        /// <returns></returns>
        public static LispVariant Eval(string lispCode, LispScope scope = null)
        {
            // first create global scope, needed for macro expanding
            var globalScope = scope ?? LispEnvironment.CreateDefaultScope();
            var ast = LispParser.Parse(lispCode, globalScope);
            var expandedAst = LispInterpreter.ExpandMacros(ast, globalScope);
            var result = LispInterpreter.EvalAst(expandedAst, globalScope);
            globalScope.Finished = true; // needed for debugging support                
            return result;
        }

        /// <summary>
        /// Evals the specified lisp code.
        /// All exceptions will be filtered and an error value will be returned.
        /// </summary>
        /// <param name="lispCode">The lisp code.</param>
        /// <param name="scope">The scope.</param>
        /// <returns></returns>
        public static LispVariant SaveEval(string lispCode, LispScope scope = null)
        {
            LispVariant result;
            try
            {
                result = Eval(lispCode, scope);
            }
            catch (Exception exc)
            {
                Console.WriteLine("Exception in eval(): {0}", exc);
                result = LispVariant.CreateErrorValue(exc.Message);
            }
            return result;
        }

        #endregion
    }
}
