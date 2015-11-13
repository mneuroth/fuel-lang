using System;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using Microsoft.CSharp;

// TODO --> CODE CLEANUPS needed !!!

namespace CsLisp
{
    /// <summary>
    /// The compiler module for FUEL. 
    /// Compiles a Lisp program into a C# program.
    /// </summary>
    public class LispCompiler : ILispCompiler
    {
        #region public methods 

        /// <summary>
        /// Compiles the specified lisp code into C# code.
        /// </summary>
        /// <param name="code">The lisp code.</param>
        /// <returns>The C# code packed into a LispVariant</returns>
        public /*static*/ LispVariant CompileToCsCode(string code)
        {
            var globalScope = LispEnvironment.CreateDefaultScope();
            var ast = LispParser.Parse(code, globalScope);
            //var expandedAst = ExpandMacros(ast, globalScope);
            var compileResult = Compile(ast, globalScope, "        ", true, "scope");
            var csCode = "namespace CsLisp\n{\nusing System;\nclass CompiledLisp\n{\n    // functions:\n" +
                        ShiftLines(compileResult.Item2, "    ") +
                        "\n    public static LispVariant LispMain(string[] args)\n    {\n        var scope = new LispScope();\n\n" +
                        ShiftLines(compileResult.Item1, "        ") +
                         "    }\n\n    public static void Main(string[] args)\n    {\n        var startTickCount = Environment.TickCount;\n        LispMain(args);\n        Console.WriteLine(string.Format(\"Execution time = {0} s\", (Environment.TickCount - startTickCount)*0.001));\n    }\n}\n}";
            return new LispVariant(csCode);
        }

        /// <summary>
        /// Compile C# code into an assembly.
        /// see: https://support.microsoft.com/de-de/kb/304655
        /// </summary>
        /// <param name="csCode">The cs code.</param>
        /// <param name="outputFileName">Name of the output file.</param>
        /// <param name="debug">if set to <c>true</c> use debug option for C# compiler.</param>
        /// <returns></returns>
        public /*static*/ CompilerResults CompileCsCodeToAssembly(string csCode, string outputFileName, bool debug = false)
        {
            var codeProvider = new CSharpCodeProvider();
            var parameters = new CompilerParameters();

            parameters.GenerateExecutable = true;
            parameters.OutputAssembly = outputFileName;
            parameters.CompilerOptions = "/reference:cslispinterpreter.dll";
            if (debug)
            {
                parameters.CompilerOptions += " /debug+";                
            }

            return codeProvider.CompileAssemblyFromSource(parameters, csCode);
        }

        /// <summary>
        /// Compiles the given lisp code into an executable.
        /// </summary>
        /// <param name="lispCode">The lisp code.</param>
        /// <param name="exeFileName">Name of the executable file.</param>
        /// <returns></returns>
        public /*static*/ LispVariant CompileToExe(string lispCode, string exeFileName)
        {
            var csCode = CompileToCsCode(lispCode);
            var exeResult = CompileCsCodeToAssembly(csCode.StringValue, exeFileName);
            foreach (var error in exeResult.Errors)
            {
                Console.WriteLine(error.ToString());
            }
            return new LispVariant(exeResult.Errors.Count==0);
        }

        #endregion

        #region private methods

        private static Tuple<string, string> Compile(object ast, LispScope globalScope, string shift, bool lastMustReturn, string scopeName)
        {
            if(ast is IEnumerable<object>)
            {
                return Compile((IEnumerable<object>)ast, globalScope, string.Empty, lastMustReturn, scopeName);
            }
            var value = ast as LispVariant;
            var temp = value != null ? value.ToStringCompiler() : ast.ToString();
            return new Tuple<string, string>(temp, string.Empty);
        }

        private static Tuple<string, string> Compile(IEnumerable<object> ast, LispScope globalScope, string shift, bool lastMustReturn, string scopeName)
        {
            string code = string.Empty;
            string functions = string.Empty;

            var astWithResolvedValues = LispInterpreter.ResolveArgsInScopes(globalScope, ast, true);

            // do --> sequence, d. h. ignorieren
            // defn --> funktion deklarieren
            // def --> variable deklarieren ==> LispVariant a = value; 
            // quote
            // setf --> variable zuweisen
            // while 
            // lambda
            // if
            // weitere special forms speziell behandeln...
            // --> and, or, apply, quasiquote, definemacro
            object first = null;
            string args = string.Empty;
            string separator = ", ";
            string closeStatement = "";

            for (int i = 0; i < astWithResolvedValues.Count; i++ )
            {
                var elem = astWithResolvedValues[i];
                if (first == null)
                {
                    first = elem;
                    var func = (LispVariant)first;
                    if (func.IsSymbol)
                    {
                        // process normal function call
                        string cast = GetFunctionCast(astWithResolvedValues.Count - i - 1);
                        code += shift + "/*local_func*/(("+ cast + ")" + ast.First() + ")(";
                        separator = ", ";
                        closeStatement = ", " + scopeName + ")";
                    }
                    else
                    {
                        LispFunctionWrapper function = ((LispVariant)first).FunctionValue;
                        if (function.Function == LispEnvironment.def_form)
                        {
                            var compileResult = Compile(astWithResolvedValues[i + 2], globalScope, string.Empty, false, scopeName);
// TODO --> ggf. nur einfache datentypen dekorieren !? siehe bei arg
                            code += shift + "/*def_form*/LispVariant " + astWithResolvedValues[i + 1] + " = new LispVariant( (object)" + compileResult.Item1 + ")";
                            functions += compileResult.Item2;
                            separator = "";
                            closeStatement = "";
                            i += 2;
                        }
                        else if (function.Function == LispEnvironment.defn_form)
                        {
                            // process normal function call
                            functions += shift + "/*defn*/private static LispVariant " + astWithResolvedValues[i + 1] + "(";
                            var argStrg = string.Empty;
                            foreach(var arg in (IEnumerable<object>)astWithResolvedValues[i + 2])
                            {
                                if (argStrg.Length > 0)
                                {
                                    argStrg += ", ";
                                }
                                argStrg += "LispVariant " + arg;
                            }
                            functions += argStrg + ", LispScope " + scopeName + ")\n";
                            functions += shift + "{\n";
                            functions += Compile(astWithResolvedValues[i + 3], globalScope, string.Empty, true, scopeName).Item1;
                            functions += shift + "}\n";
                            separator = "";
                            closeStatement = "";
                            i += 3;
                        }
                        else if (function.Function == LispEnvironment.quote_form)
                        {
                            // (quote (1 2 3)) --> new object[] { 1, 2, 3 }
                            code += "/*quote*/new object[] { ";
                            var aList = (IEnumerable<object>)astWithResolvedValues[i + 1];
                            var temp = string.Empty;
                            foreach (var item in aList)
                            {
                                if (temp.Length > 0)
                                {
                                    temp += ", ";
                                }
                                temp += "new LispVariant( ";
                                temp += Compile(item, globalScope, string.Empty, true, scopeName).Item1;
                                temp += " )";
                            }
                            code += temp + " }";
                            i++;
                        }
                        else if (function.Function == LispEnvironment.quasiquote_form)
                        {
                            // (quasiquote (1 2 3 ,a)) --> new object[] { 1, 2, 3, 42 }
// TODO korrekt implementieren !!!
                            code += "/*quasiquote*/new object[] { ";
                            var aList = (IEnumerable<object>)astWithResolvedValues[i + 1];
                            var temp = string.Empty;
                            foreach (var item in aList)
                            {
                                if (temp.Length > 0)
                                {
                                    temp += ", ";
                                }
                                temp += "new LispVariant( ";
                                temp += Compile(item, globalScope, string.Empty, true, scopeName).Item1;
                                temp += " )";
                            }
                            code += temp + " }";
                            i++;
                        }
                        else if (function.Function == LispEnvironment.do_form)
                        {
                            // process do_form
                            separator = "\n";
                            closeStatement = "";
                            int j;
                            string temp = string.Empty;
                            for (j = i + 1; j < astWithResolvedValues.Count; j++)
                            {
                                if (lastMustReturn && (j + 1 == astWithResolvedValues.Count))
                                {
                                    temp += "return ";
                                }
                                var compileResult = Compile(astWithResolvedValues[j], globalScope, shift, false, scopeName);
                                functions += compileResult.Item2;
                                temp += compileResult.Item1;
                                if (temp.Length > 0)
                                {
                                    temp += ";\n";
                                }
                            }
                            i = j;
                            code += temp;
                        }
                        else if (function.Function == LispEnvironment.if_form)
                        {
                            code += "if( ";
                            code += Compile(astWithResolvedValues[i + 1], globalScope, string.Empty, false, scopeName).Item1;
                            code += ".ToBool() )\n{\n";
                            code += Compile(astWithResolvedValues[i + 2], globalScope, "    ", false, scopeName).Item1;
                            code += ";\n}\n";
                            code += "else\n{\n";
                            code += Compile(astWithResolvedValues[i + 3], globalScope, "    ", false, scopeName).Item1;
                            code += ";\n}\n";
                            i += 3;
                        }
                        else if (function.Function == LispEnvironment.while_form)
                        {
                            code += "while( ";
                            code += Compile(astWithResolvedValues[i + 1], globalScope, string.Empty, false, scopeName).Item1;
                            code += ".ToBool() )\n{\n";
                            code += Compile(astWithResolvedValues[i + 2], globalScope, "    ", false, scopeName).Item1;
                            code += ";}\n";
                            i += 2;
                        }
                        else if (function.Function == LispEnvironment.setf_form)
                        {
                            code += Compile(astWithResolvedValues[i + 1], globalScope, string.Empty, false, scopeName).Item1;
                            code += " = ";
                            code += Compile(astWithResolvedValues[i + 2], globalScope, string.Empty, false, scopeName).Item1;
                            i += 2;
                        }
                        else if (function.Function == LispEnvironment.fn_form)
                        {
                            code += "/*fn*/new LispVariant( LispType.Function, new LispFunctionWrapper( (";
                            var argNames = (IEnumerable<object>)astWithResolvedValues[i + 1];
                            var temp = string.Empty;
                            foreach (var name in argNames)
                            {
                                if (temp.Length > 0)
                                {
                                    temp += ", ";                                    
                                }
                                temp += "LispVariant " + name;
                            }
                            code += temp;
                            code += ", LispScope _scope) => { ";
                            code += Compile(astWithResolvedValues[i + 2], globalScope, string.Empty, true, "_scope").Item1;
                            code += "; }, string.Empty, false ) )";
                            i += 2;
                        }
                        else
                        {
                            // process normal function call
                            if (lastMustReturn)
                            {
                                code += "return ";
                            }
                            code += shift + "/*func*/LispEnvironment." + ResolveFunction(ast.First(), globalScope) +
                                    "( new object[] { ";
                            separator = ", ";
                            closeStatement = " }, " + scopeName + ")";
                        }
                    }
                }
                else
                {
                    //if (lastMustReturn && (i + 1 == astWithResolvedValues.Count))
                    //{
                    //    code += "return /*pos2*/ ";
                    //}
                    if (args.Length > 0)
                    {
                        args += separator;
                    }
                    if (elem is IEnumerable<object>)
                    {
                        var compileResult = Compile((IEnumerable<object>)elem, globalScope, shift, false, scopeName);
                        args += compileResult.Item1;
                        functions += compileResult.Item2;
                    }
                    else
                    {
                        // decorate simple data types with LispVariant
                        var temp = (LispVariant) elem;
                        var elemType = temp.Type;
                        if (elemType == LispType.Bool ||
                            elemType == LispType.Int ||
                            elemType == LispType.Double ||
                            elemType == LispType.String)
                        {
                            args += "/*arg*/new LispVariant( (object)" + temp.ToStringCompiler() + " )";
                        }
                        else
                        {
                            args += "/*arg*/" + elem;
                        }
                    }
                }
            }
            code += args + closeStatement;

            return new Tuple<string, string>(code, functions);
        }

        private static string GetFunctionCast(int argCount)
        {
            var result = string.Empty;
            for (int i = 0; i < argCount; i++)
            {
                if (result.Length > 0)
                {
                    result += ", ";
                }
                result += "LispVariant";
            }
            result = "Func<" + result + ", LispScope, LispVariant>";
            return result;
        }

        private static string ResolveFunction(object astItem, LispScope globalScope)
        {
            LispFunctionWrapper fw = ((LispVariant)globalScope[astItem.ToString()]).FunctionValue;
            Func<object[], LispScope, LispVariant> func = fw.Function;
            MethodInfo method = func.Method;
            return method.Name;
        }

        private static string ShiftLines(string codeOutput, string shift)
        {
            string result = string.Empty;
            string[] lines = codeOutput.Split('\n');
            foreach (var line in lines)
            {
                if (line.Length > 0)
                {
                    result += shift + line + "\n";
                }
            }
            return result;
        }

        #endregion
    }
}
