﻿/*
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
            int offset;
            code = LispUtils.DecorateWithBlock(code, out offset);
            var ast = LispParser.Parse(code, offset, globalScope);
            //var expandedAst = ExpandMacros(ast, globalScope);
            var compileResult = Compile(ast, globalScope, "        ", "__return__", "scope");
            var csCode = "namespace CsLisp\n{\nusing System;\nclass CompiledLisp\n{\n    // functions:\n" +
                        ShiftLines(compileResult.Item2, "    ") +
                        "\n    public static LispVariant LispMain(string[] args)\n    {\n        var scope = new LispScope();\n        LispVariant __return__;\n\n" +
                        ShiftLines(compileResult.Item1, "        ") +
                         "        return __return__;\n    }\n\n    public static void Main(string[] args)\n    {\n        var startTickCount = Environment.TickCount;\n        LispMain(args);\n        Console.WriteLine(string.Format(\"Execution time = {0} s\", (Environment.TickCount - startTickCount)*0.001));\n    }\n}\n}";
            return new LispVariant(csCode);
        }

        /// <summary>
        /// Compile C# code into an assembly.
        /// see: https://support.microsoft.com/de-de/kb/304655
        /// </summary>
        /// <param name="csCode">The cs code.</param>
        /// <param name="outputFileName">Name of the output file.</param>
        /// <param name="debug">if set to <c>true</c> use debug option for C# compiler.</param>
        /// <returns>Compiler result</returns>
        public /*static*/ CompilerResults CompileCsCodeToAssembly(string csCode, string outputFileName, bool debug = false)
        {
            var provOptions = new Dictionary<string, string>();
            provOptions.Add("CompilerVersion", "v3.5");

            var codeProvider = new CSharpCodeProvider(provOptions);
            var parameters = new CompilerParameters();

            parameters.GenerateExecutable = true;
            parameters.OutputAssembly = outputFileName;
            parameters.CompilerOptions = "/reference:fuelinterpreter.dll /reference:System.Core.dll";
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
        /// <returns>True if no error has occured</returns>
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

        private static Tuple<string, string> Compile(object ast, LispScope globalScope, string shift, string saveReturn, string scopeName)
        {
            if(ast is IEnumerable<object>)
            {
                return Compile((IEnumerable<object>)ast, globalScope, string.Empty, saveReturn, scopeName);
            }
            var value = ast as LispVariant;
            var temp = value != null ? value.ToStringCompiler() : ast.ToString();
            return new Tuple<string, string>(temp, string.Empty);
        }

        private static Tuple<string, string> Compile(IEnumerable<object> ast, LispScope globalScope, string shift, string saveReturn, string scopeName)
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
                            var compileResult = Compile(astWithResolvedValues[i + 2], globalScope, string.Empty, null, scopeName);
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
                            functions += shift + "LispVariant __return__;\n";
                            functions += Compile(astWithResolvedValues[i + 3], globalScope, string.Empty, "__return__", scopeName).Item1;
                            functions += shift + ";\n";
                            functions += shift + "\nreturn __return__;\n";
                            functions += shift + "}\n";
                            separator = "";
                            closeStatement = "";
                            i += 3;
                        }
                        else if (function.Function == LispEnvironment.quote_form)
                        {
                            // (quote (1 2 3)) --> new object[] { 1, 2, 3 }
                            // (quote 'something) --> new LispVariant(LispType.Symbol, "something")
                            code += "/*quote*/new object[] { ";
                            var temp = string.Empty;
                            if (astWithResolvedValues[i + 1] is IEnumerable<object>)
                            {
                                var aList = (IEnumerable<object>)astWithResolvedValues[i + 1];
                                foreach (var item in aList)
                                {
                                    if (temp.Length > 0)
                                    {
                                        temp += ", ";
                                    }
                                    temp += "new LispVariant( ";
                                    temp += Compile(item, globalScope, string.Empty, null, scopeName).Item1;
                                    temp += " )";
                                }
                            }
                            else
                            {
                                LispVariant variant = (LispVariant)astWithResolvedValues[i + 1];
                                temp += "new LispVariant( LispType.Symbol, \"";
                                temp += variant.ToString();
                                temp += "\" )";
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
                                temp += Compile(item, globalScope, string.Empty, null, scopeName).Item1;
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
                            string temp = "{ /*do*/\n";
                            for (j = i + 1; j < astWithResolvedValues.Count; j++)
                            {
                                var compileResult = Compile(astWithResolvedValues[j], globalScope, shift, saveReturn, scopeName);
                                functions += compileResult.Item2;
                                temp += compileResult.Item1;
                                if (temp.Length > 0)
                                {
                                    temp += ";\n";
                                }
                            }
                            i = j;
                            code += temp + "/*do*/ }\n";
                        }
                        else if (function.Function == LispEnvironment.if_form)
                        {
                            code += "if( ";
                            code += Compile(astWithResolvedValues[i + 1], globalScope, string.Empty, null, scopeName).Item1;
                            code += ".ToBool() )\n{\n";
                            code += Compile(astWithResolvedValues[i + 2], globalScope, "    ", null, scopeName).Item1;
                            code += ";\n}\n";
                            if (astWithResolvedValues.Count > i + 3)
                            {
                                code += "else\n{\n";
                                code += Compile(astWithResolvedValues[i + 3], globalScope, "    ", null, scopeName).Item1;
                                code += ";\n}\n";
                                i += 3;
                            }
                            else
                            {
                                i += 2;
                            }
                        }
                        else if (function.Function == LispEnvironment.while_form)
                        {
                            code += "while( ";
                            code += Compile(astWithResolvedValues[i + 1], globalScope, string.Empty, null, scopeName).Item1;
                            code += ".ToBool() )\n{\n";
                            code += Compile(astWithResolvedValues[i + 2], globalScope, "    ", null, scopeName).Item1;
                            code += ";}\n";
                            i += 2;
                        }
                        else if (function.Function == LispEnvironment.setf_form)
                        {
                            code += Compile(astWithResolvedValues[i + 1], globalScope, string.Empty, null, scopeName).Item1;
                            code += " = new LispVariant(";
                            code += Compile(astWithResolvedValues[i + 2], globalScope, string.Empty, null, scopeName).Item1;
                            code += ")";
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
                            code += ", LispScope _scope) => { LispVariant __ret__; ";
                            code += Compile(astWithResolvedValues[i + 2], globalScope, string.Empty, "__ret__", "_scope").Item1;
                            code += "; return __ret__; }, /*signature*/string.Empty, /*documentation*/string.Empty, false ) )";
                            i += 2;
                        }
                        else
                        {
                            // process normal function call
                            code += shift;
                            if (!string.IsNullOrEmpty(saveReturn))
                            {
                                code += saveReturn + " = ";
                            }
                            code += "/*func*/LispEnvironment." + ResolveFunction(ast.First(), globalScope) +
                                    "( new object[] { ";
                            separator = ", ";
                            closeStatement = " }, " + scopeName + ")";
                        }
                    }
                }
                else
                {
                    if (args.Length > 0)
                    {
                        args += separator;
                    }
                    if (elem is IEnumerable<object>)
                    {
                        var compileResult = Compile((IEnumerable<object>)elem, globalScope, shift, saveReturn, scopeName);
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
