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

using System;
using System.IO;
using System.Linq;

namespace CsLisp
{
    /// <summary>
    /// Tuple class, because .NET 3.5 does not have a Tuple class.
    /// </summary>
    /// <typeparam name="T1">The type of the type parameter 1.</typeparam>
    /// <typeparam name="T2">The type of the type parameter 2.</typeparam>
    public class Tuple<T1, T2>
    {
        public Tuple(T1 v1, T2 v2)
        {
            Item1 = v1;
            Item2 = v2;
        }

        public T1 Item1 { get; set; }

        public T2 Item2 { get; set; }
    }

    /// <summary>
    /// Tuple class, because .NET 3.5 does not have a Tuple class.
    /// </summary>
    /// <typeparam name="T1">The type of the type parameter 1.</typeparam>
    /// <typeparam name="T2">The type of the type parameter 2.</typeparam>
    /// <typeparam name="T3">The type of the type parameter 3.</typeparam>
    public class Tuple<T1, T2, T3>
    {
        public Tuple(T1 v1, T2 v2, T3 v3)
        {
            Item1 = v1;
            Item2 = v2;
            Item3 = v3;
        }

        public T1 Item1 { get; set; }

        public T2 Item2 { get; set; }

        public T3 Item3 { get; set; }
    }

    /// <summary>
    /// Helper class for FUEL lisp interpreter.
    /// </summary>
    public static class LispUtils
    {
        #region constants

        /// <summary>
        /// Constant to transport line number info in exception
        /// </summary>
        public const string LineNo = "LineNo";

        /// <summary>
        /// Constant to transport the start position info of the current statement in exception
        /// </summary>
        public const string StartPos = "StartPos";

        /// <summary>
        /// Constant to transport the stop position info of the current statement in exception
        /// </summary>
        public const string StopPos = "StopPos";

        /// <summary>
        /// Constant to transport module name and path info in exception
        /// </summary>
        public const string ModuleName = "ModuleName";

        /// <summary>
        /// Constant to transport a stack info in exception
        /// </summary>
        public const string StackInfo = "StackInfo";

        /// <summary>
        /// Constant for the command line module
        /// </summary>
        public const string CommandLineModule = "command-line";

        #endregion

		#region static properties

        /// <summary>
        /// Gets or sets the fuel library path.
        /// </summary>
        /// <value>
        /// The library path.
        /// </value>
		public static string LibraryPath { get; set; }

        /// <summary>
        /// Gets a value indicating whether the compile time macros are supported.
        /// </summary>
        /// <value>
        /// <c>true</c> if compile time macros are enabled; otherwise, <c>false</c>.
        /// </value>
        public static bool IsCompileTimeMacroEnabled { get; private set; }

		#endregion

		#region static constructor

		static LispUtils()
		{
			LibraryPath = string.Empty;
#if ENABLE_COMPILE_TIME_MACROS
		    IsCompileTimeMacroEnabled = true;
#else
		    IsCompileTimeMacroEnabled = false;
#endif
		}

		#endregion

        #region extension methods

        /// <summary>
        /// Add infos about given token to exception data.
        /// </summary>
        /// <param name="ex">The exception.</param>
        /// <param name="token">The token.</param>
        public static void AddTokenInfos(this Exception ex, LispToken token)
        {
            ex.Data[LineNo] = token != null ? token.LineNo : -1;
            ex.Data[StartPos] = token != null ? token.StartPos : -1;
            ex.Data[StopPos] = token != null ? token.StopPos : -1;
        }

        /// <summary>
        /// Adds the module name and stack infos.
        /// </summary>
        /// <param name="ex">The exception.</param>
        /// <param name="moduleName">Name of the module.</param>
        /// <param name="stackInfo">The stack information.</param>
        public static void AddModuleNameAndStackInfos(this Exception ex, string moduleName, string stackInfo)
        {
            ex.Data[ModuleName] = moduleName;
            ex.Data[StackInfo] = stackInfo;
        }

        /// <summary>
        /// Gets the additional arguments from the scope.
        /// </summary>
        /// <param name="scope">The scope.</param>
        /// <returns>The result.</returns>
        public static object[] GetAdditionalArgs(this LispScope scope)
        {
            if (scope.ContainsKey(LispEnvironment.ArgsMeta))
            {
                LispVariant variant = scope[LispEnvironment.ArgsMeta] as LispVariant;
                if (variant != null)
                {
                    return variant.ListValue.ToArray();
                }
            }
            return new object[0];
        }
        
        #endregion

        #region helper methods

        /// <summary>
        /// Show the version of this FUEL interpreter.
        /// </summary>
        /// <param name="output">The output stream.</param>
        public static void ShowVersion(TextWriter output)
        {
            output.WriteLine();
            output.WriteLine(Lisp.Name + " " + Lisp.Version + " (for " + Lisp.Platform + ") from " + Lisp.Date + ", " + Lisp.Copyright);
            output.WriteLine();
        }

        /// <summary>
        /// Show informations about this FUEL interperter.
        /// </summary>
        /// <param name="output">The output stream.</param>
        public static void ShowAbout(TextWriter output)
        {
            ShowVersion(output);
            output.WriteLine(Lisp.Info);
            output.WriteLine();
        }

        /// <summary>
        /// Gets the script files from program arguments.
        /// Returns all elements of the given args array which does not start with a "-".
        /// </summary>
        /// <param name="args">The arguments.</param>
        /// <returns>Array of string names</returns>
        public static string[] GetScriptFilesFromProgramArgs(string[] args)
        {
            return args.Where(s => !s.StartsWith("-")).ToArray();
        }

        /// <summary>
        /// Reads a file or returns an empty string.
        /// </summary>
        /// <param name="fileName">Name of the file.</param>
        /// <returns>Content of the file as string</returns>
        public static string ReadFileOrEmptyString(string fileName)
        {
            bool exists;
            try
            {
                exists = File.Exists(fileName);
            }
            catch (ArgumentException)
            {
                exists = false;
            }
            return  exists ? File.ReadAllText(fileName) : string.Empty;
        }

        /// <summary>
        /// Decorates the code with a block.
        /// </summary>
        /// <param name="code">The code.</param>
        /// <param name="offset">The position offset created by the decorated code.</param>
        /// <returns>Decorated code.</returns>
        public static string DecorateWithBlock(string code, out int offset)
        {
            const string block = "(do ";
            offset = block.Length;
            return block + code + "\n)";
        }

        #endregion
    }

    /// <summary>
    /// Helper class, needed for unit tests
    /// </summary>
    public class DummyNative
    {
        public string MyValue { get; set; }

        public DummyNative()
            : this(string.Empty)
        {            
        }

        public DummyNative(string value)
        {
            MyValue = value;            
        }

        public string GetMessage(string text)
        {
            return text + " " + MyValue;
        }

        public int Test()
        {
            Console.WriteLine(">>> dummy native call --> Test()");
            return 42;
        }
    }
}
