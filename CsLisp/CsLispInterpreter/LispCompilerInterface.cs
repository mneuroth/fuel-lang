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

using System.CodeDom.Compiler;

namespace CsLisp
{
    /// <summary>
    /// The interface to the compiler module
    /// </summary>
    public interface ILispCompiler
    {
        /// <summary>
        /// Compiles the specified lisp code into C# code.
        /// </summary>
        /// <param name="code">The lisp code.</param>
        /// <returns>The C# code packed into a LispVariant</returns>
        LispVariant CompileToCsCode(string code);

        /// <summary>
        /// Compile C# code into an assembly.
        /// see: https://support.microsoft.com/de-de/kb/304655
        /// </summary>
        /// <param name="csCode">The cs code.</param>
        /// <param name="outputFileName">Name of the output file.</param>
        /// <param name="debug">if set to <c>true</c> use debug option for C# compiler.</param>
        /// <returns>Compiler result</returns>
        CompilerResults CompileCsCodeToAssembly(string csCode, string outputFileName, bool debug = false);

        /// <summary>
        /// Compiles the given lisp code into an executable.
        /// </summary>
        /// <param name="lispCode">The lisp code.</param>
        /// <param name="exeFileName">Name of the executable file.</param>
        /// <returns>True if no error has occured</returns>
        LispVariant CompileToExe(string lispCode, string exeFileName);
    }
}
