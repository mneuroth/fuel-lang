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
