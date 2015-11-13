using System.CodeDom.Compiler;

namespace CsLisp
{
    public interface ILispCompiler
    {
        LispVariant CompileToCsCode(string code);
        LispVariant CompileToExe(string lispCode, string exeFileName);
        CompilerResults CompileCsCodeToAssembly(string csCode, string outputFileName, bool debug = false);
    }
}
