using System;
using System.IO;
using System.Linq;

namespace CsLisp
{
    /// <summary>
    /// Helper class for FUEL lisp interpreter.
    /// </summary>
    public class LispUtils
    {
        #region constants

        /// <summary>
        /// Constant to transport line number info in exception
        /// </summary>
        public const string LineNo = "LineNo";

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
    }

    /// <summary>
    /// Helper class, needed for unit tests
    /// </summary>
    public class DummyNative
    {
        public string MyValue { get; set; }

        public int Test()
        {
            Console.WriteLine(">>> dummy native call --> Test()");
            return 42;
        }
    }
}
