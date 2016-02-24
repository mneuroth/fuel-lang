﻿using System;
using System.IO;
using System.Linq;
using System.Reflection;

namespace CsLisp
{
    /// <summary>
    /// Fast Usable Embeddable Lisp Interpreter and Compiler (FUEL).
    /// </summary>
    public class Fuel
    {
        public static void Main(string[] args)
        {
            MainExtended(args, Console.Out, Console.In);
        }

        public static void MainExtended(string[] args, TextWriter output, TextReader input)
        {
            if (args.Length == 0)
            {
                Usage(output);
                return;
            }

            string script = null;
            var loadFiles = true;
            var trace = false;
            var compile = false;
            var showCompileOutput = false;
            var measureTime = false;
            var lengthyErrorOutput = false;
            var result = new LispVariant();
            var startTickCount = Environment.TickCount;
            var debugger = TryGetDebugger();

            if (args.Contains("-m"))
            {
                measureTime = true;
            }
            if (args.Contains("-v"))
            {
                output.WriteLine(Lisp.Version);
                return;
            }
            if (args.Contains("-h"))
            {
                Usage(output);
                return;
            }
            if (args.Contains("-l"))
            {
                lengthyErrorOutput = true;
            }
            if (args.Contains("-t"))
            {
                trace = true;
            }
            if (args.Contains("-e"))
            {
                script = LispUtils.GetScriptFilesFromProgramArgs(args).FirstOrDefault();
                loadFiles = false;
            }

            // handle options for compiler
            if (args.Contains("-c"))
            {
                compile = true;
            }
            if (args.Contains("-s"))
            {
                showCompileOutput = true;
            }

            // handle options for debugger
            if (debugger != null)
            {                
                if (args.Contains("-i"))
                {
                    InteractiveLoopHeader(output);
                    debugger.InteractiveLoop(startedFromMain:true, tracing: trace);
                    loadFiles = false;
                }
                if (args.Contains("-d"))
                {
                    var fileName = LispUtils.GetScriptFilesFromProgramArgs(args).FirstOrDefault();
                    // process -e option if script is given via command line
                    if (script == null)
                    {
                        script = LispUtils.ReadFileOrEmptyString(fileName);
                    }
                    else
                    {
                        fileName = "command-line";
                    }

                    InteractiveLoopHeader(output);
                    result = debugger.DebuggerLoop(script, fileName, output, input, tracing: trace);
                    loadFiles = false;
                }                
            }

            if (loadFiles)
            {
                var scriptFiles = LispUtils.GetScriptFilesFromProgramArgs(args);

                foreach (var fileName in scriptFiles)
                {
                    script = LispUtils.ReadFileOrEmptyString(fileName);
                    ILispCompiler compiler = TryGetCompiler();
                    if (compile && compiler != null)
                    {
                        result = compiler.CompileToExe(script, fileName + ".exe");
                    }
                    else if (showCompileOutput && compiler != null)
                    {
                        result = compiler.CompileToCsCode(script);
                        output.WriteLine(result.StringValue);
                    }
                    else
                    {
                        result = Lisp.SaveEval(script, moduleName: fileName, verboseErrorOutput: lengthyErrorOutput, tracing: trace);
                    }
                }
            }
            else if (script != null)
            {
                // process -e option
                result = Lisp.SaveEval(script);
            }

            if (trace)
            {
                output.WriteLine("Result=" + result);
            }
            if (measureTime)
            {
                output.WriteLine("Execution time = {0} s", (Environment.TickCount - startTickCount) * 0.001);
            }
        }

        #region private methods

        private static void Usage(TextWriter output)
        {
            LispUtils.ShowAbout(output);
            output.WriteLine("usage:");
            output.WriteLine(">" + Lisp.ProgramName + " [options] [script_file_name]");
            output.WriteLine();
            output.WriteLine("options:");
            output.WriteLine("  -v          : show version");
            output.WriteLine("  -h          : show help");
            output.WriteLine("  -e \"script\" : execute given script");
            output.WriteLine("  -m          : measure execution time");
            output.WriteLine("  -t          : enable tracing");
            output.WriteLine("  -l          : lengthy error output");
            if (TryGetDebugger() != null)
            {
                output.WriteLine("  -i          : interactive shell");
                output.WriteLine("  -d          : start debugger");
            }
            else
            {
                output.WriteLine();
                output.WriteLine("Info: no debugger support installed !");
            }
            if (TryGetCompiler() != null)
            {
                output.WriteLine("  -c          : compile program");
                output.WriteLine("  -s          : show C# compiler output");
            }
            else
            {
                output.WriteLine();
                output.WriteLine("Info: no compiler support installed !");                
            }
            output.WriteLine();
        }

        private static void InteractiveLoopHeader(TextWriter output)
        {
            LispUtils.ShowVersion(output);
            output.WriteLine("Type \"help\" for informations.");
            output.WriteLine();
        }

        private static ILispCompiler TryGetCompiler()
        {
            return TryGetClassFromDll<ILispCompiler>("CsLispCompiler.dll", "CsLisp.LispCompiler");
        }

        private static ILispDebugger TryGetDebugger()
        {
            return TryGetClassFromDll<ILispDebugger>("CsLispDebugger.dll", "CsLisp.LispDebugger");
        }
        
        private static T TryGetClassFromDll<T>(string dllName, string className)
        {
            var applicationPath = GetApplicationPath();
            var dllPath = Path.Combine(applicationPath, dllName);

            if (File.Exists(dllPath))
            {
                var dll = Assembly.LoadFile(dllPath);
                if (dll != null)
                {
                    var classType = dll.GetType(className);
                    var instance = Activator.CreateInstance(classType);
                    return (T)instance;
                }
            }

            return default(T);
        }

        private static string GetApplicationPath()
        {
            var application = Assembly.GetExecutingAssembly().Location;
            var applicationPath = Path.GetDirectoryName(application);
            return applicationPath;
        }

        #endregion
    }


    // TODO:
    // (- debuggen: run funktioniert nicht in errorinmodule.fuel
    // ((- debuggen: anzeige module und line no in stack
    // (- debuggen: anzeige des korrekten codes, falls module geladen ist
    // ((- debuggen: set breakpoints in andren modulen realisieren
    // (- debuggen: up/down sollte auch den --> Zeiger anpassen
    // (- ist LispScope.Finished und LispScope.SourceCode noch notwendig? 
    // - Makro Behandlung aufraeumen
    // - Quellcode aufraeumen
    // - debuggen: set next statement realisieren?
    // - bug: step out funktioniert anscheinend bei modulen nicht ganz korrekt
    // (- ggf. module als eigenen Scope implementieren --> ###modules###
    // - debuggen: show loaded module names 
    // - debuggen: funcs befehl um module erweitern und anzeige von funktionen in modulen
    // - stdlib um list<object> erweitern, damit man immer mit listen arbeiten kann
    // (- debugger: v (step over) funktioniert nicht so wie erwartet --> haengt bei quote
    // - Behandlung von Variablen im Modulen korrekt realisieren --> sind global nicht sichtbar, nur im Modul selbst --> im debugger anzeigen
    // - unit tests erweitern um neue Features: set breakpoints in modulen, debuggen von modulen, line no anzeige in stack, source code anzeige aktualisierung in up/down
    // - setf macro implementieren...
    // - out funktioniert aus import module nicht korrekt...-?
    // ((- option -e funktioniert nicht mit -d korrekt
    // ((- start unhd end position bei aktuellem step ausgeben --> besserer Support für debuggen
    // - License und Copyright header in quellcode dateien einbauen
    // - Tuple<int, int, int> in einen typsicheren struct verwandeln? --> gibt es auch in Interface --> unschoen !
    // ((- Start/StopPos in LispException setzen, fuer bessere fehlermeldung
    // ((- GetPosInfoString() in Exceptions ggf. nicht mehr notwendig, da infos an Exception gesetzt wird
    // - tracen auch mit Start/StopPos fuer besseren debugger support  --> highlighte aktuelles statement: (+ 1 2 3) 


    /*
mindia: pfade beim film erzeugen mit "" versehen, wegen leerzeichen im pfad !
mindia: probleme mit msc++ in minbase.h mit mem_fun()
mindia: filelist --> mcisendstring_test.txt; mindia_org.pro; *.bat; *.lst
mindia: readme.txt --> windows konform CR/LF!
mindia: demo diashow mit auslieferen und automatisch anzeigen
mindia: einfache Anleitung erstellen (HTML) --> Dias sortieren, Sound hinzufügen, Zeiten anpassen, Ausschnitt anpassen, Dyn-Texte hinzufügen, vorführen/testen, als Film exportieren
mindia: *.gz enthält noch ._* Dateien --> make_src_tar.sh anpassen, siehe visiscript batches
mindia: TODO.txt nicht in source code distribution aufnehmen --> aus file.lst entfernen
visiscript: win32-msvc2013:LIBS += ../../build-visiscript-Desktop_Qt_5_4_1_MSVC2013_32bit-Debug/QScintilla-gpl-2.9/Qt4Qt5/debug/qscintilla2.lib <-- lib aus namen entfernen
visiscript: Execution time Ausgabe unschoen:
Measure execution time for interpreter: QScript
QScript return=ReferenceError: Can't find variable: graphicslib
Fehler: unbehandelte Ausnahme in Zeile 71 ReferenceError: Can't find variable: graphicslibAusführungszeit: 150 ms
visiscript: TODO.txt nicht in source code distribution aufnehmen --> aus file.lst entfernen
visiscript: visiscriptextensions/files/android aus distribution entfernen --> *.lst
    visiscriptextensions/files/windows aus distribution entfernen --> *.lst
visiscript: main.cpp so anpassen, dass auch fuer Windows die extensions.dll (out of the box) gefunden wird und kein Pfad-Patchen notwendig ist
#if defined(Q_OS_WIN)
    sExtension = QCoreApplication::applicationDirPath()+QDir::separator()+"extensions/extensions%0.dll";

https://todoist.com/

http://www.mobiflip.de/asus-zenbook-ux303lb-testbericht/
         
     */

    //private static bool IsMacroDefine(LispVariant funcName)
    //{
    //    if (funcName != null)
    //    {
    //        var name = funcName.ToString();
    //        return name == DEFINEMACRO;
    //    }
    //    return false;
    //}

    //public LispScope CurrentTop
    //{
    //    get
    //    {
    //        var current = GlobalScope;
    //        while (current.Next != null)
    //        {
    //            current = current.Next;
    //        }
    //        return current;
    //    }            
    //}

    // TODO --> flaches expandieren ?
    //var lstResult = expandResult as IEnumerable<object>;
    //if (lstResult == null)
    //{
    //    var variant = expandResult as LispVariant;
    //    if (variant != null && variant.IsList)
    //    {
    //        lstResult = variant.ListValue;
    //    }
    //}
    //if (lstResult != null)
    //{
    //    foreach (var item in lstResult)
    //    {
    //        expandedAst.Add(item);
    //    }
    //}
    //else

    // csc /debug+ simplebench.cs ..\..\CsLispDll\LispVariant.cs ..\..\CsLispDll\LispToken.cs ..\..\CsLispDll\LispEnvironment.cs ..\..\CsLispDll\LispException.cs ..\..\CsLispDll\LispInterpreter.cs ..\..\CsLispDll\LispDebuggerInterface.cs
    // csc /debug+ simplebench.cs /reference:cslispinterpreter.dll

    // TODO:
    // source code sauberer und kompakter machen (falls moeglich)
    //    --> LispCompiler
    // License header für jede datei
    // unit test: abdeckung verbessern
    // unit test: compiler modus testen
    // unit test: gdef, gdefn, nop, sym

    // module implementieren, z. B. fuer foreach, etc. runtime lib...

    // closure fuer compiler modus implementieren --> siehe ClosoureChain

    // (nop)  functor fuer c# support --> sollte immer letzte anweisung sein, falls kein return benoetigt wird

    // echte macro expansion realisieren --> code erzeugen (fuer Compiler notwendig)

    // compiler modus einbauen --> csc.exe aufrufen und echtes exe erzeugen...
    //http://stackoverflow.com/questions/11501697/programmatically-call-c-sharp-compiler-to-compile-c-sharp-code

    // Name? --> LispInfo ggf. umbenennen?
    // environment: file io implementieren --> makros fuer File class verwenden
    // environment: math funktionen implementieren --> makros verwenden
    // environment: hoehere Datentypen wie array/dyn-liste und map implementieren

    // native calls verbessern --> type mapping implementieren --> ggf. reflection optimieren --> lambda zurueckgeben?

    // refactoring zum aufloesen von funktionen verwenden?
    // warum ist LispScope bei Environment funktionen notwendig? --> fuer meta befehle wie z. b. vars, break, trace, ...
    // fuer compiler: braucht man wirklich LispToken.cs module? --> Referenzen von LispVariant.Token pruefen?
    // fuer compiler: manche environment funktionen benoetigen den Interpreter/Eval, dies gibt doch probleme im Compile modus?

    /*
    https://www.quora.com/What-are-the-special-forms-for-Scheme
    http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Special-Forms.html#Special-Forms

        
// --> http://howtowriteaprogram.blogspot.de/2010/11/lisp-interpreter-in-90-lines-of-c.html
// --> http://stackoverflow.com/questions/6169706/lisp-interpreter-in-a-c-program
// --> http://norvig.com/lispy.html
// --> http://stackoverflow.com/questions/70004/using-lisp-in-c-sharp
    (define-macro forx (lambda (e in container block) (display `(,e in ,container ,block))))

    (define-macro forx (lambda (e in container block) (begin (set! e (car container)) (display `(,e in ,container ,block)))))

    (define-macro forx (lambda (ee in container block) (begin (set! ee (car container)) block)))

    (forx e in '(1 2 3) (display e))

    geht so was mit lisp macros?
    (forx e in [1 2 3] : print e*e)
    (forx e in (1 2 3) : print e*e)

    e  --> symbol (formaler parameter)
    in --> keyword (kann ignoriert werden)
    */

    // OK:
    // (help)
    // (break)
    // REPL loop
    // unit tests einen ordern hoeher verschieben
    // Kommentare parsen --> ;
    // compare logische operatoren: ==, <, >, <=, >=, and, or, not
    // reduce
    // (def 6 9) ist erlaubt !!!
    // runtime methoden robuster machen --> anzahl argumente pruefen und typ pruefen
    // funcs --> builtin functions
    // (vars)/vars anzeige verbessern
    // (stack)/where implementieren        
    // debugger commands: stepout, step over, step into, callstack up/down
    // breakpoints angeben
    // lamba mit closures testen
    // harte referenzen entfernen: compiler und ggf. debugger

    /*
                //LispVariant variant = args[0] as LispVariant;
                //if (variant != null)
                //{
                //    if (variant.IsSymbol)
                //    {
                //        macroFcn = (LispVariant)LispInterpreter.ResolveArgsInScopes(scope, new object[] { variant.ToString() }, false)[0];
                //    }
                //}
                //else
                //{
                //    macroFcn = LispInterpreter.EvalAst(args[0], scope);                
                //}

 
                string program      = "(do (test 3 4 hallo  \"string \\\" \\\\ blub\" (+ 1.23 asdf -345.34)))";
                string programX     = "(do (print \"hallo\")\n (print \"world\"))";
                //string programY   = "(do (def a 42) (print a) (print (quote (1 2 3))))";
                //string programY   = "(do (define-macro blub (lambda (x y) (print x y))) (print (quote (1 2 3))) (blub 3 4))";
                //string programY   = "(apply (lambda (x) (print \"hello\" x)) 55)";
                //string programY   = "(print `(1 2 3))";
                //string programY   = "(print '(1 2 3))";
                //string programY   = "(do (def a 42) (print `(1 2 3 ,@a)))";
                //string programY   = "(do (def a '(42 99)) (print `(1 2 3 ,@a)))";
                //string programY   = "(do (def a '(42 99)) (print (len `(1 2 3 ,@a))))";
                //string programY   = "(do (def a '(42 99 102 \"hello\")) (def b 55) (print (type a)) (print (nth 1 `(1 2 3 ,@a))))";
                //string programY     = "(do (def obj 0) (setf obj (call \"CsLisp.DummyNative\" \"dummy\")) (print obj (type obj)) (call obj \"Test\"))";
                //string programY = "(do (def g_prn \"START:\") (defn prn (x) (setf g_prn (add g_prn x))) (prn (print \"hallo\")) (print g_prn))";
                //string programY = "(do (def g_prn \"START:\") (defn prn (x) (setf g_prn (add g_prn x))) (prn \"34\") (print g_prn))";
                //string programY = "(do (trace #t) (print \"hello world\") (gettrace))";
                string programY = "(do (def g_prn \"START:\") (def a 42) (defn prn (x) (do (setf g_prn (add g_prn x)) (break))) (prn \"34\") (print g_prn) (break))";
                //string programY = "(help)";

                LispVariant val = Lisp.Eval(programY);

                var resultx = LispInterpreter.Tokenize(programY);
                foreach (var elem in resultx)
                {
                    Console.WriteLine(string.Format("{0} {1} {2}",elem, elem.Type, elem.Value));
                }

    //            Console.WriteLine("Result=" + res);
                Console.WriteLine("done.");

    // (if true (print "then") (print "else")) 
    // read token (space/whitespace and ( ) separated)
    // --> ( if true ( print "then" ) ( print "else" ) )
    // ( --> new List<object>
    // ) --> close current list
    // other --> add to current list

    // Arrays ? (add [1, 2 , 3])
    // (add '(1, 2, 3))

    // state machine:
    // WS* [QUOTE | LIST_START | SYMBOL] WS* 
    //
    // QUOTE        = '
    // LIST_START   = (
    // LIST_STOP    = )
    // LIST         = LIST_START [LIST | ITEM]* LIST_STOP
    // ITEM         = STRING | INT | DOUBLE | SYMBOL
    // STRING       = "..."
    // INT
    // DOUBLE
    // SYMBOL       = alles not WS ' ( )

     http://stackoverflow.com/questions/517113/lisp-grammar-in-yacc
     %%
    sexpr: atom                 {printf("matched sexpr\n");}
        | list
        ;
    list: '(' members ')'       {printf("matched list\n");}
        | '('')'                {printf("matched empty list\n");}
        ;
    members: sexpr              {printf("members 1\n");}
        | sexpr members         {printf("members 2\n");}
        ;
    atom: ID                    {printf("ID\n");}
        | NUM                   {printf("NUM\n");}
        | STR                   {printf("STR\n");}
        ;
     %%



    program: (sexpr)*;

    sexpr: list
        |  atom            {Console.WriteLine("matched sexpr");}
        ;

    list:     
       '('')'              {Console.WriteLine("matched empty list");}
       | '(' members ')'   {Console.WriteLine("matched list");}

        ;

    members: (sexpr)+      {Console.WriteLine("members 1");};

    atom: Id               {Console.WriteLine("ID");}
        | Num              {Console.WriteLine("NUM");}
        ;


    Num: ( '0' .. '9')+;
    Id: ('a' .. 'z' | 'A' .. 'Z')+;
    Whitespace : ( ' ' | '\r' '\n' | '\n' | '\t' ) {Skip();};             

     */
}
