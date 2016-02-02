using System;
using LispUnitTests;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using CsLisp;

namespace CsLispUnitTests
{
    [TestClass]
    public class LispInterpreterTest
    {
        [TestMethod]
        public void Test_Comments()
        {
            LispVariant result = Lisp.Eval("(do (print \"hello\") ; a comment\n(print; separate lists with comments\n\"world\"));comment in last line");
            Assert.AreEqual("world", result.ToString());
        }

        [TestMethod]
        public void Test_DoAndPrint()
        {
            LispVariant result = Lisp.Eval("(do (print \"hello\")\n (print \"world\"))");
            Assert.AreEqual("world", result.ToString());
        }

        [TestMethod]
        public void Test_PrintTrace()
        {
            LispVariant result = Lisp.Eval("(do (trace #t) (print \"hello world\") (print (+ 9 8)) (gettrace))");
            Assert.AreEqual("hello world17", result.ToString());
        }

        [TestMethod]
        public void Test_If1()
        {
            LispVariant result = Lisp.Eval("(if #t (+ 1 2) (- 3 5))");
            Assert.AreEqual(3, result.ToInt());
        }

        [TestMethod]
        public void Test_If2()
        {
            LispVariant result = Lisp.Eval("(if #f (* 1 0) (/ 6 3))");
            Assert.AreEqual(2, result.ToInt());
        }

        [TestMethod]
        public void Test_If3()
        {
            LispVariant result = Lisp.Eval("(if true 1 0)");
            Assert.AreEqual(1, result.ToInt());
        }

        [TestMethod]
        public void Test_If4()
        {
            LispVariant result = Lisp.Eval("(if false 1 0)");
            Assert.AreEqual(0, result.ToInt());
        }

        [TestMethod]
        public void Test_Setf1()
        {
            LispVariant result = Lisp.Eval("(do (def a 1) (def b 1) (setf (first (list 'a 'b 'c)) 9))");
            Assert.AreEqual(9, result.ToInt());
        }

        [TestMethod]
        public void Test_Def1()
        {
            LispVariant result = Lisp.Eval("(do (def a 1) (def b 1) (def (nth 2 (list 'a 'b 'c)) 9) (+ c 2))");
            Assert.AreEqual(11, result.ToInt());
        }

        [TestMethod]
        public void Test_DefWithNil()
        {
            LispVariant result = Lisp.Eval("(do (def a nil) (print a))");
            Assert.AreEqual("NIL", result.ToString());
        }

        [TestMethod]
        public void Test_Eval1()
        {
            LispVariant result = Lisp.Eval("(eval (list 'def 'x 43))");
            Assert.AreEqual(43, result.ToInt());
        }

        [TestMethod]
        public void Test_Eval2()
        {
            LispVariant result = Lisp.Eval("(eval '(def x 456))");
            Assert.AreEqual(456, result.ToInt());
        }

        [TestMethod]
        public void Test_While1()
        {
            LispVariant result = Lisp.Eval("(do (def a 1) (def b 1) (while (< a 10) (do (setf a (+ a 1)) (setf b (+ b 1)))))");
            Assert.AreEqual(10, result.ToInt());
        }

        [TestMethod]
        public void Test_Defn1()
        {
            LispVariant result = Lisp.Eval("(do (def g_prn \"START:\") (defn prn (x) (setf g_prn (add g_prn x))) (prn \"34\") (print g_prn))");
            Assert.AreEqual("START:34", result.ToString());
        }

        [TestMethod]
        public void Test_AddString()
        {
            LispVariant result = Lisp.Eval("(+ \"abc\" \"def() ; blub\" \"xxx\")");
            Assert.AreEqual("abcdef() ; blubxxx", result.ToString());
        }

        [TestMethod]
        public void Test_AddLists()
        {
            LispVariant result = Lisp.Eval("(+ '(1 2 3) '(\"hello\" 2.3 42))");
            Assert.AreEqual("(1 2 3 hello 2.3 42)", result.ToString());
        }

        [TestMethod]
        public void Test_ListFirst()
        {
            LispVariant result = Lisp.Eval("(first '(1 2 3))");
            Assert.AreEqual(1, result.ToInt());
        }

        [TestMethod]
        public void Test_ListRest()
        {
            LispVariant result = Lisp.Eval("(rest '(1 2 3))");
            Assert.AreEqual("(2 3)", result.ToString());
        }

        [TestMethod]
        public void Test_ListLength()
        {
            LispVariant result = Lisp.Eval("(len '(1 2 3))");
            Assert.AreEqual(3, result.ToInt());
        }

        [TestMethod]
        public void Test_ListAppend()
        {
            LispVariant result = Lisp.Eval("(append (list 4 54 3) (list 7 9))");
            Assert.AreEqual("(4 54 3 7 9)", result.ToString());
        }

        [TestMethod]
        public void Test_LogicalOperators()
        {
            LispVariant result = Lisp.Eval("(list (and #t #f) (and #t #t) (or #t #f) (or #f #f) (or #t #f #t))");
            Assert.AreEqual("(#f #t #t #f #t)", result.ToString());
        }

        [TestMethod]
        public void Test_CompareOperators1()
        {
            LispVariant result = Lisp.Eval("(list (= 1 2) (= 4 4) (== \"blub\" \"blub\") (== #t #f) (equal 3 4))");
            Assert.AreEqual("(#f #t #t #f #f)", result.ToString());
        }

        [TestMethod]
        public void Test_CompareOperators2()
        {
            LispVariant result = Lisp.Eval("(list (< 1 2) (< 4 1) (> 5 2) (> 1 3) (> 4.0 4.0))");
            Assert.AreEqual("(#t #f #t #f #f)", result.ToString());
        }

        [TestMethod]
        public void Test_CompareOperators3()
        {
            LispVariant result = Lisp.Eval("(list (<= 1 2) (<= 4 1) (>= 5 2) (>= 1 3) (>= 4.0 4.0) (<= 42 42))");
            Assert.AreEqual("(#t #f #t #f #t #t)", result.ToString());
        }

        [TestMethod]
        public void Test_Not()
        {
            LispVariant result = Lisp.Eval("(list (! #t) (not #t) (not #f) (! #f))");
            Assert.AreEqual("(#f #f #t #t)", result.ToString());
        }

        [TestMethod]
        public void Test_Arithmetric1()
        {
            LispVariant result = Lisp.Eval("(+ 1 2 3 4)");
            Assert.AreEqual(10, result.ToInt());
        }

        [TestMethod]
        public void Test_Arithmetric2()
        {
            LispVariant result = Lisp.Eval("(+ 1.1 2.2 3.3 4.3)");
            int res = (int)Math.Round(result.ToDouble() * 10.0);
            Assert.AreEqual(109, res);
        }

        [TestMethod]
        public void Test_Arithmetric3()
        {
            LispVariant result = Lisp.Eval("(* 3 8 2)");
            Assert.AreEqual(48, result.ToInt());
        }

        [TestMethod]
        public void Test_Arithmetric4()
        {
            LispVariant result = Lisp.Eval("(/ 1.0 2.0)");
            int res = (int)Math.Round(result.ToDouble() * 10.0);
            Assert.AreEqual(5, res);
        }

        [TestMethod]
        public void Test_Arithmetric5()
        {
            LispVariant result = Lisp.Eval("(/ 10 2)");
            Assert.AreEqual(5, result.ToInt());
        }

        [TestMethod]
        public void Test_Arithmetric6()
        {
            LispVariant result = Lisp.Eval("(- 42 12 6)");
            Assert.AreEqual(24, result.ToInt());
        }

        [TestMethod]
        public void Test_Arithmetric7()
        {
            LispVariant result = Lisp.Eval("(- 42.5 0.5)");
            int res = (int)Math.Round(result.ToDouble() * 10.0);
            Assert.AreEqual(420, res);
        }

        [TestMethod]
        public void Test_Macros1()
        {
            LispVariant result = Lisp.Eval("(do (define-macro blub (lambda (x y) (print x y))) (print (quote (1 2 3))) (blub 3 4))");
            Assert.AreEqual("3 4", result.ToString());
        }

        [TestMethod]
        public void Test_MacrosExpand1()
        {
            LispVariant result = Lisp.Eval("(do (define-macro-expand blub (x y) (print x y)) (print (quote (1 2 3))) (blub 3 4))");
            Assert.AreEqual("3 4", result.ToString());
        }

        [TestMethod]
        public void Test_MacrosExpand2()
        {
            const string MacroExpandScript = @"(do
  (define-macro-expand first-macro
        (a b) 
        (do 
    	   (def i 1)
           (+ a b i)
        )
  )
  
  (define-macro-expand second-macro
        (x y) 
        (do 
           (* x y (first-macro x y))
        )
  )
  
  (def m (second-macro 4 3))
)";
            LispVariant result = Lisp.Eval(MacroExpandScript);
            Assert.AreEqual("96", result.ToString());
        }

        [TestMethod]
        public void Test_Quasiquote1()
        {
            LispVariant result = Lisp.Eval("(do (def a '(42 99 102 \"hello\")) (def b 55) (print (type a)) (print (nth 3 `(1 2 3 ,@a))))");
            Assert.AreEqual("42", result.ToString());
        }

        [TestMethod]
        public void Test_Quasiquote2()
        {
            LispVariant result = Lisp.Eval("(do (def a 42) (print `(1 2 3 ,a)))");
            Assert.AreEqual("(1 2 3 42)", result.ToString());
        }

        [TestMethod]
        public void Test_Quote1()
        {
            LispVariant result = Lisp.Eval("(do (def x 42) (print 'x))");
            Assert.AreEqual("x", result.ToString());
        }

        [TestMethod]
        public void Test_String1()
        {
            LispVariant result = Lisp.Eval("(print \"hello \\\\ \\' öäü \n \\\"blub\\\"\")");
            Assert.AreEqual("hello \\ ' öäü \n \"blub\"", result.ToString());
        }

        [TestMethod]
        public void Test_String2()
        {
            LispVariant result = Lisp.Eval("(string \"hello\" \"-\" \"world\")");
            Assert.AreEqual("hello-world", result.ToString());
        }

        [TestMethod]
        public void Test_Map()
        {
            LispVariant result = Lisp.Eval("(map (lambda (x) (+ x 1)) '(1 2 3))");
            Assert.AreEqual("(2 3 4)", result.ToString());
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_MapError1()
        {
            LispVariant result = Lisp.Eval("(map 4 '(1 2 3))");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_MapError2()
        {
            LispVariant result = Lisp.Eval("(map (lambda (x) (+ x 1)) 4)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_ReduceError1()
        {
            LispVariant result = Lisp.Eval("(reduce \"blub\" '(1 2 3) 0)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_ReduceError2()
        {
            LispVariant result = Lisp.Eval("(reduce (lambda (x y) (+ x y))  \"test\" 0)");
        }

        [TestMethod]
        public void Test_Reduce1()
        {
            LispVariant result = Lisp.Eval("(reduce (lambda (x y) (+ x y)) '(1 2 3) 0)");
            Assert.AreEqual(6, result.ToInt());
        }

        [TestMethod]
        public void Test_Reduce2()
        {
            LispVariant result = Lisp.Eval("(reduce (lambda (x y) (* x y)) '(2 3 4 5) 2)");
            Assert.AreEqual(240, result.ToInt());
        }

        [TestMethod]
        public void Test_Closure1()
        {
            LispVariant result = Lisp.Eval("(do (defn addx (delta) (lambda (x) (+ x delta))) (def addclosure (addx 41)) (print (addclosure 1)))");
            Assert.AreEqual(42, result.ToInt());
        }
        
        [TestMethod]
        public void Test_RecursiveCall1()
        {
            LispVariant result = Lisp.Eval("(do (defn addConst (x a) (+ x a)) (def add2 (lambda (x) (addConst x 2))) (print (addConst 8 2)) (print (add2 4)))");
            Assert.AreEqual(6, result.ToInt());
        }

        [TestMethod]
        public void Test_Return1()
        {
            LispVariant result = Lisp.Eval("(do (defn f (x) (return (+ x x))) (print (f 7)))");
            Assert.AreEqual(14, result.ToInt());
        }

        [TestMethod]
        public void Test_Call1()
        {
            LispVariant result = Lisp.Eval("(do (def obj 0) (setf obj (call \"CsLisp.DummyNative\")) (print obj (type obj)) (call obj \"Test\"))");
            Assert.AreEqual(42, result.ToInt());
        }

        [TestMethod]
        public void Test_Apply1()
        {
            LispVariant result = Lisp.Eval("(apply (lambda (x) (print \"hello\" x)) '(55))");
            Assert.AreEqual("hello 55", result.ToString());
        }

        [TestMethod]
        public void Test_Apply2()
        {
            LispVariant result = Lisp.Eval("(do (def f (lambda (x) (+ x x))) (apply f '(5)))");
            Assert.AreEqual(10, result.ToInt());
        }

        [TestMethod]
        public void Test_Apply3()
        {
            LispVariant result = Lisp.Eval("(do (def f '+) (apply f '(5 6 7)))");
            Assert.AreEqual(18, result.ToInt());
        }

        [TestMethod]
        public void Test_Cons1()
        {
            LispVariant result = Lisp.Eval("(cons 1 2)");
            Assert.AreEqual("(1 2)", result.ToString());
        }

        [TestMethod]
        public void Test_Cons2()
        {
            LispVariant result = Lisp.Eval("(cons 1 '(2 3 4))");
            Assert.AreEqual("(1 2 3 4)", result.ToString());
        }

        [TestMethod]
        public void Test_Cons3()
        {
            LispVariant result = Lisp.Eval("(cons 12)");
            Assert.AreEqual("(12)", result.ToString());
        }

        [TestMethod]
        public void Test_Cons4()
        {
            LispVariant result = Lisp.Eval("(cons)");
            Assert.AreEqual("()", result.ToString());
        }

        [TestMethod]
        public void Test_Nop()
        {
            LispVariant result = Lisp.Eval("(nop)");
            Assert.IsTrue(result.IsUndefined);
        }

        [TestMethod]
        public void Test_Symbol()
        {
            LispVariant result = Lisp.Eval("(sym a)");
            Assert.IsTrue(result.IsSymbol);
            Assert.AreEqual("a", result.StringValue);
        }

        [TestMethod]
        public void Test_Str()
        {
            LispVariant result = Lisp.Eval("(str abc)");
            Assert.IsTrue(result.IsString);
            Assert.AreEqual("abc", result.StringValue);
        }

        #region expected errors

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_Parser1()
        {
            LispVariant result = Lisp.Eval("(print \"hello\"))");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_Parser2()
        {
            LispVariant result = Lisp.Eval("((print \"hello\")");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_Parser3()
        {
            LispVariant result = Lisp.Eval("(blub 1 2 3)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_SetfError()
        {
            LispVariant result = Lisp.Eval("(setf a 2.0)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_NotError()
        {
            LispVariant result = Lisp.Eval("(not a 2.0)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_CompareError1()
        {
            LispVariant result = Lisp.Eval("(> 2.0)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_CompareError2()
        {
            LispVariant result = Lisp.Eval("(> 2.0 5 234)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_ScriptToLong()
        {
            LispVariant result = Lisp.Eval("(setf a 2.0) asdf");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_DefError()
        {
            LispVariant result = Lisp.Eval("(def 1 2)");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_DoError()
        {
            LispVariant result = Lisp.Eval("(do (def a 2) blub (setf a 5))");
        }

        [TestMethod]
        [ExpectedException(typeof(LispException))]
        public void Test_IfError()
        {
            LispVariant result = Lisp.Eval("(if #t 1 2 3)");
        }

        #endregion

        #region interpreter internals

        [TestMethod]
        public void Test_LispVariantToString()
        {
            var result = new LispVariant("hello");
            string s = result.ToString();
            Assert.AreEqual("hello", s);
        }

        [TestMethod]
        public void Test_LispScope1()
        {
            var scope = new LispScope();
            var result = scope.GetPreviousToken(new LispToken("a", 0, 0, 1));
            Assert.AreEqual(null, result);
        }

        [TestMethod]
        public void Test_LispTokenToString()
        {
            var token = new LispToken("(", 0, 1, 1);
            string s = token.ToString();
            Assert.AreEqual("(", s);
        }

        #endregion

        #region infrastructure and debugging

        [TestMethod]
        public void Test_Vars()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(do (def a 4) (def b \"asdf\") (vars))");
                Assert.IsTrue(result.IsUndefined);

                string s = cr.ToString().Trim();
                Assert.AreEqual(true, s.Contains("a --> 4"));
                Assert.AreEqual(true, s.Contains("b --> asdf"));
            }
        }

        [TestMethod]
        public void Test_Fuel()
        {
            LispVariant result = Lisp.Eval("(fuel)");
            Assert.IsTrue(result.IsString);
            Assert.IsTrue(result.StringValue.Contains("fuel version"));
        }

        [TestMethod]
        public void Test_Copyright()
        {
            LispVariant result = Lisp.Eval("(copyright)");
            Assert.IsTrue(result.IsString);
            Assert.IsTrue(result.StringValue.Contains("Copyright: MIT-License"));
        }

        [TestMethod]
        public void Test_Help()
        {
            LispVariant result = Lisp.Eval("(help)");
            Assert.IsTrue(result.IsString);
            Assert.IsTrue(result.StringValue.Contains("available functions:"));
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\Library\fuellib.fuel", "Library")]
        public void Test_Import()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(do (import \"Library\\\\fuellib.fuel\") (foreach '(1 2 3) (lambda (x) (print x))))");
                Assert.IsTrue(result.IsInt);
                Assert.AreEqual(3, result.IntValue);

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("1"));
                Assert.IsTrue(s.Contains("2"));
                Assert.IsTrue(s.Contains("3"));
            }
        }

        [TestMethod]
        [DeploymentItem(@"..\..\..\Library\fuellib.fuel", "Library")]
        public void Test_Import2()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(do (import fuellib) (foreach '(1 2 3) (lambda (x) (print x))))");
                Assert.IsTrue(result.IsInt);
                Assert.AreEqual(3, result.IntValue);

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("1"));
                Assert.IsTrue(s.Contains("2"));
                Assert.IsTrue(s.Contains("3"));
            }
        }

        [TestMethod]
        public void Test_Break()
        {
            using (ConsoleRedirector cr = new ConsoleRedirector())
            {
                LispVariant result = Lisp.Eval("(break)");
                Assert.IsTrue(result.IsUndefined);

                string s = cr.ToString().Trim();
                Assert.IsTrue(s.Contains("no debugger support"));
            }            
        }

        #endregion

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
}
