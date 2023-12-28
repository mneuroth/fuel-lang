/**
    Multi-line comments for documentation.
**/

import LispUtils.CurrentTickCount;
using LispScope;
using LispUtils;
using LispDebugger;

using StringTools;

class Main {

  static public function main():Void {
      //var args = Sys.args();  // -> Accessing this field requires a system platform (php,neko,cpp,etc.)
      trace("Hello World");
      var token = new LispToken.LispToken("1.2", 0, 2, 7);
      trace(token);
      var token2 = new LispToken.LispToken("42", 0, 2, 7);
      trace(token2);
      trace(Std.parseFloat("1.234d"));
      trace(haxe.Json.parse("1.234"));

      var i = 3;
      var lv:Dynamic = LispVariant.forValue(3.3);
      trace("#############################");
      trace(i is LispVariant);

      var scope = LispEnvironment.CreateDefaultScope();
      //var ast = LispParser.Parse("(do (+ 1 2 (* 3 42)) (- 3 2))");
      //var ast = LispParser.Parse("(do (defn f (x) 
      //                                    (+ x 1)
      //                                )                              
      //                                (f 7))");
      //var ast = LispParser.Parse("(do (def i 1) (setf i (+ i 1)))");
      //var ast = LispParser.Parse("(list 1 2 3)");
      //var ast = LispParser.Parse("(map (lambda (x) (* x x)) (list 1 2 3 4))");
      //var ast = LispParser.Parse("(car (list 17 4 3 2 1))");
      //var ast = LispParser.Parse("(rest (list 17 4 3 2 1))");
      //var ast = LispParser.Parse("(reverse (list 4 3 2 1))");
      //var ast = LispParser.Parse("(println '(list 1 2 3))");
      //var ast = LispParser.Parse("(do (defn test (x y) (do (argscount))) (def x (test 4 5)))");
      //var ast = LispParser.Parse("(evalstr \"(* 4 2 3)\")");
//      var ast = LispParser.Parse("(trim \"  Hallo 2 Welt!\")");
//      var ast = LispParser.Parse("(do (print 99 \"hallo\"))");
      //var ast = LispParser.Parse("(fuel)");
      //var ast = LispParser.Parse("(doc)");
      //var ast = LispParser.Parse("(!= 1 2)");
      //var ast = LispParser.Parse("(fuel 1 2 3 4)");
     
      //trace("AST:");
      //trace(ast);
      //var interpRes = LispInterpreter.EvalAst(ast, scope);

//      var interpRes = Lisp.Eval("(do (defn test (x y) (+ x y 1)) (apply test '(4 5)))");
//      var interpRes = Lisp.Eval("(do (defn test (x y) (+ x y 1)) (apply test (list 4 5)))");
      //var interpRes = Lisp.Eval("(+ x y 1)");
      //var interpRes = Lisp.Eval("'(1 2 3)");
      //var interpRes = Lisp.Eval("(doc)");
      //var interpRes = LispParser.LispParser.Parse("(do (print #t 2.54 \"string\"))");
      //var interpRes = Lisp.Eval("(replace \"Hallo Welt\" \"Welt\" \"earth\")");
      //var interpRes = Lisp.Eval("(searchdoc \"doc\")");
      //var interpRes = Lisp.Eval("(do (def x 1) (def hallo \"asdf\") (vars))");
      //var interpRes = Lisp.Eval("(do (def d (make-dict)) (dict-set d \"a\" 7) (print (dict-get d \"b\")))");
      //var interpRes = Lisp.Eval("(do (def d (make-dict)) (dict-set d \"a\" 7) (dict-set d \"def\" \"nix\") (dict-clear d) (len (dict-keys d)))");
      //var interpRes = Lisp.Eval("(do (def d (make-dict)) (dict-set d \"a\" 7) (dict-set d \"def\" \"nix\") (dict-contains-value d \"nix\"))");
      //var interpRes = Lisp.Eval("(do (define-macro-expand blub (x y) '(println x y)) (println (quote (1 2 3))) (blub 3 4))");
      //var interpRes = Lisp.Eval("(do (define-macro-eval blub (x y) '(println x y)) (println (quote (1 2 3))) (blub 3 4) (println \"done.\"))");
      //var interpRes = Lisp.Eval("(do (define-macro-eval blub (x y) (println x y)) (blub 3 4))");

      var interpRes = Lisp.Eval("(do (define-macro-eval dotimes (counterinfo statements)
        (do
          (def (first 'counterinfo) 0)
          (while (eval (list < (first 'counterinfo) (eval (nth 1 'counterinfo))))
            (do
               (eval 'statements)
               (setf (rval (first 'counterinfo)) (eval (list + (first 'counterinfo) 1)))
            )
          )
          ;;(delvar (first 'counterinfo))
        )
    )

    (dotimes (c 10) (println c))
");

      trace("RESULT:"/*,interpRes*/, "value=",interpRes.ToStr());
      trace("--->",Type.typeof(interpRes.ToStr()));
      //trace(Sys.systemName());
    }
}
