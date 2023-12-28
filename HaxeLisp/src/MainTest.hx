/**
    Multi-line comments for documentation.
**/

import LispUtils.Ref;

//import haxe.ds.ObjectMap;

function testx(val:Ref<Int>) {
    trace(val);
    val.value = 7;
}

class LispScopeX extends /*haxe.ds.StringMap<Dynamic>*//*Map<String,Dynamic>*/haxe.ds.ObjectMap<String,Dynamic> {
    public function new() {
        super();
    }

    @:arrayAccess
    public inline function getx(key:String) {
      return this.get(key);
    }    
    
}

//class LispScopeX implements Map<String,Dynamic> {
//}

class Main {
    static public function main():Void {
      /*
      trace("Hello World");
      var token = new LispToken.LispToken("1.2", 0, 2, 7);
      trace(token);
      var token2 = new LispToken.LispToken("42", 0, 2, 7);
      trace(token2);
      trace(token2.Type);
      trace(Std.parseFloat("1.234d"));
      trace("??????????????");
      trace(haxe.Json.parse("1.234"));
      //$type(haxe.Json.parse("1.234"));
      trace("??????????????-------------");
      trace(haxe.Json.parse("42"));
      var x = haxe.Json.parse("42.3");
      trace(Type.typeof(x) == TFloat);  // -> TInt | TFloat
      trace("??????????????-------------");

      var token1a = new LispToken.LispToken("1.2321", 0, 2, 7);
      var token2b = new LispToken.LispToken("3", 0, 2, 7);
      trace("!!!!!!!!!!!!!!!!!!!!!!");
      trace(token1a.Type, token1a.Value);
      trace(token2b.Type, token2b.Value);

      var token1 = new LispToken.LispToken("1.2", 0, 2, 7);
      var token2 = new LispToken.LispToken("asdf", 0, 2, 7);
      trace(token1.Type);
      trace(token2.Type);

      var tokenizer = new LispTokenizer.LispTokenizer();
      trace(tokenizer);
      var tokens = LispTokenizer.LispTokenizer.Tokenize("()");
      trace(tokens);
      trace(tokens[0].ToStr());

      var result = LispParser.LispParser.Parse("(+ 1 2 3)");      
      trace("PARSER:", result);
      trace("PARSER-0:", result[0].value, result[0].type, Type.typeof(result[0]));
      trace(Type.typeof(token));
      trace("XXX",Type.typeof("token"));
      trace("xxx" is String);
      trace(2 is Int);
      trace(x is LispToken.LispToken);
      */
      /*
//      var result2 = LispParser.LispParser.Parse("(print 1 2.54 \"string\")");
      var result2 = LispParser.LispParser.Parse("(do (print #t 2.54 \"string\"))");      
      trace("----------------------------");
      trace(result2);
      trace(Type.typeof(result2[0]));
      trace(Type.typeof(result2[1]));
      trace("----------------------------");
      trace(result2[1] is Array);
      var temp = result2[1];
      trace(temp.length);
      trace(temp[0]);
      trace(temp[0].Value);
      */
      //LispInterpreter.LispInterpreter.ResolveInScopes(null, false);
      //var scope = new LispInterpreter.LispScope();
      var variant = LispVariant.forValue(3);
      var t:LispVariant.LispType = List;
      var s:String = LispVariant.ToStringT(t);
      trace("+++++++++++++++++++++++++++++");
      trace(t);
      trace(s);
      trace(variant);
      trace(">>>>",variant.TypeString);
      trace(">>>>",variant.IsInt);

      //var value1 = LispVariant.LispVariant.forValue(4.3);
      //trace(value1.CompareTo(1.23));

      //var s:String = 'abc';
      //trace(s.charAt(1));

//      var x = new LispScopeX();
//      var x = new Map<String,Dynamic>();
//      trace(x);

        //var myMap:Map<String,Dynamic> = ["hello" => 7, "a" => "aaa"]; //new Map<String,Dynamic>(); //haxe.ds.StringMap<Dynamic>();
        var myMap = new LispScopeX();
        //myMap.set("hello", "a4");
        trace(myMap.get('hello'));
        //trace(myMap.getx('hello'));
        //trace(myMap['hello']);
        trace(myMap);

        var scope = LispEnvironment.CreateDefaultScope();
        var ast = LispParser.Parse("(+ 1 2 3 4)");
        //var ast = LispParser.Parse("(fuel 1 2 3 4)");
        trace("AST:");
        trace(ast);
        var interpRes = LispInterpreter.EvalAst(ast, scope);
        trace("RESULT:",interpRes, "value=",interpRes.Value);

/*
      var x = new Ref<Int>(5);
      trace("----------------");
      trace("start:",x);
      testx(x);
      trace("final:",x);
      trace(x.value == 7);
*/
      trace("----------------------------done.");

    }
}
