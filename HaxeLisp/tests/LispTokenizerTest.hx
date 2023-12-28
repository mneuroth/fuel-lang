/*
 * FUEL(isp) is a fast usable embeddable lisp interpreter.
 *
 * Copyright (c) 2023 Michael Neuroth
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

import utest.Assert;
import utest.Async;

class LispTokenizerTest extends utest.Test {
    public function testTokenizer1() {
        var tokens = LispTokenizer.LispTokenizer.Tokenize("");
        Assert.equals(0, tokens.length);
        var tokens = LispTokenizer.LispTokenizer.Tokenize("     \t \n   ");
        Assert.equals(0, tokens.length);
    }
    public function testTokenizer2() {
        var tokens = LispTokenizer.LispTokenizer.Tokenize("()");
        Assert.equals(2, tokens.length);
        var tokens = LispTokenizer.LispTokenizer.Tokenize("  (  \n    )  ");
        Assert.equals(2, tokens.length);
        Assert.equals("(", tokens[0].ToStr());
        Assert.equals(")", tokens[tokens.length-1].ToStr());
    }
    public function testTokenizer3() {
        var tokens = LispTokenizer.LispTokenizer.Tokenize("(+ 1 #t 3.1415 \"asdf blub\" #f )");
        var arrTokens:Array<LispToken> = new Array<LispToken>();
        for(tok in tokens) {
            arrTokens.push(tok);
        }
        Assert.equals(8, tokens.length);
        Assert.equals("(", tokens[0].ToStr());
        Assert.equals(")", tokens[tokens.length-1].ToStr());
        Assert.equals("+", arrTokens[1].ToStr());
        Assert.equals(1, arrTokens[2].Value);
        Assert.equals(true, arrTokens[3].Value);
        Assert.equals(3.1415, arrTokens[4].Value);
        Assert.equals("asdf blub", arrTokens[5].Value);
        Assert.equals(false, arrTokens[6].Value);
    }
    public function testTokenizer4() {
        var tokens = LispTokenizer.LispTokenizer.Tokenize("(do (print (* 9 9)))");
        var arrTokens:Array<LispToken> = new Array<LispToken>();
        for(tok in tokens) {
            arrTokens.push(tok);
        }
        Assert.equals(11, tokens.length);
        Assert.equals("(", tokens[0].ToStr());
        Assert.equals(")", tokens[tokens.length-1].ToStr());
        Assert.equals("do", arrTokens[1].ToStr());
        Assert.equals("(", arrTokens[2].ToStr());
        Assert.equals("print", arrTokens[3].ToStr());
        Assert.equals("(", arrTokens[4].ToStr());
        Assert.equals("*", arrTokens[5].ToStr());
        Assert.equals(9, arrTokens[6].Value);
        Assert.equals(9, arrTokens[7].Value);
        Assert.equals(")", arrTokens[8].ToStr());
        Assert.equals(")", arrTokens[9].ToStr());
        Assert.equals(")", arrTokens[10].ToStr());
    }
}
