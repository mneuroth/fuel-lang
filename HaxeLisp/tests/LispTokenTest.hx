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

class LispTokenTest extends utest.Test {
    public function testToken1() {
        var token = new LispToken.LispToken("1.2", 0, 2, 7);
        Assert.equals(1.2, token.Value);
        Assert.equals(Type.enumConstructor(token.Type), "Double");
        Assert.equals(0, token.StartPos);
    }
    public function testToken2() {
        var token = new LispToken.LispToken("1.2abc", 0, 5, 7);
        Assert.equals("1.2abc", token.Value);
        Assert.equals(Type.enumConstructor(token.Type), "Symbol");
        Assert.equals(0, token.StartPos);
    }
    public function testToken3() {
        var token = new LispToken.LispToken("421", 0, 2, 7);
        Assert.equals(421, token.Value);
        Assert.equals(Type.enumConstructor(token.Type), "Int");
        Assert.equals(0, token.StartPos);
    }
    public function testToken4() {
        var token1 = new LispToken.LispToken("1.2", 0, 2, 7);
        var token2 = new LispToken.LispToken("3.4", 0, 2, 7);
        Assert.equals(token1.Type, token2.Type);
    }
    public function testToken5() {
        var token1 = new LispToken.LispToken("1.2", 0, 2, 7);
        var token2 = new LispToken.LispToken("3", 0, 1, 7);
        Assert.notEquals(token1.Type, token2.Type);
    }
}
