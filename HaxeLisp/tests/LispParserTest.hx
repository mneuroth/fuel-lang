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

class LispParserTest extends utest.Test {
    public function testParser1() {
        var result = LispParser.LispParser.Parse("()");
        Assert.equals(0, result.length);
    }
    public function testParser2() {
        var result = LispParser.LispParser.Parse("(print 1 2.54 \"string\")");
        Assert.equals(4, result.length);
        Assert.equals("print", result[0].Value);
        Assert.equals(LispVariant.LispType.Symbol, result[0].ValueType);
        Assert.equals(1, result[1].Value);
        Assert.equals(LispVariant.LispType.Int, result[1].ValueType);
        Assert.equals(2.54, result[2].Value);
        Assert.equals(LispVariant.LispType.Double, result[2].ValueType);
        Assert.equals("string", result[3].Value);
        Assert.equals(LispVariant.LispType.String, result[3].ValueType);
    }
    public function testParser3() {
        var result = LispParser.LispParser.Parse("(do (print #t 2.54 \"string\"))");
        Assert.equals(2, result.length);
        Assert.equals("do", result[0].Value);
        Assert.equals(LispVariant.LispType.Symbol, result[0].ValueType);
        var temp = result[1].copy();    // copy() because of a bug in Haxe ??? Array access is not allowed on Unknown<0>
        Assert.isTrue(temp is Array);
        Assert.equals(4, temp.length);     
        Assert.equals("print", temp[0].Value);
        Assert.equals(LispVariant.LispType.Symbol, temp[0].ValueType);
        Assert.equals(true, temp[1].Value);
        Assert.equals(LispVariant.LispType.Bool, temp[1].ValueType);
        Assert.equals(2.54, temp[2].Value);
        Assert.equals(LispVariant.LispType.Double, temp[2].ValueType);
        Assert.equals("string", temp[3].Value);
        Assert.equals(LispVariant.LispType.String, temp[3].ValueType);
    }
}
