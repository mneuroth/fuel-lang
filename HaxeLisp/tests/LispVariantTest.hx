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

class LispVariantTest extends utest.Test {
    public function testVariant1() {
        var value = LispVariant.LispVariant.forValue(42);
        Assert.equals(LispVariant.LispType.Int, value.ValueType);
        Assert.equals(42, value.Value);
        Assert.isTrue(value.IsInt);
        Assert.isTrue(value.IsNumber);
        Assert.isFalse(value.IsDouble);
        var value = LispVariant.LispVariant.forValue(2.54);
        Assert.equals(LispVariant.LispType.Double, value.ValueType);
        Assert.equals(2.54, value.Value);
        var value = LispVariant.LispVariant.forValue("hello");
        Assert.equals(LispVariant.LispType.String, value.ValueType);
        Assert.equals("hello", value.Value);
        var value = LispVariant.LispVariant.forValue(false);
        Assert.equals(LispVariant.LispType.Bool, value.ValueType);
        Assert.equals(false, value.Value);
        var value = LispVariant.LispVariant.forValue(null);
        Assert.equals(LispVariant.LispType.Nil, value.ValueType);
        Assert.equals(null, value.Value);
    }
    public function testVariant2() {
        var value1 = LispVariant.LispVariant.forValue(4.3);
        var value2 = LispVariant.LispVariant.forValue(56.1);
        var value3 = LispVariant.LispVariant.forValue(42);
        var value4 = LispVariant.LispVariant.forValue("abc");
        Assert.isTrue(value1.IsDouble);
        Assert.isTrue(value1.IsNumber);
        Assert.isTrue(value1.CompareTo(value2) < 0);
        Assert.isTrue(value2.CompareTo(value1) > 0);
        Assert.isTrue(value1.CompareTo(1.23) > 0);
        Assert.isTrue(value1.CompareTo(-5) > 0);
        Assert.isTrue(value3.CompareTo(42) == 0);
        Assert.isTrue(value4.CompareTo("abc") == 0);
        Assert.isTrue(value4.CompareTo("xyz") < 0);
    }
    public function testVariant3() {
        var value1 = LispVariant.LispVariant.forValue(4.3);
        var value2 = LispVariant.LispVariant.forValue(56.1);
        var value3 = LispVariant.LispVariant.forValue(42);
        var value4 = LispVariant.LispVariant.forValue("4.5");
        var value5 = LispVariant.LispVariant.forValue(true);
        var value6 = new LispVariant.LispVariant(LispVariant.LispType.Int, 0);
        Assert.equals(true, value1.ToBool());
        Assert.equals(true, value3.ToBool());
        Assert.equals(false, value6.ToBool());
        Assert.equals(4.5, value4.ToDouble());
        Assert.equals(1.0, value5.ToDouble());
        Assert.equals(56, value2.ToInt());
        Assert.equals(true, value2.ToBool());
    }
}
