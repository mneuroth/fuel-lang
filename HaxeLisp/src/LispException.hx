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

package;

import haxe.ds.StringMap;
import haxe.Exception;

import LispToken.LispToken;

class LispException extends haxe.Exception {
    
    public var Token:LispToken;
    public var ModuleName:String;
    public var StackInfo:String;

    public var ExcData:StringMap<Dynamic>;

    public function new(text:String, token:LispToken=null, moduleName:String="", stackInfo:String="not available") {
        super(text);
        Token = token;
        ModuleName = moduleName;
        StackInfo = stackInfo;
    }

    public static function fromScope(text:String, scope:LispScope):LispException {
        var exc:LispException;
        if( scope != null ) {
            exc = new LispException(text, scope.CurrentToken, scope.ModuleName);
        } else {
            exc = new LispException(text);
        }
        return exc;
    }
}
