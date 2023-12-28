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

import LispException.LispException;

using StringTools;
using LispUtils;

/// <summary>
/// The FUEL lisp tokenizer
/// </summary>
/*public*/ class LispTokenizer
{
    public function new() {        
    }

    /// <summary>
    /// Tokenizes the specified code.
    /// </summary>
    /// <param name="code">The code.</param>
    /// <param name="offset">The position offset (decorated code).</param>
    /// <returns>Container with tokens</returns>
    public static function Tokenize(code:String, offset:Int = 0):Array<LispToken>    // IEnumerable<LispToken>
    {
        var tokens = new Array<LispToken>();
        var currentToken = ""; //string.Empty;
        var currentTokenStartPos = 0;
        var lineCount = 1;
        var isInString = false;
        var isInSymbol = false;
        var wasLastBackslash = false;

        /*Action<string, int, int>*/var addToken = function(currentTok, pos, line) //=>
        {
            tokens.Add(new LispToken(currentTok, currentTokenStartPos - offset, pos - offset, line));
            isInSymbol = false;
            isInString = false;
            currentToken = ""; //string.Empty;
            currentTokenStartPos = pos+1;
        };

        var i = 0;
        //for (i in 0...code.length)
        while (i < code.length)
        {
            var ch:String = code.charAt(i);
            if (ch.isSpace(0))
            {
                if (isInString)
                {
                    currentToken += ch;
                }
                else if (isInSymbol)
                {
                    addToken(currentToken, i, lineCount);
                }
                wasLastBackslash = false;
                if (ch == '\n')
                {
                    lineCount++;
                }
            }
            else if (ch == '\\')
            {
                if (wasLastBackslash)
                {
                    currentToken += ch;
                    wasLastBackslash = false;
                }
                else
                {
                    wasLastBackslash = true;
                }
            }
            else if (ch == '(' || ch == ')')
            {
                if (isInString)
                {
                    currentToken += ch;
                }
                else if (isInSymbol)
                {
                    addToken(currentToken, i, lineCount);
                    addToken(""/*string.Empty*/ + ch, i, lineCount);
                }
                else
                {
                    addToken(""/*string.Empty*/ + ch, i, lineCount);
                }
                wasLastBackslash = false;
            }
            else if (ch == ';')
            {
                if (isInString)
                {
                    currentToken += ch;
                }
                else if (isInSymbol)
                {
                    addToken(currentToken, i, lineCount);
                    i = ProcessComment(code, i, lineCount, ch, addToken);
                }
                else
                {
                    i = ProcessComment(code, i, lineCount, ch, addToken);
                }
                wasLastBackslash = false;
                // comment ends always with new line
                lineCount++;
            }
            else if (ch == '\'' || ch == '`' || ch == ',')
            {
                if (isInString)
                {
                    currentToken += ch;
                }
                else
                {
                    if (code.charAt(i + 1) == '@')
                    {
                        // process unquotesplicing
                        var s = ""; //string.Empty;
                        s += ch;
                        i++;
                        s += code.charAt(i);
                        addToken(s, i, lineCount);
                    }
                    else
                    {
                        addToken(""/*string.Empty*/ + ch, i, lineCount);
                    }
                }
                wasLastBackslash = false;
            }
            else if (ch == '"')
            {
                if (wasLastBackslash)
                {
                    currentToken += ch;
                }
                else if (isInString)
                {
                    // finish string
                    addToken("\"" + currentToken + "\"", i, lineCount);
                }
                else
                {
                    // start string
                    isInString = true;
                    currentToken = ""; //string.Empty;
                }
                wasLastBackslash = false;
            }
            else
            {
                if (!isInSymbol && !isInString)
                {
                    isInSymbol = true;
                }
                if (wasLastBackslash)
                {
                    ch = ProcessCharAfterBackslash(ch);
                }
                currentToken += ch;
                wasLastBackslash = false;
            }
            i++;
        }
        if (currentToken != ""/*string.Empty*/)
        {
            addToken(currentToken, -1, lineCount);
        }
        return tokens;
    }

    /*private*/ static function ProcessCharAfterBackslash(ch:String):String
    {
        switch (ch)
        {
            case "n":
                return "\n";
            case "r":
                return "\r";
            case "t":
                return "\t";
            case "\\":
                return "\\";
        }
        throw new LispException("Invalid character after backslash ${ch}");
    }

    /*private*/ static function ProcessComment(code:String, i:Int, lineCount:Int, /*char*/ ch:String, /*Action<string, int, int>*/ addToken:Dynamic):Int
    {
        var temp = GetRestOfLine(code, i + 1/*, out newIndex*/);
        var newIndex:Int = temp.value2;
        var comment = /*string.Empty*/"" + ch + temp.value1;
        addToken(comment, i, lineCount);
        i = newIndex;
        return i;
    }

    /*private*/ static function GetRestOfLine(code:String, i:Int/*, out int newIndex*/):TupleReturn<String,Int>
    {
        var newIndex:Int;
        var rest = code.substr(i);
        var pos = rest.indexOf("\n"/*, StringComparison.InvariantCulture*/);
        if (pos > 0)
        {
            newIndex = i + pos;
            return new TupleReturn<String,Int>(rest.substr(0, pos+1),newIndex);
        }
        newIndex = i + rest.length -1;
        return new TupleReturn<String,Int>(rest,newIndex);
    }
}
