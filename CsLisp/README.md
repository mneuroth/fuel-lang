# fuel-lang == FUEL(isp)
FUEL(isp) is a fast usable embeddable lisp interpreter (currently for the .NET platform)

FUEL
----
FUEL is a lisp interperter which is intended to be used as a scripting
and extension language for applications.

FUEL comes with a command line application which can execute programs: 

    >type test.fuel
    (println "hello fuel")
    
    >fuel.exe test.fuel
    hello fuel
  
The FUEL interpreter can easily be embedded into .NET applications.
Just link the CsLispInterpreter.dll to your .NET solution and use
the static Lisp.Eval() function.

    using CsLisp;
  
    ...
  
    // script = "(* 6 7)"
    private double InvokeFuelScript(string script)
    {
        // evaluate the given script
        LispVariant result = Lisp.Eval(script);
        
        // convert result to expected type
        double d = result.DoubleValue;
        
        // returns 42
        return d;
    }
  
Objects from the embedding application can easily be registered 
at the FUEL interpreter and used in a script.

    private double InvokeFuelScriptWithAppObj(string script, string appObjName, object appObj)
    {
        var nativeItems = new Dictionary<string, object>();
        nativeItems[appObjName] = appObj;
        
        // script = '(call appObjName "MethodName" args ...)'
        LispVariant result = Lisp.Eval(script, nativeItems: nativeItems);
        
        return result.DoubleValue;
    }

Even complete classes can be registered at the FUEL interpreter.

    (import fuellib)
    
    ;; register in FUEL script
    (evalstr (create-native "FUEL_name" "full_NET_name"))
    
    ;; use in FUEL script
    (def obj (create-FUEL_name))    ; create native object
    (FUEL_name-DoIt obj 123)        ; use native object
    

Interactive loop
----------------
FUEL comes with an interactive loop.

    >fuel -i
  
    FUEL(isp) v0.99.1 (for .NET/C#) from 31.3.2016, (C) by Michael Neuroth
  
    Type "help" for informations.
  
    FUEL(isp)-DBG> (println "Hello fuel")
    hello fuel
    result=Hello fuel
    FUEL(isp)-DBG>

  
Debugger
--------
FUEL comes with a command line debugger. 

    >fuel -d test.fuel
  
    FUEL(isp) v0.99.1 (for .NET/C#) from 31.3.2016, (C) by Michael Neuroth
  
    Type "help" for informations.
  
    --> do line=1 start=-3 stop=-1 module=test.fuel
    FUEL(isp)-DBG>
  
A graphical front end for the debugger is available in the VisiScript
Text Editor in version 0.6.0 and above. 
See: http://mneuroth.de/projects/Visiscript.html.


Compiler
--------
FUEL comes with a compiler (experimental).

    >fuel -c test.fuel
  

Documentation
-------------
For more documentation of the FUEL language see:

    >fuel --doc
  
Or navigate to the homepage(s):

https://github.com/mneuroth/fuel-lang
    
http://mneuroth.de/projects/Fuel.html.
  
Or inspect the demo and test scripts and
look at the TestAppUsingFuel project.

  
Deployment  
----------
The binary distribution of FUEL consists of the following components:

    LICENSE                   (license)
    README.md                 (this file)
    fuel.exe                  (command line application)
    fuel.exe.config           
    Library/fuellib.fuel      (standard library)
    CsLispInterpreter.dll     
    CsLispDebugger.dll        (optional)
    CsLispCompiler.dll        (optional)
  
  
Platforms  
---------
FUEL is developed with the Microsoft .NET version 3.5 under Windows.
FUEL can be used with mono environment under Linux and Mac OS X.
  

License
-------
FUEL is released under the MIT license:

>  FUEL(isp) is a fast usable embeddable lisp interpreter.
>  
>  Copyright (c) 2016 Michael Neuroth
>
>  Permission is hereby granted, free of charge, to any person obtaining
>  a copy of this software and associated documentation files (the "Software"),
>  to deal in the Software without restriction, including without limitation
>  the rights to use, copy, modify, merge, publish, distribute, sublicense,
>  and/or sell copies of the Software, and to permit persons to whom the
>  Software is furnished to do so, subject to the following conditions:
>
>  The above copyright notice and this permission notice shall be included
>  in all copies or substantial portions of the Software.
>
>  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
>  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
>  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
>  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
>  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
>  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
>  OTHER DEALINGS IN THE SOFTWARE.
  