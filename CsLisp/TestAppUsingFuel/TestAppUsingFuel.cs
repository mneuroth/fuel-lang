using System;
using System.Collections.Generic;
using CsLisp;

namespace TestAppUsingFuel
{
    /// <summary>
    /// Demo application to show the embedding of the fuel interpreter
    /// and the interaction between fuel interpreter and embedding application.
    /// </summary>
    class TestAppUsingFuel
    {
        static void ExecuteFuelScript(string script)
        {
            var model = new TestModel("fuel");

            // create a dictionary with name <--> object pairs
            // the objects will be registered under the name in the fuel interpreter
            var nativeItems = new Dictionary<string, object>();
            nativeItems["model"] = model;

            LispVariant result = Lisp.Eval(script, nativeItems: nativeItems);

            Console.WriteLine("Script result={0}", result);
        }

        static void Main(string[] args)
        {
            Console.WriteLine("Test application using fuel interpreter.");

            ExecuteFuelScript(
                "(println model)" +
                "(println (call model GetGreetings \"hello\")) " +      // call a method
                "(println (call model get_Name))" +                     // access property --> get
                "(println (call model set_Name \"test\"))" +            // access property --> set
                "(println (call model GetGreetings \"hello\")) "        // call a method
            );

            Console.WriteLine("done.");
        }
    }

    /// <summary>
    /// Demo model class to be used in fuel interpreter
    /// </summary>
    internal class TestModel
    {
        public string Name { get; set; }

        public string GetGreetings(string text)
        {
            return text + " " + Name;
        }

        public TestModel(string name)
        {
            Name = name;
        }
    }

}
