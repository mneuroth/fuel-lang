/*
* FUEL(isp) is a fast usable embeddable lisp interpreter.
*
* Copyright (c) 2016 Michael Neuroth
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

#include "Scope.h"
#include "Interpreter.h"

namespace CppLisp
{
	LispScope::LispScope(const string & fcnName, std::shared_ptr<LispScope> globalScope, std::shared_ptr<string> moduleName, std::shared_ptr<TextWriter> outp, std::shared_ptr<TextReader> inp)
	{
		Debugger = null;
		IsInEval = false;
		IsInReturn = false;
		NeedsLValue = false;
		Name = fcnName;
		ModuleName = moduleName ? *moduleName : string::Empty;
		//GlobalScope = globalScope != null ? globalScope : shared_from_this();
		if (globalScope.get() != null)
		{
			GlobalScope = globalScope;
		}
		if (ModuleName == string::Empty && globalScope != null)
		{
			ModuleName = globalScope->ModuleName;
		}
		CurrentToken = null;
		Input = /*Console.In;*/inp != null ? inp : std::make_shared<TextReader>();
		Output = /*Console.Out*/outp != null ? outp : std::make_shared<TextWriter>();
	}

	bool LispScope::IsInClosureChain(const string & name, /*out*/ std::shared_ptr<LispScope> & closureScopeFound)
	{
		closureScopeFound = null;
		if (ClosureChain != null)
		{
			if (ClosureChain->ContainsKey(name))
			{
				closureScopeFound = ClosureChain;
				return true;
			}
			return ClosureChain->IsInClosureChain(name, closureScopeFound);
		}
		return false;
	}

	static void UpdateFunctionCache(LispVariant & elemAsVariant, std::shared_ptr<object> value, bool isFirst)
	{
		if (value->IsLispVariant())
		{
			std::shared_ptr<LispVariant> valueAsVariant = value->ToLispVariant();
			if (isFirst && valueAsVariant->IsFunction())
			{
				elemAsVariant.CachedFunction = valueAsVariant;
			}
		}
	}

	std::shared_ptr<object> LispScope::ResolveInScopes(std::shared_ptr<object> elem, bool isFirst)
	{
		std::shared_ptr<object> result;

		// try to access the cached function value (speed optimization)
		//if (elem->IsLispVariant())
		//{
		//	LispVariant & elemAsVariant = elem->ToLispVariantNotConstRef();
		//	if (elemAsVariant.CachedFunction.get() != 0)
		//	{
		//		return std::make_shared<object>(*(elemAsVariant.CachedFunction));
		//	}
		//}

		var name = elem->ToString();
		std::shared_ptr<LispScope> foundClosureScope;
		// first try to resolve in this scope
		auto item = find(name);
		if (item != end())
		{
			result = item->second;
//			UpdateFunctionCache(elem->ToLispVariantNotConstRef(), result, isFirst);
		}
		// then try to resolve in global scope
		else if (GlobalScope != null && GlobalScope->ContainsKey(name))
		{
			result = (*GlobalScope)[name];
//			UpdateFunctionCache(elem->ToLispVariantNotConstRef(), result, isFirst);
		}
		// then try to resolve in closure chain scope(s)
		else if (IsInClosureChain(name, foundClosureScope))
		{
			result = (*foundClosureScope)[name];
//			UpdateFunctionCache(elem->ToLispVariantNotConstRef(), result, isFirst);
		}
		// then try to resolve in scope of loaded modules
		else if (LispEnvironment::IsInModules(name, GlobalScope))
		{
			result = LispEnvironment::GetFunctionInModules(name, GlobalScope);
		}
		else
		{
			result = elem;
		}

		return result;
	}

	void LispScope::SetInScopes(const string & symbolName, std::shared_ptr<object> value)
	{
		std::shared_ptr<LispScope> foundClosureScope;
		if (!string::IsNullOrEmpty(symbolName) && ContainsKey(symbolName))
		{
			(*this)[symbolName] = value;
		}
		else if (!string::IsNullOrEmpty(symbolName) && IsInClosureChain(symbolName, foundClosureScope))
		{
			(*foundClosureScope)[symbolName] = value;
		}
		else if (!string::IsNullOrEmpty(symbolName) && GlobalScope != null && GlobalScope->ContainsKey(symbolName))
		{
			(*GlobalScope)[symbolName] = value;
		}
		else
		{
			throw LispException("Symbol " + symbolName + " not found", this);
		}
	}

	std::shared_ptr<LispToken> LispScope::GetPreviousToken(std::shared_ptr<LispToken> token)
	{
		std::shared_ptr<LispToken> previous = null;
		//if (Tokens)
		{
			//foreach (var item in Tokens)
			for (std::shared_ptr<LispToken> item : Tokens)
			{
				if ((token.get() != 0) && (*item == *token))
				{
					return previous;
				}
				previous = item;
			}
		}
		return null;
	}

	int LispScope::GetCallStackSize() const
	{
		std::shared_ptr<const LispScope> current = shared_from_this();
		int i = 0;
		do
		{
			current = current->Previous;
			i++;
		} while (current != null);
		return i;
	}

	void LispScope::DumpStack(int currentLevel)
	{
		string stackInfo = DumpStackToString(currentLevel);
		Output->WriteLine(stackInfo);
	}

	string LispScope::DumpStackToString(int currentLevel)
	{
		string ret = string::Empty;
		std::shared_ptr<LispScope> current = shared_from_this();
		int i = GetCallStackSize();
		do
		{
			string currentItem = currentLevel == i ? "-->" : "   ";

			ret = string::Format("{0,3}{1,5} name={2,-35} lineno={3,-4} module={4}\n", currentItem, std::to_string(i), current->Name, std::to_string(current->CurrentLineNo()), current->ModuleName) + ret;
			current = current->Previous;
			i--;
		} while (current != null);
		return ret;
	}

	/*private*/ inline string ExtractModuleName(string moduleName)
	{
		var temp = moduleName.StartsWith(LispEnvironment::EvalStrTag) ? moduleName.Substring(LispEnvironment::EvalStrTag.size(), moduleName.IndexOf(":", LispEnvironment::EvalStrTag.size()) - LispEnvironment::EvalStrTag.size()) : moduleName;
		return temp;
	}

	void LispScope::DumpFunctions()
	{
		Dump([](const LispVariant & v)-> bool { return v.IsFunction(); }, [](const LispVariant & v) -> string { return " : module=" + ExtractModuleName(v.FunctionValue().ModuleName); });

		ProcessMetaScope(LispEnvironment::Modules, [](KeyValuePair<string, std::shared_ptr<object>> modu) -> void
		{
			//var mod = module.Value as LispScope;
			if (modu.Value->IsLispScope())
			{
				var mod = modu.Value->GetLispScopeRef();
				mod->DumpFunctions();
			}
		});
	}

	void LispScope::DumpBuiltinFunctionsHelpHtmlFormated()
	{
		Output->WriteLine("<html>");
		Output->WriteLine("<head>");
		Output->WriteLine("<title>");
		Output->WriteLine("Documentation of fuel language");
		Output->WriteLine("</title>");
		Output->WriteLine("</head>");
		Output->WriteLine("<h2>Documentation of builtin functions of the fuel language:</h2>");
		Output->WriteLine("<body>");
		Dump([](const LispVariant & v) -> bool { return v.IsFunction() && v.FunctionValue().IsBuiltin(); }, null, /*showHelp:*/false, /*sort:*/ true, /*format:*/[](const LispVariant & v) -> string { return v.FunctionValue().GetHtmlFormatedDoc(); });
		Output->WriteLine("</body>");
		Output->WriteLine("</html>");
	}

	string LispScope::GetFunctionsHelpFormated(const string & functionName, /*Func<string, string, bool>*/std::function<bool(const string &, const string &)> select)
	{
		string result = string::Empty;
		//foreach (var key in Keys)
		for (const string & key : GetKeys())
		{
			if (select != null)
			{
				if (select(key, functionName))
				{
					const LispVariant & value = /*(LispVariant)*/(*this)[key]->ToLispVariantRef();
					result += value.FunctionValue().GetFormatedDoc();
				}
			}
			else if (key.StartsWith(functionName))
			{
				const LispVariant & value = /*(LispVariant)*/(*this)[key]->ToLispVariantRef();
				result += value.FunctionValue().GetFormatedDoc();
			}
		}
		return result;
	}

	void LispScope::ProcessMetaScope(const string & metaScope, /*Action<KeyValuePair<string, std::shared_ptr<object>>>*/std::function<void(KeyValuePair<string, std::shared_ptr<object>>)> action)
	{
		if (ContainsKey(metaScope))
		{
			var items = (*this)[metaScope]; // as LispScope;
			if (items->IsLispScope())
			{
				for (/*KeyValuePair<string, std::shared_ptr<object>>*/auto const & item : *(items->GetLispScopeRef()))
				{
					KeyValuePair<string, std::shared_ptr<object>> temp(item.first, item.second);
					action(temp);
				}
			}
		}
	}

	void LispScope::Dump(std::function<bool(const LispVariant &)>/*Func<LispVariant, bool>*/ select, std::function<string(const LispVariant &)>/*Func<LispVariant, string>*/ show, bool showHelp, bool sort, std::function<string(const LispVariant &)>/*Func<LispVariant, string>*/ format)
	{
		var keys = GetKeys();
		if (sort)
		{
			std::sort(keys.begin(), keys.end());
		}
		for (var key : keys)
		{
			if (!key.StartsWith(LispEnvironment::MetaTag))
			{
				const LispVariant & value = /*(LispVariant)*/(*this)[key]->ToLispVariantRef();
				if (select(value))
				{
					if (format != null)
					{
						Output->WriteLine("{0}", format(value));
					}
					else
					{
						string info = show != null ? show(value) : string::Empty;
						if (showHelp)
						{
							Output->WriteLine("{0,20} --> {1}", key, value.FunctionValue().Signature);
							if (!string::IsNullOrEmpty(info))
							{
								Output->WriteLine("{0,20}     {1}", "", info);
							}
						}
						else
						{
							Output->WriteLine("{0,20} --> {1,-40} : {2} {3}", key, value.ToStringDebugger(), value.TypeString(), info);
						}
					}
				}
			}
		}
	}
}
