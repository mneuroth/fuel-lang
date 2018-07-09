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

#ifndef _CSTYPES_H
#define _CSTYPES_H

#include <list>
#include <vector>
#include <map>
#include <tuple>

#include "csstring.h"
#include "Exception.h"

#define ENABLE_COMPILE_TIME_MACROS

#define var auto

#define null 0

namespace CsLisp
{
	class LispToken;
	class LispScope;
	class LispVariant;
	class object;

	typedef std::function<void()> Action;
	typedef std::function<std::shared_ptr<LispVariant>(std::vector<std::shared_ptr<object>>, std::shared_ptr<LispScope>)> FuncX;

// TODO --> pruefen ob man IEnumerable ueberhaupt braucht oder nicht direct list verwendet --> Komplexitaet reduzieren
	template <class T>
	class IEnumerable : public std::list<T>
	{
	public:
		size_t Count() const
		{
			return std::list<T>::size();
		}

		std::vector<T> ToArray() const
		{
			return std::vector<T>(std::list<T>::begin(), std::list<T>::end());
		}

		std::list<T> ToList() const
		{
			return std::list<T>(std::list<T>::begin(), std::list<T>::end());
		}

		const T & First() const
		{
			return std::list<T>::front();
		}

		const T & Last() const
		{
			return std::list<T>::back();
		}

		const T ElementAt(int index) const
		{
            std::vector<T> aCopy(std::list<T>::begin(), std::list<T>::end());
			return aCopy[index];
		}

		IEnumerable<T> Skip(int skipNoOfElements)
		{
			IEnumerable<T> temp(*this);
			for (int i = 0; i < skipNoOfElements; i++)
			{
				temp.pop_front();
			}
			return temp;
		}

		void RemoveAt(int index)
		{
            typename IEnumerable<T>::iterator iter = std::list<T>::begin();
			for (int i = 0; i < index; i++)
			{
				iter++;
			}
            if (iter != std::list<T>::end())
			{
                std::list<T>::erase(iter);
			}
		}

		bool SequenceEqual(const IEnumerable & other) const
		{
            if (this->size() == other.size())
			{
				var iter2 = other.begin();
                for (var iter = this->begin(); iter != this->end(); ++iter)
				{
					const T & val1 = *iter;
					const T & val2 = *iter2;
					if (*val1 != *val2)
					{
						return false;
					}
					++iter2;
				}
			}
			else
			{
				return false;
			}
			return true;
		}

		void Add(const T & elem)
		{
			std::list<T>::push_back(elem);
		}

		void AddRange(const IEnumerable<T> & other)
		{
            std::list<T>::insert(this->end(), other.begin(), other.end());
		}

		// for debugging...
		string DumpList()
		{
			string ret = "(";
			for (var elem : *this)
			{
				ret += elem->ToString();
				ret += ",";
			}
			return ret + ")";
		}
	};

	//template <class T>
	//class IList : public IEnumerable<T>
	//{
	//public:
	//};

	//template <class T>
	//class List : public IEnumerable<T>
	//{
	//public:
	//};

	template <class K, class V>
	class Dictionary : public std::map<K,V>
	{
	public:
		IEnumerable<K> GetKeys() const
		{
			IEnumerable<K> keys;
			for (auto & kvp : *this)
			{
				keys.push_back(kvp.first);
			}
			return keys;
		}

		bool ContainsKey(const K & key) const
		{
            return std::map<K,V>::count(key) == 1;
		}
	};

	template <class T1, class T2>
	class Tuple : std::pair<T1, T2>
	{
	public:
		Tuple(const T1 & val1, const T2 & val2)
			: std::pair<T1, T2>(val1, val2)
		{
		}
		T1 Item1() const
		{
			return std::pair<T1, T2>::first;
		}
		T2 Item2() const
		{
			return std::pair<T1, T2>::second;
		}
	};

	template <class T1, class T2, class T3>
	class Tuple3 : public std::tuple<T1, T2, T3>
	{
	public:
		Tuple3(const T1 & val1, const T2 & val2, const T3 & val3)
			: std::tuple<T1, T2, T3>(val1, val2, val3)
		{
		}
		T1 Item1() const
		{
			return std::get<0>(*this);
		}
		T2 Item2() const
		{
			return std::get<1>(*this);
		}
		T3 Item3() const
		{
			return std::get<2>(*this);
		}
	};

	class TextWriter
	{
	private:
		bool	m_bToString;
		string	m_sText;

	public:
		TextWriter(bool bToString = false)
			: m_bToString(bToString)
		{
		}
		void EnableToString(bool value = true)
		{
			m_bToString = value;
		}
		string GetContent() const
		{
			return m_sText;
		}
		void Write(const string & txt);
		void WriteLine();
		void WriteLine(const string & txt);
		void WriteLine(const string & txt, const string & txt1);
		void WriteLine(const string & txt, const string & txt1, const string & txt2);
		void WriteLine(const string & txt, const string & txt1, const string & txt2, const string & txt3);
		void WriteLine(const string & txt, const string & txt1, const string & txt2, const string & txt3, const string & txt4);
	};

	class TextReader
	{
	private:
		bool								m_bFromString;
		string								m_sText;
		std::vector<string>					m_aAllLines;
		std::vector<string>::const_iterator m_aCurrentPos;

	public:
		TextReader(const string & txt = string::Empty)
		{
			m_bFromString = !string::IsNullOrEmpty(txt);
			SetContent(txt);
		}
		void EnableFromString(bool value = true)
		{
			m_bFromString = value;
		}
		void SetContent(const string & txt)
		{
			m_sText = txt;
			m_aAllLines = txt.Split("\n");
			m_aCurrentPos = m_aAllLines.begin();
		}
		string ReadLine();
	};

	template <class K, class V>
	class KeyValuePair
	{
	public:
		KeyValuePair(K key, V value)
			: Key(key), Value(value)
		{
		}

		K Key;
		V Value;
	};

	struct LispFunctionWrapper
	{
	private:
		bool m_bIsSpecialForm;
		bool m_bIsEvalInExpand;
		bool m_bIsBuiltin;

	public:
		LispFunctionWrapper()
			: Signature(""), 
			  m_bIsSpecialForm(false), 
			  m_bIsEvalInExpand(false),
			  m_bIsBuiltin(false)
		{
		}

		/*public*/ /*Func<object[], LispScope, LispVariant>*/FuncX Function; // { get; private set; }

		/*public*/ string Signature; // { get; private set; }

		/*public*/ string Documentation; // { get; private set; }

		bool IsBuiltin() const
		{
			return m_bIsBuiltin;
		}
		void SetBuiltin(bool val)
		{
			m_bIsBuiltin = val;
		}

		string ModuleName;

		/*public*/ string GetFormatedDoc() const
		{
			const string separator = "\n\n";
			const string splitter = "-------------------------------------------------" + separator;
			return GetFormatedHelpString(separator, splitter);
		}

		/*public*/ string GetHtmlFormatedDoc() const
		{
			const string separator = "<br><br>";
			const string splitter = string("<hr>") + string("<br>");
			return GetFormatedHelpString(separator, splitter, [](const string & s) -> string { return "<b>" + s + "</b>"; }, [](const string & s) -> string { return "<code>" + s + "</code>"; });
		}

		bool IsSpecialForm() const
		{
			return m_bIsSpecialForm;
		}
		void SetSpecialForm(bool value)
		{
			m_bIsSpecialForm = value;
		}
		bool IsEvalInExpand() const
		{
			return m_bIsEvalInExpand;
		}
		void SetEvalInExpand(bool value)
		{
			m_bIsEvalInExpand = value;
		}

	private:
		string GetFormatedHelpString(const string & separator, const string & splitter, std::function<string(const string &)> nameDecorator = null, std::function<string(const string &)> syntaxDecorator = null) const 
		{
			if (nameDecorator == null)
			{
				nameDecorator = [](const string & s) -> string { return s; };
			}
			if (syntaxDecorator == null)
			{
				syntaxDecorator = [](const string & s) -> string { return s; };
			}
			string name = "???";
			string signature = (!string::IsNullOrEmpty(Signature) ? Signature : string::Empty);
			if (signature.size() > 0 && signature.StartsWith("("))
			{
				size_t len = signature.IndexOf(" ", "StringComparison.Ordinal");
				// process commands like: (doc)
				if (len < 0)
				{
					len = signature.IndexOf(")", "StringComparison.Ordinal") - 1;
				}
				name = nameDecorator(signature.Substring(1, len));
			}
			name += IsSpecialForm() ? " [special form]" : string::Empty;
			name += separator;
			string syntax = syntaxDecorator("Syntax: " + signature) + separator;
			string doc = (!string::IsNullOrEmpty(Documentation) ? Documentation : "<not available>");
			doc += separator;
			return splitter + name + syntax + doc + "\n";
		}
	};

	//class Exception
	//{
	//};

	template <class T>
	int CompareToType(T d1, T d2)
	{
		if (d1 < d2)
		{
			return -1;
		}
		else if (d1 > d2)
		{
			return 1;
		}
		return 0;
	}
}

#endif
