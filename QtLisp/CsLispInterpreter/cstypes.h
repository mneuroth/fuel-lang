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

#include "csstring.h"
//#include "csobject.h"
//#include "Variant.h"

#define var auto

namespace CsLisp
{
	class LispToken;
	class LispScope;
	class LispVariant;
	class object;

	typedef std::function<void()> Action;
	typedef std::function<std::shared_ptr<LispVariant>(std::vector<std::shared_ptr<object>>, LispScope &)> FuncX;

	class LispException
	{
	public:
		LispException(const string & txt, LispScope * scope = 0)
		{
		}
		LispException(const string & txt, std::shared_ptr<LispToken> token, const string & moduleName = "", const string & stackInfo = "not available")
		{
		}

		void AddTokenInfos(std::shared_ptr<LispToken> token)
		{
		}
	};

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

		bool SequenceEqual(const IEnumerable & other) const
		{
            if (this->size() == other.size())
			{
				var iter2 = other.begin();
                for (var iter = this->begin(); iter != this->end(); ++iter)
				{
					if (*iter != *iter2)
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
		IEnumerable<K> GetKeys() const;

		bool ContainsKey(const K & key) const
		{
			return count(key) == 1;
		}
	};

	template <class T1, class T2>
	class Tuple : std::pair<T1, T2>
	{
	public:
	};

	class TextWriter
	{
	public:
		void WriteLine(const string & txt);
		void WriteLine(const string & txt, const string & txt1);
		void WriteLine(const string & txt, const string & txt1, const string & txt2);
		void WriteLine(const string & txt, const string & txt1, const string & txt2, const string & txt3);
		void WriteLine(const string & txt, const string & txt1, const string & txt2, const string & txt3, const string & txt4);
	};

	class TextReader
	{
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
		LispFunctionWrapper()
			: Signature("")
		{
		}

		/*public*/ /*Func<object[], LispScope, LispVariant>*/FuncX Function; // { get; private set; }

		/*public*/ string Signature; // { get; private set; }

		/*public*/ string Documentation; // { get; private set; }

		bool IsBuiltin() const;

		string ModuleName;

		/*public*/ string GetFormatedDoc() const;

		/*public*/ string GetHtmlFormatedDoc() const;

		bool IsSpecialForm() const
		{
// TODO --> only dummy impl !
			return false;
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
// TODO ggf. auf epsilon gleichheit pruefen
		return 0;
	}
}

#endif
