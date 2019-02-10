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

#include <vector>
#include <map>
#include <tuple>
#include <functional>
#include <memory>

#include "csstring.h"
#include "Exception.h"

#define ENABLE_COMPILE_TIME_MACROS

#define var auto

#define null 0

namespace CppLisp
{
	class LispToken;
	class LispScope;
	class LispVariant;
	class object;

	typedef std::function<void()> Action;
	typedef std::function<std::shared_ptr<LispVariant>(const std::vector<std::shared_ptr<object>> &, std::shared_ptr<LispScope>)> FuncX;

	// **********************************************************************
	// Wrapp stl vector class with methods to support IEnumerable interface of C#
	template <class T>
	class IEnumerable : public std::vector<T>
	{
	public:
		IEnumerable()
			: std::vector<T>()
		{
		}

		IEnumerable(size_t capacity)
			: std::vector<T>(capacity)
		{
		}

		inline size_t Count() const
		{
			return std::vector<T>::size();
		}

		inline const std::vector<T> & ToArray() const
		{
			return *this;
		}

		inline const T & First() const
		{
			return std::vector<T>::front();
		}

		inline const T & Last() const
		{
			return std::vector<T>::back();
		}

		inline const T ElementAt(size_t index) const
		{
			return (*this)[index];
		}

		inline void Add(const T & elem)
		{
			std::vector<T>::push_back(elem);
		}

		inline void AddRange(const IEnumerable<T> & other)
		{
            std::vector<T>::insert(this->end(), other.begin(), other.end());
		}

		inline void Insert(int index, const T & elem)
		{
			std::vector<T>::insert(this->begin() + index, elem);
		}

		IEnumerable<T> Skip(int skipNoOfElements)
		{
			IEnumerable<T> temp(*this);
			for (int i = 0; i < skipNoOfElements; i++)
			{
				temp.erase(temp.begin());
			}
			return temp;
		}

		void RemoveAt(int index)
		{
			typename IEnumerable<T>::iterator iter = std::vector<T>::begin();
			for (int i = 0; i < index; i++)
			{
				iter++;
			}
			if (iter != std::vector<T>::end())
			{
				std::vector<T>::erase(iter);
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

		// for debugging...
		string DumpList() const
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

	// **********************************************************************
	// Wrapp stl map class with methods to support Dictionary class of C#
	template <class K, class V>
	class Dictionary : public std::map<K,V>
	{
	public:
		inline IEnumerable<K> GetKeys() const
		{
			IEnumerable<K> keys;
			for (auto & kvp : *this)
			{
				keys.push_back(kvp.first);
			}
			return keys;
		}

		inline bool ContainsKey(const K & key) const
		{
			return std::map<K, V>::find(key) != std::map<K, V>::end();
		}

		inline bool ContainsValue(const V & value) const
		{
			auto iter = std::map<K, V>::begin();
			while (iter != std::map<K, V>::end())
			{
				if (*iter == value)
				{
					return true;
				}
				iter++;
			}
			return false;
		}

		inline bool Remove(const K & key)
		{
			return std::map<K, V>::erase(key) > 0;
		}

		inline void Clear()
		{
			std::map<K, V>::clear();
		}
	};

	// **********************************************************************
	template <class K, class V>
	class KeyValuePair
	{
	public:
		inline KeyValuePair(K key, V value)
			: Key(key), Value(value)
		{
		}

		K Key;
		V Value;
	};

	// **********************************************************************
	template <class T1, class T2>
	class Tuple : std::pair<T1, T2>
	{
	public:
		inline Tuple(const T1 & val1, const T2 & val2)
			: std::pair<T1, T2>(val1, val2)
		{
		}
		inline T1 Item1() const
		{
			return std::pair<T1, T2>::first;
		}
		inline T2 Item2() const
		{
			return std::pair<T1, T2>::second;
		}
	};

	// **********************************************************************
	template <class T1, class T2, class T3>
	class Tuple3 : public std::tuple<T1, T2, T3>
	{
	public:
		inline Tuple3(const T1 & val1, const T2 & val2, const T3 & val3)
			: std::tuple<T1, T2, T3>(val1, val2, val3)
		{
		}
		inline T1 Item1() const
		{
			return std::get<0>(*this);
		}
		inline T2 Item2() const
		{
			return std::get<1>(*this);
		}
		inline T3 Item3() const
		{
			return std::get<2>(*this);
		}
	};

	// **********************************************************************
	// Implement C++ version of TextWriter class of C#
	class TextWriter
	{
	private:
		bool	m_bToString;
		string	m_sText;

	public:
		TextWriter(bool bToString = false);
		
		inline void EnableToString(bool value = true)
		{
			m_bToString = value;
		}
		inline string GetContent() const
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
		void Flush();
	};

	// **********************************************************************
	// Implement C++ version of TextWriter class of C#
	class TextReader
	{
	private:
		bool								m_bFromString;
		string								m_sText;
		std::vector<string>					m_aAllLines;
		std::vector<string>::const_iterator m_aCurrentPos;

	public:
		TextReader(const string & txt = string::Empty);

		inline void EnableFromString(bool value = true)
		{
			m_bFromString = value;
		}

		void SetContent(const string & txt);
		string ReadLine();
	};

	// **********************************************************************
	struct LispFunctionWrapper
	{
	private:
		bool m_bIsSpecialForm;
		bool m_bIsEvalInExpand;
		bool m_bIsBuiltin;

	public:
		inline LispFunctionWrapper()
			: m_bIsSpecialForm(false), 
			  m_bIsEvalInExpand(false),
			  m_bIsBuiltin(false)
		{
		}

		/*public*/ /*Func<object[], LispScope, LispVariant>*/FuncX Function; // { get; private set; }

		/*public*/ string Signature; // { get; private set; }

		/*public*/ string Documentation; // { get; private set; }

		inline bool IsBuiltin() const
		{
			return m_bIsBuiltin;
		}
		inline void SetBuiltin(bool val)
		{
			m_bIsBuiltin = val;
		}

		string ModuleName;

		/*public*/ string GetFormatedDoc() const;
		/*public*/ string GetHtmlFormatedDoc() const;

		inline bool IsSpecialForm() const
		{
			return m_bIsSpecialForm;
		}
		inline void SetSpecialForm(bool value)
		{
			m_bIsSpecialForm = value;
		}
		inline bool IsEvalInExpand() const
		{
			return m_bIsEvalInExpand;
		}
		inline void SetEvalInExpand(bool value)
		{
			m_bIsEvalInExpand = value;
		}

	private:
		string GetFormatedHelpString(const string & separator, const string & splitter, std::function<string(const string &)> nameDecorator = null, std::function<string(const string &)> syntaxDecorator = null) const;
	};

	// **********************************************************************
	template <class T>
	inline int CompareToType(T d1, T d2)
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
