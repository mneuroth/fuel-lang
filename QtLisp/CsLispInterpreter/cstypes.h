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

#include "csstring.h"

typedef std::function<void()> Action;

#define var auto

namespace CsLisp
{
	class LispToken;

	class LispException
	{
	public:
		LispException(const string & txt)
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
			if (size() == other.size())
			{
				var iter2 = other.begin();
				for (var iter = begin(); iter != end(); ++iter)
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
			std::list<T>::insert(end(), other.begin(), other.end());
		}
	};

	template <class T>
	class List : public IEnumerable<T>
	{
	public:
	};

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
