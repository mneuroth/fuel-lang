#ifndef _CSTYPES_H
#define _CSTYPES_H

#include <list>
#include <vector>

#include "csstring.h"

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
// TODO --> implement
			return false;
		}
	};

	template <class T>
	class List : public IEnumerable<T>
	{
	public:
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
