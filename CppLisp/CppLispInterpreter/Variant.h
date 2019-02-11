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

#ifndef _LISP_VARIANT_H
#define _LISP_VARIANT_H

 //using System;
//using System.Collections;
//using System.Collections.Generic;
//using System.Globalization;
//using System.Linq;

#include "csstring.h"
#include "cstypes.h"
#include "csobject.h"
#include "Token.h"
#include "Tokenizer.h"

#include <memory>
#include <math.h>
#include <cstdlib>

namespace CppLisp
{
    /// <summary>
    /// Enumeration for unqoute modus
    /// </summary>
    /*public*/ enum LispUnQuoteModus
    {
        _None = 0,
        _UnQuote = 1,
        _UnQuoteSplicing = 2
    };

	inline LispType ConvertObjectTypeToVariantType(ObjectType objType)
	{
		switch (objType)
		{
			case ObjectType::__Undefined:
				return LispType::_Undefined;
			case ObjectType::__Nil:
				return LispType::_Nil;
			case ObjectType::__Bool:
				return LispType::_Bool;
			case ObjectType::__Int:
				return LispType::_Int;
			case ObjectType::__Double:
				return LispType::_Double;
			case ObjectType::__String:
				return LispType::_String;
			case ObjectType::__List:
				return LispType::_List;
			case ObjectType::__Function:
				return LispType::_Function;
			case ObjectType::__Symbol:
				return LispType::_Symbol;
			case ObjectType::__NativeObject:
				return LispType::_NativeObject;
					//__Array = 10,
			case ObjectType::__LispVariant:
				return LispType::_Undefined;
			case ObjectType::__LispFunctionWrapper:
				return LispType::_Function;
			case ObjectType::__LispToken:
				return LispType::_String;
			case ObjectType::__IEnumerableOfObject:
				return LispType::_List;
			case ObjectType::__VoidPtr:
				return LispType::_Undefined;
			case ObjectType::__LispScope:
				return LispType::_Undefined;
			case ObjectType::__LispMacroRuntimeEvaluate:
				return LispType::_Undefined;
			case ObjectType::__Error:
				return LispType::_Error;
			default:
				return LispType::_Undefined;
		}
	}

    /// <summary>
    /// Generic data container for lisp data types.
    /// </summary>
    /*public*/ class LispVariant
    {
        //#region constants

        /*private*/ const static string CanNotConvertTo;
        /*private*/ const static string NoOperatorForTypes;

        //#endregion

        //#region properties

    private:
		// disable assignment operator
		LispVariant & operator=(const LispVariant & other);
		
		/*private*/ static double Tolerance; //{ get; set; }

    public:
        /*public*/ LispUnQuoteModus IsUnQuoted; //{ get; private set; }

        /*public*/ std::shared_ptr<object> Value; //{ get; set; }

        /*public*/ LispType Type; //{ get; set; }

		/*public*/ string TypeString() const;

        /*public*/ std::shared_ptr<LispToken> Token; // { get; private set; }

        /*public*/ inline bool IsNil() const
        {
            /*get {*/ return Type == LispType::_Nil; //}
        }

        /*public*/ inline bool IsError() const
        {
            /*get {*/ return Type == LispType::_Error; //}
        }

        /*public*/ inline bool IsUndefined() const
        {
            /*get {*/ return Type == LispType::_Undefined; //}
        }

        /*public*/ inline bool IsString() const
        {
            /*get {*/ return Type == LispType::_String; //}
        }

        /*public*/ inline bool IsDouble() const
        {
            /*get {*/ return Type == LispType::_Double; //}
        }

        /*public*/ inline bool IsInt() const
        {
            /*get {*/ return Type == LispType::_Int; //}
        }

        /*public*/ inline bool IsNumber() const
        {
            /*get {*/ return IsInt() || IsDouble(); //}
        }

        /*public*/ inline bool IsBool() const
        {
            /*get {*/ return Type == LispType::_Bool; //}
        }

        /*public*/ inline bool IsList() const
        {
            /*get {*/ return Type == LispType::_List || Type == LispType::_Nil; //}
        }

        /*public*/ inline bool IsFunction() const
        {
            /*get {*/ return Type == LispType::_Function; //}
        }

        /*public*/ inline bool IsSymbol() const
        {
            /*get {*/ return Type == LispType::_Symbol; //}
        }

        /*public*/ inline bool IsNativeObject() const
        {
            /*get {*/ return Type == LispType::_NativeObject; //}
        }

		/*public*/ inline bool IsLValue() const
		{
			/*get {*/ return Type == LispType::_LValue; //}
		}


        //#endregion

        //#region Constructor

        /// <summary>
        /// Initializes the static elements of the <see cref="LispVariant"/> class.
        /// </summary>
        //static LispVariant()
        //{
        //    Tolerance = 1e-8;
        //}

        /// <summary>
        /// Initializes a new instance of the <see cref="LispVariant"/> class.
        /// </summary>
        /// <param name="val">The value.</param>
        /// <remarks>Needed for compiler module and .NET 3.5</remarks>
		/*public*/ explicit LispVariant(std::shared_ptr<object> val);

        /// <summary>
        /// Initializes a new instance of the <see cref="LispVariant"/> class.
        /// </summary>
        /// <param name="type">The type.</param>
        /// <param name="value">The value.</param>
        /// <param name="unQuoted">The unquoted modus.</param>
		/*public*/ explicit LispVariant(LispType type = LispType::_Undefined, std::shared_ptr<object> value = null, LispUnQuoteModus unQuoted = LispUnQuoteModus::_None);

        /// <summary>
        /// Initializes a new instance of the <see cref="LispVariant"/> class.
        /// </summary>
        /// <param name="val">The value.</param>
        /// <param name="unQuoted">The unquoted modus.</param>
		/*public*/ explicit LispVariant(std::shared_ptr<object> val, LispUnQuoteModus unQuoted /*= LispUnQuoteModus::_None*/);

		explicit LispVariant(std::function<void(std::shared_ptr<object>)> action);

		LispVariant(const LispVariant & other);

	//protected:
        /// <summary>
        /// Initializes a new instance of the <see cref="LispVariant"/> class.
        /// </summary>
        /// <param name="token">The token.</param>
        /// <param name="unQuoted">The unquoted modus.</param>
		/*internal*/ explicit LispVariant(std::shared_ptr<LispToken> token, LispUnQuoteModus unQuoted = LispUnQuoteModus::_None);
        
    public:
        /// <summary>
        /// Creates a new value representing an error.
        /// </summary>
        /// <param name="errorMessage">The error message.</param>
        /// <returns>The value</returns>
		/*public*/ static std::shared_ptr<LispVariant> CreateErrorValue(const string & errorMessage);

        //#endregion

        //#region IComparable

        /// <summary>
        /// Compares to other object.
        /// </summary>
        /// <param name="other">The other.</param>
        /// <returns></returns>
        /*public*/ int CompareTo(std::shared_ptr<object> other);

		int CompareTo(std::shared_ptr<LispVariant> other);

        //#endregion

        //#region Casts

        /*public*/ const LispFunctionWrapper & FunctionValue() const;

        /*public*/ std::shared_ptr<IEnumerable<std::shared_ptr<object>>> ListValue() const;

		/*public*/ const IEnumerable<std::shared_ptr<object>> & ListValueRef() const;
		/*public*/ IEnumerable<std::shared_ptr<object>> & ListValueNotConstRef();

        /*public*/ double DoubleValue() const;

        /*public*/ int IntValue() const;

        /*public*/ bool BoolValue() const;

        /*public*/ std::shared_ptr<object> NativeObjectValue() const;

        /*public*/ string NativeObjectStringRepresentation() const;

		/*public*/ bool ToBool() const;

		/*public*/ int ToInt() const;
   
		/*public*/ double ToDouble() const;

		/*public*/ string StringValue() const;

		/*public*/ std::function<void(std::shared_ptr<object>)> ToSetterAction() const;

        /// <summary>
        /// Comverts this value into a string representation used by the compiler module.
        /// </summary>
        /// <returns>The string representation</returns>
		/*public*/ string ToStringCompiler() const;

        /// <summary>
        /// Comverts this value into a string representation used by the debugger module.
        /// </summary>
        /// <returns>The string representation</returns>
		/*public*/ string ToStringDebugger() const;

		/*public override*/ string ToString() const;
        
		/*private*/ static string ExpandContainerToString(std::shared_ptr<object> maybeContainer);

		/*private*/ static string ExpandItemForContainer(std::shared_ptr<object> item);

        //#endregion

		/*public*/ bool SymbolCompare(std::shared_ptr<object> other) const;

        //#region overloaded methods

        /// <summary>
        /// Determines whether the specified <see cref="T:System.Object"/> is equal to the current <see cref="T:System.Object"/>.
        /// </summary>
        /// <returns>
        /// true if the specified <see cref="T:System.Object"/> is equal to the current <see cref="T:System.Object"/>; otherwise, false.
        /// </returns>
        /// <param name="other">The object to compare with the current object. </param><filterpriority>2</filterpriority>
		/*public override*/ bool Equals(std::shared_ptr<object>  other) const;

        //#endregion

        //#region Operations

		/*public*/ void Add(std::shared_ptr<object>  value);

		bool operator==(const LispVariant & other) const;
		bool operator!=(const LispVariant & other) const;

		/*public static*/ LispVariant operator+(const LispVariant & r);
		/*public static*/ LispVariant operator -(const LispVariant & r);
		/*public static*/ LispVariant operator *(const LispVariant & r);
		/*public static*/ LispVariant operator /(const LispVariant & r);
		/*public static*/ LispVariant operator %(const LispVariant & r);
		/*public static*/ bool operator <(const LispVariant & r) const;
		/*public static*/ inline bool operator >(const LispVariant & r) const
        {
            return r < *this;
        }
		/*public static*/ bool operator <=(const LispVariant & r) const;
        /*public static*/ inline bool operator >=(const LispVariant & r) const
        {
            return r <= *this;
        }

		/*public*/ static bool EqualOp(const LispVariant & l, const LispVariant & r);

        //#endregion

    private:
        //#region private methods

		/*private*/ static LispType TypeOf(std::shared_ptr<object> obj);

		/*private*/ LispException CreateInvalidCastException(const string & name, const string & msg = "no") const;
	    /*private*/ static LispException CreateInvalidOperationException(const string & operation, const LispVariant & l, const LispVariant & r);

        //#endregion
    };
}

#endif
