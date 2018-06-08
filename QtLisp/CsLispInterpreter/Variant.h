#ifndef _LISP_VARIANT_H
#define _LISP_VARIANT_H

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

namespace CsLisp
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

    /// <summary>
    /// Generic data container for lisp data types.
    /// </summary>
    /*public*/ class LispVariant //: public object // TODO: IComparable
    {
        //#region constants

        /*private*/ const static string CanNotConvertTo;
        /*private*/ const static string NoOperatorForTypes;

        //#endregion

        //#region properties

    private:
        /*private*/ static double Tolerance; //{ get; set; }

    public:
        /*public*/ LispUnQuoteModus IsUnQuoted; //{ get; private set; }

        /*public*/ std::shared_ptr<object> Value; //{ get; set; }

        /*public*/ LispType Type; //{ get; set; }

        /*public*/ string TypeString()
        {
            //get
            //{
				switch (Type)
				{
					case _Undefined:
						return "Undefined";
					case _Nil:
						return "Nil";
					case _Bool:
						return "Bool";
					case _Int:
						return "Int";
					case _Double:
						return "Double";
					case _String:
						return "String";
					case _List:
						return "List";
					case _Function:
						return "Function";
					case _Symbol:
						return "Symbol";
					case _NativeObject:
						return /*Type+*/string("NativeObject<") + Value->GetTypeName() + string(">");
					//_Array = 10,
					case _Error:
						return "Error";
					default:
						return "unknown_type";
				}
            //}
        }

        /*public*/ std::shared_ptr<LispToken> Token; // { get; private set; }

        /*public*/ bool IsNil() const
        {
            /*get {*/ return Type == LispType::_Nil; //}
        }

        /*public*/ bool IsError() const
        {
            /*get {*/ return Type == LispType::_Error; //}
        }

        /*public*/ bool IsUndefined() const
        {
            /*get {*/ return Type == LispType::_Undefined; //}
        }

        /*public*/ bool IsString() const
        {
            /*get {*/ return Type == LispType::_String; //}
        }

        /*public*/ bool IsDouble() const
        {
            /*get {*/ return Type == LispType::_Double; //}
        }

        /*public*/ bool IsInt() const
        {
            /*get {*/ return Type == LispType::_Int; //}
        }

        /*public*/ bool IsNumber() const
        {
            /*get {*/ return IsInt() || IsDouble(); //}
        }

        /*public*/ bool IsBool() const
        {
            /*get {*/ return Type == LispType::_Bool; //}
        }

        /*public*/ bool IsList() const
        {
            /*get {*/ return Type == LispType::_List || Type == LispType::_Nil; //}
        }

        /*public*/ bool IsFunction() const
        {
            /*get {*/ return Type == LispType::_Function; //}
        }

        /*public*/ bool IsSymbol() const
        {
            /*get {*/ return Type == LispType::_Symbol; //}
        }

        /*public*/ bool IsNativeObject() const
        {
            /*get {*/ return Type == LispType::_NativeObject; //}
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
        /*public*/ explicit LispVariant(std::shared_ptr<object> val)
            : LispVariant(val, LispUnQuoteModus::_None)
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="LispVariant"/> class.
        /// </summary>
        /// <param name="type">The type.</param>
        /// <param name="value">The value.</param>
        /// <remarks>Needed for compiler module and .NET 3.5</remarks>
//        /*public*/ explicit LispVariant(LispType type, std::shared_ptr<object> value)
//            : LispVariant(type, value, LispUnQuoteModus::_None)
//        {
//        }

        /// <summary>
        /// Initializes a new instance of the <see cref="LispVariant"/> class.
        /// </summary>
        /// <param name="type">The type.</param>
        /// <param name="value">The value.</param>
        /// <param name="unQuoted">The unquoted modus.</param>
        /*public*/ explicit LispVariant(LispType type /*= LispType::_Undefined*/, std::shared_ptr<object> value = null, LispUnQuoteModus unQuoted = LispUnQuoteModus::_None)
        {
            Type = type;
            Value = value;
            IsUnQuoted = unQuoted;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="LispVariant"/> class.
        /// </summary>
        /// <param name="val">The value.</param>
        /// <param name="unQuoted">The unquoted modus.</param>
        /*public*/ explicit LispVariant(std::shared_ptr<object> val, LispUnQuoteModus unQuoted /*= LispUnQuoteModus::_None*/)
            : LispVariant(TypeOf(val), val, unQuoted)
        {
            //std::shared_ptr<LispVariant> value = std::dynamic_pointer_cast<LispVariant>(val); //val as LispVariant;
			//if (value /*!= null*/)
			if (val->IsLispVariant())
            { 
				std::shared_ptr<LispVariant> value = val->ToLispVariant();
                Type = value->Type;
                Value = value->Value;
                IsUnQuoted = value->IsUnQuoted;
            }
        }

		LispVariant(const LispVariant & other)
		{
			Type = other.Type;
			Value = other.Value;
			IsUnQuoted = other.IsUnQuoted;
		}

	//protected:
        /// <summary>
        /// Initializes a new instance of the <see cref="LispVariant"/> class.
        /// </summary>
        /// <param name="token">The token.</param>
        /// <param name="unQuoted">The unquoted modus.</param>
        /*internal*/ explicit LispVariant(std::shared_ptr<LispToken> token, LispUnQuoteModus unQuoted = LispUnQuoteModus::_None)
            : LispVariant(TypeOf(token->Value), token->Value, unQuoted)
        {
            std::shared_ptr<LispToken> Token = token;
            if (token->Type == LispTokenType::Nil)
            {
                Type = LispType::_Nil;
            }
            if (token->Type == LispTokenType::Symbol)
            {
                Type = LispType::_Symbol;
            }
        }
        
    public:
        /// <summary>
        /// Creates a new value representing an error.
        /// </summary>
        /// <param name="errorMessage">The error message.</param>
        /// <returns>The value</returns>
        /*public*/ static std::shared_ptr<LispVariant> CreateErrorValue(string errorMessage)
        {
            std::shared_ptr<object> msg = std::make_shared<object>(object(errorMessage));
            return std::make_shared<LispVariant>(LispVariant(LispType::_Error, msg));
        }

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

        /*public*/ LispFunctionWrapper FunctionValue() const;

        /*public*/ std::shared_ptr<IEnumerable<std::shared_ptr<object>>> ListValue() const;
 
        /*public*/ double DoubleValue() const;

        /*public*/ int IntValue() const;

        /*public*/ bool BoolValue() const;

        /*public*/ std::shared_ptr<object> NativeObjectValue() const;

        /*public*/ string NativeObjectStringRepresentation() const;

        /*public*/ bool ToBool() const
        {
            if (IsBool())
            {
                return BoolValue();
            }
            if (IsInt())
            {
                return IntValue() != 0;
            }
            if (IsDouble())
            {
                return /*Math.A*/fabs(DoubleValue()) > Tolerance;
            }
            throw CreateInvalidCastException("bool", CanNotConvertTo);
        }

        /*public*/ int ToInt() const
        {
            if (IsBool())
            {
                return BoolValue() ? 1 : 0;
            }
            if (IsInt())
            {
                return IntValue();
            }
            if (IsDouble())
            {
                return (int) DoubleValue();
            }
            if (IsString())
            {
                return /*Convert.ToInt32*/atoi(StringValue().c_str());
            }
            throw CreateInvalidCastException("int", CanNotConvertTo);
        }

        /*public*/ double ToDouble() const
        {
            if (IsBool())
            {
                return BoolValue() ? 1.0 : 0.0;
            }
            if (IsInt())
            {
                return IntValue();
            }
            if (IsDouble())
            {
                return DoubleValue();
            }
            if (IsString())
            {
                return atof(StringValue().c_str()); //Convert.ToDouble(StringValue(), CultureInfo.InvariantCulture);
            }
            throw CreateInvalidCastException("double", CanNotConvertTo);
        }

        /*public*/ string StringValue() const
        {
            //get
            //{
                return Value->ToString();
            //}
        }

        /// <summary>
        /// Comverts this value into a string representation used by the compiler module.
        /// </summary>
        /// <returns>The string representation</returns>
        /*public*/ string ToStringCompiler() const
        {
            if (IsBool())
            {
                return BoolValue() ? "true" : "false";
            }
            if (IsDouble())
            {
                return std::to_string(DoubleValue()); //.ToString("F", CultureInfo.InvariantCulture);
            }
            if (IsString())
            {
                return "\"" + StringValue() + "\"";
            }
            return ToString();
        }

        /// <summary>
        /// Comverts this value into a string representation used by the debugger module.
        /// </summary>
        /// <returns>The string representation</returns>
        /*public*/ string ToStringDebugger() const
        {
            if (IsString())
            {
                return "\"" + StringValue() + "\"";
            }
            return ToString();
        }

        /*public override*/ string ToString() const
        {
            if (IsString())
            {
                return StringValue();
            }
            if (IsInt())
            {
                return std::to_string(IntValue()); //.ToString(CultureInfo.InvariantCulture);
            }
            if (IsDouble())
            {
                return std::to_string(DoubleValue()); //.ToString(CultureInfo.InvariantCulture);
            }
            if (IsBool())
            {
                return BoolValue() ? "#t" : "#f";
            }
            if (IsNil())
            {
                return LispToken::NilConst;
            }
            if (IsList())
            {
                return ExpandContainerToString(std::make_shared<object>(*(ListValue())));
            }
            if (IsFunction())
            {
                return "function " + /*(FunctionValue().Signature != null ?*/ FunctionValue().Signature /*: "<unknown>")*/;
            }
            if (IsSymbol())
            {
                return Value->ToString();
            }
            if (IsNativeObject())
            {
                return NativeObjectStringRepresentation();
            }
            if (IsUndefined())
            {
                return "<undefined>";
            }
            if (IsError())
            {
                return "Error: " + Value->ToString();
            }
            return "?";
        }
        
        //template <T1, T2, T3>
        //typedef (*Func<T1,T2,T3>)();

// TODO Func Operatoren realisieren ?
//        // used for compiler module
//        /*public*/ static explicit operator Func<object[], LispScope, LispVariant>(LispVariant variant)
//        {
//            return variant.FunctionValue.Function;
//        }
//
//        // used for compiler module
//        /*public*/ static explicit operator Func<LispVariant, LispScope, LispVariant>(LispVariant variant)
//        {
//            return (arg1, scope) => variant.FunctionValue.Function(new object[] { arg1 }, scope);
//        }
//
//        // used for compiler module
//        /*public*/ static explicit operator Func<LispVariant, LispVariant, LispScope, LispVariant>(LispVariant variant)
//        {
//            return (arg1, arg2, scope) => variant.FunctionValue.Function(new object[] { arg1, arg2 }, scope);
//        }
//
//        // used for compiler module
//        /*public*/ static explicit operator Func<LispVariant, LispVariant, LispVariant, LispScope, LispVariant>(LispVariant variant)
//        {
//            return (arg1, arg2, arg3, scope) => variant.FunctionValue.Function(new object[] { arg1, arg2, arg3 }, scope);
//        }

		/*private*/ static string ExpandContainerToString(std::shared_ptr<object> maybeContainer);

        /*private*/ static string ExpandItemForContainer(std::shared_ptr<object> item)
        {
            if (item->IsLispVariant())
            {
				std::shared_ptr<LispVariant> variant = item->ToLispVariant();
                if (variant->IsString())
                {
                    return variant->ToStringDebugger();
                }
            }
            return item->ToString();
        }

        //#endregion

        /*public*/ bool SymbolCompare(std::shared_ptr<object> other)
        {
            if (other->IsLispVariant())
            {
				std::shared_ptr<LispVariant> otherVariant = other->ToLispVariant();
				return Value->Equals(*(otherVariant->Value));
            }
            return false;
        }

        //#region overloaded methods

        /// <summary>
        /// Determines whether the specified <see cref="T:System.Object"/> is equal to the current <see cref="T:System.Object"/>.
        /// </summary>
        /// <returns>
        /// true if the specified <see cref="T:System.Object"/> is equal to the current <see cref="T:System.Object"/>; otherwise, false.
        /// </returns>
        /// <param name="other">The object to compare with the current object. </param><filterpriority>2</filterpriority>
        /*public override*/ bool Equals(std::shared_ptr<object>  other)
        {
            if (other->IsLispVariant())
            {
				std::shared_ptr<LispVariant> otherVariant = other->ToLispVariant();
				return EqualOp(*this, *otherVariant);
            }
            return false;
        }

        /// <summary>
        /// Returns a hash code for this instance.
        /// </summary>
        /// <returns>
        /// A hash code for this instance, suitable for use in hashing algorithms and data structures like a hash table. 
        /// </returns>
        ///*public override*/ int GetHashCode()
        //{
        //    // ReSharper disable once BaseObjectGetHashCodeCallInGetHashCode
        //    return base.GetHashCode();
        //}

        //#endregion

        //#region Operations

        /*public*/ void Add(std::shared_ptr<object>  value)
        {
            if (Type != LispType::_List)
            {
                throw CreateInvalidCastException("list");
            }
            var list = Value->ToList();
            list->Add(value);
        }

        /*public*/ inline /*static*/ LispVariant operator+(const LispVariant & r)
        {
            if (IsString() || r.IsString())
            {
                return /*new*/ LispVariant(std::make_shared<object>(StringValue() + r.StringValue()));
            }
            if (IsDouble() || r.IsDouble())
            {
                return /*new*/ LispVariant(std::make_shared<object>(ToDouble() + r.ToDouble()));
            }
            if (IsInt() || r.IsInt())
            {
                return /*new*/ LispVariant(std::make_shared<object>(ToInt() + r.ToInt()));
            }
            if (IsList() && r.IsList())
            {
                var newList = new IEnumerable<std::shared_ptr<object>>();
                newList->AddRange(*(ListValue()));
                newList->AddRange(*(r.ListValue()));
                return /*new*/ LispVariant(std::make_shared<object>(newList));
            }
            throw CreateInvalidOperationException("+", *this, r);
        }

        /*public static*/ LispVariant operator -(const LispVariant & r)
        {
            if (IsDouble() || r.IsDouble())
            {
                return /*new*/ LispVariant(std::make_shared<object>(ToDouble() - r.ToDouble()));
            }
            if (IsInt() || r.IsInt())
            {
                return /*new*/ LispVariant(std::make_shared<object>(ToInt() - r.ToInt()));
            }
            throw CreateInvalidOperationException("-", *this, r);
        }

        /*public static*/ LispVariant operator *(const LispVariant & r)
        {
            if (IsDouble() || r.IsDouble())
            {
                return /*new*/ LispVariant(std::make_shared<object>(ToDouble() * r.ToDouble()));
            }
            if (IsInt() || r.IsInt())
            {
                return /*new*/ LispVariant(std::make_shared<object>(ToInt() * r.ToInt()));
            }
            throw CreateInvalidOperationException("*", *this, r);
        }

        /*public static*/ LispVariant operator /(const LispVariant & r)
        {
            if (IsDouble() || r.IsDouble())
            {
                return /*new*/ LispVariant(std::make_shared<object>(ToDouble() / r.ToDouble()));
            }
            if (IsInt() || r.IsInt())
            {
                return /*new*/ LispVariant(std::make_shared<object>(ToInt() / r.ToInt()));
            }
            throw CreateInvalidOperationException("/", *this, r);
        }

        /*public static*/ LispVariant operator <(const LispVariant & r) const
        {
            if (IsDouble() || r.IsDouble())
            {
                return /*new*/ LispVariant(std::make_shared<object>(ToDouble() < r.ToDouble()));
            }
            if (IsInt() || r.IsInt())
            {
                return /*new*/ LispVariant(std::make_shared<object>(ToInt() < r.ToInt()));
            }
            if (IsString() || r.IsString())
            {
                return /*new*/ LispVariant(std::make_shared<object>(String::CompareOrdinal(ToString(), r.ToString()) < 0));
            }
            throw CreateInvalidOperationException("< or >", *this, r);
        }

        /*public static*/ LispVariant operator >(const LispVariant & r) const
        {
            return r < *this;
        }

        /*public static*/ LispVariant operator <=(const LispVariant & r) const
        {
            if (IsDouble() || r.IsDouble())
            {
                return /*new*/ LispVariant(std::make_shared<object>(ToDouble() <= r.ToDouble()));
            }
            if (IsInt() || r.IsInt())
            {
                return /*new*/ LispVariant(std::make_shared<object>(ToInt() <= r.ToInt()));
            }
            if (IsString() || r.IsString())
            {
                return /*new*/ LispVariant(std::make_shared<object>(String::CompareOrdinal(ToString(), r.ToString()) <= 0));
            }
            throw CreateInvalidOperationException("<= or >=", *this, r);
        }

        /*public static*/ LispVariant operator >=(const LispVariant & r) const
        {
            return r <= *this;
        }

        /*public*/ static bool EqualOp(const LispVariant & l, const LispVariant & r)
        {
            if (l.IsNativeObject() && r.IsNativeObject())
            {
                return l.NativeObjectValue() == r.NativeObjectValue();
            }
            if (l.IsSymbol() || r.IsSymbol())
            {
                return l.IsSymbol() && r.IsSymbol() && (l.ToString() == r.ToString());
            }
            if (l.IsBool() && r.IsBool())
            {
                return l.BoolValue() == r.BoolValue();
            }
            if (l.IsNil() || r.IsNil())
            {
                return l.IsNil() && r.IsNil();
            }
            if (l.IsList() && r.IsList())
            {
                return l.ListValue()->SequenceEqual(*(r.ListValue()));
            }
            if (l.IsUndefined() || r.IsUndefined())
            {
                return l.IsUndefined() && r.IsUndefined();
            }
            if (l.IsDouble() || r.IsDouble())
            {
                return /*Math.A*/fabs(l.ToDouble() - r.ToDouble()) < Tolerance;
            }
            if (l.IsInt() || r.IsInt())
            {
                return l.ToInt() == r.ToInt();
            }
            if (l.IsString() || r.IsString())
            {
                return l.ToString() == r.ToString();
            }
            throw CreateInvalidOperationException("==", l, r);
        }

        //#endregion

    private:
        //#region private methods

        /*private*/ static LispType TypeOf(std::shared_ptr<object> obj)
        {
            if (obj->IsInt())
            {
                return LispType::_Int;
            }
            if (obj->IsDouble())
            {
                return LispType::_Double;
            }
            if (obj->IsBool())
            {
                return LispType::_Bool;
            }
            if (obj->IsString())
            {
                return LispType::_String;
            }
			if (obj->IsIEnumerableOfObject())
            {
                return LispType::_List;
            }
			/* 
			if (obj is IEnumerable)        // needed for .NET 3.5
            {
                return LispType::_List;
            }
			*/
            if (obj->IsLispFunctionWrapper())
            {
                return LispType::_Function;
            }			
            if (obj->IsLispVariant())
            {
                return obj->ToLispVariant()->Type;
            }
            if (obj->IsLispToken())
            {
                return LispType::_Symbol;
            }

            return LispType::_Undefined;
        }

        /*private*/ LispException CreateInvalidCastException(string name, string msg = "no") const
        {
            var exception = /*new*/ LispException(string::Format("Invalid cast for {2}, value={1} {0}", msg, StringValue(), name));
            exception.AddTokenInfos(Token);
            return exception;
        }

        static /*private*/ LispException CreateInvalidOperationException(string operation, LispVariant l, LispVariant r)
        {
            var exception = /*new*/ LispException(string::Format(NoOperatorForTypes, operation, l.Type, r.Type));
            exception.AddTokenInfos(l.Token);
            return exception;
        }

        //#endregion
    };
}

#endif
