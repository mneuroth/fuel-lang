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

using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;

namespace CsLisp
{
    /// <summary>
    /// Lisp data types.
    /// </summary>
    public enum LispType
    {
        Undefined = 0,
        Nil = 1,
        Bool = 2,
        Int = 3,
        Double = 4,
        String = 5,
        List = 6,
        Function = 7,
        Symbol = 8,
        NativeObject = 9,
        //Array = 10,
        Error = 999
    }

    /// <summary>
    /// Enumeration for unqoute modus
    /// </summary>
    public enum LispUnQuoteModus
    {
        None = 0,
        UnQuote = 1,
        UnQuoteSplicing = 2
    }

    /// <summary>
    /// Generic data container for lisp data types.
    /// </summary>
    public class LispVariant : IComparable
    {
        #region constants

        private const string CanNotConvertTo = "can not convert {0} to {1}";
        private const string NoOperatorForTypes = "no {0} operator for types {1} and {2}";

        #endregion

        #region properties

        private static double Tolerance { get; set; }

        public LispUnQuoteModus IsUnQuoted { get; private set; }

        public object Value { get; set; }

        public LispType Type { get; set; }

        public string TypeString
        {
            get
            {
                if (Type == LispType.NativeObject)
                {
                    return Type+"<"+Value.GetType()+">";                    
                }
                return Type.ToString();
            }
        }

        public LispToken Token { get; private set; }

        public bool IsNil
        {
            get { return Type == LispType.Nil; }
        }

        public bool IsError
        {
            get { return Type == LispType.Error; }
        }

        public bool IsUndefined
        {
            get { return Type == LispType.Undefined; }
        }

        public bool IsString
        {
            get { return Type == LispType.String; }
        }

        public bool IsDouble
        {
            get { return Type == LispType.Double; }
        }

        public bool IsInt
        {
            get { return Type == LispType.Int; }
        }

        public bool IsNumber
        {
            get { return IsInt || IsDouble; }
        }

        public bool IsBool
        {
            get { return Type == LispType.Bool; }
        }

        public bool IsList
        {
            get { return Type == LispType.List || Type == LispType.Nil; }
        }

        public bool IsFunction
        {
            get { return Type == LispType.Function; }
        }

        public bool IsSymbol
        {
            get { return Type == LispType.Symbol; }
        }

        public bool IsNativeObject
        {
            get { return Type == LispType.NativeObject; }
        }

        #endregion

        #region Constructor

        /// <summary>
        /// Initializes the static elements of the <see cref="LispVariant"/> class.
        /// </summary>
        static LispVariant()
        {
            Tolerance = 1e-8;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="LispVariant"/> class.
        /// </summary>
        /// <param name="val">The value.</param>
        /// <remarks>Needed for compiler module and .NET 3.5</remarks>
        public LispVariant(object val)
            : this(val, LispUnQuoteModus.None)
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="LispVariant"/> class.
        /// </summary>
        /// <param name="type">The type.</param>
        /// <param name="value">The value.</param>
        /// <remarks>Needed for compiler module and .NET 3.5</remarks>
        public LispVariant(LispType type, object value)
            : this(type, value, LispUnQuoteModus.None)
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="LispVariant"/> class.
        /// </summary>
        /// <param name="type">The type.</param>
        /// <param name="value">The value.</param>
        /// <param name="unQuoted">The unquoted modus.</param>
        public LispVariant(LispType type = LispType.Undefined, object value = null, LispUnQuoteModus unQuoted = LispUnQuoteModus.None)
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
        public LispVariant(object val, LispUnQuoteModus unQuoted /*= LispUnQuoteModus.None*/)
            : this(TypeOf(val), val, unQuoted)
        {
            var value = val as LispVariant;
            if (value != null)
            {                
                Type = value.Type;
                Value = value.Value;
                IsUnQuoted = value.IsUnQuoted;
            }
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="LispVariant"/> class.
        /// </summary>
        /// <param name="token">The token.</param>
        /// <param name="unQuoted">The unquoted modus.</param>
        internal LispVariant(LispToken token, LispUnQuoteModus unQuoted = LispUnQuoteModus.None)
            : this(TypeOf(token.Value), token.Value, unQuoted)
        {
            Token = token;
            if (token.Type == LispTokenType.Nil)
            {
                Type = LispType.Nil;
            }
            if (token.Type == LispTokenType.Symbol)
            {
                Type = LispType.Symbol;
            }
        }

        /// <summary>
        /// Creates a new value representing an error.
        /// </summary>
        /// <param name="errorMessage">The error message.</param>
        /// <returns>The value</returns>
        public static LispVariant CreateErrorValue(string errorMessage)
        {
            return new LispVariant(LispType.Error, errorMessage);
        }

        #endregion

        #region IComparable

        /// <summary>
        /// Compares to other object.
        /// </summary>
        /// <param name="other">The other.</param>
        /// <returns></returns>
        public int CompareTo(Object other)
        {
            if (other is LispVariant)
            {
                var otherVariant = (LispVariant)other;
                if (IsNumber && otherVariant.IsNumber)
                {
                    if (IsDouble || otherVariant.IsDouble)
                    {
                        return ToDouble().CompareTo(otherVariant.ToDouble());
                    }
                    return IntValue.CompareTo(otherVariant.IntValue);
                }
                // all other types will be compared like a string
                return string.Compare(StringValue, otherVariant.StringValue, StringComparison.Ordinal);
            }            
            return CompareTo(new LispVariant(other));
        }

        #endregion

        #region Casts

        public LispFunctionWrapper FunctionValue
        {
            get
            {
                if (Type != LispType.Function)
                {
                    throw CreateInvalidCastException("function", "not found");
                }
                return (LispFunctionWrapper)Value;
            }
        }

        public IEnumerable<object> ListValue
        {
            get
            {
                // Nil is an empty list () !
                if (Type == LispType.Nil)
                {
                    return new List<object>();
                }
                if (Type == LispType.NativeObject && NativeObjectValue is IEnumerable<object>)
                {
                    return (IEnumerable<object>)NativeObjectValue;
                }
                if (Type != LispType.List)
                {
                    throw CreateInvalidCastException("list");
                }
                return ((IEnumerable)Value).Cast<object>();
            }
        }

        public double DoubleValue
        {
            get
            {
                if (Type != LispType.Double)
                {
                    throw CreateInvalidCastException("double");
                }
                return (double)Value;
            }
        }

        public int IntValue
        {
            get
            {
                if (Type != LispType.Int)
                {
                    throw CreateInvalidCastException("int");
                }
                return (int)Value;
            }
        }

        public bool BoolValue
        {
            get
            {
                if (Type != LispType.Bool)
                {
                    throw CreateInvalidCastException("bool");
                }
                return (bool)Value;
            }
        }

        public object NativeObjectValue
        {
            get
            {
                if (Type != LispType.NativeObject && Type != LispType.Nil)
                {
                    throw CreateInvalidCastException("native object");
                }
                return Value;
            }
        }

        public string NativeObjectStringRepresentation
        {
            get
            {
                string result = string.Empty;

                object native = NativeObjectValue;
                if (native is IEnumerable<object>)
                {
                    var container = (IEnumerable<object>)native;
                    foreach (var element in container)
                    {
                        if (result.Length > 0)
                        {
                            result += " ";
                        }
                        result += element != null ? element.ToString() : LispToken.Nil;
                    }
                    result = "(" + result + ")";
                }
                else
                {
                    result = native.ToString();
                }

                return result;
            }
        }

        public bool ToBool()
        {
            if (IsBool)
            {
                return BoolValue;
            }
            if (IsInt)
            {
                return IntValue != 0;
            }
            if (IsDouble)
            {
                return Math.Abs(DoubleValue) > Tolerance;
            }
            throw CreateInvalidCastException("bool", string.Format(CanNotConvertTo, TypeString, "bool"));
        }

        public int ToInt()
        {
            if (IsBool)
            {
                return BoolValue ? 1 : 0;
            }
            if (IsInt)
            {
                return IntValue;
            }
            if (IsDouble)
            {
                return (int) DoubleValue;
            }
            if (IsString)
            {
                return Convert.ToInt32(StringValue);
            }
            throw CreateInvalidCastException("int", string.Format(CanNotConvertTo, TypeString, "int"));
        }

        public double ToDouble()
        {
            if (IsBool)
            {
                return BoolValue ? 1.0 : 0.0;
            }
            if (IsInt)
            {
                return IntValue;
            }
            if (IsDouble)
            {
                return DoubleValue;
            }
            if (IsString)
            {
                return Convert.ToDouble(StringValue, CultureInfo.InvariantCulture);
            }
            throw CreateInvalidCastException("double", string.Format(CanNotConvertTo, TypeString, "double"));
        }

        public string StringValue
        {
            get
            {
                return Value.ToString();
            }
        }

        /// <summary>
        /// Comverts this value into a string representation used by the compiler module.
        /// </summary>
        /// <returns>The string representation</returns>
        public string ToStringCompiler()
        {
            if (IsBool)
            {
                return BoolValue ? "true" : "false";
            }
            if (IsDouble)
            {
                return DoubleValue.ToString("F", CultureInfo.InvariantCulture);
            }
            if (IsString)
            {
                return "\"" + StringValue + "\"";
            }
            return ToString();
        }

        /// <summary>
        /// Comverts this value into a string representation used by the debugger module.
        /// </summary>
        /// <returns>The string representation</returns>
        public string ToStringDebugger()
        {
            if (IsString)
            {
                return "\"" + StringValue + "\"";
            }
            return ToString();
        }

        public override string ToString()
        {
            if (IsString)
            {
                return StringValue;
            }
            if (IsInt)
            {
                return IntValue.ToString(CultureInfo.InvariantCulture);
            }
            if (IsDouble)
            {
                return DoubleValue.ToString(CultureInfo.InvariantCulture);
            }
            if (IsBool)
            {
                return BoolValue ? "#t" : "#f";
            }
            if (IsNil)
            {
                return LispToken.Nil;
            }
            if (IsList)
            {
                return ExpandContainerToString(ListValue);
            }
            if (IsFunction)
            {
                return "function " + (FunctionValue.Signature != null ? FunctionValue.Signature : "<unknown>");
            }
            if (IsSymbol)
            {
                return Value.ToString();
            }
            if (IsNativeObject)
            {
                return NativeObjectStringRepresentation;
            }
            if (IsUndefined)
            {
                return "<undefined>";
            }
            if (IsError)
            {
                return "Error: " + Value;
            }
            return "?";
        }

        // used for compiler module
        public static explicit operator Func<object[], LispScope, LispVariant>(LispVariant variant)
        {
            return variant.FunctionValue.Function;
        }

        // used for compiler module
        public static explicit operator Func<LispVariant, LispScope, LispVariant>(LispVariant variant)
        {
            return (arg1, scope) => variant.FunctionValue.Function(new object[] { arg1 }, scope);
        }

        // used for compiler module
        public static explicit operator Func<LispVariant, LispVariant, LispScope, LispVariant>(LispVariant variant)
        {
            return (arg1, arg2, scope) => variant.FunctionValue.Function(new object[] { arg1, arg2 }, scope);
        }

        // used for compiler module
        public static explicit operator Func<LispVariant, LispVariant, LispVariant, LispScope, LispVariant>(LispVariant variant)
        {
            return (arg1, arg2, arg3, scope) => variant.FunctionValue.Function(new object[] { arg1, arg2, arg3 }, scope);
        }

        private static string ExpandContainerToString(object maybeContainer)
        {
            string ret = string.Empty;

            if (maybeContainer is IEnumerable<object>)
            {
                var container = (IEnumerable<object>)maybeContainer;
                foreach (var item in container)
                {
                    if (ret.Length > 0)
                    {
                        ret += " ";
                    }
                    ret += ExpandContainerToString(item);
                }
                ret = "(" + ret + ")";
            }
            else
            {
                ret += ExpandItemForContainer(maybeContainer);
            }

            return ret;
        }

        private static string ExpandItemForContainer(object item)
        {
            if (item is LispVariant)
            {
                LispVariant variant = (LispVariant)item;
                if (variant.IsString)
                {
                    return variant.ToStringDebugger();
                }
            }
            return item.ToString();
        }

        #endregion

        public bool SymbolCompare(object other)
        {
            if (other is LispVariant)
            {
                return Value.Equals(((LispVariant)other).Value);
            }
            return false;
        }

        #region overloaded methods

        /// <summary>
        /// Determines whether the specified <see cref="T:System.Object"/> is equal to the current <see cref="T:System.Object"/>.
        /// </summary>
        /// <returns>
        /// true if the specified <see cref="T:System.Object"/> is equal to the current <see cref="T:System.Object"/>; otherwise, false.
        /// </returns>
        /// <param name="other">The object to compare with the current object. </param><filterpriority>2</filterpriority>
        public override bool Equals(object other)
        {
            if (other is LispVariant)
            {
                return EqualOp(this, (LispVariant)other);
            }
            return false;
        }

        /// <summary>
        /// Returns a hash code for this instance.
        /// </summary>
        /// <returns>
        /// A hash code for this instance, suitable for use in hashing algorithms and data structures like a hash table. 
        /// </returns>
        public override int GetHashCode()
        {
            // ReSharper disable once BaseObjectGetHashCodeCallInGetHashCode
            return base.GetHashCode();
        }

        #endregion

        #region Operations

        public void Add(object value)
        {
            if (Type != LispType.List)
            {
                throw CreateInvalidCastException("list");
            }
            var list = (List<object>)Value;
            list.Add(value);
        }

        public static LispVariant operator +(LispVariant l, LispVariant r)
        {
            if (l.IsString || r.IsString)
            {
                return new LispVariant(l.StringValue + r.StringValue);
            }
            if (l.IsDouble || r.IsDouble)
            {
                return new LispVariant(l.ToDouble() + r.ToDouble());
            }
            if (l.IsInt || r.IsInt)
            {
                return new LispVariant(l.ToInt() + r.ToInt());
            }
            if (l.IsList && r.IsList)
            {
                var newList = new List<object>();
                newList.AddRange(l.ListValue);
                newList.AddRange(r.ListValue);
                return new LispVariant(newList);
            }
            throw CreateInvalidOperationException("+", l, r);
        }

        public static LispVariant operator -(LispVariant l, LispVariant r)
        {
            if (l.IsDouble || r.IsDouble)
            {
                return new LispVariant(l.ToDouble() - r.ToDouble());
            }
            if (l.IsInt || r.IsInt)
            {
                return new LispVariant(l.ToInt() - r.ToInt());
            }
            throw CreateInvalidOperationException("-", l, r);
        }

        public static LispVariant operator *(LispVariant l, LispVariant r)
        {
            if (l.IsDouble || r.IsDouble)
            {
                return new LispVariant(l.ToDouble() * r.ToDouble());
            }
            if (l.IsInt || r.IsInt)
            {
                return new LispVariant(l.ToInt() * r.ToInt());
            }
            throw CreateInvalidOperationException("*", l, r);
        }

        public static LispVariant operator /(LispVariant l, LispVariant r)
        {
            if (l.IsDouble || r.IsDouble)
            {
                return new LispVariant(l.ToDouble() / r.ToDouble());
            }
            if (l.IsInt || r.IsInt)
            {
                return new LispVariant(l.ToInt() / r.ToInt());
            }
            throw CreateInvalidOperationException("/", l, r);
        }

        public static LispVariant operator <(LispVariant l, LispVariant r)
        {
            if (l.IsDouble || r.IsDouble)
            {
                return new LispVariant(l.ToDouble() < r.ToDouble());
            }
            if (l.IsInt || r.IsInt)
            {
                return new LispVariant(l.ToInt() < r.ToInt());
            }
            if (l.IsString || r.IsString)
            {
                return new LispVariant(string.CompareOrdinal(l.ToString(), r.ToString()) < 0);
            }
            throw CreateInvalidOperationException("< or >", l, r);
        }

        public static LispVariant operator >(LispVariant l, LispVariant r)
        {
            return r < l;
        }

        public static LispVariant operator <=(LispVariant l, LispVariant r)
        {
            if (l.IsDouble || r.IsDouble)
            {
                return new LispVariant(l.ToDouble() <= r.ToDouble());
            }
            if (l.IsInt || r.IsInt)
            {
                return new LispVariant(l.ToInt() <= r.ToInt());
            }
            if (l.IsString || r.IsString)
            {
                return new LispVariant(string.CompareOrdinal(l.ToString(), r.ToString()) <= 0);
            }
            throw CreateInvalidOperationException("<= or >=", l, r);
        }

        public static LispVariant operator >=(LispVariant l, LispVariant r)
        {
            return r <= l;
        }

        public static bool EqualOp(LispVariant l, LispVariant r)
        {
            if (l.IsNativeObject && r.IsNativeObject)
            {
                return l.NativeObjectValue == r.NativeObjectValue;
            }
            if (l.IsSymbol || r.IsSymbol)
            {
                return l.IsSymbol && r.IsSymbol && (l.ToString() == r.ToString());
            }
            if (l.IsBool && r.IsBool)
            {
                return l.BoolValue == r.BoolValue;
            }
            if (l.IsNil || r.IsNil)
            {
                return l.IsNil && r.IsNil;
            }
            if (l.IsList && r.IsList)
            {
                return l.ListValue.SequenceEqual(r.ListValue);
            }
            if (l.IsUndefined || r.IsUndefined)
            {
                return l.IsUndefined && r.IsUndefined;
            }
            if (l.IsDouble || r.IsDouble)
            {
                return Math.Abs(l.ToDouble() - r.ToDouble()) < Tolerance;
            }
            if (l.IsInt || r.IsInt)
            {
                return l.ToInt() == r.ToInt();
            }
            if (l.IsString || r.IsString)
            {
                return l.ToString() == r.ToString();
            }
            throw CreateInvalidOperationException("==", l, r);
        }

        #endregion

        #region private methods

        private static LispType TypeOf(object obj)
        {
            if (obj is int)
            {
                return LispType.Int;
            }
            if (obj is double)
            {
                return LispType.Double;
            }
            if (obj is bool)
            {
                return LispType.Bool;
            }
            if (obj is string)
            {
                return LispType.String;
            }
            if (obj is IEnumerable<object>)
            {
                return LispType.List;
            }
            if (obj is IEnumerable)        // needed for .NET 3.5
            {
                return LispType.List;
            }
            if (obj is LispFunctionWrapper)
            {
                return LispType.Function;
            }
            if (obj is LispVariant)
            {
                return ((LispVariant)obj).Type;
            }
            if (obj is LispToken)
            {
                return LispType.Symbol;
            }
            return LispType.Undefined;
        }

        private Exception CreateInvalidCastException(string name, string msg = "no")
        {
            var exception = new LispException(string.Format("Invalid cast for {2}, value={1} {0}", msg, StringValue, name));
            exception.AddTokenInfos(Token);
            return exception;
        }

        static private Exception CreateInvalidOperationException(string operation, LispVariant l, LispVariant r)
        {
            var exception = new LispException(string.Format(NoOperatorForTypes, operation, l.Type, r.Type));
            exception.AddTokenInfos(l.Token);
            return exception;
        }

        #endregion
    }
}
