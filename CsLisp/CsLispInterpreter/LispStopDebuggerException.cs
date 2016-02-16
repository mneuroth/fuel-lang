namespace CsLisp
{
    public class LispStopDebuggerException : LispException
    {
        public LispStopDebuggerException(string text = "")
            : base(text)
        {
        }
    }

}
