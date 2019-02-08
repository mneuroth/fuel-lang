
function TestLoopAndSum(max)
{
    var i = 0;
    var sum = 0.0;
    
    while( i < max )
    {
      sum = sum + i;
      i = i + 1;
    }
    
    return sum;
}

function g(x)
{
    return x * 3.4;
}
  
function f(x)
{
    return x * (x + 2.1 + g(x));
}

function TestCalls(max)
{
    var result = 0;
    var n;
       
    for( n=0; n<max; n++ )
    {
        result = f(n);
    }
        
    return result;
}

function count_found(searchtext, text)
{
    var count = 0;
    var pos = 0;
    var found = true;
    while( found )
    {
        pos = text.indexOf(searchtext, pos);
        found = pos >= 0;
        if( found )
        {
            count = count + 1;
            pos = pos + 1;
        }
    }    
    return count;
}
    
function TestStrings(max, s)
{
    var result = "";

    var arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
    arr.forEach(
      function(x)
      {
        result = result + x + " --> " + s + "\n";
      } );
        
    print( "found pos = ", result.indexOf("content") );
    print( "count     = ", count_found("content", result) );
    print( "count     = ", count_found("tests", result) );
        
    return result.length;
}  
  
var sLongString = "This is a very long string with out ony meaningful content. It is used for string manipulation tests! And for string search tests.";
  
print( "***LoopAndSum =", TestLoopAndSum(1000000) );
print( "***Calls      =", TestCalls(300000) );
print( "***Strings    =", TestStrings(10000, sLongString) );
