function TestLoopAndSum(max)
    i = 0
    sum = 0.0
    
    while( i < max )
    do
      sum = sum + i
      i = i + 1
    end

    return sum
end  
  
function g(x)
    return x * 3.4
end

function f(x)
    return x * (x + 2.1 + g(x))  
end    
  
function TestCalls(max)
    result = 0
       
    for n=0, max, 1 do
        result = f(n)       
    end
        
    return result
end

function count_found(searchtext, text)
    count = 0
    pos = 0
    found = True
    while( found )
    do
        pos = string.find(text, searchtext, pos)
        found = pos >= 0
        if( found )
        then
            count = count + 1
            pos = pos + 1
        end
    end
    return count
end    
  
function TestStrings(max, s)
    result = ""

    arr =  { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 }
    for k, v in pairs(arr)
    do
        result = result .. tostring(k) .. " --> " .. s .. "\n"
    end
    
    print( "found pos = ", string.find(result, "content") )
    print( "count     = ", count_found("content", result) )
    print( "count     = ", count_found("tests", result) )
        
    return string.len(result)
end  
  
sLongString = "This is a very long string with out ony meaningful content. It is used for string manipulation tests! And for string search tests."
  
print( "***LoopAndSum =", TestLoopAndSum(1000000) )
print( "***Calls      =", TestCalls(300000) )
print( "***Strings    =", TestStrings(10000, sLongString) )
