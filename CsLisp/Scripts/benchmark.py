import time
start = time.time()

def TestLoopAndSum(max):
    i = 0
    sum = 0.0
    
    while i < max:
      sum = sum + i
      i = i + 1

    return sum
  
def g(x):
    return x * 3.4
  
def f(x):
    return x * (x + 2.1 + g(x))  
  
def TestCalls(max):
    result = 0
       
    for n in range(max):
        result = f(n)       
        
    return result
  
def count_found(searchtext, text):
    count = 0
    pos = 0
    found = True
    while found:
        pos = text.find(searchtext, pos)
        found = pos >= 0
        if found:
            count = count + 1
            pos = pos + 1
    return count
  
def TestStrings(max, s):
    result = ""

    for x in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]:
        result = result + str(x) + " --> " + s + "\n"
        
    print "found pos = ", result.find("content")
    print "count     = ", count_found("content", result)
    print "count     = ", count_found("tests", result)
        
    return len(result)
  
  
sLongString = "This is a very long string with out ony meaningful content. It is used for string manipulation tests! And for string search tests."
  
print "***LoopAndSum =", TestLoopAndSum(1000000)
print "***Calls      =", TestCalls(300000)
print "***Strings    =", TestStrings(10000, sLongString)

end = time.time()
print "time=", (end-start)